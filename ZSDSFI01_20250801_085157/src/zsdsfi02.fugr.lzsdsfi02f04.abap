*----------------------------------------------------------------------*
***INCLUDE LZSDSFI02F04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form process_block_sales
*&---------------------------------------------------------------------*
*& Proceed to block.
*&---------------------------------------------------------------------*
FORM PROCESS_BLOCK_SALES  CHANGING CS_BLOCK TYPE ZSDSFIS116.


  DATA: LT_PARTNER TYPE ZSDSFIS083_TT,
        LV_GUID    TYPE BU_PARTNER_GUID.

  DATA:
    LS_BP           TYPE CVIS_EI_EXTERN,
    LT_BP           TYPE CVIS_EI_EXTERN_T,
    LT_PARTNER_FUNC TYPE CMDS_EI_FUNCTIONS_T,
    LT_CUS_SALES    TYPE CMDS_EI_SALES_T,
    LV_PARZA        TYPE KNVP-PARZA.


  CALL FUNCTION 'BUPA_NUMBERS_GET'
    EXPORTING
      IV_PARTNER      = CS_BLOCK-PARTNER
    IMPORTING
      EV_PARTNER_GUID = LV_GUID.

  LS_BP-PARTNER-HEADER-OBJECT_TASK  = 'U'.
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = LV_GUID.
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER     = CS_BLOCK-PARTNER.


  "Block
  LS_BP-CUSTOMER-HEADER-OBJECT_TASK = 'U'.

  "Sales Order Block (All Sales Areas)
  LS_BP-CUSTOMER-CENTRAL_DATA-CENTRAL-DATA-AUFSD      = CS_BLOCK-SO_ALL.
  LS_BP-CUSTOMER-CENTRAL_DATA-CENTRAL-DATAX-AUFSD     = ABAP_TRUE.

  "Delivery Block (All Sales Areas)
  LS_BP-CUSTOMER-CENTRAL_DATA-CENTRAL-DATA-LIFSD      = CS_BLOCK-DO_ALL.
  LS_BP-CUSTOMER-CENTRAL_DATA-CENTRAL-DATAX-LIFSD     = ABAP_TRUE.

  "Billing Block (All Sales Areas)
  LS_BP-CUSTOMER-CENTRAL_DATA-CENTRAL-DATA-FAKSD      = CS_BLOCK-BILL_ALL.
  LS_BP-CUSTOMER-CENTRAL_DATA-CENTRAL-DATAX-FAKSD     = ABAP_TRUE.

*"Sale data
  IF CS_BLOCK-VTWEG IS INITIAL.
    SELECT
      VKORG,
      VTWEG,
      SPART
     FROM KNVV
     INTO TABLE @DATA(LT_KNVV)
     WHERE KUNNR = @CS_BLOCK-PARTNER.
    IF SY-SUBRC = 0.
      SORT LT_KNVV BY VTWEG.
    ENDIF.
  ELSE.
    APPEND INITIAL LINE TO LT_KNVV ASSIGNING FIELD-SYMBOL(<LFS_KNVV>).
    <LFS_KNVV>-VKORG = GC_SALES_AREA-VKORG.
    <LFS_KNVV>-VTWEG = CS_BLOCK-VTWEG.
    <LFS_KNVV>-SPART = GC_SALES_AREA-SPART.
  ENDIF.

  LOOP AT LT_KNVV ASSIGNING <LFS_KNVV>.
    APPEND INITIAL LINE TO LT_CUS_SALES ASSIGNING FIELD-SYMBOL(<LFS_CUS_SALES>).
    <LFS_CUS_SALES>-TASK           = 'U'.
    <LFS_CUS_SALES>-DATA_KEY-VKORG = <LFS_KNVV>-VKORG.
    <LFS_CUS_SALES>-DATA_KEY-VTWEG = <LFS_KNVV>-VTWEG.
    <LFS_CUS_SALES>-DATA_KEY-SPART = <LFS_KNVV>-SPART.

    "Sales Order Block (Selected Sales Areas)
    <LFS_CUS_SALES>-DATA-AUFSD      = CS_BLOCK-SO_SEL.
    <LFS_CUS_SALES>-DATAX-AUFSD     = ABAP_TRUE.

    "Delivery Block (Selected Sales Areas)
    <LFS_CUS_SALES>-DATA-LIFSD      = CS_BLOCK-DO_SEL.
    <LFS_CUS_SALES>-DATAX-LIFSD     = ABAP_TRUE.

    "Billing Block (Selected Sales Areas)
    <LFS_CUS_SALES>-DATA-FAKSD      = CS_BLOCK-BILL_SEL.
    <LFS_CUS_SALES>-DATAX-FAKSD     = ABAP_TRUE.
  ENDLOOP.

*<lfs_cus_sales>-functions-functions = lt_partner_func[].
  LS_BP-CUSTOMER-SALES_DATA-SALES = LT_CUS_SALES[].

*------------------------------------------------------------------------------
* Validate data
*------------------------------------------------------------------------------
  CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE(
    EXPORTING
      I_DATA        = LS_BP
    IMPORTING
      ET_RETURN_MAP = DATA(LT_RETURN_MAP)
  ).

  "Filter message out ->Errors occurred during call of function module BUPA_CREATE_FROM_DATA
  PERFORM FILTER_ERROR_OUT  CHANGING LT_RETURN_MAP.

  IF LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'E' ] ) OR
     LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'A' ] ).

    LOOP AT LT_RETURN_MAP INTO DATA(LS_RETURN_MAP)
                          WHERE TYPE = 'E'
                             OR TYPE = 'A'.
      CS_BLOCK-MSG_TYP = LS_RETURN_MAP-TYPE.
      CS_BLOCK-MESSAGE = LS_RETURN_MAP-MESSAGE.
      EXIT. "exit loop
    ENDLOOP.

    EXIT. "Exit program
  ENDIF.

  INSERT LS_BP INTO TABLE LT_BP.

  CL_MD_BP_MAINTAIN=>MAINTAIN(
    EXPORTING
      I_DATA     = LT_BP
      I_TEST_RUN = ''
    IMPORTING
      E_RETURN   = DATA(LT_RETURN)
  ).

  TRY.
      DATA(LT_OBJ_MSG) = LT_RETURN[ 1 ]-OBJECT_MSG.
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      "ok to error
  ENDTRY.
  IF LINE_EXISTS( LT_OBJ_MSG[ TYPE = 'E' ] ) OR
     LINE_EXISTS( LT_OBJ_MSG[ TYPE = 'A' ] ).
    "Error case
    LOOP AT LT_OBJ_MSG INTO DATA(LS_OBJ_MSG)
                      WHERE TYPE = 'E' OR
                            TYPE = 'A'.
      CS_BLOCK-MSG_TYP = LS_OBJ_MSG-TYPE.
      CS_BLOCK-MESSAGE = LS_OBJ_MSG-MESSAGE.
    ENDLOOP.
  ELSE.
*  PERFORM commit_work.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.

    CS_BLOCK-MSG_TYP = 'S'.
    CS_BLOCK-MESSAGE = |{ CS_BLOCK-PARTNER } has been updated.|.
  ENDIF.

ENDFORM.
