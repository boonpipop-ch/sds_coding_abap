*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0070
*  Creation Date      : 08.03.2024
*  Author             : B.Chiewsarikij
*  Add-on ID          : ZMMF008
*  Description        : PO form
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSMMR0070
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSMMR0070.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: NAST,
        TNAPR,
        EKKO,
        EKPO,
        ARC_PARAMS,
        TOA_DARA,
        ADDR_KEY.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
*TYPES:

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
*DATA:
*-> internal tables
*-> range
*-> work areas
*-> variables
*-> reference

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
*CONSTANTS:

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
*SELECTION-SCREEN BEGIN OF BLOCK s01 WITH FRAME TITLE TEXT-001.
* PARAMETERS:
* SELECT-OPTIONS:
*SELECTION-SCREEN END OF BLOCK s01.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
FORM ENTRY_NEU USING UV_RETCO  TYPE SY-SUBRC
                     UV_SCREEN TYPE C.

  DATA: LV_DRUVO       LIKE T166K-DRUVO.
*        lv_nast        LIKE nast,
*        lv_from_memory,
*        lv_doc         TYPE meein_purchase_doc_print.

  CHECK NAST-KAPPL = 'EF'.  "Check for print Purchase Order (PO)


  CLEAR UV_RETCO.
  IF NAST-AENDE EQ SPACE.
    LV_DRUVO = '1'.
  ELSE.
    LV_DRUVO = '2'.
  ENDIF.

  PERFORM PRINT_OUTPUT USING LV_DRUVO
                             UV_SCREEN
                       CHANGING UV_RETCO.


*  "Get Purchase order data
*  CREATE OBJECT cl_output_po
*    TYPE
*    cl_purchase_order_output
*    EXPORTING
*      c_mode     = l_druvo
*      es_nast    = nast
*      iv_preview = l_from_memory.

*  CALL FUNCTION 'ME_READ_PO_FOR_PRINTING'
*    EXPORTING
*      ix_nast        = nast
*      ix_screen      = ent_screen
*    IMPORTING
*      ex_retco       = ent_retco
*      ex_nast        = l_nast
*      doc            = l_doc
*    CHANGING
*      cx_druvo       = l_druvo
*      cx_from_memory = l_from_memory.
*  CHECK uv EQ 0.


*  ls_control_param-getotf       = abap_true.
*  ls_control_param-no_dialog    = abap_true.
*  ls_composer_param-tdprinter   = 'LOCL'.

  "Get function name
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname = tnapr-sform
*    IMPORTING
*      fm_name  = lv_fn_name.
*  IF sy-subrc <> 0.
**    Implement suitable error handling here
*  ENDIF.

*  l_doc
*  DATA(ls_doc_header) = l_doc-xekko.    "Purchase Header
*  DATA(lt_doc_item)   = l_doc-xekpo[].  "Purchase Item

*  CALL FUNCTION lv_fn_name
*    EXPORTING
**     ARCHIVE_INDEX      =
**     ARCHIVE_INDEX_TAB  =
**     ARCHIVE_PARAMETERS =
*      control_parameters = ls_control_param
**     MAIL_APPL_OBJ      =
**     MAIL_RECIPIENT     =
**     MAIL_SENDER        =
**     OUTPUT_OPTIONS     =
**     USER_SETTINGS      = 'X'
**     is_doc_header      =
** IMPORTING
**     DOCUMENT_OUTPUT_INFO       =
**     JOB_OUTPUT_INFO    =
**     JOB_OUTPUT_OPTIONS =
** EXCEPTIONS
**     FORMATTING_ERROR   = 1
**     INTERNAL_ERROR     = 2
**     SEND_ERROR         = 3
**     USER_CANCELED      = 4
**     OTHERS             = 5
*    .
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

*  "Get form name
*  CALL FUNCTION 'ME_PRINT_PO'
*    EXPORTING
*      ix_nast        = l_nast
*      ix_druvo       = l_druvo
*      doc            = l_doc
*      ix_screen      = ent_screen
*      ix_from_memory = l_from_memory
*      ix_toa_dara    = toa_dara
*      ix_arc_params  = arc_params
*      ix_fonam       = tnapr-fonam
*    IMPORTING
*      ex_retco       = ent_retco.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form print_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM PRINT_OUTPUT USING VALUE(UV_DRUVO)   TYPE DRUVO
                        VALUE(UV_PREVIEW) TYPE C
                  CHANGING CV_RETCO       TYPE SY-SUBRC.

  DATA: LV_FN_NAME         TYPE RS38L_FNAM,
        LS_COMPOSER_PARAM  TYPE SSFCOMPOP,
        CL_OUTPUT_PO       TYPE REF TO CL_PURCHASE_ORDER_OUTPUT,
        LS_JOB_OUTPUT_INFO TYPE SSFCRESCL,
        LV_SUBRC           TYPE SY-SUBRC.

  DATA(LO_OUTPUT_PO) = NEW CL_PURCHASE_ORDER_OUTPUT( C_MODE     = UV_DRUVO
                                                     ES_NAST    = NAST
                                                     IV_PREVIEW = UV_PREVIEW ).
  LO_OUTPUT_PO->READ( ).

*  DATA(LS_LFA1) = LO_OUTPUT_PO->IT_LFA1[ 1 ].
  SELECT SINGLE ADRNR
    FROM LFA1
    INTO @DATA(LV_ADRNR)
    WHERE LIFNR = @LO_OUTPUT_PO->IS_EKKO-LIFNR.
  IF LV_ADRNR IS NOT INITIAL.
    "CALL CDS View
    SELECT SINGLE *
      FROM ZSDSVC_GET_ADDRESS( I_ADDRNUMBER = @LV_ADRNR,
                               I_LANG       = @SY-LANGU )
      INTO @DATA(LS_VENDOR_ADDR).
  ENDIF.

  IF TNAPR-SFORM IS NOT INITIAL.

    "GET function name
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME = TNAPR-SFORM
      IMPORTING
        FM_NAME  = LV_FN_NAME.
    IF SY-SUBRC <> 0.
      PERFORM PROTOCOL_UPDATE USING 'ME'
                                    SYST-MSGNO
                                    SYST-MSGTY
                                    SYST-MSGV1
                                    SYST-MSGV2
                                    SYST-MSGV3
                                    SYST-MSGV4.

      CV_RETCO = SY-SUBRC.
      RETURN.
    ENDIF.
  ELSE.
    PERFORM PROTOCOL_UPDATE USING 'VN'
                                  '027'
                                  'E'
                                  SYST-MSGV1
                                  SYST-MSGV2
                                  SYST-MSGV3
                                  SYST-MSGV4.
    CV_RETCO = 1.
    RETURN.
  ENDIF.

  "check return code
  CV_RETCO = LO_OUTPUT_PO->RET_CODE.
  IF CV_RETCO IS NOT INITIAL.
    SYST-MSGV1 = LO_OUTPUT_PO->IS_EKKO-EBELN.
    SYST-MSGNO = '140'.
  ENDIF.

  IF CV_RETCO IS NOT INITIAL.
    PERFORM PROTOCOL_UPDATE USING 'ME'
                                  SYST-MSGNO
                                  'W'
                                  SYST-MSGV1
                                  SYST-MSGV2
                                  SYST-MSGV3
                                  SYST-MSGV4.

    RETURN.
  ENDIF.

  DATA(LS_CONTROL_PARAM) = VALUE SSFCTRLOP( PREVIEW = UV_PREVIEW
                                            LANGU = NAST-SPRAS ).

  DATA(LS_OUTPUT_OPTION) = VALUE SSFCOMPOP(
                              TDDEST = NAST-LDEST
                              TDIMMED = NAST-DIMME
                              TDDELETE = NAST-DELET
                              TDNEWID = ABAP_TRUE
                              TDNOPRINT = SWITCH #( LO_OUTPUT_PO->IS_EKKO-FRGKE
                                                    WHEN 'W' THEN ABAP_TRUE
                                                    ELSE ABAP_FALSE ) ).
  CLEAR LS_OUTPUT_OPTION-TDDEST.

  PERFORM GET_AMOUNT USING LO_OUTPUT_PO.

  PERFORM CALL_PO_FORM USING LV_FN_NAME
                             LO_OUTPUT_PO
                             LS_CONTROL_PARAM
                             LS_OUTPUT_OPTION
                             LS_VENDOR_ADDR
                       CHANGING LS_JOB_OUTPUT_INFO
                                LV_SUBRC.
  IF LV_SUBRC <> 0.
    CV_RETCO = 1.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form protocol_update
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM PROTOCOL_UPDATE USING UV_MSG_ARBGB TYPE SY-MSGID
                           UV_MSG_NR    TYPE SY-MSGNO
                           UV_MSG_TY    TYPE SY-MSGTY
                           UV_MSG_V1    TYPE SY-MSGV1
                           UV_MSG_V2    TYPE SY-MSGV2
                           UV_MSG_V3    TYPE SY-MSGV3
                           UV_MSG_V4    TYPE SY-MSGV4.

  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      MSG_ARBGB = UV_MSG_ARBGB
      MSG_NR    = UV_MSG_NR
      MSG_TY    = UV_MSG_TY
      MSG_V1    = UV_MSG_V1
      MSG_V2    = UV_MSG_V2
      MSG_V3    = UV_MSG_V3
      MSG_V4    = UV_MSG_V4
    EXCEPTIONS
      OTHERS    = 1 ##FM_SUBRC_OK.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form call_po_form
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CALL_PO_FORM  USING   UV_FN_NAME             TYPE TNAPR-FUNCNAME
                           UO_OUTPUT_PO           TYPE REF TO CL_PURCHASE_ORDER_OUTPUT
                           US_CONTROL_PARAM       TYPE SSFCTRLOP
                           US_OUTPUT_OPTION       TYPE SSFCOMPOP
                           US_VENDOR_ADDR         TYPE ZSDSVC_GET_ADDRESS
                   CHANGING CS_JOB_OUTPUT_INFO    TYPE SSFCRESCL
                            CV_SUBRC              TYPE SYST_SUBRC.


  "Replace Delivery date
*  SORT UO_OUTPUT_PO->IT_EKET BY EBELN EBELP ETENR.
  SELECT EBELN,
         EBELP,
         ETENR,
         EINDT
    FROM EKET
    INTO TABLE @DATA(LT_EKET)
    WHERE EBELN = @UO_OUTPUT_PO->IS_EKKO-EBELN.
  IF SY-SUBRC = 0.
    SORT LT_EKET BY EBELN EBELP EINDT.
  ENDIF.
  LOOP AT UO_OUTPUT_PO->IT_EKPO ASSIGNING FIELD-SYMBOL(<LFS_EKPO>).
    READ TABLE LT_EKET  INTO DATA(LS_DELIVERY)
                        WITH KEY EBELN = <LFS_EKPO>-EBELN
                                 EBELP = <LFS_EKPO>-EBELP
                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      <LFS_EKPO>-LFDAT = LS_DELIVERY-EINDT.
    ELSE.
      <LFS_EKPO>-LFDAT = SY-DATUM.
    ENDIF.

    "Convert JPY
    IF UO_OUTPUT_PO->IS_EKKO-WAERS = 'JPY'.
      <LFS_EKPO>-NETPR = <LFS_EKPO>-NETPR * 100.
      <LFS_EKPO>-NETWR = <LFS_EKPO>-NETWR * 100.
      <LFS_EKPO>-BRTWR = <LFS_EKPO>-BRTWR * 100.
    ENDIF.
  ENDLOOP.

*  DATA: LV_RUNNING TYPE N.
*  DO 180 TIMES.
*    READ TABLE UO_OUTPUT_PO->IT_EKPO INTO DATA(LS_EKPO)
*                                     INDEX 2.
*    IF SY-SUBRC = 0.
*      LS_EKPO-EBELP = LS_EKPO-EBELP + SY-INDEX.
*      APPEND LS_EKPO TO UO_OUTPUT_PO->IT_EKPO.
*    ENDIF.
*  ENDDO.

  CALL FUNCTION UV_FN_NAME
    EXPORTING
*     ARCHIVE_INDEX      =
*     ARCHIVE_INDEX_TAB  =
*     ARCHIVE_PARAMETERS =
      CONTROL_PARAMETERS = US_CONTROL_PARAM
*     MAIL_APPL_OBJ      =
*     MAIL_RECIPIENT     =
*     MAIL_SENDER        =
      OUTPUT_OPTIONS     = US_OUTPUT_OPTION
      USER_SETTINGS      = SPACE
      IS_DOC_HEADER      = UO_OUTPUT_PO->IS_EKKO
      IT_DOC_ITEM        = UO_OUTPUT_PO->IT_EKPO
      IS_VENDOR_ADDR     = US_VENDOR_ADDR
    IMPORTING
      JOB_OUTPUT_INFO    = CS_JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here

    CV_SUBRC = SY-SUBRC.

    PERFORM PROTOCOL_UPDATE USING SY-MSGID
                                  SY-MSGNO
                                  SY-MSGTY
                                  SY-MSGV1
                                  SY-MSGV2
                                  SY-MSGV3
                                  SY-MSGV4.

    RETURN.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_amount
*&---------------------------------------------------------------------*
*& Calculate amount
*&---------------------------------------------------------------------*
FORM GET_AMOUNT  USING UO_OUTPUT_PO TYPE REF TO CL_PURCHASE_ORDER_OUTPUT.


  "Get condition type and value
  SELECT
    KSCHL,
    KPOSN,
    LIFNR,
    KBETR,
    KKURS,
    KAWRT,
    KWERT
  INTO TABLE @DATA(LT_KONV)
  FROM PRCD_ELEMENTS "KONV
  WHERE KNUMV = @UO_OUTPUT_PO->IS_KOMK-KNUMV
    AND KSCHL EQ 'ZVAT'.
  IF SY-SUBRC = 0.


  ENDIF.


*  PRICING_GET_CONDITIONS







ENDFORM.
