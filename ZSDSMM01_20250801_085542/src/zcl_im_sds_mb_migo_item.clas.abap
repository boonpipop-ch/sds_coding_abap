class ZCL_IM_SDS_MB_MIGO_ITEM definition
  public
  final
  create public .

public section.

  interfaces IF_EX_MB_MIGO_ITEM_BADI .

  methods VALIDATE_FREE_STOCK
    importing
      !IS_GOITEM type GOITEM
      !IS_GOHEAD type GOHEAD
    exporting
      !ET_RETURN type TY_T_BAPIRET2 .
protected section.
private section.

  class-data GRT_BWART type FIP_T_BWART_RANGE .
  class-data GRT_WERKS type RANGE_T_WERKS .
  class-data GRT_MTART type MD_RANGE_T_MTART .
  class-data GC_REPID type PROGRAMM value 'ZCL_IM_SDS_MB_MIGO_ITEM' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_IM_SDS_MB_MIGO_ITEM IMPLEMENTATION.


METHOD IF_EX_MB_MIGO_ITEM_BADI~ITEM_MODIFY.
*-----------------------------------------------------------------------
*  Program ID         : ZMME013
*  Creation Date      : 03.05.2024
*  Author             : Boontip
*  Add-on ID          : N/A
*  Description        : badi to validate in item of MIGO
*  Purpose            : to validate free stock
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  CLEAR: E_STGE_LOC,
         E_ITEM_TEXT,
         ET_RETURN.
  CALL METHOD ME->VALIDATE_FREE_STOCK
    EXPORTING
      IS_GOITEM = IS_GOITEM
      IS_GOHEAD = IS_GOHEAD
    IMPORTING
      ET_RETURN = ET_RETURN.

*<-- Start of Insertion CME018 14.08.2024
* SDS: Assign item text from unloading point
  ZCL_SDSCM_ENHANCEMENT=>MB_MIGO_ITEM_BADI_ITEM_MODIFY(
    EXPORTING
      IS_GOITEM    = IS_GOITEM
      IS_GOHEAD    = IS_GOHEAD
    CHANGING
      CF_STGE_LOC  = E_STGE_LOC
      CF_ITEM_TEXT = E_ITEM_TEXT
      CT_RETURN    = ET_RETURN ).
*--> End of Insertion CME018 14.08.2024

ENDMETHOD.


METHOD VALIDATE_FREE_STOCK.
*-----------------------------------------------------------------------
*  Program ID         : ZMME013
*  Creation Date      : 03.05.2024
*  Author             : Boontip
*  Add-on ID          : N/A
*  Description        : call in  ITEM_MODIFY method for validation
*  Purpose            : to validate free stock
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA:LT_INPUT           TYPE TABLE OF BAPIWMDVS,
       LT_OUTPUT          TYPE TABLE OF BAPIWMDVE,
       LF_COM_QTY         TYPE SY-MSGV1,
       LF_MATERIAL        TYPE MATNR18,
       LS_BAPIRET2        TYPE BAPIRET2,
       LV_RESV_AMT_ITSELF TYPE RESB-VMENG.
  CLEAR ET_RETURN.

  CHECK IS_GOITEM-XLOEK = ABAP_FALSE
  AND   IS_GOITEM-ERFMG IS NOT INITIAL.

  IF GRT_BWART IS INITIAL.
    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                    IF_PARAM = 'BWART'
                                          IMPORTING ET_RANGE = GRT_BWART ).
  ENDIF.
  IF GRT_WERKS IS INITIAL.
    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                    IF_PARAM = 'WERKS'
                                          IMPORTING ET_RANGE = GRT_WERKS ).
  ENDIF.
  IF GRT_MTART IS INITIAL.
    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                    IF_PARAM = 'MTART'
                                          IMPORTING ET_RANGE = GRT_MTART ).
  ENDIF.

  SELECT SINGLE MTART
  INTO @DATA(LF_MTART)
  FROM MARA
  WHERE MATNR = @IS_GOITEM-MATNR.

  IF GRT_BWART[] IS NOT INITIAL AND IS_GOITEM-BWART IN GRT_BWART
  AND GRT_WERKS[] IS NOT INITIAL AND IS_GOITEM-WERKS IN GRT_WERKS
  AND GRT_MTART[] IS NOT INITIAL AND LF_MTART IN GRT_MTART.
    IF IS_GOITEM-RSNUM IS NOT INITIAL
    AND IS_GOITEM-RSPOS IS NOT INITIAL.
      SELECT SINGLE
              VMENG, "availbility stock
              ENMNG  "Quantity withdrawn
      INTO @DATA(LS_RESB)
      FROM RESB
      WHERE RSNUM = @IS_GOITEM-RSNUM
      AND RSPOS = @IS_GOITEM-RSPOS
      AND RSART = ''.
      IF LS_RESB-VMENG > 0
      AND LS_RESB-VMENG > LS_RESB-ENMNG .
        LV_RESV_AMT_ITSELF = LS_RESB-VMENG - LS_RESB-ENMNG .
      ENDIF.
    ENDIF.
    LF_MATERIAL = IS_GOITEM-MATNR.
    CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
      EXPORTING
        PLANT      = IS_GOITEM-WERKS
        MATERIAL   = LF_MATERIAL
        UNIT       = IS_GOITEM-ERFME
        CHECK_RULE = '03'
        STGE_LOC   = IS_GOITEM-LGORT
      TABLES
        WMDVSX     = LT_INPUT
        WMDVEX     = LT_OUTPUT.
    READ TABLE LT_OUTPUT INTO DATA(LS_OUTPUT) INDEX 1.
    IF SY-SUBRC = 0.
      IF IS_GOITEM-ERFMG > LS_OUTPUT-COM_QTY + LV_RESV_AMT_ITSELF.
        WRITE LS_OUTPUT-COM_QTY UNIT IS_GOITEM-ERFME TO LF_COM_QTY LEFT-JUSTIFIED.
        CALL FUNCTION 'BALW_BAPIRETURN_GET2'
          EXPORTING
            TYPE   = 'E'
            CL     = 'ZSDSMM01'
            NUMBER = '002'
            PAR1   = LF_COM_QTY
          IMPORTING
            RETURN = LS_BAPIRET2.
        APPEND LS_BAPIRET2 TO ET_RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
