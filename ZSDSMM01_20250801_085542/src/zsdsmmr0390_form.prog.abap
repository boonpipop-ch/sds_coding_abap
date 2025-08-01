*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0390_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_INIT_DATA .
*  IF go_data IS INITIAL.
*    CREATE OBJECT go_data.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_EXTAB   text
*----------------------------------------------------------------------*
FORM PF_STATUS_1 USING US_EXTAB TYPE SLIS_T_EXTAB.

  CONSTANTS LC_STATUS TYPE C LENGTH 9 VALUE 'ZSTANDARD'.

  SET PF-STATUS LC_STATUS EXCLUDING US_EXTAB.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                        P_SELFLD TYPE SLIS_SELFIELD.
*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
  ENDIF.
*&---------------------------------------------------------------------*

  CASE P_UCOMM.
    WHEN '&IC1'.
      PERFORM F_CALL_TRANSECTION.
    WHEN 'ALL'.
      PERFORM F_CHECK_BOX USING 'X'.
    WHEN 'SAL'.
      PERFORM F_CHECK_BOX USING SPACE.
  ENDCASE.

  CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
  CLEAR : REF_GRID.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_TRANSECTION.
  DATA: LE_ROW     TYPE I,
        LE_VALUE   TYPE C,
        LE_COL     TYPE I,
        LES_ROW_ID TYPE LVC_S_ROW,
        LES_COL_ID TYPE LVC_S_COL,
        LES_ROW_NO TYPE LVC_S_ROID.

  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.
  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
  ENDIF.

  CALL METHOD REF_GRID->GET_CURRENT_CELL
    IMPORTING
      E_ROW     = LE_ROW
      E_VALUE   = LE_VALUE
      E_COL     = LE_COL
      ES_ROW_ID = LES_ROW_ID
      ES_COL_ID = LES_COL_ID.

  CLEAR : BDCDATA[],MESSTAB[].

  READ TABLE GT_RESULT INTO GS_RESULT INDEX LES_ROW_ID-INDEX.
  IF SY-SUBRC = 0.

  ENDIF.

ENDFORM.                    " F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = GC_X.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION  USING    TCODE.
  DATA: L_MSTRING(480).
  DATA: L_SUBRC LIKE SY-SUBRC.
  DATA: LV_OPT  TYPE CTU_PARAMS.

  LV_OPT-DISMODE  = GC_E."'A'
  LV_OPT-UPDMODE  = GC_L.
  LV_OPT-NOBINPT  = GC_X.
  "lv_opt-cattmode = 'A'.
  "lv_opt-defsize  = 'X'.

  CALL TRANSACTION TCODE USING BDCDATA
*                   MODE   'E'"N
*                   UPDATE 'L'
                   OPTIONS FROM LV_OPT
                   MESSAGES INTO MESSTAB.

ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0645   text
*----------------------------------------------------------------------*
FORM F_CHECK_BOX  USING LV_CHECK.
  GS_RESULT-CHECK = LV_CHECK.
  MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING CHECK
                                         WHERE CHECK NE GS_RESULT-CHECK.

ENDFORM.                    " F_CHECK_BOX
*&---------------------------------------------------------------------*
*&      Form  F_GET_FORM_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0645   text
*----------------------------------------------------------------------*
FORM F_GET_FORM_INIT USING UV_DOC    TYPE EBAN-BANFN
                  CHANGING CT_DETAIL TYPE ZSDSMMS036_TT
                           CS_DATA   TYPE ZSDSMMS035
                           C_LOA     TYPE ZSDSCAS009.

  DATA LS_DETAIL LIKE LINE OF CT_DETAIL.

  DATA LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

  DATA : LV_AMOUNT TYPE P DECIMALS 2,
         LV_GRAND  TYPE P DECIMALS 2.

  DATA : LV_QTY TYPE I.

  DATA : LV_DATE_TEXT TYPE C LENGTH 10.

  IF LCL_UTIL IS NOT BOUND.
    CREATE OBJECT LCL_UTIL.
  ENDIF.

  C_LOA = LCL_UTIL->GET_LOA_PR( I_DOC_NO  = UV_DOC
                                I_RUNNING = ABAP_TRUE ).

  LOOP AT GT_RESULT INTO GS_RESULT WHERE BANFN = UV_DOC.
    CS_DATA-PR_NO               = GS_RESULT-BANFN.
    WRITE GS_RESULT-ERDAT TO LV_DATE_TEXT.
    CS_DATA-PR_CREATE_DATE = LV_DATE_TEXT.
    CS_DATA-COST_CENTER         = GS_RESULT-KOSTL.
    IF GS_RESULT-PS_PSP_PNR IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          INPUT  = GS_RESULT-PS_PSP_PNR
        IMPORTING
          OUTPUT = CS_DATA-PROJECT_NAME.
    ENDIF.
    CS_DATA-IO                  = GS_RESULT-AUFNR.

    LV_AMOUNT = GS_RESULT-PREIS * GS_RESULT-MENGE.
    ADD LV_AMOUNT TO LV_GRAND.

    LS_DETAIL-PR_NO        = GS_RESULT-BANFN.
    LS_DETAIL-PR_ITEM      = GS_RESULT-BNFPO.
    LS_DETAIL-MET_CODE     = GS_RESULT-MATNR.
    LS_DETAIL-GL           = GS_RESULT-SAKTO.
    LS_DETAIL-DESCRIPTION  = GS_RESULT-TXZ01.
    LV_QTY                 = GS_RESULT-MENGE.
    LS_DETAIL-QTY          = LV_QTY.
    LS_DETAIL-BASE_UNIT    = GS_RESULT-MSEHL.
    WRITE GS_RESULT-LFDAT TO LS_DETAIL-REQ_DATE.
    WRITE GS_RESULT-PREIS TO LS_DETAIL-PRICE.
    WRITE LV_AMOUNT       TO LS_DETAIL-TOTAL_AMOUNT.
    LS_DETAIL-CURRENCY     = GS_RESULT-WAERS.

    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-QTY          WITH SPACE.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-PRICE        WITH SPACE.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-TOTAL_AMOUNT WITH SPACE.

    APPEND LS_DETAIL TO CT_DETAIL.
  ENDLOOP.

  WRITE LV_GRAND TO CS_DATA-GRAND_TOTAL.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN CS_DATA-GRAND_TOTAL WITH SPACE.

ENDFORM.
