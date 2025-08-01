*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0420_FORM
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

  DATA: LV_DATE TYPE C LENGTH 10.

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

  WRITE SY-DATUM TO LV_DATE.

  READ TABLE GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>) INDEX LES_ROW_ID-INDEX.
  MOVE-CORRESPONDING <LFS_DATA> TO GS_RESULT.
  IF SY-SUBRC = 0.
    IF GS_RESULT-BANFN IS NOT INITIAL AND
       GS_RESULT-PRSTU NE 'COMPLEATED'.
      LCL_DATA=>CREATE_PO( ).
    ELSEIF GS_RESULT-BANFN IS INITIAL AND
           GS_RESULT-STATU EQ 'COM'.
      <LFS_DATA>-BANFN = LCL_DATA=>CREATE_PR( ).
      IF <LFS_DATA>-BANFN IS NOT INITIAL.
        <LFS_DATA>-STATU = 'COM'.
      ENDIF.
    ENDIF.
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

  LV_OPT-DISMODE  = GC_A."'A'
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
