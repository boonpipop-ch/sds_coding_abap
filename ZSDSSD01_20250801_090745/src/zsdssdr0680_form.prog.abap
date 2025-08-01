*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0680_FORM
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
    WHEN 'SAVE'.
      LCL_DATA=>SAVE( ).
    WHEN 'PRINT'.
      LCL_DATA=>PRINT( ).
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
    LCL_DATA=>UPDATE_DATA( ).
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
*& Form F_PRINT_ZIV5
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PRINT_ZIV5 .
  DATA : LV_RETCODE     TYPE SY-SUBRC,
         LS_NAST        TYPE NAST,
         LV_FORM_NAME   TYPE TNAPR-SFORM,
         LV_PROC_SCREEN TYPE C,
         LV_NO_DIALOG   TYPE C,
         LV_MHA         TYPE C.
  DATA : LV_MEMORY TYPE C LENGTH 255.

  DATA : P_ETAX TYPE CHAR1.


  CONCATENATE SY-UNAME '_' SY-DATUM INTO LV_MEMORY.
  FREE MEMORY ID LV_MEMORY.

  EXPORT P_ETAX TO MEMORY ID LV_MEMORY.


*--------------------------------------------------------------------*
  LV_NO_DIALOG       = ABAP_TRUE.
*--------------------------------------------------------------------*
  LS_NAST-KAPPL      = 'V3'.
  LS_NAST-OBJKY      = GS_RESULT-INVNO.
  LS_NAST-KSCHL      = 'ZIV5'.
  LS_NAST-SPRAS      = '2'.
  LS_NAST-PARNR      = GS_RESULT-KUNNR.
  LS_NAST-PARVW      = 'RE'.
  LS_NAST-ERDAT      = SY-DATUM.
  LS_NAST-ERUHR      = SY-UZEIT.
  LS_NAST-ADRNR      = GS_RESULT-ADRNR.
  LS_NAST-NACHA      = '1'.
  LS_NAST-ANZAL      = '1'.
  LS_NAST-VSZTP      = '3'.
  LS_NAST-MANUE      = 'X'.
  LS_NAST-DATVR      = SY-DATUM.
  LS_NAST-UHRVR      = SY-UZEIT.
  LS_NAST-USNAM      = SY-UNAME.
  LS_NAST-VSTAT      = '1'.
  LS_NAST-LDEST      = 'SCAN'.
  LS_NAST-DIMME      = SPACE.
  LS_NAST-DELET      = 'X'.
  LS_NAST-SNDBC      = '1'.
  LS_NAST-NAUTO      = 'X'.
  LS_NAST-TDRECEIVER = SY-UNAME.
  LS_NAST-TDARMOD    = '1'.
  LS_NAST-OBJTYPE    = 'VBRK'.
*--------------------------------------------------------------------*
  LV_FORM_NAME       = 'ZSDSSD012'.
*--------------------------------------------------------------------*

  PERFORM F_PRINT(ZSDSSDR0420) USING LV_RETCODE
                                     LS_NAST
                                     LV_FORM_NAME
                                     LV_PROC_SCREEN
                                     LV_NO_DIALOG
                                     LV_MHA.

  CONCATENATE SY-UNAME '_' SY-DATUM INTO LV_MEMORY.
  FREE MEMORY ID LV_MEMORY.

  IF LV_RETCODE EQ 0.
    UPDATE ZSDSSDT025 SET PETAX  = ABAP_TRUE
                    WHERE INVNO EQ GS_RESULT-INVNO.
    COMMIT WORK AND WAIT.
  ENDIF.

  MESSAGE S000 WITH TEXT-101.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PRINT_ZIV5
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PRINT_ZIR1.
  DATA : LV_RETCODE     TYPE SY-SUBRC,
         LS_NAST        TYPE NAST,
         LV_FORM_NAME   TYPE TNAPR-SFORM,
         LV_PROC_SCREEN TYPE C,
         LV_NO_DIALOG   TYPE C.
  DATA : LV_MEMORY TYPE C LENGTH 255.

  DATA : P_ETAX TYPE CHAR1.


  CONCATENATE SY-UNAME '_' SY-DATUM INTO LV_MEMORY.
  FREE MEMORY ID LV_MEMORY.

  EXPORT P_ETAX TO MEMORY ID LV_MEMORY.

*--------------------------------------------------------------------*
  LV_NO_DIALOG       = ABAP_TRUE.
*--------------------------------------------------------------------*
  LS_NAST-KAPPL      = 'V3'.
  LS_NAST-OBJKY      = GS_RESULT-INVNO.
  LS_NAST-KSCHL      = 'ZIR1'.
  LS_NAST-SPRAS      = '2'.
  LS_NAST-PARNR      = GS_RESULT-KUNNR.
  LS_NAST-PARVW      = 'RE'.
  LS_NAST-ERDAT      = SY-DATUM.
  LS_NAST-ERUHR      = SY-UZEIT.
  LS_NAST-ADRNR      = GS_RESULT-ADRNR.
  LS_NAST-NACHA      = '1'.
  LS_NAST-ANZAL      = '1'.
  LS_NAST-VSZTP      = '3'.
  LS_NAST-MANUE      = 'X'.
  LS_NAST-DATVR      = SY-DATUM.
  LS_NAST-UHRVR      = SY-UZEIT.
  LS_NAST-USNAM      = SY-UNAME.
  LS_NAST-VSTAT      = '1'.
  LS_NAST-LDEST      = 'SCAN'.
  LS_NAST-DIMME      = SPACE.
  LS_NAST-DELET      = 'X'.
  LS_NAST-SNDBC      = '1'.
  LS_NAST-NAUTO      = 'X'.
  LS_NAST-TDRECEIVER = SY-UNAME.
  LS_NAST-TDARMOD    = '1'.
  LS_NAST-OBJTYPE    = 'VBRK'.
*--------------------------------------------------------------------*
  LV_FORM_NAME       = 'ZSDSFI003'.
*--------------------------------------------------------------------*

  PERFORM F_PRINT(ZSDSSDR0040) USING LV_PROC_SCREEN
                                     LV_RETCODE
                                     LS_NAST
                                     SPACE
                                     LV_FORM_NAME
                                     LV_NO_DIALOG.

  CONCATENATE SY-UNAME '_' SY-DATUM INTO LV_MEMORY.
  FREE MEMORY ID LV_MEMORY.

  MESSAGE S000 WITH TEXT-101.

ENDFORM.
