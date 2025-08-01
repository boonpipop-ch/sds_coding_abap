*&---------------------------------------------------------------------*
*& Include          ZSDSFII0050_MODULE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.
  IF GV_MODE = GC_MODE-REPOTU.
    LEAVE PROGRAM.
  ENDIF.
  CASE SY-UCOMM.
    WHEN GC_BACK.
      LCL_DATA=>CLEAR_ALL( ).
      LEAVE TO SCREEN 0.
    WHEN GC_EXIT.
      LCL_DATA=>CLEAR_ALL( ).
      LEAVE TO SCREEN 0.
    WHEN GC_CANC.
      LCL_DATA=>CLEAR_ALL( ).
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  CASE SY-UCOMM.
    WHEN GC_GEN.
      LCL_DATA=>TRANSFER_DATA_TO_ALV( ).
    WHEN GC_SAVE.
      LCL_DATA=>SAVE( ).
    WHEN GC_ATTCH.
      LCL_DATA=>ATTACH_FILE( ).
    WHEN GC_DELETE.
      LCL_DATA=>DELETE_FILE( ).
    WHEN GC_ADD_AMT.
      LCL_DATA=>ADD_AMT( ).
    WHEN GC_INS.
      LCL_DATA=>SET_RADIO( ).
    WHEN GC_FI_START.
      LCL_DATA=>POST_START( ).
    WHEN GC_FI_END.
      LCL_DATA=>POST_END( ).
    WHEN GC_SAMPLE.
      LCL_DATA=>SAMPLE( ).
    WHEN GC_MIGRATION.
      LCL_DATA=>MIGRATION( ).
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  LCL_DATA=>SET_HEADER_STATUS_SCREEN( ).
ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*& Module M_SET_DATA OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE M_SET_SCREEN OUTPUT.
  LCL_DATA=>SET_SCREEN( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_GET_CURSOR INPUT.
  GET CURSOR FIELD GV_CURSOR.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module M_SET_CURSOR OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE M_SET_CURSOR OUTPUT.
  SET CURSOR FIELD GV_CURSOR.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_LEASE_TERM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_LEASE_TERM INPUT.
  DATA : LV_LEAST LIKE GS_HEADER-LEAST,
         LV_TMP   LIKE GS_HEADER-LEAST.
  CLEAR : LV_LEAST.
  IF GT_AMT IS NOT INITIAL.
    LOOP AT GT_AMT INTO GS_AMT.
      LV_TMP = LCL_DATA=>GET_MONTH_FROM_2_DATE( EXPORTING I_DATE_START = GS_AMT-STDDT
                                                            I_DATE_END   = GS_AMT-ENDDT ).
      ADD LV_TMP TO LV_LEAST.
    ENDLOOP.

    GS_HEADER-LEAST = LV_LEAST.
  ELSE.
    GS_HEADER-LEAST = LCL_DATA=>GET_MONTH_FROM_2_DATE( EXPORTING I_DATE_START = GS_HEADER-STADT
                                                                 I_DATE_END   = GS_HEADER-ENDDT ).
  ENDIF.

  IF GS_HEADER-PAYMP NE 0.
    GS_HEADER-TIMES = GS_HEADER-LEAST / GS_HEADER-PAYMP.
  ENDIF.

  GS_HEADER-TODAY = LCL_DATA=>GET_DAYS_FROM_2_DATE( EXPORTING I_DATE_START = GS_HEADER-STADT
                                                              I_DATE_END   = GS_HEADER-ENDDT ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_CURRENCY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_CURRENCY INPUT.
  GS_HEADER-TEXT_CUR_COST = GS_HEADER-WAERS.
  GS_HEADER-TEXT_CUR_REST = GS_HEADER-WAERS.
  GS_HEADER-TEXT_CUR_RENT = GS_HEADER-WAERS.
  GS_HEADER-TEXT_CUR_DEPO = GS_HEADER-WAERS.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_TIMES  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_TIMES INPUT.
  IF GS_HEADER-PAYMP NE 0.
    GS_HEADER-TIMES = GS_HEADER-LEAST / GS_HEADER-PAYMP.
  ELSE.
    GS_HEADER-TIMES = GS_HEADER-LEAST.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_COSEV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_COSEV INPUT.

  IF R_PV EQ ABAP_TRUE.
    LCL_DATA=>PV( ).
  ELSE.
    LCL_DATA=>NPV( ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_SET_VAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_SET_VAT INPUT.
  IF GS_HEADER-RENEV IS NOT INITIAL.
    GS_HEADER-VATMT = ( ( GS_HEADER-RENEV * GS_HEADER-TIMES ) * 7 ) / 100.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0102 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0102 OUTPUT.
  SET PF-STATUS 'ZPF_STATUS_102'.
  IF     GV_MODE EQ GC_MODE-UPDATE.
    SET TITLEBAR 'Z02'.
  ELSEIF GV_MODE EQ GC_MODE-DELETE.
    SET TITLEBAR 'Z04'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102 INPUT.
  CASE SY-UCOMM.
    WHEN GC_EXE.
      LCL_DATA=>GET_DATA_FOR_UPDATE( ).
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102_EXIT INPUT.
  CASE SY-UCOMM.
    WHEN GC_BACK.
      LCL_DATA=>CLEAR_ALL( ).
      LEAVE TO SCREEN 0.
    WHEN GC_EXIT.
      LCL_DATA=>CLEAR_ALL( ).
      LEAVE TO SCREEN 0.
    WHEN GC_CANC.
      LCL_DATA=>CLEAR_ALL( ).
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0103 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0103 OUTPUT.
  SET PF-STATUS 'ZPF_STATUS_103'.
  SET TITLEBAR 'Z03'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0103 INPUT.
  CASE SY-UCOMM.
    WHEN GC_BT_103-NEW.
      GV_MODE = GC_MODE-CREATE.
      LCL_DATA=>START_PROCESS( ).
    WHEN GC_BT_103-UPDATE.
      GV_MODE = GC_MODE-UPDATE.
      LCL_DATA=>START_PROCESS( ).
    WHEN GC_BT_103-DELETE.
      GV_MODE = GC_MODE-DELETE.
      LCL_DATA=>START_PROCESS( ).
    WHEN GC_BT_103-REPORT.
      GV_MODE = GC_MODE-REPORT.
      LCL_DATA=>START_PROCESS( ).
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0103_EXIT INPUT.
  CASE SY-UCOMM.
    WHEN GC_BACK.
      LEAVE PROGRAM.
    WHEN GC_EXIT.
      LEAVE PROGRAM.
    WHEN GC_CANC.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_VENDOR_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_VENDOR_NAME INPUT.
  LCL_DATA=>GET_VENDOR_NAME( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module M_GEN_CONTAINER OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE M_GEN_CONTAINER OUTPUT.
  IF GCL_ALV IS INITIAL.
    LCL_DATA=>CREATE_OBJECT_ALV( ).
    LCL_DATA=>EXCLUDING_TOOLBAR( ).
    LCL_DATA=>ADD_EVENT( ).
    LCL_DATA=>GUI_STATUS( ).
    LCL_DATA=>SET_LAYOUT_OO( ).
    LCL_DATA=>SET_FCAT_OO( ).
    LCL_DATA=>CALL_ALV( ).
  ENDIF.

  IF GCL_ALV_AMT IS INITIAL.
    LCL_DATA=>CREATE_OBJECT_ALV_AMT( ).
    LCL_DATA=>EXCLUDING_TOOLBAR_AMT( ).
*    LCL_DATA=>ADD_EVENT( ).
    LCL_DATA=>GUI_STATUS_AMT( ).
    LCL_DATA=>SET_LAYOUT_OO( ).
    LCL_DATA=>SET_FCAT_OO_AMT( ).
    LCL_DATA=>CALL_ALV_AMT( ).
  ENDIF.

  IF GCL_EDITOR IS INITIAL.
    LCL_DATA=>CREATE_OBJECT_REMARK( ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_BEFOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_USER_COMMAND_BEFOR INPUT.
  CASE SY-UCOMM.
    WHEN GC_ADD_AMT.
      LCL_DATA=>ADD_AMT( ).
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0104 INPUT.

  DATA : LV_MESSAGE TYPE C LENGTH 255.

  CASE SY-UCOMM.
    WHEN GC_POST_ST.
      LV_MESSAGE = LCL_DATA=>POST_ASSET_ACQUISITION( ).
*      LV_MESSAGE = LCL_DATA=>POST_ST_ASSET( ).
      IF LV_MESSAGE IS INITIAL.
*      LCL_DATA=>POST_ST( ).
        LCL_DATA=>POST_ST_NO_ASSET( ).
      ENDIF.
    WHEN GC_CANCEL_POST.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0104 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0104 OUTPUT.
  SET PF-STATUS 'ZPF_STATUS_104'.
  SET TITLEBAR 'Z05'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0104_EXIT INPUT.
  CASE SY-UCOMM.
    WHEN GC_BACK.
      LEAVE TO SCREEN 0.
    WHEN GC_EXIT.
      LEAVE TO SCREEN 0.
    WHEN GC_CANC.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_ACC_DESC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ACC_DESC INPUT.
  IF GS_FI-DEBIT IS NOT INITIAL.
    SELECT SINGLE TXT20
      FROM SKAT
      INTO GS_FI-DEBIT_DESC
      WHERE SPRAS EQ SY-LANGU
        AND KTOPL EQ GC_CHART_ACC
        AND SAKNR EQ GS_FI-DEBIT.
  ENDIF.

  IF GS_FI-CREDIT IS NOT INITIAL.
    SELECT SINGLE TXT20
      FROM SKAT
      INTO GS_FI-CREDIT_DESC
      WHERE SPRAS EQ SY-LANGU
        AND KTOPL EQ GC_CHART_ACC
        AND SAKNR EQ GS_FI-CREDIT.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_ASSET_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ASSET_NAME INPUT.

  IF GS_HEADER-ANLN2 IS INITIAL.
    GS_HEADER-ANLN2 = GC_DEFAULT_SUB.
  ENDIF.

  SELECT SINGLE TXT50
    FROM ANLA
    INTO GS_HEADER-ASSNA
    WHERE BUKRS EQ GC_COM_CODE
      AND ANLN1 EQ GS_HEADER-ANLN1
      AND ANLN2 EQ GS_HEADER-ANLN2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_ASSET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_ASSET INPUT.

  IF GS_HEADER-ANLN2 IS INITIAL.
    GS_HEADER-ANLN2 = GC_DEFAULT_SUB.
  ENDIF.

  SELECT COUNT( * )
    FROM ZSDSFIT002
    WHERE ANLN1 EQ GS_HEADER-ANLN1
      AND ANLN2 EQ GS_HEADER-ANLN2.
  IF SY-SUBRC EQ 0.
    GV_ASSET = LCL_UTIL=>SHOW_MESSAGE_ERROR( TEXT-E06 ).
  ELSE.
    CLEAR : GV_ASSET.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_VALUE INPUT.
  IF GV_POST_DATE_ST IS INITIAL.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_COST_CENTER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_COST_CENTER INPUT.
  LCL_DATA=>GET_COST_CENTER( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_WBS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CHECK_WBS INPUT.
*  SELECT COUNT( * )
*    FROM PRPS
*    WHERE POSID EQ GS_HEADER-BUDGT.
*  IF SY-SUBRC NE 0.
*    CLEAR : GS_HEADER-BUDGT.
*    LCL_UTIL=>SHOW_MESSAGE_ERROR( TEXT-E31 ).
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0105 INPUT.
  CASE SY-UCOMM.
    WHEN GC_POST_ST.
      LCL_UTIL=>UPDATE_ZSDSFIT022( I_DOC_NO   = 'SKIP'
                                   I_COM_CODE = '1000'
                                   I_YEAR     = SY-DATUM+0(4)
                                   I_TYPE     = GC_ASSET_POST ).
      GS_HEADER-FIACO = 'SKIP'.

      LCL_UTIL=>UPDATE_ZSDSFIT022( I_DOC_NO   = 'SKIP'
                                   I_COM_CODE = '1000'
                                   I_YEAR     = SY-DATUM+0(4)
                                   I_TYPE     = GC_START ).
      GS_HEADER-FISCO = 'SKIP'.

      LCL_DATA=>UPDATE_ZSDSFIT004( ).
      MESSAGE S000 WITH TEXT-S01.
      LEAVE TO SCREEN 0.
    WHEN GC_CANCEL_POST.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
