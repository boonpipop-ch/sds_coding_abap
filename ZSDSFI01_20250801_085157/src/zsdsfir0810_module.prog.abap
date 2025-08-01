*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0810_MODULE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.

  CASE  SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  IF GCL_ALV IS BOUND.
    LCL_DATA=>UPDATE_CHECK( ).
  ENDIF.

  IF GCL_ALV2 IS BOUND.
    LCL_DATA=>UPDATE_CHECK2( ).
  ENDIF.

  CASE  SY-UCOMM.
    WHEN 'ADD_DEBIT'.
      DATA(LV_CHECK) = LCL_DATA=>VALIDATON_DETAIL( ).
      IF LV_CHECK IS INITIAL.
        LCL_DATA=>ADD_DEBIT( ).
      ENDIF.
    WHEN 'ADD_CREDIT'.
      LV_CHECK = LCL_DATA=>VALIDATON_DETAIL( ).
      IF LV_CHECK IS INITIAL.
        LCL_DATA=>ADD_CREDIT( ).
      ENDIF.
    WHEN 'DEL_LINE'.
      LCL_DATA=>DEL_LINE( ).
    WHEN 'DEL_HEAD'.
      LCL_DATA=>DEL_HEAD( ).
    WHEN 'DEL_DETAIL'.
      LCL_DATA=>DEL_DETAIL( ).
    WHEN 'SAVE'.
      LV_CHECK = LCL_DATA=>VALIDATON( ).
      IF LV_CHECK IS INITIAL.
        LCL_DATA=>SAVE( ).
      ENDIF.
    WHEN 'HED'.
      LCL_DATA=>HIDE_SHOW_HEAD( ).
    WHEN 'DET'.
      LCL_DATA=>HIDE_SHOW_DETIAL( ).
    WHEN 'BT_DALL'.
      LCL_DATA=>DELETE_ALL_ITEM( ).
  ENDCASE.

  IF GCL_ALV IS BOUND.
    GCL_ALV->REFRESH_TABLE_DISPLAY( ).
  ENDIF.

  IF GCL_ALV2 IS BOUND.
    GCL_ALV2->REFRESH_TABLE_DISPLAY( ).
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  SET PF-STATUS 'Z101'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*& Module CALL_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CALL_ALV OUTPUT.
  IF GCL_ALV IS INITIAL.
    LCL_DATA=>CREATE_OBJECT_ALV( ).
    LCL_DATA=>EXCLUDING_TOOLBAR( ).
    LCL_DATA=>ADD_EVENT( ).
    LCL_DATA=>GUI_STATUS( ).
    LCL_DATA=>SET_LAYOUT_OO( ).
    LCL_DATA=>SET_SORT_OO( ).
    LCL_DATA=>SET_FCAT_OO( ).
    LCL_DATA=>CALL_ALV( ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CALL_ALV2 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CALL_ALV2 OUTPUT.
  IF GCL_ALV2 IS INITIAL.
    LCL_DATA=>CREATE_OBJECT_ALV2( ).
    LCL_DATA=>EXCLUDING_TOOLBAR2( ).
    LCL_DATA=>ADD_EVENT2( ).
    LCL_DATA=>GUI_STATUS2( ).
    LCL_DATA=>SET_LAYOUT2_OO( ).
    LCL_DATA=>SET_SORT2_OO( ).
    LCL_DATA=>SET_FCAT2_OO( ).
    LCL_DATA=>CALL_ALV2( ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_CALL_DES  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CALL_DES INPUT.

  DATA: LS_DES TYPE GY_RESULT.

  SELECT SINGLE MAKTX
     INTO LS_DES-SGTXT
     FROM MAKT
     WHERE MATNR = GS_RESULT-MATNR
       AND SPRAS = SY-LANGU.

  IF SY-SUBRC = 0.
    GS_RESULT-SGTXT = LS_DES-SGTXT.
  ELSE.
    CLEAR GS_RESULT-SGTXT.

  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_CALL_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CALL_NAME INPUT.

  DATA: LV_NAME TYPE CHAR255,
        LS_LFA1 TYPE LFA1.


  SELECT SINGLE LIFNR NAME1 NAME2 NAME3 NAME4
    INTO (LS_LFA1-LIFNR, LS_LFA1-NAME1, LS_LFA1-NAME2, LS_LFA1-NAME3, LS_LFA1-NAME4)
    FROM LFA1
    WHERE LIFNR = GS_RESULT-VENDOR.

  IF SY-SUBRC = 0.
    CONCATENATE LS_LFA1-NAME1 LS_LFA1-NAME2 LS_LFA1-NAME3 LS_LFA1-NAME4 INTO LV_NAME SEPARATED BY SPACE.
    GS_RESULT-NAME = LV_NAME.
  ELSE.
    CLEAR GS_RESULT-NAME.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_POST_ACC_DOC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_POST_ACC_DOC INPUT.




ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_CALL_VENDERNAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CALL_VENDERNAME INPUT.

  DATA: LV_NAME2 TYPE CHAR255,
        LS_LFA2  TYPE LFA1.


  SELECT SINGLE LIFNR NAME1 NAME2 NAME3 NAME4
    INTO (LS_LFA2-LIFNR, LS_LFA2-NAME1, LS_LFA2-NAME2, LS_LFA2-NAME3, LS_LFA2-NAME4)
    FROM LFA1
    WHERE LIFNR = GS_HEADER-SALES_VENDC.

  IF SY-SUBRC = 0.
    CONCATENATE LS_LFA2-NAME1 LS_LFA2-NAME2 LS_LFA2-NAME3 LS_LFA2-NAME4 INTO LV_NAME2 SEPARATED BY SPACE.
    GS_HEADER-SALES_VENDN = LV_NAME2.
  ELSE.
    CLEAR GS_HEADER-SALES_VENDN.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CONTROL_OT01_FIELDS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

MODULE CONTROL_OT01_FIELDS OUTPUT.
  DATA: LT_FIELDS TYPE STANDARD TABLE OF STRING,
        LV_FIELD  TYPE STRING.

  APPEND 'GS_RESULT-OT01_NAME1'     TO LT_FIELDS.
  APPEND 'GS_RESULT-OT01_NAME2'     TO LT_FIELDS.
  APPEND 'GS_RESULT-OT01_NAME3'     TO LT_FIELDS.
  APPEND 'GS_RESULT-OT01_NAME4'     TO LT_FIELDS.
  APPEND 'GS_RESULT-ADDR_NUM'       TO LT_FIELDS.
  APPEND 'GS_RESULT-DISTRIC'        TO LT_FIELDS.
  APPEND 'GS_RESULT-SUB_DISTRIC'    TO LT_FIELDS.
  APPEND 'GS_RESULT-POSTCODE'       TO LT_FIELDS.
  APPEND 'GS_RESULT-TAXID'          TO LT_FIELDS.
  APPEND 'GS_RESULT-PHONE'          TO LT_FIELDS.
  APPEND 'GS_RESULT-EMAIL'          TO LT_FIELDS.
  APPEND 'GS_RESULT-CITY'           TO LT_FIELDS.
  APPEND 'GS_RESULT-COUNTRY'        TO LT_FIELDS.

  LOOP AT SCREEN.
    READ TABLE LT_FIELDS INTO LV_FIELD WITH KEY TABLE_LINE = SCREEN-NAME.
    IF SY-SUBRC = 0.
      IF GS_RESULT-VENDOR = 'OT01'.
        SCREEN-INPUT = 1.
        IF GS_RESULT-COUNTRY IS INITIAL.
          GS_RESULT-COUNTRY = 'TH'.
        ENDIF.
        IF GS_RESULT-CITY IS INITIAL.
          GS_RESULT-CITY = '-'.
        ENDIF.
      ELSE.
        SCREEN-INPUT = 0.
        CLEAR : GS_RESULT-COUNTRY.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-NAME EQ 'GS_RESULT-TAX_AMT'.
      IF GS_RESULT-VENDOR IS INITIAL.
        SCREEN-INPUT = 1.
      ELSE.
        SCREEN-INPUT = 0.
*        CLEAR : GS_RESULT-TAXCODE,GS_RESULT-TAX_AMT.
        CLEAR : GS_RESULT-TAX_AMT.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-NAME EQ 'GS_RESULT-RACCT'.
      IF GS_RESULT-VENDOR IS INITIAL.
        SCREEN-INPUT = 1.
      ELSE.
        SCREEN-INPUT = 0.
        CLEAR : GS_RESULT-RACCT.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'HED'.
      IF GV_HEAD = 'SHOW'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'DET'.
      IF GV_DETIAL = 'SHOW'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-NAME EQ 'GS_HEADER-CLRDB'.
      IF CB_ADVANCE EQ ABAP_TRUE.
        GS_HEADER-CLRDB = 'ADVANCE'.
        SCREEN-ACTIVE = 0.
      ELSE.
        IF GS_HEADER-CLRDB = 'ADVANCE'.
          CLEAR : GS_HEADER-CLRDB.
        ENDIF.
        SCREEN-ACTIVE = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102 INPUT.

  IF GCL_ALV IS BOUND.
    LCL_DATA=>UPDATE_CHECK( ).
  ENDIF.

  CASE  SY-UCOMM.
    WHEN 'UPDATE'.
      LV_CHECK = LCL_DATA=>VALIDATON_DETAIL( ).
      IF LV_CHECK IS INITIAL.
        LCL_DATA=>UPDATE( ).
      ENDIF.
  ENDCASE.
  IF GCL_ALV IS BOUND.
    GCL_ALV->REFRESH_TABLE_DISPLAY( ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102_EXIT INPUT.
  CASE  SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0102 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0102 OUTPUT.
  SET PF-STATUS 'Z102'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0103 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0103 OUTPUT.
  SET PF-STATUS 'Z103'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0103_EXIT INPUT.
  CASE  SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0103 INPUT.

  IF GCL_ALV2 IS BOUND.
    LCL_DATA=>UPDATE_CHECK2( ).
  ENDIF.

  CASE  SY-UCOMM.
    WHEN 'UPDATE'.
      LCL_DATA=>UPDATE2( ).
  ENDCASE.

  IF GCL_ALV2 IS BOUND.
    GCL_ALV2->REFRESH_TABLE_DISPLAY( ).
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_CURSOR OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_CURSOR OUTPUT.
  SET CURSOR FIELD GV_CURSOR.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_SALES  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SALES INPUT.

  SELECT SINGLE BSIK~LIFNR
      FROM BSIK_VIEW AS BSIK
      INNER JOIN BKPF ON BSIK~BELNR EQ BKPF~BELNR AND
                         BKPF~BLART EQ 'K1'
      WHERE BSIK~BELNR EQ @GS_HEADER-DOC_ADVAN
        AND BSIK~GJAHR EQ @GS_HEADER-DOC_ADVAN_Y
        AND BSIK~BSCHL EQ '29'
       INTO @GS_HEADER-SALES_VENDC.
  IF SY-SUBRC NE 0.
    CLEAR : GS_HEADER-DOC_ADVAN,GS_HEADER-DOC_ADVAN_Y.
    MESSAGE S998 WITH TEXT-106 DISPLAY LIKE 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module HOLD_DATA OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE HOLD_DATA OUTPUT.
  SET HOLD DATA ON.
ENDMODULE.
