*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0330_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module TC_INPUT_MODIFY INPUT
*&---------------------------------------------------------------------*
*&SPWIZARD: INPUT MODULE FOR TC 'TC_INPUT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TC_INPUT_MODIFY INPUT.
  READ TABLE GT_INPUT TRANSPORTING NO FIELDS INDEX TC_INPUT-CURRENT_LINE.
  IF SY-SUBRC = 0.
    MODIFY GT_INPUT
      FROM ZSDSSDS102
      INDEX TC_INPUT-CURRENT_LINE.
  ELSE.
    IF ZSDSSDS102 IS NOT INITIAL.
      APPEND ZSDSSDS102 TO GT_INPUT.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module TC_INPUT_USER_COMMAND INPUT
*&---------------------------------------------------------------------*
*&SPWIZARD: INPUT MODULE FOR TC 'TC_INPUT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TC_INPUT_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TC_INPUT'
                              'GT_INPUT'
                              'MARK'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.
  GF_SAVE_OK = GF_OKCODE.
  CLEAR GF_OKCODE.

  CASE GF_SAVE_OK.
    WHEN 'ZSEND'.
      DELETE GT_INPUT WHERE VBELN IS INITIAL
                      AND   POSNR IS INITIAL
                      AND   ETENR IS INITIAL.
      IF GT_INPUT[] IS INITIAL.
        "Please input value
        MESSAGE E027(ZSDSSD01).
      ENDIF.
      PERFORM F_SEND_INTERFACE  CHANGING GT_INPUT
                                         GT_REP.
      IF GT_REP[] IS INITIAL.
        "No data found.
        MESSAGE S003(ZSDSSD01).
        LEAVE TO SCREEN 0.
      ELSE.
        PERFORM F_DISPLAY_REPORT USING GT_REP.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMANDS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_COMMANDS INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_DATA  INPUT
*&---------------------------------------------------------------------*
MODULE VALIDATE_DATA INPUT.
  PERFORM F_VALIDATE_INPUT_DATA USING ZSDSSDS102.
ENDMODULE.
