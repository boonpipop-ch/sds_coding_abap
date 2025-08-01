*&SPWIZARD: OUTPUT MODULE FOR TC 'TABLE_CONTROL01'. DO NOT CHANGE THIS L
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TABLE_CONTROL01_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE ITAB_TABLE01 LINES TABLE_CONTROL01-LINES.
ENDMODULE.                    "TABLE_CONTROL01_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TABLE_CONTROL01'. DO NOT CHANGE THIS L
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TABLE_CONTROL01_INFO_MOVE OUTPUT.

  G_TABLE_CONTROL01_LINES = SY-LOOPC.

  MOVE-CORRESPONDING ITAB_TABLE01 TO TABLE_CONTROL01.

  LOOP AT SCREEN.

    CASE SCREEN-NAME.
      WHEN 'ITAB_TABLE01-I_QTY'.
        IF ITAB_TABLE01-DABMG > 1.
          SCREEN-INPUT     = 1.
          SCREEN-INVISIBLE = 0.
        ELSE.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 1.
          CLEAR: ITAB_TABLE01-I_QTY.
        ENDIF.
        IF NOT ITAB_TABLE01-MULTI IS INITIAL. "Multiple schedule
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 1.
          CLEAR: ITAB_TABLE01-I_QTY.
        ENDIF.
        IF NOT ITAB_TABLE01-LOEKZ IS INITIAL. "Deleted
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 1.
          CLEAR: ITAB_TABLE01-I_QTY.
        ENDIF.

      WHEN 'ITAB_TABLE01-I_DAT'.
        IF ITAB_TABLE01-DABMG > 1.
          SCREEN-INPUT = 1.
          SCREEN-VALUE_HELP = 1.
        ELSE.
          SCREEN-INPUT = 0.
          SCREEN-VALUE_HELP = 0.
          CLEAR: ITAB_TABLE01-I_DAT.
        ENDIF.
        IF NOT ITAB_TABLE01-MULTI IS INITIAL. "Multiple schedule
          SCREEN-INPUT = 0.
          SCREEN-VALUE_HELP = 0.
          CLEAR: ITAB_TABLE01-I_DAT.
        ENDIF.
        IF NOT ITAB_TABLE01-LOEKZ IS INITIAL. "Deleted
          SCREEN-INPUT = 0.
          SCREEN-VALUE_HELP = 0.
          CLEAR: ITAB_TABLE01-I_DAT.
        ENDIF.

      WHEN 'ITAB_TABLE01-I_LGORT'.
*        if itab_table01-dabmg > 1.
*          screen-input      = 1.
*          screen-value_help = 1.
*        else.
*          screen-input      = 0.
*          screen-value_help = 0.
*          clear: itab_table01-i_lgort.
*        endif.
*        if not itab_table01-multi is initial. "Multiple schedule
*          screen-input      = 0.
*          screen-value_help = 0.
*          clear: itab_table01-i_lgort.
*        endif.
*        if not itab_table01-loekz is initial. "Deleted
*          screen-input      = 0.
*          screen-value_help = 0.
*          clear: itab_table01-i_lgort.
*        endif.

      WHEN OTHERS.
        CONTINUE.

    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

*  LOOP AT SCREEN.
*    IF screen-name = g_srch_fld.
*      IF itab_table01-ref = 1.
*        screen-intensified = 1.
*      ELSE.
*        screen-intensified = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

ENDMODULE.                    "TABLE_CONTROL01_INFO_MOVE OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_0100 OUTPUT.

  IF D0100-EBELN IS INITIAL OR NOT G_CLEAR IS INITIAL.
    CLEAR:   D0100, ITAB_EKKOPO, ITAB_TABLE01, G_CLEAR.
    REFRESH: ITAB_EKKOPO, ITAB_TABLE01.
    REFRESH CONTROL 'TABLE_CONTROL01' FROM SCREEN '0100'.
    G_EBELN = D0100-EBELN.

    CALL FUNCTION 'DEQUEUE_ALL'.

    G_TODAY = SY-DATUM.

  ENDIF.

ENDMODULE.                 " INIT_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'STATUS0100'.
  SET TITLEBAR  '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0900 OUTPUT.

  CLEAR G_D0900_UCOMM.
  SET PF-STATUS 'STATUS0900'.
  SET CURSOR FIELD 'D0900-TEXT1'.

ENDMODULE.                 " STATUS_0900  OUTPUT
