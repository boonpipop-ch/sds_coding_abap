*&---------------------------------------------------------------------*
*& Include ZSDSFIR0350_O01
*&---------------------------------------------------------------------*
MODULE TABSTRIP_CTRL_ACTIVE_TAB_SET OUTPUT.
  TABSTRIP_CTRL-ACTIVETAB = G_TABSTRIP_CTRL-PRESSED_TAB.
  CASE G_TABSTRIP_CTRL-PRESSED_TAB.
    WHEN C_TABSTRIP_CTRL-TAB1.
      G_TABSTRIP_CTRL-SUBSCREEN = '9001'.
    WHEN C_TABSTRIP_CTRL-TAB2.
      G_TABSTRIP_CTRL-SUBSCREEN = '9002'.
    WHEN C_TABSTRIP_CTRL-TAB3.
      G_TABSTRIP_CTRL-SUBSCREEN = '9003'.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module table_ctrl_main_init OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE TABLE_CTRL_MAIN_INIT OUTPUT.
  IF GF_MAINITEM_COPIED EQ 'X'.
    GF_MAINITEM_COPIED = ''.
    REFRESH CONTROL 'TABLE_CTRL_MAIN' FROM SCREEN '9001'.
    CLEAR ZSDSFIS075.
  ENDIF.

  DESCRIBE TABLE GT_MAINITEM LINES TABLE_CTRL_MAIN-LINES.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module table_ctrl_main_move OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE TABLE_CTRL_MAIN_MOVE OUTPUT.

  PERFORM F_GET_ACC_DESC USING GS_MAINITEM-HKONT_DR
                          CHANGING GS_MAINITEM-TXT20_DR.

  MOVE-CORRESPONDING GS_MAINITEM TO ZSDSFIS075.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module table_ctrl_main_get_lines OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE TABLE_CTRL_MAIN_GET_LINES OUTPUT.
  G_TABLE_CTRL_MAIN_LINES = SY-LOOPC.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module table_ctrl_acca_move OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE TABLE_CTRL_ACCA_MOVE OUTPUT.
  MOVE-CORRESPONDING GS_ACCA TO ZSDSFIS076.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  PERFORM F_STATUS_9000.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_GRID OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE DISPLAY_GRID OUTPUT.

  PERFORM F_DISPLAY_GRID.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE PBO_9000 OUTPUT.

  LOOP AT SCREEN.
*..Disable Document Overview Screen (Report)
    IF SCREEN-NAME EQ 'TABSTRIP_CTRL_TAB3'.
      IF GF_MODE EQ GC_CRET.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

*..Disable Posting Data and Account Assignment screen
    IF SCREEN-NAME EQ 'TABSTRIP_CTRL_TAB1' OR  SCREEN-NAME EQ 'TABSTRIP_CTRL_TAB2'.
      IF GF_MODE EQ GC_REPT.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-GROUP1 = '001'.
      IF GF_MODE NE GC_CRET.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF SCREEN-GROUP2 = '001'.
      IF ZSDSFIS074-POPER IS INITIAL AND ZSDSFIS074-BEGDA IS INITIAL AND ZSDSFIS074-ENDDA IS INITIAL.
        SCREEN-INPUT = 1.
      ELSE.
        SCREEN-INPUT = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.

  PERFORM F_STATUS_9001.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.

  PERFORM F_STATUS_9002.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module TABLE_CTRL_ACCA_INIT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE TABLE_CTRL_ACCA_INIT OUTPUT.

  DESCRIBE TABLE GT_ACCA LINES TABLE_CTRL_ACCA-LINES.
  IF LINES( GT_ACCA ) LT 5.

    DO.
      APPEND INITIAL LINE TO GT_ACCA.
      IF LINES( GT_ACCA ) EQ 5.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDMODULE.
