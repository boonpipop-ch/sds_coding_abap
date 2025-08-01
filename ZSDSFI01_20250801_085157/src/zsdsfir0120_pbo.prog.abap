*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0120_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  TC_CFG_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_CFG_LINES OUTPUT.

  DATA : ROWS TYPE I.
  IF I_DETAIL[] IS INITIAL.
    TC_DETAIL-LINES = 10.
  ELSE.
    ROWS = LINES( I_DETAIL[] ).
    TC_DETAIL-LINES = ROWS.
  ENDIF.
ENDMODULE.                 " TC_CFG_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MODIFY_0120 OUTPUT.

  IF W_DETAIL-POSNR EQ '999999'.
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'W_DETAIL-MAKTX'  OR
             'W_DETAIL-NETPR'.
          SCREEN-INPUT = 1.
        WHEN 'W_DETAIL-KWMENG'.
          SCREEN-INPUT = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " TC_MODIFY_0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
SET PF-STATUS 'STATUS_9000'.
SET TITLEBAR 'STATUS_9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MODIFY_0220 OUTPUT.

ENDMODULE.                 " TC_MODIFY_0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_CFG_LINES_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_CFG_LINES_200 OUTPUT.
  PERFORM F_TC_DETAIL_200_LINES.
ENDMODULE.                 " TC_CFG_LINES_200  OUTPUT
