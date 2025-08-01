*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0130_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'STATUS_0100'.
  CHECK FIRST_INITIAL IS NOT INITIAL.
  DATA : ITMP TYPE TABLE OF TYP_DETAIL.

  PERFORM F_ASSIGN_HEADER_DATA
              TABLES
                 I_HEADER.


  LOOP AT I_DETAIL INTO W_DETAIL WHERE MEINH NE 'SET'.
    PERFORM F_MODIFY_BOM TABLES I_DETAIL
                                    USING W_DETAIL-VBELN
                                          W_DETAIL-POSNR
                                          W_DETAIL-KWMENG
                                   .
  ENDLOOP.
  CLEAR W_DETAIL.

  PERFORM F_SIMULATE.
  CLEAR : FIRST_INITIAL,W_DETAIL.
ENDMODULE.                 " STATUS_0100  OUTPUT
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
*  IF w_detail-add EQ 'X'.
*    LOOP AT SCREEN.
*      CASE screen-name.
*        WHEN 'W_DETAIL-MAKTX' OR
*             'W_DETAIL-NETPR' OR
*             'W_DETAIL-POSNR' OR
*             'W_DETAIL-KWMENG' .
*          screen-input = 1.
*        WHEN 'W_DETAIL-KWMENG'.
*          screen-input = 0.
*      ENDCASE.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ENDIF.

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
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'STATUS_0200'.
  CHECK FIRST_INITIAL IS NOT INITIAL.

  PERFORM F_ASSIGN_HEADER_DATA
              TABLES
                 I_HEADER.

  PERFORM F_SIMULATE_200.

  CLEAR FIRST_INITIAL.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_CFG_LINES_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_CFG_LINES_200 OUTPUT.
  PERFORM F_TC_DETAIL_200_LINES.
ENDMODULE.                 " TC_CFG_LINES_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MODIFY_0220 OUTPUT.

ENDMODULE.                 " TC_MODIFY_0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS 'STATUS_0300'.
  SET TITLEBAR 'STATUS_0300'.

ENDMODULE.                 " STATUS_0300  OUTPUT
