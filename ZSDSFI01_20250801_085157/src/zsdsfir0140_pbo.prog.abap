*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0140_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'STATUS_0100'.
  CHECK first_initial IS NOT INITIAL.

  PERFORM f_assign_header_data
              TABLES
                 i_header.


  LOOP AT i_detail INTO w_detail WHERE meinh NE 'SET'.
    PERFORM f_modify_bom IN PROGRAM zar_msbill_cred_chg IF FOUND TABLES i_detail
                                    USING w_detail-vbeln
                                          w_detail-posnr
                                          w_detail-kwmeng
                                   .
  ENDLOOP.

  PERFORM f_simulate.

   clear : first_initial,w_detail.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_CFG_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_cfg_lines OUTPUT.
  DATA : rows TYPE i.
  IF i_detail[] IS INITIAL.
    tc_detail-lines = 10.
  ELSE.
    rows = LINES( i_detail[] ).
    tc_detail-lines = rows.
  ENDIF.
ENDMODULE.                 " TC_CFG_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_modify_0120 OUTPUT.
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

*  IF w_detail-posnr EQ '999999'.
*    LOOP AT SCREEN.
*      CASE screen-name.
*        WHEN 'W_DETAIL-MAKTX'  OR
*             'W_DETAIL-NETPR'.
*          screen-input = 1.
*        WHEN 'W_DETAIL-KWMENG'.
*          screen-input = 0.
*      ENDCASE.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ENDIF.
ENDMODULE.                 " TC_MODIFY_0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'STATUS_0200'.
  CHECK first_initial IS NOT INITIAL.

  PERFORM f_assign_header_data
              TABLES
                 i_header.

  PERFORM f_simulate_200.

  CLEAR first_initial.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_CFG_LINES_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_cfg_lines_200 OUTPUT.
  PERFORM f_tc_detail_200_lines.
ENDMODULE.                 " TC_CFG_LINES_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_modify_0220 OUTPUT.

ENDMODULE.                 " TC_MODIFY_0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'STATUS_0300'.
  SET TITLEBAR 'STATUS_0300'.

ENDMODULE.                 " STATUS_0300  OUTPUT
