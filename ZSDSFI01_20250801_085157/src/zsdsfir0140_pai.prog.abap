*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0140_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_modify_0100 INPUT.

  READ TABLE i_detail_old WITH KEY vbeln = w_detail-vbeln
                             posnr = w_detail-posnr
               INTO w_detail_old.

  IF sy-subrc EQ 0.
    IF w_detail_old-kwmeng LT w_detail-kwmeng.
      MESSAGE i000(38) WITH 'Quantity more than original!' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

*  Quantity Changed
  w_detail-netwr = w_detail-netpr * w_detail-kwmeng.

* Calculate Adv
  w_detail-kwert = 0.
  LOOP AT i_konv INTO w_konv WHERE knumv = w_detail-knumv AND kposn = w_detail-posnr.
    w_detail-kwert = w_detail-kwert + w_konv-kwert.
  ENDLOOP.

  READ TABLE i_vbap INTO w_vbap WITH KEY vbeln = w_detail-vbeln
                                         posnr = w_detail-posnr.
  IF sy-subrc EQ 0.
    w_detail-kwert = w_detail-kwert * ( w_detail-kwmeng / w_vbap-kwmeng ).
    w_detail-kwert = ( w_detail-kwert * txt_prcnt ) / 100.
  ENDIF.

  CLEAR : w_konv,w_vbap.
  MODIFY i_detail FROM w_detail INDEX tc_detail-current_line.

  PERFORM f_cal_amt TABLES i_detail[].
ENDMODULE.                 " TC_MODIFY_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_modify_0110 INPUT.
  PERFORM f_simulate.
ENDMODULE.                 " TC_MODIFY_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK' OR '&LEAVE' OR '&EXIT'.
      LEAVE TO SCREEN 0.

*    WHEN '&SIM'.
*      PERFORM f_simulate.
*    WHEN '&REF'.
*      i_detail[] = i_detail_old[].
*      CLEAR : txt_amt_inc_vat,
*              txt_amt_inc_wht,
*              txt_amt_inc_vat_o,
*              txt_amt_inc_wht_o,
*              txt_total_amt1,
*              txt_total_amt1_o,
*              txt_total_amt2,
*              txt_total_amt2_o,
*              txt_total_qty,
*              txt_total_qty_o,
*              txt_grandtotal,
*              txt_grandtotal_o,
*              txt_adv,
*              txt_adv_o.
*
*      txt_prcnt = 100.
*      chk_vat = 'X'.
*
*    WHEN '&SALL'.
*      w_detail-sel = 'X'.
*      MODIFY i_detail FROM w_detail TRANSPORTING sel WHERE matnr IS NOT INITIAL.
*      CLEAR w_detail.
*    WHEN '&DALL'.
*      w_detail-sel = space.
*      MODIFY i_detail FROM w_detail TRANSPORTING sel WHERE matnr IS NOT INITIAL.
*      CLEAR w_detail.
*
*    WHEN '&CDOC'.
*      PERFORM f_assign_topic IN PROGRAM zar_msbill_cred IF FOUND USING lst_doctype CHANGING txt_topic txt_attend.
*
*    WHEN '&CVAT' OR '&CWHT'.
*      PERFORM f_simulate.
*
*    WHEN '&CMGR'.
*      PERFORM f_assign_pos USING lst_mgrnr CHANGING txt_pos.
*
*    WHEN '&ADD'.
*      PERFORM f_add_remark TABLES i_detail.
*
*    WHEN '&DEL'.
*      PERFORM f_del_remark  TABLES i_detail.
*
*    WHEN '&DELH'.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
*          textline1      = 'Are you sure to delete billing document ?'
*          titel          = 'Confirm to delete'
*          cancel_display = space
*        IMPORTING
*          answer         = gv_answer.
*      IF gv_answer = 'J'.
*        PERFORM f_del_billing_document USING txt_docnr.
*      ENDIF.
*
*      CLEAR gv_answer.
*
*    WHEN '&CRE' OR '&SAVE'.
*      PERFORM f_chek_require_field CHANGING r_field.
*      CHECK r_field EQ 0.
*
*      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
*          textline1      = 'Are you sure save changed billing document ?'
*          titel          = 'Confirm to change'
*          cancel_display = space
*        IMPORTING
*          answer         = gv_answer.
*      IF gv_answer = 'J'.
*        PERFORM f_simulate.
*        PERFORM f_save_changed_bill TABLES i_detail.
*      ENDIF.
*
*      CLEAR gv_answer.
*    WHEN '&AMAT'.
*      PERFORM f_get_vbap_pop TABLES i_vbap_pop USING txt_vbeln.
*      IF i_vbap_pop[] IS NOT INITIAL.
*        CALL SCREEN 0300 STARTING AT 5 2 ENDING AT 125 18.
*      ELSE.
*        MESSAGE i000(38) WITH 'No sale order items to added!' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.

  ENDCASE.


  CLEAR sy-ucomm.



ENDMODULE.                 " USER_COMMAND_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_modify_0200 INPUT.
*  Quantity Changed
  w_detail_200-netwr = w_detail_200-netpr * w_detail_200-kwmeng.

*  Material changed
*  PERFORM f_matnr_desc USING w_detail_200-matnr CHANGING w_detail_200-maktx.

* Material description changed
  IF w_detail_200-matnr IS INITIAL.
    PERFORM f_maktx_desc USING w_detail_200-maktx CHANGING w_detail_200-matnr w_detail_200-maktx.
  ENDIF.

  READ TABLE i_detail_200 INTO w_detail_add INDEX  tc_detail_200-current_line.
  IF sy-subrc EQ 0.
    MODIFY i_detail_200 FROM w_detail_200 INDEX tc_detail_200-current_line.
  ELSE.
    APPEND w_detail_200 TO i_detail_200.
  ENDIF.

  PERFORM f_cal_amt_200 TABLES i_detail_200[].
  PERFORM f_tc_detail_200_lines.
ENDMODULE.                 " TC_MODIFY_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_modify_0210 INPUT.
  PERFORM f_simulate_200.
ENDMODULE.                 " TC_MODIFY_0210  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK' OR '&LEAVE' OR '&EXIT'.
      LEAVE TO SCREEN 0.
*    WHEN '&SIM'.
*      PERFORM f_simulate_200.
*
*    WHEN '&SALL'.
*      w_detail_200-sel = 'X'.
*      MODIFY i_detail_200 FROM w_detail_200 TRANSPORTING sel WHERE maktx IS NOT INITIAL.
*      CLEAR w_detail_200.
*    WHEN '&DALL'.
*      w_detail_200-sel = space.
*      MODIFY i_detail_200 FROM w_detail_200 TRANSPORTING sel WHERE maktx IS NOT INITIAL.
*      CLEAR w_detail_200.
*
*    WHEN '&CDOC'.
*      PERFORM f_assign_topic IN PROGRAM zar_msbill_cred IF FOUND  USING lst_doctype CHANGING txt_topic txt_attend.
*
*    WHEN '&CVAT' OR '&CWHT'.
*      PERFORM f_simulate_200.
*
*    WHEN '&CMGR'.
*      PERFORM f_assign_pos USING lst_mgrnr CHANGING txt_pos.
*
*    WHEN '&CRE' OR '&SAVE'.
*
*      PERFORM f_chek_require_field_200 CHANGING r_field.
*      CHECK r_field EQ 0.
*
*      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
*          textline1      = 'Are you sure to save changed billing document ?'
*          titel          = 'Confirm to change'
*          cancel_display = space
*        IMPORTING
*          answer         = gv_answer.
*      IF gv_answer = 'J'.
*
*        PERFORM f_simulate_200.
*        PERFORM f_save_changed_bill_200 TABLES i_detail_200[].
*      ENDIF.
*
*    WHEN '&DEL'.
*      PERFORM f_del_rows TABLES i_detail_200[].
*
*    WHEN '&DELH'.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
*          textline1      = 'Are you sure to delete billing document ?'
*          titel          = 'Confirm to delete'
*          cancel_display = space
*        IMPORTING
*          answer         = gv_answer.
*      IF gv_answer = 'J'.
*        PERFORM f_del_billing_document USING txt_docnr.
*      ENDIF.
*
*      CLEAR gv_answer.
*    WHEN OTHERS.
  ENDCASE.

  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN '&OK'.
      PERFORM f_add_line_items TABLES i_vbap_pop i_detail i_detail_old i_vbap.
      LEAVE TO SCREEN 0.
    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MODIFY_300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_modify_300 INPUT.
  MODIFY i_vbap_pop FROM w_vbap_pop INDEX tc_vbap_300-current_line.
ENDMODULE.                 " TC_MODIFY_300  INPUT
