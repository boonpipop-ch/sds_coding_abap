*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250_F03
*  Creation Date      : 20.08.2024
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic - update for bank's vendor
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0250_F03
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f_get_open_items_update_bank
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_open_items_update_bank .

  CASE gc_true.
    WHEN rb_creu.
      PERFORM f_get_open_items_for_create.
      PERFORM f_prepare_create_output.
    WHEN rb_chgu.
      PERFORM f_get_bp_log_open.
      PERFORM f_prepare_change_output.
    WHEN rb_disu.
      PERFORM f_get_bp_log_all.
      PERFORM f_prepare_change_output.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_input_update_bank
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALID
*&      <-- LREF_MSG
*&---------------------------------------------------------------------*
FORM f_validate_input_update_bank  CHANGING
                                    cf_valid TYPE abap_bool ##NEEDED
                                    cref_msg TYPE REF TO cl_alv_changed_data_protocol.

  CHECK cf_valid = gc_true.

  LOOP AT gt_output INTO DATA(ls_output)
    WHERE sel = gc_true
    AND   delete_flag = ''.

    DATA(lv_row_id) = sy-tabix.
*
*    IF rb_creu = gc_true OR
*       rb_chgu = gc_true.
*      IF ( ls_output-pymt_method IN gr_bank OR
*           ls_output-pymt_method IN gr_pdc ).
*        READ TABLE gt_action_status
*          WITH KEY action_type = ls_output-action_type
*                   status      = ls_output-status
*          TRANSPORTING NO FIELDS.
*        IF sy-subrc EQ 0.
*          DATA(lv_text1) = |{ ls_output-action_type }/{ ls_output-status }|.
*          PERFORM f_add_msg USING gc_msg_ty-err_action_status_before lv_row_id 'PYMT_METHOD'
*                                  lv_text1 ls_output-pymt_method  CHANGING cref_msg.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*

    IF ls_output-hbkid IS NOT INITIAL AND
       ls_output-hktid IS NOT INITIAL.
      SELECT SINGLE hkont
        INTO @DATA(lv_hkont)  ##NEEDED
        FROM t012k
        WHERE bukrs = @ls_output-bukrs
        AND   hbkid = @ls_output-hbkid
        AND   hktid = @ls_output-hktid.
      IF sy-subrc <> 0.
          PERFORM f_add_msg USING gc_msg_ty-err_invalid_acc_id lv_row_id 'HKTID'
                                  ls_output-hktid ''  CHANGING cref_msg.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
