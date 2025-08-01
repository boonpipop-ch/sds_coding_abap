*----------------------------------------------------------------------*
***INCLUDE LZSDSFI18F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_popup_to_input
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- EV_REJECT
*&      <-- CT_COLL_LOG
*&---------------------------------------------------------------------*
FORM f_popup_to_input  CHANGING ct_coll_log TYPE zsdsfis144_tt.

  DATA: lx_msg       TYPE REF TO cx_salv_msg.
  DATA: lo_cols      TYPE REF TO cl_salv_columns.
  DATA: lo_functions TYPE REF TO cl_salv_functions.

  DATA: lv_pos  TYPE i,
        lv_len  TYPE lvc_outlen,
        lv_edit TYPE char01.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = ct_coll_log ).

    CATCH cx_salv_msg INTO lx_msg.
      RETURN.
  ENDTRY.

  lo_functions = go_alv->get_functions( ).
  lo_functions->set_all( abap_true ).

  CALL METHOD go_alv->set_screen_status
    EXPORTING
      report   = sy-repid
      pfstatus = 'POPUP_STATUS'.

  lo_cols      = go_alv->get_columns( ).
*  lo_cols->set_optimize( ).

  DATA(lt_col) = lo_cols->get( ).

  LOOP AT lt_col INTO DATA(ls_col).
    CASE ls_col-columnname.
      WHEN 'XBLNR'.
        lv_pos = 1.
        CLEAR lv_len.
        lv_edit = ''.
      WHEN 'REJRSN'.
        lv_pos = 2.
        lv_len = 50.
        lv_edit = abap_true.
      WHEN OTHERS.
        CLEAR: lv_pos,
               lv_edit.
    ENDCASE.

    TRY.
        PERFORM f_fcat_field USING go_alv ls_col-columnname lv_pos lv_len lv_edit.
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER

    ENDTRY.

  ENDLOOP.

  go_alv->set_screen_popup(
    start_column = 2
    end_column   = 80
    start_line   = 2
    end_line     = 20
  ).


  DATA(lo_selections) = go_alv->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>none ).

  DATA(lo_events) = go_alv->get_event( ).

  CREATE OBJECT go_events.
  SET HANDLER go_events->on_user_command FOR lo_events.

  go_alv->display( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_fcat_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_fcat_field USING uo_alv TYPE REF TO cl_salv_table
                      uf_fname  TYPE lvc_fname
                      uf_pos    TYPE i
                      uf_len    TYPE lvc_outlen
                      uf_edit   TYPE char01.

  DATA: lo_cols      TYPE REF TO cl_salv_columns.
  DATA: lo_column    TYPE REF TO cl_salv_column_table.

  lo_cols      = uo_alv->get_columns( ).

  lo_cols->set_column_position(
    EXPORTING
      columnname = uf_fname    " ALV Control: Field Name of Internal Table Field
      position   = uf_pos
  ).

  TRY.
      lo_column ?= lo_cols->get_column( uf_fname ).
    CATCH cx_salv_not_found.
      RETURN.
  ENDTRY.

  IF uf_pos IS INITIAL.
    lo_column->set_visible( if_salv_c_bool_sap=>false ).
    lo_column->set_technical( ).
  ELSEIF uf_pos < 2.
    lo_column->set_key( ).
  ENDIF.

  IF uf_len IS NOT INITIAL.
    lo_column->set_output_length( value = uf_len  ).
  ENDIF.

  IF uf_edit = abap_true.

    TRY.
        DATA(lo_api) = uo_alv->extended_grid_api(
        ).
      CATCH cx_salv_api_contract_violation. " API contract violated by caller
        RETURN.
    ENDTRY.

    DATA(lo_edit) = lo_api->editable_restricted( ).

    TRY.
        lo_edit->set_attributes_for_columnname(
          columnname                  = uf_fname
          all_cells_input_enabled     = abap_true
        ).
      CATCH cx_salv_not_found. " ALV: General Error Class (Checked in Syntax Check)
        RETURN.
    ENDTRY.

  ENDIF.

ENDFORM.
