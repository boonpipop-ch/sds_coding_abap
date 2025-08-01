*&---------------------------------------------------------------------*
*& Include          LZSDSFI18P01
*&---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    CLEAR gv_cancel.

    CASE e_salv_function.
      WHEN 'OK'.
        TRY.
            DATA(lo_api) = go_alv->extended_grid_api(
            ).
          CATCH cx_salv_api_contract_violation. " API contract violated by caller
            RETURN.
        ENDTRY.

        DATA(lo_edit) = lo_api->editable_restricted( ).

        lo_edit->validate_changed_data( ).

        LOOP AT gt_coll_log INTO DATA(ls_coll_log)
          WHERE rejrsn IS INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          MESSAGE e000(38) WITH 'Please input reject reason'(e01).
        ELSE.
          FREE go_alv.
          LEAVE TO SCREEN 0.
        ENDIF.

      WHEN 'CANCEL'.
        gv_cancel = abap_true.
        FREE go_alv.
        LEAVE TO SCREEN 0.
      WHEN OTHERS.
* do nothing
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
