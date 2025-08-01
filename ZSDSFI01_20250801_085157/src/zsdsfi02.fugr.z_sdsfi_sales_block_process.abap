FUNCTION z_sdsfi_sales_block_process .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_BLOCK) TYPE  ZSDSFIS116_TT OPTIONAL
*"  EXPORTING
*"     VALUE(ET_BLOCK) TYPE  ZSDSFIS116_TT
*"----------------------------------------------------------------------


  DATA: lt_return TYPE bapiret2_t.


  LOOP AT it_block ASSIGNING FIELD-SYMBOL(<lfs_block>).

    CLEAR: gv_flag_err,
          lt_return[].

    PERFORM chk_bp_number USING <lfs_block>-partner
                          CHANGING lt_return.

    IF gv_flag_err IS INITIAL.
      PERFORM process_block_sales CHANGING <lfs_block>.
    ELSE.
      "return error
      LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'E'
                                               OR type = 'A'.
        <lfs_block>-msg_typ     = ls_return-type.
        <lfs_block>-message     = ls_return-message.
        EXIT.
      ENDLOOP.
      CONTINUE.
    ENDIF.

  ENDLOOP.


  REFRESH et_block[].
  et_block[] = it_block[].


ENDFUNCTION.
