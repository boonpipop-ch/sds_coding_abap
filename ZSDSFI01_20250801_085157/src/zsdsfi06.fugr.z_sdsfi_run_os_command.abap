FUNCTION Z_SDSFI_RUN_OS_COMMAND.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_COMMAND)
*"  EXPORTING
*"     REFERENCE(EX_SUBRC)
*"     REFERENCE(EX_MSG)
*"----------------------------------------------------------------------

  DATA: BEGIN OF itab OCCURS 0,
          line(200),
        END OF itab.

  DATA: lv_subrc           TYPE sy-subrc,
        lv_command         LIKE rs37a-line,
        lv_msg             TYPE string.

  DATA: lref_ex_root  TYPE REF TO cx_root.

  lv_command = im_command.

  TRY.
    CALL 'SYSTEM' ID 'COMMAND' FIELD lv_command
      ID 'TAB' FIELD itab-*sys*.
    lv_subrc   = sy-subrc.
    lv_msg     = lv_command.

  CATCH cx_root INTO lref_ex_root.
    lv_subrc   = 9.
    lv_msg     = lref_ex_root->get_text( ).

  ENDTRY.

  ex_subrc = lv_subrc.
  ex_msg   = lv_msg.

  IF ex_subrc IS NOT INITIAL.
    RAISE error.
  ENDIF.



ENDFUNCTION.
