FUNCTION Z_SDSFI_MOVE_FILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_SOURCE) TYPE  TEXT255
*"     REFERENCE(IM_TARGET) TYPE  TEXT255
*"  EXPORTING
*"     REFERENCE(EX_SUBRC) TYPE  SY-SUBRC
*"     REFERENCE(EX_COMMAND)
*"     REFERENCE(EX_RETURN) TYPE  BAPIRET2
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lv_source_pattern  TYPE string,
        lv_sourcepath_file TYPE string,
        lv_targetpath_file TYPE string,
        lv_subrc           TYPE sy-subrc,
        lv_command         LIKE rs37a-line,
        lv_msg             TYPE string.

  DATA: lwa_return         LIKE ex_return.

  lv_sourcepath_file = im_source.
  lv_targetpath_file = im_target.

  CONCATENATE 'mv'
              lv_sourcepath_file
              lv_targetpath_file
         INTO lv_command SEPARATED BY space.

  PERFORM run_os_command USING lv_command
                      CHANGING lv_subrc
                               lv_msg.

  IF lv_subrc IS INITIAL.
    lwa_return-type    = gc_success.
    lwa_return-message = 'Move file: success( $msg )'.
  ELSE.
    lwa_return-type    = gc_error.
    lwa_return-message = 'Move file: error( $msg )'.
  ENDIF.

  REPLACE '$msg' IN lwa_return-message WITH lv_msg.

  PERFORM message_for_batch USING lwa_return-message.

  ex_subrc   = lv_subrc.
  ex_command = lv_command.
  ex_return  = lwa_return.

  IF ex_return-type EQ gc_error.
    RAISE error.
  ENDIF.



ENDFUNCTION.
