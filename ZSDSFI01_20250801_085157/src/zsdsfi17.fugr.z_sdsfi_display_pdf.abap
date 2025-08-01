FUNCTION z_sdsfi_display_pdf.
*"----------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     REFERENCE(IV_FILENAME) TYPE  STRING
*"     REFERENCE(IV_EXT) TYPE  ZSDSDE_ATTACH_EXT
*"----------------------------------------------------------------------

  gv_filename = iv_filename.
  gv_ext = iv_ext.
  IF gv_ext IS INITIAL.
    gv_ext = 'PDF'.
    gv_filename = |{ gv_filename }{ gv_ext }|.
  ENDIF.

* Read PDF File
  PERFORM f_read_file.

* Display to screen
  PERFORM f_display_pdf.


ENDFUNCTION.
