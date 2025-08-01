FUNCTION Z_SDSFI_GET_FILENAME_FROM_PATH.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_PATH_FILENAME) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(EX_PATH) TYPE  STRING
*"     REFERENCE(EX_FILENAME) TYPE  STRING
*"----------------------------------------------------------------------
  DATA: lv_im_path TYPE c LENGTH 500,
        lv_ex_path TYPE draw-filep, "length 255
        lv_ex_file TYPE draw-filep. "length 255

  ex_path      = space.
  ex_filename  = space.
  lv_im_path   = im_path_filename.

  CALL FUNCTION 'CV120_SPLIT_PATH'
    EXPORTING
      pf_path  = lv_im_path
    IMPORTING
      pfx_path = lv_ex_path
      pfx_file = lv_ex_file.

  ex_path     = lv_ex_path.
  ex_filename = lv_ex_file.

ENDFUNCTION.
