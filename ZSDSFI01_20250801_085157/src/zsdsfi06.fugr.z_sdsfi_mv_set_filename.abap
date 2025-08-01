FUNCTION Z_SDSFI_MV_SET_FILENAME.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_FILENAME)
*"  EXPORTING
*"     REFERENCE(EX_FILENAME)
*"----------------------------------------------------------------------

* Input:   EZTAX_20200114_152646_100920200000830003.pdf
* Output:  EZTAX_*_100920200000830003.pdf

  DATA: lv_filename TYPE C LENGTH 500.

  lv_filename = im_filename.

*  lv_filename+27(13) = '*'.
*  lv_filename+6(13) = '*'.
  lv_filename+26(13) = '*'.
  CONDENSE lv_filename NO-GAPS.

  ex_filename = lv_filename.



ENDFUNCTION.
