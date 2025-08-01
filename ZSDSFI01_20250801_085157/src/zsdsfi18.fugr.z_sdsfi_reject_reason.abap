FUNCTION z_sdsfi_reject_reason.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_CANCEL) TYPE  CHAR01
*"  CHANGING
*"     REFERENCE(CT_COLL_LOG) TYPE  ZSDSFIS144_TT
*"----------------------------------------------------------------------
  gt_coll_log = ct_coll_log.

  PERFORM f_popup_to_input CHANGING gt_coll_log.

  ct_coll_log = gt_coll_log.
  ev_cancel = gv_cancel.

ENDFUNCTION.
