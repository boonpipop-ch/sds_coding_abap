FUNCTION z_sdsfi_interface_00001040.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_XVBUP) LIKE  OFIWA-XVBUP DEFAULT 'X'
*"     REFERENCE(I_RF05R) LIKE  RF05R STRUCTURE  RF05R
*"  TABLES
*"      T_XRAGL STRUCTURE  RAGL1
*"--------------------------------------------------------------------

  PERFORM f_get_coll_log USING I_RF05R CHANGING gt_coll_log gt_bank_statement.
  PERFORM f_reverse_coll_log USING gt_coll_log.
  PERFORM f_update_bank_statement USING gt_bank_statement.


ENDFUNCTION.
