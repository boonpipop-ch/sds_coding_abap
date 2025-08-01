FUNCTION Z_SDSFI_BILLPL_REPRINT.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  TABLES
*"      IT_ZSDSFIT034 STRUCTURE  ZSDSFIT034
*"----------------------------------------------------------------------
  DATA: LT_ACCCHG TYPE STANDARD TABLE OF ACCCHG,
        LS_ACCCHG LIKE LINE OF LT_ACCCHG.

  INSERT ZSDSFIT034 FROM TABLE IT_ZSDSFIT034.


ENDFUNCTION.
