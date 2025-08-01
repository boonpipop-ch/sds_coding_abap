FUNCTION Z_SDSSD_BACKORDER_UPDATE.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMMIT_EXTERNAL) TYPE  ABAP_BOOL DEFAULT SPACE
*"     VALUE(SD_READ_AND_SAVE) TYPE  FLAG DEFAULT 'X'
*"     VALUE(FIXMG_SET) TYPE  FLAG DEFAULT SPACE
*"     VALUE(NO_LOCK) TYPE  FLAG DEFAULT 'X'
*"     VALUE(P_CALL_ACTIVITY) TYPE  CHAR4 DEFAULT 'V03R'
*"     VALUE(IV_NO_BOPF_TRANS_MGR_CALLS) TYPE  FLAG DEFAULT SPACE
*"  TABLES
*"      KORTAB STRUCTURE  MDVU OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  CALL FUNCTION 'SD_BACKORDER_UPDATE'
    EXPORTING
      COMMIT_EXTERNAL            = COMMIT_EXTERNAL
      SD_READ_AND_SAVE           = SD_READ_AND_SAVE
      FIXMG_SET                  = FIXMG_SET
      NO_LOCK                    = NO_LOCK
      P_CALL_ACTIVITY            = P_CALL_ACTIVITY
      IV_NO_BOPF_TRANS_MGR_CALLS = IV_NO_BOPF_TRANS_MGR_CALLS
    TABLES
      KORTAB                     = KORTAB
      ET_RETURN                  = ET_RETURN.

*  COMMIT WORK.


ENDFUNCTION.
