*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSDSFIT022
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSDSFIT022         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
