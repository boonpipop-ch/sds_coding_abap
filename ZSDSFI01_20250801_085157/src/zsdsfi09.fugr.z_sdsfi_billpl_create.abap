FUNCTION Z_SDSFI_BILLPL_CREATE.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  TABLES
*"      IT_ZSDSFIT033 STRUCTURE  ZSDSFIT033
*"      IT_ZSDSFIT034 STRUCTURE  ZSDSFIT034
*"      IT_ZSDSFIT035 STRUCTURE  ZSDSFIT035
*"----------------------------------------------------------------------
  DATA: LT_ACCCHG TYPE STANDARD TABLE OF ACCCHG,
        LS_ACCCHG LIKE LINE OF LT_ACCCHG.

  INSERT ZSDSFIT033 FROM TABLE IT_ZSDSFIT033.
  INSERT ZSDSFIT034 FROM TABLE IT_ZSDSFIT034.
  INSERT ZSDSFIT035 FROM TABLE IT_ZSDSFIT035.

  "-Update XREF2 at FI document AR Item
  LOOP AT IT_ZSDSFIT035 ASSIGNING FIELD-SYMBOL(<L_035>).
    CLEAR LT_ACCCHG.
    LS_ACCCHG-FDNAME    = 'XREF2'.
    LS_ACCCHG-NEWVAL = <L_035>-BILLPL_NO.
    APPEND LS_ACCCHG TO LT_ACCCHG.
    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        I_BUZEI              = <L_035>-BUZEI
        X_LOCK               = ''
        I_BUKRS              = <L_035>-BUKRS
        I_BELNR              = <L_035>-BELNR
        I_GJAHR              = <L_035>-GJAHR
      TABLES
        T_ACCCHG             = LT_ACCCHG
      EXCEPTIONS
        NO_REFERENCE         = 1
        NO_DOCUMENT          = 2
        MANY_DOCUMENTS       = 3
        WRONG_INPUT          = 4
        OVERWRITE_CREDITCARD = 5
        ERROR_MESSAGE        = 6
        OTHERS               = 7.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

  ENDLOOP.


ENDFUNCTION.
