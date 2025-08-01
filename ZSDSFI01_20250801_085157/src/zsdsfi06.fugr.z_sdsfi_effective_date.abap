FUNCTION Z_SDSFI_EFFECTIVE_DATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_DATE) TYPE  SY-DATUM
*"  EXPORTING
*"     REFERENCE(EX_RET2) TYPE  BAPIRET2
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: LV_EFF_TXT TYPE CHAR255,
        LV_EFF_DAT TYPE SY-DATUM.

  SELECT SINGLE
         LOW_VALUE
      FROM ZSDSFIC015
      INTO LV_EFF_TXT
      WHERE ID    = 'ZETX001'
      AND   NAME  = 'EFFECTIVE DATE'
      AND   ENDDA >= SY-DATUM
      AND   BEGDA <= SY-DATUM.
  IF LV_EFF_TXT IS NOT INITIAL.

    REPLACE ALL OCCURRENCES OF : '/' IN LV_EFF_TXT WITH SPACE,
                                 '.' IN LV_EFF_TXT WITH SPACE.
    CONDENSE LV_EFF_TXT.

    CONCATENATE LV_EFF_TXT+4(4) LV_EFF_TXT+2(2) LV_EFF_TXT+0(2)
    INTO LV_EFF_DAT.

    IF IM_DATE >= LV_EFF_DAT.
      EX_RET2-TYPE    = GC_SUCCESS.
      EX_RET2-MESSAGE = 'OK'.
    ELSE.
      EX_RET2-TYPE    = GC_ERROR.
      EX_RET2-MESSAGE = 'This document is issued before E-Tax Project'.
    ENDIF.
  ENDIF.

*  IF ex_ret2-type EQ gc_error.
*    STOP.
*  ENDIF.



ENDFUNCTION.
