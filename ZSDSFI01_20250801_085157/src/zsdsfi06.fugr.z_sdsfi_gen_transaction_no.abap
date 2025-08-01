FUNCTION Z_SDSFI_GEN_TRANSACTION_NO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EX_TRANSACTION_NO) TYPE  ZSDSDE_TRANS_NO
*"----------------------------------------------------------------------

  DATA: LV_DATE TYPE  DATS,
        LV_TIME TYPE  NUMC10,
        LV_SSS  TYPE  NUMC3.

  CALL FUNCTION 'ZETX_GEN_TIMESTAMP'
    IMPORTING
      EX_DATE = LV_DATE
      EX_TIME = LV_TIME.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZETAX_NNN'
    IMPORTING
      NUMBER                  = LV_SSS
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.

  ENDIF.

  IF LV_SSS IS NOT INITIAL.
    LV_TIME+6(3) = LV_SSS.
  ENDIF.

  CONCATENATE LV_DATE LV_TIME
         INTO EX_TRANSACTION_NO.



ENDFUNCTION.
