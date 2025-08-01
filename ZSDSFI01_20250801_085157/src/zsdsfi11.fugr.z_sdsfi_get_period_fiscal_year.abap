FUNCTION Z_SDSFI_GET_PERIOD_FISCAL_YEAR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BUKRS) TYPE  BUKRS
*"     VALUE(I_DATUM) TYPE  DATUM
*"  EXPORTING
*"     VALUE(E_PERIOD) TYPE  POPER
*"     VALUE(E_GJAHR) TYPE  GJAHR
*"----------------------------------------------------------------------

  DATA: LV_PERIV TYPE PERIV,
        LV_POPER TYPE POPER,
        LV_GJAHR TYPE GJAHR.

  SELECT SINGLE PERIV
    INTO LV_PERIV
    FROM T001
    WHERE BUKRS = I_BUKRS.

  IF SY-SUBRC = 0.
* get current period.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
      EXPORTING
        I_DATE  = I_DATUM
        I_PERIV = LV_PERIV
      IMPORTING
        E_BUPER = LV_POPER
        E_GJAHR = LV_GJAHR.
  ENDIF.

  E_PERIOD = LV_POPER.
  E_GJAHR = LV_GJAHR.

ENDFUNCTION.
