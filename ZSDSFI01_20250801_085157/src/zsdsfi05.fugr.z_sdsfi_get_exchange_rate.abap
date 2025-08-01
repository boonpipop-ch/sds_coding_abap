FUNCTION Z_SDSFI_GET_EXCHANGE_RATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_START_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(IV_END_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(IV_CURRENCY) TYPE  CHAR10 OPTIONAL
*"     VALUE(IV_UPDATE_K2) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(ES_EXCHANGE_RATE) TYPE  ZSDSFIS097_TT
*"     VALUE(EV_MESTYPE) TYPE  CHAR1
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"  EXCEPTIONS
*"      CURRENCY_NOT_EXIST
*"      DATE_IS_INVALID
*"      API_URL_NOT_FOUND
*"----------------------------------------------------------------------

  DATA: LV_URL  TYPE STRING.

  PERFORM PF_CHECK_VALID_DATA USING IV_START_DATE
                                    IV_END_DATE
                                    IV_CURRENCY.

  PERFORM PF_GET_API_URL USING iv_update_k2
                      CHANGING LV_URL.

  PERFORM PF_CALL_API USING LV_URL
                            IV_START_DATE
                            IV_END_DATE
                            IV_CURRENCY
                   CHANGING ES_EXCHANGE_RATE
                            EV_MESTYPE
                            EV_MESSAGE.

ENDFUNCTION.
