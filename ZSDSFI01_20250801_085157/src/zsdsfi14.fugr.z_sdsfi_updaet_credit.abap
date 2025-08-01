FUNCTION Z_SDSFI_UPDAET_CREDIT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BP_NO) TYPE  BU_PARTNER OPTIONAL
*"     VALUE(I_TEST) TYPE  BOOLE_D OPTIONAL
*"     VALUE(I_CREDIT_LIMIT) TYPE  UKM_CREDIT_LIMIT OPTIONAL
*"     VALUE(I_LIMIT_VALID_DATE) TYPE  UKM_VALID_DATE OPTIONAL
*"     VALUE(I_PAY_TERM) TYPE  DZTERM OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESTYPE) TYPE  FLAG
*"     VALUE(E_MESSAGE) TYPE  CHAR255
*"----------------------------------------------------------------------

  DATA : LCL_DATA TYPE REF TO ZCL_SDSFI_BP.

  IF LCL_DATA IS NOT BOUND.
    CREATE OBJECT LCL_DATA.
  ENDIF.

  E_MESTYPE = LCL_DATA->UPDATE_CREDIT( EXPORTING I_BP_NO            = I_BP_NO
                                                 I_TEST             = I_TEST
                                                 I_CREDIT_LIMIT     = I_CREDIT_LIMIT
                                                 I_LIMIT_VALID_DATE = I_LIMIT_VALID_DATE
                                                 I_PAY_TERM         = I_PAY_TERM
                                       IMPORTING E_MESSAGE          = E_MESSAGE ).

ENDFUNCTION.
