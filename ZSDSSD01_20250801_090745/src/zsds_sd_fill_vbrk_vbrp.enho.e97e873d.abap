"Name: \PR:SAPLV60A\FO:USEREXIT_FILL_VBRK_VBRP\SE:BEGIN\EI
ENHANCEMENT 0 ZSDS_SD_FILL_VBRK_VBRP.
*--------------------------------------------------------------------*
* SDS Enhance
*--------------------------------------------------------------------*
ZCL_SDSCA_UTIL_SDS=>CHECK_SDS( EXPORTING LV_MANDT = SY-MANDT
                               IMPORTING LV_SDS   = DATA(LV_SDS) ).
IF LV_SDS EQ ABAP_TRUE.
  DATA : LCL_ENHANCE TYPE REF TO ZCL_SDSSD_BILLING_ENHANCE.

  IF LCL_ENHANCE IS NOT BOUND.
    CREATE OBJECT LCL_ENHANCE.
  ENDIF.

  DATA(LV_ERROR) = LCL_ENHANCE->VALIDATION_BILLING( I_VBRK  = VBRK
                                                    I_VBRP  = VBRP
                                                    I_VBPA  = VBPA ).
  IF LV_ERROR IS NOT INITIAL.
    MESSAGE E000(ZSDSSD01) WITH LV_ERROR.
  ENDIF.
ENDIF.
*--------------------------------------------------------------------*
* End SDS Enhance
*--------------------------------------------------------------------*
ENDENHANCEMENT.
