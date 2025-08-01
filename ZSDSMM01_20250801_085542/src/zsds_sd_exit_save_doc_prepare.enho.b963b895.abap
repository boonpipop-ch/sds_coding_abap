"Name: \PR:SAPMV50A\FO:USEREXIT_SAVE_DOCUMENT_PREPARE\SE:END\EI
ENHANCEMENT 0 ZSDS_SD_EXIT_SAVE_DOC_PREPARE.
DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

DATA : LS_STORG TYPE LGORT_D,
       LT_STORG TYPE DFPS_LGORT_T.

DATA : LCL_ENHANCE TYPE REF TO ZCL_SDSMM_ENHANCEMENT.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
IF LCL_UTIL IS NOT BOUND.
  CREATE OBJECT LCL_UTIL.
ENDIF.

LCL_UTIL->CHECK_SDS( EXPORTING LV_MANDT = SY-MANDT
                     IMPORTING LV_SDS   = LV_CHECK_SDS ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
IF LV_CHECK_SDS EQ ABAP_TRUE.
  IF ( SY-TCODE = 'VL01N' OR SY-TCODE = 'VL01' OR SY-TCODE = 'VL02N' OR SY-TCODE = 'VL02' ).
    IF LCL_ENHANCE IS NOT BOUND.
      CREATE OBJECT LCL_ENHANCE.
    ENDIF.

    LOOP AT XLIPS INTO DATA(LS_LIPS).
      LS_STORG = LS_LIPS-LGORT.
      APPEND LS_STORG TO LT_STORG.
    ENDLOOP.

    SORT LT_STORG.
    DELETE ADJACENT DUPLICATES FROM LT_STORG.

    IF LT_STORG IS NOT INITIAL.
      DATA(LV_CHECK) = LCL_ENHANCE->CHECK_USER_STOCK( LT_STORG ).
      IF LV_CHECK EQ ABAP_TRUE.
        MESSAGE E004(ZSDSMM01).
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
ENDENHANCEMENT.
