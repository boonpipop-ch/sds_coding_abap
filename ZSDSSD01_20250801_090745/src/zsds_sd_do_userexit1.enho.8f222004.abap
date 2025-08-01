"Name: \PR:SAPMV50A\FO:USEREXIT_DELETE_DOCUMENT\SE:BEGIN\EI
ENHANCEMENT 0 ZSDS_SD_DO_USEREXIT1.
*-----------------------------------------------------------------------
*  Program ID         : ZSDS_SD_DO_USEREXIT1
*  Creation Date      : 26.06.2024
*  Author             : Boontip
*  Add-on ID          : SDI036
*  Description        : When deleted DO , before DO data is deleted
*  Purpose            : Save Deleted Data to table
*                       ZSDSSDT011(LIKP data), ZSDSSDT012(VBFA data)
*                       for interface to salesforces
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
IF ZCL_SDSSD_SCHD_LN_STAT=>CHECK_ACTIVE_SALE_ORG( IF_SALES_ORG = LIKP-VKORG ) = ABAP_TRUE .
  IF T180-TRTYP = 'V'   "Change
  AND XLIKP_UPDKZ = 'D'. "Deleted
    CALL FUNCTION 'Z_SDSSD_DELETED_DO_SAVE_TAB'
      IN UPDATE TASK
      EXPORTING
        IF_VBELN = LIKP-VBELN.

  ENDIF.
ENDIF.
ENDENHANCEMENT.
