*-----------------------------------------------------------------------
*  Program ID         : func Z_SDSSD_DELETED_DO_SAVE_TAB
*  Creation Date      : 26.06.2024
*  Author             : Boontip
*  Add-on ID          : SDI036
*  Description        : Save DO data into ZSDSSDT011 (LIKP data),
*                       ZSDSSDT012 ( VBFA data)
*  Purpose            : to store data for interface to salesforces
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  10.03.2025  F36K912029  Boontip R.  Change checked field LIKP-LFDAT TO LIKP-WADAT
*                                      420000461(CH01)
*-----------------------------------------------------------------------
FUNCTION Z_SDSSD_DELETED_DO_SAVE_TAB .
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IF_VBELN) TYPE  LIKP-VBELN
*"----------------------------------------------------------------------
  DATA: LS_INS_ZSDSSDT011 TYPE ZSDSSDT011,
        LT_INS_ZSDSSDT012 TYPE TABLE OF ZSDSSDT012.

*  SELECT SINGLE LFDAT, LFART  "CH01-
   SELECT SINGLE "CH01+
          WADAT, "CH01+
          LFART  "CH01+
  INTO @DATA(LS_LIKP)
  FROM LIKP
  WHERE VBELN = @IF_VBELN.
  IF SY-SUBRC = 0.
    LS_INS_ZSDSSDT011-VBELN = IF_VBELN.
*    LS_INS_ZSDSSDT011-LFDAT = LS_LIKP-LFDAT. "CH01-
    LS_INS_ZSDSSDT011-WADAT = LS_LIKP-WADAT.  "CH01+
    LS_INS_ZSDSSDT011-LFART = LS_LIKP-LFART.
    INSERT ZSDSSDT011 FROM LS_INS_ZSDSSDT011.
  ENDIF.

  SELECT MANDT,
         RUUID,
         VBELV,
         POSNV,
         VBELN,
         POSNN,
         VBTYP_N,
         VBTYP_V,
         RFMNG,
         MEINS
  INTO TABLE @LT_INS_ZSDSSDT012
  FROM VBFA
  WHERE VBELN = @IF_VBELN "DO
  AND   VBTYP_N = @GC_VBTYP_DO
  AND   VBTYP_V = @GC_VBTYP_SO.

  IF SY-SUBRC = 0.
    INSERT ZSDSSDT012 FROM TABLE @LT_INS_ZSDSSDT012.
  ENDIF.
ENDFUNCTION.
