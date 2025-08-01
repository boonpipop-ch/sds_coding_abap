*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0230
*  Creation Date      : 26.07.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : FIAR007
*  Description        : Sales Order Status Report
*  Purpose            : Program Check Invoice from Logistic
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  22/01/2025  F36K911497  Boontip R.  CH01- IMS420000174
*  fix get credit limit from ZSDSFIT036
*-----------------------------------------------------------------------
REPORT ZSDSSDR0230 ##TEXT_USE.
INCLUDE ZSDSSDR0230_TOP.
INCLUDE ZSDSCAI9990 ##INCL_OK.
INCLUDE ZSDSSDR0230_F01.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_DATA .
  PERFORM F_SELECT_AND_PREPARE_DATA CHANGING GT_DATA.
  IF GT_DATA IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM F_DISPLAY_RESULT USING GT_DATA.
