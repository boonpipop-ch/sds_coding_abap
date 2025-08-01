*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0480
*  Creation Date      : 23.05.2025
*  Author             : Boonpipop Ch. (SDS)
*  Add-on ID          : N/A
*  Description        : Display Contract Services to RAR
*                       WRICEP ZFIARE040
*  Purpose            : ZSDSFIR0480 - Program Post Contract Services to RAR
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCMR0160.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE ZSDSCMR0160_TOP.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_CON-TCODE.
  PERFORM F_GET_CONSTANTS.
  PERFORM F_SET_SELSCR_DEFAULT.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SET_SEL_SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM F_SET_INIT_DATA.
  PERFORM F_CHK_SEL_SCREEN.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_GET_LOGS_SOMA CHANGING GT_LOGS
                                   GT_BKPF.

  PERFORM F_GET_DATA_SOMA CHANGING GT_LOGS
                                   GT_DATA_H
                                   GT_DATA.
  PERFORM F_PREPARE_REPORT_SOMA CHANGING GT_LOGS
                                       GT_OUTPUT.
  IF GT_OUTPUT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_OUTPUT.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.
  INCLUDE ZSDSCMR0160_F01.
