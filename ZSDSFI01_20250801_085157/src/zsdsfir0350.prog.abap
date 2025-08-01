*-----------------------------------------------------------------------
*  Program ID         : ZFIAPE008
*  Creation Date      : 02.05.2024
*  Author             : Nadtaya T.(Eviden)
*  Add-on ID          : N/A
*  Description        : FI AP Pre-paid expense amortization program
*  Purpose            : This is use to determine the posting condition
*                       for pre-paid expense amortization and
*                       post pre-paid expense amortization
*                       with “Linear-Daily” amortization method
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  26.11.2024  CH01      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0350.
*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE ZSDSFIR0350_TOP.
INCLUDE ZSDSFIR0350_CLS.
INCLUDE ZSDSFIR0350_S01.
INCLUDE ZSDSFIR0350_F01.
INCLUDE ZSDSFIR0350_O01.
INCLUDE ZSDSFIR0350_I01.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_DATA_INIT.
  PERFORM F_GET_CONSTANTS.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SCR_MODIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR.
  PERFORM F4_VARIANT CHANGING P_VAR.


*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM F_REFRESH_DATA.

  PERFORM F_MAINTAIN_DATA.

  PERFORM F_PREPARE_POSTING_DATA.

  PERFORM F_PREPARE_POSTING_REPORT.
