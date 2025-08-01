*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0220
*  Creation Date      : 03.05.2024
*  Author             : Apichat Ch.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program Mass post AR Special GL to AR SP GL Tax
*                       invoice
*                       RICEP ZFIARE002
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
* 02.01.2025  F36K910537  Apichat Ch.  - Adjust default selection screen
*-----------------------------------------------------------------------
* 13.06.2025  F36K919121  Apichat Ch.  - 420000663 Add xref2 data for clearing doc
*                                        ref to original doc
*-----------------------------------------------------------------------


REPORT zsdsfir0220.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE zsdsfir0220_top.


*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_authorize_check USING gc_tcode.
  PERFORM f_set_selscr_default.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_adj_sel_screen.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_get_open_items.
  PERFORM f_post_document USING     gc_mode-test_run
                          CHANGING  gt_group
                                    gt_open_itms.


*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
  PERFORM f_display_result USING gt_output.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE zsdsfir0220_alv.
  INCLUDE zsdsfir0220_f01.
