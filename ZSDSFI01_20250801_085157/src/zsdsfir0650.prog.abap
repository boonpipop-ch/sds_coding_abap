*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0650
*  Creation Date      : 24.01.2025
*  Author             : Apichat Ch.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program mass clearing AR document
*                       RICEP ZFIARE002
*                       Incident 420000186
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
* 14.07.2025  F36K921145  Apichat Ch.  420000706
* - 01. Adjust query logic
*-----------------------------------------------------------------------


REPORT ZSDSFIR0650.
*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE zsdsfir0650_top.


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
  INCLUDE zsdsfir0650_alv.
  INCLUDE zsdsfir0650_f01.
