*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0520
*  Creation Date      : 11.11.2024
*  Author             : Apichat Ch(Eviden)
*  Add-on ID          : N/A
*  Description        : Program Advance Received Report
*                       RICEP ZFIAR036
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
* 30.12.2024  F36K910492  Atitep B.  - Change and add new column
*-----------------------------------------------------------------------
* 14.01.2025  F36K911030  Apichat Ch.  420000242 : Fix display attach file
*-----------------------------------------------------------------------
* 28.03.2025  F36K914879  Apichat Ch.  420000431
* 01 - Add column : Inv Refence (REBZG) to the report
* 02 - Adjust selection screen for open item mode
* 03 - Adjust display layout
* 04 - Fix duplicate data
* 05 - Add selection screen field assignment
*-----------------------------------------------------------------------


REPORT zsdsfir0520.
*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
  INCLUDE zsdsfir0520_top.
  INCLUDE zsdsfir0520_alv.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF sy-ucomm = 'ONLI'.
    IF rb_open = gc_true AND
       s_keydt[] IS INITIAL.
      MESSAGE  w019(msitem).
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_get_constant.
  PERFORM f_get_data.
  PERFORM f_perpare_output.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM f_display_result USING gt_output.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE zsdsfir0520_f01.
