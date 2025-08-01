*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0240
*  Creation Date      : 13.05.2024
*  Author             : Apichat Ch.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program Mass Auto AR Settlement to clear AR
*                       invoice
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
*  07.01.2025  F36K910747  Apichat Ch. 420000106 - Fix memo for new entry
*-----------------------------------------------------------------------
*  08.01.2025  F36K910815  Apichat Ch. 420000236 - Adjust default doc date
*-----------------------------------------------------------------------
*  09.01.2025  F36K910900  Apichat Ch. 420000137 - Adjust grouping payment
*-----------------------------------------------------------------------
*  10.01.2025  F36K910967  Apichat Ch. 420000106 - Add t-code on history log
*                                      ( Activity log )
*-----------------------------------------------------------------------
*  31.03.2025  F36K917933  Apichat Ch.
* 420000577
*  01 - Fix clearing logic
* No ticket
*  02 - Adjust query to skip inv ref check
* No ticket
*  03 - Adjust selection screen layout
* 4200004366 / 420000472 / 420000473
*  04 - Adjust posting logic for witholoding tax
* 420000577
*  05 - If the transaction is already cleared, you must create a new one;
*       it cannot be reprocessed.
*-----------------------------------------------------------------------
*  15.07.2025  F36K921253  Boontip R. 420000700 - WHT tax field
*                                    select WITH_ITEM only R1
*-----------------------------------------------------------------------
*  15.07.2025  F36K921347  Boontip R. 420000700 - WHT tax field
*                                    add checking if wt_withcd = blank
*-----------------------------------------------------------------------

REPORT zsdsfir0240.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE zsdsfir0240_top.

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
  PERFORM f_set_sel_screen.

AT SELECTION-SCREEN.
*  PERFORM f_set_tab. "<<F36K917933 - 03 del
  PERFORM f_chk_sel_screen.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.
      PERFORM f_init_var.
      PERFORM f_get_data.
      PERFORM f_lock_tran.
      PERFORM f_post_document USING    gc_mode-test_run
                                       'N'
                              CHANGING gt_output.
"<<F36K917933 - 03 begin of del
*    WHEN gc_tab_sel-new_entry.
*      PERFORM f_get_data_new_entry.
*      PERFORM f_lock_tran_new_entry.
*      PERFORM f_post_document_new_entry USING gc_mode-test_run
*                                        CHANGING gt_output_new.
"<<F36K917933 - 03 end of del
  ENDCASE.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.
      PERFORM f_display_result USING gt_output.
"<<F36K917933 - 03 begin of del
*    WHEN gc_tab_sel-new_entry.
*      PERFORM f_display_result_new USING gt_output_new.
"<<F36K917933 - 03 end of del
  ENDCASE.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE zsdsfir0240_alv   ##INCL_OK.
  INCLUDE zsdsfir0240_f01   ##INCL_OK.
  INCLUDE zsdsfir0240_f02   ##INCL_OK.
