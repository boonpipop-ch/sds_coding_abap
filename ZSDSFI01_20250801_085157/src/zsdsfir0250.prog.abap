*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250
*  Creation Date      : 21.05.2024
*  Author             : Apichat Ch.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program mass update AR Invoice document  for settlement
*                       status
*                       RICEP ZFIARE005
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
* 03.01.2025  F36K910583  Apichat Ch.  - Skip enqueue on display mode
*-----------------------------------------------------------------------
* 10.01.2025  F36K910967  Apichat Ch.  420000106
*                                      - Add t-code on history log ( Activity log )
*-----------------------------------------------------------------------
* 13.01.2025  F36K910991  Apichat Ch.  420000137
*                                      - Adjust partial to changeable
*                                      - Adjust memo fiscal year
*-----------------------------------------------------------------------
* 17.01.2025  F36K911258  Apichat Ch.  420000188
*                                      - Fix witholding tax calculation logic
*-----------------------------------------------------------------------
* 29.01.2025  F36K911749  Apichat Ch.  420000338
*                                      - Fix missing billing date
*-----------------------------------------------------------------------
* 30.01.2025  F36K911780  Apichat Ch.  420000165
*                                      - Fix missing sale order data
*-----------------------------------------------------------------------
* 14.02.2025  F36K912524  Apichat Ch.  420000378
*                                      - Fix missing leading zero
*-----------------------------------------------------------------------
* 27.03.2025  F36K914812  Apichat Ch.  420000467
* - 01. Hide delete button for change mode ( Use delete on change activity bill only )
* - 02. Adjust retrive sale order, Customer Reference logic
* - 03. Add selection payment method
* - 04. Get personal no., name from bill placement for activity bill
* - 05. Fix reversal document link
* - 06. Adjust activity bill workdate = bill placement date
* - 07. Adjust get one-time customer from payee data for DO in activity bill
* - 08. Fix remaining amt calculation
*-----------------------------------------------------------------------
* 20.05.2025  F36K917931  Apichat Ch.
* 420000467
* - 01. Set memory id for default value in ZSDSFIR0240
* No log..
* - 02. Adjust Selection screen layout to prevent scroll bar
* 420000577
* - 03. Add reversal flag, reversal doc for activity bill
* - 04. Activity bill cannot be deleted if the invoice is already cleared with a clearing document.
* - 05. Collector log cannot change if transaction already cleared
* 420000165
* - 06. Adjust logic 'sale document' for billing type service / MA
* - 07. Add action type , status data from Invoice Check
* 420000159
* - 08. Fix missing item from bill placement program
* - 09. Add per id, name from bill placement
*-----------------------------------------------------------------------
* 17.06.2025 F36K919549 Apichat Ch.
* 420000165
* - 01. Adjust activity bill to get all historical data
*-----------------------------------------------------------------------
* 24.06.2025 F36K919968 Apichat Ch.
* - 01. Add per id, name, working date from invoice check
*-----------------------------------------------------------------------
* 04.07.2025 F36K920651 Apichat Ch.
* - 01. Bug fixing activity bill report
*-----------------------------------------------------------------------


REPORT zsdsfir0250.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE zsdsfir0250_top ##INCL_OK.
INCLUDE zsdsfir0250_sel.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_authorize_check USING sy-tcode. "gc_tcode.
  PERFORM f_set_selscr_default.
  PERFORM f_init_global_var.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_hide_sel_tab.
  PERFORM f_set_sel_screen.

AT SELECTION-SCREEN.
  PERFORM f_set_tab.
  PERFORM f_chk_sel_screen.

AT SELECTION-SCREEN ON BLOCK b1.
  PERFORM f_chk_sel_screen_block_b1.

AT SELECTION-SCREEN ON BLOCK bn1.
  PERFORM f_chk_sel_screen_block_bn1.

AT SELECTION-SCREEN ON BLOCK bu1.
  PERFORM f_chk_sel_screen_block_bu1.

AT SELECTION-SCREEN ON BLOCK bm1.
  PERFORM f_chk_sel_screen_block_bm1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  PERFORM f_variant_f4 CHANGING p_var.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_nvar.
  PERFORM f_variant_f4 CHANGING p_nvar.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_uvar.
  PERFORM f_variant_f4 CHANGING p_uvar.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_mvar.
  PERFORM f_variant_f4 CHANGING p_mvar.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_hvar.
  PERFORM f_variant_f4 CHANGING p_hvar.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.
      PERFORM f_init_var.
      PERFORM f_get_open_items.
      PERFORM f_lock_tran USING gv_data_type gt_output.
    WHEN gc_tab_sel-new_entry.
      PERFORM f_init_var_new_entry.
      PERFORM f_get_open_items_new_entry.
      PERFORM f_lock_tran_new_entry USING gt_output_new.
    WHEN gc_tab_sel-update_bank.
      PERFORM f_init_var.
      PERFORM f_get_open_items_update_bank.
      PERFORM f_lock_tran USING gv_data_type gt_output.
    WHEN gc_tab_sel-memo.
      PERFORM f_init_var_memo.
      PERFORM f_get_open_items_memo.
      PERFORM f_lock_tran USING gv_data_type gt_output.
    WHEN gc_tab_sel-history_log.
      PERFORM f_init_var .
      PERFORM f_get_history_log.
      PERFORM f_lock_update_log USING gt_bplog.
  ENDCASE.

*-Beg of INS by Jutamas Y.
  PERFORM f_get_fidoc .
*-End of INS by Jutamas Y.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.
      PERFORM f_display_result USING p_var gt_output.
    WHEN gc_tab_sel-new_entry.
      PERFORM f_display_result_new USING gt_output_new.
    WHEN gc_tab_sel-update_bank.
      PERFORM f_display_result USING p_uvar gt_output.
    WHEN gc_tab_sel-memo.
      PERFORM f_display_result_memo USING gt_output.
    WHEN gc_tab_sel-history_log.
      PERFORM f_display_log USING gt_history_log.
  ENDCASE.
*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE zsdsfir0250_alv ##INCL_OK.
  INCLUDE zsdsfir0250_f00 ##INCL_OK.
  INCLUDE zsdsfir0250_f01 ##INCL_OK.
  INCLUDE zsdsfir0250_f02 ##INCL_OK.
  INCLUDE zsdsfir0250_f03 ##INCL_OK.
  INCLUDE zsdsfir0250_f04 ##INCL_OK.
  INCLUDE zsdsfir0250_f05 ##INCL_OK.
