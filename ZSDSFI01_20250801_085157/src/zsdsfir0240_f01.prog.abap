*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0240_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_set_sel_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_sel_screen .
  LOOP AT SCREEN.
    CASE gc_true.
      WHEN rb_cust.
        IF screen-group1 = 'BIL' OR
           screen-group1 = 'INV'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN rb_bill.
        IF screen-group1 = 'CUS' OR
          screen-group1 = 'INV'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN rb_inv.
        IF screen-group1 = 'CUS' OR
           screen-group1 = 'BIL'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.

    CASE gc_true.
      WHEN rb_tran OR rb_pdc OR rb_memo.
        IF screen-group1 = 'OPY'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN rb_oth.
        "Do nothing
    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_authorize_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_TCODE
*&---------------------------------------------------------------------*
FORM f_authorize_check  USING uf_tcode  TYPE  sy-tcode.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD uf_tcode.

  IF sy-subrc <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE s172(00) WITH uf_tcode.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_selscr_default
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_selscr_default .
  p_bukrs  = '1000'.
*  p_bukrsn = '1000'. "<<F36K917933 - 03 del
*  p_blartc = 'DZ'.

  p_bldatc = sy-datum.
  p_budatc = sy-datum.
  p_brnchc = '0000'.
  p_pdspgl = 'D'.

*<<F36K917933 - 03 Begin of del
*  tab_bt1           = TEXT-tb1.  "Collector log
**  tab_bt2           = TEXT-tb2.
*  tab_blk-prog      = sy-repid.
*  tab_blk-dynnr     = 100.
*  tab_blk-activetab = gc_tab_sel-collection_log.
*<<F36K917933 - 03 End of del
  gv_data_type      = gc_tab_sel-collection_log.

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = sy-repid          " ABAP Program Name
*      irt_param =                  " Parameter name
*      irt_ext   =                  " Parameter extension
    IMPORTING
      et_gen_c  = DATA(lt_genc)     " General Constants (GENC)
  ).

  LOOP AT lt_genc INTO DATA(ls_genc)  ##INTO_OK.
    CASE ls_genc-param.
      WHEN gc_genc_param-blart_memo.
        gv_blart_memo  = ls_genc-value_low.
      WHEN gc_genc_param-blart_pdc.
        gv_blart_pdc  = ls_genc-value_low.
      WHEN gc_genc_param-blart_bank_tran.
        gv_blart_bank = ls_genc-value_low.
    ENDCASE.
  ENDLOOP.

  p_blartc = gv_blart_bank.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_init_var
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_var .
  DATA:
    lt_genc          TYPE zcl_sdsca_utilities=>tt_gen_c,
    lt_action_status TYPE zcl_sdsca_utilities=>tt_gen_c.


  SELECT SINGLE waers
    INTO gv_waers
    FROM t001
    WHERE bukrs = p_bukrs.

  "Reuse GenC from ZSDSFIR0250
  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = 'ZSDSFIR0250'           " ABAP Program Name
      if_param = gc_genc_param-doc_type  " Parameter name
    IMPORTING
      et_range = gr_blart
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = 'ZSDSFIR0250'        " ABAP Program Name
      if_param = gc_genc_param-sp_gl  " Parameter name
    IMPORTING
      et_range = gr_spgl
  ).

  gr_spgl = VALUE #( BASE gr_spgl ( sign = 'I' option = 'EQ' low = '' ) ).

  "Payment method
  CASE gc_true.
    WHEN rb_tran OR rb_oth.
      zcl_sdsca_utilities=>get_gen_c_range(
        EXPORTING
          if_repid = 'ZSDSFIR0250'                  " ABAP Program Name
          if_param = gc_genc_param-pymt_meth_bank   " Parameter name
        IMPORTING
          et_range = gr_pymt
      ).
    WHEN rb_pdc.
      zcl_sdsca_utilities=>get_gen_c_range(
        EXPORTING
          if_repid = 'ZSDSFIR0250'                  " ABAP Program Name
          if_param = gc_genc_param-pymt_meth_pdc    " Parameter name
        IMPORTING
          et_range = gr_pymt
      ).
  ENDCASE.

  "Manual whtax document type
  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = 'ZSDSFIR0250'                    " ABAP Program Name
      if_param = gc_genc_param-man_whtax_doc_typ  " Parameter name
*        if_ext   =                               " Parameter Extension
    IMPORTING
      et_range = gr_whtax_blart
  ).

  "Action type / status
  DATA: lr_param TYPE zcl_sdsca_utilities=>trt_range_param.
  lr_param = VALUE #( ( sign = 'I' option = 'EQ' low = gc_genc_param-action_status ) ).

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = 'ZSDSFIR0250'     " ABAP Program Name
      irt_param = lr_param          " Parameter name
*      irt_ext   =                  " Parameter extension
    IMPORTING
      et_gen_c  = lt_action_status  " General Constants (GENC)
  ).

  "One time customer code
  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                         " ABAP Program Name
      if_param = gc_genc_param-kunnr_one_time     " Parameter name
*        if_ext   =                               " Parameter Extension
    IMPORTING
      et_range = gr_one_time
  ).

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = sy-repid          " ABAP Program Name
*      irt_param =                  " Parameter name
*      irt_ext   =                  " Parameter extension
    IMPORTING
      et_gen_c  = lt_genc                 " General Constants (GENC)
  ).

  LOOP AT lt_genc INTO DATA(ls_genc)      ##INTO_OK.
    IF ls_genc-param = gc_genc_param-memo_account.
      gv_memo_account   = ls_genc-value_low.
      "Default Account
    ELSEIF ls_genc-param CP '*ACCOUNT'.
      ASSIGN COMPONENT ls_genc-param OF STRUCTURE gs_account TO FIELD-SYMBOL(<lv_comp>).
      IF sy-subrc EQ 0.
        <lv_comp> = ls_genc-value_low.
      ENDIF.

      "Default profit/cost center
    ELSEIF ls_genc-param = gc_genc_param-cost_center OR
           ls_genc-param = gc_genc_param-profit_center OR
           ls_genc-param = gc_genc_param-order.

      READ TABLE gt_default_value
        ASSIGNING FIELD-SYMBOL(<ls_default_value>)
        WITH KEY param      = ls_genc-param
                 param_ext  = ls_genc-param_ext.
      IF sy-subrc EQ 0.
        <ls_default_value>-r_chk = VALUE #( BASE <ls_default_value>-r_chk ( sign   = ls_genc-param_sign
                                                                            option = ls_genc-param_option
                                                                            low    = ls_genc-value_low
                                                                            high   = ls_genc-value_high ) ).
      ELSE.
        APPEND INITIAL LINE TO gt_default_value ASSIGNING FIELD-SYMBOL(<ls_default>).
        <ls_default>-param      = ls_genc-param.
        <ls_default>-param_ext  = ls_genc-param_ext.
        <ls_default>-r_chk      = VALUE #( BASE <ls_default>-r_chk ( sign   = ls_genc-param_sign
                                                                     option = ls_genc-param_option
                                                                     low    = ls_genc-value_low
                                                                     high   = ls_genc-value_high ) ).
      ENDIF.
    ELSEIF ls_genc-param = gc_genc_param-post_dated_check.
      gv_post_dated_chk  = ls_genc-value_low.
    ELSEIF ls_genc-param = gc_genc_param-memo_tax_code.
      gv_memo_tax_code  = ls_genc-value_low.
    ENDIF.

  ENDLOOP.

  "Action status
  LOOP AT lt_action_status INTO ls_genc ##INTO_OK.

    READ TABLE gt_action_status
      ASSIGNING FIELD-SYMBOL(<ls_action_status>)
      WITH KEY seq = ls_genc-sequence.
    IF sy-subrc EQ 0.
      IF ls_genc-param_ext = gc_genc_param_ext-action_type.
        <ls_action_status>-action_type = ls_genc-value_low.
      ELSEIF ls_genc-param_ext = gc_genc_param_ext-status.
        <ls_action_status>-status = ls_genc-value_low.
      ENDIF.
    ELSE.
      APPEND INITIAL LINE TO gt_action_status
        ASSIGNING <ls_action_status>.
      <ls_action_status>-seq = ls_genc-sequence.
      IF ls_genc-param_ext = gc_genc_param_ext-action_type.
        <ls_action_status>-action_type = ls_genc-value_low.
      ELSEIF ls_genc-param_ext = gc_genc_param_ext-status.
        <ls_action_status>-status = ls_genc-value_low.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CASE gc_true.
    WHEN rb_cust.
      CLEAR: s_plno[],
             s_pldate[],
             s_belnr[],
             s_gjahr[].
    WHEN rb_bill.
      CLEAR: s_kunnr[],
             s_belnr[],
             s_gjahr[].
    WHEN rb_inv.
      CLEAR: s_kunnr[],
             s_plno[],
             s_pldate[].
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_open_items
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_data .
  DATA:
    lt_open_item TYPE tt_open_item,
    lt_pay_item  TYPE tt_open_item,
    lt_with_item TYPE tt_with_item,
    lt_clr_link  TYPE tt_clr_link.

  "Get open item & bill collection log
  PERFORM f_get_open_item "USING rb_bill
                           CHANGING
                             gt_coll_log
                             lt_open_item.
  "Get partial payment item
  PERFORM f_get_partial_payment_item USING lt_open_item
                                     CHANGING lt_pay_item
                                              lt_clr_link.

  "Get One-Time data
  PERFORM f_get_one_time_data CHANGING lt_open_item.
  "Get Witholding tax data
  PERFORM f_get_with_item USING lt_open_item
                          CHANGING lt_with_item.


  "Prepare output
  PERFORM f_prepare_output USING lt_open_item
                                 gt_coll_log
                                 lt_with_item
                                 lt_pay_item
                                 lt_clr_link
                           CHANGING gt_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_display_result  USING  ut_result TYPE tt_output.

* Show progress
* Text-p99 : Generating ALV Report . . .
  mc_show_progress 99 TEXT-p99.

* Set Container name
  gf_container = 'CTL_ALV'.

  gv_edit = gc_true.

* Disable Header area
*  gf_alv_header = gc_true.

*   No auto refresh in edit mode
  gf_no_auto_refresh = gc_true.

* ALV Layout
  PERFORM f_alv_layout CHANGING gs_layout
                                gs_variant
                                gs_print.

* Assign Output Data
* Assign Size
  gf_header_hight = gc_header_height.
  gf_alv_height   = gc_alv_height.
  ASSIGN ut_result TO <g_list>.                       "#EC CI_FLDEXT_OK
* Build Field cat
  PERFORM f_alv_build_fieldcat CHANGING gt_fieldcat.
* Sort data
  PERFORM f_alv_sort_result CHANGING gt_sort.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM f_alv_layout CHANGING cs_layout  TYPE  lvc_s_layo
                           cs_variant TYPE  disvariant
                           cs_print   TYPE  lvc_s_prnt.

* Initialize Output
  CLEAR:  cs_layout, cs_variant, cs_print.

* determine layout
  cs_layout-sel_mode   = 'B'. "Multiple Selection with Push Box
  cs_layout-cwidth_opt = gc_true.
  cs_layout-zebra      = gc_true.
  cs_layout-no_rowmark = gc_true.
  cs_layout-stylefname = 'CELLTAB'.

* For Variant Saving
  cs_variant-report  = sy-repid.

  cs_print-no_colwopt = gc_true.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM f_alv_build_fieldcat CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  DATA:
    lv_structure   TYPE tabname.


  CASE gc_true.
    WHEN rb_tran OR rb_oth.
      lv_structure = gc_structure_tran.
    WHEN rb_pdc OR rb_memo.
      lv_structure = gc_structure_pdc.
  ENDCASE.

* Build Field cat from Structure.
  PERFORM f_prepare_fieldcat_o  USING  lv_structure
                              CHANGING ct_fieldcat.

  CASE gc_true.
    WHEN rb_tran OR rb_oth.
      PERFORM f_adjust_fieldcat_tran CHANGING ct_fieldcat.
    WHEN rb_pdc OR rb_memo.
      PERFORM f_adjust_fieldcat_pdc CHANGING ct_fieldcat.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ADJUST_FIELDCAT_TRAN
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM f_adjust_fieldcat_tran CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <l_fieldcat>   TYPE  lvc_s_fcat.

  LOOP AT ct_fieldcat ASSIGNING <l_fieldcat>.
    CASE <l_fieldcat>-fieldname.
      WHEN 'SEL'.
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-col_pos    = 1.
        <l_fieldcat>-key        = gc_true.
      WHEN 'STATUS_ICON'.
        <l_fieldcat>-icon       = gc_true.
      WHEN 'BELNR'.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'BELNR_OPN'.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'GJAHR_OPN'.
        <l_fieldcat>-no_out     = gc_true.
*      WHEN 'BILL_COL_FLAG'.
*        <l_fieldcat>-tech       = gc_true.
      WHEN 'XBLNR'.
        <l_fieldcat>-seltext    = TEXT-f11.
        <l_fieldcat>-scrtext_m  = TEXT-f11.
        <l_fieldcat>-scrtext_l  = TEXT-f11.

      WHEN 'KURSF'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'HBKID' OR
           'HKTID'.
        <l_fieldcat>-no_out     = gc_true.
*      WHEN 'RETENTION'.
*        <l_fieldcat>-tech       = gc_true.
*<<F36K917933 - 04 Begin of ins
      WHEN 'WT_QBSHB_I' OR
           'WT_QSSHH'   OR
           'WT_QSSHB'.
        <l_fieldcat>-tech       = gc_true.
*<<F36K917933 - 04 End of ins
      WHEN 'SORT_FIELD'.
        <l_fieldcat>-tech       = gc_true.
      WHEN 'TOTAL_BIL'.
        <l_fieldcat>-tech       = gc_true.
      WHEN 'BAL_AMT'    OR
           'EXPS_AMT'   OR
           'FEE'        OR
           'RETENTION'  OR
           'INCOME_AMT' OR
           'CASH_CON'   OR
           'NET'.
        <l_fieldcat>-do_sum     = gc_true.
      WHEN 'REMARK'.
        <l_fieldcat>-edit       = gc_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.


*----------------------------------------------------------------------*
*  Form F_ADJUST_FIELDCAT_PDC
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM f_adjust_fieldcat_pdc CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <l_fieldcat>   TYPE  lvc_s_fcat.

  LOOP AT ct_fieldcat ASSIGNING <l_fieldcat>.
    CASE <l_fieldcat>-fieldname.
      WHEN 'SEL'.
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-col_pos    = 1.
        <l_fieldcat>-key        = gc_true.
      WHEN 'STATUS_ICON'.
        <l_fieldcat>-icon       = gc_true.
      WHEN 'BELNR'.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'BELNR_OPN'.
        IF rb_memo IS INITIAL.
          <l_fieldcat>-hotspot    = gc_true.
        ENDIF.
      WHEN 'GJAHR_OPN'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'WRBTR'      OR
           'DMBTR'      OR
           'FEE'        OR
           'RETENTION'  OR
           'INCOME_AMT' OR
           'PAYIN_AMT'  OR
           'NET'.
        <l_fieldcat>-do_sum     = gc_true.
*<<F36K917933 - 04 Begin of ins
      WHEN 'WT_QBSHB_I' OR
           'WT_QSSHH'   OR
           'WT_QSSHB'.
        <l_fieldcat>-tech       = gc_true.
*<<F36K917933 - 04 End of ins
      WHEN 'REMARK'.
        <l_fieldcat>-edit       = gc_true.
      WHEN 'CHEQUE_NO'.
        <l_fieldcat>-seltext    = TEXT-f01.
        <l_fieldcat>-scrtext_s  = TEXT-f01.
        <l_fieldcat>-scrtext_m  = TEXT-f01.
        <l_fieldcat>-scrtext_l  = TEXT-f01.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM f_alv_sort_result  CHANGING ct_sort TYPE lvc_t_sort.

  CONSTANTS:
    lc_sort1 TYPE  lvc_s_sort-fieldname VALUE 'SEL',
    lc_sort2 TYPE  lvc_s_sort-fieldname VALUE 'BILLPL_NO',
    lc_sort3 TYPE  lvc_s_sort-fieldname VALUE 'KUNNR'.

  DATA:
    ls_sort  TYPE  lvc_s_sort.

* Initialize Output
  CLEAR: ct_sort.

  "Sort by check box
  CLEAR ls_sort.
  ls_sort-spos      = 1.
  ls_sort-fieldname = lc_sort1.
  ls_sort-up        = space.
  ls_sort-down      = gc_true.
  ls_sort-subtot    = gc_true.
  APPEND ls_sort TO ct_sort.

  "Sort by bill placement no
  CLEAR ls_sort.
  ls_sort-spos      = 2.
  ls_sort-fieldname = lc_sort2.
  ls_sort-up        = gc_true.
  ls_sort-down      = space.
  ls_sort-subtot    = gc_true.
  APPEND ls_sort TO ct_sort.

  "Sort by customer code
  CLEAR ls_sort.
  ls_sort-spos      = 3.
  ls_sort-fieldname = lc_sort3.
  ls_sort-up        = gc_true.
  ls_sort-down      = space.
  ls_sort-subtot    = gc_true.
  APPEND ls_sort TO ct_sort.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_user_command
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_ucomm
*&---------------------------------------------------------------------*
FORM f_user_command USING uf_ucomm  TYPE  sy-ucomm ##CALLED.
  DATA:
    lv_tran_mode TYPE rfpdo-allgazmd,
    lv_sel       TYPE char01.

  CASE gv_data_type.
*--------------------------------------------------------------------*
* Collection log
*--------------------------------------------------------------------*
    WHEN gc_tab_sel-collection_log.

      CASE uf_ucomm.
        WHEN gc_func-sel_a.
          lv_sel = gc_true.

          PERFORM f_mark_sel_all USING    lv_sel
                                 CHANGING gt_output.

          gref_grid->refresh_table_display( ).

        WHEN gc_func-sel_n.
          lv_sel = ''.

          PERFORM f_mark_sel_all USING    lv_sel
                                 CHANGING gt_output.

          gref_grid->refresh_table_display( ).

        WHEN gc_func-auto_exe.
          lv_tran_mode = 'N'. "Processing without display of the screens.

          PERFORM f_post_document USING     gc_mode-prod_run
                                            lv_tran_mode
                                  CHANGING  gt_output.

          gref_grid->refresh_table_display( ).

        WHEN gc_func-man_exe.
          lv_tran_mode = 'A'. "Processing with display of the screens

          PERFORM f_post_document USING     gc_mode-prod_run
                                            lv_tran_mode
                                  CHANGING  gt_output.

          gref_grid->refresh_table_display( ).

        WHEN gc_func-reject.

          PERFORM f_reject_data CHANGING gt_output.

          gref_grid->refresh_table_display( ).

        WHEN OTHERS.
          "Do nothing
      ENDCASE.

*<<F36K917933 - 03 begin of del
**--------------------------------------------------------------------*
** New entry
**--------------------------------------------------------------------*
*    WHEN gc_tab_sel-new_entry.
*      CASE uf_ucomm.
*        WHEN gc_func-sel_a.
*          lv_sel = gc_true.
*
*          PERFORM f_mark_sel_all_new  USING    lv_sel
*                                      CHANGING gt_output_new.
*
*          gref_grid->refresh_table_display( ).
*
*        WHEN gc_func-sel_n.
*          lv_sel = ''.
*
*          PERFORM f_mark_sel_all_new  USING    lv_sel
*                                      CHANGING gt_output_new.
*
*          gref_grid->refresh_table_display( ).
*
*        WHEN gc_func-auto_exe.
*
*          PERFORM f_post_document_new_entry
*                                  USING     gc_mode-prod_run
*                                  CHANGING  gt_output_new.
*
*          gref_grid->refresh_table_display( ).
*
*        WHEN gc_func-reject.
*
*          PERFORM f_reject_data_new CHANGING gt_output_new.
*
*          gref_grid->refresh_table_display( ).
*
*      ENDCASE.
*<<F36K917933 - 03 end of del
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_on_data_changed_finished
*----------------------------------------------------------------------*
*       ALV ON_DATA_CHANGED_FINISHED Event
*----------------------------------------------------------------------*
FORM f_on_data_changed_finished USING                           ##CALLED
                                  uf_modified   TYPE  char01    ##NEEDED
                                  ut_good_cells TYPE  lvc_t_modi.

  DATA: ls_stable  TYPE lvc_s_stbl,
        lv_refresh TYPE abap_bool.

  LOOP AT ut_good_cells INTO DATA(ls_good)  ##INTO_OK.

    READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<ls_output>)
      INDEX ls_good-row_id.

    CHECK sy-subrc EQ 0.

    CHECK <ls_output> IS ASSIGNED.

    CASE ls_good-fieldname.
      WHEN 'SEL'.
        lv_refresh = abap_true.
      WHEN 'REMARK'.
        PERFORM f_update_remark USING <ls_output>.
    ENDCASE.

  ENDLOOP.

  IF lv_refresh = abap_true.
*    ls_stable-row = gc_true.
    ls_stable-col = gc_true.

    gref_grid->refresh_table_display(
     is_stable = ls_stable
     ).
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR
*----------------------------------------------------------------------*
*  Event ALV Toolbar
*----------------------------------------------------------------------*
FORM f_handle_toolbar USING uref_object TYPE REF TO	cl_alv_event_toolbar_set ##CALLED
                              uf_interactive TYPE	char01 ##NEEDED.

  DATA: ls_toolbar TYPE stb_button.

* Handle Toolbar as needed
  IF gv_edit EQ gc_true.
    DELETE uref_object->mt_toolbar WHERE function EQ '&CHECK'.
*    DELETE uref_object->mt_toolbar WHERE function EQ '&REFRESH'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&&SEP01'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&CUT'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&COPY'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&PASTE'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&UNDO'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&&SEP02'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&APPEND'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&DELETE_ROW'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&COPY_ROW'.
    DELETE uref_object->mt_toolbar WHERE function EQ '&&SEP03'.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '3'.   "separator
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    "Select All
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '0'.   "normal Button
    ls_toolbar-function = gc_func-sel_a.   "fcode
    ls_toolbar-icon = '@4B@'.
*    ls_toolbar-text = TEXT-b03.
    ls_toolbar-quickinfo = TEXT-b03.
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    "Select None
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '0'.   "normal Button
    ls_toolbar-function = gc_func-sel_n.   "fcode
    ls_toolbar-icon = '@4D@'.
*    ls_toolbar-text = TEXT-b04.
    ls_toolbar-quickinfo = TEXT-b04.
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '3'.   "separator
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    "Post Online
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '0'.   "normal Button
    ls_toolbar-function = gc_func-auto_exe.   "fcode
    ls_toolbar-icon = '@15@'.
    ls_toolbar-text = TEXT-b01.
    ls_toolbar-quickinfo = TEXT-b01.
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    IF gv_data_type <> gc_tab_sel-new_entry.
      " Manual Adjust
      CLEAR ls_toolbar.
      ls_toolbar-butn_type = '0'.   "normal Button
      ls_toolbar-function = gc_func-man_exe.   "fcode
      ls_toolbar-icon = '@15@'.
      ls_toolbar-text = TEXT-b02.
      ls_toolbar-quickinfo = TEXT-b02.
      APPEND ls_toolbar TO uref_object->mt_toolbar.
    ENDIF.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '3'.   "separator
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    " Reject
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '0'.   "normal Button
    ls_toolbar-function = gc_func-reject.   "fcode
    ls_toolbar-icon = '@8Y@'.
    ls_toolbar-text = TEXT-b05.
    ls_toolbar-quickinfo = TEXT-b05.
    APPEND ls_toolbar TO uref_object->mt_toolbar.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_lock_tran
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_lock_tran .

  LOOP AT gt_output INTO DATA(ls_output)  ##INTO_OK
    GROUP BY ( key1 = ls_output-kunnr
               key2 = ls_output-belnr_opn
               key3 = ls_output-gjahr_opn ).

    CALL FUNCTION 'ENQUEUE_EZSDSFIS090'
      EXPORTING
        mode_zsdsfis090 = 'E'
        kunnr           = ls_output-kunnr
        bukrs           = p_bukrs
        belnr           = ls_output-belnr_opn
        gjahr           = ls_output-gjahr_opn
        _collect        = 'X'
      EXCEPTIONS
        foreign_lock    = 1
        system_failure  = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.
  CALL FUNCTION 'FLUSH_ENQUEUE'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_unlock_tran
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_unlock_tran .

  LOOP AT gt_output INTO DATA(ls_output)  ##INTO_OK
    GROUP BY ( key1 = ls_output-kunnr
               key2 = ls_output-belnr_opn
               key3 = ls_output-gjahr_opn ).

    CALL FUNCTION 'DEQUEUE_EZSDSFIS090'
      EXPORTING
        mode_zsdsfis090 = 'E'
        mandt           = sy-mandt
        kunnr           = ls_output-kunnr
        bukrs           = p_bukrs
        belnr           = ls_output-belnr_opn
        gjahr           = ls_output-gjahr_opn
*       _SCOPE          = '3'
*       _SYNCHRON       = ' '
*       _COLLECT        = ' '
      .
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_document
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&      --> GC_MODE_TEST_RUN
*&---------------------------------------------------------------------*
FORM f_post_document  USING    uf_mode      TYPE char1
                               uf_proc_mode TYPE rfpdo-allgazmd
                      CHANGING ct_result    TYPE tt_output.

  DATA:
    ls_clearing TYPE gty_output.

  DATA(lt_group) = ct_result.

  IF uf_mode = gc_mode-test_run.
    DELETE lt_group WHERE status_icon = gc_status-error.
    DELETE lt_group WHERE net IS INITIAL.
  ELSE.   "gc_mode-prod_run
    DELETE lt_group WHERE sel IS INITIAL.
  ENDIF.

  SORT lt_group BY sort_field.
  DELETE ADJACENT DUPLICATES FROM lt_group COMPARING sort_field.

  IF rb_memo IS INITIAL.

    LOOP AT lt_group ASSIGNING FIELD-SYMBOL(<ls_group>).

      ls_clearing = CORRESPONDING #( <ls_group> ).

      PERFORM f_posting_interface_start USING uf_proc_mode.

      PERFORM f_post_interface_clearing USING uf_mode
                                        CHANGING ls_clearing
                                                 ct_result.

      PERFORM f_posting_interface_end.

      PERFORM f_update_status USING uf_mode
                                    ls_clearing
                              CHANGING ct_result.

    ENDLOOP.

  ELSE.

    LOOP AT lt_group ASSIGNING FIELD-SYMBOL(<ls_group_memo>).
      SELECT SINGLE hkont
        INTO @<ls_group_memo>-hkont
        FROM t012k
        WHERE bukrs = @p_bukrs
        AND   hbkid = @<ls_group_memo>-hbkid
        AND   hktid = @<ls_group_memo>-hktid.

      <ls_group_memo>-mwskz = gv_memo_tax_code.

      PERFORM f_posting_document_memo USING uf_mode
                                            uf_proc_mode
                                      CHANGING <ls_group_memo>.

      PERFORM f_update_status         USING uf_mode
                                            <ls_group_memo>
                                      CHANGING ct_result.
*
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f_update_status
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&      --> GC_MODE_TEST_RUN
*&---------------------------------------------------------------------*
FORM f_update_status USING
                        uf_mode     TYPE char01
                        uf_clearing TYPE gty_output
                     CHANGING
                        ct_items    TYPE tt_output.

  DATA: lt_celltab        TYPE lvc_t_styl,
        lt_clr_link       TYPE tt_clr_link,
        lt_bank_statement TYPE tt_bank_statement.

  IF uf_mode = gc_mode-test_run.
    LOOP AT ct_items ASSIGNING FIELD-SYMBOL(<ls_test>)
      WHERE sort_field = uf_clearing-sort_field
      AND   belnr_opn  IS NOT INITIAL
      AND   net        IS NOT INITIAL.

      <ls_test>-sel         = ''.
      <ls_test>-status_icon = uf_clearing-status_icon.
      <ls_test>-status_msg  = uf_clearing-status_msg.
      <ls_test>-belnr       = uf_clearing-belnr.
      <ls_test>-gjahr       = uf_clearing-gjahr.
    ENDLOOP.
  ELSE.

    PERFORM f_fill_celltab USING 'RO'
                         CHANGING lt_celltab.

    LOOP AT ct_items ASSIGNING FIELD-SYMBOL(<ls_result>)
      WHERE sort_field = uf_clearing-sort_field
      AND   belnr_opn  IS NOT INITIAL
      AND   sel        = gc_true.

      <ls_result>-sel         = ''.
      <ls_result>-status_icon = uf_clearing-status_icon.
      <ls_result>-status_msg  = uf_clearing-status_msg.
      <ls_result>-belnr       = uf_clearing-belnr.
      <ls_result>-gjahr       = uf_clearing-gjahr.
      <ls_result>-celltab     = lt_celltab.

      IF uf_clearing-belnr IS NOT INITIAL AND
         uf_clearing-gjahr IS NOT INITIAL.

        APPEND INITIAL LINE TO lt_clr_link ASSIGNING FIELD-SYMBOL(<ls_clr_link>).
        <ls_clr_link>-data_type   = <ls_result>-data_type.
        <ls_clr_link>-bukrs       = p_bukrs.
        <ls_clr_link>-belnr       = <ls_result>-belnr_opn.
        <ls_clr_link>-gjahr       = <ls_result>-gjahr_opn.
        <ls_clr_link>-buzei       = <ls_result>-buzei_opn.
        <ls_clr_link>-seq         = <ls_result>-seq.
        <ls_clr_link>-bukrs_clr   = p_bukrs.
        <ls_clr_link>-belnr_clr   = <ls_result>-belnr.
        <ls_clr_link>-gjahr_clr   = <ls_result>-gjahr.
        <ls_clr_link>-billpl_no   = <ls_result>-billpl_no.
        <ls_clr_link>-billpl_date = <ls_result>-billpl_date.
        <ls_clr_link>-wrbtr       = <ls_result>-bal_amt.

        IF rb_memo IS INITIAL.    "<<F36K910747 ins++

          APPEND INITIAL LINE TO lt_bank_statement ASSIGNING FIELD-SYMBOL(<ls_bank>).
          <ls_bank>-hbkid           = <ls_result>-hbkid.
          <ls_bank>-zbank_item      = <ls_result>-zbank_item.
          <ls_bank>-fi_clearing_no  = <ls_result>-belnr.
          <ls_bank>-fyear_clearing  = <ls_result>-gjahr.
          <ls_bank>-clearing_user   = sy-uname.
          <ls_bank>-date_clear      = sy-datum.
          <ls_bank>-time_clear      = sy-uzeit.

          <ls_bank>-col_ind-fi_clearing_no  = gc_true.
          <ls_bank>-col_ind-fyear_clearing  = gc_true.
          <ls_bank>-col_ind-clearing_user   = gc_true.
          <ls_bank>-col_ind-date_clear      = gc_true.
          <ls_bank>-col_ind-time_clear      = gc_true.

        ENDIF.                  "<<F36K910747 ins++
      ENDIF.

    ENDLOOP.
  ENDIF.

  IF lt_clr_link IS NOT INITIAL.
    MODIFY zsdsfit037 FROM TABLE lt_clr_link.
  ENDIF.

  IF lt_bank_statement IS NOT INITIAL.
    SORT lt_bank_statement BY hbkid zbank_item trnfer_number DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_bank_statement COMPARING hbkid	zbank_item.

    UPDATE zsdsfit042
      FROM TABLE @lt_bank_statement INDICATORS SET STRUCTURE col_ind.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_posting_interface_start
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_posting_interface_start USING VALUE(uf_proc_mode) TYPE rfpdo-allgazmd.

  CALL FUNCTION 'Z_SDSFI_POSTING_INTERFACE_ST'
*  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_function         = 'C'
*     I_GROUP            = ' '
*     I_KEEP             = ' '
      i_mode             = uf_proc_mode
*     I_UPDATE           = 'S'
*     I_USER             = ' '
*     I_XBDCC            = ' '
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      user_invalid       = 6
      OTHERS             = 7.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_posting_interface_end
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_posting_interface_end .

  CALL FUNCTION 'Z_SDSFI_POSTING_INTERFACE_END'
*  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = gc_true
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_interface_clearing
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_post_interface_clearing  USING    uf_mode TYPE char1
                                CHANGING cs_clearing TYPE gty_output
                                         ct_items    TYPE tt_output ##NEEDED.

  DATA:
    lv_auglv   TYPE t041a-auglv,
    lv_accept  TYPE char01,       "<<F36K917933 - 04 insert
    lv_msgid   TYPE sy-msgid,
    lv_msgno   TYPE sy-msgno,
    lv_msgty   TYPE sy-msgty,
    lv_msgv1   TYPE sy-msgv1,
    lv_msgv2   TYPE sy-msgv2,
    lv_msgv3   TYPE sy-msgv3,
    lv_msgv4   TYPE sy-msgv4,
    lv_subrc   TYPE sy-subrc,
*    lv_total_amt TYPE zsdsfis092-net,
    lv_item_no TYPE syst_tabix,
    ls_total   TYPE gty_total,

    lt_blntab  TYPE STANDARD TABLE OF blntab,
    lt_clear   TYPE STANDARD TABLE OF ftclear,
    lt_post    TYPE STANDARD TABLE OF ftpost,
    lt_tax     TYPE STANDARD TABLE OF fttax,
    lt_partial TYPE tt_partial.

  "Skip test run for all invalid data
  IF uf_mode = gc_mode-test_run.
    LOOP AT ct_items
      TRANSPORTING NO FIELDS
      WHERE status_icon <> gc_status-error.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ENDIF.

  SELECT SINGLE hkont
    INTO @DATA(lv_hkont)
    FROM t012k
    WHERE bukrs = @p_bukrs
    AND   hbkid = @cs_clearing-hbkid
    AND   hktid = @cs_clearing-hktid.

  PERFORM f_post_prepare_header USING cs_clearing
                        CHANGING lt_post.

  IF uf_mode = gc_mode-test_run.
    LOOP AT ct_items INTO DATA(ls_item)
      WHERE sort_field = cs_clearing-sort_field
      AND   belnr_opn  IS NOT INITIAL
      AND   net        IS NOT INITIAL
      AND   status_icon <> gc_status-error.

      ls_total-net        += ls_item-net.
      ls_total-exp_amt    += ls_item-exps_amt.
      ls_total-fee        += ls_item-fee.
      ls_total-retention  += ls_item-retention.
*      ls_total-refund_amt += ls_item-refund_amt.
      ls_total-income_amt += ls_item-income_amt.
      ls_total-cash_con   += ls_item-cash_con.

*      IF ls_item-blart_opn IN gr_whtax_blart.      "DEL- IMS 420000142 24/12/2024
      IF ls_item-wt_qsshb_i IS INITIAL               "<<F36K917933 - 04 ins
      OR ls_item-wt_withcd IS INITIAL.                      "420000700+
        ls_total-whtax_amt  += ls_item-wt_qbshb.
      ENDIF.

      lv_item_no   += 1.

      PERFORM f_post_prepare_ftclear USING
                                       lv_item_no
                                       ls_item
                                     CHANGING
                                       lt_clear
                                       lt_partial.

    ENDLOOP.
  ELSE.
    LOOP AT ct_items INTO ls_item
      WHERE sort_field = cs_clearing-sort_field
      AND   belnr_opn  IS NOT INITIAL
      AND   net        IS NOT INITIAL
      AND   sel        = gc_true.

      ls_total-net        += ls_item-net.
      ls_total-exp_amt    += ls_item-exps_amt.
      ls_total-fee        += ls_item-fee.
      ls_total-retention  += ls_item-retention.
*      ls_total-refund_amt += ls_item-refund_amt.
      ls_total-income_amt += ls_item-income_amt.
      ls_total-cash_con   += ls_item-cash_con.

*      IF ls_item-blart_opn IN gr_whtax_blart.      "DEL- IMS 420000142 24/12/2024
      IF ls_item-wt_qsshb_i IS INITIAL                "<<F36K917933 - 04 ins
      OR ls_item-wt_withcd IS INITIAL.                 "420000700+
        ls_total-whtax_amt  += ls_item-wt_qbshb.
      ENDIF.

      lv_item_no   += 1.

      PERFORM f_post_prepare_ftclear USING
                                       lv_item_no
                                       ls_item
                                     CHANGING
                                       lt_clear
                                       lt_partial.

    ENDLOOP.
  ENDIF.

  CASE gc_true.
    WHEN rb_tran OR rb_oth.
      PERFORM f_post_prepare_ftpost_tran
                                   USING  lv_hkont
                                          ls_total
                                          cs_clearing
                                   CHANGING
                                          lt_post.
    WHEN rb_pdc.
      PERFORM f_post_prepare_ftpost_pdc
                                   USING
                                          ls_total
                                          cs_clearing
                                   CHANGING
                                          lt_post.

  ENDCASE.

  CASE gc_true.
    WHEN rb_pdc.
      lv_auglv = 'UMBUCHNG'.
    WHEN rb_tran OR rb_oth.
      lv_auglv = 'EINGZAHL'.
**<<F36K917933 - 04 begin of insert  "accept button on popup
*      IF ls_total-whtax_amt IS NOT INITIAL.
*        lv_accept = gc_true.
*      ENDIF.
**<<F36K917933 - 04 end of insert  "accept button on popup
  ENDCASE.

  CALL FUNCTION 'Z_SDSFI_POSTING_INTF_CLR_PART'
    EXPORTING
      i_auglv                    = lv_auglv
      i_tcode                    = 'FB05'
      i_xsimu                    = uf_mode
      i_accept                   = lv_accept
    IMPORTING
      e_msgid                    = lv_msgid
      e_msgno                    = lv_msgno
      e_msgty                    = lv_msgty
      e_msgv1                    = lv_msgv1
      e_msgv2                    = lv_msgv2
      e_msgv3                    = lv_msgv3
      e_msgv4                    = lv_msgv4
      e_subrc                    = lv_subrc
    TABLES
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_clear
      t_ftpost                   = lt_post
      t_fttax                    = lt_tax
      t_partial                  = lt_partial
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.

  IF sy-subrc <> 0 OR lv_subrc <> 0.

    IF uf_mode = gc_mode-test_run AND
     ( lv_msgid EQ '00' AND
       lv_msgno EQ '344' AND
       lv_msgv1 EQ 'SAPMF05A' AND
       lv_msgv2 EQ '0700' ).
      cs_clearing-status_msg  = TEXT-i01.
      cs_clearing-status_icon = gc_status-warning.
    ELSEIF lv_msgty CN 'EAX'.
      IF lv_msgid IS NOT INITIAL AND
         lv_msgty IS NOT INITIAL AND
         lv_msgno IS NOT INITIAL.
        MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
          WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO cs_clearing-status_msg.
      ENDIF.

      cs_clearing-status_icon = gc_status-warning.
    ELSEIF lv_msgty IS NOT INITIAL.
      MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
        WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO cs_clearing-status_msg.

      cs_clearing-status_icon = gc_status-error.
    ENDIF.
  ELSE.
    IF uf_mode = gc_mode-test_run.
      cs_clearing-status_msg  = TEXT-i01.
      cs_clearing-status_icon = gc_status-warning.
    ELSE.
      READ TABLE lt_blntab INTO DATA(ls_bln) INDEX 1.
      IF sy-subrc EQ 0.
        cs_clearing-status_msg  = TEXT-i02.
        cs_clearing-status_icon = gc_status-success.
        cs_clearing-belnr = ls_bln-belnr.
        cs_clearing-gjahr = ls_bln-gjahr.
      ELSE.
        cs_clearing-status_icon = gc_status-warning.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_fill_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_header  USING    uf_docu TYPE gty_output
                       CHANGING ct_post TYPE feb_t_ftpost.

* <<F36K910815 start ins
  DATA lv_bldat TYPE bkpf-bldat.

  IF rb_pdc = gc_true.
    lv_bldat = p_bldatc.
  ELSE.
* <<F36K910815 end ins
    lv_bldat = COND bkpf-bldat( WHEN uf_docu-bank_date IS NOT INITIAL
                                THEN uf_docu-bank_date
                                ELSE p_bldatc ).
  ENDIF. "<<F36K910815 ins

  ct_post = VALUE feb_t_ftpost( BASE ct_post (
                    stype = gc_stype-header
                    count = 1:
                    fnam = 'BKPF-BLDAT'
                    fval = |{ lv_bldat DATE = USER }| ) ),
                    fnam = 'BKPF-BUDAT'
                    fval = |{ p_budatc DATE = USER }| ) ),
                    fnam = 'BKPF-BLART'
                    fval = p_blartc ) ),
                    fnam = 'BKPF-BUKRS'
                    fval = p_bukrs ) ),
                    fnam = 'BKPF-BKTXT'
                    fval = p_bktxtc ) ),
                    fnam = 'BKPF-XBLNR'
                    fval = uf_docu-xblnr ) ),
                    fnam = 'BKPF-WAERS'
                    fval = uf_docu-waers_doc ) ),
                    fnam = 'BKPF-BRNCH'
                    fval = p_brnchc ) ).

*  IF p_waersc <> gv_waers.
*    ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                    stype = gc_stype-header
*                    count = 1:
*                    fnam = 'BKPF-KURSF'
*                    fval = |{ p_kursfc ALIGN = LEFT }|  ) ).
*
*  ENDIF.

  IF rb_pdc = gc_true.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                    stype = gc_stype-header
                    count = 1:
                    fnam = 'RF05A-PORTF'
                    fval = gv_post_dated_chk  ) ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_prepare_ftclear
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_CLEARING
*&      <-- LT_CLEAR
*&---------------------------------------------------------------------*
FORM f_post_prepare_ftclear  USING
                                uf_line_no  TYPE syst_tabix
                                uf_clearing TYPE gty_output
                             CHANGING
                                ct_clear    TYPE feb_t_ftclear
                                ct_partial  TYPE tt_partial.

  DATA: ls_ftclear TYPE ftclear.
  CONSTANTS: lc_agkoa TYPE koart VALUE 'D', "Customers
             lc_selfd TYPE fld30_f05a VALUE 'BELNR'.


  ls_ftclear-agkoa = lc_agkoa.
  ls_ftclear-agkon = uf_clearing-kunnr. "Customer
  ls_ftclear-agbuk = p_bukrs.           "Company code
  ls_ftclear-agums = uf_clearing-umskz. "Sp.Gl
  ls_ftclear-xnops = 'X'.               "Normal O/I
  ls_ftclear-selfd = lc_selfd.
  CONCATENATE uf_clearing-belnr_opn
              uf_clearing-gjahr_opn
              uf_clearing-buzei_opn INTO ls_ftclear-selvon.
  ls_ftclear-selbis = uf_clearing-belnr_opn.
*
  APPEND ls_ftclear TO ct_clear.

  IF ( uf_clearing-bal_amt + uf_clearing-retention ) < uf_clearing-wrbtr.
*  IF uf_clearing-payin_amt <> uf_clearing-wrbtr.

    ct_partial = VALUE tt_partial( BASE ct_partial (
                      line_no         = uf_line_no
                      payment_amount  = uf_clearing-bal_amt + uf_clearing-retention
*                      wt_entered      = COND #( WHEN uf_clearing-wt_qbshb IS NOT INITIAL THEN uf_clearing-wt_qbshb )
                      ) ).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_ftpost_tran
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_CLEARING
*&      <-- LT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_ftpost_tran  USING
                                uf_hkont  TYPE t012k-hkont
                                uf_amount TYPE gty_total
                                uf_docu   TYPE gty_output
                            CHANGING
                                ct_post     TYPE feb_t_ftpost.

  DATA: lv_count    TYPE count_pi,
        lv_df_order TYPE abap_bool.

  DATA(lv_valut) = COND bkpf-bldat( WHEN uf_docu-bank_date IS NOT INITIAL
                                      THEN uf_docu-bank_date
                                      ELSE p_budatc ).

  lv_count += 1.

  ct_post = VALUE feb_t_ftpost( BASE ct_post (
                    stype = gc_stype-item
                    count = lv_count:

                    fnam = 'RF05A-NEWKO'
                    fval = uf_hkont ) ),
                    fnam = 'RF05A-NEWBS'
                    fval = COND #( WHEN uf_amount-net > 0 THEN '40' ELSE '50' ) ) ),
                    fnam = 'BSEG-WRBTR'
                    fval = |{ abs( uf_amount-net ) ALIGN = LEFT }| ) ),
*                      fnam = 'BSEG-MWSKZ'
*                      fval = 'O7' ) ),
*                    fnam = 'BKPF-XMWST'
*                    fval = '' ) ),
                    fnam = 'BSEG-SGTXT'
                    fval = p_sgtxt ) ),
                    fnam = 'BSEG-VALUT'
                    fval = |{ lv_valut  DATE = USER }| ) ).

  lv_df_order = gc_true.

  PERFORM f_post_prepare_ftpost_default USING uf_hkont
                                              lv_count
                                              uf_docu
                                              lv_df_order
                                        CHANGING ct_post.


*  IF uf_docu-mwskz IS NOT INITIAL.
*    ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                      stype = gc_stype-item
*                      count = 1
*                      fnam = 'BKPF-XMWST'
*                      fval = 'X'  ) ).
*  ENDIF.

  PERFORM f_post_prepare_ftpost_oth USING uf_amount
                                         uf_docu
                                   CHANGING lv_count
                                            ct_post.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_prepare_ftpost_pdc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_CLEARING
*&      <-- LT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_ftpost_pdc  USING
                                uf_amount TYPE gty_total
                                uf_docu   TYPE gty_output
                            CHANGING
                                ct_post     TYPE feb_t_ftpost.

  DATA: lv_count TYPE count_pi.

  lv_count += 1.

  ct_post = VALUE feb_t_ftpost( BASE ct_post (
                    stype = gc_stype-item
                    count = lv_count:

                    fnam = 'RF05A-NEWKO'
                    fval = uf_docu-kunnr ) ),
                    fnam = 'RF05A-NEWBS'
                    fval = COND #( WHEN uf_amount-net > 0 THEN '09' ELSE '19' ) ) ),
                    fnam = 'RF05A-NEWUM'
                    fval = p_pdspgl ) ),
                    fnam = 'BSEG-WRBTR'
                    fval = |{ abs( uf_amount-net ) ALIGN = LEFT }| ) ),
                    fnam = 'BSEG-ZFBDT'
                    fval = |{ uf_docu-due_on DATE = USER }| ) ),
                    fnam = 'BSED-BOENO'
                    fval = |{ uf_docu-cheque_no ALIGN = LEFT }|  ) ),
                    fnam = 'BSED-BANK'
                    fval = uf_docu-bankk ) ),
                    fnam = 'BSED-ACCOU'
                    fval = uf_docu-bankn  ) ).
*                    fnam = 'BSED-PORTF'
*                    fval = gv_post_dated_chk ) ).

  PERFORM f_post_prepare_ftpost_oth USING uf_amount
                                         uf_docu
                                   CHANGING lv_count
                                            ct_post.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_open_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_OPEN_ITEM
*&      <-- LT_ONE_TIME
*&---------------------------------------------------------------------*
FORM f_get_open_item
                    CHANGING
                      ct_coll_log  TYPE tt_coll_log
                      ct_open_item TYPE tt_open_item.

  DATA(lv_data_type) = COND #( WHEN rb_memo = gc_true THEN gc_data_type-memo ELSE gc_data_type-invoice ).

  SELECT log~*,
      CASE
        WHEN cust~ktokd = 'Z010' THEN
          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
        WHEN cust~ktokd = 'Z050' THEN
          concat_with_space( partner~name_first, partner~name_last, 1 )
        ELSE
          concat_with_space( partner~name_org1, partner~name_org2, 1 )
      END AS cust_name
    FROM zsdsfit029 AS log
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    INNER JOIN @gt_action_status AS sta
    ON  log~action_type = sta~action_type
    AND log~status      = sta~status
    WHERE log~data_type   =  @lv_data_type
    AND   log~bukrs       =  @p_bukrs
    AND   log~billpl_no   IN @s_plno
    AND   log~billpl_date IN @s_pldate
    AND   log~kunnr       IN @s_kunnr
    AND   log~belnr       IN @s_belnr
    AND   log~gjahr       IN @s_gjahr
    AND   log~pymt_method IN @gr_pymt
    AND   log~tranf_no    IN @s_trnf
    AND   log~delete_flag = ''
    AND NOT ( log~action_type = @gc_reject-action AND
              log~status      = @gc_reject-status )
    INTO CORRESPONDING FIELDS OF TABLE @ct_coll_log  ##TOO_MANY_ITAB_FIELDS.

  IF ct_coll_log IS NOT INITIAL.
    IF rb_memo IS INITIAL.        "<<F36K910747 ins++

      "Get open item
      SELECT
        opn~bukrs,
        opn~belnr,
        opn~gjahr,
        opn~buzei,
        opn~budat,
        opn~bldat,
        opn~waers,
        opn~xblnr,
        opn~blart,
        opn~kunnr,
        opn~mwskz,
        opn~dmbtr,
        opn~wrbtr,
        opn~xref1,
        opn~xref2,
        opn~zuonr,
        opn~rebzg,
        opn~rebzj,
        opn~rebzz,
        opn~sgtxt,
        opn~prctr,
        opn~waers AS waers_doc,
        com~waers AS waers_loc,
        opn~vbeln,
        partner~name_org1,
        partner~name_org2,
        cust~xcpdk,
        hdr~kursf,
*      hdr~brnch,
        opn~bupla AS brnch,
        hdr~bktxt,
        opn~shkzg,
        opn~umskz,
        bill_pl~billpl_no,
        bill_pl~billpl_date,
        log~seq
        FROM bsid_view AS opn
        INNER JOIN t001 AS com
        ON com~bukrs = opn~bukrs
        INNER JOIN bkpf AS hdr
        ON  hdr~bukrs = opn~bukrs
        AND hdr~belnr = opn~belnr
        AND hdr~gjahr = opn~gjahr
        INNER JOIN kna1 AS cust
        ON opn~kunnr = cust~kunnr
        INNER JOIN cvi_cust_link AS link
        ON cust~kunnr = link~customer
        INNER JOIN but000 AS partner
        ON partner~partner_guid = link~partner_guid
        INNER JOIN @ct_coll_log AS log
        ON    opn~bukrs = log~bukrs
        AND   opn~belnr = log~belnr
        AND   opn~gjahr = log~gjahr
        AND   opn~buzei = log~buzei
        LEFT JOIN zsdsfit033 AS bill_pl
        ON opn~xref2 = bill_pl~billpl_no
*      WHERE hdr~waers = @p_waersc
*<<F36K917933 - 02 Begin of del
*        WHERE ( opn~rebzg = '' OR
*                opn~rebzg = 'V' OR
*              opn~rebzg = opn~belnr )
*        AND   opn~blart IN @gr_blart
*<<F36K917933 - 02 End of del
        WHERE opn~blart IN @gr_blart "<<F36K917933 - 02 ins
        AND   opn~umskz IN @gr_spgl
        INTO CORRESPONDING FIELDS OF TABLE @ct_open_item. "#EC CI_BUFFJOIN

** <<F36K910747 start ins++
    ELSE.
*<<F36K917933 - 01 Begin of del
*      SELECT *
*        FROM zsdsfit037
*        FOR ALL ENTRIES IN @ct_coll_log
*        WHERE data_type = @ct_coll_log-data_type
*        AND   bukrs     = @ct_coll_log-bukrs
*        AND   belnr     = @ct_coll_log-belnr
*        AND   gjahr     = @ct_coll_log-gjahr
*        AND   buzei     = @ct_coll_log-buzei
*        AND   seq       = @ct_coll_log-seq
*        INTO TABLE @DATA(lt_clr_link).             "#EC CI_NO_TRANSFORM
*<<F36K917933 - 01 End of del

*<<F36K917933 - 01 Begin of ins
      SELECT lnk~*
        FROM zsdsfit037 AS lnk
        INNER JOIN bkpf AS doc
        ON  doc~bukrs = lnk~bukrs_clr
        AND doc~belnr = lnk~belnr_clr
        AND doc~gjahr = lnk~gjahr_clr
        FOR ALL ENTRIES IN @ct_coll_log
        WHERE data_type = @ct_coll_log-data_type
        AND   lnk~bukrs = @ct_coll_log-bukrs
        AND   lnk~belnr = @ct_coll_log-belnr
        AND   lnk~gjahr = @ct_coll_log-gjahr
        AND   lnk~buzei = @ct_coll_log-buzei
        AND   lnk~seq   = @ct_coll_log-seq
        AND   doc~xreversal = ''
        INTO TABLE @DATA(lt_clr_link).             "#EC CI_NO_TRANSFORM
*<<F36K917933 - 01 End of ins

      LOOP AT ct_coll_log INTO DATA(ls_coll_log)  ##INTO_OK.
        DATA(lv_tabix) = sy-tabix.

        "Skip data already received
        READ TABLE lt_clr_link
          TRANSPORTING NO FIELDS
          WITH KEY data_type = ls_coll_log-data_type
                   bukrs = ls_coll_log-bukrs
                   belnr = ls_coll_log-belnr
                   gjahr = ls_coll_log-gjahr
                   buzei = ls_coll_log-buzei
                   seq   = ls_coll_log-seq.
        IF sy-subrc EQ 0.
          DELETE ct_coll_log INDEX lv_tabix.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.
** <<F36K910747 end ins++

    IF ct_coll_log IS NOT INITIAL.
      "Get change history all data to find next seq
      SELECT hist~*
        FROM zsdsfit038 AS hist
        INNER JOIN @ct_coll_log AS log
        ON   hist~data_type = log~data_type
        AND  hist~bukrs = log~bukrs
        AND  hist~belnr = log~belnr
        AND  hist~gjahr = log~gjahr
        AND  hist~buzei = log~buzei
        AND  hist~seq   = log~seq
        INTO TABLE @gt_status_hist.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_partial_payment_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_OPEN_ITEM
*&      <-- LT_PAY_ITEM
*&---------------------------------------------------------------------*
FORM f_get_partial_payment_item USING
                                  ut_open_item TYPE tt_open_item
                                CHANGING
                                  ct_pay_item  TYPE tt_open_item
                                  ct_clr_link  TYPE tt_clr_link.


  CHECK ut_open_item IS NOT INITIAL.

*<<F36K917933 - 01 Begin of del
*  SELECT *
*    FROM zsdsfit037
*    FOR ALL ENTRIES IN @ut_open_item
*    WHERE data_type = ''
*    AND   bukrs = @ut_open_item-bukrs
*    AND   belnr = @ut_open_item-belnr
*    AND   gjahr = @ut_open_item-gjahr
*    AND   buzei = @ut_open_item-buzei
*    AND   seq   = @ut_open_item-seq
*    INTO TABLE @ct_clr_link.                       "#EC CI_NO_TRANSFORM
*<<F36K917933 - 01 End of del

*<<F36K917933 - 01 Begin of ins
  SELECT lnk~*
    FROM zsdsfit037 AS lnk
    INNER JOIN bkpf AS doc
    ON  doc~bukrs = lnk~bukrs_clr
    AND doc~belnr = lnk~belnr_clr
    AND doc~gjahr = lnk~gjahr_clr
    FOR ALL ENTRIES IN @ut_open_item
    WHERE data_type = ''
    AND   lnk~bukrs = @ut_open_item-bukrs
    AND   lnk~belnr = @ut_open_item-belnr
    AND   lnk~gjahr = @ut_open_item-gjahr
    AND   lnk~buzei = @ut_open_item-buzei
    AND   lnk~seq   = @ut_open_item-seq
*    AND   doc~xreversal = ''                      "<<F36K917933 - 05 del
    INTO TABLE @ct_clr_link.                       "#EC CI_NO_TRANSFORM
*<<F36K917933 - 01 End of ins

  SELECT
    opn~bukrs,
    opn~belnr,
    opn~gjahr,
    opn~buzei,
    opn~budat,
    opn~bldat,
    opn~waers,
    opn~xblnr,
    opn~blart,
    opn~kunnr,
    opn~mwskz,
    opn~dmbtr,
    opn~wrbtr,
    opn~xref1,
    opn~xref2,
    opn~zuonr,
    opn~rebzg,
    opn~rebzj,
    opn~rebzz,
    opn~prctr,
    opn~waers AS waers_doc,
    opn~vbeln,
    hdr~kursf,
*    hdr~brnch
    opn~bupla AS brnch
    FROM bsid_view AS opn
    INNER JOIN bkpf AS hdr
    ON  hdr~bukrs = opn~bukrs
    AND hdr~belnr = opn~belnr
    AND hdr~gjahr = opn~gjahr
    INNER JOIN @ut_open_item AS itm
    ON    opn~bukrs = itm~bukrs
    AND   opn~rebzg = itm~belnr
    AND   opn~rebzj = itm~gjahr
    AND   opn~rebzz = itm~buzei
    INTO CORRESPONDING FIELDS OF TABLE @ct_pay_item  ##TOO_MANY_ITAB_FIELDS.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_one_time_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_OPEN_ITEM
*&      <-- LT_ONE_TIME
*&---------------------------------------------------------------------*
FORM f_get_one_time_data  CHANGING
                            ct_open_item TYPE tt_open_item.

  DATA(lt_one_time) = ct_open_item.

  DELETE lt_one_time WHERE xcpdk IS INITIAL.

  IF lt_one_time IS INITIAL.
    RETURN.
  ENDIF.

  SELECT
    bukrs,
    belnr,
    gjahr,
    buzei,
    name1,
    name2
    FROM bsec
    FOR ALL ENTRIES IN @lt_one_time
    WHERE bukrs = @lt_one_time-bukrs
    AND   belnr = @lt_one_time-belnr
    AND   gjahr = @lt_one_time-gjahr
    AND   buzei = @lt_one_time-buzei
    INTO TABLE @DATA(lt_bsec).                          "#EC CI_NOORDER

  SORT lt_bsec BY bukrs belnr gjahr buzei.

  LOOP AT ct_open_item ASSIGNING FIELD-SYMBOL(<ls_open_item>).
    READ TABLE lt_bsec
      INTO DATA(ls_bsec)
      WITH KEY bukrs = <ls_open_item>-bukrs
               belnr = <ls_open_item>-belnr
               gjahr = <ls_open_item>-gjahr
               buzei = <ls_open_item>-buzei
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      <ls_open_item>-name_org1 = ls_bsec-name1.
      <ls_open_item>-name_org2 = ls_bsec-name2.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_with_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_OPEN_ITEM
*&      <-- LT_WITH_ITEM
*&---------------------------------------------------------------------*
FORM f_get_with_item  USING    ut_open_item TYPE tt_open_item
                      CHANGING ct_with_item TYPE tt_with_item.

  CHECK ut_open_item IS NOT INITIAL.

  SELECT
      wti~bukrs,
      wti~belnr,
      wti~gjahr,
      wti~buzei,
      wti~witht,
      wti~wt_withcd,
      wti~wt_qbshh,
      wti~wt_qsshb,
      wti~wt_qbshb
    FROM with_item AS wti
    INNER JOIN @ut_open_item AS opn
    ON    wti~bukrs = opn~bukrs
    AND   wti~belnr = opn~belnr
    AND   wti~gjahr = opn~gjahr
    AND   wti~buzei = opn~buzei
    AND   wti~witht = @gc_witht-ar_at_payment               "420000700+
    INTO TABLE @ct_with_item.

  SORT ct_with_item BY bukrs belnr gjahr buzei.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_prepare_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_OPEN_ITEM
*&      --> LT_WITH_ITEM
*&      <-- GT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_prepare_output  USING
                          ut_open_item  TYPE tt_open_item
                          ut_coll_log   TYPE tt_coll_log
                          ut_with_item  TYPE tt_with_item
                          ut_pay_item   TYPE tt_open_item ##NEEDED
                          ut_clr_link   TYPE tt_clr_link
                       CHANGING
                          ct_output     TYPE tt_output.
  DATA:
*    lv_count    TYPE num9.
    lt_celltab  TYPE lvc_t_styl.

*  DATA(lt_pay_item) = ut_pay_item. *<<F36K917933 - 01 del

  LOOP AT ut_coll_log INTO DATA(ls_coll_log)  ##INTO_OK.
    CLEAR lt_celltab.

    "Skip data already received
    READ TABLE ut_clr_link
      INTO DATA(ls_clr_link)     ##NEEDED
      WITH KEY bukrs = ls_coll_log-bukrs
               belnr = ls_coll_log-belnr
               gjahr = ls_coll_log-gjahr
               buzei = ls_coll_log-buzei
               seq   = ls_coll_log-seq.
    IF sy-subrc EQ 0.
*<<F36K917933 - 01 Begin of del
*      DELETE lt_pay_item
*        WHERE bukrs = ls_clr_link-belnr
*        AND   belnr = ls_clr_link-belnr_clr
*        AND   gjahr = ls_clr_link-gjahr_clr.
*<<F36K917933 - 01 End of del

      CONTINUE.
    ENDIF.

    IF rb_memo <> gc_true.  "Skip open item check for memo
      READ TABLE ut_open_item
        INTO DATA(ls_open_item)
        WITH KEY bukrs = ls_coll_log-bukrs
                 belnr = ls_coll_log-belnr
                 gjahr = ls_coll_log-gjahr
                 buzei = ls_coll_log-buzei
                 seq   = ls_coll_log-seq.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ELSE.

      ls_open_item = CORRESPONDING #( ls_coll_log ).

    ENDIF.

    APPEND INITIAL LINE TO ct_output ASSIGNING FIELD-SYMBOL(<ls_output>).

    <ls_output> = CORRESPONDING #( ls_open_item MAPPING gjahr_opn = gjahr
                                                        belnr_opn = belnr
                                                        buzei_opn = buzei
                                                        blart_opn = blart
                                                EXCEPT  belnr gjahr blart
                                  ).

    <ls_output>-blart_clr = p_blartc.

    <ls_output>-wrbtr = COND #( WHEN ls_open_item-shkzg = 'H'
                                THEN <ls_output>-wrbtr * -1
                                ELSE <ls_output>-wrbtr ).

    <ls_output>-status_icon = gc_status-new.

    IF rb_memo <> gc_true.
      <ls_output>-cust_name = |{ ls_open_item-name_org1 } { ls_open_item-name_org2 }|.
    ELSE.
      <ls_output>-cust_name = ls_coll_log-cust_name.
    ENDIF.

    "Witholding tax data

    IF <ls_output>-blart_opn IN gr_whtax_blart.
      <ls_output>-wt_qbshb  = ls_coll_log-wht_amt.
    ELSE.
      READ TABLE ut_with_item
        INTO DATA(ls_with_item)
        WITH KEY bukrs = p_bukrs
                 belnr = ls_open_item-belnr
                 gjahr = ls_open_item-gjahr
                 buzei = ls_open_item-buzei

        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_output>-wt_withcd = ls_with_item-wt_withcd.
        <ls_output>-wt_qsshh  = ls_with_item-wt_qbshh.
*        <ls_output>-wt_qsshb  = ls_with_item-wt_qsshb.   "<<F36K917933 - 04 del
        <ls_output>-wt_qsshb_i  = ls_with_item-wt_qsshb.  "<<F36K917933 - 04 ins
        IF <ls_output>-bal_amt = <ls_output>-wrbtr.
          IF ls_coll_log-wht_amt = ls_with_item-wt_qbshb.
            <ls_output>-wt_qbshb   = ls_coll_log-wht_amt.
          ELSE.
            <ls_output>-status_icon = gc_status-error.
            <ls_output>-status_msg  = TEXT-e02. "Witholding tax amount is difference between bill collector log and Invoice doc
            <ls_output>-wt_qbshb  = ls_with_item-wt_qbshb.
          ENDIF.
        ELSE.
          <ls_output>-wt_qbshb  = ls_coll_log-wht_amt.
        ENDIF.
      ELSE.
        <ls_output>-wt_qbshb  = ls_coll_log-wht_amt.     "<<F36K917933 - 04 ins
      ENDIF.
    ENDIF.

    IF rb_oth = gc_true.
      <ls_output>-hbkid = p_hbkid.
      <ls_output>-hktid = p_hktid.
    ENDIF.
    <ls_output>-data_type   = ls_coll_log-data_type.
    <ls_output>-seq         = ls_coll_log-seq.
    <ls_output>-billpl_no   = ls_coll_log-billpl_no.
    <ls_output>-billpl_date = ls_coll_log-billpl_date.
    <ls_output>-bal_amt     = ls_coll_log-bal_amt.
    <ls_output>-exps_amt    = ls_coll_log-exps_amt.
    <ls_output>-aufnr_exps  = ls_coll_log-aufnr_exps.
    <ls_output>-fee         = ls_coll_log-fee.
    <ls_output>-aufnr_fee   = ls_coll_log-aufnr_fee.
    <ls_output>-retention   = ls_coll_log-retention.
    <ls_output>-income_amt  = ls_coll_log-income_amt.
    <ls_output>-cash_con    = ls_coll_log-cash_con.
*    <ls_output>-net         = ls_coll_log-payin_amt.
    <ls_output>-net         = ls_coll_log-received_amt + ls_coll_log-income_amt + ls_coll_log-cash_con.
    <ls_output>-kostl       = ls_coll_log-kostl.
*    <ls_output>-due_on      = ls_coll_log-faedt.
    <ls_output>-hbkid       = ls_coll_log-hbkid.
    <ls_output>-hktid       = ls_coll_log-hktid.
    <ls_output>-tranf_no    = ls_coll_log-tranf_no.

    <ls_output>-brnch_doc   = ls_coll_log-brnch.
    <ls_output>-cheque_no   = ls_coll_log-cheque_no.
    <ls_output>-bankn       = ls_coll_log-bankn.
    <ls_output>-bankl       = ls_coll_log-bankl.
    <ls_output>-bankk       = ls_coll_log-bankk.
    <ls_output>-payin_amt   = ls_coll_log-payin_amt.
    <ls_output>-vbeln_vf    = ls_coll_log-vbeln_vf.
    <ls_output>-pernr       = ls_coll_log-pernr.
    <ls_output>-work_date   = ls_coll_log-work_date.
    <ls_output>-remark      = ls_coll_log-remark.
    <ls_output>-zbank_item  = ls_coll_log-zbank_item.
    <ls_output>-bank_date   = ls_coll_log-bank_date.

    IF rb_pdc = gc_true.
      <ls_output>-due_on      = ls_coll_log-bank_date.
    ELSE.
      <ls_output>-due_on      = ls_coll_log-faedt.
    ENDIF.

    CASE gc_true.
      WHEN rb_pdc.
        IF <ls_output>-cheque_no IS INITIAL.
          <ls_output>-status_icon = gc_status-error.
          <ls_output>-status_msg  = TEXT-e04. "Cheque no. does not exist.
        ENDIF.
      WHEN rb_tran OR
           rb_oth.
        IF <ls_output>-hbkid IS INITIAL AND
           <ls_output>-hktid IS INITIAL AND
          <ls_output>-status_icon = gc_status-error.
          <ls_output>-status_msg  = TEXT-e03. "Bank account does not exist, please use other GL Bank transfer option
        ENDIF.
    ENDCASE.

    "Build sort key for grouping
*<< F36K910900 begin of ins
    CASE gc_true.
      WHEN rb_memo.
        <ls_output>-sort_field = |{ <ls_output>-tranf_no }|.
      WHEN rb_pdc OR rb_oth.
        <ls_output>-sort_field =  |{ <ls_output>-bankk }{ <ls_output>-cheque_no }|.
      WHEN rb_tran.
        <ls_output>-sort_field = |{ <ls_output>-tranf_no }|.
    ENDCASE.
*<< F36K910900 end of ins

*<< F36K910900 begin of del
*    IF <ls_output>-kunnr IN gr_one_time.
*      <ls_output>-sort_field = |{ <ls_output>-kunnr }{ <ls_output>-tranf_no }|.
*    ELSE.
*      CASE gc_true.
*        WHEN rb_memo.
*          <ls_output>-sort_field = |{ <ls_output>-belnr_opn }{ <ls_output>-gjahr_opn }{ <ls_output>-buzei_opn }{ <ls_output>-seq }|.
*        WHEN rb_pdc.
**        <ls_output>-sort_field = |{ <ls_output>-kunnr }{ <ls_output>-billpl_no }{ <ls_output>-bankk }{ <ls_output>-cheque_no }|.
**        <ls_output>-sort_field =  |{ <ls_output>-kunnr }{ <ls_output>-bankk }{ <ls_output>-cheque_no }|.
*          <ls_output>-sort_field =  |{ <ls_output>-tranf_no }|.
*        WHEN rb_tran OR rb_oth.
*          <ls_output>-sort_field = |{ <ls_output>-kunnr }{ <ls_output>-billpl_no }{ <ls_output>-hbkid }{ <ls_output>-hktid }|.
*      ENDCASE.
*    ENDIF.
*<< F36K910900 end of del

*    IF <ls_output>-billpl_no IS NOT INITIAL.
*    <ls_output>-sort_field = |{ <ls_output>-kunnr }{ <ls_output>-billpl_no }{ <ls_output>-hbkid }{ <ls_output>-hktid }|.
*    ELSE.
*      lv_count += 1.
*      <ls_output>-sort_field = |{ <ls_output>-kunnr }${ lv_count }{ <ls_output>-hbkid }{ <ls_output>-hktid }|.
*    ENDIF.

    "Set record status
    IF <ls_output>-status_icon = gc_status-error.
      PERFORM f_fill_celltab USING 'RO'
                           CHANGING lt_celltab.
    ELSE.
      PERFORM f_fill_celltab USING 'RW'
                           CHANGING lt_celltab.
    ENDIF.

    <ls_output>-celltab = lt_celltab.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_celltab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_CELLTAB
*&---------------------------------------------------------------------*
FORM f_fill_celltab  USING VALUE(uf_mode) TYPE char02
                  CHANGING ct_celltab TYPE lvc_t_styl.

  DATA: ls_celltab TYPE lvc_s_styl,
        lv_mode    TYPE raw4.

  IF uf_mode EQ 'RW'.
    lv_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE. "uf_mode eq 'RO'
    lv_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  ls_celltab-fieldname = 'SEL'.
  ls_celltab-style = lv_mode.
  INSERT ls_celltab INTO TABLE ct_celltab.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_post_prepare_ftpost_gl
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UF_AMOUNT
*&      <-- LV_COUNT
*&      <-- CT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_ftpost_oth  USING
                                 uf_amount TYPE gty_total
                                 uf_docu   TYPE gty_output
                               CHANGING
                                 cv_count  TYPE count_pi
                                 ct_post   TYPE feb_t_ftpost.

  DATA lv_df_order TYPE abap_bool.

  "Expense
  IF uf_amount-exp_amt IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-expense_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_amount-exp_amt < 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_amount-exp_amt ) ALIGN = LEFT }| ) ),
                      fnam = 'COBL-AUFNR'
                      fval = |{ uf_docu-aufnr_exps ALIGN = LEFT }| ) ).

    lv_df_order = COND #( WHEN uf_docu-aufnr_exps IS INITIAL THEN gc_true ELSE '' ).

    PERFORM f_post_prepare_ftpost_default USING gs_account-expense_account
                                                cv_count
                                                uf_docu
                                                lv_df_order
                                          CHANGING ct_post.

  ENDIF.

  "Fee
  IF uf_amount-fee IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-fee_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_amount-fee < 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_amount-fee ) ALIGN = LEFT }| ) ),
                      fnam = 'COBL-AUFNR'
                      fval = |{ uf_docu-aufnr_fee ALIGN = LEFT }| ) ).

    lv_df_order = COND #( WHEN uf_docu-aufnr_exps IS INITIAL THEN gc_true ELSE '' ).

    PERFORM f_post_prepare_ftpost_default USING gs_account-fee_account
                                                cv_count
                                                uf_docu
                                                lv_df_order
                                          CHANGING ct_post.

  ENDIF.

  "Retention
  IF uf_amount-retention IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = uf_docu-kunnr ) ),
                      fnam = 'RF05A-NEWBS'
*                      fval = COND #( WHEN uf_amount-retention < 0 THEN '09' ELSE '19' ) ) ),
                      fval = COND #( WHEN uf_amount-retention > 0 THEN '09' ELSE '19' ) ) ),
                      fnam = 'RF05A-NEWUM'
                      fval = 'Y' ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_amount-retention ) ALIGN = LEFT }| ) ),
                      fnam = 'BSEG-ZFBDT'
*                      fval = |{ uf_docu-due_on DATE = USER }| ) ).
*                      fval = |{ COND #( WHEN p_zfbdt IS NOT INITIAL THEN p_zfbdt ELSE uf_docu-due_on ) DATE = USER }| ) ).
                      fval = |{ COND #( WHEN p_zfbdt IS NOT INITIAL THEN p_zfbdt ELSE p_budatc ) DATE = USER }| ) ).

  ENDIF.

  "Refund
*  IF uf_amount-refund_amt IS NOT INITIAL.
*    cv_count += 1.
*    ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                      stype = gc_stype-item
*                      count = cv_count:
*                      fnam = 'RF05A-NEWKO'
*                      fval = gs_account-refund_account ) ),
*                      fnam = 'RF05A-NEWBS'
*                      fval = COND #( WHEN uf_amount-refund_amt < 0 THEN '40' ELSE '50' ) ) ),
*                      fnam = 'BSEG-WRBTR'
*                      fval = |{ abs( uf_amount-refund_amt ) ALIGN = LEFT }| ) ).
*  ENDIF.

  "Income
  IF uf_amount-income_amt IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-income_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_amount-income_amt < 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_amount-income_amt ) ALIGN = LEFT }| ) ).
*                      fnam = 'COBL-PRCTR'
*                      fval = uf_docu-prctr ) ).

    lv_df_order = gc_true.

    PERFORM f_post_prepare_ftpost_default USING gs_account-income_account
                                                cv_count
                                                uf_docu
                                                lv_df_order
                                          CHANGING ct_post.

  ENDIF.

  "Cash contra
  IF uf_amount-cash_con IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-cash_contra_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_amount-cash_con < 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_amount-cash_con ) ALIGN = LEFT }| ) ),
                      fnam = 'BSEG-SGTXT'
                      fval = |{ uf_docu-kunnr };{ uf_docu-cheque_no };{ uf_docu-bankn };{ uf_docu-due_on DATE = USER }| ) ).


    lv_df_order = gc_true.

    PERFORM f_post_prepare_ftpost_default USING gs_account-cash_contra_account
                                                cv_count
                                                uf_docu
                                                lv_df_order
                                          CHANGING ct_post.

  ENDIF.

  "Whtax gl line item
  IF uf_amount-whtax_amt IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-manual_whtax_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_amount-whtax_amt < 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_amount-whtax_amt ) ALIGN = LEFT }| ) ).
*                      fnam = 'BSEG-SGTXT'
*                      fval = |{ uf_docu-kunnr };{ uf_docu-cheque_no };{ uf_docu-bankn };{ uf_docu-due_on DATE = USER }| ) ).
*

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_prepare_ftpost_default
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_account
*&      <-- CT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_ftpost_default USING uf_account TYPE hkont
                                         uf_count   TYPE count_pi
                                         uf_docu    TYPE gty_output
                                         uf_order   TYPE abap_bool
                                   CHANGING ct_post TYPE feb_t_ftpost.

  LOOP AT gt_default_value INTO DATA(ls_default_value)  ##INTO_OK.
    IF uf_account IN ls_default_value-r_chk.
      IF ls_default_value-param = gc_genc_param-cost_center.
*        IF uf_docu-kostl IS NOT INITIAL.
*          ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                            stype = gc_stype-item
*                            count = uf_count
*                            fnam = 'COBL-KOSTL'
*                            fval = uf_docu-kostl ) ).
*        ELSEIF uf_docu-prctr IS NOT INITIAL.  "Use cost center = profit center from inv
*          ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                            stype = gc_stype-item
*                            count = uf_count
*                            fnam = 'COBL-KOSTL'
*                            fval = uf_docu-prctr ) ).
        "No default value is needed
*        ELSE.
        ct_post = VALUE feb_t_ftpost( BASE ct_post (
                          stype = gc_stype-item
                          count = uf_count
                          fnam = 'COBL-KOSTL'
                          fval = ls_default_value-param_ext ) ).

*        ENDIF.
      ELSEIF ls_default_value-param = gc_genc_param-profit_center.
        IF uf_docu-prctr IS NOT INITIAL.
          ct_post = VALUE feb_t_ftpost( BASE ct_post (
                            stype = gc_stype-item
                            count = uf_count
                            fnam = 'COBL-PRCTR'
                            fval = uf_docu-prctr ) ).

          "No default value is needed
*        ELSE.
*          ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                            stype = gc_stype-item
*                            count = uf_count
*                            fnam = 'COBL-PRCTR'
*                            fval = ls_default_value-param_ext ) ).
        ENDIF.
      ELSEIF ls_default_value-param = gc_genc_param-order AND
             uf_order = gc_true.
        ct_post = VALUE feb_t_ftpost( BASE ct_post (
                          stype = gc_stype-item
                          count = uf_count
                          fnam = 'COBL-AUFNR'
                          fval = ls_default_value-param_ext ) ).
      ENDIF.
    ENDIF.
  ENDLOOP.



ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_hotspot_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_row_id
*&      --> uf_column_id
*&---------------------------------------------------------------------*
FORM f_hotspot_click USING
                        uf_row_id     TYPE  lvc_s_row
                        uf_column_id  TYPE  lvc_s_col
                        uf_row_no     TYPE  lvc_s_roid   ##NEEDED ##CALLED.

  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.

      READ TABLE gt_output INTO DATA(ls_output)
        INDEX uf_row_id-index.

      CASE uf_column_id-fieldname.
        WHEN 'BELNR'.
          IF ls_output-belnr IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD ls_output-belnr.
            SET PARAMETER ID 'BUK' FIELD p_bukrs.
            SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.

        WHEN 'BELNR_OPN'.
          IF ls_output-belnr_opn IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD ls_output-belnr_opn.
            SET PARAMETER ID 'BUK' FIELD p_bukrs.
            SET PARAMETER ID 'GJR' FIELD ls_output-gjahr_opn.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.
      ENDCASE.

    WHEN gc_tab_sel-new_entry.
      READ TABLE gt_output_new INTO DATA(ls_output_new)
        INDEX uf_row_id-index.

      CASE uf_column_id-fieldname.
        WHEN 'BELNR'.
          IF ls_output_new-belnr IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD ls_output_new-belnr.
            SET PARAMETER ID 'BUK' FIELD p_bukrs.
            SET PARAMETER ID 'GJR' FIELD ls_output_new-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.
      ENDCASE.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_mark_sel_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SEL
*&      <-- GT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_mark_sel_all  USING    uf_sel    TYPE char01
                     CHANGING ct_output TYPE tt_output.

  LOOP AT ct_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    IF VALUE #( <ls_output>-celltab[ fieldname = 'SEL' ]-style OPTIONAL ) = cl_gui_alv_grid=>mc_style_enabled .
      <ls_output>-sel = uf_sel.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_reject_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_reject_data  CHANGING ct_output TYPE tt_output.

  DATA:
    lt_celltab     TYPE lvc_t_styl,
    lt_status_hist TYPE tt_status_hist.

  LOOP AT ct_output ASSIGNING FIELD-SYMBOL(<ls_output>)
    WHERE sel = gc_true.

    CLEAR <ls_output>-sel.

    PERFORM f_fill_celltab USING 'RO'
                         CHANGING lt_celltab.

    <ls_output>-celltab = lt_celltab.

    UPDATE zsdsfit029 SET action_type = gc_reject-action
                          status      = gc_reject-status
        WHERE data_type = ''
        AND   bukrs = p_bukrs
        AND   belnr = <ls_output>-belnr_opn
        AND   gjahr = <ls_output>-gjahr_opn
        AND   buzei = <ls_output>-buzei_opn
        AND   seq   = <ls_output>-seq.

    PERFORM f_create_status_hist USING <ls_output>
                                       gc_reject-action
                                       gc_reject-status
                                 CHANGING lt_status_hist.

  ENDLOOP.

  IF lt_status_hist IS NOT INITIAL.
    MODIFY zsdsfit038 FROM TABLE lt_status_hist.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_create_status_hist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_ZSDSFIT029>
*&      <-- LT_STATUS_HIST
*&---------------------------------------------------------------------*
FORM f_create_status_hist  USING    uf_log         TYPE gty_output
                                    uf_action      TYPE zsdsfit029-action_type
                                    uf_status      TYPE zsdsfit029-status
                           CHANGING ct_status_hist TYPE tt_status_hist.

  DATA: ls_status_hist TYPE zsdsfit038.

  SORT gt_status_hist BY bukrs belnr gjahr buzei seq sub_seq DESCENDING.

  "Get lastest hist data
  LOOP AT gt_status_hist
    ASSIGNING FIELD-SYMBOL(<ls_status_hist>)
    WHERE bukrs = p_bukrs
    AND   belnr = uf_log-belnr_opn
    AND   gjahr = uf_log-gjahr_opn
    AND   buzei = uf_log-buzei_opn
    AND   seq   = uf_log-seq.
    EXIT.
  ENDLOOP.

  READ TABLE gt_coll_log INTO DATA(ls_coll_log)
    WITH KEY bukrs = p_bukrs
             belnr = uf_log-belnr_opn
             gjahr = uf_log-gjahr_opn
             buzei = uf_log-buzei_opn
             seq   = uf_log-seq.

  ls_status_hist = CORRESPONDING #( ls_coll_log ).
  ls_status_hist = CORRESPONDING #( uf_log ).

  IF <ls_status_hist> IS ASSIGNED.
    IF <ls_status_hist>-pernr       = uf_log-pernr AND
       <ls_status_hist>-work_date   = uf_log-work_date AND
       <ls_status_hist>-action_type = uf_action AND
       <ls_status_hist>-status      = uf_status.
      "No history log add for no action type and status change
      RETURN.
    ENDIF.

    ls_status_hist-sub_seq = <ls_status_hist>-sub_seq + 1.
  ELSE.
    ls_status_hist-sub_seq = 1.
  ENDIF.

  ls_status_hist-bukrs        = p_bukrs.
  ls_status_hist-belnr        = uf_log-belnr_opn.
  ls_status_hist-gjahr        = uf_log-gjahr_opn.
  ls_status_hist-buzei        = uf_log-buzei_opn.
  ls_status_hist-seq          = uf_log-seq.
  ls_status_hist-action_type  = uf_action.
  ls_status_hist-status       = uf_status.
  ls_status_hist-update_by    = sy-uname.
  ls_status_hist-update_on    = sy-datum.
  ls_status_hist-update_time  = sy-uzeit.
  ls_status_hist-tcode        = sy-tcode. "<<F36K910967 ins
  ls_status_hist-ernam_hist   = sy-uname.
  ls_status_hist-erdat_hist   = sy-datum.
  ls_status_hist-erzmt_hist   = sy-uzeit.

  APPEND ls_status_hist TO ct_status_hist.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_remark
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_OUTPUT>
*&---------------------------------------------------------------------*
FORM f_update_remark  USING    uf_change    TYPE gty_output.

  UPDATE zsdsfit029 SET remark = uf_change-remark
    WHERE  data_type    = ''
    AND    bukrs        = p_bukrs
    AND    belnr        = uf_change-belnr_opn
    AND    gjahr        = uf_change-gjahr_opn
    AND    buzei        = uf_change-buzei_opn
    AND    seq          = uf_change-seq.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_tab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*<<F36K917933 - 03 Begin of del
*FORM f_set_tab .
*  CASE sy-dynnr.
*    WHEN 1000.
*      CASE sscrfields-ucomm.
*        WHEN gc_tab_sel-collection_log.
*          tab_blk-dynnr = '100'.
*          gv_data_type = sscrfields-ucomm.
*        WHEN gc_tab_sel-new_entry.
*          tab_blk-dynnr = '200'.
*          gv_data_type = sscrfields-ucomm.
*        WHEN OTHERS.
*      ENDCASE.
*  ENDCASE.
*ENDFORM.
*<<F36K917933 - 03 End of del

*&---------------------------------------------------------------------*
*& Form f_chk_sel_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_chk_sel_screen .


  CASE gv_data_type.
*--------------------------------------------------------------------*
* Collector log
*--------------------------------------------------------------------*
    WHEN gc_tab_sel-collection_log.

      CASE gc_true.
        WHEN rb_memo.
          p_blartc = gv_blart_memo.
        WHEN rb_pdc.
          p_blartc = gv_blart_pdc.
        WHEN rb_tran.
          p_blartc = gv_blart_bank.
        WHEN rb_oth.
          p_blartc = gv_blart_bank.
      ENDCASE.

      "Below check process only during F8
      IF sy-ucomm <> 'ONLI'.
        RETURN.
      ENDIF.

      IF p_bldatc IS INITIAL.
        MESSAGE e000(38) WITH TEXT-e91.
      ENDIF.

      IF p_budatc IS INITIAL.
        MESSAGE e000(38) WITH TEXT-e92.
      ENDIF.

      IF p_brnchc IS INITIAL.
        MESSAGE e000(38) WITH TEXT-e93.
      ENDIF.

      IF p_pdspgl IS INITIAL AND rb_pdc = gc_true.
        MESSAGE e000(38) WITH TEXT-e94.
      ENDIF.

*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
    WHEN gc_tab_sel-new_entry.

  ENDCASE.

ENDFORM.
