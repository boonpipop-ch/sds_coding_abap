*----------------------------------------------------------------------*
***INCLUDE ZSDSFIR0250_F05.
*----------------------------------------------------------------------*
*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250_F05
*  Creation Date      : 03.10.2024
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic - memo
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Form f_init_var_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_var_memo .

  DATA:
    lr_order_exp TYPE RANGE OF zsdsfit029-aufnr_exps,
    lr_order_fee TYPE RANGE OF zsdsfit029-aufnr_fee,

    lt_genc      TYPE zcl_sdsca_utilities=>tt_gen_c.

  IF rb_crem = gc_true OR
     rb_chgm = gc_true.
    gv_edit = abap_true.
  ELSE.
    gv_edit = abap_false.
  ENDIF.

  SELECT SINGLE waers
    FROM t001
    WHERE bukrs = @p_bukrsm
    INTO @gv_waers.

  "Default IO expense and bank fee
  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                         " ABAP Program Name
      if_param = gc_genc_param-df_order_exp   " Parameter name
    IMPORTING
      et_range = lr_order_exp
  ).

  IF lr_order_exp IS NOT INITIAL.
    gv_df_order_exp = lr_order_exp[ 1 ]-low.
  ENDIF.

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                         " ABAP Program Name
      if_param = gc_genc_param-df_order_fee       " Parameter name
    IMPORTING
      et_range = lr_order_fee
  ).

  IF lr_order_fee IS NOT INITIAL.
    gv_df_order_fee = lr_order_fee[ 1 ]-low.
  ENDIF.

  "Default branch and profit center

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = sy-repid          " ABAP Program Name
*      irt_param =                  " Parameter name
*      irt_ext   =                  " Parameter extension
    IMPORTING
      et_gen_c  = lt_genc           " General Constants (GENC)
  ).

  LOOP AT lt_genc INTO DATA(ls_genc)      ##INTO_OK.
    CASE ls_genc-param.
      WHEN gc_genc_param_memo-bupla.
        gs_default_memo-brnch = ls_genc-value_low.
      WHEN gc_genc_param_memo-prctr.
*        gs_default_memo-prctr = ls_genc-value_low.  "<<F36K912524 - del
        gs_default_memo-prctr = |{ ls_genc-value_low ALPHA = IN }|.  "<<F36K912524 - ins
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_open_items_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_open_items_memo .

  CASE gc_true.
    WHEN rb_crem.
      PERFORM f_set_new_memo.
      PERFORM f_prepare_memo_output.
    WHEN rb_chgm.
      PERFORM f_get_memo_open.
      PERFORM f_prepare_memo_output.
    WHEN rb_dism.
      PERFORM f_get_memo_all.
      PERFORM f_prepare_memo_output.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_default_new_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- UREF_DATA_CHANGED_>MT_INSERTED
*&---------------------------------------------------------------------*
FORM f_default_new_memo  CHANGING ct_data_changed TYPE REF TO  cl_alv_changed_data_protocol.

  DATA:
    ls_ins_row    TYPE lvc_s_moce ##NEEDED,
    ls_outtab_new TYPE zsdsfis085.

  FIELD-SYMBOLS: <fs> TYPE STANDARD TABLE.

  LOOP AT ct_data_changed->mt_inserted_rows INTO ls_ins_row ##INTO_OK.
    ASSIGN ct_data_changed->mp_mod_rows->* TO <fs>.
    READ TABLE <fs> INTO ls_outtab_new INDEX 1. "ls_ins_row-row_id.
    IF sy-subrc EQ 0.
      PERFORM f_set_default_init_memo CHANGING ls_outtab_new.

      PERFORM f_fill_celltab_memo USING 'RW'
        CHANGING ls_outtab_new-celltab.


      MODIFY <fs> FROM ls_outtab_new INDEX 1. "ls_ins_row-row_id.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_result_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_display_result_memo  USING ut_result TYPE tt_output.

* Set Container name
  gf_container = 'CTL_ALV'.
* Disable Header area
  gf_alv_header = gc_true.

*   No auto refresh in edit mode
  gf_no_auto_refresh = gc_true.

* ALV Layout
  PERFORM f_alv_layout USING p_mvar
                       CHANGING gs_layout
                                gs_variant
                                gs_print.

* Assign Output Data
* Assign Size
  gf_header_hight = gc_header_height.
  gf_alv_height   = gc_alv_height.
  ASSIGN ut_result TO <g_list>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM f_alv_build_fieldcat_memo CHANGING gt_fieldcat.
* Sort data
*  PERFORM f_alv_sort_result CHANGING gt_sort.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_build_fieldcat_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_alv_build_fieldcat_memo  CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <l_fieldcat>   TYPE  lvc_s_fcat.

* Build Field cat from Structure.
  PERFORM f_prepare_fieldcat_o  USING  gc_structure
                              CHANGING ct_fieldcat.

  LOOP AT ct_fieldcat ASSIGNING <l_fieldcat>.
    CASE <l_fieldcat>-fieldname.
      WHEN 'SEL'.
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-col_pos    = 10.
        <l_fieldcat>-key        = gc_true.

        IF rb_crem = gc_true OR
           rb_dism = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'DELETE_FLAG'.
*        IF rb_dis = gc_true or   "<<F36K914812 - 01 del
        IF rb_dism = gc_true OR   "<<F36K914812 - 01 is
           rb_chgm = gc_true.     "<<F36K914812 - 01 ins
          <l_fieldcat>-col_pos    = 15  ##NUMBER_OK.
          <l_fieldcat>-key        = gc_true.
        ELSE.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'TRANF_NO'.
        <l_fieldcat>-col_pos    = 20   ##NUMBER_OK.
        <l_fieldcat>-key        = gc_true.
      WHEN 'BUKRS'.
        <l_fieldcat>-col_pos    = 25    ##NUMBER_OK.
        <l_fieldcat>-key        = gc_true.
      WHEN 'PERNR'.
        <l_fieldcat>-col_pos    = 30    ##NUMBER_OK.
        <l_fieldcat>-key        = gc_true.
      WHEN 'FULL_NAME'.
        <l_fieldcat>-col_pos    = 40    ##NUMBER_OK.
        <l_fieldcat>-key        = gc_true.
      WHEN 'WORK_DATE'.
        <l_fieldcat>-col_pos    = 50    ##NUMBER_OK.
        <l_fieldcat>-key        = gc_true.
      WHEN 'KUNNR'.
        <l_fieldcat>-col_pos    = 60    ##NUMBER_OK.
      WHEN 'CUST_NAME'.
        <l_fieldcat>-col_pos    = 70    ##NUMBER_OK.
      WHEN 'UMSKZ'.
        <l_fieldcat>-col_pos    = 80    ##NUMBER_OK.
      WHEN 'BILLPL_NO'.
        <l_fieldcat>-col_pos    = 90    ##NUMBER_OK.
      WHEN 'BILLPL_DATE'.
        <l_fieldcat>-col_pos    = 100   ##NUMBER_OK.
      WHEN 'XBLNR'. "Tax Invoice
        <l_fieldcat>-seltext    = TEXT-f11.
        <l_fieldcat>-scrtext_m  = TEXT-f11.
        <l_fieldcat>-scrtext_l  = TEXT-f11.
        <l_fieldcat>-reptext    = TEXT-f11.

        <l_fieldcat>-col_pos    = 110   ##NUMBER_OK.
      WHEN 'BLDAT'.
        <l_fieldcat>-col_pos    = 120   ##NUMBER_OK.
      WHEN 'BELNR'.
*        <l_fieldcat>-hotspot    = gc_true.
        <l_fieldcat>-col_pos    = 130   ##NUMBER_OK.
      WHEN 'VBELN_VF'.
        <l_fieldcat>-col_pos    = 140   ##NUMBER_OK.
      WHEN 'BUDAT'.
        <l_fieldcat>-col_pos    = 150   ##NUMBER_OK.
      WHEN 'FAEDT'.
        <l_fieldcat>-col_pos    = 160   ##NUMBER_OK.
      WHEN 'WRBTR'.
        <l_fieldcat>-col_pos    = 170   ##NUMBER_OK.
*        <l_fieldcat>-do_sum     = gc_true.
      WHEN 'WAERS'.
        <l_fieldcat>-col_pos    = 180   ##NUMBER_OK.
      WHEN 'DEDUCT_AMT'.
        <l_fieldcat>-col_pos    = 190   ##NUMBER_OK.
        <l_fieldcat>-do_sum     = gc_true.
*      WHEN 'PAYNO'.
*        <l_fieldcat>-col_pos    = 200   ##NUMBER_OK.
      WHEN 'BRNCH'.
        <l_fieldcat>-col_pos    = 210   ##NUMBER_OK.
      WHEN 'BAL_AMT'.
        <l_fieldcat>-col_pos    = 220   ##NUMBER_OK.
        <l_fieldcat>-do_sum     = gc_true.
      WHEN 'EXPS_AMT'.
        <l_fieldcat>-col_pos    = 230   ##NUMBER_OK.
*        <l_fieldcat>-do_sum     = gc_true.
      WHEN 'AUFNR_EXPS'.
        <l_fieldcat>-seltext    = TEXT-f09.
        <l_fieldcat>-scrtext_m  = TEXT-f09.
        <l_fieldcat>-scrtext_l  = TEXT-f09.
        <l_fieldcat>-reptext    = TEXT-f09.

        <l_fieldcat>-col_pos    = 235   ##NUMBER_OK.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'WHT_AMT'.
        <l_fieldcat>-col_pos    = 240   ##NUMBER_OK.
*        <l_fieldcat>-do_sum     = gc_true.
      WHEN 'FEE'.
        <l_fieldcat>-col_pos    = 250   ##NUMBER_OK.
*        <l_fieldcat>-do_sum     = gc_true.
      WHEN 'AUFNR_FEE'.
        <l_fieldcat>-seltext    = TEXT-f10.
        <l_fieldcat>-scrtext_m  = TEXT-f10.
        <l_fieldcat>-scrtext_l  = TEXT-f10.
        <l_fieldcat>-reptext    = TEXT-f10.

        <l_fieldcat>-col_pos    = 255   ##NUMBER_OK.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'RECEIVED_AMT'.
        <l_fieldcat>-col_pos    = 290   ##NUMBER_OK.
      WHEN 'RETENTION'.
        <l_fieldcat>-col_pos    = 300   ##NUMBER_OK.
      WHEN 'INCOME_AMT'.
        <l_fieldcat>-col_pos    = 320   ##NUMBER_OK.
      WHEN 'CASH_CON'.
        <l_fieldcat>-col_pos    = 330   ##NUMBER_OK.
      WHEN 'PAYIN_AMT'.
        <l_fieldcat>-col_pos    = 340   ##NUMBER_OK.
        <l_fieldcat>-do_sum     = gc_true.
      WHEN 'REMAIN_AMT'.
        <l_fieldcat>-seltext    = TEXT-f07.
        <l_fieldcat>-scrtext_s  = TEXT-f07.
        <l_fieldcat>-scrtext_m  = TEXT-f07.
        <l_fieldcat>-scrtext_l  = TEXT-f07.
        <l_fieldcat>-reptext    = TEXT-f07.

        <l_fieldcat>-col_pos    = 350   ##NUMBER_OK.
      WHEN 'REMAIN_C_AMT'.
        <l_fieldcat>-seltext    = TEXT-f08.
        <l_fieldcat>-scrtext_s  = TEXT-f08.
        <l_fieldcat>-scrtext_m  = TEXT-f08.
        <l_fieldcat>-scrtext_l  = TEXT-f08.
        <l_fieldcat>-reptext    = TEXT-f08.

        <l_fieldcat>-col_pos    = 360   ##NUMBER_OK.
      WHEN 'KOSTL'.
        <l_fieldcat>-col_pos    = 370   ##NUMBER_OK.
      WHEN 'PRCTR'.
        <l_fieldcat>-col_pos    = 380   ##NUMBER_OK.

      WHEN 'HBKID'.
        <l_fieldcat>-col_pos    = 400   ##NUMBER_OK.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'HKTID'.
        <l_fieldcat>-col_pos    = 410   ##NUMBER_OK.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'ZBANK_ITEM'.
        <l_fieldcat>-col_pos    = 415   ##NUMBER_OK.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'PYMT_METHOD'.
        <l_fieldcat>-col_pos    = 420   ##NUMBER_OK.
      WHEN 'ACTION_TYPE'.
        <l_fieldcat>-col_pos    = 430   ##NUMBER_OK.
      WHEN 'STATUS'.
        <l_fieldcat>-col_pos    = 440   ##NUMBER_OK.
*        <l_fieldcat>-drdn_hndl  = 2.
*        <l_fieldcat>-drdn_alias = 'X'.
      WHEN 'REMARK'.
        <l_fieldcat>-col_pos    = 450   ##NUMBER_OK.
      WHEN 'PAYMENT_DATE'.
        <l_fieldcat>-col_pos    = 460   ##NUMBER_OK.
      WHEN 'RECEIVED_DATE'.
        <l_fieldcat>-col_pos    = 470   ##NUMBER_OK.
      WHEN 'RECEIPT_NO'.
        <l_fieldcat>-col_pos    = 480   ##NUMBER_OK.
      WHEN 'BANK_DATE'.
        <l_fieldcat>-col_pos    = 490   ##NUMBER_OK.
      WHEN 'BANKK'.
        <l_fieldcat>-reptext    = TEXT-f06.
        <l_fieldcat>-seltext    = TEXT-f06.
        <l_fieldcat>-scrtext_s  = TEXT-f05.
        <l_fieldcat>-scrtext_m  = TEXT-f06.
        <l_fieldcat>-scrtext_l  = TEXT-f06.
        <l_fieldcat>-col_pos    = 495   ##NUMBER_OK.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'BANKN'.
        <l_fieldcat>-col_pos    = 496   ##NUMBER_OK.
      WHEN 'CHEQUE_NO'.
        <l_fieldcat>-seltext    = TEXT-f02.
        <l_fieldcat>-scrtext_s  = TEXT-f02.
        <l_fieldcat>-scrtext_m  = TEXT-f02.
        <l_fieldcat>-scrtext_l  = TEXT-f02.
        <l_fieldcat>-reptext    = TEXT-f02.
        <l_fieldcat>-col_pos    = 500   ##NUMBER_OK.
      WHEN 'BANKL'.
        <l_fieldcat>-col_pos    = 510   ##NUMBER_OK.
      WHEN 'VBELN'.
        <l_fieldcat>-col_pos    = 540   ##NUMBER_OK.
      WHEN 'DOC_SIGN_DATE'.
        <l_fieldcat>-col_pos    = 550   ##NUMBER_OK.
      WHEN 'FKDAT'.
        <l_fieldcat>-col_pos    = 560   ##NUMBER_OK.
      WHEN 'CHEQUE_DATE'.
        <l_fieldcat>-col_pos    = 570   ##NUMBER_OK.
      WHEN 'FOLLOW_DATE'.
        <l_fieldcat>-col_pos    = 580   ##NUMBER_OK.
*      WHEN 'DOCUMENT_STATUS'.
*        IF gv_data_type = gc_tab_sel-update_bank.
*          <l_fieldcat>-col_pos  = 600   ##NUMBER_OK.
*        ELSE.
*          <l_fieldcat>-no_out   = gc_true.
*        ENDIF.
*      WHEN 'INV_STATUS'.
*        IF gv_data_type = gc_tab_sel-update_bank.
*          <l_fieldcat>-col_pos  = 610   ##NUMBER_OK.
*        ELSE.
*          <l_fieldcat>-no_out   = gc_true.
*        ENDIF.
      WHEN 'REMARK_VENDOR'.
        <l_fieldcat>-col_pos    = 620   ##NUMBER_OK.
*      WHEN 'REASON_VENDOR'.
*        IF gv_data_type = gc_tab_sel-update_bank.
*          <l_fieldcat>-col_pos  = 630   ##NUMBER_OK.
*        ELSE.
*          <l_fieldcat>-no_out   = gc_true.
*        ENDIF.
*      WHEN 'BILLING_CYCLE'.
*        IF gv_data_type = gc_tab_sel-update_bank.
*          <l_fieldcat>-col_pos  = 640   ##NUMBER_OK.
*        ELSE.
*          <l_fieldcat>-no_out   = gc_true.
*        ENDIF.
*      WHEN 'COLLECTION_CYCLE'.
*        IF gv_data_type = gc_tab_sel-update_bank.
*          <l_fieldcat>-col_pos  = 650   ##NUMBER_OK.
*        ELSE.
*          <l_fieldcat>-no_out   = gc_true.
*        ENDIF.
*      WHEN 'AWB_NO'.
*        IF gv_data_type = gc_tab_sel-update_bank.
*          <l_fieldcat>-col_pos  = 660   ##NUMBER_OK.
*        ELSE.
*          <l_fieldcat>-no_out   = gc_true.
*        ENDIF.
      WHEN OTHERS.
        <l_fieldcat>-no_out     = gc_true.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&      --> GT_STATUS_HIST
*&---------------------------------------------------------------------*
FORM f_create_memo  USING ut_output TYPE tt_output
                             ut_curr_status_hist TYPE tt_status_hist.

  DATA:
*    lt_grp            TYPE tt_grp_doc,
    lt_bplog       TYPE STANDARD TABLE OF zsdsfit029,
    lt_status_hist TYPE tt_status_hist.
*    lt_bank_statement TYPE tt_bank_statement.

  lt_bplog = CORRESPONDING #( ut_output  ).

  IF lt_bplog IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e10.
    RETURN.
  ENDIF.

  SELECT
    col~bukrs,
    col~belnr,
    col~gjahr,
    col~buzei,
    col~seq
    FROM zsdsfit029 AS col
    FOR ALL ENTRIES IN @lt_bplog
    WHERE data_type = @gc_data_type-memo
    AND   bukrs = @lt_bplog-bukrs
    AND   belnr = @lt_bplog-belnr
    AND   gjahr = @lt_bplog-gjahr
    INTO TABLE @DATA(lt_max_seq).             "#EC CI_FAE_LINES_ENSURED

  SORT lt_max_seq BY bukrs belnr gjahr seq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_max_seq COMPARING bukrs belnr gjahr.

  LOOP AT lt_bplog ASSIGNING FIELD-SYMBOL(<ls_zsdsfit029>).

    "Get max sequence
    READ TABLE lt_max_seq INTO DATA(ls_max_seq)
      WITH KEY bukrs = <ls_zsdsfit029>-bukrs
               belnr = <ls_zsdsfit029>-belnr
               gjahr = <ls_zsdsfit029>-gjahr
               buzei = <ls_zsdsfit029>-buzei
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      <ls_zsdsfit029>-seq = ls_max_seq-seq + 1.
    ELSE.
      <ls_zsdsfit029>-seq = 1.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = gc_no-nr
        object                  = gc_no-obj
        toyear                  = sy-datum(4)
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = <ls_zsdsfit029>-tranf_no
*       RETURNCODE              =
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    READ TABLE ut_output ASSIGNING FIELD-SYMBOL(<ls_output>)
      WITH KEY
               bukrs = <ls_zsdsfit029>-bukrs
               belnr = <ls_zsdsfit029>-belnr
               gjahr = <ls_zsdsfit029>-gjahr.
    IF sy-subrc EQ 0.
      <ls_output>-tranf_no = <ls_zsdsfit029>-tranf_no.
    ENDIF.

    <ls_zsdsfit029>-ernam = sy-uname. "<<F36K914812 - 04 ins
    <ls_zsdsfit029>-erdat = sy-datum.
    <ls_zsdsfit029>-erzmt = sy-uzeit.

    PERFORM f_create_status_hist USING <ls_zsdsfit029>
                                       ut_curr_status_hist
                                 CHANGING lt_status_hist.

  ENDLOOP.

  IF lt_bplog IS NOT INITIAL.
    MODIFY zsdsfit029 FROM TABLE lt_bplog.
  ENDIF.

  IF lt_status_hist IS NOT INITIAL.
    MODIFY zsdsfit038 FROM TABLE lt_status_hist.
  ENDIF.

  "Collection log saved
  MESSAGE s000(38) WITH TEXT-r04.
  COMMIT WORK AND WAIT. "Added 02.12.2024

  gref_grid->set_ready_for_input(
                     EXPORTING i_ready_for_input = 0 ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_memo_hist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&      <-- GT_STATUS_HIST
*&---------------------------------------------------------------------*
FORM f_get_memo_hist  USING ut_output TYPE tt_output
                      CHANGING ct_curr_status_hist TYPE tt_status_hist.

  IF ut_output IS INITIAL.
    RETURN.
  ENDIF.

  SELECT hist~*
    FROM zsdsfit038 AS hist
    INNER JOIN @ut_output AS new_itm
    ON  hist~data_type = new_itm~data_type
    AND hist~bukrs     = new_itm~bukrs
    AND hist~belnr     = new_itm~belnr
    AND hist~gjahr     = new_itm~gjahr
    AND hist~buzei     = new_itm~buzei
    INTO TABLE @ct_curr_status_hist.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_mandatory_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALID
*&      <-- LREF_MSG
*&---------------------------------------------------------------------*
FORM f_validate_mandatory_memo  CHANGING cf_valid TYPE abap_bool
                                   cref_msg TYPE REF TO cl_alv_changed_data_protocol.


  cf_valid = abap_true.

  LOOP AT gt_output INTO DATA(ls_output)  ##INTO_OK.

    "Skip check for delete data
    CHECK ls_output-delete_flag = ''.

    DATA(lv_row_id) = sy-tabix.
    IF ls_output-work_date IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'WORK_DATE' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-belnr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'BELNR' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-gjahr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'GJAHR' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-pernr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'PERNR' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-kunnr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'KUNNR' '' '' CHANGING cref_msg.
    ENDIF.

    "Skip check action type <> 02 and status <> 02
    READ TABLE gt_action_status
      WITH KEY action_type = ls_output-action_type
               status      = ls_output-status
      TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      CONTINUE. "Skip check
    ENDIF.

    IF ls_output-wrbtr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'WRBTR' '' '' CHANGING cref_msg.
    ENDIF.


*    IF ls_output-pymt_method IS INITIAL.
**      cf_valid = abap_false.
**      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'PYMT_METHOD' '' CHANGING cref_msg.
*    ELSEIF ls_output-pymt_method IN gr_bank.  "Bank transfer
*      READ TABLE gt_action_status
*        INTO DATA(ls_action_status)
*        WITH KEY action_type = ls_output-action_type
*                 status      = ls_output-status.
*
*      ENDIF.
*
*    ENDIF.


    READ TABLE gt_action_status
      INTO DATA(ls_action_status)
      WITH KEY action_type = ls_output-action_type
               status      = ls_output-status.
    IF ls_action_status IS NOT INITIAL.
      IF ls_output-hbkid IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'HBKID' '' '' CHANGING cref_msg.
      ENDIF.

      IF ls_output-hktid IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'HKTID' '' '' CHANGING cref_msg.
      ENDIF.

      IF ls_output-cheque_no IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'CHEQUE_NO' '' '' CHANGING cref_msg.
      ENDIF.

      IF ls_output-bankk IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'BANKK' '' '' CHANGING cref_msg.
      ENDIF.

      IF ls_output-bankn IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'BANKN' '' '' CHANGING cref_msg.
      ENDIF.

      IF ls_output-prctr IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'PRCTR' '' '' CHANGING cref_msg.
      ENDIF.

    ENDIF.

    IF ls_output-action_type IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'ACTION_TYPE' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-status IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'STATUS' '' '' CHANGING cref_msg.
    ENDIF.

    CLEAR ls_action_status.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_memo_open
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_memo_open .

  DATA:
    lr_workdate TYPE RANGE OF zsdsfit029-work_date,
    lr_pernr    TYPE RANGE OF zsdsfit029-pernr.

  IF p_wrkdtm IS NOT INITIAL.
    lr_workdate = VALUE #( BASE lr_workdate ( sign = 'I' option = 'EQ' low = p_wrkdtm ) ).
  ENDIF.

  IF p_pernrm <> '*'.
    lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = |{ p_pernrm ALPHA = IN }| ) ).
  ENDIF.

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
    WHERE log~data_type   =  @gc_data_type-memo
    AND   log~belnr       IN @s_belnrm
    AND   log~gjahr       IN @s_gjahrm
    AND   log~kunnr       IN @s_kunnrm
    AND   log~work_date   IN @lr_workdate
    AND   log~delete_flag = ''
    AND   log~action_type IN @s_acttym
    AND   log~status      IN @s_statum
    AND   log~tranf_no    IN @s_trnfm
    AND   log~xblnr       IN @s_xblnrm
    AND   log~pernr       IN @lr_pernr
    AND NOT EXISTS ( SELECT 'X'
                      FROM zsdsfit037 AS lnk
*<<F36K914812 Begin of del
*                      WHERE data_type = log~data_type
*                      AND   bukrs     = log~bukrs
*                      AND   belnr     = log~belnr
*                      AND   gjahr     = log~gjahr
*                      AND   buzei     = log~buzei
*                      AND   seq       = log~seq )
*<<F36K914812 End of del
*<<F36K914812 Begin of ins
                      INNER JOIN bkpf AS doc
                      ON  doc~bukrs = lnk~bukrs_clr
                      AND doc~belnr = lnk~belnr_clr
                      AND doc~gjahr = lnk~gjahr_clr
                      WHERE lnk~data_type = log~data_type
                      AND   lnk~bukrs     = log~bukrs
                      AND   lnk~belnr     = log~belnr
                      AND   lnk~gjahr     = log~gjahr
                      AND   lnk~buzei     = log~buzei
                      AND   lnk~seq       = log~seq
                      AND   doc~xreversal = '' )
*<<F36K914812 End of ins
    INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS.

  IF gt_output IS NOT INITIAL.

    gt_bplog = CORRESPONDING #( gt_output ).
    IF gt_bplog IS NOT INITIAL.

      SELECT *
        INTO TABLE @gt_status_hist                 "#EC CI_NO_TRANSFORM
        FROM zsdsfit038
        FOR ALL ENTRIES IN @gt_bplog
        WHERE data_type = @gt_bplog-data_type
        AND   bukrs = @gt_bplog-bukrs
        AND   belnr = @gt_bplog-belnr
        AND   gjahr = @gt_bplog-gjahr
        AND   buzei = @gt_bplog-buzei
        AND   seq   = @gt_bplog-seq.          "#EC CI_ALL_FIELDS_NEEDED

    ENDIF.

  ELSE.
    MESSAGE s000(38) WITH TEXT-e00. "No data found
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_memo_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_memo_all .

  DATA:
    lr_workdate TYPE RANGE OF zsdsfit029-work_date,
    lr_pernr    TYPE RANGE OF zsdsfit029-pernr.

  IF p_wrkdtm IS NOT INITIAL.
    lr_workdate = VALUE #( BASE lr_workdate ( sign = 'I' option = 'EQ' low = p_wrkdtm ) ).
  ENDIF.

  IF p_pernrm <> '*'.
    lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = |{ p_pernrm ALPHA = IN }| ) ).
  ENDIF.

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
    WHERE log~data_type   =  @gc_data_type-memo
    AND   log~belnr       IN @s_belnrm
    AND   log~gjahr       IN @s_gjahrm
    AND   log~kunnr       IN @s_kunnrm
    AND   log~work_date   IN @lr_workdate
    AND   log~action_type IN @s_acttym
    AND   log~status      IN @s_statum
    AND   log~tranf_no    IN @s_trnfm
    AND   log~xblnr       IN @s_xblnrm
    AND   log~pernr       IN @lr_pernr
    INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS.

  IF gt_output IS NOT INITIAL.

    gt_bplog = CORRESPONDING #( gt_output ).
    IF gt_bplog IS NOT INITIAL.

      SELECT *
        INTO TABLE @gt_status_hist                 "#EC CI_NO_TRANSFORM
        FROM zsdsfit038
        FOR ALL ENTRIES IN @gt_bplog
        WHERE data_type = @gt_bplog-data_type
        AND   bukrs = @gt_bplog-bukrs
        AND   belnr = @gt_bplog-belnr
        AND   gjahr = @gt_bplog-gjahr
        AND   buzei = @gt_bplog-buzei
        AND   seq   = @gt_bplog-seq.          "#EC CI_ALL_FIELDS_NEEDED

    ENDIF.

  ELSE.
    MESSAGE s000(38) WITH TEXT-e00. "No data found
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_memo_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_prepare_memo_output .

  DATA:
    lt_celltab  TYPE lvc_t_styl.

  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    CLEAR:
      lt_celltab.

    PERFORM f_get_pernr_name USING <ls_output>-pernr
                         CHANGING <ls_output>-full_name.

    IF rb_dism = gc_true.
      PERFORM f_fill_celltab_memo USING 'RO'
                           CHANGING lt_celltab.
    ELSE.
      PERFORM f_fill_celltab_memo USING 'RW'
                           CHANGING lt_celltab.
    ENDIF.

    <ls_output>-celltab     = lt_celltab.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_celltab_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_CELLTAB
*&---------------------------------------------------------------------*
FORM f_fill_celltab_memo  USING VALUE(uf_mode) TYPE char02
                     CHANGING ct_celltab  TYPE lvc_t_styl.

  DATA: lv_mode    TYPE raw4.

  IF uf_mode EQ 'RW'.
    lv_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE. "uf_mode eq 'RO'
    lv_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  PERFORM f_add_celltab USING lv_mode:
        'BLDAT'         CHANGING ct_celltab,
        'BUDAT'         CHANGING ct_celltab,
        'WRBTR'         CHANGING ct_celltab,
        'WAERS'         CHANGING ct_celltab,
        'PERNR'         CHANGING ct_celltab,
        'KUNNR'         CHANGING ct_celltab,
        'UMSKZ'         CHANGING ct_celltab,
        'XBLNR'         CHANGING ct_celltab,
        'WORK_DATE'     CHANGING ct_celltab,
*        'EXPS_AMT'      CHANGING ct_celltab,
*        'AUFNR_EXPS'    CHANGING ct_celltab,
        'FEE'           CHANGING ct_celltab,
        'AUFNR_FEE'     CHANGING ct_celltab,
*        'CASH_CON'      CHANGING ct_celltab,
        'FAEDT'         CHANGING ct_celltab,
        'DEDUCT_AMT'    CHANGING ct_celltab,
        'PAYNO'         CHANGING ct_celltab,
        'BRNCH'         CHANGING ct_celltab,
        'KOSTL'         CHANGING ct_celltab,
        'PRCTR'         CHANGING ct_celltab,
        'HBKID'         CHANGING ct_celltab,
        'HKTID'         CHANGING ct_celltab,
        'PYMT_METHOD'   CHANGING ct_celltab,
        'ACTION_TYPE'   CHANGING ct_celltab,
        'STATUS'        CHANGING ct_celltab,
        'REMARK'        CHANGING ct_celltab,
        'PAYMENT_DATE'  CHANGING ct_celltab,
        'RECEIVED_DATE' CHANGING ct_celltab,
        'RECEIPT_NO'    CHANGING ct_celltab,
        'BANK_DATE'     CHANGING ct_celltab,
        'CHEQUE_NO'     CHANGING ct_celltab,
        'BANKK'         CHANGING ct_celltab,
        'BANKN'         CHANGING ct_celltab,
        'BANKL'         CHANGING ct_celltab,
        'ZBANK_ITEM'    CHANGING ct_celltab,
        'FOLLOW_DATE'   CHANGING ct_celltab,
        'REMARK_VENDOR' CHANGING ct_celltab.

  IF rb_crem = gc_true.
    PERFORM f_add_celltab USING lv_mode:
        'BUKRS'           CHANGING ct_celltab,
        'BELNR'           CHANGING ct_celltab,
        'GJAHR'           CHANGING ct_celltab.
  ELSEIF rb_chgm = gc_true.
    PERFORM f_add_celltab USING lv_mode:
        'SEL'           CHANGING ct_celltab.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&      --> GT_STATUS_HIST
*&---------------------------------------------------------------------*
FORM f_update_memo  USING ut_output           TYPE tt_output
                          ut_curr_status_hist TYPE tt_status_hist.

  DATA:
    lt_grp            TYPE tt_grp_doc,
    lt_bplog          TYPE STANDARD TABLE OF zsdsfit029,
    lt_status_hist    TYPE tt_status_hist,
    lt_bank_statement TYPE tt_bank_statement.

  lt_grp = VALUE #( FOR ls_output IN ut_output WHERE ( sel = 'X'  ) ( CORRESPONDING #( ls_output  ) )  ).

  IF lt_grp IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e10.
    RETURN.
  ENDIF.

  lt_bplog = CORRESPONDING #( lt_grp ).

  LOOP AT lt_bplog ASSIGNING FIELD-SYMBOL(<ls_zsdsfit029>).

    "<<F36K912524 begin of ins
    IF <ls_zsdsfit029>-prctr IS NOT INITIAL.
      <ls_zsdsfit029>-prctr       = |{ <ls_zsdsfit029>-prctr ALPHA = IN }|.
    ENDIF.
    IF <ls_zsdsfit029>-kostl IS NOT INITIAL.
      <ls_zsdsfit029>-kostl       = |{ <ls_zsdsfit029>-kostl ALPHA = IN }|.
    ENDIF.
    "<<F36K912524 end of ins

    <ls_zsdsfit029>-update_by   = sy-uname.
    <ls_zsdsfit029>-update_on   = sy-datum.
    <ls_zsdsfit029>-update_time = sy-uzeit.

    PERFORM f_create_status_hist USING <ls_zsdsfit029>
                                       ut_curr_status_hist
                                 CHANGING lt_status_hist.

  ENDLOOP.

  IF lt_status_hist IS NOT INITIAL.
    MODIFY zsdsfit038 FROM TABLE lt_status_hist.
  ENDIF.

  MODIFY zsdsfit029 FROM TABLE lt_bplog.

  IF lt_bank_statement IS NOT INITIAL.
    SORT lt_bank_statement BY hbkid	zbank_item trnfer_number DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_bank_statement COMPARING hbkid	zbank_item.

    UPDATE zsdsfit042
      FROM TABLE @lt_bank_statement INDICATORS SET STRUCTURE col_ind.
  ENDIF.

*  IF lt_doc_update IS NOT INITIAL.
*    PERFORM fi_doc_update USING lt_doc_update.
*  ENDIF.

  MESSAGE s000(38) WITH TEXT-r04.
  COMMIT WORK AND WAIT. "Added 02.12.2024

  IF gref_grid IS BOUND.
    gref_grid->set_ready_for_input(
                       EXPORTING i_ready_for_input = 0 ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_default_init_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_OUTTAB_NEW
*&---------------------------------------------------------------------*
FORM f_set_default_init_memo  CHANGING cs_outtab_new TYPE zsdsfis085.

*<<F36K910991 start of ins
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = p_bukrsm
      date  = sy-datum
    IMPORTING
*     CURRM =
      curry = cs_outtab_new-gjahr.
*<<F36K910991 end of ins

  cs_outtab_new-data_type   = gc_data_type-memo.
  cs_outtab_new-bukrs       = p_bukrsm.
*  cs_outtab_new-gjahr       = sy-datum(4). "<<F36K910991 del
  cs_outtab_new-waers       = gv_waers.
  cs_outtab_new-pernr       = p_pernrm.
  cs_outtab_new-full_name   = gv_fullname.
  cs_outtab_new-work_date   = p_wrkdtm.
  cs_outtab_new-pymt_method = p_dfpymm.
  cs_outtab_new-action_type = p_dfactm.
  cs_outtab_new-status      = p_dfstam.
  cs_outtab_new-hbkid       = p_dfhbkm.
  cs_outtab_new-hktid       = p_dfhktm.
  cs_outtab_new-bank_date   = p_dfbkdm.
  cs_outtab_new-cheque_no   = p_dfchqm.
  cs_outtab_new-umskz       = 'D'.

  cs_outtab_new-brnch       = gs_default_memo-brnch.
  cs_outtab_new-prctr       = gs_default_memo-prctr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_new_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_new_memo .

  DO 1 TIMES.
    APPEND INITIAL LINE TO gt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    PERFORM f_set_default_init_memo CHANGING <ls_output>.
  ENDDO.

ENDFORM.
