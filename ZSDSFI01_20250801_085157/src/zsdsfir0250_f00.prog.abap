*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250_F00
*  Creation Date      : 21.05.2024
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic - common logic
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f_variant_f4
*&---------------------------------------------------------------------*
FORM f_variant_f4 CHANGING cs_variant TYPE disvariant-variant.

  DATA:
    lv_exit    TYPE char01,
    ls_variant TYPE disvariant.


  gs_variant-report  = sy-repid.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      is_variant    = gs_variant
*     IT_DEFAULT_FIELDCAT =
*     I_SAVE        = G_VARIANT_SAVE
    IMPORTING
      e_exit        = lv_exit
      es_variant    = ls_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE i000(0k) WITH TEXT-101.
  ENDIF.

  IF lv_exit IS INITIAL.
*    gs_variant-variant = ls_variant-variant.
    cs_variant         = ls_variant-variant.
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

  p_bukrs           = '1000'.
  p_bukrsn          = '1000'.
  p_bukrsu          = '1000'.
  p_bukrsm          = '1000'.
  p_wrkdt           = sy-datum.
  p_wrkdtn          = sy-datum.
  p_wrkdtu          = sy-datum.
  p_wrkdtm          = sy-datum.

  tab_bt1           = TEXT-tb1.
*  tab_bt2           = TEXT-tb2.
  tab_bt3           = TEXT-tb3.
  tab_bt4           = TEXT-tb4.
  tab_bt5           = TEXT-tb5.
  tab_blk-prog      = sy-repid.

  IF sy-tcode = gc_tcode_his.
    tab_blk-dynnr     = '400'.
    tab_blk-activetab = gc_tab_sel-history_log.
    gv_data_type      = gc_tab_sel-history_log.
  ELSE.
    IF tab_blk-activetab IS INITIAL.
      GET PARAMETER ID 'ZSDS_TAB' FIELD tab_blk-dynnr.
      CASE tab_blk-dynnr.
        WHEN '100'.
          tab_blk-activetab = gc_tab_sel-collection_log.
          gv_data_type      = gc_tab_sel-collection_log.
        WHEN '200'.
          tab_blk-activetab = gc_tab_sel-new_entry.
          gv_data_type      = gc_tab_sel-new_entry.
        WHEN '300'.
          tab_blk-activetab = gc_tab_sel-update_bank.
          gv_data_type      = gc_tab_sel-update_bank.
        WHEN '350'.
          tab_blk-activetab = gc_tab_sel-memo.
          gv_data_type      = gc_tab_sel-memo.
*      WHEN '400'.
*        tab_blk-activetab = gc_tab_sel-history_log.
*        gv_data_type      = gc_tab_sel-history_log.
        WHEN OTHERS.
          tab_blk-dynnr     = '100'.
          tab_blk-activetab = gc_tab_sel-collection_log.
          gv_data_type      = gc_tab_sel-collection_log.
      ENDCASE.
    ENDIF.
  ENDIF.

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid = sy-repid          " ABAP Program Name
*     irt_param =                  " Parameter name
*     irt_ext  =                  " Parameter extension
    IMPORTING
      et_gen_c = DATA(lt_genc)     " General Constants (GENC)
  ).

  IF p_dfhbkn IS INITIAL AND
     p_dfhktn IS INITIAL.
    LOOP AT lt_genc INTO DATA(ls_genc)      ##INTO_OK.
      CASE ls_genc-param.
        WHEN gc_genc_param-df_house_bank.
          p_dfhbkn  = ls_genc-value_low.
        WHEN gc_genc_param-df_bank_acct.
          p_dfhktn = ls_genc-value_low.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  IF p_dfhbkm IS INITIAL AND
     p_dfhktm IS INITIAL.
    LOOP AT lt_genc INTO ls_genc      ##INTO_OK.
      CASE ls_genc-param.
        WHEN gc_genc_param-df_house_bank.
          p_dfhbkm  = ls_genc-value_low.
        WHEN gc_genc_param-df_bank_acct.
          p_dfhktm = ls_genc-value_low.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  IF sy-tcode = gc_tcode_his.
    SET TITLEBAR  '9001' ##TITL_UNDEF.
  ELSE.
    SET TITLEBAR  '9000' ##TITL_UNDEF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_set_sel_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_sel_screen .

  DATA: lt_exclude TYPE TABLE OF sy-ucomm.

  IF sy-tcode <> gc_tcode_his.
    APPEND gc_func-mass_upd TO lt_exclude.
  ELSE.
    APPEND: gc_func-post_inc  TO lt_exclude,
            gc_func-his_dsp   TO lt_exclude.
  ENDIF.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = 'SEL'
      p_program = sy-repid
    TABLES
      p_exclude = lt_exclude.



  LOOP AT SCREEN.

    CASE gv_data_type.

*--------------------------------------------------------------------*
* Collection log - Sub screen 100
*--------------------------------------------------------------------*
      WHEN gc_tab_sel-collection_log.
        CASE gc_true.
            "Create mode
          WHEN rb_cre.
            IF screen-group1 = 'TRF'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.

          WHEN rb_chg.
            IF screen-group1 = 'DEF'.
              screen-active = 0.
              MODIFY SCREEN.
              CONTINUE.
            ENDIF.

            IF screen-group1 = 'CRE'.
              screen-active = 0.
              MODIFY SCREEN.
              CONTINUE.
            ENDIF.

          WHEN rb_dis.
            IF screen-group1 = 'DEF'.
              screen-active = 0.
              MODIFY SCREEN.
              CONTINUE.
            ELSEIF screen-group1 = 'AMT'.
              screen-active = 0.
              MODIFY SCREEN.
              CONTINUE.
            ENDIF.

            IF screen-group1 = 'CRE'.
              screen-active = 0.
              MODIFY SCREEN.
              CONTINUE.
            ENDIF.

        ENDCASE.

        IF rb_chg = gc_true OR
           rb_dis = gc_true.
          IF screen-group1 = 'BI1'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF cb_inck IS INITIAL AND
           cb_inbl IS INITIAL.
          IF screen-group1 = 'BI1' OR
             screen-group1 = 'BL2'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.

        ELSEIF cb_inbl IS INITIAL.
          IF screen-group1 = 'BI1'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.

        "Disable all change
        IF screen-group1 = 'CIN' OR
           screen-group1 = 'CBL'.
          screen-active = 0.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.

      WHEN gc_tab_sel-new_entry.
*--------------------------------------------------------------------*
* New entry - Sub screen 200
*--------------------------------------------------------------------*
        CASE gc_true.
          WHEN rb_cren.
            IF screen-group1 = 'CSN' OR
               screen-group1 = 'TRN'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.
          WHEN rb_chgn OR rb_disn.
            IF screen-group1 = 'DFN'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.
        ENDCASE.

*--------------------------------------------------------------------*
* Update for bank's vendor - Sub screen 300
*--------------------------------------------------------------------*
      WHEN gc_tab_sel-update_bank.

        CASE gc_true.
          WHEN rb_creu.
            IF screen-group1 = 'TRU'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.
          WHEN rb_chgu OR rb_disu.
            IF screen-group1 = 'DFU'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.

            IF screen-group1 = 'CRU'.
              screen-active = 0.
              MODIFY SCREEN.
              CONTINUE.
            ENDIF.

        ENDCASE.

        IF rb_chgu IS NOT INITIAL OR
           rb_disu IS NOT INITIAL.
          IF screen-group1 = 'BI3'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.

        ELSEIF cb_inblu IS INITIAL AND
               cb_incku IS INITIAL.
          IF screen-group1 = 'BI3' OR
             screen-group1 = 'BI4'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.

        ELSEIF cb_inblu IS INITIAL.
          IF screen-group1 = 'BI3'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.

*--------------------------------------------------------------------*
* Update memo - Sub screen 350
*--------------------------------------------------------------------*
      WHEN gc_tab_sel-memo.
        CASE gc_true.
          WHEN rb_crem.
            IF screen-group1 = 'CSM' OR
               screen-group1 = 'TRM'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.
          WHEN rb_chgm OR rb_dism.
            IF screen-group1 = 'DFM'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.
        ENDCASE.

*<-- Start of Insertion 06.12.2024 (History Log Criteria format)
*--------------------------------------------------------------------*
* History Log - Sub screen 400
*--------------------------------------------------------------------*
      WHEN gc_tab_sel-history_log.
*       Display Only Criteria
        IF screen-group1 = 'DIS'.
          IF rb_hdis EQ gc_true.
            screen-active = 1.
          ELSE.
            screen-active = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
*--> End of Insertion 06.12.2024
    ENDCASE.

  ENDLOOP.
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
    lr_bankk          TYPE RANGE OF zsdsfit029-bankk,
    lr_bankn          TYPE RANGE OF zsdsfit029-bankn,
    lr_order_exp      TYPE RANGE OF zsdsfit029-aufnr_exps,
    lr_order_fee      TYPE RANGE OF zsdsfit029-aufnr_fee,
    lr_srv_whtax_rate TYPE RANGE OF with_item-qsatz,

    lt_genc           TYPE zcl_sdsca_utilities=>tt_gen_c.

*<<F36K920651 - 01 Begin of ins
  CLEAR:
    gt_history_log,
    gt_clr_link,
    gt_onetime,
    gt_do,
    gt_output,
    gt_whtax,
    gt_bplog,
    gt_status_hist,
    gt_partial,
    gt_tax_base,
    gt_whtax_type,
    gt_so,
    gt_user_data.
*<<F36K920651 - 01 End of ins

  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.
      IF rb_cre = gc_true OR
         rb_chg = gc_true.
        gv_edit = abap_true.
      ELSE.
        gv_edit = abap_false.
      ENDIF.
    WHEN gc_tab_sel-update_bank.
      IF rb_creu = gc_true OR
         rb_chgu = gc_true.
        gv_edit = abap_true.
      ELSE.
        gv_edit = abap_false.
      ENDIF.
    WHEN gc_tab_sel-history_log.
      IF rb_hchg = gc_true.
        gv_edit = abap_true.
      ELSE.
        gv_edit = abap_false.
      ENDIF.
  ENDCASE.

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid           " ABAP Program Name
      if_param = gc_genc_param-doc_type  " Parameter name
*     if_ext   =                   " Parameter Extension
    IMPORTING
      et_range = gr_blart
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                     " ABAP Program Name
      if_param = gc_genc_param-bank_transfer  " Parameter name
*     if_ext   =                             " Parameter Extension
    IMPORTING
      et_range = gr_bank
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                     " ABAP Program Name
      if_param = gc_genc_param-pdc            " Parameter name
*     if_ext   =                             " Parameter Extension
    IMPORTING
      et_range = gr_pdc
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                         " ABAP Program Name
      if_param = gc_genc_param-man_whtax_doc_typ  " Parameter name
*     if_ext   =                               " Parameter Extension
    IMPORTING
      et_range = gr_whtax_blart
  ).

  "Default PDC bank key , account
  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                         " ABAP Program Name
      if_param = gc_genc_param-df_pdc_bank_key    " Parameter name
    IMPORTING
      et_range = lr_bankk
  ).

  IF lr_bankk IS NOT INITIAL.
    gv_df_pdc_bank_key = lr_bankk[ 1 ]-low.
  ENDIF.

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                         " ABAP Program Name
      if_param = gc_genc_param-df_pdc_bank_acct   " Parameter name
    IMPORTING
      et_range = lr_bankn
  ).

  IF lr_bankn IS NOT INITIAL.
    gv_df_pdc_bank_acct = lr_bankn[ 1 ]-low.
  ENDIF.

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
*

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                         " ABAP Program Name
      if_param = gc_genc_param-srv_inv_doc_type   " Parameter name
    IMPORTING
      et_range = gr_srv_blart
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                         " ABAP Program Name
      if_param = gc_genc_param-srv_whtax_rate     " Parameter name
    IMPORTING
      et_range = lr_srv_whtax_rate
  ).

  IF lr_srv_whtax_rate IS NOT INITIAL.
    gv_srv_whtax_rate = lr_srv_whtax_rate[ 1 ]-low.
  ENDIF.

  IF ( gv_data_type = gc_tab_sel-collection_log AND cb_spgl IS NOT INITIAL  ) OR
     ( gv_data_type = gc_tab_sel-update_bank    AND cb_spglu IS NOT INITIAL ).
    zcl_sdsca_utilities=>get_gen_c_range(
      EXPORTING
        if_repid = sy-repid             " ABAP Program Name
        if_param = gc_genc_param-sp_gl  " Parameter name
*       if_ext   =                     " Parameter Extension
      IMPORTING
        et_range = gr_spgl
    ).
  ENDIF.

  IF ( gv_data_type = gc_tab_sel-collection_log AND cb_norm IS NOT INITIAL ) OR
     ( gv_data_type = gc_tab_sel-update_bank    AND cb_normu IS NOT INITIAL ).
    gr_spgl = VALUE #( BASE gr_spgl ( sign = 'I' option = 'EQ' low = '' ) ).
  ENDIF.

  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.

      SELECT SINGLE waers
        FROM t001
        WHERE bukrs = @p_bukrs
        INTO @gv_waers.

    WHEN gc_tab_sel-update_bank.

      SELECT SINGLE waers
        FROM t001
        WHERE bukrs = @p_bukrsu
        INTO @gv_waers.

  ENDCASE.

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid = sy-repid          " ABAP Program Name
*     irt_param =                  " Parameter name
*     irt_ext  =                  " Parameter extension
    IMPORTING
      et_gen_c = lt_genc                 " General Constants (GENC)
  ).

  LOOP AT lt_genc INTO DATA(ls_genc)      ##INTO_OK.
    CASE ls_genc-param.
      WHEN gc_genc_param-prctr.
*        gs_default_coll_log-prctr = ls_genc-value_low. "<<F36K912524 - del
        gs_default_coll_log-prctr = |{ ls_genc-value_low ALPHA = IN }|. "<<F36K912524 - ins
      WHEN gc_genc_param-kostl.
*        gs_default_coll_log-kostl = ls_genc-value_low. "<<F36K912524 - del
        gs_default_coll_log-kostl = |{ ls_genc-value_low ALPHA = IN }|. "<<F36K912524 - ins
    ENDCASE.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_chk_sel_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_chk_sel_screen.

  CASE sy-ucomm.
    WHEN gc_func-post_inc.
      TRY.
          CALL TRANSACTION 'ZSDSFI020' WITH AUTHORITY-CHECK.
        CATCH cx_sy_authorization_error .
*   Error You are not authorized to use transaction &
          MESSAGE s172(00) WITH 'Post incoming payment'(001).
          LEAVE PROGRAM.
      ENDTRY.
    WHEN gc_func-mass_upd.
      TRY.
          CALL TRANSACTION 'ZSDSFI014' WITH AUTHORITY-CHECK.
        CATCH cx_sy_authorization_error .
*   Error You are not authorized to use transaction &
          MESSAGE s172(00) WITH 'Post incoming payment'(001).
          LEAVE PROGRAM.
      ENDTRY.
    WHEN gc_func-his_dsp.
      TRY.
          CALL TRANSACTION 'ZSDSFI048' WITH AUTHORITY-CHECK.
        CATCH cx_sy_authorization_error .
*   Error You are not authorized to use transaction &
          MESSAGE s172(00) WITH 'Post incoming payment'(001).
          LEAVE PROGRAM.
      ENDTRY.
  ENDCASE.

  IF sy-ucomm <> 'ONLI'.
    RETURN.
  ENDIF.

  CASE gv_data_type.
*--------------------------------------------------------------------*
* Collector log
*--------------------------------------------------------------------*
    WHEN gc_tab_sel-collection_log.

      CASE gc_true.
        WHEN rb_cre.
          IF p_wrkdt IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e22.
          ENDIF.
          IF p_dfpymt IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e12.
          ENDIF.
          IF p_dfact IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e13.
          ENDIF.
          IF p_dfsta IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e14.
          ENDIF.
          IF p_rcvamt IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e11.
          ENDIF.

          IF p_dfpymt IN gr_bank_trn AND
             gr_bank_trn IS NOT INITIAL.
            IF p_dfhbk IS INITIAL.
              MESSAGE e000(38) WITH TEXT-e25. "Please input house bank
            ENDIF.

            IF p_zbank IS INITIAL.
              MESSAGE e000(38) WITH TEXT-e21.
            ELSE.
              SELECT SINGLE cheque_date
                FROM zsdsv_bank_stmt
                WHERE hbkid      = @p_dfhbk
                AND   zbank_item = @p_zbank
                INTO @DATA(lv_cheque_date)    ##NEEDED ##WARN_OK.
              IF sy-subrc NE 0.
                MESSAGE e000(38) WITH TEXT-e23. "Invalid Bank Item
              ENDIF.
            ENDIF.
          ENDIF.

          IF p_dfbkdt IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e28. "Please input date for payment by cheque
          ENDIF.

          IF p_dfhbk IS NOT INITIAL AND
             p_dfhkt IS NOT INITIAL.
            SELECT SINGLE hkont
              INTO @DATA(lv_hkont)      ##NEEDED
              FROM t012k
              WHERE bukrs = @p_bukrs
              AND   hbkid = @p_dfhbk
              AND   hktid = @p_dfhkt.
            IF sy-subrc NE 0.
              MESSAGE e000(38) WITH TEXT-e26 p_dfhkt TEXT-e27.
            ENDIF.
          ENDIF.
        WHEN rb_chg.
          IF p_rcvamt IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e11.
          ENDIF.
      ENDCASE.

      IF cb_norm IS INITIAL AND cb_spgl IS INITIAL.
        MESSAGE e000(38) WITH TEXT-e01.
      ENDIF.

*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
    WHEN gc_tab_sel-new_entry.

      CASE gc_true.
        WHEN rb_cren.
          IF p_wrkdtn IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e22.
          ENDIF.
          IF p_rcvamn IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e11.
          ENDIF.
          IF p_dfpymn IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e12.
          ENDIF.
          IF p_dfactn IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e13.
          ENDIF.
          IF p_dfstan IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e14.
          ENDIF.
        WHEN rb_chgn.
          IF p_rcvamn IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e11.
          ENDIF.
      ENDCASE.

*--------------------------------------------------------------------*
* Update for bank's vendor
*--------------------------------------------------------------------*
    WHEN gc_tab_sel-update_bank.

*      IF p_pernru IS INITIAL.
*        MESSAGE e000(38) WITH TEXT-e15.
*      ENDIF.

      IF cb_normu IS INITIAL AND cb_spglu IS INITIAL.
        MESSAGE e000(38) WITH TEXT-e01.
      ENDIF.

      CASE gc_true.
        WHEN rb_creu.
          IF p_wrkdtu IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e22.
          ENDIF.
*          IF p_dfpymu IS INITIAL.
*            MESSAGE e000(38) WITH TEXT-e12.
*          ENDIF.
*          IF p_dfactu IS INITIAL.
*            MESSAGE e000(38) WITH TEXT-e13.
*          ENDIF.
*          IF p_dfstau IS INITIAL.
*            MESSAGE e000(38) WITH TEXT-e14.
*          ENDIF.
      ENDCASE.

*--------------------------------------------------------------------*
* Memo
*--------------------------------------------------------------------*
    WHEN gc_tab_sel-memo.

      CASE gc_true.
        WHEN rb_crem.
          IF p_wrkdtm IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e22.
          ENDIF.
*          IF p_dfpymm IS INITIAL.
*            MESSAGE e000(38) WITH TEXT-e12.
*          ENDIF.
*          IF p_dfactm IS INITIAL.
*            MESSAGE e000(38) WITH TEXT-e13.
*          ENDIF.
*          IF p_dfstam IS INITIAL.
*            MESSAGE e000(38) WITH TEXT-e14.
*          ENDIF.
*        WHEN rb_chgm.
*
      ENDCASE.

  ENDCASE.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_chk_sel_screen_block_b1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_chk_sel_screen_block_b1.
  DATA: lv_pernr TYPE zsdsfit029-pernr.

  CHECK sy-ucomm = 'ONLI'.

*--------------------------------------------------------------------*
* Collector log
*--------------------------------------------------------------------*
  CHECK gv_data_type = gc_tab_sel-collection_log.

  IF p_pernr IS INITIAL.
    MESSAGE e000(38) WITH TEXT-e15.
  ENDIF.

  lv_pernr = |{ p_pernr ALPHA = IN }|.

  IF NOT ( rb_dis = gc_true AND p_pernr = '*' ).

    PERFORM f_get_pernr_name USING lv_pernr
                             CHANGING gv_fullname.

    IF gv_fullname IS INITIAL.
      MESSAGE e000(38) WITH 'Personnel Number'(e91) | { p_pernr ALPHA = OUT } | 'does not exist'(e92).
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_open_items
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_open_items .

  CASE gc_true.
    WHEN rb_cre.
      PERFORM f_get_open_items_for_create.
      PERFORM f_prepare_create_output.
    WHEN rb_chg.
      PERFORM f_get_bp_log_open.
      PERFORM f_prepare_change_output.
    WHEN rb_dis.
      PERFORM f_get_bp_log_all.
      PERFORM f_prepare_change_output.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_open_items_for_create
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_open_items_for_create .

  DATA:
    gv_pernr   TYPE zsdsfit029-pernr.


  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.
      DATA(lr_belnr)        = s_belnr[].
      DATA(lr_gjahr)        = s_gjahr[].
      DATA(lr_kunnr)        = s_kunnr[].
      DATA(lr_acttyp)       = s_acttyp[].
      DATA(lr_status)       = s_status[].
*      DATA(lr_trnf)        = s_trnf[].
      DATA(lr_bill)         = s_bill[].
      DATA(lr_pldate)       = s_pldate[].
      DATA(lr_xblnr)        = s_xblnr[].

      DATA(lv_include_bl)   = cb_inbl.
      DATA(lv_include_inv)  = cb_inck.
      DATA(lv_wrkdate)      = p_wrkdt.

      gv_pernr = |{ p_pernr ALPHA = IN }|.
    WHEN gc_tab_sel-update_bank.
      lr_belnr              = s_belnru[].
      lr_gjahr              = s_gjahru[].
      lr_kunnr              = s_kunnru[].
      lr_acttyp             = s_acttyu[].
      lr_status             = s_statuu[].
*      lr_trnf              = s_trnfu[].
      lr_bill               = s_billu[].
      lr_pldate             = s_pldatu[].
      lr_xblnr              = s_xblnru[].

      lv_include_bl         = cb_inblu.
      lv_include_inv        = cb_incku.
      lv_wrkdate            = p_wrkdtu.

      gv_pernr = |{ p_pernru ALPHA = IN }|.
  ENDCASE.

  IF lv_include_bl IS NOT INITIAL AND
   ( lr_bill    IS NOT INITIAL OR
     lr_pldate  IS NOT INITIAL OR
     lr_acttyp  IS NOT INITIAL OR
     lr_status  IS NOT INITIAL ).
    SELECT
      @lv_wrkdate AS work_date,
      opn~bukrs,
      opn~belnr,
      opn~gjahr,
      opn~buzei,
      opn~blart,
      bill_pl~billpl_no,
      bill_pl~billpl_date,
      bill_pl~action_type,
      bill_pl~status,
      @gv_pernr AS pernr,
      @gv_fullname AS full_name,
      opn~kunnr,
      opn~umskz,
      opn~xblnr,
      opn~bldat,
      opn~budat,
      opn~zfbdt,
      opn~zterm,
*      docu~prctr,
*      docu~prctr AS kostl,
      @gs_default_coll_log-prctr AS prctr,
      @gs_default_coll_log-kostl AS kostl,
      CASE
        WHEN cust~ktokd = 'Z010' THEN
          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
        WHEN cust~ktokd = 'Z050' THEN
          concat_with_space( partner~name_first, partner~name_last, 1 )
        ELSE
          concat_with_space( partner~name_org1, partner~name_org2, 1 )
      END AS cust_name,
      opn~wrbtr,
      opn~waers,
      opn~shkzg,
      opn~bupla AS brnch,
      opn~belnr AS receipt_no,
      hdr~awkey  AS vbeln_vf
      FROM bsid_view AS opn
      INNER JOIN acdoca AS docu
      ON  docu~rbukrs = opn~bukrs
      AND docu~belnr  = opn~belnr
      AND docu~gjahr  = opn~gjahr
      AND docu~buzei  = opn~buzei
      INNER JOIN bkpf AS hdr
      ON  hdr~bukrs = opn~bukrs
      AND hdr~belnr = opn~belnr
      AND hdr~gjahr = opn~gjahr
      INNER JOIN t001 AS comp
      ON  opn~bukrs = comp~bukrs
      AND opn~waers = comp~waers
      INNER JOIN kna1 AS cust
      ON opn~kunnr = cust~kunnr
      INNER JOIN cvi_cust_link AS link
      ON cust~kunnr = link~customer
      INNER JOIN but000 AS partner
      ON partner~partner_guid = link~partner_guid
*      INNER JOIN zsdsfit033 AS bill_pl
*      ON opn~xref2 = bill_pl~billpl_no
      INNER JOIN zsdsfit035 AS bill_pl_i
      ON  bill_pl_i~bukrs = opn~bukrs
      AND bill_pl_i~belnr = opn~belnr
      AND bill_pl_i~gjahr = opn~gjahr
      AND bill_pl_i~delfg = ''
      INNER JOIN zsdsfit033 AS bill_pl
      ON bill_pl_i~billpl_no = bill_pl~billpl_no
      AND  bill_pl~delfg     = ''
      WHERE opn~kunnr           IN @lr_kunnr
      AND   opn~blart           IN @gr_blart
      AND   opn~umskz           IN @gr_spgl
      AND   opn~belnr           IN @lr_belnr
      AND   opn~gjahr           IN @lr_gjahr
      AND   opn~xblnr           IN @lr_xblnr
      AND   bill_pl~billpl_no   IN @lr_bill
      AND   bill_pl~billpl_date IN @lr_pldate
      AND   bill_pl~action_type IN @lr_acttyp
      AND   bill_pl~status      IN @lr_status
      AND   bill_pl~delfg       = ''
      AND   docu~rldnr          = '0L'
      INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS. "#EC CI_BUFFJOIN

  ELSEIF lr_acttyp  IS INITIAL AND
         lr_status  IS INITIAL .

    SELECT
      @lv_wrkdate AS work_date,
      opn~bukrs,
      opn~belnr,
      opn~gjahr,
      opn~buzei,
      opn~blart,
      bill_pl~billpl_no,
      bill_pl~billpl_date,
      bill_pl~action_type,
      bill_pl~status,
      @gv_pernr AS pernr,
      @gv_fullname AS full_name,
      opn~kunnr,
      opn~umskz,
      opn~xblnr,
      opn~bldat,
      opn~budat,
      opn~zfbdt,
      opn~zterm,
*      docu~prctr,
*      docu~prctr AS kostl,
      @gs_default_coll_log-prctr AS prctr,
      @gs_default_coll_log-kostl AS kostl,
      CASE
        WHEN cust~ktokd = 'Z010' THEN
          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
        WHEN cust~ktokd = 'Z050' THEN
          concat_with_space( partner~name_first, partner~name_last, 1 )
        ELSE
          concat_with_space( partner~name_org1, partner~name_org2, 1 )
      END AS cust_name,
      opn~wrbtr,
      opn~waers,
      opn~shkzg,
      opn~bupla AS brnch,
      opn~belnr AS receipt_no,
      hdr~awkey  AS vbeln_vf
      FROM bsid_view AS opn
      INNER JOIN acdoca AS docu
      ON  docu~rbukrs = opn~bukrs
      AND docu~belnr  = opn~belnr
      AND docu~gjahr  = opn~gjahr
      AND docu~buzei  = opn~buzei
      INNER JOIN bkpf AS hdr
      ON  hdr~bukrs = opn~bukrs
      AND hdr~belnr = opn~belnr
      AND hdr~gjahr = opn~gjahr
      INNER JOIN t001 AS comp
      ON  opn~bukrs = comp~bukrs
      AND opn~waers = comp~waers
      INNER JOIN kna1 AS cust
      ON opn~kunnr = cust~kunnr
      INNER JOIN cvi_cust_link AS link
      ON cust~kunnr = link~customer
      INNER JOIN but000 AS partner
      ON partner~partner_guid = link~partner_guid
*      LEFT JOIN zsdsfit033 AS bill_pl
*      ON    bill_pl~billpl_no   = opn~xref2
*      AND   bill_pl~delfg       = ''
      LEFT JOIN zsdsfit035 AS bill_pl_i
      ON  bill_pl_i~bukrs = opn~bukrs
      AND bill_pl_i~belnr = opn~belnr
      AND bill_pl_i~gjahr = opn~gjahr
      AND bill_pl_i~delfg = ''
      LEFT JOIN zsdsfit033 AS bill_pl
      ON bill_pl_i~billpl_no = bill_pl~billpl_no
      AND bill_pl~delfg = ''
      WHERE opn~kunnr           IN @lr_kunnr
      AND   opn~blart           IN @gr_blart
      AND   opn~umskz           IN @gr_spgl
      AND   opn~belnr           IN @lr_belnr
      AND   opn~gjahr           IN @lr_gjahr
      AND   opn~xblnr           IN @lr_xblnr
      AND   docu~rldnr          = '0L'
      INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS. "#EC CI_BUFFJOIN

  ENDIF.

  IF lv_include_inv IS NOT INITIAL.

    SELECT
      @p_wrkdt AS work_date,
      opn~bukrs,
      opn~belnr,
      opn~gjahr,
      opn~buzei,
      opn~blart,
      opn~xref2 AS billpl_no,
      inv_st~bl_actty AS action_type,
      inv_st~bl_stat AS status,
      @gv_pernr AS pernr,
      @gv_fullname AS full_name,
      opn~kunnr,
      opn~umskz,
      opn~xblnr,
      opn~bldat,
      opn~budat,
      opn~zfbdt,
      opn~zterm,
*      docu~prctr,
*      docu~prctr AS kostl,
      @gs_default_coll_log-prctr AS prctr,
      @gs_default_coll_log-kostl AS kostl,
      CASE
        WHEN cust~ktokd = 'Z010' THEN
          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
        WHEN cust~ktokd = 'Z050' THEN
          concat_with_space( partner~name_first, partner~name_last, 1 )
        ELSE
          concat_with_space( partner~name_org1, partner~name_org2, 1 )
      END AS cust_name,
      opn~wrbtr,
      opn~waers,
      opn~shkzg,
      opn~bupla AS brnch,
      opn~belnr AS receipt_no,
      hdr~awkey  AS vbeln_vf
      FROM bsid_view AS opn
*<<F36K914812 - 07 Begin of del
*      INNER JOIN acdoca AS docu
*      ON  docu~rbukrs = opn~bukrs
*      AND docu~belnr  = opn~belnr
*      AND docu~gjahr  = opn~gjahr
*      AND docu~buzei  = opn~buzei
*<<F36K914812 - 07 End of del
      INNER JOIN bkpf AS hdr
      ON  hdr~bukrs = opn~bukrs
      AND hdr~belnr = opn~belnr
      AND hdr~gjahr = opn~gjahr
      INNER JOIN t001 AS comp
      ON  opn~bukrs = comp~bukrs
      AND opn~waers = comp~waers
      INNER JOIN kna1 AS cust
      ON opn~kunnr = cust~kunnr
      INNER JOIN cvi_cust_link AS link
      ON cust~kunnr = link~customer
      INNER JOIN but000 AS partner
      ON partner~partner_guid = link~partner_guid
      INNER JOIN zsdsfit027 AS inv
      ON  hdr~awtyp = 'VBRK'
      AND hdr~awkey = inv~vbeln
      INNER JOIN zsdsfic028 AS inv_st
      ON inv_st~stat_act = inv~zstat_act
      WHERE opn~kunnr           IN @lr_kunnr
      AND   opn~blart           IN @gr_blart
      AND   opn~umskz           IN @gr_spgl
      AND   opn~belnr           IN @lr_belnr
      AND   opn~gjahr           IN @lr_gjahr
      AND   opn~xblnr           IN @lr_xblnr
      AND   inv_st~bl_actty     IN @lr_acttyp
      AND   inv_st~bl_stat      IN @lr_status
*      AND   docu~rldnr          = '0L'  "<<F36K914812 - 07 Begin of del
      UNION
    SELECT
      @p_wrkdt AS work_date,
      opn~bukrs,
      opn~belnr,
      opn~gjahr,
      opn~buzei,
      opn~blart,
      opn~xref2 AS billpl_no,
      inv_st~bl_actty AS action_type,
      inv_st~bl_stat AS status,
      @gv_pernr AS pernr,
      @gv_fullname AS full_name,
      opn~kunnr,
      opn~umskz,
      opn~xblnr,
      opn~bldat,
      opn~budat,
      opn~zfbdt,
      opn~zterm,
*      docu~prctr,
*      docu~prctr AS kostl,
      @gs_default_coll_log-prctr AS prctr,
      @gs_default_coll_log-kostl AS kostl,
      CASE
        WHEN cust~ktokd = 'Z010' THEN
          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
        WHEN cust~ktokd = 'Z050' THEN
          concat_with_space( partner~name_first, partner~name_last, 1 )
        ELSE
          concat_with_space( partner~name_org1, partner~name_org2, 1 )
      END AS cust_name,
      opn~wrbtr,
      opn~waers,
      opn~shkzg,
      opn~bupla AS brnch,
      opn~belnr AS receipt_no,
      hdr~awkey  AS vbeln_vf
      FROM bsid_view AS opn
*<<F36K914812 - 07 Begin of del
*      INNER JOIN acdoca AS docu
*      ON  docu~rbukrs = opn~bukrs
*      AND docu~belnr  = opn~belnr
*      AND docu~gjahr  = opn~gjahr
*      AND docu~buzei  = opn~buzei
*<<F36K914812 - 07 End of del
      INNER JOIN bkpf AS hdr
      ON  hdr~bukrs = opn~bukrs
      AND hdr~belnr = opn~belnr
      AND hdr~gjahr = opn~gjahr
      INNER JOIN t001 AS comp
      ON  opn~bukrs = comp~bukrs
      AND opn~waers = comp~waers
      INNER JOIN kna1 AS cust
      ON opn~kunnr = cust~kunnr
      INNER JOIN cvi_cust_link AS link
      ON cust~kunnr = link~customer
      INNER JOIN but000 AS partner
      ON partner~partner_guid = link~partner_guid
      INNER JOIN vbrk AS bil_hdr
      ON  hdr~awtyp = 'VBRK'
      AND hdr~awkey = bil_hdr~vbeln
      INNER JOIN vbrp AS bil_itm
      ON bil_itm~vbeln = bil_hdr~vbeln
      INNER JOIN zsdsfit028 AS do
      ON do~vbeln = bil_itm~vgbel
      INNER JOIN zsdsfic028 AS inv_st
      ON inv_st~stat_act = do~zstat_act
      WHERE opn~kunnr           IN @lr_kunnr
      AND   opn~blart           IN @gr_blart
      AND   opn~umskz           IN @gr_spgl
      AND   opn~belnr           IN @lr_belnr
      AND   opn~gjahr           IN @lr_gjahr
      AND   opn~xblnr           IN @lr_xblnr
      AND   inv_st~bl_actty     IN @lr_acttyp
      AND   inv_st~bl_stat      IN @lr_status
*      AND   docu~rldnr          = '0L'   "<<F36K914812 - 07 del
      APPENDING CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS. "#EC CI_BUFFJOIN

  ENDIF.

*  IF lv_include_bl IS NOT INITIAL.
*    DELETE gt_output WHERE billpl_no IS NOT INITIAL.
*  ENDIF.

  SORT gt_output BY bukrs belnr gjahr buzei.
  DELETE ADJACENT DUPLICATES FROM gt_output COMPARING bukrs belnr gjahr buzei.

  IF gt_output IS NOT INITIAL.

    SELECT
      wi~bukrs,
      wi~belnr,
      wi~gjahr,
      wi~buzei,
      wi~witht,
      wi~wt_withcd,
      wi~wt_qsshb,
      rt~qsatz,
      rt~qproz
      FROM with_item AS wi
      INNER JOIN t001  AS comp
      ON  wi~bukrs      = comp~bukrs
      INNER JOIN t059z AS rt
      ON  rt~land1      = comp~land1
      AND rt~witht      = wi~witht
      AND rt~wt_withcd  = wi~wt_withcd
      INNER JOIN @gt_output AS opt                     "#EC CI_BUFFJOIN
      ON  opt~bukrs = wi~bukrs
      AND opt~belnr = wi~belnr
      AND opt~gjahr = wi~gjahr
      AND opt~buzei = wi~buzei
      WHERE wi~wt_qsshb IS NOT INITIAL
      INTO CORRESPONDING FIELDS OF TABLE @gt_whtax  ##TOO_MANY_ITAB_FIELDS .

    SELECT
      bukrs,
      belnr,
      gjahr,
      buzei,
      name1,
      name2
      FROM bsec
      FOR ALL ENTRIES IN @gt_output
      WHERE bukrs = @gt_output-bukrs
      AND   belnr = @gt_output-belnr
      AND   gjahr = @gt_output-gjahr
      AND   buzei = @gt_output-buzei                    "#EC CI_NOORDER
      INTO TABLE @gt_onetime.                      "#EC CI_NO_TRANSFORM

    "<<F36K917931 - 05 Begin of del
*    SELECT *
*      INTO TABLE @gt_bplog
*      FROM zsdsfit029
*      FOR ALL ENTRIES IN @gt_output
*      WHERE data_type = @gc_data_type-invoice
*      AND   bukrs = @gt_output-bukrs
*      AND   belnr = @gt_output-belnr
*      AND   gjahr = @gt_output-gjahr
*      AND   buzei = @gt_output-buzei
*      AND   delete_flag = ''                       "#EC CI_NO_TRANSFORM
    "<<F36K917931 - 05 End of del
    "<<F36K917931 - 05 Begin of ins
    SELECT col~*
      FROM zsdsfit029 AS col
      INNER JOIN @gt_output AS opt
      ON    col~data_type = @gc_data_type-invoice
      AND   col~bukrs = opt~bukrs
      AND   col~belnr = opt~belnr
      AND   col~gjahr = opt~gjahr
      AND   col~buzei = opt~buzei
      AND   col~delete_flag = ''                   "#EC CI_NO_TRANSFORM
      WHERE
        NOT EXISTS ( SELECT 'X' FROM zsdsfit037 AS lnk
                        INNER JOIN bkpf AS clr
                        ON  lnk~bukrs_clr = clr~bukrs
                        AND lnk~belnr_clr = clr~belnr
                        AND lnk~gjahr_clr = clr~gjahr
                        WHERE lnk~data_type = col~data_type
                        AND   lnk~bukrs     = col~bukrs
                        AND   lnk~belnr     = col~belnr
                        AND   lnk~gjahr     = col~gjahr
                        AND   lnk~buzei     = col~buzei
                        AND   lnk~seq       = col~seq
                        AND   clr~xreversed = 'X'
                      )
      INTO TABLE @gt_bplog.
    "<<F36K917931 - 05 End of ins

    IF gt_bplog IS NOT INITIAL.
      "Clearing link list
      SELECT lnk~*
        INTO TABLE @gt_clr_link                    ##TOO_MANY_ITAB_FIELDS
        FROM zsdsfit037 AS lnk
        INNER JOIN bkpf AS doc
        ON  doc~bukrs = lnk~bukrs_clr
        AND doc~belnr = lnk~belnr_clr
        AND doc~gjahr = lnk~gjahr_clr
        FOR ALL ENTRIES IN @gt_bplog
        WHERE lnk~data_type = @gt_bplog-data_type
        AND   lnk~bukrs = @gt_bplog-bukrs
        AND   lnk~belnr = @gt_bplog-belnr
        AND   lnk~gjahr = @gt_bplog-gjahr
        AND   lnk~buzei = @gt_bplog-buzei
        AND   lnk~seq   = @gt_bplog-seq            "#EC CI_NO_TRANSFORM
        AND   doc~xreversal = ''.

      "Change history
      SELECT *
        INTO TABLE @gt_status_hist
        FROM zsdsfit038
        FOR ALL ENTRIES IN @gt_bplog
        WHERE data_type = @gc_data_type-invoice
        AND   bukrs = @gt_bplog-bukrs
        AND   belnr = @gt_bplog-belnr
        AND   gjahr = @gt_bplog-gjahr
        AND   buzei = @gt_bplog-buzei              "#EC CI_NO_TRANSFORM
        AND   seq   = @gt_bplog-seq.          "#EC CI_ALL_FIELDS_NEEDED
    ENDIF.

    SELECT
      FROM bsid_view AS itm
      INNER JOIN bkpf AS hdr
      ON  hdr~bukrs = itm~bukrs
      AND hdr~belnr = itm~belnr
      AND hdr~gjahr = itm~gjahr
      FIELDS
        itm~bukrs,
        itm~belnr,
        itm~gjahr,
        itm~buzei,
        itm~waers,
        itm~dmbtr,
        itm~wrbtr,
        itm~rebzg,
        itm~rebzj,
        itm~rebzz,
        hdr~xref2_hd
      FOR ALL ENTRIES IN @gt_output
      WHERE itm~bukrs = @gt_output-bukrs
      AND   itm~rebzg = @gt_output-belnr
      AND   itm~rebzj = @gt_output-gjahr
      AND   itm~rebzz = @gt_output-buzei
      INTO TABLE @gt_partial.                      "#EC CI_NO_TRANSFORM

    SELECT
      FROM bseg AS bseg
      INNER JOIN @gt_output AS outp
      ON  outp~bukrs = bseg~bukrs
      AND outp~belnr = bseg~belnr
      AND outp~gjahr = bseg~gjahr
      FIELDS
        bseg~bukrs,
        bseg~belnr,
        bseg~gjahr,
        bseg~buzei,
        bseg~wrbtr,
        bseg~fwbas
      WHERE bseg~buzid = 'T'
      INTO TABLE @gt_tax_base.                          "#EC CI_NOORDER

    SELECT
      FROM knbw AS cust
      INNER JOIN @gt_output AS outp
      ON outp~kunnr = cust~kunnr
      FIELDS
        cust~kunnr,
        cust~qsrec
      INTO TABLE @gt_whtax_type.

*<<F36K914812 - 07 Begin of del
** "<<F36K911780 start of ins
*    "DO <-> Billing
*    SELECT
*      bil~vbeln,
*      bilh~fkdat,
*      bil~aubel
*      FROM vbrp AS bil
*      INNER JOIN vbrk AS bilh
*      ON bilh~vbeln = bil~vbeln
*      INNER JOIN @gt_output AS log
*      ON bil~vbeln = log~vbeln_vf
**      WHERE bil~vgtyp = 'J'
*      INTO CORRESPONDING FIELDS OF TABLE @gt_do ##TOO_MANY_ITAB_FIELDS.
*
*    SORT gt_do BY vbeln.
** "<<F36K911780 end of ins
*<<F36K914812 - 07 End of del
*<<F36K914812 - 07 Begin of ins
    "DO <-> Billing
    SELECT
      bil~vbeln,
      bilh~fkdat,
      bil~aubel,
      bil~vgbel,
      do~lfdat,
      so_part~adrnr
      FROM vbrp AS bil
      INNER JOIN vbrk AS bilh
      ON bil~vbeln = bilh~vbeln
      INNER JOIN likp AS do
      ON do~vbeln = bil~vgbel
      INNER JOIN lips AS doitm
      ON doitm~vbeln = do~vbeln
      INNER JOIN vbap AS so
      ON  doitm~vgbel = so~vbeln
      AND doitm~vgpos = so~posnr
      INNER JOIN vbpa AS so_part
      ON  so_part~vbeln = so~vbeln
      AND so_part~parvw = 'RG'  "payer
      INNER JOIN @gt_output AS opt
      ON bil~vbeln = opt~vbeln_vf
      WHERE bil~vgtyp = 'J'
      INTO TABLE @gt_do.

    SORT gt_do BY vbeln fkdat aubel vgbel lfdat adrnr.
    DELETE ADJACENT DUPLICATES FROM gt_do COMPARING ALL FIELDS.

    IF gt_do IS NOT INITIAL.

*<<F36K914812 - 07 End of ins

*<<F36K914812 - 02 Begin of ins
      SELECT fl~vbeln,
             fl~vbelv AS vbeln_so,
             po~zz1_cus_po AS bstkd
        FROM vbfa AS fl
        INNER JOIN @gt_do AS do
        ON fl~vbeln = do~vbeln
        LEFT JOIN crms4d_serv_h AS po
        ON ( po~objtype_h EQ 'BUS2000116' OR po~objtype_h EQ 'BUS2000112' )
        AND  po~object_id EQ fl~vbelv
        WHERE fl~vbtyp_n EQ 'EBDR'
        AND ( fl~vbtyp_v EQ 'CSVO' OR fl~vbtyp_v EQ 'CSCT' )
        INTO TABLE @gt_so.

      SORT gt_so BY vbeln.
*<<F36K914812 - 02 End of ins

      DATA(lt_adrc) = gt_do.
      SORT lt_adrc BY adrnr.
      DELETE ADJACENT DUPLICATES FROM lt_adrc COMPARING adrnr.

      SELECT adrc~addrnumber,
             adrc~nation,
             adrc~name1
        FROM adrc
        INNER JOIN @lt_adrc AS do
        ON adrc~addrnumber = do~adrnr
        INTO TABLE @gt_cust_adrc.
    ENDIF.
*<<F36K914812 - 07 End of ins

  ELSE.
    MESSAGE s000(38) WITH TEXT-e00. "No data found
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_bp_log_open
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_bp_log_open.
  DATA:
    lr_workdate TYPE RANGE OF zsdsfit029-work_date,
    lr_pernr    TYPE RANGE OF zsdsfit029-pernr.


  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.
      DATA(lr_belnr)  = s_belnr[].
      DATA(lr_gjahr)  = s_gjahr[].
      DATA(lr_kunnr)  = s_kunnr[].
      DATA(lr_pymn)   = s_pymn[].   "<<F36K914812 - 03 insert
      DATA(lr_acttyp) = s_acttyp[].
      DATA(lr_status) = s_status[].
      DATA(lr_trnf)   = s_trnf[].
      DATA(lr_bill)   = s_bill[].
      DATA(lr_pldate) = s_pldate[].
      DATA(lr_xblnr)   = s_xblnr[].

      IF p_wrkdt IS NOT INITIAL.
        lr_workdate = VALUE #( BASE lr_workdate ( sign = 'I' option = 'EQ' low = p_wrkdt ) ).
      ENDIF.

      IF p_pernr <> '*'.
        lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = |{ p_pernr ALPHA = IN }| ) ).
      ENDIF.

    WHEN gc_tab_sel-update_bank.
      lr_belnr    = s_belnru[].
      lr_gjahr    = s_gjahru[].
      lr_kunnr    = s_kunnru[].
      lr_pymn     = s_pymnu[].    "<<F36K914812 - 03 insert
      lr_acttyp   = s_acttyu[].
      lr_status   = s_statuu[].
      lr_trnf     = s_trnfu[].
      lr_bill     = s_billu[].
      lr_pldate   = s_pldatu[].
      lr_xblnr    = s_xblnru[].

      IF p_wrkdtu IS NOT INITIAL.
        lr_workdate = VALUE #( BASE lr_workdate ( sign = 'I' option = 'EQ' low = p_wrkdtu ) ).
      ENDIF.

      IF p_pernru <> '*'.
        lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = |{ p_pernru ALPHA = IN }| ) ).
      ENDIF.
  ENDCASE.

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
    INNER JOIN bkpf AS hdr
    ON  hdr~bukrs = log~bukrs
    AND hdr~belnr = log~belnr
    AND hdr~gjahr = log~gjahr
    INNER JOIN bsid_view AS opn
    ON  opn~bukrs = log~bukrs
    AND opn~belnr = log~belnr
    AND opn~gjahr = log~gjahr
    AND opn~buzei = log~buzei
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    WHERE log~data_type   = @gc_data_type-invoice
    AND   log~belnr       IN @lr_belnr
    AND   log~gjahr       IN @lr_gjahr
    AND   log~kunnr       IN @lr_kunnr
    AND   log~work_date   IN @lr_workdate
    AND   log~umskz       IN @gr_spgl
    AND   log~delete_flag = ''
    AND   log~pymt_method IN @lr_pymn       "<<F36K914812 - 03 insert
    AND   log~action_type IN @lr_acttyp
    AND   log~status      IN @lr_status
    AND   log~tranf_no    IN @lr_trnf
    AND   log~billpl_no   IN @lr_bill
    AND   log~billpl_date IN @lr_pldate
    AND   log~xblnr       IN @lr_xblnr
    AND   log~pernr       IN @lr_pernr
    AND   hdr~xreversal   = ''
"<<F36K917931 - 05 Begin of ins
    AND  NOT EXISTS ( SELECT 'X' FROM zsdsfit037 AS lnk
                        INNER JOIN bkpf AS clr
                        ON  lnk~bukrs_clr = clr~bukrs
                        AND lnk~belnr_clr = clr~belnr
                        AND lnk~gjahr_clr = clr~gjahr
                        WHERE lnk~data_type = log~data_type
                        AND   lnk~bukrs     = log~bukrs
                        AND   lnk~belnr     = log~belnr
                        AND   lnk~gjahr     = log~gjahr
                        AND   lnk~buzei     = log~buzei
                        AND   lnk~seq       = log~seq
                      )
"<<F36K917931 - 05 End of ins
    INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS.

*  "Exclude status 02,02 to edit for data Update for bank's vendor
*  IF gv_data_type = gc_tab_sel-update_bank.
*    LOOP AT gt_action_status INTO DATA(ls_action_status).
*      DELETE gt_output WHERE action_type = ls_action_status-action_type
*                       AND   status      = ls_action_status-status.
*    ENDLOOP.
*  ENDIF.

  IF gt_output IS NOT INITIAL.

    gt_bplog = CORRESPONDING #( gt_output ).
    IF gt_bplog IS NOT INITIAL.
      SELECT lnk~*
        INTO TABLE @gt_clr_link    ##TOO_MANY_ITAB_FIELDS "#EC CI_NO_TRANSFORM
        FROM zsdsfit037 AS lnk
        INNER JOIN bkpf AS doc
        ON  doc~bukrs = lnk~bukrs_clr
        AND doc~belnr = lnk~belnr_clr
        AND doc~gjahr = lnk~gjahr_clr
        FOR ALL ENTRIES IN @gt_bplog
        WHERE lnk~data_type = @gt_bplog-data_type
        AND   lnk~bukrs = @gt_bplog-bukrs
        AND   lnk~belnr = @gt_bplog-belnr
        AND   lnk~gjahr = @gt_bplog-gjahr
        AND   lnk~buzei = @gt_bplog-buzei
        AND   lnk~seq   = @gt_bplog-seq.
*          AND   doc~xreversal = ''.  "<<F36K917931 - 05 del

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

    SELECT
      FROM bsid_view
      FIELDS
        bukrs,
        belnr,
        gjahr,
        buzei,
        waers,
        dmbtr,
        wrbtr,
        rebzg,
        rebzj,
        rebzz
      FOR ALL ENTRIES IN @gt_output
      WHERE bukrs = @gt_output-bukrs
      AND   rebzg = @gt_output-belnr
      AND   rebzj = @gt_output-gjahr
      AND   rebzz = @gt_output-buzei               "#EC CI_NO_TRANSFORM
      INTO TABLE @gt_partial    ##TOO_MANY_ITAB_FIELDS.

    SELECT
      FROM bseg AS bseg
      INNER JOIN @gt_output AS outp
      ON  outp~bukrs = bseg~bukrs
      AND outp~belnr = bseg~belnr
      AND outp~gjahr = bseg~gjahr
      FIELDS
        bseg~bukrs,
        bseg~belnr,
        bseg~gjahr,
        bseg~buzei,
        bseg~wrbtr,
        bseg~fwbas
      WHERE bseg~buzid = 'T'
      INTO TABLE @gt_tax_base.                          "#EC CI_NOORDER

    SELECT
      FROM knbw AS cust
      INNER JOIN @gt_output AS outp
      ON outp~kunnr = cust~kunnr
      FIELDS
        cust~kunnr,
        cust~qsrec
      INTO TABLE @gt_whtax_type.

  ELSE.
    MESSAGE s000(38) WITH TEXT-e00. "No data found
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_bp_log_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_bp_log_all.
  DATA:
    lr_workdate TYPE RANGE OF zsdsfit029-work_date,
    lr_pernr    TYPE RANGE OF zsdsfit029-pernr.

  CASE gv_data_type.
    WHEN gc_tab_sel-collection_log.
      DATA(lr_belnr)  = s_belnr[].
      DATA(lr_gjahr)  = s_gjahr[].
      DATA(lr_kunnr)  = s_kunnr[].
      DATA(lr_acttyp) = s_acttyp[].
      DATA(lr_status) = s_status[].
      DATA(lr_trnf)   = s_trnf[].
      DATA(lr_bill)   = s_bill[].
      DATA(lr_pldate) = s_pldate[].
      DATA(lr_xblnr)  = s_xblnr[].

      IF p_wrkdt IS NOT INITIAL.
        lr_workdate = VALUE #( BASE lr_workdate ( sign = 'I' option = 'EQ' low = p_wrkdt ) ).
      ENDIF.

      IF p_pernr <> '*'.
        lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = |{ p_pernr ALPHA = IN }| ) ).
      ENDIF.

    WHEN gc_tab_sel-update_bank.
      lr_belnr    = s_belnru[].
      lr_gjahr    = s_gjahru[].
      lr_kunnr    = s_kunnru[].
      lr_acttyp   = s_acttyu[].
      lr_status   = s_statuu[].
      lr_trnf     = s_trnfu[].
      lr_bill     = s_billu[].
      lr_pldate   = s_pldatu[].
      lr_xblnr    = s_xblnru[].

      IF p_wrkdtu IS NOT INITIAL.
        lr_workdate = VALUE #( BASE lr_workdate ( sign = 'I' option = 'EQ' low = p_wrkdtu ) ).
      ENDIF.

      IF p_pernru <> '*'.
        lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = |{ p_pernru ALPHA = IN }| ) ).
      ENDIF.
  ENDCASE.

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
    INNER JOIN bkpf AS hdr
    ON  hdr~bukrs = log~bukrs
    AND hdr~belnr = log~belnr
    AND hdr~gjahr = log~gjahr
    INNER JOIN bsid_view AS opn
    ON  opn~bukrs = log~bukrs
    AND opn~belnr = log~belnr
    AND opn~gjahr = log~gjahr
    AND opn~buzei = log~buzei
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    WHERE log~data_type   = @gc_data_type-invoice
    AND   log~belnr       IN @lr_belnr
    AND   log~gjahr       IN @lr_gjahr
    AND   log~work_date   IN @lr_workdate
    AND   log~kunnr       IN @lr_kunnr
    AND   log~umskz       IN @gr_spgl
    AND   log~action_type IN @lr_acttyp
    AND   log~status      IN @lr_status
    AND   log~tranf_no    IN @lr_trnf
    AND   log~billpl_no   IN @lr_bill
    AND   log~billpl_date IN @lr_pldate
    AND   log~xblnr       IN @lr_xblnr
    AND   log~pernr       IN @lr_pernr
    AND   hdr~xreversal   = ''
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

*<<F36K914812 - 10 Begin of ins
      SELECT lnk~*
        INTO TABLE @gt_clr_link               ##TOO_MANY_ITAB_FIELDS "#EC CI_NO_TRANSFORM
        FROM zsdsfit037 AS lnk
        INNER JOIN bkpf AS doc
        ON  doc~bukrs = lnk~bukrs_clr
        AND doc~belnr = lnk~belnr_clr
        AND doc~gjahr = lnk~gjahr_clr
        FOR ALL ENTRIES IN @gt_bplog
        WHERE lnk~data_type = @gt_bplog-data_type
        AND   lnk~bukrs = @gt_bplog-bukrs
        AND   lnk~belnr = @gt_bplog-belnr
        AND   lnk~gjahr = @gt_bplog-gjahr
        AND   lnk~buzei = @gt_bplog-buzei
        AND   lnk~seq   = @gt_bplog-seq
        AND   doc~xreversal = ''.

      SELECT
        FROM bsid_view
        FIELDS
          bukrs,
          belnr,
          gjahr,
          buzei,
          waers,
          dmbtr,
          wrbtr,
          rebzg,
          rebzj,
          rebzz
        FOR ALL ENTRIES IN @gt_output
        WHERE bukrs = @gt_output-bukrs
        AND   rebzg = @gt_output-belnr
        AND   rebzj = @gt_output-gjahr
        AND   rebzz = @gt_output-buzei             "#EC CI_NO_TRANSFORM
        INTO TABLE @gt_partial    ##TOO_MANY_ITAB_FIELDS.

*<<F36K914812 - 10 End of ins

    ENDIF.
  ELSE.
    MESSAGE s000(38) WITH TEXT-e00. "No data found
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_prepare_create_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_prepare_create_output .

  DATA:
    lv_zfbdt   TYPE rbkp-zfbdt,
    lv_zbd1t   TYPE rbkp-zbd1t,
    lt_celltab TYPE lvc_t_styl.

  DATA(lt_partial) = gt_partial.

  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    CLEAR:
      lt_celltab.

    IF <ls_output>-vbeln_vf = <ls_output>-belnr.
      CLEAR <ls_output>-vbeln_vf.
    ENDIF.

    <ls_output>-wrbtr = COND #( WHEN <ls_output>-shkzg = 'H'
                                THEN <ls_output>-wrbtr * -1
                                ELSE <ls_output>-wrbtr ).

    "Calculate remain amount
    <ls_output>-remain_amt = <ls_output>-wrbtr.

    "Net due date
    CALL FUNCTION 'MRM_PAYMENT_TERMS_GET'
      EXPORTING
        if_zterm = <ls_output>-zterm
        if_bldat = <ls_output>-bldat
        if_budat = <ls_output>-budat
        if_zfbdt = <ls_output>-zfbdt
      IMPORTING
        ef_zfbdt = lv_zfbdt
        ef_zbd1t = lv_zbd1t.
    IF sy-subrc EQ 0  ##FM_SUBRC_OK.
      <ls_output>-faedt = lv_zfbdt + lv_zbd1t.
    ENDIF.

    "Check with previous collection log
    LOOP AT gt_bplog INTO DATA(ls_bplog)                            ##INTO_OK
      WHERE bukrs = <ls_output>-bukrs
      AND   belnr = <ls_output>-belnr
      AND   gjahr = <ls_output>-gjahr
      AND   buzei = <ls_output>-buzei.

      "No Payment receive
      IF NOT line_exists( gt_clr_link[ bukrs = ls_bplog-bukrs
                                       belnr = ls_bplog-belnr
                                       gjahr = ls_bplog-gjahr
                                       buzei = ls_bplog-buzei
                                       seq   = ls_bplog-seq ] ).

        APPEND INITIAL LINE TO <ls_output>-billpl_list ASSIGNING FIELD-SYMBOL(<ls_billpl_list>).
        <ls_billpl_list>-tranf_no     = ls_bplog-tranf_no.
        <ls_billpl_list>-billpl_no    = ls_bplog-billpl_no.
        <ls_billpl_list>-billpl_date  = ls_bplog-billpl_date.
        <ls_billpl_list>-wrbtr        = ls_bplog-bal_amt.
      ENDIF.

      "Payment received found
      LOOP AT gt_clr_link INTO DATA(ls_list)      ##INTO_OK
        WHERE bukrs = ls_bplog-bukrs
        AND   belnr = ls_bplog-belnr
        AND   gjahr = ls_bplog-gjahr
        AND   buzei = ls_bplog-buzei
        AND   seq   = ls_bplog-seq.

        APPEND INITIAL LINE TO <ls_output>-billpl_list ASSIGNING <ls_billpl_list>.
        <ls_billpl_list>-tranf_no     = ls_bplog-tranf_no.
        <ls_billpl_list>-billpl_no    = ls_list-billpl_no.
        <ls_billpl_list>-billpl_date  = ls_list-billpl_date.
        <ls_billpl_list>-belnr        = ls_list-belnr_clr.
        <ls_billpl_list>-gjahr        = ls_list-gjahr_clr.
        <ls_billpl_list>-wrbtr        = ls_list-wrbtr.
        <ls_billpl_list>-received     = gc_true.

        "Remove payment doc where data already populated
        DELETE lt_partial WHERE bukrs = ls_list-bukrs
                          AND   belnr = ls_list-belnr_clr
                          AND   gjahr = ls_list-gjahr_clr.

      ENDLOOP.
*      IF sy-subrc NE 0.
*        lv_unmatch = gc_true.
*      ENDIF.

      <ls_output>-remain_amt -= ls_bplog-bal_amt.
      <ls_output>-total_bil  += ls_bplog-bal_amt.
    ENDLOOP.

    "Check with manual received - no bill placement matched
    LOOP AT lt_partial INTO DATA(ls_partial)        ##INTO_OK
      WHERE bukrs = <ls_output>-bukrs
      AND   rebzg = <ls_output>-belnr
      AND   rebzj = <ls_output>-gjahr
      AND   rebzz = <ls_output>-buzei.

      DATA(lv_tabix) = sy-tabix.

      APPEND INITIAL LINE TO <ls_output>-billpl_list ASSIGNING <ls_billpl_list>.
      <ls_billpl_list>-belnr    = ls_partial-belnr.
      <ls_billpl_list>-gjahr    = ls_partial-gjahr.
      <ls_billpl_list>-wrbtr    = ls_partial-wrbtr.
      <ls_billpl_list>-received = gc_true.

      <ls_output>-remain_amt -= ls_partial-wrbtr.
      <ls_output>-total_bil  += ls_partial-wrbtr.

      DELETE lt_partial INDEX lv_tabix.
    ENDLOOP.

*    "Set default value
*    PERFORM f_default_value_new_item CHANGING <ls_output>.

    CASE gv_data_type.
      WHEN gc_tab_sel-collection_log.
        "Set default value
        PERFORM f_default_value_new_item CHANGING <ls_output>.

        <ls_output>-pymt_method  = p_dfpymt.
        <ls_output>-action_type  = p_dfact.
        <ls_output>-status       = p_dfsta.
        <ls_output>-hbkid        = p_dfhbk.
        <ls_output>-hktid        = p_dfhkt.
        <ls_output>-bank_date    = p_dfbkdt.
        <ls_output>-cheque_no    = p_dfchq.
        <ls_output>-zbank_item   = p_zbank.

      WHEN gc_tab_sel-update_bank.
        <ls_output>-pymt_method  = p_dfpymu.
        <ls_output>-action_type  = p_dfactu.
        <ls_output>-status       = p_dfstau.
        <ls_output>-hbkid        = p_dfhbku.
        <ls_output>-hktid        = p_dfhktu.
        <ls_output>-bank_date    = p_dfbkdu.
        <ls_output>-cheque_no    = p_dfchqu.

        <ls_output>-remain_c_amt = <ls_output>-remain_amt.
      WHEN OTHERS.
        <ls_output>-remain_c_amt = <ls_output>-remain_amt.

    ENDCASE.

    READ TABLE gt_onetime INTO DATA(ls_onetime)
      WITH KEY bukrs = <ls_output>-bukrs
               belnr = <ls_output>-belnr
               gjahr = <ls_output>-gjahr
               buzei = <ls_output>-buzei.
    IF sy-subrc EQ 0.
      <ls_output>-cust_name = |{ ls_onetime-name1 } { ls_onetime-name2 }|.
    ENDIF.

* "<<F36K911780 start of ins
    READ TABLE gt_do
      INTO DATA(ls_do)
      WITH KEY vbeln = <ls_output>-vbeln_vf
      BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_output>-fkdat = ls_do-fkdat.
*      <ls_output>-vbeln = ls_do-aubel.  "<<F36K914812 - 02 del

*<<F36K914812 - 07 Begin of ins
      READ TABLE gt_cust_adrc INTO DATA(ls_cust_adrc)
        WITH KEY addrnumber = ls_do-adrnr.
      IF sy-subrc EQ 0.
        <ls_output>-cust_name = ls_cust_adrc-name1.
      ENDIF.
*<<F36K914812 - 07 End of ins

*<<F36K914812 - 02 Begin of ins
      READ TABLE gt_so INTO DATA(ls_so)
        WITH KEY vbeln = ls_do-aubel
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_output>-vbeln   = ls_so-vbeln_so.
        <ls_output>-po_ref  = ls_so-bstkd.
      ENDIF.
*<<F36K914812 - 02 End of ins

      "<<F36K917931 - 06 Begin of ins
    ELSE.
      PERFORM f_get_sale_doc USING
                                <ls_output>-vbeln_vf
                             CHANGING
                                <ls_output>-vbeln
                                <ls_output>-po_ref.
      "<<F36K917931 - 06 End of ins
    ENDIF.
* "<<F36K911780 end of ins


    PERFORM f_fill_celltab USING 'RW'
                         CHANGING lt_celltab.

    <ls_output>-celltab     = lt_celltab.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_prepare_change_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_prepare_change_output .

  DATA:
    lv_readonly TYPE abap_bool,
*    lv_partial  TYPE abap_bool,  "<<F36K910991 - del
    lt_celltab  TYPE lvc_t_styl.

*  DATA(lt_partial) = gt_partial. "<<F36K914812 - 8 del

  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    DATA(lv_output_tabix) = sy-tabix.

    DATA(lt_partial) = gt_partial.  "<<F36K914812 - 8 ins

    CLEAR:
      lv_readonly,
      lt_celltab.
*      lv_partial.                "<<F36K910991 - del

    PERFORM f_get_pernr_name USING <ls_output>-pernr
                          CHANGING <ls_output>-full_name.

    "Calculate remain amount
    <ls_output>-remain_amt = <ls_output>-wrbtr. "<<F36K914812 - 08 ins

    LOOP AT gt_clr_link INTO DATA(ls_list)      ##INTO_OK
       WHERE bukrs = <ls_output>-bukrs
       AND   belnr = <ls_output>-belnr
       AND   gjahr = <ls_output>-gjahr
       AND   buzei = <ls_output>-buzei.
*       AND   seq   = <ls_output>-seq.  "<<F36K914812 - 08 del

      APPEND INITIAL LINE TO <ls_output>-billpl_list ASSIGNING FIELD-SYMBOL(<ls_billpl_list>).
*      <ls_billpl_list>-tranf_no     = <ls_output>-tranf_no. "<<F36K914812 - 08 del
      <ls_billpl_list>-billpl_no    = ls_list-billpl_no.
      <ls_billpl_list>-billpl_date  = ls_list-billpl_date.
      <ls_billpl_list>-wrbtr        = ls_list-wrbtr.
      <ls_billpl_list>-belnr        = ls_list-belnr_clr.
      <ls_billpl_list>-gjahr        = ls_list-gjahr_clr.
      <ls_billpl_list>-received     = gc_true.

*      lv_partial = gc_true.    "<<F36K910991 - del

*<<F36K914812 - 08 Begin of ins
      READ TABLE lt_partial
        INTO DATA(ls_partial_cal)
        WITH KEY bukrs = ls_list-bukrs
                 belnr = ls_list-belnr_clr
                 gjahr = ls_list-gjahr_clr
                 rebzz = ls_list-buzei.
      IF sy-subrc EQ 0.
*        <ls_output>-remain_amt -= ls_partial_cal-wrbtr.
        <ls_output>-total_bil  += ls_partial_cal-wrbtr.
      ENDIF.
*<<F36K914812 - 08 End of ins

      DELETE lt_partial WHERE bukrs = ls_list-bukrs
                        AND   belnr = ls_list-belnr_clr
                        AND   gjahr = ls_list-gjahr_clr
                        AND   rebzz = ls_list-buzei.    "<<F36K914812 - 08 ins

      IF <ls_output>-seq = ls_list-seq.  "<<F36K914812 - 08 ins
        <ls_billpl_list>-tranf_no     = <ls_output>-tranf_no.
        "Already received - do not change data
        lv_readonly = gc_true.
*"<<F36K914812 - 08 Begin of ins
      ELSE.
        <ls_output>-remain_amt -= ls_partial_cal-wrbtr.

        READ TABLE gt_bplog INTO DATA(ls_bplog)
          WITH KEY bukrs = ls_list-bukrs
          belnr = ls_list-belnr
          gjahr = ls_list-gjahr
          buzei = ls_list-buzei
          seq   = ls_list-seq.
        IF sy-subrc EQ 0.
          <ls_billpl_list>-tranf_no = ls_bplog-tranf_no.
        ENDIF.
      ENDIF.
*"<<F36K914812 - 08 End of ins
    ENDLOOP.

*    "Check with manual received
    LOOP AT lt_partial INTO DATA(ls_partial)    ##INTO_OK
      WHERE bukrs = <ls_output>-bukrs
      AND   rebzg = <ls_output>-belnr
      AND   rebzj = <ls_output>-gjahr
      AND   rebzz = <ls_output>-buzei.

      DATA(lv_tabix) = sy-tabix.
*      lv_partial = gc_true.      "<<F36K910991 - del

      APPEND INITIAL LINE TO <ls_output>-billpl_list ASSIGNING <ls_billpl_list>.
      <ls_billpl_list>-belnr    = ls_partial-belnr.
      <ls_billpl_list>-gjahr    = ls_partial-gjahr.
      <ls_billpl_list>-wrbtr    = ls_partial-wrbtr.
      <ls_billpl_list>-received = gc_true.

      <ls_output>-remain_amt -= ls_partial-wrbtr.
      <ls_output>-total_bil  += ls_partial-wrbtr.

      DELETE lt_partial INDEX lv_tabix.
    ENDLOOP.

    <ls_output>-remain_c_amt = <ls_output>-remain_amt - <ls_output>-bal_amt.

*    "Check is editable
*<<F36K910991 - Start of del
*    IF lv_partial = gc_true. "New partial found
*      lv_readonly = gc_true.
*    ENDIF.
*<<F36K910991 - end of del

*<<F36K914812 - 8 Begin of ins
    IF lv_readonly = gc_true
     AND ( ( gv_data_type = gc_tab_sel-collection_log AND
         rb_chg = gc_true ) OR
        ( gv_data_type = gc_tab_sel-update_bank AND
         rb_chgu = gc_true ) ).
      DELETE gt_output INDEX lv_output_tabix.
      CONTINUE.
*<<F36K914812 - 8 end of ins
*<<F36K914812 - 8 begin of del
*    IF lv_readonly = gc_true.
*      PERFORM f_fill_celltab USING 'RO'
*                           CHANGING lt_celltab.
*<<F36K914812 - 8 end of del
    ELSE.
      PERFORM f_fill_celltab USING 'RW'
                           CHANGING lt_celltab.
    ENDIF.

    <ls_output>-celltab     = lt_celltab.


  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_tab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_tab .
  CASE sy-dynnr.
    WHEN 1000.
      CASE sscrfields-ucomm.
        WHEN gc_tab_sel-collection_log.
          tab_blk-dynnr = '100'.
          gv_data_type = sscrfields-ucomm.
        WHEN gc_tab_sel-new_entry.
          tab_blk-dynnr = '200'.
          gv_data_type = sscrfields-ucomm.
        WHEN gc_tab_sel-update_bank.
          tab_blk-dynnr = '300'.
          gv_data_type = sscrfields-ucomm.
*        WHEN gc_tab_sel-history_log.
*          tab_blk-dynnr = '400'.
*          gv_data_type = sscrfields-ucomm.
        WHEN gc_tab_sel-memo.
          tab_blk-dynnr = '350'.
          gv_data_type = sscrfields-ucomm.
        WHEN OTHERS.
      ENDCASE.
      SET PARAMETER ID 'ZSDS_TAB' FIELD tab_blk-dynnr.
  ENDCASE.

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid = sy-repid          " ABAP Program Name
*     irt_param =                  " Parameter name
*     irt_ext  =                  " Parameter extension
    IMPORTING
      et_gen_c = DATA(lt_genc)     " General Constants (GENC)
  ).

  IF gv_data_type = gc_tab_sel-collection_log.
    IF p_dfpymt = '01' OR
       p_dfpymt = '04' AND
      ( p_dfhbk IS INITIAL AND
        p_dfhkt IS INITIAL ).
      LOOP AT lt_genc INTO DATA(ls_genc)      ##INTO_OK.
        CASE ls_genc-param.
          WHEN gc_genc_param-df_house_bank.
            p_dfhbk  = ls_genc-value_low.
          WHEN gc_genc_param-df_bank_acct.
            p_dfhkt = ls_genc-value_low.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_init_global_var
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_global_var .

  DATA:
    lr_param         TYPE zcl_sdsca_utilities=>trt_range_param.

  lr_param = VALUE #( ( sign = 'I' option = 'EQ' low = gc_genc_param-action_status ) ).

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = 'ZSDSFIR0250'           " ABAP Program Name
      irt_param = lr_param                " Parameter name
*     irt_ext   =                        " Parameter extension
    IMPORTING
      et_gen_c  = DATA(lt_action_status)  " General Constants (GENC)
  ).

  "Action status
  LOOP AT lt_action_status INTO DATA(ls_genc)   ##INTO_OK.

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

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                     " ABAP Program Name
      if_param = gc_genc_param-bank_method    " Parameter name
*     if_ext   =                             " Parameter Extension
    IMPORTING
      et_range = gr_bank_trn
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_chk_sel_screen_block_bn1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_chk_sel_screen_block_bn1 .
  DATA: lv_pernr TYPE zsdsfit029-pernr.

  CHECK sy-ucomm = 'ONLI'.

*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
  CHECK gv_data_type = gc_tab_sel-new_entry.

  IF p_pernrn IS INITIAL.
    MESSAGE e000(38) WITH TEXT-e15.
  ENDIF.

  lv_pernr = |{ p_pernrn ALPHA = IN }|.

  IF NOT ( rb_disn = gc_true AND p_pernrn = '*' ).

    PERFORM f_get_pernr_name USING lv_pernr
                             CHANGING gv_fullname.

    IF gv_fullname IS INITIAL.
      MESSAGE e000(38) WITH 'Personnel Number'(e91) | { p_pernrn ALPHA = OUT } | 'does not exist'(e92).
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_chk_sel_screen_block_bu1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_chk_sel_screen_block_bu1 .
  DATA: lv_pernr TYPE zsdsfit029-pernr.

  CHECK sy-ucomm = 'ONLI'.

*--------------------------------------------------------------------*
* Update for bank's vendor
*--------------------------------------------------------------------*
  CHECK gv_data_type = gc_tab_sel-update_bank.

  IF p_pernru IS INITIAL.
    MESSAGE e000(38) WITH TEXT-e15.
  ENDIF.

  lv_pernr = |{ p_pernru ALPHA = IN }|.

  IF NOT ( rb_disu = gc_true AND p_pernru = '*' ).
    PERFORM f_get_pernr_name USING lv_pernr
                             CHANGING gv_fullname.

    IF gv_fullname IS INITIAL.
      MESSAGE e000(38) WITH 'Personnel Number'(e91) | { p_pernru ALPHA = OUT } | 'does not exist'(e92).
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_chk_sel_screen_block_bm1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_chk_sel_screen_block_bm1 .

  DATA: lv_pernr TYPE zsdsfit029-pernr.

  CHECK sy-ucomm = 'ONLI'.

*--------------------------------------------------------------------*
* Memo
*--------------------------------------------------------------------*
  CHECK gv_data_type = gc_tab_sel-memo.

  IF p_pernrm IS INITIAL.
    MESSAGE e000(38) WITH TEXT-e15.
  ENDIF.

  lv_pernr = |{ p_pernrm ALPHA = IN }|.

  IF NOT ( rb_dism = gc_true AND p_pernrm = '*' ).

    PERFORM f_get_pernr_name USING lv_pernr
                             CHANGING gv_fullname.

    IF gv_fullname IS INITIAL.
      MESSAGE e000(38) WITH 'Personnel Number'(e91) | { p_pernrm ALPHA = OUT } | 'does not exist'(e92).
    ENDIF.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_default_value_new_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_OUTPUT>
*&---------------------------------------------------------------------*
FORM f_default_value_new_item  CHANGING cf_defalut TYPE zsdsfis085.

  DATA:
         lv_cal_whtax TYPE char01.

  cf_defalut-deduct_amt   = cf_defalut-remain_amt.

  cf_defalut-bal_amt      = cf_defalut-deduct_amt.


  "Witholding tax amount
  CLEAR lv_cal_whtax.

  PERFORM is_cal_whtax USING cf_defalut-kunnr
                       CHANGING lv_cal_whtax.

  IF lv_cal_whtax = gc_true.

    IF cf_defalut-blart IN gr_srv_blart.
      IF gv_srv_whtax_rate <> 0.
        READ TABLE gt_tax_base
          INTO DATA(ls_tax_base)
          WITH KEY bukrs = cf_defalut-bukrs
                   belnr = cf_defalut-belnr
                   gjahr = cf_defalut-gjahr.
        IF sy-subrc = 0.
*<<F36K911258 start of del
*          cf_defalut-wht_amt = ( ( cf_defalut-bal_amt - ( cf_defalut-bal_amt * ( ls_tax_base-wrbtr / ls_tax_base-fwbas ) ) )
*                                  * ( gv_srv_whtax_rate / 100 ) ) * -1.
*<<F36K911258 end of del
*<<F36K911258 start of ins
          cf_defalut-wht_amt = ( ( cf_defalut-bal_amt * 100 / ( 100 + ( ls_tax_base-wrbtr / ls_tax_base-fwbas * 100 ) ) )
                                  * ( gv_srv_whtax_rate / 100 ) ) * -1.
*<<F36K911258 end of ins
        ELSE.
          cf_defalut-wht_amt = ( cf_defalut-remain_amt * ( gv_srv_whtax_rate / 100 ) ) * -1.
        ENDIF.
      ENDIF.
    ELSEIF cf_defalut-blart IN gr_whtax_blart.
      IF gv_srv_whtax_rate <> 0.
        cf_defalut-wht_amt = ( cf_defalut-remain_amt * ( gv_srv_whtax_rate / 100 ) ) * -1.
      ENDIF.
    ELSE.
      READ TABLE gt_whtax
        INTO DATA(ls_whtax)
        WITH KEY bukrs = cf_defalut-bukrs
                 belnr = cf_defalut-belnr
                 gjahr = cf_defalut-gjahr
                 buzei = cf_defalut-buzei.
      IF sy-subrc EQ 0.
        cf_defalut-wht_amt = ( cf_defalut-remain_amt * ( ls_whtax-qsatz / ls_whtax-qproz ) ) * -1.
      ENDIF.
    ENDIF.

  ENDIF.


  cf_defalut-received_amt = cf_defalut-bal_amt +
                            cf_defalut-exps_amt +
                            cf_defalut-wht_amt +
                            cf_defalut-fee.

  cf_defalut-payin_amt    = cf_defalut-received_amt +
                            cf_defalut-retention +
*                             cf_defalut-refund_amt +
                            cf_defalut-income_amt +
                            cf_defalut-cash_con.

  cf_defalut-remain_c_amt = cf_defalut-remain_amt - cf_defalut-bal_amt.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_hide_sel_tab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_hide_sel_tab .

  LOOP AT SCREEN.
    IF screen-name(3) = 'TAB'.
      IF sy-tcode = gc_tcode_his.
        IF screen-name <> 'TAB_BT4'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-name = 'TAB_BT4'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_fidoc
*&---------------------------------------------------------------------*
FORM f_get_fidoc .
  IF gt_output[] IS NOT INITIAL .
    SELECT b~bukrs,     b~belnr, b~gjahr, b~blart,
           a~billpl_no, c~billpl_date , a~buzei    "#EC CI_NO_TRANSFORM
      FROM zsdsfit035 AS a INNER JOIN bkpf AS b
                                   ON a~bukrs = b~bukrs AND
                                      a~belnr = b~belnr AND
                                      a~gjahr = b~gjahr
                           INNER JOIN zsdsfit033 AS c
                                   ON a~billpl_no = c~billpl_no
       FOR ALL ENTRIES IN @gt_output
      WHERE a~bukrs = @gt_output-bukrs
        AND a~belnr = @gt_output-belnr
        AND a~gjahr = @gt_output-gjahr
       INTO TABLE @gt_billpl .
    IF sy-subrc = 0 .
      SORT gt_billpl BY bukrs belnr gjahr .
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form is_cal_whtax
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_OUTPUT>_KUNNR
*&      <-- LV_CAL_WHTAX
*&---------------------------------------------------------------------*
FORM is_cal_whtax  USING    uf_kunnr     TYPE bseg-kunnr
                   CHANGING cv_cal_whtax TYPE char01.

  READ TABLE gt_whtax_type
    INTO DATA(ls_whtax_type)
    WITH KEY kunnr = uf_kunnr.

  IF ls_whtax_type-qsrec = gc_qsrec_03.
    cv_cal_whtax = ''.
  ELSEIF ls_whtax_type-qsrec IS NOT INITIAL.
    cv_cal_whtax = gc_true.
  ENDIF.

ENDFORM.

*<<F36K917931 - 06 begin of ins
*&---------------------------------------------------------------------*
*& Form f_get_sale_doc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_vbeln
*&      <-- cv_so
*&      <-- cv_po_ref
*&---------------------------------------------------------------------*
FORM f_get_sale_doc  USING    uf_vbeln    TYPE vbrk-vbeln
                     CHANGING cv_so       TYPE vbak-vbeln
                              cv_po_ref   TYPE bstkd.

  DATA lt_docflow TYPE tdt_docflow.

  SELECT SINGLE vbeln
    INTO @DATA(lv_vbeln)      ##NEEDED
    FROM vbrk
    WHERE vbeln = @uf_vbeln.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
    EXPORTING
      iv_docnum        = uf_vbeln
*     IV_ITEMNUM       =
*     IV_ALL_ITEMS     =
      iv_self_if_empty = gc_true
    IMPORTING
      et_docflow       = lt_docflow.

  READ TABLE lt_docflow INTO DATA(ls_docflow)
    WITH KEY vbeln = uf_vbeln.
  IF sy-subrc EQ 0.
    cv_so = ls_docflow-vbelv.
  ENDIF.
*<-- Start of Insertion 20.06.2025 (Get CM Doc from BDR)
* If related document is BDR
  IF ls_docflow-vbtyp_v EQ 'EBDR'.
*   Get Related CM document
    DATA(lf_vbeln) = ls_docflow-vbelv.
    READ TABLE lt_docflow INTO ls_docflow
      WITH KEY vbeln = lf_vbeln.
    IF sy-subrc EQ 0.
      cv_so = ls_docflow-vbelv.
    ENDIF.
  ENDIF.
*--> End of Insertion 20.06.2025

  LOOP AT lt_docflow INTO ls_docflow  ##INTO_OK
    WHERE vbtyp_n = 'CSVO' OR
          vbtyp_n = 'CSCT'.
    cv_po_ref = ls_docflow-docnum.
    EXIT.
  ENDLOOP.

ENDFORM.
*<<F36K917931 - 06 end of ins
