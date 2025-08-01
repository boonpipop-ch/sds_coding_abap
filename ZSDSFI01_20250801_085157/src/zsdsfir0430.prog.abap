*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0430
*  Creation Date      : 22.08.2024
*  Author             : Atitep B(Eviden)
*  Add-on ID          : N/A
*  Description        : Program Post Advance Recceived
*                       RICEP ZFIAR036
*  Purpose            :
*  Copied from        : ZSDSFIR0250
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
* 30.12.2024  F36K910484  Atitep B.  - Required house bank & Acct ID
*-----------------------------------------------------------------------
* 05.02.2025  F36K912090  Apichat C. - Skip delete data for duplicate check
*-----------------------------------------------------------------------
* 31.03.2025  F36K914952  Apichat C. - 420000515: Adjust default document date logic
*-----------------------------------------------------------------------

REPORT zsdsfir0430.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE zsdsfir0430_top.
INCLUDE zsdsfir0430_sel.
INCLUDE zsdsfir0430_alv.


*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SELECTION-SCREEN FUNCTION KEY 1.
  sscrfields-functxt_01 = 'Post Advance Received'(r00).
  SELECTION-SCREEN FUNCTION KEY 2.
  sscrfields-functxt_02 = 'Advance Received Report'(r01).

  PERFORM f_set_selscr_default.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_set_sel_screen.

AT SELECTION-SCREEN.
  PERFORM f_chk_sel_screen.
  IF sscrfields-ucomm = 'FC01'.
    CALL TRANSACTION 'ZSDSFI042'.                        "#EC CI_CALLTA
  ELSEIF sscrfields-ucomm = 'FC02'.
    CALL TRANSACTION 'ZSDSFI052'.                        "#EC CI_CALLTA
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_init_range.
  PERFORM f_init_var_new_entry.
  PERFORM f_get_open_items_new_entry.
  PERFORM f_lock_tran_new_entry.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM f_display_result_new USING gt_output_new.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f_set_selscr_default
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_selscr_default .

  DATA: lr_param   TYPE RANGE OF zsdscac001-param,
        lrs_dfpymn LIKE LINE OF grt_dfpymn,
        ls_genc    TYPE zcl_sdsca_utilities=>ts_gen_c,
        lrs_param  LIKE LINE OF lr_param,
        lf_param   TYPE zsdscac001-param.

  FIELD-SYMBOLS <l_field> TYPE any.

  lrs_param = 'ICP'.
  lrs_param-low = 'DF_*'.
  APPEND lrs_param TO lr_param.

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid = sy-repid          " ABAP Program Name
*     irt_param = lr_param          " Parameter name
*     irt_ext  =                  " Parameter extension
    IMPORTING
      et_gen_c = gt_genc                 " General Constants (GENC)
  ).

  LOOP AT gt_genc INTO ls_genc WHERE param_ext EQ 'DEFAULT' ##INTO_OK.
    CONCATENATE 'P_' ls_genc-param INTO lf_param.
    ASSIGN (lf_param) TO <l_field>.
    IF sy-subrc EQ 0.
      <l_field> = ls_genc-value_low.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_genc INTO ls_genc WHERE param EQ 'DFPYMN'
                                 AND param_ext EQ 'PDC' ##INTO_OK.
    lrs_dfpymn-sign = ls_genc-param_sign.
    lrs_dfpymn-option = ls_genc-param_option.
    lrs_dfpymn-low = ls_genc-value_low.
    APPEND lrs_dfpymn TO grt_dfpymn.
  ENDLOOP.

*  p_bukrs           = '1000'.
*  p_bukrsn          = '1000'.
*  p_bukrsu          = '1000'.
*  p_wrkdt           = sy-datum.
  p_wrkdtn          = sy-datum.
*  p_wrkdtu          = sy-datum.

*  tab_bt1           = TEXT-tb1.
*  tab_bt2           = TEXT-tb2.
*  tab_bt3           = TEXT-tb3.
*  tab_blk-prog      = sy-repid.
*  tab_blk-dynnr     = 100.
*  tab_blk-activetab = gc_tab_sel-collection_log.
*  gf_data_type      = gc_tab_sel-new_entry."COLLECTION_LOG.
  gf_dfpymn = p_dfpymn.

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = 'ZSDSFIR0250'                " ABAP Program Name
      if_param = gc_genc_param-bank_transfer  " Parameter name
*     if_ext   =                             " Parameter Extension
    IMPORTING
      et_range = grt_bank
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = 'ZSDSFIR0250'                " ABAP Program Name
      if_param = gc_genc_param-pdc            " Parameter name
*     if_ext   =                             " Parameter Extension
    IMPORTING
      et_range = grt_pdc
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_chk_sel_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_chk_sel_screen.

*  check sy-ucomm = 'ONLI'.
*  CASE gc_true.
*    WHEN rb_cren.
*      IF p_pernrn IS INITIAL.
*        MESSAGE e000(38) WITH TEXT-e15.
*      ELSE.
*        PERFORM validate_pernr.
*      ENDIF.
*    WHEN rb_chgn OR rb_disn.
*      IF p_pernrn IS INITIAL.
*      ELSE.
*        PERFORM validate_pernr.
*      ENDIF.
*    WHEN OTHERS.
*  ENDCASE.
  CASE sscrfields-ucomm.
    WHEN 'ONLI'.
      CASE gc_true.
        WHEN rb_cren.
          IF p_pernrn IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e15.
          ELSE.
            PERFORM validate_pernr.
          ENDIF.

*          IF p_rcvamn IS INITIAL.
*            MESSAGE e000(38) WITH TEXT-e11. "Please input Amount Receive
*          ENDIF.

          IF p_dfpymn IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e12.
          ELSE.
            PERFORM check_payment_method.
          ENDIF.

          IF p_dfactn IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e13.
          ENDIF.
          IF p_dfstan IS INITIAL.
            MESSAGE e000(38) WITH TEXT-e14.
          ENDIF.
*          IF p_dfbkdn IS INITIAL.
*            MESSAGE e000(38) WITH TEXT-e20. "Please input TF/Cheque Date
*          ENDIF.
*          IF rb_ar EQ gc_true.
*          ELSE.
*            IF p_dfhbkn IS INITIAL.
*              MESSAGE e000(38) WITH TEXT-e23. "Please input House Bank
*            ENDIF.
*            IF p_dfhktn IS INITIAL.
*              MESSAGE e000(38) WITH TEXT-e24.  "Please input Account ID
*            ENDIF.
*          ENDIF.
*
*          IF p_dfchqn IS INITIAL.
*            IF rb_pdc EQ gc_true.
*              MESSAGE e000(38) WITH TEXT-e19. "Please input Check number
*            ENDIF.
*          ENDIF.

        WHEN rb_chgn.
          IF p_pernrn IS INITIAL.
          ELSE.
            PERFORM validate_pernr.
          ENDIF.

        WHEN rb_disn.
          IF p_pernrn IS INITIAL.
          ELSE.
            PERFORM validate_pernr.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.

    WHEN 'OP'.
      IF rb_pdc EQ gc_true.
        PERFORM default_pdc.

      ELSE.
        p_dfpymn = gf_dfpymn.
        CLEAR: p_dfhbkn, p_dfhktn.

      ENDIF.
    WHEN OTHERS.
  ENDCASE.

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
  LOOP AT SCREEN.
*--------------------------------------------------------------------*
* New entry - Sub screen 200
*--------------------------------------------------------------------*
    CASE gc_true.
      WHEN rb_cren.
        IF screen-group1 = 'CSN' OR
           screen-group1 = 'TRN' OR
           screen-group1 = 'CHG'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

        CASE gc_true.
          WHEN rb_tran.
            IF screen-group1 = 'OP2'.
              screen-active = 0.
              MODIFY SCREEN.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

      WHEN rb_chgn OR rb_disn.
        IF screen-group1 = 'DFN'
        OR screen-group1 CP 'OP*'
        OR screen-group1 = 'CRE'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

        IF rb_disn EQ gc_true.
          IF screen-group1 = 'DF2'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_var_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_var_new_entry .

  DATA: ls_genc  TYPE zcl_sdsca_utilities=>ts_gen_c,
        lf_param TYPE zsdscac001-param.

  FIELD-SYMBOLS <l_value> TYPE any.

  IF rb_cren = gc_true OR
     rb_chgn = gc_true.
    gf_edit = abap_true.
  ELSE.
    gf_edit = abap_false.
  ENDIF.

  SELECT SINGLE waers
    FROM t001
    WHERE bukrs = @p_bukrsn
    INTO @gf_waers.

  IF p_pernrn IS INITIAL.
    CLEAR gf_fullname.
  ELSE.
    SELECT SINGLE
      concat_with_space( vorna, nachn, 1 ) AS fullname
      FROM pa0002
      WHERE pernr = @p_pernrn
      INTO @gf_fullname   ##WARN_OK.                    "#EC CI_NOORDER
  ENDIF.

  LOOP AT gt_genc INTO ls_genc WHERE param_ext EQ 'NEW' ##INTO_OK.
    ASSIGN COMPONENT ls_genc-param OF STRUCTURE gs_default_new_entry TO <l_value>.
    <l_value> = ls_genc-value_low.
*    CASE ls_genc-param.
*      WHEN gc_genc_param_new_entry-blart.
*        gs_default_new_entry-blart = ls_genc-value_low.
*      WHEN gc_genc_param_new_entry-bupla.
*        gs_default_new_entry-bupla = ls_genc-value_low.
*      WHEN gc_genc_param_new_entry-mwsk1.
*        gs_default_new_entry-mwsk1 = ls_genc-value_low.
*    ENDCASE.
  ENDLOOP.

  READ TABLE gt_genc INTO ls_genc WITH KEY param = 'FILE_PATH'.
  IF sy-subrc EQ 0.
    gf_path = ls_genc-value_low.
  ENDIF.

  CASE gc_true.
    WHEN rb_ar.
      lf_param = 'UMSKZ_AR'.
    WHEN rb_bank.
      lf_param = 'UMSKZ_BANK'.
    WHEN rb_pdc.
      lf_param = 'UMSKZ_PDC'.
    WHEN rb_tran.
      lf_param = 'UMSKZ_TRAN'.
    WHEN OTHERS.
  ENDCASE.
  READ TABLE gt_genc INTO ls_genc WITH KEY param = lf_param.
  IF sy-subrc EQ 0.
    gs_default_new_entry-umskz = ls_genc-value_low.
  ENDIF.

  READ TABLE gt_genc INTO ls_genc WITH KEY param = 'UMSKZ_TRAN'.
  IF sy-subrc EQ 0.
    gf_umskz_tran = ls_genc-value_low.
  ENDIF.

  READ TABLE gt_genc INTO ls_genc WITH KEY param = 'UMSKZ_PDC'.
  IF sy-subrc EQ 0.
    gf_umskz_pdc = ls_genc-value_low.
  ENDIF.

  READ TABLE gt_genc INTO ls_genc WITH KEY param = 'UMSKZ_AR'.
  IF sy-subrc EQ 0.
    gf_umskz_ar = ls_genc-value_low.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_open_items_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_open_items_new_entry .

  CASE gc_true.
    WHEN rb_cren.
* Get all new entries for duplicated checking.
      PERFORM f_get_new_entry_all.

    WHEN rb_chgn.
      PERFORM f_get_new_entry_open.
      PERFORM f_prepare_output.
    WHEN rb_disn.
      PERFORM f_get_new_entry_all.
*      PERFORM f_prepare_change_output_new.
  ENDCASE.
*<-- Start of Insertion 06.12.2024 (Get Additional Data)
  PERFORM f_get_addition_data CHANGING gt_output_new.
*--> End of Insertion 06.12.2024
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_new_entry_open
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_new_entry_open .

  SELECT log~*,
         CASE
           WHEN cust~ktokd = 'Z010' THEN
             concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
           WHEN cust~ktokd = 'Z050' THEN
             concat_with_space( partner~name_first, partner~name_last, 1 )
           ELSE
             concat_with_space( partner~name_org1, partner~name_org2, 1 )
         END AS cust_name,
         concat_with_space( per~vorna, per~nachn, 1 ) AS full_name
    FROM zsdsfit045 AS log
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    INNER JOIN pa0002 AS per
    ON per~pernr = log~pernr
    WHERE log~pernr IN @grt_pernr
    AND   log~bukrs       =  @p_bukrsn
    AND   log~work_date   IN @s_wrkdtn
    AND   log~kunnr       IN @s_kunnrn
    AND   log~xblnr       IN @s_xblnrn
*    AND   log~wrbtr       IN @grt_rcvamn
    AND   log~delete_flag = ''
    AND   log~belnr       = ''
    AND   log~gjahr       = ''
    AND   log~tranf_no    IN @s_trnfn
*    AND   log~block_status <> @gc_block_status
    INTO CORRESPONDING FIELDS OF TABLE @gt_output_new ##TOO_MANY_ITAB_FIELDS.

  IF gt_output_new IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e00. "No data found
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_new_entry_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_new_entry_all .
  SELECT log~*,
         CASE
           WHEN cust~ktokd = 'Z010' THEN
             concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
           WHEN cust~ktokd = 'Z050' THEN
             concat_with_space( partner~name_first, partner~name_last, 1 )
          ELSE
             concat_with_space( partner~name_org1, partner~name_org2, 1 )
         END AS cust_name,
         concat_with_space( per~vorna, per~nachn, 1 ) AS full_name
    FROM zsdsfit045 AS log
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    INNER JOIN pa0002 AS per
    ON per~pernr = log~pernr
    WHERE log~pernr IN @grt_pernr
    AND   log~bukrs       =  @p_bukrsn
    AND   log~work_date   IN @s_wrkdtn
    AND   log~xblnr       IN @s_xblnrn
    AND   log~kunnr       IN @s_kunnrn
*    AND   log~wrbtr       IN @grt_rcvamn
    AND   log~tranf_no    IN @s_trnfn
    INTO CORRESPONDING FIELDS OF TABLE @gt_output_new ##TOO_MANY_ITAB_FIELDS.

  IF gt_output_new IS INITIAL.
    IF rb_disn EQ gc_true.
      MESSAGE s000(38) WITH TEXT-e00. "No data found
    ENDIF.
  ELSE.
    IF rb_cren EQ gc_true.
      gt_output_temp = gt_output_new.
      DELETE gt_output_temp WHERE delete_flag = gc_true.  "<<F36K912090 ins
      CLEAR gt_output_new.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_lock_tran_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_lock_tran_new_entry .

  IF rb_chgn IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT gt_output_new INTO DATA(ls_output)    ##INTO_OK
    GROUP BY (
                key1 = ls_output-kunnr
                key2 = ls_output-tranf_no
              ).

    CALL FUNCTION 'ENQUEUE_EZSDSFIS144'
      EXPORTING
        mode_zsdsfis144 = 'E'
        kunnr           = ls_output-kunnr
        tranf_no        = ls_output-tranf_no
*       X_KUNNR         = ' '
*       _SCOPE          = '2'
*       _WAIT           = ' '
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
*& Form f_display_result_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT_NEW
*&---------------------------------------------------------------------*
FORM f_display_result_new  USING  ut_result TYPE tt_output_new.

  FIELD-SYMBOLS <lfs_result> TYPE zsdsfis144.

  IF rb_cren EQ gc_true AND ut_result IS INITIAL.
    APPEND INITIAL LINE TO ut_result ASSIGNING <lfs_result>.
    PERFORM set_default_new_entry CHANGING <lfs_result>.
  ENDIF.

* Set Container name
  gf_container = 'CTL_ALV'.
* Disable Header area
  IF rb_disn IS INITIAL.
    gf_alv_header = gc_true.
  ENDIF.

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
  ASSIGN ut_result TO <g_list>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM f_alv_build_fieldcat_new CHANGING gt_fieldcat.
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
*&---------------------------------------------------------------------*
*& Form f_alv_build_fieldcat_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_alv_build_fieldcat_new  CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <l_fieldcat>   TYPE  lvc_s_fcat.

* Build Field cat from Structure.
  PERFORM f_prepare_fieldcat_o  USING  gc_structure_new
                              CHANGING ct_fieldcat.

  LOOP AT ct_fieldcat ASSIGNING <l_fieldcat>.
    CASE <l_fieldcat>-fieldname.
      WHEN 'SEL'.
        <l_fieldcat>-edit       = gf_edit.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-col_pos    = 1.
        <l_fieldcat>-key        = gc_true.
        IF rb_disn = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'FULL_NAME' OR
           'CUST_NAME'.
        <l_fieldcat>-edit       = ''.
      WHEN 'BELNR'.
        <l_fieldcat>-hotspot    = gc_true.
        <l_fieldcat>-edit       = ''.
        IF rb_disn <> gc_true.
          <l_fieldcat>-no_out   = gc_true.
        ENDIF.
      WHEN 'GJAHR'.
        <l_fieldcat>-edit       = ''.
        IF rb_disn <> gc_true.
          <l_fieldcat>-no_out   = gc_true.
        ENDIF.


      WHEN 'INC_AMT'.
        <l_fieldcat>-reptext    = TEXT-f01.
        <l_fieldcat>-seltext    = TEXT-f01.
        <l_fieldcat>-scrtext_s  = TEXT-f01.
        <l_fieldcat>-scrtext_m  = TEXT-f01.
        <l_fieldcat>-scrtext_l  = TEXT-f01.

        <l_fieldcat>-edit       = ''.

      WHEN 'EXP_AMT' OR
*           'INC_AMT' OR
           'BANK_FEE'.
        <l_fieldcat>-edit       = ''.
      WHEN 'DELETE_FLAG' OR 'DELETE_DATE'.
        <l_fieldcat>-edit       = ''.
        IF rb_cren = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'REJDAT' OR 'REJRSN'.
        <l_fieldcat>-edit       = ''.
        IF rb_cren = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'TRANF_NO'.
        <l_fieldcat>-edit       = ''.
      WHEN 'UUID'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'BLOCK_STATUS'.
        IF rb_cren = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'UMSKZ' OR 'BUPLA' OR 'BUKRS' OR 'BLART' "OR 'MWSK1'
        OR 'PYMT_METHOD' OR 'BANK_DATE'
        OR 'CHEQUE_NO' OR 'PERNR' OR 'WORK_DATE' "OR 'ACTION_TYPE'
        .
        CLEAR <l_fieldcat>-edit.
      WHEN 'ATTACH_FILE_FLAG'.
        <l_fieldcat>-style = cl_gui_alv_grid=>mc_style_button.
        CLEAR <l_fieldcat>-edit.

*<-- Start of Insertion 06.12.2024 (Add Profit Center Desc)
      WHEN 'PRCTR_TXT'.
*       Text-f02: Profit Center Desc.
        <l_fieldcat>-reptext    = TEXT-f02.
        <l_fieldcat>-seltext    = TEXT-f02.
        <l_fieldcat>-scrtext_s  = TEXT-f02.
        <l_fieldcat>-scrtext_m  = TEXT-f02.
        <l_fieldcat>-scrtext_l  = TEXT-f02.
*--> End of Insertion 06.12.2024
*      WHEN OTHERS.
*        <l_fieldcat>-edit       = gf_edit.
      WHEN 'ERNAM' OR 'ERDAT' OR 'ERZMT' OR
           'REL_BLK_BY' OR 'REL_BLK_ON' OR 'REL_BLK_TIME'.
        <l_fieldcat>-tech = gc_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM f_user_command USING uf_ucomm  TYPE  sy-ucomm ##CALLED.

  DATA:
*    lv_uuid    TYPE zsdsfit045-uuid,
    lv_valid   TYPE char01,
    lv_refresh TYPE char01,
    lv_sel     TYPE char01.

  DATA(lref_msg) = NEW cl_alv_changed_data_protocol( i_calling_alv = gref_grid ).

  gref_grid->get_frontend_fieldcatalog(
    IMPORTING
      et_fieldcatalog = lref_msg->mt_fieldcatalog
  ).

  lref_msg->refresh_protocol( ).
*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
  CASE uf_ucomm.
    WHEN gc_func-sel_a_new.
      lv_sel = gc_true.

      PERFORM f_mark_sel_all_new USING lv_sel
                             CHANGING gt_output_new.

      gref_grid->refresh_table_display( ).

    WHEN gc_func-sel_n_new.
      lv_sel = ''.

      PERFORM f_mark_sel_all_new USING lv_sel
                             CHANGING gt_output_new.

      gref_grid->refresh_table_display( ).

    WHEN gc_func-save.
      IF gref_grid->is_ready_for_input( ) = 0.
        RETURN.
      ENDIF.

*     Validate Data
      gref_grid->check_changed_data( IMPORTING e_valid   = lv_valid
                                     CHANGING  c_refresh = lv_refresh ).
*     Continue processing only when valid
      IF lv_valid IS INITIAL.
        RETURN.
      ENDIF.

      PERFORM f_validate_mandatory_new CHANGING lv_valid lref_msg.
      PERFORM f_validate_input        CHANGING lv_valid lref_msg.

      IF lv_valid <> gc_true.
        lref_msg->display_protocol(  ).
        IF lv_valid IS INITIAL.
          RETURN.
        ENDIF.
      ENDIF.
      CASE gc_true.
        WHEN rb_cren.
          PERFORM f_create_new_entry.
        WHEN rb_chgn.
          PERFORM f_update_new_entry USING gc_true.
          PERFORM f_unlock_tran_new.
      ENDCASE.

      gref_grid->refresh_table_display( ).

    WHEN gc_func-del_new.
      PERFORM f_delete_new_entry.

    WHEN gc_func-attach_file.
      PERFORM f_attach_file CHANGING lv_valid lref_msg.

      IF lv_valid <> gc_true.
        lref_msg->display_protocol(  ).
        RETURN.
      ELSE.
        gref_grid->refresh_table_display( ).
      ENDIF.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_mark_sel_all_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SEL
*&      <-- GT_OUTPUT_NEW
*&---------------------------------------------------------------------*
FORM f_mark_sel_all_new  USING    uf_sel    TYPE char01
                         CHANGING ct_output TYPE tt_output_new.

  LOOP AT ct_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    <ls_output>-sel = uf_sel.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_mandatory_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALID
*&      <-- LREF_MSG
*&---------------------------------------------------------------------*
FORM f_validate_mandatory_new  CHANGING cf_valid TYPE abap_bool
                                   cref_msg TYPE REF TO cl_alv_changed_data_protocol.

  DATA:
*    lf_total    TYPE zsdsfit029-payin_amt,
    lf_valid TYPE abap_bool,
*    LF_TRANF_NO TYPE ZSDSFIS144-TRANF_NO,
*    lf_remain   TYPE zsdsfit029-payin_amt,
    lf_check TYPE xflag.
*    lt_chg      TYPE tt_output_new.

  cf_valid = abap_true.

  LOOP AT gt_output_new INTO DATA(ls_output)    ##INTO_OK
    WHERE sel = gc_true.

    "Skip check for delete data
    CHECK ls_output-delete_flag = ''.

    DATA(lf_row_id) = sy-tabix.

    IF ls_output-xblnr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'XBLNR' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-bukrs IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BUKRS' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-kunnr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'KUNNR' '' '' CHANGING cref_msg.
    ELSE.
      PERFORM validate_kunnr USING ls_output-kunnr CHANGING lf_valid.
      IF lf_valid = abap_false.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_invalid_value lf_row_id 'KUNNR' '' '' CHANGING cref_msg.
      ENDIF.
    ENDIF.

    IF ls_output-bldat IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BLDAT' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-prctr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'PRCTR' '' '' CHANGING cref_msg.
    ELSE.
      PERFORM validate_prctr USING ls_output-prctr CHANGING lf_valid.
      IF lf_valid = abap_false.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_invalid_value lf_row_id 'PRCTR' '' '' CHANGING cref_msg.
*<-- Start of Insertion 06.12.2024 (Get Profit Center Text)
      ELSE.
        PERFORM f_get_prctr_txt  USING  ls_output-prctr
                               CHANGING ls_output-prctr_txt.
        MODIFY gt_output_new FROM ls_output
                             INDEX lf_row_id.
*--> End of Insertion 06.12.2024
      ENDIF.
    ENDIF.

    IF ls_output-zfbdt IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'ZFBDT' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-bktxt IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BKTXT' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-sgtxt IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'SGTXT' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-budat IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BUDAT' '' '' CHANGING cref_msg.
    ENDIF.

*    IF ls_output-umskz IS INITIAL.
*      cf_valid = abap_false.
*      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'UMSKZ' '' '' CHANGING cref_msg.
*    ENDIF.

    IF ls_output-waers IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'WAERS' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-mwsk1 IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'MWSK1' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-bupla IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BUPLA' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-blart IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BLART' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-umskz IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'UMSKZ' '' '' CHANGING cref_msg.
    ENDIF.

*   <<F36K910484 start ins
    IF rb_tran IS NOT INITIAL.
      IF ls_output-hbkid IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'HBKID' '' '' CHANGING cref_msg.
      ENDIF.

      IF ls_output-hktid IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'HKTID' '' '' CHANGING cref_msg.
      ENDIF.
    ENDIF.
*   <<F36K910484 end ins

    CASE gc_true.
*      WHEN rb_cren.
*        IF rb_ar EQ gc_true.
*          lf_check = gc_true.
*        ELSE.
*          CLEAR lf_check.
*        ENDIF.
*
*      WHEN rb_chgn.
*        CLEAR lf_check.
      WHEN rb_disn.
        CLEAR lf_check.
      WHEN OTHERS.
        IF ls_output-blart = gs_default_new_entry-blart_ar_invoice.
          CLEAR lf_check.
        ELSE.
          lf_check = gc_true.
        ENDIF.

    ENDCASE.
    IF lf_check EQ gc_true.
*      IF ls_output-hbkid IS INITIAL.
*        cf_valid = abap_false.
*        PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'HBKID' '' '' CHANGING cref_msg.
*      ELSE.
*        PERFORM validate_hbkid USING ls_output-hbkid CHANGING lf_valid.
*        IF lf_valid = abap_false.
*          cf_valid = abap_false.
*          PERFORM f_add_msg USING gc_msg_ty-err_invalid_value lf_row_id 'HBKID' '' '' CHANGING cref_msg.
*        ENDIF.
*      ENDIF.
*
*      IF ls_output-hktid IS INITIAL.
*        cf_valid = abap_false.
*        PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'HKTID' '' '' CHANGING cref_msg.
*      ELSE.
*        PERFORM validate_hktid USING ls_output-hbkid ls_output-hktid CHANGING lf_valid.
*        IF lf_valid = abap_false.
*          cf_valid = abap_false.
*          PERFORM f_add_msg USING gc_msg_ty-err_invalid_value lf_row_id 'HKTID' '' '' CHANGING cref_msg.
*        ENDIF.
*      ENDIF.

      IF ls_output-pymt_method IN grt_bank   "Bank transfer
        AND grt_bank IS NOT INITIAL.
        READ TABLE gt_action_status
          INTO DATA(ls_action_status)
          WITH KEY action_type = ls_output-action_type
                   status      = ls_output-status.
        IF ls_action_status IS NOT INITIAL.
          IF ls_output-hbkid IS INITIAL.
            cf_valid = abap_false.
            PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'HBKID' '' '' CHANGING cref_msg.
          ELSE.
            PERFORM validate_hbkid USING ls_output-hbkid CHANGING lf_valid.
            IF lf_valid = abap_false.
              cf_valid = abap_false.
              PERFORM f_add_msg USING gc_msg_ty-err_invalid_value lf_row_id 'HBKID' '' '' CHANGING cref_msg.
            ENDIF.
          ENDIF.

          IF ls_output-hktid IS INITIAL.
            cf_valid = abap_false.
            PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'HKTID' '' '' CHANGING cref_msg.
          ELSE.
            PERFORM validate_hktid USING ls_output-hbkid ls_output-hktid CHANGING lf_valid.
            IF lf_valid = abap_false.
              cf_valid = abap_false.
              PERFORM f_add_msg USING gc_msg_ty-err_invalid_value lf_row_id 'HKTID' '' '' CHANGING cref_msg.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF ls_output-pymt_method IN grt_pdc  "PDC
        AND grt_pdc IS NOT INITIAL.

        READ TABLE gt_action_status
          WITH KEY action_type = ls_output-action_type
                   status      = ls_output-status
          TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.

          IF ls_output-bank_date IS INITIAL.
            cf_valid = abap_false.
            PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BANK_DATE' '' '' CHANGING cref_msg.
          ENDIF.

          IF ls_output-cheque_no IS INITIAL.
            cf_valid = abap_false.
            PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'CHEQUE_NO' '' '' CHANGING cref_msg.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    IF ls_output-wrbtr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'WRBTR' '' '' CHANGING cref_msg.
    ENDIF.

*    PERFORM check_duplicate USING ls_output CHANGING lf_valid lf_tranf_no.
*    IF lf_valid = abap_false.
*      cf_valid = abap_false.
*      PERFORM f_add_msg USING gc_msg_ty-err_duplicate lf_row_id '' lf_tranf_no '' CHANGING cref_msg.
*    ENDIF.

    IF ls_output-umskz = gf_umskz_tran. "SP GL. 'N'
      IF ls_output-bank_date IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BANK_DATE' '' '' CHANGING cref_msg.
      ENDIF.
    ELSEIF ls_output-umskz = gf_umskz_pdc. "SP GL. 'E'
      IF ls_output-bank_date IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'BANK_DATE' '' '' CHANGING cref_msg.
      ENDIF.

      IF ls_output-cheque_no IS INITIAL.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_required lf_row_id 'CHEQUE_NO' '' '' CHANGING cref_msg.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF cf_valid <> gc_true.
    RETURN.
  ENDIF.

*  IF p_rcvamn IS NOT INITIAL.
*    lf_remain = p_rcvamn - lf_total.
*
*    lt_chg = VALUE #( FOR ls_chg IN gt_output_new WHERE ( sel = 'X' AND delete_flag = ''  ) ( CORRESPONDING #( ls_chg  ) )  ).
*
*    IF lf_remain <> 0 AND lt_chg IS NOT INITIAL. "contain data change / create
*      cf_valid = abap_false.
*      PERFORM f_add_msg USING gc_msg_ty-err_amt_balance lf_row_id 'PAYIN_AMT' '' '' CHANGING cref_msg.
*    ENDIF.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_add_msg
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_add_msg USING
                uf_err_type   TYPE char20
                uf_rowid      TYPE int4
                uf_fieldname  TYPE lvc_fname
                uf_text1                                ##PERF_NO_TYPE
                uf_text2                                ##PERF_NO_TYPE
               CHANGING
                cref_msg      TYPE REF TO cl_alv_changed_data_protocol.

  DATA:
    lv_msgid     TYPE symsgid,
    lv_msgty     TYPE symsgty,
    lv_msgno     TYPE symsgno,
    lv_msgv1     TYPE symsgv1,
    lv_msgv2     TYPE symsgv2,
    lv_msgv3     TYPE symsgv3,
    lv_msgv4     TYPE symsgv4,
    lv_fieldname TYPE lvc_fname,
    lv_rowid     TYPE int4.


  CASE uf_err_type.
    WHEN gc_msg_ty-err_no_item_select.
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e26 }|. "Please select item to process

    WHEN gc_msg_ty-err_item_select_gt_1.
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e27 }|. "Please select only 1 item per process

    WHEN gc_msg_ty-err_required.
      gref_grid->get_frontend_fieldcatalog(
        IMPORTING
          et_fieldcatalog = DATA(lt_fcat)        " Field Catalog
      ).

      "Error in item nnn, please input required field xxx
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e04 } { uf_rowid },|. "Error in item nn
      lv_msgv2 = |{ TEXT-e03 } { VALUE #( lt_fcat[ fieldname = uf_fieldname ]-seltext OPTIONAL ) }| .    "please input required field
      lv_msgv3 = uf_text1.
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.
    WHEN gc_msg_ty-err_amt_over.
      "Error in item nnn, input amount is over xxx
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e04 } { uf_rowid },|. "Error in item nnn
      lv_msgv2 = |{ TEXT-e06 } { uf_text1 }|. "input amount is over xxx
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN gc_msg_ty-err_amt_balance.
      "The difference is too large for clearing
      lv_msgid = 'F5'.
      lv_msgty = 'E'.
      lv_msgno = '263'.
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN gc_msg_ty-err_action_status.
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e04 } { uf_rowid },|. "Error in item nnn
      lv_msgv2 = |{ TEXT-e07 } { uf_text1 }|. "Can not input action type xxx
      lv_msgv3 = |{ TEXT-e08 } { uf_text2 } { TEXT-e09 }|. "Status xxx with empty input Amount Receive
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN gc_msg_ty-err_action_status_before.
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e04 } { uf_rowid },|. "Error in item nnn
      lv_msgv2 = |{ TEXT-e16 } ({ uf_text1 })|. "Can not input action type / status (xxx)
      lv_msgv3 = |{ TEXT-e17 } { uf_text2 } { TEXT-e18 }|. "payment method yyy for mode before receiving payment
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN gc_msg_ty-err_invalid_value.
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e04 } { uf_rowid },|. "Error in item nnn
      lv_msgv2 = TEXT-e22. "Invalid value
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN gc_msg_ty-err_duplicate.
      lv_msgid = '38'.
      lv_msgty = 'W'.
      lv_msgno = '000'.
*      lv_msgv1 = |{ TEXT-e04 } { uf_rowid },|. "Error in item nnn
      lv_msgv1 = |{ TEXT-e25 } { uf_text1 }|.  "Duplicate Data with Transfer no. xxxx
      lv_msgv2 = TEXT-e37.    "please contact approver
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN gc_msg_ty-err_invalid_input.
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e04 } { uf_rowid }|. "Error in item nnn
      lv_msgv2 = |{ TEXT-e28 } ({ uf_text1 })|. "Invalid input value (xxx)
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN gc_msg_ty-err_change_not_permit.
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e04 } { uf_rowid }|. "Error in item nnn
      lv_msgv2 = |{ TEXT-e29 } ({ uf_text1 })|. "Do not input xxx
      lv_msgv3 = uf_text2.
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN gc_msg_ty-err_invalid_acc_id.
      lv_msgid = '38'.
      lv_msgty = 'E'.
      lv_msgno = '000'.
      lv_msgv1 = |{ TEXT-e04 } { uf_rowid }|. "Error in item nnn
      lv_msgv2 = |{ TEXT-e35 } ({ uf_text1 }).|. "Incorrect account ID (xxx)
      lv_msgv3 = |{ TEXT-e36 }|.  "Please select new Account ID
      lv_fieldname = uf_fieldname.
      lv_rowid     = uf_rowid.

    WHEN OTHERS.
  ENDCASE.

  cref_msg->add_protocol_entry(
    i_msgid     = lv_msgid            " Message ID
    i_msgty     = lv_msgty            " Message Type
    i_msgno     = lv_msgno            " Message No.
    i_msgv1     = lv_msgv1            " Message Variable1
    i_msgv2     = lv_msgv2            " Message Variable2
    i_msgv3     = lv_msgv3            " Message Variable3
    i_msgv4     = lv_msgv4            " Message Variable4
    i_fieldname = lv_fieldname        " Field Name
    i_row_id    = lv_rowid            " RowID
*   i_tabix     = lv_tabix            " Table Index
  ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_input
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALID
*&      <-- LREF_MSG
*&---------------------------------------------------------------------*
FORM f_validate_input  CHANGING cf_valid TYPE abap_bool     ##NEEDED
                                cref_msg TYPE REF TO cl_alv_changed_data_protocol.

*  CHECK cf_valid = gc_true.

  DATA:
    lf_valid    TYPE abap_bool,
    lf_tranf_no TYPE zsdsfis144-tranf_no.

  LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output>)
    WHERE sel = gc_true
    AND   delete_flag = ''.

    DATA(lv_row_id) = sy-tabix.

    CLEAR: lf_tranf_no.
    lf_valid = cf_valid.

    IF <ls_output>-hbkid IS NOT INITIAL AND
       <ls_output>-hktid IS NOT INITIAL.
      SELECT SINGLE hkont
        INTO @DATA(lv_hkont)        ##NEEDED
        FROM t012k
        WHERE bukrs = @<ls_output>-bukrs
        AND   hbkid = @<ls_output>-hbkid
        AND   hktid = @<ls_output>-hktid.
      IF sy-subrc <> 0.
        CLEAR cf_valid.
        PERFORM f_add_msg USING gc_msg_ty-err_invalid_acc_id lv_row_id 'HKTID'
                                <ls_output>-hktid ''  CHANGING cref_msg.
      ENDIF.
    ENDIF.

    PERFORM check_duplicate USING <ls_output> CHANGING lf_valid lf_tranf_no.
    IF lf_valid <> gc_true.
      IF cf_valid IS NOT INITIAL.
        cf_valid = lf_valid.
      ENDIF.
      <ls_output>-block_status = gc_block_status.
      PERFORM f_add_msg USING gc_msg_ty-err_duplicate lv_row_id '' lf_tranf_no '' CHANGING cref_msg.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_create_new_entry .

  DATA: lt_new_entry       TYPE STANDARD TABLE OF zsdsfit045.

*  lt_new_entry = VALUE #( FOR ls_output IN gt_output_new WHERE ( sel = 'X'  ) ( CORRESPONDING #( ls_output  ) )  ).
*
*  IF lt_new_entry IS INITIAL.
*    MESSAGE s000(38) WITH TEXT-e10.
*    RETURN.
*  ENDIF.

  LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output>)
    WHERE sel = 'X'.

*    "Get UUID
*    TRY.
*        <ls_output>-uuid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
*      CATCH cx_uuid_error. " Error Class for UUID Processing Errors
*        RETURN.
*    ENDTRY.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = gc_no-nr
        object                  = gc_no-obj
        toyear                  = sy-datum(4)
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = <ls_output>-tranf_no
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

    APPEND INITIAL LINE TO lt_new_entry ASSIGNING FIELD-SYMBOL(<ls_zsdsfit045>).
    <ls_zsdsfit045>       = CORRESPONDING #( <ls_output> ).
    <ls_zsdsfit045>-ernam = sy-uname.
    <ls_zsdsfit045>-erdat = sy-datum.
    <ls_zsdsfit045>-erzmt = sy-uzeit.
    IF rb_ar EQ gc_true.
      CLEAR: <ls_zsdsfit045>-hbkid, <ls_zsdsfit045>-hktid.
    ENDIF.
  ENDLOOP.

  IF lt_new_entry IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e10.
    RETURN.
  ELSE.
    MODIFY zsdsfit045 FROM TABLE lt_new_entry.

    "Collection log saved
    IF <ls_output> IS ASSIGNED.
      MESSAGE s000(38) WITH TEXT-r04 <ls_output>-tranf_no.
    ELSE.
      MESSAGE s000(38) WITH TEXT-r02.
    ENDIF.

    PERFORM f_reload_create_screen.
  ENDIF.

*  gref_grid->set_ready_for_input(
*                     EXPORTING i_ready_for_input = 0 ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_update_new_entry USING uf_read_only TYPE char01 .

  DATA: lt_new_entry       TYPE STANDARD TABLE OF zsdsfit045.

  lt_new_entry = VALUE #( FOR ls_output IN gt_output_new WHERE ( sel = 'X'  ) ( CORRESPONDING #( ls_output  ) )  ).

  IF lt_new_entry IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e10.
    RETURN.
  ENDIF.

  LOOP AT lt_new_entry ASSIGNING FIELD-SYMBOL(<ls_zsdsfit045>).

    <ls_zsdsfit045>-update_by   = sy-uname.
    <ls_zsdsfit045>-update_on   = sy-datum.
    <ls_zsdsfit045>-update_time = sy-uzeit.
  ENDLOOP.

  MODIFY zsdsfit045 FROM TABLE lt_new_entry.
  IF sy-subrc EQ 0.
    MESSAGE s000(38) WITH TEXT-r02.
  ELSE.
    MESSAGE i000(38) WITH TEXT-r03 DISPLAY LIKE 'E'.
  ENDIF.

*  LEAVE TO SCREEN 0.
  IF uf_read_only = gc_true.
    gref_grid->set_ready_for_input(
      EXPORTING
        i_ready_for_input = 0 ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_unlock_tran_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_unlock_tran_new .

  LOOP AT gt_output_new INTO DATA(ls_output)      ##INTO_OK
    GROUP BY (
                key1 = ls_output-kunnr
                key2 = ls_output-tranf_no
              ).

    CALL FUNCTION 'DEQUEUE_EZSDSFIS144'
      EXPORTING
*       MODE_ZSDSFIS144       = 'E'
        kunnr    = ls_output-kunnr
        tranf_no = ls_output-tranf_no
*       X_KUNNR  = ' '
*       _SCOPE   = '3'
*       _SYNCHRON             = ' '
*       _COLLECT = ' '
      .

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_delete_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_delete_new_entry .

  DATA:
    lf_ans        TYPE xflag.

* Confirm to reject.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-t01
      text_question         = TEXT-t02
      display_cancel_button = space
    IMPORTING
      answer                = lf_ans
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF lf_ans <> '1' OR sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF rb_cren = gc_true.
    DELETE gt_output_new WHERE sel = 'X'.
  ELSE.
    LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output>)
      WHERE sel = 'X'.
      <ls_output>-delete_flag = gc_true.
      <ls_output>-delete_date = sy-datum.
    ENDLOOP.

    PERFORM f_update_new_entry USING ''.

    DELETE gt_output_new WHERE sel = 'X'.
  ENDIF.

  gref_grid->refresh_table_display(
    EXCEPTIONS
      finished = 1                " Display was Ended (by Export)
      OTHERS   = 2
  ).

  IF sy-subrc NE 0.
    RETURN.
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
  IF gf_edit EQ gc_true.
*----------------------------------------------------------------------*
* New entry
*----------------------------------------------------------------------*

*    IF rb_disn IS INITIAL.
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '3'.   "separator
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    "Select All
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '0'.   "normal Button
    ls_toolbar-function = gc_func-sel_a_new.   "fcode
    ls_toolbar-icon = '@4B@'.
*    ls_toolbar-text = TEXT-b03.
    ls_toolbar-quickinfo = 'Select All'(b03)  ##SHARE_OK.
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    "Select None
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '0'.   "normal Button
    ls_toolbar-function = gc_func-sel_n_new.   "fcode
    ls_toolbar-icon = '@4D@'.
*    ls_toolbar-text = TEXT-b04.
    ls_toolbar-quickinfo = 'Select None'(b04)  ##SHARE_OK.
    APPEND ls_toolbar TO uref_object->mt_toolbar.

    "Attach file
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '0'.   "normal Button
    ls_toolbar-function = gc_func-attach_file. "fcode
    ls_toolbar-icon = '@FM@'.
    ls_toolbar-text = TEXT-b05.
    ls_toolbar-quickinfo = 'Attach file'(b05)  ##SHARE_OK.
    APPEND ls_toolbar TO uref_object->mt_toolbar.

*    ENDIF.

*    "Display attach file
*    CLEAR ls_toolbar.
*    ls_toolbar-butn_type = '0'.   "normal Button
*    ls_toolbar-function = gc_func-disp_attach. "fcode
*    ls_toolbar-icon = '@FM@'.
*    ls_toolbar-text = TEXT-b06.
*    ls_toolbar-quickinfo = 'Display Attach File'(b06)  ##SHARE_OK.
*    APPEND ls_toolbar TO uref_object->mt_toolbar.

    IF rb_chgn = gc_true.

      DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&PASTE'.
      DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&APPEND'.
      DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.
      DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&DELETE_ROW'.
      DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&COPY_ROW'.

      CLEAR ls_toolbar.
      ls_toolbar-butn_type = '3'.   "separator
      APPEND ls_toolbar TO uref_object->mt_toolbar.
      CLEAR ls_toolbar.
      ls_toolbar-butn_type = '0'.   "normal Button
      ls_toolbar-function = gc_func-del_new.   "fcode
      ls_toolbar-icon = '@11@'.
      ls_toolbar-quickinfo = 'Delete selected item'(b01)   ##SHARE_OK.
      APPEND ls_toolbar TO uref_object->mt_toolbar.
    ENDIF.

*  ELSE.
*
*    "Display attach file
*    CLEAR ls_toolbar.
*    ls_toolbar-butn_type = '0'.   "normal Button
*    ls_toolbar-function = gc_func-disp_attach. "fcode
*    ls_toolbar-icon = '@FM@'.
*    ls_toolbar-text = TEXT-b06.
*    ls_toolbar-quickinfo = 'Display Attach File'(b06)  ##SHARE_OK.
*    APPEND ls_toolbar TO uref_object->mt_toolbar.

  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_prepare_f4   ##CALLED.

  DATA:
    ls_f4 TYPE lvc_s_f4,
    lt_f4 TYPE lvc_t_f4.


  CLEAR ls_f4.
  ls_f4-fieldname = 'BANKK'.
  ls_f4-register  = gc_true.
  ls_f4-getbefore = gc_true.
  ls_f4-chngeafter = space.
  INSERT ls_f4 INTO TABLE lt_f4.

  CALL METHOD gref_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

* register events for abap objects (backend)
  CREATE OBJECT gref_onf4.
  SET HANDLER gref_onf4->on_f4 FOR gref_grid.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_on_data_changed
*----------------------------------------------------------------------*
*       ALV ON_DATA_CHANGED Event
*----------------------------------------------------------------------*
FORM f_on_data_changed USING
                          uref_data_changed	TYPE REF TO	cl_alv_changed_data_protocol
                          uf_onf4           TYPE  char01    ##NEEDED
                          uf_onf4_before    TYPE  char01    ##NEEDED
                          uf_onf4_after     TYPE  char01    ##NEEDED
                          uf_ucomm          TYPE  sy-ucomm  ##CALLED.


  DATA:
    ls_ins_row    TYPE lvc_s_moce.

  FIELD-SYMBOLS: <l_mod>  TYPE STANDARD TABLE,
                 <l_data> TYPE zsdsfis144.

  READ TABLE uref_data_changed->mt_inserted_rows INTO ls_ins_row INDEX 1.
  IF sy-subrc EQ 0 AND ls_ins_row-row_id IS NOT INITIAL.
    ASSIGN uref_data_changed->mp_mod_rows->* TO <l_mod>.
    IF sy-subrc EQ 0.
      UNASSIGN <l_data>.
      READ TABLE <l_mod> ASSIGNING <l_data> INDEX ls_ins_row-row_id." 1.
      IF sy-subrc EQ 0.
      ELSE.
        READ TABLE <l_mod> ASSIGNING <l_data> INDEX 1.
*        IF sy-subrc EQ 0.
*        ENDIF.
      ENDIF.

      IF <l_data> IS ASSIGNED.
        PERFORM set_default_new_entry CHANGING <l_data>.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM f_validate_chg USING uref_data_changed.


ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_top_of_page
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM f_top_of_page USING uref_dyndoc_id  TYPE  REF TO cl_dd_document. "#EC CALLED

  CONSTANTS:
    lc_size_key TYPE  sdydo_value  VALUE '30%',
    lc_size_val TYPE  sdydo_value  VALUE '70%'.

  DATA:
    lf_total     TYPE  zsdsfit029-payin_amt,
*    lf_remain    TYPE  zsdsfit029-payin_amt,
    lf_text      TYPE  sdydo_text_element,
    lref_table   TYPE  REF TO cl_dd_table_element,
    lref_col_key TYPE  REF TO cl_dd_area,
    lref_col_val TYPE  REF TO cl_dd_area.

* Create table
  CALL METHOD uref_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '50%'
    IMPORTING
      table         = lref_table.
* Get Column Element
  CALL METHOD lref_table->add_column
    EXPORTING
      width  = lc_size_key
    IMPORTING
      column = lref_col_key.
* Get Column Element
  CALL METHOD lref_table->add_column
    EXPORTING
      width  = lc_size_val
    IMPORTING
      column = lref_col_val.
* Set Key column style
  CALL METHOD lref_table->set_column_style
    EXPORTING
      col_no       = 1
      sap_emphasis = cl_dd_area=>strong.

*  CASE gf_data_type.
*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
*    WHEN gc_tab_sel-new_entry.

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD lref_table->new_row.
* Text-h01 : Bill Collector Input Data
  lf_text = TEXT-h01.
  CALL METHOD lref_col_key->add_text
    EXPORTING
      text = lf_text.


*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD lref_table->new_row.

* Text-h01 : Collector Name
  lf_text = TEXT-h02.
  CALL METHOD lref_col_key->add_text
    EXPORTING
      text = lf_text.

  IF p_pernrn IS INITIAL.
    lf_text = '*'.
  ELSE.
    lf_text = |{ p_pernrn ALPHA = OUT } { gf_fullname }|.
  ENDIF.
  CALL METHOD lref_col_val->add_text
    EXPORTING
      text = lf_text.

  LOOP AT gt_output_new INTO DATA(ls_output_new) WHERE sel = gc_true  ##INTO_OK.
    lf_total += ls_output_new-wrbtr.
    lf_total += ls_output_new-exp_amt.
    lf_total -= ls_output_new-inc_amt.
    lf_total += ls_output_new-bank_fee.
  ENDLOOP.
*  lf_remain = p_rcvamn - lf_total.
*
**-----------------------
** Add value in Line3
**-----------------------
*  IF p_rcvamn IS NOT INITIAL.
*    CALL METHOD lref_table->new_row.
*
** Text-h04 : Received Amount
*    lf_text = |{ TEXT-h04 }|.
*    CALL METHOD lref_col_key->add_text
*      EXPORTING
*        text = lf_text.
*
*    WRITE p_rcvamn CURRENCY gf_waers TO lf_text.
*    CALL METHOD lref_col_val->add_text
*      EXPORTING
*        text = lf_text.
*
*  ENDIF.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD lref_table->new_row.

* Text-h05 : Assigned Amount
  lf_text = |{ TEXT-h05 }|.
  CALL METHOD lref_col_key->add_text
    EXPORTING
      text = lf_text.

*  LF_TEXT = |{ lf_total CURRENCY = gf_WAERS  } |.
  WRITE lf_total CURRENCY gf_waers TO lf_text.
  CALL METHOD lref_col_val->add_text
    EXPORTING
      text = lf_text.

**-----------------------
** Add value in Line5
**-----------------------
*  IF p_rcvamn IS NOT INITIAL.
*
*    CALL METHOD lref_table->new_row.
*
** Text-h06 : Balance
*    lf_text = |{ TEXT-h06 }|.
*    CALL METHOD lref_col_key->add_text
*      EXPORTING
*        text = lf_text.
*
**    LF_TEXT = |{ lf_remain CURRENCY = gf_WAERS } |.
*    WRITE lf_remain CURRENCY gf_waers TO lf_text.
*    CALL METHOD lref_col_val->add_text
*      EXPORTING
*        text = lf_text.
*
*  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_on_data_changed_finished
*----------------------------------------------------------------------*
*       ALV ON_DATA_CHANGED_FINISHED Event
*----------------------------------------------------------------------*
FORM f_on_data_changed_finished USING
                                  uf_modified   TYPE  char01      ##NEEDED
                                  ut_good_cells TYPE  lvc_t_modi  ##CALLED.

  DATA: "ls_output  TYPE zsdsfis085,
    ls_stable  TYPE lvc_s_stbl,
    lv_refresh TYPE abap_bool.

  LOOP AT ut_good_cells INTO DATA(ls_good)    ##INTO_OK.

*    CASE gf_data_type.
*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
*      WHEN gc_tab_sel-new_entry.
    READ TABLE gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output_new>)
      INDEX ls_good-row_id.

    CHECK sy-subrc EQ 0.

    CHECK <ls_output_new> IS ASSIGNED.

    CASE ls_good-fieldname.
      WHEN 'SEL'.
        lv_refresh = abap_true.

      WHEN 'WRBTR' OR 'EXP_AMT' OR 'INC_AMT' OR 'BANK_FEE'.
        lv_refresh = abap_true.

      WHEN 'BLDAT'.
        IF <ls_output_new>-budat IS INITIAL.
          <ls_output_new>-budat = <ls_output_new>-bldat.
        ENDIF.

        IF <ls_output_new>-zfbdt IS INITIAL.
          <ls_output_new>-zfbdt = <ls_output_new>-bldat.
        ENDIF.

        lv_refresh = abap_true.
      WHEN 'KUNNR'.
        SELECT SINGLE
           CASE
           WHEN cust~ktokd = 'Z010' THEN
             concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
           WHEN cust~ktokd = 'Z050' THEN
             concat_with_space( partner~name_first, partner~name_last, 1 )
           ELSE
               concat_with_space( partner~name_org1, partner~name_org2, 1 )
           END AS cust_name
           FROM kna1 AS cust
           INNER JOIN cvi_cust_link AS link
           ON cust~kunnr = link~customer
           INNER JOIN but000 AS partner
           ON partner~partner_guid = link~partner_guid
           WHERE cust~kunnr = @<ls_output_new>-kunnr
           INTO @<ls_output_new>-cust_name  ##WARN_OK.
*        IF sy-subrc EQ 0.
*        ELSE.
*          MESSAGE i125(67) DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.

        lv_refresh = abap_true.

*<<F36K914952 Begin of ins
     WHEN 'BANK_DATE'.
      IF <ls_output_new>-umskz = gf_umskz_tran.
        <ls_output_new>-bldat = <ls_output_new>-bank_date.

        lv_refresh = abap_true.
      ENDIF.
*<<F36K914952 End of ins

    ENDCASE.
*    ENDCASE.
  ENDLOOP.


  IF lv_refresh = abap_true.
*    ls_stable-row = gc_true.
    ls_stable-col = gc_true.

    PERFORM f_init_doc_alv_header.

    gref_grid->refresh_table_display(
      is_stable = ls_stable
    ).

  ENDIF.

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
                        uf_column_id  TYPE  lvc_s_col       ##CALLED.

  READ TABLE gt_output_new INTO DATA(ls_output)
    INDEX uf_row_id-index.

  CASE uf_column_id-fieldname.
    WHEN 'BELNR'.
      IF ls_output-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_output-belnr.
        SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_pernr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM validate_pernr .

  DATA: lv_pernr TYPE pa0002-pernr.

  CHECK p_pernrn IS NOT INITIAL.

  SELECT SINGLE pernr
    INTO lv_pernr
    FROM pa0002
    WHERE pernr EQ p_pernrn ##WARN_OK.
  IF sy-subrc NE 0.
    MESSAGE e172(hrpadinpe1).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form default_pdc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM default_pdc .

  DATA: ls_genc  TYPE zcl_sdsca_utilities=>ts_gen_c,
        lf_param TYPE zsdscac001-param.

  FIELD-SYMBOLS <l_field> TYPE any.

  LOOP AT gt_genc INTO ls_genc WHERE param_ext EQ 'PDC'
                                 AND sequence EQ 1  ##INTO_OK.

    CONCATENATE 'P_' ls_genc-param INTO lf_param.
    ASSIGN (lf_param) TO <l_field>.
    IF sy-subrc EQ 0.
      <l_field> = ls_genc-value_low.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_payment_method
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM check_payment_method .

  CHECK p_dfpymn IS NOT INITIAL.

  IF rb_pdc EQ gc_true.
    IF p_dfpymn IN grt_dfpymn.
    ELSE.
      MESSAGE e000(38) WITH TEXT-e21.
    ENDIF.

  ELSE.
    IF p_dfpymn IN grt_dfpymn.
      MESSAGE e000(38) WITH TEXT-e21.
*    ELSE.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_range
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_init_range .

  DATA: lrs_pernr LIKE LINE OF grt_pernr.
*        ls_rcvamn LIKE LINE OF grt_rcvamn.

  CASE gc_true.
    WHEN rb_cren.

    WHEN rb_chgn OR rb_disn.
      IF p_pernrn IS INITIAL.
      ELSE.
        lrs_pernr = 'IEQ'.
        lrs_pernr-low = p_pernrn.
        APPEND lrs_pernr TO grt_pernr.
      ENDIF.

*      IF p_rcvamn IS INITIAL.
*      ELSE.
*        ls_rcvamn = 'IEQ'.
*        ls_rcvamn-low = p_rcvamn.
*        APPEND ls_rcvamn TO grt_rcvamn.
*      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_default_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_default_new_entry  CHANGING cs_result TYPE zsdsfis144.

  DATA:
    lv_mode    TYPE char03,
    lt_celltab TYPE lvc_t_styl.

  "Get UUID
  TRY.
      cs_result-uuid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
    CATCH cx_uuid_error. " Error Class for UUID Processing Errors
      RETURN.
  ENDTRY.

  cs_result-bukrs       = p_bukrsn.
  cs_result-waers       = gf_waers.
  cs_result-pernr       = p_pernrn.
  cs_result-full_name   = gf_fullname.
  cs_result-work_date   = p_wrkdtn.
  cs_result-pymt_method = p_dfpymn.
  cs_result-action_type = p_dfactn.
  cs_result-status      = p_dfstan.
  cs_result-hbkid       = p_dfhbkn.
  cs_result-hktid       = p_dfhktn.
  cs_result-bank_date   = p_dfbkdn.
  cs_result-cheque_no   = p_dfchqn.
  cs_result-bldat       = sy-datum.
  cs_result-budat       = sy-datum.
  cs_result-zfbdt       = sy-datum.

  cs_result-umskz       = gs_default_new_entry-umskz.
*  cs_result-blart       = gs_default_new_entry-blart.
  cs_result-bupla       = gs_default_new_entry-bupla.
*  cs_result-mwsk1       = gs_default_new_entry-mwsk1.

  IF rb_ar = gc_true.
    cs_result-blart       = gs_default_new_entry-blart_ar_invoice.
    cs_result-mwsk1       = gs_default_new_entry-mwsk1_ar_invoice.
  ELSEIF rb_bank = gc_true.
    cs_result-blart       = gs_default_new_entry-blart_ar_invoice.
    cs_result-mwsk1       = gs_default_new_entry-mwsk1_ar_invoice.
  ELSE.
    cs_result-blart       = gs_default_new_entry-blart.
    cs_result-mwsk1       = gs_default_new_entry-mwsk1.
  ENDIF.

  IF rb_tran = gc_true.
    lv_mode = 'RWN'.
  ELSEIF rb_pdc = gc_true.
    lv_mode = 'RWE'.
  ELSEIF rb_ar = gc_true.
    lv_mode = 'RWA'.
  ELSE.
    lv_mode = 'RW'.
  ENDIF.

  PERFORM f_fill_celltab USING lv_mode CHANGING lt_celltab.

  "Set read only house bank for sp gl 'A'
  IF cs_result-umskz = gf_umskz_ar OR
     cs_result-umskz = gf_umskz_bank.
    lv_mode = 'ROH'.

    PERFORM f_fill_celltab USING lv_mode CHANGING lt_celltab.

  ENDIF.

  cs_result-celltab     = lt_celltab.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_kunnr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM validate_kunnr  USING  uf_kunnr TYPE kna1-kunnr
                     CHANGING cf_valid TYPE char01.

  DATA: lf_kunnr TYPE kna1-kunnr.

  SELECT SINGLE kunnr
    INTO lf_kunnr
    FROM kna1
    WHERE kunnr EQ uf_kunnr.
  IF sy-subrc EQ 0.
    cf_valid = gc_true.
  ELSE.
    CLEAR cf_valid.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_prctr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM validate_prctr  USING    uf_prctr TYPE cepc-prctr
                     CHANGING cf_valid TYPE abap_bool.

  DATA: lf_prctr TYPE cepc-prctr.

  SELECT SINGLE prctr
    INTO lf_prctr
    FROM cepc
    WHERE prctr EQ uf_prctr
      AND datbi GE sy-datum
      AND kokrs EQ p_bukrsn ##WARN_OK.
  IF sy-subrc EQ 0.
    cf_valid = gc_true.
  ELSE.
    CLEAR cf_valid.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_HBKID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM validate_hbkid  USING    uf_hbkid TYPE t012-hbkid
                     CHANGING cf_valid TYPE abap_bool.

  DATA: lf_hbkid TYPE t012-hbkid.

  SELECT SINGLE hbkid
    INTO lf_hbkid
    FROM t012
    WHERE bukrs EQ p_bukrsn
      AND hbkid EQ uf_hbkid.
  IF sy-subrc EQ 0.
    cf_valid = gc_true.
  ELSE.
    CLEAR cf_valid.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_HKTID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM validate_hktid  USING    uf_hbkid TYPE t012k-hbkid
                              uf_hktid TYPE t012k-hktid
                     CHANGING cf_valid TYPE abap_bool.

  DATA: lf_hktid TYPE t012k-hktid.

  SELECT SINGLE hktid
    INTO lf_hktid
    FROM t012k
    WHERE bukrs EQ p_bukrsn
      AND hbkid EQ uf_hbkid
      AND hktid EQ uf_hktid.
  IF sy-subrc EQ 0.
    cf_valid = gc_true.
  ELSE.
    CLEAR cf_valid.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_duplicate
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM check_duplicate  USING    us_output  TYPE zsdsfis144
                      CHANGING cf_valid   TYPE abap_bool
                               cf_tranf_no TYPE zsdsfis144-tranf_no.

  DATA: ls_output TYPE zsdsfis144.

  ls_output = us_output.
  READ TABLE gt_output_temp INTO ls_output
  WITH KEY  kunnr = ls_output-kunnr
            wrbtr = ls_output-wrbtr
            budat = ls_output-budat
            umskz = ls_output-umskz
            pymt_method = ls_output-pymt_method
            hbkid = ls_output-hbkid
            hktid = ls_output-hktid
            bank_date = ls_output-bank_date
            cheque_no = ls_output-cheque_no.
  IF sy-subrc EQ 0.
    cf_valid = gc_warning.
    cf_tranf_no = ls_output-tranf_no.
  ELSE.
    cf_valid = gc_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_attach_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_attach_file CHANGING cf_valid TYPE abap_bool
                            cref_msg TYPE REF TO cl_alv_changed_data_protocol.

  TYPES : BEGIN OF lty_upload,
            field(255),
          END OF lty_upload.

  DATA: lt_upload           TYPE STANDARD TABLE OF lty_upload,
*        LV_LOCAL            TYPE IBIPPARMS-PATH,
        lv_upload_file_name TYPE c LENGTH 255,
        lt_file             TYPE filetable,
        lv_rc               TYPE i,
        lv_ext              TYPE zsdsfit045-attach_ext.

  DATA(lv_count) = REDUCE i( INIT i = 0 FOR ls IN gt_output_new WHERE ( sel = 'X' ) NEXT i = i + 1 ).

  IF lv_count < 1.
    cf_valid = abap_false.
    PERFORM f_add_msg USING gc_msg_ty-err_no_item_select '' '' '' '' CHANGING cref_msg.
    RETURN.
  ELSEIF lv_count > 1.
    PERFORM f_add_msg USING gc_msg_ty-err_item_select_gt_1 '' '' '' '' CHANGING cref_msg.
    RETURN.
  ENDIF.

  cf_valid = gc_true.

  READ TABLE gt_output_new ASSIGNING FIELD-SYMBOL(<ls_sel>)
    WITH KEY sel = gc_true.

  IF <ls_sel> IS NOT ASSIGNED.
    RETURN.
  ENDIF.

  cl_gui_frontend_services=>file_open_dialog(
*   EXPORTING
*      window_title            =                    " Title Of File Open Dialog
*      default_extension       =  " Default Extension
*      default_filename        =                    " Default File Name
*       file_filter             =                  " File Extension Filter String
*      with_encoding           =                  " File Encoding
*      initial_directory       =                  " Initial Directory
*      multiselection          =                  " Multiple selections poss.
    CHANGING
      file_table              = lt_file           " Table Holding Selected Files
      rc                      = lv_rc              " Return Code, Number of Files or -1 If Error Occurred
*     user_action             =                  " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*     file_encoding           =
    EXCEPTIONS
      file_open_dialog_failed = 1                " "Open File" dialog failed
      cntl_error              = 2                " Control error
      error_no_gui            = 3                " No GUI available
      not_supported_by_gui    = 4                " GUI does not support this
      OTHERS                  = 5
  ) ##SUBRC_OK.

  IF lt_file IS NOT INITIAL.
    READ TABLE lt_file INTO DATA(ls_file) INDEX 1.
  ENDIF.

  IF ls_file-filename IS INITIAL.
    RETURN.
  ELSE.
    CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
      EXPORTING
        filename  = ls_file-filename
      IMPORTING
        extension = lv_ext.

  ENDIF.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = CONV string( ls_file-filename ) "CONV string( lv_local )           " Name of file
      filetype                = 'BIN'            " File Type (ASCII, Binary)
    CHANGING
      data_tab                = lt_upload                " Transfer table for file contents
    EXCEPTIONS
      file_open_error         = 1                " File does not exist and cannot be opened
      file_read_error         = 2                " Error when reading file
      no_batch                = 3                " Front-End Function Cannot Be Executed in Backgrnd
      gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
      invalid_type            = 5                " Incorrect parameter FILETYPE
      no_authority            = 6                " No Upload Authorization
      unknown_error           = 7                " Unknown error
      bad_data_format         = 8                " Cannot Interpret Data in File
      header_not_allowed      = 9                " Invalid header
      separator_not_allowed   = 10               " Invalid separator
      header_too_long         = 11               " Header information currently restricted to 1023 bytes
      unknown_dp_error        = 12               " Error when calling data provider
      access_denied           = 13               " Access to File Denied
      dp_out_of_memory        = 14               " Not Enough Memory in DataProvider
      disk_full               = 15               " Storage Medium full
      dp_timeout              = 16               " Timeout of DataProvider
      not_supported_by_gui    = 17               " GUI does not support this
      error_no_gui            = 18               " GUI not available
      OTHERS                  = 19
  ).
  IF sy-subrc <> 0.
    MESSAGE i000(38) WITH 'Error! Please check File path'(e34).
  ELSE.
    IF lt_upload[] IS NOT INITIAL.

      lv_upload_file_name = |{ gf_path }\\{ <ls_sel>-uuid }.{ lv_ext }|.
      DELETE DATASET lv_upload_file_name.
      OPEN DATASET lv_upload_file_name FOR OUTPUT IN BINARY MODE.
      IF sy-subrc <> 0.

      ELSE.
        LOOP AT lt_upload INTO DATA(ls_upload)  ##INTO_OK.
          TRANSFER ls_upload TO lv_upload_file_name.
        ENDLOOP.
        IF sy-subrc = 0.
          <ls_sel>-attach_file_flag = gc_true.
          <ls_sel>-attach_ext       = lv_ext.
          MESSAGE s000(38) WITH 'Data has been saved.'(s04).

          IF rb_chgn = gc_true.
            PERFORM f_update_new_entry USING ''.
          ENDIF.

        ENDIF.
        CLOSE DATASET lv_upload_file_name.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_disp_attach
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALID
*&      <-- LREF_MSG
*&---------------------------------------------------------------------*
FORM f_disp_attach  USING    uf_uuid     TYPE zsdsfis144-uuid
                             uf_type     TYPE zsdsfis144-attach_ext.


  DATA: lv_file_name TYPE c LENGTH 255.

  lv_file_name = |{ gf_path }\\{ uf_uuid }.{ uf_type }|.


  CALL FUNCTION 'Z_SDSFI_DISPLAY_PDF'
    EXPORTING
      iv_filename = CONV string( lv_file_name )
      iv_ext      = uf_type.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_button_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALID
*&      <-- LREF_MSG
*&---------------------------------------------------------------------*
FORM f_button_click USING us_row_no TYPE lvc_s_roid   ##CALLED
                          us_col_id TYPE lvc_s_col.

*  DATA lv_valid TYPE abap_bool.
*
*  DATA(lr_msg) = NEW cl_alv_changed_data_protocol(  ).

  IF us_col_id-fieldname = 'ATTACH_FILE_FLAG'.
    READ TABLE gt_output_new
      INTO DATA(ls_output_new)
      INDEX us_row_no-row_id.
    IF sy-subrc EQ 0 AND ls_output_new-attach_file_flag = gc_true.
      PERFORM f_disp_attach USING ls_output_new-uuid ls_output_new-attach_ext.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_chg
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UREF_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM f_validate_chg  USING uref_data_changed TYPE REF TO  cl_alv_changed_data_protocol.

  DATA:
    ls_mod_cell   TYPE lvc_s_modi ##NEEDED.

  LOOP AT uref_data_changed->mt_mod_cells INTO ls_mod_cell ##INTO_OK.

    READ TABLE gt_output_new
      INTO DATA(ls_output)
      INDEX ls_mod_cell-row_id.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    CASE ls_mod_cell-fieldname.
      WHEN 'MWSK1'.

        IF ls_output-blart = gs_default_new_entry-blart_ar_invoice.
          IF ls_mod_cell-value <> 'O7' AND
             ls_mod_cell-value <> 'O0'.
            PERFORM f_add_msg USING gc_msg_ty-err_invalid_input ls_mod_cell-row_id 'MWSK1'
                                  ls_mod_cell-value '' CHANGING uref_data_changed.

          ENDIF.
        ELSEIF ls_mod_cell-value <> gs_default_new_entry-mwsk1.
          PERFORM f_add_msg USING gc_msg_ty-err_invalid_input ls_mod_cell-row_id 'MWSK1'
                                ls_mod_cell-value '' CHANGING uref_data_changed.
        ENDIF.

      WHEN 'EXP_AMT'.
        IF ls_output-blart = gs_default_new_entry-blart_ar_invoice.
          IF ls_mod_cell-value IS NOT INITIAL.
            PERFORM f_add_msg USING gc_msg_ty-err_change_not_permit ls_mod_cell-row_id 'EXP_AMT'
                                TEXT-e30 TEXT-e33 CHANGING uref_data_changed.
          ENDIF.
        ENDIF.

      WHEN 'INC_AMT'.
        IF ls_output-blart = gs_default_new_entry-blart_ar_invoice.
          IF ls_mod_cell-value IS NOT INITIAL.
            PERFORM f_add_msg USING gc_msg_ty-err_change_not_permit ls_mod_cell-row_id 'INC_AMT'
                                TEXT-e31 TEXT-e33 CHANGING uref_data_changed.
          ENDIF.
        ENDIF.
      WHEN 'BANK_FEE'.
        IF ls_output-blart = gs_default_new_entry-blart_ar_invoice.
          IF ls_mod_cell-value IS NOT INITIAL.
            PERFORM f_add_msg USING gc_msg_ty-err_change_not_permit ls_mod_cell-row_id 'BANK_FEE'
                                TEXT-e32 TEXT-e33 CHANGING uref_data_changed.
          ENDIF.
        ENDIF.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_prepare_output .

  DATA:
    lv_mode    TYPE char03,
    lt_celltab TYPE lvc_t_styl.


  LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output>).
    CLEAR:
      lt_celltab.

    IF <ls_output>-block_status IS NOT INITIAL.
      lv_mode = 'RO'.
    ELSEIF <ls_output>-umskz = gf_umskz_ar AND
      <ls_output>-blart  = gs_default_new_entry-blart_ar_invoice.
      lv_mode = 'RWA'.
    ELSEIF <ls_output>-umskz = gf_umskz_pdc.
      lv_mode = 'RWE'.
    ELSEIF <ls_output>-umskz = gf_umskz_tran.
      lv_mode = 'RWN'.
    ELSE.
      lv_mode = 'RW'.
    ENDIF.

    PERFORM f_fill_celltab USING lv_mode CHANGING lt_celltab.

    "Set read only house bank for sp gl 'A'
    IF <ls_output>-umskz = gf_umskz_ar OR
       <ls_output>-umskz = gf_umskz_bank.
      lv_mode = 'ROH'.

      PERFORM f_fill_celltab USING lv_mode CHANGING lt_celltab.

    ENDIF.

    <ls_output>-celltab     = lt_celltab.
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
FORM f_fill_celltab  USING VALUE(uf_mode) TYPE char03
                     CHANGING ct_celltab  TYPE lvc_t_styl.

  DATA: lv_mode    TYPE raw4.

  IF uf_mode(2) EQ 'RW'.
    lv_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE. "uf_mode eq 'RO'
    lv_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.


  CASE uf_mode.
    WHEN 'ROH'.
      lv_mode = cl_gui_alv_grid=>mc_style_disabled.

      PERFORM f_add_celltab USING lv_mode:
            'HBKID'       CHANGING ct_celltab,
            'HKTID'       CHANGING ct_celltab,
            'BANK_DATE'   CHANGING ct_celltab,
            'CHEQUE_NO'   CHANGING ct_celltab.
    WHEN 'RWE'.
      PERFORM f_add_celltab USING lv_mode:
             'BANK_DATE'    CHANGING ct_celltab,
             'CHEQUE_NO'    CHANGING ct_celltab.
    WHEN 'RWN'.
*<<F36K914952 Begin of ins
      PERFORM f_add_celltab USING cl_gui_alv_grid=>mc_style_disabled
             'BLDAT'        CHANGING ct_celltab.
*<<F36K914952 End of ins

      PERFORM f_add_celltab USING lv_mode:
             'BANK_DATE'    CHANGING ct_celltab.
  ENDCASE.

  IF uf_mode <> 'RWA'.
    PERFORM f_add_celltab USING lv_mode:
          'EXP_AMT'       CHANGING ct_celltab,
          'INC_AMT'       CHANGING ct_celltab,
          'BANK_FEE'      CHANGING ct_celltab.
  ENDIF.

  PERFORM f_add_celltab USING lv_mode:
        'KUNNR'       CHANGING ct_celltab,
        'XBLNR'       CHANGING ct_celltab,
        'BLDAT'       CHANGING ct_celltab,
        'BUDAT'       CHANGING ct_celltab,
        'WRBTR'       CHANGING ct_celltab,
        'PRCTR'       CHANGING ct_celltab,
        'ZFBDT'       CHANGING ct_celltab,
        'BKTXT'       CHANGING ct_celltab,
        'SGTXT'       CHANGING ct_celltab,
        'WAERS'       CHANGING ct_celltab,
        'MWSKZ'       CHANGING ct_celltab,

        'HBKID'       CHANGING ct_celltab,
        'HKTID'       CHANGING ct_celltab,
        'BANK_DATE'   CHANGING ct_celltab,
        'CHEQUE_NO'   CHANGING ct_celltab,

        'ACTION_TYPE' CHANGING ct_celltab,
        'STATUS'      CHANGING ct_celltab.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_add_celltab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_add_celltab USING uf_mode TYPE raw4
                         uf_field TYPE lvc_fname
                   CHANGING ct_celltab TYPE lvc_t_styl.

  DATA: ls_celltab TYPE lvc_s_styl.

  ls_celltab-fieldname = uf_field.
  ls_celltab-style = uf_mode.
  INSERT ls_celltab INTO TABLE ct_celltab.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_reload_create_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_reload_create_screen .

  CLEAR:
    gt_output_temp,
    gt_fieldcat,
    gt_output_new.

  PERFORM f_get_open_items_new_entry.

  IF rb_cren EQ gc_true AND gt_output_new IS INITIAL.
    APPEND INITIAL LINE TO gt_output_new ASSIGNING FIELD-SYMBOL(<lfs_result>).
    PERFORM set_default_new_entry CHANGING <lfs_result>.
  ENDIF.

  gref_grid->refresh_table_display( ).

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_ADDITION_DATA
*----------------------------------------------------------------------*
*  Get Additional Data
*----------------------------------------------------------------------*
FORM f_get_addition_data  CHANGING ct_output_new TYPE tt_output_new.

  LOOP AT ct_output_new ASSIGNING FIELD-SYMBOL(<l_output>).
    PERFORM f_get_prctr_txt  USING  <l_output>-prctr
                           CHANGING <l_output>-prctr_txt.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_PRCTR_TXT
*----------------------------------------------------------------------*
*  Get Profir Center Text
*----------------------------------------------------------------------*
FORM f_get_prctr_txt  USING  uf_prctr  TYPE  zsdsfis144-prctr
                    CHANGING cf_prctr_txt TYPE zsdsfis144-prctr_txt.

  TYPES: BEGIN OF ts_cepct,
           prctr TYPE cepct-prctr,
           ltext TYPE cepct-ltext,
         END OF ts_cepct.
  TYPES: tt_cepct TYPE SORTED TABLE OF ts_cepct
                       WITH UNIQUE KEY prctr.

  CONSTANTS:
    lc_kokrs  TYPE  cepc-kokrs  VALUE '1000'.

  STATICS:
    lt_cepct TYPE  tt_cepct,
    ls_cepct TYPE  ts_cepct.


* Initialize Output
  CLEAR: cf_prctr_txt.

* Only Value exist
  IF uf_prctr IS INITIAL.
    RETURN.
  ENDIF.

* Check Memory
  IF uf_prctr NE ls_cepct-prctr.
*   Check Buffer
    READ TABLE lt_cepct INTO ls_cepct
                        WITH KEY prctr = uf_prctr
                        BINARY SEARCH.
    IF sy-subrc NE 0.
*     Read from DB
      SELECT b~prctr,
             b~ltext
        FROM cepc AS a
               INNER JOIN cepct AS b
                 ON  b~spras = @sy-langu
                 AND b~prctr = a~prctr
                 AND b~datbi = a~datbi
                 AND b~kokrs = a~kokrs
       WHERE a~prctr EQ @uf_prctr
         AND a~kokrs EQ @lc_kokrs
         AND a~datbi GE @sy-datum
         AND a~datab LE @sy-datum
       ORDER BY a~datab ASCENDING,
                a~datbi ASCENDING
        INTO @ls_cepct
          UP TO 1 ROWS.                                "#EC CI_BUFFJOIN
      ENDSELECT.
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.
*     Save to Memory
      INSERT ls_cepct INTO TABLE lt_cepct.
    ENDIF.

  ENDIF.

* Assign Output
  cf_prctr_txt = ls_cepct-ltext.

ENDFORM.
