*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0650_F01
*  Creation Date      : 24.01.2025
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic
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
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM f_authorize_check USING uf_tcode  TYPE  sy-tcode.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD uf_tcode.

  IF sy-subrc <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE s172(00) WITH uf_tcode.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_set_selscr_default
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_selscr_default .

  DATA: "lr_spgl             TYPE RANGE OF bsid_view-umskz,
        "lr_item_txt         TYPE RANGE OF bsid_view-sgtxt,
    lr_posting_doc_type TYPE RANGE OF bsid_view-blart.

  p_bukrs = '1000'.

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid           " ABAP Program Name
      if_param = gc_genc_param-doc_type  " Parameter name
    IMPORTING
      et_range = s_blart[]
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid           " ABAP Program Name
      if_param = gc_genc_param-sp_gl  " Parameter name
    IMPORTING
      et_range = s_umskz[]
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                  " ABAP Program Name
      if_param = gc_genc_param-posting_doc_type " Parameter name
    IMPORTING
      et_range = lr_posting_doc_type
  ).

  p_bldat = sy-datum.
  p_budat = sy-datum.

  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = p_bukrs
*     DATE  = SY-DATUM
    IMPORTING
*     CURRM =
      curry = p_gjahr.


  p_blart = VALUE #( lr_posting_doc_type[ 1 ]-low OPTIONAL ).


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
  DATA: ls_grp_dsp LIKE LINE OF gt_group.

  SELECT
    FROM bsid_view AS doc
    INNER JOIN acdoca AS acdoc
    ON  acdoc~rbukrs = doc~bukrs
    AND acdoc~belnr  = doc~belnr
    AND acdoc~gjahr  = doc~gjahr
    AND acdoc~buzei  = doc~buzei
    INNER JOIN kna1 AS cus
    ON doc~kunnr = cus~kunnr
    INNER JOIN bkpf AS hdr
    ON  doc~bukrs = hdr~bukrs
    AND doc~belnr = hdr~belnr
    AND doc~gjahr = hdr~gjahr
    FIELDS
      doc~kunnr,                "Customer
      cus~name1,                "Customer Name
      hdr~bktxt,                "Header text
      doc~xblnr,                "Reference
      doc~gjahr  AS gjahr_opn,
      doc~belnr  AS belnr_opn,  "Document Number
      doc~buzei  AS buzei_opn,
      doc~blart,                "Document Type
      doc~bldat,                "Document Date
      doc~budat,                "Posting Date
      doc~umskz,                      "Special G/L
      ( doc~dmbtr + doc~mwsts ) AS dmbtr,
      doc~waers,                "Currency
      acdoc~prctr,              "Profit center
      doc~mwskz,                "Tax Code
      doc~zuonr,                "Assignment
      doc~sgtxt,                "Item text
      doc~shkzg                 "DR/CR ind
    WHERE
      doc~bukrs =  @p_bukrs  AND
      doc~kunnr IN @s_kunnr  AND
      doc~umskz IN @s_umskz  AND
      doc~blart IN @s_blart  AND
      doc~belnr IN @s_belnr  AND
      doc~gjahr =  @p_gjahr  AND
      doc~prctr IN @s_prctr  AND
      doc~xstov =  ''        AND
*      doc~mwskz <> 'O7'      AND   "F36K921145 - 01 del
      acdoc~rldnr = '0L'
    INTO CORRESPONDING FIELDS OF TABLE @gt_open_itms      ##TOO_MANY_ITAB_FIELDS.

  IF sy-subrc <> 0.
    MESSAGE s004(zsdsfi01). "No data found!
  ENDIF.

  IF gt_open_itms IS NOT INITIAL.
    SELECT
      cust~kunnr,
      cust~j_1tpbupl
      FROM fitha_pbupl_d AS cust
      INNER JOIN @gt_open_itms AS opn
      ON cust~kunnr = opn~kunnr
      WHERE cust~default_branch = @gc_true
      INTO TABLE @gt_cust_branch.

    SORT gt_cust_branch BY kunnr.
  ENDIF.

  LOOP AT gt_open_itms ASSIGNING FIELD-SYMBOL(<ls_open_item>).
    <ls_open_item>-dmbtr = COND #( WHEN <ls_open_item>-shkzg = 'H'
                                    THEN <ls_open_item>-dmbtr * -1
                                    ELSE <ls_open_item>-dmbtr ).

    PERFORM f_get_prctr_text USING <ls_open_item>-prctr
                             CHANGING <ls_open_item>-ktext.

  ENDLOOP.

  SORT gt_open_itms BY kunnr zuonr blart DESCENDING gjahr_opn belnr_opn.

  LOOP AT gt_open_itms INTO DATA(ls_open_itm)
    GROUP BY ( kunnr = ls_open_itm-kunnr zuonr = ls_open_itm-zuonr )  ##INTO_OK.

    CLEAR ls_grp_dsp.
    ls_grp_dsp-kunnr = ls_open_itm-kunnr.
    ls_grp_dsp-name1 = ls_open_itm-name1.
*    ls_grp_dsp-bktxt = ls_open_itm-bktxt.
*    ls_grp_dsp-xblnr = ls_open_itm-xblnr.
    ls_grp_dsp-zuonr = ls_open_itm-zuonr.
*    IF ls_open_itm-blart = gc_blart_dz.
*      ls_grp_dsp-prctr = ls_open_itm-prctr.
*      ls_grp_dsp-ktext = ls_open_itm-ktext.
*    ENDIF.
    ls_grp_dsp-dmbtr = REDUCE bsid-dmbtr( INIT total TYPE zsdsde_amount
                            FOR open_amt IN gt_open_itms
                            WHERE ( kunnr = ls_grp_dsp-kunnr AND zuonr = ls_grp_dsp-zuonr )
                            NEXT total = total + open_amt-dmbtr ).
    ls_grp_dsp-waers = ls_open_itm-waers.
*    ls_grp_dsp-mwskz = gc_default-mwskz.

    APPEND ls_grp_dsp TO gt_group.

  ENDLOOP.

  DELETE gt_group WHERE dmbtr IS NOT INITIAL.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_prctr_text
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_GRP_DSP_PRCTR
*&      <-- LS_GRP_DSP_KTEXT
*&---------------------------------------------------------------------*
FORM f_get_prctr_text  USING    uf_prctr TYPE cepct-prctr
                       CHANGING cf_ktext TYPE cepct-ktext.
  READ TABLE gt_profit_center
    INTO DATA(ls_profit_center)
    WITH KEY prctr = uf_prctr.
  IF sy-subrc NE 0.
    SELECT SINGLE
      prctr, ktext
      FROM cepct
      WHERE spras =  @sy-langu
      AND   prctr =  @uf_prctr
      AND   datbi >= @sy-datum
      INTO @DATA(ls_prctr)        ##WARN_OK.                "#EC WARNOK
    IF sy-subrc EQ 0.
      cf_ktext = ls_prctr-ktext.
      INSERT ls_prctr INTO TABLE gt_profit_center.
    ENDIF.
  ELSE.
    cf_ktext = ls_profit_center-ktext.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_display_result  USING  ut_result TYPE tt_output        ##NEEDED.

* Show progress
* Text-p99 : Generating ALV Report . . .
  mc_show_progress 99 TEXT-p99.

* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM f_alv_build_fieldcat CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <l_fieldcat>   TYPE  lvc_s_fcat.

* Build Field cat from Structure.
  PERFORM f_prepare_fieldcat_o  USING  gc_structure
                              CHANGING ct_fieldcat.

  LOOP AT ct_fieldcat ASSIGNING <l_fieldcat>.

    CASE <l_fieldcat>-fieldname.
      WHEN 'SEL'.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-edit       = gc_true.
        <l_fieldcat>-coltext    = TEXT-co1.
        <l_fieldcat>-seltext    = TEXT-co1.
        <l_fieldcat>-scrtext_s  = TEXT-co1.
        <l_fieldcat>-scrtext_m  = TEXT-co1.
        <l_fieldcat>-scrtext_l  = TEXT-co1.
      WHEN 'STATUS_ICON'.
        <l_fieldcat>-coltext    = TEXT-co2.
        <l_fieldcat>-seltext    = TEXT-co2.
        <l_fieldcat>-scrtext_s  = TEXT-co2.
        <l_fieldcat>-scrtext_m  = TEXT-co2.
        <l_fieldcat>-scrtext_l  = TEXT-co2.
        <l_fieldcat>-icon       = gc_true.
      WHEN 'KUNNR'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'NAME1'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'GJAHR_OPN'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'BUZEI_OPN'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'SHKZG'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'KTEXT'.
        <l_fieldcat>-coltext    = TEXT-co3.
        <l_fieldcat>-seltext    = TEXT-co3.
        <l_fieldcat>-scrtext_s  = TEXT-co3.
        <l_fieldcat>-scrtext_m  = TEXT-co3.
        <l_fieldcat>-scrtext_l  = TEXT-co3.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_hierarchy
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_create_hierarchy .

  DATA:
    lv_1st_key  TYPE lvc_nkey,
    lv_grp_key  TYPE lvc_nkey,
    lv_last_key TYPE lvc_nkey,

    lt_node_key TYPE lvc_t_nkey.

  SORT gt_open_itms BY kunnr zuonr blart DESCENDING gjahr_opn belnr_opn.

  LOOP AT gt_group INTO DATA(ls_grp).

    PERFORM f_add_node_grp USING ls_grp
                           CHANGING lv_grp_key.

    IF lv_1st_key IS INITIAL.
      lv_1st_key = lv_grp_key.
    ENDIF.

    APPEND lv_grp_key TO gt_group_key.
    APPEND lv_grp_key TO gt_all_nodes.

    LOOP AT gt_open_itms INTO DATA(ls_open_item)
      WHERE kunnr = ls_grp-kunnr
      AND   zuonr = ls_grp-zuonr.

      PERFORM f_add_node_complete_line USING
                                         ls_open_item
                                         lv_grp_key
                                       CHANGING
                                          lv_last_key.

      APPEND lv_last_key TO gt_all_nodes.
    ENDLOOP.

    APPEND lv_grp_key TO lt_node_key.

    CLEAR lv_grp_key.
  ENDLOOP.

  gref_tree->expand_nodes(
    EXPORTING
      it_node_key             = lt_node_key                  " Node Key
    EXCEPTIONS
      failed                  = 1                " General Error
      cntl_system_error       = 2                " "
      error_in_node_key_table = 3                " Node Table Contains Errors
      dp_error                = 4                " Error in Data Provider
      node_not_found          = 5                " node_not_found
      OTHERS                  = 6
  ).

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  gref_tree->set_top_node(
    EXPORTING
      i_node_key        = lv_1st_key       " Node Key to be Selected
    EXCEPTIONS
      cntl_system_error = 1                " "
      node_not_found    = 2                " Error in Data Provider
      failed            = 3                " General Error
      OTHERS            = 4
  ).

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CALL METHOD gref_tree->frontend_update.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_add_grp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_GRP_KEY
*&---------------------------------------------------------------------*
FORM f_add_node_grp USING    us_grp     TYPE gty_group
               CHANGING cf_grp_key TYPE lvc_nkey.


  DATA: lv_node_text   TYPE lvc_value,
        lv_relat_key   TYPE lvc_nkey,

* set item-layout
        lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

*  ls_item_layout-t_image = '@3P@'.
  ls_item_layout-fieldname = gref_tree->c_hierarchy_column_name.
  ls_item_layout-style     = cl_gui_column_tree=>style_intensifd_critical.
  ls_item_layout-class     = cl_gui_column_tree=>item_class_text.
*  ls_item_layout-editable  = gc_true.
  APPEND ls_item_layout TO lt_item_layout.

  CLEAR ls_item_layout.
  ls_item_layout-fieldname = 'SEL'.
  ls_item_layout-style     = cl_gui_column_tree=>style_intensified.
  ls_item_layout-class     = cl_gui_column_tree=>item_class_checkbox.
  ls_item_layout-editable  = gc_true.
  APPEND ls_item_layout TO lt_item_layout.

  CLEAR ls_item_layout.
  ls_item_layout-fieldname = 'BELNR'.
  ls_item_layout-class = cl_gui_column_tree=>item_class_link.
  APPEND ls_item_layout TO lt_item_layout.


* add node
  lv_node_text =  |{ us_grp-kunnr ALPHA = OUT } { us_grp-name1 } - { us_grp-zuonr }|.

  DATA: ls_node TYPE lvc_s_layn.
  ls_node-n_image   = space.
  ls_node-exp_image = space.
  ls_node-style     = cl_gui_column_tree=>style_emphasized_a.

  CALL METHOD gref_tree->add_node
    EXPORTING
      i_relat_node_key = lv_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_text
      is_outtab_line   = us_grp
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = cf_grp_key.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form add_complete_line
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_OPEN_ITEM
*&      --> LV_GRP_KEY
*&      <-- LV_LAST_KEY
*&---------------------------------------------------------------------*
FORM f_add_node_complete_line  USING
                                   us_open_item TYPE zsdsfis077
                                   uf_grp_key   TYPE lvc_nkey
                          CHANGING cv_last_key TYPE lvc_nkey.


  DATA: lv_node_text   TYPE lvc_value,
        lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = gref_tree->c_hierarchy_column_name.
  ls_item_layout-class     = cl_gui_column_tree=>item_class_text.
  ls_item_layout-editable = ''.
  APPEND ls_item_layout TO lt_item_layout.

  CLEAR ls_item_layout.
  ls_item_layout-fieldname = 'BELNR_OPN'.
  ls_item_layout-class = cl_gui_column_tree=>item_class_link.
  APPEND ls_item_layout TO lt_item_layout.

  CLEAR ls_item_layout.
  ls_item_layout-fieldname  = 'SEL'.
  ls_item_layout-class      = cl_gui_column_tree=>item_class_text.
  ls_item_layout-disabled   = gc_true.
  APPEND ls_item_layout TO lt_item_layout.

  lv_node_text =  |{ us_open_item-belnr_opn }|.

  DATA: ls_node TYPE lvc_s_layn.
  ls_node-n_image   = space.
  ls_node-exp_image = space.
*  ls_node-disabled  = gc_true.

  CALL METHOD gref_tree->add_node
    EXPORTING
      i_relat_node_key = uf_grp_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = us_open_item
      i_node_text      = lv_node_text
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = cv_last_key.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_document
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&      --> GC_MODE_TEST_RUN
*&---------------------------------------------------------------------*
FORM f_post_document  USING    uf_mode   TYPE char1
                      CHANGING ct_group  TYPE tt_group
                               ct_result TYPE tt_output.

  DATA: ls_clearing TYPE zsdsfis077.

  LOOP AT ct_group ASSIGNING FIELD-SYMBOL(<ls_group>)
    WHERE belnr IS INITIAL.

    ls_clearing = CORRESPONDING #( <ls_group> ).

    PERFORM f_posting_interface_start.

    PERFORM f_post_interface_clearing USING uf_mode
                                      CHANGING ls_clearing
                                               ct_result.

    PERFORM f_posting_interface_end.

    <ls_group>-status_icon = ls_clearing-status_icon.
    <ls_group>-status_msg  = ls_clearing-status_msg.
    <ls_group>-belnr       = ls_clearing-belnr.
    <ls_group>-gjahr       = ls_clearing-gjahr.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_posting_interface_start
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_posting_interface_start .
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_function         = 'C'
*     I_GROUP            = ' '
*     I_KEEP             = ' '
*     I_MODE             = 'N'
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

  CALL FUNCTION 'POSTING_INTERFACE_END'
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
FORM f_post_interface_clearing  USING uf_mode TYPE char1
                                CHANGING cs_clearing TYPE zsdsfis077
                                         ct_items    TYPE tt_output ##NEEDED.

  DATA:
    lv_msgid  TYPE sy-msgid,
    lv_msgno  TYPE sy-msgno,
    lv_msgty  TYPE sy-msgty,
    lv_msgv1  TYPE sy-msgv1,
    lv_msgv2  TYPE sy-msgv2,
    lv_msgv3  TYPE sy-msgv3,
    lv_msgv4  TYPE sy-msgv4,
    lv_subrc  TYPE sy-subrc,

    lt_blntab TYPE STANDARD TABLE OF blntab,
    lt_clear  TYPE STANDARD TABLE OF ftclear,
    lt_post   TYPE STANDARD TABLE OF ftpost,
    lt_tax    TYPE STANDARD TABLE OF fttax.

  PERFORM f_prepare_header USING cs_clearing
                        CHANGING lt_post.

*  PERFORM f_prepare_ftpost USING cs_clearing
*                        CHANGING lt_post.

  LOOP AT ct_items INTO DATA(ls_item)
    WHERE kunnr = cs_clearing-kunnr
    AND   zuonr = cs_clearing-zuonr
    AND   belnr_opn IS NOT INITIAL.

    PERFORM f_prepare_ftclear USING ls_item
                              CHANGING lt_clear.

  ENDLOOP.


  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = 'UMBUCHNG'
      i_tcode                    = 'FB05'
*     I_SGFUNCT                  = ' '
*     I_NO_AUTH                  = ' '
      i_xsimu                    = uf_mode
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
    ELSE.
      MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
        WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO cs_clearing-status_msg.

      cs_clearing-status_icon = gc_status-error.
    ENDIF.
  ELSE.
    IF uf_mode = gc_mode-test_run.
      cs_clearing-status_msg  = TEXT-i01.
      cs_clearing-status_icon = gc_status-warning.
    ELSE.
      cs_clearing-status_msg  = TEXT-i02.
      cs_clearing-status_icon = gc_status-success.
      READ TABLE lt_blntab INTO DATA(ls_bln) INDEX 1.
      IF sy-subrc EQ 0.
        cs_clearing-belnr = ls_bln-belnr.
        cs_clearing-gjahr = ls_bln-gjahr.
*
*        PERFORM f_update_assignment USING p_bukrs cs_clearing-belnr cs_clearing-gjahr
*                                          cs_clearing-zuonr
*                                    CHANGING cs_clearing-status_msg.
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
FORM f_prepare_header  USING    uf_docu TYPE zsdsfis077
                       CHANGING ct_post TYPE feb_t_ftpost.

  ct_post = VALUE feb_t_ftpost( BASE ct_post (
                    stype = gc_stype-header
                    count = 1:
                    fnam = 'BKPF-BLDAT'
                    fval = |{ p_bldat DATE = USER }| ) ),
                    fnam = 'BKPF-BUDAT'
                    fval = |{ p_budat DATE = USER }| ) ),
                    fnam = 'BKPF-BLART'
                    fval = p_blart ) ),
                    fnam = 'BKPF-BUKRS'
                    fval = p_bukrs ) ),
                    fnam = 'BKPF-BKTXT'
                    fval = uf_docu-bktxt ) ),
*                    fnam = 'BKPF-XBLNR'
*                    fval = uf_docu-xblnr ) ),
                    fnam = 'BKPF-WAERS'
                    fval = uf_docu-waers ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_ftclear
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_CLEARING
*&      <-- LT_CLEAR
*&---------------------------------------------------------------------*
FORM f_prepare_ftclear  USING    uf_clearing TYPE zsdsfis077
                        CHANGING ct_clear TYPE feb_t_ftclear.

  DATA: ls_ftclear TYPE ftclear.
  CONSTANTS: lc_agkoa TYPE koart VALUE 'D', "Customers
             lc_selfd TYPE fld30_f05a VALUE 'BELNR'.

  ls_ftclear-agkoa = lc_agkoa.
  ls_ftclear-agkon = uf_clearing-kunnr. "Customer
  ls_ftclear-agbuk = p_bukrs.           "Company code
  "G/L Indicator
  if uf_clearing-umskz IS NOT INITIAL.
    ls_ftclear-xnops = ''.
  ELSE.
    ls_ftclear-xnops = gc_true.
  ENDIF.
  ls_ftclear-selfd = lc_selfd.
  "Special G/L
  ls_ftclear-agums = uf_clearing-umskz.
  CONCATENATE uf_clearing-belnr_opn
              uf_clearing-gjahr_opn
              uf_clearing-buzei_opn INTO ls_ftclear-selvon.
  ls_ftclear-selbis = uf_clearing-belnr_opn.
*
  APPEND ls_ftclear TO ct_clear.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form f_prepare_ftpost
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> CS_CLEARING
**&      <-- LT_POST
**&---------------------------------------------------------------------*
*FORM f_prepare_ftpost  USING    uf_docu TYPE zsdsfis077
*                       CHANGING ct_post TYPE feb_t_ftpost.
*
*
*
*ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_user_command
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_ucomm
*&---------------------------------------------------------------------*
FORM f_user_command USING uf_ucomm  TYPE  sy-ucomm ##CALLED.
  DATA:
    lv_valid     TYPE char01                                  ##NEEDED,
    lv_refresh   TYPE char01                                  ##NEEDED,

    ls_group     TYPE gty_group,
    lt_group     TYPE tt_group,
    ls_node_data TYPE zsdsfis077,
    lt_sel_nodes TYPE lvc_t_nkey                              ##NEEDED.

  CASE uf_ucomm.
    WHEN 'POST'.

      gref_tree->get_checked_items(
        IMPORTING
          et_checked_items = DATA(lt_checked_items)                 " Item table
      ).

      IF lt_checked_items IS NOT INITIAL.

        LOOP AT lt_checked_items INTO DATA(ls_checked_item).
          CLEAR ls_node_data.

          gref_tree->get_outtab_line(
            EXPORTING
              i_node_key     = ls_checked_item-nodekey  " Node Key
            IMPORTING
              e_outtab_line  = ls_node_data             " Line of Outtab
            EXCEPTIONS
              node_not_found = 1                " Node does not exist
              OTHERS         = 2
          ).

          CHECK sy-subrc EQ 0.

          ls_group         = CORRESPONDING #( ls_node_data ).
          ls_group-nodekey = ls_checked_item-nodekey.
          APPEND ls_group TO lt_group.

        ENDLOOP.

        PERFORM f_post_document USING     gc_mode-prod_run
                                CHANGING  lt_group
                                          gt_output.

        LOOP AT lt_group INTO ls_group                        ##INTO_OK.
          CLEAR ls_node_data.

          ls_node_data = CORRESPONDING #( ls_group ).
          ls_node_data-sel = ''.

          gref_tree->change_node(
            EXPORTING
              i_node_key     = ls_group-nodekey " Key of Changed Line
              i_outtab_line  = ls_node_data      " Outtab Line to be Changed
            EXCEPTIONS
              node_not_found = 1                " Node does not exist
              OTHERS         = 2
          ).

          CHECK sy-subrc EQ 0.
        ENDLOOP.

        gref_tree->frontend_update( ).

      ELSE.
        MESSAGE s998(zsdsfi01) WITH TEXT-e01.
      ENDIF.

    WHEN 'PRN_TAX'.
      CALL TRANSACTION 'ZSDSFI012'.                      "#EC CI_CALLTA
    WHEN 'PRN_RCPT'.
      CALL TRANSACTION 'ZSDSFI017'.                      "#EC CI_CALLTA

  ENDCASE.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form f_update_assignment
**&---------------------------------------------------------------------*
*FORM f_update_assignment  USING uf_bukrs TYPE bukrs
*                                uf_belnr TYPE belnr_d
*                                uf_gjahr TYPE gjahr
*                                uf_zuonr TYPE dzuonr
*                        CHANGING cs_msg  TYPE bapi_msg.
*
*  DATA: lt_accchg TYPE STANDARD TABLE OF accchg,
*        lt_enq    TYPE STANDARD TABLE OF seqg3.
*
*  DATA: ls_accchg TYPE accchg.
*  DATA: lf_garg TYPE eqegraarg.
*
*  DO 3 TIMES.
*    SELECT SINGLE bukrs ##WARN_OK,
*                  belnr,
*                  gjahr,
*                  buzei
*      FROM bsid_view
*      WHERE bukrs = @uf_bukrs
*        AND belnr = @uf_belnr
*        AND gjahr = @uf_gjahr
*      INTO @DATA(ls_bsid).
*    IF sy-subrc = 0.
*      ls_accchg-fdname = 'ZUONR'.
*      ls_accchg-newval = uf_zuonr.
*      APPEND ls_accchg TO lt_accchg.
*
*      ls_accchg-fdname = 'XBLNR'.
*      ls_accchg-newval = uf_belnr.
*      APPEND ls_accchg TO lt_accchg.
*
**       Check if there is a lock on the document before proceeding
*      CALL FUNCTION 'ENQUEUE_READ'
*        EXPORTING
*          gclient               = sy-mandt
*          gname                 = 'BKPF'
*          garg                  = lf_garg
*        TABLES
*          enq                   = lt_enq
*        EXCEPTIONS
*          communication_failure = 1
*          system_failure        = 2
*          OTHERS                = 3.
*
*      IF sy-subrc = 0 AND lt_enq IS INITIAL.
** Call THE FUNCTION MODULE TO UPDATE THE FI DOCUMENT
*        CALL FUNCTION 'FI_DOCUMENT_CHANGE'
*          EXPORTING
*            i_bukrs      = ls_bsid-bukrs
*            i_belnr      = ls_bsid-belnr
*            i_gjahr      = ls_bsid-gjahr
*            i_buzei      = ls_bsid-buzei
*          TABLES
*            t_accchg     = lt_accchg
*          EXCEPTIONS
*            no_reference = 1
*            no_document  = 2
*            OTHERS       = 3.
*        IF sy-subrc <> 0.
*          cs_msg = |{ cs_msg }/Can not update assignment| ##NO_TEXT.
*        ENDIF.
*      ELSE.
*        cs_msg = |{ cs_msg }/Can not update assignment| ##NO_TEXT.
*      ENDIF.
*
*      EXIT.
*    ELSE.
*      WAIT UP TO 10 SECONDS.
*    ENDIF.
*  ENDDO.
*ENDFORM.
