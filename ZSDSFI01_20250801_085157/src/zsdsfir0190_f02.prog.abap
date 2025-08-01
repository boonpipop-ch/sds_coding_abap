*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0190_F02
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
***INCLUDE ZETX003_F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

  DATA: lt_slis_fcat  TYPE lvc_t_fcat.

  DATA: lwa_layout    TYPE lvc_s_layo,
        lwa_variant   TYPE disvariant.

  DATA: lv_grid_title TYPE lvc_title.

  IF gt_head IS INITIAL.
    MESSAGE s000(38) WITH TEXT-m01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  lv_grid_title = sy-title.

  PERFORM : set_layout                 CHANGING lwa_layout,
            set_variant                CHANGING lwa_variant,

            lvc_fieldcatalog_merge     USING    'ZSE_ZETX003_OUTPUT' "gc_doc_header CHG CH01
                                                'GT_HEAD'
                                       CHANGING lt_slis_fcat,

            manage_fcat_head           USING    'GT_HEAD'
                                       CHANGING lt_slis_fcat,

            reuse_alv_grid_display_lvc TABLES   gt_head
                                        USING   'PF_STATUS_HEAD'
                                                'USER_COMMAND'
                                                ''
                                                lv_grid_title
                                                lwa_layout
                                                lwa_variant
                                                lt_slis_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LWA_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout CHANGING pwa_layout TYPE lvc_s_layo.

  pwa_layout-zebra      = abap_on.
  pwa_layout-cwidth_opt = abap_on.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LVC_FIELDCATALOG_MERGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_DOC_HEADER
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM lvc_fieldcatalog_merge  USING pi_structure_etax TYPE dd02l-tabname
                                   pi_tab_name
                          CHANGING pt_slis_fcat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     i_buffer_active        = 'X'
      i_structure_name       = pi_structure_etax
*     i_client_never_display = 'X'
*     i_bypassing_buffer     = 'X'
      i_internal_tabname     = pi_tab_name
    CHANGING
      ct_fieldcat            = pt_slis_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_HEAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM manage_fcat_head     USING pi_tab_name
                       CHANGING cht_slis_fcat TYPE lvc_t_fcat.

  DATA: lwa_fcat           TYPE lvc_s_fcat.
  DATA: lv_col             TYPE i.
  FIELD-SYMBOLS <lfs_fcat> TYPE lvc_s_fcat.

  DESCRIBE TABLE cht_slis_fcat LINES lv_col.

  m_add_fieldcat lwa_fcat cht_slis_fcat: 'CANCEL' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' pi_tab_name,
                                         'SAP_DOC_TYPE_DESC' pi_tab_name,
                                         'LINK_PARTNER' pi_tab_name,
                                         'REPLACE_DOC' pi_tab_name,
                                         'RD_DOC_RESN_DESC' pi_tab_name,
                                         'LINK_REF_DOC' pi_tab_name,
                                         'TAX_CODE' pi_tab_name,
                                         'VAT_RATE' pi_tab_name,
                                         'GROSS_AMT' pi_tab_name,
                                         'RD_VAT_TYPE' pi_tab_name,
                                         'LINK_DISC_CHARGE' pi_tab_name,
                                         'LINK_VAT' pi_tab_name,
                                         'STATUS_DESC' pi_tab_name.

  LOOP AT cht_slis_fcat ASSIGNING <lfs_fcat>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    CASE <lfs_fcat>-fieldname.
      WHEN 'STATUS'.
        %fcat <lfs_fcat> TEXT-a88 '' '' '' '' '' '' 1.
      WHEN 'STATUS_DESC'.
        %fcat <lfs_fcat> TEXT-a89 '' '' '' '' '' '' 2.
      WHEN 'BUKRS'.
        %fcat <lfs_fcat> TEXT-a10 '' '' '' '' '' '' 3.
      WHEN 'COMP_NAME'.
        %fcat <lfs_fcat> TEXT-a11 '' '' '' '' '' '' 2.
      WHEN 'SAP_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a01 '' '' 'X' '' '' '' 3.
      WHEN 'GJAHR'.
        %fcat <lfs_fcat> TEXT-a12 '' '' '' '' '' '' 4.
      WHEN 'RD_DOC_TYPE'.
        %fcat <lfs_fcat> TEXT-a05 '' '' '' '' '' '' 5.
      WHEN 'RD_DOC_TYPE_DESC'.
        %fcat <lfs_fcat> TEXT-a06 '' '' '' '' '' '' 6.
      WHEN 'MODULE_ETX'.
        %fcat <lfs_fcat> TEXT-a87 '' '' '' '' '' '' 7.
      WHEN 'DOCUMENT_NO'.
        %fcat <lfs_fcat> TEXT-a02 '' '' '' '' '' '' 8.
      WHEN 'SAP_POSTING_DATE'.
        %fcat <lfs_fcat> TEXT-a03 '' '' '' '' '' '' 9.
      WHEN 'RD_DOC_GRP'.
        %fcat <lfs_fcat> TEXT-a04 '' '' '' '' '' '' 10.
      WHEN 'SAP_DOC_TYPE'.
        %fcat <lfs_fcat> TEXT-a07 '' '' '' '' '' '' 11.
      WHEN 'SAP_DOC_TYPE_DESC'.
        %fcat <lfs_fcat> TEXT-a08 '' '' '' '' '' '' 12.
      WHEN 'CANCEL'.
        %fcat <lfs_fcat> TEXT-a09 '' '' '' '' '' '' 13.
      WHEN 'BUPLA'.
        %fcat <lfs_fcat> TEXT-a13 '' '' '' '' '' '' 14.
      WHEN 'KUNNR'.
        %fcat <lfs_fcat> TEXT-a14 '' '' '' '' '' '' 15.
      WHEN 'KUNNR_NAME'.
        %fcat <lfs_fcat> TEXT-a15 '' '' '' '' '' '' 16.
      WHEN 'KUNNR_BRANCH'.
        %fcat <lfs_fcat> TEXT-a16 '' '' '' '' '' '' 17.
      WHEN 'KUNNR_TAX_ID'.
        %fcat <lfs_fcat> TEXT-a17 '' '' '' '' '' '' 18.
      WHEN 'LINK_PARTNER'.
        %fcat <lfs_fcat> TEXT-a18 '' '' 'X' '' '' '' 19.
      WHEN 'REPLACE_DOC'.
        %fcat <lfs_fcat> TEXT-a19 '' '' '' '' '' '' 20.
      WHEN 'SAP_DOC_RESN'.
        %fcat <lfs_fcat> TEXT-a20 '' '' '' '' '' '' 21.
      WHEN 'SAP_DOC_RESN_DESC'.
        %fcat <lfs_fcat> TEXT-a21 '' '' '' '' '' '' 22.
      WHEN 'RD_DOC_RESN'.
        %fcat <lfs_fcat> TEXT-a22 '' '' '' '' '' '' 23.
      WHEN 'RD_DOC_RESN_DESC'.
        %fcat <lfs_fcat> TEXT-a23 '' '' '' '' '' '' 24.
      WHEN 'LINK_REF_DOC'.
        %fcat <lfs_fcat> TEXT-a24 '' '' 'X' '' '' '' 25.
      WHEN 'REQ_DEV_DATE'.
        %fcat <lfs_fcat> TEXT-a25 '' '' '' '' '' '' 26.
      WHEN 'PAY_TERM'.
        %fcat <lfs_fcat> TEXT-a26 '' '' '' '' '' '' 27.
      WHEN 'PAY_TERM_DESC'.
        %fcat <lfs_fcat> TEXT-a27 '' '' '' '' '' '' 28.
      WHEN 'PAY_DUE_DATE'.
        %fcat <lfs_fcat> TEXT-a28 '' '' '' '' '' '' 29.
      WHEN 'INCO_TERM'.
        %fcat <lfs_fcat> TEXT-a29 '' '' '' '' '' '' 30.
      WHEN 'SAP_PO_NO'.
        %fcat <lfs_fcat> TEXT-a30 '' '' '' '' '' '' 31.
      WHEN 'SAP_PO_DATE'.
        %fcat <lfs_fcat> TEXT-a31 '' '' '' '' '' '' 32.
      WHEN 'GLOBAL_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a32 '' '' '' '' '' '' 33.
      WHEN 'SAP_CURR'.
        %fcat <lfs_fcat> TEXT-a33 '' '' '' '' '' '' 34.
      WHEN 'RD_CURR_CODE'.
        %fcat <lfs_fcat> TEXT-a34 '' '' '' '' '' '' 35.
      WHEN 'RD_VAT_TYPE'.
        %fcat <lfs_fcat> TEXT-a35 '' '' '' '' '' '' 36.
      WHEN 'TAX_CODE'.
        %fcat <lfs_fcat> TEXT-a36 '' '' '' '' '' '' 37.
      WHEN 'VAT_RATE'.
        %fcat <lfs_fcat> TEXT-a37 '' '' '' '' '' '' 38.
      WHEN 'GROSS_AMT'.
        %fcat <lfs_fcat> TEXT-a38 '' '' '' '' '' '' 39.
      WHEN 'TOTAL_DISC_AMT'.
        %fcat <lfs_fcat> TEXT-a39 '' '' '' '' '' '' 40.
      WHEN 'TOTAL_CHARGE_AMT'.
        %fcat <lfs_fcat> TEXT-a40 '' '' '' '' '' '' 41.
      WHEN 'LINK_DISC_CHARGE'.
        %fcat <lfs_fcat> TEXT-a41 '' '' 'X' '' '' '' 42.
      WHEN 'VAT_BASE_AMT'.
        %fcat <lfs_fcat> TEXT-a42 '' '' '' '' '' '' 43.
      WHEN 'NET_AMT_BF_VAT'.
        %fcat <lfs_fcat> TEXT-a43 '' '' '' '' '' '' 44.
      WHEN 'VAT_AMT'.
        %fcat <lfs_fcat> TEXT-a44 '' '' '' '' '' '' 45.
      WHEN 'NET_AMT_AFT_VAT'.
        %fcat <lfs_fcat> TEXT-a45 '' '' '' '' '' '' 46.
      WHEN 'LINK_VAT'.
        %fcat <lfs_fcat> TEXT-a46 '' '' 'X' '' '' '' 47.
      WHEN 'REF_DOC_AMT'.
        %fcat <lfs_fcat> TEXT-a47 '' '' '' '' '' '' 48.
      WHEN 'CORRECT_AMT'.
        %fcat <lfs_fcat> TEXT-a48 '' '' '' '' '' '' 49.
      WHEN 'DIFF_AMT'.
        %fcat <lfs_fcat> TEXT-a49 '' '' '' '' '' '' 50.
*>>> BEGIN OF INSERTION: <ETAX003> on 16.09.2020 <<<
      WHEN 'BUPLA_INFO'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 51.
      WHEN 'RD_FLAG'.
        %fcat <lfs_fcat> 'RD Send' '' '' '' '' '' '' 52.
      WHEN 'SUBJECT'.
        %fcat <lfs_fcat> 'Subject' '' '' '' '' '' '' 53.
        <lfs_fcat>-no_out = 'X'.
      WHEN 'CONTENT'.
        %fcat <lfs_fcat> 'Content' '' '' '' '' '' '' 54.
        <lfs_fcat>-no_out = 'X'.
      WHEN 'DEPOSIT_FLAG'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 55.
*>>> END OF INSERTION: <ETAX003> on 16.09.2020 <<<
*>>> BOI CH01 >>>
      WHEN 'ETAX_BY'.
        %fcat <lfs_fcat> 'Executed By' '' '' '' '' '' '' 56.
      WHEN 'ETAX_DATE'.
        %fcat <lfs_fcat> 'Executed Date' '' '' '' '' '' '' 57.
      WHEN 'PRINT_USER'.
        %fcat <lfs_fcat> '(lasted) Printed By' '' '' '' '' '' '' 58.
      WHEN 'PRINT_DATE'.
        %fcat <lfs_fcat> '(lasted) Printed Date' '' '' '' '' '' '' 59.
      WHEN 'VAT_REPORT'.
        %fcat <lfs_fcat> 'VAT Report' '' '' '' '' '' '' 60.
*<<< EOI CH01 <<<
      WHEN OTHERS.
        %fcat <lfs_fcat> '' '' '' '' 'X' '' '' lv_col.
        ADD 1 TO lv_col.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REUSE_ALV_GRID_DISPLAY_LVC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_HEAD
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> LV_GRID_TITLE
*&      --> LWA_LAYOUT
*&      --> LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM reuse_alv_grid_display_lvc TABLES pt_outtab TYPE table
                                USING  p_status_set
                                       p_user_command
                                       p_top_of_page
                                       p_grid_title
                                       pwa_layout   TYPE lvc_s_layo
                                       pwa_variant  TYPE disvariant
                                       pt_slis_fcat TYPE lvc_t_fcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program          = sy-repid
      i_callback_pf_status_set    = p_status_set
      i_callback_user_command     = p_user_command
*     i_callback_top_of_page      = 'TOP_OF_PAGE'
      i_callback_html_top_of_page = p_top_of_page "'HTML_TOP_OF_PAGE'
      i_grid_title                = p_grid_title
      is_layout_lvc               = pwa_layout
      it_fieldcat_lvc             = pt_slis_fcat
      i_save                      = 'X'
      is_variant                  = pwa_variant
    TABLES
      t_outtab                    = pt_outtab
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  PF_STATUS_HEAD
*&---------------------------------------------------------------------*
FORM pf_status_head USING pi_command.

  DATA: lt_exclude TYPE TABLE OF sy-ucomm.

  APPEND: '&EB9' TO lt_exclude,
          '&REFRESH' TO lt_exclude,
          '&RNT_PREV' TO lt_exclude,
          '&AQW' TO lt_exclude,
          '%SL' TO lt_exclude,
          '&ABC' TO lt_exclude,
          '&GRAPH' TO lt_exclude,
          '&INFO' TO lt_exclude.

  SET PF-STATUS 'PF_STATUS_HEAD' EXCLUDING lt_exclude.

ENDFORM.                    " user_command_set
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING pi_ucomm      TYPE sy-ucomm
                        pwa_selfield TYPE slis_selfield.

  DATA: lref_grid TYPE REF TO cl_gui_alv_grid,
        lwa_head  TYPE gty_head.

  IF lref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = lref_grid.
  ENDIF.

  IF lref_grid IS NOT INITIAL.
    CALL METHOD lref_grid->check_changed_data .
  ENDIF.

  CASE pi_ucomm.

    WHEN '&IC1'.

      READ TABLE gt_head INTO lwa_head INDEX pwa_selfield-tabindex.
      IF sy-subrc = 0.
        IF pwa_selfield-sel_tab_field EQ 'GT_HEAD-SAP_DOC_NO'.
          PERFORM display_item USING lwa_head ''.
        ELSEIF pwa_selfield-sel_tab_field EQ 'GT_HEAD-LINK_PARTNER'.
          PERFORM display_partner USING lwa_head ''.
        ELSEIF pwa_selfield-sel_tab_field EQ 'GT_HEAD-LINK_REF_DOC'.
          PERFORM display_ref USING lwa_head ''.
        ELSEIF pwa_selfield-sel_tab_field EQ 'GT_HEAD-LINK_VAT'.
          PERFORM display_vat USING lwa_head ''.
        ELSEIF pwa_selfield-sel_tab_field EQ 'GT_HEAD-LINK_DISC_CHARGE'.
          PERFORM display_charge USING lwa_head 'X'.
        ENDIF.
      ENDIF.

    WHEN '&ZPAR'.
      PERFORM display_partner USING lwa_head 'X'.
    WHEN '&ZITM'.
      PERFORM display_item USING lwa_head 'X'.
    WHEN '&ZREF'.
      PERFORM display_ref USING lwa_head 'X'.
    WHEN '&ZVAT'.
      PERFORM display_vat USING lwa_head 'X'.
    WHEN '&F03' OR '&F15' OR '&F12'.
      LEAVE SCREEN.

  ENDCASE.

*--- Refresh result list
  pwa_selfield-refresh    = abap_on.
  pwa_selfield-col_stable = abap_on.
  pwa_selfield-row_stable = abap_on.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_ITEM
*&---------------------------------------------------------------------*
FORM user_command_item USING pi_ucomm      TYPE sy-ucomm
                             pwa_selfield TYPE slis_selfield.

  DATA: lwa_head TYPE gty_head.

  CASE pi_ucomm.
    WHEN '&IC1'.
      IF pwa_selfield-sel_tab_field EQ 'LT_ITEM-LINK_DISC_CHARGE'.
        PERFORM display_charge USING lwa_head 'X'.
      ENDIF.
  ENDCASE.

*--- Refresh result list
  pwa_selfield-refresh    = abap_on.
  pwa_selfield-col_stable = abap_on.
  pwa_selfield-row_stable = abap_on.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HTML_TOP_OF_PAGE_PARTNER
*&---------------------------------------------------------------------*
FORM html_top_of_page_partner USING top TYPE REF TO cl_dd_document.

  DATA: lo_table   TYPE REF TO cl_dd_table_area.

  DATA: lv_text1(255) TYPE c,
        lv_text2(255) TYPE c.

  CALL METHOD top->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '300'
    IMPORTING
      tablearea     = lo_table.

*  lv_text1 = TEXT-h09.
*  lv_text2 = p_bukrs.
*  PERFORM set_top    USING lv_text1 lv_text2
*                  CHANGING lo_table.
*
*  lv_text1 = TEXT-h10.
*  lv_text2 = p_gjahr.
*  PERFORM set_top    USING lv_text1 lv_text2
*                  CHANGING lo_table.

  lv_text1 = TEXT-a01.
  lv_text2 = gv_sap_doc_no.
  PERFORM set_top    USING lv_text1 lv_text2
                  CHANGING lo_table.

  lv_text1 = TEXT-a05.
  lv_text2 = gv_rd_doc_type.
  PERFORM set_top    USING lv_text1 lv_text2
                  CHANGING lo_table.

  lv_text1 = TEXT-a14.
  lv_text2 = gv_kunnr .
  PERFORM set_top    USING lv_text1 lv_text2
                  CHANGING lo_table.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM display_item  USING    pi_head TYPE gty_head
                            pi_list TYPE abap_bool.

  DATA: lt_item       TYPE gtty_doc_item,
        lt_slis_fcat  TYPE lvc_t_fcat.

  DATA: lwa_layout    TYPE lvc_s_layo,
        lwa_variant   TYPE disvariant,
        lwa_head      TYPE gty_head,
        lwa_item      TYPE gty_doc_item.

  DATA: lv_grid_title TYPE lvc_title.

  REFRESH gt_charge[].
  gt_charge[] = pi_head-it_charge[].

  CLEAR: gv_sap_doc_no,
         gv_rd_doc_type,
         gv_kunnr,
         gv_dis_detail.

  IF pi_list IS INITIAL.    "Link from cell
    lt_item[] = pi_head-it_item[].

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  pi_head-sap_doc_no
                         CHANGING gv_sap_doc_no.

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  pi_head-kunnr
                         CHANGING gv_kunnr.

    gv_rd_doc_type = pi_head-rd_doc_type.
    gv_dis_detail  = 'X'.

  ELSE.                     "Press List Of Items button
    gv_dis_detail  = ''.

    LOOP AT gt_head INTO lwa_head.
      CLEAR lwa_item.
      LOOP AT lwa_head-it_item INTO lwa_item.
        APPEND lwa_item TO lt_item.
      ENDLOOP.
    ENDLOOP.

    gv_sap_doc_no  = '-'.
    gv_rd_doc_type = '-'.
    gv_kunnr       = '-'.

  ENDIF.

  CHECK lt_item[] IS NOT INITIAL.

  SORT lt_item BY bukrs sap_doc_no gjahr item_no ASCENDING.

  CONCATENATE sy-title ':' TEXT-h06 INTO lv_grid_title SEPARATED BY space.

  PERFORM : set_layout               CHANGING lwa_layout,

            lvc_fieldcatalog_merge      USING gc_doc_item
                                              'LT_ITEM'
                                     CHANGING lt_slis_fcat,

            manage_fcat_item             USING 'LT_ITEM'
                                     CHANGING lt_slis_fcat,

            reuse_alv_grid_display_lvc TABLES lt_item
                                        USING '' 'USER_COMMAND_ITEM'
                                              'HTML_TOP_OF_PAGE_PARTNER'
                                              lv_grid_title
                                              lwa_layout
                                              lwa_variant
                                              lt_slis_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERSION_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_ALPHA
*&      --> P_
*&      --> PI_HEAD_SAP_DOC_NO
*&      <-- GV_SAP_DOC_NO
*&---------------------------------------------------------------------*
FORM conversion_exit USING VALUE(p_exit)
                           VALUE(p_type)
                           VALUE(p_input)
                     CHANGING p_output.
  DATA: l_conexit TYPE char30.

  CONCATENATE 'CONVERSION_EXIT_' p_exit '_' p_type INTO l_conexit.

  CALL FUNCTION l_conexit
    EXPORTING
      input         = p_input
    IMPORTING
      output        = p_output
    EXCEPTIONS
      error_message = 1
      OTHERS        = 2.

ENDFORM.                    "conversion_exit
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM manage_fcat_item  USING pi_tab_name
                       CHANGING cht_slis_fcat TYPE lvc_t_fcat.

  DATA: lwa_fcat           TYPE lvc_s_fcat.
  DATA: lv_col  TYPE i,
        lv_tech TYPE flag.
  FIELD-SYMBOLS <lfs_fcat> TYPE lvc_s_fcat.

  DESCRIBE TABLE cht_slis_fcat LINES lv_col.

  m_add_fieldcat lwa_fcat cht_slis_fcat: 'DOCUMENT_NO' pi_tab_name,
*                                         'RD_DOC_TYPE' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' pi_tab_name,
                                         'KUNNR' pi_tab_name,
                                         'KUNNR_NAME' pi_tab_name,
                                         'RD_UNIT' pi_tab_name,
                                         'LINK_DISC_CHARGE' pi_tab_name.

  IF gv_dis_detail IS INITIAL.
    lv_tech = 'X'.
  ENDIF.

  LOOP AT cht_slis_fcat ASSIGNING <lfs_fcat>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <lfs_fcat>-key = ''.
    CASE <lfs_fcat>-fieldname.
      WHEN 'BUKRS'.
        %fcat <lfs_fcat> TEXT-a10 '' '' '' '' '' '' 1.
      WHEN 'SAP_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a01 '' '' '' '' '' '' 2.
*      WHEN 'COMP_NAME'.
*        %fcat <lfs_fcat> TEXT-a11 '' '' '' '' '' '' 2.
      WHEN 'GJAHR'.
        %fcat <lfs_fcat> TEXT-a12 '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE'.
        %fcat <lfs_fcat> TEXT-a05 '' '' '' '' '' '' 4.
      WHEN 'RD_DOC_TYPE_DESC'.
        %fcat <lfs_fcat> TEXT-a06 '' '' '' '' '' '' 5.
      WHEN 'MODULE_ETX'.
        %fcat <lfs_fcat> TEXT-a87 '' '' '' '' '' '' 6.
      WHEN 'DOCUMENT_NO'.
        %fcat <lfs_fcat> TEXT-a02 '' '' '' '' '' gc_alpha 7.
      WHEN 'KUNNR'.
        %fcat <lfs_fcat> TEXT-a14 '' '' '' '' '' gc_alpha 8.
      WHEN 'KUNNR_NAME'.
        %fcat <lfs_fcat> TEXT-a15 '' '' '' '' '' '' 9.
      WHEN 'ITEM_NO'.
        %fcat <lfs_fcat> TEXT-a50 '' '' '' '' '' '' 10.
      WHEN 'ITEM_CODE'.
        %fcat <lfs_fcat> TEXT-a51 '' '' '' '' '' '' 11.
      WHEN 'ITEM_NAME'.
        %fcat <lfs_fcat> TEXT-a52 '' '' '' '' '' '' 12.
      WHEN 'ITEM_DESC'.
        %fcat <lfs_fcat> TEXT-a53 '' '' '' '' '' '' 13.
      WHEN 'BATCH'.
        %fcat <lfs_fcat> TEXT-a54 '' '' '' '' '' '' 14.
      WHEN 'EXP_DATE'.
        %fcat <lfs_fcat> TEXT-a55 '' '' '' '' '' '' 15.
      WHEN 'MAT_GRP'.
        %fcat <lfs_fcat> TEXT-a56 '' '' '' '' '' '' 16.
      WHEN 'MAT_GRP_NAME'.
        %fcat <lfs_fcat> TEXT-a57 '' '' '' '' '' '' 17.
      WHEN 'UNIT_PRICE'.
        %fcat <lfs_fcat> TEXT-a58 '' '' '' '' '' '' 18.
      WHEN 'QTY'.
        %fcat <lfs_fcat> TEXT-a59 '' '' '' '' '' '' 19.
      WHEN 'UNIT'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 20.
      WHEN 'RD_UNIT'.
        %fcat <lfs_fcat> TEXT-a60 '' '' '' '' '' '' 21.
      WHEN 'RD_VAT_TYPE'.
        %fcat <lfs_fcat> TEXT-a61 '' '' '' '' '' '' 22.
      WHEN 'TAX_CODE'.
        %fcat <lfs_fcat> TEXT-a62 '' '' '' '' '' '' 23.
      WHEN 'VAT_RATE'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 24.
      WHEN 'GROSS_AMT'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 25.
      WHEN 'RD_DISC_AMT'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 26.
      WHEN 'CHARGE_AMT'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 27.
      WHEN 'LINK_DISC_CHARGE'.
        %fcat <lfs_fcat> TEXT-a41 '' '' gv_dis_detail lv_tech '' '' 28.
      WHEN 'VAT_BASE_AMT'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 29.
      WHEN 'NET_AMT_BF_VAT'.
        %fcat <lfs_fcat> TEXT-a63 '' '' '' '' '' '' 30.
      WHEN 'VAT_AMT'.
        %fcat <lfs_fcat> TEXT-a64 '' '' '' '' '' '' 31.
      WHEN 'NET_AMT_AFT_VAT'.
        %fcat <lfs_fcat> TEXT-a65 '' '' '' '' '' '' 32.
      WHEN 'SAP_PO_NO' .
        %fcat <lfs_fcat> 'PO number' '' '' '' '' '' '' 33.
      WHEN 'SAP_PO_ITEM' .
        %fcat <lfs_fcat> 'PO item' '' '' '' '' '' '' 34.
      WHEN OTHERS.
        %fcat <lfs_fcat> '' '' '' '' 'X' '' '' lv_col.
        ADD 1 TO lv_col.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_TEXT1
*&      --> LV_TEXT2
*&      <-- LO_TABLE
*&---------------------------------------------------------------------*
FORM set_top  USING    pi_text1
                       pi_text2
              CHANGING po_table TYPE REF TO cl_dd_table_area.

  "first column
  CALL METHOD po_table->add_text
    EXPORTING
      text = pi_text1.

  "second column
  CALL METHOD po_table->add_text
    EXPORTING
      text = pi_text2.

  "add this new filled row
  CALL METHOD po_table->new_row.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_CHARGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM display_charge  USING  pi_head TYPE gty_head
                            pi_item_flag.

  DATA: lt_charge     TYPE gtty_doc_dischg,
        lt_slis_fcat  TYPE lvc_t_fcat,
        lwa_layout    TYPE lvc_s_layo,
        lwa_variant   TYPE disvariant,
        lwa_charge    TYPE gty_doc_dischg,
        lv_grid_title TYPE lvc_title,
        lv_tab_name   TYPE dd02l-tabname.

  CLEAR: gv_sap_doc_no,
         gv_rd_doc_type,
         gv_kunnr.

  IF pi_item_flag = 'X'.
*    lt_charge[] = gt_charge[].
    lt_charge[] = pi_head-it_charge[].
    lv_tab_name = 'LT_CHARGE'.

    READ TABLE lt_charge INTO lwa_charge INDEX 1.

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  lwa_charge-sap_doc_no
                         CHANGING gv_sap_doc_no.

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  lwa_charge-kunnr
                         CHANGING gv_kunnr.

    gv_rd_doc_type = lwa_charge-rd_doc_type.

  ELSE.
    lt_charge[] = pi_head-it_charge[].
    lv_tab_name = 'LT_CHARGE'.

    gv_sap_doc_no  = '-'.
    gv_rd_doc_type = '-'.
    gv_kunnr       = '-'.
  ENDIF.

  CHECK lt_charge[] IS NOT INITIAL.

  SORT lt_charge BY bukrs sap_doc_no gjahr rd_doc_type module_etx
                    item_no seq_no ASCENDING.

  CONCATENATE sy-title ':' TEXT-h07 INTO lv_grid_title SEPARATED BY space.

  PERFORM : set_layout               CHANGING lwa_layout,

            lvc_fieldcatalog_merge      USING gc_doc_dischg
                                              lv_tab_name
                                     CHANGING lt_slis_fcat,

            manage_fcat_charge          USING lv_tab_name
                                     CHANGING lt_slis_fcat,

            reuse_alv_grid_display_lvc TABLES lt_charge
                                        USING '' '' 'HTML_TOP_OF_PAGE_PARTNER'
                                              lv_grid_title
                                              lwa_layout
                                              lwa_variant
                                              lt_slis_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_CHARGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_TAB_NAME
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM manage_fcat_charge    USING pi_tab_name
                        CHANGING cht_slis_fcat TYPE lvc_t_fcat.

  DATA: lwa_fcat           TYPE lvc_s_fcat.
  DATA: lv_col             TYPE i.
  FIELD-SYMBOLS <lfs_fcat> TYPE lvc_s_fcat.

  DESCRIBE TABLE cht_slis_fcat LINES lv_col.

  m_add_fieldcat lwa_fcat cht_slis_fcat: 'DOCUMENT_NO' pi_tab_name,
*                                         'RD_DOC_TYPE' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' pi_tab_name,
                                         'KUNNR' pi_tab_name,
                                         'KUNNR_NAME' pi_tab_name,
                                         'SAP_CURR' pi_tab_name,
                                         'RD_DISCHG' pi_tab_name,
                                         'RD_DISCHG_DESC' pi_tab_name.

  LOOP AT cht_slis_fcat ASSIGNING <lfs_fcat>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <lfs_fcat>-key = ''.
    CASE <lfs_fcat>-fieldname.
      WHEN 'BUKRS'.
        %fcat <lfs_fcat> TEXT-a10 '' '' '' '' '' '' 1.
      WHEN 'SAP_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a01 '' '' '' '' '' '' 2.
      WHEN 'GJAHR'.
        %fcat <lfs_fcat> TEXT-a12 '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE'.
        %fcat <lfs_fcat> TEXT-a05 '' '' '' '' '' '' 4.
      WHEN 'RD_DOC_TYPE_DESC'.
        %fcat <lfs_fcat> TEXT-a06 '' '' '' '' '' '' 5.
      WHEN 'MODULE_ETX'.
        %fcat <lfs_fcat> TEXT-a87 '' '' '' '' '' '' 6.
      WHEN 'DOCUMENT_NO'.
        %fcat <lfs_fcat> TEXT-a02 '' '' '' '' '' gc_alpha 7.
      WHEN 'KUNNR'.
        %fcat <lfs_fcat> TEXT-a14 '' '' '' '' '' gc_alpha 8.
      WHEN 'KUNNR_NAME'.
        %fcat <lfs_fcat> TEXT-a15 '' '' '' '' '' '' 9.
      WHEN 'RD_CHARGE_FLAG'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 10.
      WHEN 'CHARGE_AMT'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 11.
      WHEN 'SAP_CURR'.
        %fcat <lfs_fcat> TEXT-a66 '' '' '' '' '' '' 12.
      WHEN 'RD_CHARGE_CODE'.
        %fcat <lfs_fcat> TEXT-a67 '' '' '' '' '' '' 13.
      WHEN 'CHARGE_RESN'.
        %fcat <lfs_fcat> TEXT-a68 '' '' '' '' '' '' 14.
      WHEN OTHERS.
        %fcat <lfs_fcat> '' '' '' '' 'X' '' '' lv_col.
        ADD 1 TO lv_col.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM display_partner  USING    pi_head TYPE gty_head
                               pi_list TYPE abap_bool.

  DATA: lt_partner    TYPE gtty_doc_partner,
        lt_slis_fcat  TYPE lvc_t_fcat,
        lwa_layout    TYPE lvc_s_layo,
        lwa_variant   TYPE disvariant,
        lwa_head      TYPE gty_head,
        lwa_partner   TYPE gty_doc_partner,
        lv_grid_title TYPE lvc_title.

  CLEAR: gv_sap_doc_no,
         gv_rd_doc_type,
         gv_kunnr.

  IF pi_list IS INITIAL.        "Link from cell
    lt_partner[] = pi_head-it_partner[].

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  pi_head-sap_doc_no
                         CHANGING gv_sap_doc_no.

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  pi_head-kunnr
                         CHANGING gv_kunnr.

    gv_rd_doc_type = pi_head-rd_doc_type.

  ELSE.                         "Press List Of Partners button

    LOOP AT gt_head INTO lwa_head.
      CLEAR lwa_partner.
      LOOP AT lwa_head-it_partner INTO lwa_partner.
        APPEND lwa_partner TO lt_partner.
      ENDLOOP.
    ENDLOOP.

    gv_sap_doc_no  = '-'.
    gv_rd_doc_type = '-'.
    gv_kunnr       = '-'.

  ENDIF.

  CHECK lt_partner[] IS NOT INITIAL.

  SORT lt_partner BY bukrs sap_doc_no gjahr rd_partner ASCENDING.

  CONCATENATE sy-title ':' TEXT-h03 INTO lv_grid_title SEPARATED BY space.

  PERFORM : set_layout               CHANGING lwa_layout,

            lvc_fieldcatalog_merge      USING gc_doc_partner
                                              'LT_PARTNER'
                                     CHANGING lt_slis_fcat,

            manage_fcat_partner         USING 'LT_PARTNER'
                                     CHANGING lt_slis_fcat,

            reuse_alv_grid_display_lvc TABLES lt_partner
                                        USING '' '' 'HTML_TOP_OF_PAGE_PARTNER'
                                              lv_grid_title
                                              lwa_layout
                                              lwa_variant
                                              lt_slis_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM manage_fcat_partner  USING pi_tab_name
                       CHANGING cht_slis_fcat TYPE lvc_t_fcat.

  DATA: lwa_fcat           TYPE lvc_s_fcat.
  DATA: lv_col             TYPE i.
  FIELD-SYMBOLS <lfs_fcat> TYPE lvc_s_fcat.

  DESCRIBE TABLE cht_slis_fcat LINES lv_col.

  m_add_fieldcat lwa_fcat cht_slis_fcat: 'DOCUMENT_NO' pi_tab_name,
*                                         'RD_DOC_TYPE' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' pi_tab_name,
                                         'RD_PARTNER_DESC' pi_tab_name.

  LOOP AT cht_slis_fcat ASSIGNING <lfs_fcat>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.

    <lfs_fcat>-key = ''.
    CASE <lfs_fcat>-fieldname.
      WHEN 'BUKRS'.
        %fcat <lfs_fcat> TEXT-a10 '' '' '' '' '' '' 1.
      WHEN 'SAP_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a01 '' '' '' '' '' '' 2.
      WHEN 'GJAHR'.
        %fcat <lfs_fcat> TEXT-a12 '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE'.
        %fcat <lfs_fcat> TEXT-a05  '' '' '' '' '' '' 4.
      WHEN 'RD_DOC_TYPE_DESC'.
        %fcat <lfs_fcat> TEXT-a06  '' '' '' '' '' '' 5.
      WHEN 'MODULE_ETX'.
        %fcat <lfs_fcat> TEXT-a87 '' '' '' '' '' '' 6.
      WHEN 'DOCUMENT_NO'.
        %fcat <lfs_fcat> TEXT-a02 '' '' '' '' '' gc_alpha 7.
      WHEN 'RD_PARTNER'.
        %fcat <lfs_fcat> TEXT-a69  '' '' '' '' '' '' 8.
      WHEN 'RD_PARTNER_DESC'.
        %fcat <lfs_fcat> TEXT-a70  '' '' '' '' '' '' 9.
      WHEN 'KUNNR'.
        %fcat <lfs_fcat> TEXT-a71  '' '' '' '' '' '' 10.
      WHEN 'PARTNER_NAME'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 11.
      WHEN 'PARTNER_SCH_ID'.
        %fcat <lfs_fcat> TEXT-a72  '' '' '' '' '' '' 12.
      WHEN 'GLOBAL_ID'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 13.
      WHEN 'TAX_ID'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 14.
      WHEN 'CONTACT_NAME'.
        %fcat <lfs_fcat> TEXT-a73  '' '' '' '' '' '' 15.
      WHEN 'CONTACT_DEPT'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 16.
      WHEN 'POSTAL'.
        %fcat <lfs_fcat> TEXT-a74  '' '' '' '' '' '' 17.
      WHEN 'HOME_NO'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 18.
      WHEN 'ADDR_LINE1'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 19.
      WHEN 'ADDR_LINE2'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 20.
      WHEN 'SUB_DIST'.
        %fcat <lfs_fcat> TEXT-a75  '' '' '' '' '' '' 21.
      WHEN 'SUB_DIST_DESC'.
        %fcat <lfs_fcat> TEXT-a76  '' '' '' '' '' '' 22.
      WHEN 'DISTRICT'.
        %fcat <lfs_fcat> TEXT-a77  '' '' '' '' '' '' 23.
      WHEN 'DISTRICT_DESC'.
        %fcat <lfs_fcat> TEXT-a78  '' '' '' '' '' '' 24.
      WHEN 'PROVINCE_CODE'.
        %fcat <lfs_fcat> TEXT-a79  '' '' '' '' '' '' 25.
      WHEN 'PROVINCE_DESC'.
        %fcat <lfs_fcat> TEXT-a80  '' '' '' '' '' '' 26.
      WHEN 'COUNTRY'.
        %fcat <lfs_fcat> TEXT-a81  '' '' '' '' '' '' 27.
      WHEN 'COUNTRY_SCH_ID'.
        %fcat <lfs_fcat> TEXT-a82  '' '' '' '' '' '' 28.
*      WHEN 'ADDR_NO'.
*        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 26.

      WHEN OTHERS.
        %fcat <lfs_fcat> '' '' '' '' 'X' '' '' lv_col.
        ADD 1 TO lv_col.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM display_ref  USING        pi_head TYPE gty_head
                               pi_list TYPE abap_bool.

  DATA: lt_ref        TYPE gtty_doc_h_ref,
        lt_slis_fcat  TYPE lvc_t_fcat.

  DATA: lwa_layout    TYPE lvc_s_layo,
        lwa_variant   TYPE disvariant,
        lwa_head      TYPE gty_head,
        lwa_ref       TYPE gty_doc_h_ref.

  DATA: lv_grid_title TYPE lvc_title.

  CLEAR: gv_sap_doc_no,
         gv_rd_doc_type,
         gv_kunnr.

  IF pi_list IS INITIAL.          "Link from cell
    lt_ref[] = pi_head-it_h_ref[].

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  pi_head-sap_doc_no
                         CHANGING gv_sap_doc_no.

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  pi_head-kunnr
                         CHANGING gv_kunnr.

    gv_rd_doc_type = pi_head-rd_doc_type.

  ELSE.                         "Press List Of Ref. button

    LOOP AT gt_head INTO lwa_head.
      CLEAR lwa_ref.
      LOOP AT lwa_head-it_h_ref INTO lwa_ref.
        APPEND lwa_ref TO lt_ref.
      ENDLOOP.
    ENDLOOP.

    gv_sap_doc_no  = '-'.
    gv_rd_doc_type = '-'.
    gv_kunnr       = '-'.

  ENDIF.

  CHECK lt_ref[] IS NOT INITIAL.

  CONCATENATE sy-title ':' TEXT-h05 INTO lv_grid_title SEPARATED BY space.

  PERFORM : set_layout               CHANGING lwa_layout,

            lvc_fieldcatalog_merge      USING gc_doc_h_ref
                                              'LT_REF'
                                     CHANGING lt_slis_fcat,

            manage_fcat_ref             USING 'LT_REF'
                                     CHANGING lt_slis_fcat,

            reuse_alv_grid_display_lvc TABLES lt_ref
                                        USING '' '' 'HTML_TOP_OF_PAGE_PARTNER'
                                              lv_grid_title
                                              lwa_layout
                                              lwa_variant
                                              lt_slis_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM manage_fcat_ref      USING pi_tab_name
                       CHANGING cht_slis_fcat TYPE lvc_t_fcat.

  DATA: lwa_fcat           TYPE lvc_s_fcat.
  DATA: lv_col             TYPE i.
  FIELD-SYMBOLS <lfs_fcat> TYPE lvc_s_fcat.

  DESCRIBE TABLE cht_slis_fcat LINES lv_col.

  m_add_fieldcat lwa_fcat cht_slis_fcat: 'DOCUMENT_NO' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' pi_tab_name,
                                         'KUNNR' pi_tab_name,
                                         'KUNNR_NAME' pi_tab_name,
                                         'REF_RD_DOC_TYPE_DESC' pi_tab_name.

  LOOP AT cht_slis_fcat ASSIGNING <lfs_fcat>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <lfs_fcat>-key = ''.
    CASE <lfs_fcat>-fieldname.
      WHEN 'BUKRS'.
        %fcat <lfs_fcat> TEXT-a10 '' '' '' '' '' '' 1.
      WHEN 'SAP_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a01 '' '' '' '' '' '' 2.
      WHEN 'GJAHR'.
        %fcat <lfs_fcat> TEXT-a12 '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE'.
        %fcat <lfs_fcat> TEXT-a05 '' '' '' '' '' '' 4.
      WHEN 'RD_DOC_TYPE_DESC'.
        %fcat <lfs_fcat> TEXT-a06 '' '' '' '' '' '' 5.
      WHEN 'MODULE_ETX'.
        %fcat <lfs_fcat> TEXT-a87 '' '' '' '' '' '' 6.
      WHEN 'DOCUMENT_NO'.
        %fcat <lfs_fcat> TEXT-a02 '' '' '' '' '' gc_alpha 7.
      WHEN 'KUNNR'.
        %fcat <lfs_fcat> TEXT-a14 '' '' '' '' '' gc_alpha 8.
      WHEN 'KUNNR_NAME'.
        %fcat <lfs_fcat> TEXT-a15 '' '' '' '' '' '' 9.
      WHEN 'REF_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a83 '' '' '' '' '' gc_alpha 10.
      WHEN 'REF_SAP_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a84 '' '' '' '' '' gc_alpha 11.
      WHEN 'REF_SAP_POST_DATE'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 12.
      WHEN 'REF_SAP_DOC_TYPE'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 13.
      WHEN 'REF_BUKRS'.
        %fcat <lfs_fcat> '' '' '' '' '' '' '' 14.
      WHEN 'REF_GJAHR'.
        %fcat <lfs_fcat> '' '' '' '' '' '' gc_gjahr 15.
      WHEN 'REF_RD_DOC_TYPE'.
        %fcat <lfs_fcat> TEXT-a85 '' '' '' '' '' '' 16.
      WHEN 'REF_RD_DOC_TYPE_DESC'.
        %fcat <lfs_fcat> TEXT-a86 '' '' '' '' '' '' 17.

      WHEN OTHERS.
        %fcat <lfs_fcat> '' '' '' '' 'X' '' '' lv_col.
        ADD 1 TO lv_col.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_VAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEAD
*&      --> P_
*&---------------------------------------------------------------------*
FORM display_vat  USING        pi_head TYPE gty_head
                               pi_list TYPE abap_bool.

  DATA: lt_vat        TYPE gtty_doc_h_vat,
        lt_slis_fcat  TYPE lvc_t_fcat.

  DATA: lwa_layout    TYPE lvc_s_layo,
        lwa_variant   TYPE disvariant,
        lwa_head      TYPE gty_head,
        lwa_vat       TYPE gty_doc_h_vat.

  DATA: lv_grid_title TYPE lvc_title.

  CLEAR: gv_sap_doc_no,
         gv_rd_doc_type,
         gv_kunnr.

  IF pi_list IS INITIAL.            "Link from cell

    lt_vat[] = pi_head-it_h_vat[].

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  pi_head-sap_doc_no
                         CHANGING gv_sap_doc_no.

    PERFORM conversion_exit USING gc_alpha
                                  'OUTPUT'
                                  pi_head-kunnr
                         CHANGING gv_kunnr.

    gv_rd_doc_type = pi_head-rd_doc_type.

  ELSE.                       "Press List Of Vat button

    LOOP AT gt_head INTO lwa_head.
      CLEAR lwa_vat.
      LOOP AT lwa_head-it_h_vat INTO lwa_vat.
        APPEND lwa_vat TO lt_vat.
      ENDLOOP.
    ENDLOOP.

    gv_sap_doc_no  = '-'.
    gv_rd_doc_type = '-'.
    gv_kunnr       = '-'.

  ENDIF.

  CHECK lt_vat[] IS NOT INITIAL.

  SORT lt_vat BY bukrs sap_doc_no gjahr rd_doc_type module_etx seq
                 tax_code ASCENDING.

  CONCATENATE sy-title ':' TEXT-h04 INTO lv_grid_title SEPARATED BY space.

  PERFORM : set_layout               CHANGING lwa_layout,

            lvc_fieldcatalog_merge      USING gc_doc_h_vat
                                              'LT_VAT'
                                     CHANGING lt_slis_fcat,

            manage_fcat_vat             USING 'LT_VAT'
                                     CHANGING lt_slis_fcat,

            reuse_alv_grid_display_lvc TABLES lt_vat
                                        USING '' '' 'HTML_TOP_OF_PAGE_PARTNER'
                                              lv_grid_title
                                              lwa_layout
                                              lwa_variant
                                              lt_slis_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MANAGE_FCAT_VAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_SLIS_FCAT
*&---------------------------------------------------------------------*
FORM manage_fcat_vat      USING pi_tab_name
                       CHANGING cht_slis_fcat TYPE lvc_t_fcat.

  DATA: lwa_fcat           TYPE lvc_s_fcat.
  DATA: lv_col             TYPE i.
  FIELD-SYMBOLS <lfs_fcat> TYPE lvc_s_fcat.

  DESCRIBE TABLE cht_slis_fcat LINES lv_col.

  m_add_fieldcat lwa_fcat cht_slis_fcat: 'DOCUMENT_NO' pi_tab_name,
                                         'RD_DOC_TYPE_DESC' pi_tab_name,
                                         'KUNNR' pi_tab_name,
                                         'KUNNR_NAME' pi_tab_name.

  LOOP AT cht_slis_fcat ASSIGNING <lfs_fcat>.
*  &1-reptext   = &2.
*  &1-checkbox  = &3.
*  &1-edit      = &4.
*  &1-hotspot   = &5.
*  &1-tech      = &6.
*  &1-just      = &7.
*  &1-convexit  = &8.
*  &1-col_pos   = &9.
    <lfs_fcat>-key = ''.
    CASE <lfs_fcat>-fieldname.
      WHEN 'BUKRS'.
        %fcat <lfs_fcat> TEXT-a10 '' '' '' '' '' '' 1.
      WHEN 'SAP_DOC_NO'.
        %fcat <lfs_fcat> TEXT-a01 '' '' '' '' '' '' 2.
      WHEN 'GJAHR'.
        %fcat <lfs_fcat> TEXT-a12 '' '' '' '' '' '' 3.
      WHEN 'RD_DOC_TYPE'.
        %fcat <lfs_fcat> TEXT-a05  '' '' '' '' '' '' 4.
      WHEN 'RD_DOC_TYPE_DESC'.
        %fcat <lfs_fcat> TEXT-a06  '' '' '' '' '' '' 5.
      WHEN 'MODULE_ETX'.
        %fcat <lfs_fcat> TEXT-a87 '' '' '' '' '' '' 6.
      WHEN 'DOCUMENT_NO'.
        %fcat <lfs_fcat> TEXT-a02 '' '' '' '' '' gc_alpha 7.
      WHEN 'KUNNR'.
        %fcat <lfs_fcat> TEXT-a14  '' '' '' '' '' gc_alpha 8.
      WHEN 'KUNNR_NAME'.
        %fcat <lfs_fcat> TEXT-a15  '' '' '' '' '' '' 9.
      WHEN 'RD_VAT_TYPE'.
        %fcat <lfs_fcat> TEXT-a35  '' '' '' '' '' '' 10.
      WHEN 'TAX_CODE'.
        %fcat <lfs_fcat> TEXT-a36  '' '' '' '' '' '' 11.
      WHEN 'VAT_RATE'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 12.
      WHEN 'NET_AMT_BF_VAT'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 13.
      WHEN 'VAT_AMT'.
        %fcat <lfs_fcat> ''  '' '' '' '' '' '' 14.

      WHEN OTHERS.
        %fcat <lfs_fcat> '' '' '' '' 'X' '' '' lv_col.
        ADD 1 TO lv_col.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_VARIANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LWA_VARIANT
*&---------------------------------------------------------------------*
FORM set_variant  CHANGING pwa_variant TYPE disvariant.

  IF p_vari IS NOT INITIAL.
    pwa_variant-report = sy-repid.
    pwa_variant-variant = p_vari.
  ENDIF.

ENDFORM.
