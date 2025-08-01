*-----------------------------------------------------------------------
*  Program ID         : ZSDSCOR0010
*  Creation Date      : 10.06.2024
*  Author             : Nadtaya T.(Eviden)
*  Add-on ID          : N/A
*  Description        : IO Planning and Budget Return
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
REPORT zsdscor0010.

*----------------------------------------------------------------------*
*   I N C L U D E                                                      *
*----------------------------------------------------------------------*
INCLUDE rkassela ##INCL_OK .
INCLUDE zsdscor0010_top ##INCL_OK .

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION ##CLASS_FINAL .
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_top_of_page FOR EVENT top_of_page OF cl_salv_events_hierseq
        IMPORTING r_top_of_page.
ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command
  METHOD on_top_of_page.
    DATA: lr_content TYPE REF TO cl_salv_form_element.

**... create the content
    PERFORM create_alv_form_content_top
      CHANGING lr_content.

*... set the content
    r_top_of_page->set_content( lr_content ).
  ENDMETHOD.                    "on_top_of_page
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE t001-bukrs DEFAULT '1000' OBLIGATORY.
  PARAMETERS: p_gjahr TYPE gjahr DEFAULT sy-datum(4) OBLIGATORY.
  SELECT-OPTIONS: s_aufnr FOR aufk-aufnr.
  SELECT-OPTIONS: s_cegrp FOR setheader-setname NO INTERVALS NO-EXTENSION ##NEEDED.
  SELECT-OPTIONS: s_aufex FOR aufk-aufex.
  SELECT-OPTIONS: s_kostv FOR aufk-kostv.
  SELECT-OPTIONS: s_akstl FOR aufk-akstl.
  SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: rb_1 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 7(40) TEXT-003 FOR FIELD rb_1.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 31.
      PARAMETERS: p_versn TYPE versn DEFAULT 'Z05'.
      SELECTION-SCREEN COMMENT 7(10) TEXT-005 FOR FIELD p_versn.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: rb_2 RADIOBUTTON GROUP rad1.
      SELECTION-SCREEN COMMENT 7(40) TEXT-004 FOR FIELD rb_2.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END   OF BLOCK b02.
SELECTION-SCREEN END   OF BLOCK b01.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM f_screen_check.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cegrp-low.
  PERFORM f4_ordgrp.


AT SELECTION-SCREEN OUTPUT.
* Set Screen format

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_get_data.


*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM f_alv_display.
  PERFORM display_hierseq.

*&---------------------------------------------------------------------*
*& Form f_alv_display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_alv_display .

  DATA:
##NEEDED       lr_text             TYPE REF TO cl_salv_form_text,
    ls_alvtable         TYPE REF TO cl_salv_table,
    lr_columns          TYPE REF TO cl_salv_columns_table,
    lr_content          TYPE REF TO cl_salv_form_layout_grid,
    lr_label            TYPE REF TO cl_salv_form_label,
    lf_text             TYPE string,
    ls_layout           TYPE REF TO cl_salv_layout,
    ls_key              TYPE salv_s_layout_key,
    ls_display_settings TYPE REF TO cl_salv_display_settings,
    ls_functions_list   TYPE REF TO cl_salv_functions_list.

  FIELD-SYMBOLS <lfs_table> TYPE ANY TABLE.

  CHECK rb_1 EQ abap_true.

  ASSIGN gt_slave TO <lfs_table>.

*Initialize ref to cl_salv_table
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = ls_alvtable
        CHANGING
          t_table      = <lfs_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      MESSAGE TEXT-e01 TYPE 'E'.
  ENDTRY.

  lr_columns = ls_alvtable->get_columns( ).
  lr_columns->set_optimize( abap_true ).

* Set column
  PERFORM f_detail_column_settings_rept USING lr_columns.

  ls_display_settings = ls_alvtable->get_display_settings( ).
  ls_display_settings->set_striped_pattern( 'X' ).
  ls_display_settings->set_list_header( sy-title ).

  ls_functions_list = ls_alvtable->get_functions( ).
  ls_functions_list->set_all( ).
*
  ls_key-report = sy-cprog.
  ls_key-handle = '0001'.
  ls_layout = ls_alvtable->get_layout( ).
  ls_layout->set_key( ls_key ).
  ls_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  ls_layout->set_default( 'X' ).

* Top_of_list
  CREATE OBJECT lr_content.
  lr_label = lr_content->create_label(
    row    = 1
    column = 1
    text   = sy-title ).

  lf_text = TEXT-007.
  lr_label = lr_content->create_label(
    row    = 2
    column = 1
    text   = lf_text ).
  lf_text = p_gjahr.
  lr_label = lr_content->create_label(
    row    = 2
    column = 2
    text   = lf_text ).

  lf_text = TEXT-005.
  lr_label = lr_content->create_label(
    row    = 3
    column = 1
    text   = lf_text ).
  lf_text = p_versn.
  lr_label = lr_content->create_label(
    row    = 3
    column = 2
    text   = lf_text ).

  lr_label->set_label_for( lr_text ).
  ls_alvtable->set_top_of_list( lr_content ).
*
* Display Table
  ls_alvtable->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f4_ordgrp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_ordgrp .

  DATA: lf_help_setnr     TYPE rgsmh-setnr,
        lf_help_searchfld TYPE rgsmh-searchfld VALUE '1000',
        lf_help_set       TYPE rgsbs-setnr,
        lf_help_setclass  TYPE rgsmh-class VALUE '0103',
        lf_f4_fieldname   TYPE fieldname VALUE 'AUFNR'.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      class           = lf_help_setclass
      field_name      = lf_f4_fieldname
      searchfld       = lf_help_searchfld
      searchfld_input = ' '
      set             = lf_help_set
    IMPORTING
      set_name        = lf_help_setnr
    EXCEPTIONS
      no_set_picked   = 1.
  IF sy-subrc EQ 0.
    s_cegrp-low = lf_help_setnr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_screen_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_screen_check.

  CHECK s_cegrp-low IS NOT INITIAL.

  DATA: lf_help_setnr_new TYPE sethier-setid.

  lf_help_setnr_new = '0103' && s_cegrp-low.

  CALL FUNCTION 'G_SET_GET_INFO'
    EXPORTING
*     class            = help_setclass
      no_set_title     = 'X'
      setname          = lf_help_setnr_new
*     table            = settabname
      use_table_buffer = ''
*    IMPORTING
*     info             = ls_setinfo
    EXCEPTIONS
      set_not_found    = 01.

  IF sy-subrc NE 0.
    SET CURSOR FIELD 'S_CEGRP-LOW'.
    MESSAGE 'Invalid set name'(009) TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_posting
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_posting  TABLES ut_row TYPE salv_t_row.
  DATA: lf_subrc TYPE sysubrc.

  FREE gt_result.

  LOOP AT ut_row ASSIGNING FIELD-SYMBOL(<l_row>).
    READ TABLE gt_master ASSIGNING FIELD-SYMBOL(<l_master>) INDEX <l_row>.
    IF sy-subrc EQ 0.

      lf_subrc = 0.

      PERFORM f_commitment_check USING <l_master> CHANGING lf_subrc.
      IF lf_subrc NE 0.
        CONTINUE.
      ENDIF.

*  4.2.1  Copy all IO planning data from version “0” to “Z05”
*         by G/L account and period (Refer to T-code: KO14).
      PERFORM f_io_copy_plan_to_plan USING <l_master> CHANGING lf_subrc.

*  4.2.2  Adjust IO plan to version “0” by G/L account and period
*  (Refer to T-code: KPF6) when “Plan Cost” = “Assigned” amount.
      CHECK lf_subrc EQ 0.
      PERFORM f_io_adjust_plan USING <l_master> CHANGING lf_subrc.

*  4.2.3  Post IO budget return (Refer to T-code: KO26)
*  with total “Available” amount.
      CHECK lf_subrc EQ 0.
      PERFORM f_io_post_budget_return USING <l_master> CHANGING lf_subrc.

*  4.2.4  Set status “TECO”
*  and then “CLSD” to Internal Order (Refer to T-code: KO02”.
* I0046 CLSD Closed
* I0045 TECO Technically completed
      CHECK lf_subrc EQ 0.
      PERFORM f_io_set_status USING <l_master> CHANGING lf_subrc.

      IF lf_subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<l_result>).
        MOVE-CORRESPONDING <l_master> TO <l_result>.
        <l_result>-msgtx = 'Order Successfully updated'(010).
        <l_result>-icon  = icon_green_light.
      ELSE.
        APPEND INITIAL LINE TO gt_result ASSIGNING <l_result>.
        MOVE-CORRESPONDING <l_master> TO <l_result>.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        <l_result>-msgtx = 'No any update'(011).
        <l_result>-icon  = icon_red_light.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_version
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_VERSN_000
*&---------------------------------------------------------------------*
FORM f_check_version  USING   uf_versn TYPE versn.

  DATA: lf_copy.

  CALL FUNCTION 'KBPS_CHECK_VERSION'
    EXPORTING
      im_applik  = 'O'           "Auftrag
      im_version = uf_versn
      im_gjahr   = p_gjahr
      im_wrttp   = '01'          "Planung
      im_kokrs   = gc_kokrs_1000
      im_copy    = lf_copy.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_version_compare
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_VERSN_000
*&      --> GC_VERSN_Z05
*&      --> ENDFORM
*&---------------------------------------------------------------------*
FORM f_check_version_compare  USING    uf_source_versn TYPE versn
                                       uf_target_versn TYPE versn.

  CALL FUNCTION 'KBPA_VERSIONS_COMPARE'
    EXPORTING
      i_version_1        = uf_source_versn
      i_version_2        = uf_target_versn
      i_year_1           = p_gjahr
      i_year_2           = p_gjahr
      i_kokrs            = gc_kokrs_1000
    EXCEPTIONS
      version_diff_kurst = 01
      version_diff_pldat = 02.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_online_processing
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_online_processing
                       USING us_master TYPE g_type_s_master
                             id_tcode TYPE sy-tcode
                               id_kokrs TYPE ccss-kokrs
                               id_mode  TYPE char02
                               id_kass_vrgng TYPE tj01-vrgng
                               id_bltxt TYPE cobk-bltxt
                               id_selart TYPE kass_selart
                               is_rkpt3_dynp TYPE rkpt3_dynp
                               is_settings TYPE kpu3_settings
                                CHANGING cf_subrc TYPE sysubrc.

  DATA: lt_tobjnr TYPE STANDARD TABLE OF kpu3_objnr,
        lt_sobjnr TYPE STANDARD TABLE OF kpu3_objnr,
        ls_sinfo  TYPE kpu3_sinfo,
        ls_tinfo  TYPE kpu3_tinfo,
        ls_ctrl   TYPE kpu3_ctrl.
  DATA: lt_vrgng     TYPE STANDARD TABLE OF kpu3_vrgng,
        ls_vrgng_grp TYPE kpu3_vrgng_grp.
  DATA: ls_result     TYPE kpu3_result,
        lt_mesg       TYPE STANDARD TABLE OF smesg,
        lt_list       TYPE STANDARD TABLE OF kpu3_list,
        ld_msgid      TYPE sy-uzeit,
        ls_rkpt3_dynp TYPE rkpt3_dynp.

*> Set parameters
  SET PARAMETER ID 'SAPMKPT4_DYNP' FIELD is_rkpt3_dynp.
  SET PARAMETER ID 'CAC' FIELD id_kokrs.
  SET PARAMETER ID 'TCODE' FIELD id_tcode.                  "note416911
  PERFORM check_budget_period IN PROGRAM sapmkpt4 IF FOUND. " check if BudPer is activated
*  SET PARAMETER ID 'SAPMKPT4_FUFU' FIELD gs_fm_sel_crit.

  gs_info-kokrs = id_kokrs.


  APPEND INITIAL LINE TO lt_sobjnr ASSIGNING FIELD-SYMBOL(<l_sobjnr>).
  <l_sobjnr>-objnr = |OR{ us_master-aufnr }|.
  APPEND INITIAL LINE TO lt_tobjnr ASSIGNING FIELD-SYMBOL(<l_tobjnr>).
  <l_tobjnr>-objnr = |OR{ us_master-aufnr }|.

*> Create Sinfo/Tinfo

  ls_sinfo-versn = is_rkpt3_dynp-sversn.
  ls_sinfo-gjahr = is_rkpt3_dynp-sgjahr.
  ls_sinfo-perab = is_rkpt3_dynp-sperab.
  ls_sinfo-perbi = is_rkpt3_dynp-sperbi.
  ls_tinfo-versn = is_rkpt3_dynp-tversn.
  ls_tinfo-gjahr = is_rkpt3_dynp-tgjahr.
  ls_tinfo-perab = is_rkpt3_dynp-tperab.
  ls_tinfo-perbi = is_rkpt3_dynp-tperbi.

*> Fill ctrl
  IF ( is_rkpt3_dynp-vrgng_rkp2p = true AND
       is_rkpt3_dynp-vrgng_rkp2q = true ) OR
       is_rkpt3_dynp-flg_vrgall  = true.
*    both
    ls_ctrl-price_quant = 'B'.
  ELSEIF is_rkpt3_dynp-vrgng_rkp2q = true.
*    prices
    ls_ctrl-price_quant = 'Q'.
  ELSEIF is_rkpt3_dynp-vrgng_rkp2p = true.
*    quantities
    ls_ctrl-price_quant = 'P'.
  ELSE.
*    nothing
*    CLEAR ls_ctrl-price_quant.
    ls_ctrl-price_quant = 0.
  ENDIF.
  ls_ctrl-flg_test = is_rkpt3_dynp-flg_test.
  ls_ctrl-flg_cltext   = is_rkpt3_dynp-flg_ltext.
  ls_ctrl-flg_update   = is_rkpt3_dynp-update_yes.
  IF is_rkpt3_dynp-update_kum = true.
    ls_ctrl-flg_delta    = is_rkpt3_dynp-update_kum.
    ls_ctrl-flg_update   = true.
  ENDIF.

  ls_ctrl-flg_ckalk       = is_rkpt3_dynp-flg_kalk.
  ls_ctrl-flg_resonly     = space.
  ls_ctrl-flg_strumi      = is_rkpt3_dynp-flg_strum.
  ls_ctrl-flg_split       = is_rkpt3_dynp-flg_split.
  ls_ctrl-flg_specper     = is_rkpt3_dynp-flg_specper.
  ls_ctrl-flg_ctwaer      = is_rkpt3_dynp-flg_ctwaer.
  ls_ctrl-flg_cowaer      = is_rkpt3_dynp-flg_cowaer.
  ls_ctrl-flg_ckwaer      = is_rkpt3_dynp-flg_ckwaer.
  ls_ctrl-flg_powaer      = is_rkpt3_dynp-flg_powaer.
  ls_ctrl-flg_pkwaer      = is_rkpt3_dynp-flg_pkwaer.
  ls_ctrl-flg_no_versenqu = true.

  ls_ctrl-revprct = is_rkpt3_dynp-revprct.
  t823z-proz      = is_rkpt3_dynp-revprct.

* projection/forecast
  IF is_rkpt3_dynp-flg_projection = space.
    CLEAR: ls_ctrl-fover, ls_ctrl-fover_perio.
  ELSE.
    ls_ctrl-fover         = is_rkpt3_dynp-fover.
    ls_ctrl-fover_perio   = is_rkpt3_dynp-fover_perio.
  ENDIF.

  ls_rkpt3_dynp = is_rkpt3_dynp.

*> Fill business transactions
  PERFORM fill_transactions IN PROGRAM sapmkpt4 TABLES   lt_vrgng
                            USING    id_mode
                                     id_selart
                                     id_kass_vrgng
                            CHANGING ls_rkpt3_dynp
                                     ls_vrgng_grp.


**> Initialize SCHEDULE MANAGER
*  PERFORM initialize_schedman(sapmkpt4)
*                TABLES   lt_tobjnr
*                USING    is_rkpt3_dynp
*                         ls_ctrl
*                         is_witem
*                         id_tcode
*                         id_mode
*                         id_kass_vrgng
*                         id_selart
*                         ls_sinfo
*                         ls_tinfo
*                         id_kokrs
*                CHANGING ld_schedman_active
*                         ls_schedman_key
*                         ld_msgid.

*> Call function modules
  PERFORM call_function_module IN PROGRAM sapmkpt4 TABLES lt_sobjnr lt_tobjnr lt_vrgng
                                      gt_callback
                                      lt_mesg lt_list
                               USING  id_mode is_rkpt3_dynp-flg_proto
                                              id_bltxt ls_vrgng_grp
                                      id_kokrs ls_sinfo ls_tinfo
                                      ls_ctrl is_settings
                                      is_rkpt3_dynp
                               CHANGING ls_result .

  PERFORM fill_message_handler IN PROGRAM sapmkpt4 TABLES   lt_mesg
                                        lt_list
                               USING    ld_msgid
                               CHANGING ls_result.

  LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<l_list>).
    APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<l_result>).
    MOVE-CORRESPONDING us_master TO <l_result>.
    <l_result>-kstar = <l_list>-kstar.
    <l_result>-tcode = 'KO14'.
    <l_result>-logmsg = TEXT-t01.
    READ TABLE lt_mesg INTO DATA(ls_mesg) WITH KEY msgty = 'E'.
    IF sy-subrc EQ 0.
      <l_result>-msgtx = ls_mesg-text.
      <l_result>-icon = icon_red_light.
    ENDIF.
    IF <l_list>-cerror EQ abap_true.
      cf_subrc = 1.
      <l_result>-icon = icon_red_light.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    APPEND INITIAL LINE TO gt_result ASSIGNING <l_result>.
    MOVE-CORRESPONDING us_master TO <l_result>.
    <l_result>-tcode = 'KO14'.
    <l_result>-logmsg = TEXT-t01.
    <l_result>-msgtx = 'No process'(012).
    cf_subrc = 1.

  ENDIF.
*
**> Close SCHEDULE MANAGER
*  PERFORM close_schedman(sapmkpt4) USING ld_schedman_active
*                               ls_schedman_key
*                               is_witem
*                               id_okey
*                               ld_msgid.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_start_program
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_INFO_BATCH_WAIT
*&      --> GS_INFO_KOKRS
*&      --> GS_CONTROL_MODE
*&      --> GS_CONTROL_KASS_VRGNG
*&      --> GS_CONTROL_BLTXT
*&      --> GS_CONTROL_SELART
*&      --> GS_INFO_SDATAB
*&      --> GS_INFO_SDATBI
*&      --> GS_INFO_SOBJNR
*&      --> GS_INFO_TDATAB
*&      --> GS_INFO_TDATBI
*&      --> GS_CONTROL_BATCH_REPORT
*&      --> RKPT3_DYNP
*&      --> KPU3_SETTINGS
*&---------------------------------------------------------------------*
FORM f_start_program  USING us_master TYPE g_type_s_master
                         id_kokrs     TYPE ccss-kokrs
                         id_mode       TYPE char02
                         id_kass_vrgng TYPE tj01-vrgng
                         id_bltxt      TYPE cobk-bltxt
                         id_selart     TYPE kass_selart
                         is_rkpt3_dynp TYPE rkpt3_dynp
                         is_settings TYPE kpu3_settings
   CHANGING cf_subrc TYPE sysubrc.

  PERFORM f_online_processing
                                USING us_master
*                                      lf_tcode"
                                      sy-tcode
                                      id_kokrs
                                      id_mode
                                      id_kass_vrgng
                                      id_bltxt
                                      id_selart
                                      is_rkpt3_dynp
                                      is_settings
                                       CHANGING cf_subrc .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_io_copy_plan_to_plan
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_io_copy_plan_to_plan USING us_master TYPE g_type_s_master
                              CHANGING cf_subrc TYPE sysubrc.

  gs_control-mode = '10'.
  gs_control-selart = 'OR1'.
  gs_control-curr = 'X'.
  rkpt3_dynp-sversn = gc_versn_000.
  rkpt3_dynp-sgjahr = p_gjahr.
  rkpt3_dynp-sperab = '001'.
  rkpt3_dynp-sperbi = '012'.
  rkpt3_dynp-tversn = gc_versn_z05.
  rkpt3_dynp-tgjahr = p_gjahr.
  rkpt3_dynp-tperab = '001'.
  rkpt3_dynp-tperbi = '012'.
  rkpt3_dynp-revprct = '100.00'.
  rkpt3_dynp-fover = 'H1'.
  rkpt3_dynp-flg_vrgsel = 'X'.

*  rkpt3_dynp-update_not   = 'X'.
  rkpt3_dynp-update_yes   = 'X'.

  rkpt3_dynp-flg_sobj_no = 'X'.
*  rkpt3_dynp-flg_test = 'X'.
  rkpt3_dynp-flg_test = ''.
  rkpt3_dynp-flg_proto = 'X'.
  rkpt3_dynp-flg_strum = 'X'.
  rkpt3_dynp-flg_split = 'X'.
  rkpt3_dynp-flg_ctwaer = 'X'.
  rkpt3_dynp-flg_pkwaer = 'X'.

***
  rkpt3_dynp-vrgng_rkp1 = 'X'.
  rkpt3_dynp-vrgng_rkp5 = 'X'.

  PERFORM f_start_program
     USING us_master
                              gc_kokrs_1000
                              gs_control-mode
                              gs_control-kass_vrgng
                              gs_control-bltxt
                              gs_control-selart
                              rkpt3_dynp
                              kpu3_settings
                              CHANGING cf_subrc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_io_adjust_plan
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_io_adjust_plan USING us_master TYPE g_type_s_master
                            CHANGING cf_subrc TYPE sysubrc.

  DATA: lt_itrku01ja TYPE TABLE OF rku01ja,
        ls_rku01_cur TYPE rku01_cur,
        lf_kstar     TYPE v_cosp_view-kstar.
  DATA:
    lf_target TYPE c LENGTH 20.

*   planning in object currency
  ls_rku01_cur-wog_man = 'X'.

  APPEND INITIAL LINE TO lt_itrku01ja ASSIGNING FIELD-SYMBOL(<l_itrku01ja>).

  LOOP AT gt_slave ASSIGNING FIELD-SYMBOL(<l_slave>) WHERE aufnr EQ us_master-aufnr.
    lf_target = |<L_ITRKU01JA>-WOG{ <l_slave>-poper }|.
    ASSIGN (lf_target) TO FIELD-SYMBOL(<l_target>).
    IF sy-subrc EQ 0.
      <l_target> = <l_slave>-assign.
    ENDIF.
    lf_kstar = |{ <l_slave>-kstar ALPHA  = IN }|.
  ENDLOOP.
*
  IF sy-subrc EQ 0.
    <l_itrku01ja>-aufnr = us_master-aufnr.
    <l_itrku01ja>-kstar = lf_kstar.
    <l_itrku01ja>-fcwkg    = '1'.                    "must be filled
    <l_itrku01ja>-fcwkf    = '1'.                    "must be filled
    <l_itrku01ja>-fcwkv    = '1'.                    "must be filled
    <l_itrku01ja>-fcmeg    = '1'.                    "must be filled
    <l_itrku01ja>-fcmef    = '1'.                    "must be filled
    <l_itrku01ja>-fcmev    = '1'.                    "must be filled
    <l_itrku01ja>-twaer    = 'THB'.

    CALL FUNCTION 'K_COSTS_PLAN_INTERFACE_PERIOD'
      EXPORTING
*       BLTXT            = ' '
        commit           = ''
        delta            = ''
        gjahr            = p_gjahr
        kokrs            = gc_kokrs_1000
        messages_show    = 'X'
        perab            = '001'
        perbi            = '012'
        update_values    = 'X'
        versn            = gc_versn_000
        vrgng            = 'RKP1'
        online_vb        = ''
        irku01_cur       = ls_rku01_cur
        testmode         = abap_false
*       WSVALUE          = 1
*       KEEP_TWAER       = ' '
      TABLES
        irku01ja         = lt_itrku01ja
*       IRKU0RJA         =
      EXCEPTIONS
        messages_occured = 1
        OTHERS           = 2.

    IF sy-subrc NE 0.
      cf_subrc = 1.
    ELSE.
      cf_subrc = sy-subrc.
    ENDIF.

    APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<l_result>).
    MOVE-CORRESPONDING us_master TO <l_result>.
    <l_result>-tcode = 'KPF6'.
    <l_result>-logmsg = TEXT-t02.
    IF cf_subrc = 1.

      <l_result>-icon = icon_red_light.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                       INTO <l_result>-msgtx.
    ELSE.
      cf_subrc = sy-subrc.
    ENDIF.

  ELSE.
    APPEND INITIAL LINE TO gt_result ASSIGNING <l_result>.
    MOVE-CORRESPONDING us_master TO <l_result>.
    <l_result>-tcode = 'KPF6'.
    <l_result>-logmsg = TEXT-t02.
    <l_result>-msgtx = 'Zero Amount'(013).
    <l_result>-icon = icon_red_light.
    cf_subrc = 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_io_post_budget_return
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_io_post_budget_return USING us_master TYPE g_type_s_master
                             CHANGING cf_subrc TYPE sysubrc.
  DATA:
    lf_amount      TYPE bpja-wljhr,
    lt_return      TYPE bapiret2_t,
    lt_bpak        TYPE STANDARD TABLE OF bpak,
    lf_error_found TYPE oax.
*
  FREE: lt_return, lt_bpak.
*
  CLEAR lf_amount.
  LOOP AT gt_slave ASSIGNING FIELD-SYMBOL(<l_slave>) WHERE aufnr EQ us_master-aufnr.
    lf_amount =  lf_amount + <l_slave>-avai.
  ENDLOOP.

  IF lf_amount GT 0.

    APPEND INITIAL LINE TO lt_bpak ASSIGNING FIELD-SYMBOL(<l_bpak>).
    <l_bpak>-e_objnr = 'OR' && us_master-aufnr.
*    <l_bpak>-e_vorga = 'BOR0'.  "KO26
    <l_bpak>-e_gjahr = p_gjahr.
    <l_bpak>-wert    = - lf_amount.
    <l_bpak>-twaer   = 'THB'.

    CALL FUNCTION 'KBPP_EXTERN_UPDATE_CO'
      EXPORTING
        i_budget_activity      = 'BOR0'
        i_budget_activ_sup_ret = abap_true
*       I_BUDGET_DISTRIBUTION_ALLOWED       = ' '
*       i_commit_data          = ''
        i_delta_amounts        = 'X'
*       i_rollup_data          = ''
*       i_check_plan_data      = ''
        i_application          = 'O'
        i_commit_all           = 'X'
      IMPORTING
        e_errors_found         = lf_error_found
      TABLES
        it_bpak                = lt_bpak
        it_return              = lt_return
      EXCEPTIONS ##FM_SUBRC_OK
        no_update              = 1
        OTHERS                 = 2.
    IF lf_error_found IS NOT INITIAL.
      cf_subrc = 1.
    ENDIF.

    APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<l_result>).
    MOVE-CORRESPONDING us_master TO <l_result>.
    <l_result>-tcode = 'KO26'.
    <l_result>-logmsg = TEXT-t03.

    IF  cf_subrc = 1.
      <l_result>-icon = icon_red_light.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                          INTO <l_result>-msgtx.
    ENDIF.
  ELSE.
    APPEND INITIAL LINE TO gt_result ASSIGNING <l_result>.
    MOVE-CORRESPONDING us_master TO <l_result>.
    <l_result>-tcode = 'KO26'.
    <l_result>-logmsg = TEXT-t03.
    <l_result>-msgtx = 'Zero Amount'(013).
    cf_subrc = 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_io_set_status
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_io_set_status USING us_master TYPE g_type_s_master
                      CHANGING cf_subrc TYPE sysubrc.

  DATA: lt_status TYPE STANDARD TABLE OF jstat.

  APPEND INITIAL LINE TO lt_status ASSIGNING FIELD-SYMBOL(<l_status>).
  <l_status>-stat = 'I0045'.
  APPEND INITIAL LINE TO lt_status ASSIGNING <l_status>.
  <l_status>-stat = 'I0046'.

  CALL FUNCTION 'STATUS_CHANGE_INTERN'
    EXPORTING
      objnr               = us_master-objnr
    TABLES
      status              = lt_status
    EXCEPTIONS
      object_not_found    = 1
      status_inconsistent = 2
      status_not_allowed  = 3
      OTHERS              = 4.
  IF sy-subrc NE 0.
    cf_subrc = sy-subrc.
  ELSE.
    cf_subrc = 0.
  ENDIF.

  APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<l_result>).
  MOVE-CORRESPONDING us_master TO <l_result>.
  <l_result>-tcode = 'KO02'.
  <l_result>-logmsg = TEXT-t04.
  IF cf_subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                     INTO <l_result>-msgtx.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_data.

  CONSTANTS: lc_actual TYPE char20 VALUE 'ACTUAL',
             lc_commit TYPE char20 VALUE 'COMMIT',
             lc_assign TYPE char20 VALUE 'ASSIGN',
             lc_plan   TYPE char20 VALUE 'PLAN',
             lc_avai   TYPE char20 VALUE 'AVAI'.

  DATA: lt_aufk      TYPE STANDARD TABLE OF aufk,
        lt_aufnr     TYPE STANDARD TABLE OF bapi1117_values,
        lt_hier      TYPE STANDARD TABLE OF bapiset_hier,
        lf_groupname TYPE bapico_group-groupname,
        lr_versn     TYPE RANGE OF versn.

  CASE abap_true.
    WHEN rb_1.
      IF p_versn EQ 'Z05'.
        lr_versn = VALUE #( sign = 'I' option = 'EQ' ( low = p_versn )
                                                           ( low = gc_versn_000 ) ).
      ELSE.
        lr_versn = VALUE #( sign = 'I' option = 'EQ' ( low = p_versn ) ).
      ENDIF.
    WHEN rb_2.
      lr_versn = VALUE #( sign = 'I' option = 'EQ' ( low = gc_versn_000 ) ).
  ENDCASE.

  IF s_cegrp-low IS NOT INITIAL.
    lf_groupname = s_cegrp-low.
    CALL FUNCTION 'BAPI_INTERNALORDRGRP_GETDETAIL'
      EXPORTING
        groupname       = lf_groupname
      TABLES
        hierarchynodes  = lt_hier
        hierarchyvalues = lt_aufnr.

**..Order Group
    LOOP AT lt_aufnr ASSIGNING FIELD-SYMBOL(<l_aufnr>).
      APPEND INITIAL LINE TO lt_aufk ASSIGNING FIELD-SYMBOL(<l_aufk_h>).
      <l_aufk_h>-aufnr = <l_aufnr>-valfrom.
    ENDLOOP.
  ENDIF.

  IF lt_aufk IS NOT INITIAL.
*..get Order Master
    SELECT objnr,         "#EC CI_NO_TRANSFORM #EC CI_FAE_LINES_ENSURED
            aufnr,
            ktext,
            auart,
            aufex,
            akstl,
            kostv
 ##TOO_MANY_ITAB_FIELDS       FROM aufk INTO CORRESPONDING FIELDS OF TABLE @gt_master "#EC CI_NO_TRANSFORM
   FOR ALL ENTRIES IN @lt_aufk
       WHERE aufnr EQ @lt_aufk-aufnr
         AND bukrs EQ @p_bukrs
         AND aufnr IN @s_aufnr
         AND aufex IN @s_aufex
         AND kostv IN @s_kostv
         AND akstl IN @s_akstl
         AND auart EQ 'ZI04' ORDER BY PRIMARY KEY.

  ELSE.
*..get Order Master
    SELECT objnr,         "#EC CI_NO_TRANSFORM #EC CI_FAE_LINES_ENSURED
            aufnr,
            ktext,
            auart,
            aufex,
            akstl,
            kostv
 ##TOO_MANY_ITAB_FIELDS       FROM aufk INTO CORRESPONDING FIELDS OF TABLE @gt_master "#EC CI_NO_TRANSFORM
       WHERE bukrs EQ @p_bukrs
         AND aufnr IN @s_aufnr
         AND aufex IN @s_aufex
         AND kostv IN @s_kostv
         AND akstl IN @s_akstl
         AND auart EQ 'ZI04' ORDER BY PRIMARY KEY.
  ENDIF.
  IF lines( gt_master ) EQ 0.
    MESSAGE 'No data found'(018) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*..Get COSP
  SELECT lednr, versn, objnr, wrttp, gjahr, kstar, twaer, wog001, wog002, wog003, "#EC CI_NO_TRANSFORM
         wog004, wog005, wog006, wog007, wog008, wog009, wog010, wog011, wog012
    INTO TABLE @DATA(lt_cosp) FROM v_cosp_view
        FOR ALL ENTRIES IN @gt_master
        WHERE objnr EQ @gt_master-objnr
          AND lednr EQ '00'
          AND gjahr EQ @p_gjahr
          AND versn IN @lr_versn .
  IF rb_1 EQ abap_true AND p_versn EQ gc_versn_z05.
    DELETE lt_cosp WHERE ( versn EQ gc_versn_z05 AND wrttp NE '10' ).
  ENDIF.

*..Transform to REPORT format
  LOOP AT lt_cosp ASSIGNING FIELD-SYMBOL(<l_cosp>).

    READ TABLE gt_master ASSIGNING FIELD-SYMBOL(<l_aufk>) WITH KEY objnr = <l_cosp>-objnr.

    CASE <l_cosp>-wrttp.
      WHEN '11'.
        PERFORM f_get_cosp_data  TABLES gt_slave USING lc_actual <l_aufk> <l_cosp>.

        PERFORM f_get_cosp_data TABLES gt_slave USING lc_assign <l_aufk> <l_cosp>.

        PERFORM f_reverse_sign CHANGING <l_cosp>.

        PERFORM f_get_cosp_data TABLES gt_slave USING lc_avai <l_aufk> <l_cosp>.

      WHEN '21' OR '22'.
        PERFORM f_get_cosp_data TABLES gt_slave USING lc_commit <l_aufk> <l_cosp>.
        PERFORM f_get_cosp_data TABLES gt_slave USING lc_assign <l_aufk> <l_cosp>.
        PERFORM f_reverse_sign CHANGING <l_cosp>.
        PERFORM f_get_cosp_data TABLES gt_slave USING lc_avai <l_aufk> <l_cosp>.

      WHEN '10'.
        PERFORM f_get_cosp_data TABLES gt_slave USING lc_plan <l_aufk> <l_cosp>.
        PERFORM f_get_cosp_data TABLES gt_slave USING lc_avai <l_aufk> <l_cosp>.

      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  IF lines( gt_slave ) EQ 0.
    MESSAGE 'No COSP data found'(008) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_slave.

ENDFORM.
FORM display_hierseq.

  DATA:
    lt_binding TYPE salv_t_hierseq_binding,
    ls_binding TYPE salv_s_hierseq_binding.

*  DATA:
*    lr_functions TYPE REF TO cl_salv_functions_list.

  DATA:
    lr_columns TYPE REF TO cl_salv_columns_hierseq,
    lr_column  TYPE REF TO cl_salv_column_hierseq.

  DATA:
    lr_level TYPE REF TO cl_salv_hierseq_level.

  DATA:
    lr_selections TYPE REF TO cl_salv_selections.

  DATA: lr_events TYPE REF TO cl_salv_events_hierseq.
*... set list title
  DATA: lr_display_settings TYPE REF TO cl_salv_display_settings,
        l_title             TYPE lvc_title.

  CHECK rb_2 EQ abap_true.

*... create the binding information between master and slave

  ls_binding-master = 'OBJNR'.
  ls_binding-slave  = 'OBJNR'.
  APPEND ls_binding TO lt_binding.

*... create an ALV hierseq table
  TRY.
      cl_salv_hierseq_table=>factory(
        EXPORTING
          t_binding_level1_level2 = lt_binding
        IMPORTING
          r_hierseq               = gr_hierseq
        CHANGING
          t_table_level1          = gt_master
          t_table_level2          = gt_slave ).
    CATCH cx_salv_data_error cx_salv_not_found.
      MESSAGE TEXT-e01 TYPE 'E'.
  ENDTRY.

*... §3 Functions
*... §3.2 include own functions by setting own status
  gr_hierseq->set_screen_status(
    pfstatus      = 'ZSTANDARD'
    report        = sy-repid
    set_functions = cl_salv_hierseq_table=>c_functions_all ).

*... §3.1 activate ALV generic Functions
*  lr_functions = gr_hierseq->get_functions( ).
*  lr_functions->set_all( abap_true ).

*
*... *** MASTER Settings ***
  TRY.
      lr_columns = gr_hierseq->get_columns( 1 ).
    ##NO_HANDLER    CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found.
  ENDTRY.
*... optimize the columns
  lr_columns->set_optimize( abap_true ).
*... set the columns technical
  TRY.
      lr_column ?= lr_columns->get_column( 'MANDT' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    ##NO_HANDLER   CATCH cx_salv_data_error cx_salv_not_found.
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'OBJNR' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    ##NO_HANDLER    CATCH cx_salv_data_error cx_salv_object_not_found cx_salv_not_found.
  ENDTRY.

*... set expand column
  TRY.
      lr_columns->set_expand_column( 'EXPAND' ).
    ##NO_HANDLER    CATCH cx_salv_data_error cx_salv_object_not_found cx_salv_not_found.
  ENDTRY.

*... set items expanded
  TRY.
      lr_level = gr_hierseq->get_level( 1 ).
    ##NO_HANDLER    CATCH cx_salv_data_error cx_salv_not_found.
  ENDTRY.
  lr_level->set_items_expanded( ).

*... *** SLAVE Settings ***
  TRY.
      lr_columns = gr_hierseq->get_columns( 2 ).
    ##NO_HANDLER    CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found.
  ENDTRY.

*... optimize the columns
  lr_columns->set_optimize( abap_true ).

* Set column
  PERFORM f_detail_column_settings USING lr_columns.

  TRY.
      lr_column ?= lr_columns->get_column( 'OBJNR' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    ##NO_HANDLER    CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found.
  ENDTRY.
  TRY.
      lr_column ?= lr_columns->get_column( 'GJAHR' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    ##NO_HANDLER    CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found.
  ENDTRY.

*... *** GENERAL Settings ***
*... §6 register to the events of cl_salv_hierseq_table

  lr_events = gr_hierseq->get_event( ).

  CREATE OBJECT gr_events.

*... §6.1 register to the event USER_COMMAND
  SET HANDLER gr_events->on_user_command FOR lr_events.
  SET HANDLER gr_events->on_top_of_page FOR lr_events.

  TRY.
      lr_selections = gr_hierseq->get_selections( 1 ).
    ##NO_HANDLER   CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found.
  ENDTRY.

*... §7.1 set selection mode
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

  l_title = TEXT-t01.
  lr_display_settings = gr_hierseq->get_display_settings( ).
  lr_display_settings->set_list_header( l_title ).

*... §7 display the table
  gr_hierseq->display( ).

ENDFORM.                    "display_hierseq
*&---------------------------------------------------------------------*
*&      Form  f_detail_column_settings
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_detail_column_settings USING ir_columns
                              TYPE REF TO cl_salv_columns_hierseq.

  DATA: lr_column TYPE REF TO cl_salv_column_hierseq,
        lf_s_text TYPE scrtext_s,
        lf_m_text TYPE scrtext_m,
        lf_l_text TYPE scrtext_l.

  lf_s_text = lf_m_text = lf_l_text = TEXT-c01.
  TRY.
      lr_column ?= ir_columns->get_column( 'ACTUAL' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER    CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.

  ENDTRY.
*
  lf_s_text = lf_m_text = lf_l_text = TEXT-c02.
  TRY.
      lr_column ?= ir_columns->get_column( 'COMMIT' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER  CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.

  ENDTRY.
*
  lf_s_text = lf_m_text = lf_l_text = TEXT-c03.
  TRY.
      lr_column ?= ir_columns->get_column( 'ASSIGN' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER   CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.

  ENDTRY.

  lf_s_text = lf_m_text = lf_l_text = TEXT-c04.
  TRY.
      lr_column ?= ir_columns->get_column( 'PLAN' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER  CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.

  ENDTRY.
*
  lf_s_text = lf_m_text = lf_l_text = TEXT-c05.
  TRY.
      lr_column ?= ir_columns->get_column( 'AVAI' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.

  ENDTRY.

ENDFORM.                    " set_columns_technical(
*&---------------------------------------------------------------------*
*&      Form  get_selections
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_selections .

  DATA: lr_selections TYPE REF TO cl_salv_selections,
        lt_rows       TYPE salv_t_row.

  TRY.
      lr_selections = gr_hierseq->get_selections( 1 ).
    ##NO_HANDLER    CATCH cx_salv_not_found.
  ENDTRY.

  lt_rows = lr_selections->get_selected_rows( ).

  IF lines( lt_rows ) EQ 0.
    MESSAGE TEXT-006 TYPE 'E'.
  ENDIF.
*
  PERFORM f_check_version USING gc_versn_000.
  PERFORM f_check_version USING gc_versn_z05.
  PERFORM f_check_version_compare USING gc_versn_000 gc_versn_z05.

  PERFORM f_posting TABLES lt_rows.

  PERFORM f_display_list.

ENDFORM.                    " get_selections
*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_user_command USING i_ucomm TYPE salv_de_function.

  CASE i_ucomm.
    WHEN 'POST'.
      PERFORM get_selections.
  ENDCASE.

ENDFORM.                    " handle_user_command
*&---------------------------------------------------------------------*
*&      Form  create_alv_form_content_top
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_alv_form_content_top
  CHANGING cr_content     TYPE REF TO cl_salv_form_element.

  DATA: lr_grid   TYPE REF TO cl_salv_form_layout_grid,
        lr_grid_1 TYPE REF TO cl_salv_form_layout_grid,
        lr_label  TYPE REF TO cl_salv_form_label,
        lr_text   TYPE REF TO cl_salv_form_text,
        l_text    TYPE string.

*... create a grid
  CREATE OBJECT lr_grid.

  l_text = sy-title.
  lr_grid->create_header_information(
    row     = 1
    column  = 1
    text    = l_text
    tooltip = l_text ).

*... add a row to the grid -> row 2
  lr_grid->add_row( ).

*... in the cell [3,1] create a grid
  lr_grid_1 = lr_grid->create_grid(
    row    = 3
    column = 1 ).

*... in the cell [1,1] of the second grid create a label
  l_text = |{ TEXT-002 }:|.
  lr_label = lr_grid_1->create_label(
    row     = 1
    column  = 1
    text    = l_text
    tooltip = space ).

*... in the cell [1,2] of the second grid create a text
  CASE abap_true.
    WHEN rb_1.
      l_text = TEXT-003.
    WHEN rb_2.
      l_text = TEXT-004.
  ENDCASE.

  lr_text = lr_grid_1->create_text(
    row     = 1
    column  = 2
    text    = l_text
    tooltip = space ).

  l_text = |{ TEXT-005 }:|.
  lr_label = lr_grid_1->create_label(
    row     = 2
    column  = 1
    text    = l_text
    tooltip = space ).
  CASE abap_true.
    WHEN rb_1.
      l_text = p_versn.
    WHEN rb_2.
      l_text = '0'.
  ENDCASE.

  lr_text = lr_grid_1->create_text(
    row     = 2
    column  = 2
    text    = l_text
    tooltip = space ).

  l_text = |{ TEXT-007 }:|.
  lr_label = lr_grid_1->create_label(
    row     = 3
    column  = 1
    text    = l_text
    tooltip = space ).

  l_text = p_gjahr.
  lr_text = lr_grid_1->create_text(
    row     = 3
    column  = 2
    text    = l_text
    tooltip = space ).


  lr_label->set_label_for( lr_text ).

*... content is the top grid
  cr_content = lr_grid.

ENDFORM.                    " create_alv_form_content_top
*&---------------------------------------------------------------------*
*& Form f_reverse_sign
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_COSP
*&---------------------------------------------------------------------*
FORM f_reverse_sign
##NEEDED                    CHANGING cs_cosp TYPE ty_cosp.
  ##NEEDED
  CONSTANTS: lc_num    TYPE numc2 VALUE 12.
  DATA: lf_from   TYPE poper,
        lf_target TYPE c LENGTH 20.

  DO lc_num TIMES.
    lf_from = lf_from + 1.
    lf_target = |CS_COSP-WOG{ lf_from }|.
    ASSIGN (lf_target) TO FIELD-SYMBOL(<l_target>).
    IF sy-subrc EQ 0.
      <l_target> = <l_target> * -1.
    ENDIF.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_cosp_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_SLAVE
*&      --> LC_ACTUAL
*&      --> <L_AUFK>
*&      --> <L_COSP>
*&---------------------------------------------------------------------*
FORM f_get_cosp_data  TABLES   ut_slave TYPE gty_slave
                      USING    uf_column TYPE char20
                               us_aufk TYPE g_type_s_master
                               us_cosp TYPE ty_cosp.

  CONSTANTS: lc_num TYPE poper VALUE 12.
  DATA: ls_slave TYPE g_type_s_slave.

  DATA:
    lf_source TYPE c LENGTH 20,
    lf_target TYPE c LENGTH 20,
    lf_period TYPE poper.

  CLEAR ls_slave.

  MOVE-CORRESPONDING us_aufk TO ls_slave.

  DO lc_num TIMES.

    lf_period = lf_period + 1.

    lf_source = |US_COSP-WOG{ lf_period }|.
    lf_target = |LS_SLAVE-{ uf_column }|.

    ASSIGN (lf_source) TO FIELD-SYMBOL(<l_source>).
    IF sy-subrc EQ 0.
      CHECK <l_source> IS NOT INITIAL.
      ASSIGN (lf_target) TO FIELD-SYMBOL(<l_target>).
      IF sy-subrc EQ 0.
        <l_target> = <l_source>.
        ls_slave-poper = lf_period.
        ls_slave-gjahr = p_gjahr.
        ls_slave-kstar = us_cosp-kstar.
        ls_slave-waers = 'THB'.

        SELECT SINGLE txt20 INTO ls_slave-txt20  ##WARN_OK
          FROM skat
          WHERE spras EQ sy-langu
            AND ktopl EQ 'RCOA'
            AND saknr EQ ls_slave-kstar.

        COLLECT ls_slave INTO ut_slave.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_detail_column_settings_rept
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_COLUMNS
*&---------------------------------------------------------------------*
FORM f_detail_column_settings_rept  USING ur_columns TYPE REF TO cl_salv_columns_table.
  DATA: lr_column TYPE REF TO cl_salv_column_table,
        lf_s_text TYPE scrtext_s,
        lf_m_text TYPE scrtext_m,
        lf_l_text TYPE scrtext_l.

  TRY.
      lr_column ?= ur_columns->get_column( 'OBJNR' ).
      lr_column->set_technical( abap_true ).
    CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found. "#EC NO_HANDLER
  ENDTRY.


  lf_s_text = lf_m_text = lf_l_text = TEXT-c01.
  TRY.
      lr_column ?= ur_columns->get_column( 'ACTUAL' ).
      lr_column->set_currency_column( value = 'WAERS' ).

      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER   CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.
  ENDTRY.
*
  lf_s_text = lf_m_text = lf_l_text = TEXT-c02.
  TRY.
      lr_column ?= ur_columns->get_column( 'COMMIT' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER    CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.
  ENDTRY.
*
  lf_s_text = lf_m_text = lf_l_text = TEXT-c03.
  TRY.
      lr_column ?= ur_columns->get_column( 'ASSIGN' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER   CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.
  ENDTRY.

  lf_s_text = lf_m_text = lf_l_text = TEXT-c04.
  TRY.
      lr_column ?= ur_columns->get_column( 'PLAN' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER   CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.
  ENDTRY.
*
  lf_s_text = lf_m_text = lf_l_text = TEXT-c05.
  TRY.
      lr_column ?= ur_columns->get_column( 'AVAI' ).
      lr_column->set_currency_column( value = 'WAERS' ).
      lr_column->set_short_text( value = lf_s_text ).
      lr_column->set_medium_text( value = lf_m_text ).
      lr_column->set_long_text( value = lf_l_text ).
    ##NO_HANDLER  CATCH cx_salv_wrong_call cx_salv_msg cx_salv_object_not_found cx_salv_not_found cx_salv_data_error.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_commitment_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <L_MASTER>
*&      <-- LF_SUBRC
*&---------------------------------------------------------------------*
FORM f_commitment_check  USING   us_master TYPE g_type_s_master
                         CHANGING cf_subrc TYPE sysubrc.

  LOOP AT gt_slave TRANSPORTING NO FIELDS WHERE aufnr EQ us_master-aufnr
                                            AND commit GT 0.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    APPEND INITIAL LINE TO gt_result ASSIGNING FIELD-SYMBOL(<l_result>).
    MOVE-CORRESPONDING us_master TO <l_result>.
    <l_result>-icon = icon_red_light.
    <l_result>-msgtx = TEXT-c06.
    REPLACE '&1' WITH us_master-aufnr INTO <l_result>-msgtx.
    cf_subrc = 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_list
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_list .
  DATA: lr_table         TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_true
        IMPORTING
          r_salv_table = lr_table
        CHANGING
          t_table      = gt_result ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

*... §3 Functions
*... §3.1 activate ALV generic Functions
  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = lr_table->get_functions( ).
  lr_functions->set_default( abap_true ).

*... set the columns technical
  DATA: lr_columns TYPE REF TO cl_salv_columns,
        lr_column  TYPE REF TO cl_salv_column_table.

  lr_columns = lr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

*...
  TRY.
      lr_column ?= lr_columns->get_column( 'ICON' ).
      lr_column->set_icon( if_salv_c_bool_sap=>true ).
      lr_column->set_long_text( 'Status'(017) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'AUFEX' ).
      lr_column->set_long_text( 'E-Memo'(014) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'LOGMSG' ).
      lr_column->set_long_text( 'Log'(015) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'MSGTX' ).
      lr_column->set_long_text( 'Message'(016) ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

*... §4 display the table
  lr_table->display( ).

ENDFORM.
