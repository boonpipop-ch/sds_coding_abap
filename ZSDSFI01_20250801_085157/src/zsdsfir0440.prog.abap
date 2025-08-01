*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0440
*  Creation Date      : 22.08.2024
*  Author             : Atitep B(Eviden)
*  Add-on ID          : N/A
*  Description        : Program Post Advance Received
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
* 14.01.2025  F36K911030  Apichat Ch.  420000242 : Fix missing update column
*-----------------------------------------------------------------------

REPORT zsdsfir0440.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zsdsfit045.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF gty_output.
         INCLUDE TYPE zsdsfis144.
TYPES:  msg(200),
       END OF gty_output,

       BEGIN OF gty_t012k,
         bukrs TYPE t012k-bukrs,
         hbkid TYPE t012k-hbkid,
         hktid TYPE t012k-hktid,
         hkont TYPE t012k-hkont,
       END OF gty_t012k,

       BEGIN OF gty_action_status,
         seq         TYPE zsdsde_param_seq,
         action_type TYPE zsdsfis144-action_type,
         status      TYPE zsdsfis144-status,
       END OF gty_action_status.

TYPES:
  tt_action_status TYPE STANDARD TABLE OF gty_action_status WITH EMPTY KEY,
  "New entry
  tt_output_new    TYPE STANDARD TABLE OF gty_output WITH EMPTY KEY, "ZSDSFIS144.
  tt_t012k         TYPE STANDARD TABLE OF gty_t012k.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: gt_output_new    TYPE tt_output_new                         ##NEEDED,
      gt_t012k         TYPE tt_t012k                              ##NEEDED,

      gt_genc          TYPE zcl_sdsca_utilities=>tt_gen_c         ##NEEDED,
      gt_action_status TYPE tt_action_status                      ##NEEDED,

      gt_ftpost        TYPE TABLE OF ftpost ##NEEDED,
      gt_fttax         TYPE TABLE OF fttax ##NEEDED,
      gt_blntab        TYPE TABLE OF blntab ##NEEDED,
      gs_ftpost        TYPE ftpost ##NEEDED,
      gf_count         TYPE i ##NEEDED.
*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA: gv_edit           TYPE abap_bool               ##NEEDED,
      gf_path           TYPE char255                 ##NEEDED,
      gf_post_dated_chk TYPE bsed-portf    ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  BEGIN OF gc_func,
    del       TYPE ui_func  VALUE 'DEL',
    sel_a     TYPE sy-ucomm VALUE 'SEL_ALL',
    sel_n     TYPE sy-ucomm VALUE 'SEL_NONE',
    save      TYPE ui_func  VALUE 'SAVE',

    del_new   TYPE ui_func  VALUE 'DEL_N',
    sel_a_new TYPE sy-ucomm VALUE 'SEL_ALL_N',
    sel_n_new TYPE sy-ucomm VALUE 'SEL_NONE_N',
    post      TYPE sy-ucomm VALUE 'POST',
    man_exe   TYPE sy-ucomm VALUE 'MANUAL_POST',
    sim       TYPE sy-ucomm VALUE 'SIM',
    rej       TYPE sy-ucomm VALUE 'REJ',
  END OF gc_func.

CONSTANTS:
  gc_structure_new TYPE  tabname  VALUE 'ZSDSFIS144',
  gc_header_height TYPE  i                VALUE 16,
  gc_alv_height    TYPE  i                VALUE 90,

  BEGIN OF gc_genc_param,
    action_status  TYPE zsdsde_param_name VALUE 'DOCUMENT_ACTION_STATUS',
    post_dated_chk TYPE zsdsde_param_name VALUE 'POST_DATED_CHECK',
  END OF gc_genc_param,

  BEGIN OF gc_genc_param_ext,
    action_type TYPE zsdsde_param_ext VALUE 'ACTION_TYPE',
    status      TYPE zsdsde_param_ext VALUE 'STATUS',
  END OF gc_genc_param_ext,

  BEGIN OF gc_reject,
    action TYPE zsdsfit045-action_type VALUE '18',
    status TYPE zsdsfit045-status      VALUE '10',
  END OF gc_reject,

  BEGIN OF  gc_block_status,
    block   TYPE zsdsfit045-block_status VALUE 'B',
    approve TYPE zsdsfit045-block_status VALUE 'A',
  END OF  gc_block_status,

  gc_true TYPE  char1     VALUE 'X'.
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bn1 WITH FRAME TITLE TEXT-s01.

  SELECT-OPTIONS:
    s_kunnrn   FOR  zsdsfit045-kunnr OBLIGATORY,
    s_bldat    FOR  zsdsfit045-bldat,
    s_trnfn    FOR  zsdsfit045-tranf_no.

SELECTION-SCREEN END OF BLOCK bn1.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_constant.
  PERFORM f_get_new_entry_all.
  PERFORM f_lock_tran_new_entry.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM f_display_result_new USING gt_output_new.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE zsdsfir0440_alv ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
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
    INNER JOIN @gt_action_status AS sta
    ON  log~action_type = sta~action_type
    AND log~status      = sta~status
    WHERE log~bldat       IN @s_bldat
    AND   log~kunnr       IN @s_kunnrn
*    AND   LOG~XBLNR       IN @S_XBLNRN
    AND   log~tranf_no    IN @s_trnfn
    AND   log~delete_flag = ''
    AND   log~belnr    = ''
    AND   log~gjahr   = ''
    AND   log~block_status <> @gc_block_status-block
    INTO CORRESPONDING FIELDS OF TABLE @gt_output_new ##TOO_MANY_ITAB_FIELDS.

  IF gt_output_new IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e00. "No data found
  ELSE.
*<-- Start of Insertion 06.12.2024 (Get Additional Data)
    LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<l_output>).
      PERFORM f_get_prctr_txt  USING  <l_output>-prctr
                             CHANGING <l_output>-prctr_txt.
    ENDLOOP.
*--> End of Insertion 06.12.2024
    SELECT a~bukrs,
           a~hbkid,
           a~hktid,
           a~hkont
      FROM t012k AS a
      INNER JOIN @gt_output_new AS b
      ON  a~bukrs = b~bukrs
      AND a~hbkid = b~hbkid
      AND a~hktid = b~hktid
      GROUP BY a~bukrs, a~hbkid, a~hktid, a~hkont
      INTO TABLE @gt_t012k.
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

* Set Container name
  gf_container = 'CTL_ALV'.
* Disable Header area
  gf_alv_header = gc_true.

*   No auto refresh in edit mode
  gf_no_auto_refresh = gc_true.
  gf_alv_header = space.

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
* Sort data
*  PERFORM f_alv_sort_result_new CHANGING gt_sort.
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
        <l_fieldcat>-edit       = gc_true."GV_EDIT.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-col_pos    = 1.
        <l_fieldcat>-key        = gc_true.
      WHEN 'BELNR'.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'INC_AMT'.
        <l_fieldcat>-reptext    = TEXT-f01.
        <l_fieldcat>-seltext    = TEXT-f01.
        <l_fieldcat>-scrtext_s  = TEXT-f01.
        <l_fieldcat>-scrtext_m  = TEXT-f01.
        <l_fieldcat>-scrtext_l  = TEXT-f01.

      WHEN 'ATTACH_FILE_FLAG'.
        <l_fieldcat>-style = cl_gui_alv_grid=>mc_style_button.

*<-- Start of Insertion 06.12.2024 (Add Profit Center Desc)
      WHEN 'PRCTR_TXT'.
*       Text-f02: Profit Center Desc.
        <l_fieldcat>-reptext    = TEXT-f02.
        <l_fieldcat>-seltext    = TEXT-f02.
        <l_fieldcat>-scrtext_s  = TEXT-f02.
        <l_fieldcat>-scrtext_m  = TEXT-f02.
        <l_fieldcat>-scrtext_l  = TEXT-f02.
*--> End of Insertion 06.12.2024
      WHEN 'ERNAM' OR 'ERDAT' OR 'ERZMT' OR
           'REL_BLK_BY' OR 'REL_BLK_ON' OR 'REL_BLK_TIME'.
        <l_fieldcat>-tech = gc_true.
    ENDCASE.
  ENDLOOP.

  APPEND INITIAL LINE TO ct_fieldcat ASSIGNING <l_fieldcat>.
  <l_fieldcat>-col_pos = '99'.
  <l_fieldcat>-fieldname = 'MSG' ##NO_TEXT.
  <l_fieldcat>-reptext   = 'Message' ##NO_TEXT.
  <l_fieldcat>-coltext   = 'Message' ##NO_TEXT.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_top_of_page
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM f_top_of_page USING uref_dyndoc_id  TYPE  REF TO cl_dd_document. "#EC CALLED

  CONSTANTS:
    lc_size_key TYPE  sdydo_value  VALUE '40%',
    lc_size_val TYPE  sdydo_value  VALUE '60%'.

  DATA:
*    LV_TOTAL     TYPE  ZSDSFIT029-PAYIN_AMT,
*    LV_REMAIN    TYPE  ZSDSFIT029-PAYIN_AMT,
    lf_text      TYPE  sdydo_text_element,
    lref_table   TYPE  REF TO cl_dd_table_element,
    lref_col_key TYPE  REF TO cl_dd_area,
    lref_col_val TYPE  REF TO cl_dd_area ##NEEDED.

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

*  CASE gv_data_type.
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


**-----------------------
** Add value in Line2
**-----------------------
*      CALL METHOD lref_table->new_row.
*
** Text-h01 : Collector Name
*      lf_text = TEXT-h02.
*      CALL METHOD lref_col_key->add_text
*        EXPORTING
*          text = lf_text.
*
*      lf_text = |{ p_pernrn ALPHA = OUT } { gv_fullname }|.
*      CALL METHOD lref_col_val->add_text
*        EXPORTING
*          text = lf_text.
*
*      LOOP AT gt_output_new INTO DATA(ls_output_new) WHERE sel = gc_true  ##INTO_OK.
*        lv_total += ls_output_new-wrbtr.
*      ENDLOOP.
*      lv_remain = p_rcvamn - lv_total.
*
**-----------------------
** Add value in Line3
**-----------------------
*      IF p_rcvamn IS NOT INITIAL.
*        CALL METHOD lref_table->new_row.
*
** Text-h04 : Received Amount
*        lf_text = |{ TEXT-h04 }|.
*        CALL METHOD lref_col_key->add_text
*          EXPORTING
*            text = lf_text.
*
*        WRITE p_rcvamn CURRENCY gv_waers TO lf_text.
*        CALL METHOD lref_col_val->add_text
*          EXPORTING
*            text = lf_text.
*
*      ENDIF.
*
**-----------------------
** Add value in Line4
**-----------------------
*      CALL METHOD lref_table->new_row.
*
** Text-h05 : Assigned Amount
*      lf_text = |{ TEXT-h05 }|.
*      CALL METHOD lref_col_key->add_text
*        EXPORTING
*          text = lf_text.
*
**  LF_TEXT = |{ LV_TOTAL CURRENCY = GV_WAERS  } |.
*      WRITE lv_total CURRENCY gv_waers TO lf_text.
*      CALL METHOD lref_col_val->add_text
*        EXPORTING
*          text = lf_text.
*
**-----------------------
** Add value in Line5
**-----------------------
*      IF p_rcvamn IS NOT INITIAL.
*
*        CALL METHOD lref_table->new_row.
*
** Text-h05 : Balance
*        lf_text = |{ TEXT-h06 }|.
*        CALL METHOD lref_col_key->add_text
*          EXPORTING
*            text = lf_text.
*
**    LF_TEXT = |{ LV_REMAIN CURRENCY = GV_WAERS } |.
*        WRITE lv_remain CURRENCY gv_waers TO lf_text.
*        CALL METHOD lref_col_val->add_text
*          EXPORTING
*            text = lf_text.
*
*      ENDIF.
*
*
**  ENDCASE.
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
*  IF gv_edit EQ gc_true.
*----------------------------------------------------------------------*
* New entry
*----------------------------------------------------------------------*

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.   "separator
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  "Select All
  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '0'.   "normal Button
  ls_toolbar-function = gc_func-sel_a_new.   "fcode
  ls_toolbar-icon = '@4B@'.
  ls_toolbar-quickinfo = 'Select All'  ##SHARE_OK ##NO_TEXT.
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  "Select None
  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '0'.   "normal Button
  ls_toolbar-function = gc_func-sel_n_new.   "fcode
  ls_toolbar-icon = '@4D@'.
  ls_toolbar-quickinfo = 'Select None'  ##SHARE_OK ##NO_TEXT.
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  "Simulation
  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '0'.   "normal Button
  ls_toolbar-function = gc_func-sim.   "fcode
  ls_toolbar-icon = '@15@'.
  ls_toolbar-text = 'Simulate' ##NO_TEXT.
  ls_toolbar-quickinfo = 'Simulate'  ##SHARE_OK ##NO_TEXT.
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  "Post Doc
  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '0'.   "normal Button
  ls_toolbar-function = gc_func-post.   "fcode
  ls_toolbar-icon = '@15@'.
  ls_toolbar-text = 'Post Doc' ##NO_TEXT.
  ls_toolbar-quickinfo = 'Post Doc'  ##SHARE_OK ##NO_TEXT.
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  " Manual Adjust
  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '0'.   "normal Button
  ls_toolbar-function = gc_func-man_exe.   "fcode
  ls_toolbar-icon = '@15@'.
  ls_toolbar-text = 'Manual Adjust' ##NO_TEXT.
  ls_toolbar-quickinfo = 'Manual Adjust' ##SHARE_OK ##NO_TEXT.
  APPEND ls_toolbar TO uref_object->mt_toolbar.


  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.   "separator
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  "Reject
  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '0'.   "normal Button
  ls_toolbar-function = gc_func-rej.   "fcode
  ls_toolbar-icon = '@8Y@'.
  ls_toolbar-text = 'Reject' ##NO_TEXT.
  ls_toolbar-quickinfo = 'Reject'  ##SHARE_OK ##NO_TEXT.
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  DELETE uref_object->mt_toolbar WHERE function EQ '&CHECK'.
  DELETE uref_object->mt_toolbar WHERE function EQ '&REFRESH'.
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
  DELETE uref_object->mt_toolbar WHERE function EQ '&PRINT_BACK'.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM f_user_command USING uf_ucomm  TYPE  sy-ucomm ##CALLED.

  DATA:
*    LV_VALID   TYPE CHAR01,
*    LV_REFRESH TYPE CHAR01,
    lv_tran_mode TYPE rfpdo-allgazmd,
    lv_sel       TYPE char01.

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

    WHEN gc_func-sim.
      lv_tran_mode = 'N'. "Processing with display of the screens

      PERFORM post_doc USING 'X' lv_tran_mode.

      gref_grid->refresh_table_display( ).

*      PERFORM F_UNLOCK_TRAN_NEW.
    WHEN gc_func-post.
      lv_tran_mode = 'N'. "Processing with display of the screens

      PERFORM post_doc USING '' lv_tran_mode.

      gref_grid->refresh_table_display( ).

    WHEN gc_func-man_exe.
      lv_tran_mode = 'A'. "Processing with display of the screens

      PERFORM post_doc USING '' lv_tran_mode.

      gref_grid->refresh_table_display( ).

    WHEN gc_func-rej.
      PERFORM reject_entry.
      gref_grid->refresh_table_display( ).

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
*& Form post_doc
*&---------------------------------------------------------------------*
FORM post_doc  USING uf_test TYPE char01
                     uf_proc_mode TYPE rfpdo-allgazmd.

  DATA: lt_zsdsfit045     TYPE TABLE OF zsdsfit045,
        lt_zsdsfit045_tmp TYPE TABLE OF zsdsfit045 WITH EMPTY KEY.

  DATA: lv_amt        TYPE bseg-dmbtr,
        lf_subrc      TYPE sy-subrc ##NEEDED,
        lf_msgid      TYPE sy-msgid,
        lf_msgno      TYPE sy-msgno,
        lf_msgty      TYPE sy-msgty ##NEEDED,
        lf_msgv1      TYPE sy-msgv1,
        lf_msgv2      TYPE sy-msgv2,
        lf_msgv3      TYPE sy-msgv3,
        lf_msgv4      TYPE sy-msgv4,
        lf_count      TYPE i,
        lt_genc_exp   TYPE zcl_sdsca_utilities=>tt_gen_c,
        lt_genc_inc   TYPE zcl_sdsca_utilities=>tt_gen_c,
        lt_genc_bnk   TYPE zcl_sdsca_utilities=>tt_gen_c,
        ls_genc       TYPE zcl_sdsca_utilities=>ts_gen_c,
        ls_ftpost     TYPE ftpost,
        lf_hkont_exp  TYPE bseg-hkont,
        lf_hkont_inc  TYPE bseg-hkont,
        lf_hkont_bnk  TYPE bseg-hkont,
*        lf_param_ext  TYPE zsdscac001-param_ext,
        lf_linep(110).

  DATA: lf_mode     TYPE  rfpdo-allgazmd,
        lf_date(10).

  FIELD-SYMBOLS <l_t012k> TYPE gty_t012k.

  lf_mode = uf_proc_mode.

  LOOP AT gt_genc INTO ls_genc WHERE param_ext EQ 'EXPENSE' ##INTO_OK.
    IF ls_genc-param EQ 'HKONT'.
      lf_hkont_exp = ls_genc-value_low.
    ELSE.
      APPEND ls_genc TO lt_genc_exp.
    ENDIF.

  ENDLOOP.

  LOOP AT gt_genc INTO ls_genc WHERE param_ext EQ 'INCOME'  ##INTO_OK.
    IF ls_genc-param EQ 'HKONT'.
      lf_hkont_inc = ls_genc-value_low.
    ELSE.
      APPEND ls_genc TO lt_genc_inc.
    ENDIF.

  ENDLOOP.

  LOOP AT gt_genc INTO ls_genc WHERE param_ext EQ 'BANK_FEE'  ##INTO_OK.
    IF ls_genc-param EQ 'HKONT'.
      lf_hkont_bnk = ls_genc-value_low.
    ELSE.
      APPEND ls_genc TO lt_genc_bnk.
    ENDIF.

  ENDLOOP.


  LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<l_output>) WHERE sel IS NOT INITIAL
                                                             AND belnr IS INITIAL
                                                             AND gjahr IS INITIAL.
    CLEAR: lf_count,
           lv_amt.
    "Header
    gs_ftpost-stype = 'K'.
    gs_ftpost-count =  1.
    WRITE <l_output>-bldat TO lf_date.
    PERFORM add_field USING 'BKPF-BLDAT' lf_date. "Document Date
    PERFORM add_field USING 'BKPF-BLART' <l_output>-blart.
    PERFORM add_field USING 'BKPF-BUKRS' <l_output>-bukrs.
    WRITE <l_output>-budat TO lf_date.
    PERFORM add_field USING 'BKPF-BUDAT' lf_date. "Posting Date
    PERFORM add_field USING 'BKPF-WAERS' <l_output>-waers.
    PERFORM add_field USING 'BKPF-XBLNR' <l_output>-xblnr. "Reference
    PERFORM add_field USING 'BKPF-BKTXT' <l_output>-bktxt. "Doc.Header Text
    PERFORM add_field USING 'BKPF-BRNCH' <l_output>-bupla.

*    IF <l_output>-umskz = 'E'.
*      PERFORM add_field USING 'RF05A-PORTF' gf_post_dated_chk.
*    ENDIF.

    "Item
    gs_ftpost-stype = 'P'.
*item 1
    lv_amt = <l_output>-wrbtr + <l_output>-exp_amt + <l_output>-bank_fee - <l_output>-inc_amt.

*    ADD 1 TO lf_count.
    lf_count += 1.
    gs_ftpost-count = lf_count.
    PERFORM add_field USING 'RF05A-NEWBS' '19'.
    PERFORM add_field USING 'RF05A-NEWKO' <l_output>-kunnr.
    PERFORM add_field USING 'RF05A-NEWUM' <l_output>-umskz.
*    PERFORM add_field USING 'BSEG-WRBTR'  <l_output>-wrbtr.
    PERFORM add_field USING 'BSEG-WRBTR'  lv_amt.
    PERFORM add_field USING 'BSEG-MWSKZ' <l_output>-mwsk1.
    IF <l_output>-prctr IS NOT INITIAL.
      PERFORM add_field USING 'BSEG-PRCTR' <l_output>-prctr.
    ENDIF.
    PERFORM add_field USING 'BSEG-SGTXT' <l_output>-sgtxt.
    PERFORM add_field USING 'BSEG-XREF1' <l_output>-tranf_no.

    IF <l_output>-mwsk1 = 'O7'.
      PERFORM add_field USING 'RF05A-XMWST' gc_true.
*      PERFORM add_field USING 'BKPF-XMWST' gc_true.
    ENDIF.

*item 2
*    ADD 1 TO lf_count.
    lf_count += 1.
    gs_ftpost-count = lf_count.
* Posting key for each case.
    CASE <l_output>-umskz.
      WHEN 'A'.
        IF <l_output>-hbkid IS INITIAL.
* Down payment - AR Inv
          PERFORM add_field USING 'RF05A-NEWBS' '01'.
          PERFORM add_field USING 'RF05A-NEWKO' <l_output>-kunnr.
          PERFORM add_field USING 'BSEG-ZTERM'  ' '.
        ELSE.
          PERFORM add_field USING 'RF05A-NEWBS' '40'.

          READ TABLE gt_t012k ASSIGNING <l_t012k>
          WITH KEY bukrs = <l_output>-bukrs
                   hbkid = <l_output>-hbkid
                   hktid = <l_output>-hktid.
          IF sy-subrc = 0.
            PERFORM add_field USING 'RF05A-NEWKO' <l_t012k>-hkont.
          ENDIF.
        ENDIF.

        PERFORM add_field USING 'BSEG-WRBTR' <l_output>-wrbtr.
        PERFORM add_field USING 'BSEG-SGTXT' <l_output>-sgtxt.
*        IF <l_output>-prctr IS NOT INITIAL.
*          PERFORM add_field USING 'COBL-PRCTR' <l_output>-prctr.
*        ENDIF.

      WHEN 'N' OR
           'E'.
        PERFORM add_field USING 'RF05A-NEWBS' '40'.

        READ TABLE gt_t012k ASSIGNING <l_t012k>
        WITH KEY bukrs = <l_output>-bukrs
                 hbkid = <l_output>-hbkid
                 hktid = <l_output>-hktid.
        IF sy-subrc = 0.
          PERFORM add_field USING 'RF05A-NEWKO' <l_t012k>-hkont.
        ENDIF.

        PERFORM add_field USING 'BSEG-WRBTR' <l_output>-wrbtr.
        PERFORM add_field USING 'BSEG-SGTXT' <l_output>-sgtxt.
        IF <l_output>-prctr IS NOT INITIAL.
          PERFORM add_field USING 'COBL-PRCTR' <l_output>-prctr.
        ENDIF.

*      WHEN 'E'.
*        PERFORM add_field USING 'RF05A-NEWBS' '09'.
*        PERFORM add_field USING 'RF05A-NEWUM' 'D'.
*        PERFORM add_field USING 'RF05A-NEWKO' <l_output>-kunnr.
*
*        PERFORM add_field USING 'BSEG-WRBTR' <l_output>-wrbtr.
*        PERFORM add_field USING 'BSEG-SGTXT' <l_output>-sgtxt.
*        WRITE <l_output>-zfbdt TO lf_date.
*        PERFORM add_field USING 'BSEG-ZFBDT' lf_date.
**        IF <l_output>-prctr IS NOT INITIAL.
**          PERFORM add_field USING 'COBL-PRCTR' <l_output>-prctr.
**        ENDIF.
*
*        PERFORM add_field USING 'BSED-BOENO' <l_output>-cheque_no.
*        PERFORM add_field USING 'BSED-BANK' <l_output>-hbkid.
*        PERFORM add_field USING 'BSED-ACCOU' <l_output>-hktid.
*        PERFORM add_field USING 'RF05A-PORTF' gf_post_dated_chk.

      WHEN OTHERS.
    ENDCASE.

*specially for down payment-AR item 3,4,5
*    IF <l_output>-umskz EQ 'A' AND <l_output>-hbkid IS INITIAL.

    IF <l_output>-exp_amt IS NOT INITIAL.
*      ADD 1 TO lf_count.
      lf_count += 1.
      gs_ftpost-count = lf_count.

      PERFORM add_field USING 'RF05A-NEWBS' '40'.
      PERFORM add_field USING 'RF05A-NEWKO' lf_hkont_exp.

      PERFORM add_field USING 'BSEG-WRBTR' <l_output>-exp_amt.
      PERFORM add_field USING 'BSEG-SGTXT' <l_output>-sgtxt.

      LOOP AT lt_genc_exp INTO ls_genc.
        ls_ftpost-fnam = |COBL-{ ls_genc-param }|.
        PERFORM add_field USING ls_ftpost-fnam ls_genc-value_low.
      ENDLOOP.
    ENDIF.

    IF <l_output>-inc_amt IS NOT INITIAL.
*      ADD 1 TO lf_count.
      lf_count += 1.
      gs_ftpost-count = lf_count.

      PERFORM add_field USING 'RF05A-NEWBS' '50'.
      PERFORM add_field USING 'RF05A-NEWKO' lf_hkont_inc.

      PERFORM add_field USING 'BSEG-WRBTR' <l_output>-inc_amt.
      PERFORM add_field USING 'BSEG-SGTXT' <l_output>-sgtxt.

      LOOP AT lt_genc_inc INTO ls_genc.
        ls_ftpost-fnam = |COBL-{ ls_genc-param }|.
        PERFORM add_field USING ls_ftpost-fnam ls_genc-value_low.
      ENDLOOP.

    ENDIF.

    IF <l_output>-bank_fee IS NOT INITIAL.
*      ADD 1 TO lf_count.
      lf_count += 1.
      gs_ftpost-count = lf_count.

      PERFORM add_field USING 'RF05A-NEWBS' '40'.
      PERFORM add_field USING 'RF05A-NEWKO' lf_hkont_bnk.

      PERFORM add_field USING 'BSEG-WRBTR' <l_output>-bank_fee.
      PERFORM add_field USING 'BSEG-SGTXT' <l_output>-sgtxt.

      LOOP AT lt_genc_bnk INTO ls_genc.
        ls_ftpost-fnam = |COBL-{ ls_genc-param }|.
        PERFORM add_field USING ls_ftpost-fnam ls_genc-value_low.
      ENDLOOP.
    ENDIF.

*    ENDIF.

    CALL FUNCTION 'Z_SDSFI_POSTING_INTERFACE_ST' ##FM_SUBRC_OK
*    CALL FUNCTION 'POSTING_INTERFACE_START' ##FM_SUBRC_OK
      EXPORTING
        i_function         = 'C'
        i_mode             = lf_mode
      EXCEPTIONS
        client_incorrect   = 1
        function_invalid   = 2
        group_name_missing = 3
        mode_invalid       = 4
        update_invalid     = 5
        user_invalid       = 6
        OTHERS             = 7.

    IF sy-subrc = 0.

      CALL FUNCTION 'Z_SDSFI_POSTING_INTERFACE_DOC'
*      CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT' ##FM_SUBRC_OK
        EXPORTING
          i_tcode                  = 'FB01'
          i_xsimu                  = uf_test
          i_sgfunct                = ' '
          i_no_auth                = ' '
        IMPORTING
          e_subrc                  = lf_subrc
          e_msgid                  = lf_msgid
          e_msgty                  = lf_msgty
          e_msgno                  = lf_msgno
          e_msgv1                  = lf_msgv1
          e_msgv2                  = lf_msgv2
          e_msgv3                  = lf_msgv3
          e_msgv4                  = lf_msgv4
        TABLES
          t_blntab                 = gt_blntab
          t_ftpost                 = gt_ftpost
          t_fttax                  = gt_fttax
        EXCEPTIONS
          account_missing          = 1
          company_code_missing     = 2
          posting_key_invalid      = 3
          posting_key_missing      = 4
          record_type_invalid      = 5
          transaction_code_invalid = 6
          amount_format_error      = 7
          too_many_line_items      = 8
          company_code_invalid     = 9
          screen_not_found         = 10
          no_authorization         = 11
          OTHERS                   = 12 ##FM_SUBRC_OK.

      READ TABLE gt_blntab INTO DATA(ls_blntab) INDEX 1.
      IF sy-subrc = 0.
        <l_output>-belnr = ls_blntab-belnr.
        <l_output>-gjahr = ls_blntab-gjahr.

        APPEND INITIAL LINE TO lt_zsdsfit045_tmp ASSIGNING FIELD-SYMBOL(<l_zsdsfit045_tmp>).
        <l_zsdsfit045_tmp>-uuid = <l_output>-uuid.
        <l_zsdsfit045_tmp>-belnr = ls_blntab-belnr.
        <l_zsdsfit045_tmp>-gjahr = ls_blntab-gjahr.
        <l_zsdsfit045_tmp>-update_by = sy-uname.
        <l_zsdsfit045_tmp>-update_on = sy-datum.
        <l_zsdsfit045_tmp>-update_time = sy-uzeit.

        <l_output>-msg = 'Successfully' ##NO_TEXT.

        PERFORM update_assignment USING ls_blntab-belnr
                                        ls_blntab-gjahr
                                  CHANGING <l_output>-msg.

      ELSE.
        IF uf_test = 'X' AND lf_msgty = 'S'.
          <l_output>-msg = 'Test Run Successfully' ##NO_TEXT.
        ELSE.
          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = lf_msgid
              lang      = sy-langu
              no        = lf_msgno
              v1        = lf_msgv1
              v2        = lf_msgv2
              v3        = lf_msgv3
              v4        = lf_msgv4
            IMPORTING
              msg       = lf_linep
            EXCEPTIONS ##FM_SUBRC_OK
              not_found = 1
              OTHERS    = 2.
          <l_output>-msg = lf_linep.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'Z_SDSFI_POSTING_INTERFACE_END' ##FM_SUBRC_OK
*      CALL FUNCTION 'POSTING_INTERFACE_END' ##FM_SUBRC_OK
        EXCEPTIONS
          session_not_processable = 1
          OTHERS                  = 2.

    ELSE.
      <l_output>-msg = 'Error open function post Doc.' ##NO_TEXT.
    ENDIF.

    CLEAR: lf_date, gt_ftpost[], gt_fttax[], gt_blntab[], gs_ftpost.
  ENDLOOP.

  IF lt_zsdsfit045_tmp[] IS NOT INITIAL.
    SELECT a~mandt,
           a~uuid,
           a~pernr,
           a~work_date,
           a~bukrs,
           a~xblnr,
           a~kunnr,
           a~umskz,
           a~bldat,
           a~budat,
           a~bktxt,
           a~sgtxt,
           a~wrbtr,
           a~waers,
           a~prctr,
           a~zfbdt,
           a~hbkid,
           a~hktid,
           a~mwsk1,
           a~bupla,
           a~blart,
           a~pymt_method,
           a~bank_date,
           a~cheque_no,
           a~tranf_no,
           a~exp_amt,
           a~inc_amt,
           a~bank_fee,
           a~action_type,
           a~status,
           a~delete_flag,
           a~delete_date,
           a~rejrsn,
           a~rejdat,
           a~ernam,
           a~erdat,
           a~erzmt,
           b~update_by,
           b~update_on,
           b~update_time,
           b~belnr,
           b~gjahr,
           a~attach_file_flag,
*<<F36K911030 - Start of ins
           a~attach_ext,
           a~block_status,
           a~rel_blk_by,
           a~rel_blk_on,
           a~rel_blk_time
*<<F36K911030 - End of ins
      FROM zsdsfit045 AS a
      INNER JOIN @lt_zsdsfit045_tmp AS b
      ON a~uuid = b~uuid
      INTO TABLE @lt_zsdsfit045 ##TOO_MANY_ITAB_FIELDS.
    IF sy-subrc = 0.
      MODIFY zsdsfit045 FROM TABLE lt_zsdsfit045[].
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_FIELD
*&---------------------------------------------------------------------*
FORM add_field  USING uf_fnam ##PERF_NO_TYPE
                      uf_fval ##PERF_NO_TYPE.
  APPEND INITIAL LINE TO gt_ftpost ASSIGNING FIELD-SYMBOL(<l_ftpost>).
  <l_ftpost>-stype = gs_ftpost-stype.
  <l_ftpost>-count = gs_ftpost-count.
  <l_ftpost>-fnam = uf_fnam.
  <l_ftpost>-fval = uf_fval.
  CONDENSE: <l_ftpost>-fnam, <l_ftpost>-fval.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_assignment
*&---------------------------------------------------------------------*
FORM update_assignment  USING fu_belnr TYPE belnr_d
                              fu_gjahr TYPE gjahr
                        CHANGING fu_msg TYPE char200.

  DATA: lt_accchg TYPE STANDARD TABLE OF accchg,
        lt_enq    TYPE STANDARD TABLE OF seqg3.

  DATA: ls_accchg TYPE accchg.
  DATA: lf_garg TYPE eqegraarg.

  DO 3 TIMES.
    SELECT SINGLE bukrs ##WARN_OK,
                  belnr,
                  gjahr,
                  buzei
      FROM bsid_view
      WHERE belnr = @fu_belnr
        AND gjahr = @fu_gjahr
      INTO @DATA(ls_bsid).
    IF sy-subrc = 0.
      ls_accchg-fdname = 'ZUONR'.
      ls_accchg-newval = fu_belnr.
      APPEND ls_accchg TO lt_accchg.

*       Check if there is a lock on the document before proceeding
      CALL FUNCTION 'ENQUEUE_READ'
        EXPORTING
          gclient               = sy-mandt
          gname                 = 'BKPF'
          garg                  = lf_garg
        TABLES
          enq                   = lt_enq
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          OTHERS                = 3.

      IF sy-subrc = 0 AND lt_enq IS INITIAL.
* Call THE FUNCTION MODULE TO UPDATE THE FI DOCUMENT
        CALL FUNCTION 'FI_DOCUMENT_CHANGE'
          EXPORTING
            i_bukrs      = ls_bsid-bukrs
            i_belnr      = ls_bsid-belnr
            i_gjahr      = ls_bsid-gjahr
            i_buzei      = ls_bsid-buzei
          TABLES
            t_accchg     = lt_accchg
          EXCEPTIONS
            no_reference = 1
            no_document  = 2
            OTHERS       = 3.
        IF sy-subrc <> 0.
          fu_msg = |{ fu_msg }/Can not update assignment| ##NO_TEXT.
        ENDIF.
      ELSE.
        fu_msg = |{ fu_msg }/Can not update assignment| ##NO_TEXT.
      ENDIF.

      EXIT.
    ELSE.
      WAIT UP TO 10 SECONDS.
    ENDIF.
  ENDDO.
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
*& Form reject_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM reject_entry .

  DATA: lt_line       TYPE tline_tab,
        lt_text       TYPE soli_tab,
        lt_text2      TYPE soli_tab,
        lt_recipients TYPE STANDARD TABLE OF string,
        lt_rej        TYPE zsdsfis144_tt,
        ls_line       TYPE tline,
*        ls_text       TYPE soli,
        ls_genc       TYPE zcl_sdsca_utilities=>ts_gen_c,
        lf_tdname     TYPE thead-tdname,
        lf_error      TYPE xflag,
        lf_subject    TYPE string,
        lf_subject2   TYPE string,
*        lf_ans        TYPE xflag,
        lv_cancel     TYPE char01,
        lf_sender     TYPE adr6-smtp_addr.

  FIELD-SYMBOLS <l_text> TYPE soli.

** Confirm to reject.
*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
*      titlebar              = TEXT-t01
*      text_question         = TEXT-t02
**     TEXT_BUTTON_1         = 'Ja'(001)
**     ICON_BUTTON_1         = ' '
**     TEXT_BUTTON_2         = 'Nein'(002)
**     ICON_BUTTON_2         = ' '
**     DEFAULT_BUTTON        = '1'
*      display_cancel_button = space
**     USERDEFINED_F1_HELP   = ' '
**     START_COLUMN          = 25
**     START_ROW             = 6
**     POPUP_TYPE            =
**     IV_QUICKINFO_BUTTON_1 = ' '
**     IV_QUICKINFO_BUTTON_2 = ' '
*    IMPORTING
*      answer                = lf_ans
** TABLES
**     PARAMETER             =
*    EXCEPTIONS
*      text_not_found        = 1
*      OTHERS                = 2 ##FM_SUBRC_OK.
*
*  IF lf_ans NE '1'.
*    RETURN.
*  ENDIF.

  lt_rej = VALUE #( BASE lt_rej FOR ls_rej IN gt_output_new
                                    WHERE ( sel   = gc_true AND
                                            belnr IS INITIAL AND
                                            gjahr IS INITIAL ) ( CORRESPONDING #( ls_rej ) ) ) .


  CALL FUNCTION 'Z_SDSFI_REJECT_REASON'
    IMPORTING
      ev_cancel   = lv_cancel
    CHANGING
      ct_coll_log = lt_rej.

  IF lv_cancel IS NOT INITIAL.
    RETURN.
  ENDIF.

  LOOP AT gt_genc INTO ls_genc WHERE param_ext EQ 'EMAIL' ##INTO_OK.
    CASE ls_genc-param.
      WHEN 'SENDER'.
        lf_sender = ls_genc-value_low.
      WHEN 'BODY'.
        lf_tdname = ls_genc-value_low.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

* Get email text
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'ST'
      language                = sy-langu
      name                    = lf_tdname
      object                  = 'TEXT'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*     USE_OLD_PERSISTENCE     = ABAP_FALSE
*   IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = lt_line
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE i000(38) WITH TEXT-e01 TEXT-e02 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE lt_line INTO ls_line INDEX 1.
  IF sy-subrc EQ 0.
    lf_subject = ls_line-tdline.
  ENDIF.

  LOOP AT lt_line INTO ls_line FROM 3.
    REPLACE ALL OCCURRENCES OF '<(>' IN ls_line-tdline WITH space.
    REPLACE ALL OCCURRENCES OF '<)>' IN ls_line-tdline WITH space.
    CASE ls_line-tdformat.
      WHEN '*'.
        APPEND INITIAL LINE TO lt_text ASSIGNING <l_text>.
        <l_text>-line = ls_line-tdline.
      WHEN '='.
        <l_text>-line = |{ <l_text>-line }{ ls_line-tdline }|.
*        CONCATENATE <l_text>-line ls_line-tdline INTO <l_text>-line.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

*  LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<l_output>) WHERE sel IS NOT INITIAL
*                                                             AND belnr IS INITIAL
*                                                             AND gjahr IS INITIAL.

  LOOP AT lt_rej INTO DATA(ls_reject) ##INTO_OK.
    READ TABLE gt_output_new ASSIGNING FIELD-SYMBOL(<l_output>)
      WITH KEY uuid = ls_reject-uuid.

    <l_output>-action_type = gc_reject-action.
    <l_output>-status      = gc_reject-status.
    <l_output>-rejrsn      = ls_reject-rejrsn.
    <l_output>-rejdat      = sy-datum.

    UPDATE zsdsfit045 SET action_type = @<l_output>-action_type,
                          status      = @<l_output>-status,
                          rejrsn      = @<l_output>-rejrsn,
                          rejdat      = @<l_output>-rejdat
                      WHERE uuid = @<l_output>-uuid.

    lf_subject2 = lf_subject.
    lt_text2 = lt_text.

    REPLACE '&TRANF_NO&' INTO lf_subject2 WITH <l_output>-tranf_no.
    PERFORM set_mail_text USING <l_output> CHANGING lt_text2.
    PERFORM set_mail_recipients USING <l_output> CHANGING lt_recipients.

    IF lt_recipients IS INITIAL.
* set error.
      <l_output>-msg = TEXT-e03.
      CONTINUE.
    ELSE.
      PERFORM send_mail USING lt_text2 lf_sender lt_recipients lf_subject2
            CHANGING lf_error.
      IF lf_error EQ gc_true.
        <l_output>-msg = TEXT-e04.
      ENDIF.
    ENDIF.

    <l_output>-msg = TEXT-e05.

    CALL FUNCTION 'DEQUEUE_EZSDSFIS144'
      EXPORTING
*       MODE_ZSDSFIS144       = 'E'
        kunnr    = <l_output>-kunnr
        tranf_no = <l_output>-tranf_no
*       X_KUNNR  = ' '
*       _SCOPE   = '3'
*       _SYNCHRON             = ' '
*       _COLLECT = ' '
      .

    DELETE gt_output_new WHERE uuid = <l_output>-uuid.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_mail_text
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_mail_text  USING   us_output TYPE gty_output
                    CHANGING ct_text TYPE soli_tab.

  DATA: lf_text  TYPE string,
        lf_text2 TYPE string,
        lf_text3 TYPE string    ##NEEDED,
        lf_text4 TYPE soli-line.

  FIELD-SYMBOLS: <l_text>  TYPE soli,
                 <l_field> TYPE any.

  LOOP AT ct_text ASSIGNING <l_text> WHERE line CA '&'.
    SPLIT <l_text> AT '&' INTO lf_text lf_text2 lf_text3.

    lf_text = |&{ lf_text2 }&|.
    ASSIGN COMPONENT lf_text2 OF STRUCTURE us_output TO <l_field>.

    CASE lf_text2.
      WHEN 'KUNNR'.
        lf_text4 = |{ <l_field> ALPHA = OUT }|.
      WHEN 'WRBTR'.
        lf_text4 = us_output-wrbtr + us_output-exp_amt + us_output-bank_fee - us_output-inc_amt.
        CONDENSE lf_text4.
      WHEN OTHERS.
        lf_text4 = |{ <l_field> ALIGN = LEFT }|.
    ENDCASE.
*    WRITE <l_field> TO lf_text4.
*    SHIFT lf_text4 LEFT DELETING LEADING space.
    REPLACE lf_text INTO <l_text> WITH lf_text4.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_constant
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_constant .

  DATA:
    lt_action_status TYPE zcl_sdsca_utilities=>tt_gen_c.

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid = sy-repid          " ABAP Program Name
*     irt_param = lr_param          " Parameter name
*     irt_ext  =                  " Parameter extension
    IMPORTING
      et_gen_c = gt_genc                 " General Constants (GENC)
  ).

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid = 'ZSDSFIR0430'          " ABAP Program Name
*     irt_param = lr_param          " Parameter name
*     irt_ext  =                  " Parameter extension
    IMPORTING
      et_gen_c = DATA(lt_genc)     " General Constants (GENC)
  ).

  READ TABLE lt_genc INTO DATA(ls_genc_430) WITH KEY param = 'FILE_PATH'.
  IF sy-subrc EQ 0.
    gf_path = ls_genc_430-value_low.
  ENDIF.

  READ TABLE lt_genc INTO ls_genc_430 WITH KEY param = gc_genc_param-post_dated_chk.
  IF sy-subrc EQ 0.
    gf_post_dated_chk = ls_genc_430-value_low.
  ENDIF.


  "Action type / status
  DATA: lr_param TYPE zcl_sdsca_utilities=>trt_range_param.
  lr_param = VALUE #( ( sign = 'I' option = 'EQ' low = gc_genc_param-action_status ) ).

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = sy-repid          " ABAP Program Name
      irt_param = lr_param          " Parameter name
*     irt_ext   =                  " Parameter extension
    IMPORTING
      et_gen_c  = lt_action_status  " General Constants (GENC)
  ).

  "Action status combination check
  LOOP AT lt_action_status INTO DATA(ls_genc) ##INTO_OK.

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



ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_mail_recipients
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_mail_recipients  USING    us_output TYPE gty_output
                          CHANGING ct_recipients  ##PERF_NO_TYPE.

  DATA: lt_recipients TYPE STANDARD TABLE OF string,
        lt_comm       TYPE TABLE OF bapip0105nl,
        ls_comm       TYPE bapip0105nl,
        lf_recipient  TYPE string.

  CALL FUNCTION 'BAPI_EMPLCOMM_GETDETAILEDLIST'
    EXPORTING
      employeenumber = us_output-pernr
*     SUBTYPE        =
*     TIMEINTERVALLOW        = '18000101'
*     TIMEINTERVALHIGH       = '99991231'
* IMPORTING
*     RETURN         =
    TABLES
      communication  = lt_comm.

  IF lt_comm IS NOT INITIAL.
* Email from HR master
    READ TABLE lt_comm INTO ls_comm WITH KEY subtype = '0010'.
    IF sy-subrc EQ 0.
      lf_recipient = ls_comm-id.
      IF lf_recipient IS NOT INITIAL.
        APPEND lf_recipient TO lt_recipients.
      ENDIF.
    ELSE.
* USER ID
      READ TABLE lt_comm INTO ls_comm WITH KEY subtype = '0001'.
      IF sy-subrc EQ 0 AND ls_comm-id IS NOT INITIAL.
        PERFORM get_user_mail USING ls_comm-id CHANGING lf_recipient.
        IF lf_recipient IS NOT INITIAL.
          APPEND lf_recipient TO lt_recipients.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  ct_recipients = lt_recipients.
** Input person
*  PERFORM get_user_mail USING us_output-update_by CHANGING lf_recipient.
*  IF lf_recipient IS NOT INITIAL.
*    APPEND lf_recipient TO lt_recipients.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_user_mail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_user_mail  USING    uf_id
                    CHANGING cf_recipient ##PERF_NO_TYPE.

  DATA: lf_user    TYPE bapibname-bapibname,
        lt_return  TYPE bapiret2_tab,
        ls_address TYPE bapiaddr3.

  lf_user = uf_id.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = lf_user
*     CACHE_RESULTS           = 'X'
*     EXTUID_GET              =
    IMPORTING
*     LOGONDATA               =
*     DEFAULTS =
      address  = ls_address
*     COMPANY  =
*     SNC      =
*     REF_USER =
*     ALIAS    =
*     UCLASS   =
*     LASTMODIFIED            =
*     ISLOCKED =
*     IDENTITY =
*     ADMINDATA               =
*     DESCRIPTION             =
*     TECH_USER               =
*     SAPUSER_UUID            =
    TABLES
*     PARAMETER               =
*     PROFILES =
*     ACTIVITYGROUPS          =
      return   = lt_return
*     ADDTEL   =
*     ADDFAX   =
*     ADDTTX   =
*     ADDTLX   =
*     ADDSMTP  =
*     ADDRML   =
*     ADDX400  =
*     ADDRFC   =
*     ADDPRT   =
*     ADDSSF   =
*     ADDURI   =
*     ADDPAG   =
*     ADDCOMREM               =
*     PARAMETER1              =
*     GROUPS   =
*     UCLASSSYS               =
*     EXTIDHEAD               =
*     EXTIDPART               =
*     SYSTEMS  =
*     EXTUID   =
*     SAPUSER_UUID_HIST       =
*     USATTRIBUTE             =
    .
  cf_recipient = ls_address-e_mail.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form send_mail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM send_mail  USING  ut_text TYPE soli_tab
                       uf_sender TYPE adr6-smtp_addr
                       ut_recipients TYPE string_table
                       uf_subject TYPE string
                CHANGING cf_error TYPE xflag.

  DATA: lo_send_request  TYPE REF TO cl_bcs,
        lo_document      TYPE REF TO cl_document_bcs,
        lo_sender        TYPE REF TO if_sender_bcs,
        lo_recipient     TYPE REF TO if_recipient_bcs,
        lx_bcs_exception TYPE REF TO cx_bcs ##NEEDED,
        lf_recipients    TYPE string,
        lf_des           TYPE so_obj_des,
        lf_result        TYPE os_boolean    ##NEEDED,
        lf_reciept       TYPE adr6-smtp_addr.

  CLEAR cf_error.

  TRY.
      lo_send_request = cl_bcs=>create_persistent( ).

      lf_des = uf_subject.
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = ut_text
        i_subject = lf_des ).

      lo_send_request->set_document( lo_document ).

      lo_sender = cl_cam_address_bcs=>create_internet_address( uf_sender ) ##NO_TEXT.
      lo_send_request->set_sender( lo_sender ).

      LOOP AT ut_recipients INTO lf_recipients  ##INTO_OK.
        lf_reciept = lf_recipients.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( lf_reciept ).
        lo_send_request->add_recipient( lo_recipient ).
      ENDLOOP .

      lo_send_request->set_send_immediately( 'X' ).
      lo_send_request->set_message_subject( uf_subject ).

      CALL METHOD lo_send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = lf_result ).

    CATCH cx_bcs INTO lx_bcs_exception.
      cf_error = gc_true.

  ENDTRY.

  COMMIT WORK.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_button_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALID
*&      <-- LREF_MSG
*&---------------------------------------------------------------------*
FORM f_button_click USING us_row_no TYPE lvc_s_roid
                          us_col_id TYPE lvc_s_col    ##CALLED.

  IF us_col_id-fieldname = 'ATTACH_FILE_FLAG'.
    READ TABLE gt_output_new
      INTO DATA(ls_output_new)
      INDEX us_row_no-row_id.
    IF sy-subrc EQ 0 AND ls_output_new-attach_file_flag = gc_true.
      PERFORM f_disp_attach USING ls_output_new-uuid
                                  ls_output_new-attach_ext.
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
                             uf_ext      TYPE zsdsfis144-attach_ext.

  DATA: lv_file_name TYPE c LENGTH 255.

  lv_file_name = |{ gf_path }\\{ uf_uuid }.{ uf_ext }|.

  CALL FUNCTION 'Z_SDSFI_DISPLAY_PDF'
    EXPORTING
      iv_filename = CONV string( lv_file_name )
      iv_ext      = uf_ext.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_PRCTR_TXT
*----------------------------------------------------------------------*
*  Get Profir Center Text
*----------------------------------------------------------------------*
FORM f_get_prctr_txt  USING  uf_prctr  TYPE  gty_output-prctr
                    CHANGING cf_prctr_txt TYPE gty_output-prctr_txt.

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
