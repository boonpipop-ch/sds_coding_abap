*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0540_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form f_display_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_display_result  USING ut_output TYPE tt_output.

* Set Container name
  gf_container = 'CTL_ALV'.
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
  ASSIGN ut_output TO <g_list>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM f_alv_build_fieldcat CHANGING gt_fieldcat.
* Sort data
*  PERFORM f_alv_sort CHANGING gt_sort.
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
*& Form f_alv_build_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_alv_build_fieldcat  CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <l_fieldcat>   TYPE  lvc_s_fcat.

* Build Field cat from Structure.
  PERFORM f_prepare_fieldcat_o  USING  gc_structure
                              CHANGING ct_fieldcat.

  LOOP AT ct_fieldcat ASSIGNING <l_fieldcat>.
    CASE <l_fieldcat>-fieldname.
      WHEN 'SEL'.
        <l_fieldcat>-edit       = gc_true."GV_EDIT.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-col_pos    = 1.
        <l_fieldcat>-key        = gc_true.

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

      WHEN 'ERNAM' OR 'ERDAT' OR 'ERZMT'.
        <l_fieldcat>-tech = gc_true.

      WHEN 'REL_BLK_BY' OR 'REL_BLK_ON' OR 'REL_BLK_TIME'.
        <l_fieldcat>-no_out = gc_true.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_data .

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
    WHERE log~bukrs         =  @p_bukrsn
    AND   log~bldat         IN @s_bldat
    AND   log~kunnr         IN @s_kunnrn
    AND   log~xblnr         IN @s_xblnrn
    AND   log~tranf_no      IN @s_trnfn
    AND   log~pernr         IN @s_pernrn
    AND   log~tranf_no      IN @s_trnfn
    AND   log~work_date     IN @s_wrkdtn
    AND   log~delete_flag   = ''
    AND   log~belnr         = ''
    AND   log~gjahr         = ''
    AND   log~block_status  = @gc_block_status-block
    INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS.

*<-- Start of Insertion 06.12.2024 (Get Additional Data)
  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<l_output>).
    PERFORM f_get_prctr_txt  USING  <l_output>-prctr
                           CHANGING <l_output>-prctr_txt.
  ENDLOOP.
*--> End of Insertion 06.12.2024
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
    READ TABLE gt_output
      INTO DATA(ls_output)
      INDEX us_row_no-row_id.
    IF sy-subrc EQ 0 AND ls_output-attach_file_flag = gc_true.
      PERFORM f_disp_attach USING ls_output-uuid
                                  ls_output-attach_ext.
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
*&---------------------------------------------------------------------*
*& Form f_get_constant
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_constant .

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

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR
*----------------------------------------------------------------------*
*  Event ALV Toolbar
*----------------------------------------------------------------------*
FORM f_handle_toolbar USING uref_object TYPE REF TO	cl_alv_event_toolbar_set ##CALLED
                              uf_interactive TYPE	char01 ##NEEDED.

  DATA: ls_toolbar TYPE stb_button.

  DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&PASTE'.
  DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&APPEND'.
  DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.
  DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&DELETE_ROW'.
  DELETE uref_object->mt_toolbar WHERE function EQ '&LOCAL&COPY_ROW'.

* Handle Toolbar as needed
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

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.   "separator
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  "Approve
  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '0'.   "normal Button
  ls_toolbar-function = gc_func-approve.   "fcode
  ls_toolbar-icon = '@8X@'.
  ls_toolbar-text = TEXT-b02.
  ls_toolbar-quickinfo = 'Approve'(b02)   ##SHARE_OK.
  APPEND ls_toolbar TO uref_object->mt_toolbar.

  "Delete
  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '0'.   "normal Button
  ls_toolbar-function = gc_func-del.   "fcode
  ls_toolbar-icon = '@11@'.
  ls_toolbar-text = TEXT-b05.
  ls_toolbar-quickinfo = 'Delete selected item'(b01)   ##SHARE_OK.
  APPEND ls_toolbar TO uref_object->mt_toolbar.


ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM f_user_command USING uf_ucomm  TYPE  sy-ucomm ##CALLED.

  DATA:
*    lv_refresh TYPE char01,
    lv_sel     TYPE char01.


*--------------------------------------------------------------------*
* New entry
*--------------------------------------------------------------------*
  CASE uf_ucomm.
    WHEN gc_func-sel_a_new.
      lv_sel = gc_true.

      PERFORM f_mark_sel_all_new USING lv_sel
                             CHANGING gt_output.

      gref_grid->refresh_table_display( ).

    WHEN gc_func-sel_n_new.
      lv_sel = ''.

      PERFORM f_mark_sel_all_new USING lv_sel
                             CHANGING gt_output.

      gref_grid->refresh_table_display( ).

    WHEN gc_func-approve.
      PERFORM f_approve.

    WHEN gc_func-del.
      PERFORM f_delete_new_entry.


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
                         CHANGING ct_output TYPE tt_output.

  LOOP AT ct_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    <ls_output>-sel = uf_sel.
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

  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>)
    WHERE sel = 'X'.
    <ls_output>-delete_flag = gc_true.
    <ls_output>-delete_date = sy-datum.
  ENDLOOP.

  PERFORM f_update_new_entry USING ''.
  PERFORM f_unlock_tran_new.

  DELETE gt_output WHERE sel = 'X'.

  gref_grid->refresh_table_display(
    EXCEPTIONS
      finished = 1                " Display was Ended (by Export)
      OTHERS   = 2
  ).

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  PERFORM f_lock_tran_new_entry.


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

  lt_new_entry = VALUE #( FOR ls_output IN gt_output WHERE ( sel = 'X'  ) ( CORRESPONDING #( ls_output  ) )  ).

  IF lt_new_entry IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e10.
    RETURN.
  ENDIF.

  LOOP AT lt_new_entry ASSIGNING FIELD-SYMBOL(<ls_zsdsfit045>).
    IF <ls_zsdsfit045>-block_status = gc_block_status-approve.
      <ls_zsdsfit045>-rel_blk_by = sy-uname.
      <ls_zsdsfit045>-rel_blk_on = sy-datum.
      <ls_zsdsfit045>-rel_blk_time = sy-uzeit.
    ENDIF.

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
*& Form f_approve
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_approve.

  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>)
    WHERE sel = gc_true.

    <ls_output>-block_status = gc_block_status-approve.

  ENDLOOP.

  PERFORM f_update_new_entry USING ''.
  PERFORM f_unlock_tran_new.

  DELETE gt_output WHERE sel = 'X'.

  gref_grid->refresh_table_display(
    EXCEPTIONS
      finished = 1                " Display was Ended (by Export)
      OTHERS   = 2
  ).

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  PERFORM f_lock_tran_new_entry.


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


  LOOP AT gt_output INTO DATA(ls_output)    ##INTO_OK
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
*& Form f_unlock_tran_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_unlock_tran_new .

  LOOP AT gt_output INTO DATA(ls_output)      ##INTO_OK
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
