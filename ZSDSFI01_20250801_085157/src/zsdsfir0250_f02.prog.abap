*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250_F02
*  Creation Date      : 09.08.2024
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic - new entry
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
***INCLUDE ZSDSFIR0250_F02.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f_init_var_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_var_new_entry .

  DATA:
    lt_genc          TYPE zcl_sdsca_utilities=>tt_gen_c.

  IF rb_cren = gc_true OR
     rb_chgn = gc_true.
    gv_edit = abap_true.
  ELSE.
    gv_edit = abap_false.
  ENDIF.

  SELECT SINGLE waers
    FROM t001
    WHERE bukrs = @p_bukrsn
    INTO @gv_waers.

*  SELECT SINGLE
*    concat_with_space( vorna, nachn, 1 ) AS fullname
*    FROM pa0002
*    WHERE pernr = @p_pernrn
*    INTO @gv_fullname   ##WARN_OK.                      "#EC CI_NOORDER

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = sy-repid          " ABAP Program Name
*      irt_param =                  " Parameter name
*      irt_ext   =                  " Parameter extension
    IMPORTING
      et_gen_c  = lt_genc                 " General Constants (GENC)
  ).

  LOOP AT lt_genc INTO DATA(ls_genc)      ##INTO_OK.
    CASE ls_genc-param.
      WHEN gc_genc_param_new_entry-blart.
        gs_default_new_entry-blart = ls_genc-value_low.
      WHEN gc_genc_param_new_entry-bupla.
        gs_default_new_entry-bupla = ls_genc-value_low.
      WHEN gc_genc_param_new_entry-mwsk1.
        gs_default_new_entry-mwsk1 = ls_genc-value_low.
      WHEN gc_genc_param_new_entry-umskz.
        gs_default_new_entry-umskz = ls_genc-value_low.
      WHEN gc_genc_param_new_entry-prctr.
*        gs_default_new_entry-prctr = ls_genc-value_low. "<<F36K912524 - del
        gs_default_new_entry-prctr = |{ ls_genc-value_low ALPHA = IN }|. "<<F36K912524 - ins
    ENDCASE.
  ENDLOOP.

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
      PERFORM f_set_new_entry_create.
    WHEN rb_chgn.
      PERFORM f_get_new_entry_open.
*      PERFORM f_prepare_change_output_new.
    WHEN rb_disn.
      PERFORM f_get_new_entry_all.
*      PERFORM f_prepare_change_output_new.
  ENDCASE.

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

* ALV Layout
  PERFORM f_alv_layout USING p_nvar
                       CHANGING gs_layout
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
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-col_pos    = 1.
        <l_fieldcat>-key        = gc_true.
        IF rb_disn = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'FULL_NAME' OR
           'CUST_NAME'.
        <l_fieldcat>-edit       = ''.
      WHEN 'BELNR' OR
           'GJAHR'.
        <l_fieldcat>-edit       = ''.
        IF rb_disn <> gc_true.
          <l_fieldcat>-no_out   = gc_true.
        ENDIF.
      WHEN 'UMSKZ'.
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'HBKID'.
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'HKTID'.
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'MWSK1'.
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-f4availabl = gc_true.
      WHEN 'DELETE_FLAG'.
        <l_fieldcat>-edit       = ''.
        IF rb_cren = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'DELETE_DATE'.
        <l_fieldcat>-edit       = ''.
        IF rb_cren = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'TRANF_NO'.
        <l_fieldcat>-edit       = ''.
      WHEN 'UUID'.
        <l_fieldcat>-tech       = gc_true.
      WHEN OTHERS.
        <l_fieldcat>-edit       = gv_edit.
    ENDCASE.
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


  cf_valid = abap_true.

  LOOP AT gt_output_new INTO DATA(ls_output)    ##INTO_OK
    WHERE sel = gc_true.

    "Skip check for delete data
    CHECK ls_output-delete_flag = ''.

    DATA(lv_row_id) = sy-tabix.
    IF ls_output-bukrs IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'BUKRS' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-kunnr IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'KUNNR' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-bldat IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'BLDAT' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-budat IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'BUDAT' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-zfbdt IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'ZFBDT' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-waers IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'WAERS' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-mwsk1 IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'MWSK1' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-bupla IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'BUPLA' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-blart IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'BLART' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-umskz IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'UMSKZ' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-hbkid IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'HBKID' '' '' CHANGING cref_msg.
    ENDIF.

    IF ls_output-hktid IS INITIAL.
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_required lv_row_id 'HKTID' '' '' CHANGING cref_msg.
    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_input_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALID
*&      <-- LREF_MSG
*&---------------------------------------------------------------------*
FORM f_validate_input_new  CHANGING cf_valid TYPE abap_bool
                                   cref_msg TYPE REF TO cl_alv_changed_data_protocol.

  DATA:
    lv_total  TYPE zsdsfit029-payin_amt,
    lv_remain TYPE zsdsfit029-payin_amt,
    lt_chg    TYPE tt_output_new.

  CHECK cf_valid = gc_true.

  LOOP AT gt_output_new INTO DATA(ls_output)
    WHERE sel = gc_true
    AND   delete_flag = ''.

    DATA(lv_row_id) = sy-tabix.

    lv_total += ls_output-wrbtr.

    IF ls_output-umskz = 'D'.
      cf_valid = abap_false.
      DATA(lv_spgl) = | Sp.GL { ls_output-umskz }|.
      PERFORM f_add_msg USING gc_msg_ty-err_input_not_allow lv_row_id 'UMSKZ'
                              lv_spgl '' CHANGING cref_msg.
    ENDIF.

    IF p_rcvamn IS INITIAL.
      READ TABLE gt_action_status
        WITH KEY action_type = ls_output-action_type
                 status      = ls_output-status
        TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        cf_valid = abap_false.
        PERFORM f_add_msg USING gc_msg_ty-err_action_status lv_row_id 'ACTION_TYPE'
                                ls_output-action_type ls_output-status CHANGING cref_msg.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF p_rcvamn IS NOT INITIAL.
    lv_remain = p_rcvamn - lv_total.

    lt_chg = VALUE #( FOR ls_chg IN gt_output WHERE ( sel = 'X' AND delete_flag = ''  ) ( CORRESPONDING #( ls_chg  ) )  ).

    IF lv_remain <> 0 AND lt_chg IS NOT INITIAL. "contain data change / create
      cf_valid = abap_false.
      PERFORM f_add_msg USING gc_msg_ty-err_amt_balance lv_row_id 'PAYIN_AMT' '' '' CHANGING cref_msg.
    ENDIF.
  ENDIF.

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

  DATA: lt_new_entry       TYPE STANDARD TABLE OF zsdsfit043.

*  lt_new_entry = VALUE #( FOR ls_output IN gt_output_new WHERE ( sel = 'X'  ) ( CORRESPONDING #( ls_output  ) )  ).
*
*  IF lt_new_entry IS INITIAL.
*    MESSAGE s000(38) WITH TEXT-e10.
*    RETURN.
*  ENDIF.

  LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output>)
    WHERE sel = 'X'.

    "Get UUID
    TRY.
        <ls_output>-uuid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
      CATCH cx_uuid_error. " Error Class for UUID Processing Errors
        RETURN.
    ENDTRY.

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

    APPEND INITIAL LINE TO lt_new_entry ASSIGNING FIELD-SYMBOL(<ls_zsdsfit043>).
    <ls_zsdsfit043>       = CORRESPONDING #( <ls_output> ).
    <ls_zsdsfit043>-erdat = sy-datum.
    <ls_zsdsfit043>-erzmt = sy-uzeit.
  ENDLOOP.

  IF lt_new_entry IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e10.
    RETURN.
  ELSE.
    MODIFY zsdsfit043 FROM TABLE lt_new_entry.

    "Collection log saved
    MESSAGE s000(38) WITH TEXT-r02.

    COMMIT WORK AND WAIT. "Added 02.12.2024

  ENDIF.

  gref_grid->set_ready_for_input(
                     EXPORTING i_ready_for_input = 0 ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_update_new_entry .

  DATA: lt_new_entry       TYPE STANDARD TABLE OF zsdsfit043.

  lt_new_entry = VALUE #( FOR ls_output IN gt_output_new WHERE ( sel = 'X'  ) ( CORRESPONDING #( ls_output  ) )  ).

  IF lt_new_entry IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e10.
    RETURN.
  ENDIF.

  LOOP AT lt_new_entry ASSIGNING FIELD-SYMBOL(<ls_zsdsfit043>).

    "<<F36K912524 begin of ins
    IF <ls_zsdsfit043>-prctr IS NOT INITIAL.
      <ls_zsdsfit043>-prctr       = |{ <ls_zsdsfit043>-prctr ALPHA = IN }|.
    ENDIF.
    "<<F36K912524 end of ins

    <ls_zsdsfit043>-update_by   = sy-uname.
    <ls_zsdsfit043>-update_on   = sy-datum.
    <ls_zsdsfit043>-update_time = sy-uzeit.
  ENDLOOP.

  MODIFY zsdsfit043 FROM TABLE lt_new_entry.
  MESSAGE s000(38) WITH TEXT-r02.
  COMMIT WORK AND WAIT. "Added 02.12.2024

*  LEAVE TO SCREEN 0.
  gref_grid->set_ready_for_input(
                     EXPORTING i_ready_for_input = 0 ).


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

  LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output>)
    WHERE sel = 'X'.
    IF <ls_output>-delete_flag = gc_true.
      <ls_output>-delete_flag = ''.
      CLEAR <ls_output>-delete_date.
    ELSE.
      <ls_output>-delete_flag = gc_true.
      <ls_output>-delete_date = sy-datum.
    ENDIF.
  ENDLOOP.

  gref_grid->refresh_table_display(
    EXCEPTIONS
      finished       = 1                " Display was Ended (by Export)
      OTHERS         = 2
  ).

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

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
*& Form f_set_new_entry_create
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_new_entry_create.

  DO 1 TIMES.
    APPEND INITIAL LINE TO gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output_new>).
    PERFORM f_set_default_init_new_entry CHANGING <ls_output_new>.
  ENDDO.

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

  DATA:
    lr_workdate TYPE RANGE OF zsdsfit029-work_date,
    lr_pernr    TYPE RANGE OF zsdsfit029-pernr.

  IF p_wrkdtn IS NOT INITIAL.
    lr_workdate = VALUE #( BASE lr_workdate ( sign = 'I' option = 'EQ' low = p_wrkdtn ) ).
  ENDIF.

  IF p_pernrn <> '*'.
    lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = p_pernrn ) ).
  ENDIF.

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
    FROM zsdsfit043 AS log
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    INNER JOIN pa0002 AS per
    ON per~pernr = log~pernr
    WHERE log~bukrs       =  @p_bukrsn
    AND   log~work_date   IN @lr_workdate
    AND   log~kunnr       IN @s_kunnrn
    AND   log~xblnr       IN @s_xblnrn
    AND   log~delete_flag = ''
    AND   log~belnr       = ''
    AND   log~gjahr       = ''
    AND   log~tranf_no    IN @s_trnfn
    AND   log~pernr       IN @lr_pernr
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

  DATA:
    lr_workdate TYPE RANGE OF zsdsfit029-work_date,
    lr_pernr    TYPE RANGE OF zsdsfit029-pernr.

  IF p_wrkdtn IS NOT INITIAL.
    lr_workdate = VALUE #( BASE lr_workdate ( sign = 'I' option = 'EQ' low = p_wrkdtn ) ).
  ENDIF.

  IF p_pernr <> '*'.
    lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = p_pernru ) ).
  ENDIF.

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
    FROM zsdsfit043 AS log
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    INNER JOIN pa0002 AS per
    ON per~pernr = log~pernr
    WHERE log~bukrs       =  @p_bukrsn
    AND   log~work_date   IN @lr_workdate
    AND   log~kunnr       IN @s_kunnrn
    AND   log~xblnr       IN @s_xblnrn
    AND   log~tranf_no    IN @s_trnfn
    AND   log~pernr       IN @lr_pernr
    INTO CORRESPONDING FIELDS OF TABLE @gt_output_new ##TOO_MANY_ITAB_FIELDS.

  IF gt_output_new IS INITIAL.
    MESSAGE s000(38) WITH TEXT-e00. "No data found
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
FORM f_lock_tran_new_entry USING ut_output_new TYPE tt_output_new.

  IF rb_cren = gc_true OR
     rb_disn = gc_true.
    RETURN.
  ENDIF.

  LOOP AT ut_output_new INTO DATA(ls_output)    ##INTO_OK
    GROUP BY (
                key1 = ls_output-kunnr
              ).

    CALL FUNCTION 'ENQUEUE_EZSDSFIS123'
      EXPORTING
        mode_zsdsfis123 = 'E'
        kunnr           = ls_output-kunnr
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
FORM f_unlock_tran_new USING ut_output_new TYPE tt_output_new.

  LOOP AT ut_output_new INTO DATA(ls_output)      ##INTO_OK
    GROUP BY (
                key1 = ls_output-kunnr
              ).

    CALL FUNCTION 'DEQUEUE_EZSDSFIS123'
      EXPORTING
*       MODE_ZSDSFIS123       = 'E'
        kunnr = ls_output-kunnr
*       X_KUNNR               = ' '
*       _SCOPE                = '3'
*       _SYNCHRON             = ' '
*       _COLLECT              = ' '
      .

  ENDLOOP.

ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_prepare_change_output_new
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_prepare_change_output_new .
*
*  DATA: lt_celltab  TYPE lvc_t_styl.
*
*  LOOP AT gt_output_new ASSIGNING FIELD-SYMBOL(<ls_output>).
*    CLEAR:
*      lt_celltab.
*
*    IF gv_edit = gc_true.
*      PERFORM f_fill_celltab_new USING 'RO'
*                           CHANGING lt_celltab.
*    ELSE.
*      PERFORM f_fill_celltab_new USING 'RW'
*                           CHANGING lt_celltab.
*    ENDIF.
*
*    <ls_output>-celltab     = lt_celltab.
*
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_celltab_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_CELLTAB
*&---------------------------------------------------------------------*
FORM f_fill_celltab_new  USING VALUE(uf_mode) TYPE char02
                         CHANGING ct_celltab  TYPE lvc_t_styl   ##CALLED.

  DATA: lv_mode    TYPE raw4.

  IF uf_mode EQ 'RW'.
    lv_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE. "uf_mode eq 'RO'
    lv_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  PERFORM f_add_celltab USING lv_mode:
        'SEL'           CHANGING ct_celltab,
        'PERNR'         CHANGING ct_celltab,
        'WORK_DATE'     CHANGING ct_celltab,
        'BUKRS'         CHANGING ct_celltab,
        'XBLNR'         CHANGING ct_celltab,
        'KUNNR'         CHANGING ct_celltab,
        'UMSKZ'         CHANGING ct_celltab,
        'BLDAT'         CHANGING ct_celltab,
        'BUDAT'         CHANGING ct_celltab,
        'BKTXT'         CHANGING ct_celltab,
        'SGTXT'         CHANGING ct_celltab,
        'WRBTR'         CHANGING ct_celltab,
        'WAERS'         CHANGING ct_celltab,
        'ZFBDT'         CHANGING ct_celltab,
        'HBKID'         CHANGING ct_celltab,
        'HKTID'         CHANGING ct_celltab,
        'MWSK1'         CHANGING ct_celltab,
        'BUPLA'         CHANGING ct_celltab,
        'BLART'         CHANGING ct_celltab,
        'PYMT_METHOD'   CHANGING ct_celltab,
        'BANK_DATE'     CHANGING ct_celltab,
        'CHEQUE_NO'     CHANGING ct_celltab,
        'ACTION_TYPE'   CHANGING ct_celltab,
        'STATUS'        CHANGING ct_celltab.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_default_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- UREF_DATA_CHANGED_>MT_INSERTED
*&---------------------------------------------------------------------*
FORM f_default_new_entry  CHANGING ct_data_changed TYPE REF TO  cl_alv_changed_data_protocol.

  DATA:
    ls_ins_row    TYPE lvc_s_moce ##NEEDED,
    ls_outtab_new TYPE zsdsfis123.

  FIELD-SYMBOLS: <fs> TYPE STANDARD TABLE.

  LOOP AT ct_data_changed->mt_inserted_rows INTO ls_ins_row ##INTO_OK.
    ASSIGN ct_data_changed->mp_mod_rows->* TO <fs>.
    READ TABLE <fs> INTO ls_outtab_new INDEX 1. "ls_ins_row-row_id.
    IF sy-subrc EQ 0.
      PERFORM f_set_default_init_new_entry CHANGING ls_outtab_new.

      MODIFY <fs> FROM ls_outtab_new INDEX 1. "ls_ins_row-row_id.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_default_init_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_OUTPUT_NEW>
*&---------------------------------------------------------------------*
FORM f_set_default_init_new_entry  CHANGING cs_outtab_new TYPE zsdsfis123.

  cs_outtab_new-bukrs       = p_bukrsn.
  cs_outtab_new-waers       = gv_waers.
  cs_outtab_new-pernr       = p_pernrn.
  cs_outtab_new-full_name   = gv_fullname.
  cs_outtab_new-work_date   = p_wrkdtn.
  cs_outtab_new-pymt_method = p_dfpymn.
  cs_outtab_new-action_type = p_dfactn.
  cs_outtab_new-status      = p_dfstan.
  cs_outtab_new-hbkid       = p_dfhbkn.
  cs_outtab_new-hktid       = p_dfhktn.
  cs_outtab_new-bank_date   = p_dfbkdn.
  cs_outtab_new-cheque_no   = p_dfchqn.

  cs_outtab_new-blart       = gs_default_new_entry-blart.
  cs_outtab_new-bupla       = gs_default_new_entry-bupla.
  cs_outtab_new-umskz       = gs_default_new_entry-umskz.
  cs_outtab_new-mwsk1       = gs_default_new_entry-mwsk1.

ENDFORM.
