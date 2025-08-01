*----------------------------------------------------------------------*
***INCLUDE ZSDSFIR0240_F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_get_data_new_entry
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
"<<F36K917933 - 03 Begin of del
*FORM f_get_data_new_entry .
*
*  SELECT log~*,
*         bank~hkont,
*         concat_with_space( partner~name_org1, partner~name_org2, 1 ) AS cust_name,
*         concat_with_space( per~vorna, per~nachn, 1 ) AS full_name
*    FROM zsdsfit043 AS log
*    INNER JOIN kna1 AS cust
*    ON log~kunnr = cust~kunnr
*    INNER JOIN cvi_cust_link AS link
*    ON cust~kunnr = link~customer
*    INNER JOIN but000 AS partner
*    ON partner~partner_guid = link~partner_guid
*    INNER JOIN pa0002 AS per
*    ON per~pernr = log~pernr
*    INNER JOIN t012k AS bank
*    ON  bank~bukrs = log~bukrs
*    AND bank~hbkid = log~hbkid
*    AND bank~hktid = log~hktid
*    WHERE log~bukrs       =  @p_bukrsn
*    AND   log~erdat       IN @s_erdatn
*    AND   log~pernr       IN @s_pernrn
*    AND   log~kunnr       IN @s_kunnrn
*    AND   log~xblnr       IN @s_xblnrn
*    AND   log~tranf_no    IN @s_trnfn
*    AND   log~delete_flag = ''
**    AND   log~action_type IN @s_acttyp
**    AND   log~status      IN @s_status
*    AND   log~belnr       = ''
*    AND   log~gjahr       = ''
*    AND NOT ( log~action_type = @gc_reject-action AND
*              log~status      = @gc_reject-status )
*    INTO CORRESPONDING FIELDS OF TABLE @gt_output_new ##TOO_MANY_ITAB_FIELDS.
*
*  IF gt_output_new IS INITIAL.
*    MESSAGE s000(38) WITH TEXT-e00. "No data found
*  ENDIF.
*
*  "Prepare output
*  PERFORM f_prepare_output_new CHANGING gt_output_new.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_lock_tran_new_entry
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_lock_tran_new_entry .
*
*  LOOP AT gt_output_new INTO DATA(ls_output)    ##INTO_OK
*    GROUP BY (
*                key1 = ls_output-kunnr
*              ).
*
*    CALL FUNCTION 'ENQUEUE_EZSDSFIS123'
*      EXPORTING
*        mode_zsdsfis123 = 'E'
*        kunnr           = ls_output-kunnr
**       X_KUNNR         = ' '
**       _SCOPE          = '2'
**       _WAIT           = ' '
*        _collect        = 'X'
*      EXCEPTIONS
*        foreign_lock    = 1
*        system_failure  = 2
*        OTHERS          = 3.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDLOOP.
*  CALL FUNCTION 'FLUSH_ENQUEUE'
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form f_unlock_tran_new_entry
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_unlock_tran_new_entry.
*
*  LOOP AT gt_output_new INTO DATA(ls_output)      ##INTO_OK
*    GROUP BY (
*                key1 = ls_output-kunnr
*              ).
*
*    CALL FUNCTION 'DEQUEUE_EZSDSFIS123'
*      EXPORTING
**       MODE_ZSDSFIS123       = 'E'
*        kunnr = ls_output-kunnr
**       X_KUNNR               = ' '
**       _SCOPE                = '3'
**       _SYNCHRON             = ' '
**       _COLLECT              = ' '
*      .
*
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_post_document_new_entry
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> GC_MODE_TEST_RUN
**&      --> P_
**&      <-- GT_OUTPUT_NEW
**&---------------------------------------------------------------------*
*FORM f_post_document_new_entry  USING    uf_mode      TYPE char1
*                                CHANGING ct_result    TYPE tt_output_new.
*
*
*  DATA(lt_group) = ct_result.
*
*  IF uf_mode = gc_mode-test_run.
*    DELETE lt_group WHERE status_icon = gc_status-error.
*  ELSE.   "gc_mode-prod_run
*    DELETE lt_group WHERE sel IS INITIAL.
*  ENDIF.
*
*  LOOP AT lt_group ASSIGNING FIELD-SYMBOL(<ls_group>).
*
*
*    PERFORM f_posting_document_new_entry USING uf_mode
*                                      CHANGING <ls_group>.
*
*    PERFORM f_update_status_new_entry USING uf_mode
*                                            <ls_group>
*                                     CHANGING ct_result.
*
*  ENDLOOP.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_posting_document_new_entry
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> UF_MODE
**&      <-- LS_CLEARING
**&      <-- CT_RESULT
**&---------------------------------------------------------------------*
*FORM f_posting_document_new_entry  USING uf_mode    TYPE char1
*                                CHANGING cs_output  TYPE gty_output_new.
*
*  DATA:
*    lv_simu      TYPE char1,
*    lv_msgid     TYPE sy-msgid,
*    lv_msgno     TYPE sy-msgno,
*    lv_msgty     TYPE sy-msgty,
*    lv_msgv1     TYPE sy-msgv1,
*    lv_msgv2     TYPE sy-msgv2,
*    lv_msgv3     TYPE sy-msgv3,
*    lv_msgv4     TYPE sy-msgv4,
*    lv_subrc     TYPE sy-subrc,
*    lv_tran_mode TYPE rfpdo-allgazmd VALUE 'N',
*
*    lt_blntab    TYPE STANDARD TABLE OF blntab,
*    lt_post      TYPE STANDARD TABLE OF ftpost,
*    lt_tax       TYPE STANDARD TABLE OF fttax.
*
*  IF uf_mode = gc_mode-test_run.
*    lv_simu = gc_true.
*  ENDIF.
*
*  PERFORM f_posting_interface_start
*    USING lv_tran_mode.
*
*  PERFORM f_post_prepare_header_new USING cs_output
*                                    CHANGING lt_post.
*
*  PERFORM f_post_prepare_ar_new USING cs_output
*                                CHANGING lt_post.
*
*  PERFORM f_post_prepare_gl_new USING cs_output
*                                CHANGING lt_post.
*
*  CALL FUNCTION 'Z_SDSFI_POSTING_INTERFACE_DOC'
*    EXPORTING
*      i_tcode                  = 'FB01'
**     i_sgfunct                = 'C'
**     I_NO_AUTH                = ' '
*      i_xsimu                  = lv_simu
*    IMPORTING
*      e_msgid                  = lv_msgid
*      e_msgno                  = lv_msgno
*      e_msgty                  = lv_msgty
*      e_msgv1                  = lv_msgv1
*      e_msgv2                  = lv_msgv2
*      e_msgv3                  = lv_msgv3
*      e_msgv4                  = lv_msgv4
*      e_subrc                  = lv_subrc
*    TABLES
*      t_blntab                 = lt_blntab
*      t_ftpost                 = lt_post
*      t_fttax                  = lt_tax
*    EXCEPTIONS
*      account_missing          = 1
*      company_code_missing     = 2
*      posting_key_invalid      = 3
*      posting_key_missing      = 4
*      record_type_invalid      = 5
*      transaction_code_invalid = 6
*      amount_format_error      = 7
*      too_many_line_items      = 8
*      company_code_invalid     = 9
*      screen_not_found         = 10
*      no_authorization         = 11
*      OTHERS                   = 12.
*  IF sy-subrc <> 0 OR lv_subrc <> 0.
*    IF uf_mode = gc_mode-test_run AND
*   ( lv_msgid EQ '00' AND
*     lv_msgno EQ '344' AND
*     lv_msgv1 EQ 'SAPMF05A' AND
*     lv_msgv2 EQ '0700' ).
*
*      cs_output-status_msg  = TEXT-i01.
*      cs_output-status_icon = gc_status-warning.
*    ELSEIF lv_msgty CN 'EAX'.
*      MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
*        WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO cs_output-status_msg.
*
*      cs_output-status_icon = gc_status-warning.
*    ELSEIF lv_msgty IS NOT INITIAL.
*      MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
*        WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO cs_output-status_msg.
*
*      cs_output-status_icon = gc_status-error.
*    ENDIF.
*  ELSE.
*    IF uf_mode = gc_mode-test_run.
*      cs_output-status_msg  = TEXT-i01.
*      cs_output-status_icon = gc_status-warning.
*    ELSE.
*      cs_output-status_msg  = TEXT-i02.
*      cs_output-status_icon = gc_status-success.
*      READ TABLE lt_blntab INTO DATA(ls_bln) INDEX 1.
*      IF sy-subrc EQ 0.
*        cs_output-belnr = ls_bln-belnr.
*        cs_output-gjahr = ls_bln-gjahr.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  PERFORM f_posting_interface_end.
*
*
***********************************************************************
*
**  DATA: ls_docheader      TYPE bapiache09,
**        lv_obj_key        TYPE bapiache09-obj_key,
**
**        lt_accountgl      TYPE STANDARD TABLE OF bapiacgl09,
**        lt_accountar      TYPE STANDARD TABLE OF bapiacar09,
**        lt_accounttax      TYPE STANDARD TABLE OF bapiactx09,
**        lt_currencyamount  TYPE STANDARD TABLE OF bapiaccr09,
**        lt_return         TYPE STANDARD TABLE OF bapiret2.
**
**  PERFORM f_prepare_bapi USING cs_output CHANGING ls_docheader
**                                                  lt_accountgl
**                                                  lt_accountar
**                                                  lt_accounttax
**                                                  lt_currencyamount.
**
**  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
**    EXPORTING
**      documentheader    = ls_docheader
**    TABLES
**      accountgl         = lt_accountgl
**      accountreceivable = lt_accountar
**      accounttax        = lt_accounttax
**      currencyamount    = lt_currencyamount
***     CRITERIA          =
***     VALUEFIELD        =
***     EXTENSION1        =
**      return            = lt_return
***     PAYMENTCARD       =
***     EXTENSION2        =
***     ACCOUNTWT         =
**    .
**
**  LOOP AT lt_return INTO DATA(ls_return)
**    WHERE type CA 'EAX'
**    AND NOT ( id EQ 'RW' AND number EQ '609' ).
**    EXIT.
**  ENDLOOP.
**
**  CASE uf_mode.
**    WHEN gc_mode-test_run.
**      IF ls_return-type CA 'EAX'.
**        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
**          WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
**          INTO cs_output-status_msg.
**        cs_output-status_icon = gc_status-error.
**      ELSE.
**        cs_output-status_msg  = TEXT-i01.
**        cs_output-status_icon = gc_status-warning.
**      ENDIF.
**
**    WHEN gc_mode-prod_run.
**      CHECK ls_return-type NA 'EAX'.
**
**      CLEAR lt_return.
**
**      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
**        EXPORTING
**          documentheader    = ls_docheader
**        IMPORTING
**          obj_key           = lv_obj_key
**        TABLES
**          accountgl         = lt_accountgl
**          accountreceivable = lt_accountar
***         accounttax        = lt_accounttax
***         ACCOUNTWT         = GT_ACCOUNTWT
**          currencyamount    = lt_currencyamount
***         EXTENSION2        = GT_EXTENSION2
**          return            = lt_return.
**
***  CLEAR LS_RETURN.
**      READ TABLE lt_return WITH KEY type = 'E'
**                                    INTO ls_return.
**      IF sy-subrc EQ 0.
**        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
**          IMPORTING
**            return = ls_return.
**      ELSE.
**        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**          EXPORTING
**            wait   = 'X'
**          IMPORTING
**            return = ls_return.
**      ENDIF.
**
**      CLEAR ls_return.
**      LOOP AT lt_return INTO ls_return
**                        WHERE ( type CA 'EAX' )
**                        AND NOT ( id EQ 'RW' AND number EQ '609' ).
**        EXIT.
**      ENDLOOP.
**      IF sy-subrc EQ 0.
**        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
**          WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
**          INTO cs_output-status_msg.
**        cs_output-status_icon = gc_status-error.
**      ELSE.
**        cs_output-status_msg  = TEXT-i02.
**        cs_output-status_icon = gc_status-success.
**        cs_output-belnr = lv_obj_key(10).
**        cs_output-gjahr = lv_obj_key+14(4).
**      ENDIF.
**
**  ENDCASE.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_display_result_new
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> GT_OUTPUT_NEW
**&---------------------------------------------------------------------*
*FORM f_display_result_new USING  ut_result TYPE tt_output_new.
*
** Show progress
** Text-p99 : Generating ALV Report . . .
*  mc_show_progress 99 TEXT-p99.
*
** Set Container name
*  gf_container = 'CTL_ALV'.
*
*  gv_edit = gc_true.
*
** Disable Header area
**  gf_alv_header = gc_true.
*
**   No auto refresh in edit mode
*  gf_no_auto_refresh = gc_true.
*
** ALV Layout
*  PERFORM f_alv_layout CHANGING gs_layout
*                                gs_variant
*                                gs_print.
*
** Assign Output Data
** Assign Size
*  gf_header_hight = gc_header_height.
*  gf_alv_height   = gc_alv_height.
*  ASSIGN ut_result TO <g_list>.                       "#EC CI_FLDEXT_OK
** Build Field cat
*  PERFORM f_alv_build_fieldcat_new_entry CHANGING gt_fieldcat.
*** Sort data
**  PERFORM f_alv_sort_result CHANGING gt_sort.
** Call ALV Screen
*  CALL SCREEN 9000.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_alv_build_fieldcat_new_entry
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      <-- GT_FIELDCAT
**&---------------------------------------------------------------------*
*FORM f_alv_build_fieldcat_new_entry  CHANGING ct_fieldcat  TYPE lvc_t_fcat.
*
*  DATA:
*    lv_structure   TYPE tabname.
*
*  lv_structure = gc_structure_new.
*
** Build Field cat from Structure.
*  PERFORM f_prepare_fieldcat_o  USING  lv_structure
*                              CHANGING ct_fieldcat.
*
*  PERFORM f_adjust_fieldcat_new_entry CHANGING ct_fieldcat.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_adjust_fieldcat_new_entry
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      <-- CT_FIELDCAT
**&---------------------------------------------------------------------*
*FORM f_adjust_fieldcat_new_entry  CHANGING ct_fieldcat  TYPE lvc_t_fcat.
*
*  FIELD-SYMBOLS:
*    <l_fieldcat>   TYPE  lvc_s_fcat.
*
*  LOOP AT ct_fieldcat ASSIGNING <l_fieldcat>.
*    CASE <l_fieldcat>-fieldname.
*      WHEN 'SEL'.
*        <l_fieldcat>-edit       = gv_edit.
*        <l_fieldcat>-checkbox   = gc_true.
*        <l_fieldcat>-col_pos    = 1.
*        <l_fieldcat>-key        = gc_true.
*      WHEN 'STATUS_ICON'.
*        <l_fieldcat>-icon       = gc_true.
*      WHEN 'BELNR'.
*        <l_fieldcat>-hotspot    = gc_true.
*      WHEN 'GJAHR'.
*        <l_fieldcat>-no_out     = gc_true.
*      WHEN 'WRBTR'.
*        <l_fieldcat>-do_sum     = gc_true.
*      WHEN 'HKONT' OR 'UUID'.
*        <l_fieldcat>-tech       = gc_true.
*    ENDCASE.
*  ENDLOOP.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_mark_sel_all_new
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> LV_SEL
**&      <-- GT_OUTPUT_NEW
**&---------------------------------------------------------------------*
*FORM f_mark_sel_all_new  USING    uf_sel    TYPE char01
*                         CHANGING ct_output TYPE tt_output_new.
*
*  LOOP AT ct_output ASSIGNING FIELD-SYMBOL(<ls_output>).
*    IF VALUE #( <ls_output>-celltab[ fieldname = 'SEL' ]-style OPTIONAL ) = cl_gui_alv_grid=>mc_style_enabled .
*      <ls_output>-sel = uf_sel.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_prepare_output_new
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      <-- GT_OUTPUT_NEW
**&---------------------------------------------------------------------*
*FORM f_prepare_output_new  CHANGING
*                            ct_output_new TYPE tt_output_new.
*  DATA:
*    lt_celltab  TYPE lvc_t_styl.
*
*
*  LOOP AT ct_output_new ASSIGNING FIELD-SYMBOL(<ls_output>).
*    CLEAR lt_celltab.
*
*    "Build sort key for grouping
*    <ls_output>-sort_field = |{ <ls_output>-kunnr }{ <ls_output>-umskz }{ <ls_output>-hbkid }{ <ls_output>-hktid }|.
*
*    "Set record status
*    IF <ls_output>-status_icon = gc_status-error.
*      PERFORM f_fill_celltab USING 'RO'
*                           CHANGING lt_celltab.
*    ELSE.
*      PERFORM f_fill_celltab USING 'RW'
*                           CHANGING lt_celltab.
*    ENDIF.
*
*    <ls_output>-celltab = lt_celltab.
*
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_reject_data_new
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      <-- GT_OUTPUT_NEW
**&---------------------------------------------------------------------*
*FORM f_reject_data_new  CHANGING ct_output TYPE tt_output_new.
*
*  DATA:
*    lt_celltab     TYPE lvc_t_styl.
*
*  LOOP AT ct_output ASSIGNING FIELD-SYMBOL(<ls_output>)
*    WHERE sel = gc_true.
*
*    CLEAR <ls_output>-sel.
*
*    PERFORM f_fill_celltab USING 'RO'
*                         CHANGING lt_celltab.
*
*    <ls_output>-celltab = lt_celltab.
*
*    UPDATE zsdsfit043 SET action_type = gc_reject-action
*                          status      = gc_reject-status
*        WHERE uuid  = <ls_output>-uuid.
*
*  ENDLOOP.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form f_update_status_new_entry
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> UF_MODE
**&      --> LS_OUTPUT
**&      <-- CT_RESULT
**&---------------------------------------------------------------------*
*FORM f_update_status_new_entry  USING
*                                    uf_mode     TYPE char01
*                                    uf_output   TYPE gty_output_new
*                                 CHANGING
*                                    ct_items    TYPE tt_output_new.
*
*  DATA: lt_celltab   TYPE lvc_t_styl,
*        lt_new_entry TYPE STANDARD TABLE OF zsdsfit043.
*
*  IF uf_mode = gc_mode-test_run.
*    LOOP AT ct_items ASSIGNING FIELD-SYMBOL(<ls_test>)
**      WHERE "sort_field = uf_output-sort_field.
*      WHERE uuid  = uf_output-uuid.
*
*      <ls_test>-sel         = ''.
*      <ls_test>-status_icon = uf_output-status_icon.
*      <ls_test>-status_msg  = uf_output-status_msg.
*      <ls_test>-belnr       = uf_output-belnr.
*      <ls_test>-gjahr       = uf_output-gjahr.
*
*      IF uf_output-status_icon = gc_status-error.
*
*        PERFORM f_fill_celltab USING 'RO'
*                     CHANGING lt_celltab.
*        <ls_test>-celltab = lt_celltab.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
*
*    PERFORM f_fill_celltab USING 'RO'
*                         CHANGING lt_celltab.
*
*    LOOP AT ct_items ASSIGNING FIELD-SYMBOL(<ls_result>)
**      WHERE sort_field = uf_output-sort_field
**      AND   sel        = gc_true.
*      WHERE uuid  = uf_output-uuid.
*
*      <ls_result>-sel         = ''.
*      <ls_result>-status_icon = uf_output-status_icon.
*      <ls_result>-status_msg  = uf_output-status_msg.
*      <ls_result>-belnr       = uf_output-belnr.
*      <ls_result>-gjahr       = uf_output-gjahr.
*      <ls_result>-celltab     = lt_celltab.
*
*    ENDLOOP.
*  ENDIF.
*
*  lt_new_entry = VALUE #( FOR ls_output IN ct_items WHERE ( belnr IS NOT INITIAL AND gjahr IS NOT INITIAL  ) ( CORRESPONDING #( ls_output  ) )  ).
*
*  LOOP AT lt_new_entry INTO DATA(ls_new_entry) ##INTO_OK.
*    UPDATE zsdsfit043 SET belnr       = ls_new_entry-belnr
*                          gjahr       = ls_new_entry-gjahr
*                          update_by   = sy-uname
*                          update_on   = sy-datum
*                          update_time = sy-uzeit
*        WHERE uuid = ls_new_entry-uuid.
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_post_prepare_header_new
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> CS_OUTPUT
**&      <-- LT_POST
**&---------------------------------------------------------------------*
*FORM f_post_prepare_header_new  USING    uf_docu TYPE gty_output_new
*                                CHANGING ct_post TYPE feb_t_ftpost.
*
*  ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                    stype = gc_stype-header
*                    count = 1:
*                    fnam = 'BKPF-BLDAT'
*                    fval = |{ uf_docu-bldat DATE = USER }| ) ),
*                    fnam = 'BKPF-BUDAT'
*                    fval = |{ uf_docu-budat DATE = USER }| ) ),
*                    fnam = 'BKPF-BLART'
*                    fval = uf_docu-blart ) ),
*                    fnam = 'BKPF-BUKRS'
*                    fval = uf_docu-bukrs ) ),
*                    fnam = 'BKPF-BKTXT'
*                    fval = uf_docu-bktxt ) ),
*                    fnam = 'BKPF-XBLNR'
*                    fval = uf_docu-xblnr ) ),
*                    fnam = 'BKPF-WAERS'
*                    fval = uf_docu-waers ) ).
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_prepare_ar
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> CS_OUTPUT
**&      <-- LT_POST
**&---------------------------------------------------------------------*
*FORM f_post_prepare_ar_new  USING    uf_docu TYPE gty_output_new
*                            CHANGING ct_post TYPE feb_t_ftpost.
*
*  ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                    stype = gc_stype-item
*                    count = 1:
*
*                    fnam = 'RF05A-NEWKO'
*                    fval = uf_docu-kunnr ) ),
*                    fnam = 'RF05A-NEWBS'
*                    fval = '19' ) ),
*                    fnam = 'RF05A-NEWUM'
*                    fval = uf_docu-umskz ) ),
*
*                    fnam = 'BSEG-WRBTR'
*                    fval = |{ abs( uf_docu-wrbtr ) ALIGN = LEFT }| ) ),
*                    fnam = 'BSEG-MWSKZ'
*                    fval = uf_docu-mwsk1 ) ),
*                    fnam = 'BSEG-BUPLA'
*                    fval = uf_docu-bupla ) ),
*                    fnam = 'BSEG-PRCTR'
*                    fval = uf_docu-prctr ) ),
*                    fnam = 'BSEG-SGTXT'
*                    fval = uf_docu-sgtxt ) ).
*
*  IF uf_docu-umskz = 'D'.
*    DELETE ct_post WHERE stype = gc_stype-item
*                   AND   count = 1
*                   AND   fnam  := 'BSEG-PRCTR',
*                                = 'BSEG-MWSKZ'.
*
*    ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                   stype = gc_stype-item
*                   count = 1:
*
*                   fnam = 'BSED-WMWKZ'
*                   fval = uf_docu-mwsk1 ) ),
*                   fnam = 'BSEG-ZFBDT'
*                   fval = |{ uf_docu-zfbdt DATE = USER }| ) ),
*                   fnam = 'BSED-BOENO'
*                   fval = uf_docu-cheque_no ) ).
**                    fnam = 'BSED-WMWKZ'
**                    fval = uf_docu-hbkid ) ),
**                    fnam = 'BSED-ACCOU'
**                    fval = uf_docu-hbkid ) ).
*
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_post_prepare_gl_new
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> CS_OUTPUT
**&      <-- LT_POST
**&---------------------------------------------------------------------*
*FORM f_post_prepare_gl_new  USING    uf_docu TYPE gty_output_new
*                            CHANGING ct_post TYPE feb_t_ftpost.
*
*
*  ct_post = VALUE feb_t_ftpost( BASE ct_post (
*                    stype = gc_stype-item
*                    count = 2:
*
*                    fnam = 'RF05A-NEWKO'
*                    fval = uf_docu-hkont ) ),
*                    fnam = 'RF05A-NEWBS'
*                    fval = '40' ) ),
*
*                    fnam = 'BSEG-WRBTR'
*                    fval = |{ abs( uf_docu-wrbtr ) ALIGN = LEFT }| ) ),
*                    fnam = 'BSEG-VALUT'
*                    fval = |{ uf_docu-zfbdt DATE = USER }| ) ),
*                    fnam = 'BSEG-BUPLA'
*                    fval = uf_docu-bupla ) ),
*                    fnam = 'COBL-PRCTR'
*                    fval = uf_docu-prctr ) ),
*                    fnam = 'BSEG-SGTXT'
*                    fval = uf_docu-sgtxt ) ).
*
*ENDFORM.
"<<F36K917933 - 03 End of del

**********************************************************************
* MEMO
**********************************************************************

*&---------------------------------------------------------------------*
*& Form f_posting_document_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UF_MODE
*&      <-- <LS_GROUP>
*&---------------------------------------------------------------------*
FORM f_posting_document_memo  USING uf_mode    TYPE char1
                                    uf_proc_mode TYPE rfpdo-allgazmd
                                CHANGING cs_output  TYPE gty_output.

  DATA:
    lv_auglv     TYPE t041a-auglv,
    lv_simu      TYPE char1,
    lv_msgid     TYPE sy-msgid,
    lv_msgno     TYPE sy-msgno,
    lv_msgty     TYPE sy-msgty,
    lv_msgv1     TYPE sy-msgv1,
    lv_msgv2     TYPE sy-msgv2,
    lv_msgv3     TYPE sy-msgv3,
    lv_msgv4     TYPE sy-msgv4,
    lv_subrc     TYPE sy-subrc,
    lv_tran_mode TYPE rfpdo-allgazmd VALUE 'N',

    lt_clear     TYPE STANDARD TABLE OF ftclear,
    lt_partial   TYPE tt_partial,
    lt_blntab    TYPE STANDARD TABLE OF blntab,
    lt_post      TYPE STANDARD TABLE OF ftpost,
    lt_tax       TYPE STANDARD TABLE OF fttax.

  IF uf_proc_mode IS NOT INITIAL.
    lv_tran_mode = uf_proc_mode.
  ENDIF.

  IF uf_mode = gc_mode-test_run.
    lv_simu = gc_true.
  ENDIF.

  lv_auglv = 'EINGZAHL'.

  PERFORM f_posting_interface_start
    USING lv_tran_mode.

  PERFORM f_post_prepare_header_memo USING cs_output
                                     CHANGING lt_post.

  PERFORM f_post_prepare_ar_memo USING cs_output
                                 CHANGING lt_post.

  PERFORM f_post_prepare_gl_memo USING cs_output
                                 CHANGING lt_post.

  CALL FUNCTION 'Z_SDSFI_POSTING_INTF_CLR_PART'
    EXPORTING
      i_auglv                    = lv_auglv
      i_tcode                    = 'FB05'
      i_xsimu                    = lv_simu
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


*  CALL FUNCTION 'Z_SDSFI_POSTING_INTERFACE_DOC'
*    EXPORTING
*      i_tcode                  = 'FB01'
**     i_sgfunct                = 'C'
**     I_NO_AUTH                = ' '
*      i_xsimu                  = lv_simu
*    IMPORTING
*      e_msgid                  = lv_msgid
*      e_msgno                  = lv_msgno
*      e_msgty                  = lv_msgty
*      e_msgv1                  = lv_msgv1
*      e_msgv2                  = lv_msgv2
*      e_msgv3                  = lv_msgv3
*      e_msgv4                  = lv_msgv4
*      e_subrc                  = lv_subrc
*    TABLES
*      t_blntab                 = lt_blntab
*      t_ftpost                 = lt_post
*      t_fttax                  = lt_tax
*    EXCEPTIONS
*      account_missing          = 1
*      company_code_missing     = 2
*      posting_key_invalid      = 3
*      posting_key_missing      = 4
*      record_type_invalid      = 5
*      transaction_code_invalid = 6
*      amount_format_error      = 7
*      too_many_line_items      = 8
*      company_code_invalid     = 9
*      screen_not_found         = 10
*      no_authorization         = 11
*      OTHERS                   = 12.
  IF sy-subrc <> 0 OR lv_subrc <> 0.
    IF uf_mode = gc_mode-test_run AND
   ( lv_msgid EQ '00' AND
     lv_msgno EQ '344' AND
     lv_msgv1 EQ 'SAPMF05A' AND
     lv_msgv2 EQ '0700' ).

      cs_output-status_msg  = TEXT-i01.
      cs_output-status_icon = gc_status-warning.
    ELSEIF lv_msgty IS NOT INITIAL.
      MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
        WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO cs_output-status_msg.

      cs_output-status_icon = gc_status-error.
    ENDIF.
  ELSE.
    IF uf_mode = gc_mode-test_run.
      cs_output-status_msg  = TEXT-i01.
      cs_output-status_icon = gc_status-warning.
    ELSE.
      cs_output-status_msg  = TEXT-i02.
      cs_output-status_icon = gc_status-success.
      READ TABLE lt_blntab INTO DATA(ls_bln) INDEX 1.
      IF sy-subrc EQ 0.
        cs_output-belnr = ls_bln-belnr.
        cs_output-gjahr = ls_bln-gjahr.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM f_posting_interface_end.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_prepare_header_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_OUTPUT
*&      <-- LT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_header_memo  USING    uf_docu TYPE gty_output
                                CHANGING ct_post TYPE feb_t_ftpost.

  ct_post = VALUE feb_t_ftpost( BASE ct_post (
                    stype = gc_stype-header
                    count = 1:
                    fnam = 'BKPF-BLDAT'
                    fval = |{ p_bldatc DATE = USER }| ) ),
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
                    fval = uf_docu-waers ) ),
                    fnam = 'RF05A-PORTF'
                    fval = gv_post_dated_chk  ) ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_prepare_ar_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_OUTPUT
*&      <-- LT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_ar_memo USING    uf_docu TYPE gty_output
                            CHANGING ct_post TYPE feb_t_ftpost.

  ct_post = VALUE feb_t_ftpost( BASE ct_post (
                    stype = gc_stype-item
                    count = 1:

                    fnam = 'RF05A-NEWKO'
                    fval = uf_docu-kunnr ) ),
                    fnam = 'RF05A-NEWBS'
                    fval = '09' ) ),
                    fnam = 'RF05A-NEWUM'
                    fval = uf_docu-umskz ) ),

                    fnam = 'BSEG-WRBTR'
                    fval = |{ abs( uf_docu-bal_amt ) ALIGN = LEFT }| ) ),
                    fnam = 'BSEG-MWSKZ'
                    fval = uf_docu-mwskz ) ),
                    fnam = 'BSEG-BUPLA'
                    fval = p_brnchc ) ),
                    fnam = 'BSEG-PRCTR'
                    fval = uf_docu-prctr ) ),
                    fnam = 'BSEG-SGTXT'
                    fval = COND #( WHEN p_sgtxt IS NOT INITIAL THEN p_sgtxt ELSE uf_docu-sgtxt ) ) ).

  IF uf_docu-umskz = 'D'.
    DELETE ct_post WHERE stype = gc_stype-item
                   AND   count = 1
                   AND   fnam  := 'BSEG-PRCTR',
                                = 'BSEG-MWSKZ'.

    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                   stype = gc_stype-item
                   count = 1:

                   fnam = 'BSED-WMWKZ'
                   fval = uf_docu-mwskz ) ),
                   fnam = 'BSEG-ZFBDT'
*                   fval = |{ p_bldatc DATE = USER }| ) ),
                   fval = |{ uf_docu-bank_date DATE = USER }| ) ),
                   fnam = 'BSED-BOENO'
                   fval = uf_docu-cheque_no ) ),
                    fnam = 'BSED-BANK'
                    fval = uf_docu-bankk ) ),
                    fnam = 'BSED-ACCOU'
                    fval = uf_docu-bankn  ) ).

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_post_prepare_gl_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_OUTPUT
*&      <-- LT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_gl_memo  USING    uf_docu TYPE gty_output
                            CHANGING ct_post TYPE feb_t_ftpost.

  DATA:
        lv_count  TYPE count_pi.

  lv_count = 2.

  ct_post = VALUE feb_t_ftpost( BASE ct_post (
                    stype = gc_stype-item
                    count = lv_count:

                    fnam = 'RF05A-NEWBS'
                    fval = '50' ) ),
                    fnam = 'RF05A-NEWKO'
                    fval = gv_memo_account ) ),
*                    fval = uf_docu-hkont ) ),

                    fnam = 'BSEG-WRBTR'
                    fval = |{ abs( uf_docu-payin_amt ) ALIGN = LEFT }| ) ),
                    fnam = 'BSEG-VALUT'
                    fval = |{ p_bldatc DATE = USER }| ) ),
                    fnam = 'BSEG-BUPLA'
                    fval = p_brnchc ) ),
                    fnam = 'COBL-PRCTR'
                    fval = uf_docu-prctr ) ),
                    fnam = 'BSEG-SGTXT'
*                    fval = COND #( WHEN p_sgtxt IS NOT INITIAL THEN p_sgtxt ELSE uf_docu-sgtxt ) ) ).
                    fval = |{ uf_docu-kunnr };{ uf_docu-cheque_no };{ uf_docu-bankn };{ uf_docu-due_on DATE = USER }| ) ).


  PERFORM f_post_prepare_ftpost_oth_memo USING
                                            uf_docu
                                         CHANGING
                                            lv_count
                                            ct_post.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_post_prepare_ftpost_oth_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UF_AMOUNT
*&      <-- LV_COUNT
*&      <-- CT_POST
*&---------------------------------------------------------------------*
FORM f_post_prepare_ftpost_oth_memo  USING
                                 uf_docu   TYPE gty_output
                               CHANGING
                                 cv_count  TYPE count_pi
                                 ct_post   TYPE feb_t_ftpost.

  DATA lv_df_order TYPE abap_bool.

  "Expense
  IF uf_docu-exps_amt IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-expense_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_docu-exps_amt > 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_docu-exps_amt ) ALIGN = LEFT }| ) ),
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
  IF uf_docu-fee IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-fee_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_docu-fee > 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_docu-fee ) ALIGN = LEFT }| ) ),
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
  IF uf_docu-retention IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = uf_docu-kunnr ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_docu-retention > 0 THEN '09' ELSE '19' ) ) ),
                      fnam = 'RF05A-NEWUM'
                      fval = 'Y' ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_docu-retention ) ALIGN = LEFT }| ) ),
                      fnam = 'BSEG-ZFBDT'
*                      fval = |{ uf_docu-due_on DATE = USER }| ) ).
*                      fval = |{ COND #( WHEN p_zfbdt IS NOT INITIAL THEN p_zfbdt ELSE uf_docu-due_on ) DATE = USER }| ) ).
                      fval = |{ COND #( WHEN p_zfbdt IS NOT INITIAL THEN p_zfbdt ELSE p_budatc ) DATE = USER }| ) ).

  ENDIF.

  "Income
  IF uf_docu-income_amt IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-income_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_docu-income_amt > 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_docu-income_amt ) ALIGN = LEFT }| ) ).
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
  IF uf_docu-cash_con IS NOT INITIAL.
    cv_count += 1.
    ct_post = VALUE feb_t_ftpost( BASE ct_post (
                      stype = gc_stype-item
                      count = cv_count:
                      fnam = 'RF05A-NEWKO'
                      fval = gs_account-cash_contra_account ) ),
                      fnam = 'RF05A-NEWBS'
                      fval = COND #( WHEN uf_docu-cash_con > 0 THEN '40' ELSE '50' ) ) ),
                      fnam = 'BSEG-WRBTR'
                      fval = |{ abs( uf_docu-cash_con ) ALIGN = LEFT }| ) ),
                      fnam = 'BSEG-SGTXT'
                      fval = |{ uf_docu-kunnr };{ uf_docu-cheque_no };{ uf_docu-bankn };{ uf_docu-due_on DATE = USER }| ) ).


    lv_df_order = gc_true.

    PERFORM f_post_prepare_ftpost_default USING gs_account-cash_contra_account
                                                cv_count
                                                uf_docu
                                                lv_df_order
                                          CHANGING ct_post.

  ENDIF.

ENDFORM.
