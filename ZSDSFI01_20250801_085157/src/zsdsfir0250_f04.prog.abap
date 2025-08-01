*----------------------------------------------------------------------*
***INCLUDE ZSDSFIR0250_F04.
*----------------------------------------------------------------------*
*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250_F04
*  Creation Date      : 20.09.2024
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic - history log
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
*& Form f_get_history_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_history_log .
  CASE gc_true.
    WHEN rb_hchg.
      PERFORM f_get_history_log_open.
      PERFORM f_prepare_history_output.
    WHEN rb_hdis.
      PERFORM f_get_history_log_all.    "Get history log
      PERFORM f_get_history_open_item.  "Get open item
      PERFORM f_prepare_create_output.
      PERFORM f_prepare_history_output. "Merge data output + history
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_history_log_open
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_history_log_open .

  "Get history log where data not yet clear/delete
  SELECT log~*,
      CASE
        WHEN cust~ktokd = 'Z010' THEN
          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
        WHEN cust~ktokd = 'Z050' THEN
          concat_with_space( partner~name_first, partner~name_last, 1 )
        ELSE
          concat_with_space( partner~name_org1, partner~name_org2, 1 )
      END AS cust_name
    FROM zsdsfit038 AS log
    INNER JOIN zsdsfit029 AS bpl
    ON   bpl~data_type = log~data_type
    AND  bpl~bukrs = log~bukrs
    AND  bpl~belnr = log~belnr
    AND  bpl~gjahr = log~gjahr
    AND  bpl~buzei = log~buzei
    AND  bpl~seq   = log~seq
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    WHERE log~data_type   IN @s_htype
    AND   log~pernr       IN @s_hpernr
    AND   log~tranf_no    IN @s_htrnf
    AND   log~kunnr       IN @s_hkunnr
    AND   log~work_date   IN @s_hwrkdt
    AND   log~billpl_no   IN @s_hbill
    AND   log~billpl_date IN @s_hpldat
    AND   log~action_type IN @s_hactty
    AND   log~status      IN @s_hstat
    AND   log~belnr       IN @s_hbelnr
    AND   log~gjahr       IN @s_hgjahr
    AND   log~xblnr       IN @s_hxblnr
    AND   log~umskz       IN @s_humskz
    AND   log~follow_date IN @s_hfllw
    AND   log~bank_date   IN @s_hbnkdt
    AND   log~bankl       IN @s_hbankl
    AND   log~vbeln_vf    IN @s_hvbeln
    AND   log~pymt_method IN @s_hpymt
    AND   log~cheque_no   IN @s_hchq
    AND   log~delete_flag = ''
    AND   bpl~delete_flag = ''
    AND NOT EXISTS ( SELECT 'X' FROM zsdsfit037 AS lnk
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
*                      AND   clr~xreversal = ''     "<<F36K917931 - 04 del
                      )
    INTO CORRESPONDING FIELDS OF TABLE @gt_history_log ##TOO_MANY_ITAB_FIELDS. "#EC CI_NO_TRANSFORM

  IF gt_history_log IS NOT INITIAL.
    SORT gt_history_log BY bukrs gjahr DESCENDING belnr buzei seq.

    SELECT *
      INTO TABLE @gt_bplog
      FROM zsdsfit029 AS log
      FOR ALL ENTRIES IN @gt_history_log
*      WHERE log~data_type = @gc_data_type-invoice    "<<F36K910583 del
      WHERE log~data_type = @gt_history_log-data_type "<<F36K910583 ins
      AND   log~bukrs = @gt_history_log-bukrs
      AND   log~belnr = @gt_history_log-belnr
      AND   log~gjahr = @gt_history_log-gjahr
      AND   log~buzei = @gt_history_log-buzei
      AND   log~seq   = @gt_history_log-seq.  "#EC CI_ALL_FIELDS_NEEDED

    SELECT
      bukrs,
      belnr,
      gjahr,
      buzei,
      name1,
      name2
      FROM bsec
      FOR ALL ENTRIES IN @gt_history_log
      WHERE bukrs = @gt_history_log-bukrs
      AND   belnr = @gt_history_log-belnr
      AND   gjahr = @gt_history_log-gjahr
      AND   buzei = @gt_history_log-buzei               "#EC CI_NOORDER
      INTO TABLE @gt_onetime.                      "#EC CI_NO_TRANSFORM

    "DO <-> Billing
    SELECT
      bil~vbeln,
      bilh~fkdat,               "<<F36K911749 ins
      bil~aubel,                "<<F36K911780 ins
      bil~vgbel,
      do~lfdat
      FROM vbrp AS bil
      INNER JOIN vbrk AS bilh   "<<F36K911749 ins
      ON bilh~vbeln = bil~vbeln "<<F36K911749 ins
      INNER JOIN likp AS do
      ON do~vbeln = bil~vgbel
      INNER JOIN @gt_history_log AS log
      ON bil~vbeln = log~vbeln_vf
      WHERE bil~vgtyp = 'J'
      INTO TABLE @gt_do  ##TOO_MANY_ITAB_FIELDS.

    SORT gt_do BY vbeln.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_history_log_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_history_log_all .

*<-- Start of Insertion 06.12.2024 (New Delete Check condition)
  DATA:
    lr_delete TYPE RANGE OF zsdsfit038-delete_flag.


  CLEAR lr_delete.
  IF cb_wtdel IS INITIAL.
    INSERT VALUE #( sign = 'I'
                    option = 'EQ'
                    low    = space )
           INTO TABLE lr_delete.
  ENDIF.
*--> End of Insertion 06.12.2024

  "Get all history log
  SELECT log~*,
      CASE
        WHEN cust~ktokd = 'Z010' THEN
          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
        WHEN cust~ktokd = 'Z050' THEN
          concat_with_space( partner~name_first, partner~name_last, 1 )
        ELSE
          concat_with_space( partner~name_org1, partner~name_org2, 1 )
      END AS cust_name,
      bpl~delete_flag AS doc_delete
*<<F36K914812 - 05 Begin of del
*      lnk~bukrs_clr,
*      lnk~belnr_clr,
*      lnk~gjahr_clr
*<<F36K914812 - 05 End of del
    FROM zsdsfit038 AS log
    INNER JOIN kna1 AS cust
    ON log~kunnr = cust~kunnr
    INNER JOIN zsdsfit029 AS bpl
    ON   bpl~data_type = log~data_type
    AND  bpl~bukrs = log~bukrs
    AND  bpl~belnr = log~belnr
    AND  bpl~gjahr = log~gjahr
    AND  bpl~buzei = log~buzei
    AND  bpl~seq   = log~seq
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
*<<F36K914812 - 05 Begin of del
*    LEFT JOIN zsdsfit037 AS lnk
*    ON    lnk~data_type = log~data_type
*    AND   lnk~bukrs     = log~bukrs
*    AND   lnk~belnr     = log~belnr
*    AND   lnk~gjahr     = log~gjahr
*    AND   lnk~buzei     = log~buzei
*    AND   lnk~seq       = log~seq
*<<F36K914812 - 05 End of del
    WHERE log~data_type   IN @s_htype
    AND   log~pernr       IN @s_hpernr
    AND   log~tranf_no    IN @s_htrnf
    AND   log~kunnr       IN @s_hkunnr
    AND   log~work_date   IN @s_hwrkdt
    AND   log~billpl_no   IN @s_hbill
    AND   log~billpl_date IN @s_hpldat
    AND   log~action_type IN @s_hactty
    AND   log~status      IN @s_hstat
    AND   log~belnr       IN @s_hbelnr
    AND   log~gjahr       IN @s_hgjahr
    AND   log~xblnr       IN @s_hxblnr
    AND   log~umskz       IN @s_humskz
    AND   log~follow_date IN @s_hfllw
    AND   log~bank_date   IN @s_hbnkdt
    AND   log~bankl       IN @s_hbankl
    AND   log~vbeln_vf    IN @s_hvbeln
    AND   log~pymt_method IN @s_hpymt
    AND   log~cheque_no   IN @s_hchq
*<-- Start of Insertion 06.12.2024 (New Delete Check condition)
    AND   log~delete_flag IN @lr_delete
    AND   bpl~delete_flag IN @lr_delete
*--> End of Insertion 06.12.2024
    INTO CORRESPONDING FIELDS OF TABLE @gt_history_log ##TOO_MANY_ITAB_FIELDS. "#EC CI_NO_TRANSFORM

  "<<F36K919968 - 01 Begin of ins
  DELETE gt_history_log
    WHERE action_type IS INITIAL AND
          status      IS INITIAL.
  "<<F36K919968 - 01 end of ins

  IF gt_history_log IS NOT INITIAL.

*<<F36K914812 - 05 Begin of ins
    SELECT lnk~*,
*<<F36K917931 - 03 Begin of ins
           doc~xreversed,
           doc~stblg,
           doc~stjah
*<<F36K917931 - 03 End of ins
      FROM zsdsfit037 AS lnk
      INNER JOIN bkpf AS doc
      ON  doc~bukrs = lnk~bukrs_clr
      AND doc~belnr = lnk~belnr_clr
      AND doc~gjahr = lnk~gjahr_clr
      FOR ALL ENTRIES IN @gt_history_log
      WHERE data_type = ''
      AND   lnk~bukrs = @gt_history_log-bukrs
      AND   lnk~belnr = @gt_history_log-belnr
      AND   lnk~gjahr = @gt_history_log-gjahr
      AND   lnk~buzei = @gt_history_log-buzei
      AND   lnk~seq   = @gt_history_log-seq
*      AND   doc~xreversal = ''         "<<F36K917931 - 03 del
      INTO TABLE @gt_clr_link.                     "#EC CI_NO_TRANSFORM
*<<F36K914812 - 05 End of ins

    SELECT
      bukrs,
      belnr,
      gjahr,
      buzei,
      name1,
      name2
      FROM bsec
      FOR ALL ENTRIES IN @gt_history_log
      WHERE bukrs = @gt_history_log-bukrs
      AND   belnr = @gt_history_log-belnr
      AND   gjahr = @gt_history_log-gjahr
      AND   buzei = @gt_history_log-buzei               "#EC CI_NOORDER
      INTO TABLE @gt_onetime.                      "#EC CI_NO_TRANSFORM

    "DO <-> Billing
    SELECT
      bil~vbeln,
      bilh~fkdat,                 "<<F36K911749 ins
      bil~aubel,                  "<<F36K911780 ins
      bil~vgbel,
      do~lfdat
      FROM vbrp AS bil
      INNER JOIN vbrk AS bilh     "<<F36K911749 ins
      ON bil~vbeln = bilh~vbeln   "<<F36K911749 ins
      INNER JOIN likp AS do
      ON do~vbeln = bil~vgbel
      INNER JOIN @gt_history_log AS log
      ON bil~vbeln = log~vbeln_vf
      WHERE bil~vgtyp = 'J'
      INTO TABLE @gt_do ##TOO_MANY_ITAB_FIELDS.

    SORT gt_do BY vbeln.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_history_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_prepare_history_output .
  DATA: lt_celltab TYPE lvc_t_styl.
*<<F36K917931 - 09 begin of ins
  DATA: lv_ernam  TYPE bapibname-bapibname,
        ls_addr   TYPE bapiaddr3,
        lt_return TYPE STANDARD TABLE OF bapiret2.
*<<F36K917931 - 09 end of ins

*<<F36K914812 - 04 Begin of ins
  "Merge data
  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
*<<F36K917931 - 09 Begin of ins
    IF <ls_output>-pernr IS NOT INITIAL.
      READ TABLE gt_user_data ASSIGNING FIELD-SYMBOL(<ls_pernr_data>)
        WITH KEY pernr = <ls_output>-pernr.             "#EC CI_SORTSEQ
      IF sy-subrc = 0.
        <ls_output>-pernr     = <ls_pernr_data>-pernr.
        <ls_output>-full_name = <ls_pernr_data>-full_name.
      ENDIF.
    ELSEIF <ls_output>-ernam IS NOT INITIAL.
*<<F36K917931 - 09 End of ins
*    IF <ls_output>-ernam IS NOT INITIAL.  "<<F36K917931 - 09 del
      READ TABLE gt_user_data ASSIGNING FIELD-SYMBOL(<ls_user_data>)
        WITH KEY crusr = <ls_output>-ernam.
      IF sy-subrc = 0.
        <ls_output>-pernr     = <ls_user_data>-pernr.
        <ls_output>-full_name = <ls_user_data>-full_name.
*<<F36K917931 - 09 Begin of ins
      ELSE.
        CLEAR:
          lv_ernam,
          ls_addr,
          lt_return.

        lv_ernam = <ls_output>-ernam.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = lv_ernam
*           CACHE_RESULTS           = 'X'
          IMPORTING
            address  = ls_addr
          TABLES
            return   = lt_return.
        <ls_output>-full_name = ls_addr-fullname.
*<<F36K917931 - 09 End of ins
      ENDIF.
    ENDIF.

*<-- Start of Deletion 20.06.2025 (Remove Old code)
*    IF NOT LINE_EXISTS( GT_HISTORY_LOG[ DATA_TYPE = ''
*                                       BUKRS     = <LS_OUTPUT>-BUKRS
*                                       BELNR     = <LS_OUTPUT>-BELNR
*                                       GJAHR     = <LS_OUTPUT>-GJAHR
*                                       BUZEI     = <LS_OUTPUT>-BUZEI ] )
*"<<F36K917931 - 08 Begin of ins
*      OR <LS_OUTPUT>-BILLPL_NO IS NOT INITIAL
*      OR ( <LS_OUTPUT>-ACTION_TYPE IS NOT INITIAL AND
*           <LS_OUTPUT>-STATUS      IS NOT INITIAL ).
*      "<<F36K917931 - 08 End of ins
*--> End of Deletion 20.06.2025
*<-- Start of Insertion 20.06.2025 (Add new Condition)
    IF NOT line_exists( gt_history_log[ data_type = ''
                                       bukrs       = <ls_output>-bukrs
                                       belnr       = <ls_output>-belnr
                                       gjahr       = <ls_output>-gjahr
                                       buzei       = <ls_output>-buzei
                                       billpl_no   = <ls_output>-billpl_no
                                       action_type = <ls_output>-action_type
                                       status      = <ls_output>-status
                                       ] ).
*--> End of Insertion 20.06.2025
      APPEND INITIAL LINE TO gt_history_log ASSIGNING FIELD-SYMBOL(<ls_log>).
      <ls_log> = CORRESPONDING #( <ls_output> ).
    ENDIF.
  ENDLOOP.
*<<F36K914812 - 04 End of ins

*<<F36K917931 - 07 Begin of ins
  DELETE gt_history_log WHERE action_type NOT IN s_hactty
                        OR    status      NOT IN s_hstat.
*<<F36K917931 - 07 End of ins


*<<F36K914812 - 04 Begin of del
*  gt_history_log = VALUE #( BASE gt_history_log FOR ls_output IN gt_output
*                                    ( CORRESPONDING #( ls_output ) ) ) .
*<<F36K914812 - 04 End of del

  LOOP AT gt_history_log ASSIGNING FIELD-SYMBOL(<ls_history_log>).
    IF <ls_history_log>-pernr IS NOT INITIAL. "<<F36K919549 - 01 ins
      PERFORM f_get_pernr_name USING <ls_history_log>-pernr
                           CHANGING <ls_history_log>-full_name.
    ENDIF.                                    "<<F36K919549 - 01 ins

    <ls_history_log>-action_type_text = zcl_sdsfi_coll_log=>get_action_type_text( iv_action_type = <ls_history_log>-action_type ).
    <ls_history_log>-status_text      = zcl_sdsfi_coll_log=>get_status_text( iv_status = <ls_history_log>-status ).

    READ TABLE gt_do
      INTO DATA(ls_do)
      WITH KEY vbeln = <ls_history_log>-vbeln_vf
      BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_history_log>-fkdat = ls_do-fkdat.   "<<F36K911749 ins
*      <ls_history_log>-vbeln = ls_do-aubel.   "<<F36K911780 ins *<<F36K914812 - 02 del
      <ls_history_log>-vgbel = ls_do-vgbel.
      <ls_history_log>-lfdat = ls_do-lfdat.

*<<F36K914812 - 02 Begin of ins
      READ TABLE gt_so INTO DATA(ls_so)
        WITH KEY vbeln = ls_do-aubel
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_history_log>-vbeln   = ls_so-vbeln_so.
        <ls_history_log>-po_ref  = ls_so-bstkd.
      ENDIF.
*<<F36K914812 - 02 End of ins
*<<F36K917931 - 06 Begin of ins
    ELSE.
      PERFORM f_get_sale_doc USING
                                <ls_history_log>-vbeln_vf
                             CHANGING
                                <ls_history_log>-vbeln
                                <ls_history_log>-po_ref.
*<<F36K917931 - 06 End of ins
    ENDIF.

*<<F36K914812 - 05 Begin of ins
    READ TABLE gt_clr_link INTO DATA(ls_clr_link)
      WITH KEY bukrs = <ls_history_log>-bukrs
             belnr = <ls_history_log>-belnr
             gjahr = <ls_history_log>-gjahr
             buzei = <ls_history_log>-buzei
             seq   = <ls_history_log>-seq.
    IF sy-subrc EQ 0.
      <ls_history_log>-bukrs_clr = ls_clr_link-bukrs_clr.
      <ls_history_log>-belnr_clr = ls_clr_link-belnr_clr.
      <ls_history_log>-gjahr_clr = ls_clr_link-gjahr_clr.
*<<F36K917931 - 03 Begin of ins
      <ls_history_log>-xreversed = ls_clr_link-xreversed.
      <ls_history_log>-stblg     = ls_clr_link-stblg.
      <ls_history_log>-stjah     = ls_clr_link-stjah.
*<<F36K917931 - 03 End of ins
    ENDIF.
*<<F36K914812 - 05 End of ins


    READ TABLE gt_onetime INTO DATA(ls_onetime)
      WITH KEY bukrs = <ls_history_log>-bukrs
               belnr = <ls_history_log>-belnr
               gjahr = <ls_history_log>-gjahr
               buzei = <ls_history_log>-buzei.
    IF sy-subrc EQ 0.
      <ls_history_log>-cust_name = |{ ls_onetime-name1 } { ls_onetime-name2 }|.
    ENDIF.


*<<F36K914812 - 07 Begin of ins
    READ TABLE gt_cust_adrc INTO DATA(ls_cust_adrc)
      WITH KEY addrnumber = ls_do-adrnr.
    IF sy-subrc EQ 0.
      <ls_history_log>-cust_name = ls_cust_adrc-name1.
    ENDIF.
*<<F36K914812 - 07 End of ins

    IF gv_edit = gc_true AND <ls_history_log>-belnr_clr IS INITIAL.
      PERFORM f_fill_celltab_log USING 'RW'
                           CHANGING lt_celltab.
    ELSE.
      PERFORM f_fill_celltab_log USING 'RO'
                           CHANGING lt_celltab.
    ENDIF.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_HISTORY_LOG
*&---------------------------------------------------------------------*
FORM f_display_log  USING  ut_log TYPE tt_history_log.

* Set Container name
  gf_container = 'CTL_ALV'.
* Disable Header area
*  gf_alv_header = gc_true.

*   No auto refresh in edit mode
  gf_no_auto_refresh = gc_true.

* ALV Layout
  PERFORM f_alv_layout USING    p_hvar
                       CHANGING gs_layout
                                gs_variant
                                gs_print.

* Assign Output Data
* Assign Size
  gf_header_hight = gc_header_height.
  gf_alv_height   = gc_alv_height.
  ASSIGN ut_log TO <g_list>.                 "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM f_alv_build_fieldcat_log CHANGING gt_fieldcat.
* Sort data
  PERFORM f_alv_sort_log CHANGING gt_sort.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_alv_build_fieldcat_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_alv_build_fieldcat_log  CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <l_fieldcat>   TYPE  lvc_s_fcat.

  DATA:
    ls_fcat TYPE lvc_s_fcat.

* Build Field cat from Structure.
  PERFORM f_prepare_fieldcat_o  USING  gc_structure_log
                              CHANGING ct_fieldcat.

  LOOP AT ct_fieldcat ASSIGNING <l_fieldcat>.
    CASE <l_fieldcat>-fieldname.
      WHEN 'SEL'.
        <l_fieldcat>-edit       = gv_edit.
        <l_fieldcat>-checkbox   = gc_true.
        <l_fieldcat>-col_pos    = 1.
        <l_fieldcat>-key        = gc_true.
        IF rb_hdis = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'DELETE_FLAG'.
        IF rb_hchg = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
        <l_fieldcat>-col_pos    = 2.
        <l_fieldcat>-seltext    = TEXT-f51.
        <l_fieldcat>-scrtext_s  = TEXT-f51.
        <l_fieldcat>-scrtext_m  = TEXT-f51.
        <l_fieldcat>-scrtext_l  = TEXT-f51.
        <l_fieldcat>-reptext    = TEXT-f51.

      WHEN 'VBELN'.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'VBELN_VF'.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'FULL_NAME'.
        <l_fieldcat>-col_pos    = 14  ##NUMBER_OK.
      WHEN 'CUST_NAME'.
        <l_fieldcat>-col_pos    = 16  ##NUMBER_OK.
      WHEN 'DOC_DELETE'.
        IF rb_hchg = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
        <l_fieldcat>-seltext    = TEXT-f52.
        <l_fieldcat>-scrtext_s  = TEXT-f52.
        <l_fieldcat>-scrtext_m  = TEXT-f52.
        <l_fieldcat>-scrtext_l  = TEXT-f52.
        <l_fieldcat>-reptext    = TEXT-f52.

      WHEN 'BELNR'.
        <l_fieldcat>-hotspot    = gc_true.

      WHEN 'BUKRS_CLR'.
*        IF rb_hchg = gc_true.
        <l_fieldcat>-tech     = gc_true.
*        ENDIF.
      WHEN 'BELNR_CLR'.
        <l_fieldcat>-seltext    = TEXT-f53.
        <l_fieldcat>-reptext    = TEXT-f53.
        <l_fieldcat>-scrtext_s  = TEXT-f53.
        <l_fieldcat>-scrtext_m  = TEXT-f53.
        <l_fieldcat>-scrtext_l  = TEXT-f53.

        IF rb_hchg = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.

        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'GJAHR_CLR'.
        IF rb_hchg = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.

        <l_fieldcat>-seltext    = TEXT-f57.
        <l_fieldcat>-reptext    = TEXT-f57.
        <l_fieldcat>-scrtext_s  = TEXT-f57.
        <l_fieldcat>-scrtext_m  = TEXT-f57.
        <l_fieldcat>-scrtext_l  = TEXT-f57.

*<<F36K917931 - 03 Begin of ins
      WHEN 'XREVERSED'.
        IF rb_hchg = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
      WHEN 'STBLG'.
        IF rb_hchg = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.

        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'STJAH'.
        IF rb_hchg = gc_true.
          <l_fieldcat>-tech     = gc_true.
        ENDIF.
*<<F36K917931 - 03 End of ins

        "Repleace display of action type & status with text
      WHEN 'ACTION_TYPE'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'ACTION_TYPE_TEXT'.
        READ TABLE ct_fieldcat
          INTO ls_fcat
          WITH KEY fieldname = 'ACTION_TYPE' .
        IF sy-subrc EQ 0.
          <l_fieldcat>-col_pos = ls_fcat-col_pos.
        ENDIF.
      WHEN 'STATUS'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'STATUS_TEXT'.
        READ TABLE ct_fieldcat
          INTO ls_fcat
          WITH KEY fieldname = 'STATUS' .
        IF sy-subrc EQ 0.
          <l_fieldcat>-col_pos = ls_fcat-col_pos.
        ENDIF.
      WHEN 'VGBEL'.
        <l_fieldcat>-col_pos    = 11  ##NUMBER_OK.
        <l_fieldcat>-reptext    = TEXT-f12.
        <l_fieldcat>-seltext    = TEXT-f12.
        <l_fieldcat>-scrtext_s  = TEXT-f12.
        <l_fieldcat>-scrtext_m  = TEXT-f12.
        <l_fieldcat>-scrtext_l  = TEXT-f12.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'LFDAT'.
        <l_fieldcat>-col_pos    = 11  ##NUMBER_OK.

      WHEN 'ERNAM_HIST'.
        <l_fieldcat>-seltext    = TEXT-f54.
        <l_fieldcat>-reptext    = TEXT-f54.
        <l_fieldcat>-scrtext_s  = TEXT-f54.
        <l_fieldcat>-scrtext_m  = TEXT-f54.
        <l_fieldcat>-scrtext_l  = TEXT-f54.

      WHEN 'ERDAT_HIST'.
        <l_fieldcat>-seltext    = TEXT-f55.
        <l_fieldcat>-reptext    = TEXT-f55.
        <l_fieldcat>-scrtext_s  = TEXT-f55.
        <l_fieldcat>-scrtext_m  = TEXT-f55.
        <l_fieldcat>-scrtext_l  = TEXT-f55.

      WHEN 'ERZMT_HIST'.
        <l_fieldcat>-seltext    = TEXT-f56.
        <l_fieldcat>-reptext    = TEXT-f56.
        <l_fieldcat>-scrtext_s  = TEXT-f56.
        <l_fieldcat>-scrtext_m  = TEXT-f56.
        <l_fieldcat>-scrtext_l  = TEXT-f56.

    ENDCASE.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_fill_celltab_log
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM f_fill_celltab_log  USING VALUE(uf_mode) TYPE char02
                         CHANGING ct_celltab  TYPE lvc_t_styl   ##CALLED.

  DATA: lv_mode    TYPE raw4.

  IF uf_mode EQ 'RW'.
    lv_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE. "uf_mode eq 'RO'
    lv_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  PERFORM f_add_celltab USING lv_mode:
        'SEL'           CHANGING ct_celltab.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_LOG
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM f_alv_sort_log  CHANGING ct_sort TYPE lvc_t_sort.

  CONSTANTS:
    lc_sort1 TYPE  lvc_s_sort-fieldname VALUE 'BUKRS',
    lc_sort2 TYPE  lvc_s_sort-fieldname VALUE 'GJAHR',
    lc_sort3 TYPE  lvc_s_sort-fieldname VALUE 'BELNR',
    lc_sort4 TYPE  lvc_s_sort-fieldname VALUE 'BUZEI',
    lc_sort5 TYPE  lvc_s_sort-fieldname VALUE 'SEQ',
    lc_sort6 TYPE  lvc_s_sort-fieldname VALUE 'SUB_SEQ'.
  DATA:
    ls_sort  TYPE  lvc_s_sort.

* Initialize Output
  CLEAR: ct_sort.

  CLEAR ls_sort.
  ls_sort-spos      = 1.
  ls_sort-fieldname = lc_sort1.
  ls_sort-up        = gc_true.
  ls_sort-down      = space.
  APPEND ls_sort TO ct_sort.

  CLEAR ls_sort.
  ls_sort-spos      = 2.
  ls_sort-fieldname = lc_sort2.
  ls_sort-up        = space.
  ls_sort-down      = gc_true.
  APPEND ls_sort TO ct_sort.

  CLEAR ls_sort.
  ls_sort-spos      = 3.
  ls_sort-fieldname = lc_sort3.
  ls_sort-up        = gc_true.
  ls_sort-down      = space.
*  ls_sort-subtot    = gc_true.
  APPEND ls_sort TO ct_sort.

  CLEAR ls_sort.
  ls_sort-spos      = 4.
  ls_sort-fieldname = lc_sort4.
  ls_sort-up        = gc_true.
  ls_sort-down      = space.
  APPEND ls_sort TO ct_sort.

  CLEAR ls_sort.
  ls_sort-spos      = 5.
  ls_sort-fieldname = lc_sort5.
  ls_sort-up        = gc_true.
  ls_sort-down      = space.
  APPEND ls_sort TO ct_sort.

  CLEAR ls_sort.
  ls_sort-spos      = 6.
  ls_sort-fieldname = lc_sort6.
  ls_sort-up        = gc_true.
  ls_sort-down      = space.
  APPEND ls_sort TO ct_sort.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_adjust_history_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_HISTORY_LOG
*&---------------------------------------------------------------------*
FORM f_adjust_history_log  USING ut_history_log TYPE tt_history_log.

  DATA:
    lt_del            TYPE STANDARD TABLE OF gty_log_del,
    lt_coll_log       TYPE STANDARD TABLE OF zsdsfit029,
    lt_bank_statement TYPE tt_bank_statement.


  lt_del = VALUE #( FOR ls_output IN ut_history_log WHERE ( sel = 'X'  ) ( CORRESPONDING #( ls_output  ) )  ).

  DATA(lt_history_log) = ut_history_log.

  SORT lt_del BY bukrs belnr gjahr buzei seq sub_seq DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_del COMPARING bukrs belnr gjahr buzei seq.

  IF rb_hchg IS INITIAL.
    RETURN.
  ENDIF.

  SORT lt_history_log BY bukrs belnr gjahr buzei seq sub_seq DESCENDING.

  LOOP AT lt_del ASSIGNING FIELD-SYMBOL(<ls_del>)
    WHERE delete_flag = gc_true.

    PERFORM f_adjust_coll_log USING <ls_del>
                                    lt_history_log
                              CHANGING lt_coll_log.

    <ls_del>-col_ind-delete_date = gc_true.
    <ls_del>-col_ind-delete_flag = gc_true.

    PERFORM f_reset_transfer_no USING <ls_del>
                                CHANGING
                                      lt_bank_statement.

  ENDLOOP.

  IF lt_del IS NOT INITIAL.
    UPDATE zsdsfit038
      FROM TABLE @lt_del INDICATORS SET STRUCTURE col_ind.
  ENDIF.

  IF lt_coll_log IS NOT INITIAL.
    MODIFY zsdsfit029 FROM TABLE lt_coll_log.
    "Collection log saved
    MESSAGE s000(38) WITH TEXT-r02.
  ENDIF.

  IF lt_bank_statement IS NOT INITIAL.
    SORT lt_bank_statement BY hbkid zbank_item trnfer_number DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_bank_statement COMPARING hbkid	zbank_item.

    UPDATE zsdsfit042
      FROM TABLE @lt_bank_statement INDICATORS SET STRUCTURE col_ind.
  ENDIF.

  COMMIT WORK AND WAIT. "Added 02.12.2024

  gref_grid->set_ready_for_input(
    EXPORTING
      i_ready_for_input = 0 ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_mark_sel_all_hist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SEL
*&      <-- GT_OUTPUT_NEW
*&---------------------------------------------------------------------*
FORM f_mark_sel_all_hist  USING    uf_sel    TYPE char01
                         CHANGING ct_output TYPE tt_history_log.

  LOOP AT ct_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    <ls_output>-sel = uf_sel.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_del_history_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_HISTORY_LOG
*&---------------------------------------------------------------------*
FORM f_del_history_log  CHANGING ct_history_log TYPE tt_history_log.

  LOOP AT ct_history_log ASSIGNING FIELD-SYMBOL(<ls_history_log>)
    WHERE sel = gc_true.
    <ls_history_log>-delete_flag = gc_true.
    <ls_history_log>-delete_date = sy-datum.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_adjust_coll_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_DEL>
*&      --> GT_HISTORY_LOG
*&      <-- LT_COLL_LOG
*&---------------------------------------------------------------------*
FORM f_adjust_coll_log  USING    us_del         TYPE gty_log_del
                                 ut_history_log TYPE tt_history_log
                        CHANGING ct_coll_log    TYPE tt_bplog.

  LOOP AT ut_history_log INTO DATA(ls_history_log)  ##INTO_OK
    WHERE bukrs = us_del-bukrs
    AND   belnr = us_del-belnr
    AND   gjahr = us_del-gjahr
    AND   buzei = us_del-buzei
    AND   seq   = us_del-seq
    AND   delete_flag IS INITIAL.

    READ TABLE gt_bplog INTO DATA(ls_coll_log)
      WITH KEY data_type = us_del-data_type "<<F36K910583 ins
               bukrs = us_del-bukrs
               belnr = us_del-belnr
               gjahr = us_del-gjahr
               buzei = us_del-buzei
               seq   = us_del-seq.

    IF ls_history_log-pernr       <> ls_coll_log-pernr OR
       ls_history_log-work_date   <> ls_coll_log-work_date OR
       ls_history_log-action_type <> ls_coll_log-action_type OR
       ls_history_log-status      <> ls_coll_log-status.

      APPEND INITIAL LINE TO ct_coll_log ASSIGNING FIELD-SYMBOL(<ls_coll_log>).
      <ls_coll_log> = CORRESPONDING #( ls_history_log ).
    ENDIF.

    EXIT.
  ENDLOOP.
  "No active item found
  IF sy-subrc NE 0.
    READ TABLE gt_bplog INTO ls_coll_log
      WITH KEY data_type = us_del-data_type "<<F36K910583 ins
               bukrs = us_del-bukrs
               belnr = us_del-belnr
               gjahr = us_del-gjahr
               buzei = us_del-buzei
               seq   = us_del-seq.
    IF sy-subrc EQ 0.
      ls_coll_log-delete_date = sy-datum.
      ls_coll_log-delete_flag = gc_true.

      APPEND ls_coll_log TO ct_coll_log.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_lock_update_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_HISTORY_LOG
*&---------------------------------------------------------------------*
FORM f_lock_update_log  USING ut_bplog TYPE tt_bplog.

*   <<F36K910583 start ins
  IF rb_hdis = gc_true.
    RETURN.
  ENDIF.
*   <<F36K910583 end ins

  LOOP AT ut_bplog INTO DATA(ls_output)    ##INTO_OK
  GROUP BY ( key1 = ls_output-kunnr
             key2 = ls_output-bukrs
             key3 = ls_output-belnr
             key4 = ls_output-gjahr ).

    CALL FUNCTION 'ENQUEUE_EZSDSFIS090'
      EXPORTING
        mode_zsdsfis090 = 'E'
        kunnr           = ls_output-kunnr
        bukrs           = ls_output-bukrs
        belnr           = ls_output-belnr
        gjahr           = ls_output-gjahr
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
*& Form f_unlock_tran_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_BPLOG
*&---------------------------------------------------------------------*
FORM f_unlock_tran_log  USING ut_bplog TYPE tt_bplog.

  LOOP AT ut_bplog INTO DATA(ls_bplog)      ##INTO_OK
    GROUP BY ( key1 = ls_bplog-kunnr
               key2 = ls_bplog-bukrs
               key3 = ls_bplog-belnr
               key4 = ls_bplog-gjahr ).

    CALL FUNCTION 'DEQUEUE_EZSDSFIS090'
      EXPORTING
        mode_zsdsfis090 = 'E'
        mandt           = sy-mandt
        kunnr           = ls_bplog-kunnr
        bukrs           = ls_bplog-bukrs
        belnr           = ls_bplog-belnr
        gjahr           = ls_bplog-gjahr
*       _SCOPE          = '3'
*       _SYNCHRON       = ' '
*       _COLLECT        = ' '
      .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_reset_transfer_no
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_DEL>
*&      --> GT_HISTORY_LOG
*&      <-- LT_BANK_STATEMENT
*&---------------------------------------------------------------------*
FORM f_reset_transfer_no  USING    uf_del TYPE gty_log_del
                          CHANGING ct_bank_statement TYPE tt_bank_statement.

  DATA lv_found TYPE abap_bool.

  IF uf_del-tranf_no IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT gt_bplog
    TRANSPORTING NO FIELDS
    WHERE data_type = uf_del-data_type
    AND   tranf_no  = uf_del-tranf_no
    AND NOT ( bukrs  = uf_del-bukrs
        AND   belnr  = uf_del-belnr
        AND   gjahr  = uf_del-gjahr
        AND   buzei  = uf_del-buzei
        AND   seq    = uf_del-seq
    ).
    lv_found = gc_true.
    EXIT.
  ENDLOOP.

  "No longer use with other item -> reset transfer number in bank statement
  IF lv_found IS INITIAL.
    APPEND INITIAL LINE TO ct_bank_statement ASSIGNING FIELD-SYMBOL(<ls_bank>).
    <ls_bank>-hbkid           = uf_del-hbkid.
    <ls_bank>-zbank_item      = uf_del-zbank_item.
    CLEAR:
      <ls_bank>-map_status,
      <ls_bank>-trnfer_number,
      <ls_bank>-fyear_trnferno.
    <ls_bank>-col_ind-trnfer_number   = gc_true.
    <ls_bank>-col_ind-fyear_trnferno  = gc_true.
    <ls_bank>-col_ind-map_status      = gc_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_history_open_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_history_open_item .

  DATA:lv_data_type TYPE zsdsfit029-data_type VALUE ''.

  IF "s_hwrkdt IS NOT INITIAL OR  "-20.06.2025
     "S_HPERNR IS NOT INITIAL OR  "-20.06.2025
     s_htrnf  IS NOT INITIAL OR
     s_hfllw  IS NOT INITIAL OR
     s_hbnkdt IS NOT INITIAL OR
     s_hbankl IS NOT INITIAL OR
     s_hpymt  IS NOT INITIAL OR
     s_hchq   IS NOT INITIAL OR
     lv_data_type NOT IN s_htype.
    RETURN.
  ENDIF.

*<<F36K919549 - 01 Begin of del
*  IF
*     s_hbill   IS NOT INITIAL OR
*     s_hpldat  IS NOT INITIAL.
*     s_hactty  IS NOT INITIAL OR
*     s_hstat   IS NOT INITIAL.
*    SELECT
**      bill_pl~billpl_pmtdt AS work_date, "<<F36K914812 - 06 del
*      bill_pl~billpl_date AS work_date,   "<<F36K914812 - 06 ins
*      opn~bukrs,
*      opn~belnr,
*      opn~gjahr,
*      opn~buzei,
*      opn~blart,
*      opn~xref2 AS billpl_no,
*      bill_pl~billpl_date,
*      bill_pl~action_type,
*      bill_pl~status,
*      bill_pl~billpl_pmtdt AS payment_date,
**<<F36K914812 - 04 Begin of del
**      bill_pl~crusr AS update_by,
**      bill_pl~crdat AS update_on,
**      bill_pl~crtim AS update_time,
**<<F36K914812 - 04 End of del
**<<F36K914812 - 04 Begin of ins
*      bill_pl~crusr AS ernam,
*      bill_pl~crdat AS erdat,
*      bill_pl~crtim AS erzmt,
**<<F36K914812 - 04 End of ins
**      @gv_pernr AS pernr,
**      @gv_fullname AS full_name,
**<<F36K917931 - 09 Begin of ins
*      bill_pl~cr_pernr AS pernr,
**<<F36K917931 - 09 Begin of ins
*      opn~kunnr,
*      opn~umskz,
*      opn~xblnr,
*      opn~bldat,
*      opn~budat,
*      opn~zfbdt,
*      opn~zterm,
*      @gs_default_coll_log-prctr AS prctr,
*      @gs_default_coll_log-kostl AS kostl,
*      CASE
*        WHEN cust~ktokd = 'Z010' THEN
*          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
*        WHEN cust~ktokd = 'Z050' THEN
*          concat_with_space( partner~name_first, partner~name_last, 1 )
*        ELSE
*          concat_with_space( partner~name_org1, partner~name_org2, 1 )
*      END AS cust_name,
*      opn~wrbtr,
*      opn~waers,
*      opn~shkzg,
*      opn~bupla AS brnch,
*      opn~belnr AS receipt_no,
*      hdr~awkey  AS vbeln_vf,
**<<F36K917931 - 08 Begin of ins
*      bill_pl_itm~billpl_amt,
*      bill_pl~updat AS update_on,
*      bill_pl~uptim AS update_time,
*      bill_pl~upusr AS update_by
**<<F36K917931 - 08 End of ins
*      FROM bsid_view AS opn
*      INNER JOIN acdoca AS docu
*      ON  docu~rbukrs = opn~bukrs
*      AND docu~belnr  = opn~belnr
*      AND docu~gjahr  = opn~gjahr
*      AND docu~buzei  = opn~buzei
*      INNER JOIN bkpf AS hdr
*      ON  hdr~bukrs = opn~bukrs
*      AND hdr~belnr = opn~belnr
*      AND hdr~gjahr = opn~gjahr
*      INNER JOIN t001 AS comp
*      ON  opn~bukrs = comp~bukrs
*      AND opn~waers = comp~waers
*      INNER JOIN kna1 AS cust
*      ON opn~kunnr = cust~kunnr
*      INNER JOIN cvi_cust_link AS link
*      ON cust~kunnr = link~customer
*      INNER JOIN but000 AS partner
*      ON partner~partner_guid = link~partner_guid
*      INNER JOIN zsdsfit033 AS bill_pl
*      ON opn~xref2 = bill_pl~billpl_no
**<<F36K917931 - 08 Begin of ins
*      INNER JOIN zsdsfit035 AS bill_pl_itm
*      ON    bill_pl_itm~billpl_no   = opn~xref2
*      AND   bill_pl_itm~bukrs       = opn~bukrs
*      AND   bill_pl_itm~belnr       = opn~belnr
*      AND   bill_pl_itm~gjahr       = opn~gjahr
*      AND   bill_pl_itm~buzei       = opn~buzei
*      AND   bill_pl_itm~delfg       = ''
**<<F36K917931 - 08 End of ins
*      WHERE opn~kunnr           IN @s_hkunnr
*      AND   opn~blart           IN @gr_blart
*      AND   opn~umskz           IN @gr_spgl
*      AND   opn~umskz           IN @s_humskz
*      AND   opn~belnr           IN @s_hbelnr
*      AND   opn~gjahr           IN @s_hgjahr
*      AND   opn~xblnr           IN @s_hxblnr
*      AND   bill_pl~billpl_no   IN @s_hbill
*      AND   bill_pl~billpl_date IN @s_hpldat
*      AND   bill_pl~action_type IN @s_hactty
*      AND   bill_pl~status      IN @s_hstat
*      AND   hdr~awkey           IN @s_hvbeln
*      AND   bill_pl~delfg       = ''
*      AND   docu~rldnr          = '0L'
*      INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS. "#EC CI_BUFFJOIN
*  ELSE.
*    SELECT
**      bill_pl~billpl_pmtdt AS work_date,  "<<F36K914812 - 06 del
*      bill_pl~billpl_date AS work_date,   "<<F36K914812 - 06 ins
*      opn~bukrs,
*      opn~belnr,
*      opn~gjahr,
*      opn~buzei,
*      opn~blart,
*      opn~xref2 AS billpl_no,
*      bill_pl~billpl_date,
*      bill_pl~action_type,
*      bill_pl~status,
*      bill_pl~billpl_pmtdt AS payment_date,
**<<F36K914812 - 04 Begin of del
**      bill_pl~crusr AS update_by,
**      bill_pl~crdat AS update_on,
**      bill_pl~crtim AS update_time,
**<<F36K914812 - 04 Begin of del
**<<F36K914812 - 04 Begin of ins
*      bill_pl~crusr AS ernam,
*      bill_pl~crdat AS erdat,
*      bill_pl~crtim AS erzmt,
**<<F36K914812 - 04 End of ins
**<<F36K917931 - 09 Begin of ins
*      bill_pl~cr_pernr AS pernr,
**<<F36K917931 - 09 Begin of ins
**      @gv_pernr AS pernr,
**      @gv_fullname AS full_name,
*      opn~kunnr,
*      opn~umskz,
*      opn~xblnr,
*      opn~bldat,
*      opn~budat,
*      opn~zfbdt,
*      opn~zterm,
*      @gs_default_coll_log-prctr AS prctr,
*      @gs_default_coll_log-kostl AS kostl,
*      CASE
*        WHEN cust~ktokd = 'Z010' THEN
*          concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
*        WHEN cust~ktokd = 'Z050' THEN
*          concat_with_space( partner~name_first, partner~name_last, 1 )
*        ELSE
*          concat_with_space( partner~name_org1, partner~name_org2, 1 )
*      END AS cust_name,
*      opn~wrbtr,
*      opn~waers,
*      opn~shkzg,
*      opn~bupla AS brnch,
*      opn~belnr AS receipt_no,
*      hdr~awkey  AS vbeln_vf,
**<<F36K917931 - 08 Begin of ins
*      bill_pl_itm~billpl_amt,
*      bill_pl~updat AS update_on,
*      bill_pl~uptim AS update_time,
*      bill_pl~upusr AS update_by
**<<F36K917931 - 08 End of ins
*      FROM bsid_view AS opn
**<<F36K914812 - 07 Begin of del
**      INNER JOIN acdoca AS docu
**      ON  docu~rbukrs = opn~bukrs
**      AND docu~belnr  = opn~belnr
**      AND docu~gjahr  = opn~gjahr
**      AND docu~buzei  = opn~buzei
**<<F36K914812 - 07 End of del
*      INNER JOIN bkpf AS hdr
*      ON  hdr~bukrs = opn~bukrs
*      AND hdr~belnr = opn~belnr
*      AND hdr~gjahr = opn~gjahr
*      INNER JOIN t001 AS comp
*      ON  opn~bukrs = comp~bukrs
*      AND opn~waers = comp~waers
*      INNER JOIN kna1 AS cust
*      ON opn~kunnr = cust~kunnr
*      INNER JOIN cvi_cust_link AS link
*      ON cust~kunnr = link~customer
*      INNER JOIN but000 AS partner
*      ON partner~partner_guid = link~partner_guid
*      LEFT JOIN zsdsfit033 AS bill_pl
*      ON    bill_pl~billpl_no   = opn~xref2
*      AND   bill_pl~delfg       = ''
**<<F36K917931 - 08 Begin of ins
*      LEFT JOIN zsdsfit035 AS bill_pl_itm
*      ON    bill_pl_itm~billpl_no   = opn~xref2
*      AND   bill_pl_itm~bukrs       = opn~bukrs
*      AND   bill_pl_itm~belnr       = opn~belnr
*      AND   bill_pl_itm~gjahr       = opn~gjahr
*      AND   bill_pl_itm~buzei       = opn~buzei
*      AND   bill_pl_itm~delfg       = ''
**<<F36K917931 - 08 End of ins
*      WHERE opn~kunnr           IN @s_hkunnr
*      AND   opn~blart           IN @gr_blart
*      AND   opn~umskz           IN @gr_spgl
*      AND   opn~umskz           IN @s_humskz
*      AND   opn~belnr           IN @s_hbelnr
*      AND   opn~gjahr           IN @s_hgjahr
*      AND   opn~xblnr           IN @s_hxblnr
*      AND   hdr~awkey           IN @s_hvbeln
**      AND   docu~rldnr          = '0L'  "<<F36K914812 - 07 del
*      INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS. "#EC CI_BUFFJOIN
*  ENDIF.
*<<F36K919549 - 01 End of del

*<<F36K919549 - 01 Begin of ins
  SELECT
    bill_pl~billpl_date AS work_date,
    opn~bukrs,
    opn~belnr,
    opn~gjahr,
    opn~buzei,
    opn~blart,
    opn~xref2 AS billpl_no,
    bill_pl~billpl_date,
    bill_pl~action_type,
    bill_pl~status,
    bill_pl~billpl_pmtdt AS payment_date,
    CASE
      WHEN bill_pl~crusr <> ' ' THEN bill_pl~crusr
      ELSE hdr~usnam
    END AS ernam,
    bill_pl~crdat AS erdat,
    bill_pl~crtim AS erzmt,
    bill_pl~cr_pernr AS pernr,
    opn~kunnr,
    opn~umskz,
    opn~xblnr,
    opn~bldat,
    opn~budat,
    opn~zfbdt,
    opn~zterm,
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
    hdr~awkey  AS vbeln_vf,
    bill_pl_itm~billpl_amt,
    bill_pl~updat AS update_on,
    bill_pl~uptim AS update_time,
    bill_pl~upusr AS update_by
    FROM bsid_view AS opn
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
    LEFT JOIN zsdsfit033 AS bill_pl
    ON    bill_pl~billpl_no   = opn~xref2
    AND   bill_pl~delfg       = ''
    LEFT JOIN zsdsfit035 AS bill_pl_itm
    ON    bill_pl_itm~billpl_no   = opn~xref2
    AND   bill_pl_itm~bukrs       = opn~bukrs
    AND   bill_pl_itm~belnr       = opn~belnr
    AND   bill_pl_itm~gjahr       = opn~gjahr
    AND   bill_pl_itm~buzei       = opn~buzei
    AND   bill_pl_itm~delfg       = ''
    WHERE opn~kunnr           IN @s_hkunnr
    AND   opn~blart           IN @gr_blart
    AND   opn~umskz           IN @gr_spgl
    AND   opn~umskz           IN @s_humskz
    AND   opn~belnr           IN @s_hbelnr
    AND   opn~gjahr           IN @s_hgjahr
    AND   opn~xblnr           IN @s_hxblnr
    AND   hdr~awkey           IN @s_hvbeln
    AND   hdr~xreversed       =  ''
*<<F36K919968 - 01 Begin of del
**<-- Start of Insertion 20.06.2025 (Add Criterias)
*    AND   bill_pl~billpl_date IN @s_hwrkdt
*    AND   bill_pl~cr_pernr    IN @s_hpernr
**--> End of Insertion 20.06.2025
*<<F36K919968 - 01 End of del
 UNION
    SELECT
    bill_pl~billpl_date AS work_date,
    clr~bukrs,
    clr~belnr,
    clr~gjahr,
    clr~buzei,
    clr~blart,
    clr~xref2 AS billpl_no,
    bill_pl~billpl_date,
    bill_pl~action_type,
    bill_pl~status,
    bill_pl~billpl_pmtdt AS payment_date,
    CASE
      WHEN bill_pl~crusr <> ' ' THEN bill_pl~crusr
      ELSE hdr~usnam
    END AS ernam,
    bill_pl~crdat AS erdat,
    bill_pl~crtim AS erzmt,
    bill_pl~cr_pernr AS pernr,
    clr~kunnr,
    clr~umskz,
    clr~xblnr,
    clr~bldat,
    clr~budat,
    clr~zfbdt,
    clr~zterm,
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
    clr~wrbtr,
    clr~waers,
    clr~shkzg,
    clr~bupla AS brnch,
    clr~belnr AS receipt_no,
    hdr~awkey  AS vbeln_vf,
    bill_pl_itm~billpl_amt,
    bill_pl~updat AS update_on,
    bill_pl~uptim AS update_time,
    bill_pl~upusr AS update_by
    FROM bsad_view AS clr
    INNER JOIN bkpf AS hdr
    ON  hdr~bukrs = clr~bukrs
    AND hdr~belnr = clr~belnr
    AND hdr~gjahr = clr~gjahr
    INNER JOIN t001 AS comp
    ON  clr~bukrs = comp~bukrs
    AND clr~waers = comp~waers
    INNER JOIN kna1 AS cust
    ON clr~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    LEFT JOIN zsdsfit033 AS bill_pl
    ON    bill_pl~billpl_no   = clr~xref2
    AND   bill_pl~delfg       = ''
    LEFT JOIN zsdsfit035 AS bill_pl_itm
    ON    bill_pl_itm~billpl_no   = clr~xref2
    AND   bill_pl_itm~bukrs       = clr~bukrs
    AND   bill_pl_itm~belnr       = clr~belnr
    AND   bill_pl_itm~gjahr       = clr~gjahr
    AND   bill_pl_itm~buzei       = clr~buzei
    AND   bill_pl_itm~delfg       = ''
    WHERE clr~kunnr           IN @s_hkunnr
    AND   clr~blart           IN @gr_blart
    AND   clr~umskz           IN @gr_spgl
    AND   clr~umskz           IN @s_humskz
    AND   clr~belnr           IN @s_hbelnr
    AND   clr~gjahr           IN @s_hgjahr
    AND   clr~xblnr           IN @s_hxblnr
    AND   hdr~awkey           IN @s_hvbeln
    AND   hdr~xreversed       =  ''
*<<F36K919968 - 01 Begin of del
**<-- Start of Insertion 20.06.2025 (Add Criterias)
*    AND   bill_pl~billpl_date IN @s_hwrkdt
*    AND   bill_pl~cr_pernr    IN @s_hpernr
**--> End of Insertion 20.06.2025
*<<F36K919968 - 01 End of del
    INTO CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS. "#EC CI_BUFFJOIN

*"<<F36K919968 - 01 Begin of ins
  DELETE gt_output WHERE
      action_type NOT IN s_hactty OR
      status      NOT IN s_hstat.

  SELECT
    inv_chk~zrcvd_d AS work_date,
    opn~bukrs,
    opn~belnr,
    opn~gjahr,
    opn~buzei,
    opn~blart,
    opn~xref2 AS billpl_no,
    bill_pl~billpl_date,
    cfg~bl_actty AS action_type,
    cfg~bl_stat AS status,
    bill_pl~billpl_pmtdt AS payment_date,
    CASE
      WHEN inv_chk~zrcvd_n <> ' ' THEN inv_chk~zrcvd_n
      ELSE hdr~usnam
    END AS ernam,
    inv_chk~zrcvd_d AS erdat,
    inv_chk~zrcvd_t AS erzmt,
    inv_chk~zrcvd_pernr AS pernr,
    opn~kunnr,
    opn~umskz,
    opn~xblnr,
    opn~bldat,
    opn~budat,
    opn~zfbdt,
    opn~zterm,
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
    hdr~awkey  AS vbeln_vf,
    bill_pl_itm~billpl_amt,
    bill_pl~updat AS update_on,
    bill_pl~uptim AS update_time,
    bill_pl~upusr AS update_by
    FROM bsid_view AS opn
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
    INNER JOIN zsdsfit027 AS inv_chk
    ON inv_chk~vbeln = hdr~awkey
    INNER JOIN zsdsfic028 AS  cfg
    ON inv_chk~zstat_act = cfg~stat_act
    LEFT JOIN zsdsfit033 AS bill_pl
    ON    bill_pl~billpl_no   = opn~xref2
    AND   bill_pl~delfg       = ''
    LEFT JOIN zsdsfit035 AS bill_pl_itm
    ON    bill_pl_itm~billpl_no   = opn~xref2
    AND   bill_pl_itm~bukrs       = opn~bukrs
    AND   bill_pl_itm~belnr       = opn~belnr
    AND   bill_pl_itm~gjahr       = opn~gjahr
    AND   bill_pl_itm~buzei       = opn~buzei
    AND   bill_pl_itm~delfg       = ''
    WHERE opn~kunnr           IN @s_hkunnr
    AND   opn~blart           IN @gr_blart
    AND   opn~umskz           IN @gr_spgl
    AND   opn~umskz           IN @s_humskz
    AND   opn~belnr           IN @s_hbelnr
    AND   opn~gjahr           IN @s_hgjahr
    AND   opn~xblnr           IN @s_hxblnr
    AND   hdr~awkey           IN @s_hvbeln
    AND   hdr~xreversed       =  ''
    AND   cfg~bl_actty        IN @s_hactty
    AND   cfg~bl_stat         IN @s_hstat
    AND   inv_chk~zrcvd_pernr IN @s_hpernr
    AND   inv_chk~zrcvd_d     IN @s_hwrkdt
 UNION
    SELECT
    inv_chk~zrcvd_d AS work_date,
    clr~bukrs,
    clr~belnr,
    clr~gjahr,
    clr~buzei,
    clr~blart,
    clr~xref2 AS billpl_no,
    bill_pl~billpl_date,
    cfg~bl_actty AS action_type,
    cfg~bl_stat AS status,
    bill_pl~billpl_pmtdt AS payment_date,
    CASE
      WHEN inv_chk~zrcvd_n <> ' ' THEN inv_chk~zrcvd_n
      ELSE hdr~usnam
    END AS ernam,
    inv_chk~zrcvd_d AS erdat,
    inv_chk~zrcvd_t AS erzmt,
    inv_chk~zrcvd_pernr AS pernr,
    clr~kunnr,
    clr~umskz,
    clr~xblnr,
    clr~bldat,
    clr~budat,
    clr~zfbdt,
    clr~zterm,
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
    clr~wrbtr,
    clr~waers,
    clr~shkzg,
    clr~bupla AS brnch,
    clr~belnr AS receipt_no,
    hdr~awkey  AS vbeln_vf,
    bill_pl_itm~billpl_amt,
    bill_pl~updat AS update_on,
    bill_pl~uptim AS update_time,
    bill_pl~upusr AS update_by
    FROM bsad_view AS clr
    INNER JOIN bkpf AS hdr
    ON  hdr~bukrs = clr~bukrs
    AND hdr~belnr = clr~belnr
    AND hdr~gjahr = clr~gjahr
    INNER JOIN t001 AS comp
    ON  clr~bukrs = comp~bukrs
    AND clr~waers = comp~waers
    INNER JOIN kna1 AS cust
    ON clr~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    INNER JOIN zsdsfit027 AS inv_chk
    ON inv_chk~vbeln = hdr~awkey
    INNER JOIN zsdsfic028 AS  cfg
    ON inv_chk~zstat_act = cfg~stat_act
    LEFT JOIN zsdsfit033 AS bill_pl
    ON    bill_pl~billpl_no   = clr~xref2
    AND   bill_pl~delfg       = ''
    LEFT JOIN zsdsfit035 AS bill_pl_itm
    ON    bill_pl_itm~billpl_no   = clr~xref2
    AND   bill_pl_itm~bukrs       = clr~bukrs
    AND   bill_pl_itm~belnr       = clr~belnr
    AND   bill_pl_itm~gjahr       = clr~gjahr
    AND   bill_pl_itm~buzei       = clr~buzei
    AND   bill_pl_itm~delfg       = ''
    WHERE clr~kunnr           IN @s_hkunnr
    AND   clr~blart           IN @gr_blart
    AND   clr~umskz           IN @gr_spgl
    AND   clr~umskz           IN @s_humskz
    AND   clr~belnr           IN @s_hbelnr
    AND   clr~gjahr           IN @s_hgjahr
    AND   clr~xblnr           IN @s_hxblnr
    AND   hdr~awkey           IN @s_hvbeln
    AND   hdr~xreversed       =  ''
    AND   cfg~bl_actty        IN @s_hactty
    AND   cfg~bl_stat         IN @s_hstat
    AND   inv_chk~zrcvd_pernr IN @s_hpernr
    AND   inv_chk~zrcvd_d     IN @s_hwrkdt
    APPENDING CORRESPONDING FIELDS OF TABLE @gt_output ##TOO_MANY_ITAB_FIELDS. "#EC CI_BUFFJOIN

  DELETE gt_output WHERE
      action_type IS INITIAL AND
      status      IS INITIAL.
*"<<F36K919968 - 01 End of ins

  IF s_hbill   IS NOT INITIAL OR
     s_hpldat  IS NOT INITIAL.
    DELETE gt_output WHERE
          billpl_no   NOT IN s_hbill OR
          billpl_date NOT IN s_hpldat.
  ENDIF.

*"<<F36K919968 - 01 Begin of del
*  SORT gt_output BY bukrs belnr gjahr buzei.
*  DELETE ADJACENT DUPLICATES FROM gt_output COMPARING bukrs belnr gjahr buzei.
*"<<F36K919968 - 01 End of del

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
*      INTO TABLE @gt_onetime.                      "#EC CI_NO_TRANSFORM "<<F36K920651 - 01 del
      APPENDING TABLE @gt_onetime.        "<<F36K920651 - 01 ins

    SELECT *
      INTO TABLE @gt_bplog
      FROM zsdsfit029
      FOR ALL ENTRIES IN @gt_output
*      WHERE data_type = @gc_data_type-invoice    "<<F36K910583 del
      WHERE data_type = @gt_output-data_type      "<<F36K910583 ins
      AND   bukrs = @gt_output-bukrs
      AND   belnr = @gt_output-belnr
      AND   gjahr = @gt_output-gjahr
      AND   buzei = @gt_output-buzei
      AND   delete_flag = ''.                      "#EC CI_NO_TRANSFORM
    IF gt_bplog IS NOT INITIAL.
      "Clearing link list
      SELECT lnk~*,
*<<F36K917931 - 03 Begin of ins
           doc~xreversed,
           doc~stblg,
           doc~stjah
*<<F36K917931 - 03 End of ins
*        INTO TABLE @gt_clr_link "<<F36K920651 - 01 del
        APPENDING TABLE @gt_clr_link "<<F36K920651 - 01 ins
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
        AND   lnk~seq   = @gt_bplog-seq.           "#EC CI_NO_TRANSFORM
*      AND   doc~xreversal = ''         "<<F36K917931 - 03 del

      "Change history
      SELECT *
        INTO TABLE @gt_status_hist
        FROM zsdsfit038
        FOR ALL ENTRIES IN @gt_bplog
*        WHERE data_type = @gc_data_type-invoice   "<<F36K910583 del
        WHERE data_type = @gt_bplog-data_type      "<<F36K910583 ins
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

*<<F36K919968 - 01 Begin of del
**<<F36K914812 - 04 Begin of ins
*    SELECT
*      pa0105~usrid,
*      pa0002~pernr AS pernr,
*      concat_with_space( pa0002~vorna, pa0002~nachn, 1 ) AS full_name
*      FROM pa0105 AS pa0105
*      INNER JOIN pa0002 AS pa0002
*      ON pa0002~pernr = pa0105~pernr
*      INNER JOIN @gt_output AS outp
*      ON pa0105~usrid = outp~ernam
*      WHERE pa0105~usrty = '0001'
*      INTO TABLE @gt_user_data.
**<<F36K914812 - 04 End of ins
*
**<<F36K917931 - 09 Begin of ins
*    SELECT
*      ' ' AS usrid,
*      pa0002~pernr AS pernr,
*      concat_with_space( pa0002~vorna, pa0002~nachn, 1 ) AS full_name
*      FROM pa0002 AS pa0002
*      INNER JOIN @gt_output AS outp
*      ON pa0002~pernr = outp~pernr
*      APPENDING TABLE @gt_user_data.
*
*    DELETE ADJACENT DUPLICATES FROM gt_user_data COMPARING crusr pernr.
**<<F36K917931 - 09 Begin of ins
*<<F36K919968 - 01 End of del

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
*      INTO TABLE @gt_do.         "<<F36K920651 - 01 del
      APPENDING TABLE @gt_do.     "<<F36K920651 - 01 ins

    SORT gt_do BY vbeln fkdat aubel vgbel lfdat adrnr.
    DELETE ADJACENT DUPLICATES FROM gt_do COMPARING ALL FIELDS.

    IF gt_do IS NOT INITIAL.

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

*"<<F36K919968 - 01 Begin of del
**<<F36K917931 - 07 Begin of ins
*    SELECT
*      inv_chk~vbeln,
*      inv_chk~zrcvd_pernr,
*      inv_chk~zrcvd_d,
*      cfg~bl_actty AS action_type,
*      cfg~bl_stat AS status
*      FROM zsdsfit027 AS inv_chk
*      INNER JOIN @gt_output AS opt
*      ON inv_chk~vbeln = opt~vbeln_vf
*      INNER JOIN zsdsfic028 AS  cfg
*      ON inv_chk~zstat_act = cfg~stat_act
*      WHERE cfg~bl_actty        IN @s_hactty
*      AND   cfg~bl_stat         IN @s_hstat
*      AND   inv_chk~zrcvd_pernr IN @s_hpernr
*      AND   inv_chk~zrcvd_d     IN @s_hwrkdt
*      INTO TABLE @gt_inv_chk_status.
*
*    SELECT
*      inv_chk~vbeln,
*      cfg~bl_actty AS action_type,
*      cfg~bl_stat AS status
*      FROM zsdsfit028 AS inv_chk
*      INNER JOIN @gt_output AS opt
*      ON inv_chk~vbeln = opt~vbeln_vf
*      INNER JOIN zsdsfic028 AS  cfg
*      ON inv_chk~zstat_act = cfg~stat_act
*      WHERE cfg~bl_actty IN @s_hactty
*      AND   cfg~bl_stat  IN @s_hstat
*      APPENDING CORRESPONDING FIELDS OF TABLE @gt_inv_chk_status ##TOO_MANY_ITAB_FIELDS.
**<<F36K917931 - 07 End of ins

**<<F36K919549 - 01 Begin of ins
*    LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>)
*      WHERE action_type IS INITIAL AND
*            status      IS INITIAL.
*      DATA(lv_tabix) = sy-tabix.
*      READ TABLE gt_inv_chk_status INTO DATA(ls_inv_chk_status)
*        WITH KEY vbeln = <ls_output>-vbeln_vf.
*      IF sy-subrc = 0 AND
**<<F36K919968 - 01 Begin of ins
*       ( ls_inv_chk_status-action_type IS NOT INITIAL OR
*         ls_inv_chk_status-status IS NOT INITIAL ).
*        <ls_output>-pernr       = ls_inv_chk_status-zrcvd_pernr.
*        <ls_output>-work_date   = ls_inv_chk_status-zrcvd_d.
**<<F36K919968 - 01 End of ins
*        <ls_output>-action_type = ls_inv_chk_status-action_type.
*        <ls_output>-status      = ls_inv_chk_status-status.
*      ELSE.
*        DELETE gt_output INDEX lv_tabix.
*        CONTINUE.
*      ENDIF.
*    ENDLOOP.
**<<F36K919549 - 01 End of ins
*"<<F36K919968 - 01 End of del

*<<F36K919968 - 01 Begin of ins
    SELECT
      pa0105~usrid,
      pa0002~pernr AS pernr,
      concat_with_space( pa0002~vorna, pa0002~nachn, 1 ) AS full_name
      FROM pa0105 AS pa0105
      INNER JOIN pa0002 AS pa0002
      ON pa0002~pernr = pa0105~pernr
      INNER JOIN @gt_output AS outp
      ON pa0105~usrid = outp~ernam
      WHERE pa0105~usrty = '0001'
      INTO TABLE @gt_user_data.

    SELECT
      ' ' AS usrid,
      pa0002~pernr AS pernr,
      concat_with_space( pa0002~vorna, pa0002~nachn, 1 ) AS full_name
      FROM pa0002 AS pa0002
      INNER JOIN @gt_output AS outp
      ON pa0002~pernr = outp~pernr
      APPENDING TABLE @gt_user_data.

    DELETE ADJACENT DUPLICATES FROM gt_user_data COMPARING crusr pernr.
*<<F36K919968 - 01 End of ins

  ENDIF.

*<<F36K919968 - 01 Begin of ins
  DELETE gt_output
    WHERE
        pernr NOT IN s_hpernr OR
        work_date NOT IN s_hwrkdt.
*<<F36K919968 - 01 End of ins

ENDFORM.
