*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0520_F01
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0520_F01
*  Creation Date      : 11.11.2024
*  Author             : Apichat Ch.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------


*&---------------------------------------------------------------------*
*& Form get_constant
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_get_constant .

  DATA:
    lr_param         TYPE zcl_sdsca_utilities=>trt_range_param.

  lr_param = VALUE #( ( sign = 'I' option = 'EQ' low = gc_genc_param-gl_acct ) ).


  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = 'ZSDSFIR0430'          " ABAP Program Name
*      irt_param = lr_param          " Parameter name
*      irt_ext   =                  " Parameter extension
    IMPORTING
      et_gen_c  = DATA(lt_genc)     " General Constants (GENC)
  ).

  READ TABLE lt_genc INTO DATA(ls_genc_430) WITH KEY param = 'FILE_PATH'.
  IF sy-subrc EQ 0.
    gf_path = ls_genc_430-value_low.
  ENDIF.

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                     " ABAP Program Name
      if_param = gc_genc_param-doc_type    " Parameter name
*      if_ext   =                             " Parameter Extension
    IMPORTING
      et_range = gr_blart
  ).

  zcl_sdsca_utilities=>get_gen_c_range(
    EXPORTING
      if_repid = sy-repid                     " ABAP Program Name
      if_param = gc_genc_param-sp_gl          " Parameter name
*      if_ext   =                             " Parameter Extension
    IMPORTING
      et_range = gr_umskz
  ).

  zcl_sdsca_utilities=>get_gen_c(
    EXPORTING
      if_repid  = 'ZSDSFIR0440'     " ABAP Program Name
      irt_param = lr_param          " Parameter name
*      irt_ext   =                  " Parameter extension
    IMPORTING
      et_gen_c  = gt_genc_gl        " General Constants (GENC)
  ).

  gr_gl = VALUE #( FOR ls_gl IN gt_genc_gl
      LET s = 'I' o = 'EQ'
      IN sign     = s
         option   = o
    ( low = ls_gl-value_low )
     ).


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

*<<F36K914879 - 02 Begin of ins
  CASE gc_true.
    WHEN rb_open.
      PERFORM f_get_open_items.
    WHEN rb_all.
      PERFORM f_get_all_items.
  ENDCASE.
*<<F36K914879 - 02 End of ins

  IF gt_document IS NOT INITIAL.

*   <<F36K910492 start ins
    SELECT a~objectclas,
           a~objectid,
           a~changenr,
           a~username,
           a~udate,
           a~utime,
           a~tcode
      FROM cdhdr AS a
      INNER JOIN @gt_document AS doc
      ON a~objectid = concat( concat( @sy-mandt,doc~bukrs ),concat( doc~belnr,doc~gjahr ) )
      WHERE a~objectclas = 'BELEG'
      INTO TABLE @gt_cdhdr.

    IF sy-subrc = 0.
      SORT gt_cdhdr BY objectid udate DESCENDING utime DESCENDING changenr DESCENDING.
      DELETE ADJACENT DUPLICATES FROM gt_cdhdr COMPARING objectid.
    ENDIF.
*   <<F36K910492 end ins

    "GL line item
    SELECT
      itm~bukrs,
      itm~belnr,
      itm~gjahr,
      itm~buzei,
      itm~hkont,
      itm~dmbtr
      FROM bseg AS itm
      INNER JOIN @gt_document AS doc
      ON  itm~bukrs = doc~bukrs
      AND itm~belnr = doc~belnr
      AND itm~gjahr = doc~gjahr
      WHERE itm~hkont IN @gr_gl
      INTO TABLE @gt_doc_item.                          "#EC CI_NOORDER

    "Advance posting
    SELECT adv~*
      FROM zsdsfit045 AS adv
      INNER JOIN @gt_document AS doc
      ON  adv~bukrs = doc~bukrs
      AND adv~belnr = doc~belnr
      AND adv~gjahr = doc~gjahr
      WHERE delete_flag = ''
      AND   adv~block_status IN @s_block
      INTO TABLE @gt_advance.

    "Sale order & Billing
    SELECT ord~*
      FROM zsdssdt015 AS ord
      INNER JOIN @gt_document AS doc
      ON  ord~bukrs = doc~bukrs
      AND ord~belnr = doc~belnr
      AND ord~gjahr = doc~gjahr
      WHERE ord~belnr_c = ''
      INTO TABLE @gt_order.

*<<F36K914879 - 04 Begin of ins
    SORT gt_order BY vbeln posnr bukrs belnr gjahr belnr_s gjahr_s.
    DELETE ADJACENT DUPLICATES FROM gt_order COMPARING vbeln posnr bukrs belnr gjahr belnr_s gjahr_s.
*<<F36K914879 - 04 End of ins
    IF gt_order IS NOT INITIAL.
      "Billing data
      SELECT
        bil~vbeln,
        bil~fkdat
        FROM vbrk AS bil
        INNER JOIN @gt_order AS ord
        ON bil~vbeln = ord~vbeln_b
        INTO TABLE @gt_billing_data.

      SORT gt_billing_data BY vbeln.

      "DO <-> Billing
      SELECT
        bil~vbeln,
        bil~vgbel
        FROM vbrp AS bil
        INNER JOIN @gt_order AS ord
        ON bil~vbeln = ord~vbeln_b
        WHERE bil~vgtyp = 'J'
        INTO TABLE @gt_do.

      SORT gt_do BY vbeln.

      IF gt_do IS NOT INITIAL.
        SELECT
          do~vbeln,
          do~lfdat
          FROM likp AS do
          INNER JOIN @gt_do AS lnk
          ON do~vbeln = lnk~vgbel
          INTO TABLE @gt_do_data.

        SORT gt_do_data BY vbeln.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_perpare_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_perpare_output .

  DATA: ls_output_bak TYPE zsdsfis179,
        lv_seq        TYPE n LENGTH 4. "<<F36K914879 - 03
*        lv_sign       TYPE i.          "<<F36K914879 - 03

  SORT gt_order BY bukrs belnr gjahr vbeln vbeln_b.

  LOOP AT gt_document INTO DATA(ls_document)  ##INTO_OK.
    APPEND INITIAL LINE TO gt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
    DATA(lv_idx) = sy-tabix.
    <ls_output> = CORRESPONDING #( ls_document ).

    LOOP AT gt_doc_item INTO DATA(ls_doc_item)
      WHERE bukrs = ls_document-bukrs
      AND   belnr = ls_document-belnr
      AND   gjahr = ls_document-gjahr.

      READ TABLE gt_genc_gl
        INTO DATA(ls_gl)
        WITH KEY value_low = ls_doc_item-hkont.
      CASE ls_gl-param_ext.
        WHEN 'BANK_FEE'.
          <ls_output>-bank_fee += ls_doc_item-dmbtr.
        WHEN 'EXPENSE'.
          <ls_output>-exp_amt  += ls_doc_item-dmbtr.
        WHEN 'INCOME'.
          <ls_output>-inc_amt  += ls_doc_item-dmbtr.
      ENDCASE.

      CLEAR: ls_gl.
    ENDLOOP.

    READ TABLE gt_advance
      INTO DATA(ls_advance)
      WITH KEY bukrs = ls_document-bukrs
               belnr = ls_document-belnr
               gjahr = ls_document-gjahr.
    IF sy-subrc EQ 0.
      <ls_output>-uuid              = ls_advance-uuid.
      <ls_output>-attach_file_flag  = ls_advance-attach_file_flag.
      <ls_output>-attach_ext        = ls_advance-attach_ext.
      <ls_output>-tranf_no          = ls_advance-tranf_no.
      <ls_output>-pymt_method       = ls_advance-pymt_method.
      <ls_output>-pymt_desc         = zcl_sdsfi_coll_log=>get_pymt_method_text( ls_advance-pymt_method ).
      <ls_output>-bank_date         = ls_advance-bank_date.
      <ls_output>-cheque_no         = ls_advance-cheque_no.
      <ls_output>-hbkid             = ls_advance-hbkid.
      <ls_output>-hktid             = ls_advance-hktid.
*      <ls_output>-update_by         = ls_advance-update_by.
*      <ls_output>-update_on         = ls_advance-update_on.
*      <ls_output>-update_time       = ls_advance-update_time.
      <ls_output>-update_by         = ls_advance-ernam.
      <ls_output>-update_on         = ls_advance-erdat.
      <ls_output>-update_time       = ls_advance-erzmt.
      <ls_output>-block_status      = ls_advance-block_status.
    ENDIF.

    IF ls_advance-block_status NOT IN s_block.
      DELETE gt_output INDEX lv_idx.
      CONTINUE.
    ENDIF.
*   <<F36K910492 start ins
    READ TABLE gt_cdhdr ASSIGNING FIELD-SYMBOL(<ls_cdhdr>)
                        WITH KEY objectid = |{ sy-mandt }{ <ls_output>-bukrs }{ <ls_output>-belnr }{ <ls_output>-gjahr }| ##WARN_OK.
    IF sy-subrc = 0.
      <ls_output>-username = <ls_cdhdr>-username.
      <ls_output>-udate    = <ls_cdhdr>-udate.
      <ls_output>-utime    = <ls_cdhdr>-utime.
    ENDIF.
*   <<F36K910492 end ins
*   <<F36K910492 start del
*    IF ls_advance-ernam IS NOT INITIAL.
*      <ls_output>-usnam = ls_advance-ernam.
*    ENDIF.
*
*    IF <ls_output>-update_on IS INITIAL.
*      <ls_output>-update_on = ls_document-aedat.
*    ENDIF.
*   <<F36K910492 end del

    <ls_output>-umskz_seq = COND #( WHEN <ls_output>-umskz = 'N' THEN '1'
                                    WHEN <ls_output>-umskz = 'E' THEN '2'
                                    WHEN <ls_output>-umskz = 'S' THEN '3'
                                    WHEN <ls_output>-umskz = 'A' THEN '4' ).

    <ls_output>-sort_key = |{ <ls_output>-kunnr }{ <ls_output>-umskz_seq }{ <ls_output>-budat }|.

    "<<F36K914879 - 04 Begin of del
*
*    CLEAR: ls_output_bak.
*    LOOP AT gt_order INTO DATA(ls_order)  ##INTO_OK
*      WHERE bukrs = ls_document-bukrs
*      AND   belnr = ls_document-belnr
*      AND   gjahr = ls_document-gjahr.
*
*      IF ls_output_bak IS NOT INITIAL AND
*       ( ls_output_bak-vbeln    <> ls_order-vbeln OR
*         ls_output_bak-vbeln_b  <> ls_order-vbeln_b ).
*        APPEND INITIAL LINE TO gt_output ASSIGNING <ls_output>.
*        <ls_output> = CORRESPONDING #( ls_output_bak EXCEPT vbeln vgbel lfdat vbeln_b fkdat dmbtr rmamt ).
*        <ls_output>-wrbtr = ls_output_bak-rmamt.
*      ENDIF.
*
*      READ TABLE gt_do
*        INTO DATA(ls_do)
*        WITH KEY vbeln = ls_order-vbeln_b
*        BINARY SEARCH.
*      IF sy-subrc = 0.
*        <ls_output>-vgbel   = ls_do-vgbel.
*
*        READ TABLE gt_do_data
*          INTO DATA(ls_do_data)
*          WITH KEY vbeln = ls_do-vgbel
*          BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          <ls_output>-lfdat = ls_do_data-lfdat.
*        ENDIF.
*      ENDIF.
*
*      READ TABLE gt_billing_data
*        INTO DATA(ls_billing_data)
*        WITH KEY vbeln = ls_order-vbeln_b
*        BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        <ls_output>-fkdat = ls_billing_data-fkdat.
*      ENDIF.
*
*      <ls_output>-vbeln   = ls_order-vbeln.
*      <ls_output>-vbeln_b = ls_order-vbeln_b.
*      <ls_output>-dmbtr   += ls_order-dmbtr.
**      <ls_output>-rmamt   = <ls_output>-wrbtr - <ls_output>-dmbtr. "*<<F36K914879 - 03 del
*
**<<F36K914879 - 03 Begin of ins
*      IF <ls_output>-wrbtr >= 0.
*        <ls_output>-rmamt   = <ls_output>-wrbtr - <ls_output>-dmbtr.
*      ELSE.
*        <ls_output>-rmamt   = ( abs( <ls_output>-wrbtr ) - <ls_output>-dmbtr ) * -1.
*      ENDIF.
*
*
*      IF ls_output_bak IS NOT INITIAL AND
*       ( ls_output_bak-vbeln    <> ls_order-vbeln OR
*         ls_output_bak-vbeln_b  <> ls_order-vbeln_b ).
*        CLEAR <ls_output>-wrbtr.
*
*        lv_seq += 1.
*        <ls_output>-sort_key = |{ <ls_output>-sort_key }{ lv_seq }|.
*      ELSE.
*        lv_seq = 1.
*      ENDIF.
**<<F36K914879 - 03 End of ins
*
*      ls_output_bak = <ls_output>.
*
*      CLEAR: ls_do,
*             ls_do_data,
*             ls_billing_data.
*
*
*    ENDLOOP.
*    IF sy-subrc <> 0.
*      <ls_output>-rmamt   = <ls_output>-wrbtr.
*    ENDIF.

*  ENDLOOP.

*  SORT gt_output BY sort_key.

*<<F36K914879 - 04 End of del

*<<F36K914879 - 04 Begin of ins
  ENDLOOP.
  SORT gt_output BY sort_key.

  DATA(lt_output) = gt_output.

  LOOP AT lt_output INTO DATA(ls_output)  ##INTO_OK.
    READ TABLE gt_output ASSIGNING <ls_output>
      WITH KEY
        bukrs = ls_output-bukrs
        belnr = ls_output-belnr
        gjahr = ls_output-gjahr
        buzei = ls_output-buzei.

    CLEAR: ls_output_bak.
    LOOP AT gt_order INTO DATA(ls_order)  ##INTO_OK
      WHERE bukrs = ls_output-bukrs
      AND   belnr = ls_output-belnr
      AND   gjahr = ls_output-gjahr.

      DATA(lv_tabix) = sy-tabix.

      IF ls_output_bak IS NOT INITIAL AND
       ( ls_output_bak-vbeln    <> ls_order-vbeln OR
         ls_output_bak-vbeln_b  <> ls_order-vbeln_b ).
        APPEND INITIAL LINE TO gt_output ASSIGNING <ls_output>.
        <ls_output> = CORRESPONDING #( ls_output_bak EXCEPT vbeln vgbel lfdat vbeln_b fkdat dmbtr rmamt ).
        <ls_output>-wrbtr = ls_output_bak-rmamt.
      ENDIF.

      READ TABLE gt_do
        INTO DATA(ls_do)
        WITH KEY vbeln = ls_order-vbeln_b
        BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_output>-vgbel   = ls_do-vgbel.

        READ TABLE gt_do_data
          INTO DATA(ls_do_data)
          WITH KEY vbeln = ls_do-vgbel
          BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_output>-lfdat = ls_do_data-lfdat.
        ENDIF.
      ENDIF.

      READ TABLE gt_billing_data
        INTO DATA(ls_billing_data)
        WITH KEY vbeln = ls_order-vbeln_b
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_output>-fkdat = ls_billing_data-fkdat.
      ENDIF.

      <ls_output>-vbeln   = ls_order-vbeln.
      <ls_output>-vbeln_b = ls_order-vbeln_b.
      <ls_output>-dmbtr   += ls_order-dmbtr.

      IF <ls_output>-wrbtr >= 0.
        <ls_output>-rmamt   = <ls_output>-wrbtr - <ls_output>-dmbtr.
      ELSE.
        <ls_output>-rmamt   = ( abs( <ls_output>-wrbtr ) - <ls_output>-dmbtr ) * -1.
      ENDIF.


      IF ls_output_bak IS NOT INITIAL AND
       ( ls_output_bak-vbeln    <> ls_order-vbeln OR
         ls_output_bak-vbeln_b  <> ls_order-vbeln_b ).
        CLEAR <ls_output>-wrbtr.

        lv_seq += 1.
        <ls_output>-sort_key = |{ <ls_output>-sort_key }{ lv_seq }|.
      ELSE.
        lv_seq = 1.
      ENDIF.

      ls_output_bak = <ls_output>.

      CLEAR ls_order-dmbtr.
      MODIFY gt_order FROM ls_order INDEX lv_tabix TRANSPORTING dmbtr.

      CLEAR: ls_do,
             ls_do_data,
             ls_billing_data.

    ENDLOOP.
    IF sy-subrc <> 0.
      <ls_output>-rmamt   = <ls_output>-wrbtr.
    ENDIF.

  ENDLOOP.

  SORT gt_output BY sort_key.
  "<<F36K914879 - 04 End of ins


ENDFORM.
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
      WHEN 'GJAHR'.
        <l_fieldcat>-tech       = gc_true.
      WHEN 'UUID'.
        <l_fieldcat>-tech       = gc_true.
      WHEN 'BUKRS'.
        <l_fieldcat>-tech       = gc_true.
      WHEN 'BANKL'.
        <l_fieldcat>-tech       = gc_true.
      WHEN 'TEXT1'.
        <l_fieldcat>-tech       = gc_true.
      WHEN 'BELNR'.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'AUGBL'.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'AUGGJ'.
        <l_fieldcat>-no_out     = gc_true.
      WHEN 'PYMT_DESC'.
        <l_fieldcat>-reptext    = TEXT-f06.
        <l_fieldcat>-seltext    = TEXT-f06.
        <l_fieldcat>-scrtext_s  = TEXT-f06.
        <l_fieldcat>-scrtext_m  = TEXT-f06.
        <l_fieldcat>-scrtext_l  = TEXT-f06.
      WHEN 'VBELN'.
        <l_fieldcat>-reptext    = TEXT-f01.
        <l_fieldcat>-seltext    = TEXT-f01.
        <l_fieldcat>-scrtext_s  = TEXT-f01.
        <l_fieldcat>-scrtext_m  = TEXT-f01.
        <l_fieldcat>-scrtext_l  = TEXT-f01.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'VGBEL'.
        <l_fieldcat>-reptext    = TEXT-f02.
        <l_fieldcat>-seltext    = TEXT-f02.
        <l_fieldcat>-scrtext_s  = TEXT-f02.
        <l_fieldcat>-scrtext_m  = TEXT-f02.
        <l_fieldcat>-scrtext_l  = TEXT-f02.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'VBELN_B'.
        <l_fieldcat>-reptext    = TEXT-f03.
        <l_fieldcat>-seltext    = TEXT-f03.
        <l_fieldcat>-scrtext_s  = TEXT-f03.
        <l_fieldcat>-scrtext_m  = TEXT-f03.
        <l_fieldcat>-scrtext_l  = TEXT-f03.
        <l_fieldcat>-hotspot    = gc_true.
      WHEN 'ATTACH_FILE_FLAG'.
        <l_fieldcat>-style = cl_gui_alv_grid=>mc_style_button.
      WHEN 'INC_AMT'.
        <l_fieldcat>-reptext    = TEXT-f04.
        <l_fieldcat>-seltext    = TEXT-f04.
        <l_fieldcat>-scrtext_s  = TEXT-f04.
        <l_fieldcat>-scrtext_m  = TEXT-f04.
        <l_fieldcat>-scrtext_l  = TEXT-f04.
      WHEN 'UMSKZ_SEQ'.
        <l_fieldcat>-reptext    = TEXT-f05.
        <l_fieldcat>-seltext    = TEXT-f05.
        <l_fieldcat>-scrtext_s  = TEXT-f05.
        <l_fieldcat>-scrtext_m  = TEXT-f05.
        <l_fieldcat>-scrtext_l  = TEXT-f05.
      WHEN 'SORT_KEY'.
        <l_fieldcat>-tech       = gc_true.
*     <<F36K910492 start ins
      WHEN 'USNAM'.
        <l_fieldcat>-reptext    = TEXT-f10.
        <l_fieldcat>-seltext    = TEXT-f10.
        <l_fieldcat>-scrtext_s  = TEXT-f10.
        <l_fieldcat>-scrtext_m  = TEXT-f10.
        <l_fieldcat>-scrtext_l  = TEXT-f10.
      WHEN 'CPUDT'.
        <l_fieldcat>-reptext    = TEXT-f11.
        <l_fieldcat>-seltext    = TEXT-f11.
        <l_fieldcat>-scrtext_s  = TEXT-f11.
        <l_fieldcat>-scrtext_m  = TEXT-f11.
        <l_fieldcat>-scrtext_l  = TEXT-f11.
      WHEN 'CPUTM'.
        <l_fieldcat>-reptext    = TEXT-f12.
        <l_fieldcat>-seltext    = TEXT-f12.
        <l_fieldcat>-scrtext_s  = TEXT-f12.
        <l_fieldcat>-scrtext_m  = TEXT-f12.
        <l_fieldcat>-scrtext_l  = TEXT-f12.
      WHEN 'UPDATE_BY'.
        <l_fieldcat>-reptext    = TEXT-f07.
        <l_fieldcat>-seltext    = TEXT-f07.
        <l_fieldcat>-scrtext_s  = TEXT-f07.
        <l_fieldcat>-scrtext_m  = TEXT-f07.
        <l_fieldcat>-scrtext_l  = TEXT-f07.
      WHEN 'UPDATE_ON'.
        <l_fieldcat>-reptext    = TEXT-f08.
        <l_fieldcat>-seltext    = TEXT-f08.
        <l_fieldcat>-scrtext_s  = TEXT-f08.
        <l_fieldcat>-scrtext_m  = TEXT-f08.
        <l_fieldcat>-scrtext_l  = TEXT-f08.
      WHEN 'UPDATE_TIME'.
        <l_fieldcat>-reptext    = TEXT-f09.
        <l_fieldcat>-seltext    = TEXT-f09.
        <l_fieldcat>-scrtext_s  = TEXT-f09.
        <l_fieldcat>-scrtext_m  = TEXT-f09.
        <l_fieldcat>-scrtext_l  = TEXT-f09.
      WHEN 'USERNAME'.
        <l_fieldcat>-reptext    = TEXT-f13.
        <l_fieldcat>-seltext    = TEXT-f13.
        <l_fieldcat>-scrtext_s  = TEXT-f13.
        <l_fieldcat>-scrtext_m  = TEXT-f13.
        <l_fieldcat>-scrtext_l  = TEXT-f13.
      WHEN 'UDATE'.
        <l_fieldcat>-reptext    = TEXT-f14.
        <l_fieldcat>-seltext    = TEXT-f14.
        <l_fieldcat>-scrtext_s  = TEXT-f14.
        <l_fieldcat>-scrtext_m  = TEXT-f14.
        <l_fieldcat>-scrtext_l  = TEXT-f14.
      WHEN 'UTIME'.
        <l_fieldcat>-reptext    = TEXT-f15.
        <l_fieldcat>-seltext    = TEXT-f15.
        <l_fieldcat>-scrtext_s  = TEXT-f15.
        <l_fieldcat>-scrtext_m  = TEXT-f15.
        <l_fieldcat>-scrtext_l  = TEXT-f15.
*     <<F36K910492 end ins
    ENDCASE.
  ENDLOOP.


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

  READ TABLE gt_output INTO DATA(ls_output)
    INDEX uf_row_id-index.

  CASE uf_column_id-fieldname.
    WHEN 'BELNR'.
      IF ls_output-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_output-belnr.
        SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
    WHEN 'AUGBL'.
      IF ls_output-augbl IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_output-augbl.
        SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-auggj.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
    WHEN 'VBELN'.
      IF ls_output-vbeln IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD ls_output-vbeln.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
    WHEN 'VGBEL'.
      IF ls_output-vgbel IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD ls_output-vgbel.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.  "#EC CI_CALLTA
      ENDIF.
    WHEN 'VBELN_B'.
      IF ls_output-vbeln_b IS NOT INITIAL.
        SET PARAMETER ID 'VF' FIELD ls_output-vbeln_b.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
  ENDCASE.

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

*  lv_file_name = |{ gf_path }\\{ uf_uuid }.PDF|.        "<<F36K911030 del
  lv_file_name = |{ gf_path }\\{ uf_uuid }.{ uf_ext }|.  "<<F36K911030 ins


  CALL FUNCTION 'Z_SDSFI_DISPLAY_PDF'
    EXPORTING
      iv_filename = CONV string( lv_file_name )
      iv_ext      = uf_ext.

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
  SELECT
    h~bukrs,
    h~belnr,
    h~gjahr,
    o~buzei,  "<<F36K914879 - 03 ins
    h~usnam,
    h~cpudt,
    h~cputm,
    h~bldat,
    h~budat,
    h~bktxt,
    h~xblnr,
    h~waers,
    h~aedat,
    o~zuonr,
    o~umskz,
    o~sgtxt,
    o~kunnr,
    CASE
      WHEN cust~ktokd = 'Z010' THEN
        concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
      WHEN cust~ktokd = 'Z050' THEN
        concat_with_space( partner~name_first, partner~name_last, 1 )
      ELSE
        concat_with_space( partner~name_org1, partner~name_org2, 1 )
    END AS cust_name,
*    o~dmbtr AS wrbtr, *<<F36K914879 - 03 del
*<<F36K914879 - 03 Begin of ins
    CASE
      WHEN o~shkzg = 'H' THEN
        CAST( o~dmbtr * -1 AS CURR( 23,2 ) ) ##NUMBER_OK
      ELSE
        o~dmbtr
    END AS wrbtr,
*<<F36K914879 - 03 End of ins
    o~xref1 AS tranf_no,
    o~zfbdt,
    o~augdt,
    o~augbl,
    o~auggj,
*<<F36K914879 - 01 Begin of ins
    o~rebzg,
    o~rebzj,
    o~rebzz
*<<F36K914879 - 01 End of ins
    FROM bkpf AS h
    INNER JOIN bsid_view AS o
    ON  h~bukrs = o~bukrs
    AND h~belnr = o~belnr
    AND h~gjahr = o~gjahr
    INNER JOIN kna1 AS cust
    ON o~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    WHERE h~blart IN @gr_blart
    AND   o~umskz IN @gr_umskz
*    AND   o~budat <= @p_keydt    "<<F36K914879 - 02 del
    AND ( o~budat <  @s_keydt-low "<<F36K914879 - 02 ins
     OR   o~budat IN @s_keydt )   "<<F36K914879 - 02 ins
    AND   o~zuonr in @s_zuonr     "<<F36K914879 - 05 ins
    AND   o~kunnr IN @s_kunnr
    AND   h~bldat IN @s_bldat
    AND   h~belnr IN @s_belnr
    AND   h~gjahr IN @s_gjahr
    AND   o~augbl =  ''
    AND   o~xref1 IN @s_trnfn
  UNION
  SELECT
    h~bukrs,
    h~belnr,
    h~gjahr,
    c~buzei,  "<<F36K914879 - 03 ins
    h~usnam,
    h~cpudt,
    h~cputm,
    h~bldat,
    h~budat,
    h~bktxt,
    h~xblnr,
    h~waers,
    h~aedat,
    c~zuonr,
    c~umskz,
    c~sgtxt,
    c~kunnr,
    CASE
      WHEN cust~ktokd = 'Z010' THEN
        concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
      WHEN cust~ktokd = 'Z050' THEN
        concat_with_space( partner~name_first, partner~name_last, 1 )
      ELSE
        concat_with_space( partner~name_org1, partner~name_org2, 1 )
    END AS cust_name,
*    c~dmbtr AS wrbtr, *<<F36K914879 - 03 del
*<<F36K914879 - 03 Begin of ins
    CASE
      WHEN c~shkzg = 'H' THEN
        CAST( c~dmbtr * -1 AS CURR( 23,2 ) ) ##NUMBER_OK
      ELSE
        c~dmbtr
    END AS wrbtr,
*<<F36K914879 - 03 End of ins
    c~xref1 AS tranf_no,
    c~zfbdt,
    c~augdt,
    c~augbl,
    c~auggj,
*<<F36K914879 - 01 Begin of ins
    c~rebzg,
    c~rebzj,
    c~rebzz
*<<F36K914879 - 01 End of ins
    FROM bkpf AS h
    INNER JOIN bsad_view AS c
    ON  h~bukrs = c~bukrs
    AND h~belnr = c~belnr
    AND h~gjahr = c~gjahr
    INNER JOIN kna1 AS cust
    ON c~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    WHERE h~blart IN @gr_blart
    AND   c~umskz IN @gr_umskz
*    AND   c~augdt >  @p_keydt    "<<F36K914879 - 02 del
    AND   c~augdt >  @s_keydt-low "<<F36K914879 - 02 ins
    AND   c~zuonr in @s_zuonr     "<<F36K914879 - 05 ins
    AND   c~kunnr IN @s_kunnr
    AND   h~bldat IN @s_bldat
    AND   h~belnr IN @s_belnr
    AND   h~gjahr IN @s_gjahr
    AND   c~xref1 IN @s_trnfn
    INTO CORRESPONDING FIELDS OF TABLE @gt_document.

ENDFORM.

*<<F36K914879 - 03 Begin of ins

*&---------------------------------------------------------------------*
*& Form f_get_all_items
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_all_items .

  SELECT
    h~bukrs,
    h~belnr,
    h~gjahr,
    o~buzei,  "<<F36K914879 - 03 ins
    h~usnam,
    h~cpudt,
    h~cputm,
    h~bldat,
    h~budat,
    h~bktxt,
    h~xblnr,
    h~waers,
    h~aedat,
    o~zuonr,
    o~umskz,
    o~sgtxt,
    o~kunnr,
    CASE
      WHEN cust~ktokd = 'Z010' THEN
        concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
      WHEN cust~ktokd = 'Z050' THEN
        concat_with_space( partner~name_first, partner~name_last, 1 )
      ELSE
        concat_with_space( partner~name_org1, partner~name_org2, 1 )
    END AS cust_name,
*    o~dmbtr AS wrbtr, *<<F36K914879 - 03 del
*<<F36K914879 - 03 Begin of ins
    CASE
      WHEN o~shkzg = 'H' THEN
        CAST( o~dmbtr * -1 AS CURR( 23,2 ) ) ##NUMBER_OK
      ELSE
        o~dmbtr
    END AS wrbtr,
*<<F36K914879 - 03 End of ins
    o~xref1 AS tranf_no,
    o~zfbdt,
    o~augdt,
    o~augbl,
    o~auggj,
    o~rebzg,
    o~rebzj,
    o~rebzz
    FROM bkpf AS h
    INNER JOIN bsid_view AS o
    ON  h~bukrs = o~bukrs
    AND h~belnr = o~belnr
    AND h~gjahr = o~gjahr
    INNER JOIN kna1 AS cust
    ON o~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    WHERE h~blart IN @gr_blart
    AND   o~umskz IN @gr_umskz
    AND   o~budat IN @s_budat
    AND   o~kunnr IN @s_kunnr
    AND   o~zuonr in @s_zuonr     "<<F36K914879 - 05 ins
    AND   h~bldat IN @s_bldat
    AND   h~belnr IN @s_belnr
    AND   h~gjahr IN @s_gjahr
    AND   o~augbl =  ''
    AND   o~xref1 IN @s_trnfn
  UNION
  SELECT
    h~bukrs,
    h~belnr,
    h~gjahr,
    c~buzei,  "<<F36K914879 - 03 ins
    h~usnam,
    h~cpudt,
    h~cputm,
    h~bldat,
    h~budat,
    h~bktxt,
    h~xblnr,
    h~waers,
    h~aedat,
    c~zuonr,
    c~umskz,
    c~sgtxt,
    c~kunnr,
    CASE
      WHEN cust~ktokd = 'Z010' THEN
        concat_with_space( concat_with_space( partner~name_first, partner~name_last, 1 ), concat_with_space( partner~name_org1, partner~name_org2, 1 ), 1 )
      WHEN cust~ktokd = 'Z050' THEN
        concat_with_space( partner~name_first, partner~name_last, 1 )
      ELSE
        concat_with_space( partner~name_org1, partner~name_org2, 1 )
    END AS cust_name,
*    c~dmbtr AS wrbtr, *<<F36K914879 - 03 del
*<<F36K914879 - 03 Begin of ins
    CASE
      WHEN c~shkzg = 'H' THEN
        CAST( c~dmbtr * -1 AS CURR( 23,2 ) ) ##NUMBER_OK
      ELSE
        c~dmbtr
    END AS wrbtr,
*<<F36K914879 - 03 End of ins
    c~xref1 AS tranf_no,
    c~zfbdt,
    c~augdt,
    c~augbl,
    c~auggj,
    c~rebzg,
    c~rebzj,
    c~rebzz
    FROM bkpf AS h
    INNER JOIN bsad_view AS c
    ON  h~bukrs = c~bukrs
    AND h~belnr = c~belnr
    AND h~gjahr = c~gjahr
    INNER JOIN kna1 AS cust
    ON c~kunnr = cust~kunnr
    INNER JOIN cvi_cust_link AS link
    ON cust~kunnr = link~customer
    INNER JOIN but000 AS partner
    ON partner~partner_guid = link~partner_guid
    WHERE h~blart IN @gr_blart
    AND   c~umskz IN @gr_umskz
    AND   c~budat IN @s_budat
    AND   c~kunnr IN @s_kunnr
    AND   c~zuonr in @s_zuonr     "<<F36K914879 - 05 ins
    AND   h~bldat IN @s_bldat
    AND   h~belnr IN @s_belnr
    AND   h~gjahr IN @s_gjahr
    AND   c~xref1 IN @s_trnfn
    INTO CORRESPONDING FIELDS OF TABLE @gt_document.

  SORT gt_document BY bukrs belnr gjahr buzei. "<<F36K914879 - 03 ins

ENDFORM.
