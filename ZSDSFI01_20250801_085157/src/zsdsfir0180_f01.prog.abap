*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .

  PERFORM: get_param,
           default_scr.

ENDFORM.                    "initialization
*&---------------------------------------------------------------------*
*& Form START
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM start .

  PERFORM: validate_data,
           init_data,
           get_range,
           get_param,
           get_mapping,
           process_main.
  "assign_head_val.

ENDFORM.                    "start
*&---------------------------------------------------------------------*
*& Form END
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM end .

*  IF gv_stop IS INITIAL.
*    IF p_memid IS INITIAL.
  IF sy-batch IS INITIAL.
    PERFORM display_data USING ''.
  ELSE.
    PERFORM process_called.
  ENDIF.
*    ELSE.
**     For online mode that submitted from any printing programs.
*      PERFORM process_called.
*      PERFORM fill_success.
*      PERFORM export_data.
*    ENDIF.
*  ENDIF.

ENDFORM.                    "end
*&---------------------------------------------------------------------*
*& Form DEFAULT_SCR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM default_scr .

  DATA: lv_name      TYPE ZSDSFIC015-name VALUE 'CREATE_BACKGROUD_LOGS',
        lv_param_ext TYPE ZSDSFIC015-param_ext,
        lv_sequence  TYPE ZSDSFIC015-sequence VALUE '1',
        lv_value     TYPE ZSDSFIC015-low_value.

  FIELD-SYMBOLS: <lfs_bldat> LIKE LINE OF s_bldat.

  PERFORM read_param    USING lv_name
                              lv_param_ext
                              lv_sequence
                     CHANGING lv_value.

  p_logd = lv_value.

  p_bukrs  = '1000'.
*  p_gjahr  = sy-datum+0(4).

  CALL FUNCTION 'GET_CURRENT_YEAR'
   EXPORTING
     bukrs         = p_bukrs
     date          = sy-datum
   IMPORTING
*     CURRM         =
     curry         = p_gjahr
*     PREVM         =
*     PREVY         =
            .
  IF s_bldat[] IS INITIAL.
    APPEND INITIAL LINE TO s_bldat ASSIGNING <lfs_bldat>.
    <lfs_bldat>     = 'IBT'.
    <lfs_bldat>-low = sy-datum.
    <lfs_bldat>-low+6(2) = '01'.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = sy-datum
      IMPORTING
        last_day_of_month = <lfs_bldat>-high
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.                    "default_scr
*&---------------------------------------------------------------------*
*& Form INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_data .

*>>> BOI CH01 >>>
  IF p_gjahr IS INITIAL.
    CALL FUNCTION 'GET_CURRENT_YEAR'
       EXPORTING
         bukrs         = p_bukrs
         date          = sy-datum
       IMPORTING
*         CURRM         =
         curry         = p_gjahr
*         PREVM         =
*         PREVY         =
    .
  ENDIF.
*<<< EOI CH01 <<<

  REFRESH: gt_head[],gt_charge[].

  REFRESH: gt_sd[],gt_fi[],gt_mail[],
           gt_doc_header[],gt_doc_item[],
           gt_doc_partner[],gt_doc_h_vat[],
           gt_doc_h_ref[],gt_doc_dischg[].

  REFRESH: gt_map_doc_resn[], gt_map_curr[],
           gt_map_vat_type[], gt_map_doc_sd[],
           gt_map_doc_fi[], gt_map_sch_ctry[],
           gt_map_dischg[],
           gt_company_addr[].

  REFRESH: gt_rd_doc_type[],gt_rd_doc_resn[],
           gt_rd_partner[], gt_rd_sub_dist[],
           gt_rd_district[],gt_rd_dischg[],
           gt_rd_province[].

*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
*  REFRESH: gt_collector[],gt_one_sd[].
  REFRESH: gt_one_sd[].
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<

  REFRESH: gt_tvfkt[],gt_t003t[],gt_ref_doc[],
           gt_rd_sent[],gt_param[].
  "gt_text_table[]. "Only FUNCTION 'READ_TEXT_TABLE' does exist on client system

  REFRESH: gr_status[], gr_rd_dctypgrp[],
           gr_kunnr[],gr_sap_doc_no[],
           gr_fkart[],gr_blart[].

  CLEAR: gv_count, gv_count_success, gv_count_error,
         gv_sap_doc_no, gv_rd_doc_type,gv_kunnr,
         gv_dis_detail,gv_read_text_tab.
ENDFORM.                    "init_data
*&---------------------------------------------------------------------*
*& Form VALIDATE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_data .

  PERFORM message_for_batch USING text-c01. "'Validate data.'.

*>>> BOI CH01 >>>
  "Validate Fiscal Year
  IF p_gjahr  IS INITIAL AND sy-batch IS INITIAL.
    m_message: 'S' '055' '00' '' 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
*<<< EOI CH01 <<<

  "Validate Module selection
  IF cb_sd IS INITIAL AND
     cb_fi IS INITIAL.
    m_message: 'S' '000' '38' text-e38 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF cb_inv IS INITIAL AND  "INV-ใบแจ้งหนี้
     cb_rct IS INITIAL AND  "RCT-ใบเสร็จรับเงิน
     cb_tiv IS INITIAL AND  "TIV-ใบกำกับภาษี
     cb_dcn IS INITIAL.     "DCN-ใบเพิ่มหนี้ /ลดหนี้
    m_message: 'S' '000' '38' text-e00 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "Validate Document status selection
  IF p_new  IS INITIAL AND
     p_tf   IS INITIAL.
    m_message: 'S' '000' '38' text-e39 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    "validate_data
*&---------------------------------------------------------------------*
*&      Form  throw_message
*&---------------------------------------------------------------------*
FORM throw_message USING p_msgty TYPE sy-msgty
                         p_msgno TYPE sy-msgno
                         p_msgid TYPE sy-msgid
                         p_msgtx
                         p_displ TYPE c.

*  DATA: lwa_return LIKE LINE OF gt_return,
*        lv_message TYPE string.

*  IF p_memid IS INITIAL.

  MESSAGE ID p_msgid TYPE p_msgty NUMBER p_msgno
        WITH p_msgtx DISPLAY LIKE p_displ.

*  ELSE.
*
*    MESSAGE ID p_msgid TYPE p_msgty NUMBER p_msgno
*          WITH p_msgtx INTO lv_message.
*
*    CLEAR lwa_return.
*    lwa_return-type    = p_msgty.
*    lwa_return-id      = p_msgid.
*    lwa_return-number  = p_msgno.
*    lwa_return-message = lv_message.
*    APPEND lwa_return TO gt_return.
*
*    IF p_msgty EQ 'E' OR
*       p_displ EQ 'E'.
*
*      PERFORM export_data.
*      STOP.
*
*    ENDIF.
*
*  ENDIF.

ENDFORM.                    "throw_message
*&---------------------------------------------------------------------*
*& Form GET_RANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_range .

  FIELD-SYMBOLS: <lfs_status>     LIKE LINE OF gr_status,
                 <lfs_rd_dctypgr> LIKE LINE OF gr_rd_dctypgrp.

*-- Status
*'' = No data in table header
*1 = Printed Complete
*2 = Transferred Complete
*3 = Rejected Complete(Tobe resent)
*4 = Transfer Incomplete (Tobe resent)
*5 = Reprint Aft Submit
*6 = Reject Incomplete

*>>> BOI CH05 >>>
*  APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
*  <lfs_status>     = 'IEQ'.
*  <lfs_status>-low = '9'.
*<<< EOI CH05 <<<

  IF p_new IS NOT INITIAL.
    APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
    <lfs_status>     = 'IEQ'.
    <lfs_status>-low = ' '.
    APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
    <lfs_status>     = 'IEQ'.
    <lfs_status>-low = '1'.
    APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
    <lfs_status>     = 'IEQ'.
    <lfs_status>-low = '3'.
    APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
    <lfs_status>     = 'IEQ'.
    <lfs_status>-low = '4'.
  ENDIF.

  IF p_tf IS NOT INITIAL.
    APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
    <lfs_status>     = 'IEQ'.
    <lfs_status>-low = '2'.
    APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
    <lfs_status>     = 'IEQ'.
    <lfs_status>-low = '5'.
    APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
    <lfs_status>     = 'IEQ'.
    <lfs_status>-low = '6'.
  ENDIF.
  ">>>>>> BEGIN OF INSERTION: <CH12> 30.11.2020 10:23:26 >>>>>>
  IF p_obslt IS NOT INITIAL.
    APPEND INITIAL LINE TO gr_status ASSIGNING <lfs_status>.
    <lfs_status>     = 'IEQ'.
    <lfs_status>-low = '9'.
  ENDIF.
  "<<<<<< END OF INSERTION: <CH12> 30.11.2020 10:23:26  <<<<<<


*-- กลุ่มประเภทเอกสาร RD
  IF cb_inv IS NOT INITIAL.
    APPEND INITIAL LINE TO gr_rd_dctypgrp ASSIGNING <lfs_rd_dctypgr>.
    <lfs_rd_dctypgr>     = 'IEQ'.
    <lfs_rd_dctypgr>-low = 'INV'.
  ENDIF.

  IF cb_rct IS NOT INITIAL.
    APPEND INITIAL LINE TO gr_rd_dctypgrp ASSIGNING <lfs_rd_dctypgr>.
    <lfs_rd_dctypgr>     = 'IEQ'.
    <lfs_rd_dctypgr>-low = 'RCT'.
  ENDIF.

  IF cb_tiv IS NOT INITIAL.
    APPEND INITIAL LINE TO gr_rd_dctypgrp ASSIGNING <lfs_rd_dctypgr>.
    <lfs_rd_dctypgr>     = 'IEQ'.
    <lfs_rd_dctypgr>-low = 'TIV'.
  ENDIF.

  IF cb_dcn IS NOT INITIAL.
    APPEND INITIAL LINE TO gr_rd_dctypgrp ASSIGNING <lfs_rd_dctypgr>.
    <lfs_rd_dctypgr>     = 'IEQ'.
    <lfs_rd_dctypgr>-low = 'DCN'.
  ENDIF.

ENDFORM.                    "get_range
*&---------------------------------------------------------------------*
*& Form GET_PARAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_param .

  SELECT id name param_ext sequence
         endda begda param_sign param_option
         low_value high_value comments
    FROM ZSDSFIC015
    INTO TABLE gt_param
    WHERE id    = 'ZETX001'
    AND   endda >= sy-datum
    AND   begda <= sy-datum.

ENDFORM.                    "get_param
*&---------------------------------------------------------------------*
*& Form PROCESS_MAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_main .

  DATA: lwa_styl TYPE lvc_s_styl.

  FIELD-SYMBOLS: <lfs_head> LIKE LINE OF gt_head.

  PERFORM message_for_batch USING 'Start main process.'.

  PERFORM: check_read_txt,
           process_sd,
           process_fi.

  CASE abap_true.
    WHEN rb_trn.
      PERFORM assign_pdf_to_head.
    WHEN rb_rej.
  ENDCASE.

  LOOP AT gt_head ASSIGNING <lfs_head>.
    PERFORM read_messg   TABLES <lfs_head>-it_messg
                       CHANGING <lfs_head>-icon
                                <lfs_head>-messg.

    "To Disable error record
    CLEAR lwa_styl.
    IF <lfs_head>-icon EQ icon_red_light.
      lwa_styl-style = cl_gui_alv_grid=>mc_style_disabled.
      lwa_styl-fieldname = 'MAIL_CB'.
      APPEND lwa_styl TO <lfs_head>-styl.
      lwa_styl-fieldname = 'SEL_CB'.
      APPEND lwa_styl TO <lfs_head>-styl.
    ELSE.
      IF <lfs_head>-email_flag NE 'Y'.
        lwa_styl-style = cl_gui_alv_grid=>mc_style_disabled.
        lwa_styl-fieldname = 'MAIL_CB'.
        APPEND lwa_styl TO <lfs_head>-styl.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "process_main
*&---------------------------------------------------------------------*
*& Form GET_OTH_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_oth_data USING pt_bill TYPE gtty_bill
                        value(p_module).

  DATA: lt_bill TYPE gtty_bill.

  DATA: lwa_header LIKE LINE OF gt_doc_header.

  FIELD-SYMBOLS: <lfs_ref_doc> TYPE gty_vbrk,
                 <lfs_kunnr>   LIKE LINE OF gr_kunnr.

*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
*  DATA: lt_collector  LIKE gt_collector,
*        lwa_collector LIKE LINE OF gt_collector.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
  ">>>>>> BEGIN OF INSERTION: <CH09> on 05.11.2020 00:34:30 >>>>>>
  DATA: "ls_tmp_coll     LIKE LINE OF lt_collector,
        "lt_all_receipt  LIKE gt_collector,
        lv_dyn_query    TYPE string.
*  RANGES:   lr_recepit  FOR ztfit_collector-receipt_no,
*            lr_bukrs    FOR ztfit_collector-bukrs.

  "<<<<<< END OF INSERTION: <CH09> on 05.11.2020 00:34:30  <<<<<<
  REFRESH: gr_kunnr[].

  lt_bill[] = pt_bill[].
  DELETE lt_bill WHERE sap_doc_no IS INITIAL.

*  REFRESH : gt_doc_header[],gt_doc_item[],
*            gt_doc_partner[], gt_doc_h_vat[],
*            gt_doc_h_ref[],gt_ref_doc[],
*            gt_doc_dischg[],
*            gt_collector[],gt_one_sd[].     "INS: ETAX001

    REFRESH : gt_doc_header[],gt_doc_item[],
              gt_doc_partner[], gt_doc_h_vat[],
              gt_doc_h_ref[],gt_ref_doc[],
              gt_doc_dischg[],
              gt_one_sd[].     "INS: ETAX001

  IF lt_bill[] IS NOT INITIAL.
    SELECT *
      FROM ZSDSFIT014
      INTO TABLE gt_doc_header
      FOR ALL ENTRIES IN lt_bill
     WHERE bukrs       EQ lt_bill-bukrs
       AND sap_doc_no  EQ lt_bill-sap_doc_no
       AND gjahr       EQ lt_bill-gjahr
       AND rd_doc_type EQ lt_bill-rd_doc_type
       AND module_etx  EQ p_module.

    LOOP AT gt_doc_header INTO lwa_header.
      IF lwa_header-kunnr IS NOT INITIAL.
        APPEND INITIAL LINE TO gr_kunnr ASSIGNING <lfs_kunnr>.
        <lfs_kunnr>     = 'IEQ'.
        <lfs_kunnr>-low = lwa_header-kunnr.
      ENDIF.
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
      IF ( lwa_header-rd_doc_type EQ 'T01' OR
           lwa_header-rd_doc_type EQ 'T03' ) AND
          lwa_header-net_amt_bf_vat IS NOT INITIAL.
*        CLEAR lwa_collector.
*        lwa_collector-bukrs = lwa_header-bukrs.
*        lwa_collector-belnr = lwa_header-document_no.
*        lwa_collector-gjahr = lwa_header-gjahr.
*        APPEND lwa_collector TO lt_collector.
      ENDIF.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
    ENDLOOP.

    SORT: gr_kunnr BY low.
    DELETE ADJACENT DUPLICATES FROM : gr_kunnr COMPARING low.

    SELECT *
      FROM ZSDSFIT015
      INTO TABLE gt_doc_item
      FOR ALL ENTRIES IN lt_bill
     WHERE bukrs      EQ lt_bill-bukrs
       AND sap_doc_no EQ lt_bill-sap_doc_no
       AND gjahr      EQ lt_bill-gjahr
       AND rd_doc_type EQ lt_bill-rd_doc_type
       AND module_etx  EQ p_module.

    SELECT *
      FROM ZSDSFIT018
      INTO TABLE gt_doc_partner
      FOR ALL ENTRIES IN lt_bill
     WHERE bukrs      EQ lt_bill-bukrs
       AND sap_doc_no EQ lt_bill-sap_doc_no
       AND gjahr      EQ lt_bill-gjahr
       AND rd_doc_type EQ lt_bill-rd_doc_type
       AND module_etx  EQ p_module.

*    เก็บข้อมูลเอกสาร – VAT ระดับ Header
    SELECT *
      FROM ZSDSFIT017
      INTO TABLE gt_doc_h_vat
      FOR ALL ENTRIES IN lt_bill
     WHERE bukrs      EQ lt_bill-bukrs
       AND sap_doc_no EQ lt_bill-sap_doc_no
       AND gjahr      EQ lt_bill-gjahr
       AND rd_doc_type EQ lt_bill-rd_doc_type
       AND module_etx  EQ p_module.

* เก็บข้อมูลเอกสาร – อ้างอิง ระดับ Header
    SELECT *
     FROM ZSDSFIT016
      INTO TABLE gt_doc_h_ref
     FOR ALL ENTRIES IN lt_bill
    WHERE bukrs      EQ lt_bill-bukrs
      AND sap_doc_no EQ lt_bill-sap_doc_no
      AND gjahr      EQ lt_bill-gjahr
       AND rd_doc_type EQ lt_bill-rd_doc_type
       AND module_etx  EQ p_module.

    IF gt_doc_h_ref[] IS NOT INITIAL.

      "เอกสารใบลดหนี้
      IF cb_sd EQ 'X'.
        SELECT gjahr vbeln fkdat bukrs fkart
          FROM vbrk
          INTO TABLE gt_ref_doc
          FOR ALL ENTRIES IN gt_doc_h_ref
        WHERE vbeln EQ gt_doc_h_ref-ref_sap_doc_no.
*          AND gjahr EQ gt_doc_h_ref-ref_gjahr.
        LOOP AT gt_ref_doc ASSIGNING <lfs_ref_doc>.
          <lfs_ref_doc>-module = p_module.
        ENDLOOP.
      ENDIF.

      IF cb_fi EQ 'X'.
        SELECT gjahr belnr budat bukrs blart
          FROM bkpf
          APPENDING TABLE gt_ref_doc
          FOR ALL ENTRIES IN gt_doc_h_ref
        WHERE belnr EQ gt_doc_h_ref-ref_sap_doc_no
          AND gjahr EQ gt_doc_h_ref-ref_gjahr.
        LOOP AT gt_ref_doc ASSIGNING <lfs_ref_doc>.
          <lfs_ref_doc>-module = p_module.
        ENDLOOP.
      ENDIF.

      SORT gt_ref_doc BY gjahr vbeln bukrs.
    ENDIF.

* เก็บข้อมูลเอกสาร-ระดับ disc Charge
    SELECT *
      FROM ZSDSFIT013
      INTO TABLE gt_doc_dischg
      FOR ALL ENTRIES IN lt_bill
     WHERE bukrs      EQ lt_bill-bukrs
       AND sap_doc_no EQ lt_bill-sap_doc_no
       AND gjahr      EQ lt_bill-gjahr
       AND rd_doc_type EQ lt_bill-rd_doc_type
       AND module_etx  EQ p_module.

*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
    "Check table ZTFIT_COLLECTOR
*    IF lt_collector[] IS NOT INITIAL.
**>>> BOI CH06 >>>
*      SELECT bukrs belnr gjahr run_id
*             work_date pernr action_type
*             status receipt_no
*             delete_flag   "INS CH02 add delete_flag
*        FROM ztfit_collector
*        INTO TABLE gt_collector
*        FOR ALL ENTRIES IN lt_collector
*        WHERE bukrs EQ lt_collector-bukrs
*        AND   belnr EQ lt_collector-belnr.
***          AND gjahr EQ lt_collector-gjahr.        "CH09 DEL
**<<< EOI CH06 <<<
*      ">>>>>> BEGIN OF INSERTION: <CH09> on 04.11.2020 22:49:40 >>>>>>
***            AND ( belnr EQ lt_collector-belnr
***                OR
***                receipt_no EQ lt_collector-belnr ). "CH09
*      LOOP AT lt_collector INTO ls_tmp_coll
*        WHERE belnr CP 'R*' OR belnr CP 'C*' OR belnr CP 'A*'.
*        IF ls_tmp_coll-belnr NOT IN lr_recepit OR lr_recepit IS INITIAL.
*          lr_recepit-sign = 'I'.
*          lr_recepit-option = 'EQ'.
*          lr_recepit-low = ls_tmp_coll-belnr.
*          APPEND lr_recepit.
*        ENDIF.
*        IF ls_tmp_coll-bukrs NOT IN lr_bukrs OR lr_bukrs IS INITIAL.
*          lr_bukrs-sign = 'I'.
*          lr_bukrs-option = 'EQ'.
*          lr_bukrs-low = ls_tmp_coll-bukrs.
*          APPEND lr_bukrs.
*        ENDIF.
*      ENDLOOP.
*      IF lr_recepit IS NOT INITIAL.
*        lv_dyn_query = '( receipt_no LIKE ''R%'' OR receipt_no LIKE ''C%'' OR receipt_no LIKE ''A%'' )'.
***          lv_dyn_query = 'AND receipt_no IN lr_recepit'. "case 2
*        SELECT bukrs belnr gjahr run_id
*               work_date pernr action_type
*               status receipt_no
*               delete_flag   "INS CH02 add delete_flag
*          FROM ztfit_collector
*          INTO TABLE lt_all_receipt
*          WHERE bukrs IN lr_bukrs
*          AND (lv_dyn_query).
***            AND   receipt_no <> space.
*        IF sy-subrc = 0.
*          DELETE lt_all_receipt WHERE receipt_no NOT IN lr_recepit.
*          APPEND LINES OF lt_all_receipt TO gt_collector.
*        ENDIF.
*      ENDIF.
*
*      "<<<<<< END OF INSERTION: <CH09> on 04.11.2020 22:49:40  <<<<<<
*
**>>> BOD CH06 >>>
**      SELECT bukrs belnr gjahr run_id
**             work_date pernr action_type
**             status receipt_no
**             delete_flag   "INS CH02 add delete_flag
**        FROM ztfit_collector
**        INTO TABLE gt_collector
**        FOR ALL ENTRIES IN lt_collector
**        WHERE bukrs EQ lt_collector-bukrs
***          AND belnr EQ lt_collector-belnr
**          AND gjahr EQ lt_collector-gjahr.
**
***      SELECT bukrs belnr gjahr run_id
***             work_date pernr action_type
***             status receipt_no
***             delete_flag   "INS CH02 add delete_flag
***        FROM ztfit_collector
***        APPENDING TABLE gt_collector
***        FOR ALL ENTRIES IN lt_collector
***        WHERE bukrs EQ lt_collector-bukrs
***          AND receipt_no EQ lt_collector-belnr.
***          AND gjahr EQ lt_collector-gjahr.
**<<< EOD CH06 <<<
*    ENDIF.

    "Get One-time cust
    SELECT a~vbeln a~kunnr a~adrnr b~smtp_addr c~tel_number
      FROM vbpa AS a
      LEFT JOIN adr6 AS b ON a~adrnr EQ b~addrnumber
      LEFT JOIN adr2 AS c  ON a~adrnr EQ c~addrnumber AND
                              c~r3_user EQ '3'
      INTO TABLE gt_one_sd
      FOR ALL ENTRIES IN lt_bill
      WHERE a~vbeln = lt_bill-sap_doc_no
        AND a~parvw = 'AG'. "SP

    SORT gt_one_sd BY vbeln kunnr.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<

  ENDIF.

ENDFORM.                    "get_oth_data
*&---------------------------------------------------------------------*
*& Form GET_MAPPING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_mapping .

*  เก็บประเภทรหัสเอกสารของสรรพากร
  SELECT rd_doc_type
         "rd_doc_type_desc_th
         "rd_doc_type_desc_en
         rd_doc_typ_ds_th
         rd_doc_typ_ds_en
         rd_doc_type_grp
    FROM ZSDSFIC005
    INTO TABLE gt_rd_doc_type.

*  Mapping ประเภทรหัสสาเหตุการออกเอกสารของสรรพากร
  SELECT sap_resn rd_doc_type rd_doc_resn
    FROM ZSDSFIC010
    INTO TABLE gt_map_doc_resn.

* Mapping ประเภทรหัสสกุลเงินของสรรพากร
  SELECT rd_curr_code waerk
    FROM ZSDSFIC007
    INTO TABLE gt_map_curr.

* Mapping รหัสประเภทภาษี
  SELECT tax_code rd_vat_type
    FROM ZSDSFIC014
    INTO TABLE gt_map_vat_type.

* Mapping ประเภทรหัสเอกสารของสรรพากร (SD)
  SELECT vkorg fkart tax_code rd_doc_type
         rd_flag email_flag
    FROM ZSDSFIC011
    INTO TABLE gt_map_doc_sd.

* Mapping ประเภทรหัสเอกสารของสรรพากร (FI)
  SELECT bukrs blart tax_code rd_doc_type
         rd_flag email_flag
    FROM ZSDSFIC009
    INTO TABLE gt_map_doc_fi.

* Mapping Schema id ประเทศ
  SELECT cntry_code rd_cntry_code
    FROM ZSDSFIC013
    INTO TABLE gt_map_sch_ctry.

*  Mapping ประเภทรหัสส่วนลดค่าธรรมเนียมของสรรพากร
  SELECT sd_cond sd_subt fi_gl rd_dischg
    FROM ZSDSFIC008
    INTO TABLE gt_map_dischg.

*   เก็บประเภทรหัสสาเหตุการออกเอกสารของสรรพากร
  SELECT rd_doc_resn rd_doc_resn_desc
    FROM ZSDSFIC004
    INTO TABLE gt_rd_doc_resn.

* เก็บประเภทคู่ค้าของสรรพากร
  SELECT rd_partner rd_partner_desc rd_partner_type
    FROM ZSDSFIC018
    INTO TABLE gt_rd_partner.

*  เก็บประเภทรหัสส่วนลดค่าธรรมเนียมของสรรพากร
  SELECT rd_dischg rd_dischg_desc_t
         rd_dischg_desc_e rd_dischg_type
    FROM ZSDSFIC016
    INTO TABLE gt_rd_dischg.

* เก็บรหัสตำบลของสรรพากร
  SELECT sub_dist_code sub_dist_name dist_code province_code
    FROM ZSDSFIC020
    INTO TABLE gt_rd_sub_dist.

* เก็บรหัสอำเภอของสรรพากร
  SELECT dist_code dist_name province_code
      FROM ZSDSFIC017
      INTO TABLE gt_rd_district.

* เก็บรหัสจังหวัดของสรรพากร
  SELECT province_code province_name
      FROM ZSDSFIC019
      INTO TABLE gt_rd_province.

*  เก็บรหัสหน่วย
  SELECT sap_unit rd_unit_code rd_unit_name
    FROM ZSDSFIC021
    INTO TABLE gt_rd_unit.

* เก็บที่อยู่ของบริษัท
  SELECT bukrs bupla home_no sub_dist_code
         postal country sch_id
    FROM ZSDSFIC003
    INTO TABLE gt_company_addr
   WHERE bukrs = p_bukrs.

ENDFORM.                    "get_mapping
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data USING pt_bill TYPE gtty_bill
                        value(p_module).

  DATA: lwa_bill TYPE gty_bill.

  DATA: lv_name     TYPE ZSDSFIC015-name,
        lv_module   TYPE c LENGTH 2,
        lv_tdid     TYPE thead-tdid,
        lv_sequence TYPE ZSDSFIC015-sequence,
        lv_idx_bill TYPE subrc.

*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
  DATA: lv_sch_id TYPE zsdsde_partner_sch_id.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<

*  DATA: lt_valid_header   TYPE TABLE OF gty_doc_header.     "INS CH05

  FIELD-SYMBOLS: <lfs_head> LIKE LINE OF gt_head.

  lv_name   = 'TEXT_OBJ_NOTRD'.
  lv_module = p_module.

  CASE lv_module.
    WHEN 'SD'.
      lv_sequence = '1'.
    WHEN 'FI'.
      lv_sequence = '1'.
    WHEN OTHERS.
  ENDCASE.

  PERFORM get_text_obj    USING lv_name
                                lv_module
                                lv_sequence
                       CHANGING lv_tdid.

  LOOP AT pt_bill INTO lwa_bill.
    APPEND INITIAL LINE TO gt_head ASSIGNING <lfs_head>.

    <lfs_head>-icon        = icon_green_light.
    <lfs_head>-sel_cb      = abap_off.
    <lfs_head>-mail_cb     = abap_off.
    <lfs_head>-sap_doc_no  = lwa_bill-vbeln.
    <lfs_head>-bukrs       = lwa_bill-bukrs.
    <lfs_head>-gjahr       = lwa_bill-gjahr.
    <lfs_head>-ernam       = lwa_bill-ernam.                "INS Ch11
    <lfs_head>-cpudt       = lwa_bill-erdat.                "INS Ch11

*>>> BOI CH05 >>>
*    lt_valid_header = gt_doc_header.
*
*    DELETE lt_valid_header
*      WHERE bukrs       <> lwa_bill-bukrs
*         OR sap_doc_no  <> lwa_bill-sap_doc_no
*         OR gjahr       <> lwa_bill-gjahr
*         OR module_etx  <> lv_module
*         OR status      = '9'.
*
*    IF LINES( lt_valid_header ) = 0.
*      APPEND INITIAL LINE TO gt_head ASSIGNING <lfs_head>.
*
*      <lfs_head>-icon        = icon_green_light.
*      <lfs_head>-sel_cb      = abap_off.
*      <lfs_head>-mail_cb     = abap_off.
*      <lfs_head>-sap_doc_no  = lwa_bill-vbeln.
*      <lfs_head>-bukrs       = lwa_bill-bukrs.
*      <lfs_head>-gjahr       = lwa_bill-gjahr.
*
*      PERFORM add_messg        USING icon_red_light text-e04
*                                     '' '' '' ''
*                            CHANGING <lfs_head>-it_messg.
*    ELSE.
*      IF lwa_bill-status = '9'.
*        CONTINUE.
*      ELSE.
*        APPEND INITIAL LINE TO gt_head ASSIGNING <lfs_head>.
*
*        <lfs_head>-icon        = icon_green_light.
*        <lfs_head>-sel_cb      = abap_off.
*        <lfs_head>-mail_cb     = abap_off.
*        <lfs_head>-sap_doc_no  = lwa_bill-vbeln.
*        <lfs_head>-bukrs       = lwa_bill-bukrs.
*        <lfs_head>-gjahr       = lwa_bill-gjahr.
*      ENDIF.
*    ENDIF.
*<<< EOI CH05 <<<

*>>> BEGIN OF INSERTION: <ETAX001> on 11.09.2020 <<<
    PERFORM check_eff_date    USING lwa_bill-fkdat
                           CHANGING <lfs_head>-it_messg.
*>>> END OF INSERTION: <ETAX001> on 11.09.2020 <<<

    PERFORM check_scope   USING lv_module
                                lwa_bill-bukrs
                                lwa_bill-fkart
                                lwa_bill-vkorg
                                lwa_bill-rfbsk
                                lwa_bill-vbeln
                                <lfs_head>-sel_cb
                                <lfs_head>-mail_cb
                       CHANGING <lfs_head>-it_messg.

    PERFORM read_doc_header    USING lwa_bill
                                     lv_module
                                     lv_tdid
                            CHANGING <lfs_head>
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
                                     lv_sch_id.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<

    PERFORM get_partner        USING lwa_bill
                                     lv_module
                                     <lfs_head>
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
                                     lv_sch_id
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
                            CHANGING <lfs_head>-it_partner
                                     <lfs_head>-it_messg.

    PERFORM get_h_ref          USING lwa_bill
                                     lv_module
                                     <lfs_head>
                            CHANGING <lfs_head>-it_h_ref
                                     <lfs_head>-it_messg.

    PERFORM get_disc_amt      USING  lwa_bill
                                     lv_module
                                     <lfs_head>
                            CHANGING <lfs_head>-total_disc_amt
                                     <lfs_head>-total_charge_amt
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
                                     <lfs_head>-gross_amt
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
                                     <lfs_head>-link_disc_charge
                                     <lfs_head>-it_charge
                                     <lfs_head>-it_messg.

    PERFORM get_doc_item       USING lwa_bill
                                     lv_module
                                     <lfs_head>
                            CHANGING <lfs_head>-it_item
                                     <lfs_head>-it_messg.

    PERFORM set_sapobject_id USING lwa_bill-bukrs
                                   lwa_bill-vbeln
                                   lwa_bill-gjahr
                                   lv_module
                          CHANGING <lfs_head>-sapobjectid.

  ENDLOOP.

ENDFORM.                    "process_data
*&---------------------------------------------------------------------*
*& Form READ_EMAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_BILL_KUNNR
*&      <-- <LFS_HEAD>_EMAIL
*&---------------------------------------------------------------------*
FORM read_email  USING    pi_kunnr TYPE vbrk-kunrg
                          pi_bukrs TYPE vbrk-bukrs
                          pi_vkorg TYPE vbrk-vkorg
*>>> BEGIN OF INSERTION: <ETAX001> on 22.09.2020 <<<
                          value(pii_module)
                          pi_sap_doc_no TYPE ZSDSFIT014-sap_doc_no
                          pi_gjahr  TYPE ZSDSFIT014-gjahr
*>>> END OF INSERTION: <ETAX001> on 22.09.2020 <<<
                 CHANGING po_email  TYPE ZSDSFIC001-e_mail
                          po_mobile TYPE ZSDSFIC001-mobile_phone.

  DATA: lwa_mail TYPE gty_email,
*>>> BEGIN OF INSERTION: <ETAX001> on 22.09.2020 <<<
        lwa_one_sd LIKE LINE OF gt_one_sd,
        lwa_param  LIKE LINE OF gt_param.

  DATA: lv_name     TYPE ZSDSFIC015-name,
        lv_module   TYPE c LENGTH 2,
        lv_tdid     TYPE thead-tdid,
        lv_sequence TYPE ZSDSFIC015-sequence.

  DATA: lv_tname    TYPE thead-tdname,
        lv_tobj     TYPE thead-tdobject,
        lv_value    TYPE char255,
        lv_email    TYPE ZSDSFIC001-e_mail,
        lv_mobile   TYPE adr2-tel_number.

  DATA: lr_sms TYPE RANGE OF char255.

  FIELD-SYMBOLS <lfs_r_sms> LIKE LINE OF lr_sms.

  CLEAR: po_email,po_mobile.

  IF pi_kunnr(2) = 'OT'.      "One-time customer

    LOOP AT gt_param INTO lwa_param
      WHERE name  = 'SMS_ONETIME'.
      APPEND INITIAL LINE TO lr_sms ASSIGNING <lfs_r_sms>.
      <lfs_r_sms>-sign   = lwa_param-param_sign.
      <lfs_r_sms>-option = lwa_param-param_option.
      <lfs_r_sms>-low    = lwa_param-low_value.
      <lfs_r_sms>-high   = lwa_param-high_value.
    ENDLOOP.

    IF pii_module EQ 'SD'.

      READ TABLE gt_one_sd INTO lwa_one_sd
        WITH KEY vbeln = pi_sap_doc_no
                 kunnr = pi_kunnr BINARY SEARCH.
      IF sy-subrc EQ 0.

        IF pi_sap_doc_no IN lr_sms.
          "ถ้า doc no ขึ้นต้นด้วย 511*
          "-->ถ้ามี mail หยิบ mail มา ถ้าไม่มี mail หยิบ เบอร์โทร
          IF lwa_one_sd-email IS NOT INITIAL.
            po_email  = lwa_one_sd-email.
          ELSE.
            PERFORM delete_char_in_text CHANGING lwa_one_sd-tel_number. "INS CH01
            po_mobile = lwa_one_sd-tel_number.
          ENDIF.

        ELSE.
          "--> ถ้ามี mail หยิบ mail
          IF lwa_one_sd-email IS NOT INITIAL.
            po_email  = lwa_one_sd-email.
          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF pii_module EQ 'FI'.

      CONCATENATE pi_bukrs pi_sap_doc_no pi_gjahr INTO lv_tname.
      lv_tobj  = 'BELEG'.

      "email
      lv_name   = 'TEXT_OBJ_ONETIME_MAIL'.
      lv_module = pii_module.

      PERFORM get_text_obj    USING lv_name
                                    lv_module
                                    lv_sequence
                           CHANGING lv_tdid.

      PERFORM read_text    USING lv_tdid
                                 lv_tname
                                 lv_tobj
                        CHANGING lv_value.

      lv_email = lv_value.

      CLEAR: lv_name, lv_module, lv_sequence, lv_tdid.

      "mobile
      lv_name   = 'TEXT_OBJ_ONETIME_MOBILEPHONE'.
      lv_module = pii_module.

      PERFORM get_text_obj    USING lv_name
                                    lv_module
                                    lv_sequence
                           CHANGING lv_tdid.

      PERFORM read_text    USING lv_tdid
                                 lv_tname
                                 lv_tobj
                        CHANGING lv_value.

      lv_mobile = lv_value.

      IF pi_sap_doc_no IN lr_sms.
        "ถ้า doc no ขึ้นต้นด้วย 511*
        "-->ถ้ามี mail หยิบ mail มา ถ้าไม่มี mail หยิบ เบอร์โทร
        IF lv_email IS NOT INITIAL.
          po_email  = lv_email.
        ELSE.
          PERFORM delete_char_in_text CHANGING lv_mobile.   "INS CH01
          po_mobile = lv_mobile.
        ENDIF.

      ELSE.
        "--> ถ้ามี mail หยิบ mail
        IF lv_email IS NOT INITIAL.
          po_email  = lv_email.
        ENDIF.

      ENDIF.
    ENDIF.

  ELSE.
*>>> END OF INSERTION: <ETAX001> on 22.09.2020 <<<

    LOOP AT gt_mail INTO lwa_mail
     WHERE kunnr = pi_kunnr.
*            bukrs = pi_bukrs
*            vkorg = pi_vkorg.
*Note - ในกรณีที่ต้องการ Filter E-Mail ตามเงื่อนไขต่าง ๆ สามารถเพิ่มเงื่อนไขด้วย RD_DOC_TYPE, BUKRS, VKORG
      IF po_email IS INITIAL.
        po_email  = lwa_mail-e_mail.
      ELSE.
        CONCATENATE po_email lwa_mail-e_mail INTO po_email
        SEPARATED BY ';'.
      ENDIF.

*ในกรณีที่ e-mail มีหลายรายการ ให้แสดงทุกรายการโดยคั่นด้วย ;
      CLEAR lv_mobile.
      lv_mobile = lwa_mail-mobile_phone.
      PERFORM delete_char_in_text CHANGING lv_mobile.       "INS CH01

      IF po_mobile IS INITIAL.
        po_mobile = lv_mobile.
      ELSE.
        CONCATENATE po_mobile lv_mobile INTO po_mobile
        SEPARATED BY ';'.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    "read_email
*&---------------------------------------------------------------------*
*& Form READ_DOC_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_BILL_SAP_DOC_NO
*&      --> LWA_BILL_BUKRS
*&      --> LWA_BILL_VKORG
*&      <-- <LFS_HEAD>
*&---------------------------------------------------------------------*
FORM read_doc_header  USING     pi_bill       TYPE gty_bill
                                value(pi_module)
                                pi_tdid       TYPE thead-tdid
                      CHANGING  po_head       TYPE gty_head
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
                                po_sch_id.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
  DATA lwa_header TYPE gty_doc_header.

  DATA: lv_verified TYPE abap_bool.

  DATA: ls_param LIKE LINE OF gt_param,
        ls_sd_bseg LIKE LINE OF gt_sd_bseg.

*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
  CLEAR po_sch_id.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<

  READ TABLE gt_doc_header INTO lwa_header
  WITH TABLE KEY bukrs       = pi_bill-bukrs
                 sap_doc_no  = pi_bill-sap_doc_no
                 gjahr       = pi_bill-gjahr
                 rd_doc_type = pi_bill-rd_doc_type
                 module_etx  = pi_module.
  IF sy-subrc EQ 0.

    MOVE-CORRESPONDING lwa_header TO po_head.

    PERFORM read_email    USING po_head-kunnr
                                pi_bill-bukrs
                                pi_bill-vkorg
*>>> BEGIN OF INSERTION: <ETAX001> on 22.09.2020 <<<
                                pi_module
                                pi_bill-sap_doc_no
                                pi_bill-gjahr
*>>> END OF INSERTION: <ETAX001> on 22.09.2020 <<<
                       CHANGING po_head-e_mail
                                po_head-mobile_phone.

    IF pi_module EQ 'SD'.
      CLEAR pi_bill-fksto.
    ELSE.

      "กรณี Document ยังไม่ Reverse
      IF pi_bill-fksto IS INITIAL.
        po_head-reverse_flag = 'N'.
      ELSE.
        po_head-reverse_flag = 'Y'.
      ENDIF.

    ENDIF.
    "Check Doc verification Status form cloud
    PERFORM check_rd_sent    USING pi_bill
                                   pi_module
                          CHANGING lv_verified.

    PERFORM check_error    USING pi_bill
                                 lwa_header
                                 lv_verified
                        CHANGING po_head-it_messg.
    ">>>>>> BEGIN OF INSERTION: <CH14> on 13.01.2021 13:33:21 >>>>>>
**    IF pi_module EQ 'SD'.
**      LOOP AT gt_sd_bseg INTO ls_sd_bseg
**        where bukrs = lwa_header-bukrs
**        AND belnr = lwa_header-sap_doc_no
**        AND gjahr = lwa_header-gjahr
**        AND koart = 'D'
**        AND augbl <> space
**        AND augdt <> space.
**        exit. "เจอก็จะไม่เช็ค collector
**      ENDLOOP.
**      IF sy-subrc <> 0.
**        PERFORM check_collector USING lwa_header
**                                      pi_module
**                             CHANGING po_head-it_messg.
**      ENDIF.
**    ELSEIF pi_module EQ 'FI'.

    ">>> CH17 : BOI
    IF gv_check_collector_arc EQ 'X' .
      PERFORM check_arc_collector USING lwa_header
                                        pi_module
                               CHANGING po_head-it_messg.
      "<<<CH17 : EOI
    ELSE .
      PERFORM check_collector USING lwa_header
                                    pi_module
                           CHANGING po_head-it_messg.
    ENDIF.

**    ENDIF.
    "<<<<<< END OF INSERTION: <CH14> on 13.01.2021 13:33:21  <<<<<<

    po_head-document_no      = lwa_header-document_no.
    po_head-sap_posting_date = lwa_header-sap_posting_date.
    po_head-rd_doc_type      = lwa_header-rd_doc_type.
    po_head-rd_doc_type_desc = pi_bill-rd_doc_type_desc_th.
    po_head-sap_doc_type     = lwa_header-sap_doc_type.

    PERFORM read_sap_doc_typ_des    USING pi_module
                                          po_head-sap_doc_type
                                 CHANGING po_head-sap_doc_type_desc.

    PERFORM check_cancel    USING pi_bill
                                  po_head-reverse_flag
                                  po_head-replace_flag
                                  pi_module
                                  pi_tdid
                         CHANGING po_head-cancel
                                  po_head-rd_flag.

    po_head-bupla        = lwa_header-bupla.
    po_head-kunnr        = lwa_header-kunnr.
    po_head-kunnr_name   = lwa_header-kunnr_name.
    po_head-kunnr_branch = lwa_header-kunnr_branch.
    po_head-kunnr_tax_id = lwa_header-kunnr_tax_id.

*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
    PERFORM check_cust_tax    USING po_head-kunnr_branch
                           CHANGING po_head-kunnr_tax_id
                                    po_head-it_messg
                                    po_sch_id.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<

    po_head-link_partner = gc_detail.

    IF lwa_header-replace_flag EQ 'Y'.
      po_head-link_replace_doc = gc_yes.
    ENDIF.

    CLEAR ls_param.
    READ TABLE gt_param INTO ls_param
      WITH KEY name = 'CDN_VF11'
               param_ext = po_head-sap_doc_type.
    IF sy-subrc EQ 0.
      po_head-sap_doc_resn = ls_param-low_value.
    ELSE.
      po_head-sap_doc_resn = lwa_header-sap_doc_resn.
    ENDIF.

*    po_head-sap_doc_resn_desc = lwa_header-sap_doc_resn_desc.
    po_head-sap_doc_resn_des = lwa_header-sap_doc_resn_des.

    IF lwa_header-replace_flag EQ 'Y' OR
       ( po_head-rd_doc_type EQ '80' OR po_head-rd_doc_type EQ '81' ).

      PERFORM read_rd_doc_reason    USING po_head-sap_doc_resn
                                          po_head-rd_doc_type
                                          lwa_header-sap_doc_resn_des
                                 CHANGING po_head-rd_doc_resn
                                          po_head-rd_doc_resn_desc
                                          po_head-it_messg.

      po_head-link_ref_doc = gc_detail.
    ENDIF.

    po_head-req_dev_date  = lwa_header-req_dev_date.
    po_head-pay_term      = lwa_header-pay_term.
    po_head-pay_due_date  = lwa_header-pay_due_date.
    po_head-inco_term     = lwa_header-inco_term.
    po_head-sap_po_no     = lwa_header-sap_po_no.
    po_head-sap_po_date   = lwa_header-sap_po_date.
    po_head-global_doc_no = lwa_header-global_doc_no.
    po_head-sap_curr      = lwa_header-sap_curr.

    PERFORM read_map_curr    USING po_head-sap_curr
                          CHANGING po_head-rd_curr_code
                                   po_head-it_messg.

    PERFORM read_h_vat   USING  pi_bill
                                pi_module
                                po_head
                       CHANGING po_head-tax_code
                                po_head-vat_rate
                                po_head-gross_amt
                                po_head-it_h_vat
                                po_head-it_messg.

    po_head-vat_base_amt    = lwa_header-vat_base_amt.
    po_head-net_amt_bf_vat  = lwa_header-net_amt_bf_vat.
    po_head-vat_amt         = lwa_header-vat_amt.
    po_head-net_amt_aft_vat = lwa_header-net_amt_aft_vat.
    po_head-link_vat        = gc_detail.
    ">>>>>> BEGIN OF INSERTION: <CH13> on 13.01.2021 18:59:41 >>>>>>
    IF po_head-net_amt_bf_vat < 0 .
      PERFORM add_messg    USING icon_red_light text-e44
                                 'Net amount before vat'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.
    IF po_head-vat_amt < 0.
      PERFORM add_messg    USING icon_red_light text-e44
                                 'Vat amount'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.
    IF po_head-net_amt_aft_vat < 0.
      PERFORM add_messg    USING icon_red_light text-e44
                                 'Net amount after vat'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.
    "<<<<<< END OF INSERTION: <CH13> on 13.01.2021 18:59:41  <<<<<<
*    Display เฉพาะใบเพิ่มหนี้ลดหนี้
    IF po_head-rd_doc_type EQ '80' OR po_head-rd_doc_type EQ '81'.
      po_head-ref_doc_amt = lwa_header-ref_doc_amt.
      po_head-correct_amt = lwa_header-correct_amt.
      po_head-diff_amt = lwa_header-diff_amt.
    ENDIF.

    IF po_head-document_no IS INITIAL.
      "Tax Document No. cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Tax Document No.'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.

    IF po_head-rd_doc_type_desc IS INITIAL.
      "RD Document Type Desc. cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'RD Document Type Desc.'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.

    IF po_head-rd_doc_type IS INITIAL.
      "RD Document Type cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'RD Document Type'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.

    IF po_head-sap_posting_date IS INITIAL.
      "Document Date cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Document Date'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.

    IF po_head-sap_create_date IS INITIAL AND
       po_head-rd_doc_type_grp NE 'INV'.
      "Create Date cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Create Date'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.

    IF po_head-rd_doc_type EQ '80' OR po_head-rd_doc_type EQ '81' OR
       po_head-replace_flag EQ 'Y'.
      IF po_head-rd_doc_resn IS INITIAL.
        "RD Doc Reason cannot be blank
        PERFORM add_messg    USING icon_red_light text-e41
                                   'RD Doc Reason'
                                   '' '' ''
                          CHANGING po_head-it_messg.
      ENDIF.

      IF po_head-rd_doc_resn IS INITIAL.
        "RD Document Reason  cannot be blank
        PERFORM add_messg    USING icon_red_light text-e41
                                   'RD Document Reason'
                                   '' '' ''
                          CHANGING po_head-it_messg.
      ENDIF.

    ENDIF.

    IF po_head-rd_curr_code IS INITIAL.
      "RD Currency cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'RD Currency'
                                 '' '' ''
                        CHANGING po_head-it_messg.
    ENDIF.

  ELSE.

    IF pi_module EQ 'FI'.
      "กรณี Document ยังไม่ Reverse
      IF pi_bill-fksto IS INITIAL.
        PERFORM add_messg        USING icon_red_light text-e04
                                       '' '' '' ''
                              CHANGING po_head-it_messg.
      ELSE.
        "กรณี Document ถูก Reverse
        PERFORM add_messg        USING icon_red_light text-e20
                                       '' '' '' ''
                              CHANGING po_head-it_messg.

      ENDIF.
    ELSE.
      PERFORM add_messg        USING icon_red_light text-e04
                                     '' '' '' ''
                            CHANGING po_head-it_messg.

    ENDIF.
  ENDIF.

ENDFORM.                    "read_doc_header
*&---------------------------------------------------------------------*
*& Form READ_SAP_DOC_TYP_DES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- <LFS_HEAD>_SAP_DOC_TYPE_DESC
*&---------------------------------------------------------------------*
FORM read_sap_doc_typ_des  USING    value(p_module)
                                    pi_sap_doc_type TYPE tvfkt-fkart
                           CHANGING po_desc.

  DATA: lwa_tvfkt TYPE gty_tvfkt,
        lwa_t003t TYPE gty_t003t.

  CLEAR po_desc.

  CASE p_module.
    WHEN 'SD'.
      READ TABLE gt_tvfkt INTO lwa_tvfkt
        WITH TABLE KEY fkart = pi_sap_doc_type.
      IF sy-subrc EQ 0.
        po_desc = lwa_tvfkt-vtext.
      ENDIF.
    WHEN 'FI'.
      READ TABLE gt_t003t INTO lwa_t003t
        WITH TABLE KEY blart = pi_sap_doc_type.
      IF sy-subrc EQ 0.
        po_desc = lwa_t003t-ltext.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "read_sap_doc_typ_des
*&---------------------------------------------------------------------*
*& Form READ_RD_DOC_REASON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PO_HEAD_SAP_DOC_RESN
*&      --> PO_HEAD_RD_DOC_TYPE
*&      <-- PO_HEAD_RD_DOC_TYPE
*&---------------------------------------------------------------------*
FORM read_rd_doc_reason  USING    pi_sap_doc_resn TYPE ZSDSFIT014-sap_doc_resn
                                  pi_rd_doc_type  TYPE ZSDSFIT014-rd_doc_type
                                  value(pi_sap_doc_resn_des)
                         CHANGING po_rd_doc_resn  TYPE ZSDSFIT014-rd_doc_resn
                                  po_rd_doc_resn_desc TYPE ZSDSFIC004-rd_doc_resn_desc
                                  pit_messg TYPE gtty_messg.

  DATA: lwa_map_doc_resn TYPE gty_map_doc_resn,
        lwa_rd_doc_resn  TYPE gty_rd_doc_resn.

  CHECK pi_sap_doc_resn IS NOT INITIAL AND
        pi_rd_doc_type IS NOT INITIAL.

  READ TABLE gt_map_doc_resn INTO lwa_map_doc_resn
  WITH TABLE KEY sap_resn    = pi_sap_doc_resn
                 rd_doc_type = pi_rd_doc_type.
  IF sy-subrc NE 0.
    "No RD Doc Reason for &SAP_DOC_RESN And RD Doc Type & RD_DOC_TYPE
    PERFORM add_messg    USING icon_red_light text-e01
                               pi_sap_doc_resn
                               pi_rd_doc_type
                               ''
                               ''
                      CHANGING pit_messg.

  ELSE.
    po_rd_doc_resn = lwa_map_doc_resn-rd_doc_resn.

    READ TABLE gt_rd_doc_resn INTO lwa_rd_doc_resn
      WITH TABLE KEY rd_doc_resn = po_rd_doc_resn.
    IF sy-subrc EQ 0.
      po_rd_doc_resn_desc = lwa_rd_doc_resn-rd_doc_resn_desc.

      IF po_rd_doc_resn+4(2) = '99'.
        REPLACE ALL OCCURRENCES OF 'ระบุสาเหตุ' IN po_rd_doc_resn_desc WITH pi_sap_doc_resn_des.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "read_rd_doc_reason
*&---------------------------------------------------------------------*
*& Form READ_MAP_CURR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PO_HEAD_SAP_CURR
*&      <-- PO_HEAD_RD_CURR_CODE
*&      <-- PO_HEAD_ICON
*&      <-- PO_HEAD_MESSG
*&---------------------------------------------------------------------*
FORM read_map_curr  USING    pi_sap_curr     TYPE ZSDSFIT014-sap_curr
                    CHANGING po_rd_curr_code TYPE ZSDSFIT014-rd_curr_code
                             pit_messg       TYPE gtty_messg.

  DATA lwa_map_curr TYPE gty_map_curr.

  IF pi_sap_curr IS NOT INITIAL.

    READ TABLE gt_map_curr INTO lwa_map_curr
      WITH TABLE KEY waerk = pi_sap_curr.
    IF sy-subrc NE 0.
      "No RD Currency for & SAP_CURR
      PERFORM add_messg        USING icon_red_light text-e02
                                     pi_sap_curr
                                     ''
                                     ''
                                     ''
                            CHANGING pit_messg.

    ELSE.
      po_rd_curr_code = lwa_map_curr-rd_curr_code.
    ENDIF.

  ELSE.

    "SAP Currency cannot be blank
    PERFORM add_messg    USING icon_red_light text-e41
                               'SAP Currency'
                               '' '' ''
                      CHANGING pit_messg.

  ENDIF.

ENDFORM.                    "read_map_curr
*&---------------------------------------------------------------------*
*& Form GET_EMAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_email .

  SORT: gr_kunnr BY low.

  DELETE ADJACENT DUPLICATES FROM : gr_kunnr COMPARING low.

  REFRESH gt_mail[].
  SELECT kunnr runno e_mail mobile_phone rd_doc_type
         sap_doc_type bukrs vkorg
    FROM ZSDSFIC001
    INTO TABLE gt_mail
   WHERE kunnr IN gr_kunnr.
*     AND rd_doc_type IN gr.
*      AND bukrs EQ p_bukrs
*      AND vkorg IN s_vkorg.

ENDFORM.                    "get_email
*&---------------------------------------------------------------------*
*& Form READ_H_VAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_SAP_DOC_NO
*&      --> PI_BUKRS
*&      --> PI_GJAHR
*&      <-- PO_HEAD_RD_VAT_TYPE
*&---------------------------------------------------------------------*
FORM read_h_vat  USING    ppi_bill       TYPE gty_bill
                          value(ppi_module)
                          pi_head        TYPE gty_head
                 CHANGING po_tax_code    TYPE ZSDSFIT017-tax_code
                          po_vat_rate    TYPE ZSDSFIT017-vat_rate
                          po_gross_amt   TYPE ZSDSFIT017-net_amt_bf_vat
                          pit_h_vat      TYPE gtty_doc_h_vat
                          pit_messg      TYPE gtty_messg.

  DATA lt_sum_vat TYPE gtty_doc_h_vat.

  DATA: lwa_doc_h_vat TYPE gty_doc_h_vat,
        lwa_sum_vat   TYPE gty_doc_h_vat.

  LOOP AT gt_doc_h_vat INTO lwa_doc_h_vat
    WHERE bukrs = ppi_bill-bukrs
      AND sap_doc_no = ppi_bill-sap_doc_no
      AND gjahr = ppi_bill-gjahr
      AND rd_doc_type = ppi_bill-rd_doc_type
      AND module_etx  = ppi_module.

    lwa_sum_vat-tax_code       = lwa_doc_h_vat-tax_code.
    lwa_sum_vat-net_amt_bf_vat = lwa_doc_h_vat-net_amt_bf_vat.
    lwa_sum_vat-vat_rate_txt   = lwa_doc_h_vat-vat_rate.
    CONDENSE lwa_sum_vat-vat_rate_txt NO-GAPS.
    COLLECT lwa_sum_vat INTO lt_sum_vat.


    lwa_doc_h_vat-document_no      = pi_head-document_no.
    lwa_doc_h_vat-rd_doc_type      = pi_head-rd_doc_type.
    lwa_doc_h_vat-rd_doc_type_desc = pi_head-rd_doc_type_desc.
    lwa_doc_h_vat-kunnr            = pi_head-kunnr.
    lwa_doc_h_vat-kunnr_name       = pi_head-kunnr_name.

    PERFORM read_map_vat_type    USING lwa_doc_h_vat-tax_code
                              CHANGING lwa_doc_h_vat-rd_vat_type
                                       pit_messg.

    IF lwa_doc_h_vat-rd_vat_type IS INITIAL.
      "RD VAT Type cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'RD VAT Type (Header)'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

    ">>>>>> BEGIN OF INSERTION: <CH13> on 13.01.2021 09:36:23 >>>>>>
    IF lwa_doc_h_vat-net_amt_bf_vat < 0.
      "Net Amount before vat (Header) Must Grather than 0
      PERFORM add_messg    USING icon_red_light text-e44
                                 'Net Amount before vat (Header)'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

    IF lwa_doc_h_vat-vat_amt < 0.
      "Vat Amount (Header) Must Grather than 0
      PERFORM add_messg    USING icon_red_light text-e44
                                 'Vat Amount (Header)'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.
    "<<<<<< END OF INSERTION: <CH13> on 13.01.2021 09:36:23  <<<<<<

    APPEND lwa_doc_h_vat TO pit_h_vat.

  ENDLOOP.
*>>> BOI CH07 >>>
  IF sy-subrc <> 0.
    PERFORM add_messg    USING icon_red_light text-e55
                               '' '' '' ''
                      CHANGING pit_messg.
  ENDIF.
*<<< EOI CH07 <<<

  po_gross_amt = pi_head-net_amt_bf_vat.

ENDFORM.                    "read_h_vat
*&---------------------------------------------------------------------*
*& Form READ_MAP_VAT_TYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PO_HEAD_TAX_CODE
*&      <-- PO_HEAD_RD_VAT_TYPE
*&      <-- PO_HEAD_ICON
*&      <-- PO_HEAD_MESSG
*&---------------------------------------------------------------------*
FORM read_map_vat_type  USING    pi_tax_code     TYPE ZSDSFIT017-tax_code
                        CHANGING pi_rd_vat_type  TYPE ZSDSFIC014-rd_vat_type
                                 pit_messg TYPE gtty_messg.

  DATA lwa_map_vat_type TYPE gty_map_vat_type.

  CHECK pi_tax_code NE '**' AND
        pi_tax_code IS NOT INITIAL.

  READ TABLE gt_map_vat_type INTO lwa_map_vat_type
    WITH TABLE KEY tax_code = pi_tax_code.
  IF sy-subrc NE 0.
    "No VAT Type for Tax Code: & TAX_CODE
    PERFORM add_messg        USING icon_red_light text-e03
                                   pi_tax_code '' '' ''
                          CHANGING pit_messg.
  ELSE.
    pi_rd_vat_type = lwa_map_vat_type-rd_vat_type.
  ENDIF.

ENDFORM.                    "read_map_vat_type
*&---------------------------------------------------------------------*
*& Form GET_DISC_AMT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_SAP_DOC_NO
*&      --> PI_BUKRS
*&      --> PI_GJAHR
*&      <-- PO_HEAD_DISC_AMT
*&      <-- PO_HEAD_CHARGE_AMT
*&      <-- PO_HEAD_LINK_DISC_CHARGE
*&---------------------------------------------------------------------*
FORM get_disc_amt  USING    pi_bill        TYPE gty_bill
                            value(pi_module)
                            pi_head        TYPE gty_head
                   CHANGING pi_disc_amt
                            pi_charge_amt
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
                            pi_gross_amt
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
                            pi_link_disc_charge
                            pit_charge     TYPE gtty_doc_dischg
                            pit_messg     TYPE gtty_messg.

  DATA: lwa_doc_dischg TYPE gty_doc_dischg,
        lwa_doc_item   TYPE gty_doc_item,
        lwa_map_dischg TYPE gty_map_dischg,
        lwa_rd_dischg  TYPE gty_rd_dischg.

  DATA: lv_charge_amt     TYPE zsdsde_sap_charge_amt,
        lv_charge_amt_tmp TYPE zsdsde_sap_charge_amt,
        lv_found          TYPE abap_bool.

  LOOP AT gt_doc_dischg INTO lwa_doc_dischg
    WHERE bukrs       = pi_bill-bukrs
      AND sap_doc_no  = pi_bill-sap_doc_no
      AND gjahr       = pi_bill-gjahr
      AND rd_doc_type = pi_bill-rd_doc_type
      AND module_etx  = pi_module.

    lv_found = abap_on.

    lwa_doc_dischg-line_type        = lwa_doc_dischg-line_type.
    lwa_doc_dischg-document_no      = pi_head-document_no.
    lwa_doc_dischg-rd_doc_type      = pi_head-rd_doc_type.
    lwa_doc_dischg-rd_doc_type_desc = pi_head-rd_doc_type_desc.
    lwa_doc_dischg-kunnr            = pi_head-kunnr.
    lwa_doc_dischg-kunnr_name       = pi_head-kunnr_name.
    lwa_doc_dischg-sap_curr         = pi_head-sap_curr.

    CLEAR: lv_charge_amt,lv_charge_amt_tmp.
    lv_charge_amt     = lwa_doc_dischg-charge_amt.
    lv_charge_amt_tmp = lwa_doc_dischg-charge_amt.

    IF lwa_doc_dischg-line_type = 'DC'.

      CLEAR lwa_doc_item.
      READ TABLE gt_doc_item INTO lwa_doc_item
        WITH KEY bukrs      = lwa_doc_dischg-bukrs
                 sap_doc_no = lwa_doc_dischg-sap_doc_no
                 gjahr      = lwa_doc_dischg-gjahr
                 rd_doc_type = lwa_doc_dischg-rd_doc_type
                 module_etx  = lwa_doc_dischg-module_etx
                 item_no     = lwa_doc_dischg-item_no.
      IF sy-subrc EQ 0.
        lv_charge_amt = lwa_doc_dischg-charge_amt / lwa_doc_item-qty.
      ELSE.
        lv_charge_amt = lwa_doc_dischg-charge_amt.
      ENDIF.
    ELSE.
      lv_charge_amt = lwa_doc_dischg-charge_amt.
    ENDIF.

    lwa_doc_dischg-charge_amt     = lv_charge_amt.
    lwa_doc_dischg-charge_amt_alv = lv_charge_amt_tmp.

    IF lwa_doc_dischg-rd_charge_flag EQ 'true'.
      pi_disc_amt = pi_disc_amt + lv_charge_amt.
    ELSE.
      pi_charge_amt = pi_charge_amt + lv_charge_amt.
    ENDIF.

    IF lwa_doc_dischg-sap_srce_val IS NOT INITIAL.
      "RD Charge Reason
      CLEAR lwa_map_dischg.
      CASE lwa_doc_dischg-sap_source.
        WHEN 'COND'.
          READ TABLE gt_map_dischg INTO lwa_map_dischg
            WITH KEY sd_cond = lwa_doc_dischg-sap_srce_val.
          IF sy-subrc = 0.
            lwa_doc_dischg-rd_charge_code = lwa_map_dischg-rd_dischg.
          ENDIF.
        WHEN 'SUBT'.
          READ TABLE gt_map_dischg INTO lwa_map_dischg
            WITH KEY sd_subt = lwa_doc_dischg-sap_srce_val.
          IF sy-subrc = 0.
            lwa_doc_dischg-rd_charge_code = lwa_map_dischg-rd_dischg.
          ENDIF.
        WHEN 'FIGL'.
          READ TABLE gt_map_dischg INTO lwa_map_dischg
            WITH KEY fi_gl = lwa_doc_dischg-sap_srce_val.
          IF sy-subrc = 0.
            lwa_doc_dischg-rd_charge_code = lwa_map_dischg-rd_dischg.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      CLEAR lwa_rd_dischg.
      READ TABLE gt_rd_dischg INTO lwa_rd_dischg
        WITH TABLE KEY rd_dischg = lwa_doc_dischg-rd_charge_code.
      IF sy-subrc = 0.
        lwa_doc_dischg-charge_resn = lwa_rd_dischg-rd_dischg_desc_th.
      ENDIF.

    ENDIF.

    IF lwa_doc_dischg-charge_amt_alv <= 0.
      "Charge or Discount Amount Must Grather than 0
      PERFORM add_messg    USING icon_red_light text-e44
                                 'Charge or Discount Amount'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

    IF lwa_doc_dischg-rd_charge_flag IS INITIAL.
      "Charge flag can not be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Charge flag'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

    IF lwa_doc_dischg-rd_charge_code IS INITIAL.
      "Charge Code can not be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Charge Code'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

    IF lwa_doc_dischg-charge_resn IS INITIAL.
      "Charge Code Description can not be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Charge Code Description'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

    APPEND lwa_doc_dischg TO pit_charge.

  ENDLOOP.

  IF lv_found EQ abap_on.
    pi_link_disc_charge = gc_detail.
  ENDIF.

*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
  pi_gross_amt = pi_gross_amt + pi_disc_amt - pi_charge_amt.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<

ENDFORM.                    "get_disc_amt
*&---------------------------------------------------------------------*
*& Form CHECK_SCOPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LWA_BILL_BUKRS
*&      --> LWA_BILL_FKART
*&      --> LWA_BILL_VKORG
*&      <-- <LFS_HEAD>_ICON
*&      <-- <LFS_HEAD>_MESSG
*&---------------------------------------------------------------------*
FORM check_scope  USING    value(p_module)
                           pi_bukrs TYPE ZSDSFIT014-bukrs
                           pi_fkart TYPE vbrk-fkart
                           pi_vkorg TYPE vbrk-vkorg
                           pi_rfbsk TYPE vbrk-rfbsk
                           pi_vbeln TYPE vbrk-vbeln
                           pi_sel_cb
                           pi_mail_cb
                  CHANGING pit_messg TYPE gtty_messg.

  DATA: lwa_map_doc_sd TYPE gty_map_doc_sd,
        lwa_map_doc_fi TYPE gty_map_doc_fi.

  DATA: ls_param LIKE LINE OF gt_param.       "INS: <ETAX002_02>

  CASE p_module.
    WHEN 'SD'.

      IF pi_vbeln(1) = 'A' OR
           pi_vbeln(1) = 'R' OR
           pi_vbeln(1) = 'C'.
        "CASE A.R.C

        IF lwa_map_doc_sd-rd_flag = 'N' AND
               lwa_map_doc_sd-email_flag = 'N'.

          IF pi_sel_cb EQ 'X'.

            PERFORM add_messg    USING icon_red_light text-e05
                                       '' '' '' ''
                              CHANGING pit_messg.
          ELSEIF pi_sel_cb NE 'X' AND
                 pi_mail_cb EQ 'X'.

            PERFORM add_messg    USING icon_yellow_light text-e19
                                       '' '' '' ''
                              CHANGING pit_messg.
          ENDIF.

        ENDIF.

      ELSE.

        READ TABLE gt_map_doc_sd INTO lwa_map_doc_sd
          WITH KEY vkorg = pi_vkorg
                   fkart = pi_fkart.
        IF sy-subrc NE 0.

          PERFORM add_messg    USING icon_red_light text-e05
                                      '' '' '' ''
                             CHANGING pit_messg.

        ELSEIF lwa_map_doc_sd-rd_flag = 'N' AND
               lwa_map_doc_sd-email_flag = 'N'.

          IF pi_sel_cb EQ 'X'.
            PERFORM add_messg    USING icon_red_light text-e05
                                       '' '' '' ''
                              CHANGING pit_messg.
          ELSEIF pi_sel_cb NE 'X' AND
                 pi_mail_cb EQ 'X'.
            PERFORM add_messg    USING icon_yellow_light text-e19
                                       '' '' '' ''
                              CHANGING pit_messg.

          ENDIF.

        ENDIF.

        IF pi_rfbsk <> 'C'.
          "Check Document Posting Status
          PERFORM add_messg    USING icon_red_light text-e33
                                     '' '' '' ''
                            CHANGING pit_messg.
        ENDIF.

      ENDIF.

    WHEN 'FI'.

      READ TABLE gt_map_doc_fi INTO lwa_map_doc_fi
        WITH KEY bukrs = pi_bukrs
                 blart = pi_fkart.  "BLART
      IF sy-subrc NE 0.
*>>> BEGIN OF INSERTION: <ETAX002_02> on 08.09.2020 <<<

        READ TABLE gt_param INTO ls_param
          WITH KEY id	  = 'ZETX001'
                   name	= 'DOCTYPE_ETAXSCOPE'
                   param_ext = 'FI'
                   low_value = pi_fkart.
        IF sy-subrc EQ 0.

        ELSE.
*>>> END OF INSERTION: <ETAX002_02> on 08.09.2020 <<<
          PERFORM add_messg    USING icon_red_light text-e05
                                     '' '' '' ''
                            CHANGING pit_messg.
        ENDIF.                                                  "INS: <ETAX002_02>

      ELSEIF lwa_map_doc_fi-rd_flag = 'N' AND
             lwa_map_doc_fi-email_flag = 'N'.

        IF pi_sel_cb EQ 'X'.
          PERFORM add_messg    USING icon_red_light text-e05
                                     '' '' '' ''
                            CHANGING pit_messg.
        ELSEIF pi_sel_cb NE 'X' AND
               pi_mail_cb EQ 'X'.
          PERFORM add_messg    USING icon_yellow_light text-e19
                                     '' '' '' ''
                            CHANGING pit_messg.

        ENDIF.

      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "check_scope
*&---------------------------------------------------------------------*
*& Form GET_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_BILL_SAP_DOC_NO
*&      --> LWA_BILL_BUKRS
*&      --> LWA_BILL_GJAHR
*&      <-- <LFS_HEAD>_IT_PARTNER
*&---------------------------------------------------------------------*
FORM get_partner  USING    pi_bill       TYPE gty_bill
                           value(pi_module)
                           pi_head        TYPE gty_head
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
                           value(pi_sch_id)
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
                  CHANGING pit_partner   TYPE gtty_doc_partner
                           pit_messg     TYPE gtty_messg.

  DATA: lwa_partner_tmp  TYPE gty_doc_partner,
        lwa_partner      TYPE gty_doc_partner,
        lwa_rd_partner   TYPE gty_rd_partner,
        lwa_map_sch_ctry TYPE gty_map_sch_ctry,
        lwa_company_addr TYPE gty_company_addr,
        lwa_rd_sub_dist  TYPE gty_rd_sub_dist,
        lwa_rd_district  TYPE gty_rd_district,
        lwa_rd_province  TYPE gty_rd_province.

  DATA: lv_len TYPE i.

  LOOP AT gt_doc_partner INTO lwa_partner_tmp
    WHERE bukrs = pi_bill-bukrs
      AND sap_doc_no = pi_bill-sap_doc_no
      AND gjahr = pi_bill-gjahr
      AND rd_doc_type = pi_bill-rd_doc_type
      AND module_etx  = pi_module.

    CLEAR lwa_partner.
    MOVE-CORRESPONDING lwa_partner_tmp TO lwa_partner.
    lwa_partner-bukrs             = lwa_partner_tmp-bukrs.
    lwa_partner-sap_doc_no        = lwa_partner_tmp-sap_doc_no.
    lwa_partner-gjahr             = lwa_partner_tmp-gjahr.
    lwa_partner-sap_doc_no        = pi_bill-sap_doc_no.
    lwa_partner-module_etx        = lwa_partner_tmp-module_etx.
    lwa_partner-document_no       = pi_head-document_no.
    lwa_partner-rd_doc_type       = pi_head-rd_doc_type.
    lwa_partner-rd_doc_type_desc  = pi_head-rd_doc_type_desc.
**    lwa_partner-rd_partner        = lwa_partner_tmp-rd_partner.
    lwa_partner-rd_partner_desc   = lwa_partner_tmp-rd_partner_desc.
    lwa_partner-kunnr             = lwa_partner_tmp-kunnr.
    lwa_partner-partner_name      = lwa_partner_tmp-partner_name.

    IF lwa_partner-rd_partner EQ 'SELL' OR
       lwa_partner-rd_partner EQ 'BUYR' OR
       lwa_partner-rd_partner EQ 'SHTO'.

      IF lwa_partner-postal IS INITIAL.
        "&RD_PARTNER Postal code cannot be blank
        PERFORM add_messg    USING icon_red_light text-e43
                                   lwa_partner-rd_partner
                                   'Postal code' '' ''
                          CHANGING pit_messg.
      ENDIF.

    ENDIF.


    READ TABLE gt_rd_partner INTO lwa_rd_partner
     WITH TABLE KEY rd_partner = lwa_partner-rd_partner.

    IF sy-subrc EQ 0.
      lwa_partner-rd_partner_desc  = lwa_rd_partner-rd_partner_desc.

      IF lwa_rd_partner-rd_partner_type = 'CUST'.
*>>> BEGIN OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
*        lwa_partner-partner_sch_id  = lwa_partner_tmp-partner_sch_id.
        lwa_partner-partner_sch_id  = pi_sch_id.
*>>> END OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
        lwa_partner-global_id       = lwa_partner_tmp-global_id.
        IF pi_sch_id EQ 'OTHR'.
          lwa_partner-tax_id        = 'N/A'.
        ELSE.
          lwa_partner-tax_id          = lwa_partner_tmp-tax_id.
        ENDIF.
        lwa_partner-contact_name    = lwa_partner_tmp-contact_name.
        lwa_partner-contact_dept    = lwa_partner_tmp-contact_dept.
        lwa_partner-postal          = lwa_partner_tmp-postal.
        lwa_partner-home_no         = lwa_partner_tmp-home_no.
        lwa_partner-addr_line1      = lwa_partner_tmp-addr_line1.
        lwa_partner-addr_line2      = lwa_partner_tmp-addr_line2.
        lwa_partner-sub_dist        = lwa_partner_tmp-sub_dist.
        lwa_partner-sub_dist_desc   = lwa_partner_tmp-sub_dist_desc.
        lwa_partner-district        = lwa_partner_tmp-district.
        lwa_partner-district_desc   = lwa_partner_tmp-district_desc.
        lwa_partner-province_code   = lwa_partner_tmp-province_code.
        lwa_partner-province_desc   = lwa_partner_tmp-province_desc.
        lwa_partner-country         = lwa_partner_tmp-country.
        lwa_partner-addr_no         = lwa_partner_tmp-addr_no.

        CLEAR lwa_map_sch_ctry.
        READ TABLE gt_map_sch_ctry INTO lwa_map_sch_ctry
         WITH TABLE KEY cntry_code = lwa_partner-country.
        IF sy-subrc EQ 0.
          lwa_partner-country_sch_id = lwa_map_sch_ctry-rd_cntry_code.
        ELSE.

          "No Country Scheme ID for & COUNTRY
          PERFORM add_messg    USING icon_red_light text-e06
                                     lwa_partner-country '' '' ''
                            CHANGING pit_messg.
        ENDIF.

        IF lwa_partner-addr_line1 IS INITIAL.
          "&RD_PARTNER Address Line 1 can not be blank
          PERFORM add_messg    USING icon_red_light text-e43
                                     lwa_partner-rd_partner
                                     'Address Line 1' '' ''
                            CHANGING pit_messg.
        ENDIF.

        IF lwa_partner-addr_line2 IS INITIAL.
          "&RD_PARTNER Address Line 2 can not be blank
          PERFORM add_messg    USING icon_red_light text-e43
                                     lwa_partner-rd_partner
                                     'Address Line 2' '' ''
                            CHANGING pit_messg.
        ENDIF.

      ELSEIF lwa_rd_partner-rd_partner_type = 'COMP'.

        CLEAR lwa_company_addr.
        READ TABLE gt_company_addr INTO lwa_company_addr
          WITH TABLE KEY bukrs = lwa_partner_tmp-bukrs
                         bupla = lwa_partner_tmp-partner_branch.
        IF sy-subrc EQ 0.
          lwa_partner-partner_sch_id  = lwa_company_addr-sch_id.
          lwa_partner-tax_id          = lwa_partner_tmp-tax_id.
          lwa_partner-postal          = lwa_partner_tmp-postal.
          lwa_partner-home_no         = lwa_partner_tmp-home_no.
          lwa_partner-sub_dist        = lwa_company_addr-sub_dist_code.
          lwa_partner-country         = lwa_company_addr-country.
*          lwa_partner-country_sch_id  = lwa_company_addr-sch_id.
          lwa_partner-sub_dist_desc   = lwa_partner_tmp-sub_dist_desc.

          IF lwa_company_addr-sub_dist_code IS NOT INITIAL.
            lwa_partner-district      = lwa_company_addr-sub_dist_code(4).
            lwa_partner-province_code = lwa_company_addr-sub_dist_code(2).
          ENDIF.

          IF lwa_partner-province_code IS INITIAL.
            "&RD_PARTNER Province can not be blank
            PERFORM add_messg    USING icon_red_light text-e43
                                       lwa_partner-rd_partner
                                       'Province' '' ''
                              CHANGING pit_messg.

          ENDIF.

          IF lwa_partner_tmp-postal <> lwa_company_addr-postal.
            "Company Postal is inconsistency &1 and &2
            PERFORM add_messg    USING icon_yellow_light text-e08
                                       lwa_partner_tmp-postal lwa_company_addr-postal '' ''
                              CHANGING pit_messg.
          ENDIF.

          IF lwa_partner_tmp-home_no <> lwa_company_addr-home_no.
            "Company Home Addr No.is inconsistency &1 and &2
            PERFORM add_messg    USING icon_yellow_light text-e09
                                       lwa_partner_tmp-home_no lwa_company_addr-home_no '' ''
                              CHANGING pit_messg.
          ENDIF.

          CLEAR lwa_rd_sub_dist.
          READ TABLE gt_rd_sub_dist INTO lwa_rd_sub_dist
            WITH TABLE KEY sub_dist_code = lwa_company_addr-sub_dist_code.
*          IF sy-subrc EQ 0.
          IF lwa_rd_sub_dist-sub_dist_name <> lwa_partner_tmp-sub_dist_desc.

            "Sub district is inconsistency &1 and &2
            PERFORM add_messg    USING icon_yellow_light text-e10
                                       lwa_rd_sub_dist-sub_dist_name
                                       lwa_partner_tmp-sub_dist_desc '' ''
                              CHANGING pit_messg.
          ENDIF.

          CLEAR lwa_rd_district.
          READ TABLE gt_rd_district INTO lwa_rd_district
           WITH TABLE KEY dist_code = lwa_partner-district.
          IF lwa_rd_district-dist_name <> lwa_partner_tmp-district_desc.

            "District is inconsistency &1 and &2
            PERFORM add_messg    USING icon_yellow_light text-e11
                                       lwa_rd_district-dist_name
                                       lwa_partner_tmp-district_desc '' ''
                              CHANGING pit_messg.
          ENDIF.

          CLEAR lwa_rd_province.
          READ TABLE gt_rd_province INTO lwa_rd_province
           WITH TABLE KEY province_code = lwa_partner-province_code.
          IF lwa_rd_province-province_name <> lwa_partner_tmp-province_desc.
            "Province is inconsistency &1 and &2
            PERFORM add_messg    USING icon_yellow_light text-e12
                                       lwa_rd_province-province_name
                                       lwa_partner_tmp-province_desc '' ''
                              CHANGING pit_messg.
          ENDIF.

          IF lwa_company_addr-country <> lwa_partner_tmp-country.

            "Company Country is inconsistency &1 and &2
            PERFORM add_messg    USING icon_yellow_light text-e13
                                       lwa_company_addr-country
                                       lwa_partner_tmp-country '' ''
                              CHANGING pit_messg.
          ELSE.

            CLEAR lwa_map_sch_ctry.
            READ TABLE gt_map_sch_ctry INTO lwa_map_sch_ctry
             WITH TABLE KEY cntry_code = lwa_company_addr-country.
            IF sy-subrc EQ 0.
              lwa_partner-country_sch_id = lwa_map_sch_ctry-rd_cntry_code.
            ELSE.

              "No Country Scheme ID for & COUNTRY
              PERFORM add_messg    USING icon_red_light text-e06
                                         lwa_partner-country '' '' ''
                                CHANGING pit_messg.
            ENDIF.

          ENDIF.

        ELSE.
          "No Company data for company &1 and branch &2
          PERFORM add_messg    USING icon_red_light text-e07
                                     lwa_partner_tmp-bukrs
                                     lwa_partner_tmp-partner_branch '' ''
                            CHANGING pit_messg.
        ENDIF.

        IF lwa_partner-district IS INITIAL.
          "&RD_PARTNER District can not be blank
          PERFORM add_messg    USING icon_red_light text-e43
                                     lwa_partner-rd_partner
                                     'District' '' ''
                            CHANGING pit_messg.
        ENDIF.

        IF lwa_partner-sub_dist IS INITIAL.
          "&RD_PARTNER Sub District can not be blank
          PERFORM add_messg    USING icon_red_light text-e43
                                     lwa_partner-rd_partner
                                     'Sub District' '' ''
                            CHANGING pit_messg.
        ENDIF.

        IF lwa_partner-home_no IS INITIAL.
          "&RD_PARTNER Home no. can not be blank
          PERFORM add_messg    USING icon_red_light text-e43
                                     lwa_partner-rd_partner
                                     'Home no.' '' ''
                            CHANGING pit_messg.
        ENDIF.

      ENDIF.

    ENDIF.

    IF lwa_partner-rd_partner EQ 'SELL' OR
       lwa_partner-rd_partner EQ 'BUYR'.

      IF lwa_partner-partner_sch_id IS INITIAL.
        "&RD_PARTNER Schema ID cannot be blank
        PERFORM add_messg    USING icon_red_light text-e43
                                   lwa_partner-rd_partner
                                   'Schema ID' '' ''
                          CHANGING pit_messg.

      ELSE.

        CLEAR lv_len.
        lv_len = STRLEN( lwa_partner-tax_id ).

        IF lwa_partner-partner_sch_id EQ 'TXID'.
          IF lv_len NE 18.
            "&RD_PARTNER TAX ID must be 18 digits
            PERFORM add_messg    USING icon_red_light text-e42
                                       lwa_partner-rd_partner 'TAX ID' '18' ''
                              CHANGING pit_messg.
          ENDIF.
        ELSEIF lwa_partner-partner_sch_id EQ 'NIDN'.
          IF lv_len NE 13.
            PERFORM add_messg    USING icon_red_light text-e42
                                       lwa_partner-rd_partner 'TAX ID' '13' ''
                              CHANGING pit_messg.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF lwa_partner-country IS INITIAL.
      "&RD_PARTNER Country can not be blank
      PERFORM add_messg    USING icon_red_light text-e43
                                 lwa_partner-rd_partner
                                 'Country' '' ''
                        CHANGING pit_messg.
    ENDIF.

    APPEND lwa_partner TO pit_partner.
  ENDLOOP.

ENDFORM.                    "get_partner
*&---------------------------------------------------------------------*
*& Form ADD_MESSG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ICON_RED_LIGHT
*&      --> TEXT_E05
*&      <-- PIT_MESSG
*&---------------------------------------------------------------------*
FORM add_messg  USING    pi_icon TYPE icon_d
                         pi_text
                         value(pi_v1)
                         value(pi_v2)
                         value(pi_v3)
                         value(pi_v4)
                CHANGING pt_messg TYPE gtty_messg.

  DATA: lv_lines TYPE i,
        lv_text  TYPE c LENGTH 255.

  FIELD-SYMBOLS: <lfs_messg> TYPE gty_messg.

  DESCRIBE TABLE pt_messg LINES lv_lines.

  ADD 1 TO lv_lines.

  APPEND INITIAL LINE TO pt_messg ASSIGNING <lfs_messg>.
  <lfs_messg>-seq   = lv_lines.
  <lfs_messg>-icon  = pi_icon.

  lv_text = pi_text.

  IF pi_v1 IS NOT INITIAL.
    REPLACE '&1' IN lv_text WITH pi_v1.
  ENDIF.

  IF pi_v2 IS NOT INITIAL.
    REPLACE '&2' IN lv_text WITH pi_v2.
  ENDIF.

  IF pi_v3 IS NOT INITIAL.
    REPLACE '&3' IN lv_text WITH pi_v3.
  ENDIF.

  IF pi_v4 IS NOT INITIAL.
    REPLACE '&4' IN lv_text WITH pi_v4.
  ENDIF.

  <lfs_messg>-messg = lv_text.

ENDFORM.                    "add_messg
*&---------------------------------------------------------------------*
*& Form READ_MESSG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LFS_HEAD>_IT_MESSG
*&      <-- <LFS_HEAD>_ICON
*&      <-- <LFS_HEAD>_MESSG
*&---------------------------------------------------------------------*
FORM read_messg  TABLES   pit_messg TYPE gtty_messg
                 CHANGING pi_icon TYPE icon_d
                          pi_messg.

  DATA lwa_messg TYPE gty_messg.

  DATA lv_found TYPE abap_bool.

  LOOP AT pit_messg INTO lwa_messg
    WHERE icon = icon_red_light.
    CLEAR lv_found.
    lv_found = abap_on.
    pi_icon  = lwa_messg-icon.
    pi_messg = lwa_messg-messg.
    EXIT.
  ENDLOOP.
  IF lv_found <> abap_on.
    CLEAR lwa_messg.
    LOOP AT pit_messg INTO lwa_messg
      WHERE icon = icon_yellow_light.
      pi_icon  = lwa_messg-icon.
      pi_messg = lwa_messg-messg.
      EXIT.
    ENDLOOP.
  ENDIF.

  ADD 1 TO gv_count.

  IF pi_icon = icon_red_light.
    ADD 1 TO gv_count_error.
  ELSE.
    ADD 1 TO gv_count_success.
  ENDIF.

ENDFORM.                    "read_messg
*&---------------------------------------------------------------------*
*& Form GET_H_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_BILL_SAP_DOC_NO
*&      --> LWA_BILL_BUKRS
*&      --> LWA_BILL_GJAHR
*&      --> <LFS_HEAD>
*&      --> LV_MODULE
*&      <-- <LFS_HEAD>_IT_H_REF
*&      <-- <LFS_HEAD>_IT_MESSG
*&---------------------------------------------------------------------*
FORM get_h_ref  USING    pi_bill        TYPE gty_bill
                         value(pi_module)
                         pi_head        TYPE gty_head
                CHANGING pit_h_ref      TYPE gtty_doc_h_ref
                         pit_messg      TYPE gtty_messg.

  DATA: lwa_doc_h_ref   TYPE gty_doc_h_ref,
        lwa_ref_doc     TYPE gty_vbrk,
        lwa_map_doc_sd  TYPE gty_map_doc_sd,
        lwa_map_doc_fi  TYPE gty_map_doc_fi,
        lwa_rd_doc_type TYPE gty_rd_doc_type,
        lwa_param       LIKE LINE OF gt_param,
        lwa_doc_header  TYPE ZSDSFIT014,                "INS CH03
        lt_doc_header   TYPE TABLE OF ZSDSFIT014.       "INS CH03

  DATA: lv_dat1 TYPE c LENGTH 10,
        lv_dat2 TYPE c LENGTH 10,
        lv_cndn TYPE sap_bool.          "INS: <ETAX001>

  LOOP AT gt_doc_h_ref INTO lwa_doc_h_ref
    WHERE bukrs       = pi_bill-bukrs
      AND sap_doc_no  = pi_bill-sap_doc_no
      AND gjahr       = pi_bill-gjahr
      AND rd_doc_type = pi_bill-rd_doc_type
      AND module_etx  = pi_module.

    lwa_doc_h_ref-document_no      = pi_head-document_no.
    lwa_doc_h_ref-rd_doc_type      = pi_head-rd_doc_type.
    lwa_doc_h_ref-rd_doc_type_desc = pi_head-rd_doc_type_desc.
    lwa_doc_h_ref-kunnr            = pi_head-kunnr.
    lwa_doc_h_ref-kunnr_name       = pi_head-kunnr_name.

    "lwa_doc_h_ref-ref_doc_no
    "lwa_doc_h_ref-ref_sap_doc_no
    "lwa_doc_h_ref-ref_bukrs

    CLEAR: lwa_ref_doc,
           lv_dat1,
           lv_dat2.

*>>> BOD CH03 >>>
*    CHECK lwa_doc_h_ref-ref_sap_doc_no IS NOT INITIAL.
*
*    "gt_ref_doc -> Get from VBRK & BKPF
*    IF pi_module EQ 'FI'.
*      READ TABLE gt_ref_doc INTO lwa_ref_doc
*        WITH KEY vbeln  = lwa_doc_h_ref-ref_sap_doc_no
*                 bukrs  = lwa_doc_h_ref-ref_bukrs
*                 gjahr  = lwa_doc_h_ref-ref_gjahr
*                 module = pi_module.
*    ELSEIF pi_module EQ 'SD'.
*      READ TABLE gt_ref_doc INTO lwa_ref_doc
*        WITH KEY vbeln  = lwa_doc_h_ref-ref_sap_doc_no
*                 bukrs  = lwa_doc_h_ref-ref_bukrs
**                 gjahr  = lwa_doc_h_ref-ref_gjahr        "SD ไม่ต้องอ่าน gjahr
*                 module = pi_module.
*    ENDIF.
*
**    IF sy-subrc = 0.
*    IF lwa_ref_doc IS NOT INITIAL.
*      lwa_doc_h_ref-ref_sap_doc_type  = lwa_ref_doc-fkart.
*
**      IF lwa_ref_doc-fkdat <> lwa_doc_h_ref-ref_sap_post_date.
*      IF lwa_ref_doc-fkdat <> lwa_doc_h_ref-ref_sap_post_dat.
*
*        IF lwa_ref_doc-fkdat IS NOT INITIAL.
*          WRITE lwa_ref_doc-fkdat TO lv_dat1
*          USING EDIT MASK '__.__.____'.
*        ENDIF.
**        IF lwa_doc_h_ref-ref_sap_post_date IS NOT INITIAL.
*        IF lwa_doc_h_ref-ref_sap_post_dat IS NOT INITIAL.
**          WRITE lwa_doc_h_ref-ref_sap_post_date TO lv_dat2
*          WRITE lwa_doc_h_ref-ref_sap_post_dat TO lv_dat2
*          USING EDIT MASK '__.__.____'.
*        ENDIF.
*
*        "Inconsistency Ref. doc date &1 and &2
*        PERFORM add_messg    USING icon_yellow_light text-e14
*                                   lv_dat1
*                                   lv_dat2
*                                   ''
*                                   ''
*                          CHANGING pit_messg.
*      ELSE.
**        lwa_doc_h_ref-ref_sap_post_date = lwa_ref_doc-fkdat.
*        lwa_doc_h_ref-ref_sap_post_dat = lwa_ref_doc-fkdat.
*      ENDIF.
*
*      IF pi_module  = 'SD'.
*        "Case SD ใบลดหนี้ (RD doc type = ‘80’ or ‘81’)
*        IF lwa_doc_h_ref-rd_doc_type = '80' OR
*           lwa_doc_h_ref-rd_doc_type = '81'.
*          "lwa_doc_h_ref-ref_gjahr
*        ELSEIF pi_head-replace_flag = 'Y'.
**          lwa_doc_h_ref-ref_gjahr = lwa_doc_h_ref-ref_sap_post_date(4).
*          lwa_doc_h_ref-ref_gjahr = lwa_doc_h_ref-ref_sap_post_dat(4).
*        ENDIF.
*      ELSEIF pi_module = 'FI'.
*        "lwa_doc_h_ref-ref_gjahr
*      ENDIF.
*
*      "1st priority
*
*      "2nd priority
*      IF lwa_doc_h_ref-ref_rd_doc_type IS INITIAL.
**        2.1) SD
*        IF pi_module = 'SD'.
*          CLEAR lwa_map_doc_sd.
*          READ TABLE gt_map_doc_sd INTO lwa_map_doc_sd
*            WITH KEY vkorg    = pi_bill-vkorg
*                     fkart    = lwa_doc_h_ref-ref_sap_doc_type
*                     tax_code = pi_bill-mwsk1.
*          IF sy-subrc = 0.
*            lwa_doc_h_ref-ref_rd_doc_type = lwa_map_doc_sd-rd_doc_type.
*          ENDIF.
**        2.2) FI
*        ELSEIF pi_module = 'FI'.
*          CLEAR lwa_map_doc_fi.
*          READ TABLE gt_map_doc_fi INTO lwa_map_doc_fi
*            WITH KEY bukrs    = lwa_doc_h_ref-ref_bukrs
*                     blart    = lwa_doc_h_ref-ref_sap_doc_type
*                     tax_code = pi_bill-mwsk1.         "BSEG-MWSKZ  ** Priority O7
*          IF sy-subrc = 0.
*            lwa_doc_h_ref-ref_rd_doc_type = lwa_map_doc_fi-rd_doc_type.
*          ELSE.
**>>> BEGIN OF INSERTION: <ETAX001> on 10.09.2020 <<<
*            "กรณี map ไม่ได้ --> หา doc ref จาก header ที่สร้างจาก FI
*            SELECT SINGLE rd_doc_type
*              FROM ZSDSFIT014
*              INTO lwa_doc_h_ref-ref_rd_doc_type
*             WHERE bukrs      = lwa_doc_h_ref-ref_bukrs
*               AND sap_doc_no = lwa_doc_h_ref-ref_doc_no
*               AND gjahr      = lwa_doc_h_ref-ref_gjahr
*               AND module_etx = 'FI'
*               AND form_name  = 'ZSF_FI_RECEIPT_INV_A4'.
*            IF sy-subrc NE 0.
*              "กรณี Ref doc SD ไม่สามารถ map ได้ --> ดึงจาก param
*              CLEAR lwa_param.
*              READ TABLE gt_param INTO lwa_param
*               WITH KEY name       = 'CDNFI_REF.SD'
*                        param_ext = lwa_doc_h_ref-ref_sap_doc_type.
*              IF sy-subrc EQ 0.
*                lwa_doc_h_ref-ref_rd_doc_type = lwa_param-low_value.
*              ENDIF.
*            ENDIF.
**>>> END OF INSERTION: <ETAX001> on 10.09.2020 <<<
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      "No RD Doc type for Original Doc
*      IF lwa_doc_h_ref-ref_rd_doc_type IS INITIAL.
*        PERFORM add_messg    USING icon_red_light text-e16
*                                   lwa_doc_h_ref-ref_sap_doc_no
*                                   ''
*                                   ''
*                                   ''
*                          CHANGING pit_messg.
*      ELSE.
*
*        CLEAR lwa_rd_doc_type.
*        READ TABLE gt_rd_doc_type INTO lwa_rd_doc_type
*         WITH TABLE KEY rd_doc_type = lwa_doc_h_ref-ref_rd_doc_type.
*        IF sy-subrc = 0.
*          lwa_doc_h_ref-ref_rd_doc_type_desc = lwa_rd_doc_type-rd_doc_type_desc_th.
*        ENDIF.
*
*      ENDIF.
*
*    ELSE.
*      "Incorrect Original Doc &1
*      PERFORM add_messg    USING icon_red_light text-e15
*                                 lwa_doc_h_ref-ref_sap_doc_no
*                                 ''
*                                 ''
*                                 ''
*                         CHANGING pit_messg.
*    ENDIF.
*<<< EOD CH03 <<<

*>>> BOI CH03 >>>
    SELECT bukrs
           sap_doc_no
           gjahr
           sap_doc_type
           rd_doc_type
      FROM ZSDSFIT014
      INTO CORRESPONDING FIELDS OF TABLE lt_doc_header
     WHERE bukrs      = lwa_doc_h_ref-ref_bukrs
       AND sap_doc_no = lwa_doc_h_ref-ref_doc_no
       AND gjahr      = lwa_doc_h_ref-ref_gjahr
       AND status <> '9'
       AND rd_doc_type <> 'T01'.
    IF sy-subrc = 0.
      IF LINES( lt_doc_header ) > 1.
        PERFORM add_messg    USING icon_yellow_light text-e51
                                   lwa_doc_h_ref-ref_doc_no
                                   ''
                                   ''
                                   ''
                          CHANGING pit_messg.
      ENDIF.

      READ TABLE lt_doc_header INTO lwa_doc_header INDEX 1.
      IF sy-subrc = 0.
        lwa_doc_h_ref-ref_sap_doc_type = lwa_doc_header-sap_doc_type.
        lwa_doc_h_ref-ref_rd_doc_type = lwa_doc_header-rd_doc_type.
      ENDIF.

    ELSE.
*        2.1) SD
      IF pi_module = 'SD'.
        "Set SAP doc type
        READ TABLE gt_ref_doc INTO lwa_ref_doc
          WITH KEY vbeln  = lwa_doc_h_ref-ref_doc_no
                   bukrs  = lwa_doc_h_ref-ref_bukrs
*                   gjahr  = lwa_doc_h_ref-ref_gjahr   "SD ไม่ต้องอ่าน gjahr
                   module = pi_module.
        IF sy-subrc = 0.
          lwa_doc_h_ref-ref_sap_doc_type  = lwa_ref_doc-fkart.
        ELSE.
*          SELECT SINGLE COUNT(*)
*            FROM ztfit_collector
*            WHERE receipt_no = lwa_doc_h_ref-ref_doc_no
*              AND delete_flag = abap_false.
*          IF sy-subrc = 0.
*            lwa_doc_h_ref-ref_sap_doc_type = lwa_doc_h_ref-ref_doc_no(1).
*          ELSE.
*            PERFORM add_messg    USING icon_red_light text-e52
*                                        lwa_doc_h_ref-ref_doc_no
*                                        ''
*                                        ''
*                                        ''
*                               CHANGING pit_messg.
*          ENDIF.
        ENDIF.

        "Set RD doc type
        READ TABLE gt_map_doc_sd INTO lwa_map_doc_sd
          WITH KEY vkorg    = pi_bill-vkorg
                   fkart    = lwa_doc_h_ref-ref_sap_doc_type
                   tax_code = pi_bill-mwsk1.
        IF sy-subrc = 0.
          lwa_doc_h_ref-ref_rd_doc_type = lwa_map_doc_sd-rd_doc_type.
        ELSE.
          IF lwa_doc_h_ref-ref_doc_no(1) = 'A' OR
             lwa_doc_h_ref-ref_doc_no(1) = 'R' OR
             lwa_doc_h_ref-ref_doc_no(1) = 'C'.
            ">>>>>> BEGIN OF MODIFITION: <CH10> on 10.11.2020 13:20:09  <<<<<<
            "Reason:
**            lwa_doc_h_ref-ref_rd_doc_type = 'T01'.
            lwa_doc_h_ref-ref_rd_doc_type = 'T03'.
            "<<<<<< END OF MODIFITION: <CH10> on 10.11.2020 13:20:09  >>>>>>
          ELSE.
            PERFORM add_messg    USING icon_red_light text-e53
                                        lwa_doc_h_ref-ref_doc_no
                                        lwa_doc_h_ref-ref_sap_doc_type
                                        pi_bill-mwsk1
                                        ''
                               CHANGING pit_messg.
          ENDIF.
        ENDIF.

      ELSEIF pi_module = 'FI'.
*        2.2) FI
        "Set SAP doc type
        READ TABLE gt_ref_doc INTO lwa_ref_doc
          WITH KEY vbeln  = lwa_doc_h_ref-ref_sap_doc_no
                   bukrs  = lwa_doc_h_ref-ref_bukrs
                   gjahr  = lwa_doc_h_ref-ref_gjahr
                   module = pi_module.
        IF sy-subrc = 0.
          lwa_doc_h_ref-ref_sap_doc_type  = lwa_ref_doc-fkart.
        ELSE.
          PERFORM add_messg    USING icon_red_light text-e52
                                      lwa_doc_h_ref-ref_doc_no
                                      ''
                                      ''
                                      ''
                             CHANGING pit_messg.
        ENDIF.

        "Set RD doc type
        READ TABLE gt_map_doc_fi INTO lwa_map_doc_fi
          WITH KEY bukrs    = lwa_doc_h_ref-ref_bukrs
                   blart    = lwa_doc_h_ref-ref_sap_doc_type
                   tax_code = pi_bill-mwsk1.         "BSEG-MWSKZ  ** Priority O7
        IF sy-subrc = 0.
          lwa_doc_h_ref-ref_rd_doc_type = lwa_map_doc_fi-rd_doc_type.
        ELSE.
          READ TABLE gt_param INTO lwa_param
            WITH KEY name       = 'CDNFI_REF.SD'
                     param_ext = lwa_doc_h_ref-ref_sap_doc_type.
          IF sy-subrc EQ 0.
            lwa_doc_h_ref-ref_rd_doc_type = lwa_param-low_value.
          ELSE.
            PERFORM add_messg    USING icon_red_light text-e53
                          lwa_doc_h_ref-ref_doc_no
                          lwa_doc_h_ref-ref_sap_doc_type
                          pi_bill-mwsk1
                          ''
                 CHANGING pit_messg.
          ENDIF.
        ENDIF.
      ENDIF.

      "Check posting date
      IF lwa_ref_doc IS NOT INITIAL.
        IF lwa_ref_doc-fkdat <> lwa_doc_h_ref-ref_sap_post_dat.

          IF lwa_ref_doc-fkdat IS NOT INITIAL.
            WRITE lwa_ref_doc-fkdat TO lv_dat1
            USING EDIT MASK '__.__.____'.
          ENDIF.
*        IF lwa_doc_h_ref-ref_sap_post_date IS NOT INITIAL.
          IF lwa_doc_h_ref-ref_sap_post_dat IS NOT INITIAL.
*          WRITE lwa_doc_h_ref-ref_sap_post_date TO lv_dat2
            WRITE lwa_doc_h_ref-ref_sap_post_dat TO lv_dat2
            USING EDIT MASK '__.__.____'.
          ENDIF.

          "Inconsistency Ref. doc date &1 and &2
          PERFORM add_messg    USING icon_yellow_light text-e14
                                     lv_dat1
                                     lv_dat2
                                     ''
                                     ''
                            CHANGING pit_messg.
        ELSE.
*        lwa_doc_h_ref-ref_sap_post_date = lwa_ref_doc-fkdat.
          lwa_doc_h_ref-ref_sap_post_dat = lwa_ref_doc-fkdat.
        ENDIF.
      ENDIF.

    ENDIF.
*<<< EOI CH03 <<<

    APPEND lwa_doc_h_ref TO pit_h_ref.
  ENDLOOP.
*>>> BOI CH07 >>>
  IF sy-subrc <> 0.
    IF pi_head-rd_doc_type EQ '80' OR
       pi_head-rd_doc_type EQ '81'.

      PERFORM add_messg    USING icon_red_light text-e56
                                 '' '' '' ''
                        CHANGING pit_messg.
    ENDIF.
  ENDIF.
*<<< EOI CH07 <<<

*>>> BEGIN OF INSERTION: <ETAX001> on 23.09.2020 <<<
  IF ( pi_head-rd_doc_type EQ '80' OR pi_head-rd_doc_type EQ '81' ).
    lv_cndn = abap_true.
  ELSEIF pi_head-replace_flag EQ 'Y'.
    lv_cndn = abap_true.
  ENDIF.

  IF gt_doc_h_ref[] IS INITIAL AND
     lv_cndn EQ abap_true AND
     pi_module EQ 'FI'.
    "No List of Ref.
    PERFORM add_messg    USING icon_red_light text-e50
                               ''
                               ''
                               ''
                               ''
                       CHANGING pit_messg.
  ENDIF.
*>>> END OF INSERTION: <ETAX001> on 23.09.2020 <<<

ENDFORM.                    "get_h_ref
*&---------------------------------------------------------------------*
*& Form GET_DOC_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_BILL_SAP_DOC_NO
*&      --> LWA_BILL_BUKRS
*&      --> LWA_BILL_GJAHR
*&      --> <LFS_HEAD>
*&      <-- <LFS_HEAD>_IT_ITEM
*&      <-- <LFS_HEAD>_IT_MESSG
*&---------------------------------------------------------------------*
FORM get_doc_item  USING    pi_bill        TYPE gty_bill
                            value(pi_module)
                            pi_head        TYPE gty_head
                   CHANGING pit_item       TYPE gtty_doc_item
                            pit_messg      TYPE gtty_messg.

  DATA: lwa_doc_item     TYPE gty_doc_item,
        lwa_unit         TYPE gty_rd_unit,
        lwa_map_vat_type TYPE gty_map_vat_type.

  CLEAR lwa_doc_item.
  LOOP AT gt_doc_item INTO lwa_doc_item
    WHERE bukrs      = pi_bill-bukrs
      AND sap_doc_no = pi_bill-sap_doc_no
      AND gjahr      = pi_bill-gjahr
      AND rd_doc_type = pi_bill-rd_doc_type
      AND module_etx  = pi_module.

    lwa_doc_item-document_no      = pi_head-document_no.
    lwa_doc_item-rd_doc_type      = pi_head-rd_doc_type.
    lwa_doc_item-rd_doc_type_desc = pi_head-rd_doc_type_desc.
    lwa_doc_item-kunnr            = pi_head-kunnr.
    lwa_doc_item-kunnr_name       = pi_head-kunnr_name.
    lwa_doc_item-link_disc_charge = pi_head-link_disc_charge.

    IF lwa_doc_item-item_no IS INITIAL.
      "Item No cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Item No'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

    IF lwa_doc_item-item_code IS INITIAL AND
       pi_module EQ 'SD'.
      "Item Code cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Item Code'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

    IF lwa_doc_item-item_name IS INITIAL.
      "Item Name cannot be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'Item Name'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.
    ">>>>>> BEGIN OF INSERTION: <CH15> on 21.01.2021 12:56:39 >>>>>>
    "[======= New =======]
    IF lwa_doc_item-qty <= 0.
      PERFORM add_messg    USING icon_red_light text-e44
                                 'Item quantity'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.
    "<<<<<< END OF INSERTION: <CH15> on 21.01.2021 12:56:39  <<<<<<

*    IF lwa_doc_item-vat_amt <= 0.
*      "Vat Amount (Item) Must Grather than 0
*      PERFORM add_messg    USING icon_red_light TEXT-e44
*                                 'Vat Amount (Item)'
*                                 '' '' ''
*                        CHANGING pit_messg.
*    ENDIF.

    CLEAR lwa_unit.
    READ TABLE gt_rd_unit INTO lwa_unit
      WITH KEY sap_unit = lwa_doc_item-unit.
    IF sy-subrc = 0.
      lwa_doc_item-rd_unit = lwa_unit-rd_unit_code.
    ELSE.

      IF lwa_doc_item-unit IS NOT INITIAL.
        "No RD UNIT for &1
        PERFORM add_messg    USING icon_red_light text-e17
                                   lwa_doc_item-unit
                                   ''
                                   ''
                                   ''
                           CHANGING pit_messg.
      ENDIF.
    ENDIF.

    PERFORM read_map_vat_type    USING lwa_doc_item-tax_code
                              CHANGING lwa_doc_item-rd_vat_type
                                       pit_messg.

    IF lwa_doc_item-rd_vat_type IS INITIAL.
      "RD VAT Type (Item) can not be blank
      PERFORM add_messg    USING icon_red_light text-e41
                                 'RD VAT Type (Item)'
                                 '' '' ''
                        CHANGING pit_messg.
    ENDIF.

*>>> ch17: start of insert
    IF rb_trn EQ 'X' AND lwa_doc_item-module_etx EQ 'SD'.
      IF ( gr_po_kunnr[] IS NOT INITIAL AND lwa_doc_item-kunnr IN gr_po_kunnr ) AND
         ( gr_dv_doctype[] IS NOT INITIAL AND pi_head-rd_doc_type IN gr_dv_doctype ) .

        "*****************************************************
        " Required field PO no in ZSDSFIT014
        "*****************************************************
        IF pi_head-sap_po_no IS INITIAL .
          PERFORM add_messg    USING icon_red_light text-e62
                                     lwa_doc_item-item_no '' '' ''
                            CHANGING pit_messg.

          "*****************************************************
          " Required field PO no and item in ZSDSFIT015
          "*****************************************************
        ELSEIF lwa_doc_item-sap_po_no IS INITIAL .
          PERFORM add_messg    USING icon_red_light text-e58
                                     lwa_doc_item-item_no '' '' ''
                            CHANGING pit_messg.
        ELSEIF lwa_doc_item-sap_po_item IS INITIAL .
          PERFORM add_messg    USING icon_red_light text-e59
                                     lwa_doc_item-item_no '' '' ''
                            CHANGING pit_messg.
        ELSE.
          DATA: lv_len TYPE i .
          lv_len = STRLEN( lwa_doc_item-sap_po_no ).
          IF lv_len NE 10  .
            PERFORM add_messg USING icon_red_light text-e61
                                    lwa_doc_item-item_no '' '' ''
                           CHANGING pit_messg.
          ELSE.
            CLEAR: lv_len .
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = lwa_doc_item-sap_po_item
              IMPORTING
                output = lwa_doc_item-sap_po_item.
            lv_len = STRLEN( lwa_doc_item-sap_po_item ).
            IF lv_len GT 5  .
              PERFORM add_messg USING icon_red_light text-e63
                                      lwa_doc_item-item_no '' '' ''
                             CHANGING pit_messg.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
*<<< CH17: End of Insert

    APPEND lwa_doc_item TO pit_item.
  ENDLOOP.
*>>> BOI CH07 >>>
  IF sy-subrc <> 0.
    PERFORM add_messg    USING icon_red_light text-e55
                               '' '' '' ''
                      CHANGING pit_messg.
  ENDIF.
*<<< EOI CH07 <<<



ENDFORM.                    "get_doc_item
*&---------------------------------------------------------------------*
*&      form  conversion_exit
*&---------------------------------------------------------------------*
FORM conversion_exit USING value(p_exit)
                           value(p_type)
                           value(p_input)
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
*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
*&---------------------------------------------------------------------*
*& Form CHECK_COLLECTOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEADER
*&      <-- PO_HEAD_IT_MESSG
*&---------------------------------------------------------------------*
FORM check_collector USING    ppi_header TYPE gty_doc_header
                              value(ppi_module)
                     CHANGING pit_messg  TYPE gtty_messg.

  DATA: "lwa_collector TYPE gty_collector,
        lv_is_cancelled TYPE flag,
        ls_sd_bseg LIKE LINE OF gt_sd_bseg,
        lv_valid_collect TYPE flag.                         "INS CH02

  DATA: lv_pass TYPE c.

*2.5nd priority (For SDS)
*IF ZSDSFIT014-RD_DOC_TYPE = ‘T01’ or ‘T03’
*AND ZSDSFIT014-NET_AMOUNT_BF_VAT <> 0
*--ใบเสร็จรับเงินเป็น 0 สามารถส่งได้เลยไม่ต้องเช็ค collector
  CHECK ( ppi_header-rd_doc_type EQ 'T01' OR
          ppi_header-rd_doc_type EQ 'T03' ) AND
          ppi_header-net_amt_bf_vat IS NOT INITIAL.
  ">>>>>> BEGIN OF INSERTION: <CH14> on 13.01.2021 16:22:38 >>>>>>
**  IF ppi_module = 'SD'.
  LOOP AT gt_sd_bseg INTO ls_sd_bseg
    WHERE bukrs = ppi_header-bukrs
    AND belnr =   ppi_header-sap_doc_no
    AND gjahr =   ppi_header-gjahr
    AND koart = 'D'
    AND augbl <> space
    AND augdt <> space.
    EXIT. "เจอก็จะไม่เช็ค collector
  ENDLOOP.
  IF sy-subrc = 0.
    EXIT. "get out not check collector
  ENDIF.
**  ENDIF.
  "<<<<<< END OF INSERTION: <CH14> on 13.01.2021 16:22:38  <<<<<<
  CASE ppi_header-form_name.
    WHEN 'ZSF_SD_RECEIPT_NEW_A4' OR "010
         'ZSF_FI_RECEIPT_A4'.       "002

*>>> BOI CH04 >>>
      IF ppi_header-form_name = 'ZSF_SD_RECEIPT_NEW_A4' AND "010
         ppi_header-pay_term = 'A000'.

        EXIT.
      ENDIF.
*<<< EOI CH04 <<<

*>>> BOM CH02 >>>
*      READ TABLE gt_collector INTO lwa_collector
*       WITH KEY bukrs = ppi_header-bukrs
*                belnr = ppi_header-document_no
*                status = '02'.
*      IF sy-subrc EQ 0 AND
*        ( lwa_collector-action_type EQ '02' OR
*          lwa_collector-action_type EQ '11' ).
*      ELSE.

      CLEAR lv_valid_collect.

*      LOOP AT gt_collector INTO lwa_collector
*        WHERE bukrs = ppi_header-bukrs
*          AND belnr = ppi_header-document_no
***          AND gjahr = ppi_header-gjahr                "INS CH06 "P'JIBBIZ cancel it by 04.11.2020
*          AND delete_flag = abap_false
*          AND status = '02'
*          AND ( action_type = '02' OR action_type = '11' ).
*
*        lv_valid_collect = abap_true.
*        EXIT.
*      ENDLOOP.

      IF lv_valid_collect = abap_true.
        "Do nothing
      ELSE.
*<<< EOM CH02 <<<

        IF ppi_module EQ 'FI'.

          PERFORM add_messg    USING icon_red_light text-e45
                                     '' '' '' ''
                            CHANGING pit_messg.

        ELSE.
*        "**เคสใบเสร็จ(โยกเงินมัดจำมาตัดใบแจ้งหนี้ )
          PERFORM check_dg    USING ppi_header
                                    ppi_module
                           CHANGING lv_pass.

          IF lv_pass NE 'X'.

            PERFORM add_messg    USING icon_red_light text-e45
                                       '' '' '' ''
                              CHANGING pit_messg.
          ENDIF.

        ENDIF.

      ENDIF.

    WHEN 'ZSF_FI_RECEIPT_NEW_A4'.   "011
*>>> BOI CH04 >>>
      IF ppi_header-pay_term = 'A000'.
        EXIT.
      ENDIF.
*<<< EOI CH04 <<<

*>>> BOM CH07 >>>
*      READ TABLE gt_collector INTO lwa_collector
*       WITH KEY bukrs = ppi_header-bukrs
*                receipt_no = ppi_header-document_no
**                gjahr = ppi_header-gjahr   "INS CH06 "P'JIBBIZ cancel it by 04.11.2020
*                status = '02'.
*      IF sy-subrc EQ 0 AND
*        ( lwa_collector-action_type EQ '02' OR
*          lwa_collector-action_type EQ '11' ).

      ">>>>>> BEGIN OF INSERTION: <CH09> on 04.11.2020 13:10:34 >>>>>>
      SELECT SINGLE fksto
        INTO lv_is_cancelled
        FROM ZSDSFIT021
        WHERE xblnr = ppi_header-document_no.
**        vbeln = ''
      IF lv_is_cancelled IS NOT INITIAL.
        PERFORM add_messg    USING icon_red_light text-e54
                                     '' '' '' ''
                          CHANGING pit_messg.
        CLEAR lv_is_cancelled.
        EXIT.
      ENDIF.


      "<<<<<< END OF INSERTION: <CH09> on 04.11.2020 13:10:34  <<<<<<

      CLEAR lv_valid_collect.

*      LOOP AT gt_collector INTO lwa_collector
*        WHERE bukrs = ppi_header-bukrs
*          AND receipt_no = ppi_header-document_no
***          AND gjahr = ppi_header-gjahr                      "INS CH06 "P'JIBBIZ cancel it by 04.11.2020
*          AND status = '02'
*          AND ( action_type = '02' OR action_type = '11' )
*          AND delete_flag <> 'X'.                             "INS CH16 by 03.02.2021
*
*        lv_valid_collect = abap_true.
*        EXIT.
*      ENDLOOP.
      IF lv_valid_collect = abap_true.
**        IF lwa_collector-delete_flag = abap_true.           "DEL CH16 by 03.02.2021
***          PERFORM add_messg    USING icon_red_light text-e54 "DEL CH16 by 10.02.2021
***                                       '' '' '' ''           "DEL CH16 by 10.02.2021
***                            CHANGING pit_messg.              "DEL CH16 by 10.02.2021
**        ENDIF.
*<<< EOM CH07 <<<

      ELSE.

        IF ppi_module EQ 'FI'.

          PERFORM add_messg    USING icon_red_light text-e45
                                     '' '' '' ''
                            CHANGING pit_messg.

        ELSE.
*        "**เคสใบเสร็จ(โยกเงินมัดจำมาตัดใบแจ้งหนี้ )
          PERFORM check_dg    USING ppi_header
                                    ppi_module
                           CHANGING lv_pass.

          IF lv_pass NE 'X'.

            PERFORM add_messg    USING icon_red_light text-e45
                                       '' '' '' ''
                              CHANGING pit_messg.
          ENDIF.

        ENDIF.
      ENDIF.

    WHEN 'ZSF_FI_RECEIPT_INV_A4'.   "001

*>>> BOM CH07 >>>
*      READ TABLE gt_collector INTO lwa_collector
*       WITH KEY bukrs = ppi_header-bukrs
*                belnr = ppi_header-document_no
*                gjahr = ppi_header-gjahr   "INS CH06
*                delete_flag = abap_false
*                status = '02'.
*      IF sy-subrc EQ 0 AND
*        ( lwa_collector-action_type EQ '02' OR
*          lwa_collector-action_type EQ '11' ).

      CLEAR lv_valid_collect.

*      LOOP AT gt_collector INTO lwa_collector
*        WHERE bukrs = ppi_header-bukrs
*          AND belnr = ppi_header-document_no
***          AND gjahr = ppi_header-gjahr                   "INS CH06 "P'JIBBIZ cancel it by 04.11.2020
***          AND delete_flag = abap_false
*          AND delete_flag <> 'X'                           "INS CH16 by 03.02.2021
*          AND status = '02'
*          AND ( action_type = '02' OR action_type = '11' ).
*
*        lv_valid_collect = abap_true.
*        EXIT.
*      ENDLOOP.

      IF lv_valid_collect = abap_true.
        "Do nothing
      ELSE.
*<<< BOM CH07 <<<
        IF ppi_header-rd_doc_type EQ 'T01'.

          IF ppi_module EQ 'FI'.

            PERFORM add_messg    USING icon_red_light text-e45
                                       '' '' '' ''
                              CHANGING pit_messg.

          ELSE.

*        "**เคสใบเสร็จ(โยกเงินมัดจำมาตัดใบแจ้งหนี้ )
            PERFORM check_dg    USING ppi_header
                                      ppi_module
                             CHANGING lv_pass.

            IF lv_pass NE 'X'.

              PERFORM add_messg    USING icon_red_light text-e45
                                         '' '' '' ''
                                CHANGING pit_messg.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "check_collector
*>>> CH17: BOI
FORM check_arc_collector USING ppi_header TYPE gty_doc_header
                               value(ppi_module)
                      CHANGING pit_messg  TYPE gtty_messg.

  DATA: "lwa_collector TYPE gty_collector,
        lv_is_cancelled TYPE flag,
        ls_sd_bseg LIKE LINE OF gt_sd_bseg,
        lv_valid_collect TYPE flag.                         "INS CH02

  DATA: lv_pass TYPE c.

**2.5nd priority (For SDS)
**IF ZSDSFIT014-RD_DOC_TYPE = ‘T01’ or ‘T03’
**AND ZSDSFIT014-NET_AMOUNT_BF_VAT <> 0
**--ใบเสร็จรับเงินเป็น 0 สามารถส่งได้เลยไม่ต้องเช็ค collector
*  CHECK ( ppi_header-rd_doc_type EQ 'T01' OR
*          ppi_header-rd_doc_type EQ 'T03' ) AND
*          ppi_header-net_amt_bf_vat IS NOT INITIAL.
*  ">>>>>> BEGIN OF INSERTION: <CH14> on 13.01.2021 16:22:38 >>>>>>
***  IF ppi_module = 'SD'.
*  LOOP AT gt_sd_bseg INTO ls_sd_bseg
*    WHERE bukrs = ppi_header-bukrs
*    AND belnr =   ppi_header-sap_doc_no
*    AND gjahr =   ppi_header-gjahr
*    AND koart = 'D'
*    AND augbl <> space
*    AND augdt <> space.
*    EXIT. "เจอก็จะไม่เช็ค collector
*  ENDLOOP.
*  IF sy-subrc = 0.
*    EXIT. "get out not check collector
*  ENDIF.
***  ENDIF.
  "<<<<<< END OF INSERTION: <CH14> on 13.01.2021 16:22:38  <<<<<<
  CASE ppi_header-form_name.
    WHEN 'ZSF_SD_RECEIPT_NEW_A4' OR "010
         'ZSF_FI_RECEIPT_A4'.       "002

**>>> BOI CH04 >>>
*      IF ppi_header-form_name = 'ZSF_SD_RECEIPT_NEW_A4' AND "010
*         ppi_header-pay_term = 'A000'.
*
*        EXIT.
*      ENDIF.
**<<< EOI CH04 <<<
*
**>>> BOM CH02 >>>
**      READ TABLE gt_collector INTO lwa_collector
**       WITH KEY bukrs = ppi_header-bukrs
**                belnr = ppi_header-document_no
**                status = '02'.
**      IF sy-subrc EQ 0 AND
**        ( lwa_collector-action_type EQ '02' OR
**          lwa_collector-action_type EQ '11' ).
**      ELSE.
*
*      CLEAR lv_valid_collect.
*
*      LOOP AT gt_collector INTO lwa_collector
*        WHERE bukrs = ppi_header-bukrs
*          AND belnr = ppi_header-document_no
***          AND gjahr = ppi_header-gjahr                "INS CH06 "P'JIBBIZ cancel it by 04.11.2020
*          AND delete_flag = abap_false
*          AND status = '02'
*          AND ( action_type = '02' OR action_type = '11' ).
*
*        lv_valid_collect = abap_true.
*        EXIT.
*      ENDLOOP.
*
*      IF lv_valid_collect = abap_true.
*        "Do nothing
*      ELSE.
**<<< EOM CH02 <<<
*
*        IF ppi_module EQ 'FI'.
*
*          PERFORM add_messg    USING icon_red_light text-e45
*                                     '' '' '' ''
*                            CHANGING pit_messg.
*
*        ELSE.
**        "**เคสใบเสร็จ(โยกเงินมัดจำมาตัดใบแจ้งหนี้ )
*          PERFORM check_dg    USING ppi_header
*                                    ppi_module
*                           CHANGING lv_pass.
*
*          IF lv_pass NE 'X'.
*
*            PERFORM add_messg    USING icon_red_light text-e45
*                                       '' '' '' ''
*                              CHANGING pit_messg.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.

    WHEN 'ZSF_FI_RECEIPT_NEW_A4'.   "011
*>>> BOI CH04 >>>
      IF ppi_header-pay_term = 'A000'.
        EXIT.
      ENDIF.
*<<< EOI CH04 <<<

*>>> BOM CH07 >>>
*      READ TABLE gt_collector INTO lwa_collector
*       WITH KEY bukrs = ppi_header-bukrs
*                receipt_no = ppi_header-document_no
**                gjahr = ppi_header-gjahr   "INS CH06 "P'JIBBIZ cancel it by 04.11.2020
*                status = '02'.
*      IF sy-subrc EQ 0 AND
*        ( lwa_collector-action_type EQ '02' OR
*          lwa_collector-action_type EQ '11' ).

      ">>>>>> BEGIN OF INSERTION: <CH09> on 04.11.2020 13:10:34 >>>>>>
      SELECT SINGLE fksto
        INTO lv_is_cancelled
        FROM ZSDSFIT021
        WHERE xblnr = ppi_header-document_no.
**        vbeln = ''
      IF lv_is_cancelled IS NOT INITIAL.
        PERFORM add_messg    USING icon_red_light text-e54
                                     '' '' '' ''
                          CHANGING pit_messg.
        CLEAR lv_is_cancelled.
        EXIT.
      ENDIF.


      "<<<<<< END OF INSERTION: <CH09> on 04.11.2020 13:10:34  <<<<<<

      CLEAR lv_valid_collect.

*      LOOP AT gt_collector INTO lwa_collector
*        WHERE bukrs = ppi_header-bukrs
*          AND receipt_no = ppi_header-document_no
***          AND gjahr = ppi_header-gjahr                      "INS CH06 "P'JIBBIZ cancel it by 04.11.2020
*          AND status = '02'
*          AND ( action_type = '02' OR action_type = '11' )
*          AND delete_flag <> 'X'.                             "INS CH16 by 03.02.2021
*
*        lv_valid_collect = abap_true.
*        EXIT.
*      ENDLOOP.
      IF lv_valid_collect = abap_true.
**        IF lwa_collector-delete_flag = abap_true.           "DEL CH16 by 03.02.2021
***          PERFORM add_messg    USING icon_red_light text-e54 "DEL CH16 by 10.02.2021
***                                       '' '' '' ''           "DEL CH16 by 10.02.2021
***                            CHANGING pit_messg.              "DEL CH16 by 10.02.2021
**        ENDIF.
*<<< EOM CH07 <<<

      ELSE.

        IF ppi_module EQ 'FI'.

          PERFORM add_messg    USING icon_red_light text-e45
                                     '' '' '' ''
                            CHANGING pit_messg.

        ELSE.
*        "**เคสใบเสร็จ(โยกเงินมัดจำมาตัดใบแจ้งหนี้ )
          PERFORM check_dg    USING ppi_header
                                    ppi_module
                           CHANGING lv_pass.

          IF lv_pass NE 'X'.

            PERFORM add_messg    USING icon_red_light text-e45
                                       '' '' '' ''
                              CHANGING pit_messg.
          ENDIF.

        ENDIF.
      ENDIF.

    WHEN 'ZSF_FI_RECEIPT_INV_A4'.   "001
*
**>>> BOM CH07 >>>
**      READ TABLE gt_collector INTO lwa_collector
**       WITH KEY bukrs = ppi_header-bukrs
**                belnr = ppi_header-document_no
**                gjahr = ppi_header-gjahr   "INS CH06
**                delete_flag = abap_false
**                status = '02'.
**      IF sy-subrc EQ 0 AND
**        ( lwa_collector-action_type EQ '02' OR
**          lwa_collector-action_type EQ '11' ).
*
*      CLEAR lv_valid_collect.
*
*      LOOP AT gt_collector INTO lwa_collector
*        WHERE bukrs = ppi_header-bukrs
*          AND belnr = ppi_header-document_no
***          AND gjahr = ppi_header-gjahr                   "INS CH06 "P'JIBBIZ cancel it by 04.11.2020
***          AND delete_flag = abap_false
*          AND delete_flag <> 'X'                           "INS CH16 by 03.02.2021
*          AND status = '02'
*          AND ( action_type = '02' OR action_type = '11' ).
*
*        lv_valid_collect = abap_true.
*        EXIT.
*      ENDLOOP.
*
*      IF lv_valid_collect = abap_true.
*        "Do nothing
*      ELSE.
**<<< BOM CH07 <<<
*        IF ppi_header-rd_doc_type EQ 'T01'.
*
*          IF ppi_module EQ 'FI'.
*
*            PERFORM add_messg    USING icon_red_light text-e45
*                                       '' '' '' ''
*                              CHANGING pit_messg.
*
*          ELSE.
*
**        "**เคสใบเสร็จ(โยกเงินมัดจำมาตัดใบแจ้งหนี้ )
*            PERFORM check_dg    USING ppi_header
*                                      ppi_module
*                             CHANGING lv_pass.
*
*            IF lv_pass NE 'X'.
*
*              PERFORM add_messg    USING icon_red_light text-e45
*                                         '' '' '' ''
*                                CHANGING pit_messg.
*            ENDIF.
*          ENDIF.
*
*        ENDIF.
*      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM .                    "check_arc_collector
*<<< CH17: EOI

*&---------------------------------------------------------------------*
*& Form CHECK_CUST_TAX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PO_HEAD_IT_MESSG
*&---------------------------------------------------------------------*
FORM check_cust_tax       USING pi_kunnr_branch
                       CHANGING pi_kunnr_tax_id
                                pit_messg  TYPE gtty_messg
                                ch_sch_id.

  DATA: lv_digits TYPE i,
        lv_htype LIKE dd01v-datatype.

*1.	Customer Tax ID is blank (No error SCH_ID = OTHR)
  IF pi_kunnr_tax_id IS INITIAL OR
     pi_kunnr_tax_id EQ 'N/A'.
    ch_sch_id = 'OTHR'.
    pi_kunnr_tax_id = 'N/A'.
  ELSE.

    lv_digits = STRLEN( pi_kunnr_tax_id ).
    IF lv_digits EQ 13.

      CALL FUNCTION 'NUMERIC_CHECK'
        EXPORTING
          string_in = pi_kunnr_tax_id
        IMPORTING
          htype     = lv_htype.

      IF lv_htype EQ 'NUMC'.
*      2.	Customer Tax ID has 13 digits (เป็นตัวเลขทั้งหมด)

*        2.1.	Customer Branch is blank (No error  SCH_ID = NIDN)
*        2.2.	Customer Branch has 5 digits (เป็นตัวเลขทั้งหมด) (No error  SCH_ID = TXID)
*        2.3.	Customer Branch has 5 digits (ไม่เป็นตัวเลขทั้งหมด)
*        Return Error: “Customer Tax Branch must be numeric with 5 digits”
*        2.4.	Customer Branch must be 5 digits
*        Return Error: “Customer Tax Branch has less or more 5 digits”

        IF pi_kunnr_branch IS INITIAL.
          ch_sch_id = 'NIDN'.
        ELSE.
          lv_digits = STRLEN( pi_kunnr_branch ).
          IF lv_digits EQ 5.

            CLEAR lv_htype.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = pi_kunnr_tax_id
              IMPORTING
                htype     = lv_htype.

            IF lv_htype EQ 'NUMC'.
              ch_sch_id = 'TXID'.
            ELSE.
              PERFORM add_messg    USING icon_red_light text-e49
                                         '' '' '' ''
                                CHANGING pit_messg.
            ENDIF.

          ELSE.

            PERFORM add_messg    USING icon_red_light text-e48
                                       '' '' '' ''
                              CHANGING pit_messg.
          ENDIF.
        ENDIF.
      ELSE.
        "Customer Tax ID must be numeric with 13 digits
        PERFORM add_messg        USING icon_red_light text-e46
                                       '' '' '' ''
                              CHANGING pit_messg.
      ENDIF.

    ELSE.

      IF pi_kunnr_tax_id = 'N/A'. "Customer Tax ID is blank (No error SCH_ID = OTHR)

      ELSE.
        "Customer Tax ID must be 13 digits
        PERFORM add_messg        USING icon_red_light text-e47
                                       '' '' '' ''
                              CHANGING pit_messg.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "check_cust_tax
*&---------------------------------------------------------------------*
*& Form CHECK_DG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_HEADER
*&      <-- PO_HEAD_IT_MESSG
*&---------------------------------------------------------------------*
FORM check_dg USING    pi_header TYPE gty_doc_header
                       value(pi_module)
              CHANGING po_pass.

  CLEAR po_pass.

  DATA: ls_param LIKE LINE OF gt_param.

  DATA: lv_augbl TYPE bseg-augbl,
        lv_auggj TYPE bseg-auggj,
        lv_blart TYPE bkpf-blart.
******* เฉพาะ  SD *******
**เคสใบเสร็จ(โยกเงินมัดจำมาตัดใบแจ้งหนี้ )
*check doc. 501*(SD)  BSEG-AUGBL (Clearing) <>blank,
*นำ BSEG-AUGBL (422*)เพื่อเช็คว่าเป็น doc.type DG ใน param  เงื่อนไข
*BKPF-BELNR=BSEG-AUGBL
*BKPF-BLART=DG form param
*จะสามารถส่งใบเสร็จขึ้น cloud ได้

*  IF pi_module EQ 'FI'.
*    po_pass = 'X'.
*  ELSE.
  IF pi_header-document_no+0(3) = '501'.

    SELECT SINGLE augbl auggj
      FROM bseg
      INTO (lv_augbl, lv_auggj)
      WHERE bukrs = pi_header-bukrs
        AND belnr = pi_header-document_no
        AND gjahr = pi_header-gjahr
        AND augbl NE space.

    IF lv_augbl IS NOT INITIAL AND
       lv_auggj IS NOT INITIAL.

      SELECT SINGLE blart
        FROM bkpf
        INTO lv_blart
       WHERE bukrs = pi_header-bukrs
         AND belnr = lv_augbl
         AND gjahr = lv_auggj.

      LOOP AT gt_param INTO ls_param
       WHERE id        = 'ZETX001'
         AND name      = 'DOCTYPE_OUTCLTOR'
         AND param_ext = 'FI'
         AND low_value = lv_blart.
        po_pass = 'X'.
        EXIT.
      ENDLOOP.

    ENDIF.
  ENDIF.
*  ENDIF.

ENDFORM.                    "check_dg
*&---------------------------------------------------------------------*
*& Form CHECK_EFF_DATE
*&---------------------------------------------------------------------*
FORM check_eff_date    USING pi_date  TYPE sy-datum
                    CHANGING pit_messg TYPE gtty_messg.

  DATA: ls_ret2 TYPE bapiret2.

  CALL FUNCTION 'Z_SDSFI_EFFECTIVE_DATE'
    EXPORTING
      im_date = pi_date
    IMPORTING
      ex_ret2 = ls_ret2.

  IF ls_ret2-type NE 'S'.

    PERFORM add_messg        USING icon_red_light ls_ret2-message
                                   '' '' '' ''
                          CHANGING pit_messg.

  ENDIF.

ENDFORM.                    "check_eff_date
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<
*&---------------------------------------------------------------------*
*& Form CHECK_ERROR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_BILL
*&      --> LWA_HEADER
*&      <-- PO_HEAD_IT_MESSG
*&---------------------------------------------------------------------*
FORM check_error  USING    ppi_bill   TYPE gty_bill
                           ppi_header TYPE gty_doc_header
                           pi_verified
                  CHANGING pit_messg  TYPE gtty_messg.

  CASE 'X'.
    WHEN rb_trn.

      CASE ppi_header-status.
        WHEN '1'.

          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.

          ELSE.
            "กรณี Document ถูก Reverse
            PERFORM add_messg        USING icon_yellow_light text-e21
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ENDIF.

        WHEN '2' OR '6'.
          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.

            PERFORM add_messg        USING icon_red_light text-e22
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ELSE.
            "กรณี Document ถูก Reverse

            IF ppi_header-reverse_flag = 'Y'.
              PERFORM add_messg        USING icon_red_light text-e24
                                             '' '' '' ''
                                    CHANGING pit_messg.
            ELSE.
              PERFORM add_messg        USING icon_yellow_light text-e23
                                             '' '' '' ''
                                    CHANGING pit_messg.
            ENDIF.

          ENDIF.
        WHEN '3'.

          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.

            PERFORM add_messg        USING icon_red_light text-e40
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ELSE.
            "กรณี Document ถูก Reverse
            PERFORM add_messg        USING icon_yellow_light text-e21
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ENDIF.

        WHEN '4'.

          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.

          ELSE.
            "กรณี Document ถูก Reverse
            PERFORM add_messg        USING icon_yellow_light text-e21
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ENDIF.

        WHEN '5'.

          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.
            PERFORM add_messg        USING icon_red_light text-e26
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ELSE.
            "กรณี Document ถูก Reverse

            IF ppi_header-reverse_flag = 'Y'.
              PERFORM add_messg        USING icon_red_light text-e24
                                             '' '' '' ''
                                    CHANGING pit_messg.
            ELSE.
              PERFORM add_messg        USING icon_yellow_light text-e27
                                             '' '' '' ''
                                    CHANGING pit_messg.
            ENDIF.

          ENDIF.


        WHEN OTHERS.
      ENDCASE.

    WHEN rb_rej.

      CASE ppi_header-status.
        WHEN '1'.
          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.
            PERFORM add_messg        USING icon_red_light text-e28
                                           '' '' '' ''
                                  CHANGING pit_messg.
          ELSE.

            "กรณี Document ถูก Reverse
            PERFORM add_messg        USING icon_red_light text-e29
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ENDIF.
        WHEN '2' OR '6'.
          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.

            IF pi_verified EQ abap_on.
              PERFORM add_messg        USING icon_red_light text-e36
                                             '' '' '' ''
                                    CHANGING pit_messg.
            ENDIF.

          ELSE.

            IF ppi_header-reverse_flag = 'Y'.
              IF pi_verified EQ abap_on.
                PERFORM add_messg        USING icon_red_light text-e36
                                               '' '' '' ''
                                      CHANGING pit_messg.
              ELSE.
                PERFORM add_messg        USING icon_yellow_light text-e32
                                               '' '' '' ''
                                      CHANGING pit_messg.
              ENDIF.
            ELSE.
              PERFORM add_messg        USING icon_red_light text-e30
                                             '' '' '' ''
                                    CHANGING pit_messg.
            ENDIF.

          ENDIF.
        WHEN '3'.
          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.
            PERFORM add_messg        USING icon_red_light text-e31
                                           '' '' '' ''
                                  CHANGING pit_messg.
          ELSE.

            "กรณี Document ถูก Reverse
            PERFORM add_messg        USING icon_red_light text-e31
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ENDIF.
        WHEN '4'.
          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.
            PERFORM add_messg        USING icon_red_light text-e28
                                           '' '' '' ''
                                  CHANGING pit_messg.
          ELSE.

            "กรณี Document ถูก Reverse
            PERFORM add_messg        USING icon_red_light text-e29
                                           '' '' '' ''
                                  CHANGING pit_messg.

          ENDIF.
        WHEN '5'.
          "กรณี Document ยังไม่ Reverse
          IF ppi_bill-fksto IS INITIAL.

            IF pi_verified EQ abap_on.
              PERFORM add_messg        USING icon_red_light text-e36
                                             '' '' '' ''
                                    CHANGING pit_messg.
            ENDIF.

          ELSE.
            "กรณี Document ถูก Reverse

            IF ppi_header-reverse_flag = 'Y'.
              IF pi_verified EQ abap_on.
                PERFORM add_messg        USING icon_red_light text-e36
                                               '' '' '' ''
                                      CHANGING pit_messg.
              ELSE.
                PERFORM add_messg        USING icon_yellow_light text-e32
                                               '' '' '' ''
                                      CHANGING pit_messg.
              ENDIF.
            ELSE.
              PERFORM add_messg        USING icon_red_light text-e30
                                             '' '' '' ''
                                    CHANGING pit_messg.
            ENDIF.

          ENDIF.
        WHEN OTHERS.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.
  ">>>>>> BEGIN OF INSERTION: <CH12> 30.11.2020 10:30:51 >>>>>>
  IF ppi_header-status = '9'.
    PERFORM add_messg        USING icon_red_light text-e57
                                   '' '' '' ''
                          CHANGING pit_messg.
  ENDIF.
  "<<<<<< END OF INSERTION: <CH12> 30.11.2020 10:30:51  <<<<<<

ENDFORM.                    "check_error
*&---------------------------------------------------------------------*
*& Form MESSAGE_FOR_BATCH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_C01
*&---------------------------------------------------------------------*
FORM message_for_batch  USING    pi_message.

  IF sy-batch IS NOT INITIAL.
    MESSAGE i398(00) WITH pi_message.
  ENDIF.

ENDFORM.                    "message_for_batch
*&---------------------------------------------------------------------*
*& Form ASSIGN_PDF_TO_HEAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM assign_pdf_to_head .

*  IF p_memid IS NOT INITIAL.
*    PERFORM assign_pdf_from_mem.
*  ELSE.
  PERFORM assign_pdf_from_al11.
*  ENDIF.

ENDFORM.                    "assign_pdf_to_head
*&---------------------------------------------------------------------*
*& Form ASSIGN_PDF_FROM_AL11
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM assign_pdf_from_al11 .

  DATA: lit_files TYPE ZSDSFIS024_TT.
  DATA: lwa_file LIKE LINE OF lit_files,
        lwa_pdf  LIKE LINE OF gt_pdf_result.

  FIELD-SYMBOLS: <lfs_head> LIKE LINE OF gt_head.

  PERFORM get_files_from_al11 CHANGING lit_files.

  LOOP AT gt_head ASSIGNING <lfs_head>
    WHERE rd_doc_type NE 'T07'.

    CLEAR lwa_file.
    READ TABLE lit_files INTO lwa_file
      WITH KEY wa_latest-sapobjectid = <lfs_head>-sapobjectid
               wa_latest-companycode = <lfs_head>-bukrs
               wa_latest-outputtype  = <lfs_head>-outtype
               "wa_latest-outputtype  = <lfs_head>-wa_typcod_map-kschl
               BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <lfs_head>-x_pdf   = abap_true.
      <lfs_head>-wa_al11 = lwa_file.

      CLEAR lwa_pdf.
      lwa_pdf-fyear         = <lfs_head>-gjahr.
      lwa_pdf-comp          = <lfs_head>-bukrs.
      lwa_pdf-sap_doc_no    = <lfs_head>-sap_doc_no.
      lwa_pdf-sap_doc_type  = <lfs_head>-sap_doc_type.
      lwa_pdf-doc_type      = <lfs_head>-rd_doc_type.
      lwa_pdf-file_name     = lwa_file-wa_latest-filename.
      lwa_pdf-file_path     = lwa_file-wa_latest-fullfilename.
      lwa_pdf-module        = <lfs_head>-module_etx.
      "lwa_pdf-kschl         = <lfs_head>-wa_typcod_map-kschl.
      APPEND lwa_pdf TO <lfs_head>-it_pdf.

    ELSE.

      IF <lfs_head>-reverse_flag EQ 'N'. "เฉพาะ ส่งกรม
        PERFORM add_messg       USING icon_red_light text-e37
                                      '' '' '' ''
                             CHANGING <lfs_head>-it_messg.
      ENDIF.

    ENDIF.

  ENDLOOP.  "gt_head
ENDFORM.                    "assign_pdf_from_al11
*&---------------------------------------------------------------------*
*& Form GET_FILES_FROM_AL11
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LIT_FILES
*&---------------------------------------------------------------------*
FORM get_files_from_al11  CHANGING cht_al11_files TYPE ZSDSFIS024_TT.

  REFRESH cht_al11_files.

  CALL FUNCTION 'Z_SDSFI_AL11_FILELIST'
    EXPORTING
      im_directory  = gwa_file_path-sap_dir_out
    IMPORTING
      ex_al11_files = cht_al11_files
    EXCEPTIONS
      no_file_found = 1
      OTHERS        = 2.

  SORT cht_al11_files BY wa_latest-sapobjectid
                         wa_latest-companycode
                         wa_latest-outputtype
                         wa_latest-yyyymmdd DESCENDING
                         wa_latest-hhmmss   DESCENDING.

ENDFORM.                    "get_files_from_al11
*&---------------------------------------------------------------------*
*& Form SET_SAPOBJECT_ID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_BILL_BUKRS
*&      --> LWA_BILL_SAP_DOC_NO
*&      --> LWA_BILL_GJAHR
*&      --> LV_MODULE
*&      <-- <LFS_HEAD>_SAPOBJECTID
*&---------------------------------------------------------------------*
FORM set_sapobject_id  USING    pi_bukrs      TYPE vbrk-bukrs
                                pi_sap_doc_no TYPE ZSDSFIT014-sap_doc_no
                                pi_gjahr      TYPE vbrk-gjahr
                                value(pi_module)
                       CHANGING po_sapobjectid TYPE saeobjid.

  DATA: lv_sapobjectid  TYPE saeobjid.

  CASE pi_module.
    WHEN 'SD'.
      lv_sapobjectid = pi_sap_doc_no.

    WHEN 'FI'.
      CONCATENATE pi_bukrs pi_sap_doc_no pi_gjahr
             INTO lv_sapobjectid.

  ENDCASE.

  po_sapobjectid = lv_sapobjectid.

ENDFORM.                    "set_sapobject_id
*&---------------------------------------------------------------------*
*& Form INIT_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_screen .

  FIELD-SYMBOLS: <lfs_bldat> LIKE LINE OF s_bldat[].

  PERFORM get_file_path CHANGING gwa_file_path.

  p_server = gwa_file_path-sap_dir_out.
*  p_bukrs  = '0010'.
*  p_gjahr  = sy-datum+0(4).

*  IF s_bldat[] IS INITIAL.
*    APPEND INITIAL LINE TO s_bldat ASSIGNING <lfs_bldat>.
*    <lfs_bldat>     = 'IBT'.
*    <lfs_bldat>-low = sy-datum.
*    <lfs_bldat>-low+6(2) = '01'.
*
*    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*      EXPORTING
*        day_in            = sy-datum
*      IMPORTING
*        last_day_of_month = <lfs_bldat>-high
*      EXCEPTIONS
*        day_in_no_date    = 1
*        OTHERS            = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*  ENDIF.

ENDFORM.                    "init_screen
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM modify_screen .

  DATA: lv_save_pc TYPE c.

  PERFORM check_save_to_pc CHANGING lv_save_pc.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'M03'.
      IF lv_save_pc EQ 'X'.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

*>>> BOI CH01 >>>
    IF screen-name CP '*P_GJAHR*'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
*<<< EOI CH01 <<<
  ENDLOOP.

ENDFORM.                    "modify_screen
*&---------------------------------------------------------------------*
*& Form GET_FILE_PATH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GWA_FILE_PATH
*&---------------------------------------------------------------------*
FORM get_file_path  CHANGING ch_wa_file_path TYPE ZSDSFIS028.

  CALL FUNCTION 'Z_SDSFI_GET_FILE_PATH'
    IMPORTING
      ex_wa_file_path = ch_wa_file_path.

ENDFORM.                    "get_file_path
*&---------------------------------------------------------------------*
*& Form OPEN_DIR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_PC_DIR
*&---------------------------------------------------------------------*
FORM open_dir  CHANGING ch_dir.

  DATA: lv_title TYPE string.

  lv_title = 'Folder for files'.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = lv_title
    CHANGING
      selected_folder      = ch_dir
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

ENDFORM.                    "open_dir
*&---------------------------------------------------------------------*
*& Form GET_RD_SENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_SD
*&      --> GT_RD_SENT
*&---------------------------------------------------------------------*
FORM get_rd_sent  TABLES   pt_bill    TYPE gtty_bill
                           pt_rd_sent TYPE ZSDSFIS027_TT
                  USING value(pi_module).

  DATA: lt_request TYPE ZSDSFIS029_TT,
        lt_rd_sent TYPE ZSDSFIS027_TT.

  DATA: lwa_bill      TYPE gty_bill,
        lwa_request   LIKE LINE OF lt_request,
        lwa_return    TYPE bapiret2,
        lwa_doc_h_ref LIKE LINE OF gt_doc_h_ref.

  CHECK rb_rej EQ 'X'.

  LOOP AT pt_bill INTO lwa_bill.
    CLEAR lwa_request.
    IF lwa_bill-sap_doc_no IS INITIAL.
      CONTINUE. "Not Printed
    ENDIF.
    lwa_request-company           = lwa_bill-bukrs.
    lwa_request-document_no       = lwa_bill-document_no.
    lwa_request-document_type     = lwa_bill-rd_doc_type.
    lwa_request-fisical_year      = lwa_bill-gjahr.
    lwa_request-module            = pi_module.

    READ TABLE gt_doc_h_ref INTO lwa_doc_h_ref
      WITH KEY bukrs      = lwa_bill-bukrs
               sap_doc_no = lwa_bill-sap_doc_no
               gjahr      = lwa_bill-gjahr
               module_etx = pi_module.
    IF sy-subrc EQ 0.
      lwa_request-ref_document_type = lwa_doc_h_ref-ref_rd_doc_type.
    ENDIF.

    lwa_request-sap_document_no   = lwa_bill-sap_doc_no.

    APPEND lwa_request TO lt_request.
  ENDLOOP.

  CHECK lt_request[] IS NOT INITIAL.

  REFRESH pt_rd_sent[].
  CALL FUNCTION 'Z_SDSFI_WS_CHECK_RD_SENT'
    EXPORTING
      im_it_docs     = lt_request
    IMPORTING
      ex_it_output   = lt_rd_sent
      ex_wa_bapiret2 = lwa_return
    EXCEPTIONS
      empty_it_docs  = 1
      error          = 2
      OTHERS         = 3.

  pt_rd_sent[] = lt_rd_sent[].

ENDFORM.                    "get_rd_sent
*&---------------------------------------------------------------------*
*& Form CHECK_RD_SENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_BILL
*&      <-- LV_VERIFIED
*&---------------------------------------------------------------------*
FORM check_rd_sent  USING    ppi_bill       TYPE gty_bill
                             value(pi_module)
                    CHANGING ch_verified.

  DATA: lwa_rd_sent LIKE LINE OF gt_rd_sent.

  CLEAR ch_verified.

  CHECK rb_rej EQ 'X'.

  READ TABLE gt_rd_sent INTO lwa_rd_sent
    WITH KEY base-company         = ppi_bill-bukrs
             base-sap_document_no = ppi_bill-sap_doc_no
             base-fisical_year    = ppi_bill-gjahr
             base-module          = pi_module.
  CHECK sy-subrc EQ 0.
  IF lwa_rd_sent-msgty = 'E'.
    ch_verified = abap_on.
  ELSE.
    ch_verified = abap_off.
  ENDIF.

ENDFORM.                    "check_rd_sent
*&---------------------------------------------------------------------*
*& Form READ_PARAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_NAME
*&      --> LV_PARAM_EXT
*&      --> LV_SEQUENCE
*&      <-- LV_VALUE
*&---------------------------------------------------------------------*
FORM read_param  USING    pi_name       TYPE ZSDSFIC015-name
                          pi_param_ext  TYPE ZSDSFIC015-param_ext
                          pi_sequence   TYPE ZSDSFIC015-sequence
                 CHANGING ch_value      TYPE ZSDSFIC015-low_value.

  DATA lwa_param LIKE LINE OF gt_param.

  CLEAR ch_value.
  READ TABLE gt_param INTO lwa_param
    WITH KEY name      = pi_name
             param_ext = pi_param_ext.
*             sequence  = pi_sequence.
  IF sy-subrc EQ 0.
    ch_value = lwa_param-low_value.
  ENDIF.

ENDFORM.                    "read_param
*&---------------------------------------------------------------------*
*& Form CHECK_READ_TXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_read_txt .

  DATA: lv_name      TYPE ZSDSFIC015-name VALUE 'READ_TEXT_TABLE',
        lv_param_ext TYPE ZSDSFIC015-param_ext,
        lv_sequence  TYPE ZSDSFIC015-sequence VALUE '1',
        lv_value     TYPE ZSDSFIC015-low_value.

  PERFORM read_param    USING lv_name
                              lv_param_ext
                              lv_sequence
                     CHANGING lv_value.

  gv_read_text_tab = lv_value.

ENDFORM.                    "check_read_txt
*&---------------------------------------------------------------------*
*& Form GET_TEXT_OBJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_TDID
*&---------------------------------------------------------------------*
FORM get_text_obj      USING pi_name     TYPE ZSDSFIC015-name
                             value(pi_module)
                             pi_sequence TYPE ZSDSFIC015-sequence
                    CHANGING ch_tdid     TYPE thead-tdid.

  DATA: lv_param_ext TYPE ZSDSFIC015-param_ext,
        lv_value     TYPE ZSDSFIC015-low_value.

  CLEAR ch_tdid.

  lv_param_ext = pi_module.

  PERFORM read_param    USING pi_name
                              lv_param_ext
                              pi_sequence
                     CHANGING lv_value.

  ch_tdid = lv_value.
  TRANSLATE ch_tdid TO UPPER CASE.
ENDFORM.                    "get_text_obj
*&---------------------------------------------------------------------*
*& Form CHECK_CANCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_BILL
*&      --> PO_HEAD_REVERSE_FLAG
*&      <-- PO_HEAD_CANCEL
*&      <-- PO_HEAD_RD_FLAG
*&---------------------------------------------------------------------*
FORM check_cancel  USING    pi_bill        TYPE gty_bill
                            pi_reverse_flag
                            pi_replace_flag
                            value(ppi_module)
                            ppi_tdid       TYPE thead-tdid
                   CHANGING ch_cancel
                            ch_rd_flag.

  DATA: lv_name        TYPE thead-tdname,
        lv_obj         TYPE thead-tdobject,
        lv_not_send_rd TYPE abap_bool,
        lv_value       TYPE char255.


  CASE ppi_module.
    WHEN 'SD'.
      lv_name = pi_bill-vbeln.
      lv_obj  = 'VBBK'.
    WHEN 'FI'.
      CONCATENATE pi_bill-bukrs pi_bill-vbeln pi_bill-gjahr
      INTO lv_name.
      lv_obj  = 'BELEG'.
    WHEN OTHERS.
  ENDCASE.

  IF gv_read_text_tab EQ abap_on.

    PERFORM read_text_tab    USING ppi_tdid
                                   lv_name
                          CHANGING lv_not_send_rd.
  ELSE.

    PERFORM read_text    USING ppi_tdid
                               lv_name
                               lv_obj
                      CHANGING lv_value. "lv_not_send_rd.
  ENDIF.

  lv_not_send_rd = lv_value.
  TRANSLATE lv_not_send_rd TO UPPER CASE.


  IF pi_reverse_flag EQ 'Y'.

*    pi_replace_flag = 'Y'.

    IF lv_not_send_rd EQ abap_on.
      ch_cancel  = text-c02.
      ch_rd_flag = 'N'.
    ELSE.
      ch_cancel  = text-c03.
    ENDIF.
  ELSE.
    IF lv_not_send_rd EQ abap_on.
      "Blank
    ELSE.
      "Blank
    ENDIF.
  ENDIF.

ENDFORM.                    "check_cancel
*&---------------------------------------------------------------------*
*& Form READ_TEXT_TAB
*&---------------------------------------------------------------------*
*& Only FUNCTION 'READ_TEXT_TABLE' does exist on client system
*&---------------------------------------------------------------------*
*&      --> PPI_TDID
*&      --> LV_NAME
*&---------------------------------------------------------------------*
FORM read_text_tab  USING    pi_tdid TYPE thead-tdid
                             pi_name TYPE thead-tdname
                    CHANGING ch_not_send_rd TYPE abap_bool.

*  DATA: lt_tline TYPE TABLE OF tline.
*
*  DATA: lwa_text_table LIKE LINE OF gt_text_table,
*        lwa_tline      LIKE LINE OF lt_tline.
*
*  CLEAR ch_not_send_rd.
*
*  READ TABLE gt_text_table INTO lwa_text_table
*    WITH KEY header-tdname = pi_name
*             header-tdid   = pi_tdid.
*  IF sy-subrc EQ 0.
*    lt_tline = lwa_text_table-lines.
*
*    LOOP AT lt_tline INTO lwa_tline.
*
**        " In case TDFORMAT is blank, concat text in the same line
**        "   else append new line
*      IF lwa_tline-tdformat IS NOT INITIAL
*         AND lwa_tline-tdformat NE '='.
*        ch_not_send_rd = lwa_tline-tdline.
*        TRANSLATE ch_not_send_rd TO UPPER CASE.
*      ENDIF.
*
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    "read_text_tab
*&---------------------------------------------------------------------*
*& Form READ_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PPI_TDID
*&      --> LV_NAME
*&      <-- LV_NOT_SEND_RD
*&---------------------------------------------------------------------*
FORM read_text  USING    pi_tdid        TYPE thead-tdid
                         pi_name        TYPE thead-tdname
                         pi_obj         TYPE thead-tdobject
                CHANGING ch_value       TYPE char255."ch_not_send_rd TYPE abap_bool.

  DATA: lt_tline  TYPE TABLE OF tline.

  DATA: lwa_tline LIKE LINE OF lt_tline.

*  CLEAR ch_not_send_rd.
  CLEAR ch_value.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = pi_tdid
      language                = 'E'
      name                    = pi_name
      object                  = pi_obj
    TABLES
      lines                   = lt_tline[]
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  LOOP AT lt_tline INTO lwa_tline.
*        " In case TDFORMAT is blank, concat text in the same line
*        "   else append new line
    IF lwa_tline-tdformat IS NOT INITIAL
       AND lwa_tline-tdformat NE '='.
*      ch_not_send_rd = lwa_tline-tdline.
*      TRANSLATE ch_not_send_rd TO UPPER CASE.
      ch_value = lwa_tline-tdline.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "read_text
*&---------------------------------------------------------------------*
*& Form PROCESS_CALLED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_called .

  DATA: lv_error   TYPE char1.

  PERFORM auto_select.
  PERFORM verify_error_exe CHANGING lv_error.
  IF lv_error IS INITIAL.
    PERFORM send_data.
  ELSE.
    m_message: 'S' '000' '38' text-m02 'E'.
  ENDIF.

ENDFORM.                    "process_called
*&---------------------------------------------------------------------*
*& Form AUTO_SELECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM auto_select .

  FIELD-SYMBOLS <lfs_head> TYPE  gty_head.

  LOOP AT gt_head ASSIGNING <lfs_head>
    WHERE icon NE icon_red_light.
    <lfs_head>-sel_cb  = 'X'.
    IF <lfs_head>-email_flag = 'Y'.
      <lfs_head>-mail_cb = 'X'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "auto_select
*>>> BOI CH01 >>>
*&---------------------------------------------------------------------*
*& Form DELETE_CHAR_IN_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM delete_char_in_text CHANGING p_text TYPE adr2-tel_number.

  DATA: lv_int TYPE i,
        lv_len TYPE i.

  lv_len = STRLEN( p_text ).

  WHILE lv_int < lv_len.

    IF NOT p_text+lv_int(1) CA '0123456789'.

      MOVE space TO p_text+lv_int(1).

    ENDIF.

    ADD 1 TO lv_int.

  ENDWHILE.

  CONDENSE p_text NO-GAPS.

ENDFORM.                    "DELETE_CHAR_IN_TEXT
*<<< EOI CH01 <<<
