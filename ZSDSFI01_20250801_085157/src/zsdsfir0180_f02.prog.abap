*&---------------------------------------------------------------------*
*& Include          ZETX001_F02
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form PROCESS_SD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_sd .

  DATA: lv_module TYPE c LENGTH 2 VALUE 'SD'.

  CHECK cb_sd EQ 'X'.

  PERFORM: get_sd,
           get_oth_data USING gt_sd
                              lv_module,
           get_email,
           get_rd_sent  TABLES gt_sd
                               gt_rd_sent
                         USING lv_module,
           get_collector_arc_param ,
           get_dv_customer_param ,
           get_dv_doctype_param ,
           process_data  USING gt_sd
                               lv_module.

ENDFORM.                    "process_sd
*&---------------------------------------------------------------------*
*& Form GET_SD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_sd .

  TYPES: BEGIN OF lty_vbrk,
         bukrs        TYPE vbrk-bukrs,
         gjahr        TYPE vbrk-gjahr,
         vbeln        TYPE vbrk-vbeln,
         knumv        TYPE vbrk-knumv,
         fkdat        TYPE vbrk-fkdat,
         kunnr        TYPE vbrk-kunrg,
         bupla        TYPE vbrk-bupla,
         fkart        TYPE vbrk-fkart,
         ernam        TYPE vbrk-ernam,
         erdat        TYPE vbrk-erdat,
         erzet        TYPE vbrk-erzet,
         fksto        TYPE vbrk-fksto,
         vkorg        TYPE vbrk-vkorg,
         vtweg        TYPE vbrk-vtweg,
         rfbsk        TYPE vbrk-rfbsk,
         posnr        TYPE vbrp-posnr,
         mwsk1        TYPE vbrp-mwskz,
         status       TYPE ZSDSFIT014-status,
         gjahr_h      TYPE ZSDSFIT014-gjahr,
         sap_doc_no   TYPE ZSDSFIT014-sap_doc_no,
         sap_doc_type TYPE ZSDSFIT014-sap_doc_type,
         document_no  TYPE ZSDSFIT014-document_no,
         rd_doc_type  TYPE ZSDSFIT014-rd_doc_type,
         form_name    TYPE ZSDSFIT014-form_name,
       END OF lty_vbrk.

  TYPES:  BEGIN OF lty_map_sd,
            vkorg     TYPE vbrk-vkorg,
            fkart     TYPE vbrk-fkart,
            tax_code  TYPE ZSDSFIC009-tax_code,
            rd_doc_type TYPE ZSDSFIC005-rd_doc_type,
            rd_doc_typ_ds_th TYPE ZSDSFIC005-rd_doc_typ_ds_th,
            rd_doc_typ_ds_en TYPE ZSDSFIC005-rd_doc_typ_ds_en,
            rd_doc_type_grp  TYPE ZSDSFIC005-rd_doc_type_grp,
          END OF lty_map_sd.

  DATA: "lt_sd     TYPE gtty_bill,    "CHG ETAX001
        lt_sd     TYPE gtty_bill2,
        lt_sd_tmp TYPE gtty_bill,
        lt_konv   TYPE ZCL_SDSFI_ETAX002=>tty_prcd.

  DATA: lt_sd2    TYPE TABLE OF lty_vbrk,
        lt_map_sd TYPE TABLE OF lty_map_sd,
        lt_doc_header  TYPE gtty_doc_header.

  DATA: "lwa_sd     TYPE gty_bill,
        lwa_sd     TYPE gty_bill2,    "CHG ETAX001
        lwa_sd_tmp TYPE gty_bill,
        lwa_sd2    TYPE lty_vbrk,
        ls_map_sd  LIKE LINE OF lt_map_sd,
        ls_doc_header LIKE LINE OF lt_doc_header,
        ls_rd_doc_type LIKE LINE OF gt_rd_doc_type,
        ls_konv        LIKE LINE OF lt_konv.

  DATA: lt_thead    TYPE TABLE OF thead,
        ls_thead    TYPE thead,
        lv_name     TYPE ZSDSFIC015-name,
        lv_tdid     TYPE thead-tdid,
        lv_sequence TYPE ZSDSFIC015-sequence VALUE '1',
        lv_form_name    TYPE ZSDSFIT014-form_name.

  DATA: lr_doc_acr TYPE RANGE OF ZSDSFIT014-document_no.

  FIELD-SYMBOLS: <lfs_fkart> LIKE LINE OF gr_fkart,
                 <lfs_sd>    LIKE LINE OF gt_sd,
                 <lfs_sd2>   LIKE LINE OF lt_sd2,
                 <lfs_r_doc_acr> LIKE LINE OF lr_doc_acr.

  REFRESH: gt_sd[],
           gr_blart[].

  lv_name   = 'TEXT_OBJ_NOTRD'.

  PERFORM get_text_obj    USING lv_name
                                'SD'
                                lv_sequence
                       CHANGING lv_tdid.

*  SELECT a~bukrs a~gjahr a~vbeln a~fkdat a~kunrg
*         a~bupla a~fkart a~ernam
*         a~erdat a~erzet a~fksto
*         b~posnr b~mwskz c~tax_code
*         d~rd_doc_type d~rd_doc_typ_ds_th
**         e~rd_doc_type d~rd_doc_typ_ds_th
*         d~rd_doc_typ_ds_en d~rd_doc_type_grp
*         e~status e~sap_doc_no e~sap_doc_type e~document_no
*         a~vkorg a~vtweg a~rfbsk
*    FROM vbrk AS a
*    INNER JOIN vbrp AS b ON a~vbeln EQ b~vbeln
*    INNER JOIN zte_map_doc_sd AS c ON a~vkorg EQ c~vkorg AND
*                                      a~fkart EQ c~fkart AND
*                                      b~mwskz EQ c~tax_code
*    INNER JOIN ZSDSFIC005 AS d ON c~rd_doc_type EQ d~rd_doc_type
*    LEFT OUTER JOIN ZSDSFIT014 AS e ON a~bukrs EQ e~bukrs AND
*                                           a~vbeln EQ e~sap_doc_no AND
**                                           a~gjahr EQ e~gjahr AND
**                                           c~rd_doc_type EQ e~rd_doc_type AND
*                                           e~module_etx EQ 'SD'
*    INTO TABLE gt_sd
*    WHERE a~bukrs EQ p_bukrs    "Company Code
**      AND a~gjahr EQ p_gjahr    "Fiscal Year         "CHG 06.09.2020
*      AND a~fkdat IN s_bldat    "posting Date
*      AND a~kunrg IN s_kunnr    "Customer
*      AND a~bupla IN s_bupla    "Business Place
*      AND a~vkorg IN s_vkorg    "Sales Organization
*      AND a~vtweg IN s_vtweg    "Distribution Channel
*      AND a~fkart IN s_fkart    "Billing Type
*      AND a~vbeln IN s_vbeln    "Billing Number
*      AND a~ernam IN s_usnam    "Create User
*      AND a~erdat IN s_cpudt    "Create Date
*      AND a~erzet IN s_cputm    "Create Time
*      AND d~rd_doc_type_grp IN gr_rd_dctypgrp.

  SELECT a~bukrs a~gjahr a~vbeln a~knumv
         a~fkdat a~kunrg
         a~bupla a~fkart a~ernam
         a~erdat a~erzet a~fksto
         a~vkorg a~vtweg a~rfbsk
         b~posnr b~mwskz
         e~status e~gjahr e~sap_doc_no e~sap_doc_type
         e~document_no e~rd_doc_type e~form_name
    FROM vbrk AS a
    INNER JOIN vbrp AS b ON a~vbeln EQ b~vbeln
*    INNER JOIN konv AS c ON c~knumv EQ a~knumv
*                            c~kposn EQ b~vbeln
*                            c~kschl EQ 'MWST'
    LEFT OUTER JOIN ZSDSFIT014 AS e ON a~bukrs EQ e~bukrs AND
                                           a~vbeln EQ e~sap_doc_no AND
                                           e~module_etx EQ 'SD'
    INTO TABLE lt_sd2
    WHERE a~bukrs EQ p_bukrs    "Company Code
*      AND a~gjahr EQ p_gjahr    "Fiscal Year
      AND a~fkdat IN s_bldat    "posting Date
      AND a~kunrg IN s_kunnr    "Customer
      AND a~bupla IN s_bupla    "Business Place
      AND a~vkorg IN s_vkorg    "Sales Organization
      AND a~vtweg IN s_vtweg    "Distribution Channel
      AND a~fkart IN s_fkart    "Billing Type
      AND a~vbeln IN s_vbeln    "Billing Number
      AND a~ernam IN s_usnam    "Create User
      AND a~erdat IN s_cpudt    "Create Date
      AND a~erzet IN s_cputm.    "Create Time
*      AND d~rd_doc_type_grp IN gr_rd_dctypgrp.

*>>>>
  "CASE A.C.R
  APPEND INITIAL LINE TO lr_doc_acr ASSIGNING <lfs_r_doc_acr>.
  <lfs_r_doc_acr>-sign   = 'I'.
  <lfs_r_doc_acr>-option = 'CP'.
  <lfs_r_doc_acr>-low    = 'A*'.
  APPEND INITIAL LINE TO lr_doc_acr ASSIGNING <lfs_r_doc_acr>.
  <lfs_r_doc_acr>-sign   = 'I'.
  <lfs_r_doc_acr>-option = 'CP'.
  <lfs_r_doc_acr>-low    = 'C*'.
  APPEND INITIAL LINE TO lr_doc_acr ASSIGNING <lfs_r_doc_acr>.
  <lfs_r_doc_acr>-sign   = 'I'.
  <lfs_r_doc_acr>-option = 'CP'.
  <lfs_r_doc_acr>-low    = 'R*'.

  SELECT *
    FROM ZSDSFIT014 AS a
    INTO TABLE lt_doc_header
    WHERE a~bukrs EQ p_bukrs    "Company Code
    AND a~gjahr EQ p_gjahr    "Fiscal Year
    AND a~sap_posting_date IN s_bldat    "posting Date
    AND a~kunnr IN s_kunnr    "Customer
    AND a~bupla IN s_bupla    "Business Place
    AND a~sap_doc_type IN s_fkart    "Billing Type
    AND a~sap_doc_no IN s_vbeln    "Billing Number
    AND a~sap_doc_no IN lr_doc_acr "Billing Number A.C.R
    AND a~module_etx EQ 'SD'.

  LOOP AT lt_doc_header INTO ls_doc_header.
    APPEND INITIAL LINE TO lt_sd2 ASSIGNING <lfs_sd2>.
    <lfs_sd2>-bukrs        = ls_doc_header-bukrs.
    <lfs_sd2>-gjahr        = ls_doc_header-gjahr.
    <lfs_sd2>-vbeln        = ls_doc_header-sap_doc_no.
*      <lfs_sd2>-knumv
    <lfs_sd2>-fkdat        = ls_doc_header-sap_posting_date.
    <lfs_sd2>-kunnr        = ls_doc_header-kunnr.
    <lfs_sd2>-bupla        = ls_doc_header-bupla.
    <lfs_sd2>-fkart        = ls_doc_header-sap_doc_type.
    <lfs_sd2>-ernam        = ls_doc_header-print_user.
    <lfs_sd2>-erdat        = ls_doc_header-print_date.
*      <lfs_sd2>-erzet
*      <lfs_sd2>-fksto
*      <lfs_sd2>-vkorg
*      <lfs_sd2>-vtweg
*      <lfs_sd2>-rfbsk
*      <lfs_sd2>-posnr
*      <lfs_sd2>-mwsk1
    <lfs_sd2>-status       = ls_doc_header-status.
    <lfs_sd2>-gjahr_h      = ls_doc_header-gjahr.
    <lfs_sd2>-sap_doc_no   = ls_doc_header-sap_doc_no.
    <lfs_sd2>-sap_doc_type = ls_doc_header-sap_doc_type.
    <lfs_sd2>-document_no  = ls_doc_header-document_no.
    <lfs_sd2>-rd_doc_type  = ls_doc_header-rd_doc_type.
    <lfs_sd2>-form_name    = ls_doc_header-form_name.
  ENDLOOP.
*<<<<

  IF lt_sd2[] IS NOT INITIAL.

    REFRESH lt_doc_header[].
    SELECT *
     FROM ZSDSFIT014
     INTO TABLE lt_doc_header
     FOR ALL ENTRIES IN lt_sd2
     WHERE bukrs      EQ lt_sd2-bukrs
      AND sap_doc_no  EQ lt_sd2-vbeln
*     AND gjahr       EQ lt_sd2-gjahr
      AND module_etx  EQ 'SD'.

    SELECT  knumv kposn stunr zaehk
            kschl kawrt mwsk1 kwert
            kbetr koaid
       FROM konv
       INTO TABLE lt_konv
       FOR ALL ENTRIES IN lt_sd2
       WHERE knumv = lt_sd2-knumv
         AND kschl = 'MWST'.
    ">>>>>> BEGIN OF INSERTION: <CH14> on 13.01.2021 15:14:30 >>>>>>
    SELECT bukrs
           belnr
           gjahr
           buzei
           koart
           augbl
           augdt
      FROM bseg
      INTO TABLE gt_sd_bseg
**      FOR ALL ENTRIES IN lt_sd2
      WHERE bukrs = p_bukrs
      AND   belnr IN s_vbeln
      AND   gjahr = p_gjahr.
    "<<<<<< END OF INSERTION: <CH14> on 13.01.2021 15:14:30  <<<<<<
  ENDIF.

*>>> BEGIN OF INSERTION: <ETAX001> on 06.09.2020 <<<
*  LOOP AT gt_sd ASSIGNING <lfs_sd>.
  LOOP AT lt_sd2 INTO lwa_sd2.
    CALL FUNCTION 'GET_CURRENT_YEAR'
      EXPORTING
        bukrs         = lwa_sd2-bukrs
        date          = lwa_sd2-fkdat
      IMPORTING
*     CURRM         =
        curry         = lwa_sd2-gjahr.

*    IF lwa_sd2-gjahr EQ lwa_sd2-gjahr_h.
    MOVE-CORRESPONDING lwa_sd2 TO lwa_sd_tmp.

    CLEAR ls_konv.
    READ TABLE lt_konv INTO ls_konv
     WITH KEY knumv = lwa_sd2-knumv
              kposn = lwa_sd2-posnr.
    IF sy-subrc EQ 0.
      lwa_sd_tmp-mwsk1    = ls_konv-mwsk1.
      lwa_sd_tmp-tax_code = ls_konv-mwsk1.
    ENDIF.


*      "กรณี FIX ไม่ต้อง Map rd doc type
*      IF lwa_sd2-form_name EQ 'ZSF_SD_RECEIPT_NEW_A4' OR
*         lwa_sd2-form_name EQ 'ZSF_FI_RECEIPT_NEW_A4'.
*
*      ELSE.
*
*        READ TABLE gt_map_doc_sd INTO ls_map_sd
*          WITH KEY vkorg    = lwa_sd-vkorg
*                   fkart    = lwa_sd-fkart
*                   tax_code = lwa_sd-mwsk1.
*        IF sy-subrc EQ 0.
*          lwa_sd-tax_code    = ls_map_sd-tax_code.
*          lwa_sd-rd_doc_type = ls_map_sd-rd_doc_type.
*        ELSE.
*          "กรณีไม่สามารถ map_doc_sd ได้
    CLEAR ls_doc_header.
    READ TABLE lt_doc_header INTO ls_doc_header
      WITH KEY bukrs       = lwa_sd_tmp-bukrs
               gjahr       = lwa_sd_tmp-gjahr
               sap_doc_no  = lwa_sd_tmp-vbeln
               rd_doc_type = lwa_sd_tmp-rd_doc_type.
    IF sy-subrc EQ 0.
      lwa_sd_tmp-rd_doc_type     = ls_doc_header-rd_doc_type.
      lwa_sd_tmp-rd_doc_type_grp = ls_doc_header-rd_doc_type_grp.
    ENDIF.
*        ENDIF.

*      ENDIF.

    CLEAR ls_rd_doc_type.
    READ TABLE gt_rd_doc_type INTO ls_rd_doc_type
      WITH KEY rd_doc_type = lwa_sd_tmp-rd_doc_type.
    IF sy-subrc EQ 0.
      lwa_sd_tmp-rd_doc_type_desc_th = ls_rd_doc_type-rd_doc_type_desc_th.
      lwa_sd_tmp-rd_doc_type_desc_en = ls_rd_doc_type-rd_doc_type_desc_en.
    ENDIF.

    IF lwa_sd_tmp-rd_doc_type_grp IS NOT INITIAL.
      IF lwa_sd_tmp-rd_doc_type_grp IN gr_rd_dctypgrp.
        APPEND lwa_sd_tmp TO gt_sd.
      ENDIF.
    ELSE.
      APPEND lwa_sd_tmp TO gt_sd.
    ENDIF.

*    ENDIF.
  ENDLOOP.

  DELETE gt_sd WHERE gjahr NE p_gjahr.
*>>> END OF INSERTION: <ETAX001> on 06.09.2020 <<<

  SORT gt_sd BY bukrs gjahr vbeln bupla rd_doc_type.

  DELETE gt_sd WHERE : status NOT IN gr_status.


  IF cb_cancl EQ 'X'.

  ELSE.
*      Exclude VBRK-FKSTO = ‘X’ and not found in ZSDSFIT014
*>>> BEGIN OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
*    DELETE gt_sd WHERE fksto EQ 'X' AND
*                       sap_doc_no IS INITIAL.
*>>> END OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
  ENDIF.

*---> Priority O7
*>>> BEGIN OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
*  lt_sd_tmp[] = lt_sd[] = gt_sd[].
  lt_sd_tmp[] = gt_sd[].
  LOOP AT gt_sd INTO lwa_sd_tmp.
    MOVE-CORRESPONDING lwa_sd_tmp TO lwa_sd.
    APPEND lwa_sd TO lt_sd.
  ENDLOOP.
*>>> END OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
  REFRESH gt_sd[].
  LOOP AT lt_sd INTO lwa_sd.

    IF lwa_sd-sap_doc_type IS NOT INITIAL.
      APPEND INITIAL LINE TO gr_fkart ASSIGNING <lfs_fkart>.
      <lfs_fkart>     = 'IEQ'.
      <lfs_fkart>-low = lwa_sd-fkart.
    ENDIF.

*>>> BEGIN OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
*    AT NEW vbeln.
    AT NEW rd_doc_type.
*>>> END OF MODIFICATION: <ETAX001> on 06.09.2020 <<<

      CLEAR lwa_sd_tmp.
      LOOP AT lt_sd_tmp INTO lwa_sd_tmp
        WHERE bukrs = lwa_sd-bukrs
          AND gjahr = lwa_sd-gjahr
          AND vbeln = lwa_sd-vbeln
          AND rd_doc_type = lwa_sd-rd_doc_type
          AND ( tax_code = 'O7' OR tax_code = 'O0').    "Priority O7, O0
        APPEND lwa_sd_tmp TO gt_sd.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        LOOP AT lt_sd_tmp INTO lwa_sd_tmp
        WHERE bukrs = lwa_sd-bukrs
          AND gjahr = lwa_sd-gjahr
          AND vbeln = lwa_sd-vbeln
          AND rd_doc_type = lwa_sd-rd_doc_type.
          APPEND lwa_sd_tmp TO gt_sd.
          EXIT.
        ENDLOOP.
      ENDIF.

    ENDAT.
  ENDLOOP.

  CLEAR lwa_sd.
  LOOP AT gt_sd INTO lwa_sd.

    CLEAR ls_thead.
    IF lwa_sd-fksto EQ 'X' AND
       gv_read_text_tab EQ abap_on.
      ls_thead-tdid     = lv_tdid.
      ls_thead-tdobject = 'VBBK'.
      ls_thead-tdname   = lwa_sd-vbeln.
      ls_thead-tdspras  = 'E'.
      APPEND ls_thead TO lt_thead.
    ENDIF.

  ENDLOOP.

  SORT: gr_fkart BY low.

  DELETE ADJACENT DUPLICATES FROM : gr_fkart COMPARING low.
*  SAP Doc type (SD) FKART
  SELECT fkart vtext
    FROM tvfkt
    INTO TABLE gt_tvfkt
    WHERE fkart IN gr_fkart
      AND spras EQ sy-langu.

  IF lt_thead[] IS NOT INITIAL AND gv_read_text_tab EQ abap_on.

    "Only FUNCTION 'READ_TEXT_TABLE' does exist on client system
*    REFRESH gt_text_table[].
*    CALL FUNCTION 'READ_TEXT_TABLE'
*      IMPORTING
*        text_table              = gt_text_table
**       ERROR_TABLE             =
*      TABLES
*        text_headers            = lt_thead
*      EXCEPTIONS
*        wrong_access_to_archive = 1
*        OTHERS                  = 2.
*    IF sy-subrc <> 0.
*    ENDIF.
  ENDIF.

ENDFORM.                    "get_sd
*&---------------------------------------------------------------------*
*&      Form  GET_DV_CUSTOMER_PARAM
*&---------------------------------------------------------------------*
FORM get_dv_customer_param .

  REFRESH: gr_po_kunnr[].
  CLEAR gr_po_kunnr.

  DATA: ls_param LIKE LINE OF gt_param .

  FIELD-SYMBOLS: <lfs_kunnr> LIKE LINE OF gr_po_kunnr  .
  LOOP AT gt_param INTO ls_param WHERE name = ZCL_SDSFI_ETAX002=>c_dv_cust AND
                                       param_ext = 'SD' .

    APPEND INITIAL LINE TO gr_po_kunnr ASSIGNING <lfs_kunnr>.
    <lfs_kunnr>     = 'IEQ'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_param-low_value
      IMPORTING
        output = <lfs_kunnr>-low.
  ENDLOOP.

ENDFORM.                    " GET_PO_CUSTOMER_PARAM
*&---------------------------------------------------------------------*
*&      Form  GET_DV_DOCTYPE_PARAM
*&---------------------------------------------------------------------*
FORM get_dv_doctype_param .
  REFRESH: gr_dv_doctype[].
  CLEAR gr_dv_doctype.

  DATA: ls_param LIKE LINE OF gt_param .
  FIELD-SYMBOLS: <lfs_doctype> LIKE LINE OF gr_dv_doctype  .

  LOOP AT gt_param INTO ls_param WHERE name = ZCL_SDSFI_ETAX002=>c_dv_rd_doctype  AND
                                       param_ext = 'SD' .

    APPEND INITIAL LINE TO gr_dv_doctype ASSIGNING <lfs_doctype>.
    <lfs_doctype>     = 'IEQ'.
    <lfs_doctype>-low = ls_param-low_value .
  ENDLOOP.
ENDFORM.                    " GET_DV_DOCTYPE_PARAM
*&---------------------------------------------------------------------*
*&      Form  GET_COLLECTOR_ARC_PARAM
*&---------------------------------------------------------------------*
FORM get_collector_arc_param .

  DATA: ls_param LIKE LINE OF gt_param .

  CLEAR gv_check_collector_arc .

  READ TABLE gt_param INTO ls_param WITH KEY name = 'CHECK_COLLECTOR_ARC_ONLY'.
  IF sy-subrc EQ 0 .
    gv_check_collector_arc = ls_param-low_value.
  ENDIF.


ENDFORM.                    " GET_COLLECTOR_ARC_PARAM
