*&---------------------------------------------------------------------*
*& Include          ZETX001_F03
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form PROCESS_FI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_fi .

  DATA: lv_module TYPE c LENGTH 2 VALUE 'FI'.

  CHECK cb_fi EQ 'X'.

  PERFORM: get_fi,
           get_oth_data USING gt_fi
                              lv_module,
           get_email,
           get_rd_sent  TABLES gt_fi
                               gt_rd_sent
                         USING lv_module,
           get_collector_arc_param ,
           process_data  USING gt_fi
                               lv_module.

ENDFORM.                    "process_fi
*&---------------------------------------------------------------------*
*& Form GET_FI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_fi .

  DATA: "lt_fi     TYPE gtty_bill,    "CHG ETAX001
        lt_fi     TYPE gtty_bill2,
        lt_fi_tmp TYPE gtty_bill.

  DATA: "lwa_fi     TYPE gty_bill,    "CHG ETAX001
        lwa_fi     TYPE gty_bill2,
        lwa_fi_tmp TYPE gty_bill,
        lv_kunnr   TYPE bseg-kunnr.

  DATA: lt_thead    TYPE TABLE OF thead,
        ls_thead    TYPE thead,
        lv_name     TYPE ZSDSFIC015-name,
        lv_tdid     TYPE thead-tdid,
        lv_sequence TYPE ZSDSFIC015-sequence VALUE '1'.

  FIELD-SYMBOLS: <lfs_blart> LIKE LINE OF gr_blart.

  REFRESH: gt_fi[],gr_blart[].

  lv_name   = 'TEXT_OBJ_NOTRD'.

  PERFORM get_text_obj    USING lv_name
                                'FI'
                                lv_sequence
                       CHANGING lv_tdid.

  "Get BSEG(Cluster)
  PERFORM get_fi_cluster CHANGING gt_fi.

*  SELECT a~bukrs a~gjahr a~belnr a~budat b~kunnr
*         b~bupla a~blart a~usnam
*         a~cpudt a~cputm a~xreversal
*         b~buzei b~mwskz c~tax_code
*         d~rd_doc_type d~rd_doc_type_desc_th
*         d~rd_doc_type_desc_en d~rd_doc_type_grp
*         e~status e~sap_doc_no e~sap_doc_type e~document_no
*         "a~vkorg a~vtweg
*    FROM bkpf AS a
*    INNER JOIN bseg AS b ON a~belnr = b~belnr AND
*                            a~bukrs = b~bukrs AND
*                            a~gjahr = b~gjahr
*    INNER JOIN ZSDSFIC009 AS c ON a~bukrs EQ c~bukrs AND
*                                      a~blart EQ c~blart AND
*                                      b~mwskz EQ c~tax_code
*    INNER JOIN ZSDSFIC005 AS d ON c~rd_doc_type EQ d~rd_doc_type
*    LEFT OUTER JOIN ZSDSFIT014 AS e ON a~bukrs EQ e~bukrs AND
*                                           a~belnr EQ e~sap_doc_no AND
*                                           a~gjahr EQ e~gjahr AND
*                                           e~module_etx EQ 'FI'
*    INTO TABLE gt_fi
*    WHERE a~bukrs EQ p_bukrs    "Company Code
*      AND a~gjahr EQ p_gjahr    "Fiscal Year
*      AND a~bldat IN s_bldat    "posting Date
*      AND b~kunnr IN s_kunnr    "Customer
*      AND b~bupla IN s_bupla    "Business Place
**      AND a~vkorg IN s_vkorg    "Sales Organization
**      AND a~vtweg IN s_vtweg    "Distribution Channel
*      AND a~blart IN s_blart    "Billing Type
*      AND a~belnr IN s_belnr    "Billing Number
*      AND a~usnam IN s_usnam    "Create User
*      AND a~cpudt IN s_cpudt    "Create Date
*      AND a~cputm IN s_cputm    "Create Time
*      AND d~rd_doc_type_grp IN gr_rd_dctypgrp.

  SORT gt_fi BY bukrs gjahr vbeln bupla rd_doc_type.

  DELETE gt_fi WHERE status NOT IN gr_status.

  IF cb_cancl EQ 'X'.

  ELSE.
*      Exclude BKPF-XREVERAL <> ' ' and not found in ZSDSFIT014
*>>> BEGIN OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
*    DELETE gt_fi WHERE fksto IS NOT INITIAL AND
*                       sap_doc_no IS INITIAL.
*>>> END OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
  ENDIF.

*---> Priority O7
*>>> BEGIN OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
*  lt_fi_tmp[] = lt_fi[] = gt_fi[].
  lt_fi_tmp[] = gt_fi[].
  LOOP AT gt_fi INTO lwa_fi_tmp.
    MOVE-CORRESPONDING lwa_fi_tmp TO lwa_fi.
    APPEND lwa_fi TO lt_fi.
  ENDLOOP.
*>>> END OF MODIFICATION: <ETAX001> on 06.09.2020 <<<

  REFRESH gt_fi[].
  LOOP AT lt_fi INTO lwa_fi.

    IF lwa_fi-sap_doc_type IS NOT INITIAL.
      APPEND INITIAL LINE TO gr_blart ASSIGNING <lfs_blart>.
      <lfs_blart>     = 'IEQ'.
      <lfs_blart>-low = lwa_fi-fkart.
    ENDIF.

*>>> BEGIN OF MODIFICATION: <ETAX001> on 06.09.2020 <<<
*    AT NEW vbeln.
    AT NEW rd_doc_type.
*>>> END OF MODIFICATION: <ETAX001> on 06.09.2020 <<<

      CLEAR: lwa_fi_tmp,lv_kunnr.
      LOOP AT lt_fi_tmp INTO lwa_fi_tmp
        WHERE bukrs = lwa_fi-bukrs
          AND gjahr = lwa_fi-gjahr
          AND vbeln = lwa_fi-vbeln
          AND rd_doc_type = lwa_fi-rd_doc_type        "INS ETAX001
          AND kunnr IS NOT INITIAL.
        lv_kunnr = lwa_fi_tmp-kunnr.
        EXIT.
      ENDLOOP.

      CLEAR lwa_fi_tmp.
      LOOP AT lt_fi_tmp INTO lwa_fi_tmp
        WHERE bukrs = lwa_fi-bukrs
          AND gjahr = lwa_fi-gjahr
          AND vbeln = lwa_fi-vbeln
          AND rd_doc_type = lwa_fi-rd_doc_type        "INS ETAX001
          AND ( tax_code = 'O7' OR tax_code = 'O0').    "Priority O7, O0
        lwa_fi_tmp-kunnr = lv_kunnr.
        APPEND lwa_fi_tmp TO gt_fi.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        LOOP AT lt_fi_tmp INTO lwa_fi_tmp
        WHERE bukrs = lwa_fi-bukrs
          AND gjahr = lwa_fi-gjahr
          AND vbeln = lwa_fi-vbeln
          AND rd_doc_type = lwa_fi-rd_doc_type.        "INS ETAX001
          lwa_fi_tmp-kunnr = lv_kunnr.
          APPEND lwa_fi_tmp TO gt_fi.
          EXIT.
        ENDLOOP.
      ENDIF.

    ENDAT.

  ENDLOOP.

*  CLEAR lwa_fi.
*  LOOP AT gt_fi INTO lwa_fi.
*
*    CLEAR ls_thead.
*    IF "lwa_fi-fksto EQ 'X' AND
*       gv_read_text_tab EQ abap_on.
*      ls_thead-tdid     = lv_tdid.
*      ls_thead-tdobject = 'BELEG'.
*      CONCATENATE lwa_fi-bukrs lwa_fi-vbeln lwa_fi-gjahr INTO ls_thead-tdname.
*      ls_thead-tdspras  = 'E'.
*      APPEND ls_thead TO lt_thead.
*    ENDIF.
*
*  ENDLOOP.

  SORT: gr_fkart BY low.

  DELETE ADJACENT DUPLICATES FROM : gr_fkart COMPARING low.

*  SAP Doc type (FI) BLART
  SELECT blart ltext
    FROM t003t
    INTO TABLE gt_t003t
    WHERE blart IN gr_blart
      AND spras EQ sy-langu.

  IF lt_thead[] IS NOT INITIAL AND gv_read_text_tab EQ abap_on.

    "Only FUNCTION 'READ_TEXT_TABLE' does exist on client system
*    REFRESH gt_text_table[].
*    "Only FUNCTION 'READ_TEXT_TABLE' does exist on client system
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

ENDFORM.                    "get_fi
*&---------------------------------------------------------------------*
*& Form GET_FI_CLUSTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_fi_cluster CHANGING pit_fi TYPE gtty_bill.

  TYPES: BEGIN OF lty_bkpf,
           bukrs     TYPE bkpf-bukrs,
           gjahr     TYPE bkpf-gjahr,
           belnr     TYPE bkpf-belnr,
           budat     TYPE bkpf-budat,
           blart     TYPE bkpf-blart,
           usnam     TYPE bkpf-usnam,
           cpudt     TYPE bkpf-cpudt,
           cputm     TYPE bkpf-cputm,
           xreversal TYPE bkpf-xreversal,
           status       TYPE ZSDSFIT014-status,
           sap_doc_no   TYPE ZSDSFIT014-sap_doc_no,
           sap_doc_type TYPE ZSDSFIT014-sap_doc_type,
           document_no  TYPE ZSDSFIT014-document_no,
           rd_doc_type  TYPE ZSDSFIC005-rd_doc_type,
         END OF lty_bkpf,

         BEGIN OF lty_bseg,
           bukrs     TYPE bseg-bukrs,
           gjahr     TYPE bseg-gjahr,
           belnr     TYPE bseg-belnr,
           buzei     TYPE bseg-buzei,
           kunnr     TYPE bseg-kunnr,
           bupla     TYPE bseg-bupla,
           mwskz     TYPE bseg-mwskz,
           koart     TYPE bseg-koart, "INS by CH14
           augbl     TYPE bseg-augbl, "INS by CH14
           augdt     TYPE bseg-augdt, "INS by CH14
         END OF lty_bseg,

         BEGIN OF lty_map_fi,
           bukrs     TYPE bkpf-bukrs,
           blart     TYPE bkpf-blart,
           tax_code  TYPE ZSDSFIC009-tax_code,
           rd_doc_type TYPE ZSDSFIC005-rd_doc_type,
           rd_doc_typ_ds_th TYPE ZSDSFIC005-rd_doc_typ_ds_th,
           rd_doc_typ_ds_en TYPE ZSDSFIC005-rd_doc_typ_ds_en,
           rd_doc_type_grp  TYPE ZSDSFIC005-rd_doc_type_grp,
         END OF lty_map_fi,

         ltty_bkpf TYPE TABLE OF lty_bkpf,
         ltty_bseg TYPE TABLE OF lty_bseg,
         ltty_map_fi TYPE TABLE OF lty_map_fi.

  DATA: lt_bkpf   TYPE ltty_bkpf,
        lt_bseg   TYPE ltty_bseg,
        lt_map_fi TYPE ltty_map_fi,
        lt_doc_header  TYPE gtty_doc_header.

  DATA: ls_fi     LIKE LINE OF pit_fi,
        ls_bkpf   TYPE lty_bkpf,
        ls_bseg   TYPE lty_bseg,
        ls_map_fi TYPE lty_map_fi,
        ls_doc_header LIKE LINE OF lt_doc_header,
        ls_rd_doc_type LIKE LINE OF gt_rd_doc_type,
        ls_map_doc_fi  LIKE LINE OF gt_map_doc_fi,
        ls_param       LIKE LINE OF gt_param.

*  DATA: lr_etax_scope TYPE RANGE OF bkpf-blart.
*
*  FIELD-SYMBOLS <lfs_r_etax_scope> LIKE LINE OF lr_etax_scope.
*
*  LOOP AT gt_param INTO ls_param
*    WHERE id    = 'ZETX001'
*      AND name  = 'DOCTYPE_ETAXSCOPE'
*      AND param_ext = 'FI'.
*    APPEND INITIAL LINE TO lr_etax_scope ASSIGNING <lfs_r_etax_scope>.
*    <lfs_r_etax_scope>-sign   = 'I'.
*    <lfs_r_etax_scope>-option = 'EQ'.
*    <lfs_r_etax_scope>-low    = ls_param-low_value.
*  ENDLOOP.

  SELECT a~bukrs a~gjahr a~belnr a~budat
         a~blart a~usnam
         a~cpudt a~cputm a~xreversal
         e~status
         e~sap_doc_no
         e~sap_doc_type
         e~document_no
         e~rd_doc_type
    FROM bkpf AS a
    LEFT OUTER JOIN ZSDSFIT014 AS e ON a~bukrs EQ e~bukrs AND
                                           a~belnr EQ e~sap_doc_no AND
                                           a~gjahr EQ e~gjahr AND
                                           e~module_etx EQ 'FI'
*    INNER JOIN ZSDSFIC005 AS d ON e~rd_doc_type EQ d~rd_doc_type
    INTO TABLE lt_bkpf
    WHERE a~bukrs EQ p_bukrs    "Company Code
      AND a~gjahr EQ p_gjahr    "Fiscal Year
      AND a~bldat IN s_bldat    "posting Date
      AND a~blart IN s_blart    "Billing Type
      AND a~belnr IN s_belnr    "Billing Number
      AND a~usnam IN s_usnam    "Create User
      AND a~cpudt IN s_cpudt    "Create Date
      AND a~cputm IN s_cputm.    "Create Time

  CHECK lt_bkpf[] IS NOT INITIAL.

  SELECT b~bukrs b~gjahr b~belnr b~buzei
         b~kunnr b~bupla b~mwskz
         b~koart b~augbl b~augdt        "INS by CH14
    FROM bseg AS b
    INTO TABLE lt_bseg
    FOR ALL ENTRIES IN lt_bkpf
   WHERE b~bukrs EQ lt_bkpf-bukrs    "Company Code
     AND b~gjahr EQ lt_bkpf-gjahr
     AND b~belnr EQ lt_bkpf-belnr
     AND b~kunnr IN s_kunnr          "Customer
     AND b~bupla IN s_bupla.         "Business Place
   ">>>>>> BEGIN OF INSERTION: <CH14> on 13.01.2021 17:09:09 >>>>>>
   IF sy-subrc = 0.
    SELECT BUKRS
           BELNR
           GJAHR
           BUZEI
           koart
           AUGBL
           augdt
      from bseg
      INTO TABLE gt_sd_bseg
**      FOR ALL ENTRIES IN lt_sd2
      WHERE bukrs = p_bukrs
      AND   belnr IN S_BELNR
      AND   gjahr = p_gjahr.
   ENDIF.
   "<<<<<< END OF INSERTION: <CH14> on 13.01.2021 17:09:09  <<<<<<

  SELECT *
    FROM ZSDSFIT014
    INTO TABLE lt_doc_header
    FOR ALL ENTRIES IN lt_bkpf
    WHERE bukrs      EQ lt_bkpf-bukrs
     AND sap_doc_no  EQ lt_bkpf-belnr
     AND gjahr       EQ lt_bkpf-gjahr
     AND module_etx  EQ 'FI'.

  LOOP AT lt_bkpf INTO ls_bkpf.
    CLEAR ls_fi.

    ls_fi-bukrs = ls_bkpf-bukrs.
    ls_fi-gjahr = ls_bkpf-gjahr.
    ls_fi-vbeln = ls_bkpf-belnr.
    ls_fi-fkdat = ls_bkpf-budat.
    ls_fi-fkart = ls_bkpf-blart.
    ls_fi-ernam = ls_bkpf-usnam.
    ls_fi-erdat = ls_bkpf-cpudt.
    ls_fi-erzet = ls_bkpf-cputm.
    ls_fi-fksto = ls_bkpf-xreversal.
    ls_fi-status       = ls_bkpf-status.
    ls_fi-sap_doc_no   = ls_bkpf-sap_doc_no.
    ls_fi-sap_doc_type = ls_bkpf-sap_doc_type.
    ls_fi-document_no  = ls_bkpf-document_no.
    ls_fi-rd_doc_type  = ls_bkpf-rd_doc_type.

    READ TABLE gt_map_doc_fi INTO ls_map_doc_fi
      WITH KEY bukrs = ls_fi-bukrs
               blart = ls_fi-fkart.
    IF sy-subrc NE 0.
      READ TABLE gt_param INTO ls_param
        WITH KEY id	  = 'ZETX001'
                 name	= 'DOCTYPE_ETAXSCOPE'
                 param_ext = 'FI'
                 low_value = ls_fi-fkart.
        IF sy-subrc NE 0.
          CONTINUE.           "Not in etax scope
        ENDIF.
    ENDIF.

    READ TABLE lt_bseg INTO ls_bseg
      WITH KEY bukrs = ls_bkpf-bukrs
               gjahr = ls_bkpf-gjahr
               belnr = ls_bkpf-belnr.
    IF sy-subrc EQ 0.
      ls_fi-kunnr = ls_bseg-kunnr.
      ls_fi-bupla = ls_bseg-bupla.
      ls_fi-posnr = ls_bseg-buzei.
      ls_fi-mwsk1    = ls_bseg-mwskz.
      ls_fi-tax_code = ls_bseg-mwskz.
      ">>>>>> BEGIN OF INSERTION: <CH14> on 13.01.2021 13:59:13 >>>>>>
      ls_fi-koart = ls_bseg-koart.
      ls_fi-augbl = ls_bseg-augbl.
      ls_fi-augdt = ls_bseg-augdt.
      "<<<<<< END OF INSERTION: <CH14> on 13.01.2021 13:59:13  <<<<<<
      CLEAR ls_doc_header.
      READ TABLE lt_doc_header INTO ls_doc_header
        WITH KEY bukrs = ls_bkpf-bukrs
                 gjahr = ls_bkpf-gjahr
                 sap_doc_no = ls_bkpf-belnr
                 rd_doc_type = ls_bkpf-rd_doc_type.
      IF sy-subrc EQ 0.
        ls_fi-rd_doc_type     = ls_doc_header-rd_doc_type.
        ls_fi-rd_doc_type_grp = ls_doc_header-rd_doc_type_grp.
      ENDIF.

      CLEAR ls_rd_doc_type.
      READ TABLE gt_rd_doc_type INTO ls_rd_doc_type
        WITH KEY rd_doc_type = ls_doc_header-rd_doc_type.
      IF sy-subrc EQ 0.
        ls_fi-rd_doc_type_desc_th = ls_rd_doc_type-rd_doc_type_desc_th.
        ls_fi-rd_doc_type_desc_en = ls_rd_doc_type-rd_doc_type_desc_en.
      ENDIF.

      IF ls_fi-rd_doc_type_grp IS NOT INITIAL.
        IF ls_fi-rd_doc_type_grp IN gr_rd_dctypgrp.
          APPEND ls_fi TO pit_fi.
        ENDIF.
      ELSE.
        APPEND ls_fi TO pit_fi.
      ENDIF.
    ENDIF.

  ENDLOOP.

*  DELETE pit_fi WHERE rd_doc_type_grp NOT IN gr_rd_dctypgrp. "INS: <ETAX001>

ENDFORM.                    "get_fi_cluster
