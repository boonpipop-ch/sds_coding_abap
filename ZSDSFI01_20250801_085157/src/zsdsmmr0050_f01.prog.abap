*&---------------------------------------------------------------------*
*& Include          ZSDSMMI0050_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_get_constants
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_constants USING pv_prog_name TYPE sy-repid.

  CONSTANTS:
    lc_sales_grp  TYPE  zsdsde_param_name VALUE 'SALES_GRP'.

  STATICS:
    lv_read       TYPE  flag.

  DATA:
    lt_genc       TYPE  zcl_sdsca_utilities=>tt_gen_c.

  DATA:
    lv_repid   TYPE  programm.

* Check Already Read?
  IF lv_read EQ gc_true.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: gt_salegrp[].

* Assign REPID
  lv_repid = pv_prog_name.

* Read All GenC constants for program
  CALL METHOD zcl_sdsca_utilities=>get_gen_c
    EXPORTING
      if_repid = lv_repid
    IMPORTING
      et_gen_c = lt_genc.

* Mark Read Flag
  lv_read = gc_true.

* Assign GenC Constants
  LOOP AT lt_genc ASSIGNING FIELD-SYMBOL(<lfs_genc>).

    CASE <lfs_genc>-param.
      WHEN lc_sales_grp.
        "Sales Group (selection screen)
        APPEND INITIAL LINE TO gt_salegrp ASSIGNING FIELD-SYMBOL(<lfs_salegrp>).
        MOVE-CORRESPONDING <lfs_genc> TO <lfs_salegrp>.
      WHEN OTHERS.

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

  gs_data-p_sale  = p_sale.
  gs_data-prepare = p_pre.
  gs_data-vat     = p_vat.

  SELECT
     ekpo~ebeln
     ekpo~loekz
     ekpo~ebelp
     ekpo~banfn
     ekpo~menge
     ekpo~meins
     ekpo~netwr
     ekpo~werks
     ekpo~ko_prctr
  INTO TABLE gt_ekpo[]
  FROM ekpo
  WHERE ebeln IN s_ebeln
  AND loekz NE 'L'
  AND werks LIKE p_werks.

  SELECT
    ebeln
    ebelp
    vbeln
    aufnr
    prctr
  INTO TABLE gt_ekkn
  FROM ekkn
  WHERE ebeln IN s_ebeln.

  SELECT
    ebeln
    bedat
    lifnr
    ernam
    waers
  INTO TABLE gt_ekko
  FROM ekko WHERE ebeln IN s_ebeln.
  IF sy-subrc = 0.
    SORT gt_ekko[] BY ebeln.
  ENDIF.

*  SELECT
*  a~ebeln
*  a~bedat
*  a~lifnr
*  INTO TABLE gt_ekkov
*  FROM ekko AS a INNER JOIN lfb1 AS b ON ( a~lifnr = b~lifnr AND
*                                           b~bukrs = '1000' )
*  WHERE a~ebeln IN s_ebeln AND
*      ( b~kverm = 'VAT' OR
*        b~kverm = 'vat' OR
*        b~kverm = 'Vat' OR
*        b~kverm NE space ).
*
*  IF gt_ekkov[] IS INITIAL.
*    IF gs_data-vat <> '0.00'.
*      MESSAGE s000(38) WITH 'Cannot printing, Vendor master doesn''t have VAT!' DISPLAY LIKE 'E'.
*      EXIT.
*    ELSE.
*      READ TABLE li_list INTO l_value WITH KEY key = p_sale.
*      IF sy-subrc EQ 0.
*        SPLIT l_value-text AT '(' INTO : l_txtpsale l_txtpsale1.
*        l_prctr = l_txtpsale1+0(7).
*        READ TABLE gi_ekkn WITH KEY prctr = l_prctr.
*        IF sy-subrc EQ 4.
*          MESSAGE s000(38) WITH 'Profit number is incorrect !' DISPLAY LIKE 'E'.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ELSE.
*    IF gi_data-vat = '0.00'.
*      MESSAGE s000(38) WITH 'Cannot printing, Vendor master have a VAT!' DISPLAY LIKE 'E'.
*      EXIT.
*    ELSEIF gi_data-vat <> '0.07'.
*      MESSAGE s000(38) WITH 'Cannot printing, VAT should be 0.07 only!' DISPLAY LIKE 'E'.
*      EXIT.
*    ELSEIF gi_data-vat = '0.07'.
*      READ TABLE li_list INTO l_value WITH KEY key = p_sale.
*      IF sy-subrc EQ 0.
*        SPLIT l_value-text AT '(' INTO : l_txtpsale l_txtpsale1.
*        l_prctr = l_txtpsale1+0(7).
*        READ TABLE gi_ekkn WITH KEY prctr = l_prctr.
*        IF sy-subrc EQ 4.
*          MESSAGE s000(38) WITH 'Profit number is incorrect !' DISPLAY LIKE 'E'.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*  SORT gi_ekpo[] BY banfn.
*
*  LOOP AT gi_ekpo.
*    CLEAR: l_aufkprc.
*
*    READ TABLE gi_ekkn WITH KEY ebeln = gi_ekpo-ebeln
*                                ebelp = gi_ekpo-ebelp.
*    READ TABLE gi_ekko WITH KEY ebeln = gi_ekpo-ebeln.
*
*    IF gi_ekpo-banfn = lv_prno AND
*        gi_ekpo-ebeln = lv_pono.
*      g_sumpr = g_sumpr + gi_ekpo-netwr.
*      g_sumqty = g_sumqty + gi_ekpo-menge.
*
*    ELSE.
*      g_sumpr = gi_ekpo-netwr.
*      g_sumqty = gi_ekpo-menge.
*      g_no = g_no + 1.
*    ENDIF.
*
*    SELECT SINGLE * FROM zsds_gen_c
*    WHERE const EQ p_sale.
*
*    SPLIT zsds_gen_c-value AT '(' INTO : l_txtpsale l_txtpsale1.
*    l_prctr = l_txtpsale1+0(7).
*    IF l_prctr EQ gi_ekkn-prctr.
*      MOVE: gi_ekpo-ebeln TO gi_data-ebeln,
*      gi_ekpo-loekz TO gi_data-loekz,
*      gi_ekpo-ebelp TO gi_data-ebelp,
*      gi_ekpo-banfn TO gi_data-banfn,
*      gi_ekpo-menge TO gi_data-menge,
*      gi_ekpo-meins TO gi_data-meins,
*      g_sumpr TO gi_data-netwr,
*      gi_ekpo-werks TO gi_data-werks,
*      gi_ekkn-vbeln TO gi_data-vbeln,
*      gi_ekko-bedat TO gi_data-bedat,
*      gi_ekko-lifnr TO gi_data-lifnr,
*      zsds_gen_c-value TO gi_data-p_sale,
*      g_sumqty TO gi_data-menge,
*      g_no TO gi_data-gno,
*      gi_ekkn-aufnr TO gi_data-aufnr.
*
*      APPEND gi_data.
*    ENDIF.
*
*    lv_prno = gi_ekpo-banfn.
*    lv_pono = gi_ekpo-ebeln.
*
*  ENDLOOP.
*
*  SORT gi_data BY gno ebeln banfn netwr DESCENDING.
*
*  DELETE ADJACENT DUPLICATES FROM gi_data COMPARING ebeln banfn.
*
*  LOOP AT gi_data.
*
*    SELECT SINGLE * FROM vbak WHERE vbeln = gi_data-vbeln.
*
*    IF sy-subrc = 0.
*      MOVE: vbak-kunnr TO gi_data-kunnr,
*      vbak-auart TO gi_data-auart,
*      vbak-vgbel TO gi_data-vgbel,
*      vbak-vgbel TO thead-tdname.
*
*      CALL FUNCTION 'READ_TEXT'
*        EXPORTING
*          id                      = 'Z009'
*          language                = 'E'
*          name                    = thead-tdname
*          object                  = 'VBBK'
*        TABLES
*          lines                   = text
*        EXCEPTIONS
*          id                      = 1
*          language                = 2
*          name                    = 3
*          not_found               = 4
*          object                  = 5
*          reference_check         = 6
*          wrong_access_to_archive = 7
*          OTHERS                  = 8.
*      IF sy-subrc EQ 0.
*        LOOP AT text.
*          MOVE TEXT-tdline TO gi_data-text.
*        ENDLOOP.
*      ENDIF.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM viaufks WHERE aufnr = gi_data-aufnr.
*    IF sy-subrc = 0.
*      MOVE:viaufks-objnr TO gi_data-objnr,
*      viaufks-kunum TO gi_data-kunum,
*      viaufks-auart TO gi_data-auart2,
*      viaufks-kdauf TO gi_data-jobno,
*      viaufks-aufpl TO gi_data-aufpl.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM afko WHERE aufnr = gi_data-aufnr2.
*    IF sy-subrc = 0 AND gi_data-jobno = ''.
*
*      MOVE: afko-maufnr TO gi_data-jobno.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM afih WHERE aufnr = gi_data-aufnr.
*    IF sy-subrc = 0.
*      MOVE:afih-qmnum TO gi_data-qmnum. "Notification
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM viqmfe WHERE qmnum = gi_data-qmnum.
*    IF sy-subrc = 0.
*      MOVE:viqmfe-fecod TO gi_data-fecod. "Demage
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT * FROM afvu WHERE aufpl = gi_data-aufpl.
*      IF sy-subrc = 0 AND afvu-usr05 NE ' '.
*        MOVE:afvu-usr05 TO gi_data-usr05. " Volumn
*        MODIFY gi_data.
*      ENDIF.
*    ENDSELECT.
*
*    SELECT SINGLE * FROM t003p WHERE auart = gi_data-auart2 AND spras = 'EN'. "modify 211209
*    IF sy-subrc = 0 AND gi_data-auart2 NE 'ZV21'.
*
*      MOVE: t003p-txt TO gi_data-svotxt.
*      MODIFY gi_data.
*    ELSE.
*      gi_data-svotxt = 'Repair In Warranty'.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM vbpa WHERE vbeln = gi_data-vbeln.
*    SELECT SINGLE * FROM tvakt WHERE auart = gi_data-auart AND spras ='EN'. "S/O Doc type for link with Description Doc type
*
*    IF sy-subrc = 0.
*      MOVE:vbpa-adrnr TO gi_data-adrnr,
*      tvakt-bezei TO gi_data-bezei,
*      kna1-name1 TO gi_data-cusname_svo.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM lfa1 WHERE lifnr = gi_data-lifnr.
*    IF sy-subrc = 0.
*
*      CONCATENATE lfa1-name1 lfa1-name2 INTO gi_data-name2  SEPARATED BY space.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM kna1 WHERE kunnr = gi_data-kunum.
*    IF sy-subrc = 0.
*      MOVE: kna1-name1 TO gi_data-cusname_svo.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM ihpa WHERE objnr = gi_data-objnr AND parvw ='AG'.
*    IF sy-subrc = 0.
*      MOVE: ihpa-adrnr TO gi_data-adrnr2.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM pmsdo WHERE objnr = gi_data-objnr.
*    IF sy-subrc = 0.
*      MOVE: pmsdo-vkbur TO gi_data-vkbur,
*      pmsdo-vkgrp TO gi_data-vkgrp.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM adrc WHERE addrnumber = gi_data-adrnr OR addrnumber = gi_data-adrnr2 AND nation =''.
*    IF sy-subrc = 0.
*      MOVE:adrc-name1 TO gi_data-name1.
*      MODIFY gi_data.
*    ELSE.
*      MOVE:gi_data-cusname_svo TO gi_data-name1.
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM ztcs_header_job WHERE vkbur = gi_data-vkbur AND vkgrp = gi_data-vkgrp.
*    IF sy-subrc = 0 AND gi_data-maufnr NE''.
*
*      MOVE: ztcs_header_job-header_job TO gi_data-header_job.
*
*      MODIFY gi_data.
*    ENDIF.
*
*    SELECT SINGLE * FROM jest WHERE objnr = gi_data-objnr AND stat = 'I0045' AND inact =''.
*    IF sy-subrc = 0.
*      gi_data-status = 'TECO'.
*      MODIFY gi_data.
*    ELSE.
*      gi_data-status = 'None'.
*      MODIFY gi_data.
*    ENDIF.
*
*    READ TABLE gi_ekko INTO gs_ekko
*                       WITH KEY ebeln = gi_data-ebeln
*                       BINARY SEARCH.
*    IF sy-subrc = 0.
*      gs_header-ernam = gs_ekko-ernam.
*      gs_header-waers = gs_ekko-waers.
*    ENDIF.
*    gs_header-ebeln  = gi_data-ebeln.
*    gs_header-vendor = gi_data-lifnr.
*    gs_header-name   = gi_data-name2.
*    gs_header-aufnr  = gi_data-aufnr.
*    gs_header-sale   = gi_data-p_sale.
*    gs_header-po     = gi_data-ebeln.
*    gs_header-date1  = gi_data-bedat.
*    APPEND gs_header TO gt_header.
*    CLEAR gs_header.
*  ENDLOOP.
*
*  SELECT a~mblnr a~mjahr a~zeile a~bwart a~aufnr a~matnr b~maktx a~dmbtr
*         a~sjahr a~smbln a~smblp
*    INTO TABLE gt_mseg
*    FROM mseg AS a INNER JOIN makt AS b ON ( a~matnr = b~matnr )
*    FOR ALL ENTRIES IN gi_data
*    WHERE a~aufnr EQ gi_data-aufnr AND
*          a~ebeln EQ gi_data-ebeln AND
*          a~kzbew EQ 'B' AND
*          b~spras EQ 'E'.
*
*  LOOP AT gt_mseg INTO wa_mseg WHERE smbln NE space AND
*                                     smblp NE space.
*
*
*    DELETE gt_mseg WHERE mblnr = wa_mseg-smbln AND
*                         mjahr = wa_mseg-sjahr AND
*                         zeile = wa_mseg-smblp.
*
*  ENDLOOP.
*
*  DELETE gt_mseg WHERE smbln NE space AND smblp NE space.
*  SORT gt_mseg BY aufnr.
*
*  LOOP AT gt_mseg INTO wa_mseg.
*    gs_header-matd   = wa_mseg-mblnr.
*    MODIFY gt_header FROM gs_header TRANSPORTING matd
*                                    WHERE aufnr = wa_mseg-aufnr.
*  ENDLOOP.
*
*  CHECK gt_mseg[] IS NOT INITIAL.
*  SELECT DISTINCT a~vbeln a~fkdat b~aufnr INTO TABLE gt_vbrp
*    FROM vbrk AS a INNER JOIN vbrp AS b ON ( a~vbeln = b~vbeln )
*    FOR ALL ENTRIES IN gt_mseg
*    WHERE b~aufnr EQ gt_mseg-aufnr.
*  SORT gt_header BY ebeln.
*  DELETE ADJACENT DUPLICATES FROM gt_header COMPARING ebeln.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_call_form
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_call_form .

  DATA lc_smart_forms TYPE string VALUE 'ZSF_ESTIMATE_MA'.

*  DATA: fm_name     TYPE rs38l_fnam,
*        lv_formname TYPE tdsfname.
*
*  DATA : lv_total_line TYPE i,
*         count         TYPE ssfctrlop.
*
*  DATA : ls_header TYPE gy_header.
*
*  DATA : lv_tabix TYPE sy-tabix,
*         lv_line  TYPE i.
*
*  lv_formname = lc_smart_forms.
*
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname           = lv_formname
*    IMPORTING
*      fm_name            = fm_name
*    EXCEPTIONS
*      no_form            = 1
*      no_function_module = 2
*      OTHERS             = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  LOOP AT gt_header INTO ls_header.
*    ADD 1 TO lv_total_line.
*    CLEAR ls_header.
*  ENDLOOP.
*
*  LOOP AT gt_header INTO ls_header.
*    lv_tabix = sy-tabix.
*    CLEAR gt_header_smart[].
*    APPEND ls_header TO gt_header_smart.
**BOI CH05
*    PERFORM f_check_amount_loa  USING ls_header-ebeln
*                                      ls_header-ernam
*                                      ls_header-waers
*                                      gi_ekpo[].
**EOI CH05
*    IF lv_total_line GT 1.
*      IF     lv_tabix = 1.                "FISRT CALL
*        count-no_open   = space .
*        count-no_close  = 'X' .
*      ELSEIF lv_tabix   = lv_total_line.  "LAST CALL
*        count-no_open   = 'X' .
*        count-no_close  = space .
*      ELSE.                               "OTHER CALLS
*        count-no_open   = 'X' .
*        count-no_close  = 'X' .
*      ENDIF.
*    ENDIF.
*
*    CALL FUNCTION fm_name
*      EXPORTING
*        control_parameters = count
*        user_settings      = 'X'
*        iv_mng             = gv_mng
*        iv_gm              = gv_gm
*        iv_agm             = gv_agm
*        iv_amd             = gv_amd
*        iv_sd              = gv_sd
*        iv_md              = gv_md
*        iv_pres            = gv_pres
*        iv_namemng         = gv_namemng
*        iv_datemng         = gv_datemng
*        iv_namegm          = gv_namegm
*        iv_nameagm         = gv_nameagm
*        iv_dategm          = gv_dategm
*        iv_dateagm         = gv_dateagm
*        iv_nameamd         = gv_nameamd
*        iv_dateamd         = gv_dateamd
*        iv_namesd          = gv_namesd
*        iv_datesd          = gv_datesd
*        iv_namemd          = gv_namemd
*        iv_datemd          = gv_datemd
*        iv_namepres        = gv_namepres
*        iv_datepres        = gv_datepres
*        iv_namereq         = gv_namereq
*        iv_datereq         = gv_datereq
*      EXCEPTIONS
*        formatting_error   = 1
*        internal_error     = 2
*        send_error         = 3
*        user_canceled      = 4
*        OTHERS             = 5.
*
*  ENDLOOP.

ENDFORM.
