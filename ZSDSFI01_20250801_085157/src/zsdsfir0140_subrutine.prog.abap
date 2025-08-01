*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0140_SUBRUTINE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DOCNR  text
*----------------------------------------------------------------------*
FORM f_get_data TABLES
                  i_header STRUCTURE w_header
                  i_detail STRUCTURE w_detail
                  i_vbap STRUCTURE w_vbap
                  i_konv STRUCTURE w_konv
                  i_detail_old STRUCTURE w_detail
                USING
                  l_docnr TYPE ZSDSFIT006-docnr.

  CHECK l_docnr IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_docnr
    IMPORTING
      output = l_docnr.

* Header
  SELECT
      docnr
      vbeln
      edatu
      aufnr
      bstkd
      doctype
      reftype
      pmnttyp
      prcnt
      vat
      whtax
      kwert " Add by aphai on 06.11.2015
      docdate
      kunnr
      name1
      mgrnr
      posdesc
      topic
      attend
      zterm
      erdat
      ertim
      ernam
  INTO CORRESPONDING FIELDS OF TABLE i_header
   FROM ZSDSFIT006
   WHERE docnr EQ l_docnr
   AND dflag EQ space.

* Detail

  CLEAR w_header.
  READ TABLE i_header INTO w_header INDEX 1.

  IF sy-subrc EQ 0.

    IF w_header-reftype NE 'UE'.
*      Create with referenece QT/SO
      SELECT
            d~docnr
            lstnr
            d~vbeln
            d~posnr
            d~edatu
            d~matnr
            d~maktx
            d~kwmeng
            p~kwmeng AS kwmeng_o
            d~netpr
            d~netwr
            d~kwert
            k~knumv
            marm~meinh
            marm~umrez
            marm~umren
        INTO CORRESPONDING FIELDS OF TABLE i_detail
         FROM ZSDSFIT007 AS d
              LEFT JOIN marm ON d~matnr EQ marm~matnr
              LEFT JOIN ZSDSFIT010 AS p ON d~vbeln EQ p~vbeln
                                       AND d~posnr EQ p~posnr
                                       AND d~docnr EQ p~docnr
                                       AND d~erdat EQ p~erdat
                                       AND d~ertim EQ p~ertim
             LEFT JOIN ZSDSFIT009 AS k ON d~vbeln EQ k~vbeln
                                       AND d~docnr EQ k~docnr
                                       AND d~erdat EQ k~erdat
                                       AND d~ertim EQ k~ertim
         WHERE d~docnr EQ l_docnr
         AND dflag EQ space.
      DELETE i_detail WHERE
                     matnr NE 'REMARK'
                     AND matnr IS NOT INITIAL
                     AND ( umrez NE '1.00' OR umren NE '1.00' ).
      SORT i_detail BY vbeln posnr matnr maktx.
    ELSE.

*      User keys IN
      SELECT
            d~docnr
            d~lstnr
            d~vbeln
            d~posnr
            d~matnr
            d~maktx
            d~kwmeng
            d~netpr
            d~netwr
            d~kwert
        INTO CORRESPONDING FIELDS OF TABLE i_detail
         FROM ZSDSFIT007 AS d
         WHERE d~docnr EQ l_docnr
         AND dflag EQ space.
    ENDIF.
    SORT i_detail BY lstnr.
  ENDIF.

* konv
  SELECT
          knumv
          kposn
          kschl
          k~kwert
    INTO CORRESPONDING FIELDS OF TABLE i_konv
    FROM ZSDSFIT006 AS h
    INNER JOIN ZSDSFIT011 AS k ON h~docnr EQ k~docnr
                                  AND h~erdat EQ k~erdat
                                  AND h~ertim EQ k~ertim
    WHERE h~docnr EQ l_docnr
    AND h~dflag EQ space
    AND ( k~kschl EQ 'ZDP3' OR k~kschl EQ 'ZDP2' ).


* vbap
  SELECT
      h~vbeln
      p~posnr
      p~matnr
      p~kwmeng
      p~netpr
 INTO CORRESPONDING FIELDS OF TABLE i_vbap
 FROM ZSDSFIT006 AS h
 INNER JOIN ZSDSFIT007 AS d ON h~docnr EQ d~docnr
 INNER JOIN ZSDSFIT010 AS p ON d~docnr EQ p~docnr
                                AND d~vbeln EQ p~vbeln
                                AND d~posnr EQ p~posnr
                                AND d~erdat EQ p~erdat
                                AND d~ertim EQ p~ertim
 .

  IF i_vbap[] IS NOT INITIAL.
    LOOP AT i_vbap INTO w_vbap.
      MOVE-CORRESPONDING w_vbap TO w_detail_old.
      APPEND w_detail_old TO i_detail_old.
    ENDLOOP.
  ENDIF.



ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  f_cal_amt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM f_cal_amt TABLES it STRUCTURE w_detail.

  CHECK it[] IS NOT INITIAL.
  FIELD-SYMBOLS : <fs> LIKE LINE OF it.
  CLEAR : txt_total_amt1,txt_adv,txt_total_qty.

  LOOP AT it ASSIGNING <fs>.
    txt_total_amt1 = txt_total_amt1 +  <fs>-netwr.
    txt_adv = txt_adv + <fs>-kwert.
    txt_total_qty = txt_total_qty + <fs>-kwmeng.
  ENDLOOP.

  txt_total_amt2 = txt_total_amt1 + txt_adv.

  IF chk_vat EQ 'X'.
    txt_amt_inc_vat = txt_total_amt2 * ( txt_vat / 100 ).
  ELSE.
    txt_amt_inc_vat = 0.
  ENDIF.

  IF chk_wht EQ 'X'.
    txt_amt_inc_wht = 0 - ( txt_total_amt2 * ( txt_wht / 100 ) ).
  ELSE.
    txt_amt_inc_wht = 0.
  ENDIF.

  txt_grandtotal =  txt_total_amt2 + txt_amt_inc_vat  + txt_amt_inc_wht.



ENDFORM.                    " F_CAL_AMT

*&---------------------------------------------------------------------*
*&      Form  f_simulate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_simulate.

  IF txt_zterm NE txt_zterm_o.
    PERFORM f_get_term_desc USING txt_zterm CHANGING txt_pmnt_desc
                                                      txt_pmnt_typ.
  ENDIF.

  PERFORM f_cal_percentage_of_doc TABLES i_detail[].
  PERFORM f_cal_amt TABLES i_detail[].
  PERFORM f_cal_amt_bom IN PROGRAM zar_msbill_cred_chg IF FOUND  TABLES i_detail[].

* Keep old value
  txt_vbeln_o = txt_vbeln.
  txt_edatu_o = txt_edatu.
  txt_zterm_o = txt_zterm.
  txt_pmnt_typ_o = txt_pmnt_typ.
  lst_mgrnr_o = lst_mgrnr.
  lst_doctype_o = lst_doctype.
  txt_vat_o = txt_vat.
  txt_wht_o = txt_wht.

ENDFORM.                    "F_SIMULATE

*&---------------------------------------------------------------------*
*&      Form  f_get_term_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TXT_ZTERM      text
*      -->P_TXT_PMNT_DESC  text
*      -->P_TXT_PMNT_TYP   text
*----------------------------------------------------------------------*
FORM f_get_term_desc  USING    p_txt_zterm TYPE vbkd-zterm
                      CHANGING p_txt_pmnt_desc TYPE t052u-text1
                               p_txt_pmnt_typ TYPE ZSDSFIT006-pmnttyp.

  CLEAR p_txt_pmnt_desc.
  CHECK p_txt_zterm IS NOT INITIAL.
  SELECT SINGLE text1 INTO p_txt_pmnt_desc
    FROM t052u
    WHERE zterm EQ p_txt_zterm
    AND spras EQ 'E'.

  IF sy-subrc NE 0.
    CLEAR : p_txt_zterm,p_txt_pmnt_desc.
  ENDIF.
ENDFORM.                    "f_get_term_desc

*&---------------------------------------------------------------------*
*&      Form  f_cal_percentage_of_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM f_cal_percentage_of_doc  TABLES it STRUCTURE w_detail.
  CHECK it[] IS NOT INITIAL.

  SORT i_detail_old BY add vbeln posnr.
  SORT it BY add vbeln posnr.

  FIELD-SYMBOLS : <fs> LIKE LINE OF it,
                  <fs2> LIKE LINE OF it.

  LOOP AT i_detail ASSIGNING <fs>.
    READ TABLE i_detail_old WITH KEY vbeln = <fs>-vbeln
                                     posnr = <fs>-posnr
               ASSIGNING <fs2>.
    IF sy-subrc EQ 0.
      <fs>-netpr = ( <fs2>-netpr * txt_prcnt ) / 100.
      <fs>-netwr = <fs>-netpr * <fs>-kwmeng.
    ENDIF.

* Calculate Advance Reciept
    <fs>-kwert = 0.
    LOOP AT i_konv INTO w_konv WHERE knumv = <fs>-knumv AND kposn = <fs>-posnr.
      <fs>-kwert = <fs>-kwert + w_konv-kwert.
    ENDLOOP.

    READ TABLE i_vbap INTO w_vbap WITH KEY vbeln = <fs>-vbeln
                                          posnr = <fs>-posnr.
    IF sy-subrc EQ 0.
      <fs>-kwert = <fs>-kwert * ( <fs>-kwmeng / w_vbap-kwmeng ).
      <fs>-kwert = ( <fs>-kwert * txt_prcnt ) / 100.
    ENDIF.

    UNASSIGN : <fs2>.
  ENDLOOP.
ENDFORM.                    " F_CAL_PERCENTAGE_OF_DOC

*&---------------------------------------------------------------------*
*&      Form  f_chek_require_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R          text
*----------------------------------------------------------------------*
FORM f_chek_require_field  CHANGING r TYPE n.
  r = 0.
  DATA : itm TYPE i VALUE 0.

  IF lst_doctype IS INITIAL.
    MESSAGE s000(38) WITH 'Please input document type!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_docdate IS INITIAL.
    MESSAGE s000(38) WITH 'Please input document date!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_topic IS INITIAL.
    MESSAGE s000(38) WITH 'Please input topic!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_vbeln IS INITIAL.
    MESSAGE s000(38) WITH 'Please input sale order or quotation!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_attend IS INITIAL.
    MESSAGE s000(38) WITH 'Please input attention!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_name1 IS INITIAL.
    MESSAGE s000(38) WITH 'Please input customer name!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_zterm IS INITIAL.
    MESSAGE s000(38) WITH 'Please input payment terms!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_aufnr IS INITIAL.
    r = 1.
    RETURN.
  ENDIF.

  IF lst_mgrnr IS INITIAL.
    MESSAGE s000(38) WITH 'Please input manager!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_pos IS INITIAL.
    MESSAGE s000(38) WITH 'Please input position!' DISPLAY LIKE 'E'.
    r = 1 .
    RETURN.
  ENDIF.

  IF txt_prcnt IS INITIAL.
    MESSAGE s000(38) WITH 'Please input percentage of billing document!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  LOOP AT i_detail INTO w_detail.
    itm = itm + 1.
  ENDLOOP.

  IF itm EQ 0.
    MESSAGE s000(38) WITH 'Please input billing items!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.


  CLEAR itm.
ENDFORM.                    " F_CHEK_REQUIRE_FIELD

*&---------------------------------------------------------------------*
*&      Form  f_tc_detail_200_lines
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_tc_detail_200_lines .
  DATA rows TYPE i.
  rows = LINES( i_detail_200[] ).
  IF rows LT 7.
    rows = 7.
  ELSE.
    rows = rows + 7.
  ENDIF.
  tc_detail_200-lines = rows.
ENDFORM.                    " F_TC_DETAIL_200_LINES


*&---------------------------------------------------------------------*
*&      Form  f_maktx_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MAKTX    text
*      -->V_MATNR    text
*      -->V_MAKTX    text
*----------------------------------------------------------------------*
FORM f_maktx_desc  USING    p_maktx TYPE ZSDSFIT007-maktx
                   CHANGING v_matnr TYPE mara-matnr
                            v_maktx TYPE ZSDSFIT007-maktx.
  CHECK p_maktx IS NOT INITIAL.
  DATA : l_matnr TYPE mara-matnr,
         l_maktx TYPE makt-maktx,
         len TYPE i.

  len = STRLEN( p_maktx ).

  IF len LE 18.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_maktx
      IMPORTING
        output = l_matnr.
    SELECT SINGLE maktx INTO l_maktx
      FROM makt
      WHERE matnr EQ l_matnr
      AND spras EQ 'E'.
    IF sy-subrc EQ 0.
      v_matnr = l_matnr.
      v_maktx = l_maktx.
    ELSE.
      v_matnr = space.
      v_maktx = p_maktx.
    ENDIF.
  ELSE.
    v_matnr = space.
    v_maktx = p_maktx.
  ENDIF.

ENDFORM.                    " F_MAKTX_DESC

*&---------------------------------------------------------------------*
*&      Form  f_cal_amt_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM f_cal_amt_200  TABLES   it STRUCTURE w_detail_200.

  CHECK it[] IS NOT INITIAL.
  FIELD-SYMBOLS : <fs> LIKE LINE OF it.
  CLEAR : txt_total_amt1,txt_total_qty.

  LOOP AT it ASSIGNING <fs>.
    txt_total_amt1 = txt_total_amt1 +  <fs>-netwr.
*    txt_adv = txt_adv + <fs>-kwert.
    txt_total_qty = txt_total_qty + <fs>-kwmeng.
  ENDLOOP.

  txt_total_amt2 = txt_total_amt1 + txt_adv.

  IF chk_vat EQ 'X'.
    txt_amt_inc_vat = txt_total_amt2 * ( txt_vat / 100 ).
  ELSE.
    txt_amt_inc_vat = 0.
  ENDIF.

  IF chk_wht EQ 'X'.
    txt_amt_inc_wht = 0 - ( txt_total_amt2 * ( txt_wht / 100 ) ).
  ELSE.
    txt_amt_inc_wht = 0.
  ENDIF.

  txt_grandtotal =  txt_total_amt2 + txt_amt_inc_vat  + txt_amt_inc_wht.
ENDFORM.                    " F_CAL_AMT_200

*&---------------------------------------------------------------------*
*&      Form  f_simulate_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_simulate_200 .

  IF txt_zterm NE txt_zterm_o.
    PERFORM f_get_term_desc IN PROGRAM zar_msbill_cred IF FOUND
                                                    USING
                                                      txt_zterm
                                                    CHANGING
                                                      txt_pmnt_desc
                                                      txt_pmnt_typ.
  ENDIF.

  IF txt_kunnr NE txt_kunnr_o.
    PERFORM f_get_kunnr_desc IN PROGRAM zar_msbill_cred IF FOUND USING txt_kunnr CHANGING txt_name1.
  ENDIF.

  IF txt_aufnr NE txt_aufnr_o.
    PERFORM f_get_aufnr_desc IN PROGRAM zar_msbill_cred IF FOUND USING txt_aufnr CHANGING txt_ktext.
  ENDIF.

  PERFORM f_cal_amt_200 TABLES i_detail_200[].

* Keep old value
  txt_kunnr_o = txt_kunnr.
  txt_aufnr_o = txt_aufnr.
  txt_zterm_o = txt_zterm.
  txt_pmnt_typ_o = txt_pmnt_typ.
  lst_mgrnr_o = lst_mgrnr.
  lst_doctype_o = lst_doctype.
  txt_vat_o = txt_vat.
  txt_wht_o = txt_wht.


ENDFORM.                    " F_SIMULATE_200

*&---------------------------------------------------------------------*
*&      Form  f_assign_pos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MGRNR    text
*      -->P_POS      text
*----------------------------------------------------------------------*
FORM f_assign_pos  USING    p_mgrnr
                   CHANGING p_pos.

  READ TABLE i_apv INTO w_apv WITH KEY pernr = p_mgrnr.
  IF sy-subrc EQ 0.
    p_pos = w_apv-posdesc.
  ENDIF.

ENDFORM.                    " F_ASSIGN_POS

*&---------------------------------------------------------------------*
*&      Form  f_chek_require_field_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R          text
*----------------------------------------------------------------------*
FORM f_chek_require_field_200  CHANGING r TYPE n.
  r = 0.
  DATA : itm TYPE i VALUE 0.

  IF lst_doctype IS INITIAL.
    MESSAGE s000(38) WITH 'Please input document type!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_docdate IS INITIAL.
    MESSAGE s000(38) WITH 'Please input document date!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_topic IS INITIAL.
    MESSAGE s000(38) WITH 'Please input topic!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.


  IF txt_attend IS INITIAL.
    MESSAGE s000(38) WITH 'Please input attention!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_name1 IS INITIAL.
    MESSAGE s000(38) WITH 'Please input customer name!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_zterm IS INITIAL.
    MESSAGE s000(38) WITH 'Please input payment terms!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF lst_mgrnr IS INITIAL.
    MESSAGE s000(38) WITH 'Please input manager!' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.

  IF txt_pos IS INITIAL.
    MESSAGE s000(38) WITH 'Please input position!' DISPLAY LIKE 'E'.
    r = 1 .
    RETURN.
  ENDIF.


  DELETE i_detail_200 WHERE maktx IS INITIAL
                       OR netpr IS  INITIAL
                       OR kwmeng IS INITIAL.

  LOOP AT i_detail_200 INTO w_detail_200.
    itm = itm + 1.
  ENDLOOP.

  IF itm EQ 0.
    MESSAGE s000(38) WITH 'Please input billing line items !' DISPLAY LIKE 'E'.
    r = 1.
    RETURN.
  ENDIF.


  CLEAR itm.
ENDFORM.                    " F_CHEK_REQUIRE_FIELD_200
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_BILL_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL_200[]  text
*----------------------------------------------------------------------*
FORM f_save_changed_bill_200  TABLES   it STRUCTURE w_detail_200.
  CHECK it[] IS NOT INITIAL.
  DATA : rsel TYPE i,
        msg(100) TYPE c.
  FIELD-SYMBOLS : <fsd> LIKE LINE OF it.

  CLEAR : i_detail_db[],w_header.
* prepare header data

  txt_aedat = sy-datum.
  txt_aetim = sy-uzeit.
  txt_aenam = sy-uname.


  IF chk_vat NE 'X'.
    CLEAR txt_vat.
  ENDIF.

  IF chk_wht NE 'X'.
    CLEAR txt_wht.
  ENDIF.


  READ TABLE i_header INTO w_header INDEX 1.

* Prepare detail
  LOOP AT it ASSIGNING <fsd>.
    MOVE-CORRESPONDING <fsd> TO w_detail_db.
    IF <fsd>-add IS INITIAL.
      w_detail_db-docnr = txt_docnr.
      w_detail_db-lstnr = sy-tabix.
      w_detail_db-aedat = txt_aedat.
      w_detail_db-aetim = txt_aetim.
      w_detail_db-aenam = txt_aenam.
      w_detail_db-erdat = w_header-erdat.
      w_detail_db-ertim = w_header-ertim.
      w_detail_db-ernam = w_header-ernam.
    ELSE.
      w_detail_db-docnr = txt_docnr.
      w_detail_db-lstnr = sy-tabix.
      w_detail_db-aedat = txt_aedat.
      w_detail_db-aetim = txt_aetim.
      w_detail_db-aenam = txt_aenam.
      w_detail_db-erdat = txt_aedat.
      w_detail_db-ertim = txt_aetim.
      w_detail_db-ernam = txt_aenam.
    ENDIF.
    APPEND w_detail_db TO i_detail_db.
    CLEAR w_detail_db.
  ENDLOOP.


  TRY.

      UPDATE ZSDSFIT006       SET
                                bstkd = txt_bstkd
                                aufnr = txt_aufnr
                                doctype = lst_doctype
                                pmnttyp = txt_pmnt_typ
                                prcnt = txt_prcnt
                                vat = txt_vat
                                whtax = txt_wht
                                docdate = txt_docdate
                                kunnr = txt_kunnr
                                name1 = txt_name1
                                mgrnr = lst_mgrnr
                                posdesc = txt_pos
                                topic = txt_topic
                                attend = txt_attend
                                zterm = txt_zterm
                                aedat = txt_aedat
                                aetim = txt_aetim
                                aenam = txt_aenam
                                WHERE docnr EQ txt_docnr.

      DELETE FROM ZSDSFIT007 WHERE docnr EQ txt_docnr.
      INSERT ZSDSFIT007 FROM TABLE i_detail_db.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CONCATENATE 'Change billing No :' txt_docnr 'complete!' INTO msg SEPARATED BY space.
      MESSAGE s000(38) WITH msg DISPLAY LIKE 'S'.

      LEAVE TO SCREEN 0.
    CATCH cx_sy_dynamic_osql_error INTO oref.
      gv_message = oref->get_text( ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE e000(38) WITH gv_message.
      EXIT.
  ENDTRY.

ENDFORM.                    " F_CHANGE_BILL_200

*&---------------------------------------------------------------------*
*&      Form  f_del_remark
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM f_del_remark  TABLES   it STRUCTURE w_detail.
  DELETE it WHERE sel EQ 'X'.
  PERFORM f_simulate.
ENDFORM.                    " F_DEL_REMARK


*&---------------------------------------------------------------------*
*&      Form  f_assign_header_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM f_assign_header_data  TABLES it STRUCTURE w_header.

  CLEAR : txt_attend,txt_kunnr,txt_name1,txt_zterm,txt_pmnt_desc,txt_bstkd,txt_aufnr,txt_ktext.
  CHECK it[] IS NOT INITIAL.
  READ TABLE it INTO w_header INDEX 1.


  lst_doctype = w_header-doctype.
  txt_docnr = w_header-docnr.
  txt_vbeln = w_header-vbeln.
  txt_edatu =  w_header-edatu.
  txt_attend = w_header-attend.
  txt_kunnr = w_header-kunnr.
  txt_name1 = w_header-name1.
  txt_zterm = w_header-zterm.
  txt_vat = w_header-vat.
  txt_wht = w_header-whtax.
  txt_prcnt = w_header-prcnt.
  txt_topic = w_header-topic.
  txt_pmnt_typ = w_header-pmnttyp.
  txt_docdate = w_header-docdate.

  IF w_header-vat IS NOT INITIAL.
    chk_vat = 'X'.
  ELSE.
    txt_vat = 7.
  ENDIF.

  IF w_header-whtax IS NOT INITIAL.
    chk_wht = 'X'.
  ELSE.
    txt_wht = 3.
  ENDIF.

  txt_adv = w_header-kwert. " add by aphai 06.11.2015

  PERFORM f_get_term_desc
              USING
                 w_header-zterm
              CHANGING
                 txt_pmnt_desc
                 txt_pmnt_typ.

  txt_bstkd = w_header-bstkd.
  txt_aufnr = w_header-aufnr.

  PERFORM f_get_aufnr_desc IN PROGRAM zar_msbill_cred IF FOUND USING w_header-aufnr CHANGING txt_ktext.

  txt_pos = w_header-posdesc.
  lst_mgrnr = w_header-mgrnr.


ENDFORM.                    " F_ASSIGN_HEADER_DATA

*&---------------------------------------------------------------------*
*&      Form  f_assign_manager
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_assign_manager .

  IF i_apv[] IS INITIAL.
    SELECT * INTO TABLE i_apv FROM ZSDSFIT008 WHERE spras EQ '2' AND ( pos EQ '3' OR pos EQ '4' ).

    name = 'LST_MGRNR'.
    LOOP AT i_apv INTO w_apv. "GM Only
      value-key = w_apv-pernr.
      CONCATENATE w_apv-vorna w_apv-nachn INTO value-text SEPARATED BY space.
      APPEND value TO list.
    ENDLOOP.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list.
    CLEAR:name,list,value.
  ENDIF.


ENDFORM.                    " F_ASSIGN_MANAGER

*&---------------------------------------------------------------------*
*&      Form  f_add_remark
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM f_add_remark  TABLES   it STRUCTURE w_detail.
  CHECK it[] IS NOT INITIAL.
  DATA : wa LIKE LINE OF it.

**  wa-posnr = '999999'.
**  wa-matnr = 'REMARK'.
*
*  wa-vbeln = txt_vbeln.
*  wa-posnr = ''.
*  wa-matnr = ''.
*  wa-kwmeng = 0.
*  wa-add = 'X'.
*  APPEND wa TO it.
*  CLEAR wa.

  wa-vbeln = txt_vbeln.
  wa-posnr = '999999'.
  wa-matnr = 'REMARK'.
  wa-kwmeng = 1.
  wa-add = 'X'.
  APPEND wa TO it.
  CLEAR wa.
ENDFORM.                    " F_ADD_REMARK
*&---------------------------------------------------------------------*
*&      Form  F_GET_VBAP_POP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBAP_POP  text
*      -->P_TXT_VBELN  text
*----------------------------------------------------------------------*
FORM f_get_vbap_pop  TABLES   it STRUCTURE w_vbap_pop
                     USING    l_vbeln TYPE vbap-vbeln.
  CHECK l_vbeln IS NOT INITIAL.
  CLEAR it[].
  FIELD-SYMBOLS : <fs> LIKE LINE OF i_detail,
                  <fs1> LIKE LINE OF it.
  DATA : wtmp LIKE LINE OF it,
         it_konv TYPE STANDARD TABLE OF typ_konv.
  SELECT
    vbap~vbeln
    vbap~posnr
    vbap~matnr
    makt~maktx
    vbap~netpr
    vbap~kwmeng
    vbap~netwr
    vbak~knumv
  INTO CORRESPONDING FIELDS OF TABLE it
  FROM vbap
   INNER JOIN vbak ON vbap~vbeln EQ vbak~vbeln
   INNER JOIN makt ON vbap~matnr EQ makt~matnr AND makt~spras EQ 'E'
   WHERE vbap~vbeln EQ l_vbeln.

  DELETE it WHERE matnr+0(1) EQ 'S'.

  IF i_detail[] IS NOT INITIAL.
    LOOP AT i_detail ASSIGNING <fs>.
      READ TABLE it INTO wtmp WITH KEY vbeln = <fs>-vbeln posnr = <fs>-posnr.
      IF sy-subrc EQ 0.
        DELETE it WHERE vbeln EQ <fs>-vbeln AND posnr EQ <fs>-posnr.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CHECK it[] IS NOT INITIAL.
  SELECT
        knumv
        kposn
        kschl
        kwert
  INTO TABLE it_konv
  FROM konv
  FOR ALL ENTRIES IN it
  WHERE konv~knumv EQ it-knumv
  AND   konv~kposn EQ it-posnr
  AND ( konv~kschl EQ 'ZDP3' OR konv~kschl EQ 'ZDP2' ).

* Calculate Adv
  IF i_konv[] IS NOT INITIAL.
    LOOP AT it ASSIGNING <fs1>.
      LOOP AT i_konv INTO w_konv WHERE knumv = <fs1>-knumv AND kposn = <fs1>-posnr.
        <fs1>-kwert = <fs1>-kwert + w_konv-kwert.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_GET_VBAP_POP
*&---------------------------------------------------------------------*
*&      Form  F_ADD_LINE_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBAP_POP  text
*      -->P_I_DETAIL  text
*----------------------------------------------------------------------*
FORM f_add_line_items  TABLES   ivb STRUCTURE w_vbap_pop
                                idt STRUCTURE w_detail
                                idto STRUCTURE  w_detail_old
                                ivp STRUCTURE w_vbap.

  CHECK ivb[] IS NOT INITIAL.
  CLEAR : w_vbap_pop,w_detail.
  LOOP AT ivb INTO w_vbap_pop WHERE sel EQ 'X'.
    MOVE-CORRESPONDING w_vbap_pop TO w_detail.
    MOVE-CORRESPONDING w_vbap_pop TO w_vbap.
    w_detail-add = 'X'.
    w_detail-kwmeng_o = w_detail-kwmeng.
    APPEND w_detail TO idt.
    APPEND w_detail TO idto.
    APPEND w_vbap TO i_vbap.
  ENDLOOP.
  CLEAR : w_detail,w_vbap,w_vbap_pop.
  PERFORM f_simulate.
ENDFORM.                    " F_ADD_LINE_ITEMS
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_CHANGED_BILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL  text
*----------------------------------------------------------------------*
FORM f_save_changed_bill  TABLES  it STRUCTURE w_detail.
  CHECK it[] IS NOT INITIAL.
  DATA :
        msg(100) TYPE c.
  FIELD-SYMBOLS : <fsd> LIKE LINE OF it.

  CLEAR : i_detail_db[],w_header.
* prepare header data

  txt_aedat = sy-datum.
  txt_aetim = sy-uzeit.
  txt_aenam = sy-uname.

  IF txt_vbeln+0(1) EQ '2'.
    txt_reftype = 'QT'.
  ELSE.
    txt_reftype = 'SO'.
  ENDIF.

  IF chk_vat NE 'X'.
    CLEAR txt_vat.
  ENDIF.

  IF chk_wht NE 'X'.
    CLEAR txt_wht.
  ENDIF.


  READ TABLE i_header INTO w_header INDEX 1.

* Prepare detail
  LOOP AT it ASSIGNING <fsd>.
    MOVE-CORRESPONDING <fsd> TO w_detail_db.
    IF <fsd>-add IS INITIAL.
      w_detail_db-docnr = txt_docnr.
      w_detail_db-lstnr = sy-tabix.
      w_detail_db-aedat = txt_aedat.
      w_detail_db-aetim = txt_aetim.
      w_detail_db-aenam = txt_aenam.
      w_detail_db-erdat = w_header-erdat.
      w_detail_db-ertim = w_header-ertim.
      w_detail_db-ernam = w_header-ernam.
    ELSE.
      w_detail_db-docnr = txt_docnr.
      w_detail_db-lstnr = sy-tabix.
      w_detail_db-aedat = txt_aedat.
      w_detail_db-aetim = txt_aetim.
      w_detail_db-aenam = txt_aenam.
      w_detail_db-erdat = txt_aedat.
      w_detail_db-ertim = txt_aetim.
      w_detail_db-ernam = txt_aenam.
    ENDIF.
    APPEND w_detail_db TO i_detail_db.
    CLEAR w_detail_db.
  ENDLOOP.

*Prepare VBAK at time
  PERFORM f_get_vbak_at_time IN PROGRAM zar_msbill_cred IF FOUND  TABLES i_vbak_atme USING txt_vbeln.
  w_vbak_atme-docnr = txt_docnr.
  w_vbak_atme-erdat = txt_aedat.
  w_vbak_atme-ertim = txt_aetim.
  w_vbak_atme-ernam = txt_aenam.
  MODIFY i_vbak_atme FROM w_vbak_atme TRANSPORTING docnr erdat ertim ernam WHERE vbeln IS NOT INITIAL.
  CLEAR w_vbak_atme.

*Prepare VBAP at time
  PERFORM f_get_vbap_at_time IN PROGRAM zar_msbill_cred IF FOUND TABLES i_vbap_atme USING txt_vbeln.
  w_vbap_atme-docnr = txt_docnr.
  w_vbap_atme-erdat = txt_aedat.
  w_vbap_atme-ertim = txt_aetim.
  w_vbap_atme-ernam = txt_aenam.
  MODIFY i_vbap_atme FROM w_vbap_atme TRANSPORTING docnr erdat ertim ernam WHERE vbeln IS NOT INITIAL.
  CLEAR w_vbap_atme.

*Prepare KONV at time
  PERFORM f_get_konv_at_time IN PROGRAM zar_msbill_cred IF FOUND  TABLES i_konv_atme i_konv.
  w_konv_atme-docnr = txt_docnr.
  w_konv_atme-erdat = txt_aedat.
  w_konv_atme-ertim = txt_aetim.
  w_konv_atme-ernam = txt_aenam.
  MODIFY i_konv_atme FROM w_konv_atme TRANSPORTING docnr erdat ertim ernam WHERE knumv IS NOT INITIAL.
  CLEAR w_konv_atme.

  TRY.

      UPDATE ZSDSFIT006       SET
                                bstkd = txt_bstkd
                                aufnr = txt_aufnr
                                doctype = lst_doctype
                                reftype = txt_reftype
                                pmnttyp = txt_pmnt_typ
                                prcnt = txt_prcnt
                                vat = txt_vat
                                whtax = txt_wht
                                docdate = txt_docdate
                                kunnr = txt_kunnr
                                name1 = txt_name1
                                mgrnr = lst_mgrnr
                                posdesc = txt_pos
                                topic = txt_topic
                                attend = txt_attend
                                zterm = txt_zterm
                                aedat = txt_aedat
                                aetim = txt_aetim
                                aenam = txt_aenam
                                WHERE docnr EQ txt_docnr.

      DELETE FROM ZSDSFIT007 WHERE docnr EQ txt_docnr.
      INSERT ZSDSFIT007 FROM TABLE i_detail_db.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.


      INSERT ZSDSFIT009 FROM TABLE i_vbak_atme .
      INSERT ZSDSFIT010 FROM TABLE i_vbap_atme .
      INSERT ZSDSFIT011 FROM TABLE i_konv_atme .

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CONCATENATE 'Change billing No :' txt_docnr 'complete!' INTO msg SEPARATED BY space.
      MESSAGE s000(38) WITH msg DISPLAY LIKE 'S'.

      LEAVE TO SCREEN 0.
    CATCH cx_sy_dynamic_osql_error INTO oref.
      gv_message = oref->get_text( ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE e000(38) WITH gv_message.
      EXIT.
  ENDTRY.



ENDFORM.                    " F_SAVE_CHANGED_BILL

*&---------------------------------------------------------------------*
*&      Form  f_del_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM f_del_rows  TABLES it STRUCTURE w_detail_200.
  DELETE it WHERE sel EQ 'X'.
ENDFORM.                    " F_DEL_ROWS


**&---------------------------------------------------------------------*
**&      Form  f_get_vbak_at_time
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->IT         text
**      -->P_VBELN    text
**----------------------------------------------------------------------*
*FORM f_get_vbak_at_time TABLES it STRUCTURE w_vbak_atme
*                        USING p_vbeln TYPE vbak-vbeln
*                               .
*  CHECK p_vbeln IS NOT INITIAL.
*
*  SELECT
*      vbeln
*      audat
*      vtweg
*      vkbur
*      vkgrp
*      kunnr
*      knumv
*    INTO CORRESPONDING FIELDS OF TABLE it
*    FROM vbak
*    WHERE vbeln EQ p_vbeln
*    AND vbeln NOT IN ( SELECT vbeln FROM ZSDSFIT006 WHERE vbeln EQ vbak~vbeln AND dflag EQ space ).
*
*ENDFORM.                    " F_GET_VBAK_AT_TIME
*
**&---------------------------------------------------------------------*
**&      Form  f_get_vbap_at_time
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->IT         text
**      -->P_VBELN    text
**----------------------------------------------------------------------*
*FORM f_get_vbap_at_time  TABLES it STRUCTURE w_vbap_atme
*                         USING p_vbeln TYPE vbak-vbeln.
*
*  CHECK p_vbeln IS NOT INITIAL.
*  SELECT
*    vbeln
*    posnr
*    matnr
*    kwmeng
*    netpr
*    mwsbp
*    INTO CORRESPONDING FIELDS OF TABLE it
*    FROM vbap
*    WHERE vbeln EQ p_vbeln
*    AND posnr NOT IN ( SELECT posnr FROM ZSDSFIT007 WHERE vbeln EQ vbap~vbeln AND posnr EQ  vbap~posnr AND dflag EQ space ).
*
*ENDFORM.                    " F_GET_VBAP_AT_TIME
*
**&---------------------------------------------------------------------*
**&      Form  f_get_konv_at_time
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->IT         text
**      -->P_KONV     text
**----------------------------------------------------------------------*
*FORM f_get_konv_at_time  TABLES it STRUCTURE w_konv_atme
*                                p_konv STRUCTURE w_konv.
*
*  CHECK p_konv[] IS NOT INITIAL.
*  DATA : l_konv TYPE STANDARD TABLE OF typ_konv.
*  l_konv[] = p_konv[].
*  SORT l_konv BY knumv.
*  DELETE ADJACENT DUPLICATES FROM l_konv COMPARING knumv.
*  SELECT
*      knumv
*      kposn
*      stunr
*      zaehk
*      kschl
*      kdatu
*      krech
*      kawrt
*      kbetr
*      waers
*      kpein
*      kmein
*      kwert
*      kinak
*    INTO CORRESPONDING FIELDS OF TABLE it
*    FROM konv
*    FOR ALL ENTRIES IN l_konv
*    WHERE knumv EQ l_konv-knumv.
*ENDFORM.                    " F_GET_KONV_AT_TIME
*&---------------------------------------------------------------------*
*&      Form  F_DEL_BILLING_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TXT_DOCNR  text
*----------------------------------------------------------------------*
FORM f_del_billing_document  USING    p_docnr.
  DATA msg(100) TYPE c.
  TRY.

      UPDATE ZSDSFIT006       SET
                                dflag = 'X'
                                WHERE docnr EQ p_docnr.

      UPDATE ZSDSFIT007       SET
                                dflag = 'X'
                                WHERE docnr EQ p_docnr.


      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CONCATENATE 'Delete billing No :' p_docnr 'complete!' INTO msg SEPARATED BY space.
      MESSAGE s000(38) WITH msg DISPLAY LIKE 'S'.

      LEAVE TO SCREEN 0.
    CATCH cx_sy_dynamic_osql_error INTO oref.
      gv_message = oref->get_text( ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE e000(38) WITH gv_message.
      EXIT.
  ENDTRY.
ENDFORM.                    " F_DEL_BILLING_DOCUMENT
