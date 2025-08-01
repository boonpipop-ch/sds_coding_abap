*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0120_SUBRUTINE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_VBELN_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL  text
*      -->P_TXT_VBELN  text
*----------------------------------------------------------------------*
FORM F_GET_VBELN_DETAIL  TABLES   IT STRUCTURE W_DETAIL
                                  IVK STRUCTURE W_VBAK
                                  IKN STRUCTURE W_KONV
                                  IVP STRUCTURE W_VBAP
                         USING    P_VBELN TYPE VBAK-VBELN
                                  P_EDATU TYPE VBEP-EDATU.

  CLEAR : IT[],IVK[],IKN[],IVP[].
  CHECK P_VBELN IS NOT INITIAL.
  FIELD-SYMBOLS : <FS> LIKE LINE OF IT.

  IF P_EDATU IS INITIAL.
    SELECT
      P~VBELN
      P~POSNR
      B~EDATU
      K~AUART
      P~MATNR
      MAKTX
      NETPR
      KWMENG
      P~NETWR
      K~KNUMV
      MARM~MEINH
      INTO CORRESPONDING FIELDS OF TABLE IT
      FROM VBAP AS P
      INNER JOIN VBAK AS K ON P~VBELN EQ K~VBELN
      INNER JOIN VBEP AS B ON P~VBELN EQ B~VBELN AND P~POSNR EQ B~POSNR AND B~ETENR EQ '1'
      INNER JOIN MAKT ON P~MATNR EQ MAKT~MATNR AND MAKT~SPRAS EQ 'E'
      INNER JOIN MARM ON P~MATNR EQ MARM~MATNR
      WHERE P~VBELN EQ P_VBELN
       AND  MARM~UMREZ EQ '1.00'
       AND  MARM~UMREN EQ '1.00'.
  ELSE.
    SELECT
      P~VBELN
      P~POSNR
      B~EDATU
      K~AUART
      P~MATNR
      MAKTX
      NETPR
      KWMENG
      P~NETWR
      K~KNUMV
      MARM~MEINH
      INTO CORRESPONDING FIELDS OF TABLE IT
      FROM VBAP AS P
      INNER JOIN VBAK AS K ON P~VBELN EQ K~VBELN
      INNER JOIN VBEP AS B ON P~VBELN EQ B~VBELN AND P~POSNR EQ B~POSNR AND B~ETENR EQ '1'
      INNER JOIN MAKT ON P~MATNR EQ MAKT~MATNR AND MAKT~SPRAS EQ 'E'
      INNER JOIN MARM ON P~MATNR EQ MARM~MATNR
      WHERE P~VBELN EQ P_VBELN
       AND  B~EDATU EQ P_EDATU
       AND  MARM~UMREZ EQ '1.00'
       AND  MARM~UMREN EQ '1.00'.
  ENDIF.

  CHECK IT[] IS NOT INITIAL.

  LOOP AT IT ASSIGNING <FS>.
    <FS>-KWMENG_O = <FS>-KWMENG.
  ENDLOOP.

  SELECT
      K~VBELN
      K~KUNNR
      RC~NAME1
      RC~NAME2
      D~ZTERM
      D~BSTKD
      T~TEXT1 AS T052U
      U~AUFNR
      U~KTEXT
    UP TO 1 ROWS
    INTO CORRESPONDING FIELDS OF TABLE IVK
    FROM VBAK AS K
    INNER JOIN VBAP AS P ON K~VBELN EQ P~VBELN
    INNER JOIN VBKD AS D ON K~VBELN EQ D~VBELN
    LEFT JOIN AUFK AS U ON P~AUFNR EQ U~AUFNR
    LEFT JOIN T052U AS T ON D~ZTERM EQ T~ZTERM AND T~SPRAS EQ 'E'
    INNER JOIN KNA1 AS A ON K~KUNNR EQ A~KUNNR
    INNER JOIN ADRC AS RC ON A~ADRNR EQ RC~ADDRNUMBER
    WHERE K~VBELN EQ P_VBELN
    AND RC~NATION EQ SPACE.

  SELECT
          KNUMV
          KPOSN
          KSCHL
          KWERT
    INTO TABLE IKN
    FROM KONV
    FOR ALL ENTRIES IN IT
    WHERE KONV~KNUMV EQ IT-KNUMV
    AND   KONV~KPOSN EQ IT-POSNR
    AND ( KONV~KSCHL EQ 'ZDP3' OR KONV~KSCHL EQ 'ZDP2' ).

  SELECT
      VBELN
      POSNR
      MATNR
      KWMENG
      NETPR
      NETWR
 INTO TABLE IVP
 FROM VBAP
 WHERE VBELN EQ P_VBELN.

  LOOP AT IT ASSIGNING <FS>.
    LOOP AT IKN INTO W_KONV WHERE KNUMV EQ <FS>-KNUMV AND KPOSN EQ <FS>-POSNR.
      <FS>-KWERT = <FS>-KWERT + W_KONV-KWERT.
    ENDLOOP.
  ENDLOOP.

  SORT IT BY VBELN POSNR.
ENDFORM.                    " F_GET_VBELN_DETAIL
*&---------------------------------------------------------------------*
*&      Form  F_GET_TERM_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TXT_ZTERM  text
*      <--P_TXT_PMNT_DESC  text
*----------------------------------------------------------------------*
FORM F_GET_TERM_DESC  USING    P_TXT_ZTERM TYPE VBKD-ZTERM
                      CHANGING P_TXT_PMNT_DESC TYPE T052U-TEXT1
                               P_TXT_PMNT_TYP TYPE ZSDSFIT006-PMNTTYP.

  CLEAR P_TXT_PMNT_DESC.
  CHECK P_TXT_ZTERM IS NOT INITIAL.
  SELECT SINGLE TEXT1 INTO P_TXT_PMNT_DESC
    FROM T052U
    WHERE ZTERM EQ P_TXT_ZTERM
    AND SPRAS EQ 'E'.

  IF SY-SUBRC NE 0.
    CLEAR : P_TXT_ZTERM,P_TXT_PMNT_DESC.
  ENDIF.
ENDFORM.                    " F_GET_TERM_DESC
*&---------------------------------------------------------------------*
*&      Form  F_ASSIGN_HEADER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBAK  text
*----------------------------------------------------------------------*
FORM F_ASSIGN_HEADER_DATA  TABLES IT STRUCTURE W_VBAK.

  CLEAR : TXT_ATTEND,TXT_KUNNR,TXT_NAME1,TXT_ZTERM,TXT_PMNT_DESC,TXT_BSTKD,TXT_AUFNR,TXT_KTEXT.
  CHECK IT[] IS NOT INITIAL.

  READ TABLE IT INTO W_VBAK INDEX 1.

  TXT_ATTEND = 'กรรมการผู้จัดการ'.
  TXT_KUNNR = W_VBAK-KUNNR.
*  txt_name1 = w_vbak-name1.
  CONCATENATE W_VBAK-NAME1 W_VBAK-NAME2 INTO TXT_NAME1 SEPARATED BY SPACE.
  TXT_ZTERM = W_VBAK-ZTERM.
  TXT_PMNT_DESC = W_VBAK-T052U.
  TXT_BSTKD = W_VBAK-BSTKD.
  TXT_AUFNR = W_VBAK-AUFNR.
  TXT_KTEXT = W_VBAK-KTEXT.

ENDFORM.                    " F_ASSIGN_HEADER_DATA

*&---------------------------------------------------------------------*
*&      Form  F_SIMULATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_SIMULATE.

  IF  TXT_VBELN IS NOT INITIAL AND ( ( TXT_VBELN NE TXT_VBELN_O ) OR ( TXT_EDATU NE TXT_EDATU_O ) ).
    PERFORM F_GET_VBELN_DETAIL TABLES I_DETAIL I_VBAK I_KONV I_VBAP USING TXT_VBELN TXT_EDATU.
*    Keep original value of sale order quantity
    I_DETAIL_OLD[] = I_DETAIL[].
    IF I_VBAK[] IS NOT INITIAL.
      PERFORM F_ASSIGN_HEADER_DATA TABLES I_VBAK.
    ELSE.
*       Keep old value
      TXT_VBELN_O = TXT_VBELN.
      TXT_EDATU_O = TXT_EDATU.
      TXT_ZTERM_O = TXT_ZTERM.
      TXT_PRCNT_O = TXT_PRCNT.
      TXT_PMNT_TYP_O = TXT_PMNT_TYP.
      LST_MGRNR_O = LST_MGRNR.
      LST_DOCTYPE_O = LST_DOCTYPE.
      TXT_VAT_O = TXT_VAT.
      TXT_WHT_O = TXT_WHT.
      MESSAGE I000(38) WITH 'SO/QT items not found!' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF TXT_ZTERM NE TXT_ZTERM_O.
    PERFORM F_GET_TERM_DESC USING TXT_ZTERM CHANGING TXT_PMNT_DESC
                                                      TXT_PMNT_TYP.
  ENDIF.


  IF TXT_PRCNT NE TXT_PRCNT_O.
    PERFORM F_ASSIGN_TOPIC IN PROGRAM ZAR_MSBILL_CRED IF FOUND USING LST_DOCTYPE TXT_PRCNT CHANGING TXT_TOPIC TXT_ATTEND.
  ENDIF.

*  PERFORM f_assign_topic USING lst_doctype CHANGING txt_topic txt_attend.

  PERFORM F_CAL_PERCENTAGE_OF_DOC TABLES I_DETAIL[].
  PERFORM F_CAL_AMT TABLES I_DETAIL[].
  PERFORM F_CAL_AMT_BOM TABLES I_DETAIL[].

* Keep old value
  TXT_VBELN_O = TXT_VBELN.
  TXT_EDATU_O = TXT_EDATU.
  TXT_PRCNT_O = TXT_PRCNT.
  TXT_ZTERM_O = TXT_ZTERM.
  TXT_PMNT_TYP_O = TXT_PMNT_TYP.
  LST_MGRNR_O = LST_MGRNR.
  LST_DOCTYPE_O = LST_DOCTYPE.
  TXT_VAT_O = TXT_VAT.
  TXT_WHT_O = TXT_WHT.


ENDFORM.                    "F_SIMULATE
*&---------------------------------------------------------------------*
*&      Form  F_CAL_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CAL_AMT TABLES IT STRUCTURE W_DETAIL.

  CHECK IT[] IS NOT INITIAL.
  FIELD-SYMBOLS : <FS> LIKE LINE OF IT.
  CLEAR : TXT_TOTAL_AMT1,TXT_ADV,TXT_TOTAL_QTY.

  LOOP AT IT ASSIGNING <FS> WHERE SEL EQ 'X' AND MEINH NE 'SET'.
    TXT_TOTAL_AMT1 = TXT_TOTAL_AMT1 +  <FS>-NETWR.
    TXT_ADV = TXT_ADV + <FS>-KWERT.
    TXT_TOTAL_QTY = TXT_TOTAL_QTY + <FS>-KWMENG.
  ENDLOOP.

  TXT_TOTAL_AMT2 = TXT_TOTAL_AMT1 + TXT_ADV.

  IF CHK_VAT EQ 'X'.
    TXT_AMT_INC_VAT = TXT_TOTAL_AMT2 * ( TXT_VAT / 100 ).
  ELSE.
    TXT_AMT_INC_VAT = 0.
  ENDIF.

  IF CHK_WHT EQ 'X'.
    TXT_AMT_INC_WHT = 0 - ( TXT_TOTAL_AMT2 * ( TXT_WHT / 100 ) ).
  ELSE.
    TXT_AMT_INC_WHT = 0.
  ENDIF.

  TXT_GRANDTOTAL =  TXT_TOTAL_AMT2 + TXT_AMT_INC_VAT  + TXT_AMT_INC_WHT.



ENDFORM.                    " F_CAL_AMT
*&---------------------------------------------------------------------*
*&      Form  F_CAL_PERCENTAGE_OF_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL[]  text
*----------------------------------------------------------------------*
FORM F_CAL_PERCENTAGE_OF_DOC  TABLES IT STRUCTURE W_DETAIL.
  CHECK IT[] IS NOT INITIAL.

  SORT I_DETAIL_OLD BY VBELN POSNR.
  SORT IT BY VBELN POSNR.

  FIELD-SYMBOLS : <FS> LIKE LINE OF IT,
                  <FS2> LIKE LINE OF IT.

  LOOP AT I_DETAIL ASSIGNING <FS>.
    READ TABLE I_DETAIL_OLD WITH KEY VBELN = <FS>-VBELN
                                     POSNR = <FS>-POSNR
               ASSIGNING <FS2>.
    IF SY-SUBRC EQ 0.
      <FS>-NETPR = ( <FS2>-NETPR * TXT_PRCNT ) / 100.
      <FS>-NETWR = <FS>-NETPR * <FS>-KWMENG.
    ENDIF.

* Calculate Advance Reciept
    <FS>-KWERT = 0.
    LOOP AT I_KONV INTO W_KONV WHERE KNUMV = <FS>-KNUMV AND KPOSN = <FS>-POSNR.
      <FS>-KWERT = <FS>-KWERT + W_KONV-KWERT.
    ENDLOOP.

    READ TABLE I_VBAP INTO W_VBAP WITH KEY VBELN = <FS>-VBELN
                                          POSNR = <FS>-POSNR.
    IF SY-SUBRC EQ 0.
      <FS>-KWERT = <FS>-KWERT * ( <FS>-KWMENG / W_VBAP-KWMENG ).
      <FS>-KWERT = ( <FS>-KWERT * TXT_PRCNT ) / 100.
    ENDIF.

    UNASSIGN : <FS2>.
  ENDLOOP.
ENDFORM.                    " F_CAL_PERCENTAGE_OF_DOC

*&---------------------------------------------------------------------*
*&      Form  f_clear_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CLEAR_SCREEN.
  CLEAR : LST_DOCTYPE,
  TXT_TOPIC,
  TXT_VBELN ,
  TXT_ATTEND ,
  TXT_KUNNR ,
  TXT_NAME1 ,
  TXT_ZTERM ,
  TXT_PMNT_DESC ,
  TXT_PMNT_TYP ,
  TXT_BSTKD ,
  TXT_AUFNR ,
  LST_MGRNR ,
  TXT_KTEXT ,
  TXT_MNAME,
  TXT_POS,
  TXT_PRCNT,
  CHK_WHT,
  TXT_VAT,
  TXT_WHT,
  TXT_TOTAL_QTY,
  TXT_TOTAL_AMT1,
  TXT_ADV,
  TXT_TOTAL_AMT2,
  TXT_AMT_INC_VAT,
  TXT_AMT_INC_WHT,
  TXT_GRANDTOTAL,
  TXT_DOCDATE.

  CLEAR : LST_DOCTYPE_O,
  TXT_TOPIC_O,
  TXT_VBELN_O,
  TXT_ATTEND_O ,
  TXT_KUNNR_O ,
  TXT_NAME1_O ,
  TXT_ZTERM_O ,
  TXT_PMNT_DESC_O ,
  TXT_PMNT_TYP_O ,
  TXT_BSTKD_O ,
  TXT_AUFNR_O ,
  LST_MGRNR_O ,
  TXT_KTEXT_O ,
  TXT_MNAME_O,
  TXT_POS_O,
  TXT_PRCNT_O,
  TXT_VAT_O,
  TXT_WHT_O,
  TXT_TOTAL_QTY_O,
  TXT_TOTAL_AMT1_O,
  TXT_ADV_O,
  TXT_TOTAL_AMT2_O,
  TXT_AMT_INC_VAT_O,
  TXT_AMT_INC_WHT_O,
  TXT_GRANDTOTAL_O.

  CLEAR : I_DETAIL[],
          I_DETAIL_OLD[],
          I_VBAK[],
          I_KONV[],
          I_VBAP[],
          I_DETAIL_200[].

  CHK_VAT = 'X'.
  TXT_PRCNT = 100.

ENDFORM.                    "f_clear_screen
*&---------------------------------------------------------------------*
*&      Form  F_ASSIGN_TOPIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LST_DOCTYPE  text
*      <--P_TXT_TOPIC  text
*----------------------------------------------------------------------*
FORM F_ASSIGN_TOPIC  USING    P_LST_DOCTYPE
                              P_TXT_PRCNT
                     CHANGING P_TXT_TOPIC
                              P_TXT_ATTEND.

*01	Product Fee
*02	Installation Fee
*03	Maintainance Contract Fee
*04	Service Fee
*05	Spare Parts Fee
*06	Retention Fee
*07	Others

  DATA : TXT_P(10) TYPE C.

  CASE P_LST_DOCTYPE.
    WHEN '01'.
      P_TXT_TOPIC = 'ขอรับชำระเงินค่าเครื่องปรับอากาศไดกิ้น'.
    WHEN '02'.
      P_TXT_TOPIC = 'ขอรับชำระเงินค่าติดตั้งระบบปรับอากาศไดกิ้น'.
    WHEN '03'.
      P_TXT_TOPIC = 'ขอรับชำระเงินค่าสัญญาบริการเครื่องปรับอากาศไดกิ้น'.
    WHEN '04'.
      P_TXT_TOPIC = 'ขอรับชำระเงินค่าบริการซ่อมเครื่องปรับอากาศไดกิ้น'.
    WHEN '05'.
      P_TXT_TOPIC = 'ขอรับชำระเงินค่าอะไหล่เครื่องปรับอากาศไดกิ้น'.
    WHEN '06'.
      P_TXT_TOPIC = 'ขอรับชำระเงินประกันผลงาน'.
    WHEN '07'.
      P_TXT_TOPIC = SPACE.
    WHEN OTHERS.
      P_TXT_TOPIC = SPACE.
  ENDCASE.

  IF P_TXT_PRCNT LT 100 AND P_TXT_PRCNT GT 0.
    WRITE P_TXT_PRCNT TO TXT_P NO-ZERO.
    CONCATENATE TXT_P '%' INTO TXT_P.
    CONCATENATE P_TXT_TOPIC TXT_P INTO P_TXT_TOPIC SEPARATED BY SPACE.
  ENDIF.
  P_TXT_ATTEND = 'กรรมการผู้จัดการ'.
ENDFORM.                    " F_ASSIGN_TOPIC
*&---------------------------------------------------------------------*
*&      Form  F_ASSIGN_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LST_MGRNR  text
*      <--P_TXT_POS  text
*----------------------------------------------------------------------*
FORM F_ASSIGN_POS  USING    P_MGRNR
                   CHANGING P_POS.

  READ TABLE I_APV INTO W_APV WITH KEY PERNR = P_MGRNR.
  IF SY-SUBRC EQ 0.
    P_POS = W_APV-POSDESC.
  ENDIF.

ENDFORM.                    " F_ASSIGN_POS
*&---------------------------------------------------------------------*
*&      Form  F_ADD_REMARK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL  text
*----------------------------------------------------------------------*
FORM F_ADD_REMARK  TABLES   IT STRUCTURE W_DETAIL.
  CHECK IT[] IS NOT INITIAL.
  DATA : WA LIKE LINE OF IT.
  WA-VBELN = TXT_VBELN.
  WA-POSNR = '999999'.
  WA-MATNR = 'REMARK'.
*  wa-kwmeng = 1.
*  wa-kwmeng_o = 1.
  WA-SEL = 'X'.
  APPEND WA TO IT.
  CLEAR WA.
ENDFORM.                    " F_ADD_REMARK
*&---------------------------------------------------------------------*
*&      Form  F_DEL_REMARK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL  text
*----------------------------------------------------------------------*
FORM F_DEL_REMARK  TABLES   IT STRUCTURE W_DETAIL.
  DELETE IT WHERE SEL EQ 'X' AND POSNR EQ '999999'.
  PERFORM F_SIMULATE.
ENDFORM.                    " F_DEL_REMARK
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_BILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL  text
*----------------------------------------------------------------------*
FORM F_CREATE_BILL  TABLES   IT STRUCTURE W_DETAIL.
  DATA : RSEL TYPE I,
         MSG(100) TYPE C.
  FIELD-SYMBOLS : <FSD> LIKE LINE OF IT.
  LOOP AT IT INTO W_DETAIL_ADD WHERE SEL EQ 'X'.
    RSEL = RSEL + 1.
  ENDLOOP.
  IF RSEL IS INITIAL.
    MESSAGE I000(38) WITH 'Please select item for create billing doc' DISPLAY LIKE 'W'.
    EXIT.
  ENDIF.

  CLEAR : I_HEADER[],I_DETAIL_DB[].

  PERFORM GET_DOCNR CHANGING TXT_DOCNR.

* prepare header data

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = TXT_KUNNR
    IMPORTING
      OUTPUT = TXT_KUNNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LST_MGRNR
    IMPORTING
      OUTPUT = LST_MGRNR.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = TXT_AUFNR
    IMPORTING
      OUTPUT = TXT_AUFNR.


  TXT_ERDAT = SY-DATUM.
  TXT_ERTIM = SY-UZEIT.
  TXT_ERNAM = SY-UNAME.

  W_HEADER-DOCNR = TXT_DOCNR.
  W_HEADER-VBELN = TXT_VBELN.
  W_HEADER-BSTKD = TXT_BSTKD.
  W_HEADER-AUFNR = TXT_AUFNR.
  W_HEADER-KTEXT = TXT_KTEXT.
  W_HEADER-DOCTYPE = LST_DOCTYPE.
  W_HEADER-EDATU = TXT_EDATU.

  IF TXT_VBELN+0(1) EQ '2'.
    W_HEADER-REFTYPE = 'QT'.
  ELSE.
    W_HEADER-REFTYPE = 'SO'.
  ENDIF.

  W_HEADER-PMNTTYP = TXT_PMNT_TYP.
  W_HEADER-PRCNT = TXT_PRCNT.

  IF CHK_VAT EQ 'X'.
    W_HEADER-VAT = TXT_VAT.
  ELSE.
    W_HEADER-VAT = SPACE.
  ENDIF.

  IF CHK_WHT EQ 'X'.
    W_HEADER-WHTAX = TXT_WHT.
  ELSE.
    W_HEADER-WHTAX = SPACE.
  ENDIF.

  W_HEADER-DOCDATE = TXT_DOCDATE.
  W_HEADER-KUNNR = TXT_KUNNR.
  W_HEADER-NAME1 = TXT_NAME1.
  W_HEADER-MGRNR = LST_MGRNR.
  W_HEADER-POSDESC = TXT_POS.
  W_HEADER-TOPIC = TXT_TOPIC.
  W_HEADER-ATTEND = TXT_ATTEND.
  W_HEADER-ZTERM = TXT_ZTERM.
  W_HEADER-ERDAT = TXT_ERDAT.
  W_HEADER-ERTIM = TXT_ERTIM.
  W_HEADER-ERNAM = TXT_ERNAM.
  W_HEADER-DFLAG = SPACE.
  APPEND W_HEADER TO I_HEADER.


* Prepare detail
  LOOP AT IT ASSIGNING <FSD> WHERE SEL EQ 'X'.
    MOVE-CORRESPONDING <FSD> TO W_DETAIL_DB.
    W_DETAIL_DB-DOCNR = TXT_DOCNR.
    W_DETAIL_DB-LSTNR = SY-TABIX.
    W_DETAIL_DB-ERDAT = TXT_ERDAT.
    W_DETAIL_DB-ERTIM = TXT_ERTIM.
    W_DETAIL_DB-ERNAM = TXT_ERNAM.
    APPEND W_DETAIL_DB TO I_DETAIL_DB.
    CLEAR W_DETAIL_DB.
  ENDLOOP.

*Prepare VBAK at time
  PERFORM F_GET_VBAK_AT_TIME TABLES I_VBAK_ATME USING TXT_VBELN.
  W_VBAK_ATME-DOCNR = TXT_DOCNR.
  W_VBAK_ATME-ERDAT = TXT_ERDAT.
  W_VBAK_ATME-ERTIM = TXT_ERTIM.
  W_VBAK_ATME-ERNAM = TXT_ERNAM.
  MODIFY I_VBAK_ATME FROM W_VBAK_ATME TRANSPORTING DOCNR ERDAT ERTIM ERNAM WHERE VBELN IS NOT INITIAL.
  CLEAR W_VBAK_ATME.

*Prepare VBAP at time
  PERFORM F_GET_VBAP_AT_TIME TABLES I_VBAP_ATME USING TXT_VBELN.
  W_VBAP_ATME-DOCNR = TXT_DOCNR.
  W_VBAP_ATME-ERDAT = TXT_ERDAT.
  W_VBAP_ATME-ERTIM = TXT_ERTIM.
  W_VBAP_ATME-ERNAM = TXT_ERNAM.
  MODIFY I_VBAP_ATME FROM W_VBAP_ATME TRANSPORTING DOCNR ERDAT ERTIM ERNAM WHERE VBELN IS NOT INITIAL.
  CLEAR W_VBAP_ATME.

*Prepare KONV at time
  PERFORM F_GET_KONV_AT_TIME TABLES I_KONV_ATME I_KONV.
  W_KONV_ATME-DOCNR = TXT_DOCNR.
  W_KONV_ATME-ERDAT = TXT_ERDAT.
  W_KONV_ATME-ERTIM = TXT_ERTIM.
  W_KONV_ATME-ERNAM = TXT_ERNAM.
  MODIFY I_KONV_ATME FROM W_KONV_ATME TRANSPORTING DOCNR ERDAT ERTIM ERNAM WHERE KNUMV IS NOT INITIAL.
  CLEAR W_KONV_ATME.

  TRY.

      INSERT ZSDSFIT006 FROM TABLE I_HEADER.
      INSERT ZSDSFIT007 FROM TABLE I_DETAIL_DB.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      INSERT ZSDSFIT009 FROM TABLE I_VBAK_ATME .
      INSERT ZSDSFIT010 FROM TABLE I_VBAP_ATME .
      INSERT ZSDSFIT011 FROM TABLE I_KONV_ATME .

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CONCATENATE 'Create billing No :' TXT_DOCNR 'complete!' INTO MSG SEPARATED BY SPACE.
      MESSAGE S000(38) WITH MSG DISPLAY LIKE 'S'.

      LEAVE TO SCREEN 0.
    CATCH CX_SY_DYNAMIC_OSQL_ERROR INTO OREF.
      GV_MESSAGE = OREF->GET_TEXT( ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE E000(38) WITH GV_MESSAGE.
      EXIT.
  ENDTRY.


  CLEAR RSEL.
ENDFORM.                    " F_CREATE_BILL
*&---------------------------------------------------------------------*
*&      Form  GET_DOCNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_TXT_DOCNR  text
*----------------------------------------------------------------------*
FORM GET_DOCNR  CHANGING P_DOCNR TYPE ZSDSFIT006-DOCNR.
  DATA : LV_DOCNR TYPE ZSDSFIT006-DOCNR,
         L_YRS(4) TYPE N,
         L_RUNNO(6) TYPE N.

  RANGES : S_ERDAT FOR ZSDSFIT006-ERDAT.

  S_ERDAT-SIGN = 'I'.
  S_ERDAT-OPTION = 'BT'.
  CONCATENATE SY-DATUM+0(4) '01' '01' INTO S_ERDAT-LOW.
  CONCATENATE SY-DATUM+0(4) '12' '31' INTO S_ERDAT-HIGH.
  APPEND S_ERDAT.

  SELECT DOCNR INTO (LV_DOCNR) UP TO 1 ROWS
    FROM ZSDSFIT006
    WHERE ERDAT IN S_ERDAT
    ORDER BY DOCNR DESCENDING.
  ENDSELECT.

  L_YRS = ( SY-DATUM+0(4) + 543 ).

  IF LV_DOCNR IS INITIAL.
    CONCATENATE 'B' L_YRS  '/' '000001' INTO LV_DOCNR.
  ELSE.
    L_RUNNO = ( LV_DOCNR+6 + 1 ).
    CONCATENATE 'B' L_YRS  '/' L_RUNNO INTO LV_DOCNR.
  ENDIF.

  P_DOCNR = LV_DOCNR.
ENDFORM.                    " GET_DOCNR
*&---------------------------------------------------------------------*
*&      Form  F_GET_VBAK_AT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBAK_ATME  text
*----------------------------------------------------------------------*
FORM F_GET_VBAK_AT_TIME TABLES IT STRUCTURE W_VBAK_ATME
                        USING P_VBELN TYPE VBAK-VBELN
                               .
  CHECK P_VBELN IS NOT INITIAL.

  SELECT
      VBELN
      AUDAT
      VTWEG
      VKBUR
      VKGRP
      KUNNR
      KNUMV
    INTO CORRESPONDING FIELDS OF TABLE IT
    FROM VBAK
    WHERE VBELN EQ P_VBELN.

ENDFORM.                    " F_GET_VBAK_AT_TIME
*&---------------------------------------------------------------------*
*&      Form  F_GET_VBAP_AT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBAP_ATME  text
*----------------------------------------------------------------------*
FORM F_GET_VBAP_AT_TIME  TABLES IT STRUCTURE W_VBAP_ATME
                         USING P_VBELN TYPE VBAK-VBELN.

  CHECK P_VBELN IS NOT INITIAL.
  SELECT
    VBELN
    POSNR
    MATNR
    KWMENG
    NETPR
    MWSBP
    INTO CORRESPONDING FIELDS OF TABLE IT
    FROM VBAP
    WHERE VBELN EQ P_VBELN.

ENDFORM.                    " F_GET_VBAP_AT_TIME
*&---------------------------------------------------------------------*
*&      Form  F_GET_KONV_AT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_KONV_ATME  text
*----------------------------------------------------------------------*
FORM F_GET_KONV_AT_TIME  TABLES IT STRUCTURE W_KONV_ATME
                                P_KONV STRUCTURE W_KONV.

  CHECK P_KONV[] IS NOT INITIAL.
  DATA : L_KONV TYPE STANDARD TABLE OF TYP_KONV.
  L_KONV[] = P_KONV[].
  SORT L_KONV BY KNUMV.
  DELETE ADJACENT DUPLICATES FROM L_KONV COMPARING KNUMV.
  SELECT
      KNUMV
      KPOSN
      STUNR
      ZAEHK
      KSCHL
      KDATU
      KRECH
      KAWRT
      KBETR
      WAERS
      KPEIN
      KMEIN
      KWERT
      KINAK
    INTO CORRESPONDING FIELDS OF TABLE IT
    FROM KONV
    FOR ALL ENTRIES IN L_KONV
    WHERE KNUMV EQ L_KONV-KNUMV.
ENDFORM.                    " F_GET_KONV_AT_TIME
*&---------------------------------------------------------------------*
*&      Form  F_ASSIGN_MANAGER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ASSIGN_MANAGER .

  IF I_APV[] IS INITIAL.
    SELECT * INTO TABLE I_APV FROM ZSDSFIT008 WHERE SPRAS EQ '2' AND ( POS EQ '3' OR POS EQ '4' ).

    NAME = 'LST_MGRNR'.
    LOOP AT I_APV INTO W_APV. "GM Only
      VALUE-KEY = W_APV-PERNR.
      CONCATENATE W_APV-VORNA W_APV-NACHN INTO VALUE-TEXT SEPARATED BY SPACE.
      APPEND VALUE TO LIST.
    ENDLOOP.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID     = NAME
        VALUES = LIST.
    CLEAR:NAME,LIST,VALUE.
  ENDIF.


ENDFORM.                    " F_ASSIGN_MANAGER
*&---------------------------------------------------------------------*
*&      Form  F_SIMULATE_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SIMULATE_200 .

  IF TXT_ZTERM NE TXT_ZTERM_O.
    PERFORM F_GET_TERM_DESC USING TXT_ZTERM CHANGING TXT_PMNT_DESC
                                                     TXT_PMNT_TYP.
  ENDIF.

  IF TXT_KUNNR NE TXT_KUNNR_O.
    PERFORM F_GET_KUNNR_DESC USING TXT_KUNNR CHANGING TXT_NAME1.
  ENDIF.

  IF TXT_AUFNR NE TXT_AUFNR_O.
    PERFORM F_GET_AUFNR_DESC USING TXT_AUFNR CHANGING TXT_KTEXT.
  ENDIF.

  PERFORM F_CAL_AMT_200 TABLES I_DETAIL_200[].

* Keep old value
  TXT_KUNNR_O = TXT_KUNNR.
  TXT_AUFNR_O = TXT_AUFNR.
  TXT_ZTERM_O = TXT_ZTERM.
  TXT_PMNT_TYP_O = TXT_PMNT_TYP.
  LST_MGRNR_O = LST_MGRNR.
  LST_DOCTYPE_O = LST_DOCTYPE.
  TXT_VAT_O = TXT_VAT.
  TXT_WHT_O = TXT_WHT.


ENDFORM.                    " F_SIMULATE_200
*&---------------------------------------------------------------------*
*&      Form  F_GET_KUNNR_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TXT_KUNNR  text
*      <--P_TXT_NAME1  text
*----------------------------------------------------------------------*
FORM F_GET_KUNNR_DESC  USING    P_TXT_KUNNR TYPE KNA1-KUNNR
                       CHANGING P_TXT_NAME1 TYPE ZSDSFIT006-NAME1.

  CHECK P_TXT_KUNNR IS NOT INITIAL.
  DATA : L_KUNNR TYPE KNA1-KUNNR,
         L_NAME1 TYPE ADRC-NAME1,
         L_NAME2 TYPE ADRC-NAME2.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_TXT_KUNNR
    IMPORTING
      OUTPUT = L_KUNNR.

  SELECT SINGLE ADRC~NAME1
                ADRC~NAME2
    INTO (L_NAME1,L_NAME2)
    FROM KNA1
    INNER JOIN ADRC ON KNA1~ADRNR EQ ADRC~ADDRNUMBER
    WHERE KNA1~KUNNR EQ L_KUNNR
    AND ADRC~NATION EQ SPACE.

  IF SY-SUBRC EQ 0.
    CONCATENATE L_NAME1 L_NAME2 INTO P_TXT_NAME1 SEPARATED BY SPACE.
  ELSE.
    CLEAR : P_TXT_KUNNR,P_TXT_NAME1.
  ENDIF.

ENDFORM.                    " F_GET_KUNNR_DESC
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUFNR_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TXT_AUFNR  text
*      <--P_TXT_KTEXT  text
*----------------------------------------------------------------------*
FORM F_GET_AUFNR_DESC  USING    P_TXT_AUFNR TYPE AUFK-AUFNR
                       CHANGING P_TXT_KTEXT TYPE AUFK-KTEXT.

  CHECK P_TXT_AUFNR IS NOT INITIAL.
  DATA : L_AUFNR TYPE AUFK-AUFNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_TXT_AUFNR
    IMPORTING
      OUTPUT = L_AUFNR.

  SELECT SINGLE KTEXT
  INTO (P_TXT_KTEXT)
  FROM AUFK WHERE
  AUFNR EQ L_AUFNR.

  IF SY-SUBRC NE 0.
    CLEAR : P_TXT_AUFNR,P_TXT_KTEXT.
  ENDIF.

ENDFORM.                    " F_GET_AUFNR_DESC
*&---------------------------------------------------------------------*
*&      Form  F_TC_DETAIL_200_LINES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_TC_DETAIL_200_LINES .
  DATA ROWS TYPE I.
  ROWS = LINES( I_DETAIL_200[] ).
  IF ROWS LT 7.
    ROWS = 7.
  ELSE.
    ROWS = ROWS + 7.
  ENDIF.
  TC_DETAIL_200-LINES = ROWS.
ENDFORM.                    " F_TC_DETAIL_200_LINES
*&---------------------------------------------------------------------*
*&      Form  F_MATNR_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_DETAIL_200_MATNR  text
*      <--P_W_DETAIL_200_MAKTX  text
*----------------------------------------------------------------------*
FORM F_MATNR_DESC  USING    P_MATNR TYPE MARA-MATNR
                   CHANGING P_MAKTX TYPE MAKT-MAKTX.

  CHECK P_MATNR IS NOT INITIAL.
  DATA : L_MATNR TYPE MARA-MATNR.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_MATNR
    IMPORTING
      OUTPUT = L_MATNR.

  SELECT SINGLE MAKTX INTO P_MAKTX
    FROM MAKT
    WHERE MATNR EQ L_MATNR
    AND SPRAS EQ 'E'.

  IF SY-SUBRC NE 0.
    CLEAR : P_MATNR,P_MAKTX.
  ENDIF.

ENDFORM.                    " F_MATNR_DESC
*&---------------------------------------------------------------------*
*&      Form  F_CAL_AMT_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL_200[]  text
*----------------------------------------------------------------------*
FORM F_CAL_AMT_200  TABLES   IT STRUCTURE W_DETAIL_200.

  CHECK IT[] IS NOT INITIAL.
  FIELD-SYMBOLS : <FS> LIKE LINE OF IT.
  CLEAR : TXT_TOTAL_AMT1,TXT_TOTAL_QTY.

  LOOP AT IT ASSIGNING <FS>.
    TXT_TOTAL_AMT1 = TXT_TOTAL_AMT1 +  <FS>-NETWR.
*    txt_adv = txt_adv + <fs>-kwert.
    TXT_TOTAL_QTY = TXT_TOTAL_QTY + <FS>-KWMENG.
  ENDLOOP.

  TXT_TOTAL_AMT2 = TXT_TOTAL_AMT1 + TXT_ADV.

  IF CHK_VAT EQ 'X'.
    TXT_AMT_INC_VAT = TXT_TOTAL_AMT2 * ( TXT_VAT / 100 ).
  ELSE.
    TXT_AMT_INC_VAT = 0.
  ENDIF.

  IF CHK_WHT EQ 'X'.
    TXT_AMT_INC_WHT = 0 - ( TXT_TOTAL_AMT2 * ( TXT_WHT / 100 ) ).
  ELSE.
    TXT_AMT_INC_WHT = 0.
  ENDIF.

  TXT_GRANDTOTAL =  TXT_TOTAL_AMT2 + TXT_AMT_INC_VAT  + TXT_AMT_INC_WHT.
ENDFORM.                    " F_CAL_AMT_200
*&---------------------------------------------------------------------*
*&      Form  F_MAKTX_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_DETAIL_200_MAKTX  text
*      <--P_W_DETAIL_200_MATNR  text
*      <--P_W_DETAIL_200_MAKTX  text
*----------------------------------------------------------------------*
FORM F_MAKTX_DESC  USING    P_MAKTX TYPE ZSDSFIT007-MAKTX
                   CHANGING V_MATNR TYPE MARA-MATNR
                            V_MAKTX TYPE ZSDSFIT007-MAKTX.
  CHECK P_MAKTX IS NOT INITIAL.
  DATA : L_MATNR TYPE MARA-MATNR,
         L_MAKTX TYPE MAKT-MAKTX,
         LEN TYPE I.

  LEN = STRLEN( P_MAKTX ).

  IF LEN LE 18.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_MAKTX
      IMPORTING
        OUTPUT = L_MATNR.
    SELECT SINGLE MAKTX INTO L_MAKTX
      FROM MAKT
      WHERE MATNR EQ L_MATNR
      AND SPRAS EQ 'E'.
    IF SY-SUBRC EQ 0.
      V_MATNR = L_MATNR.
      V_MAKTX = L_MAKTX.
    ELSE.
      V_MATNR = SPACE.
      V_MAKTX = P_MAKTX.
    ENDIF.
  ELSE.
    V_MATNR = SPACE.
    V_MAKTX = P_MAKTX.
  ENDIF.

ENDFORM.                    " F_MAKTX_DESC
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_BILL_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL_200[]  text
*----------------------------------------------------------------------*
FORM F_CREATE_BILL_200 TABLES IT STRUCTURE W_DETAIL_200.
  DATA : MSG(100) TYPE C.
  FIELD-SYMBOLS : <FSD> LIKE LINE OF IT.
  CHECK IT[] IS NOT INITIAL.
  CLEAR : I_HEADER[],I_DETAIL_DB[].
  PERFORM GET_DOCNR CHANGING TXT_DOCNR.

* prepare header data

  TXT_ERDAT = SY-DATUM.
  TXT_ERTIM = SY-UZEIT.
  TXT_ERNAM = SY-UNAME.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = TXT_KUNNR
    IMPORTING
      OUTPUT = TXT_KUNNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LST_MGRNR
    IMPORTING
      OUTPUT = LST_MGRNR.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = TXT_AUFNR
    IMPORTING
      OUTPUT = TXT_AUFNR.

  W_HEADER-DOCNR = TXT_DOCNR.
  W_HEADER-VBELN = TXT_VBELN.
  W_HEADER-BSTKD = TXT_BSTKD.
  W_HEADER-AUFNR = TXT_AUFNR.
  W_HEADER-KTEXT = TXT_KTEXT.
  W_HEADER-DOCTYPE = LST_DOCTYPE.

  W_HEADER-REFTYPE = 'UE'.

  W_HEADER-PMNTTYP = TXT_PMNT_TYP.
  W_HEADER-PRCNT = TXT_PRCNT.

  IF CHK_VAT EQ 'X'.
    W_HEADER-VAT = TXT_VAT.
  ELSE.
    W_HEADER-VAT = SPACE.
  ENDIF.

  IF CHK_WHT EQ 'X'.
    W_HEADER-WHTAX = TXT_WHT.
  ELSE.
    W_HEADER-WHTAX = SPACE.
  ENDIF.

  W_HEADER-KWERT = TXT_ADV. " Add by aphai 06.11.2015

  W_HEADER-DOCDATE = TXT_DOCDATE.
  W_HEADER-KUNNR = TXT_KUNNR.
  W_HEADER-NAME1 = TXT_NAME1.
  W_HEADER-MGRNR = LST_MGRNR.
  W_HEADER-POSDESC = TXT_POS.
  W_HEADER-TOPIC = TXT_TOPIC.
  W_HEADER-ATTEND = TXT_ATTEND.
  W_HEADER-ZTERM = TXT_ZTERM.
  W_HEADER-ERDAT = TXT_ERDAT.
  W_HEADER-ERTIM = TXT_ERTIM.
  W_HEADER-ERNAM = TXT_ERNAM.
  W_HEADER-DFLAG = SPACE.
  APPEND W_HEADER TO I_HEADER.


* Prepare detail
  LOOP AT IT ASSIGNING <FSD>.
    MOVE-CORRESPONDING <FSD> TO W_DETAIL_DB.
    W_DETAIL_DB-DOCNR = TXT_DOCNR.
    W_DETAIL_DB-LSTNR = SY-TABIX.
    W_DETAIL_DB-ERDAT = TXT_ERDAT.
    W_DETAIL_DB-ERTIM = TXT_ERTIM.
    W_DETAIL_DB-ERNAM = TXT_ERNAM.
    APPEND W_DETAIL_DB TO I_DETAIL_DB.
    CLEAR W_DETAIL_DB.
  ENDLOOP.

  TRY.

      INSERT ZSDSFIT006 FROM TABLE I_HEADER.
      INSERT ZSDSFIT007 FROM TABLE I_DETAIL_DB.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CONCATENATE 'Create billing No :' TXT_DOCNR 'complete!' INTO MSG SEPARATED BY SPACE.
      MESSAGE S000(38) WITH MSG DISPLAY LIKE 'S'.

      LEAVE TO SCREEN 0.
    CATCH CX_SY_DYNAMIC_OSQL_ERROR INTO OREF.
      GV_MESSAGE = OREF->GET_TEXT( ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE E000(38) WITH GV_MESSAGE.
      EXIT.
  ENDTRY.

ENDFORM.                    " F_CREATE_BILL_200
*&---------------------------------------------------------------------*
*&      Form  F_CHEK_REQUIRE_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_R_FIELD  text
*----------------------------------------------------------------------*
FORM F_CHEK_REQUIRE_FIELD  CHANGING R TYPE N.
  R = 0.
  DATA : ITM TYPE I VALUE 0.

  IF LST_DOCTYPE IS INITIAL.
    MESSAGE S000(38) WITH 'Please input document type!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_DOCDATE IS INITIAL.
    MESSAGE S000(38) WITH 'Please input document date!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_TOPIC IS INITIAL.
    MESSAGE S000(38) WITH 'Please input topic!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_VBELN IS INITIAL.
    MESSAGE S000(38) WITH 'Please input sale order or quotation!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_ATTEND IS INITIAL.
    MESSAGE S000(38) WITH 'Please input attention!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_NAME1 IS INITIAL.
    MESSAGE S000(38) WITH 'Please input customer name!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_ZTERM IS INITIAL.
    MESSAGE S000(38) WITH 'Please input payment terms!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

*  IF txt_aufnr IS INITIAL.
*    MESSAGE s000(38) WITH 'Please input IO!' DISPLAY LIKE 'E'.
*    r = 1.
*    RETURN.
*  ENDIF.

  IF LST_MGRNR IS INITIAL.
    MESSAGE S000(38) WITH 'Please input manager!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_POS IS INITIAL.
    MESSAGE S000(38) WITH 'Please input position!' DISPLAY LIKE 'E'.
    R = 1 .
    RETURN.
  ENDIF.

  IF TXT_PRCNT IS INITIAL.
    MESSAGE S000(38) WITH 'Please input percentage of billing document!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  LOOP AT I_DETAIL INTO W_DETAIL WHERE SEL EQ 'X'.
    ITM = ITM + 1.
  ENDLOOP.

  IF ITM EQ 0.
    MESSAGE S000(38) WITH 'Please select material list !' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.


  CLEAR ITM.
ENDFORM.                    " F_CHEK_REQUIRE_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_CHEK_REQUIRE_FIELD_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_R_FIELD  text
*----------------------------------------------------------------------*
FORM F_CHEK_REQUIRE_FIELD_200  CHANGING R TYPE N.
  R = 0.
  DATA : ITM TYPE I VALUE 0.

  IF LST_DOCTYPE IS INITIAL.
    MESSAGE S000(38) WITH 'Please input document type!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_DOCDATE IS INITIAL.
    MESSAGE S000(38) WITH 'Please input document date!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_TOPIC IS INITIAL.
    MESSAGE S000(38) WITH 'Please input topic!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.


  IF TXT_ATTEND IS INITIAL.
    MESSAGE S000(38) WITH 'Please input attention!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_NAME1 IS INITIAL.
    MESSAGE S000(38) WITH 'Please input customer name!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_ZTERM IS INITIAL.
    MESSAGE S000(38) WITH 'Please input payment terms!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF LST_MGRNR IS INITIAL.
    MESSAGE S000(38) WITH 'Please input manager!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.

  IF TXT_POS IS INITIAL.
    MESSAGE S000(38) WITH 'Please input position!' DISPLAY LIKE 'E'.
    R = 1 .
    RETURN.
  ENDIF.


*  DELETE i_detail_200 WHERE maktx IS INITIAL
*                       OR netpr IS  INITIAL
*                       OR kwmeng IS INITIAL.

  LOOP AT I_DETAIL_200 INTO W_DETAIL_200.
    ITM = ITM + 1.
  ENDLOOP.

  IF ITM EQ 0.
    MESSAGE S000(38) WITH 'Please input billing line items !' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.


  CLEAR ITM.
ENDFORM.                    " F_CHEK_REQUIRE_FIELD_200
*&---------------------------------------------------------------------*
*&      Form  F_DEL_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL_200[]  text
*----------------------------------------------------------------------*
FORM F_DEL_ROWS  TABLES IT STRUCTURE W_DETAIL_200.
  DELETE IT WHERE SEL EQ 'X'.
ENDFORM.                    " F_DEL_ROWS
*&---------------------------------------------------------------------*
*&      Form  F_ADD_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL_200[]  text
*----------------------------------------------------------------------*
FORM F_ADD_ROWS  TABLES IT STRUCTURE W_DETAIL_200.
  TC_DETAIL_200-LINES = TC_DETAIL_200-LINES + 1.
ENDFORM.                    " F_ADD_ROWS
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_LINE_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL  text
*      -->P_W_DETAIL_VBELN  text
*      -->P_W_DETAIL_POSNR  text
*----------------------------------------------------------------------*
FORM F_MODIFY_LINE_ITEMS  TABLES   LT_DETAIL STRUCTURE W_DETAIL
                          USING    L_VBELN TYPE VBAK-VBELN
                                   L_POSNR TYPE VBAP-POSNR
                                   L_SEL TYPE C.

  CHECK : LT_DETAIL[] IS NOT INITIAL,
          L_VBELN IS NOT INITIAL,
          L_POSNR IS NOT INITIAL.

  DATA : W_TMP LIKE LINE OF LT_DETAIL.

  READ TABLE LT_DETAIL INTO W_TMP WITH KEY VBELN = L_VBELN
                                           POSNR = L_POSNR.

  IF SY-SUBRC EQ 0.
    CP_POSNR = L_POSNR + 10.
    IF W_TMP-SEL EQ 'X'.
      MODIFY LT_DETAIL FROM W_TMP TRANSPORTING SEL KWMENG WHERE VBELN EQ L_VBELN AND POSNR GE L_POSNR AND POSNR LT CP_POSNR.
    ELSE.
      IF W_TMP-SEL NE L_SEL.
        READ TABLE I_DETAIL_OLD INTO W_TMP WITH KEY VBELN = L_VBELN
                                                    POSNR = L_POSNR.
        MODIFY LT_DETAIL FROM W_TMP TRANSPORTING SEL KWMENG WHERE VBELN EQ L_VBELN AND POSNR GE L_POSNR AND POSNR LT CP_POSNR.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR : CP_POSNR.

ENDFORM.                    " F_MODIFY_LINE_ITEMS
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL  text
*      -->P_W_DETAIL_VBELN  text
*      -->P_W_DETAIL_POSNR  text
*----------------------------------------------------------------------*
FORM F_MODIFY_BOM  TABLES   LT_DETAIL STRUCTURE W_DETAIL
                   USING    L_VBELN TYPE VBAP-VBELN
                            L_POSNR TYPE VBAP-POSNR
                            L_KWMENG TYPE VBAP-KWMENG
                    CHANGING R_CORRECT TYPE C
                             R_POSNR TYPE VBAP-POSNR.

  CHECK : LT_DETAIL[] IS NOT INITIAL,
        L_VBELN IS NOT INITIAL,
        L_POSNR IS NOT INITIAL,
        L_KWMENG IS NOT INITIAL.
  DATA : W_TMP LIKE LINE OF LT_DETAIL,
         BG_POSNR TYPE VBAP-POSNR,
         V_CORRECT(1) TYPE C VALUE 'X'.

  CP_POSNR = CEIL( L_POSNR / 10 ) * 10.
  BG_POSNR = FLOOR( L_POSNR / 10 ) * 10.

  LOOP AT LT_DETAIL INTO W_TMP WHERE VBELN EQ L_VBELN AND POSNR GT BG_POSNR AND POSNR LT CP_POSNR.
    IF W_TMP-SEL EQ 'X'.
      IF W_TMP-KWMENG NE L_KWMENG.
        CLEAR V_CORRECT.
      ENDIF.
    ELSE.
      CLEAR V_CORRECT.
    ENDIF.
  ENDLOOP.

  IF V_CORRECT EQ 'X'. "Full SET
    W_TMP-SEL = 'X'.
    W_TMP-KWMENG = L_KWMENG.
    MODIFY LT_DETAIL FROM W_TMP TRANSPORTING SEL KWMENG WHERE VBELN EQ L_VBELN AND POSNR EQ BG_POSNR.
  ELSE.
    W_TMP-SEL = SPACE.
    MODIFY LT_DETAIL FROM W_TMP TRANSPORTING SEL WHERE VBELN EQ L_VBELN AND POSNR EQ BG_POSNR.
  ENDIF.

  R_CORRECT = V_CORRECT.
  R_POSNR = BG_POSNR.
  RETURN.
ENDFORM.                    " F_MODIFY_BOM
*&---------------------------------------------------------------------*
*&      Form  F_CAL_AMT_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL[]  text
*----------------------------------------------------------------------*
FORM F_CAL_AMT_BOM  TABLES LT_DETAIL STRUCTURE W_DETAIL.

  CHECK LT_DETAIL[] IS NOT INITIAL.
  DATA : I_BOM TYPE TABLE OF TYP_DETAIL,
         W_BOM LIKE LINE OF I_BOM,
         B_POSNR TYPE VBAP-POSNR.

  FIELD-SYMBOLS : <FS> LIKE LINE OF I_BOM,
                  <FSD> LIKE LINE OF LT_DETAIL.
  LOOP AT LT_DETAIL ASSIGNING <FS> WHERE MEINH EQ 'SET' AND SEL EQ 'X'.
    MOVE-CORRESPONDING <FS> TO W_BOM.
    APPEND W_BOM TO I_BOM.
  ENDLOOP.
  UNASSIGN : <FS>.

  LOOP AT LT_DETAIL ASSIGNING <FS> WHERE MEINH EQ 'SET' AND SEL EQ SPACE.
    <FS>-NETWR = '0.00'.
    <FS>-KWERT = '0.00'.
    MODIFY LT_DETAIL FROM <FS> TRANSPORTING NETWR KWERT WHERE VBELN EQ <FS>-VBELN AND POSNR EQ <FS>-POSNR.
  ENDLOOP.
  UNASSIGN : <FS>.

  CHECK I_BOM[] IS NOT INITIAL.
  LOOP AT I_BOM ASSIGNING <FS>.
    B_POSNR = <FS>-POSNR + 10.
    CLEAR :  <FS>-KWERT,<FS>-NETWR.
    LOOP AT LT_DETAIL ASSIGNING <FSD> WHERE VBELN EQ <FS>-VBELN AND POSNR GT <FS>-POSNR AND POSNR LT B_POSNR.
      <FS>-NETWR = <FS>-NETWR + <FSD>-NETWR.
      <FS>-KWERT = <FS>-KWERT + <FSD>-KWERT.
    ENDLOOP.
    MODIFY LT_DETAIL FROM <FS> TRANSPORTING NETWR KWERT WHERE VBELN EQ <FS>-VBELN AND POSNR EQ <FS>-POSNR.
    CLEAR B_POSNR.

  ENDLOOP.
ENDFORM.                    " F_CAL_AMT_BOM
