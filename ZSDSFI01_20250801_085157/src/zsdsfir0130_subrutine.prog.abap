*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0130_SUBRUTINE
*&---------------------------------------------------------------------*
FORM F_GET_DATA TABLES
                  I_HEADER STRUCTURE W_HEADER
                  I_DETAIL STRUCTURE W_DETAIL
                  I_VBAP STRUCTURE W_VBAP
                  I_KONV STRUCTURE W_KONV
                  I_DETAIL_OLD STRUCTURE W_DETAIL
                USING
                  L_DOCNR TYPE ZSDSFIT006-DOCNR.

  CHECK L_DOCNR IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = L_DOCNR
    IMPORTING
      OUTPUT = L_DOCNR.

* Header
  SELECT
      DOCNR
      VBELN
      EDATU
      AUFNR
      KTEXT
      BSTKD
      DOCTYPE
      REFTYPE
      PMNTTYP
      PRCNT
      VAT
      WHTAX
      KWERT "Add by aphai on 06.11.2015
      DOCDATE
      KUNNR
      NAME1
      MGRNR
      POSDESC
      TOPIC
      ATTEND
      ZTERM
      ERDAT
      ERTIM
      ERNAM
  INTO CORRESPONDING FIELDS OF TABLE I_HEADER
   FROM ZSDSFIT006
   WHERE DOCNR EQ L_DOCNR
   AND DFLAG EQ SPACE.

* Detail

  CLEAR W_HEADER.
  READ TABLE I_HEADER INTO W_HEADER INDEX 1.

  IF SY-SUBRC EQ 0.

    IF W_HEADER-REFTYPE NE 'UE'.
*      Create with referenece QT/SO
      SELECT
            D~DOCNR
            LSTNR
            D~VBELN
            D~POSNR
            D~EDATU
            D~MATNR
            D~MAKTX
            D~KWMENG
            P~KWMENG AS KWMENG_O
            D~NETPR
            D~NETWR
            D~KWERT
            K~KNUMV
            MARM~MEINH
            MARM~UMREZ
            MARM~UMREN
        INTO CORRESPONDING FIELDS OF TABLE I_DETAIL
         FROM ZSDSFIT007 AS D
              LEFT JOIN MARM ON D~MATNR EQ MARM~MATNR
              LEFT JOIN ZSDSFIT010 AS P ON D~VBELN EQ P~VBELN
                                       AND D~POSNR EQ P~POSNR
                                       AND D~DOCNR EQ P~DOCNR
                                       AND D~ERDAT EQ P~ERDAT
                                       AND D~ERTIM EQ P~ERTIM
             LEFT JOIN ZSDSFIT009 AS K ON D~VBELN EQ K~VBELN
                                       AND D~DOCNR EQ K~DOCNR
                                       AND D~ERDAT EQ K~ERDAT
                                       AND D~ERTIM EQ K~ERTIM
         WHERE D~DOCNR EQ L_DOCNR
         AND DFLAG EQ SPACE.
      DELETE I_DETAIL WHERE
                     MATNR NE 'REMARK'
                     AND MATNR IS NOT INITIAL
                     AND ( UMREZ NE '1.00' OR UMREN NE '1.00' ).

      SORT I_DETAIL BY VBELN POSNR.
    ELSE.

*      User keys IN
      SELECT
            D~DOCNR
            D~LSTNR
            D~VBELN
            D~POSNR
            D~MATNR
            D~MAKTX
            D~KWMENG
            D~NETPR
            D~NETWR
            D~KWERT
        INTO CORRESPONDING FIELDS OF TABLE I_DETAIL
         FROM ZSDSFIT007 AS D
         WHERE D~DOCNR EQ L_DOCNR
         AND DFLAG EQ SPACE.
    ENDIF.
    SORT I_DETAIL BY  LSTNR.
  ENDIF.

* konv
  SELECT
          KNUMV
          KPOSN
          KSCHL
          K~KWERT
    INTO CORRESPONDING FIELDS OF TABLE I_KONV
    FROM ZSDSFIT006 AS H
    INNER JOIN ZSDSFIT011 AS K ON H~DOCNR EQ K~DOCNR
                                  AND H~ERDAT EQ K~ERDAT
                                  AND H~ERTIM EQ K~ERTIM
    WHERE H~DOCNR EQ L_DOCNR
    AND H~DFLAG EQ SPACE
    AND ( K~KSCHL EQ 'ZDP3' OR K~KSCHL EQ 'ZDP2' ).


* vbap
  SELECT
      H~VBELN
      P~POSNR
      P~MATNR
      P~KWMENG
      P~NETPR
 INTO CORRESPONDING FIELDS OF TABLE I_VBAP
 FROM ZSDSFIT006 AS H
 INNER JOIN ZSDSFIT007 AS D ON H~DOCNR EQ D~DOCNR
 INNER JOIN ZSDSFIT010 AS P ON D~DOCNR EQ P~DOCNR
                                AND D~VBELN EQ P~VBELN
                                AND D~POSNR EQ P~POSNR
                                AND D~ERDAT EQ P~ERDAT
                                AND D~ERTIM EQ P~ERTIM

    WHERE H~DOCNR EQ L_DOCNR
    AND H~DFLAG EQ SPACE
 .

  IF I_VBAP[] IS NOT INITIAL.
    LOOP AT I_VBAP INTO W_VBAP.
      MOVE-CORRESPONDING W_VBAP TO W_DETAIL_OLD.
      APPEND W_DETAIL_OLD TO I_DETAIL_OLD.
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
FORM F_CAL_AMT TABLES IT STRUCTURE W_DETAIL.

  CHECK IT[] IS NOT INITIAL.
  FIELD-SYMBOLS : <FS> LIKE LINE OF IT.
  CLEAR : TXT_TOTAL_AMT1,TXT_ADV,TXT_TOTAL_QTY.

  LOOP AT IT ASSIGNING <FS> WHERE MEINH NE 'SET'.
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
*&      Form  f_simulate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_SIMULATE.

  IF TXT_ZTERM NE TXT_ZTERM_O.
    PERFORM F_GET_TERM_DESC USING TXT_ZTERM CHANGING TXT_PMNT_DESC
                                                      TXT_PMNT_TYP.
  ENDIF.

  IF TXT_PRCNT NE TXT_PRCNT_O.
    PERFORM F_ASSIGN_TOPIC IN PROGRAM ZAR_MSBILL_CRED IF FOUND USING LST_DOCTYPE TXT_PRCNT CHANGING TXT_TOPIC TXT_ATTEND.
  ENDIF.


  PERFORM F_CAL_PERCENTAGE_OF_DOC TABLES I_DETAIL[].
  PERFORM F_CAL_AMT TABLES I_DETAIL[].
  PERFORM F_CAL_AMT_BOM TABLES I_DETAIL[].

* Keep old value
  TXT_PRCNT_O =  TXT_PRCNT.
  TXT_VBELN_O = TXT_VBELN.
  TXT_EDATU_O = TXT_EDATU.
  TXT_ZTERM_O = TXT_ZTERM.
  TXT_PMNT_TYP_O = TXT_PMNT_TYP.
  LST_MGRNR_O = LST_MGRNR.
  LST_DOCTYPE_O = LST_DOCTYPE.
  TXT_VAT_O = TXT_VAT.
  TXT_WHT_O = TXT_WHT.

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
ENDFORM.                    "f_get_term_desc

*&---------------------------------------------------------------------*
*&      Form  f_cal_percentage_of_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM F_CAL_PERCENTAGE_OF_DOC  TABLES IT STRUCTURE W_DETAIL.
  CHECK IT[] IS NOT INITIAL.

  SORT I_DETAIL_OLD BY ADD VBELN POSNR.
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
*&      Form  f_chek_require_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R          text
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

  LOOP AT I_DETAIL INTO W_DETAIL.
    ITM = ITM + 1.
  ENDLOOP.

  IF ITM EQ 0.
    MESSAGE S000(38) WITH 'Please input billing items!' DISPLAY LIKE 'E'.
    R = 1.
    RETURN.
  ENDIF.


  CLEAR ITM.
ENDFORM.                    " F_CHEK_REQUIRE_FIELD

*&---------------------------------------------------------------------*
*&      Form  f_tc_detail_200_lines
*&---------------------------------------------------------------------*
*       text
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
*&      Form  f_maktx_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MAKTX    text
*      -->V_MATNR    text
*      -->V_MAKTX    text
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
*&      Form  f_cal_amt_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
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
*&      Form  f_simulate_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_SIMULATE_200 .

  IF TXT_ZTERM NE TXT_ZTERM_O.
    PERFORM F_GET_TERM_DESC IN PROGRAM ZAR_MSBILL_CRED IF FOUND
                                                    USING
                                                      TXT_ZTERM
                                                    CHANGING
                                                      TXT_PMNT_DESC
                                                      TXT_PMNT_TYP.
  ENDIF.

  IF TXT_KUNNR NE TXT_KUNNR_O.
    PERFORM F_GET_KUNNR_DESC IN PROGRAM ZAR_MSBILL_CRED IF FOUND USING TXT_KUNNR CHANGING TXT_NAME1.
  ENDIF.

  IF TXT_AUFNR NE TXT_AUFNR_O.
    PERFORM F_GET_AUFNR_DESC IN PROGRAM ZAR_MSBILL_CRED IF FOUND USING TXT_AUFNR CHANGING TXT_KTEXT.
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
  TXT_ADV_O = TXT_ADV.

ENDFORM.                    " F_SIMULATE_200

*&---------------------------------------------------------------------*
*&      Form  f_assign_pos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MGRNR    text
*      -->P_POS      text
*----------------------------------------------------------------------*
FORM F_ASSIGN_POS  USING    P_MGRNR
                   CHANGING P_POS.

  READ TABLE I_APV INTO W_APV WITH KEY PERNR = P_MGRNR.
  IF SY-SUBRC EQ 0.
    P_POS = W_APV-POSDESC.
  ENDIF.

ENDFORM.                    " F_ASSIGN_POS

*&---------------------------------------------------------------------*
*&      Form  f_chek_require_field_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R          text
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
*&      Form  F_CHANGE_BILL_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL_200[]  text
*----------------------------------------------------------------------*
FORM F_SAVE_CHANGED_BILL_200  TABLES   IT STRUCTURE W_DETAIL_200.
  CHECK IT[] IS NOT INITIAL.
  DATA : RSEL TYPE I,
        MSG(100) TYPE C.
  FIELD-SYMBOLS : <FSD> LIKE LINE OF IT.

  CLEAR : I_DETAIL_DB[],W_HEADER.
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

  TXT_AEDAT = SY-DATUM.
  TXT_AETIM = SY-UZEIT.
  TXT_AENAM = SY-UNAME.


  IF CHK_VAT NE 'X'.
    CLEAR TXT_VAT.
  ENDIF.

  IF CHK_WHT NE 'X'.
    CLEAR TXT_WHT.
  ENDIF.

  W_HEADER-KWERT = TXT_ADV. " Add by aphai 06.11.2015

  READ TABLE I_HEADER INTO W_HEADER INDEX 1.

* Prepare detail
  LOOP AT IT ASSIGNING <FSD>.
    MOVE-CORRESPONDING <FSD> TO W_DETAIL_DB.
    IF <FSD>-ADD IS INITIAL.
      W_DETAIL_DB-DOCNR = TXT_DOCNR.
      W_DETAIL_DB-LSTNR = SY-TABIX.
      W_DETAIL_DB-AEDAT = TXT_AEDAT.
      W_DETAIL_DB-AETIM = TXT_AETIM.
      W_DETAIL_DB-AENAM = TXT_AENAM.
      W_DETAIL_DB-ERDAT = W_HEADER-ERDAT.
      W_DETAIL_DB-ERTIM = W_HEADER-ERTIM.
      W_DETAIL_DB-ERNAM = W_HEADER-ERNAM.
    ELSE.
      W_DETAIL_DB-DOCNR = TXT_DOCNR.
      W_DETAIL_DB-LSTNR = SY-TABIX.
      W_DETAIL_DB-AEDAT = TXT_AEDAT.
      W_DETAIL_DB-AETIM = TXT_AETIM.
      W_DETAIL_DB-AENAM = TXT_AENAM.
      W_DETAIL_DB-ERDAT = TXT_AEDAT.
      W_DETAIL_DB-ERTIM = TXT_AETIM.
      W_DETAIL_DB-ERNAM = TXT_AENAM.
    ENDIF.
    APPEND W_DETAIL_DB TO I_DETAIL_DB.
    CLEAR W_DETAIL_DB.
  ENDLOOP.


  TRY.

      UPDATE ZSDSFIT006       SET
                                BSTKD = TXT_BSTKD
                                AUFNR = TXT_AUFNR
                                KTEXT = TXT_KTEXT
                                DOCTYPE = LST_DOCTYPE
                                PMNTTYP = TXT_PMNT_TYP
                                PRCNT = TXT_PRCNT
                                VAT = TXT_VAT
                                WHTAX = TXT_WHT
                                KWERT = TXT_ADV
                                DOCDATE = TXT_DOCDATE
                                KUNNR = TXT_KUNNR
                                NAME1 = TXT_NAME1
                                MGRNR = LST_MGRNR
                                POSDESC = TXT_POS
                                TOPIC = TXT_TOPIC
                                ATTEND = TXT_ATTEND
                                ZTERM = TXT_ZTERM
                                AEDAT = TXT_AEDAT
                                AETIM = TXT_AETIM
                                AENAM = TXT_AENAM
                                WHERE DOCNR EQ TXT_DOCNR.

      DELETE FROM ZSDSFIT007 WHERE DOCNR EQ TXT_DOCNR.
      INSERT ZSDSFIT007 FROM TABLE I_DETAIL_DB.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CONCATENATE 'Change billing No :' TXT_DOCNR 'complete!' INTO MSG SEPARATED BY SPACE.
      MESSAGE S000(38) WITH MSG DISPLAY LIKE 'S'.

      LEAVE TO SCREEN 0.
    CATCH CX_SY_DYNAMIC_OSQL_ERROR INTO OREF.
      GV_MESSAGE = OREF->GET_TEXT( ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE E000(38) WITH GV_MESSAGE.
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
FORM F_DEL_REMARK  TABLES IT STRUCTURE W_DETAIL.

  DATA : WA LIKE LINE OF IT.

  LOOP AT IT INTO WA WHERE SEL EQ 'X' AND MEINH EQ 'SET'.
    GV_POSNR = WA-POSNR + 10.
    MODIFY IT FROM WA TRANSPORTING SEL WHERE VBELN EQ WA-VBELN AND POSNR GT WA-POSNR AND POSNR LT GV_POSNR.
    CLEAR GV_POSNR.
  ENDLOOP.

  LOOP AT IT INTO WA WHERE SEL EQ 'X' AND MEINH NE 'SET'.
    GV_POSNR = FLOOR( WA-POSNR / 10 ) * 10.
    MODIFY IT FROM WA TRANSPORTING SEL WHERE VBELN EQ WA-VBELN AND POSNR EQ GV_POSNR.
    CLEAR GV_POSNR.
  ENDLOOP.

  DELETE IT WHERE SEL EQ 'X'.
  PERFORM F_SIMULATE.
ENDFORM.                    " F_DEL_REMARK


*&---------------------------------------------------------------------*
*&      Form  f_assign_header_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM F_ASSIGN_HEADER_DATA  TABLES IT STRUCTURE W_HEADER.

  CLEAR : TXT_ATTEND,TXT_KUNNR,TXT_NAME1,TXT_ZTERM,TXT_PMNT_DESC,TXT_BSTKD,TXT_AUFNR,TXT_KTEXT.
  CHECK IT[] IS NOT INITIAL.
  READ TABLE IT INTO W_HEADER INDEX 1.


  LST_DOCTYPE = W_HEADER-DOCTYPE.
  TXT_DOCNR = W_HEADER-DOCNR.
  TXT_VBELN = W_HEADER-VBELN.
  TXT_EDATU = W_HEADER-EDATU.
  TXT_ATTEND = W_HEADER-ATTEND.
  TXT_KUNNR = W_HEADER-KUNNR.
  TXT_NAME1 = W_HEADER-NAME1.
  TXT_ZTERM = W_HEADER-ZTERM.
  TXT_VAT = W_HEADER-VAT.
  TXT_WHT = W_HEADER-WHTAX.
  TXT_PRCNT = W_HEADER-PRCNT.
  TXT_TOPIC = W_HEADER-TOPIC.
  TXT_PMNT_TYP = W_HEADER-PMNTTYP.
  TXT_DOCDATE = W_HEADER-DOCDATE.

  IF W_HEADER-VAT IS NOT INITIAL.
    CHK_VAT = 'X'.
  ELSE.
    TXT_VAT = 7.
  ENDIF.

  IF W_HEADER-WHTAX IS NOT INITIAL.
    CHK_WHT = 'X'.
  ELSE.
    TXT_WHT = 3.
  ENDIF.

  TXT_ADV = W_HEADER-KWERT. " Add by aphai 06.11.2015

  PERFORM F_GET_TERM_DESC
              USING
                 W_HEADER-ZTERM
              CHANGING
                 TXT_PMNT_DESC
                 TXT_PMNT_TYP.

  TXT_BSTKD = W_HEADER-BSTKD.
  TXT_AUFNR = W_HEADER-AUFNR.
  TXT_KTEXT = W_HEADER-KTEXT.

  IF TXT_KTEXT IS INITIAL AND W_HEADER-AUFNR IS NOT INITIAL.
    PERFORM F_GET_AUFNR_DESC IN PROGRAM ZAR_MSBILL_CRED IF FOUND USING W_HEADER-AUFNR CHANGING TXT_KTEXT.
  ENDIF.

  TXT_POS = W_HEADER-POSDESC.
  LST_MGRNR = W_HEADER-MGRNR.


ENDFORM.                    " F_ASSIGN_HEADER_DATA

*&---------------------------------------------------------------------*
*&      Form  f_assign_manager
*&---------------------------------------------------------------------*
*       text
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
*&      Form  f_add_remark
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT         text
*----------------------------------------------------------------------*
FORM F_ADD_REMARK  TABLES   IT STRUCTURE W_DETAIL.
  CHECK IT[] IS NOT INITIAL.
  DATA : WA LIKE LINE OF IT.

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

  WA-VBELN = TXT_VBELN.
  WA-POSNR = '999999'.
  WA-MATNR = 'REMARK'.
  WA-KWMENG = 1.
  WA-KWMENG_O = 1.
  WA-ADD = 'X'.
  APPEND WA TO IT.
  CLEAR WA.
ENDFORM.                    " F_ADD_REMARK
*&---------------------------------------------------------------------*
*&      Form  F_GET_VBAP_POP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VBAP_POP  text
*      -->P_TXT_VBELN  text
*----------------------------------------------------------------------*
FORM F_GET_VBAP_POP  TABLES   IT STRUCTURE W_VBAP_POP
                     USING    L_VBELN TYPE VBAP-VBELN.
  CHECK L_VBELN IS NOT INITIAL.
  CLEAR IT[].
  FIELD-SYMBOLS : <FS> LIKE LINE OF I_DETAIL,
                  <FS1> LIKE LINE OF IT.
  DATA : WTMP LIKE LINE OF IT,
         IT_KONV TYPE STANDARD TABLE OF TYP_KONV.
  SELECT
    VBAP~VBELN
    VBAP~POSNR
    VBEP~EDATU
    VBAP~MATNR
    MAKT~MAKTX
    VBAP~NETPR
    VBAP~KWMENG
    VBAP~NETWR
    VBAK~KNUMV
    MARM~MEINH
  INTO CORRESPONDING FIELDS OF TABLE IT
  FROM VBAP
   INNER JOIN VBAK ON VBAP~VBELN EQ VBAK~VBELN
   INNER JOIN VBEP ON VBAP~VBELN EQ VBEP~VBELN AND VBAP~POSNR EQ VBEP~POSNR AND VBEP~ETENR EQ '1'
   INNER JOIN MARM ON VBAP~MATNR EQ MARM~MATNR
   INNER JOIN MAKT ON VBAP~MATNR EQ MAKT~MATNR AND MAKT~SPRAS EQ 'E'
   WHERE VBAP~VBELN EQ L_VBELN
    AND MARM~UMREZ EQ '1.00'
    AND MARM~UMREN EQ '1.00'.

  IF I_DETAIL[] IS NOT INITIAL.
    LOOP AT I_DETAIL ASSIGNING <FS>.
      READ TABLE IT INTO WTMP WITH KEY VBELN = <FS>-VBELN POSNR = <FS>-POSNR.
      IF SY-SUBRC EQ 0.
        DELETE IT WHERE VBELN EQ <FS>-VBELN AND POSNR EQ <FS>-POSNR.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CHECK IT[] IS NOT INITIAL.
  SELECT
        KNUMV
        KPOSN
        KSCHL
        KWERT
  INTO TABLE IT_KONV
  FROM KONV
  FOR ALL ENTRIES IN IT
  WHERE KONV~KNUMV EQ IT-KNUMV
  AND   KONV~KPOSN EQ IT-POSNR
  AND ( KONV~KSCHL EQ 'ZDP3' OR KONV~KSCHL EQ 'ZDP2' ).

* Calculate Adv
  IF I_KONV[] IS NOT INITIAL.
    LOOP AT IT ASSIGNING <FS1>.
      LOOP AT I_KONV INTO W_KONV WHERE KNUMV = <FS1>-KNUMV AND KPOSN = <FS1>-POSNR.
        <FS1>-KWERT = <FS1>-KWERT + W_KONV-KWERT.
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
FORM F_ADD_LINE_ITEMS  TABLES   IVB STRUCTURE W_VBAP_POP
                                IDT STRUCTURE W_DETAIL
                                IDTO STRUCTURE  W_DETAIL_OLD
                                IVP STRUCTURE W_VBAP.

  CHECK IVB[] IS NOT INITIAL.
  CLEAR : W_VBAP_POP,W_DETAIL.
  DATA : I_BOM TYPE TABLE OF TYP_DETAIL.

  FIELD-SYMBOLS : <FSB> LIKE LINE OF I_BOM.
*  check BoM
  LOOP AT IVB INTO W_VBAP_POP WHERE SEL EQ 'X' AND MEINH EQ 'SET'.
    GV_POSNR = W_VBAP_POP-POSNR + 10.
    MODIFY IVB FROM W_VBAP_POP TRANSPORTING SEL
                               WHERE VBELN EQ W_VBAP_POP-VBELN
                               AND POSNR GT W_VBAP_POP-POSNR
                               AND POSNR LT GV_POSNR.
    CLEAR GV_POSNR.
    APPEND W_VBAP_POP TO I_BOM.
  ENDLOOP.

  LOOP AT IVB INTO W_VBAP_POP WHERE SEL EQ 'X'.
    MOVE-CORRESPONDING W_VBAP_POP TO W_DETAIL.
    MOVE-CORRESPONDING W_VBAP_POP TO W_VBAP.
    W_DETAIL-ADD = 'X'.
    W_DETAIL-KWMENG_O = W_DETAIL-KWMENG.
    APPEND W_DETAIL TO IDT.
    APPEND W_DETAIL TO IDTO.
    APPEND W_VBAP TO I_VBAP.
  ENDLOOP.
  CLEAR : W_DETAIL,W_VBAP,W_VBAP_POP.
  SORT IDT BY VBELN POSNR.
* Modify Quantity when add BoM
  IF I_BOM[] IS NOT INITIAL.
    LOOP AT I_BOM ASSIGNING <FSB>.
      GV_POSNR = <FSB>-POSNR + 10.
      MODIFY IDT FROM <FSB> TRANSPORTING KWMENG WHERE VBELN EQ <FSB>-VBELN
                                                AND   POSNR GT <FSB>-POSNR
                                                AND   POSNR LT GV_POSNR.
      CLEAR GV_POSNR.
    ENDLOOP.
  ENDIF.

* Calculate & check full set of BoM
  LOOP AT IDT INTO W_DETAIL WHERE MEINH NE 'SET'.
    PERFORM F_MODIFY_BOM TABLES I_DETAIL
                                    USING W_DETAIL-VBELN
                                          W_DETAIL-POSNR
                                          W_DETAIL-KWMENG
                                   .
  ENDLOOP.


  PERFORM F_SIMULATE.
  CLEAR W_DETAIL.


ENDFORM.                    " F_ADD_LINE_ITEMS
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_CHANGED_BILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DETAIL  text
*----------------------------------------------------------------------*
FORM F_SAVE_CHANGED_BILL  TABLES  IT STRUCTURE W_DETAIL.
  CHECK IT[] IS NOT INITIAL.
  DATA :
        MSG(100) TYPE C.
  FIELD-SYMBOLS : <FSD> LIKE LINE OF IT.

  CLEAR : I_DETAIL_DB[],W_HEADER.
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

  TXT_AEDAT = SY-DATUM.
  TXT_AETIM = SY-UZEIT.
  TXT_AENAM = SY-UNAME.

  IF TXT_VBELN+0(1) EQ '2'.
    TXT_REFTYPE = 'QT'.
  ELSE.
    TXT_REFTYPE = 'SO'.
  ENDIF.

  IF CHK_VAT NE 'X'.
    CLEAR TXT_VAT.
  ENDIF.

  IF CHK_WHT NE 'X'.
    CLEAR TXT_WHT.
  ENDIF.


  READ TABLE I_HEADER INTO W_HEADER INDEX 1.

* Prepare detail
  LOOP AT IT ASSIGNING <FSD>.
    IF <FSD>-MEINH NE 'SET'.
      MOVE-CORRESPONDING <FSD> TO W_DETAIL_DB.
    ELSE.
      IF <FSD>-FULL_SET EQ 'X'.
        MOVE-CORRESPONDING <FSD> TO W_DETAIL_DB.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF <FSD>-ADD IS INITIAL.
      W_DETAIL_DB-DOCNR = TXT_DOCNR.
      W_DETAIL_DB-LSTNR = SY-TABIX.
      W_DETAIL_DB-AEDAT = TXT_AEDAT.
      W_DETAIL_DB-AETIM = TXT_AETIM.
      W_DETAIL_DB-AENAM = TXT_AENAM.
      W_DETAIL_DB-ERDAT = W_HEADER-ERDAT.
      W_DETAIL_DB-ERTIM = W_HEADER-ERTIM.
      W_DETAIL_DB-ERNAM = W_HEADER-ERNAM.
    ELSE.
      W_DETAIL_DB-DOCNR = TXT_DOCNR.
      W_DETAIL_DB-LSTNR = SY-TABIX.
      W_DETAIL_DB-AEDAT = TXT_AEDAT.
      W_DETAIL_DB-AETIM = TXT_AETIM.
      W_DETAIL_DB-AENAM = TXT_AENAM.
      W_DETAIL_DB-ERDAT = TXT_AEDAT.
      W_DETAIL_DB-ERTIM = TXT_AETIM.
      W_DETAIL_DB-ERNAM = TXT_AENAM.
    ENDIF.
    APPEND W_DETAIL_DB TO I_DETAIL_DB.
    CLEAR W_DETAIL_DB.
  ENDLOOP.

*Prepare VBAK at time
  PERFORM F_GET_VBAK_AT_TIME IN PROGRAM ZAR_MSBILL_CRED IF FOUND  TABLES I_VBAK_ATME USING TXT_VBELN.
  W_VBAK_ATME-DOCNR = TXT_DOCNR.
  W_VBAK_ATME-ERDAT = TXT_AEDAT.
  W_VBAK_ATME-ERTIM = TXT_AETIM.
  W_VBAK_ATME-ERNAM = TXT_AENAM.
  MODIFY I_VBAK_ATME FROM W_VBAK_ATME TRANSPORTING DOCNR ERDAT ERTIM ERNAM WHERE VBELN IS NOT INITIAL.
  CLEAR W_VBAK_ATME.

*Prepare VBAP at time
  PERFORM F_GET_VBAP_AT_TIME IN PROGRAM ZAR_MSBILL_CRED IF FOUND TABLES I_VBAP_ATME USING TXT_VBELN.
  W_VBAP_ATME-DOCNR = TXT_DOCNR.
  W_VBAP_ATME-ERDAT = TXT_AEDAT.
  W_VBAP_ATME-ERTIM = TXT_AETIM.
  W_VBAP_ATME-ERNAM = TXT_AENAM.
  MODIFY I_VBAP_ATME FROM W_VBAP_ATME TRANSPORTING DOCNR ERDAT ERTIM ERNAM WHERE VBELN IS NOT INITIAL.
  CLEAR W_VBAP_ATME.

*Prepare KONV at time
  PERFORM F_GET_KONV_AT_TIME IN PROGRAM ZAR_MSBILL_CRED IF FOUND  TABLES I_KONV_ATME I_KONV.
  W_KONV_ATME-DOCNR = TXT_DOCNR.
  W_KONV_ATME-ERDAT = TXT_AEDAT.
  W_KONV_ATME-ERTIM = TXT_AETIM.
  W_KONV_ATME-ERNAM = TXT_AENAM.
  MODIFY I_KONV_ATME FROM W_KONV_ATME TRANSPORTING DOCNR ERDAT ERTIM ERNAM WHERE KNUMV IS NOT INITIAL.
  CLEAR W_KONV_ATME.

  TRY.

      UPDATE ZSDSFIT006       SET
                                BSTKD = TXT_BSTKD
                                AUFNR = TXT_AUFNR
                                KTEXT = TXT_KTEXT
                                DOCTYPE = LST_DOCTYPE
                                REFTYPE = TXT_REFTYPE
                                PMNTTYP = TXT_PMNT_TYP
                                PRCNT = TXT_PRCNT
                                VAT = TXT_VAT
                                WHTAX = TXT_WHT
                                DOCDATE = TXT_DOCDATE
                                KUNNR = TXT_KUNNR
                                NAME1 = TXT_NAME1
                                MGRNR = LST_MGRNR
                                POSDESC = TXT_POS
                                TOPIC = TXT_TOPIC
                                ATTEND = TXT_ATTEND
                                ZTERM = TXT_ZTERM
                                AEDAT = TXT_AEDAT
                                AETIM = TXT_AETIM
                                AENAM = TXT_AENAM
                                WHERE DOCNR EQ TXT_DOCNR.

      DELETE FROM ZSDSFIT007 WHERE DOCNR EQ TXT_DOCNR.
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

      CONCATENATE 'Change billing No :' TXT_DOCNR 'complete!' INTO MSG SEPARATED BY SPACE.
      MESSAGE S000(38) WITH MSG DISPLAY LIKE 'S'.

      LEAVE TO SCREEN 0.
    CATCH CX_SY_DYNAMIC_OSQL_ERROR INTO OREF.
      GV_MESSAGE = OREF->GET_TEXT( ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE E000(38) WITH GV_MESSAGE.
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
FORM F_DEL_ROWS  TABLES IT STRUCTURE W_DETAIL_200.
  DELETE IT WHERE SEL EQ 'X'.
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
FORM F_DEL_BILLING_DOCUMENT  USING    P_DOCNR.
  DATA MSG(100) TYPE C.
  TRY.

      UPDATE ZSDSFIT006       SET
                                DFLAG = 'X'
                                WHERE DOCNR EQ P_DOCNR.

      UPDATE ZSDSFIT007       SET
                                DFLAG = 'X'
                                WHERE DOCNR EQ P_DOCNR.


      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CONCATENATE 'Delete billing No :' P_DOCNR 'complete!' INTO MSG SEPARATED BY SPACE.
      MESSAGE S000(38) WITH MSG DISPLAY LIKE 'S'.

      LEAVE TO SCREEN 0.
    CATCH CX_SY_DYNAMIC_OSQL_ERROR INTO OREF.
      GV_MESSAGE = OREF->GET_TEXT( ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE E000(38) WITH GV_MESSAGE.
      EXIT.
  ENDTRY.
ENDFORM.                    " F_DEL_BILLING_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  f_modify_line_items
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_DETAIL  text
*      -->L_VBELN    text
*      -->L_POSNR    text
*      -->L_SEL      text
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
    MODIFY LT_DETAIL FROM W_TMP TRANSPORTING SEL KWMENG WHERE VBELN EQ L_VBELN AND POSNR GE L_POSNR AND POSNR LT CP_POSNR.

    W_TMP-FULL_SET = 'X'.
    MODIFY LT_DETAIL FROM W_TMP TRANSPORTING FULL_SET WHERE VBELN EQ L_VBELN AND POSNR EQ L_POSNR.
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
                    .

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
    IF W_TMP-KWMENG NE L_KWMENG.
      CLEAR V_CORRECT.
    ENDIF.
  ENDLOOP.

  IF V_CORRECT EQ 'X'. "Full SET
    W_TMP-KWMENG = L_KWMENG.
    W_TMP-FULL_SET = 'X'.
    MODIFY LT_DETAIL FROM W_TMP TRANSPORTING KWMENG FULL_SET WHERE VBELN EQ L_VBELN AND POSNR EQ BG_POSNR.
  ELSE.
    W_TMP-FULL_SET = SPACE.
    MODIFY LT_DETAIL FROM W_TMP TRANSPORTING FULL_SET WHERE VBELN EQ L_VBELN AND POSNR EQ BG_POSNR.
  ENDIF.

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
  LOOP AT LT_DETAIL ASSIGNING <FS> WHERE MEINH EQ 'SET' AND FULL_SET EQ 'X'.
    MOVE-CORRESPONDING <FS> TO W_BOM.
    APPEND W_BOM TO I_BOM.
  ENDLOOP.
  UNASSIGN : <FS>.

  LOOP AT LT_DETAIL ASSIGNING <FS> WHERE MEINH EQ 'SET' AND FULL_SET EQ SPACE.
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
