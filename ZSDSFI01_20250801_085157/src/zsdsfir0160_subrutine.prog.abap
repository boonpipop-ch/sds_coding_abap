*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0160_SUBRUTINE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_HEADER  text
*      -->P_I_DETAIL  text
*      -->P_I_SUMMARY  text
*----------------------------------------------------------------------*
FORM F_GET_DATA  TABLES   IH STRUCTURE W_HEADER
                          ID STRUCTURE W_DETAIL
                          ISUM STRUCTURE W_SUMMARY.

  FIELD-SYMBOLS : <FSH> LIKE LINE OF IH,
                  <FSD> LIKE LINE OF ID,
                  <FSS> LIKE LINE OF ISUM.

  DATA : IT_TEMP TYPE TABLE OF  TYP_DETAIL,
         LS_TEMP TYPE TYP_DETAIL.

  DATA : IH_TMP TYPE TABLE OF TYP_HEADER.

  SELECT
    DOCNR
    VBELN
    BSTKD
    AUFNR
    KTEXT
    DOCTYPE
    REFTYPE
    PMNTTYP
    PRCNT
    VAT
    WHTAX
    H~KWERT
    DOCDATE
    KUNNR
    NAME1
    MGRNR
    H~POSDESC
    TOPIC
    ATTEND
    ZTERM
    VORNA
    NACHN
    H~ERNAM
    INTO CORRESPONDING FIELDS OF TABLE IH
    FROM ZSDSFIT006 AS H
    LEFT JOIN ZSDSFIT008 AS CFG ON H~MGRNR EQ CFG~PERNR AND CFG~SPRAS EQ '2'
    WHERE DOCNR IN S_DOCNR
    AND DOCDATE IN S_DOCDAT
    AND VBELN IN S_VBELN
    AND ERDAT IN S_ERDAT
    AND DFLAG EQ SPACE.

  SELECT
    D~DOCNR
    LSTNR
    D~VBELN
    POSNR
    D~MATNR
    MAKTX
    KWMENG
    NETPR
    NETWR
    D~KWERT
    MARM~MEINH
    INTO CORRESPONDING FIELDS OF TABLE ID
    FROM ZSDSFIT007 AS D
    LEFT JOIN MARM ON D~MATNR EQ MARM~MATNR AND MARM~UMREZ EQ '1.00' AND MARM~UMREN EQ '1.00'
    INNER JOIN ZSDSFIT006 AS H ON D~DOCNR EQ H~DOCNR
    WHERE
    D~DOCNR IN S_DOCNR
      AND H~DOCDATE IN S_DOCDAT
      AND H~VBELN IN S_VBELN
      AND H~ERDAT IN S_ERDAT
      AND H~DFLAG EQ SPACE
      AND D~DFLAG EQ SPACE.

  SORT IH BY DOCNR.
  SORT ID BY DOCNR VBELN POSNR.

  IT_TEMP[] = ID[].
  IH_TMP[] = IH[].
  SORT IH_TMP BY ERNAM.
  DELETE ADJACENT DUPLICATES FROM IH_TMP COMPARING ERNAM.

  SELECT PERNR
         BNAME
    INTO TABLE I_CONF
    FROM ZSDSFIT008
    FOR ALL ENTRIES IN IH_TMP[]
    WHERE BNAME EQ IH_TMP-ERNAM.
  SORT I_CONF BY BNAME.

  LOOP AT IH ASSIGNING <FSH>.

*    PERFORM f_get_aufnr_desc USING <fsh>-aufnr CHANGING <fsh>-txt_project.
    <FSH>-TXT_PROJECT = <FSH>-KTEXT.
    PERFORM F_GET_TEXT_DATE USING <FSH>-DOCDATE CHANGING <FSH>-TXT_DATE.
    PERFORM F_GET_TEXT_PAYMENT USING <FSH>-ZTERM <FSH>-PMNTTYP CHANGING  <FSH>-PMNTTYP.
    PERFORM F_GET_TEXT_PO USING <FSH>-BSTKD CHANGING <FSH>-TXT_PO.



* Prepare item line for print
    IF <FSH>-REFTYPE NE 'UE'.
      PERFORM F_MODIFY_CASE_FULL_SET TABLES ID
                                     USING <FSH>-DOCNR.
    ENDIF.


    W_SUMMARY-DOCNR = <FSH>-DOCNR.
    LOOP AT ID ASSIGNING <FSD> WHERE DOCNR EQ <FSH>-DOCNR.
      W_SUMMARY-TOT_QTY = W_SUMMARY-TOT_QTY + <FSD>-KWMENG.
      "w_summary-tot_amt1 = w_summary-tot_amt1 + <fsd>-netwr. "Edit by jakarin TR T41K925695 07.02.2017
      W_SUMMARY-TOT_ADV = W_SUMMARY-TOT_ADV + <FSD>-KWERT.
    ENDLOOP .

    LOOP AT IT_TEMP INTO LS_TEMP WHERE DOCNR EQ <FSH>-DOCNR.
      W_SUMMARY-TOT_AMT1 = W_SUMMARY-TOT_AMT1 + ( LS_TEMP-KWMENG * LS_TEMP-NETPR ). "Edit by jakarin TR T41K925695 07.02.2017
      CLEAR LS_TEMP.
    ENDLOOP.

    IF <FSH>-REFTYPE EQ 'UE'.
      W_SUMMARY-TOT_ADV = <FSH>-KWERT.
    ENDIF.

    W_SUMMARY-TOT_AMT2 = W_SUMMARY-TOT_AMT1 + W_SUMMARY-TOT_ADV.
    W_SUMMARY-TOT_VAT = ( W_SUMMARY-TOT_AMT2 * <FSH>-VAT ) / 100.
    W_SUMMARY-TOT_WHT = ( W_SUMMARY-TOT_AMT2 * <FSH>-WHTAX ) / 100.
    W_SUMMARY-NET_AMT = ( W_SUMMARY-TOT_AMT2 + W_SUMMARY-TOT_VAT ) -  W_SUMMARY-TOT_WHT .

    CALL FUNCTION 'ZSD_SPELL_AMOUNT'
      EXPORTING
        AMOUNT     = W_SUMMARY-NET_AMT
        CURRENCY   = 'THB'
        LANGUAGE   = '2'
      IMPORTING
        SPELL_WORD = W_SUMMARY-NET_AMT_TXT
      EXCEPTIONS
        NOT_FOUND  = 1
        TOO_LARGE  = 2
        OTHERS     = 3.

    APPEND W_SUMMARY TO ISUM.
    CLEAR W_SUMMARY.

    READ TABLE I_CONF INTO W_CONF WITH KEY BNAME = <FSH>-ERNAM.
    IF SY-SUBRC EQ 0.
      WRITE W_CONF-PERNR TO <FSH>-PERNR NO-ZERO.
      CLEAR W_CONF.
    ENDIF.

  ENDLOOP.



ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_PRINT_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PRINT_FORM .
  DATA : TOT_HEADER_ROWS TYPE I.
  TOT_HEADER_ROWS = LINES( I_HEADER[] ).
  DATA: LV_OPTION TYPE SSFCOMPOP,
        LV_CONTROL TYPE SSFCTRLOP.

  CLEAR: GWA_CONTROL_PARAMETERS.

  GWA_CONTROL_PARAMETERS-NO_DIALOG = SPACE.
  GWA_CONTROL_PARAMETERS-PREVIEW = 'X'.

  CLEAR: GWA_OUTPUT_OPTIONS.
  GWA_OUTPUT_OPTIONS-TDDEST = 'LOCL'.
  GWA_OUTPUT_OPTIONS-TDNEWID    = 'X'.
  GWA_OUTPUT_OPTIONS-TDIMMED    = 'X'.

* Mass print
  LOOP AT I_HEADER INTO W_HEADER.
    IF SY-TABIX EQ TOT_HEADER_ROWS.
      IF SY-TABIX GT 1.
        GWA_CONTROL_PARAMETERS-NO_OPEN = 'X'.
        GWA_CONTROL_PARAMETERS-NO_CLOSE = SPACE.
      ELSE.
        GWA_CONTROL_PARAMETERS-NO_OPEN = SPACE.
        GWA_CONTROL_PARAMETERS-NO_CLOSE = SPACE.
      ENDIF.
    ELSE.
      IF SY-TABIX NE 1.
        GWA_CONTROL_PARAMETERS-NO_OPEN = 'X'.
        GWA_CONTROL_PARAMETERS-NO_CLOSE = 'X'.
      ELSE.
        GWA_CONTROL_PARAMETERS-NO_OPEN = SPACE.
        GWA_CONTROL_PARAMETERS-NO_CLOSE = 'X'.
      ENDIF.
    ENDIF.

* Get function module name of smart form
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = GC_FORM_NAME
      IMPORTING
        FM_NAME            = GV_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.

    IF LV_CONTROL-NO_DIALOG = SPACE.
      LV_OPTION-TDTITLE = TEXT-002.
    ENDIF.

*  gwa_control_parameters-no_open = space.
*  gwa_control_parameters-no_close = 'X'.


    CALL FUNCTION GV_FM_NAME
      EXPORTING
         ARCHIVE_INDEX        = TOA_DARA
         ARCHIVE_PARAMETERS   = ARC_PARAMS
         CONTROL_PARAMETERS   = GWA_CONTROL_PARAMETERS
         OUTPUT_OPTIONS       = GWA_OUTPUT_OPTIONS
         USER_SETTINGS        = ' '
*    IMPORTING
*      job_output_info       = t_output_info
*    TABLES
*       gt_vbak = gt_vbak
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.
    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
*    v_output_done = t_output_info-outputdone.
    ENDIF.
    CLEAR: LV_CONTROL, LV_OPTION.
  ENDLOOP.


ENDFORM.                    " F_PRINT_FORM
*&---------------------------------------------------------------------*
*&      Form  f_init_data_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INIT_DATA_FORM TABLES
                        IH STRUCTURE W_HEADER
                        ID STRUCTURE W_DETAIL
                        ISUM STRUCTURE W_SUMMARY
                        IF STRUCTURE W_FOOT
                        IFT STRUCTURE W_FOOT_TOTAL
                        ITG STRUCTURE W_FOOT
                      CHANGING
                         WH
*                         wd
*                         wsum
*                         wf
*                         wft
                         .

  SORT : I_HEADER BY DOCNR,
         I_DETAIL BY DOCNR LSTNR.

  FIELD-SYMBOLS : <FS> LIKE LINE OF ID,
                  <FSUM> LIKE LINE OF ISUM,
                  <FH> LIKE LINE OF IH.
  WH = W_HEADER.
  IH[] = I_HEADER[].
  ID[] = I_DETAIL[].
  ISUM[] = I_SUMMARY[].

  SORT IH BY DOCNR.
  DATA : IDX TYPE I,
         TXT_WHT(15) TYPE C,
         TXT_ADV(15) TYPE C,
         I_DETAIL_TMP TYPE TABLE OF TYP_DETAIL,
         W_TMP TYPE TYP_DETAIL,
         REMAIN_ROWS TYPE I.
* Assign Mat to Mat desc
  LOOP AT IH ASSIGNING <FH>.
    IDX = 0.
    LOOP AT ID ASSIGNING <FS> WHERE DOCNR EQ <FH>-DOCNR.
      IDX = IDX + 1.
      <FS>-LSTNR = IDX.
      SHIFT <FS>-LSTNR LEFT DELETING LEADING '0'.
      WRITE <FS>-KWMENG TO <FS>-TXT_KWMENG DECIMALS 0.
      CASE <FS>-MATNR.
        WHEN SPACE OR 'REMARK'.
        WHEN OTHERS.
          <FS>-MAKTX = <FS>-MATNR.
      ENDCASE.
      IF <FS>-MEINH EQ 'SET'.
        <FS>-NETPR = <FS>-NETWR / <FS>-KWMENG.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  UNASSIGN : <FS>,<FH>.

* Append space line
  LOOP AT IH ASSIGNING <FH>.

    LOOP AT ID ASSIGNING <FS> WHERE DOCNR EQ <FH>-DOCNR.
      APPEND <FS> TO I_DETAIL_TMP.
    ENDLOOP.

    REMAIN_ROWS = LINES( I_DETAIL_TMP[] ) MOD 14.

    IF REMAIN_ROWS GT 0.
      REMAIN_ROWS = 14 - REMAIN_ROWS.
      DO REMAIN_ROWS TIMES.
        W_TMP-DOCNR = <FH>-DOCNR.
*        w_tmp-lstnr = '999999'.
        APPEND W_TMP TO ID.
      ENDDO.
    ENDIF.

    CLEAR : I_DETAIL_TMP[],REMAIN_ROWS,W_TMP.
  ENDLOOP.

  IF I_SUMMARY[] IS NOT INITIAL.
    LOOP AT I_SUMMARY ASSIGNING <FSUM>.
      W_FOOT_TOTAL-DOCNR = <FSUM>-DOCNR.
      WRITE <FSUM>-TOT_QTY TO W_FOOT_TOTAL-TOT_QTY DECIMALS 0.
      W_FOOT_TOTAL-TOT_AMT1 = <FSUM>-TOT_AMT1.
      APPEND W_FOOT_TOTAL TO IFT.
      CLEAR W_FOOT_TOTAL.


* Advance Reciept
      W_FOOT-DOCNR = <FSUM>-DOCNR.
      W_FOOT-LIST_TYPE = 'ADV'.
      W_FOOT-LIST_TEXT = 'เงินรับล่วงหน้า'.
      W_FOOT-VALUE = ABS( <FSUM>-TOT_ADV ).
      WRITE W_FOOT-VALUE TO W_FOOT-VALUE_TXT NO-GAP LEFT-JUSTIFIED.
      CONCATENATE '(' W_FOOT-VALUE_TXT ')' INTO W_FOOT-VALUE_TXT.
      W_FOOT-SORT = 1.
      APPEND W_FOOT TO IF.
      CLEAR : W_FOOT.

* Total 2
      W_FOOT-DOCNR = <FSUM>-DOCNR.
      W_FOOT-LIST_TYPE = 'TOT2'.
      W_FOOT-LIST_TEXT = 'รวม'.
      W_FOOT-VALUE = <FSUM>-TOT_AMT2.
      WRITE W_FOOT-VALUE TO W_FOOT-VALUE_TXT NO-GAP LEFT-JUSTIFIED.
      W_FOOT-SORT = 2.
      APPEND W_FOOT TO IF.
      CLEAR :W_FOOT.

*       vat
      IF  <FSUM>-TOT_VAT IS NOT INITIAL.
        W_FOOT-DOCNR = <FSUM>-DOCNR.
        W_FOOT-LIST_TYPE = 'VAT'.
        W_FOOT-LIST_TEXT = 'ภาษีมูลค่าเพิ่ม'.
        W_FOOT-VALUE = <FSUM>-TOT_VAT.
        WRITE W_FOOT-VALUE TO W_FOOT-VALUE_TXT NO-GAP LEFT-JUSTIFIED.
        W_FOOT-SORT = 3.
        APPEND W_FOOT TO IF.
        CLEAR :W_FOOT.
      ENDIF.

* With holding tax
      IF  <FSUM>-TOT_WHT IS NOT INITIAL.
        W_FOOT-DOCNR = <FSUM>-DOCNR.
        W_FOOT-LIST_TYPE = 'WHT'.
        W_FOOT-LIST_TEXT = 'หัก ณ ที่จ่าย'.
        W_FOOT-VALUE = <FSUM>-TOT_WHT.
        WRITE W_FOOT-VALUE TO W_FOOT-VALUE_TXT NO-GAP LEFT-JUSTIFIED.
        CONCATENATE '(' W_FOOT-VALUE_TXT ')' INTO W_FOOT-VALUE_TXT.
        W_FOOT-SORT = 4.
        APPEND W_FOOT TO IF.
        CLEAR :W_FOOT.
      ENDIF.

* Net Amount
      W_FOOT-DOCNR = <FSUM>-DOCNR.
      W_FOOT-LIST_TYPE = 'NET'.
      W_FOOT-LIST_TEXT = 'ยอดสุทธิ'.
      CONCATENATE '(' <FSUM>-NET_AMT_TXT ')' INTO W_FOOT-LIST_TEXT2 SEPARATED BY SPACE.
      W_FOOT-VALUE = <FSUM>-NET_AMT.
      WRITE W_FOOT-VALUE TO W_FOOT-VALUE_TXT NO-GAP LEFT-JUSTIFIED.
      W_FOOT-SORT = 5.
      APPEND W_FOOT TO ITG.
      CLEAR :W_FOOT.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " f_init_data_form
*&---------------------------------------------------------------------*
*&      Form  F_GET_TEXT_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FSH>_DOCDATE  text
*      <--P_<FSH>_TXT_DATE  text
*----------------------------------------------------------------------*
FORM F_GET_TEXT_DATE  USING    P_DOCDATE TYPE ZSDSFIT006-DOCDATE
                      CHANGING P_TEXT.

  CHECK P_DOCDATE IS NOT INITIAL.
  DATA : L_YRS(4) TYPE N,
         L_DYS(2) TYPE N,
         L_MOUNT(30) TYPE C.

  L_YRS = P_DOCDATE+0(4) + 543.
  L_DYS = P_DOCDATE+6(2).

  CASE P_DOCDATE+4(2).
    WHEN '01'.
      L_MOUNT = 'มกราคม'.
    WHEN '02'.
      L_MOUNT = 'กุมภาพันธ์'.
    WHEN '03'.
      L_MOUNT = 'มีนาคม'.
    WHEN '04'.
      L_MOUNT = 'เมษายน'.
    WHEN '05'.
      L_MOUNT = 'พฤษภาคม'.
    WHEN '06'.
      L_MOUNT = 'มิถุนายน'.
    WHEN '07'.
      L_MOUNT = 'กรกฎาคม'.
    WHEN '08'.
      L_MOUNT = 'สิงหาคม'.
    WHEN '09'.
      L_MOUNT = 'กันยายน'.
    WHEN '10'.
      L_MOUNT = 'ตุลาคม'.
    WHEN '11'.
      L_MOUNT = 'พฤศจิกายน'.
    WHEN '12'.
      L_MOUNT = 'ธันวาคม'.

  ENDCASE.

  CONCATENATE 'วันที่' L_DYS L_MOUNT L_YRS INTO P_TEXT SEPARATED BY SPACE.

ENDFORM.                    " F_GET_TEXT_DATE


*&---------------------------------------------------------------------*
*&      Form  f_get_aufnr_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TXT_AUFNR  text
*      -->P_TXT_KTEXT  text
*----------------------------------------------------------------------*
FORM F_GET_AUFNR_DESC  USING    P_TXT_AUFNR TYPE AUFK-AUFNR
                       CHANGING P_TXT_KTEXT.

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
  ELSE.
    CONCATENATE 'Project:' P_TXT_KTEXT INTO P_TXT_KTEXT SEPARATED BY SPACE.
  ENDIF.

ENDFORM.                    " F_GET_AUFNR_DESC
*&---------------------------------------------------------------------*
*&      Form  F_GET_TEXT_PAYMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FSH>_ZTERM  text
*      -->P_<FSH>_PMNTTYP  text
*      <--P_<FSH>_TXT_PAYMENT  text
*----------------------------------------------------------------------*
FORM F_GET_TEXT_PAYMENT  USING    P_ZTERM
                                  P_PMTTYPE
                         CHANGING P_TXT_PAYMENT.


*01	Cash
*02	Cheque
*03	Cheque PDC
  CHECK P_ZTERM IS NOT INITIAL.
  DATA : L_TEXT1 TYPE T052U-TEXT1,
         L_PTYPE(30) TYPE C.

  CASE P_ZTERM.
    WHEN 'A000'.
      P_TXT_PAYMENT = P_PMTTYPE.
    WHEN 'A001'.
      CONCATENATE P_PMTTYPE 'เครดิต 1 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A002'.
      CONCATENATE P_PMTTYPE 'เครดิต 2 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A003'.
      CONCATENATE P_PMTTYPE 'เครดิต 3 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A007'.
      CONCATENATE P_PMTTYPE 'เครดิต 7 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A015'.
      CONCATENATE P_PMTTYPE 'เครดิต 15 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A030'.
      CONCATENATE P_PMTTYPE 'เครดิต 30 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A045'.
      CONCATENATE P_PMTTYPE 'เครดิต 45 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A060'.
      CONCATENATE P_PMTTYPE 'เครดิต 60 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A075'.
      CONCATENATE P_PMTTYPE 'เครดิต 75 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A090'.
      CONCATENATE P_PMTTYPE 'เครดิต 90 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A120'.
      CONCATENATE P_PMTTYPE 'เครดิต 120 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A180'.
      CONCATENATE P_PMTTYPE 'เครดิต 180 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A270'.
      CONCATENATE P_PMTTYPE 'เครดิต 270 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A300'.
      CONCATENATE P_PMTTYPE 'เครดิต 300 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A340'.
      CONCATENATE P_PMTTYPE 'เครดิต 340 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A360'.
      CONCATENATE P_PMTTYPE 'เครดิต 360 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A365'.
      CONCATENATE P_PMTTYPE 'เครดิต 365 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'A730'.
      CONCATENATE P_PMTTYPE 'เครดิต 730 วัน' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'B015'.
      CONCATENATE P_PMTTYPE 'ชำระเงินภายในวันที่ 15 ของเดือนหน้า' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
    WHEN 'B031'.
      CONCATENATE P_PMTTYPE 'ชำระเงินก่อนสิ้นเดือนหน้า' INTO P_TXT_PAYMENT SEPARATED BY SPACE.
  ENDCASE.

ENDFORM.                    " F_GET_TEXT_PAYMENT
*&---------------------------------------------------------------------*
*&      Form  F_GET_TEXT_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FSH>_BSTKD  text
*      <--P_<FSH>_TXT_PO  text
*----------------------------------------------------------------------*
FORM F_GET_TEXT_PO  USING    P_BSTKD TYPE VBKD-BSTKD
                    CHANGING P_TXT_PO.

  CHECK P_BSTKD IS NOT INITIAL.
  CONCATENATE 'PO:' P_BSTKD INTO P_TXT_PO SEPARATED BY SPACE.
ENDFORM.                    " F_GET_TEXT_PO
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_CASE_FULL_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ID  text
*      -->P_<FSH>_DOCNR  text
*----------------------------------------------------------------------*
FORM F_MODIFY_CASE_FULL_SET  TABLES   IT_DETAIL STRUCTURE W_DETAIL
                             USING    LV_DOCNR TYPE ZSDSFIT006-DOCNR.

  CHECK :IT_DETAIL[] IS NOT INITIAL,
         LV_DOCNR IS NOT INITIAL.

  DATA : B_POSNR TYPE VBAP-POSNR,
         I_BOM TYPE TABLE OF TYP_DETAIL,
         V_FULL TYPE C.

  FIELD-SYMBOLS : <FS> LIKE LINE OF I_BOM,
                  <FSD> LIKE LINE OF IT_DETAIL.
  I_BOM[] = IT_DETAIL[].
  SORT I_BOM BY DOCNR.
  DELETE I_BOM WHERE DOCNR NE LV_DOCNR .

  SORT I_BOM BY MEINH.
  DELETE I_BOM WHERE MEINH NE 'SET'.

  SORT IT_DETAIL[] BY DOCNR VBELN POSNR.
  IF I_BOM[] IS NOT INITIAL.
    LOOP AT I_BOM ASSIGNING <FS> WHERE DOCNR EQ LV_DOCNR.
      V_FULL = 'X'.
      B_POSNR = <FS>-POSNR + 10.
      LOOP AT IT_DETAIL ASSIGNING <FSD> WHERE DOCNR EQ LV_DOCNR AND VBELN EQ <FS>-VBELN AND POSNR GT <FS>-POSNR AND POSNR LT B_POSNR.
        IF <FS>-KWMENG NE <FSD>-KWMENG.
          CLEAR V_FULL.
        ENDIF.
      ENDLOOP.
      IF V_FULL EQ 'X'.
        DELETE  IT_DETAIL WHERE DOCNR EQ LV_DOCNR AND VBELN EQ <FS>-VBELN AND POSNR GT <FS>-POSNR AND POSNR LT B_POSNR.
      ENDIF.
      CLEAR B_POSNR.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_MODIFY_CASE_FULL_SET
