*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0290_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
  ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEL_SCRN_SET_PROPERTIES
*&---------------------------------------------------------------------*
FORM F_SEL_SCRN_SET_PROPERTIES .
  LOOP AT SCREEN.
    CASE GC_TRUE.
      WHEN RB_NEW.
        IF SCREEN-GROUP1(1) = 'A'.
          IF SCREEN-GROUP1+1(2) = 'FI'.
            IF P_SELFI = GC_TRUE.
              SCREEN-INPUT = 1.
              SCREEN-OUTPUT = 1.
              SCREEN-ACTIVE = 1.
            ELSE.
              SCREEN-INPUT = 0.
              SCREEN-OUTPUT = 1.
              SCREEN-ACTIVE = 1.
            ENDIF.
          ELSE.
            SCREEN-INPUT = 1.
            SCREEN-OUTPUT = 1.
            SCREEN-ACTIVE = 1.
          ENDIF.
        ELSEIF SCREEN-GROUP1(1) = 'B'.
          SCREEN-INPUT = 0.
          SCREEN-OUTPUT = 1.
          SCREEN-ACTIVE = 1.
        ENDIF.
        MODIFY SCREEN.
      WHEN RB_EXIST.
        IF SCREEN-GROUP1(1) = 'A'.
          SCREEN-INPUT = 0.
          SCREEN-OUTPUT = 1.
          SCREEN-ACTIVE = 1.
        ELSEIF SCREEN-GROUP1(1) = 'B'.
          SCREEN-INPUT = 1.
          SCREEN-OUTPUT = 1.
          SCREEN-ACTIVE = 1.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_AND_PREPARE_DATA_NEW
*&---------------------------------------------------------------------*
FORM F_SELECT_AND_PREPARE_DATA_NEW .
  DATA: LT_FI_DOC     TYPE TT_FI_DOC,
        LT_FI_DOC2    TYPE TT_FI_DOC,
        LT_FI_DOC_KEY TYPE TT_FI_DOC_KEY,
        LT_VBRP       TYPE TT_VBRP,
        LT_VBKD       TYPE TT_VBKD,
        LT_CLEAR_DOC  TYPE TT_CLEAR_DOC,
        LT_CHK_INV    TYPE TT_CHK_INV,
        LT_PROJ       TYPE TT_PROJ,
        LT_PROJ_FI    TYPE TT_PROJ_FI,
        LS_DATA1      TYPE TS_DATA1,
        LS_DATA1_SUM  TYPE TS_DATA1,
        LS_STYLEROW   TYPE LVC_S_STYL,
        LS_CELLCOLOR  TYPE LVC_S_SCOL,
        LF_TABIX      TYPE SY-TABIX,
        LF_DMBTR_CLR  TYPE BSID_VIEW-DMBTR,
        LF_VBELN      TYPE VBRK-VBELN.




  PERFORM F_SELECT_FROM_SD_DOC CHANGING LT_FI_DOC
                                        LT_VBRP
                                        LT_VBKD
                                        LT_CHK_INV.


  IF P_SELFI = ABAP_TRUE.

    PERFORM F_SELECT_FI_DOC_DIRECT CHANGING LT_FI_DOC2.

    APPEND LINES OF LT_FI_DOC2 TO LT_FI_DOC.
    SORT LT_FI_DOC BY BUKRS BELNR GJAHR BUZEI.
    DELETE ADJACENT DUPLICATES FROM LT_FI_DOC COMPARING BUKRS BELNR GJAHR BUZEI.

  ENDIF.
  MOVE-CORRESPONDING LT_FI_DOC TO LT_FI_DOC_KEY.

  PERFORM F_SELECT_CLEAR_DOC USING LT_FI_DOC_KEY
                             CHANGING LT_CLEAR_DOC.

  PERFORM F_SELECT_COMP_DATA USING P_BUKRS
                             CHANGING GS_COMP .

  PERFORM F_SELECT_CUSTOMER_DATA USING LT_FI_DOC
                                 CHANGING GT_CUST.
  PERFORM F_SELECT_PROJ USING LT_VBRP
                              LT_FI_DOC
                        CHANGING LT_PROJ
                                 LT_PROJ_FI.
  SORT LT_FI_DOC BY KUNNR BELNR GJAHR BUZEI.
  SORT LT_VBKD BY VBELN_BILL .
*--------------prepare data ---------------------
  LOOP AT LT_FI_DOC ASSIGNING FIELD-SYMBOL(<L_FI_DOC>).
    CLEAR: LS_DATA1,
           LF_DMBTR_CLR.

    MOVE-CORRESPONDING <L_FI_DOC> TO LS_DATA1.
    CLEAR: LS_DATA1-BILLPL_NO,
           LS_DATA1-BILLPL_DATE.

    READ TABLE GT_CUST INTO DATA(LS_CUST) WITH  KEY KUNNR = <L_FI_DOC>-KUNNR.
    IF SY-SUBRC = 0.
      LS_DATA1-NAME1 = LS_CUST-NAME1.
    ENDIF.

    "---  find SD Document
    IF LS_DATA1-VBELN IS NOT INITIAL.
      LF_VBELN = LS_DATA1-VBELN .
      READ TABLE LT_VBKD TRANSPORTING NO FIELDS WITH KEY VBELN_BILL = LS_DATA1-VBELN
                                                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        LF_TABIX = SY-TABIX.
        LOOP AT LT_VBKD ASSIGNING FIELD-SYMBOL(<L_VBKD>) FROM LF_TABIX.
          IF <L_VBKD>-VBELN_BILL <> <L_FI_DOC>-VBELN.
            EXIT.
          ENDIF.
          IF LS_DATA1-BSTKD IS INITIAL.
            LS_DATA1-BSTKD  = <L_VBKD>-BSTKD.
          ELSE.
            LS_DATA1-BSTKD = |{ LS_DATA1-BSTKD }, { <L_VBKD>-BSTKD }|.
          ENDIF.
        ENDLOOP.
      ELSE.
        PERFORM F_GET_PO_FROM_SV_ORDER USING LS_DATA1-VBELN
                                       CHANGING LS_DATA1-BSTKD.

      ENDIF.

      READ TABLE LT_VBRP ASSIGNING FIELD-SYMBOL(<L_VBRP>) WITH KEY VBELN_BILL = LS_DATA1-VBELN.
      IF SY-SUBRC = 0.
        READ TABLE LT_PROJ INTO DATA(LS_PROJ) WITH KEY PSPNR = <L_VBRP>-PSPHI.
        IF SY-SUBRC = 0.
          LS_DATA1-PROJK = LS_PROJ-PSPID.
          LS_DATA1-POST1 = LS_PROJ-POST1.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE LT_PROJ_FI INTO DATA(LS_PROJ_FI) WITH KEY PSPID = <L_FI_DOC>-SGTXT ##WARN_OK.
      IF SY-SUBRC = 0.
        LS_DATA1-PROJK = LS_PROJ_FI-PSPID.
        LS_DATA1-POST1 = LS_PROJ_FI-POST1.
      ENDIF.
    ENDIF.

    "-- find clearing document
    READ TABLE LT_CLEAR_DOC TRANSPORTING NO FIELDS WITH KEY
                            BUKRS = <L_FI_DOC>-BUKRS
                            REBZG = <L_FI_DOC>-BELNR
                            REBZJ = <L_FI_DOC>-GJAHR
                            REBZZ = <L_FI_DOC>-BUZEI.
    IF SY-SUBRC = 0.
      LF_TABIX = SY-TABIX.
      LOOP AT LT_CLEAR_DOC ASSIGNING FIELD-SYMBOL(<L_CLEAR_DOC>) FROM LF_TABIX.
        IF  <L_CLEAR_DOC>-BUKRS <> <L_FI_DOC>-BUKRS
        OR  <L_CLEAR_DOC>-REBZG <> <L_FI_DOC>-BELNR
        OR  <L_CLEAR_DOC>-REBZJ <> <L_FI_DOC>-GJAHR
        OR  <L_CLEAR_DOC>-REBZZ <> <L_FI_DOC>-BUZEI.
          EXIT.
        ENDIF.
        IF <L_CLEAR_DOC>-SHKZG = 'S'.
          <L_CLEAR_DOC>-DMBTR = <L_CLEAR_DOC>-DMBTR * -1.
        ENDIF.
        LF_DMBTR_CLR = LF_DMBTR_CLR + <L_CLEAR_DOC>-DMBTR.
      ENDLOOP.
    ENDIF.
    IF <L_FI_DOC>-SHKZG = 'H'.
      LS_DATA1-OPEN_AMT = <L_FI_DOC>-DMBTR * - 1.
    ELSE.
      LS_DATA1-OPEN_AMT = <L_FI_DOC>-DMBTR.
    ENDIF.

    "amount = inv - clearing
    LS_DATA1-OPEN_AMT     = LS_DATA1-OPEN_AMT - LF_DMBTR_CLR.
    LS_DATA1-BILLPL_AMT   = LS_DATA1-OPEN_AMT.
    LS_DATA1_SUM-OPEN_AMT = LS_DATA1_SUM-OPEN_AMT + LS_DATA1-BILLPL_AMT.
    "---- net due date
    PERFORM F_GET_NET_DUE_DATE  USING <L_FI_DOC>-BLDAT
                                      <L_FI_DOC>-BUDAT
                                      <L_FI_DOC>-ZTERM
                                CHANGING LS_DATA1-NETDT.
    LS_DATA1-WAERS = GS_COMP-WAERS.
    LS_DATA1-RECEIPT = LS_DATA1-BELNR.

    APPEND LS_DATA1 TO GT_DATA1.
    AT END OF KUNNR.

      "-------sum line --------------------
      LS_DATA1_SUM-SUM = GC_TRUE.
      LS_DATA1_SUM-KUNNR = <L_FI_DOC>-KUNNR.
      LS_DATA1_SUM-NAME1 = LS_DATA1-NAME1.
      LS_DATA1_SUM-WAERS = GS_COMP-WAERS.
      IF LF_VBELN IS NOT INITIAL.
        READ TABLE LT_CHK_INV INTO DATA(LS_CHK_INV) WITH KEY VBELN_BILL = LF_VBELN.
        IF SY-SUBRC = 0
        AND LS_CHK_INV-ZSTAT_ACT IS NOT INITIAL.
          READ TABLE GT_STAT_ACT INTO DATA(LS_STAT_ACT) WITH KEY STAT_ACT = LS_CHK_INV-ZSTAT_ACT.
          IF SY-SUBRC = 0.
            LS_DATA1_SUM-STATUS = LS_STAT_ACT-BL_STAT.
            LS_DATA1_SUM-ACTION_TYPE = LS_STAT_ACT-BL_ACTTY.
            READ TABLE GT_STATUS INTO DATA(LS_STATUS) WITH  KEY VALUE = LS_DATA1_SUM-STATUS.
            IF SY-SUBRC = 0.
              LS_DATA1_SUM-STATUS_TX = LS_STATUS-TEXT.
            ENDIF.
            READ TABLE GT_ACTION_TYPE INTO DATA(LS_ACTION_TYPE) WITH  KEY VALUE = LS_DATA1_SUM-ACTION_TYPE.
            IF SY-SUBRC = 0.
              LS_DATA1_SUM-ACTION_TYPE_TX = LS_ACTION_TYPE-TEXT.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
      IF LS_DATA1_SUM-ACTION_TYPE IS INITIAL.
        LS_DATA1_SUM-ACTION_TYPE = GC_ACTION_TYPE_DEFAULT.
        READ TABLE GT_ACTION_TYPE INTO LS_ACTION_TYPE WITH  KEY VALUE = LS_DATA1_SUM-ACTION_TYPE.
        IF SY-SUBRC = 0.
          LS_DATA1_SUM-ACTION_TYPE_TX = LS_ACTION_TYPE-TEXT.
        ENDIF.
      ENDIF.
      IF LS_DATA1_SUM-STATUS IS INITIAL.
        LS_DATA1_SUM-STATUS = GC_STATUS_DEFAULT.
        READ TABLE GT_STATUS INTO LS_STATUS WITH  KEY VALUE = LS_DATA1_SUM-STATUS.
        IF SY-SUBRC = 0.
          LS_DATA1_SUM-STATUS_TX = LS_STATUS-TEXT.
        ENDIF.
      ENDIF.
      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      LS_STYLEROW-FIELDNAME = 'BILLPL_AMT' .
      INSERT LS_STYLEROW  INTO TABLE LS_DATA1_SUM-FIELD_STYLE.

      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      LS_STYLEROW-FIELDNAME = 'BILLPL_PMTDT' .
      INSERT LS_STYLEROW  INTO TABLE LS_DATA1_SUM-FIELD_STYLE.

      LS_STYLEROW-FIELDNAME = 'ACTION_TYPE' .
      INSERT LS_STYLEROW  INTO TABLE LS_DATA1_SUM-FIELD_STYLE.

      LS_STYLEROW-FIELDNAME = 'STATUS' .
      INSERT LS_STYLEROW  INTO TABLE LS_DATA1_SUM-FIELD_STYLE.

      LS_CELLCOLOR-COLOR-COL = '0'.
      LS_CELLCOLOR-COLOR-INT = '0'.
      LS_CELLCOLOR-COLOR-INV = '0'.
      LS_CELLCOLOR-FNAME = 'BILLPL_PMTDT'.
      INSERT LS_CELLCOLOR INTO TABLE LS_DATA1_SUM-COL_COLOR.

      LS_CELLCOLOR-FNAME = 'ACTION_TYPE'.
      INSERT LS_CELLCOLOR INTO TABLE LS_DATA1_SUM-COL_COLOR.

      LS_CELLCOLOR-FNAME = 'STATUS'.
      INSERT LS_CELLCOLOR INTO TABLE LS_DATA1_SUM-COL_COLOR.

      LS_DATA1_SUM-ROW_COLOR = 'C310'. "YELLOW
      APPEND LS_DATA1_SUM TO GT_DATA1.
      CLEAR LS_DATA1_SUM.
      CLEAR LF_VBELN.
    ENDAT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_from_sd_doc
*&---------------------------------------------------------------------*
FORM F_SELECT_FROM_SD_DOC CHANGING CT_FI_DOC TYPE TT_FI_DOC
                                   CT_VBRP   TYPE TT_VBRP
                                   CT_VBKD   TYPE TT_VBKD
                                   CT_CHK_INV TYPE TT_CHK_INV.
  DATA: LT_SD_DOC TYPE SORTED TABLE OF TS_CHK_INV WITH NON-UNIQUE KEY VBELN_BILL VBELN_SO.

  "invoice check (ZSDSFIT027-VBELN = billing no.)
  SELECT C~VBELN AS VBELN_BILL,
         C~AUBEL AS VBELN_SO,
         D~PSPHI,
         A~VBELN AS VBELN_INV_CHK,
         A~ZCHECK,
         A~ZSTAT_ACT
  INTO TABLE @CT_CHK_INV
  FROM VBRK AS B  LEFT OUTER JOIN ZSDSFIT027 AS  A
  ON A~VBELN = B~VBELN
                        INNER JOIN VBRP AS C
  ON B~VBELN = C~VBELN
                        LEFT OUTER JOIN PRPS AS D
  ON C~PS_PSP_PNR = D~PSPNR
*  WHERE A~ZCHECK = @GC_ZCHECK_RECEIVED
  WHERE B~KUNRG  IN @S_KUNNR
  AND   B~BUKRS  =  @P_BUKRS
  AND   B~VBELN  IN @S_VBELN
  AND   B~FKDAT  IN @S_FKDAT
  AND   B~VKORG  IN @S_VKORG
  AND   B~VTWEG  IN @S_VTWEG
  AND   B~SPART  IN @S_SPART
  AND   C~VKGRP  IN @S_VKGRP
  AND   C~VKBUR  IN @S_VKBUR
  AND   B~FKSTO  = ''.


  "invoice check by project (ZSDSFIT028-VBELN = Delivery no)
  SELECT B~VBELN AS VBELN_BILL,
         B~AUBEL AS VBELN_SO,
         D~PSPHI,
         A~VBELN AS VBELN_INV_CHK,
         A~ZCHECK,
         A~ZSTAT_ACT
  APPENDING TABLE @CT_CHK_INV
  FROM VBRP AS B LEFT OUTER JOIN ZSDSFIT028 AS  A
  ON   A~VBELN = B~VGBEL
                        INNER JOIN VBRK AS C
  ON   B~VBELN = C~VBELN
                        LEFT OUTER JOIN PRPS AS D
  ON   B~PS_PSP_PNR = D~PSPNR
  WHERE A~ZCHECK = @GC_ZCHECK_RECEIVED
  AND   C~KUNRG  IN @S_KUNNR
  AND   C~BUKRS  =  @P_BUKRS
  AND   C~VBELN  IN @S_VBELN
  AND   C~FKDAT  IN @S_FKDAT
  AND   C~VKORG  IN @S_VKORG
  AND   C~VTWEG  IN @S_VTWEG
  AND   C~SPART  IN @S_SPART
  AND   B~VKGRP  IN @S_VKGRP
  AND   B~VKBUR  IN @S_VKBUR
  AND   B~PS_PSP_PNR <> ''
  AND   C~FKSTO  = ''.

*  DELETE CT_CHK_INV WHERE VBELN_INV_CHK IS NOT INITIAL  "#EC CI_SORTSEQ
*                    AND   ZCHECK <> GC_ZCHECK_RECEIVED  .


  MOVE-CORRESPONDING CT_CHK_INV TO LT_SD_DOC.
  DATA(LT_BILL_KEY) = LT_SD_DOC.
  DELETE ADJACENT DUPLICATES FROM LT_BILL_KEY COMPARING VBELN_BILL.

  MOVE-CORRESPONDING LT_SD_DOC TO CT_VBRP.
  DELETE ADJACENT DUPLICATES FROM CT_VBRP COMPARING VBELN_BILL PSPHI.
  IF LT_BILL_KEY[] IS INITIAL.
    RETURN.
  ENDIF.

  " FI doc link from SD Billing
  SELECT  B~KUNNR,
          A~BUKRS,
          A~BELNR,
          A~GJAHR,
          B~BUZEI,
          A~BLART,
          A~XBLNR,
          A~BKTXT,
          A~AWTYP,
          A~BLDAT,
          B~VBELN,
          B~BUDAT,
          B~ZTERM,
          B~SHKZG,
          B~DMBTR,
          B~UMSKZ,
          B~SGTXT, "Project
          D~BILLPL_NO,
          D~DELFG
  FROM BKPF AS A INNER JOIN BSID_VIEW AS B
  ON  A~BUKRS = B~BUKRS
  AND A~GJAHR = B~GJAHR
  AND A~BELNR = B~BELNR
                 LEFT OUTER JOIN ZSDSFIT035 AS D
  ON  B~BUKRS = D~BUKRS
  AND B~GJAHR = D~GJAHR
  AND B~BELNR = D~BELNR
  AND B~BUZEI = D~BUZEI
  FOR ALL ENTRIES IN @LT_BILL_KEY
  WHERE A~BUKRS = @P_BUKRS
  AND   B~VBELN = @LT_BILL_KEY-VBELN_BILL
  AND   A~XREVERSAL = ''
  AND   A~AWTYP = @GC_AWTYP_VBRK
  INTO TABLE @CT_FI_DOC  ##TOO_MANY_ITAB_FIELDS.
  SORT CT_FI_DOC BY BUKRS BELNR GJAHR BUZEI BILLPL_NO DESCENDING .
  DELETE ADJACENT DUPLICATES FROM CT_FI_DOC COMPARING BUKRS BELNR GJAHR BUZEI.

  DELETE CT_FI_DOC WHERE BILLPL_NO <> ''
                   AND DELFG = ''.

  DELETE ADJACENT DUPLICATES FROM LT_SD_DOC COMPARING VBELN_BILL VBELN_SO.


  IF LT_SD_DOC IS NOT INITIAL.
    SELECT B~VBELN_BILL ,
           B~VBELN_SO,
           A~BSTKD
    FROM VBKD AS A INNER JOIN @LT_SD_DOC AS B
    ON   A~VBELN = B~VBELN_SO
    WHERE BSTKD  <> ''
    AND   POSNR = '000000'
    INTO TABLE @CT_VBKD.

    SORT CT_VBKD BY VBELN_BILL VBELN_SO.
    DELETE ADJACENT DUPLICATES FROM CT_VBKD COMPARING VBELN_BILL VBELN_SO.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_fi_doc_direct
*&---------------------------------------------------------------------*
FORM F_SELECT_FI_DOC_DIRECT  CHANGING CT_FI_DOC TYPE TT_FI_DOC.

  IF P_SPCGL = ABAP_TRUE.
    DATA(LRT_UMSKZ) = GRT_UMSKZ.
  ELSE.
    APPEND 'IEQ ' TO LRT_UMSKZ.
  ENDIF.
  SELECT  B~KUNNR,
          A~BUKRS,
          A~BELNR,
          A~GJAHR,
          B~BUZEI,
          A~BLART,
          A~XBLNR,
          A~BKTXT,
          A~AWTYP,
          A~BLDAT,
          B~VBELN,
          B~BUDAT,
          B~ZTERM,
          B~SHKZG,
          B~DMBTR,
          B~UMSKZ,
          B~SGTXT, "Project
          D~BILLPL_NO,
          D~DELFG
  FROM BKPF AS A INNER JOIN BSID_VIEW AS B
  ON  A~BUKRS = B~BUKRS
  AND A~GJAHR = B~GJAHR
  AND A~BELNR = B~BELNR
                LEFT OUTER JOIN ZSDSFIT035 AS D
  ON  B~BUKRS = D~BUKRS
  AND B~GJAHR = D~GJAHR
  AND B~BELNR = D~BELNR
  AND B~BUZEI = D~BUZEI
  WHERE A~BUKRS = @P_BUKRS
  AND   A~BLART IN @GRT_INV_BLART
  AND   A~BELNR IN @S_BELNR
  AND   A~XBLNR IN @S_XBLNR
  AND   A~BUDAT <= @P_BUDAT
  AND   B~UMSKZ IN @LRT_UMSKZ
  AND   B~KUNNR IN @S_KUNNR
  AND   B~REBZG = '' "no reference invoice
  AND   A~XREVERSAL = ''
  INTO TABLE @CT_FI_DOC.
  SORT CT_FI_DOC BY BUKRS BELNR GJAHR BUZEI BILLPL_NO DESCENDING .
  DELETE ADJACENT DUPLICATES FROM CT_FI_DOC COMPARING BUKRS BELNR GJAHR BUZEI.

  DELETE CT_FI_DOC WHERE BILLPL_NO <> ''
                   AND DELFG = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_GENC
*&---------------------------------------------------------------------*
FORM F_GET_GENC .
  DATA: LRS_BLART LIKE LINE OF GRT_INV_BLART,
        LRS_UMSKZ LIKE LINE OF GRT_UMSKZ.

  DATA: LT_GEN_C TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = SY-REPID
    IMPORTING
      ET_GEN_C = LT_GEN_C.

  LOOP AT LT_GEN_C ASSIGNING FIELD-SYMBOL(<L_GEN_C>).
    CASE <L_GEN_C>-PARAM.
      WHEN 'INV_BLART'.
        APPEND_RANGE LRS_BLART
                     <L_GEN_C>-PARAM_SIGN
                     <L_GEN_C>-PARAM_OPTION
                     <L_GEN_C>-VALUE_LOW
                     <L_GEN_C>-VALUE_HIGH
                     GRT_INV_BLART.
      WHEN 'UMSKZ'.
        APPEND_RANGE LRS_UMSKZ
                     <L_GEN_C>-PARAM_SIGN
                     <L_GEN_C>-PARAM_OPTION
                     <L_GEN_C>-VALUE_LOW
                     <L_GEN_C>-VALUE_HIGH
                     GRT_UMSKZ.
    ENDCASE.
  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_clear_doc
*&---------------------------------------------------------------------*
FORM F_SELECT_CLEAR_DOC  USING    CT_FI_DOC_KEY TYPE TT_FI_DOC_KEY
                         CHANGING CT_CLEAR_DOC TYPE TT_CLEAR_DOC.
  IF CT_FI_DOC_KEY IS INITIAL.
    RETURN.
  ENDIF.
  SELECT A~BUKRS,
         A~REBZG,
         A~REBZJ,
         A~REBZZ,
         A~GJAHR,
         A~BELNR,
         A~BUZEI,
         A~SHKZG,
         A~DMBTR
  FROM BSID_VIEW AS A INNER JOIN BKPF AS  B
  ON A~BUKRS = B~BUKRS
  AND A~BELNR = B~BELNR
  AND A~GJAHR = B~GJAHR
  FOR ALL ENTRIES IN @CT_FI_DOC_KEY
  WHERE A~BUKRS = @P_BUKRS
  AND   REBZG = @CT_FI_DOC_KEY-BELNR
  AND   REBZJ = @CT_FI_DOC_KEY-GJAHR
  AND   REBZZ = @CT_FI_DOC_KEY-BUZEI
  AND   B~XREVERSAL = ''
  INTO TABLE @CT_CLEAR_DOC.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_net_due_date
*&---------------------------------------------------------------------*
FORM F_GET_NET_DUE_DATE  USING  UF_BLDAT TYPE BKPF-BLDAT
                                UF_BUDAT TYPE BKPF-BUDAT
                                UF_ZTERM TYPE VBRK-ZTERM
                       CHANGING CF_NETDT TYPE FAEDE-NETDT.
  DATA: LS_FAEDE TYPE FAEDE.

  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
    EXPORTING
      I_BLDAT         = UF_BLDAT
      I_BUDAT         = UF_BUDAT
      I_ZTERM         = UF_ZTERM
      I_BUKRS         = P_BUKRS
    IMPORTING
      E_ZBD1T         = LS_FAEDE-ZBD1T
      E_ZBD2T         = LS_FAEDE-ZBD2T
      E_ZBD3T         = LS_FAEDE-ZBD3T
      E_ZFBDT         = LS_FAEDE-ZFBDT
    EXCEPTIONS
      TERMS_NOT_FOUND = 1
      OTHERS          = 2.
  IF SY-SUBRC = 0.
    LS_FAEDE-SHKZG = 'S'.
    LS_FAEDE-KOART = 'D'.
    LS_FAEDE-BLDAT = UF_BLDAT.
    CALL FUNCTION 'DETERMINE_DUE_DATE'
      EXPORTING
        I_FAEDE                    = LS_FAEDE
      IMPORTING
        E_FAEDE                    = LS_FAEDE
      EXCEPTIONS
        ACCOUNT_TYPE_NOT_SUPPORTED = 1
        OTHERS                     = 2.
    IF SY-SUBRC <> 0.
      CF_NETDT = LS_FAEDE-ZFBDT.
    ELSE.
      CF_NETDT = LS_FAEDE-NETDT.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEL_SCRN_VALIDATE
*&---------------------------------------------------------------------*
FORM F_SEL_SCRN_VALIDATE .
  IF P_SELFI = ABAP_TRUE.
    IF P_BUDAT IS INITIAL.
      MESSAGE E000(ZSDSMM01) WITH 'FI Doc At Key Date is required'(m01).
    ENDIF.
  ENDIF.
*BOI CH02
  IF P_PERNR IS NOT INITIAL.
    SELECT PERNR
    INTO @DATA(LF_PERNR)
    UP TO 1 ROWS
    FROM PA0002
    WHERE PERNR = @P_PERNR ##NEEDED.
    ENDSELECT.
    IF SY-SUBRC <> 0.
      MESSAGE E000(ZSDSCA01) WITH 'Personal ID is invalid'(m13).
    ENDIF.
  ENDIF.
*EOI CH02
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_EXIT
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_EXIT .
  DATA:
    LF_VALID   TYPE CHAR01,
    LF_REFRESH TYPE CHAR01,
    LF_ANS     TYPE CHAR01,
    LF_CHANGED TYPE FLAG.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).
*     Continue processing only when valid
  IF LF_VALID IS INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.
  PERFORM F_CHECK_CHANGED_DATA CHANGING LF_CHANGED.
  IF LF_CHANGED = ''.
    LEAVE TO SCREEN 0.
  ENDIF.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Please confirm'(m02)
      TEXT_QUESTION         = 'Data was changed. Do you want to exit without saving?'(m03)
      TEXT_BUTTON_1         = 'Yes'(M10)
      ICON_BUTTON_1         = 'ICON_CHECKED'
      TEXT_BUTTON_2         = 'No'(M11)
      ICON_BUTTON_2         = 'ICON_CANCEL'
      DISPLAY_CANCEL_BUTTON = ' '
      POPUP_TYPE            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      ANSWER                = LF_ANS
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    LEAVE TO SCREEN 0.
  ELSE.
    IF LF_ANS = 1.
      LEAVE TO SCREEN 0 .
    ELSE.
      CLEAR GF_SAVE_OK.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
FORM F_CHECK_CHANGED_DATA CHANGING CF_CHANGED TYPE FLAG.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-NEW.
      IF GT_DATA1 <> GT_DATA1_OLD
      AND GF_EDIT = ABAP_TRUE.
        CF_CHANGED = GC_TRUE.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_DATA TYPE ANY TABLE.
* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
  IF GF_EDIT EQ SPACE.
*   Enable Soft Refresh only in display mode
    GF_SOFT_REFRESH_1 = GC_TRUE.
  ELSE.
*   No auto refresh in edit mode
    GF_NO_AUTO_REFRESH_1 = GC_TRUE.
  ENDIF.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_DATA TO <G_LIST_1>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat, sort
  PERFORM F_ALV_BUILD_FIELDCAT    USING UT_DATA
                                  CHANGING GT_FIELDCAT_1.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-NEW.
      PERFORM F_ALV_MODIFY_FC_NEW   CHANGING GT_FIELDCAT_1.
    WHEN GC_EXEC_TY-DETAIL
    OR GC_EXEC_TY-REPRINT
    OR GC_EXEC_TY-CANCEL.
      PERFORM F_ALV_MODIFY_FC_EXST   CHANGING GT_FIELDCAT_1.
      PERFORM F_ALV_SORT_RESULT_EXST CHANGING GT_SORT_1.
  ENDCASE.
* Call ALV Screen
  CALL SCREEN 9000.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT USING UT_DATA TYPE ANY TABLE
                       CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  DATA: LF_COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
        LF_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
        LF_SALV_TABLE   TYPE REF TO CL_SALV_TABLE,
        LF_TABLE        TYPE REF TO DATA.

  FIELD-SYMBOLS: <TABLE> TYPE ANY TABLE.

* create unprotected table from import data
  CREATE DATA LF_TABLE LIKE  UT_DATA.
  ASSIGN LF_TABLE->* TO <TABLE>.

*...New ALV Instance ...............................................
  TRY.
      CL_SALV_TABLE=>FACTORY(
        EXPORTING
          LIST_DISPLAY = ABAP_FALSE
        IMPORTING
          R_SALV_TABLE = LF_SALV_TABLE
        CHANGING
          T_TABLE      = <TABLE> ).
    CATCH CX_SALV_MSG ##NO_HANDLER.
  ENDTRY.
  LF_COLUMNS  = LF_SALV_TABLE->GET_COLUMNS( ).
  LF_AGGREGATIONS = LF_SALV_TABLE->GET_AGGREGATIONS( ).

  CALL METHOD CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG
    EXPORTING
      R_COLUMNS      = LF_COLUMNS
      R_AGGREGATIONS = LF_AGGREGATIONS
    RECEIVING
      T_FIELDCATALOG = CT_FIELDCAT[].

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '18%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '82%'.

  DATA:
    LF_TEXT    TYPE  SDYDO_TEXT_ELEMENT,
    LF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LF_COL_VAL TYPE  REF TO CL_DD_AREA ##NEEDED.

* Create table
  CALL METHOD UF_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LF_TABLE.
* Get Column Element
  CALL METHOD LF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LF_COL_KEY.
* Get Column Element
  CALL METHOD LF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LF_COL_VAL.
* Set Key column style
  CALL METHOD LF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD LF_TABLE->NEW_ROW.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-NEW.
      LF_TEXT = 'Create Bill Placement'(h01).
    WHEN GC_EXEC_TY-DETAIL.
      LF_TEXT = 'Detail of Bill Placement'(h02).
    WHEN GC_EXEC_TY-REPRINT.
      LF_TEXT = 'Reprint of Bill Placement'(h03).
    WHEN GC_EXEC_TY-CANCEL.
      LF_TEXT = 'Cancel of Bill Placement'(h04).
  ENDCASE.
  CALL METHOD LF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_NEW
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC_NEW  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'SUM'
      OR 'WAERS'
      OR 'BUZEI'.
        <FCAT>-TECH = GC_TRUE.
      WHEN 'SEL'.
        %FCAT 'Select'(c01).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-EDIT     = GC_TRUE.
        <FCAT>-COL_POS  = 1.
      WHEN 'BILLPL_NO'.
        <FCAT>-COL_POS  = 2.
      WHEN 'BILLPL_DATE'.
        <FCAT>-COL_POS  = 3.
      WHEN OTHERS.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 4.
        CASE <FCAT>-FIELDNAME.
          WHEN 'VBELN'.
            <FCAT>-HOTSPOT = GC_TRUE.
          WHEN 'BSTKD'.
            %FCAT 'P/O No.'(c03).
          WHEN 'RECEIPT'.
            %FCAT 'Receipt No.'(c04).
          WHEN 'BUDAT'.
            %FCAT 'Invoice Date'(c05).
          WHEN 'OPEN_AMT'.
            %FCAT 'Open Inv Amount'(c07).
            <FCAT>-CFIELDNAME = 'WAERS'.
          WHEN 'BILLPL_AMT'.
            <FCAT>-EDIT       = GC_TRUE.
            <FCAT>-CFIELDNAME = 'WAERS'.
          WHEN 'BILLPL_PMTDT'.
            %FCAT 'Payment Date'(c10) ##TEXT_DUP.
            <FCAT>-REF_TABLE  = 'ZSDSFIT033'.
            <FCAT>-REF_FIELD  = 'BILLPL_PMTDT'.
          WHEN 'ACTION_TYPE'.
            <FCAT>-REF_TABLE  = 'ZSDSFIT033'.
            <FCAT>-REF_FIELD  = 'ACTION_TYPE'.
          WHEN 'ACTION_TYPE_TX'.
            %FCAT 'Action Type descr'(c08).
          WHEN 'STATUS'.
            <FCAT>-REF_TABLE  = 'ZSDSFIT033'.
            <FCAT>-REF_FIELD  = 'STATUS'.
          WHEN 'STATUS_TX'.
            %FCAT 'Status descr'(c09).
        ENDCASE.
    ENDCASE.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'B'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CASE GF_EXEC_TY .
    WHEN GC_EXEC_TY-NEW.
      CS_LAYOUT-STYLEFNAME = 'FIELD_STYLE'.
      CS_LAYOUT-INFO_FNAME = 'ROW_COLOR'.
      CS_LAYOUT-CTAB_FNAME = 'COL_COLOR'.
  ENDCASE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
  CS_VARIANT-LOG_GROUP = GF_EXEC_TY(1).
  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_value
*&---------------------------------------------------------------------*
FORM F_INIT_VALUE .
  CONSTANTS: LC_ACTIVATE TYPE DD07T-AS4LOCAL VALUE 'A'.
  DATA: LS_STATUS      TYPE TS_STATUS,
        LS_ACTION_TYPE TYPE TS_ACTION_TYPE.

  CASE GC_TRUE.
    WHEN RB_NEW.
      GF_EXEC_TY = GC_EXEC_TY-NEW.
      GF_EDIT = GC_TRUE.
    WHEN RB_EXIST.
      CASE GC_TRUE.
        WHEN RB_E_DET.
          GF_EXEC_TY = GC_EXEC_TY-DETAIL.
          GF_EDIT = GC_FALSE.
        WHEN RB_E_RP.
          GF_EXEC_TY = GC_EXEC_TY-REPRINT.
          GF_EDIT = GC_FALSE.
        WHEN RB_E_CNC.
          GF_EXEC_TY = GC_EXEC_TY-CANCEL.
          GF_EDIT = GC_TRUE.
      ENDCASE.
  ENDCASE.

  SELECT VALPOS,
         DOMVALUE_L,
         DDTEXT
  INTO TABLE @DATA(LT_DD07T)
  FROM DD07T
  WHERE DOMNAME = @GC_DOMNAME_ACTION_TYPE
  AND   DDLANGUAGE = @SY-LANGU
  AND   AS4LOCAL = @LC_ACTIVATE.

  SORT  LT_DD07T BY VALPOS.

  LOOP AT LT_DD07T ASSIGNING FIELD-SYMBOL(<L_DD07T>).
    CLEAR LS_ACTION_TYPE.
    LS_ACTION_TYPE-VALUE = <L_DD07T>-DOMVALUE_L.
    LS_ACTION_TYPE-TEXT  = <L_DD07T>-DDTEXT.
    INSERT LS_ACTION_TYPE INTO TABLE GT_ACTION_TYPE.
  ENDLOOP.

  SELECT VALPOS,
         DOMVALUE_L,
         DDTEXT
  INTO TABLE @LT_DD07T
  FROM DD07T
  WHERE DOMNAME = @GC_DOMNAME_STATUS
  AND   DDLANGUAGE = @SY-LANGU
  AND   AS4LOCAL = @LC_ACTIVATE.

  SORT  LT_DD07T BY VALPOS.

  LOOP AT LT_DD07T ASSIGNING <L_DD07T>.
    CLEAR LS_STATUS.
    LS_STATUS-VALUE = <L_DD07T>-DOMVALUE_L.
    LS_STATUS-TEXT  = <L_DD07T>-DDTEXT.
    INSERT LS_STATUS INTO TABLE GT_STATUS.
  ENDLOOP.

  SELECT *
  INTO TABLE GT_STAT_ACT
  FROM ZSDSFIC028 .                                     "#EC CI_NOWHERE
ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_ALV_DISPLAY
*----------------------------------------------------------------------*
*       Display ALV Grid
*----------------------------------------------------------------------*
FORM F_ALV_DISPLAY.

* --------------------------------------------------
* Create First ALV Container
* --------------------------------------------------
  IF GREF_GRID_1 IS INITIAL.

*   Get First ALV Container
    PERFORM F_GET_CONTAINER_1  USING  'FIRST'
                                      GF_ALV_HEIGHT_1
                             CHANGING GREF_CONTAINER_GRID_1.

*   Create First ALV Object
    CREATE OBJECT GREF_GRID_1
      EXPORTING
        I_PARENT = GREF_CONTAINER_GRID_1.

*   Set Event Handler for First ALV
    CREATE OBJECT GREF_EVENT_RECEIVER_1.
    SET HANDLER:
      GREF_EVENT_RECEIVER_1->ON_PRINT_TOP_OF_PAGE_1  FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_HOTSPOT_CLICK_1      FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DOUBLE_CLICK_1       FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->HANDLE_TOOLBAR_1        FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->HANDLE_USER_COMMAND_1   FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DATA_CHANGED_1       FOR GREF_GRID_1.


*   Show First ALV
    CALL METHOD GREF_GRID_1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_BUFFER_ACTIVE               = 'X'
        IS_VARIANT                    = GS_VARIANT_1
        IS_LAYOUT                     = GS_LAYOUT_1
        IS_PRINT                      = GS_PRINT_1
        I_SAVE                        = GC_SAVE_ALL
        IT_TOOLBAR_EXCLUDING          = GT_TOOL_EXC_1
      CHANGING
        IT_OUTTAB                     = <G_LIST_1>
        IT_FIELDCATALOG               = GT_FIELDCAT_1
        IT_SORT                       = GT_SORT_1
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
    ENDIF.
  ELSE.

    IF GF_NO_AUTO_REFRESH_1 = SPACE.
      IF GF_SOFT_REFRESH_1 IS INITIAL.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_FIELDCATALOG
          EXPORTING
            IT_FIELDCATALOG = GT_FIELDCAT_1.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_LAYOUT
          EXPORTING
            IS_LAYOUT = GS_LAYOUT_1.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_PRINT
          EXPORTING
            IS_PRINT = GS_PRINT_1.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = ' '.
      ELSE.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

* --------------------------------------------------
* Create HTML Header Container if flag is marked
* --------------------------------------------------
  IF GF_ALV_HEADER_1 EQ 'X' AND
     CL_GUI_ALV_GRID=>OFFLINE( ) IS INITIAL.

    IF GREF_CONTAINER_HTML_1 IS INITIAL.

*     Get HTML Header Container
      PERFORM F_GET_CONTAINER_1  USING  'HEADER'
                                        GF_HEADER_HIGHT_1
                               CHANGING GREF_CONTAINER_HTML_1.

*     Bind TOP-OF-PAGE Event
      SET HANDLER:
        GREF_EVENT_RECEIVER_1->ON_TOP_OF_PAGE_1        FOR GREF_GRID_1.
*     Create TOP-Document
      CREATE OBJECT GREF_DYNDOC_ID_1
        EXPORTING
          STYLE = 'ALV_GRID'.
    ENDIF.

*   Initializing document
    CALL METHOD GREF_DYNDOC_ID_1->INITIALIZE_DOCUMENT.
*   Processing events
    CALL METHOD GREF_GRID_1->LIST_PROCESSING_EVENTS
      EXPORTING
        I_EVENT_NAME = 'TOP_OF_PAGE'
        I_DYNDOC_ID  = GREF_DYNDOC_ID_1.
*   Merge all setting
    CALL METHOD GREF_DYNDOC_ID_1->MERGE_DOCUMENT.
*   Display TOP document
    CALL METHOD GREF_DYNDOC_ID_1->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = GREF_CONTAINER_HTML_1
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.
    IF SY-SUBRC NE 0 ##NEEDED.
      "Do nothing
    ENDIF.

  ENDIF.

ENDFORM.                    " F_ALV_DISPLAY
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_1
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_1 USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                               UF_ONF4 TYPE  CHAR01          ##NEEDED
                               UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                               UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                               UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-NEW.
      PERFORM F_ON_DATA_CHANGED_NEW
              USING UF_DATA_CHANGED
                    UF_ONF4
                    UF_ONF4_BEFORE
                    UF_ONF4_AFTER
                    UF_UCOMM .
    WHEN GC_EXEC_TY-REPRINT
    OR GC_EXEC_TY-CANCEL.
      PERFORM F_ON_DATA_CHANGED_EXIST
              USING UF_DATA_CHANGED
                    UF_ONF4
                    UF_ONF4_BEFORE
                    UF_ONF4_AFTER
                    UF_UCOMM .

  ENDCASE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_NEW
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM   F_ON_DATA_CHANGED_NEW  USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                                    UF_ONF4 TYPE  CHAR01          ##NEEDED
                                    UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                                    UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                                    UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  DATA: LS_STABLE  TYPE LVC_S_STBL,
        LF_REFRESH TYPE FLAG.

* For all valid changing cells
  LOOP AT UF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>)
                         INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.

      WHEN 'STATUS'.
        IF  <L_GOOD_CELL>-VALUE IS NOT INITIAL.
          READ TABLE GT_STATUS INTO DATA(LS_STATUS)
                                WITH KEY VALUE = <L_GOOD_CELL>-VALUE ##WARN_OK.
          IF SY-SUBRC = 0.
            <L_DATA1>-STATUS_TX = LS_STATUS-TEXT.
            LF_REFRESH = GC_TRUE.
          ENDIF.
        ENDIF.
      WHEN 'ACTION_TYPE'.
        IF  <L_GOOD_CELL>-VALUE IS NOT INITIAL.
          READ TABLE GT_ACTION_TYPE INTO DATA(LS_ACTION_TYPE)
                                WITH KEY VALUE = <L_GOOD_CELL>-VALUE ##WARN_OK.
          IF SY-SUBRC = 0.
            <L_DATA1>-ACTION_TYPE_TX = LS_ACTION_TYPE-TEXT.
            LF_REFRESH = GC_TRUE.
          ENDIF.
        ENDIF.
      WHEN 'BILLPL_AMT'.
        <L_DATA1>-BILLPL_AMT = <L_GOOD_CELL>-VALUE.
        PERFORM F_UPDATE_SUM USING <L_DATA1>
                             CHANGING GT_DATA1.

        LF_REFRESH = GC_TRUE.
      WHEN 'SEL'.
        IF <L_DATA1>-SUM = GC_TRUE.
          LOOP AT GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA_UP>) WHERE KUNNR = <L_DATA1>-KUNNR.
            <L_DATA_UP>-SEL = <L_GOOD_CELL>-VALUE.
          ENDLOOP.
        ELSE.
          <L_DATA1>-SEL = <L_GOOD_CELL>-VALUE.
        ENDIF.

        PERFORM F_UPDATE_SUM USING <L_DATA1>
                             CHANGING GT_DATA1.

        LF_REFRESH = GC_TRUE.
    ENDCASE.

  ENDLOOP.
  IF LF_REFRESH = GC_TRUE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.

    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = 'X'.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_NEW
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM   F_ON_DATA_CHANGED_EXIST  USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                                    UF_ONF4 TYPE  CHAR01          ##NEEDED
                                    UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                                    UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                                    UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  DATA: LS_STABLE  TYPE LVC_S_STBL,
        LF_REFRESH TYPE FLAG.

* For all valid changing cells
  LOOP AT UF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>)
                         INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.
      WHEN 'SEL'.
        LOOP AT GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA_UP>) WHERE BILLPL_NO = <L_DATA1>-BILLPL_NO.
          <L_DATA_UP>-SEL = <L_GOOD_CELL>-VALUE.
        ENDLOOP.
        LF_REFRESH = GC_TRUE.
    ENDCASE.

  ENDLOOP.
  IF LF_REFRESH = GC_TRUE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.

    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = 'X'.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.
  DATA: LS_STABLE  TYPE LVC_S_STBL.

  CASE UF_UCOMM.
    WHEN 'ZALL'.
      PERFORM F_SELECT_CHECK_BOX USING 'X'.
    WHEN 'ZSAL'.
      PERFORM F_SELECT_CHECK_BOX USING ''.
  ENDCASE.

  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_check_box
*&---------------------------------------------------------------------*
FORM F_SELECT_CHECK_BOX  USING UF_X TYPE C.
  LOOP AT GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>) .
    <L_DATA1>-SEL = UF_X.
    IF <L_DATA1>-SUM = ABAP_TRUE.
      PERFORM F_UPDATE_SUM USING <L_DATA1>
                           CHANGING GT_DATA1.
    ENDIF.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.
  IF GF_EXEC_TY = GC_EXEC_TY-NEW
  OR GF_EXEC_TY = GC_EXEC_TY-REPRINT
  OR GF_EXEC_TY = GC_EXEC_TY-CANCEL.
    INSERT VALUE #( FUNCTION = 'ZALL'
                    ICON = '@4B@'
                    QUICKINFO = 'Select All'(001) )
           INTO TABLE UF_OBJECT->MT_TOOLBAR.

    INSERT VALUE #( FUNCTION = 'ZSAL'
                    ICON = '@4D@'
                    QUICKINFO = 'Deselect All'(002) )
           INTO TABLE UF_OBJECT->MT_TOOLBAR.
  ENDIF.
* Handle Toolbar as needed
  IF GF_EXEC_TY = GC_EXEC_TY-NEW.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&REFRESH'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&PRINT_BACK'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&SORT_ASC'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&SORT_DSC'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&MB_FILTER'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&MB_SUM'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&MB_SUBTOT'.
  ELSE.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&REFRESH'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.
    DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&PRINT_BACK'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_sum
*&---------------------------------------------------------------------*
FORM F_UPDATE_SUM  USING    US_DATA1 TYPE TS_DATA1
                   CHANGING CT_DATA1 TYPE TT_DATA1.
  DATA: LV_SUM_BILLPL_AMT TYPE ZSDSFIT035-BILLPL_AMT.

  LOOP AT CT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>)
                   WHERE KUNNR = US_DATA1-KUNNR
                   AND   SEL = GC_TRUE.
    LV_SUM_BILLPL_AMT = LV_SUM_BILLPL_AMT + <L_DATA1>-BILLPL_AMT.
  ENDLOOP.

  READ TABLE CT_DATA1 ASSIGNING <L_DATA1> WITH KEY KUNNR = US_DATA1-KUNNR
                                                   SUM = GC_TRUE.
  IF SY-SUBRC = 0.
    <L_DATA1>-BILLPL_AMT = LV_SUM_BILLPL_AMT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE .

  DATA: LS_STABLE  TYPE LVC_S_STBL.
  DATA:
    LF_VALID   TYPE CHAR01  ##NEEDED,
    LF_REFRESH TYPE CHAR01  ##NEEDED.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).
  CASE GF_EXEC_TY .
    WHEN GC_EXEC_TY-NEW.
      PERFORM F_USER_COMMAND_SAVE_DB_NEW CHANGING GT_DATA1.
  ENDCASE.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = ''.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE_DB_NEW
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE_DB_NEW CHANGING CT_DATA1 TYPE TT_DATA1.
  DATA: LT_DATA1        TYPE TT_DATA1,
        LS_DATA1        TYPE TS_DATA1,
        LT_FCAT         TYPE LVC_T_FCAT,
        LT_033          TYPE STANDARD TABLE OF ZSDSFIT033,
        LS_033          LIKE LINE OF LT_033,
        LT_034          TYPE STANDARD TABLE OF ZSDSFIT034,
        LS_034          LIKE LINE OF LT_034,
        LT_035          TYPE STANDARD TABLE OF ZSDSFIT035,
        LS_035          LIKE LINE OF LT_035,
        LF_YEAR         TYPE INRI-TOYEAR,
        LF_NUMBER       TYPE ZSDSFIT033-BILLPL_NO,
        LF_DATUM        TYPE SY-DATUM,
        LF_UZEIT        TYPE SY-UZEIT,
        LF_COUNT_ITEM   TYPE I,
        LF_OPEN_AMT_SUM TYPE TS_DATA1-OPEN_AMT.

  LT_DATA1[] = CT_DATA1[].

  DELETE LT_DATA1 WHERE SEL = GC_FALSE
                  AND   SUM = GC_FALSE.
  "validate data
  LOOP AT LT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>) .
    AT NEW KUNNR.
      CLEAR LF_COUNT_ITEM .
    ENDAT.
    IF <L_DATA1>-SUM = GC_FALSE.
      LF_COUNT_ITEM = LF_COUNT_ITEM + 1.
    ENDIF.
    AT END OF KUNNR.
      IF LF_COUNT_ITEM = 0. "no item selected
        DELETE LT_DATA1 WHERE KUNNR = <L_DATA1>-KUNNR
                        AND   SUM = GC_TRUE.
        CONTINUE.
      ENDIF.
      READ TABLE CT_DATA1 INTO DATA(LS_SUM) WITH KEY KUNNR = <L_DATA1>-KUNNR
                                                     SUM = GC_TRUE.
      IF SY-SUBRC <> 0.
        CLEAR LS_SUM.
      ENDIF.
      IF LS_SUM-BILLPL_PMTDT IS INITIAL.
        MESSAGE E007(ZSDSFI01) WITH 'Payment Date'(m07) <L_DATA1>-KUNNR ##TEXT_DUP.
      ENDIF.
      IF LS_SUM-ACTION_TYPE IS INITIAL.
        MESSAGE E007(ZSDSFI01) WITH 'Action Type'(m08) <L_DATA1>-KUNNR.
      ENDIF.
      IF LS_SUM-ACTION_TYPE IS INITIAL.
        MESSAGE E007(ZSDSFI01) WITH 'Status'(m09) <L_DATA1>-KUNNR.
      ENDIF.
    ENDAT.
  ENDLOOP.
  LF_DATUM = SY-DATUM.
  LF_UZEIT = SY-UZEIT.
  LF_YEAR = LF_DATUM(4).

  "provide data for update
  LOOP AT LT_DATA1 ASSIGNING <L_DATA1>.


    AT NEW KUNNR.
      CLEAR LF_OPEN_AMT_SUM.
      READ TABLE CT_DATA1 INTO LS_SUM WITH KEY KUNNR = <L_DATA1>-KUNNR
                                               SUM = GC_TRUE.
      IF SY-SUBRC <> 0.
        CLEAR LS_SUM.
        CONTINUE.
      ENDIF.
      CLEAR LF_NUMBER.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = '01'
          OBJECT                  = GC_NUMBER_OBJECT
          QUANTITY                = '1'
          TOYEAR                  = LF_YEAR
        IMPORTING
          NUMBER                  = LF_NUMBER
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OBJECT_NOT_FOUND        = 3
          QUANTITY_IS_0           = 4
          QUANTITY_IS_NOT_1       = 5
          INTERVAL_OVERFLOW       = 6
          BUFFER_OVERFLOW         = 7
          OTHERS                  = 8.
      IF SY-SUBRC <> 0.
        MESSAGE E006(ZSDSFI01) ."Unable to generate Bill placement no.
      ENDIF.

*     ------ ZSDSFIT033 --------
      LS_033-BUKRS = P_BUKRS.
      LS_033-BILLPL_NO = LF_NUMBER.
      LS_033-BILLPL_DATE =  LF_DATUM.
      LS_033-KUNNR =  <L_DATA1>-KUNNR.
      LS_033-BILLPL_PMTDT = LS_SUM-BILLPL_PMTDT.
      LS_033-ACTION_TYPE  = LS_SUM-ACTION_TYPE.
      LS_033-STATUS = LS_SUM-STATUS.
      LS_033-DELFG = ''.
      LS_033-CRDAT = LF_DATUM.
      LS_033-CRTIM = LF_UZEIT.
      LS_033-CRUSR = SY-UNAME.
      LS_033-CR_PERNR = P_PERNR. "CH02+
      APPEND LS_033 TO LT_033.

*     -----ZSDSFIT034 ----------
      LS_034-BUKRS = P_BUKRS.
      LS_034-BILLPL_NO =  LF_NUMBER.
      LS_034-BILLPL_SEQNO = 0.
      LS_034-DELFG = ''.
      LS_034-CRDAT = LF_DATUM.
      LS_034-CRTIM = LF_UZEIT.
      LS_034-CRUSR =  SY-UNAME.
      APPEND LS_034 TO LT_034.
    ENDAT.
    IF LS_SUM IS INITIAL.
      CONTINUE.
    ENDIF.

*   -----ZSDSFIT035 ----------

    IF <L_DATA1>-SUM =  ABAP_FALSE.

      LF_OPEN_AMT_SUM =  LF_OPEN_AMT_SUM + <L_DATA1>-OPEN_AMT.
      LS_035-BUKRS = P_BUKRS.
      LS_035-BILLPL_NO = LF_NUMBER.
      LS_035-BELNR = <L_DATA1>-BELNR.
      LS_035-GJAHR = <L_DATA1>-GJAHR.
      LS_035-BUZEI = <L_DATA1>-BUZEI.
      LS_035-VBELN = <L_DATA1>-VBELN.
      LS_035-BILLPL_AMT = <L_DATA1>-BILLPL_AMT.
      LS_035-RECEIPT    = <L_DATA1>-RECEIPT.
      LS_035-DELFG = ''.
      APPEND LS_035 TO LT_035.
    ENDIF.
    AT END OF KUNNR.

      LS_DATA1-BILLPL_NO    = LF_NUMBER.
      LS_DATA1-BILLPL_DATE  = LF_DATUM.
      LS_DATA1-BILLPL_PMTDT = LS_SUM-BILLPL_PMTDT.
      LS_DATA1-ACTION_TYPE  = LS_SUM-ACTION_TYPE.
      LS_DATA1-STATUS       = LS_SUM-STATUS.
      MODIFY LT_DATA1 FROM LS_DATA1 TRANSPORTING BILLPL_NO BILLPL_DATE BILLPL_PMTDT ACTION_TYPE STATUS
      WHERE KUNNR = <L_DATA1>-KUNNR
      AND   SUM = ABAP_FALSE.

      LS_SUM-OPEN_AMT = LF_OPEN_AMT_SUM.
      MODIFY LT_DATA1 FROM LS_SUM TRANSPORTING OPEN_AMT
      WHERE KUNNR = <L_DATA1>-KUNNR
      AND   SUM = GC_TRUE.
    ENDAT.

  ENDLOOP.

  IF LT_033[] IS NOT INITIAL
  AND LT_034[] IS NOT INITIAL
  AND LT_035[] IS NOT INITIAL.
    CALL FUNCTION 'Z_SDSFI_BILLPL_CREATE' IN UPDATE TASK
      TABLES
        IT_ZSDSFIT033 = LT_033
        IT_ZSDSFIT034 = LT_034
        IT_ZSDSFIT035 = LT_035.
  ENDIF.
  COMMIT WORK AND WAIT.


  GF_EDIT = GC_FALSE.
  CALL METHOD GREF_GRID_1->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = LT_FCAT.

  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LFS_FCAT>) WHERE EDIT = GC_TRUE.
    <LFS_FCAT>-EDIT = ABAP_FALSE.
  ENDLOOP.

  CALL METHOD GREF_GRID_1->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = LT_FCAT.

  LOOP AT LT_DATA1 ASSIGNING <L_DATA1>.
    <L_DATA1>-CRDAT = LF_DATUM.
    <L_DATA1>-CRUSR = SY-UNAME.
    <L_DATA1>-CR_PERNR = P_PERNR. "CH02+
  ENDLOOP.

  CT_DATA1[] = LT_DATA1[].
* Bill Placement has been created sucessfully, please print document.
  MESSAGE I008(ZSDSFI01).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_and_prepare_data_exst
*&---------------------------------------------------------------------*
FORM F_SELECT_AND_PREPARE_DATA_EXST .
  DATA: LT_FI_DOC_KEY TYPE TT_FI_DOC_KEY,
        LT_CLEAR_DOC  TYPE TT_CLEAR_DOC,
        LT_FI_DOC     TYPE TT_FI_DOC,
        LT_SD_KEY     TYPE TT_SD_KEY,
        LT_VBRP       TYPE TT_VBRP,
        LT_VBKD       TYPE TT_VBKD,
        LT_PROJ       TYPE TT_PROJ,
        LT_PROJ_FI    TYPE TT_PROJ_FI,
        LS_DATA1      TYPE TS_DATA1,
        LF_DMBTR_CLR  TYPE BSEG-DMBTR,
        LF_TABIX      TYPE SY-TABIX.

  SELECT A~BUKRS,
         A~BILLPL_NO,
         A~BILLPL_DATE,
         A~KUNNR,
         A~BILLPL_PMTDT,
         A~ACTION_TYPE,
         A~STATUS,
         A~CRDAT,
         A~CRUSR,
         A~CR_PERNR, "CH02+
         B~BELNR,
         B~GJAHR,
         B~BUZEI,
         B~VBELN,
         B~BILLPL_AMT,
         B~RECEIPT
  INTO TABLE @DATA(LT_BP)
  FROM ZSDSFIT033 AS A INNER JOIN ZSDSFIT035 AS B
  ON  A~BUKRS = B~BUKRS
  AND A~BILLPL_NO = B~BILLPL_NO
  WHERE A~BUKRS   =  @P_BUKRS
  AND   A~KUNNR   IN @S_KUNNR
  AND   A~BILLPL_NO   IN @S_BP_NO
  AND   A~BILLPL_DATE IN @S_BP_DT
  AND   A~ACTION_TYPE IN @S_AC_TY
  AND   A~STATUS      IN @S_STAT
  AND   A~DELFG = ''.

  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING LT_BP TO LT_FI_DOC_KEY.
  PERFORM F_SELECT_FI_DOC_BY_BP USING LT_FI_DOC_KEY
                                CHANGING LT_FI_DOC.
  PERFORM F_SELECT_CLEAR_DOC USING LT_FI_DOC_KEY
                             CHANGING LT_CLEAR_DOC.
  MOVE-CORRESPONDING LT_BP TO LT_SD_KEY.
  SORT LT_SD_KEY BY VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_SD_KEY COMPARING VBELN.
  PERFORM F_SELECT_SD_DOC_BY_BP USING LT_SD_KEY
                                CHANGING LT_VBRP
                                         LT_VBKD.
  PERFORM F_SELECT_COMP_DATA USING P_BUKRS
                             CHANGING GS_COMP .

  PERFORM F_SELECT_CUSTOMER_DATA USING LT_FI_DOC
                                 CHANGING GT_CUST.

  PERFORM F_SELECT_PROJ USING LT_VBRP
                              LT_FI_DOC
                        CHANGING LT_PROJ
                                 LT_PROJ_FI.

  SORT LT_FI_DOC BY BUKRS BELNR GJAHR  BUZEI .
  SORT LT_VBKD BY VBELN_BILL.

  LOOP AT LT_BP ASSIGNING FIELD-SYMBOL(<LFS_BP>).
    CLEAR LS_DATA1.
    MOVE-CORRESPONDING <LFS_BP> TO LS_DATA1.
    READ TABLE LT_FI_DOC INTO DATA(LS_FI_DOC) WITH KEY BUKRS = <LFS_BP>-BUKRS
                                                       BELNR = <LFS_BP>-BELNR
                                                       GJAHR = <LFS_BP>-GJAHR
                                                       BINARY SEARCH.
    CHECK SY-SUBRC = 0.
    LS_DATA1-BLART =  LS_FI_DOC-BLART.
    LS_DATA1-XBLNR =  LS_FI_DOC-XBLNR.
    LS_DATA1-BKTXT =  LS_FI_DOC-BKTXT.
    LS_DATA1-BUDAT =  LS_FI_DOC-BUDAT.
    LS_DATA1-ZTERM =  LS_FI_DOC-ZTERM.

    READ TABLE GT_CUST INTO DATA(LS_CUST) WITH  KEY KUNNR = <LFS_BP>-KUNNR.
    IF SY-SUBRC = 0.
      LS_DATA1-NAME1 = LS_CUST-NAME1.
    ENDIF.
    "---  find SD Document
    IF LS_DATA1-VBELN IS NOT INITIAL.
      READ TABLE LT_VBKD TRANSPORTING NO FIELDS WITH KEY VBELN_BILL = LS_DATA1-VBELN
                                                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        LF_TABIX = SY-TABIX.
        LOOP AT LT_VBKD ASSIGNING FIELD-SYMBOL(<L_VBKD>) FROM LF_TABIX.
          IF <L_VBKD>-VBELN_BILL <> <LFS_BP>-VBELN.
            EXIT.
          ENDIF.
          IF LS_DATA1-BSTKD IS INITIAL.
            LS_DATA1-BSTKD  = <L_VBKD>-BSTKD.
          ELSE.
            LS_DATA1-BSTKD = |{ LS_DATA1-BSTKD }, { <L_VBKD>-BSTKD }|.
          ENDIF.
        ENDLOOP.
      ELSE.
        PERFORM F_GET_PO_FROM_SV_ORDER USING LS_DATA1-VBELN
                                       CHANGING LS_DATA1-BSTKD.

      ENDIF.

      READ TABLE LT_VBRP ASSIGNING FIELD-SYMBOL(<L_VBRP>) WITH KEY VBELN_BILL = LS_DATA1-VBELN.
      IF SY-SUBRC = 0.
        READ TABLE LT_PROJ INTO DATA(LS_PROJ) WITH KEY PSPNR = <L_VBRP>-PSPHI.
        IF SY-SUBRC = 0.
          LS_DATA1-PROJK = LS_PROJ-PSPID.
          LS_DATA1-POST1 = LS_PROJ-POST1.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE LT_PROJ_FI INTO DATA(LS_PROJ_FI) WITH KEY PSPID = LS_FI_DOC-SGTXT ##WARN_OK.
      IF SY-SUBRC = 0.
        LS_DATA1-PROJK = LS_PROJ_FI-PSPID.
        LS_DATA1-POST1 = LS_PROJ_FI-POST1.
      ENDIF.
    ENDIF.

*    BSTKD          TYPE TEXT80,

    "-- find clearing document
    READ TABLE LT_CLEAR_DOC TRANSPORTING NO FIELDS WITH KEY
                            BUKRS = <LFS_BP>-BUKRS
                            REBZG = <LFS_BP>-BELNR
                            REBZJ = <LFS_BP>-GJAHR
                            REBZZ = <LFS_BP>-BUZEI.
    IF SY-SUBRC = 0.
      LF_TABIX = SY-TABIX.
      LOOP AT LT_CLEAR_DOC ASSIGNING FIELD-SYMBOL(<L_CLEAR_DOC>) FROM LF_TABIX.
        IF  <L_CLEAR_DOC>-BUKRS <> <LFS_BP>-BUKRS
        OR  <L_CLEAR_DOC>-REBZG <> <LFS_BP>-BELNR
        OR  <L_CLEAR_DOC>-REBZJ <> <LFS_BP>-GJAHR
        OR  <L_CLEAR_DOC>-REBZZ <> <LFS_BP>-BUZEI.
          EXIT.
        ENDIF.
        IF <L_CLEAR_DOC>-SHKZG = 'S'.
          <L_CLEAR_DOC>-DMBTR = <L_CLEAR_DOC>-DMBTR * -1.
        ENDIF.
        LF_DMBTR_CLR = LF_DMBTR_CLR + <L_CLEAR_DOC>-DMBTR.
      ENDLOOP.
    ENDIF.
    IF LS_FI_DOC-SHKZG = 'H'.
      LS_DATA1-OPEN_AMT = LS_FI_DOC-DMBTR * - 1.
    ELSE.
      LS_DATA1-OPEN_AMT = LS_FI_DOC-DMBTR.
    ENDIF.

    "amount = inv - clearing
    LS_DATA1-OPEN_AMT     = LS_DATA1-OPEN_AMT - LF_DMBTR_CLR.
    LS_DATA1-UMSKZ        = LS_FI_DOC-UMSKZ.

    "---- net due date
    PERFORM F_GET_NET_DUE_DATE  USING LS_FI_DOC-BLDAT
                                      LS_FI_DOC-BUDAT
                                      LS_FI_DOC-ZTERM
                                CHANGING LS_DATA1-NETDT.
    LS_DATA1-WAERS = GS_COMP-WAERS.
    READ TABLE GT_ACTION_TYPE INTO DATA(LS_ACTION_TYPE) WITH  KEY VALUE = LS_DATA1-ACTION_TYPE.
    IF SY-SUBRC = 0.
      LS_DATA1-ACTION_TYPE_TX = LS_ACTION_TYPE-TEXT.
    ENDIF.
    READ TABLE GT_STATUS INTO DATA(LS_STATUS) WITH  KEY VALUE = LS_DATA1-STATUS.
    IF SY-SUBRC = 0.
      LS_DATA1-STATUS_TX = LS_STATUS-TEXT.
    ENDIF.
    APPEND LS_DATA1 TO GT_DATA1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_fi_doc_by_bp
*&---------------------------------------------------------------------*
FORM F_SELECT_FI_DOC_BY_BP  USING    UT_FI_DOC_KEY TYPE TT_FI_DOC_KEY
                            CHANGING CT_FI_DOC TYPE TT_FI_DOC.
  IF UT_FI_DOC_KEY[] IS INITIAL.
    RETURN.
  ENDIF.
  SELECT  B~KUNNR,
          A~BUKRS,
          A~BELNR,
          A~GJAHR,
          B~BUZEI,
          A~BLART,
          A~XBLNR,
          A~BKTXT,
          A~AWTYP,
          A~BLDAT,
          B~VBELN,
          A~BUDAT,
          B~ZTERM,
          B~SHKZG,
          B~DMBTR,
          B~UMSKZ,
          B~SGTXT, "Project
          D~BILLPL_NO,
          D~DELFG
  FROM BKPF AS A INNER JOIN BSEG AS B
  ON  A~BUKRS = B~BUKRS
  AND A~GJAHR = B~GJAHR
  AND A~BELNR = B~BELNR
                 INNER JOIN ZSDSFIT035 AS D
  ON  B~BUKRS = D~BUKRS
  AND B~GJAHR = D~GJAHR
  AND B~BELNR = D~BELNR
  AND B~BUZEI = D~BUZEI
  FOR ALL ENTRIES IN @UT_FI_DOC_KEY
  WHERE A~BUKRS = @P_BUKRS
  AND   A~BELNR = @UT_FI_DOC_KEY-BELNR
  AND   A~GJAHR = @UT_FI_DOC_KEY-GJAHR
  AND   B~BUZEI = @UT_FI_DOC_KEY-BUZEI
  AND   A~XREVERSAL = ''
  INTO TABLE @CT_FI_DOC.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_sd_doc_by_bp
*&---------------------------------------------------------------------*
FORM F_SELECT_SD_DOC_BY_BP  USING    UT_SD_KEY TYPE TT_SD_KEY
                            CHANGING CT_VBRP TYPE TT_VBRP
                                     CT_VBKD TYPE TT_VBKD.
  IF UT_SD_KEY IS INITIAL.
    RETURN.
  ENDIF.
  SELECT VBELN,
         PSPHI
  INTO TABLE @CT_VBRP
  FROM VBRP AS A  INNER JOIN PRPS  AS B
  ON   A~PS_PSP_PNR = B~PSPNR
  FOR ALL ENTRIES IN @UT_SD_KEY
  WHERE A~VBELN = @UT_SD_KEY-VBELN.


  SELECT A~VBELN AS VBELN_BILL,
         B~VBELN AS VBELN_SO,
         B~BSTKD
  FROM VBRP AS A INNER JOIN VBKD AS B
  ON A~AUBEL = B~VBELN
  FOR ALL ENTRIES IN @UT_SD_KEY
  WHERE A~VBELN = @UT_SD_KEY-VBELN
  AND BSTKD  <> ''
  AND B~POSNR = '000000'
  INTO TABLE @CT_VBKD.

  SORT CT_VBKD BY VBELN_BILL VBELN_SO.
  DELETE ADJACENT DUPLICATES FROM CT_VBKD COMPARING VBELN_BILL VBELN_SO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_EXST
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC_EXST  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'SUM'
      OR 'WAERS'
      OR 'BUZEI'.
        <FCAT>-TECH = GC_TRUE.
      WHEN 'SEL'.
        IF GF_EXEC_TY = GC_EXEC_TY-DETAIL.
          <FCAT>-TECH = GC_TRUE.
        ELSE.
          %FCAT 'Select'(c01).
          <FCAT>-CHECKBOX = GC_TRUE.
          <FCAT>-EDIT     = GC_TRUE.
          <FCAT>-COL_POS  = 1.
        ENDIF.
      WHEN 'KUNNR'.
        <FCAT>-COL_POS  = 2.
      WHEN 'BILLPL_NO'.
        <FCAT>-COL_POS  = 3.
      WHEN 'BILLPL_DATE'.
        <FCAT>-COL_POS  = 4.
      WHEN OTHERS.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 4.
        CASE <FCAT>-FIELDNAME.
          WHEN 'VBELN'.
            <FCAT>-HOTSPOT = GC_TRUE.
          WHEN 'BSTKD'.
            %FCAT 'P/O No.'(c03).
          WHEN 'RECEIPT'.
            %FCAT 'Receipt No.'(c04).
          WHEN 'BUDAT'.
            %FCAT 'Invoice Date'(c05).
          WHEN 'OPEN_AMT'.
            %FCAT 'Open Inv Amount'(c07).
            <FCAT>-CFIELDNAME = 'WAERS'.
            <FCAT>-TECH = GC_TRUE.
          WHEN 'BILLPL_AMT'.
            IF GF_EXEC_TY = GC_EXEC_TY-NEW.
              <FCAT>-EDIT    = GC_TRUE.
            ELSE.
              <FCAT>-DO_SUM = GC_TRUE.
            ENDIF.
            <FCAT>-CFIELDNAME = 'WAERS'.
          WHEN 'BILLPL_PMTDT'.
            %FCAT 'Payment Date'(c10) ##TEXT_DUP.
            <FCAT>-REF_TABLE  = 'ZSDSFIT033'.
            <FCAT>-REF_FIELD  = 'BILLPL_PMTDT'.
          WHEN 'ACTION_TYPE'.
            <FCAT>-REF_TABLE  = 'ZSDSFIT033'.
            <FCAT>-REF_FIELD  = 'ACTION_TYPE'.
          WHEN 'ACTION_TYPE_TX'.
            %FCAT 'Action Type descr'(c08).
          WHEN 'STATUS'.
            <FCAT>-REF_TABLE  = 'ZSDSFIT033'.
            <FCAT>-REF_FIELD  = 'STATUS'.
          WHEN 'STATUS_TX'.
            %FCAT 'Status descr'(c09).
        ENDCASE.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT_EXST
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT_EXST  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'KUNNR',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'BILLPL_NO'.


  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = GC_TRUE.
  APPEND LS_SORT TO CT_SORT.

  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = GC_TRUE.
  APPEND LS_SORT TO CT_SORT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_CANCEL
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_CANCEL  .
  DATA: LS_STABLE  TYPE LVC_S_STBL.
  DATA: LF_ANS TYPE C.
  DATA: LF_VALID   TYPE CHAR01  ##NEEDED,
        LF_REFRESH TYPE CHAR01  ##NEEDED.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).
  READ TABLE GT_DATA1 TRANSPORTING NO FIELDS WITH KEY SEL = GC_TRUE.
  IF SY-SUBRC <> 0.
    CLEAR GF_SAVE_OK.
    MESSAGE E000(ZSDSCA01) WITH 'Please select bill placement to be cancelled'(m06).
  ENDIF.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Bill Placement Cancellation'(m04)
      TEXT_QUESTION         = 'Do you really want to cancel Bill placement(s)'(M05)
      TEXT_BUTTON_1         = 'Yes'(M10)
      ICON_BUTTON_1         = 'ICON_CHECKED'
      TEXT_BUTTON_2         = 'No'(M11)
      ICON_BUTTON_2         = 'ICON_CANCEL'
      DISPLAY_CANCEL_BUTTON = ' '
      POPUP_TYPE            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      ANSWER                = LF_ANS
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ELSE.
    IF LF_ANS = 2.
      CLEAR GF_SAVE_OK.
      RETURN.
    ENDIF.
  ENDIF.

  PERFORM F_USER_COMMAND_SAVE_DB_CANC CHANGING GT_DATA1.

  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = ''.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = ''.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE_DB_CANC
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE_DB_CANC  CHANGING CT_DATA TYPE TT_DATA1.
  TYPES: BEGIN OF LTS_KEY,
           BILLPL_NO TYPE ZSDSFIT033-BILLPL_NO,
         END OF LTS_KEY.
  DATA: LT_KEY     TYPE TABLE OF LTS_KEY,
        LS_034     TYPE ZSDSFIT034,
        LT_034_INS TYPE STANDARD TABLE OF ZSDSFIT034,
        LT_FCAT    TYPE LVC_T_FCAT.

  DELETE CT_DATA WHERE SEL <> GC_TRUE.

  MOVE-CORRESPONDING CT_DATA  TO  LT_KEY .
  SORT LT_KEY BY BILLPL_NO.
  DELETE ADJACENT DUPLICATES FROM LT_KEY COMPARING BILLPL_NO.

  IF LT_KEY[] IS INITIAL.
    RETURN.
  ENDIF.

  SELECT *                                    "#EC CI_ALL_FIELDS_NEEDED
  INTO TABLE @DATA(LT_033)
  FROM ZSDSFIT033
  FOR ALL ENTRIES IN @LT_KEY
  WHERE BUKRS = @P_BUKRS
  AND   BILLPL_NO = @LT_KEY-BILLPL_NO.

  SELECT *                                    "#EC CI_ALL_FIELDS_NEEDED
  INTO TABLE @DATA(LT_034)
  FROM ZSDSFIT034
  FOR ALL ENTRIES IN @LT_KEY
  WHERE BUKRS = @P_BUKRS
  AND   BILLPL_NO = @LT_KEY-BILLPL_NO.

  SELECT *                                    "#EC CI_ALL_FIELDS_NEEDED
  INTO TABLE @DATA(LT_035)
  FROM ZSDSFIT035
  FOR ALL ENTRIES IN @LT_KEY
  WHERE BUKRS = @P_BUKRS
  AND   BILLPL_NO = @LT_KEY-BILLPL_NO.
  DATA(LV_DATUM) = SY-DATUM.
  DATA(LV_UZEIT) = SY-UZEIT.

  LOOP AT LT_033 ASSIGNING FIELD-SYMBOL(<LFS_033>).
    <LFS_033>-DELFG = GC_TRUE.
    <LFS_033>-UPDAT = LV_DATUM.
    <LFS_033>-UPTIM = LV_UZEIT.
    <LFS_033>-UPUSR = SY-UNAME.
    <LFS_033>-UP_PERNR = SY-UNAME. "CH02+
  ENDLOOP.

  SORT LT_034 BY BUKRS BILLPL_NO BILLPL_SEQNO DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_034 COMPARING BUKRS BILLPL_NO.

  LOOP AT LT_034 ASSIGNING FIELD-SYMBOL(<LFS_034>).
    CLEAR LS_034.
    LS_034 = <LFS_034>.
    LS_034-BILLPL_SEQNO = LS_034-BILLPL_SEQNO + 1.
    LS_034-DELFG = GC_TRUE.
    LS_034-CRDAT = LV_DATUM.
    LS_034-CRTIM = LV_UZEIT.
    LS_034-CRUSR = SY-UNAME.
    APPEND LS_034 TO LT_034_INS.
  ENDLOOP.

  LOOP AT LT_035 ASSIGNING FIELD-SYMBOL(<LFS_035>).
    <LFS_035>-DELFG = GC_TRUE.
  ENDLOOP.

  CALL FUNCTION 'Z_SDSFI_BILLPL_CANCEL' IN UPDATE TASK
    TABLES
      IT_ZSDSFIT033 = LT_033
      IT_ZSDSFIT034 = LT_034_INS
      IT_ZSDSFIT035 = LT_035.

  COMMIT WORK AND WAIT.

  GF_EDIT = ABAP_FALSE.

  CALL METHOD GREF_GRID_1->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = LT_FCAT.

  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LFS_FCAT>) WHERE EDIT = GC_TRUE.
    <LFS_FCAT>-EDIT = ABAP_FALSE.
  ENDLOOP.

  CALL METHOD GREF_GRID_1->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = LT_FCAT.
* Bill Placement has been cancelled sucessfully
  MESSAGE S009(ZSDSFI01).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_user_command_print
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_PRINT .
  DATA: LT_DATA1_PRINT TYPE TT_DATA1,
        LT_DATA1_GRP   TYPE TT_DATA1,
        LT_034_INS     TYPE TABLE OF ZSDSFIT034,
        LF_COUNT       TYPE I,
        LF_ALL_LINE    TYPE I,
        LF_FM_NAME     TYPE RS38L_FNAM,
        LF_VALID       TYPE CHAR01  ##NEEDED,
        LF_REFRESH     TYPE CHAR01  ##NEEDED.

  DATA: LS_OUTPUT_OPTIONS     TYPE SSFCOMPOP,
        LS_OUTPUT_OPTIONS2    TYPE SSFCRESOP,
        LS_CONTROL_PARAMETERS TYPE SSFCTRLOP.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).

  READ TABLE GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>) WITH KEY SEL = GC_TRUE.
  IF SY-SUBRC <> 0.
    CLEAR GF_SAVE_OK.
    MESSAGE E000(ZSDSCA01) WITH 'Please select bill placement for printing'(m12).
  ENDIF.
  LT_DATA1_PRINT = GT_DATA1.

  DELETE LT_DATA1_PRINT WHERE SEL = GC_FALSE
                        OR    SUM = GC_TRUE.

  SORT LT_DATA1_PRINT BY KUNNR BILLPL_NO
                         BUDAT. "CH03+
  LT_DATA1_GRP = LT_DATA1_PRINT.
  DELETE ADJACENT DUPLICATES FROM LT_DATA1_GRP COMPARING KUNNR BILLPL_NO.
  LF_ALL_LINE = LINES( LT_DATA1_GRP ).

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = GC_SMARTFORM_NAME
    IMPORTING
      FM_NAME            = LF_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  LOOP AT LT_DATA1_PRINT ASSIGNING FIELD-SYMBOL(<L_DATA1_PRINT>).
    AT NEW BILLPL_NO.
      LF_COUNT = LF_COUNT + 1.

      IF LF_ALL_LINE = 1.
        LS_CONTROL_PARAMETERS-NO_OPEN = GC_FALSE.
        LS_CONTROL_PARAMETERS-NO_CLOSE = GC_FALSE.
      ELSE.
        IF LF_COUNT = 1.
          "First Record
          LS_CONTROL_PARAMETERS-NO_OPEN = GC_FALSE.
          LS_CONTROL_PARAMETERS-NO_CLOSE = GC_TRUE.
        ELSEIF LF_COUNT = LF_ALL_LINE.
          "Last Record
          LS_CONTROL_PARAMETERS-NO_OPEN = GC_TRUE.
          LS_CONTROL_PARAMETERS-NO_CLOSE = GC_FALSE.
        ELSE.
          "Nth Records
          LS_CONTROL_PARAMETERS-NO_OPEN = GC_TRUE.
          LS_CONTROL_PARAMETERS-NO_CLOSE = GC_TRUE.
        ENDIF.
      ENDIF.
    ENDAT.
    APPEND <L_DATA1_PRINT> TO GT_DATA1_PRINT.
    AT END OF BILLPL_NO.
      READ TABLE GT_CUST INTO GS_CUST WITH KEY KUNNR = <L_DATA1_PRINT>-KUNNR.
      LS_OUTPUT_OPTIONS-TDIMMED = GC_TRUE.
      "call smartform
      CALL FUNCTION LF_FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = LS_CONTROL_PARAMETERS
          OUTPUT_OPTIONS     = LS_OUTPUT_OPTIONS
          USER_SETTINGS      = ''
          IM_WITH_RC         = P_WTH_RC
          IM_WITH_PJ         = P_WTH_PJ
        IMPORTING
          JOB_OUTPUT_OPTIONS = LS_OUTPUT_OPTIONS2
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                DISPLAY LIKE 'E'.
        LEAVE  LIST-PROCESSING.
      ELSE.
        IF LS_OUTPUT_OPTIONS2-TDPREVIEW = ABAP_FALSE.
          PERFORM F_PREPARE_DB_PRINT USING <L_DATA1_PRINT>-BILLPL_NO
                                     CHANGING LT_034_INS.
        ENDIF.
      ENDIF.

      CLEAR: GT_DATA1_PRINT,
             GS_CUST.
    ENDAT.
  ENDLOOP.
  IF LT_034_INS[] IS NOT INITIAL.
    CALL FUNCTION 'Z_SDSFI_BILLPL_REPRINT'
      TABLES
        IT_ZSDSFIT034 = LT_034_INS.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_COMP_DATA
*&---------------------------------------------------------------------*
FORM F_SELECT_COMP_DATA  USING    UF_BUKRS TYPE T001-BUKRS
                         CHANGING CS_COMP TYPE TS_COMP.
  DATA: LT_GEN_C TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C,
        LF_HBKID TYPE T012K-HBKID,
        LF_HKTID TYPE T012K-HKTID.

  SELECT SINGLE BUKRS, WAERS, ADRNR, STCEG
  INTO @DATA(LS_T001)
  FROM T001
  WHERE BUKRS = @P_BUKRS.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.
  SELECT  ADDRNUMBER,
          DATE_FROM,
          NATION,
          DATE_TO,
          NAME1,
          STREET,
          CITY2,
          CITY1,
          POST_CODE1,
          TEL_NUMBER,
          FAX_NUMBER
  INTO TABLE @DATA(LT_ADRC)
  FROM ADRC
  WHERE ADDRNUMBER = @LS_T001-ADRNR
  AND   DATE_FROM <= @SY-DATUM
  AND   DATE_TO >= @SY-DATUM
  AND   ( NATION = '' OR NATION = 'I' ).

  CS_COMP-BUKRS = UF_BUKRS.
  CS_COMP-WAERS = LS_T001-WAERS.
  CS_COMP-STCEG = LS_T001-STCEG.

  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = SY-REPID
    IMPORTING
      ET_GEN_C = LT_GEN_C.
  LOOP AT LT_GEN_C ASSIGNING FIELD-SYMBOL(<L_GEN_C>).
    CASE <L_GEN_C>-PARAM.
      WHEN 'COMMERCIAL_REGIS'.
        CS_COMP-COMMERCIAL_REGIS = <L_GEN_C>-VALUE_LOW.
      WHEN  'QF_FORM_NO'.
        CS_COMP-QF_FORM_NO = <L_GEN_C>-VALUE_LOW.
      WHEN 'COMP_HBKID'.
        LF_HBKID = <L_GEN_C>-VALUE_LOW.
      WHEN 'COMP_HKTID'.
        LF_HKTID = <L_GEN_C>-VALUE_LOW.
      WHEN 'COMP_BANK_NAME_EN'.
        CS_COMP-EN_BANK_NAME = <L_GEN_C>-VALUE_LOW.
      WHEN 'COMP_BANK_NAME_TH'.
        CS_COMP-TH_BANK_NAME = <L_GEN_C>-VALUE_LOW.
      WHEN 'COMP_TEL_EXT_NO'.
        CS_COMP-TEL_EXT_NO = <L_GEN_C>-VALUE_LOW.
    ENDCASE.
  ENDLOOP.


  IF  LF_HBKID IS NOT INITIAL
  AND  LF_HKTID IS NOT INITIAL.
    SELECT SINGLE BANKN
    INTO @CS_COMP-BANK_ID
    FROM T012K
    WHERE BUKRS = @P_BUKRS
    AND   HBKID = @LF_HBKID
    AND   HKTID = @LF_HKTID.
  ENDIF.


  LOOP AT LT_ADRC ASSIGNING FIELD-SYMBOL(<L_ADRC>).
    IF <L_ADRC>-NATION = ''.
      CS_COMP-TH_NAME1  = <L_ADRC>-NAME1.
      CS_COMP-TH_STREET = <L_ADRC>-STREET.
      CS_COMP-TH_CITY2  = <L_ADRC>-CITY2.
      CS_COMP-TH_CITY1  = <L_ADRC>-CITY1.
      CS_COMP-TH_POST_CODE1 = <L_ADRC>-POST_CODE1.
    ELSEIF <L_ADRC>-NATION = 'I'.
      CS_COMP-EN_NAME1  = <L_ADRC>-NAME1.
      CS_COMP-EN_STREET = <L_ADRC>-STREET.
      CS_COMP-EN_CITY2  = <L_ADRC>-CITY2.
      CS_COMP-EN_CITY1  = <L_ADRC>-CITY1.
      CS_COMP-EN_POST_CODE1 = <L_ADRC>-POST_CODE1.
      CS_COMP-TEL_NUMBER    = <L_ADRC>-TEL_NUMBER.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_CUSTOMER_DATA
*&---------------------------------------------------------------------*
FORM F_SELECT_CUSTOMER_DATA  USING    UT_FI_DOC TYPE TT_FI_DOC
                             CHANGING CT_CUST TYPE TT_CUST.
  TYPES:
    BEGIN OF LTS_KNA1_KEY,
      KUNNR TYPE KNA1-KUNNR,
    END OF LTS_KNA1_KEY.
  DATA: LT_KNA1_KEY TYPE STANDARD TABLE OF LTS_KNA1_KEY.

  MOVE-CORRESPONDING UT_FI_DOC TO LT_KNA1_KEY.
  SORT LT_KNA1_KEY BY KUNNR.
  DELETE ADJACENT DUPLICATES FROM LT_KNA1_KEY COMPARING KUNNR.

  IF LT_KNA1_KEY[] IS NOT INITIAL.
    SELECT KNA1~KUNNR,
           KNA1~STCD3,
           ADRC~NATION,
           ADRC~NAME1,
           ADRC~NAME2,
           ADRC~NAME3,  "CH01+
           ADRC~NAME4,  "CH01+
           ADRC~STREET,
           ADRC~STR_SUPPL3,
           LOCATION,
           ADRC~CITY2,
           ADRC~HOME_CITY,
           ADRC~CITY1,
           ADRC~POST_CODE1,
           ADRC~TEL_NUMBER,
           ADRC~FAX_NUMBER
    INTO TABLE @DATA(LT_ADRC)
    FROM KNA1 INNER JOIN ADRC
    ON KNA1~ADRNR = ADRC~ADDRNUMBER
    FOR ALL ENTRIES IN @LT_KNA1_KEY
    WHERE KUNNR = @LT_KNA1_KEY-KUNNR
    AND   ( NATION = '' OR NATION = 'I' )
    AND   DATE_FROM <= @SY-DATUM
    AND   DATE_TO >= @SY-DATUM.

    SORT LT_ADRC BY KUNNR NATION .
    DELETE ADJACENT DUPLICATES FROM LT_ADRC COMPARING KUNNR.

    SELECT KUNNR,
           J_1TPBUPL
    INTO TABLE @DATA(LT_BRANCH)
    FROM FITHA_PBUPL_D
    FOR ALL ENTRIES IN @LT_KNA1_KEY
    WHERE KUNNR = @LT_KNA1_KEY-KUNNR
    AND  DEFAULT_BRANCH = @GC_TRUE.

    SORT LT_BRANCH BY KUNNR.

  ENDIF.

  MOVE-CORRESPONDING LT_ADRC TO CT_CUST.

  LOOP AT CT_CUST ASSIGNING FIELD-SYMBOL(<L_CUST>).
    READ TABLE LT_BRANCH INTO DATA(LS_BRANCH) WITH KEY KUNNR = <L_CUST>-KUNNR
                                                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      <L_CUST>-J_1TPBUPL = LS_BRANCH-J_1TPBUPL.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_HOTSPOT_CLICK_1
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW
                             US_COLUMN_ID TYPE LVC_S_COL ##CALLED.

  READ TABLE GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>) INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC = 0 AND <L_DATA1>-VBELN  IS NOT INITIAL.
    IF US_COLUMN_ID-FIELDNAME = 'VBELN'.
      SET PARAMETER ID 'VF' FIELD <L_DATA1>-VBELN.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA_TO_SMARTFORM
*&---------------------------------------------------------------------*
FORM F_GET_DATA_TO_SMARTFORM CHANGING CT_DATA1 TYPE TT_DATA1
                                      CS_COMP TYPE TS_COMP
                                      CS_CUST TYPE TS_CUST ##CALLED.
  SORT GT_DATA1_PRINT BY BUDAT. "CH03+
  CT_DATA1 = GT_DATA1_PRINT.
  CS_COMP  = GS_COMP.
  CS_CUST  = GS_CUST.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_DB_PRINT
*&---------------------------------------------------------------------*
FORM F_PREPARE_DB_PRINT USING UF_BILLPL_NO TYPE ZSDSFIT034-BILLPL_NO
                        CHANGING CF_034_INS TYPE TT_034.
  DATA: LS_034 TYPE ZSDSFIT034.
  SELECT MAX( BILLPL_SEQNO )
  INTO @DATA(LV_MAX_SEQNO)
  FROM ZSDSFIT034
  WHERE BUKRS = @P_BUKRS
  AND   BILLPL_NO = @UF_BILLPL_NO.

  LS_034-BUKRS        = P_BUKRS.
  LS_034-BILLPL_NO    = UF_BILLPL_NO.
  LS_034-BILLPL_SEQNO = LV_MAX_SEQNO + 1.
  LS_034-REPRINT      = GC_TRUE.
  LS_034-CRDAT        = SY-DATUM.
  LS_034-CRTIM        = SY-UZEIT.
  LS_034-CRUSR        = SY-UNAME.
  APPEND LS_034 TO CF_034_INS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_proj
*&---------------------------------------------------------------------*
FORM F_SELECT_PROJ  USING   UT_VBRP TYPE TT_VBRP
                            UT_FI_DOC TYPE TT_FI_DOC
                    CHANGING UT_PROJ TYPE TT_PROJ
                             UT_PROJ_FI TYPE TT_PROJ_FI.
  TYPES: BEGIN OF LTY_SGTXT,
           SGTXT TYPE PROJ-PSPID,
         END OF LTY_SGTXT.
  DATA: LT_SGTXT TYPE STANDARD TABLE OF LTY_SGTXT.
  IF UT_VBRP IS NOT INITIAL.
    SELECT PSPNR,                                  "#EC CI_NO_TRANSFORM
           PSPID,
           POST1
    INTO TABLE @UT_PROJ
    FROM PROJ
    FOR ALL ENTRIES IN @UT_VBRP
    WHERE PSPNR  = @UT_VBRP-PSPHI.
  ENDIF.
  IF UT_FI_DOC IS NOT INITIAL.
    MOVE-CORRESPONDING UT_FI_DOC TO LT_SGTXT.
    DELETE LT_SGTXT WHERE SGTXT IS INITIAL.
    IF LT_SGTXT[] IS NOT INITIAL.
      SELECT PSPID,
             POST1
      INTO TABLE @UT_PROJ_FI
      FROM PROJ
      FOR ALL ENTRIES IN @LT_SGTXT
      WHERE PSPID = @LT_SGTXT-SGTXT.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_po_from_sv_order
*&---------------------------------------------------------------------*
FORM F_GET_PO_FROM_SV_ORDER  USING    UF_VBELN TYPE VBRP-VBELN
                             CHANGING CF_BSTKD TYPE TS_DATA1-BSTKD.
  CONSTANTS: LC_VBTYP_SVORDER TYPE VBFA-VBTYP_N VALUE 'CSVO',
             LC_VBTYP_SVCONTR TYPE VBFA-VBTYP_N VALUE 'CSCT'.
  DATA: LT_DOCFLOW  TYPE TDT_DOCFLOW,
        LR_VBTYP_SV TYPE RANGE OF VBFA-VBTYP_N.

  CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
    EXPORTING
      IV_DOCNUM  = UF_VBELN
    IMPORTING
      ET_DOCFLOW = LT_DOCFLOW.

  APPEND VALUE #( SIGN = 'I'
                 OPTION = 'EQ'
                 LOW = LC_VBTYP_SVORDER ) TO LR_VBTYP_SV.
  APPEND VALUE #( SIGN = 'I'
                 OPTION = 'EQ'
                 LOW = LC_VBTYP_SVCONTR ) TO LR_VBTYP_SV.
  DELETE LT_DOCFLOW WHERE VBTYP_N NOT IN LR_VBTYP_SV.
  IF LT_DOCFLOW IS NOT INITIAL.
    SELECT OBJECT_ID,
           ZZ1_CUS_PO
    INTO TABLE @DATA(LT_SV)
    FROM CRMS4D_SERV_H
    FOR ALL ENTRIES IN @LT_DOCFLOW
    WHERE OBJECT_ID = @LT_DOCFLOW-DOCNUM
    AND   ZZ1_CUS_PO <> ''.
    LOOP AT LT_SV ASSIGNING FIELD-SYMBOL(<L_SV>).
      IF CF_BSTKD IS INITIAL.
        CF_BSTKD  = <L_SV>-ZZ1_CUS_PO.
      ELSE.
        CF_BSTKD = |{ CF_BSTKD }, { <L_SV>-ZZ1_CUS_PO }|.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
