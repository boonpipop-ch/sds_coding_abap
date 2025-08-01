*&---------------------------------------------------------------------*
*& Include          ZETX003_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

ENDFORM.                    "initialization
*&---------------------------------------------------------------------*
*& Form START
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM START .

  PERFORM: VALIDATE_DATA,
           INIT_DATA,
           GET_DOC_HEADER,
           GET_OTH_DATA,
           GET_DESC,
           PROCESS_DATA.

ENDFORM.                    "start
*&---------------------------------------------------------------------*
*& Form END
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM END .

  PERFORM DISPLAY_DATA.

ENDFORM.                    "end
*&---------------------------------------------------------------------*
*& Form VALIDATE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM VALIDATE_DATA .

  "Validate Module selection
  IF CB_SD IS INITIAL AND
     CB_FI IS INITIAL.
    M_MESSAGE: 'S' '000' '38' TEXT-E00 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "Validate Replacement Document selection
  IF CB_N_RPL IS INITIAL AND
     CB_RPL IS INITIAL.
    M_MESSAGE: 'S' '000' '38' TEXT-E01 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "Validate Cancelled Document selection
  IF CB_NORML IS INITIAL AND
*     cb_cc_rd IS INITIAL AND
     CB_CNCL IS INITIAL.
    M_MESSAGE: 'S' '000' '38' TEXT-E02 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "Validate Document group selection
  IF CB_INV IS INITIAL AND  "INV-ใบแจ้งหนี้
     CB_RCT IS INITIAL AND  "RCT-ใบเสร็จรับเงิน
     CB_TIV IS INITIAL AND  "TIV-ใบกำกับภาษี
     CB_DCN IS INITIAL.     "DCN-ใบเพิ่มหนี้ /ลดหนี้
    M_MESSAGE: 'S' '000' '38' TEXT-E03 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "Validate Document status selection
*  IF cb_stat1 IS INITIAL AND  "Document Print, but not transfer yet
*     cb_stat2 IS INITIAL AND  "Transfer Complete
*     cb_stat5 IS INITIAL AND  "Transfer Complete, But Doc is reprinted
*     cb_stat3 IS INITIAL AND  "Reject Complete, to be re-Transferred
*     cb_stat4 IS INITIAL AND  "Transfer unsuccessfully, to be re-Transferred
*     cb_stat6 IS INITIAL.     "Reject Incomplete
  IF P_NEW IS INITIAL AND
     P_TF IS INITIAL.
    M_MESSAGE: 'S' '000' '38' TEXT-E04 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    "validate_data
*&---------------------------------------------------------------------*
*& Form INIT_DATA
*&---------------------------------------------------------------------*
FORM INIT_DATA.

  REFRESH: GT_HEAD,
           GT_DOC_HEADER,
           GT_DOC_ITEM,
           GT_DOC_PARTNER,
           GT_DOC_H_VAT,
           GT_DOC_H_REF,
           GT_DOC_DISCHG,
           GT_TVFKT,
           GT_T003T,
           GT_RD_DISCHG,
           GT_RD_DOC_TYPE,
           GT_CHARGE,
           GT_DOMA_ADD.

  CLEAR: GV_SAP_DOC_NO,
         GV_RD_DOC_TYPE,
         GV_KUNNR,
         GV_DIS_DETAIL.

ENDFORM.                    "init_data
*&---------------------------------------------------------------------*
*& Form GET_DOC_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DOC_HEADER .

  DATA: LT_WHERE_COND      TYPE GTTY_WHERE_COND.

  DATA: LR_MODULE_ETX      TYPE GTTY_R_MODULE_ETX,
        LR_REPLACE_FLAG    TYPE GTTY_R_REPLACE_FLAG,
        LR_REVERSE_FLAG    TYPE GTTY_R_REVERSE_FLAG,
*        lr_rd_flag         TYPE gtty_r_rd_flag,
        LR_RD_DOC_TYPE_GRP TYPE GTTY_R_RD_DOC_TYPE_GRP,
        LR_STATUS          TYPE GTTY_R_STATUS,
        LR_RD_SENT         TYPE GTTY_R_RD_FLAG.           "INS: <ETAX003>

  "Get ranges and where clause's conditions text
  PERFORM GET_RANGE CHANGING LR_MODULE_ETX
                             LR_REPLACE_FLAG
                             LR_REVERSE_FLAG
*                             lr_rd_flag
                             LR_RD_DOC_TYPE_GRP
                             LR_STATUS
                             LT_WHERE_COND
                             LR_RD_SENT.                  "INS: <ETAX003>

  "Select Document Header data
  SELECT A~BUKRS             A~SAP_DOC_NO         A~GJAHR             A~RD_DOC_TYPE
         A~MODULE_ETX        A~DOCUMENT_NO        A~SAP_POSTING_DATE  A~RD_DOC_TYPE_GRP
         B~RD_DOC_TYP_DS_TH
         A~SAP_DOC_TYPE
         C~BUTXT
         A~REVERSE_FLAG      A~RD_FLAG            A~BUPLA             A~KUNNR
         A~KUNNR_NAME        A~KUNNR_BRANCH       A~KUNNR_TAX_ID      A~REPLACE_FLAG
         A~SAP_DOC_RESN      A~SAP_DOC_RESN_DES  A~RD_DOC_RESN
         D~RD_DOC_RESN_DESC
         A~REQ_DEV_DATE      A~PAY_TERM           A~PAY_TERM_DESC
         E~VTEXT
         A~PAY_DUE_DATE      A~INCO_TERM          A~SAP_PO_NO
         A~SAP_PO_DATE       A~GLOBAL_DOC_NO      A~SAP_CURR          A~RD_CURR_CODE
         A~VAT_BASE_AMT      A~NET_AMT_BF_VAT     A~VAT_AMT           A~NET_AMT_AFT_VAT
         A~REF_DOC_AMT       A~CORRECT_AMT        A~DIFF_AMT          A~TOTAL_DISC_AMT
         A~TOTAL_CHARGE_AMT  A~STATUS
*>>> BEGIN OF INSERTION: <ETAX003> on 16.09.2020 <<<
         A~BUPLA_INFO        A~SUBJECT            A~CONTENT           A~DEPOSIT_FLAG
*>>> END OF INSERTION: <ETAX003> on 16.09.2020 <<<
         A~ETAX_BY           A~ETAX_DATE          A~PRINT_USER        A~PRINT_DATE "INS CH01
    INTO TABLE GT_DOC_HEADER
    FROM ZSDSFIT014 AS A
    INNER JOIN ZSDSFIC005 AS B ON A~RD_DOC_TYPE EQ B~RD_DOC_TYPE
    INNER JOIN T001 AS C ON A~BUKRS EQ C~BUKRS
    LEFT OUTER JOIN ZSDSFIC004 AS D ON A~RD_DOC_RESN  = D~RD_DOC_RESN
*    INNER JOIN tvzbt AS e ON e~spras = sy-langu
    LEFT OUTER JOIN TVZBT AS E ON E~SPRAS = SY-LANGU
                               AND A~PAY_TERM = E~ZTERM
   WHERE A~BUKRS IN S_BUKRS
     AND A~SAP_DOC_NO IN S_BELNR
     AND A~GJAHR IN S_GJAHR
     AND A~RD_DOC_TYPE IN S_RD_DTY
     AND A~MODULE_ETX IN LR_MODULE_ETX
     AND A~BUPLA IN S_BUPLA
     AND A~KUNNR IN S_KUNNR
     AND A~SAP_DOC_TYPE IN S_FKART
     AND A~RD_DOC_TYPE_GRP IN LR_RD_DOC_TYPE_GRP
     AND A~SAP_POSTING_DATE IN S_BUDAT
     AND A~REPLACE_FLAG IN LR_REPLACE_FLAG
     AND A~SAP_DOC_RESN IN S_AUGRU
     AND A~RD_DOC_RESN IN S_RD_DRS
     AND A~STATUS IN LR_STATUS
     AND A~ETAX_DATE IN S_ETX_DT
     AND A~ETAX_BY IN S_ETX_BY
     AND A~PRINT_DATE IN S_PNT_DT                           "INS CH01
     AND A~PRINT_USER IN S_PNT_BY                           "INS CH01
     AND A~REVERSE_FLAG IN LR_REVERSE_FLAG  "INS: <ETAX003>
     AND A~RD_FLAG IN LR_RD_SENT            "INS: <ETAX003>
     AND (LT_WHERE_COND).

ENDFORM.                    "get_doc_header
*&---------------------------------------------------------------------*
*&      Form  throw_message
*&---------------------------------------------------------------------*
FORM THROW_MESSAGE USING P_MSGTY TYPE SY-MSGTY
                         P_MSGNO TYPE SY-MSGNO
                         P_MSGID TYPE SY-MSGID
                         P_MSGTX
                         P_DISPL TYPE C.

  MESSAGE ID P_MSGID TYPE P_MSGTY NUMBER P_MSGNO
        WITH P_MSGTX DISPLAY LIKE P_DISPL.

ENDFORM.                    "throw_message
*&---------------------------------------------------------------------*
*& Form GET_DESC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DESC .

  DATA: LWA_DOC_HEADER TYPE GTY_DOC_HEADER.

  DATA: LR_FKART TYPE RANGE OF TVFKT-FKART,
        LR_BLART TYPE RANGE OF T003T-BLART.

  FIELD-SYMBOLS: <LFS_FKART> LIKE LINE OF LR_FKART,
                 <LFS_BLART> LIKE LINE OF LR_BLART.

  LOOP AT GT_DOC_HEADER INTO LWA_DOC_HEADER.

    "Append range of SD doc type
    IF LWA_DOC_HEADER-MODULE_ETX EQ 'SD' AND
       LWA_DOC_HEADER-SAP_DOC_TYPE IS NOT INITIAL.
      APPEND INITIAL LINE TO LR_FKART ASSIGNING <LFS_FKART>.
      <LFS_FKART>     = 'IEQ'.
      <LFS_FKART>-LOW = LWA_DOC_HEADER-SAP_DOC_TYPE.
    ENDIF.

    "Append range of FI doc type
    IF LWA_DOC_HEADER-MODULE_ETX EQ 'FI' AND
       LWA_DOC_HEADER-SAP_DOC_TYPE IS NOT INITIAL.
      APPEND INITIAL LINE TO LR_BLART ASSIGNING <LFS_BLART>.
      <LFS_BLART>     = 'IEQ'.
      <LFS_BLART>-LOW = LWA_DOC_HEADER-SAP_DOC_TYPE.
    ENDIF.

  ENDLOOP.

  IF LR_FKART[] IS NOT INITIAL.
*   SAP Doc type (SD) FKART
    SELECT FKART VTEXT
      FROM TVFKT
      INTO TABLE GT_TVFKT
     WHERE FKART IN LR_FKART
       AND SPRAS EQ SY-LANGU.
  ENDIF.

  IF LR_BLART[] IS NOT INITIAL.
*   SAP Doc type (FI) BLART
    SELECT BLART LTEXT
      FROM T003T
      INTO TABLE GT_T003T
      WHERE BLART IN LR_BLART
        AND SPRAS EQ SY-LANGU.
  ENDIF.

  IF GT_DOC_HEADER[] IS NOT INITIAL.

    "เก็บประเภทรหัสส่วนลดค่าธรรมเนียมของสรรพากร
    SELECT RD_DISCHG RD_DISCHG_DESC_T
      INTO TABLE GT_RD_DISCHG
      FROM ZSDSFIC016
      FOR ALL ENTRIES IN GT_DOC_DISCHG
     WHERE RD_DISCHG = GT_DOC_DISCHG-RD_DISCHG.

  ENDIF.

  IF GT_DOC_H_REF[] IS NOT INITIAL.

    "เก็บประเภทรหัสเอกสารของสรรพากร
    SELECT RD_DOC_TYPE RD_DOC_TYP_DS_TH
      INTO TABLE GT_RD_DOC_TYPE
      FROM ZSDSFIC005
      FOR ALL ENTRIES IN GT_DOC_H_REF
     WHERE RD_DOC_TYPE = GT_DOC_H_REF-REF_RD_DOC_TYPE.

  ENDIF.

  PERFORM GET_DOMA_ADD.

ENDFORM.                    "get_desc
*&---------------------------------------------------------------------*
*& Form GET_OTH_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_OTH_DATA .

  DATA: LT_DOC_HEADER TYPE STANDARD TABLE OF GTY_DOC_HEADER.

  LT_DOC_HEADER[] = GT_DOC_HEADER[].
  DELETE LT_DOC_HEADER WHERE SAP_DOC_NO IS INITIAL.

  IF LT_DOC_HEADER[] IS NOT INITIAL.

    "เก็บข้อมูลเอกสาร-ระดับ Item
    SELECT *
      FROM ZSDSFIT015
      INTO TABLE GT_DOC_ITEM
      FOR ALL ENTRIES IN LT_DOC_HEADER
     WHERE BUKRS      EQ LT_DOC_HEADER-BUKRS
       AND SAP_DOC_NO EQ LT_DOC_HEADER-SAP_DOC_NO
       AND GJAHR      EQ LT_DOC_HEADER-GJAHR
       AND RD_DOC_TYPE EQ LT_DOC_HEADER-RD_DOC_TYPE
       AND MODULE_ETX EQ LT_DOC_HEADER-MODULE_ETX.

    "เก็บข้อมูลเอกสาร-ระดับ Partner
    SELECT *
      FROM ZSDSFIT018
      INTO TABLE GT_DOC_PARTNER
      FOR ALL ENTRIES IN LT_DOC_HEADER
     WHERE BUKRS      EQ LT_DOC_HEADER-BUKRS
       AND SAP_DOC_NO EQ LT_DOC_HEADER-SAP_DOC_NO
       AND GJAHR      EQ LT_DOC_HEADER-GJAHR
       AND RD_DOC_TYPE EQ LT_DOC_HEADER-RD_DOC_TYPE
       AND MODULE_ETX EQ LT_DOC_HEADER-MODULE_ETX.

    "เก็บข้อมูลเอกสาร – VAT ระดับ Header
    SELECT *
      INTO TABLE GT_DOC_H_VAT
      FROM ZSDSFIT017
      FOR ALL ENTRIES IN LT_DOC_HEADER
     WHERE BUKRS = LT_DOC_HEADER-BUKRS
       AND SAP_DOC_NO = LT_DOC_HEADER-SAP_DOC_NO
       AND GJAHR = LT_DOC_HEADER-GJAHR
       AND RD_DOC_TYPE EQ LT_DOC_HEADER-RD_DOC_TYPE
       AND MODULE_ETX EQ LT_DOC_HEADER-MODULE_ETX.

    "เก็บข้อมูลเอกสาร – อ้างอิง ระดับ Header
    SELECT *
     FROM ZSDSFIT016
      INTO TABLE GT_DOC_H_REF
     FOR ALL ENTRIES IN LT_DOC_HEADER
    WHERE BUKRS      EQ LT_DOC_HEADER-BUKRS
      AND SAP_DOC_NO EQ LT_DOC_HEADER-SAP_DOC_NO
      AND GJAHR      EQ LT_DOC_HEADER-GJAHR
      AND RD_DOC_TYPE EQ LT_DOC_HEADER-RD_DOC_TYPE
      AND MODULE_ETX EQ LT_DOC_HEADER-MODULE_ETX.

    "เก็บข้อมูลเอกสาร-ระดับ disc Charge
    SELECT *
      FROM ZSDSFIT013
      INTO TABLE GT_DOC_DISCHG
      FOR ALL ENTRIES IN LT_DOC_HEADER
     WHERE BUKRS      EQ LT_DOC_HEADER-BUKRS
       AND SAP_DOC_NO EQ LT_DOC_HEADER-SAP_DOC_NO
       AND GJAHR      EQ LT_DOC_HEADER-GJAHR
       AND RD_DOC_TYPE EQ LT_DOC_HEADER-RD_DOC_TYPE
       AND MODULE_ETX EQ LT_DOC_HEADER-MODULE_ETX.

  ENDIF.

ENDFORM.                    "get_oth_data
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESS_DATA .

  DATA: LWA_DOC_HEADER TYPE GTY_DOC_HEADER.

  FIELD-SYMBOLS: <LFS_HEAD> LIKE LINE OF GT_HEAD.

  LOOP AT GT_DOC_HEADER INTO LWA_DOC_HEADER.

    APPEND INITIAL LINE TO GT_HEAD ASSIGNING <LFS_HEAD>.

    PERFORM POPULATE_DOC_HEADER USING LWA_DOC_HEADER
                               CHANGING <LFS_HEAD>.

    PERFORM GET_PARTNER    USING LWA_DOC_HEADER-SAP_DOC_NO
                                 LWA_DOC_HEADER-BUKRS
                                 LWA_DOC_HEADER-GJAHR
                                 LWA_DOC_HEADER-RD_DOC_TYPE
                                 LWA_DOC_HEADER-MODULE_ETX
                                 <LFS_HEAD>
                        CHANGING <LFS_HEAD>-IT_PARTNER.

    PERFORM GET_H_REF USING LWA_DOC_HEADER-SAP_DOC_NO
                            LWA_DOC_HEADER-BUKRS
                            LWA_DOC_HEADER-GJAHR
                            LWA_DOC_HEADER-RD_DOC_TYPE
                            LWA_DOC_HEADER-MODULE_ETX
                            <LFS_HEAD>
                   CHANGING <LFS_HEAD>-IT_H_REF.

    PERFORM GET_DISC_AMT USING LWA_DOC_HEADER-SAP_DOC_NO
                               LWA_DOC_HEADER-BUKRS
                               LWA_DOC_HEADER-GJAHR
                               LWA_DOC_HEADER-RD_DOC_TYPE
                               LWA_DOC_HEADER-MODULE_ETX
                               <LFS_HEAD>
                      CHANGING <LFS_HEAD>-LINK_DISC_CHARGE
                               <LFS_HEAD>-IT_CHARGE.

    PERFORM GET_DOC_ITEM       USING LWA_DOC_HEADER-SAP_DOC_NO
                                     LWA_DOC_HEADER-BUKRS
                                     LWA_DOC_HEADER-GJAHR
                                     LWA_DOC_HEADER-RD_DOC_TYPE
                                     LWA_DOC_HEADER-MODULE_ETX
                                     <LFS_HEAD>
                            CHANGING <LFS_HEAD>-IT_ITEM.

  ENDLOOP.

ENDFORM.                    "process_data
*&---------------------------------------------------------------------*
*& Form POPULATE_DOC_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DOC_HEADER
*&      <-- <LFS_HEAD>
*&---------------------------------------------------------------------*
FORM POPULATE_DOC_HEADER  USING    PI_DOC_HEADER TYPE GTY_DOC_HEADER
                          CHANGING PO_HEAD TYPE GTY_HEAD.

  DATA: LWA_DOMA       TYPE DD07V.

  PO_HEAD-SAP_DOC_NO  = PI_DOC_HEADER-SAP_DOC_NO.
  PO_HEAD-RD_DOC_TYPE = PI_DOC_HEADER-RD_DOC_TYPE.
  PO_HEAD-MODULE_ETX = PI_DOC_HEADER-MODULE_ETX.
  PO_HEAD-DOCUMENT_NO = PI_DOC_HEADER-DOCUMENT_NO.
  PO_HEAD-SAP_POSTING_DATE = PI_DOC_HEADER-SAP_POSTING_DATE.
  PO_HEAD-RD_DOC_TYPE_GRP = PI_DOC_HEADER-RD_DOC_TYPE_GRP.
  PO_HEAD-RD_DOC_TYPE_DESC = PI_DOC_HEADER-RD_DOC_TYPE_DESC_TH.
  PO_HEAD-SAP_DOC_TYPE = PI_DOC_HEADER-SAP_DOC_TYPE.

  "SAP Doc Type Desc
  PERFORM READ_SAP_DOC_TYP_DES USING PI_DOC_HEADER-MODULE_ETX
                                     PI_DOC_HEADER-SAP_DOC_TYPE
                            CHANGING PO_HEAD-SAP_DOC_TYPE_DESC.

  "Cancel
*>>> BEGIN OF MODIFICATION: <ETAX003> on 16.09.2020 <<<
  IF PI_DOC_HEADER-REVERSE_FLAG EQ 'N'.
    CLEAR PO_HEAD-CANCEL.
  ELSEIF PI_DOC_HEADER-REVERSE_FLAG EQ 'Y' AND
*  IF pi_doc_header-reverse_flag EQ 'Y' AND
*>>> END OF MODIFICATION: <ETAX003> on 16.09.2020 <<<
     PI_DOC_HEADER-RD_FLAG EQ 'N'.
    PO_HEAD-CANCEL = TEXT-T03.
  ELSEIF PI_DOC_HEADER-REVERSE_FLAG EQ 'Y'.
    PO_HEAD-CANCEL = TEXT-T01.
  ELSEIF PI_DOC_HEADER-RD_FLAG EQ 'N'.
    PO_HEAD-CANCEL = TEXT-T02.
  ELSE.
    CLEAR PO_HEAD-CANCEL.
  ENDIF.

  PO_HEAD-BUKRS       = PI_DOC_HEADER-BUKRS.
  PO_HEAD-COMP_NAME   = PI_DOC_HEADER-BUTXT.
  PO_HEAD-GJAHR       = PI_DOC_HEADER-GJAHR.
  PO_HEAD-BUPLA       = PI_DOC_HEADER-BUPLA.
  PO_HEAD-KUNNR       = PI_DOC_HEADER-KUNNR.
  PO_HEAD-KUNNR_NAME  = PI_DOC_HEADER-KUNNR_NAME.
  PO_HEAD-KUNNR_BRANCH = PI_DOC_HEADER-KUNNR_BRANCH.
  PO_HEAD-KUNNR_TAX_ID = PI_DOC_HEADER-KUNNR_TAX_ID.
  PO_HEAD-LINK_PARTNER = GC_DETAIL.

  IF PI_DOC_HEADER-REPLACE_FLAG EQ 'Y'.
    PO_HEAD-REPLACE_DOC = GC_YES.
  ENDIF.

  PO_HEAD-SAP_DOC_RESN      = PI_DOC_HEADER-SAP_DOC_RESN.
  PO_HEAD-SAP_DOC_RESN_DES  = PI_DOC_HEADER-SAP_DOC_RESN_DES.
  PO_HEAD-RD_DOC_RESN       = PI_DOC_HEADER-RD_DOC_RESN.
  PO_HEAD-RD_DOC_RESN_DESC  = PI_DOC_HEADER-RD_DOC_RESN_DESC.

  IF PI_DOC_HEADER-REPLACE_FLAG EQ 'Y' OR
       ( PO_HEAD-RD_DOC_TYPE EQ '80' OR PO_HEAD-RD_DOC_TYPE EQ '81' ).
    PO_HEAD-LINK_REF_DOC = GC_DETAIL.
  ENDIF.

  PO_HEAD-REQ_DEV_DATE = PI_DOC_HEADER-REQ_DEV_DATE.
  PO_HEAD-PAY_TERM = PI_DOC_HEADER-PAY_TERM.
  PO_HEAD-PAY_TERM_DESC = PI_DOC_HEADER-PAY_TERM_DESC.

*  PERFORM read_pay_term_des USING po_head-pay_term
*                         CHANGING po_head-pay_term_desc.

  PO_HEAD-PAY_DUE_DATE = PI_DOC_HEADER-PAY_DUE_DATE.
  PO_HEAD-INCO_TERM = PI_DOC_HEADER-INCO_TERM.
  PO_HEAD-SAP_PO_NO = PI_DOC_HEADER-SAP_PO_NO.
  PO_HEAD-SAP_PO_DATE = PI_DOC_HEADER-SAP_PO_DATE.
  PO_HEAD-GLOBAL_DOC_NO = PI_DOC_HEADER-GLOBAL_DOC_NO.
  PO_HEAD-SAP_CURR = PI_DOC_HEADER-SAP_CURR.
  PO_HEAD-RD_CURR_CODE = PI_DOC_HEADER-RD_CURR_CODE.

  PERFORM READ_H_VAT   USING  PI_DOC_HEADER-SAP_DOC_NO
                              PI_DOC_HEADER-BUKRS
                              PI_DOC_HEADER-GJAHR
                              PI_DOC_HEADER-RD_DOC_TYPE
                              PI_DOC_HEADER-MODULE_ETX
                              PO_HEAD
                     CHANGING PO_HEAD-RD_VAT_TYPE
                              PO_HEAD-TAX_CODE
                              PO_HEAD-VAT_RATE
                              PO_HEAD-GROSS_AMT
                              PO_HEAD-IT_H_VAT.

  PO_HEAD-TOTAL_DISC_AMT = PI_DOC_HEADER-TOTAL_DISC_AMT.
  PO_HEAD-TOTAL_CHARGE_AMT = PI_DOC_HEADER-TOTAL_CHARGE_AMT.

  PO_HEAD-VAT_BASE_AMT = PI_DOC_HEADER-VAT_BASE_AMT.
  PO_HEAD-NET_AMT_BF_VAT = PI_DOC_HEADER-NET_AMT_BF_VAT.
  PO_HEAD-VAT_AMT = PI_DOC_HEADER-VAT_AMT.
  PO_HEAD-NET_AMT_AFT_VAT = PI_DOC_HEADER-NET_AMT_AFT_VAT.
  PO_HEAD-LINK_VAT        = GC_DETAIL.
  PO_HEAD-REF_DOC_AMT = PI_DOC_HEADER-REF_DOC_AMT.
  PO_HEAD-CORRECT_AMT = PI_DOC_HEADER-CORRECT_AMT.
  PO_HEAD-DIFF_AMT = PI_DOC_HEADER-DIFF_AMT.
  PO_HEAD-STATUS   = PI_DOC_HEADER-STATUS.

  READ TABLE GT_DOMA_ADD INTO LWA_DOMA
    WITH KEY DOMVALUE_L = PO_HEAD-STATUS.
  IF SY-SUBRC EQ 0.
    PO_HEAD-STATUS_DESC = LWA_DOMA-DDTEXT.
  ENDIF.

*>>> BEGIN OF INSERTION: <ETAX003> on 16.09.2020 <<<
  PO_HEAD-BUPLA_INFO = PI_DOC_HEADER-BUPLA_INFO.
  PO_HEAD-SUBJECT    = PI_DOC_HEADER-SUBJECT.
  PO_HEAD-CONTENT    = PI_DOC_HEADER-CONTENT.
  PO_HEAD-RD_FLAG    = PI_DOC_HEADER-RD_FLAG.
  PO_HEAD-DEPOSIT_FLAG = PI_DOC_HEADER-DEPOSIT_FLAG.
*>>> END OF INSERTION: <ETAX003> on 16.09.2020 <<<

*>>> BOI CH01 >>>
  PO_HEAD-ETAX_BY     = PI_DOC_HEADER-ETAX_BY.
  PO_HEAD-ETAX_DATE   = PI_DOC_HEADER-ETAX_DATE.
  PO_HEAD-PRINT_USER  = PI_DOC_HEADER-PRINT_USER.
  PO_HEAD-PRINT_DATE  = PI_DOC_HEADER-PRINT_DATE.

  IF PI_DOC_HEADER-RD_FLAG = 'Y'.
    PO_HEAD-VAT_REPORT = 'Y'.
  ELSEIF PI_DOC_HEADER-RD_FLAG = 'N' AND PI_DOC_HEADER-DEPOSIT_FLAG = 'Y'.
    IF PO_HEAD-RD_DOC_TYPE = 'T01'.
      PO_HEAD-VAT_REPORT = 'N'. "CH03 by navapat 19.11.2020
    ELSE.
      PO_HEAD-VAT_REPORT = 'Y'.
    ENDIF.
  ELSE.
    PO_HEAD-VAT_REPORT = 'N'.
  ENDIF.
*<<< EOI CH01 <<<

ENDFORM.                    "populate_doc_header
*&---------------------------------------------------------------------*
*& Form READ_SAP_DOC_TYP_DES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_DOC_HEADER_MODULE_ETX
*&      <-- PO_HEAD_SAP_DOC_TYPE_DESC
*&---------------------------------------------------------------------*
FORM READ_SAP_DOC_TYP_DES  USING    PI_MODULE_ETX TYPE GTY_DOC_HEADER-MODULE_ETX
                                    PI_SAP_DOC_TYPE TYPE GTY_DOC_HEADER-SAP_DOC_TYPE
                           CHANGING PO_DESC TYPE GTY_HEAD-SAP_DOC_TYPE_DESC.

  DATA: LWA_TVFKT TYPE GTY_TVFKT,
        LWA_T003T TYPE GTY_T003T.

  CLEAR PO_DESC.

  CASE PI_MODULE_ETX.
    WHEN 'SD'.
      READ TABLE GT_TVFKT INTO LWA_TVFKT
        WITH TABLE KEY FKART = PI_SAP_DOC_TYPE.
      IF SY-SUBRC EQ 0.
        PO_DESC = LWA_TVFKT-VTEXT.
      ENDIF.
    WHEN 'FI'.
      READ TABLE GT_T003T INTO LWA_T003T
        WITH TABLE KEY BLART = PI_SAP_DOC_TYPE.
      IF SY-SUBRC EQ 0.
        PO_DESC = LWA_T003T-LTEXT.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "read_sap_doc_typ_des
*&---------------------------------------------------------------------*
*& Form READ_PAY_TERM_DES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PO_HEAD_PAY_TERM
*&      <-- PO_HEAD_PAY_TERM_DES
*&---------------------------------------------------------------------*
*FORM read_pay_term_des  USING    pi_pay_term TYPE ZSDSFIT014-pay_term
*                        CHANGING po_pay_term_desc TYPE ZSDSFIT014-pay_term_desc.
*
*  DATA: lwa_tvzbt TYPE gty_tvzbt.
*
*  READ TABLE gt_tvzbt INTO lwa_tvzbt WITH TABLE KEY zterm = pi_pay_term.
*  IF sy-subrc EQ 0.
*    po_pay_term_desc = lwa_tvzbt-vtext.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_H_VAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_DOC_HEADER_SAP_DOC_NO
*&      --> PI_DOC_HEADER_BUKRS
*&      --> PI_DOC_HEADER_GJAHR
*&      --> PO_HEAD
*&      <-- PO_HEAD_TAX_CODE
*&      <-- PO_HEAD_VAT_RATE
*&      <-- PO_HEAD_GROSS_AMT
*&      <-- PO_HEAD_IT_H_VAT
*&---------------------------------------------------------------------*
FORM READ_H_VAT  USING    PI_SAP_DOC_NO  TYPE ZSDSFIT014-SAP_DOC_NO
                          PI_BUKRS       TYPE ZSDSFIT014-BUKRS
                          PI_GJAHR       TYPE ZSDSFIT014-GJAHR
                          PI_RD_DOC_TYPE TYPE ZSDSFIT014-RD_DOC_TYPE
                          PI_MODULE_ETX  TYPE ZSDSFIT014-MODULE_ETX
                          PI_HEAD        TYPE GTY_HEAD
                 CHANGING PO_RD_VAT_TYPE TYPE ZSDSFIT017-RD_VAT_TYPE
                          PO_TAX_CODE    TYPE ZSDSFIT017-TAX_CODE
                          PO_VAT_RATE    TYPE ZSDSFIT017-VAT_RATE
                          PO_GROSS_AMT   TYPE ZSDSFIT017-NET_AMT_BF_VAT
                          PIT_H_VAT      TYPE GTTY_DOC_H_VAT.

  TYPES: BEGIN OF LTY_SUM_VAT,
           TAX_CODE       TYPE ZSDSFIT017-TAX_CODE,
           RD_VAT_TYPE    TYPE ZSDSFIT017-RD_VAT_TYPE,
           VAT_RATE_TXT   TYPE C LENGTH 21,
           NET_AMT_BF_VAT TYPE ZSDSFIT017-NET_AMT_BF_VAT,
         END OF LTY_SUM_VAT.

  DATA LT_SUM_VAT TYPE STANDARD TABLE OF LTY_SUM_VAT.

  DATA: LWA_DOC_H_VAT TYPE GTY_DOC_H_VAT,
        LWA_SUM_VAT   LIKE LINE OF LT_SUM_VAT.

  LOOP AT GT_DOC_H_VAT INTO LWA_DOC_H_VAT
    WHERE BUKRS = PI_BUKRS
      AND SAP_DOC_NO = PI_SAP_DOC_NO
      AND GJAHR = PI_GJAHR
      AND RD_DOC_TYPE = PI_RD_DOC_TYPE
      AND MODULE_ETX = PI_MODULE_ETX.

    CLEAR LWA_SUM_VAT.
    LWA_SUM_VAT-TAX_CODE       = LWA_DOC_H_VAT-TAX_CODE.
    LWA_SUM_VAT-RD_VAT_TYPE    = LWA_DOC_H_VAT-RD_VAT_TYPE.
    LWA_SUM_VAT-NET_AMT_BF_VAT = LWA_DOC_H_VAT-NET_AMT_BF_VAT.
    LWA_SUM_VAT-VAT_RATE_TXT   = LWA_DOC_H_VAT-VAT_RATE.
    CONDENSE LWA_SUM_VAT-VAT_RATE_TXT NO-GAPS.
    COLLECT LWA_SUM_VAT INTO LT_SUM_VAT.

    "Append to deep internal table IT_H_VAT
    LWA_DOC_H_VAT-DOCUMENT_NO      = PI_HEAD-DOCUMENT_NO.
*    lwa_doc_h_vat-rd_doc_type      = pi_head-rd_doc_type.
    LWA_DOC_H_VAT-RD_DOC_TYPE_DESC = PI_HEAD-RD_DOC_TYPE_DESC.
    LWA_DOC_H_VAT-KUNNR            = PI_HEAD-KUNNR.
    LWA_DOC_H_VAT-KUNNR_NAME       = PI_HEAD-KUNNR_NAME.
    APPEND LWA_DOC_H_VAT TO PIT_H_VAT.

  ENDLOOP.

  CLEAR LWA_SUM_VAT.
  READ TABLE LT_SUM_VAT INTO LWA_SUM_VAT INDEX 1.
  IF SY-SUBRC = 0.
    PO_RD_VAT_TYPE = LWA_SUM_VAT-RD_VAT_TYPE.
    PO_TAX_CODE  = LWA_SUM_VAT-TAX_CODE.
    PO_VAT_RATE  = LWA_SUM_VAT-VAT_RATE_TXT.
    PO_GROSS_AMT = LWA_SUM_VAT-NET_AMT_BF_VAT.
  ENDIF.

ENDFORM.                    "read_h_vat
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
FORM GET_DISC_AMT  USING    PI_SAP_DOC_NO TYPE ZSDSFIT014-SAP_DOC_NO
                            PI_BUKRS      TYPE VBRK-BUKRS
                            PI_GJAHR      TYPE VBRK-GJAHR
                            PI_RD_DOC_TYPE TYPE ZSDSFIT014-RD_DOC_TYPE
                            PI_MODULE_ETX TYPE ZSDSFIT014-MODULE_ETX
                            PI_HEAD        TYPE GTY_HEAD
                   CHANGING PO_LINK_DISC_CHARGE
                            POT_CHARGE     TYPE GTTY_DOC_DISCHG.

  DATA: LWA_DOC_DISCHG TYPE GTY_DOC_DISCHG,
        LWA_DOC_ITEM   TYPE GTY_DOC_ITEM,
        LWA_RD_DISCHG  TYPE GTY_RD_DISCHG.

  DATA: LV_CHARGE_AMT TYPE ZSDSDE_sap_charge_amt,
        LV_FOUND      TYPE FLAG.

  LOOP AT GT_DOC_DISCHG INTO LWA_DOC_DISCHG
    WHERE BUKRS      = PI_BUKRS
      AND SAP_DOC_NO = PI_SAP_DOC_NO
      AND GJAHR      = PI_GJAHR
      AND RD_DOC_TYPE = PI_RD_DOC_TYPE
      AND MODULE_ETX = PI_MODULE_ETX.

    LV_FOUND = 'X'.

    LWA_DOC_DISCHG-DOCUMENT_NO      = PI_HEAD-DOCUMENT_NO.
    LWA_DOC_DISCHG-RD_DOC_TYPE_DESC = PI_HEAD-RD_DOC_TYPE_DESC.
    LWA_DOC_DISCHG-KUNNR            = PI_HEAD-KUNNR.
    LWA_DOC_DISCHG-KUNNR_NAME       = PI_HEAD-KUNNR_NAME.
    LWA_DOC_DISCHG-SAP_CURR         = PI_HEAD-SAP_CURR.

    CLEAR LV_CHARGE_AMT.
    LV_CHARGE_AMT = LWA_DOC_DISCHG-CHARGE_AMT.

    IF LWA_DOC_DISCHG-LINE_TYPE = 'DC'.

      CLEAR LWA_DOC_ITEM.
      READ TABLE GT_DOC_ITEM INTO LWA_DOC_ITEM
        WITH KEY BUKRS      = LWA_DOC_DISCHG-BUKRS
                 SAP_DOC_NO = LWA_DOC_DISCHG-SAP_DOC_NO
                 GJAHR      = LWA_DOC_DISCHG-GJAHR
                 ITEM_NO    = LWA_DOC_DISCHG-ITEM_NO.
      IF SY-SUBRC EQ 0.
        LV_CHARGE_AMT = LWA_DOC_DISCHG-CHARGE_AMT / LWA_DOC_ITEM-QTY.
      ELSE.
        LV_CHARGE_AMT = LWA_DOC_DISCHG-CHARGE_AMT.
      ENDIF.
    ELSE.
      LV_CHARGE_AMT = LWA_DOC_DISCHG-CHARGE_AMT.
    ENDIF.

    LWA_DOC_DISCHG-CHARGE_AMT = LV_CHARGE_AMT.
    LWA_DOC_DISCHG-RD_DISCHG = LWA_DOC_DISCHG-RD_CHARGE_CODE.
*    lwa_doc_dischg-rd_dischg_desc = lwa_doc_dischg-charge_resn.

    CLEAR LWA_RD_DISCHG.
    READ TABLE GT_RD_DISCHG INTO LWA_RD_DISCHG
      WITH TABLE KEY RD_DISCHG = LWA_DOC_DISCHG-RD_CHARGE_CODE.
    IF SY-SUBRC = 0.
      LWA_DOC_DISCHG-RD_DISCHG_DESC = LWA_RD_DISCHG-RD_DISCHG_DESC_TH.
    ENDIF.

    APPEND LWA_DOC_DISCHG TO POT_CHARGE.

  ENDLOOP.

  IF LV_FOUND EQ 'X'.
    PO_LINK_DISC_CHARGE = GC_DETAIL.
  ENDIF.

ENDFORM.                    "get_disc_amt
*&---------------------------------------------------------------------*
*& Form GET_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DOC_HEADER_SAP_DOC_NO
*&      --> LWA_DOC_HEADER_BUKRS
*&      --> LWA_DOC_HEADER_GJAHR
*&      --> <LFS_HEAD>
*&      <-- <LFS_HEAD>_IT_PARTNER
*&---------------------------------------------------------------------*
FORM GET_PARTNER  USING    PI_SAP_DOC_NO TYPE ZSDSFIT014-SAP_DOC_NO
                           PI_BUKRS      TYPE VBRK-BUKRS
                           PI_GJAHR      TYPE VBRK-GJAHR
                           PI_RD_DOC_TYPE TYPE ZSDSFIT014-RD_DOC_TYPE
                           PI_MODULE_ETX TYPE ZSDSFIT014-MODULE_ETX
                           PI_HEAD       TYPE GTY_HEAD
                  CHANGING POT_PARTNER   TYPE GTTY_DOC_PARTNER.

  DATA: LWA_PARTNER_TMP TYPE GTY_DOC_PARTNER,
        LWA_PARTNER     TYPE GTY_DOC_PARTNER.

  LOOP AT GT_DOC_PARTNER INTO LWA_PARTNER_TMP
    WHERE BUKRS = PI_BUKRS
      AND SAP_DOC_NO = PI_SAP_DOC_NO
      AND GJAHR = PI_GJAHR
      AND RD_DOC_TYPE = PI_RD_DOC_TYPE
      AND MODULE_ETX = PI_MODULE_ETX.

    CLEAR LWA_PARTNER.
    LWA_PARTNER-BUKRS             = PI_BUKRS.
    LWA_PARTNER-GJAHR             = PI_GJAHR.
    LWA_PARTNER-SAP_DOC_NO        = PI_SAP_DOC_NO.
    LWA_PARTNER-RD_DOC_TYPE       = PI_HEAD-RD_DOC_TYPE.
    LWA_PARTNER-MODULE_ETX        = PI_HEAD-MODULE_ETX.
    LWA_PARTNER-DOCUMENT_NO       = PI_HEAD-DOCUMENT_NO.
    LWA_PARTNER-RD_DOC_TYPE_DESC  = PI_HEAD-RD_DOC_TYPE_DESC.
    LWA_PARTNER-RD_PARTNER        = LWA_PARTNER_TMP-RD_PARTNER.
    LWA_PARTNER-RD_PARTNER_DESC   = LWA_PARTNER_TMP-RD_PARTNER_DESC.
    LWA_PARTNER-KUNNR             = LWA_PARTNER_TMP-KUNNR.
    LWA_PARTNER-PARTNER_NAME      = LWA_PARTNER_TMP-PARTNER_NAME.
    LWA_PARTNER-PARTNER_SCH_ID    = LWA_PARTNER_TMP-PARTNER_SCH_ID.
    LWA_PARTNER-GLOBAL_ID         = LWA_PARTNER_TMP-GLOBAL_ID.
    LWA_PARTNER-TAX_ID            = LWA_PARTNER_TMP-TAX_ID.
    LWA_PARTNER-CONTACT_NAME      = LWA_PARTNER_TMP-CONTACT_NAME.
    LWA_PARTNER-CONTACT_DEPT      = LWA_PARTNER_TMP-CONTACT_DEPT.
    LWA_PARTNER-POSTAL            = LWA_PARTNER_TMP-POSTAL.
    LWA_PARTNER-HOME_NO           = LWA_PARTNER_TMP-HOME_NO.
    LWA_PARTNER-ADDR_LINE1        = LWA_PARTNER_TMP-ADDR_LINE1.
    LWA_PARTNER-ADDR_LINE2        = LWA_PARTNER_TMP-ADDR_LINE2.
    LWA_PARTNER-SUB_DIST          = LWA_PARTNER_TMP-SUB_DIST.
    LWA_PARTNER-SUB_DIST_DESC     = LWA_PARTNER_TMP-SUB_DIST_DESC.
    LWA_PARTNER-DISTRICT          = LWA_PARTNER_TMP-DISTRICT.
    LWA_PARTNER-DISTRICT_DESC     = LWA_PARTNER_TMP-DISTRICT_DESC.
    LWA_PARTNER-PROVINCE_CODE     = LWA_PARTNER_TMP-PROVINCE_CODE.
    LWA_PARTNER-PROVINCE_DESC     = LWA_PARTNER_TMP-PROVINCE_DESC.
    LWA_PARTNER-COUNTRY           = LWA_PARTNER_TMP-COUNTRY.
    LWA_PARTNER-COUNTRY_SCH_ID    = LWA_PARTNER_TMP-COUNTRY_SCH_ID.
    APPEND LWA_PARTNER TO POT_PARTNER.
  ENDLOOP.

ENDFORM.                    "get_partner
*&---------------------------------------------------------------------*
*& Form GET_MAPPING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM get_mapping .
*
* เก็บที่อยู่ของบริษัท
*  SELECT bukrs bupla home_no sub_dist_code postal sch_id
*    FROM ZSDSFIC003
*    INTO TABLE gt_company_addr
*   WHERE bukrs = p_bukrs.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_H_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DOC_HEADER_SAP_DOC_NO
*&      --> LWA_DOC_HEADER_BUKRS
*&      --> LWA_DOC_HEADER_GJAHR
*&      --> LWA_DOC_HEADER_VKORG
*&      --> LWA_DOC_HEADER_MWSK1
*&      --> <LFS_HEAD>
*&      <-- <LFS_HEAD>_IT_H_REF
*&---------------------------------------------------------------------*
FORM GET_H_REF  USING    PI_SAP_DOC_NO TYPE ZSDSFIT014-SAP_DOC_NO
                         PI_BUKRS      TYPE VBRK-BUKRS
                         PI_GJAHR      TYPE VBRK-GJAHR
                         PI_RD_DOC_TYPE TYPE ZSDSFIT014-RD_DOC_TYPE
                         PI_MODULE_ETX  TYPE ZSDSFIT014-MODULE_ETX
                         PI_HEAD        TYPE GTY_HEAD
                CHANGING POT_H_REF      TYPE GTTY_DOC_H_REF.

  DATA: LWA_DOC_H_REF   TYPE GTY_DOC_H_REF,
        LWA_RD_DOC_TYPE TYPE GTY_RD_DOC_TYPE.

  LOOP AT GT_DOC_H_REF INTO LWA_DOC_H_REF
    WHERE BUKRS = PI_BUKRS
      AND SAP_DOC_NO = PI_SAP_DOC_NO
      AND GJAHR = PI_GJAHR
      AND RD_DOC_TYPE = PI_RD_DOC_TYPE
      AND MODULE_ETX = PI_MODULE_ETX.

    LWA_DOC_H_REF-DOCUMENT_NO      = PI_HEAD-DOCUMENT_NO.
    LWA_DOC_H_REF-RD_DOC_TYPE_DESC = PI_HEAD-RD_DOC_TYPE_DESC.
    LWA_DOC_H_REF-KUNNR            = PI_HEAD-KUNNR.
    LWA_DOC_H_REF-KUNNR_NAME       = PI_HEAD-KUNNR_NAME.

    CLEAR LWA_RD_DOC_TYPE.
    READ TABLE GT_RD_DOC_TYPE INTO LWA_RD_DOC_TYPE
     WITH TABLE KEY RD_DOC_TYPE = LWA_DOC_H_REF-REF_RD_DOC_TYPE.
    IF SY-SUBRC = 0.
      LWA_DOC_H_REF-REF_RD_DOC_TYPE_DESC = LWA_RD_DOC_TYPE-RD_DOC_TYPE_DESC_TH.
    ENDIF.

    APPEND LWA_DOC_H_REF TO POT_H_REF.
  ENDLOOP.

ENDFORM.                    "get_h_ref
*&---------------------------------------------------------------------*
*& Form GET_DOC_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DOC_HEADER_SAP_DOC_NO
*&      --> LWA_DOC_HEADER_BUKRS
*&      --> LWA_DOC_HEADER_GJAHR
*&      --> <LFS_HEAD>
*&      <-- <LFS_HEAD>_IT_ITEM
*&---------------------------------------------------------------------*
FORM GET_DOC_ITEM  USING    PI_SAP_DOC_NO TYPE ZSDSFIT014-SAP_DOC_NO
                            PI_BUKRS      TYPE VBRK-BUKRS
                            PI_GJAHR      TYPE VBRK-GJAHR
                            PI_RD_DOC_TYPE TYPE ZSDSFIT014-RD_DOC_TYPE
                            PI_MODULE_ETX TYPE ZSDSFIT014-MODULE_ETX
                            PI_HEAD        TYPE GTY_HEAD
                   CHANGING POT_ITEM       TYPE GTTY_DOC_ITEM.

  DATA: LWA_DOC_ITEM     TYPE GTY_DOC_ITEM.

  CLEAR LWA_DOC_ITEM.
  LOOP AT GT_DOC_ITEM INTO LWA_DOC_ITEM
    WHERE BUKRS      = PI_BUKRS
      AND SAP_DOC_NO = PI_SAP_DOC_NO
      AND GJAHR      = PI_GJAHR
      AND RD_DOC_TYPE = PI_RD_DOC_TYPE
      AND MODULE_ETX = PI_MODULE_ETX.

    LWA_DOC_ITEM-DOCUMENT_NO      = PI_HEAD-DOCUMENT_NO.
    LWA_DOC_ITEM-RD_DOC_TYPE_DESC = PI_HEAD-RD_DOC_TYPE_DESC.
    LWA_DOC_ITEM-KUNNR            = PI_HEAD-KUNNR.
    LWA_DOC_ITEM-KUNNR_NAME       = PI_HEAD-KUNNR_NAME.
    LWA_DOC_ITEM-LINK_DISC_CHARGE = PI_HEAD-LINK_DISC_CHARGE.

    APPEND LWA_DOC_ITEM TO POT_ITEM.
  ENDLOOP.

ENDFORM.                    "get_doc_item
*&---------------------------------------------------------------------*
*& Form GET_RANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MODULE_ETX
*&      <-- LR_REPLACE_FLAG
*&      <-- LR_REVERSE_FLAG
*&      <-- LR_RD_FLAG
*&      <-- LR_RD_DOC_TYPE_GRP
*&      <-- LR_STATUS
*&---------------------------------------------------------------------*
FORM GET_RANGE  CHANGING POT_R_MODULE_ETX TYPE GTTY_R_MODULE_ETX
                         POT_R_REPLACE_FLAG TYPE GTTY_R_REPLACE_FLAG
                         POT_R_REVERSE_FLAG TYPE GTTY_R_REVERSE_FLAG
*                         pot_r_rd_flag TYPE gtty_r_rd_flag
                         POT_R_RD_DOC_TYPE_GRP TYPE GTTY_R_RD_DOC_TYPE_GRP
                         POT_R_STATUS TYPE GTTY_R_STATUS
                         POT_WHERE_COND TYPE GTTY_WHERE_COND
                         POT_R_RD_SENT  TYPE GTTY_R_RD_FLAG.           "INS: <ETAX003>

  DATA: LWA_R_MODULE_ETX      LIKE LINE OF POT_R_MODULE_ETX,
        LWA_R_REPLACE_FLAG    LIKE LINE OF POT_R_REPLACE_FLAG,
        LWA_R_RD_DOC_TYPE_GRP LIKE LINE OF POT_R_RD_DOC_TYPE_GRP,
        LWA_R_STATUS          LIKE LINE OF POT_R_STATUS,
        LWA_R_RD_SENT         LIKE LINE OF POT_R_RD_SENT,              "INS: <ETAX003>
        LWA_R_REVERSE_FLAG    LIKE LINE OF POT_R_REVERSE_FLAG.         "INS: <ETAX003>

  "Prepare range of module
  IF CB_SD EQ 'X'.
    APPEND_RANGE POT_R_MODULE_ETX LWA_R_MODULE_ETX 'SD'.
  ENDIF.

  IF CB_FI EQ 'X'.
    APPEND_RANGE POT_R_MODULE_ETX LWA_R_MODULE_ETX 'FI'.
  ENDIF.

  "Prepare range of Replacement/Non Replacement flag
  IF CB_N_RPL EQ 'X'.
    APPEND_RANGE POT_R_REPLACE_FLAG LWA_R_REPLACE_FLAG 'N'.
    APPEND_RANGE POT_R_REPLACE_FLAG LWA_R_REPLACE_FLAG ABAP_FALSE. "INS CH02
  ENDIF.

  IF CB_RPL EQ 'X'.
    APPEND_RANGE POT_R_REPLACE_FLAG LWA_R_REPLACE_FLAG 'Y'.
  ENDIF.

  "Prepare range of Reverse flag and RD flag
  IF CB_NORML EQ 'X'.   "Normal Document
*    PERFORM append_where_cond USING 'A~REVERSE_FLAG = ''N'''
*                           CHANGING pot_where_cond.
*  ELSE.
*    PERFORM append_where_cond USING 'A~REVERSE_FLAG = ''Y'''
*                           CHANGING pot_where_cond.
    APPEND_RANGE POT_R_REVERSE_FLAG LWA_R_REVERSE_FLAG 'N'.
  ENDIF.

*  IF cb_cc_rd EQ 'X'.   "Cancelled Document - Submit to RD
*    PERFORM append_where_cond USING '( A~REVERSE_FLAG = ''Y'' AND A~RD_FLAG = ''Y'' )'
*                           CHANGING pot_where_cond.
*  ENDIF.

  IF CB_CNCL EQ 'X'.    "Cancelled Document - Not submit to RD
*    PERFORM append_where_cond USING '( A~REVERSE_FLAG = ''Y'' AND A~RD_FLAG = ''N'' )'
*                           CHANGING pot_where_cond.
*    PERFORM append_where_cond USING 'A~REVERSE_FLAG = ''Y'''
*                           CHANGING pot_where_cond.
    APPEND_RANGE POT_R_REVERSE_FLAG LWA_R_REVERSE_FLAG 'Y'.
  ENDIF.

*>>> BEGIN OF INSERTION: <ETAX003> on 24.09.2020 <<<
  IF CB_RD EQ 'X'.
    APPEND_RANGE POT_R_RD_SENT LWA_R_RD_SENT 'Y'.
  ENDIF.
  IF CB_NOTRD EQ 'X'.
    APPEND_RANGE POT_R_RD_SENT LWA_R_RD_SENT 'N'.
  ENDIF.
*>>> END OF INSERTION: <ETAX003> on 24.09.2020 <<<

  "Prepare range of RD document type group
  IF CB_INV EQ 'X'.     "ใบแจ้งหนี้
    APPEND_RANGE POT_R_RD_DOC_TYPE_GRP LWA_R_RD_DOC_TYPE_GRP 'INV'.
  ENDIF.

  IF CB_RCT EQ 'X'.     "ใบเสร็จรับเงิน/ใบรับ
    APPEND_RANGE POT_R_RD_DOC_TYPE_GRP LWA_R_RD_DOC_TYPE_GRP 'RCT'.
  ENDIF.

  IF CB_TIV EQ 'X'.     "ใบกำกับภาษี
    APPEND_RANGE POT_R_RD_DOC_TYPE_GRP LWA_R_RD_DOC_TYPE_GRP 'TIV'.
  ENDIF.

  IF CB_DCN EQ 'X'.     "ใบลดหนี้/เพิ่มหนี้
    APPEND_RANGE POT_R_RD_DOC_TYPE_GRP LWA_R_RD_DOC_TYPE_GRP 'DCN'.
  ENDIF.

  "Preoare range of status
*  IF cb_stat1 EQ 'X'.   "Document Print, but not transfer yet
*    append_range pot_r_status lwa_r_status '1'.
*  ENDIF.
*
*  IF cb_stat2 EQ 'X'.   "Transfer Complete
*    append_range pot_r_status lwa_r_status '2'.
*  ENDIF.
*
*  IF cb_stat5 EQ 'X'.   "Transfer Complete, But Doc is reprinted
*    append_range pot_r_status lwa_r_status '5'.
*  ENDIF.
*
*  IF cb_stat3 EQ 'X'.   "Reject Complete, to be re-Transferred
*    append_range pot_r_status lwa_r_status '3'.
*  ENDIF.
*
*  IF cb_stat4 EQ 'X'.   "Transfer unsuccessfully, to be re-Transferred
*    append_range pot_r_status lwa_r_status '4'.
*  ENDIF.
*
*  IF cb_stat6 EQ 'X'.   "Reject Incomplete
*    append_range pot_r_status lwa_r_status '6'.
*  ENDIF.
  IF P_NEW EQ 'X'.
    APPEND_RANGE POT_R_STATUS LWA_R_STATUS '1'.
    APPEND_RANGE POT_R_STATUS LWA_R_STATUS '3'.
    APPEND_RANGE POT_R_STATUS LWA_R_STATUS '4'.
  ENDIF.

  IF P_TF EQ 'X'.
    APPEND_RANGE POT_R_STATUS LWA_R_STATUS '2'.
    APPEND_RANGE POT_R_STATUS LWA_R_STATUS '5'.
    APPEND_RANGE POT_R_STATUS LWA_R_STATUS '6'.
  ENDIF.

  ">>>>>> BEGIN OF INSERTION: <CH12> 30.11.2020 12:00:12 >>>>>>
  IF P_OBSLT EQ 'X'.
    APPEND_RANGE POT_R_STATUS LWA_R_STATUS '9'.
  ENDIF.
  "<<<<<< END OF INSERTION: <CH12> 30.11.2020 12:00:12  <<<<<<


ENDFORM.                    "get_range
*&---------------------------------------------------------------------*
*& Form APPEND_WHERE_COND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- POT_WHERE_COND
*&---------------------------------------------------------------------*
FORM APPEND_WHERE_COND  USING    PI_TEXT TYPE CHAR100
                        CHANGING POT_WHERE_COND TYPE GTTY_WHERE_COND.

  DATA: LV_WHERE_COND LIKE LINE OF POT_WHERE_COND,
        LV_LINES      TYPE I.

  IF POT_WHERE_COND[] IS INITIAL.
*    lv_where_cond = '( A~REVERSE_FLAG = ''X'' AND A~RD_FLAG = ''N'' )'.
    APPEND PI_TEXT TO POT_WHERE_COND.
  ELSE.
    DESCRIBE TABLE POT_WHERE_COND LINES LV_LINES.
    READ TABLE POT_WHERE_COND INTO LV_WHERE_COND INDEX LV_LINES.
    IF SY-SUBRC EQ 0.
      IF LV_WHERE_COND = ')'.
        DELETE POT_WHERE_COND INDEX LV_LINES.
      ELSE.
        LV_WHERE_COND = '('.
        INSERT LV_WHERE_COND INTO POT_WHERE_COND INDEX 1.
      ENDIF.
    ENDIF.
    LV_WHERE_COND = 'OR'.
    APPEND LV_WHERE_COND TO POT_WHERE_COND.
*    lv_where_cond = '( A~REVERSE_FLAG = ''X'' AND A~RD_FLAG = ''N'' )'.
    APPEND PI_TEXT TO POT_WHERE_COND.
    LV_WHERE_COND = ')'.
    APPEND LV_WHERE_COND TO POT_WHERE_COND.
  ENDIF.

ENDFORM.                    "append_where_cond
*&---------------------------------------------------------------------*
*& Form F4_VARIANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SY_REPID
*&      <-- P_LAYOUT
*&---------------------------------------------------------------------*
FORM F4_VARIANT  USING PI_REPID TYPE SY-REPID
              CHANGING PO_LAYOUT TYPE DISVARIANT-VARIANT.

  DATA: LWA_VARIANT TYPE DISVARIANT.

  LWA_VARIANT-REPORT = PI_REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = LWA_VARIANT
*     I_TABNAME_HEADER          =
*     I_TABNAME_ITEM            =
*     IT_DEFAULT_FIELDCAT       =
      I_SAVE        = 'A'
*     I_DISPLAY_VIA_GRID        = ' '
    IMPORTING
*     E_EXIT        =
      ES_VARIANT    = LWA_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.
  IF SY-SUBRC EQ 0.
    PO_LAYOUT = LWA_VARIANT-VARIANT.
  ELSE.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                                                    "f4_variant
*&---------------------------------------------------------------------*
*& Form GET_DOMA_ADD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DOMA_ADD .

  DATA : LT_DOMB_ADD TYPE TABLE OF DD07V.

  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      DOMAIN_NAME   = 'ZSDSDE_STATUS'
*     GET_STATE     = 'M'
      LANGU         = SY-LANGU
      WITHTEXT      = 'X'
    TABLES
      DD07V_TAB_A   = GT_DOMA_ADD
      DD07V_TAB_N   = LT_DOMB_ADD
    EXCEPTIONS
      ILLEGAL_VALUE = 1
      OP_FAILURE    = 2
      OTHERS        = 3.

ENDFORM.                    "get_doma_add
