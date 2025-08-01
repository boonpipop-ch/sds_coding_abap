*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0090_FORM
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  init_data_frm
*&---------------------------------------------------------------------*
FORM INIT_DATA_FRM TABLES GT_VBAK_FRM
                          GT_ITEM_FRM
                          GT_ITEM2_FRM
                          GT_ITEM_VBAP
                          GT_LINES
                          GT_KNVV_FRM
                 CHANGING LWA_HEAD_FRM
                          LWA_ATTCH_FRM
                          LWA_PREM_FRM
                          LV_TEXT_INVRMK
                          LWA_TOTAL_FRM
                          GV_LANG
                          GV_TOT_QTY
                          CV_REQ_QTY TYPE I
                          GV_ERROR_HANA TYPE CHAR255.

*  IF r_detail IS NOT INITIAL.
*    PERFORM f_get_ship_to_line.
*  ENDIF.
*  DATA : LV_MESSAGE TYPE C LENGTH 255.

  GV_ERROR_HANA = LCL_DATA=>VALIDATION( ).

  LWA_HEAD_FRM   = LWA_HEADER.
  GT_VBAK_FRM[]  = GT_VBAK[].
  LWA_ATTCH_FRM  = GWA_ATTCH.
  LWA_PREM_FRM   = GWA_PREM.
  LV_TEXT_INVRMK = GV_TXTNAME_INVRMK.
  GT_ITEM_FRM[]  = GT_ITEM1[].
  GT_ITEM2_FRM[] = GT_ITEM2[].
  LWA_TOTAL_FRM  = GWA_TOTAL.
  GT_ITEM_VBAP[] = GT_ITEM_VBAP_CHECKP[].
  GV_LANG        = P_LANGU.
  GT_LINES[]     = IT_LINES[].
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GV_QTY_ITEM WITH ''.
  GV_TOT_QTY     = GV_QTY_ITEM.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GV_REQ_QTY WITH ''.
  CV_REQ_QTY     = GV_REQ_QTY.
  GT_KNVV_FRM[]  = GT_KNVV[].
ENDFORM.                    " init_data_frm
*---------------------------------------------------------------------*
*       FORM GET_GEN_C                                                *
*---------------------------------------------------------------------*
*       Get data for zsds_gen_c                       *
*---------------------------------------------------------------------*
FORM GET_GEN_C.
  CONSTANTS : BEGIN OF LC_CON,
                RAPID TYPE C LENGTH 11 VALUE 'ZSDSSDR0090',
                PARAM TYPE C LENGTH 8  VALUE 'DISCOUNT',
              END OF LC_CON.

  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID   = LC_CON-RAPID
                                                I_PARAM   = LC_CON-PARAM
                                       CHANGING CR_RETURN = GR_KSCHL ).








*  SELECT const value
*  INTO TABLE gt_gen_c
*  FROM zsds_gen_c
*  WHERE repid EQ gc_repid.
*
*  LOOP AT gt_gen_c INTO wa_gen_c.
*    IF wa_gen_c-const(12) = 'SALES_GROUP_'.
*      IF wa_gen_c-const CA '*'.
*        r_vkgrp = 'ICP'.
*      ELSE.
*        r_vkgrp = 'IEQ'.
*      ENDIF.
*      r_vkgrp-low = wa_gen_c-const+12(3).
*      APPEND r_vkgrp.
*    ENDIF.
*    IF wa_gen_c-const(12) = 'SALES_GROUP_'.
*      IF wa_gen_c-value CA '*'.
*        r_vkbur = 'ICP'.
*      ELSE.
*        r_vkbur = 'IEQ'.
*      ENDIF.
*      r_vkbur-low = wa_gen_c-value.
*      APPEND r_vkbur.
*    ENDIF.
*  ENDLOOP.

*  zcl_sdsca_utilities=>get_gen_c_range( EXPORTING if_repid = 'ZSDSSDR0090'
*                                                  if_param = 'PLANT'
*                                        IMPORTING et_range = et_plant ).


ENDFORM.                    "get_gen_c
*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*
FORM GET_DATA.

  DATA: S_OBJNR  TYPE JEST-OBJNR.
  DATA: S_INACT  LIKE JEST-INACT.
  DATA: LV_INDEX LIKE SY-TABIX.
  DATA: S_DATE   TYPE SY-DATLO.

  DATA: LR_LIFSP TYPE RANGE OF VBEP-LIFSP.

  CLEAR : S_DATE,GW_CHECK .
  S_DATE = SY-DATLO.

  PERFORM F_CHECK_BLOCK CHANGING LR_LIFSP[].
  PERFORM F_GET_ITEM TABLES LR_LIFSP[].
  PERFORM F_GET_MARD.

  IF GT_CHECK IS NOT INITIAL.
    PERFORM F_CHECK_MOTHER.
    PERFORM F_GET_VBAK.
    PERFORM F_GET_KONV.
    PERFORM F_GET_PA0001.
    PERFORM F_GET_VBKD.
    PERFORM F_GET_PARTNER.
    PERFORM F_GET_AUFK.
    PERFORM F_GET_EMP_SALES_AREA.
    PERFORM F_GET_VKBT.
    PERFORM F_GET_PROFIT_BY_SALES_AREA.
    PERFORM F_GET_DISCOUNT_TYPE.
    PERFORM F_GET_KNVV.

*    SELECT MATNR EFDAT EFEDAT
*    INTO TABLE GT_PRODCON
*    FROM ZSDSSDT003
*    FOR ALL ENTRIES IN GT_ITEM
*    WHERE MATNR EQ GT_ITEM-MATNR
*      AND EFDAT LE S_DATE
*      AND EFEDAT GE S_DATE
*      AND CONTF NE 'X'.
*
*    SELECT MATNR EFDAT EFEDAT
*    INTO TABLE GT_DISCON
*    FROM ZSDSSDT003
*    FOR ALL ENTRIES IN GT_ITEM
*    WHERE MATNR EQ GT_ITEM-MATNR
*      AND EFDAT LE S_DATE
*      AND EFEDAT GE S_DATE
*      AND CONTF EQ 'X'.
*
*    SELECT KUNNR VTWEG VKBUR NAME2
*    INTO TABLE GT_CUSTCON
*    FROM  ZSDSSDT004.
  ENDIF.
ENDFORM.                    "GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA_HEADER
*&---------------------------------------------------------------------*
*       Prepare Header Data
*----------------------------------------------------------------------*
FORM PREPARE_DATA_HEADER.

ENDFORM.                    "prepare_data_header
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE_FORMAT
*&---------------------------------------------------------------------*
FORM CONVERT_DATE_FORMAT  USING    P_EDATU P_LANGU_NAST
                          CHANGING P_DATE_CONVERTED.

  DATA: LV_MONTH             TYPE MONTH,
        LV_DATE_CONVERTED(9) TYPE C,
        LV_DATE(2)           TYPE C,
        LV_YEAR(2)           TYPE C.
  DATA: LWA_T247 TYPE T247.

  CLEAR: LV_MONTH, LV_DATE, LV_YEAR, LV_DATE_CONVERTED.
  MOVE P_EDATU+6(2) TO LV_DATE.
  MOVE P_EDATU+2(2) TO LV_YEAR.
  MOVE P_EDATU+4(2) TO LV_MONTH.

  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      LANGU = P_LANGU_NAST
      MONTH = LV_MONTH
    IMPORTING
      T247  = LWA_T247.

  IF LWA_T247-KTX IS NOT INITIAL.
    CONCATENATE LV_DATE LWA_T247-KTX LV_YEAR INTO LV_DATE_CONVERTED SEPARATED BY '-'.
  ENDIF.

  MOVE LV_DATE_CONVERTED TO P_DATE_CONVERTED.

ENDFORM.                    " CONVERT_DATE_FORMAT
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT_HEAD  USING    P_ID
                         P_OBJECT
                         P_VBELN
                CHANGING P_VALUE.

  DATA: IT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA: WA_LINES LIKE LINE OF IT_LINES.
  DATA: V_NAME TYPE THEAD-TDNAME.

  CLEAR: P_VALUE, IT_LINES[].

  V_NAME = P_VBELN.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = V_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC = 0.
    LOOP AT IT_LINES INTO WA_LINES.
      CONCATENATE P_VALUE  WA_LINES-TDLINE INTO P_VALUE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA_ATTACHMENT
*&---------------------------------------------------------------------*
FORM PREPARE_DATA_ATTACHMENT  USING P_VBELN TYPE VBAK-VBELN
                              CHANGING GWA_ATTCH.
*
*  DATA: lwa_attch TYPE typ_attch.
*  DATA: lv_tdid   LIKE  thead-tdid,
*        lv_spras  LIKE  thead-tdspras,
*        lv_name   LIKE  thead-tdname,
*        lv_object LIKE  thead-tdobject.
*  DATA: lt_tline TYPE TABLE OF tline.
*
*  CONSTANTS: lc_spras_en TYPE spras VALUE 'E',
*             lc_spras_th TYPE spras VALUE '2'.
*  CLEAR: lv_tdid, lv_spras, lv_object, lv_name, lwa_attch.
*
*  MOVE sy-langu TO lv_spras.
*  lv_object = 'VBBK'.
*  MOVE p_vbeln TO lv_name.
*
**get text attachment payment
**  lv_tdid = 'Z014'.
**  CLEAR lt_tline.
**  REFRESH lt_tline.
**  PERFORM preapare_text TABLES lt_tline
**                        USING lv_tdid lc_spras_en lv_name lv_object.
**  IF lt_tline[] IS INITIAL.
**    PERFORM preapare_text TABLES lt_tline
**                          USING lv_tdid lc_spras_th lv_name lv_object.
**  ENDIF.
**  IF lt_tline[] IS NOT INITIAL.
**    lwa_attch-pymnt = gc_x.
**  ENDIF.
*
**get text attachment quotation
*  lv_tdid = 'Z015'.
*  CLEAR lt_tline.
*  REFRESH lt_tline.
*  PERFORM preapare_text TABLES lt_tline
*                        USING lv_tdid lc_spras_en lv_name lv_object.
*  IF lt_tline[] IS INITIAL.
*    PERFORM preapare_text TABLES lt_tline
*                          USING lv_tdid lc_spras_th lv_name lv_object.
*  ENDIF.
*  IF lt_tline[] IS NOT INITIAL.
*    lwa_attch-quotn = gc_x.
*  ENDIF.
*
**get text attachment map
*  lv_tdid = 'Z016'.
*  CLEAR lt_tline.
*  REFRESH lt_tline.
*  PERFORM preapare_text TABLES lt_tline
*                        USING lv_tdid lc_spras_en lv_name lv_object.
*  IF lt_tline[] IS INITIAL.
*    PERFORM preapare_text TABLES lt_tline
*                          USING lv_tdid lc_spras_th lv_name lv_object.
*  ENDIF.
*  IF lt_tline[] IS NOT INITIAL.
*    lwa_attch-map = gc_x.
*  ENDIF.
*
**get text attachment p/o
*  lv_tdid = 'Z017'.
*  CLEAR lt_tline.
*  REFRESH lt_tline.
*  PERFORM preapare_text TABLES lt_tline
*                        USING lv_tdid lc_spras_en lv_name lv_object.
*  IF lt_tline[] IS INITIAL.
*    PERFORM preapare_text TABLES lt_tline
*                          USING lv_tdid lc_spras_th lv_name lv_object.
*  ENDIF.
*  IF lt_tline[] IS NOT INITIAL.
*    lwa_attch-po = gc_x.
*  ENDIF.
*
*  MOVE-CORRESPONDING lwa_attch TO gwa_attch.

ENDFORM.                    " PREPARE_DATA_ATTACHMENT
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA_PREMIUM
*&---------------------------------------------------------------------*
FORM PREPARE_DATA_PREMIUM  USING P_VBELN TYPE VBAK-VBELN
                           CHANGING GWA_PREM.

  DATA: LWA_PREM TYPE TYP_PREM.
  DATA: LV_TDID   LIKE  THEAD-TDID,
        LV_SPRAS  LIKE  THEAD-TDSPRAS,
        LV_NAME   LIKE  THEAD-TDNAME,
        LV_OBJECT LIKE  THEAD-TDOBJECT,
        LV_QTY    LIKE VBAP-KWMENG.
  DATA: LT_TLINE TYPE TABLE OF TLINE.
  DATA: LT_VBAP TYPE  TYP_VBAP.

  CONSTANTS: LC_BOMH_YO01(4) TYPE C VALUE 'YO01',
             LC_BOMH_ZO01(4) TYPE C VALUE 'ZO01'.
  CONSTANTS: LC_SPRAS_EN TYPE SPRAS VALUE 'E',
             LC_SPRAS_TH TYPE SPRAS VALUE '2'.

  CLEAR: LV_TDID, LV_SPRAS, LV_OBJECT, LV_NAME, LWA_PREM.

  MOVE SY-LANGU TO LV_SPRAS.
  LV_OBJECT = 'VBBP'.
  LV_TDID = 'ZI03'.



  LOOP AT GT_ITEM INTO LT_VBAP.
*  pichon khun is given only for set buying, so check only BOM Header ItemCat. ZO01 & YO01
    IF LT_VBAP-PSTYV EQ LC_BOMH_YO01 OR LT_VBAP-PSTYV EQ LC_BOMH_ZO01.
      CLEAR LT_TLINE.
      REFRESH LT_TLINE.
      CONCATENATE LT_VBAP-VBELN LT_VBAP-POSNR INTO LV_NAME.

*    get item text: pichon khum
      PERFORM PREAPARE_TEXT TABLES LT_TLINE
                            USING LV_TDID LC_SPRAS_EN LV_NAME LV_OBJECT.
      IF LT_TLINE[] IS INITIAL.
        PERFORM PREAPARE_TEXT TABLES LT_TLINE
                              USING LV_TDID LC_SPRAS_TH LV_NAME LV_OBJECT.
      ENDIF.
      IF LT_TLINE[] IS NOT INITIAL.
        LWA_PREM-PICHONK = GC_X.
        LV_QTY = LWA_PREM-QTY + LT_VBAP-KWMENG.
        MOVE LV_QTY TO LWA_PREM-QTY.
      ENDIF.
    ENDIF.
  ENDLOOP.

  MOVE-CORRESPONDING LWA_PREM TO GWA_PREM.

ENDFORM.                    " PREPARE_DATA_PREMIUM
*&---------------------------------------------------------------------*
*&      Form  PREPARE_TEXT_INVOICE_REMARK
*&---------------------------------------------------------------------*
FORM PREPARE_TEXT_INVOICE_REMARK  USING LWA_ATTCH TYPE TYP_ATTCH
                                        LWA_PREM  TYPE TYP_PREM
                                        P_VBELN TYPE VBAK-VBELN
                                        P_KNUMV TYPE VBAK-KNUMV.

  DATA: LWA_THEAD TYPE THEAD.
  DATA: LV_TDID   LIKE  THEAD-TDID,
        LV_SPRAS  LIKE  THEAD-TDSPRAS,
        LV_NAME   LIKE  THEAD-TDNAME,
        LV_OBJECT LIKE  THEAD-TDOBJECT.
  DATA: LV_TEXT_NAME TYPE TDOBNAME.

  DATA: LT_TLINE      TYPE TABLE OF TLINE,
        LT_TLINE_Z011 TYPE TABLE OF TLINE,
        LWA_TLINE     TYPE TLINE.

  DATA: LV_KBETR      TYPE KBETR,
        LV_KBETR_TEXT TYPE N,
        LV_TEXT       TYPE TDLINE,
        LV_LINE       TYPE I.

  DATA: LV_PREMIUM_QTY(4) TYPE N,
        LV_PREMIUM_IND    TYPE I.

*  CONSTANTS:
*                     lc_discount(35) TYPE c VALUE 'ส่วนลดเงินสด',  "Delete on 07.01.2015

  DATA:  LC_DISCOUNT(35) TYPE C VALUE 'ส่วนลดเงินสด'.  "Add on 07.01.2015  Case Early Bird


  CONSTANTS: LC_PERCENTAGE  TYPE C VALUE '%',
             LC_PICHONK(10) TYPE C VALUE 'พิชอนคุง',
             LC_AMOUNT(6)   TYPE C VALUE 'จำนวน',
             LC_TUBE(5)     TYPE C VALUE '+ ท่อ',
             LC_MAP(20)     TYPE C VALUE 'ส่งตามแผนที่'.
  CONSTANTS: LC_VKBUR_1001     TYPE VKBUR VALUE '1001',
             LC_VKBUR_1003     TYPE VKBUR VALUE '1003',
             LC_VKBUR_1004     TYPE VKBUR VALUE '1004',
             LC_VKBUR_1006     TYPE VKBUR VALUE '1006',
             LC_VKBUR_1007     TYPE VKBUR VALUE '1007',
             LC_VKBUR_1008     TYPE VKBUR VALUE '1008',
             LC_VKBUR_1009     TYPE VKBUR VALUE '1009',
             LC_VKBUR_1010     TYPE VKBUR VALUE '1010',
             LC_INVOICE_REMARK TYPE TDID VALUE 'Z011',
             LC_VBBK           TYPE TDOBJECT VALUE 'VBBK',
             LC_LINE_BREAK     TYPE C VALUE '*'.
  CONSTANTS: LC_SPRAS_EN TYPE SPRAS VALUE 'E',
             LC_SPRAS_TH TYPE SPRAS VALUE '2'.


*-- Add on Check Early Bird on 07.01.2015

  DATA: LW_LINES    LIKE LINE OF IT_LINES.
*        lv_pmnt_typ LIKE  ztb_pay_in-pmnt_typ.

  DATA : LV_EARLY_B_FLAG TYPE VBAK-KVGR2.

  LOOP AT IT_LINES   INTO LW_LINES.
    IF LW_LINES-TDLINE IS NOT INITIAL.
      SELECT SINGLE KVGR2
      FROM VBAK
      INTO LV_EARLY_B_FLAG
      WHERE VBELN = P_VBELN.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.


  CLEAR GV_TXTNAME_INVRMK.

  CLEAR: LV_TEXT, LWA_TLINE.
  LWA_TLINE-TDFORMAT = LC_LINE_BREAK.


  CLEAR: LV_KBETR.
  SELECT SINGLE KBETR INTO LV_KBETR
                      FROM PRCD_ELEMENTS
                      WHERE KNUMV EQ P_KNUMV
                      AND KSCHL EQ LC_KSCHL_ZDO3.
  IF SY-SUBRC EQ 0.
    LV_KBETR = LV_KBETR / 10 .
    MOVE LV_KBETR TO LV_KBETR_TEXT.
    CONCATENATE LC_DISCOUNT LV_KBETR_TEXT LC_PERCENTAGE INTO LV_TEXT SEPARATED BY SPACE.
    MOVE LV_TEXT TO LWA_TLINE-TDLINE.
    APPEND LWA_TLINE TO LT_TLINE.
  ENDIF.

*  define invoice remark text from text map
  IF LWA_ATTCH-MAP NE SPACE.

    LV_TEXT = LC_MAP.

    IF P_VBELN+0(3) EQ '300' OR
       P_VBELN+0(3) EQ '306'.
      LV_TEXT = 'ส่งไซท์งาน'.
    ENDIF.

    MOVE LV_TEXT TO LWA_TLINE-TDLINE.
    APPEND LWA_TLINE TO LT_TLINE.
  ENDIF.

*  save text to standard text
  DESCRIBE TABLE LT_TLINE LINES LV_LINE.
  IF LV_LINE NE 0.

*  define invoice remark text from Text Invoice Remark (Z011)
    CLEAR: LV_TDID, LV_SPRAS, LV_OBJECT, LV_NAME.
    MOVE SY-LANGU TO LV_SPRAS.
    LV_OBJECT = LC_VBBK.
    MOVE P_VBELN TO LV_NAME.
    LV_TDID = LC_INVOICE_REMARK.

    CLEAR LT_TLINE_Z011.
    REFRESH LT_TLINE_Z011.

*    PERFORM PREAPARE_TEXT TABLES LT_TLINE_Z011
*                           USING LV_TDID LC_SPRAS_EN LV_NAME LV_OBJECT.
*    IF LT_TLINE_Z011[] IS INITIAL.
*      PERFORM PREAPARE_TEXT TABLES LT_TLINE_Z011
*                             USING LV_TDID LC_SPRAS_TH LV_NAME LV_OBJECT.
*    ENDIF.

    IF LT_TLINE_Z011[] IS NOT INITIAL.
      CLEAR LWA_TLINE.
      LOOP AT LT_TLINE_Z011 INTO LWA_TLINE.
*      READ TABLE  INDEX 1 lt_tline_z011 INTO lwa_tline.
        MOVE LWA_TLINE-TDLINE TO LV_TEXT.
        MOVE LV_TEXT TO LWA_TLINE-TDLINE.
        APPEND LWA_TLINE TO LT_TLINE.
      ENDLOOP.
    ENDIF.

    CLEAR LV_TEXT_NAME.
    MOVE P_VBELN TO LV_NAME.
    MOVE SY-LANGU TO LV_SPRAS.
    CONCATENATE 'Z' 'INVOICE_REMARK' LV_NAME INTO LV_TEXT_NAME SEPARATED BY '_'.
    LWA_THEAD-TDID = 'ST'.
    LWA_THEAD-TDOBJECT = 'TEXT'.
    LWA_THEAD-TDNAME = LV_TEXT_NAME.
    LWA_THEAD-TDSPRAS = LV_SPRAS.

*  save standard text
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        HEADER   = LWA_THEAD
      TABLES
        LINES    = LT_TLINE
      EXCEPTIONS
        ID       = 1
        LANGUAGE = 2
        NAME     = 3
        OBJECT   = 4
        OTHERS   = 5.

    IF SY-SUBRC EQ 0.
      GV_TXTNAME_INVRMK = LWA_THEAD-TDNAME.
    ENDIF.

  ENDIF.


ENDFORM.                    " PREPARE_TEXT_INVOICE_REMARK
*&---------------------------------------------------------------------*
*&      FORM prepare_item1
*&---------------------------------------------------------------------*
FORM PREPARE_ITEM1 USING P_VBELN TYPE VBAK-VBELN
                         P_KNUMV TYPE VBAK-KNUMV
                   CHANGING P_COUNT_ITEM TYPE I
                            P_FLG_PROCON TYPE C
                            P_FLG_PROFIT TYPE C
                            P_FLAG TYPE C
                            P_FLAG_IO TYPE C
                            P_FLAG_SHIPTO TYPE C
                            P_FLG_DISCON TYPE C
                            P_FLG_DISTICT TYPE C
                            P_FLG_CUSTGROUP TYPE C
                            P_FLG_PRICGROUP TYPE C
                            P_FLG_MATGROUP TYPE C
                            P_FLG_EMPCODE TYPE C
                            P_FLG_ROUTE
                            P_FLG_PROFIT_SALE TYPE C.


  DATA: LV_VBELN       LIKE VBAK-VBELN,
        LV_UEPOS       LIKE VBAP-UEPOS,
        LV_DESC        TYPE STRING,
        LV_QTY         TYPE VBAP-KWMENG,
        LV_INDEX       LIKE SY-TABIX,
        LV_BOM         TYPE C,
        LV_PRICE       TYPE NETWR,
        LV_RUNNO(2)    TYPE N,
        LV_TEXTID(16)  TYPE C,
        LV_TYPEAPP(20) TYPE C,
        LV_TYPEMAT(4)  TYPE C,
        LV_POSNR_CHECK TYPE VBAP-POSNR.
  DATA: WA_CHECK_POSNR TYPE TYP_VBAP.
  DATA: LV_CHECK_PROFIT TYPE VBAP-PRCTR.
  DATA: LV_CHECK_IO  TYPE AUFK-AUFNR.
  DATA: LV_CHECK_SHIPTO TYPE VBPA-ADRNR.
  DATA: LV_DISTICT_H    TYPE VBKD-BZIRK,
        LV_DISTICT_I    TYPE VBKD-BZIRK,
        LV_DISTICT_TMP  TYPE VBKD-BZIRK,
        LV_CUSTGRP_I    TYPE VBKD-KDGRP,
        LV_PRICEGRP_I   TYPE VBKD-KONDA,
        LV_CUSTGRP_TMP  TYPE VBKD-KDGRP,
        LV_PRICEGRP_TMP TYPE VBKD-KONDA,
        LV_MATGROUP_TMP TYPE VBAP-KONDM,
        LV_EMPCODE_I    TYPE VBPA-PERNR,
        LV_EMPCODE_TMP  TYPE VBPA-PERNR.
  DATA: LV_CHECK_COUNT TYPE I,
        LV_ROUTE_TMP   TYPE VBAP-ROUTE,
        LV_ROUTE       TYPE VBAP-ROUTE.

  FIELD-SYMBOLS : <LFS_VBAK> TYPE TYP_VBAK.

  SORT GT_ITEM BY POSNR.
  CLEAR: GT_ITEM1,GT_ITEM2.
  CLEAR: WA_ITEM1,WA_ITEM2,LV_POSNR_CHECK,LV_DESC,P_COUNT_ITEM.
  CLEAR: P_FLG_PROCON,
         P_FLG_DISCON,
         LV_CHECK_PROFIT.
  CLEAR: LV_CHECK_IO,P_FLAG_IO,P_FLAG,
         P_FLG_DISTICT,P_FLG_MATGROUP,
         LV_DISTICT_TMP,LV_CUSTGRP_TMP,
         LV_ROUTE_TMP,LV_ROUTE,
         LV_PRICEGRP_TMP,LV_CHECK_COUNT,LV_MATGROUP_TMP,
         LV_EMPCODE_TMP,GV_QTY_ITEM.

  IF NOT GT_ITEM[] IS INITIAL.
    P_COUNT_ITEM = 0.
    LV_CHECK_COUNT = 0.
    LOOP AT GT_ITEM INTO WA_ITEM WHERE VBELN EQ P_VBELN.

      CLEAR : LV_DISTICT_H,LV_DISTICT_I,LV_CUSTGRP_I,
              LV_PRICEGRP_I,LV_EMPCODE_I.

      READ TABLE GT_PRODCON INTO WA_PRODCON WITH KEY MATNR = WA_ITEM-MATNR.
      IF SY-SUBRC = 0.
        P_FLG_PROCON = 'X'.
        WA_ITEM1-C_PROCON = '*'.

      ENDIF.

      READ TABLE GT_DISCON INTO WA_DISCON WITH KEY MATNR = WA_ITEM-MATNR.
      IF SY-SUBRC = 0.
        P_FLG_DISCON = 'X'.
        WA_ITEM1-C_PROCON = 'X'.
      ENDIF.

      IF LV_CHECK_IO EQ ''.
        LV_CHECK_IO = WA_ITEM-AUFNR.
      ELSE.
        IF LV_CHECK_IO NE WA_ITEM-AUFNR.
          P_FLAG_IO = 'X'.
        ENDIF.
      ENDIF.
      P_COUNT_ITEM = P_COUNT_ITEM + 1.

      READ TABLE GT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ITEM-VBELN
                                               POSNR = WA_ITEM-POSNR.
      IF SY-SUBRC EQ 0.
        LV_DISTICT_I = WA_VBKD-BZIRK.
        LV_CUSTGRP_I = WA_VBKD-KDGRP.
        LV_PRICEGRP_I = WA_VBKD-KONDA.
      ELSE.
        READ TABLE GT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ITEM-VBELN
                                             POSNR = ''.
        IF SY-SUBRC EQ 0.
          LV_DISTICT_I = WA_VBKD-BZIRK.
          LV_CUSTGRP_I = WA_VBKD-KDGRP.
          LV_PRICEGRP_I = WA_VBKD-KONDA.
        ENDIF.
      ENDIF.
      IF LV_CHECK_COUNT EQ 0 .
        LV_DISTICT_TMP = LV_DISTICT_I.
      ELSE.
        IF LV_DISTICT_I EQ LV_DISTICT_TMP.
          LV_DISTICT_TMP = LV_DISTICT_I.
        ELSE.
          P_FLG_DISTICT = 'X'.
          LV_DISTICT_TMP = LV_DISTICT_I.
        ENDIF.
      ENDIF.

      IF LV_CHECK_COUNT EQ 0 .
        LV_ROUTE_TMP = WA_ITEM-ROUTE.
      ELSE.
        IF WA_ITEM-ROUTE NE LV_ROUTE_TMP.
          P_FLG_ROUTE = 'X'.
          LV_ROUTE_TMP = LV_ROUTE.
        ENDIF.
      ENDIF.

      IF LV_CHECK_COUNT EQ 0 .
        LV_CUSTGRP_TMP = LV_CUSTGRP_I.

      ELSE.
        IF LV_CUSTGRP_I EQ LV_CUSTGRP_TMP.
          LV_CUSTGRP_TMP = LV_CUSTGRP_I.
        ELSE.
          P_FLG_CUSTGROUP = 'X'.
          LV_CUSTGRP_TMP = LV_CUSTGRP_I.
        ENDIF.
      ENDIF.

      IF LV_CHECK_COUNT EQ 0 .
        LV_PRICEGRP_TMP = LV_PRICEGRP_I.

      ELSE.
        IF LV_PRICEGRP_I EQ LV_PRICEGRP_TMP.
          LV_PRICEGRP_TMP = LV_PRICEGRP_I.
        ELSE.
          P_FLG_PRICGROUP = 'X'.
          LV_PRICEGRP_TMP = LV_PRICEGRP_I.
        ENDIF.
      ENDIF.

      READ TABLE GT_PARTNER INTO WA_PARTNER WITH KEY VBELN = WA_ITEM-VBELN
                                                     POSNR = WA_ITEM-POSNR
                                                     PARVW = 'VE'.

      IF SY-SUBRC EQ 0.
        LV_EMPCODE_I =  WA_PARTNER-PERNR.
      ELSE.
        READ TABLE GT_PARTNER INTO WA_PARTNER WITH KEY VBELN = WA_ITEM-VBELN
                                                        POSNR = '000000'
                                                        PARVW = 'VE'.
        IF SY-SUBRC EQ 0.
          LV_EMPCODE_I =  WA_PARTNER-PERNR.
        ENDIF.
      ENDIF.
      IF LV_CHECK_COUNT EQ 0 .
        LV_EMPCODE_TMP = LV_EMPCODE_I.
      ELSE.
        IF LV_EMPCODE_TMP EQ LV_EMPCODE_I.
          LV_EMPCODE_TMP = LV_EMPCODE_I.
        ELSE.
          P_FLG_EMPCODE = 'X'.
          LV_EMPCODE_TMP = LV_EMPCODE_I.
        ENDIF.
      ENDIF.

      CLEAR WA_ITEM1.
      IF WA_ITEM-UEPOS EQ 000000.
        CLEAR LV_DESC.

        WA_ITEM1-VBELN = WA_ITEM-VBELN.
        WA_ITEM1-POSNR = WA_ITEM-POSNR.
        WA_ITEM1-MATNR = WA_ITEM-MATNR.
        WA_ITEM1-UEPOS = WA_ITEM-UEPOS.
        WA_ITEM1-DESC  = WA_ITEM-ARKTX.

        CLEAR : WA_VBAK, GS_MARD.
        READ TABLE GT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_ITEM-VBELN.
        IF SY-SUBRC = 0.
          IF WA_VBAK-VTWEG = '40'.
            READ TABLE GT_MARD INTO GS_MARD WITH KEY MATNR = WA_ITEM-MATNR
                                                     WERKS = WA_ITEM-WERKS
                                                     LGORT = WA_ITEM-LGORT.
            IF SY-SUBRC = 0.
              WA_ITEM1-LGORT = GS_MARD-LGORT.
              WA_ITEM1-LGPBE = GS_MARD-LGPBE.
            ENDIF.
            IF WA_ITEM1-LGPBE IS NOT INITIAL.
              CONCATENATE WA_ITEM1-DESC '(' WA_ITEM1-LGPBE ')' INTO WA_ITEM1-DESC.
            ENDIF.
          ENDIF.
        ENDIF.
        WA_ITEM1-KWMENG = WA_ITEM-KWMENG.
        WA_ITEM1-POSEX = WA_ITEM-POSEX.
        MOVE WA_ITEM-KWMENG TO WA_ITEM1-KWMENG2.
        WA_ITEM1-PRICE = WA_ITEM-NETPR.
        WA_ITEM1-EDATU  = WA_ITEM-EDATU.
        WA_ITEM1-NETWR = WA_ITEM-NETWR.
        LOOP AT GT_CHECK_MOTHER INTO WA_CHECK_MOTHER WHERE VBELN EQ P_VBELN
                                                     AND   UEPOS EQ WA_ITEM-POSNR.

          LV_INDEX = SY-TABIX.
          IF LV_DESC IS INITIAL.
            LV_DESC = WA_CHECK_MOTHER-MATNR.
          ELSE.
            CONCATENATE  LV_DESC WA_CHECK_MOTHER-MATNR  INTO LV_DESC SEPARATED BY ','.
          ENDIF.
          MOVE LV_DESC TO WA_ITEM1-DESC.

          LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = P_KNUMV AND
                                          KPOSN = WA_CHECK_MOTHER-POSNR AND
                                        ( KSCHL = 'ZDP2' OR KSCHL = 'ZDP3' ).
            WA_ITEM1-KWERT = WA_ITEM1-KWERT + ( WA_KONV-KWERT * -1 ).
          ENDLOOP.

        ENDLOOP.

        LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = P_KNUMV AND
                                           KPOSN = WA_ITEM-POSNR AND
                                         ( KSCHL = 'ZDP2' OR
                                           KSCHL = 'ZDP3' ).
          WA_ITEM1-KWERT = WA_KONV-KWERT * -1.
        ENDLOOP.
        WA_ITEM1-KWMENG_MOM = WA_ITEM-KWMENG.
        WA_ITEM1-WMENG_MOM  = WA_ITEM-WMENG.    "Req Qty

        READ TABLE GT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ITEM1-VBELN
                                                 POSNR = WA_ITEM1-POSNR.
        IF SY-SUBRC EQ 0.
          WA_ITEM1-ZTERM = WA_VBKD-ZTERM.
        ELSE.
          READ TABLE GT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ITEM1-VBELN
                                                   POSNR = ''.
          IF SY-SUBRC EQ 0.
            WA_ITEM1-ZTERM = WA_VBKD-ZTERM.
          ENDIF.
        ENDIF.

        READ TABLE GT_PRODCON INTO WA_PRODCON WITH KEY MATNR = WA_ITEM-MATNR.
        IF SY-SUBRC = 0.
          P_FLG_PROCON = 'X'.
          WA_ITEM1-C_PROCON = '*'.

        ENDIF.

        READ TABLE GT_DISCON INTO WA_DISCON WITH KEY MATNR = WA_ITEM-MATNR.
        IF SY-SUBRC = 0.
          P_FLG_DISCON = 'X'.
          WA_ITEM1-C_PROCON = 'X'.
        ENDIF.

        WRITE WA_ITEM1-EDATU TO WA_ITEM1-DELIV.
        ADD WA_ITEM1-KWMENG TO GV_QTY_ITEM.
        ADD WA_ITEM1-WMENG_MOM TO GV_REQ_QTY.
        APPEND WA_ITEM1 TO GT_ITEM1.
        LV_BOM = ' '.
      ELSE.
        READ TABLE GT_ITEM INTO WA_CHECK_POSNR WITH KEY VBELN = WA_ITEM-VBELN
                                                        POSNR = WA_ITEM-UEPOS.
        IF SY-SUBRC EQ 0.
          LV_UEPOS = WA_ITEM-UEPOS.
          READ TABLE GT_ITEM1 INTO  WA_ITEM1 WITH KEY POSNR = LV_UEPOS.
          LV_INDEX = SY-TABIX.
          IF SY-SUBRC EQ 0.
            IF LV_BOM EQ 'X'.
              WA_ITEM1-CHECK_MOM = 'X'.

              WA_ITEM1-KWMENG = WA_ITEM1-KWMENG + WA_ITEM-KWMENG.
              WA_ITEM1-NETWR = WA_ITEM1-NETWR + WA_ITEM-NETWR.
              WA_ITEM1-NETPR = WA_ITEM1-NETPR + WA_ITEM-NETPR.

              LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = P_KNUMV AND
                                                 KPOSN = WA_ITEM-POSNR AND
                                               ( KSCHL = 'ZDP2' OR
                                                 KSCHL = 'ZDP3' ).
                WA_ITEM1-KWERT = WA_ITEM1-KWERT + ( WA_KONV-KWERT * -1 ).
              ENDLOOP.

            ELSE.

              WA_ITEM1-KWMENG = WA_ITEM-KWMENG.
              WA_ITEM1-NETWR = WA_ITEM-NETWR.
              WA_ITEM1-NETPR = WA_ITEM-NETPR.

              LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = P_KNUMV AND
                                                 KPOSN = WA_ITEM-POSNR AND
                                               ( KSCHL = 'ZDP2' OR
                                                 KSCHL = 'ZDP3' ).
                WA_ITEM1-KWERT = WA_KONV-KWERT * -1.
              ENDLOOP.
            ENDIF.
*                             calculate rice = NETWR(BOM item) / KWMENG(BOM header)
            IF WA_ITEM1-KWMENG_MOM EQ 0.
              LV_PRICE = 0.
            ELSE.
              LV_PRICE = WA_ITEM-NETWR / WA_ITEM1-KWMENG_MOM .
            ENDIF.
            WA_ITEM1-PRICE = WA_ITEM1-PRICE + LV_PRICE.
            MOVE WA_ITEM1-KWMENG TO WA_ITEM1-KWMENG2.
            MODIFY GT_ITEM1 FROM WA_ITEM1 INDEX LV_INDEX.
            LV_BOM = 'X'.
          ENDIF.
        ELSE.
          WA_ITEM1-VBELN = WA_ITEM-VBELN.
          WA_ITEM1-POSNR = WA_ITEM-POSNR.
          WA_ITEM1-MATNR = WA_ITEM-MATNR.
          WA_ITEM1-UEPOS = WA_ITEM-UEPOS.
          WA_ITEM1-DESC  = WA_ITEM-ARKTX.
          WA_ITEM1-KWMENG = WA_ITEM-KWMENG.
          WA_ITEM1-POSEX = WA_ITEM-POSEX.
          MOVE WA_ITEM-KWMENG TO WA_ITEM1-KWMENG2.
          WA_ITEM1-PRICE = WA_ITEM-NETPR.
          WA_ITEM1-EDATU  = WA_ITEM-EDATU.
          WA_ITEM1-NETWR = WA_ITEM-NETWR.

          LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = P_KNUMV AND
                                             KPOSN = WA_ITEM-POSNR AND
                                           ( KSCHL = 'ZDP2' OR
                                             KSCHL = 'ZDP3' ).
            WA_ITEM1-KWERT = WA_KONV-KWERT * -1.
          ENDLOOP.
          WA_ITEM1-KWMENG_MOM = WA_ITEM-KWMENG.
          WA_ITEM1-WMENG_MOM  = WA_ITEM-WMENG. "Req Qty

          READ TABLE GT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ITEM1-VBELN
                                                   POSNR = WA_ITEM1-POSNR.
          IF SY-SUBRC EQ 0.
            WA_ITEM1-ZTERM = WA_VBKD-ZTERM.
          ELSE.
            READ TABLE GT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ITEM1-VBELN
                                                     POSNR = ''.
            IF SY-SUBRC EQ 0.
              WA_ITEM1-ZTERM = WA_VBKD-ZTERM.
            ENDIF.
          ENDIF.



          READ TABLE GT_PRODCON INTO WA_PRODCON WITH KEY MATNR = WA_ITEM-MATNR.
          IF SY-SUBRC = 0.
            P_FLG_PROCON = 'X'.
            WA_ITEM1-C_PROCON = '*'.  "Add by Wantanee 20120229

          ENDIF.
          READ TABLE GT_DISCON INTO WA_DISCON WITH KEY MATNR = WA_ITEM-MATNR.
          IF SY-SUBRC = 0.
            P_FLG_DISCON = 'X'.
*              wa_item1-c_procon = ''.  "Add by Wantanee 20120229 "Remove by Wantanee 20120301
            WA_ITEM1-C_PROCON = 'X'.  "Add by Wantanee 20120301
          ENDIF.

          APPEND WA_ITEM1 TO GT_ITEM1.
          LV_BOM = ' '.

        ENDIF.


        IF LV_CHECK_PROFIT NE ''.
          IF WA_ITEM-PRCTR NE LV_CHECK_PROFIT.
            P_FLG_PROFIT = 'X'.
          ENDIF.

        ELSE.
          LV_CHECK_PROFIT = WA_ITEM-PRCTR.
        ENDIF.

        READ TABLE GT_PARTNER INTO WA_PARTNER WITH KEY VBELN = WA_ITEM-VBELN
                                                       POSNR = WA_ITEM-POSNR
                                                       PARVW = 'WE' .

        IF SY-SUBRC EQ 0.
          IF LV_CHECK_SHIPTO = ''.
            LV_CHECK_SHIPTO = WA_PARTNER-ADRNR.
          ELSE.
            IF LV_CHECK_SHIPTO NE WA_PARTNER-ADRNR.
              P_FLAG_SHIPTO = 'X'.
            ENDIF.

          ENDIF.
        ELSE.
          READ TABLE GT_PARTNER INTO WA_PARTNER WITH KEY VBELN = WA_ITEM-VBELN
                                                         POSNR = '0000'
                                                         PARVW = 'WE' .
          IF SY-SUBRC EQ 0.
            LV_CHECK_SHIPTO = WA_PARTNER-ADRNR.
          ENDIF.
        ENDIF.

        LV_CHECK_COUNT = LV_CHECK_COUNT + 1.

        IF P_FLG_PROFIT_SALE EQ ''.
          READ TABLE GT_ZTSD_PROFIT INTO WA_ZTSD_PROFIT WITH KEY VKBUR = WA_VBAK-VKBUR
                                                                 VKGRP = WA_VBAK-VKGRP.
          IF SY-SUBRC EQ 0.
            IF WA_ZTSD_PROFIT-PRCTR = WA_ITEM-PRCTR .
              P_FLG_PROFIT_SALE = ''.
            ELSE.
              P_FLG_PROFIT_SALE = 'X'.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT GT_ITEM1[] BY POSNR.

  SELECT COUNT( * ) INTO LV_COUNT_ALL
    FROM VBAP
    WHERE VBELN = P_VBELN.

  SELECT VBELN POSNR MATNR UEPOS
  INTO TABLE GT_ITEM_VBAP_CHECKP
  FROM VBAP
  WHERE VBELN EQ P_VBELN.


* count record to define run no. for item
  CLEAR LV_RUNNO.
  LOOP AT GT_ITEM1 INTO WA_ITEM1 WHERE VBELN EQ P_VBELN.
    CLEAR: LV_TYPEAPP, LV_TYPEMAT,LV_DESC,LV_TEXTID,LV_INDEX.
*           p_flag.  "Add by Wantanee Remove by Wantanee 20120323
    LV_INDEX = SY-TABIX.
    CONCATENATE WA_ITEM1-VBELN WA_ITEM1-POSNR INTO LV_TEXTID.
    PERFORM READ_TEXT_HEAD USING 'ZI10' 'VBBP' LV_TEXTID
                                 CHANGING LV_TYPEAPP.
    IF NOT LV_TYPEAPP IS INITIAL.
      LV_TYPEMAT = LV_TYPEAPP+0(2).
      CONCATENATE WA_ITEM1-DESC '(' LV_TYPEMAT ')' INTO LV_DESC.
      MOVE LV_DESC TO WA_ITEM1-DESC.
    ENDIF.

    LV_RUNNO = LV_RUNNO + 1 .
    MOVE LV_RUNNO TO WA_ITEM1-RUNNO.
*S---------------------WCH 08.07.2011----------------------*
    READ TABLE GT_VBAK ASSIGNING <LFS_VBAK>
    WITH KEY VBELN = WA_ITEM1-VBELN.
    IF SY-SUBRC EQ 0.
      IF WA_ITEM1-ZTERM NE <LFS_VBAK>-ZTERM.
        MOVE: 'X' TO <LFS_VBAK>-SFLAG.
        P_FLAG = 'X'.
*        MODIFY GT_VBAK FROM WA_VBAK.
      ENDIF.
    ENDIF.

*E---------------------WCH 08.07.2011----------------------*
    WA_ITEM1-NETWR = WA_ITEM1-PRICE * WA_ITEM1-WMENG_MOM.

    MODIFY GT_ITEM1 FROM WA_ITEM1 INDEX LV_INDEX.

  ENDLOOP.
  LOOP AT GT_ITEM INTO WA_ITEM WHERE UEPOS NE SPACE AND VBELN EQ P_VBELN.
    CLEAR WA_ITEM2.
    "Add by Wantanee 20120229
    READ TABLE GT_PRODCON INTO WA_PRODCON WITH KEY MATNR = WA_ITEM-MATNR.
    IF SY-SUBRC = 0.
      WA_ITEM2-C_PROCON = '*'.
    ENDIF.
    READ TABLE GT_DISCON INTO WA_DISCON WITH KEY MATNR = WA_ITEM-MATNR.
    IF SY-SUBRC = 0.
*              wa_item2-c_procon = ''.  "Add by Wantanee 20120229 Remove By Wantanee 20120301
      WA_ITEM2-C_PROCON = 'X'.  "Add by Wantanee 20120301
    ENDIF.
    "End Add by Wantanee 20120229
    WA_ITEM2-VBELN = WA_ITEM-VBELN.
    WA_ITEM2-POSNR = WA_ITEM-UEPOS.
    WA_ITEM2-POSNR_1 = WA_ITEM-POSNR.
    WA_ITEM2-MATNR = WA_ITEM-MATNR.
    WA_ITEM2-WMENG = WA_ITEM-WMENG.
    WA_ITEM2-BMENG = WA_ITEM-KWMENG.
    WA_ITEM2-EDATU  = WA_ITEM-EDATU.
    WA_ITEM2-POSEX = WA_ITEM-POSEX. "Add by Wantanee 20120220
*       wa_item2-c_procon = wa_item-c_procon. "Add by Wantanee 20120220

* Edit by Wantanee 20110210
    READ TABLE GT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ITEM2-VBELN
                                             POSNR = WA_ITEM2-POSNR_1.
    IF SY-SUBRC EQ 0.
      WA_ITEM2-ZTERM = WA_VBKD-ZTERM.
*S---------------------WCH 08.07.2011----------------------*
      READ TABLE GT_VBAK ASSIGNING <LFS_VBAK>
      WITH KEY VBELN = WA_ITEM2-VBELN.
      IF SY-SUBRC EQ 0.
        IF WA_ITEM2-ZTERM NE <LFS_VBAK>-ZTERM.
          MOVE: 'X' TO <LFS_VBAK>-SFLAG.
          P_FLAG = 'X'.
*          MODIFY GT_VBAK FROM WA_VBAK.
        ENDIF.
      ENDIF.
*E---------------------WCH 08.07.2011----------------------*
    ELSE.
      READ TABLE GT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ITEM2-VBELN
                                               POSNR = ''.
      IF SY-SUBRC EQ 0.
        WA_ITEM2-ZTERM = WA_VBKD-ZTERM.
*S---------------------WCH 08.07.2011----------------------*
        READ TABLE GT_VBAK ASSIGNING <LFS_VBAK>
        WITH KEY VBELN = WA_ITEM2-VBELN.
        IF SY-SUBRC EQ 0.
          IF WA_ITEM2-ZTERM NE <LFS_VBAK>-ZTERM.
            MOVE: 'X' TO <LFS_VBAK>-SFLAG.
            P_FLAG = 'X'.
*            MODIFY GT_VBAK FROM WA_VBAK.
          ENDIF.
        ENDIF.
*E---------------------WCH 08.07.2011----------------------*
      ENDIF.
    ENDIF.
* End Edit by Wantanee 20110210
    WRITE WA_ITEM2-EDATU TO WA_ITEM2-DELIV.
    APPEND WA_ITEM2 TO GT_ITEM2.

  ENDLOOP.


  "Adjust amount

ENDFORM.                    "prepare_item1
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA_TOTAL
*&---------------------------------------------------------------------*
*       Prepare Total
*----------------------------------------------------------------------*
FORM PREPARE_DATA_TOTAL USING P_VBELN TYPE VBAK-VBELN
                              P_KNUMV TYPE VBAK-KNUMV
                        CHANGING GWA_TOTAL.

  DATA: LV_VBELN LIKE VBAK-VBELN,
        LV_KNUMV LIKE VBAK-KNUMV,
        LV_WAERK LIKE VBAK-WAERK,
        LV_KBETR LIKE KONV-KBETR,
        LV_KRECH LIKE KONV-KRECH,
        LV_KWERT LIKE KONV-KWERT.

  DATA: LWA_TOTAL TYPE TYP_TOTAL.
  DATA: LWA_KONV  TYPE TABLE OF KONV.
  DATA: LV_TOTAL  LIKE VBAK-NETWR,
        LV_VAT    LIKE VBAK-NETWR,
        LV_GTOTAL LIKE VBAK-NETWR,
        LV_QTY    LIKE VBAP-KBMENG,
        LV_DIFF   TYPE P DECIMALS 3.

  CONSTANTS: LC_MWST  LIKE KONV-KSCHL VALUE 'MWST'.
  CONSTANTS: LC_ZDP2  LIKE KONV-KSCHL VALUE 'ZDP2'.



  CLEAR: LV_TOTAL, LV_VAT, LV_GTOTAL, LV_KNUMV ,LV_KBETR, LV_KRECH, LV_KWERT.
* get total
*  SELECT SINGLE netwr INTO lv_total
*                      FROM vbak
*                     WHERE vbeln EQ p_vbeln.
*   SELECT SUM( netwr ) INTO lv_total
*   FROM   vbap
*     AND  posnr IN s_posnr.
*   WHERE  vbeln EQ p_vbeln
*     AND  posnr IN s_posnr.

* get vat & percent
  SELECT SINGLE KBETR KRECH INTO (LV_KBETR , LV_KRECH)
                            FROM PRCD_ELEMENTS
                           WHERE KNUMV EQ P_KNUMV
                             AND KSCHL EQ LC_MWST
                             AND KBETR NE 0.

  MOVE LV_KBETR TO LWA_TOTAL-PCENT.

* get Advance receipt total
  LOOP AT GT_ITEM INTO WA_ITEM WHERE VBELN EQ P_VBELN.

    LV_TOTAL = LV_TOTAL + ( WA_ITEM-NETPR * WA_ITEM-WMENG ).   "Qty * Net price
    LV_VAT = LV_VAT + ( ( WA_ITEM-NETPR * WA_ITEM-WMENG ) * LV_KBETR / 100 ).
*<<< T41K914865
    LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = P_KNUMV AND
                                       KPOSN = WA_ITEM-POSNR AND
                                     ( KSCHL = 'ZDP2' OR KSCHL = 'ZDP3' ).
      LV_KWERT = LV_KWERT + WA_KONV-KWERT * -1.
    ENDLOOP.
*>>> T41K914865
  ENDLOOP.
  MOVE LV_KWERT TO LWA_TOTAL-TOTAL_DOWN.

*  CHECK sy-subrc EQ 0.

*  MOVE lv_vat TO lwa_total-vat..  "Add  by Wantanee 20111223

*Remove by Wantanee 20111223
  IF LWA_TOTAL-TOTAL_DOWN NE 0. "Add by Wantanee 20170322
    LV_VAT = ( LV_TOTAL * LV_KBETR ) / 100 .
  ENDIF.  "End Add by Wantanee 20170322
*  MOVE lv_vat TO lwa_total-vat.
*End Remove by Wantanee 20111223
* get total
  SELECT SUM( KWMENG ) INTO LV_QTY
                       FROM VBAP
                      WHERE VBELN EQ P_VBELN.

  "Check diff < 1 (Adjust amount when diff)
  "Vat
  SELECT SUM( KWERT ) INTO @LV_KWERT FROM PRCD_ELEMENTS WHERE KNUMV = @P_KNUMV AND KSCHL = @LC_MWST.
  IF SY-SUBRC = 0.
    LV_DIFF = ABS( LV_VAT - LV_KWERT ).
    IF LV_DIFF < 1.
      LV_VAT = LV_KWERT.
    ENDIF.
    CLEAR LV_DIFF.
  ENDIF.

  "Total Amount
  SELECT SINGLE NETWR INTO @DATA(LV_NETWR) FROM VBAK WHERE VBELN = @P_VBELN.
  IF SY-SUBRC = 0.
    LV_DIFF = ABS( LV_TOTAL - LV_NETWR ).
    IF LV_DIFF < 1.
      LV_TOTAL = LV_NETWR.
    ENDIF.
    CLEAR LV_DIFF.
  ENDIF.

* calculate grand total
  LV_GTOTAL = LV_TOTAL + LV_VAT.


  MOVE LV_VAT TO LWA_TOTAL-VAT.
  MOVE LV_GTOTAL TO LWA_TOTAL-GTOTAL.
  MOVE LV_QTY TO LWA_TOTAL-QTY.
  MOVE LV_TOTAL TO LWA_TOTAL-TOTAL.

  MOVE-CORRESPONDING LWA_TOTAL TO GWA_TOTAL.

ENDFORM.                "prepare_data_total


*&---------------------------------------------------------------------*
*&      Form  DELETE_TEXT
*&---------------------------------------------------------------------*
FORM DELETE_TEXT   USING   P_TEXT_NAME
                           P_FLAG_INVRMK.

  CONSTANTS: LC_MARK_X TYPE C VALUE 'X'.

  IF P_FLAG_INVRMK EQ LC_MARK_X.

    CALL FUNCTION 'DELETE_TEXT'
      EXPORTING
        ID       = 'ST'
        LANGUAGE = SY-LANGU
        NAME     = P_TEXT_NAME
        OBJECT   = 'TEXT'.

  ENDIF.

ENDFORM.                    " DELETE_TEXT
*&---------------------------------------------------------------------*
*&      Form  PREAPARE_TEXT
*&---------------------------------------------------------------------*
FORM PREAPARE_TEXT  TABLES LT_TLINE
                     USING LV_TDID
                           LV_SPRAS
                           LV_NAME
                           LV_OBJECT.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID        = LV_TDID
      LANGUAGE  = LV_SPRAS
      NAME      = LV_NAME
      OBJECT    = LV_OBJECT
    TABLES
      LINES     = LT_TLINE
    EXCEPTIONS
      NOT_FOUND = 1.


ENDFORM.                    " PREAPARE_TEXT
*&---------------------------------------------------------------------*
*&      Form  CHECK CUSTOMER CONTROL
*&      Edit by Wantanee 20110510
*&---------------------------------------------------------------------*
FORM CUST_CONTROL   USING  P_CUST_NO TYPE KNA1-KUNNR
                           P_CHANEL  TYPE VBAK-VTWEG
                           P_SALEOFFICE TYPE VBAK-VKBUR
                           P_SALEGROUP  TYPE VBAK-VKGRP
                    CHANGING P_FLAG_CUST_CONTROL.

  CONSTANTS C_GEN_C_CHECK_PROGRAM TYPE REPID VALUE 'ZSDSSDR0090'.
  CLEAR : P_FLAG_CUST_CONTROL.

  DATA :BEGIN OF LS_SO_VKGRP,
          VALUE TYPE ZSDSCAC001-VALUE_LOW,
        END OF LS_SO_VKGRP.
  DATA : LT_SO_VKGRP LIKE TABLE OF LS_SO_VKGRP.

  DATA : LT_SO_VKBUR LIKE TABLE OF LS_SO_VKGRP,
         LS_SO_VKBUR LIKE LS_SO_VKGRP.

  SELECT VALUE_LOW
  INTO TABLE LT_SO_VKGRP
  FROM ZSDSCAC001
  WHERE REPID EQ C_GEN_C_CHECK_PROGRAM
    AND PARAM LIKE 'GROUP%'
    AND VALUE_LOW EQ  P_SALEGROUP.

  SELECT VALUE_LOW
    INTO TABLE LT_SO_VKBUR
    FROM ZSDSCAC001
    WHERE REPID EQ C_GEN_C_CHECK_PROGRAM
      AND PARAM LIKE 'OFFICEGROUP%'
      AND VALUE_LOW EQ P_SALEOFFICE.


  READ TABLE GT_CUSTCON INTO WA_CUSTCON WITH KEY KUNNR = P_CUST_NO
                                                 VTWEG = P_CHANEL
                                                 NAME2 = 'X'.
  IF SY-SUBRC EQ 0.
    READ TABLE GT_CUSTCON INTO GW_CUSTCON WITH KEY KUNNR = P_CUST_NO
                                                   VTWEG = P_CHANEL
                                                   VKBUR = P_SALEOFFICE.
    IF SY-SUBRC EQ 0.
      P_FLAG_CUST_CONTROL = ''.
    ELSE.
      P_FLAG_CUST_CONTROL = 'X'.
    ENDIF.

  ELSE.
    READ TABLE GT_CUSTCON INTO WA_CUSTCON WITH KEY KUNNR = P_CUST_NO
                                                   VTWEG = P_CHANEL
                                                   NAME2 = SPACE.
    IF SY-SUBRC = 0.
      READ TABLE LT_SO_VKGRP INTO LS_SO_VKGRP
      WITH KEY VALUE = P_SALEGROUP.
      IF SY-SUBRC = 0.
        P_FLAG_CUST_CONTROL = ''.
      ELSE.
        P_FLAG_CUST_CONTROL = 'X'.
      ENDIF.
    ELSE.
      P_FLAG_CUST_CONTROL = ''.
    ENDIF.
  ENDIF.

ENDFORM.                    " DELETE_TEXT
*&---------------------------------------------------------------------*
*&      Form  CHECK SHIP TO ADDRESS
*&      Edit by Wantanee 20110819
*&---------------------------------------------------------------------*
FORM CHECK_SHIPTO   USING  P_VBELN TYPE VBPA-VBELN
                           P_ADRNR TYPE VBPA-ADRNR
                    CHANGING P_FLAG_SHIPTO.

  CLEAR :  P_FLAG_SHIPTO.

  LOOP AT GT_PARTNER INTO WA_PARTNER WHERE VBELN EQ P_VBELN AND PARVW EQ 'WE' AND ADRNR NE P_ADRNR.
    P_FLAG_SHIPTO = 'X'.
    EXIT.
  ENDLOOP.
ENDFORM.                    " DELETE_TEXT
*&---------------------------------------------------------------------*
*&      Form  CHECK SHIP TO ADDRESS
*&      Edit by Wantanee 20110819
*&---------------------------------------------------------------------*
FORM CHECK_EMP   USING     P_VBELN TYPE VBPA-VBELN
                           P_PERNR TYPE VBPA-PERNR
                    CHANGING P_FLAG_EMP.

  CLEAR :  P_FLAG_EMP.

  LOOP AT GT_PARTNER INTO WA_PARTNER WHERE VBELN EQ P_VBELN AND PARVW EQ 'VE' AND PERNR NE P_PERNR.
    P_FLAG_EMP = 'X'.
    EXIT.
  ENDLOOP.
ENDFORM.                    " DELETE_TEXT
*&---------------------------------------------------------------------*
*&      Form  Check employee in table zsdsemp_sarea
*&      Edit by Wantanee 20120326
*&---------------------------------------------------------------------*
FORM CHECK_EMP_FORSALE   USING     P_PERNR TYPE VBPA-PERNR
                                   P_VKBUR TYPE VBAK-VKBUR
                                   P_VKGRP TYPE VBAK-VKGRP
                         CHANGING P_FLG_EMPSALE.

  CLEAR :  P_FLG_EMPSALE.



  READ TABLE GT_EMP_SAREA INTO WA_EMP_SAREA WITH KEY PERNR = P_PERNR.
  IF SY-SUBRC EQ 0.
    IF P_VKBUR NE WA_EMP_SAREA-VKBUR.
      P_FLG_EMPSALE = 'X'.
    ELSE.
      IF P_VKGRP NE WA_EMP_SAREA-VKGRP.
        P_FLG_EMPSALE = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.





ENDFORM.                    " DELETE_TEXT
*&---------------------------------------------------------------------*
*&      Form  F_PRINT_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_GC_YES  text
*      -->P_0191   text
*      -->P_0192   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_GC_YES  text
*----------------------------------------------------------------------*
FORM F_PRINT_FORM.

  DATA: LV_OPTION  TYPE SSFCOMPOP,
        LV_CONTROL TYPE SSFCTRLOP,
        LV_LINE    TYPE I.

  DATA: LV_MESSAGE TYPE CHAR255.


  DATA : LS_VBAK TYPE TYP_VBAK.
*  IF r1 EQ 'X'.
* Get function module name of smart form
*  IF P_RESE EQ 'X'.
*    PERFORM F_CALL_VKM3.
*  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = GC_FORM_NAME_NEW
    IMPORTING
      FM_NAME            = GV_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  ELSE.
*    MESSAGE i000 WITH lv_message.
*  ENDIF.
*  ELSEIF r2 EQ 'X'.
*    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*      EXPORTING
*        formname           = gc_form_name
*      IMPORTING
*        fm_name            = gv_fm_name
*      EXCEPTIONS
*        no_form            = 1
*        no_function_module = 2
*        OTHERS             = 3.
*  ELSEIF r3 EQ 'X'.
*
*    IF p_rese EQ 'X'.
*      PERFORM f_call_vkm3.
*    ENDIF.
*
**    READ TABLE gt_vbak INTO ls_vbak1 INDEX 1.
**    SELECT SINGLE kvgr2
**      FROM vbak
**      INTO lv_kvgr2_tmp
**      WHERE vbeln EQ wa_vbak-vbeln
**        AND kvgr2 EQ 'ZR5'.
**    IF sy-subrc = 0.
**      CLEAR lv_message.
**      PERFORM f_check_pdc USING lv_message.
**    ELSE.
**      PERFORM f_check_payin.
**    ENDIF.
*
*    IF lv_message IS INITIAL.
*
*      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*        EXPORTING
*          formname           = gc_form_whsales
*        IMPORTING
*          fm_name            = gv_fm_name
*        EXCEPTIONS
*          no_form            = 1
*          no_function_module = 2
*          OTHERS             = 3.
*    ELSE.
*      MESSAGE i000 WITH lv_message.
*    ENDIF.
*  ENDIF.

*  IF r3 EQ 'X'.
*    gwa_output_options-tdcopies = 3.
*  ELSE.
*    CLEAR gwa_output_options-tdcopies.
*  ENDIF.

  IF LV_CONTROL-NO_DIALOG = SPACE.
    LV_OPTION-TDTITLE = TEXT-002.
  ENDIF.

*  gwa_control_parameters-no_open = space.
*    gwa_control_parameters-no_close = 'X'.


  IF LV_MESSAGE IS INITIAL.
* Function module /1BCDWB/SF00000086
    CALL FUNCTION GV_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = TOA_DARA
        ARCHIVE_PARAMETERS = ARC_PARAMS
        CONTROL_PARAMETERS = GWA_CONTROL_PARAMETERS
        OUTPUT_OPTIONS     = GWA_OUTPUT_OPTIONS
        USER_SETTINGS      = ' '
*    IMPORTING
*       job_output_info    = t_output_info
*    TABLES
*       gt_vbak            = gt_vbak
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

*  ENDIF.

  ENDIF.

*--padd

  DATA: LV_KVGR2 TYPE VBAK-KVGR2.

  SELECT SINGLE KVGR2
  FROM VBAK
  INTO LV_KVGR2
  WHERE VBELN EQ WA_VBAK-VBELN.



  IF LV_KVGR2 EQ   'ZR3'.
*--padd


*  DATA: lv_option TYPE ssfcompop,
*            lv_control TYPE ssfctrlop.
* Get function module name of smart form
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = 'ZSF_SD_ORDER_CONFIRMA_CEILING'
      IMPORTING
        FM_NAME            = GV_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.



    IF LV_CONTROL-NO_DIALOG = SPACE.
      LV_OPTION-TDTITLE = TEXT-002.
    ENDIF.


*  gwa_control_parameters-no_open = 'X'.
*  gwa_control_parameters-no_close = space.
*
    IF GV_TABIX EQ GV_LAST_ITEM.
*        if wa_vbak-vbeln ne gwa_first-vbeln.
      GWA_CONTROL_PARAMETERS-NO_OPEN = 'X'.
*        endif.
      GWA_CONTROL_PARAMETERS-NO_CLOSE = ' '.
    ELSE.
*        if gv_tabix ne 1.
*                 if ( wa_vbak-auart ne 'ZS04' and
*                      wa_vbak-auart ne 'ZS05' ).
      GWA_CONTROL_PARAMETERS-NO_OPEN = 'X'.
      GWA_CONTROL_PARAMETERS-NO_CLOSE = 'X'.
*                 else.
*                   gwa_control_parameters-no_open = 'X'.
*                   gwa_control_parameters-no_close = ' '.
*                 endif.
*        endif.
    ENDIF.



* Function module /1BCDWB/SF00000086
    CALL FUNCTION GV_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = TOA_DARA
        ARCHIVE_PARAMETERS = ARC_PARAMS
        CONTROL_PARAMETERS = GWA_CONTROL_PARAMETERS
        OUTPUT_OPTIONS     = GWA_OUTPUT_OPTIONS
        USER_SETTINGS      = ' '
*    IMPORTING
*       job_output_info    = t_output_info
*    TABLES
*       gt_vbak            = gt_vbak
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


  ENDIF.  "padd


ENDFORM.                    " F_PRINT_FORM
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT_HEAD_PAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LINES  text
*      -->P_2265   text
*      -->P_2266   text
*      -->P_LWA_HEADER_VBELN  text
*----------------------------------------------------------------------*
FORM READ_TEXT_HEAD_PAYIN  TABLES   P_IT_LINES
                                      "Insert correct name for <...>
                           USING    P_ID
                                    P_OBJECT
                                    P_VBELN.
  CLEAR: IT_LINES[].
  V_NAME = P_VBELN.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = P_ID
      LANGUAGE                = SY-LANGU
      NAME                    = V_NAME
      OBJECT                  = P_OBJECT
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

ENDFORM.                    " READ_TEXT_HEAD_PAYIN
*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_get_selection .
*
*  CONSTANTS : lv_program   TYPE string VALUE 'ZT_SD_ORDER_CONFIRM',
*              lv_program_o TYPE string VALUE 'Z_SD_ORDER_CONFIRM_O',
*              lv_whsales   TYPE string VALUE 'ZT_SD_ORDER_WHSALE'.
*
*  IF sy-tcode = lv_program.
*    r1 = 'X'.
*    CLEAR : r2,r3.
*  ELSEIF sy-tcode = lv_program_o.
*    r2 = 'X'.
*    CLEAR : r1,r3.
*  ELSEIF sy-tcode = lv_whsales.
*    r3 = 'X'.
*    CLEAR : r1,r2.
*  ELSE.
*    r1 = 'X'.
*    CLEAR : r2,r3.
*  ENDIF.
*
*  LOOP AT SCREEN.
*    IF screen-group1 = 'MOD'.
*      screen-intensified = '1'.
*      screen-active      = '0'.
*      screen-display_3d  = '1'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.                    " F_GET_SELECTION
*&---------------------------------------------------------------------*
*&      Form  F_CALL_VKM3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_VKM3 .
  CALL FUNCTION 'SD_ORDER_CREDIT_RELEASE'
    EXPORTING
      VBELN         = WA_VBAK-VBELN
    EXCEPTIONS
      ERROR_MESSAGE = 1.
  COMMIT WORK AND WAIT.
  MESSAGE S000 WITH 'Process Compleated'.
ENDFORM.                    " F_CALL_VKM3
*&---------------------------------------------------------------------*
*&      Form  F_GET_PROFIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTSD_PROFIT  text
*----------------------------------------------------------------------*
FORM F_GET_PROFIT TABLES FT_ZTSD_PROFIT STRUCTURE ZSDSSDT006
                         FT_CHECK_PROFIT.

  DATA : LT_ZTSD_PROFIT TYPE TABLE OF ZSDSSDT006,
         LS_ZTSD_PROFIT TYPE ZSDSSDT006.

  DATA : BEGIN OF LS_CHECK_PROFIT,
           VBELN TYPE VBAK-VBELN,
         END OF LS_CHECK_PROFIT.
  DATA LT_CHECK_PROFIT LIKE TABLE OF LS_CHECK_PROFIT.

  DATA : BEGIN OF LS_VBAP,
           VBELN TYPE VBAP-VBELN,
           POSNR TYPE VBAP-POSNR,
           PRCTR TYPE VBAP-PRCTR,
           VKBUR TYPE VBAK-VKBUR,
           VKGRP TYPE VBAK-VKGRP,
         END OF LS_VBAP.
  DATA LT_VBAP LIKE TABLE OF LS_VBAP.

  SELECT VBAP~VBELN
         VBAP~POSNR
         VBAP~PRCTR
         VBAK~VKBUR
         VBAK~VKGRP
    FROM VBAP
    INNER JOIN VBAK ON VBAK~VBELN EQ VBAP~VBELN
    INTO TABLE LT_VBAP
    FOR ALL ENTRIES IN GT_VBAK
    WHERE VBAK~VBELN EQ GT_VBAK-VBELN.

  SELECT *
    FROM ZSDSSDT006
    INTO TABLE FT_ZTSD_PROFIT
    FOR ALL ENTRIES IN GT_VBAK
    WHERE VKBUR EQ GT_VBAK-VKBUR
      AND VKGRP EQ GT_VBAK-VKGRP.

  LT_ZTSD_PROFIT[] = FT_ZTSD_PROFIT[].

  LOOP AT LT_VBAP INTO LS_VBAP.
    READ TABLE LT_ZTSD_PROFIT INTO LS_ZTSD_PROFIT
    WITH KEY VKBUR = LS_VBAP-VKBUR
             VKGRP = LS_VBAP-VKGRP.
    IF SY-SUBRC NE 0.
      LS_CHECK_PROFIT-VBELN = LS_VBAP-VBELN.
      APPEND LS_CHECK_PROFIT TO LT_CHECK_PROFIT.
    ENDIF.
    CLEAR : LS_ZTSD_PROFIT,LS_CHECK_PROFIT,LS_VBAP.
  ENDLOOP.

  SORT LT_CHECK_PROFIT BY VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_CHECK_PROFIT COMPARING ALL FIELDS.

  FT_CHECK_PROFIT[] = LT_CHECK_PROFIT[].

ENDFORM.                    " F_GET_PROFIT
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_PAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_PAYIN.
*  DATA : ls_vbak     TYPE typ_vbak,
*         ls_vbak_tmp TYPE typ_vbak.
*
*  CONSTANTS : lc_zterm TYPE c LENGTH 4 VALUE 'A000',
*              lc_vtweg TYPE c LENGTH 2 VALUE '10'.
*
*  DATA : lt_reamin TYPE TABLE OF ztsd_check_payin,
*         ls_reamin TYPE ztsd_check_payin.
*
*  DATA : lt_ztsd_check_payin TYPE TABLE OF ztsd_check_payin,
*         ls_ztsd_check_payin TYPE ztsd_check_payin.
*
*  RANGES : lr_auart FOR vbak-auart,
*           lr_vkbur FOR vbak-vkbur.
*
*  PERFORM f_get_order_type TABLES lr_auart[].
*  PERFORM f_get_sales_offi TABLES lr_vkbur[].
*
*  DATA : lv_check     TYPE c,
*         lv_kunnr_tmp TYPE kunnr,
*         lv_line      TYPE i.
*
*  DATA : lv_confirmation.
*
*  CLEAR lv_line.
*  LOOP AT gt_vbak INTO ls_vbak_tmp.
*    ADD 1 TO lv_line.
*
*  ENDLOOP.
*
*  READ TABLE gt_vbak INTO ls_vbak
*  WITH KEY  zterm = lc_zterm
*            vtweg = lc_vtweg.
*  IF sy-subrc = 0 AND ls_vbak-auart IN lr_auart AND ls_vbak-vkbur NOT IN lr_vkbur.
*
*    IF lv_line > 1 .
*      MESSAGE i000 WITH 'Can not print over than 1 Document'.
*      LEAVE TO TRANSACTION 'ZT_SD_ORDER_CONFIRM'.
*    ENDIF.
*
*    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*      EXPORTING
*        textline1 = 'Would you like to mathcing payin with sales order?'
*        titel     = 'Use Pay In'
*      IMPORTING
*        answer    = lv_confirmation.
*    IF lv_confirmation = 'J'.
*      DELETE FROM ztsd_check_payin WHERE runid = '999'
*                                     AND waipi = 'X'
*                                     AND vbeln = ls_vbak-vbeln.
*
*      lt_reamin = lt_ztsd_check_payin.
*      PERFORM f_get_detail TABLES lt_ztsd_check_payin
*                         CHANGING lv_check .
*    ELSE.
*      PERFORM f_show_error USING ls_vbak.
*
*    ENDIF.
**    ENDAT.
**  ENDLOOP.
*  ELSE.
*    IF ls_vbak-vbeln IS NOT INITIAL.
*      DELETE FROM ztsd_check_payin WHERE waipi = 'X'
*                                     AND vbeln = ls_vbak-vbeln.
*      COMMIT WORK AND WAIT.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " F_CHECK_PAYIN
*&---------------------------------------------------------------------*
*&      Form  F_GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_get_detail TABLES lt_ztsd_check_payin STRUCTURE ztsd_check_payin
*                CHANGING lv_check.
*
*  DATA : BEGIN OF ls_ztb_pay_in,
*           payno  TYPE ztb_pay_in-payno,
*           kunnr  TYPE ztb_pay_in-kunnr,
*           payamt TYPE ztb_pay_in-payamt,
*           expen  TYPE ztb_pay_in-expen,
*           incom  TYPE ztb_pay_in-incom,
*           rempi  TYPE ztb_pay_in-rempi,
*         END OF ls_ztb_pay_in.
*  DATA lt_ztb_pay_in LIKE TABLE OF ls_ztb_pay_in.
*
*  DATA : BEGIN OF ls_ztsd_check_payin_tmp,
*           payno        TYPE ztsd_check_payin-payno,
*           runid_payin  TYPE ztsd_check_payin-runid_payin,
*           kunnr        TYPE ztsd_check_payin-kunnr,
*           netwr        TYPE ztsd_check_payin-netwr,
*           payamt       TYPE ztsd_check_payin-payamt,
*           payamt_check TYPE ztsd_check_payin-payamt_check,
*           netwr_check  TYPE ztsd_check_payin-netwr_check,
*         END OF ls_ztsd_check_payin_tmp.
*  DATA : lt_ztsd_check_payin_tmp   LIKE TABLE OF ls_ztsd_check_payin_tmp,
*         ls_ztsd_check_payin_tmp_a LIKE ls_ztsd_check_payin_tmp.
*
*  DATA : BEGIN OF ls_ztsd_check_payin_tmp_1,
*           payno        TYPE ztsd_check_payin-payno,
*           runid_payin  TYPE ztsd_check_payin-runid_payin,
*           kunnr        TYPE ztsd_check_payin-kunnr,
*           netwr        TYPE ztsd_check_payin-netwr,
*           payamt       TYPE ztsd_check_payin-payamt,
*           payamt_check TYPE ztsd_check_payin-payamt_check,
*           netwr_check  TYPE ztsd_check_payin-netwr_check,
*         END OF ls_ztsd_check_payin_tmp_1.
*  DATA : lt_ztsd_check_payin_tmp_1 LIKE TABLE OF ls_ztsd_check_payin_tmp_1,
*         lt_ztsd_check_payin_tmp_2 LIKE TABLE OF ls_ztsd_check_payin_tmp_1,
*         ls_ztsd_check_payin_tmp_2 LIKE ls_ztsd_check_payin_tmp_1.
*
*  DATA : BEGIN OF ls_vbap,
*           vbeln TYPE vbap-vbeln,
*           posnr TYPE vbap-posnr,
*           netwr TYPE vbap-netwr,
*           mwsbp TYPE vbap-mwsbp,
*         END OF ls_vbap.
*  DATA lt_vbap LIKE TABLE OF ls_vbap.
*
*  DATA : BEGIN OF ls_check_amount,
*           vbeln TYPE vbap-vbeln,
*           edatu TYPE vbep-edatu,
*           total TYPE vbap-netwr,
*         END OF ls_check_amount.
*  DATA lt_check_amount LIKE TABLE OF ls_check_amount.
*
*  DATA : ls_ztsd_check_payin TYPE ztsd_check_payin,
*         lt_insert_data      TYPE TABLE OF ztsd_check_payin,
*         ls_insert_data      TYPE ztsd_check_payin.
*
*  DATA : lv_run          TYPE i,
*         lv_reamin_netwr TYPE vbak-netwr,
*         lv_check_change TYPE c.
*
*  DATA ls_item2 TYPE typ_item2.
*
*  DATA : lv_tabix      TYPE sy-tabix,
*         lv_change_all TYPE c.
*
*  DATA : ls_ztsd_line_payin       TYPE ztsd_line_payin,
*         lt_ztsd_line_payin       TYPE TABLE OF ztsd_line_payin,
*         ls_ztsd_line_payin_check TYPE ztsd_line_payin,
*         lt_ztsd_line_payin_check TYPE TABLE OF ztsd_line_payin.
*
*  DATA : lt_update TYPE TABLE OF ztsd_line_payin,
*         ls_update TYPE ztsd_line_payin,
*         lt_insert TYPE TABLE OF ztsd_line_payin,
*         ls_insert TYPE ztsd_line_payin.
*
*  DATA : lt_item2 LIKE gt_item2,
*         ls_item1 TYPE typ_item.
*
*  lt_item2[] = gt_item2[].
*
*  LOOP AT gt_item1 INTO ls_item1.
*    READ TABLE gt_item2 INTO ls_item2
*    WITH KEY vbeln = ls_item1-vbeln
*             posnr = ls_item1-posnr.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
*    ls_item2-vbeln   = ls_item1-vbeln.
*    ls_item2-posnr_1 = ls_item1-posnr.
*    ls_item2-edatu   = ls_item1-edatu.
*
*    APPEND ls_item2 TO lt_item2.
*
*  ENDLOOP.
*
*  SORT lt_item2 BY posnr_1.
*  DELETE ADJACENT DUPLICATES FROM lt_item2 COMPARING posnr_1.
*
*
*
*  IF lt_item2 IS NOT INITIAL.
*    SELECT vbeln
*           posnr
*           netwr
*           mwsbp
*      FROM vbap
*      INTO TABLE lt_vbap
*      FOR ALL ENTRIES IN lt_item2
*      WHERE vbeln = lt_item2-vbeln
*        AND posnr = lt_item2-posnr_1.
*
*    SELECT *
*      FROM ztsd_line_payin
*      INTO TABLE lt_ztsd_line_payin_check
*      FOR ALL ENTRIES IN lt_item2
*      WHERE vbeln = lt_item2-vbeln
*        AND posnr = lt_item2-posnr_1.
*
*    PERFORM f_check_change_date TABLES lt_ztsd_line_payin_check.
*
*    LOOP AT lt_item2 INTO ls_item2. "If New Logic not problem, This loop will be deleted.
*      READ TABLE lt_vbap INTO ls_vbap
*      WITH KEY vbeln = ls_item2-vbeln
*               posnr = ls_item2-posnr_1.
*      IF sy-subrc = 0.
*        ls_check_amount-vbeln = ls_item2-vbeln.
*        ls_check_amount-edatu = ls_item2-edatu.
*        ls_check_amount-total = ls_vbap-netwr + ( ( ls_vbap-netwr * 7 ) / 100 ). "ls_vbap-mwsbp.
*        COLLECT ls_check_amount INTO lt_check_amount.
*      ENDIF.
*
*      READ TABLE lt_ztsd_line_payin_check INTO ls_ztsd_line_payin_check
*      WITH KEY vbeln = ls_item2-vbeln
*               posnr = ls_item2-posnr_1
*               edatu = ls_item2-posnr_1.
*      IF sy-subrc = 0.
*        lv_change_all = 'X'.
*      ENDIF.
*
*      READ TABLE lt_ztsd_line_payin_check INTO ls_ztsd_line_payin_check
*      WITH KEY vbeln = ls_item2-vbeln
*               posnr = ls_item2-posnr_1.
*      IF sy-subrc = 0.
*        IF ls_ztsd_line_payin_check-total NE ls_check_amount-total OR
*           ls_ztsd_line_payin_check-edatu NE ls_check_amount-edatu.
*
*          ls_ztsd_line_payin-vbeln = ls_item2-vbeln.
*          ls_ztsd_line_payin-posnr = ls_item2-posnr_1.
*          ls_ztsd_line_payin-edatu = ls_item2-edatu.
*          ls_ztsd_line_payin-total = ls_check_amount-total.
*          APPEND ls_ztsd_line_payin TO lt_update.
*          "UPDATE ztsd_line_payin FROM ls_ztsd_line_payin.
*        ENDIF.
*        ls_ztsd_line_payin-vbeln = ls_item2-vbeln.
*        ls_ztsd_line_payin-posnr = ls_item2-posnr_1.
*        ls_ztsd_line_payin-edatu = ls_item2-edatu.
*        ls_ztsd_line_payin-total = ls_check_amount-total.
*        ls_ztsd_line_payin-payin = ls_ztsd_line_payin_check-payin.
*        APPEND ls_ztsd_line_payin TO lt_ztsd_line_payin.
*      ELSE.
*        ls_ztsd_line_payin-vbeln = ls_item2-vbeln.
*        ls_ztsd_line_payin-posnr = ls_item2-posnr_1.
*        ls_ztsd_line_payin-edatu = ls_item2-edatu.
*        ls_ztsd_line_payin-total = ls_check_amount-total.
*        ls_ztsd_line_payin-payin = ls_ztsd_line_payin_check-payin.
*        APPEND ls_ztsd_line_payin TO lt_ztsd_line_payin.
*        APPEND ls_ztsd_line_payin TO lt_insert.
*        "INSERT ztsd_line_payin FROM ls_ztsd_line_payin.
*      ENDIF.
*      CLEAR : ls_item2,ls_vbap.
*    ENDLOOP.
*  ENDIF.
*
*  SORT lt_ztsd_check_payin_tmp BY payno runid_payin DESCENDING.
*
*  DATA lv_check_amount TYPE c.
*  PERFORM f_check_change TABLES lt_check_amount
*                                lt_item2
*                                lt_ztsd_line_payin_check
*                                lt_vbap
*                                lt_update
*                                lt_insert
*                          USING lv_change_all
*                       CHANGING lv_check_change
*                                lv_check_amount.
*  IF lv_check_change = 'X'.
*
*    PERFORM f_get_data_payin.
*    PERFORM f_show_data.
*
*    LOOP AT lt_update INTO ls_update.
*      UPDATE ztsd_line_payin FROM ls_update.
*      CLEAR ls_update.
*    ENDLOOP.
*
*    LOOP AT lt_insert INTO ls_insert.
*      INSERT ztsd_line_payin FROM ls_insert.
*      CLEAR ls_insert.
*    ENDLOOP.
*
*    IF gt_payin_alv IS NOT INITIAL.
*
*      SELECT payno
*             kunnr
*             payamt
*             expen
*             incom
*             rempi
*      FROM ztb_pay_in
*      INTO TABLE lt_ztb_pay_in
*      FOR ALL ENTRIES IN gt_payin_alv
*      WHERE payno       EQ gt_payin_alv-payno
*        AND used        NE 'X'
*        AND pmnt_typ    EQ '02'
*        AND delete_flag NE 'X'
*        AND payref      EQ space.
*
*      LOOP AT lt_ztb_pay_in INTO ls_ztb_pay_in.
*        lv_tabix = sy-tabix.
*        ls_ztb_pay_in-payamt = ( ls_ztb_pay_in-payamt - ls_ztb_pay_in-incom ) + ls_ztb_pay_in-expen.
*        MODIFY lt_ztb_pay_in FROM ls_ztb_pay_in INDEX lv_tabix
*                                         TRANSPORTING payamt.
*        CLEAR ls_ztb_pay_in.
*      ENDLOOP.
*
*      IF lt_ztsd_line_payin[] IS NOT INITIAL.
*        SELECT payno
*               runid_payin
*               kunnr
*               netwr
*               payamt
*               payamt_check
*               netwr_check
*          FROM ztsd_check_payin
*          INTO TABLE lt_ztsd_check_payin_tmp_1
*          FOR ALL ENTRIES IN lt_ztsd_line_payin
*          WHERE vbeln = lt_ztsd_line_payin-vbeln
*            AND waipi NE 'X'.
*
*        IF lt_ztsd_check_payin_tmp_1[] IS NOT INITIAL.
*          SELECT payno
*                 runid_payin
*                 kunnr
*                 netwr
*                 payamt
*                 payamt_check
*                 netwr_check
*            FROM ztsd_check_payin
*            INTO TABLE lt_ztsd_check_payin_tmp_2
*            FOR ALL ENTRIES IN lt_ztsd_check_payin_tmp_1
*            WHERE kunnr EQ lt_ztsd_check_payin_tmp_1-kunnr
*              AND payno EQ lt_ztsd_check_payin_tmp_1-payno
*              AND waipi NE 'X'.
*
*          lt_ztsd_check_payin_tmp[] = lt_ztsd_check_payin_tmp_2[].
*
*        ENDIF.
*
*      ENDIF.
*
*      SORT lt_ztsd_check_payin_tmp BY payno runid_payin DESCENDING.
*
*      PERFORM f_check_remain_payin TABLES lt_ztsd_check_payin_tmp
*                                          lt_check_amount
*                                          lt_ztb_pay_in
*                                          lt_ztsd_line_payin
*                                          lt_item2
*                                          lt_ztsd_line_payin_check
*                                    USING lv_check_amount .
*    ELSE.
*      IF lt_ztsd_line_payin_check IS INITIAL.
*        CLEAR ls_ztsd_line_payin.
*        LOOP AT lt_ztsd_line_payin INTO ls_ztsd_line_payin.
*          DELETE FROM ztsd_line_payin WHERE vbeln EQ ls_ztsd_line_payin-vbeln
*                                        AND posnr EQ ls_ztsd_line_payin-posnr.
*          CLEAR ls_ztsd_line_payin.
*        ENDLOOP.
*      ELSE.
*        CLEAR ls_ztsd_line_payin_check.
*        LOOP AT lt_ztsd_line_payin_check  INTO ls_ztsd_line_payin_check.
*          UPDATE ztsd_line_payin FROM ls_ztsd_line_payin_check.
*          CLEAR ls_ztsd_line_payin_check.
*        ENDLOOP.
*      ENDIF.
*      LEAVE TO TRANSACTION 'ZT_SD_ORDER_CONFIRM'.
*    ENDIF.
*  ENDIF.
*
*
*ENDFORM.                    " F_GET_DETAIL
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_PAYIN_TRAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTSD_CHECK_PAYIN  text
*----------------------------------------------------------------------*
*FORM f_modify_payin_tran  TABLES lt_ztsd_check_payin STRUCTURE ztsd_check_payin
*                           USING fs_ztb_pay_in.
*
*  DATA : BEGIN OF ls_ztb_pay_in,
*           payno  TYPE ztb_pay_in-payno,
*           kunnr  TYPE ztb_pay_in-kunnr,
*           payamt TYPE ztb_pay_in-payamt,
*           rempi  TYPE ztb_pay_in-rempi,
*         END OF ls_ztb_pay_in.
*
*  DATA : ls_ztsd_check_payin     TYPE ztsd_check_payin,
*         ls_ztsd_check_payin_tmp TYPE ztsd_check_payin.
*
*  ls_ztb_pay_in = fs_ztb_pay_in.
*
*  DATA lv_runid TYPE i.
*  CLEAR lv_runid.
*  READ TABLE lt_ztsd_check_payin INTO ls_ztsd_check_payin
*  WITH KEY kunnr = ls_ztb_pay_in-kunnr.
*  IF ls_ztsd_check_payin-netwr GT 0.
*
*    ls_ztsd_check_payin_tmp-vbeln        = ls_ztsd_check_payin-vbeln.
*    ls_ztsd_check_payin_tmp-runid        = ls_ztsd_check_payin-runid + 1.
*    ls_ztsd_check_payin_tmp-payno        = ls_ztb_pay_in-payno.
*    ls_ztsd_check_payin_tmp-kunnr        = ls_ztb_pay_in-kunnr.
*    ls_ztsd_check_payin_tmp-payamt_check = ls_ztb_pay_in-payamt.
*    ls_ztsd_check_payin_tmp-payamt       = ls_ztb_pay_in-payamt - ls_ztsd_check_payin-netwr.
*    ls_ztsd_check_payin_tmp-runid_payin  = 1.
*    IF ls_ztsd_check_payin_tmp-payamt LT 0.
*      UPDATE ztb_pay_in SET used = 'X'
*                      WHERE payno = ls_ztb_pay_in-payno
*                        AND kunnr = ls_ztb_pay_in-kunnr.
*      ls_ztsd_check_payin_tmp-payamt = 0.
*    ELSEIF ls_ztsd_check_payin_tmp-payamt EQ 0.
*      UPDATE ztb_pay_in SET used = 'X'
*                      WHERE payno = ls_ztb_pay_in-payno
*                        AND kunnr = ls_ztb_pay_in-kunnr.
*    ENDIF.
*
*    ls_ztsd_check_payin_tmp-netwr        = ls_ztsd_check_payin-netwr  - ls_ztb_pay_in-payamt.
*
*    IF ls_ztsd_check_payin_tmp-netwr LT 0.
*      ls_ztsd_check_payin_tmp-netwr = 0.
*    ENDIF.
*
*    ls_ztsd_check_payin_tmp-netwr_check  = ls_ztsd_check_payin_tmp-payamt_check - ls_ztsd_check_payin_tmp-payamt.
*    ls_ztsd_check_payin_tmp-erdat        = sy-datum.
*    ls_ztsd_check_payin_tmp-ernam        = sy-uname.
*
*    APPEND ls_ztsd_check_payin_tmp TO lt_ztsd_check_payin.
*
*    SORT lt_ztsd_check_payin BY payno DESCENDING runid_payin DESCENDING.
*  ENDIF.
*
*ENDFORM.                    " F_MODIFY_PAYIN_TRAN
*&---------------------------------------------------------------------*
*&      Form  F_USE_PAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CHECK_AMOUNT  text
*      -->P_LT_ZTB_PAY_IN  text
*----------------------------------------------------------------------*
*FORM f_use_payin  TABLES ft_check_amount
*                         ft_ztb_pay_in.
*  DATA : BEGIN OF ls_ztb_pay_in,
*           payno  TYPE ztb_pay_in-payno,
*           kunnr  TYPE ztb_pay_in-kunnr,
*           payamt TYPE ztb_pay_in-payamt,
*           expen  TYPE ztb_pay_in-expen,
*           incom  TYPE ztb_pay_in-incom,
*           rempi  TYPE ztb_pay_in-rempi,
*         END OF ls_ztb_pay_in.
*  DATA : lt_ztb_pay_in     LIKE TABLE OF ls_ztb_pay_in,
*         ls_ztb_pay_in_tmp LIKE ls_ztb_pay_in.
*
*  DATA : BEGIN OF ls_check_amount,
*           vbeln TYPE vbap-vbeln,
*           edatu TYPE vbep-edatu,
*           total TYPE vbap-netwr,
*         END OF ls_check_amount.
*  DATA lt_check_amount LIKE TABLE OF ls_check_amount.
*
*  DATA : lt_insert_data TYPE TABLE OF ztsd_check_payin,
*         ls_insert_data TYPE ztsd_check_payin.
*
*  DATA : lv_tabix    TYPE sy-tabix,
*         lv_tabix_o  TYPE sy-tabix,
*         lv_line     TYPE ztsd_check_payin-runid,
*         lv_line_pay TYPE ztsd_check_payin-runid_payin,
*         lv_check    TYPE c,
*         lv_payamt   TYPE ztsd_check_payin-payamt.
*
*  lt_check_amount[] = ft_check_amount[].
*  lt_ztb_pay_in[]   = ft_ztb_pay_in[].
*
*  SORT lt_ztb_pay_in BY payno kunnr.
*
*
*
*  LOOP AT lt_check_amount INTO ls_check_amount.
*    lv_tabix_o = sy-tabix.
*    AT NEW vbeln.
*      CLEAR lv_line.
*    ENDAT.
*
*    SELECT MAX( runid )
*    FROM ztsd_check_payin
*    INTO lv_line
*    WHERE vbeln = ls_check_amount-vbeln.
*
*
*
*    LOOP AT lt_ztb_pay_in INTO ls_ztb_pay_in_tmp.
*      lv_tabix = sy-tabix.
*
*      MOVE-CORRESPONDING ls_ztb_pay_in_tmp TO ls_ztb_pay_in.
*      IF ls_ztb_pay_in-rempi = 0.
*        ls_ztb_pay_in-rempi = ls_ztb_pay_in-payamt.
*      ENDIF.
*
*      AT NEW payno.
*        CLEAR lv_line_pay.
*      ENDAT.
*
*      IF lv_line_pay IS INITIAL.
*        SELECT MAX( runid_payin )
*        FROM ztsd_check_payin
*        INTO lv_line_pay
*        WHERE vbeln = ls_check_amount-vbeln.
*      ENDIF.
*
*      ADD 1 TO lv_line.
*      ADD 1 TO lv_line_pay.
*
*      ls_insert_data-vbeln        = ls_check_amount-vbeln.
*      ls_insert_data-runid        = lv_line.
*      ls_insert_data-edatu        = ls_check_amount-edatu.
*      ls_insert_data-runid_payin  = lv_line_pay.
*      ls_insert_data-payno        = ls_ztb_pay_in-payno.
*      ls_insert_data-kunnr        = ls_ztb_pay_in-kunnr.
*      lv_payamt                   = ls_ztb_pay_in-rempi.
*
*      ls_insert_data-payamt_check = ls_ztb_pay_in-rempi.
*      ls_insert_data-netwr_check  = ls_ztb_pay_in-rempi - ls_check_amount-total.
*
*      IF ls_insert_data-netwr_check LE 0.
*        ls_insert_data-netwr_check = ls_ztb_pay_in-rempi.
*      ELSE.
*        ls_insert_data-netwr_check = ls_check_amount-total.
*      ENDIF.
*
*      ls_insert_data-payamt       = ls_insert_data-payamt_check - ls_check_amount-total.
*      IF ls_insert_data-payamt LE 0.
*        ls_insert_data-payamt = 0.
*
*        DELETE lt_ztb_pay_in INDEX lv_tabix.
*
*      ELSE.
*        ls_ztb_pay_in-rempi = ls_insert_data-payamt.
*
*        MODIFY lt_ztb_pay_in FROM ls_ztb_pay_in INDEX lv_tabix
*                                         TRANSPORTING rempi.
*      ENDIF.
*
*      ls_check_amount-total = ls_check_amount-total - lv_payamt.
*
*      APPEND ls_insert_data TO lt_insert_data.
*
*      IF ls_check_amount-total LE 0.
*        EXIT.
*      ENDIF.
*
*      CLEAR : ls_insert_data,ls_ztb_pay_in.
*    ENDLOOP.
*
*    IF ls_check_amount-total GT 0 .
*      lv_check = 'X'.
*      EXIT.
*    ENDIF.
*    CLEAR ls_check_amount.
*  ENDLOOP.
*
*  IF lv_check NE 'X'.
*    DELETE FROM ztsd_check_payin WHERE waipi = 'X'.
*    COMMIT WORK AND WAIT .
*
*    LOOP AT lt_insert_data INTO ls_insert_data.
*      IF ls_insert_data-payamt LE 0.
*        ls_insert_data-payamt = 0.
*        ls_ztb_pay_in-rempi   = 0.
*        UPDATE ztb_pay_in SET used  = 'X'
*                              rempi = ls_ztb_pay_in-rempi
*                        WHERE payno = ls_insert_data-payno
*                          AND kunnr = ls_insert_data-kunnr.
*
*      ELSE.
*        ls_ztb_pay_in-payamt = ls_insert_data-payamt.
*        ls_ztb_pay_in-rempi  = ls_ztb_pay_in-payamt.
*        UPDATE ztb_pay_in SET rempi = ls_ztb_pay_in-rempi
*                        WHERE payno = ls_insert_data-payno
*                          AND kunnr = ls_insert_data-kunnr.
*
*      ENDIF.
*
*      INSERT ztsd_check_payin FROM ls_insert_data.
*    ENDLOOP.
*
*  ELSE.
*    READ TABLE lt_insert_data INTO ls_insert_data INDEX 1.
*    ls_insert_data-vbeln = ls_insert_data-vbeln.
*    ls_insert_data-runid = '0'.
*    ls_insert_data-waipi = 'X'.
*    INSERT ztsd_check_payin FROM ls_insert_data.
*  ENDIF.
*
*ENDFORM.                    " F_USE_PAYIN
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_REMAIN_PAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_check_remain_payin TABLES ft_ztsd_check_payin_tmp
*                                 ft_check_amount
*                                 ft_ztb_pay_in
*                                 lt_ztsd_line_payin STRUCTURE ztsd_line_payin
*                                 ft_item2
*                                 lt_ztsd_line_payin_check STRUCTURE ztsd_line_payin
*                           USING lv_check_amount.
*
*  DATA : lt_item2 LIKE gt_item2,
*         ls_item2 TYPE typ_item2.
*
*  DATA : ls_ztsd_line_payin_check TYPE ztsd_line_payin.
*
*  DATA : BEGIN OF ls_ztsd_check_payin,
*           payno        TYPE ztsd_check_payin-payno,
*           runid_payin  TYPE ztsd_check_payin-runid_payin,
*           kunnr        TYPE ztsd_check_payin-kunnr,
*           netwr        TYPE ztsd_check_payin-netwr,
*           payamt       TYPE ztsd_check_payin-payamt,
*           payamt_check TYPE ztsd_check_payin-payamt_check,
*           netwr_check  TYPE ztsd_check_payin-netwr_check,
*         END OF ls_ztsd_check_payin.
*  DATA : lt_ztsd_check_payin_tmp LIKE TABLE OF ls_ztsd_check_payin,
*         ls_ztsd_check_payin_tmp LIKE ls_ztsd_check_payin.
*
*  DATA : BEGIN OF ls_ztb_pay_in,
*           payno  TYPE ztb_pay_in-payno,
*           kunnr  TYPE ztb_pay_in-kunnr,
*           payamt TYPE ztb_pay_in-payamt,
*           expen  TYPE ztb_pay_in-expen,
*           incom  TYPE ztb_pay_in-incom,
*           rempi  TYPE ztb_pay_in-rempi,
*         END OF ls_ztb_pay_in.
*  DATA : lt_ztb_pay_in     LIKE TABLE OF ls_ztb_pay_in,
*         ls_ztb_pay_in_tmp LIKE ls_ztb_pay_in.
*
*  DATA : BEGIN OF ls_check_amount,
*           vbeln TYPE vbap-vbeln,
*           edatu TYPE vbep-edatu,
*           total TYPE vbap-netwr,
*         END OF ls_check_amount.
*  DATA lt_check_amount LIKE TABLE OF ls_check_amount.
*
*  DATA : lt_insert_data     TYPE TABLE OF ztsd_check_payin,
*         ls_insert_data     TYPE ztsd_check_payin,
*         ls_insert_data_tmp TYPE ztsd_check_payin.
*
*  DATA : lv_tabix    TYPE sy-tabix,
*         lv_tabix_o  TYPE sy-tabix,
*         lv_line     TYPE ztsd_check_payin-runid,
*         lv_line_pay TYPE ztsd_check_payin-runid_payin,
*         lv_check    TYPE c,
*         lv_payamt   TYPE ztsd_check_payin-payamt,
*         lv_waiat    TYPE vbap-netwr,
*         lv_exp_inc  TYPE vbap-netwr,
*         lv_kunnr    TYPE kna1-kunnr,
*         lv_payno    TYPE ztsd_check_payin-payno.
*
*  DATA ls_ztsd_line_payin TYPE ztsd_line_payin.
*
*  lt_ztsd_check_payin_tmp[] = ft_ztsd_check_payin_tmp[].
*  lt_check_amount[]         = ft_check_amount[].
*  lt_ztb_pay_in[]           = ft_ztb_pay_in[].
*  lt_item2[]                = ft_item2[].
*
*
*  LOOP AT lt_ztsd_check_payin_tmp INTO ls_ztsd_check_payin_tmp.
*    READ TABLE gt_payin_alv INTO gs_payin_alv
*    WITH KEY payno = gs_payin_alv-payno.
*    IF sy-subrc NE 0.
*      CONTINUE.
*    ENDIF.
*
*    MOVE-CORRESPONDING ls_ztsd_check_payin_tmp TO ls_ztsd_check_payin.
*    AT NEW payno.
*      ls_ztb_pay_in-payno  = ls_ztsd_check_payin-payno .
*      ls_ztb_pay_in-kunnr  = ls_ztsd_check_payin-kunnr .
*      ls_ztb_pay_in-payamt = ls_ztsd_check_payin-payamt.
*      ls_ztb_pay_in-rempi  = ls_ztsd_check_payin-payamt.
*
*      DELETE lt_ztb_pay_in WHERE payno = ls_ztsd_check_payin-payno.
*      APPEND ls_ztb_pay_in TO lt_ztb_pay_in.
*    ENDAT.
*
*    CLEAR : gs_payin_alv,ls_ztsd_check_payin_tmp,ls_ztsd_check_payin.
*  ENDLOOP.
*
*  SORT lt_ztb_pay_in BY payno kunnr.
*
*  DELETE lt_ztb_pay_in WHERE payamt = 0.
*
*  LOOP AT lt_check_amount INTO ls_check_amount.
*    lv_tabix_o = sy-tabix.
*    AT NEW vbeln.
*      CLEAR lv_line.
*    ENDAT.
*
*    IF lv_line IS INITIAL.
*      SELECT MAX( runid )
*      FROM ztsd_check_payin
*      INTO lv_line
*      WHERE vbeln = ls_check_amount-vbeln.
*    ENDIF.
*
*
*    LOOP AT lt_ztb_pay_in INTO ls_ztb_pay_in_tmp.
*      lv_tabix = sy-tabix.
*
*      MOVE-CORRESPONDING ls_ztb_pay_in_tmp TO ls_ztb_pay_in.
**      IF ls_ztb_pay_in-rempi = 0.
**        ls_ztb_pay_in-rempi = ls_ztb_pay_in-payamt.
**      ENDIF.
*
**      AT NEW payno.
**        CLEAR lv_line_pay.
**      ENDAT.
*
*      IF lv_line_pay IS INITIAL.
*        SELECT MAX( runid_payin )
*        FROM ztsd_check_payin
*        INTO lv_line_pay
*        WHERE payno = ls_ztb_pay_in_tmp-payno
*          AND kunnr = ls_ztb_pay_in_tmp-kunnr.
*      ENDIF.
*
*      ADD 1 TO lv_line.
*      ADD 1 TO lv_line_pay.
*
*      ls_insert_data-vbeln        = ls_check_amount-vbeln.
*      ls_insert_data-runid        = lv_line.
*      ls_insert_data-edatu        = ls_check_amount-edatu.
*      ls_insert_data-runid_payin  = lv_line_pay.
*      ls_insert_data-payno        = ls_ztb_pay_in-payno.
*      ls_insert_data-kunnr        = ls_ztb_pay_in-kunnr.
*      lv_kunnr                    = ls_ztb_pay_in-kunnr.
*      lv_payamt                   = ls_ztb_pay_in-rempi.
*
*      ls_insert_data-payamt_check = ls_ztb_pay_in-rempi.
*      ls_insert_data-netwr_check  = ls_ztb_pay_in-rempi - ls_check_amount-total.
*
*      IF ls_insert_data-netwr_check LE 0.
*        ls_insert_data-netwr_check = ls_ztb_pay_in-rempi.
*      ELSE.
*        ls_insert_data-netwr_check = ls_check_amount-total.
*      ENDIF.
*
*      ls_insert_data-payamt       = ls_insert_data-payamt_check - ls_check_amount-total.
*      IF ls_insert_data-payamt LE 0.
*        ls_insert_data-payamt = 0.
*        CLEAR lv_line_pay.
*        DELETE lt_ztb_pay_in INDEX lv_tabix.
*      ELSE.
*        ls_ztb_pay_in-rempi = ls_insert_data-payamt.
*        MODIFY lt_ztb_pay_in FROM ls_ztb_pay_in INDEX lv_tabix
*                                         TRANSPORTING rempi.
*      ENDIF.
*
*      ls_check_amount-total = ls_check_amount-total - lv_payamt.
*
*      APPEND ls_insert_data TO lt_insert_data.
*
*      IF ls_check_amount-total LE 0.
*        EXIT.
*      ENDIF.
*
*      CLEAR : ls_insert_data,ls_ztb_pay_in.
*    ENDLOOP.
*
*    IF ls_check_amount-total GT 0 AND lv_kunnr EQ '0001000046'.
*      lv_waiat = ls_check_amount-total.
*      lv_check = 'X'.
*      EXIT.
*    ENDIF.
*
*    IF ls_check_amount-total GT 1.
*      lv_waiat = ls_check_amount-total.
*      lv_check = 'X'.
*      EXIT.
*    ELSEIF ls_check_amount-total GT 0 AND ls_check_amount-total LE 1.
*      lv_exp_inc = ls_check_amount-total.
*    ELSEIF ls_check_amount-total LT 0 AND ls_check_amount-total GE -1.
*      lv_exp_inc = ls_check_amount-total * -1.
*    ENDIF.
*    CLEAR ls_check_amount.
*  ENDLOOP.
*
*  IF lv_check NE 'X'.
*
*    DATA : lv_vbeln TYPE vbap-vbeln,
*           lv_payin TYPE c LENGTH 255.
*
*    DATA : BEGIN OF ls_payin,
*             payno TYPE ztsd_line_payin-payin,
*           END OF ls_payin.
*    DATA lt_payin LIKE TABLE OF ls_payin.
*
*    CLEAR ls_insert_data_tmp.
*    LOOP AT lt_insert_data INTO ls_insert_data_tmp .
*      MOVE-CORRESPONDING ls_insert_data_tmp TO ls_insert_data.
*      ls_payin-payno = ls_insert_data-payno.
*      APPEND ls_payin TO lt_payin.
*      lv_vbeln = ls_insert_data-vbeln.
*      AT NEW vbeln.
*        DELETE FROM ztsd_check_payin WHERE waipi = 'X'
*                                       AND vbeln = lv_vbeln.
*        COMMIT WORK AND WAIT .
*      ENDAT.
*
*      IF ls_insert_data-payamt LE 0.
*        ls_insert_data-payamt      = 0.
*        ls_ztb_pay_in-rempi        = 0.
*        AT END OF vbeln.
*          ls_ztb_pay_in-expen        = lv_exp_inc.
*          ls_insert_data-netwr_check = ls_insert_data-netwr_check + lv_exp_inc.
*        ENDAT.
*        UPDATE ztb_pay_in SET used  = 'X'
*                              rempi = ls_ztb_pay_in-rempi
*                              rmamt = ls_ztb_pay_in-rempi
*                              expen = ls_ztb_pay_in-expen
*                        WHERE payno = ls_insert_data-payno
*                          AND kunnr = ls_insert_data-kunnr.
*
*      ELSEIF ls_insert_data-payamt GT 0 AND ls_insert_data-payamt LE 1 AND
*             lv_kunnr NE '0001000046'.
*        ls_insert_data-payamt      = 0.
*        ls_ztb_pay_in-rempi        = 0.
*        ls_ztb_pay_in-incom        = lv_exp_inc.
*        ls_insert_data-netwr_check = ls_insert_data-netwr_check.
*        UPDATE ztb_pay_in SET used  = 'X'
*                              rempi = ls_ztb_pay_in-rempi
*                              rmamt = ls_ztb_pay_in-rempi
*                              incom = ls_ztb_pay_in-incom
*                        WHERE payno = ls_insert_data-payno
*                          AND kunnr = ls_insert_data-kunnr.
*
*      ELSE.
*        ls_ztb_pay_in-payamt = ls_insert_data-payamt.
*        ls_ztb_pay_in-rempi  = ls_ztb_pay_in-payamt.
*        UPDATE ztb_pay_in SET rempi = ls_ztb_pay_in-rempi
*                              rmamt = ls_ztb_pay_in-rempi
*                        WHERE payno = ls_insert_data-payno
*                          AND kunnr = ls_insert_data-kunnr.
*
*      ENDIF.
*
*      ls_insert_data-erdat = sy-datum.
*      ls_insert_data-ernam = sy-uname.
*
*      INSERT ztsd_check_payin FROM ls_insert_data.
*    ENDLOOP.
*
*    SORT lt_payin.
*    DELETE ADJACENT DUPLICATES FROM lt_payin.
*
*    LOOP AT lt_payin INTO ls_payin.
*      IF lv_payin IS INITIAL.
*        lv_payin = ls_payin-payno.
*      ELSE.
*        CONCATENATE lv_payin '|' ls_payin-payno INTO lv_payin.
*      ENDIF .
*      CLEAR ls_payin.
*    ENDLOOP.
*
*    DATA ls_ztsd_line_payin_tmp LIKE ls_ztsd_line_payin.
*    LOOP AT lt_ztsd_line_payin INTO ls_ztsd_line_payin_tmp.
*      MOVE-CORRESPONDING ls_ztsd_line_payin_tmp TO ls_ztsd_line_payin.
*
*      AT FIRST.
*        IF ls_ztsd_line_payin-payin IS INITIAL.
*          ls_ztsd_line_payin-payin = lv_payin.
*        ELSE.
*          CONCATENATE ls_ztsd_line_payin-payin '|' lv_payin INTO lv_payin.
*        ENDIF.
*      ENDAT.
*
*      UPDATE ztsd_line_payin SET waipi = space
*                                 payin = lv_payin
*                           WHERE vbeln = ls_ztsd_line_payin-vbeln
*                             AND posnr = ls_ztsd_line_payin-posnr.
*
*    ENDLOOP.
*
*  ELSE.
*    CLEAR ls_insert_data.
*
*    DATA : BEGIN OF ls_check_update,
*             vbeln TYPE ztsd_check_payin-vbeln,
*             runid TYPE ztsd_check_payin-runid,
*           END OF ls_check_update.
*    DATA lt_check_update LIKE TABLE OF ls_check_update.
*
*    IF lv_check_amount = 'X'.
*      LOOP AT lt_ztsd_line_payin_check INTO ls_ztsd_line_payin_check.
*        UPDATE ztsd_line_payin FROM ls_ztsd_line_payin_check.
*        CLEAR ls_ztsd_line_payin_check.
*      ENDLOOP.
*
*      LOOP AT lt_ztsd_line_payin INTO ls_ztsd_line_payin_tmp.
*        READ TABLE lt_ztsd_line_payin_check INTO ls_ztsd_line_payin_check
*        WITH KEY vbeln = ls_ztsd_line_payin-vbeln
*                 posnr = ls_ztsd_line_payin-posnr.
*        IF sy-subrc NE 0.
*          DELETE FROM ztsd_line_payin WHERE vbeln = ls_ztsd_line_payin-vbeln
*                                        AND posnr = ls_ztsd_line_payin-posnr.
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDIF.
*
*    READ TABLE lt_insert_data INTO ls_insert_data INDEX 1.
*    IF sy-subrc = 0.
*
*      ls_insert_data_tmp-vbeln = ls_insert_data-vbeln.
*      ls_insert_data_tmp-runid = '0'.
*      ls_insert_data_tmp-waipi = 'X'.
*      ls_insert_data_tmp-netwr = lv_waiat.
*      ls_insert_data_tmp-waiat = lv_waiat.
*
*      SELECT SINGLE vbeln
*                    runid
*        FROM ztsd_check_payin
*        INTO ls_check_update
*        WHERE vbeln = ls_insert_data-vbeln
*          AND runid = '0'.
*      IF sy-subrc = 0.
*        UPDATE ztsd_check_payin FROM ls_insert_data_tmp.
*      ELSE.
*        INSERT ztsd_check_payin FROM ls_insert_data_tmp.
*      ENDIF.
*    ELSE.
*      SELECT vbeln
*             runid
*        FROM ztsd_check_payin
*        INTO TABLE lt_check_update
*        FOR ALL ENTRIES IN lt_item2
*        WHERE vbeln = lt_item2-vbeln
*          AND runid = '0'.
*
*      SORT lt_item2 BY vbeln.
*      DATA ls_item2_tmp LIKE ls_item2.
*      LOOP AT lt_item2 INTO ls_item2.
*        MOVE-CORRESPONDING ls_item2 TO ls_item2_tmp.
*        AT NEW vbeln.
*          ls_insert_data-vbeln = ls_item2_tmp-vbeln.
*          ls_insert_data-runid = '0'.
*          ls_insert_data-waipi = 'X'.
*          ls_insert_data-netwr = lv_waiat.
*          ls_insert_data-waiat = lv_waiat.
*
*          READ TABLE lt_check_update INTO ls_check_update
*          WITH KEY vbeln = ls_item2_tmp-vbeln.
*          IF sy-subrc = 0.
*            UPDATE ztsd_check_payin FROM ls_insert_data.
*          ELSE.
*            INSERT ztsd_check_payin FROM ls_insert_data.
*          ENDIF.
*        ENDAT.
*      ENDLOOP.
*
*    ENDIF.
*
*    LOOP AT lt_ztsd_line_payin INTO ls_ztsd_line_payin.
*      UPDATE ztsd_line_payin SET waipi = 'X'
*                           WHERE vbeln = ls_ztsd_line_payin-vbeln
*                             AND posnr = ls_ztsd_line_payin-posnr.
*
*    ENDLOOP.
*
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_REMAIN_PAYIN
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_check_change TABLES ft_check_amount
*                           ft_item2
*                           ft_ztsd_line_payin_check
*                           ft_vbap
*                           lt_update STRUCTURE ztsd_line_payin
*                           lt_insert STRUCTURE ztsd_line_payin
*                     USING lv_change_all
*                  CHANGING lv_check_change
*                           lv_check_amount.
*
*  DATA : BEGIN OF ls_check_amount,
*           vbeln TYPE vbap-vbeln,
*           edatu TYPE vbep-edatu,
*           total TYPE vbap-netwr,
*         END OF ls_check_amount.
*  DATA : lt_check_amount     LIKE TABLE OF ls_check_amount,
*         lt_check_amount_tmp LIKE TABLE OF ls_check_amount,
*         lt_check_amount_chg LIKE TABLE OF ls_check_amount.
*
*  DATA : BEGIN OF ls_item2_check,
*           vbeln TYPE vbap-vbeln,
*           edatu TYPE vbep-edatu,
*           total TYPE vbap-netwr,
*         END OF ls_item2_check.
*  DATA lt_item2_check LIKE TABLE OF ls_item2_check.
*
*  DATA : BEGIN OF ls_vbap,
*           vbeln TYPE vbap-vbeln,
*           posnr TYPE vbap-posnr,
*           netwr TYPE vbap-netwr,
*           mwsbp TYPE vbap-mwsbp,
*         END OF ls_vbap.
*  DATA lt_vbap LIKE TABLE OF ls_vbap.
*
*  DATA : BEGIN OF ls_insert_payin,
*           vbeln        TYPE ztsd_check_payin-vbeln,
*           runid        TYPE ztsd_check_payin-runid,
*           payno        TYPE ztsd_check_payin-payno,
*           kunnr        TYPE ztsd_check_payin-kunnr,
*           payamt       TYPE ztsd_check_payin-payamt,
*           payamt_check TYPE ztsd_check_payin-payamt_check,
*           netwr_check  TYPE ztsd_check_payin-netwr_check,
*           edatu        TYPE ztsd_check_payin-edatu,
*         END OF ls_insert_payin.
*  DATA : lt_insert_payin     LIKE TABLE OF ls_insert_payin,
*         ls_insert_payin_tmp LIKE ls_insert_payin.
*
*  DATA : lt_item2 TYPE TABLE OF typ_item2,
*         ls_item2 TYPE typ_item2.
*
*  DATA : ls_ztsd_line_payin_check TYPE ztsd_line_payin,
*         lt_ztsd_line_payin_check TYPE TABLE OF ztsd_line_payin.
*
*  DATA : lt_ztsd_check_payin     TYPE TABLE OF ztsd_check_payin,
*         lt_ztsd_check_payin_ins TYPE TABLE OF ztsd_check_payin,
*         ls_ztsd_check_payin     TYPE ztsd_check_payin,
*         ls_ztsd_check_payin_tmp TYPE ztsd_check_payin,
*         ls_ztsd_check_payin_ins TYPE ztsd_check_payin.
*
*  DATA : lv_total TYPE vbap-netwr,
*         lv_tabix TYPE sy-tabix.
*
*  DATA : BEGIN OF ls_ztb_pay_in,
*           payno  TYPE ztb_pay_in-payno,
*           kunnr  TYPE ztb_pay_in-kunnr,
*           payamt TYPE ztb_pay_in-payamt,
*         END OF ls_ztb_pay_in.
*  DATA lt_ztb_pay_in LIKE TABLE OF ls_ztb_pay_in.
*
*  DATA : lv_runid     TYPE ztsd_check_payin-runid,
*         lv_check_chg TYPE c.
*
*  DATA : ls_update TYPE ztsd_line_payin,
*         ls_insert TYPE ztsd_line_payin.
*
*  RANGES : lr_payno FOR ztsd_check_payin-payno,
*           lr_vbeln FOR vbak-vbeln.
*
*  lt_check_amount[]          = ft_check_amount[].
*  lt_check_amount_tmp[]      = lt_check_amount[].
*  lt_item2[]                 = ft_item2[].
*  lt_ztsd_line_payin_check[] = ft_ztsd_line_payin_check[].
*  lt_vbap[]                  = ft_vbap[].
*
*  "IF lv_change_all EQ 'X'.
*  CLEAR lt_check_amount[].
*  LOOP AT lt_item2 INTO ls_item2.
*    READ TABLE lt_vbap INTO ls_vbap
*      WITH KEY vbeln = ls_item2-vbeln
*               posnr = ls_item2-posnr_1.
*    IF sy-subrc = 0.
*      lv_total = ls_vbap-netwr + ( ( ls_vbap-netwr * 7 ) / 100 )."ls_vbap-mwsbp.
*    ENDIF.
*
*    READ TABLE lt_ztsd_line_payin_check INTO ls_ztsd_line_payin_check
*    WITH KEY vbeln = ls_item2-vbeln
*             posnr = ls_item2-posnr_1.
*    IF sy-subrc = 0.
*      lv_tabix = sy-tabix.
*      IF ls_ztsd_line_payin_check-total EQ lv_total AND ls_ztsd_line_payin_check-edatu EQ ls_item2-edatu.
*        IF ls_ztsd_line_payin_check-waipi = 'X' AND lv_check_amount NE 'X'.
*          lv_check_change = 'X'.
*          "lt_check_amount[] = lt_check_amount_tmp[].
*          ls_check_amount-vbeln = ls_item2-vbeln.
*          ls_check_amount-edatu = ls_item2-edatu.
*          ls_check_amount-total = lv_total.
*          APPEND ls_check_amount TO lt_check_amount_chg.
*        ELSE.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        IF lv_total GT ls_ztsd_line_payin_check-total.
*          ls_check_amount-vbeln = ls_item2-vbeln.
*          ls_check_amount-edatu = ls_item2-edatu.
*          ls_check_amount-total = lv_total - ls_ztsd_line_payin_check-total.
*          COLLECT ls_check_amount INTO lt_check_amount.
*          lv_check_change = 'X'.
*          lv_check_amount = 'X'.
*        ELSEIF lv_total LT ls_ztsd_line_payin_check-total.
*          ls_ztsd_check_payin-vbeln       = ls_ztsd_line_payin_check-vbeln.
*          "ls_ztsd_check_payin-payno       = ls_ztsd_line_payin_check-payno.
*          "ls_ztsd_check_payin-kunnr       = ls_ztsd_line_payin_check-kunnr.
*          ls_ztsd_check_payin-payamt      = ls_ztsd_line_payin_check-total - lv_total.
*          ls_ztsd_check_payin-netwr_check = lv_total - ls_ztsd_line_payin_check-total.
*          ls_ztsd_check_payin-edatu       = ls_item2-edatu.
*          COLLECT ls_ztsd_check_payin INTO lt_ztsd_check_payin.
*        ENDIF.
*
*        ls_ztsd_line_payin_check-total = lv_total.
*        ls_ztsd_line_payin_check-edatu = ls_item2-edatu.
*        "UPDATE ztsd_line_payin FROM ls_ztsd_line_payin_check.
*        APPEND ls_ztsd_line_payin_check TO lt_update.
*      ENDIF.
*
*    ELSE.
*      lv_check_change = 'X'.
*      lv_check_amount = 'X'.
*      ls_check_amount-vbeln = ls_item2-vbeln.
*      ls_check_amount-edatu = ls_item2-edatu.
*      ls_check_amount-total = lv_total.
*
*      COLLECT ls_check_amount INTO lt_check_amount.
*
*    ENDIF.
*
**    ls_item2_check-vbeln = ls_ztsd_line_payin_check-vbeln.
**    ls_item2_check-edatu = ls_ztsd_line_payin_check-edatu.
**    ls_item2_check-total = ls_ztsd_line_payin_check-total.
**
**    COLLECT ls_item2_check INTO lt_item2_check.
**    CLEAR : ls_item2,ls_item2_check,ls_vbap.
*  ENDLOOP.
*
*  IF lv_check_chg    NE 'X' AND
*     lv_check_amount NE 'X'.
*
*    LOOP AT lt_check_amount_chg INTO ls_check_amount.
*      COLLECT ls_check_amount INTO lt_check_amount.
*    ENDLOOP.
*
*  ENDIF.
*
*  ft_check_amount[] = lt_check_amount[].
**  ELSE.
**    lv_check_change = 'X'.
**  ENDIF.
*
*  IF lt_ztsd_check_payin IS NOT INITIAL. " New amount lese than old amount
*    LOOP AT lt_update INTO ls_update.
*      UPDATE ztsd_line_payin FROM ls_update.
*      CLEAR ls_update.
*    ENDLOOP.
*
*    LOOP AT lt_insert INTO ls_insert.
*      INSERT ztsd_line_payin FROM ls_insert.
*      CLEAR ls_insert.
*    ENDLOOP.
*
*    SELECT vbeln
*           runid
*           payno
*           kunnr
*           payamt
*           payamt_check
*           netwr_check
*           edatu
*      FROM ztsd_check_payin
*      INTO TABLE lt_insert_payin
*      FOR ALL ENTRIES IN lt_ztsd_check_payin
*      WHERE vbeln EQ lt_ztsd_check_payin-vbeln
*        AND edatu EQ lt_ztsd_check_payin-edatu.
*
*    IF lt_insert_payin[] IS INITIAL.
*      SELECT vbeln
*             runid
*             payno
*             kunnr
*             payamt
*             payamt_check
*             netwr_check
*             edatu
*        FROM ztsd_check_payin
*        INTO TABLE lt_insert_payin
*        FOR ALL ENTRIES IN lt_ztsd_line_payin_check
*        WHERE vbeln EQ lt_ztsd_line_payin_check-vbeln
*          AND edatu EQ lt_ztsd_line_payin_check-edatu.
*    ENDIF.
*
*    SORT lt_insert_payin BY vbeln runid DESCENDING.
*
*    PERFORM f_cal_data TABLES lt_insert_payin
*                              lt_ztsd_check_payin.
*
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_CHANGE
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CHANGE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTSD_LINE_PAYIN_CHECK  text
*      -->P_LOOP  text
*      -->P_AT  text
*      -->P_LT_ITEM2  text
*      -->P_INTO  text
*      -->P_LS_ITEM2  text
*----------------------------------------------------------------------*
*FORM f_check_change_date  TABLES lt_ztsd_line_payin_check STRUCTURE ztsd_line_payin.
*
*  DATA : BEGIN OF ls_check_change_date,
*           vbeln TYPE vbap-vbeln,
*           posnr TYPE vbap-posnr,
*           edatu TYPE vbep-edatu,
*           total TYPE ztsd_line_payin-total,
*         END OF ls_check_change_date.
*  DATA : lt_check_change_date     LIKE TABLE OF ls_check_change_date,
*         lt_check_change_date_tmp LIKE TABLE OF ls_check_change_date,
*         ls_check_change_date_tmp LIKE ls_check_change_date.
*
*  DATA : BEGIN OF ls_insert_payin,
*           vbeln        TYPE ztsd_check_payin-vbeln,
*           runid        TYPE ztsd_check_payin-runid,
*           payno        TYPE ztsd_check_payin-payno,
*           kunnr        TYPE ztsd_check_payin-kunnr,
*           payamt       TYPE ztsd_check_payin-payamt,
*           payamt_check TYPE ztsd_check_payin-payamt_check,
*           netwr_check  TYPE ztsd_check_payin-netwr_check,
*           edatu        TYPE ztsd_check_payin-edatu,
*         END OF ls_insert_payin.
*  DATA : lt_insert_payin LIKE TABLE OF ls_insert_payin.
*
*  DATA : BEGIN OF ls_max_payno,
*           payno       TYPE ztsd_check_payin-payno,
*           kunnr       TYPE ztsd_check_payin-kunnr,
*           runid_payin TYPE ztsd_check_payin-runid_payin,
*         END OF ls_max_payno.
*  DATA lt_max_payno LIKE TABLE OF ls_max_payno.
*
*  DATA : BEGIN OF ls_max_runid,
*           vbeln TYPE ztsd_check_payin-vbeln,
*           runid TYPE ztsd_check_payin-runid,
*         END OF ls_max_runid.
*  DATA lt_max_runid LIKE TABLE OF ls_max_runid.
*
*  DATA : BEGIN OF ls_ztb_pay_in,
*           payno  TYPE ztb_pay_in-payno,
*           kunnr  TYPE ztb_pay_in-kunnr,
*           payamt TYPE ztb_pay_in-payamt,
*         END OF ls_ztb_pay_in.
*  DATA lt_ztb_pay_in LIKE TABLE OF ls_ztb_pay_in.
*
*  DATA ls_ztsd_line_payin_check TYPE ztsd_line_payin.
*
*  DATA : lt_ztsd_check_payin TYPE TABLE OF ztsd_check_payin,
*         ls_ztsd_check_payin TYPE ztsd_check_payin.
*
*  DATA : lv_tabix       TYPE sy-tabix,
*         lv_runid       TYPE ztsd_check_payin-runid,
*         lv_runid_payin TYPE ztsd_check_payin-runid_payin.
*
*  "DATA : ls_ztsd_line_payin_check TYPE ztsd_line_payin.
*
*  RANGES : lr_payno FOR ztsd_check_payin-payno,
*           lr_vbeln FOR ztsd_check_payin-vbeln.
*
*  IF lt_ztsd_line_payin_check[] IS NOT INITIAL.
*    SELECT vbeln
*           posnr
*           edatu
*           total
*      FROM ztsd_line_payin
*      INTO TABLE lt_check_change_date
*      FOR ALL ENTRIES IN lt_ztsd_line_payin_check
*      WHERE vbeln EQ lt_ztsd_line_payin_check-vbeln
*        AND edatu EQ lt_ztsd_line_payin_check-edatu.
*    "AND statc NE 'X'.
*
*    LOOP AT lt_check_change_date INTO ls_check_change_date.
*
*      READ TABLE lt_ztsd_line_payin_check INTO ls_ztsd_line_payin_check
*      WITH KEY vbeln = ls_check_change_date-vbeln
*               posnr = ls_check_change_date-posnr.
*      IF sy-subrc NE 0.
*        DELETE FROM ztsd_line_payin WHERE vbeln = ls_check_change_date-vbeln
*                                      AND posnr = ls_check_change_date-posnr.
*        APPEND ls_check_change_date TO lt_check_change_date_tmp.
*      ENDIF.
*
*    ENDLOOP.
*  ENDIF.
*
*  IF lt_check_change_date_tmp IS NOT INITIAL.
*
*    SELECT vbeln
*           runid
*           payno
*           kunnr
*           payamt
*           payamt_check
*           netwr_check
*           edatu
*      FROM ztsd_check_payin
*      INTO TABLE lt_insert_payin
*      FOR ALL ENTRIES IN lt_check_change_date_tmp
*      WHERE vbeln EQ lt_check_change_date_tmp-vbeln
*        AND edatu EQ lt_check_change_date_tmp-edatu.
*
*    SORT lt_insert_payin BY vbeln runid DESCENDING.
*
*    LOOP AT lt_insert_payin INTO ls_insert_payin.
*
*      CLEAR lr_payno.
*      lr_payno-sign   = 'I'.
*      lr_payno-option = 'EQ'.
*      lr_payno-low    = ls_insert_payin-payno.
*      APPEND lr_payno.
*
*      CLEAR lr_vbeln.
*      lr_vbeln-sign   = 'I'.
*      lr_vbeln-option = 'EQ'.
*      lr_vbeln-low    = ls_insert_payin-vbeln.
*      APPEND lr_vbeln.
*
*      CLEAR : ls_insert_payin.
*    ENDLOOP.
*
*    DELETE ADJACENT DUPLICATES FROM lr_payno[].
*
*    IF lr_payno[] IS NOT INITIAL.
*      SELECT payno
*             kunnr
*             MAX( runid_payin )
*        FROM ztsd_check_payin
*        INTO TABLE  lt_max_payno
*        WHERE payno IN lr_payno
*        GROUP BY payno kunnr.
*    ENDIF.
*
*    IF lr_vbeln[] IS NOT INITIAL.
*      SELECT vbeln
*             MAX( runid )
*        FROM ztsd_check_payin
*        INTO TABLE lt_max_runid
*        WHERE vbeln IN lr_vbeln
*        GROUP BY vbeln.
*    ENDIF.
*
*    CLEAR lt_insert_payin[].
*    IF lt_max_payno[] IS NOT INITIAL.
*      SELECT vbeln
*             runid
*             payno
*             kunnr
*             payamt
*             payamt_check
*             netwr_check
*             edatu
*        FROM ztsd_check_payin
*        INTO TABLE lt_insert_payin
*        FOR ALL ENTRIES IN lt_max_payno
*        WHERE kunnr       EQ lt_max_payno-kunnr
*          AND payno       EQ lt_max_payno-payno
*          AND runid_payin EQ lt_max_payno-runid_payin.
*
*      SELECT payno
*             kunnr
*             payamt
*        FROM ztb_pay_in
*        INTO TABLE lt_ztb_pay_in
*        FOR ALL ENTRIES IN lt_max_payno
*        WHERE kunnr       EQ lt_max_payno-kunnr
*          AND payno       EQ lt_max_payno-payno.
*
*    ENDIF.
*
*    SORT lt_insert_payin BY payno DESCENDING.
*    SORT lt_check_change_date_tmp BY vbeln.
*
*    CLEAR ls_check_change_date.
*    DATA lv_amount TYPE vbap-netwr.
*
*
*    LOOP AT lt_check_change_date_tmp INTO ls_check_change_date_tmp.
*      MOVE-CORRESPONDING ls_check_change_date_tmp TO ls_check_change_date.
*      AT NEW vbeln.
*        CLEAR : lv_runid.
*      ENDAT.
*      "
*      LOOP AT lt_insert_payin INTO ls_insert_payin.
*        lv_tabix = sy-tabix.
*        IF lv_runid IS INITIAL.
*          READ TABLE lt_max_runid INTO ls_max_runid
*          WITH KEY vbeln = ls_insert_payin-vbeln.
*          IF sy-subrc = 0.
*            lv_runid = ls_max_runid-runid + 1.
*          ENDIF.
*        ELSE.
*          ADD 1 TO lv_runid.
*        ENDIF.
*
*        READ TABLE lt_max_payno INTO ls_max_payno
*        WITH KEY payno = ls_insert_payin-payno
*                 kunnr = ls_insert_payin-kunnr.
*        IF sy-subrc = 0 AND lv_runid_payin IS INITIAL.
*          ls_ztsd_check_payin-runid_payin = ls_max_payno-runid_payin + 1.
*          lv_runid_payin                  = ls_max_payno-runid_payin + 1.
*        ELSE.
*          ls_ztsd_check_payin-runid_payin = lv_runid_payin + 1.
*        ENDIF.
*        ls_ztsd_check_payin-vbeln        = ls_insert_payin-vbeln.
*        ls_ztsd_check_payin-runid        = lv_runid.
*        ls_ztsd_check_payin-payno        = ls_insert_payin-payno.
*        ls_ztsd_check_payin-kunnr        = ls_insert_payin-kunnr.
*        ls_ztsd_check_payin-payamt_check = ls_insert_payin-payamt_check.
*        ls_ztsd_check_payin-ernam        = sy-uname.
*        ls_ztsd_check_payin-erdat        = sy-datum.
*        ls_ztsd_check_payin-edatu        = ls_check_change_date_tmp-edatu.
*
*        READ TABLE lt_ztb_pay_in INTO ls_ztb_pay_in
*        WITH KEY payno = ls_insert_payin-payno
*                 kunnr = ls_insert_payin-kunnr.
*        IF sy-subrc = 0 AND ls_ztb_pay_in-payamt GE ls_check_change_date-total.
*          IF lv_amount IS INITIAL.
*            ls_ztsd_check_payin-payamt   = ls_insert_payin-payamt + ls_check_change_date-total.
*            lv_amount                    = ls_ztsd_check_payin-payamt.
*          ELSE.
*            ls_ztsd_check_payin-payamt  = lv_amount + ls_check_change_date-total.
*          ENDIF.
*
*          ls_ztsd_check_payin-netwr_check  = ls_check_change_date-total * -1.
*          CLEAR ls_insert_payin.
*          MOVE-CORRESPONDING ls_ztsd_check_payin TO ls_insert_payin.
*          MODIFY lt_ztb_pay_in FROM ls_ztb_pay_in INDEX sy-tabix.
*          APPEND ls_ztsd_check_payin TO lt_ztsd_check_payin.
*          UPDATE ztb_pay_in SET used  = ''
*                                rempi = ls_ztsd_check_payin-payamt
*                                rmamt = ls_ztsd_check_payin-payamt
*                          WHERE payno = ls_ztb_pay_in-payno
*                            AND kunnr = ls_ztb_pay_in-kunnr.
*          EXIT.
*        ELSE.
*          IF lv_amount IS INITIAL.
*            ls_ztsd_check_payin-payamt   = ls_insert_payin-payamt + ls_insert_payin-netwr_check.
*            lv_amount                    = ls_ztsd_check_payin-payamt.
*          ELSE.
*            ls_ztsd_check_payin-payamt  = lv_amount + ls_insert_payin-netwr_check.
*          ENDIF.
*          "ls_ztsd_check_payin-payamt       =   ls_insert_payin-payamt + ls_insert_payin-netwr_check. "ls_ztb_pay_in-payamt.
*          ls_ztsd_check_payin-netwr_check  =   ls_insert_payin-netwr_check * -1.
*          ls_check_change_date-total       =   ls_check_change_date-total - ls_insert_payin-netwr_check.
*          CLEAR ls_insert_payin.
*          MOVE-CORRESPONDING ls_ztsd_check_payin TO ls_insert_payin.
*          UPDATE ztb_pay_in SET used  = ''
*                                rempi = ls_ztsd_check_payin-payamt
*                                rmamt = ls_ztsd_check_payin-payamt
*                          WHERE payno = ls_ztb_pay_in-payno
*                            AND kunnr = ls_ztb_pay_in-kunnr.
*          DELETE lt_ztb_pay_in INDEX sy-tabix.
*          APPEND ls_ztsd_check_payin TO lt_ztsd_check_payin.
*          CLEAR : lv_amount,lv_runid_payin.
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDLOOP.
*
*    LOOP AT lt_ztsd_check_payin INTO ls_ztsd_check_payin .
*      INSERT ztsd_check_payin FROM ls_ztsd_check_payin.
*    ENDLOOP.
*
*    COMMIT WORK AND WAIT.
*
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_CHANGE_DATE
*&---------------------------------------------------------------------*
*&      Form  F_CAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_cal_data TABLES ft_insert_payin
*                       ft_ztsd_check_payin.
*
*  DATA : BEGIN OF ls_insert_payin,
*           vbeln        TYPE ztsd_check_payin-vbeln,
*           runid        TYPE ztsd_check_payin-runid,
*           payno        TYPE ztsd_check_payin-payno,
*           kunnr        TYPE ztsd_check_payin-kunnr,
*           payamt       TYPE ztsd_check_payin-payamt,
*           payamt_check TYPE ztsd_check_payin-payamt_check,
*           netwr_check  TYPE ztsd_check_payin-netwr_check,
*           edatu        TYPE ztsd_check_payin-edatu,
*         END OF ls_insert_payin.
*  DATA : lt_insert_payin     LIKE TABLE OF ls_insert_payin,
*         ls_insert_payin_tmp LIKE ls_insert_payin.
*
*  DATA : BEGIN OF ls_max_payno,
*           payno       TYPE ztsd_check_payin-payno,
*           kunnr       TYPE ztsd_check_payin-kunnr,
*           runid_payin TYPE ztsd_check_payin-runid_payin,
*         END OF ls_max_payno.
*  DATA lt_max_payno LIKE TABLE OF ls_max_payno.
*
*  DATA : BEGIN OF ls_max_runid,
*           vbeln TYPE ztsd_check_payin-vbeln,
*           runid TYPE ztsd_check_payin-runid,
*         END OF ls_max_runid.
*  DATA lt_max_runid LIKE TABLE OF ls_max_runid.
*
*  DATA : lt_item2 TYPE TABLE OF typ_item2,
*         ls_item2 TYPE typ_item2.
*
*  DATA : ls_ztsd_line_payin_check TYPE ztsd_line_payin,
*         lt_ztsd_line_payin_check TYPE TABLE OF ztsd_line_payin.
*
*  DATA : lt_ztsd_check_payin     TYPE TABLE OF ztsd_check_payin,
*         lt_ztsd_check_payin_ins TYPE TABLE OF ztsd_check_payin,
*         ls_ztsd_check_payin     TYPE ztsd_check_payin,
*         ls_ztsd_check_payin_tmp TYPE ztsd_check_payin,
*         ls_ztsd_check_payin_ins TYPE ztsd_check_payin.
*
*  DATA : lv_total   TYPE vbap-netwr,
*         lv_tabix   TYPE sy-tabix,
*         lv_tabix_r TYPE sy-tabix.
*
*  DATA : BEGIN OF ls_ztb_pay_in,
*           payno  TYPE ztb_pay_in-payno,
*           kunnr  TYPE ztb_pay_in-kunnr,
*           payamt TYPE ztb_pay_in-payamt,
*         END OF ls_ztb_pay_in.
*  DATA lt_ztb_pay_in LIKE TABLE OF ls_ztb_pay_in.
*
*  DATA : lv_runid TYPE ztsd_check_payin-runid,
*         lv_amt   TYPE vbap-netwr,
*         lv_check TYPE vbap-netwr.
*
*  RANGES : lr_payno FOR ztsd_check_payin-payno,
*           lr_vbeln FOR vbak-vbeln.
*
*
*  lt_insert_payin[]     = ft_insert_payin[].
*  lt_ztsd_check_payin[] = ft_ztsd_check_payin[].
*
*  LOOP AT lt_insert_payin INTO ls_insert_payin.
*
*    CLEAR lr_payno.
*    lr_payno-sign   = 'I'.
*    lr_payno-option = 'EQ'.
*    lr_payno-low    = ls_insert_payin-payno.
*    APPEND lr_payno.
*
*    CLEAR lr_vbeln.
*    lr_vbeln-sign   = 'I'.
*    lr_vbeln-option = 'EQ'.
*    lr_vbeln-low    = ls_insert_payin-vbeln.
*    APPEND lr_vbeln.
*
*    CLEAR : ls_insert_payin.
*  ENDLOOP.
*
*  DELETE ADJACENT DUPLICATES FROM lr_payno[].
*
*  IF lr_payno[] IS NOT INITIAL.
*    SELECT payno
*           kunnr
*           MAX( runid_payin )
*      FROM ztsd_check_payin
*      INTO TABLE  lt_max_payno
*      WHERE payno IN lr_payno
*      GROUP BY payno kunnr.
*  ENDIF.
*
*  IF lr_vbeln[] IS NOT INITIAL.
*    SELECT vbeln
*           MAX( runid )
*      FROM ztsd_check_payin
*      INTO TABLE lt_max_runid
*      WHERE vbeln IN lr_vbeln
*      GROUP BY vbeln.
*  ENDIF.
*
*  CLEAR lt_insert_payin[].
*  IF lt_max_payno[] IS NOT INITIAL.
*    SELECT vbeln
*           runid
*           payno
*           kunnr
*           payamt
*           payamt_check
*           netwr_check
*           edatu
*      FROM ztsd_check_payin
*      INTO TABLE lt_insert_payin
*      FOR ALL ENTRIES IN lt_max_payno
*      WHERE kunnr       EQ lt_max_payno-kunnr
*        AND payno       EQ lt_max_payno-payno
*        AND runid_payin EQ lt_max_payno-runid_payin.
*
*    SELECT payno
*           kunnr
*           payamt
*      FROM ztb_pay_in
*      INTO TABLE lt_ztb_pay_in
*      FOR ALL ENTRIES IN lt_max_payno
*      WHERE kunnr       EQ lt_max_payno-kunnr
*        AND payno       EQ lt_max_payno-payno.
*
*  ENDIF.
*
*  SORT lt_insert_payin BY payno DESCENDING.
*  SORT lt_ztsd_check_payin BY vbeln.
*
*  LOOP AT lt_ztsd_check_payin INTO ls_ztsd_check_payin_tmp.
*    lv_tabix = sy-tabix.
*    MOVE-CORRESPONDING ls_ztsd_check_payin_tmp TO ls_ztsd_check_payin.
*    AT NEW vbeln.
*      CLEAR lv_runid.
*    ENDAT.
*    LOOP AT lt_insert_payin INTO ls_insert_payin.
*      lv_tabix = sy-tabix.
*      IF lv_runid IS INITIAL.
*        READ TABLE lt_max_runid INTO ls_max_runid
*        WITH KEY vbeln = ls_insert_payin-vbeln.
*        IF sy-subrc = 0.
*          lv_runid = ls_max_runid-runid + 1.
*        ENDIF.
*      ELSE.
*        ADD 1 TO lv_runid.
*      ENDIF.
*
*      READ TABLE lt_max_payno INTO ls_max_payno
*      WITH KEY payno = ls_insert_payin-payno
*               kunnr = ls_insert_payin-kunnr.
*      IF sy-subrc = 0.
*        ls_ztsd_check_payin_ins-runid_payin = ls_max_payno-runid_payin + 1.
*      ENDIF.
*      ls_ztsd_check_payin_ins-vbeln        = ls_insert_payin-vbeln.
*      ls_ztsd_check_payin_ins-runid        = lv_runid.
*      ls_ztsd_check_payin_ins-payno        = ls_insert_payin-payno.
*      ls_ztsd_check_payin_ins-kunnr        = ls_insert_payin-kunnr.
*      ls_ztsd_check_payin_ins-payamt_check = ls_insert_payin-payamt_check.
*      ls_ztsd_check_payin_ins-ernam        = sy-uname.
*      ls_ztsd_check_payin_ins-erdat        = sy-datum.
*      ls_ztsd_check_payin_ins-edatu        = ls_ztsd_check_payin-edatu.
*
*      READ TABLE lt_ztb_pay_in INTO ls_ztb_pay_in
*      WITH KEY payno = ls_insert_payin-payno
*               kunnr = ls_insert_payin-kunnr.
*      lv_tabix_r = sy-tabix.
*      IF sy-subrc = 0 AND ls_ztb_pay_in-payamt EQ ls_insert_payin-payamt.
*        DELETE lt_ztb_pay_in INDEX sy-tabix.
*
*
*      ELSEIF sy-subrc = 0 AND ls_ztb_pay_in-payamt GT ls_insert_payin-payamt.
*        lv_amt   = ls_ztb_pay_in-payamt - ls_insert_payin-payamt.
*
*        lv_check = lv_amt - ls_ztsd_check_payin-payamt.
*
*        "lv_amt  = ls_ztb_pay_in-payamt + ls_ztsd_check_payin-netwr_check.
*
*
*        IF lv_check LT 0.
*          ls_ztsd_check_payin_ins-payamt       = lv_amt.
*          ls_ztsd_check_payin_ins-netwr_check  = lv_amt * -1.
*          ls_ztsd_check_payin-payamt           = ls_ztsd_check_payin-payamt - lv_amt.
*          CLEAR ls_insert_payin.
*          MOVE-CORRESPONDING ls_ztsd_check_payin TO ls_insert_payin.
*          MODIFY lt_insert_payin FROM ls_insert_payin INDEX lv_tabix.
*          UPDATE ztb_pay_in SET used  = ''
*                                rempi = ls_ztsd_check_payin_ins-payamt
*                                rmamt = ls_ztsd_check_payin_ins-payamt
*                          WHERE payno = ls_ztb_pay_in-payno
*                            AND kunnr = ls_ztb_pay_in-kunnr.
*          DELETE lt_ztb_pay_in INDEX lv_tabix_r.
*          "DELETE lt_insert_payin INDEX lv_tabix.
*          APPEND ls_ztsd_check_payin_ins TO lt_ztsd_check_payin_ins.
*        ELSE.
*          ls_ztsd_check_payin_ins-payamt       = ls_ztsd_check_payin-payamt + ls_insert_payin-payamt.
*          ls_ztsd_check_payin_ins-netwr_check  = ls_ztsd_check_payin-payamt * -1.
*          CLEAR ls_insert_payin.
*          MOVE-CORRESPONDING ls_ztsd_check_payin_ins TO ls_insert_payin.
*          MODIFY lt_insert_payin FROM ls_insert_payin INDEX lv_tabix.
*          MODIFY lt_ztb_pay_in FROM ls_ztb_pay_in INDEX lv_tabix_r.
*          APPEND ls_ztsd_check_payin_ins TO lt_ztsd_check_payin_ins.
*          UPDATE ztb_pay_in SET used  = ''
*                                rempi = ls_ztsd_check_payin_ins-payamt
*                                rmamt = ls_ztsd_check_payin_ins-payamt
*                          WHERE payno = ls_ztb_pay_in-payno
*                            AND kunnr = ls_ztb_pay_in-kunnr.
*          EXIT.
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDLOOP.
*
*  LOOP AT lt_ztsd_check_payin_ins INTO ls_ztsd_check_payin_ins .
*    INSERT ztsd_check_payin FROM ls_ztsd_check_payin_ins.
*  ENDLOOP.
*
*  COMMIT WORK AND WAIT.
*
*ENDFORM.                    " F_CAL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_show_error USING ls_vbak TYPE typ_vbak.
*
*  DATA : ls_insert_data_tmp TYPE ztsd_check_payin.
*  DATA : BEGIN OF ls_check_update,
*           vbeln TYPE ztsd_check_payin-vbeln,
*           runid TYPE ztsd_check_payin-runid,
*         END OF ls_check_update.
*  DATA lt_check_update LIKE TABLE OF ls_check_update.
*
*  ls_insert_data_tmp-vbeln = ls_vbak-vbeln.
*  ls_insert_data_tmp-runid = '999'.
*  ls_insert_data_tmp-waipi = 'X'.
**  ls_insert_data_tmp-netwr = gwa_total-gtotal.
**  ls_insert_data_tmp-waiat = gwa_total-gtotal.
*
*  INSERT ztsd_check_payin FROM ls_insert_data_tmp.
*
*ENDFORM.                    " F_SHOW_ERROR
*&---------------------------------------------------------------------*
*&      Form  F_GET_ORDER_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LR_AUART[]  text
*      -->P_CLEAR  text
*      -->P_LR_AUART  text
*----------------------------------------------------------------------*
FORM F_GET_ORDER_TYPE  TABLES FR_AUART.

  CONSTANTS : LC_ZO01 TYPE C LENGTH 4 VALUE 'ZO01',
              LC_ZO05 TYPE C LENGTH 4 VALUE 'ZO05',
              LC_ZO04 TYPE C LENGTH 4 VALUE 'ZO04',
              LC_ZO07 TYPE C LENGTH 4 VALUE 'ZO07',
              LC_ZOD1 TYPE C LENGTH 4 VALUE 'ZOD1'.

  RANGES : LR_AUART FOR VBAK-AUART.

  CLEAR LR_AUART.
  LR_AUART-SIGN   = 'I'.
  LR_AUART-OPTION = 'EQ'.
  LR_AUART-LOW    = LC_ZO01.
  APPEND LR_AUART.

  CLEAR LR_AUART.
  LR_AUART-SIGN   = 'I'.
  LR_AUART-OPTION = 'EQ'.
  LR_AUART-LOW    = LC_ZO05.
  APPEND LR_AUART.

  CLEAR LR_AUART.
  LR_AUART-SIGN   = 'I'.
  LR_AUART-OPTION = 'EQ'.
  LR_AUART-LOW    = LC_ZO04.
  APPEND LR_AUART.

  CLEAR LR_AUART.
  LR_AUART-SIGN   = 'I'.
  LR_AUART-OPTION = 'EQ'.
  LR_AUART-LOW    = LC_ZO07.
  APPEND LR_AUART.

  CLEAR LR_AUART.
  LR_AUART-SIGN   = 'I'.
  LR_AUART-OPTION = 'EQ'.
  LR_AUART-LOW    = LC_ZOD1.
  APPEND LR_AUART.

  FR_AUART[] = LR_AUART[].

ENDFORM.                    " F_GET_ORDER_TYPE
*&---------------------------------------------------------------------*
*&      Form  F_GET_SALES_OFFI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_VKBUR[]  text
*----------------------------------------------------------------------*
FORM F_GET_SALES_OFFI  TABLES FT_VKBUR.

  CONSTANTS : LC_1004 TYPE C LENGTH 4 VALUE '1004',
              LC_1009 TYPE C LENGTH 4 VALUE '1009',
              LC_1013 TYPE C LENGTH 4 VALUE '1013',
              LC_1110 TYPE C LENGTH 4 VALUE '1110'.

  RANGES : LR_VKBUR FOR VBAK-VKBUR.

  CLEAR LR_VKBUR.
  LR_VKBUR-SIGN   = 'I'.
  LR_VKBUR-OPTION = 'EQ'.
  LR_VKBUR-LOW    = LC_1004.
  APPEND LR_VKBUR.

  CLEAR LR_VKBUR.
  LR_VKBUR-SIGN   = 'I'.
  LR_VKBUR-OPTION = 'EQ'.
  LR_VKBUR-LOW    = LC_1009.
  APPEND LR_VKBUR.

  CLEAR LR_VKBUR.
  LR_VKBUR-SIGN   = 'I'.
  LR_VKBUR-OPTION = 'EQ'.
  LR_VKBUR-LOW    = LC_1013.
  APPEND LR_VKBUR.

  CLEAR LR_VKBUR.
  LR_VKBUR-SIGN   = 'I'.
  LR_VKBUR-OPTION = 'EQ'.
  LR_VKBUR-LOW    = LC_1110.
  APPEND LR_VKBUR.

  FT_VKBUR[] = LR_VKBUR[].

ENDFORM.                    " F_GET_SALES_OFFI
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*Form F_show_data .
*  Perform F_set_layout_output.
*  Perform F_build_fcat.
*  Perform F_sort.
*  Perform F_pf_alv_grid.
*Endform.                    " F_show_data
*&---------------------------------------------------------------------*
*&      Form  pf_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f_pf_alv_grid.
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program       = sy-repid
*      i_callback_pf_status_set = 'PF_STATUS'
*      i_callback_user_command  = 'USER_COMMAND'
**     I_CALLBACK_TOP_OF_PAGE   = ' '
**     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**     I_CALLBACK_HTML_END_OF_LIST       = ' '
**     I_STRUCTURE_NAME         =
**     I_BACKGROUND_ID          = ' '
**     I_GRID_TITLE             =
**     I_GRID_SETTINGS          =
*      is_layout                = gs_layout
*      it_fieldcat              = gt_fcat
**     IT_EXCLUDING             =
**     IT_SPECIAL_GROUPS        =
*      it_sort                  = gt_sort
**     IT_FILTER                =
**     IS_SEL_HIDE              =
*      i_default                = space
*      i_save                   = 'X'
**     IS_VARIANT               =
**     IT_EVENTS                =
**     IT_EVENT_EXIT            =
**     IS_PRINT                 =
**     IS_REPREP_ID             =
**     I_SCREEN_START_COLUMN    = 0
**     I_SCREEN_START_LINE      = 0
**     I_SCREEN_END_COLUMN      = 0
**     I_SCREEN_END_LINE        = 0
**     I_HTML_HEIGHT_TOP        = 0
**     I_HTML_HEIGHT_END        = 0
**     IT_ALV_GRAPHICS          =
**     IT_HYPERLINK             =
**     IT_ADD_FIELDCAT          =
**     IT_EXCEPT_QINFO          =
**     IR_SALV_FULLSCREEN_ADAPTER        =
** IMPORTING
**     E_EXIT_CAUSED_BY_CALLER  =
**     ES_EXIT_CAUSED_BY_USER   =
*    TABLES
*      t_outtab                 = gt_payin_alv
*    EXCEPTIONS
*      program_error            = 1
*      OTHERS                   = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*ENDFORM.                    "pf_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_layout_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_LAYOUT  text
*----------------------------------------------------------------------*
FORM F_SET_LAYOUT_OUTPUT." CHANGING ps_layout TYPE slis_layout_alv.
  "gs_layout-box_fieldname     = 'SEL'.
*  gs_layout-zebra             = 'X'.
*  gs_layout-info_fieldname    = 'LINE_COLOR'.
  "gs_layout-colwidth_optimize = 'X'.

ENDFORM.                    " set_layout_output
*&---------------------------------------------------------------------*
*&      Form  build_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_BUILD_FCAT.

  DATA:
   LS_FCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR : LS_FCAT,GT_FCAT .
  LS_FCAT-FIELDNAME = 'CHECK'.
  LS_FCAT-SELTEXT_L = 'Use'.
  LS_FCAT-SELTEXT_S = 'Use'.
  LS_FCAT-SELTEXT_M = 'Use'.
  LS_FCAT-CHECKBOX  = 'X'.
  LS_FCAT-INPUT     = 'X'.
  LS_FCAT-EDIT      = 'X'.
  APPEND LS_FCAT TO GT_FCAT.


  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZTSD_CHECK_PAYIN'.
  LS_FCAT-FIELDNAME   = 'KUNNR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'NAME1'.
  LS_FCAT-SELTEXT_L   = 'Customer Name'.
  LS_FCAT-SELTEXT_S   = 'Customer Name'.
  LS_FCAT-SELTEXT_M   = 'Customer Name'.
  LS_FCAT-OUTPUTLEN   = 25.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZTSD_CHECK_PAYIN'.
  LS_FCAT-FIELDNAME   = 'PAYNO'.
  LS_FCAT-OUTPUTLEN   = 13.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'PAYAMT'.
  LS_FCAT-SELTEXT_L   = 'Payin Amount'.
  LS_FCAT-SELTEXT_S   = 'Payin Amount'.
  LS_FCAT-SELTEXT_M   = 'Payin Amount'.
  LS_FCAT-NO_ZERO     = 'X'.
  LS_FCAT-OUTPUTLEN   = 13.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'REMPI'.
  LS_FCAT-SELTEXT_L   = 'Remain Amount'.
  LS_FCAT-SELTEXT_S   = 'Remain Amount'.
  LS_FCAT-SELTEXT_M   = 'Remain Amount'.
  LS_FCAT-NO_ZERO     = 'X'.
  LS_FCAT-DO_SUM      = 'X'.
  LS_FCAT-OUTPUTLEN   = 13.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'EXPEN'.
  LS_FCAT-SELTEXT_L   = 'Expense'.
  LS_FCAT-SELTEXT_S   = 'Expense'.
  LS_FCAT-SELTEXT_M   = 'Expense'.
  LS_FCAT-NO_ZERO     = 'X'.
  LS_FCAT-OUTPUTLEN   = 13.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'INCOM'.
  LS_FCAT-SELTEXT_L   = 'Income'.
  LS_FCAT-SELTEXT_S   = 'Income'.
  LS_FCAT-SELTEXT_M   = 'Income'.
  LS_FCAT-NO_ZERO     = 'X'.
  LS_FCAT-OUTPUTLEN   = 13.
  APPEND LS_FCAT TO GT_FCAT.

*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'USED'.
*  ls_fcat-seltext_l   = 'Amount Use'.
*  ls_fcat-seltext_s   = 'Amount Use'.
*  ls_fcat-seltext_m   = 'Amount Use'.
*  ls_fcat-no_zero     = 'X'.
*  ls_fcat-do_sum      = 'X'.
*  ls_fcat-edit        = 'X'.
*  ls_fcat-outputlen   = 13.
*  APPEND ls_fcat TO gt_fcat.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'PAYDATE'.
  LS_FCAT-SELTEXT_L   = 'Payment date'.
  LS_FCAT-SELTEXT_S   = 'Payment date'.
  LS_FCAT-SELTEXT_M   = 'Payment date'.
  LS_FCAT-OUTPUTLEN   = 13.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'BANK'.
  LS_FCAT-SELTEXT_L   = 'Bank Name'.
  LS_FCAT-SELTEXT_S   = 'Bank Name'.
  LS_FCAT-SELTEXT_M   = 'Bank Name'.
  LS_FCAT-OUTPUTLEN   = 25.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'REF_412'.
  LS_FCAT-SELTEXT_L   = 'Reference'.
  LS_FCAT-SELTEXT_S   = 'Reference'.
  LS_FCAT-SELTEXT_M   = 'Reference'.
  LS_FCAT-OUTPUTLEN   = 25.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'ADVNO'.
  LS_FCAT-SELTEXT_L   = 'Advance No.'.
  LS_FCAT-SELTEXT_S   = 'Advance No.'.
  LS_FCAT-SELTEXT_M   = 'Advance No.'.
  LS_FCAT-OUTPUTLEN   = 25.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'FLACM'.
  LS_FCAT-SELTEXT_L   = 'Falg CM'.
  LS_FCAT-SELTEXT_S   = 'Falg CM'.
  LS_FCAT-SELTEXT_M   = 'Falg CM'.
  LS_FCAT-OUTPUTLEN   = 5.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'REMARK_1'.
  LS_FCAT-SELTEXT_L   = 'Remark'.
  LS_FCAT-SELTEXT_S   = 'Remark'.
  LS_FCAT-SELTEXT_M   = 'Remark'.
  LS_FCAT-OUTPUTLEN   = 25.
  APPEND LS_FCAT TO GT_FCAT.

ENDFORM.                    "build_fcat_1

*&---------------------------------------------------------------------*
*&      Form  F_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SORT .
  CLEAR : GT_SORT[].

*  CLEAR gs_sort.
*  gs_sort-fieldname = 'KUNNR'.
*  gs_sort-spos = '1'.
*  gs_sort-up = 'X'.
*  gs_sort-subtot = 'X'.
*  APPEND gs_sort TO gt_sort.

ENDFORM.                    " F_SORT
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_EXTAB   text
*----------------------------------------------------------------------*
FORM PF_STATUS USING US_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'ZSTANDARD' EXCLUDING US_EXTAB.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
*FORM user_command USING p_ucomm TYPE sy-ucomm
*                        p_selfld TYPE slis_selfield.
**&---------------------------------------------------------------------*
**&for Check = 'X' when tick Check Box
**&---------------------------------------------------------------------*
*  DATA : ref_grid TYPE REF TO cl_gui_alv_grid.
*
*  IF ref_grid IS INITIAL.
*    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*      IMPORTING
*        e_grid = ref_grid.
*    CALL METHOD ref_grid->check_changed_data.
*  ENDIF.
**&---------------------------------------------------------------------*
*
*  CASE p_ucomm.
*    WHEN 'SAVE'.
*      PERFORM f_select_data.
*      IF gt_payin_alv IS INITIAL.
*        MESSAGE s000 WITH 'No Pay In' DISPLAY LIKE 'E'.
*      ENDIF.
*      LEAVE TO SCREEN 0.
*    WHEN 'BACK_1'.
*      CLEAR gt_payin_alv[].
*      LEAVE TO SCREEN 0.
*    WHEN 'ALL'.
*      PERFORM f_select_all.
*    WHEN 'SAL'.
*      PERFORM f_dselect_all.
*    WHEN '&IC1'.
*
*  ENDCASE.
*
*  CALL METHOD ref_grid->refresh_table_display.
*  CLEAR : ref_grid.

*ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_PAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_get_data_payin.
*
*  IF gt_vbak[] IS NOT INITIAL.
*    SELECT ztb_pay_in~payno
*           ztb_pay_in~kunnr
*           kna1~name1
*           ztb_pay_in~paydate
*           ztb_pay_in~payamt
*           ztb_pay_in~rempi
*           ztb_pay_in~expen
*           ztb_pay_in~incom
*           ztb_pay_in~erdat
*           ztb_pay_in~ernam
*           ztb_pay_in~incom
*           ztb_pay_in~ref_412
*           ztb_pay_in~r412dat
*           ztb_pay_in~advno
*           ztb_pay_in~flacm
*           ztb_pay_in~remark_1
*           "ztb_pay_in~rempi AS used
*      FROM ztb_pay_in
*      INNER JOIN kna1 ON ztb_pay_in~kunnr EQ kna1~kunnr
*      INTO CORRESPONDING FIELDS OF TABLE gt_payin_alv
*      FOR ALL ENTRIES IN gt_vbak
*      WHERE ztb_pay_in~kunnr       EQ gt_vbak-kunnr
*        AND ztb_pay_in~used        NE 'X'
*        AND ztb_pay_in~delete_flag NE 'X'
*        AND ztb_pay_in~pmnt_typ    EQ '02'
*        AND ztb_pay_in~payref      EQ space.
*  ENDIF.
*
*ENDFORM.                    " F_GET_DATA_PAYIN
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_select_all .
*  DATA lv_tabix TYPE sy-tabix.
*
*  LOOP AT gt_payin_alv INTO gs_payin_alv.
*    lv_tabix = sy-tabix.
*    gs_payin_alv-check = 'X'.
*
*    MODIFY gt_payin_alv FROM gs_payin_alv INDEX lv_tabix
*                                   TRANSPORTING check.
*  ENDLOOP.
*
*ENDFORM.                    " F_SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  F_DSELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_dselect_all .
*  DATA lv_tabix TYPE sy-tabix.
*
*  LOOP AT gt_payin_alv INTO gs_payin_alv.
*    lv_tabix = sy-tabix.
*    gs_payin_alv-check = space.
*
*    MODIFY gt_payin_alv FROM gs_payin_alv INDEX lv_tabix
*                                   TRANSPORTING check.
*  ENDLOOP.
*
*ENDFORM.                    " F_DSELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_select_data .
*
*  DELETE gt_payin_alv WHERE check NE 'X'.
*
*ENDFORM.                    " F_SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_SHIP_TO_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_SHIP_TO_LINE.

  DATA : BEGIN OF LS_VBAP,
           VBELN TYPE VBAP-VBELN,
           POSNR TYPE VBAP-POSNR,
         END OF LS_VBAP.
  DATA LT_VBAP LIKE TABLE OF LS_VBAP.

  DATA : BEGIN OF LS_VBPA,
           VBELN TYPE VBPA-VBELN,
           POSNR TYPE VBPA-POSNR,
           ADRNR TYPE VBPA-ADRNR,
         END OF LS_VBPA.
  DATA LT_VBPA LIKE TABLE OF LS_VBPA.

  DATA : BEGIN OF LS_ADRC,
           ADDRNUMBER TYPE ADRC-ADDRNUMBER,
           STREET     TYPE ADRC-STREET,
           LOCATION   TYPE ADRC-LOCATION,
           CITY2      TYPE ADRC-CITY2,
           CITY1      TYPE ADRC-CITY1,
         END OF LS_ADRC.
  DATA LT_ADRC LIKE TABLE OF LS_ADRC.

  DATA : LS_ITEM1 TYPE TYP_ITEM,
         LS_ITEM2 TYPE TYP_ITEM2.

*  lwa_header
  CLEAR LT_VBAP[].
  LOOP AT GT_ITEM2 INTO LS_ITEM2.
    LS_VBAP-VBELN = LS_ITEM2-VBELN.
    LS_VBAP-POSNR = LS_ITEM2-POSNR_1.
    APPEND LS_VBAP TO LT_VBAP.
    CLEAR : LS_ITEM2,LS_VBAP.
  ENDLOOP.

  LOOP AT GT_ITEM1 INTO LS_ITEM1.
    LS_VBAP-VBELN = LS_ITEM1-VBELN.
    LS_VBAP-POSNR = LS_ITEM1-POSNR.
    APPEND LS_VBAP TO LT_VBAP.
    CLEAR : LS_ITEM1,LS_VBAP.
  ENDLOOP.

  IF LT_VBAP IS NOT INITIAL.
    SELECT VBELN
           POSNR
           ADRNR
      FROM VBPA
      INTO TABLE LT_VBPA
      FOR ALL ENTRIES IN LT_VBAP
      WHERE VBELN EQ LT_VBAP-VBELN
        AND POSNR EQ LT_VBAP-POSNR
        AND PARVW EQ 'WE'.

    IF LT_VBPA IS NOT INITIAL.
      SELECT ADDRNUMBER
             STREET
             LOCATION
             CITY2
             CITY1
        FROM ADRC
        INTO TABLE LT_ADRC
        FOR ALL ENTRIES IN LT_VBPA
        WHERE ADDRNUMBER EQ LT_VBPA-ADRNR
          AND NATION     EQ SPACE.

      SORT LT_VBAP BY VBELN POSNR.
      READ TABLE LT_VBPA INTO LS_VBPA INDEX 1.
      IF SY-SUBRC = 0.
        READ TABLE LT_ADRC INTO LS_ADRC
        WITH KEY ADDRNUMBER = LS_VBPA-ADRNR.
        IF SY-SUBRC = 0.
          CONCATENATE LS_ADRC-STREET LS_ADRC-LOCATION LS_ADRC-CITY2 LS_ADRC-CITY1 INTO LWA_HEADER-SHIPTOADDR SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_SHIP_TO_LINE
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_PDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_check_pdc CHANGING lv_message.
*  DATA : ls_vbak     TYPE typ_vbak,
*         ls_vbak_tmp TYPE typ_vbak.
*
*  CONSTANTS : lc_zterm TYPE c LENGTH 4 VALUE 'A000',
*              lc_vtweg TYPE c LENGTH 2 VALUE '10'.
*
*  DATA : lt_reamin TYPE TABLE OF ztsd_check_payin,
*         ls_reamin TYPE ztsd_check_payin.
*
*  DATA : lt_ztsd_check_payin TYPE TABLE OF ztsd_check_payin,
*         ls_ztsd_check_payin TYPE ztsd_check_payin.
*
*  RANGES : lr_auart FOR vbak-auart,
*           lr_vkbur FOR vbak-vkbur.
*
*  PERFORM f_get_order_type TABLES lr_auart[].
*  PERFORM f_get_sales_offi TABLES lr_vkbur[].
*
*  DATA : lv_check     TYPE c,
*         lv_kunnr_tmp TYPE kunnr,
*         lv_line      TYPE i,
*         lv_check_dup TYPE c.
*
*  DATA : lv_confirmation.
*
*  CLEAR lv_line.
*  LOOP AT gt_vbak INTO ls_vbak_tmp.
*    ADD 1 TO lv_line.
*
*  ENDLOOP.
*
*  READ TABLE gt_vbak INTO ls_vbak
*  WITH KEY vtweg = lc_vtweg.
*  IF sy-subrc      EQ     0        AND
*     ls_vbak-auart IN     lr_auart AND
*     ls_vbak-vkbur NOT IN lr_vkbur AND
*     ls_vbak-zterm NE     lc_zterm.
*
*    IF lv_line > 1 .
*      MESSAGE i000 WITH 'Can not print over than 1 Document'.
*      LEAVE TO TRANSACTION 'ZT_SD_ORDER_CONFIRM'.
*    ENDIF.
*
*    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*      EXPORTING
*        textline1 = 'Would you like to mathcing payin with sales order?'
*        titel     = 'Use Pay In'
*      IMPORTING
*        answer    = lv_confirmation.
*    IF lv_confirmation = 'J'.
*      DELETE FROM ztsd_check_payin WHERE runid = '999'
*                                     AND vbeln = ls_vbak-vbeln.
*      PERFORM f_check_data_dup USING lv_check_dup.
*      IF lv_check_dup NE 'X'.
*        PERFORM f_get_data_payin_pdc.
*        PERFORM f_show_data.
*        IF gt_payin_alv[] IS NOT INITIAL.
*          PERFORM f_check_amount_payin USING lv_message.
*        ELSE.
*          lv_message = 'Data not found'.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      lv_message = 'Process has been canceled'.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_PDC
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_PAYIN_PDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_get_data_payin_pdc.
*  DATA lv_tabix TYPE sy-tabix.
*
*  IF gt_vbak[] IS NOT INITIAL.
*    SELECT ztb_pay_in~payno
*           ztb_pay_in~kunnr
*           kna1~name1
*           ztb_pay_in~paydate
*           ztb_pay_in~payamt
*           ztb_pay_in~rempi
*           ztb_pay_in~expen
*           ztb_pay_in~incom
*           ztb_pay_in~erdat
*           ztb_pay_in~ernam
*           ztb_pay_in~incom
*           ztb_pay_in~ref_412
*           ztb_pay_in~r412dat
*           ztb_pay_in~advno
*           ztb_pay_in~flacm
*           ztb_pay_in~remark_1
*           "ztb_pay_in~rempi AS used
*      FROM ztb_pay_in
*      INNER JOIN kna1 ON ztb_pay_in~kunnr EQ kna1~kunnr
*      INTO CORRESPONDING FIELDS OF TABLE gt_payin_alv
*      FOR ALL ENTRIES IN gt_vbak
*      WHERE ztb_pay_in~kunnr       EQ gt_vbak-kunnr
*        AND ztb_pay_in~used        NE 'X'
*        AND ztb_pay_in~delete_flag NE 'X'
*        AND ( ztb_pay_in~pmnt_typ  EQ '04' OR
*            ( ztb_pay_in~pmnt_typ  EQ '02' AND ztb_pay_in~payref NE space ) ).
*
**    LOOP AT gt_payin_alv INTO gs_payin_alv.
**      lv_tabix = sy-tabix.
**      gs_payin_alv-rempi = gs_payin_alv-rempi." + gs_payin_alv-expen ) - gs_payin_alv-incom.
**      MODIFY gt_payin_alv FROM gs_payin_alv INDEX lv_tabix
**                                     TRANSPORTING rempi.
**      CLEAR gs_payin_alv.
**    ENDLOOP.
*  ENDIF.
*
*ENDFORM.                    " F_GET_DATA_PAYIN_PDC
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AMOUNT_PAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_check_amount_payin CHANGING lv_message.
**  DATA : BEGIN OF ls_ztb_pay_in,
**    payno  TYPE ztb_pay_in-payno ,
**    kunnr  TYPE ztb_pay_in-kunnr ,
**    payamt TYPE ztb_pay_in-payamt,
**    expen  TYPE ztb_pay_in-expen,
**    incom  TYPE ztb_pay_in-incom,
**    rempi  TYPE ztb_pay_in-rempi,
**  END OF ls_ztb_pay_in.
**  DATA lt_ztb_pay_in LIKE TABLE OF ls_ztb_pay_in.
**
**  DATA : BEGIN OF ls_ztsd_check_payin_tmp,
**    payno        TYPE ztsd_check_payin-payno,
**    runid_payin  TYPE ztsd_check_payin-runid_payin,
**    kunnr        TYPE ztsd_check_payin-kunnr,
**    netwr        TYPE ztsd_check_payin-netwr,
**    payamt       TYPE ztsd_check_payin-payamt,
**    payamt_check TYPE ztsd_check_payin-payamt_check,
**    netwr_check  TYPE ztsd_check_payin-netwr_check,
**  END OF ls_ztsd_check_payin_tmp.
**  DATA : lt_ztsd_check_payin_tmp   LIKE TABLE OF ls_ztsd_check_payin_tmp,
**         ls_ztsd_check_payin_tmp_a LIKE ls_ztsd_check_payin_tmp.
**
**  DATA : BEGIN OF ls_ztsd_check_payin_tmp_1,
**    payno        TYPE ztsd_check_payin-payno,
**    runid_payin  TYPE ztsd_check_payin-runid_payin,
**    kunnr        TYPE ztsd_check_payin-kunnr,
**    netwr        TYPE ztsd_check_payin-netwr,
**    payamt       TYPE ztsd_check_payin-payamt,
**    payamt_check TYPE ztsd_check_payin-payamt_check,
**    netwr_check  TYPE ztsd_check_payin-netwr_check,
**  END OF ls_ztsd_check_payin_tmp_1.
**  DATA : lt_ztsd_check_payin_tmp_1 LIKE TABLE OF ls_ztsd_check_payin_tmp_1,
**         lt_ztsd_check_payin_tmp_2 LIKE TABLE OF ls_ztsd_check_payin_tmp_1,
**         ls_ztsd_check_payin_tmp_2 LIKE ls_ztsd_check_payin_tmp_1.
**
*  DATA : BEGIN OF ls_vbap,
*           vbeln TYPE vbap-vbeln,
*           posnr TYPE vbap-posnr,
*           netwr TYPE vbap-netwr,
*           mwsbp TYPE vbap-mwsbp,
*           kunnr TYPE vbak-kunnr,
*         END OF ls_vbap.
*  DATA lt_vbap LIKE TABLE OF ls_vbap.
**
**  DATA : BEGIN OF ls_check_amount,
**   vbeln TYPE vbap-vbeln,
**   edatu TYPE vbep-edatu,
**   total TYPE vbap-netwr,
**  END OF ls_check_amount.
**  DATA lt_check_amount LIKE TABLE OF ls_check_amount.
**
**  DATA : ls_ztsd_check_payin TYPE ztsd_check_payin,
**         lt_insert_data      TYPE TABLE OF ztsd_check_payin,
**         ls_insert_data      TYPE ztsd_check_payin.
**
**  DATA : lv_run TYPE i,
**         lv_reamin_netwr TYPE vbak-netwr,
**         lv_check_change TYPE c.
**
**
**
*  DATA : lv_tabix      TYPE sy-tabix.
**         lv_change_all TYPE c.
**
**  DATA : ls_ztsd_line_payin       TYPE ztsd_line_payin,
**         lt_ztsd_line_payin       TYPE TABLE OF ztsd_line_payin,
**         ls_ztsd_line_payin_check TYPE ztsd_line_payin,
**         lt_ztsd_line_payin_check TYPE TABLE OF ztsd_line_payin.
**
**  DATA : lt_update TYPE TABLE OF ztsd_line_payin,
**         ls_update TYPE ztsd_line_payin,
**         lt_insert TYPE TABLE OF ztsd_line_payin,
**         ls_insert TYPE ztsd_line_payin.
*  DATA : BEGIN OF ls_ztsd_tmp_pdc_check,
*           vbeln TYPE ztsd_tmp_pdc-vbeln,
*           posnr TYPE ztsd_tmp_pdc-posnr,
*         END OF ls_ztsd_tmp_pdc_check.
*  DATA lt_ztsd_tmp_pdc_check LIKE TABLE OF ls_ztsd_tmp_pdc_check.
*
*  DATA : BEGIN OF ls_ztsd_pdc_tran,
*           vbeln TYPE ztsd_pdc_tran-vbeln,
*           posnr TYPE ztsd_pdc_tran-posnr,
*         END OF ls_ztsd_pdc_tran.
*  DATA lt_ztsd_pdc_tran LIKE TABLE OF ls_ztsd_pdc_tran.
*
*  DATA : ls_ztsd_tmp_pdc TYPE ztsd_tmp_pdc,
*         lt_ztsd_tmp_pdc TYPE TABLE OF ztsd_tmp_pdc.
*
*  DATA : lt_item2 LIKE gt_item2,
*         ls_item2 TYPE typ_item2,
*         ls_item1 TYPE typ_item.
*
*  DATA : lv_total     TYPE vbap-netwr,
*         lv_used      TYPE vbap-netwr,
*         lv_over      TYPE c,
*         lv_vbeln     TYPE vbak-vbeln,
*         lv_check_out TYPE vbap-netwr.
*
*  lt_item2[] = gt_item2[].
*
*  LOOP AT gt_item1 INTO ls_item1.
*    READ TABLE gt_item2 INTO ls_item2
*    WITH KEY vbeln = ls_item1-vbeln
*             posnr = ls_item1-posnr.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
*    ls_item2-vbeln   = ls_item1-vbeln.
*    ls_item2-posnr_1 = ls_item1-posnr.
*    ls_item2-edatu   = ls_item1-edatu.
*
*    APPEND ls_item2 TO lt_item2.
*
*  ENDLOOP.
*
*  SORT lt_item2 BY posnr_1.
*  DELETE ADJACENT DUPLICATES FROM lt_item2 COMPARING posnr_1.
**
**  IF lt_item2 IS NOT INITIAL.
**    SELECT vbap~vbeln
**           vbap~posnr
**           vbap~netwr
**           vbap~mwsbp
**           vbak~kunnr
**      FROM vbap
**      INNER JOIN vbak ON vbap~vbeln EQ vbak~vbeln
**      INTO TABLE lt_vbap
**      FOR ALL ENTRIES IN lt_item2
**      WHERE vbap~vbeln = lt_item2-vbeln
**        AND vbap~posnr = lt_item2-posnr_1.
**
**    SELECT vbeln
**           posnr
**      FROM ztsd_tmp_pdc
**      INTO TABLE lt_ztsd_tmp_pdc_check
**      FOR ALL ENTRIES IN lt_item2
**      WHERE vbeln = lt_item2-vbeln
**        AND posnr = lt_item2-posnr_1.
**
**    SELECT vbeln
**           posnr
**      FROM ztsd_pdc_tran
**      INTO TABLE lt_ztsd_pdc_tran
**      FOR ALL ENTRIES IN lt_item2
**      WHERE vbeln = lt_item2-vbeln
**        AND posnr = lt_item2-posnr_1.
**
**    APPEND LINES OF lt_ztsd_pdc_tran TO lt_ztsd_tmp_pdc_check.
**
**    SORT lt_ztsd_tmp_pdc_check BY vbeln posnr.
**    DELETE ADJACENT DUPLICATES FROM lt_ztsd_tmp_pdc_check COMPARING ALL FIELDS.
**
**  ENDIF.
**
**  SORT gt_payin_alv BY kunnr payno.
**
**  LOOP AT lt_vbap INTO ls_vbap.
**    lv_tabix = sy-tabix.
**    READ TABLE lt_ztsd_tmp_pdc_check INTO ls_ztsd_tmp_pdc_check
**    WITH KEY vbeln = ls_ztsd_tmp_pdc_check-vbeln
**             posnr = ls_ztsd_tmp_pdc_check-posnr.
**    IF sy-subrc EQ 0.
**      DELETE lt_vbap INDEX lv_tabix.
**    ENDIF.
**    CLEAR : ls_vbap,ls_ztsd_tmp_pdc_check.
**  ENDLOOP.
*
*  CLEAR : lv_over,lv_tabix.
*  LOOP AT gt_vbap_pdc INTO ls_vbap.
*    lv_vbeln = ls_vbap-vbeln.
*    CLEAR : lv_total,lv_check_out,lv_used.
**    lv_total = ls_vbap-netwr + ls_vbap-mwsbp.
*    IF ls_vbap-mwsbp IS NOT INITIAL.
*      lv_total = ls_vbap-netwr + ls_vbap-mwsbp.
*    ELSE.
*      lv_total = ( ls_vbap-netwr + ( ( ls_vbap-netwr * 7 ) / 100 ) ).
*    ENDIF.
*    LOOP AT gt_payin_alv INTO gs_payin_alv WHERE used NE 'X'.
*      lv_tabix = sy-tabix.
*      ls_ztsd_tmp_pdc-payno = gs_payin_alv-payno.
*      ls_ztsd_tmp_pdc-vbeln = ls_vbap-vbeln.
*      ls_ztsd_tmp_pdc-posnr = ls_vbap-posnr.
*      ls_ztsd_tmp_pdc-kunnr = ls_vbap-kunnr.
*
*      lv_used      = gs_payin_alv-rempi - lv_total.
*      lv_check_out = lv_total - gs_payin_alv-rempi.
*
*      READ TABLE lt_item2 INTO ls_item2
*      WITH KEY vbeln   = ls_vbap-vbeln
*               posnr_1 = ls_vbap-posnr.
*      IF sy-subrc = 0.
*        ls_ztsd_tmp_pdc-edatu = ls_item2-edatu.
*      ENDIF.
*
*      ls_ztsd_tmp_pdc-ernam = sy-uname.
*      ls_ztsd_tmp_pdc-erdat = sy-datum.
*      ls_ztsd_tmp_pdc-erzet = sy-uzeit.
*
*      IF lv_used LE 0.
*        ls_ztsd_tmp_pdc-payue = gs_payin_alv-rempi.
*        gs_payin_alv-rempi    = 0.
*        gs_payin_alv-used     = 'X'.
*        APPEND ls_ztsd_tmp_pdc TO lt_ztsd_tmp_pdc.
*      ELSE.
*        ls_ztsd_tmp_pdc-payue = lv_total.
*        gs_payin_alv-rempi    = gs_payin_alv-rempi - lv_total.
*        APPEND ls_ztsd_tmp_pdc TO lt_ztsd_tmp_pdc.
*      ENDIF.
*
*      MODIFY gt_payin_alv FROM gs_payin_alv INDEX lv_tabix.
*      lv_total = lv_check_out.
*      IF lv_total LE 0.
*        EXIT.
*      ENDIF.
*
*      CLEAR : gs_payin_alv,ls_item2.
*    ENDLOOP.
*    IF ( lv_total GT 0 OR sy-subrc NE 0 ) AND
*         lv_total NE 0.
*      lv_over = 'X'.
*    ENDIF.
*    CLEAR : ls_vbap.
*  ENDLOOP.
*
*  IF lv_over NE 'X'.
*    INSERT ztsd_tmp_pdc FROM TABLE lt_ztsd_tmp_pdc.
*
*    LOOP AT gt_payin_alv INTO gs_payin_alv.
*
*      UPDATE ztb_pay_in SET chepi = 'X'
*                            rempi = gs_payin_alv-rempi
*                            rmamt = gs_payin_alv-rempi
*                            used  = gs_payin_alv-used
*                      WHERE payno EQ gs_payin_alv-payno
*                        AND kunnr EQ gs_payin_alv-kunnr.
*
*      CLEAR gs_payin_alv.
*    ENDLOOP.
*  ELSE.
*    lv_message = 'Over PDC'.
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_AMOUNT_PAYIN
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_check_data_dup USING lv_check_dup.
*  DATA : lv_tabix      TYPE sy-tabix.
*  DATA ls_vbap TYPE gy_vbap_pdc.
*
*  DATA : BEGIN OF ls_ztsd_tmp_pdc_check,
*           vbeln TYPE ztsd_tmp_pdc-vbeln,
*           posnr TYPE ztsd_tmp_pdc-posnr,
*         END OF ls_ztsd_tmp_pdc_check.
*  DATA lt_ztsd_tmp_pdc_check LIKE TABLE OF ls_ztsd_tmp_pdc_check.
*
*  DATA : BEGIN OF ls_ztsd_pdc_tran,
*           vbeln TYPE ztsd_pdc_tran-vbeln,
*           posnr TYPE ztsd_pdc_tran-posnr,
*         END OF ls_ztsd_pdc_tran.
*  DATA lt_ztsd_pdc_tran LIKE TABLE OF ls_ztsd_pdc_tran.
*
*  DATA : ls_ztsd_tmp_pdc TYPE ztsd_tmp_pdc,
*         lt_ztsd_tmp_pdc TYPE TABLE OF ztsd_tmp_pdc.
*
*  DATA : lt_item2 LIKE gt_item2,
*         ls_item2 TYPE typ_item2,
*         ls_item1 TYPE typ_item.
*
*  DATA : lv_total TYPE vbap-netwr,
*         lv_used  TYPE vbap-netwr,
*         lv_over  TYPE c,
*         lv_vbeln TYPE vbak-vbeln.
*
*  lt_item2[] = gt_item2[].
*  CLEAR gt_vbap_pdc[].
*
*  LOOP AT gt_item1 INTO ls_item1.
*    READ TABLE gt_item2 INTO ls_item2
*    WITH KEY vbeln = ls_item1-vbeln
*             posnr = ls_item1-posnr.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
*    ls_item2-vbeln   = ls_item1-vbeln.
*    ls_item2-posnr_1 = ls_item1-posnr.
*    ls_item2-edatu   = ls_item1-edatu.
*
*    APPEND ls_item2 TO lt_item2.
*
*  ENDLOOP.
*
*  SORT lt_item2 BY posnr_1.
*  DELETE ADJACENT DUPLICATES FROM lt_item2 COMPARING posnr_1.
*
*  IF lt_item2 IS NOT INITIAL.
*    SELECT vbap~vbeln
*           vbap~posnr
*           vbap~netwr
*           vbap~mwsbp
*           vbak~kunnr
*      FROM vbap
*      INNER JOIN vbak ON vbap~vbeln EQ vbak~vbeln
*      INTO TABLE gt_vbap_pdc
*      FOR ALL ENTRIES IN lt_item2
*      WHERE vbap~vbeln = lt_item2-vbeln
*        AND vbap~posnr = lt_item2-posnr_1.
*
*    SELECT vbeln
*           posnr
*      FROM ztsd_tmp_pdc
*      INTO TABLE lt_ztsd_tmp_pdc_check
*      FOR ALL ENTRIES IN lt_item2
*      WHERE vbeln = lt_item2-vbeln
*        AND posnr = lt_item2-posnr_1.
*
*    SELECT vbeln
*           posnr
*      FROM ztsd_pdc_tran
*      INTO TABLE lt_ztsd_pdc_tran
*      FOR ALL ENTRIES IN lt_item2
*      WHERE vbeln EQ lt_item2-vbeln
*        AND posnr EQ lt_item2-posnr_1
*        AND dfalg NE 'X'.
*
*    APPEND LINES OF lt_ztsd_pdc_tran TO lt_ztsd_tmp_pdc_check.
*
*    SORT lt_ztsd_tmp_pdc_check BY vbeln posnr.
*    DELETE ADJACENT DUPLICATES FROM lt_ztsd_tmp_pdc_check COMPARING ALL FIELDS.
*
*  ENDIF.
*
*  SORT gt_payin_alv BY kunnr payno.
*
*  LOOP AT gt_vbap_pdc INTO ls_vbap.
*    lv_tabix = sy-tabix.
*    READ TABLE lt_ztsd_tmp_pdc_check INTO ls_ztsd_tmp_pdc_check
*    WITH KEY vbeln = ls_vbap-vbeln
*             posnr = ls_vbap-posnr.
*    IF sy-subrc EQ 0.
*      DELETE gt_vbap_pdc INDEX lv_tabix.
*    ENDIF.
*    CLEAR : ls_vbap,ls_ztsd_tmp_pdc_check.
*  ENDLOOP.
*
*  DELETE gt_vbap_pdc WHERE netwr EQ 0.
*
*  IF gt_vbap_pdc[] IS INITIAL.
*    lv_check_dup = 'X'.
*  ENDIF.
*ENDFORM.                    " F_CHECK_DATA_DUP
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_PAYIN_NEW_VERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_check_payin_new_version CHANGING lv_message.
*  DATA : ls_vbak     TYPE typ_vbak,
*         ls_vbak_tmp TYPE typ_vbak.
*
*  CONSTANTS : lc_zterm TYPE c LENGTH 4 VALUE 'A000',
*              lc_vtweg TYPE c LENGTH 2 VALUE '10'.
*
*  DATA : lt_reamin TYPE TABLE OF ztsd_check_payin,
*         ls_reamin TYPE ztsd_check_payin.
*
*  DATA : lt_ztsd_check_payin TYPE TABLE OF ztsd_check_payin,
*         ls_ztsd_check_payin TYPE ztsd_check_payin.
*
*  RANGES : lr_auart FOR vbak-auart,
*           lr_vkbur FOR vbak-vkbur.
*
*  PERFORM f_get_order_type TABLES lr_auart[].
*  PERFORM f_get_sales_offi TABLES lr_vkbur[].
*
*  DATA : lv_check     TYPE c,
*         lv_kunnr_tmp TYPE kunnr,
*         lv_line      TYPE i,
*         lv_check_dup TYPE c.
*
*  DATA : lv_confirmation.
*
*  CLEAR lv_line.
*  LOOP AT gt_vbak INTO ls_vbak_tmp.
*    ADD 1 TO lv_line.
*  ENDLOOP.
*
*  READ TABLE gt_vbak INTO ls_vbak
*  WITH KEY  zterm = lc_zterm
*            vtweg = lc_vtweg.
*  IF sy-subrc = 0 AND ls_vbak-auart IN lr_auart
*                  AND ls_vbak-vkbur NOT IN lr_vkbur.
*
*    IF lv_line > 1 .
*      MESSAGE i000 WITH 'Can not print over than 1 Document'.
*      LEAVE TO TRANSACTION 'ZT_SD_ORDER_CONFIRM'.
*    ENDIF.
*
*    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*      EXPORTING
*        textline1 = 'Would you like to mathcing payin with sales order?'
*        titel     = 'Use Pay In'
*      IMPORTING
*        answer    = lv_confirmation.
*    IF lv_confirmation = 'J'.
*      DELETE FROM ztsd_check_payin WHERE runid = '999'
*                                     AND waipi = 'X'
*                                     AND vbeln = ls_vbak-vbeln.
*
*      COMMIT WORK AND WAIT.
*
*      PERFORM f_check_data_dup_payin USING lv_check_dup.
*      IF lv_check_dup NE 'X'.
*        PERFORM f_get_data_payin.
*        PERFORM f_show_data.
*        IF gt_payin_alv[] IS NOT INITIAL.
*          PERFORM f_check_amount_payin_new USING lv_message.
*        ELSE.
*          lv_message = 'Data not found'.
*        ENDIF.
*      ENDIF.
*
*    ELSE.
*      PERFORM f_show_error USING ls_vbak.
**      lv_message = 'Process has been canceled'.
*    ENDIF.
*  ELSE.
*    DATA lv_chk_pay LIKE ls_vbak-vbeln.
*    SELECT SINGLE vbeln
*      FROM ztsd_check_payin
*      INTO lv_chk_pay
*      WHERE runid EQ '999'
*        AND waipi EQ 'X'
*        AND vbeln EQ ls_vbak-vbeln.
*    IF sy-subrc EQ 0.
*      DELETE FROM ztsd_check_payin WHERE runid = '999'
*                                     AND waipi = 'X'
*                                     AND vbeln = ls_vbak-vbeln.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_PAYIN_NEW_VERSION
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA_DUP_PAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_CHECK_DUP  text
*----------------------------------------------------------------------*
*FORM f_check_data_dup_payin  USING lv_check_dup.
*  DATA : lv_tabix      TYPE sy-tabix.
*  DATA ls_vbap TYPE gy_vbap_pdc.
*
*  DATA : BEGIN OF ls_ztsd_tmp_payin_check,
*           vbeln TYPE ztsd_tmp_payin-vbeln,
*           posnr TYPE ztsd_tmp_payin-posnr,
*         END OF ls_ztsd_tmp_payin_check.
*  DATA lt_ztsd_tmp_payin_check LIKE TABLE OF ls_ztsd_tmp_payin_check.
*
*  DATA : BEGIN OF ls_ztsd_payin_tran,
*           vbeln TYPE ztsd_payin_tran-vbeln,
*           posnr TYPE ztsd_payin_tran-posnr,
*         END OF ls_ztsd_payin_tran.
*  DATA lt_ztsd_payin_tran LIKE TABLE OF ls_ztsd_payin_tran.
*
*  DATA : ls_ztsd_tmp_payin TYPE ztsd_tmp_payin,
*         lt_ztsd_tmp_payin TYPE TABLE OF ztsd_tmp_payin.
*
*  DATA : lt_item2 LIKE gt_item2,
*         ls_item2 TYPE typ_item2,
*         ls_item1 TYPE typ_item.
*
*  DATA : lv_total TYPE vbap-netwr,
*         lv_used  TYPE vbap-netwr,
*         lv_over  TYPE c,
*         lv_vbeln TYPE vbak-vbeln.
*
*  lt_item2[] = gt_item2[].
*  CLEAR gt_vbap_pdc[].
*
*  LOOP AT gt_item1 INTO ls_item1.
*    READ TABLE gt_item2 INTO ls_item2
*    WITH KEY vbeln = ls_item1-vbeln
*             posnr = ls_item1-posnr.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
*    ls_item2-vbeln   = ls_item1-vbeln.
*    ls_item2-posnr_1 = ls_item1-posnr.
*    ls_item2-edatu   = ls_item1-edatu.
*
*    APPEND ls_item2 TO lt_item2.
*
*  ENDLOOP.
*
*  SORT lt_item2 BY posnr_1.
*  DELETE ADJACENT DUPLICATES FROM lt_item2 COMPARING posnr_1.
*
*  IF lt_item2 IS NOT INITIAL.
*    SELECT vbap~vbeln
*           vbap~posnr
*           vbap~netwr
*           vbap~mwsbp
*           vbak~kunnr
*      FROM vbap
*      INNER JOIN vbak ON vbap~vbeln EQ vbak~vbeln
*      INTO TABLE gt_vbap_pdc
*      FOR ALL ENTRIES IN lt_item2
*      WHERE vbap~vbeln = lt_item2-vbeln
*        AND vbap~posnr = lt_item2-posnr_1.
*
*    SELECT vbeln
*           posnr
*      FROM ztsd_tmp_payin
*      INTO TABLE lt_ztsd_tmp_payin_check
*      FOR ALL ENTRIES IN lt_item2
*      WHERE vbeln = lt_item2-vbeln
*        AND posnr = lt_item2-posnr_1.
*
*    SELECT vbeln
*           posnr
*      FROM ztsd_payin_tran
*      INTO TABLE lt_ztsd_payin_tran
*      FOR ALL ENTRIES IN lt_item2
*      WHERE vbeln EQ lt_item2-vbeln
*        AND posnr EQ lt_item2-posnr_1
*        AND dfalg NE 'X'.
*
*    APPEND LINES OF lt_ztsd_payin_tran TO lt_ztsd_tmp_payin_check.
*
*    SORT lt_ztsd_tmp_payin_check BY vbeln posnr.
*    DELETE ADJACENT DUPLICATES FROM lt_ztsd_tmp_payin_check COMPARING ALL FIELDS.
*
*  ENDIF.
*
*  SORT gt_payin_alv BY kunnr payno.
*
*  LOOP AT gt_vbap_pdc INTO ls_vbap.
*    lv_tabix = sy-tabix.
*    READ TABLE lt_ztsd_tmp_payin_check INTO ls_ztsd_tmp_payin_check
*    WITH KEY vbeln = ls_vbap-vbeln
*             posnr = ls_vbap-posnr.
*    IF sy-subrc EQ 0.
*      DELETE gt_vbap_pdc INDEX lv_tabix.
*    ENDIF.
*    CLEAR : ls_vbap,ls_ztsd_tmp_payin_check.
*  ENDLOOP.
*
*  DELETE gt_vbap_pdc WHERE netwr EQ 0.
*
*  IF gt_vbap_pdc[] IS INITIAL.
*    lv_check_dup = 'X'.
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_DATA_DUP_PAYIN
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AMOUNT_PAYIN_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_MESSAGE  text
*----------------------------------------------------------------------*
*FORM f_check_amount_payin_new USING lv_message.
*
*  DATA : BEGIN OF ls_vbap,
*           vbeln TYPE vbap-vbeln,
*           posnr TYPE vbap-posnr,
*           netwr TYPE vbap-netwr,
*           mwsbp TYPE vbap-mwsbp,
*           kunnr TYPE vbak-kunnr,
*         END OF ls_vbap.
*  DATA lt_vbap LIKE TABLE OF ls_vbap.
*
*  DATA : lv_tabix      TYPE sy-tabix.
*
*  DATA : BEGIN OF ls_ztsd_tmp_payin_check,
*           vbeln TYPE ztsd_tmp_payin-vbeln,
*           posnr TYPE ztsd_tmp_payin-posnr,
*         END OF ls_ztsd_tmp_payin_check.
*  DATA lt_ztsd_tmp_payin_check LIKE TABLE OF ls_ztsd_tmp_payin_check.
*
*  DATA : BEGIN OF ls_ztsd_payin_tran,
*           vbeln TYPE ztsd_payin_tran-vbeln,
*           posnr TYPE ztsd_payin_tran-posnr,
*         END OF ls_ztsd_payin_tran.
*  DATA lt_ztsd_payin_tran LIKE TABLE OF ls_ztsd_payin_tran.
*
*  DATA : ls_ztsd_tmp_payin TYPE ztsd_tmp_payin,
*         lt_ztsd_tmp_payin TYPE TABLE OF ztsd_tmp_payin.
*
*  DATA : lt_item2 LIKE gt_item2,
*         ls_item2 TYPE typ_item2,
*         ls_item1 TYPE typ_item.
*
*  DATA : lv_total     TYPE vbap-netwr,
*         lv_used      TYPE vbap-netwr,
*         lv_over      TYPE c,
*         lv_vbeln     TYPE vbak-vbeln,
*         lv_check_out TYPE vbap-netwr.
*
*  DATA : lv_expen   TYPE ztb_pay_in-expen,
*         lv_incom   TYPE ztb_pay_in-incom,
*         lv_overpay TYPE ztb_pay_in-rempi.
*
*  lt_item2[] = gt_item2[].
*
*  LOOP AT gt_item1 INTO ls_item1.
*    READ TABLE gt_item2 INTO ls_item2
*    WITH KEY vbeln = ls_item1-vbeln
*             posnr = ls_item1-posnr.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
*    ls_item2-vbeln   = ls_item1-vbeln.
*    ls_item2-posnr_1 = ls_item1-posnr.
*    ls_item2-edatu   = ls_item1-edatu.
*
*    APPEND ls_item2 TO lt_item2.
*
*  ENDLOOP.
*
*  SORT lt_item2 BY posnr_1.
*  DELETE ADJACENT DUPLICATES FROM lt_item2 COMPARING posnr_1.
*
*  CLEAR : lv_over,lv_tabix.
*  LOOP AT gt_vbap_pdc INTO ls_vbap.
*    lv_vbeln = ls_vbap-vbeln.
*    CLEAR : lv_total,lv_check_out,lv_used.
*    IF ls_vbap-mwsbp IS NOT INITIAL.
*      lv_total = ls_vbap-netwr + ls_vbap-mwsbp.
*    ELSE.
*      lv_total = ( ls_vbap-netwr + ( ( ls_vbap-netwr * 7 ) / 100 ) ).
*    ENDIF.
*    LOOP AT gt_payin_alv INTO gs_payin_alv WHERE used NE 'X'.
*      lv_tabix = sy-tabix.
*      ls_ztsd_tmp_payin-payno = gs_payin_alv-payno.
*      ls_ztsd_tmp_payin-vbeln = ls_vbap-vbeln.
*      ls_ztsd_tmp_payin-posnr = ls_vbap-posnr.
*      ls_ztsd_tmp_payin-kunnr = ls_vbap-kunnr.
*
*      lv_used      = gs_payin_alv-rempi - lv_total.
*      lv_check_out = lv_total - gs_payin_alv-rempi.
*
*      READ TABLE lt_item2 INTO ls_item2
*      WITH KEY vbeln   = ls_vbap-vbeln
*               posnr_1 = ls_vbap-posnr.
*      IF sy-subrc = 0.
*        ls_ztsd_tmp_payin-edatu = ls_item2-edatu.
*      ENDIF.
*
*      ls_ztsd_tmp_payin-ernam = sy-uname.
*      ls_ztsd_tmp_payin-erdat = sy-datum.
*      ls_ztsd_tmp_payin-erzet = sy-uzeit.
*
*      IF lv_used LE 0.
*        ls_ztsd_tmp_payin-payue = gs_payin_alv-rempi.
*        gs_payin_alv-rempi    = 0.
*        gs_payin_alv-used     = 'X'.
*        APPEND ls_ztsd_tmp_payin TO lt_ztsd_tmp_payin.
*      ELSE.
*        ls_ztsd_tmp_payin-payue = lv_total.
*        gs_payin_alv-rempi    = gs_payin_alv-rempi - lv_total.
*        APPEND ls_ztsd_tmp_payin TO lt_ztsd_tmp_payin.
*      ENDIF.
*
*      MODIFY gt_payin_alv FROM gs_payin_alv INDEX lv_tabix.
*      lv_total = lv_check_out.
*      IF lv_total LE 0.
*        EXIT.
*      ENDIF.
*
*      CLEAR : gs_payin_alv,ls_item2.
*    ENDLOOP.
*    IF ( lv_total GT 0 OR sy-subrc NE 0 ) AND
*         lv_total NE 0.
*      IF lv_total GT 0 AND
*         lv_total LE 1.
*        lv_expen = lv_total.
*      ELSE.
*        lv_overpay = lv_total.
*        lv_over    = 'X'.
*      ENDIF.
*    ENDIF.
*    CLEAR : ls_vbap.
*  ENDLOOP.
*
*  DATA : lv_updata_expen LIKE lv_expen,
*         lv_text_over    TYPE c LENGTH 15.
*
*  IF lv_over NE 'X'.
*    INSERT ztsd_tmp_payin FROM TABLE lt_ztsd_tmp_payin.
*
*    DATA ls_payin_alv LIKE gs_payin_alv.
*    LOOP AT gt_payin_alv INTO ls_payin_alv.
*      MOVE-CORRESPONDING ls_payin_alv TO gs_payin_alv.
**      lv_incom
*      CLEAR lv_updata_expen.
*      AT LAST.
*        IF lv_expen IS NOT INITIAL.
*          lv_updata_expen = lv_expen.
*        ENDIF.
*      ENDAT.
*
*      IF gs_payin_alv-rempi GT 0 AND
*         gs_payin_alv-rempi LT 1.
*        lv_incom            = gs_payin_alv-rempi.
*        gs_payin_alv-rempi  = 0.
*        gs_payin_alv-used   = 'X'.
*      ENDIF.
*
*      UPDATE ztb_pay_in SET chepi = 'X'
*                            rempi = gs_payin_alv-rempi
*                            rmamt = gs_payin_alv-rempi
*                            used  = gs_payin_alv-used
*                            expen = lv_updata_expen
*                            incom = lv_incom
*                      WHERE payno EQ gs_payin_alv-payno
*                        AND kunnr EQ gs_payin_alv-kunnr.
*
*      CLEAR gs_payin_alv.
*    ENDLOOP.
*  ELSE.
*    lv_text_over = lv_overpay.
*    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN lv_text_over WITH ''.
*    CONCATENATE 'Over PayIN' lv_text_over 'THB' INTO lv_message SEPARATED BY space.
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_AMOUNT_PAYIN_NEW
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_check_selection .
*  CLEAR s_nopay.
*  s_nopay-sign   = 'I'.
*  s_nopay-option = 'EQ'.
*  s_nopay-low    = 'TESTPAYIN'.
*  APPEND s_nopay.
*
*  IF sy-uname EQ 'SDSBATCH' OR
*     sy-uname EQ 'JAKARIN'.
*
*  ELSE.
*    LOOP AT SCREEN.
*      IF screen-group1 = 'PAY'.
*        screen-intensified = '1'.
*        screen-active      = '0'.
*        screen-display_3d  = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.                    " F_CHECK_SELECTION
*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTION_SPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_get_selection_spe.
*
*  DATA : ls_spe TYPE ztsd_cond_spe,
*         lt_spe TYPE TABLE OF ztsd_cond_spe.
*
*  SELECT *
*    FROM ztsd_cond_spe
*    INTO TABLE lt_spe
*    WHERE repid EQ 'Z_SD_ORDER_CONFIRMATION4'.
*
*  LOOP AT lt_spe INTO ls_spe.
*    CLEAR s_matnr.
*    s_matnr-sign   = 'I'.
*    s_matnr-option = 'EQ'.
*    s_matnr-low    = ls_spe-intxt.
*    APPEND s_matnr.
*
*    CLEAR : ls_spe.
*  ENDLOOP.
*
*
*ENDFORM.                    " F_GET_SELECTION_SPE
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA_SPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_check_data_spe .
*  DATA : lv_matnr TYPE vbak-vbeln,
*         lv_posnr TYPE vbap-posnr.
*
*  DATA : lv_message TYPE c LENGTH 255.
*
*  IF s_matnr[] IS NOT INITIAL.
*    SELECT SINGLE vbap~matnr
*                  vbap~posnr
*      FROM vbap
*      INNER JOIN vbep ON vbap~vbeln EQ vbep~vbeln AND
*                         vbap~posnr EQ vbep~posnr
*      INTO (lv_matnr,lv_posnr)
*      WHERE vbap~vbeln IN s_vbeln
*        AND vbep~edatu IN s_edatu
*        AND vbap~matnr IN s_matnr.
*    IF sy-subrc = 0.
*      CONCATENATE lv_matnr 'line' lv_posnr 'is invalid' INTO lv_message.
*      MESSAGE s000 WITH lv_message DISPLAY LIKE 'E'.
*      LEAVE TO SCREEN 0.
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " F_CHECK_DATA_SPE

*&---------------------------------------------------------------------*
*&      Form  IMPROVE_DATA_HEADER
*&---------------------------------------------------------------------*
*Condition as following below.
* 1.Check Sales Office/Sales Group to show "Wrong Sales Office/Sales Group"
* 2.Contract start date   >>   "Wrong Contract Start date"
*   Contract end date     >>   "Wrong Contract end date"
*   Val.period category   >>   "Wrong Val. period cateory"
*   Contract Val.period   >>   "Wrong Val.period"
* 3.Billing Plan Header and Billing Plan by Line Item mismatch
*   to show "Billing Plan not match Contract Start Date /End Date"
* 4.Manaul Cost < 35% of net price   "Check Manual Cost"
*----------------------------------------------------------------------*
*FORM improve_data_header  CHANGING p_over_dis TYPE c.
*
*  DATA: gt_vbak_qt TYPE vbak OCCURS 0,
*        gt_konv    TYPE konv OCCURS 0,
*        gt_vbak    TYPE vbak OCCURS 0.
*
*  SELECT * FROM vbak INTO TABLE gt_vbak_qt
*    WHERE vbeln = lwa_header-quotn.
*
*  SELECT * FROM vbak INTO TABLE gt_vbak
*    WHERE vbeln = lwa_header-vbeln.
*
*  SELECT * FROM konv INTO TABLE gt_konv
*    FOR ALL ENTRIES IN gt_vbak
*    WHERE knumv = gt_vbak-knumv
*      AND kschl = 'PR00'. "CH5
*
*
*  DATA: vkbetr TYPE  konv-kbetr,
*        vnetwr TYPE  vbak-netwr.
*  CLEAR: vkbetr, vnetwr.
*
*  "CH5 Add by Wantanee 20211222 check discount over
*
*  DATA : BEGIN OF ls_ztsd_disc_qt,
*           kunnr TYPE ztsd_disc_qt-kunnr,
*           discr TYPE ztsd_disc_qt-discr,
*         END OF ls_ztsd_disc_qt.
*  DATA: lv_sum   TYPE ztmm_map_loa-amount_l.
*  DATA: lv_status_qt TYPE ztsd_status_qt-vbeln.
*  DATA: lv_ihrez TYPE vbkd-ihrez.
*  DATA: gv_vbak TYPE vbak,
*        gw_konv TYPE konv,
*        gv_konv TYPE konv.
*
*  LOOP AT gt_vbak INTO gv_vbak.
*    CLEAR: vnetwr,lv_sum,lv_ihrez,lv_status_qt.
*    LOOP AT gt_konv INTO gv_konv WHERE knumv = gv_vbak-knumv AND
*                                       kschl = 'PR00'.
*
*      vnetwr = vnetwr + gv_konv-kwert.
*
*    ENDLOOP.
*
*    IF vnetwr NE 0.
*      lv_sum =  ( gv_vbak-netwr * 100 ) / vnetwr .
*      lv_sum =  100 - lv_sum .
*    ENDIF.
*
*    CLEAR ls_ztsd_disc_qt.
*    IF lv_sum NE 0.
*      SELECT SINGLE kunnr
*                    discr
*        FROM ztsd_disc_qt
*        INTO ls_ztsd_disc_qt
*        WHERE kunnr EQ gv_vbak-kunnr.
*
*      IF sy-subrc EQ 0.
*        IF ls_ztsd_disc_qt-discr IS NOT INITIAL AND
*           lv_sum GT ls_ztsd_disc_qt-discr.
*
*          MOVE: 'X' TO p_over_dis.
*
*        ENDIF.
*      ELSE.
*        MOVE: 'X' TO p_over_dis.
*      ENDIF.
*
*      IF p_over_dis EQ 'X'.
*        SELECT SINGLE vbeln
*         FROM ztsd_status_qt
*         INTO lv_status_qt
*         WHERE vbeln EQ gv_vbak-vgbel
*          AND status EQ 'COMP'.
*
*        IF lv_status_qt IS NOT INITIAL.
*          p_over_dis = ' '.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF p_over_dis EQ 'X'.
*
*      SELECT SINGLE ihrez
*        INTO lv_ihrez
*        FROM vbkd
*        WHERE vbeln EQ gv_vbak-vbeln.
*      IF lv_ihrez IS NOT INITIAL.
*        p_over_dis = ' '.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " IMPROVE_DATA_HEADER
*&---------------------------------------------------------------------*
*& Form f_get_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_ITEM TABLES UR_LIFSP.

  SELECT  A~VBELN
          A~AUFNR
          A~POSNR A~PSTYV A~OBJNR A~MATNR A~ARKTX
          A~WERKS A~LGORT
          A~UEPOS A~UPMAT
          B~BMENG B~WMENG A~NETPR A~NETWR
          B~EDATU D~KTEXT
          A~PRCTR
          A~MWSBP
          A~POSEX
          A~KONDM
          A~ROUTE
          A~PS_PSP_PNR
  INTO TABLE GT_ITEM
  FROM VBAP AS A INNER JOIN VBEP AS B ON  ( A~VBELN EQ B~VBELN AND
                                            A~POSNR EQ B~POSNR )
            LEFT OUTER JOIN COAS AS D ON  ( A~AUFNR EQ D~AUFNR )
  WHERE A~VBELN IN S_VBELN
    AND A~POSNR IN S_POSNR
    AND B~EDATU IN S_EDATU
    AND A~ABGRU EQ SPACE
    AND B~LIFSP IN UR_LIFSP[].

  MOVE  GT_ITEM TO GT_CHECK.
  SORT GT_CHECK.

  DELETE ADJACENT DUPLICATES FROM GT_CHECK.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_BLOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CHECK_BLOCK CHANGING CR_LIFSP.

  DATA: LR_LIFSP TYPE RANGE OF VBEP-LIFSP.

  IF R_BLOC IS NOT INITIAL.
    LR_LIFSP = VALUE #( ( SIGN = GC_CON-I OPTION = GC_CON-EQ LOW = SPACE ) ).
  ELSE.
    CLEAR : LR_LIFSP[].
  ENDIF.

  CR_LIFSP = LR_LIFSP[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MARD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_MARD .
  CLEAR GT_MARD[].
  IF GT_ITEM[] IS NOT INITIAL.
    SELECT  MATNR
            WERKS
            LGORT
            LGPBE
      INTO TABLE GT_MARD
      FROM MARD
      FOR ALL ENTRIES IN GT_ITEM
      WHERE MATNR EQ GT_ITEM-MATNR
        AND WERKS EQ GT_ITEM-WERKS
        AND LGORT EQ GT_ITEM-LGORT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_MOTHER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CHECK_MOTHER .
  GT_CHECK_MOTHER = VALUE #(
                 FOR LS_TMP IN GT_ITEM INDEX INTO LV_INDEX
                   (
                     VBELN = LS_TMP-VBELN
                     POSNR = LS_TMP-POSNR
                     MATNR = LS_TMP-MATNR
                     UEPOS = LS_TMP-UEPOS
                    )
                  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_VBAK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_VBAK .
  SELECT  A~VBELN  A~ERDAT A~KUNNR B~NAME1 B~NAME2 A~VKBUR A~VKGRP
          D~KUNNR  D~ADRNR F~TITLE F~NAME1 F~NAME2 F~STREET F~CITY2 F~CITY1
          F~POST_CODE1 A~VGBEL K~ZTERM G~VTEXT K~BSTKD A~AUART J~BEZEI A~KNUMV
          L~PERNR B~ADRNR D~ADRNR
          Q~ZTERM
          R~KUNNR R~ADRNR
          A~VTWEG
          F~LOCATION
          S~STREET
          S~CITY2
          S~CITY1
          S~POST_CODE1
          S~LOCATION
          A~ERNAM
          T~BEZEI
          A~ZZPOB
          A~KVGR5
  INTO TABLE GT_VBAK
  FROM  VBAK AS A INNER JOIN KNA1  AS B ON ( A~KUNNR  EQ B~KUNNR )
                  INNER JOIN VBPA  AS D ON ( A~VBELN  EQ D~VBELN                AND
                                             D~PARVW  EQ LC_PTNRFUNC_SHIPTO     AND
                                             D~POSNR  EQ SPACE )
                  INNER JOIN ADRC  AS F ON ( D~ADRNR  EQ F~ADDRNUMBER           AND
                                             F~NATION EQ SPACE )
                  INNER JOIN VBKD  AS K ON ( A~VBELN  EQ K~VBELN                AND
                                             K~POSNR  EQ SPACE )
            LEFT OUTER JOIN  TVZBT AS G ON ( K~ZTERM  EQ G~ZTERM                AND
                                             G~SPRAS  EQ LC_SPRAS_EN )
                  INNER JOIN TVAKT AS J ON ( A~AUART  EQ J~AUART                AND
                                             J~SPRAS  EQ LC_SPRAS_EN )
                   LEFT JOIN VBPA AS L  ON ( A~VBELN  EQ L~VBELN                AND
                                             L~PARVW  EQ LC_PTNRFUNC_SALEPERSON AND
                                             L~POSNR  EQ SPACE )
                  INNER JOIN KNB1 AS Q  ON ( A~KUNNR  EQ Q~KUNNR )
                  INNER JOIN VBPA AS R  ON ( A~VBELN  EQ R~VBELN                AND
                                             R~PARVW  EQ LC_PTNRFUNC_BILLTO )
                  INNER JOIN ADRC  AS S ON ( R~ADRNR  EQ S~ADDRNUMBER           AND
                                             S~NATION EQ SPACE )
                  LEFT JOIN TVAUT AS T ON A~AUGRU EQ T~AUGRU AND
                                          T~SPRAS EQ SY-LANGU
   FOR ALL ENTRIES IN GT_CHECK
   WHERE A~VBELN EQ GT_CHECK-VBELN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_KONV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_KONV.

  SELECT KNUMV KPOSN KBETR KWERT KSCHL
    INTO TABLE GT_KONV
    FROM PRCD_ELEMENTS
    FOR ALL ENTRIES IN GT_VBAK
    WHERE KNUMV EQ GT_VBAK-KNUMV.
*      AND ( KSCHL EQ 'ZDP2'
*      OR    KSCHL EQ 'ZDP3'
*      OR    KSCHL EQ 'ZD00'
*      OR    KSCHL EQ 'ZD01'
*      OR    KSCHL EQ 'ZD02'
*      OR    KSCHL EQ 'ZD03'
*      OR    KSCHL EQ 'ZD04'
*      OR    KSCHL EQ 'ZD05'
*      OR    KSCHL EQ 'PR00'
*      OR    KSCHL EQ 'ZD30'
*      OR    KSCHL EQ 'ZD31' ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PA001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_PA0001 .

  SELECT PERNR ENAME
    INTO TABLE GT_PA0001
    FROM PA0001
    FOR ALL ENTRIES IN GT_VBAK
    WHERE PERNR EQ GT_VBAK-PERNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_VBKD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_VBKD .
  SELECT VBELN POSNR ZTERM
         BZIRK  KDGRP KONDA
         IHREZ
  INTO TABLE GT_VBKD
  FROM VBKD
  FOR ALL ENTRIES IN GT_VBAK
  WHERE VBELN EQ GT_VBAK-VBELN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_PARTNER .
  SELECT VBELN POSNR PARVW KUNNR PERNR ADRNR
    INTO TABLE GT_PARTNER
    FROM  VBPA
    FOR ALL ENTRIES IN GT_CHECK
    WHERE VBELN EQ GT_CHECK-VBELN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_AUFK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_AUFK .
  SELECT A~AUFNR A~KDAUF B~PARVW B~KUNNR B~ADRNR
    INTO TABLE GT_AUFK
    FROM AUFK AS A INNER JOIN VBPA AS B
                   ON ( A~KDAUF EQ B~VBELN
                   AND  B~PARVW EQ 'AG' )
    FOR ALL ENTRIES IN GT_CHECK
    WHERE A~AUFNR EQ GT_CHECK-AUFNR.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_EMP_SALES_AREA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_EMP_SALES_AREA .
  SELECT VKBUR VKGRP PERNR
    INTO TABLE GT_EMP_SAREA
    FROM ZSDSSDT005
    WHERE VKBUR IN R_VKBUR
    AND   VKGRP IN R_VKGRP.

  SORT GT_EMP_SAREA BY PERNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_VKBT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_VKBT .
  SELECT VKBUR BEZEI
      INTO TABLE GT_TVKBT_SSS
     FROM TVKBT
     WHERE SPRAS EQ 'E'
      AND BEZEI LIKE '%SSS'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PROFIT_BY_SALES_AREA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_PROFIT_BY_SALES_AREA .
  SELECT VKBUR VKGRP PRCTR
      INTO TABLE GT_ZTSD_PROFIT
     FROM ZSDSSDT006.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PRINT_PARAMETER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SET_PRINT_PARAMETER .
  CLEAR: GWA_CONTROL_PARAMETERS.
  GWA_CONTROL_PARAMETERS-NO_DIALOG = SPACE.
  GWA_CONTROL_PARAMETERS-PREVIEW   = ABAP_TRUE.
  GWA_CONTROL_PARAMETERS-NO_OPEN   = SPACE.
  GWA_CONTROL_PARAMETERS-NO_CLOSE  = ABAP_TRUE.

  CLEAR: GWA_OUTPUT_OPTIONS.
  GWA_OUTPUT_OPTIONS-TDDEST  = 'LOCL'.
  GWA_OUTPUT_OPTIONS-TDNEWID = ABAP_TRUE.
  GWA_OUTPUT_OPTIONS-TDIMMED = ABAP_TRUE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DISCOUNT_TYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DISCOUNT_TYPE .
  CONSTANTS : BEGIN OF LC_CON,
                A  TYPE C LENGTH 1 VALUE 'A',
                V  TYPE C LENGTH 1 VALUE 'V',
                I  TYPE C LENGTH 1 VALUE 'I',
                EQ TYPE C LENGTH 2 VALUE 'EQ',
              END OF LC_CON.

  SELECT @LC_CON-I  AS SIGN,
         @LC_CON-EQ AS OPTION,
         KSCHL      AS LOW,
         KSCHL      AS HIGH
    FROM T685A
    INTO TABLE @GR_DISC_PERCEN
    WHERE KAPPL EQ @LC_CON-V
      AND KSCHL IN @GR_KSCHL[]
      AND KRECH EQ @LC_CON-A.

  SELECT @LC_CON-I  AS SIGN,
         @LC_CON-EQ AS OPTION,
         KSCHL      AS LOW,
         KSCHL      AS HIGH
    FROM T685A
    INTO TABLE @GR_DISC_VALUE
    WHERE KAPPL EQ @LC_CON-V
      AND KSCHL IN @GR_KSCHL[]
      AND KRECH NE @LC_CON-A.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA .
  DATA: LF_RETCODE TYPE SY-SUBRC.
  DATA: LV_SALENAME(50)    TYPE C,
        LV_SALESERNAME(50) TYPE C.

  DATA: LV_FLAG TYPE C.

  DATA : LT_ZTSD_PROFIT TYPE TABLE OF ZSDSSDT006,
         LS_ZTSD_PROFIT TYPE ZSDSSDT006.

  DATA : BEGIN OF LS_CHECK_PROFIT,
           VBELN TYPE VBAK-VBELN,
         END OF LS_CHECK_PROFIT.
  DATA LT_CHECK_PROFIT LIKE TABLE OF LS_CHECK_PROFIT.

  DATA: SUM_KWERT LIKE  KONV-KWERT,
        SUM_KBETR LIKE  KONV-KWERT,
        SUM_TEST  TYPE  P DECIMALS 4,
        SUM_TOTAL TYPE  P DECIMALS 4.

  DATA: LV_MESSAGE TYPE CHAR255.

  PERFORM F_SET_PRINT_PARAMETER.

  IF NOT GT_VBAK IS INITIAL.
    CLEAR: GV_TABIX, GV_LAST_ITEM.
    PERFORM F_GET_FIRST_LAST_ITEM.
    PERFORM F_GET_PROFIT TABLES LT_ZTSD_PROFIT
                                LT_CHECK_PROFIT.

    CLEAR : GV_CHECK_PROFIT,GV_CHECK_TABIX.
    LOOP AT GT_VBAK INTO WA_VBAK.
      CLEAR: LV_DATE, LV_DATE_CONVERTED,LWA_HEADER,
             LV_FLAG,
             FLAG_DISC,LS_ZTSD_PROFIT.

*<<< Begin of check condition for discount > 20% : T41K914685
      CLEAR: SUM_KWERT, SUM_KBETR, SUM_TOTAL, SUM_TEST.
      LOOP AT GT_KONV INTO GW_KONV WHERE KSCHL EQ GC_CON-ZPR0.
        SUM_KWERT = SUM_KWERT + GW_KONV-KWERT.
      ENDLOOP.

      IF SUM_KWERT IS INITIAL.
        LOOP AT GT_KONV INTO GW_KONV WHERE KSCHL EQ GC_CON-ZPR1.
          SUM_KWERT = SUM_KWERT + GW_KONV-KWERT.
        ENDLOOP.
      ENDIF.

*<<< Check Discount Value
      LOOP AT GT_KONV INTO GW_KONV WHERE KSCHL IN GR_DISC_VALUE.
        CLEAR: SUM_KBETR.
        SUM_KBETR = ABS( GW_KONV-KBETR ).
        READ TABLE GT_KONV INTO GA_KONV WITH KEY KNUMV = GW_KONV-KNUMV
                                                 KPOSN = GW_KONV-KPOSN
                                                 KSCHL = GC_CON-ZPR0.
        IF SY-SUBRC EQ 0.
          IF GA_KONV-KBETR GT 0.
            SUM_TEST =  ( SUM_KBETR * 100 ) / GA_KONV-KBETR.
            IF SUM_TEST > SUM_TOTAL.
              SUM_TOTAL = ( SUM_KBETR * 100 ) / GA_KONV-KBETR.
            ENDIF.
          ELSE.
            READ TABLE GT_KONV INTO GA_KONV WITH KEY KNUMV = GW_KONV-KNUMV
                                                     KPOSN = GW_KONV-KPOSN
                                                     KSCHL = GC_CON-ZPR1.
            IF SY-SUBRC EQ 0.
              SUM_TEST =  ( SUM_KBETR * 100 ) / GA_KONV-KBETR.
              IF SUM_TEST > SUM_TOTAL.
                SUM_TOTAL = ( SUM_KBETR * 100 ) / GA_KONV-KBETR.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

*<<< Check Discount %
      CLEAR: SUM_KBETR, SUM_TEST, SUM_KBETR, SUM_TOTAL.
      LOOP AT GT_KONV INTO GW_KONV WHERE KSCHL IN GR_DISC_PERCEN AND
                                         KNUMV EQ WA_VBAK-KNUMV.
        CLEAR: SUM_KBETR.
        SUM_KBETR = ABS( GW_KONV-KBETR ).
        SUM_TEST = ( SUM_KBETR / 10 ).
        IF SUM_TEST > SUM_TOTAL.
          SUM_TOTAL = ( SUM_KBETR / 10 ).
        ENDIF.
      ENDLOOP.
*--------------------------------------------------------------------------*
* if vbkd-ihrez have a message for memo number that system will not shown
* "Check Discount", it is water mark on the report form.
*--------------------------------------------------------------------------*
      LOOP AT GT_VBKD INTO GW_VBKD WHERE VBELN EQ WA_VBAK-VBELN AND
                                         IHREZ NE SPACE.
        CLEAR: LWA_HEADER-FLG_DISC.
      ENDLOOP.

      GV_TABIX = SY-TABIX.
      LWA_HEADER-VBELN = WA_VBAK-VBELN.
      LWA_HEADER-ERDAT = WA_VBAK-ERDAT.
      LV_DATE = LWA_HEADER-ERDAT.
      PERFORM CONVERT_DATE_FORMAT USING LV_DATE 'E'
                                  CHANGING LV_DATE_CONVERTED.
      LWA_HEADER-CRTEDAT = LV_DATE_CONVERTED.
      LWA_HEADER-KUNNR = WA_VBAK-KUNNR.
      CONCATENATE WA_VBAK-NAME1 ' ' WA_VBAK-NAME2 INTO LWA_HEADER-SOLDTO.
      LWA_HEADER-BSTKD = WA_VBAK-BSTKD.
      LWA_HEADER-SHIPTONO = WA_VBAK-SHIPTONO.
      LWA_HEADER-SHIPTOTITLE     = WA_VBAK-SHIPTITLE.
      CONCATENATE WA_VBAK-SHIPNAME1 ' ' WA_VBAK-SHIPNAME2 INTO LWA_HEADER-SHIPTONAME.
      CONCATENATE WA_VBAK-STREET ' ' WA_VBAK-LOCATION ' ' WA_VBAK-CITY2 ' ' WA_VBAK-CITY1 ' ' WA_VBAK-POST_CODE1 INTO LWA_HEADER-SHIPTOADDR SEPARATED BY SPACE.
      CONCATENATE WA_VBAK-STREET_B ' ' WA_VBAK-LOCATION_B ' ' WA_VBAK-CITY2_B ' ' WA_VBAK-CITY1_B ' ' WA_VBAK-POST_CODE1_B INTO LWA_HEADER-BILLTOADDR SEPARATED BY SPACE.

      LWA_HEADER-QUOTN        = WA_VBAK-VGBEL.
      LWA_HEADER-VTEXT        = WA_VBAK-VTEXT.
      LWA_HEADER-ORDER_REASON = WA_VBAK-BEZEI_R.
      LWA_HEADER-KVGR5        = WA_VBAK-KVGR5.

*      LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = WA_VBAK-KNUMV AND
*                                         KPOSN = ''.
*        IF WA_KONV-KSCHL EQ 'ZDP3'.
*          LWA_HEADER-KWERT = ( WA_KONV-KBETR / 10 ) * -1 . "Add by Wantanee 20111122
*          LWA_HEADER-TXT_PER_ADV = '%'.
*        ELSEIF WA_KONV-KSCHL EQ 'ZDP2'.
*          LWA_HEADER-KWERT = WA_KONV-KBETR * -1.
*          LWA_HEADER-TXT_PER_ADV = ''.
*        ENDIF.
*      ENDLOOP.
      LWA_HEADER-VKBUR = WA_VBAK-VKBUR.
      LWA_HEADER-VKGRP = WA_VBAK-VKGRP.
      LWA_HEADER-BSTKD = WA_VBAK-BSTKD.
      LWA_HEADER-BEZEI = WA_VBAK-BEZEI.
      LWA_HEADER-ZZPOB = WA_VBAK-ZZPOB.
      LWA_HEADER-CUST_ADRNR  =  WA_VBAK-CUST_ADRNR.
      LWA_HEADER-SHIP_ADRNR  =  WA_VBAK-SHIP_ADRNR.
      LWA_HEADER-CUST_TERM  =  WA_VBAK-CUST_TERM.
      LWA_HEADER-BILL_TO_NO  = WA_VBAK-BILL_TO_NO.
      LWA_HEADER-BILL_ADRNR  = WA_VBAK-BILL_ADRNR.

      IF WA_VBAK-KUNNR CP 'OT*'.
        LWA_HEADER-CUST_ADRNR  =  WA_VBAK-ADRNR.
      ENDIF.

      READ TABLE GT_PA0001 INTO WA_PA0001 WITH KEY PERNR = WA_VBAK-PERNR.
      IF SY-SUBRC EQ 0.
*        SPLIT WA_PA0001-ENAME  AT SPACE INTO: LV_SALESERNAME LV_SALENAME.
*        CONCATENATE LV_SALENAME LV_SALESERNAME INTO LWA_HEADER-SALEPR SEPARATED BY SPACE.
        LWA_HEADER-SALEPR = WA_PA0001-ENAME .
      ENDIF.

      LOOP AT GT_ITEM INTO DATA(LS_ITEM) WHERE VBELN EQ LWA_HEADER-VBELN
                                           AND PS_PSP_PNR IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            INPUT  = LS_ITEM-PS_PSP_PNR
          IMPORTING
            OUTPUT = LWA_HEADER-PS_PSP_PNR.
        EXIT.
      ENDLOOP.
*      IF SY-SUBRC EQ 0.
      IF LS_ITEM IS INITIAL.
        READ TABLE GT_ITEM INTO LS_ITEM INDEX 1.
      ENDIF.

      LWA_HEADER-EDATU = LS_ITEM-EDATU.

      LV_DATE = LWA_HEADER-EDATU.
      PERFORM CONVERT_DATE_FORMAT USING LV_DATE 'E'
                                  CHANGING LV_DATE_CONVERTED.
      LWA_HEADER-DELIEDATU = LV_DATE_CONVERTED.
      LWA_HEADER-AUFNR = LS_ITEM-AUFNR.
      LWA_HEADER-KTEXT = LS_ITEM-KTEXT.
      PERFORM READ_TEXT_HEAD USING 'ZH06' 'VBBK' LWA_HEADER-VBELN
                             CHANGING LWA_HEADER-KTEXT.
*      ENDIF.
*      PERFORM read_text_head USING 'Z009' 'VBBK' lwa_header-vbeln
*                             CHANGING lwa_header-jobno.
      PERFORM READ_TEXT_HEAD USING 'ZH17' 'VBBK' LWA_HEADER-VBELN
                             CHANGING LWA_HEADER-REFERMEMO.
      PERFORM READ_TEXT_HEAD USING 'Z010' 'VBBK' LWA_HEADER-VBELN
                             CHANGING LWA_HEADER-DATNO.
      PERFORM READ_TEXT_HEAD USING 'Z013' 'VBBK' LWA_HEADER-VBELN
                             CHANGING LWA_HEADER-CMDONO.
      PERFORM READ_TEXT_HEAD_PAYIN TABLES IT_LINES
                             USING 'Z041' 'VBBK' LWA_HEADER-VBELN.

      PERFORM PREPARE_DATA_ATTACHMENT USING LWA_HEADER-VBELN CHANGING LWA_ATTCH.
      MOVE-CORRESPONDING LWA_ATTCH TO GWA_ATTCH.
      PERFORM PREPARE_DATA_PREMIUM USING LWA_HEADER-VBELN
                                   CHANGING LWA_PREM.
      MOVE-CORRESPONDING LWA_PREM TO GWA_PREM.
      PERFORM PREPARE_TEXT_INVOICE_REMARK USING LWA_ATTCH LWA_PREM LWA_HEADER-VBELN WA_VBAK-KNUMV.
      PERFORM PREPARE_ITEM1 USING LWA_HEADER-VBELN WA_VBAK-KNUMV CHANGING LV_COUNT_ITEM LWA_HEADER-FLG_PROCON
                                                                          LWA_HEADER-FLG_PROFIT
*                                                                              lv_flag  "Remove by Wantanee 20120301
                                                                          LWA_HEADER-FLG_PAYMENT "Add by Wantanee 20120301
                                                                          LWA_HEADER-FLG_IO "Add by WAntanee 20110819
                                                                          LWA_HEADER-FLG_SHIPTO     "Add by Wantanee 20111122
                                                                          LWA_HEADER-FLG_DISCON "WCH 270212
                                                                          LWA_HEADER-FLG_S_DISTRICT   "Add by wantanee 20120326
                                                                          LWA_HEADER-FLG_S_CUSTGROUP   "Add by wantanee 20120326
                                                                          LWA_HEADER-FLG_S_PRICGROUP   "Add by wantanee 20120326
                                                                          LWA_HEADER-FLG_S_MATGROUP   "Add by wantanee 20120326
                                                                          LWA_HEADER-FLG_EMPCODE      "Add by WAntanee 20120326
                                                                          LWA_HEADER-FLG_ROUTE "T41K914320
                                                                          LWA_HEADER-FLG_PROFIT_SALE. "T41K914320
      "CH25 Add by WAntanee 20211223
      IF WA_VBAK-AUART EQ 'ZS04' OR WA_VBAK-AUART EQ 'ZS05'.
*        PERFORM improve_data_header  CHANGING lwa_header-flg_over_dis.
      ENDIF.
      "CH25 End  by WAntanee 20211223
*      PERFORM ADJUST_ITEM TABLES
      PERFORM PREPARE_DATA_TOTAL USING LWA_HEADER-VBELN WA_VBAK-KNUMV CHANGING GWA_TOTAL.
      PERFORM CUST_CONTROL   USING  WA_VBAK-KUNNR WA_VBAK-VTWEG  WA_VBAK-VKBUR WA_VBAK-VKGRP CHANGING LWA_HEADER-FLG_CUSTCON.
*          PERFORM check_shipto   USING  wa_vbak-vbeln wa_vbak-ship_adrnr   CHANGING lwa_header-flg_shipto.  "Add by Wantanee 20110819
*          PERFORM check_emp   USING  wa_vbak-vbeln wa_vbak-pernr   CHANGING lwa_header-flg_empcode.  "Add by Wantanee 20110819
      "Add by WAntanee 20120731

      READ TABLE GT_TVKBT_SSS INTO WA_TVKBT_SSS WITH KEY VKBUR = WA_VBAK-VKBUR.

      IF SY-SUBRC NE 0.
        "Add by Wantanee 20140420
        PERFORM CHECK_EMP_FORSALE USING WA_VBAK-PERNR WA_VBAK-VKBUR WA_VBAK-VKGRP CHANGING  LWA_HEADER-FLG_EMPSALE.
        "End Add by Wantanee 20140420
      ENDIF.
      "End Add by Wantanee 20120731

      "Add by Wantanee 20110819
      READ TABLE GT_AUFK INTO WA_AUFK WITH KEY AUFNR = LWA_HEADER-AUFNR.
      IF SY-SUBRC EQ 0.
        IF WA_AUFK-KDAUF NE ''.
          IF WA_VBAK-CUST_ADRNR NE WA_AUFK-ADRNR.
            LWA_HEADER-FLG_SOLDTO = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.


*      IF ( LWA_HEADER-FLG_SHIPTO EQ 'X' ) OR ( LWA_HEADER-FLG_EMPCODE EQ 'X' )
*         OR ( LWA_HEADER-FLG_PROFIT EQ 'X' ) OR ( LWA_HEADER-FLG_SOLDTO EQ 'X' )
**             OR ( lv_flag EQ 'X' )   Remove by Wantanee 20120301
*         OR ( LWA_HEADER-FLG_PAYMENT = 'X' )
*         OR ( LWA_HEADER-FLG_IO = 'X' )
*         OR ( LWA_HEADER-FLG_S_DISTRICT = 'X' )
*         OR ( LWA_HEADER-FLG_S_CUSTGROUP = 'X' )
*         OR ( LWA_HEADER-FLG_S_PRICGROUP = 'X' )
*         OR ( LWA_HEADER-FLG_S_MATGROUP = 'X' )
*         OR ( LWA_HEADER-FLG_EMPSALE = 'X' )
*         OR ( LWA_HEADER-FLG_ROUTE = 'X' )
*         OR ( LWA_HEADER-FLG_PROFIT_SALE = 'X' ) .
*
*
*        LWA_HEADER-FLG_WRONG = 'X'.
*
*      ENDIF.
      "End Add by Wantanee 20110819
* Dalivery Status
*          IF r_bloc IS NOT INITIAL.
      IF S_POSNR IS NOT INITIAL.
        LWA_HEADER-DELI_S = 'Partially'.
      ELSE.
        IF LV_COUNT_ITEM = LV_COUNT_ALL.
          LWA_HEADER-DELI_S = 'Full'.
        ELSE.
          LWA_HEADER-DELI_S = 'Partially'.

        ENDIF.
      ENDIF.
*          ELSEIF r_all IS NOT INITIAL.
*            IF s_posnr IS NOT INITIAL.
*               lwa_header-deli_s = 'Partially'.
*            ELSE.
*              IF lv_count_item = lv_count_all.
*                lwa_header-deli_s = 'Full'.
*              ELSE.
*                lwa_header-deli_s = 'Partially'.
*
*              ENDIF.
*            ENDIF.
*
*          ENDIF.

*----  Delete on 15.01.2015
*
*      if gv_tabix eq gv_last_item.
*        if wa_vbak-vbeln ne gwa_first-vbeln.
*          gwa_control_parameters-no_open = 'X'.
*        endif.
*        gwa_control_parameters-no_close = ' '.
*      else.
*        if gv_tabix ne 1.
**                 if ( wa_vbak-auart ne 'ZS04' and
**                      wa_vbak-auart ne 'ZS05' ).
*          gwa_control_parameters-no_open = 'X'.
*          gwa_control_parameters-no_close = 'X'.
**                 else.
**                   gwa_control_parameters-no_open = ' '.
**                   gwa_control_parameters-no_close = ' '.
**                 endif.
*        endif.
*      endif.
*-- End Delete

*-- Add on 15.01.2015
      DATA: LV_KVGR2              TYPE VBAK-KVGR2,
            LV_COUNT_CEILING_PAGE TYPE I.

      SELECT SINGLE KVGR2
      FROM VBAK
      INTO LV_KVGR2
      WHERE VBELN EQ WA_VBAK-VBELN.

      IF  LV_KVGR2 EQ 'ZR3'.
        LV_COUNT_CEILING_PAGE  = 1.
      ELSE.
        LV_COUNT_CEILING_PAGE = 0.
      ENDIF.

      EXPORT LV_COUNT_CEILING_PAGE TO MEMORY ID 'Z_SD_ORDER_CONFIRMATION4'.

      IF GV_TABIX EQ GV_LAST_ITEM.

        IF WA_VBAK-VBELN NE GWA_FIRST-VBELN.
          GWA_CONTROL_PARAMETERS-NO_OPEN = 'X'.
        ELSE.
          GWA_CONTROL_PARAMETERS-NO_OPEN = SPACE.
        ENDIF.

* --padd
        IF LV_KVGR2 EQ 'ZR3'.
          GWA_CONTROL_PARAMETERS-NO_CLOSE = 'X'.
        ELSE.
          GWA_CONTROL_PARAMETERS-NO_CLOSE = SPACE.
        ENDIF.
*  --padd

*        gwa_control_parameters-no_close = 'X'.


      ELSE.
        IF GV_TABIX NE 1.
*                 if ( wa_vbak-auart ne 'ZS04' and
*                      wa_vbak-auart ne 'ZS05' ).
          GWA_CONTROL_PARAMETERS-NO_OPEN = 'X'.
          GWA_CONTROL_PARAMETERS-NO_CLOSE = 'X'.
*                 else.
*                   gwa_control_parameters-no_open = ' '.
*                   gwa_control_parameters-no_close = ' '.
*                 endif.
        ENDIF.
      ENDIF.
*-- End Add

*--------------------------------------------------------------------*
*Case Check Profit
*--------------------------------------------------------------------*
*      READ TABLE lt_check_profit INTO ls_check_profit
*      WITH KEY vbeln = wa_vbak-vbeln.
*      IF sy-subrc = 0.
*        MESSAGE i000 WITH ls_check_profit-vbeln 'Wrong Profit center'.
*        gv_check_profit = 'X'.
*        IF gv_tabix = 1.
*          gv_check_tabix = 'X'.
*        ENDIF.
*        CONTINUE.
*      ENDIF.

      IF GV_CHECK_TABIX = 'X'.
        GWA_CONTROL_PARAMETERS-NO_OPEN  = SPACE.
        GWA_CONTROL_PARAMETERS-NO_CLOSE = 'X'.
        CLEAR GV_CHECK_TABIX.
      ENDIF.

      AT LAST.
        IF GV_CHECK_PROFIT = 'X'.
          IF WA_VBAK-VBELN NE GWA_FIRST-VBELN.
            GWA_CONTROL_PARAMETERS-NO_OPEN = 'X'.
          ELSE.
            GWA_CONTROL_PARAMETERS-NO_OPEN = SPACE.
          ENDIF.
* --padd
          IF LV_KVGR2 EQ 'ZR3'.
            GWA_CONTROL_PARAMETERS-NO_CLOSE = 'X'.
          ELSE.
            GWA_CONTROL_PARAMETERS-NO_CLOSE = SPACE.
          ENDIF.
*  --padd
        ENDIF.
      ENDAT.
*--------------------------------------------------------------------*
* End Case Check Profit
*--------------------------------------------------------------------*
      PERFORM F_PRINT_FORM.
      CLEAR: LWA_HEADER.
    ENDLOOP.
  ELSE.
    MESSAGE I001.
    EXIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_FIRST_LAST_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_FIRST_LAST_ITEM .
* LAST ITEM
  DESCRIBE TABLE GT_VBAK LINES GV_LAST_ITEM.
* FIRST ITEM
  READ TABLE GT_VBAK INDEX 1 INTO GWA_FIRST.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALIDATE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM VALIDATE_DATA .
*
*  DATA(GT_COND_ZPS4) = GT_KONV[].
*  DELETE GT_COND_ZPS4[] WHERE KSCHL <> 'ZPS4'.
*
*  "Net = 0 and ZPS4(SDS Modify Condition <> 0)
*   LOOP AT GT_COND_ZPS4 ASSIGNING FIELD-SYMBOL(<LFS_COND_ZPS4>).
*     dd
*
*   ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_KNVV
*&---------------------------------------------------------------------*
*& Get KNVV
*&---------------------------------------------------------------------*
FORM F_GET_KNVV .

  CHECK GT_VBAK[] IS NOT INITIAL.

  SELECT
    KNVV~KUNNR,
    KNVV~VKORG,
    KNVV~VTWEG,
    KNVV~SPART,
    KNVV~KVGR1,
    KNVV~KVGR2,
    KNVV~KVGR3,
    KNVV~KVGR4,
    KNVV~KVGR5
  FROM KNVV
  INTO TABLE @GT_KNVV
  FOR ALL ENTRIES IN @GT_VBAK
  WHERE KUNNR = @GT_VBAK-KUNNR.
  IF SY-SUBRC = 0.
    SORT GT_KNVV BY KUNNR.
    DELETE ADJACENT DUPLICATES FROM GT_KNVV COMPARING KUNNR.
  ENDIF.

ENDFORM.
