*----------------------------------------------------------------------*
***INCLUDE LZSDSFI07F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form VALIDATE_INVOICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CT_RETURN
*&      --> I_VBELN
*&      <-- GV_SUBRC
*&---------------------------------------------------------------------*
FORM VALIDATE_INVOICE TABLES LP_RETURN STRUCTURE BAPIRET2
                       USING LP_VBELN
                    CHANGING LP_SUBRC.

  CLEAR LP_SUBRC.

  IF GW_VBRK IS INITIAL.
    LP_SUBRC = 4.
    LP_RETURN-TYPE = 'E'.
    CONCATENATE 'Invoice no.' LP_VBELN 'not found'
                INTO LP_RETURN-MESSAGE SEPARATED BY SPACE.
    APPEND LP_RETURN. CLEAR LP_RETURN.
  ELSE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_INVOICE TABLES LP_ITEM STRUCTURE ZSDSSDS004
                        LP_SERN STRUCTURE ZSDSSDS005
                 USING  LP_HEAD TYPE ZSDSSDS003
                        LP_ADDR TYPE ZSDSSDS120.

  DATA: LW_HEAD TYPE BAPIWEBINVHEAD,
        LW_VBRP TYPE VBRP,
        LW_KONV TYPE KONV,
        LW_ADRC TYPE ADRC,
        LW_VBPA TYPE VBPA,
        LT_ITEM TYPE STANDARD TABLE OF BAPIWEBINVITEM WITH HEADER LINE,
        LT_PART TYPE STANDARD TABLE OF BAPIWEBINVPART WITH HEADER LINE.
  DATA: LW_CTRL    TYPE BAPIDLVBUFFERCONTROL,
        LT_VBELN   TYPE STANDARD TABLE OF BAPIDLV_RANGE_VBELN
        WITH HEADER LINE,
        LT_SERN    TYPE STANDARD TABLE OF BAPIDLVITMSERNO WITH HEADER LINE,
        LT_ITEM_DO TYPE STANDARD TABLE OF BAPIDLVITEM WITH HEADER LINE.

  CLEAR  : LW_HEAD.
  REFRESH: LT_ITEM,LT_PART.

  CALL FUNCTION 'BAPI_WEBINVOICE_GETDETAIL'
    EXPORTING
      PARTNER_NUMBER     = GW_VBRK-KUNRG
      PARTNER_ROLE       = 'RG'
      BILLINGDOC         = GW_VBRK-VBELN
    IMPORTING
      WEBINVOICEDOCUMENT = LW_HEAD
    TABLES
      WEBINVOICEITEMS    = LT_ITEM
      WEBINVOICEPARTNERS = LT_PART.

*>> Header
  LP_HEAD-INV_NO           = LW_HEAD-BILLINGDOC.
  PERFORM GET_ADDRESS_DISP USING GW_VBRK-VBELN
                                 'AG'
                        CHANGING LP_HEAD-SOLD_TO_CODE
                                 LP_HEAD-SOLD_TO_NAME
                                 LP_HEAD-ADDRESS
                                 LP_HEAD-CITY
                                 LP_HEAD-POST_CODE
                                 LP_ADDR.

  CLEAR LT_PART.
  READ TABLE LT_PART WITH KEY PARTN_ROLE = 'VE'.

  LP_HEAD-SALES_ORG        = GW_VBRK-VKORG.
  LP_HEAD-DISTR_CHAN       = GW_VBRK-VTWEG.
  LP_HEAD-DIVISION         = GW_VBRK-SPART.
  LP_HEAD-DOC_TYPE         = LW_HEAD-BILL_TYPE.
  LP_HEAD-DOC_DATE         = LW_HEAD-BILL_DATE.
  LP_HEAD-SALES_NAME       = LT_PART-NAME_LIST.

*--------------------------------------------------------------------*
*Add by Jakarin 11.12.2016
*--------------------------------------------------------------------*
  DATA : BEGIN OF LS_VBPA,
           VBELN TYPE VBPA-VBELN,
           KUNNR TYPE VBPA-KUNNR,
           NAME1 TYPE KNA1-NAME1,
           NAME2 TYPE KNA1-NAME2,
           NAME3 TYPE KNA1-NAME3,
         END OF LS_VBPA.
  DATA LT_VBPA LIKE TABLE OF LS_VBPA.

  DATA : LV_KUNNR TYPE KNA1-KUNNR,
         LV_NAME  TYPE C LENGTH 80.

  DATA : LV_PRICE TYPE PRCD_ELEMENTS.

  CONSTANTS LC_RE TYPE C LENGTH 2 VALUE 'RE'.

  SELECT VBPA~VBELN
         VBPA~KUNNR
         ADRC~NAME1
         ADRC~NAME2
         ADRC~NAME3
    FROM VBPA
    INNER JOIN ADRC ON VBPA~ADRNR EQ ADRC~ADDRNUMBER
    INTO TABLE LT_VBPA
    WHERE VBPA~VBELN  EQ GW_VBRK-VBELN
      AND VBPA~POSNR  EQ SPACE
      AND VBPA~PARVW  EQ LC_RE
      AND ADRC~NATION EQ SPACE.

  LOOP AT LT_VBPA INTO LS_VBPA.
    LP_HEAD-KUNNR = LS_VBPA-KUNNR.

    PERFORM GET_ADDRESS_DISP USING LS_VBPA-VBELN
                                   LC_RE
                          CHANGING LV_KUNNR
                                   LV_NAME
                                   LP_HEAD-ADDRESS_B
                                   LP_HEAD-CITY_B
                                   LP_HEAD-POST_CODE_B
                                   LP_ADDR.

    CONCATENATE LS_VBPA-NAME1 LS_VBPA-NAME2 LS_VBPA-NAME3 INTO LP_HEAD-NAME1 SEPARATED BY SPACE.
    CLEAR LS_VBPA.
  ENDLOOP.
*--------------------------------------------------------------------*
*End Add by Jakarin 11.12.2016
*--------------------------------------------------------------------*

  "Project Text
  PERFORM READ_TEXT USING 'Z002'
                          'E'
                          GW_VBRK-VBELN
                          'VBBK'
                 CHANGING LP_HEAD-PROJECT.

  CLEAR LP_HEAD-SALES_EMP.
  SELECT SINGLE PERNR INTO LP_HEAD-SALES_EMP
         FROM VBPA
         WHERE VBELN = GW_VBRK-VBELN
         AND   PARVW = 'VE'.

  PERFORM GET_ADDRESS_DISP USING GW_VBRK-VBELN
                                 'VE'
                        CHANGING LP_HEAD-SALES_EMP
                                 LP_HEAD-SALES_NAME
                                 LT_PART-NAME
                                 LT_PART-NAME
                                 LT_PART-NAME
                                 LP_ADDR.

  IF LT_ITEM[] IS NOT INITIAL.
    SELECT * FROM VBRP INTO TABLE @DATA(LT_VBRP)
                       FOR ALL ENTRIES IN @LT_ITEM[]
                       WHERE VBELN = @LT_ITEM-BILLINGDOC.
    IF SY-SUBRC = 0.
      SORT LT_VBRP BY VBELN POSNR.
      SELECT A~VBELN,
             A~ZZPOB
        FROM VBAK AS A
        INNER JOIN @LT_VBRP AS B ON A~VBELN = B~AUBEL
        INTO TABLE @DATA(LT_VBAK).
      IF SY-SUBRC = 0.
        READ TABLE LT_VBAK INTO DATA(LS_VBAK)
                           INDEX 1.
        IF SY-SUBRC = 0.
          LP_HEAD-POB = LS_VBAK-ZZPOB.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* >> Item
  LOOP AT LT_ITEM.
    CLEAR LW_VBRP.
*BOD F36K917232
*    SELECT SINGLE * INTO LW_VBRP
*           FROM VBRP
*           WHERE VBELN = LT_ITEM-BILLINGDOC
*           AND   POSNR = LT_ITEM-ITEM_NUMBER.
*EOD F36K917232
*BOI F36K917232
    READ TABLE LT_VBRP INTO LW_VBRP
                       WITH KEY VBELN = LT_ITEM-BILLINGDOC
                                POSNR = LT_ITEM-ITEM_NUMBER
                                BINARY SEARCH.
    IF SY-SUBRC = 0.
*EOI F36K917232
    SELECT SINGLE ZZ1_LOB_SO_SDI
      FROM VBAP
      INTO LP_ITEM-LOB
      WHERE VBELN EQ LW_VBRP-AUBEL
        AND POSNR EQ LW_VBRP-AUPOS.
*BOI F36K917232
    ENDIF.
*EOI F36K917232

    LP_ITEM-ITM_NUMBER   = LT_ITEM-ITEM_NUMBER.
    LP_ITEM-HG_LV_ITEM   = LT_ITEM-HG_LV_ITEM.
    LP_ITEM-MATERIAL     = LT_ITEM-MATERIAL_LONG.
    LP_ITEM-PRODH        = LW_VBRP-PRODH.
    LP_ITEM-KVGR2        = LW_VBRP-KVGR2.
    LP_ITEM-PLANT        = LT_ITEM-PLANT.
    LP_ITEM-STORE_LOC    = LW_VBRP-LGORT.
    LP_ITEM-TARGET_QTY   = LT_ITEM-INV_QTY.
    LP_ITEM-TARGET_QU    = LT_ITEM-SALES_UNIT.
    LP_ITEM-ITEM_CATEG   = LT_ITEM-ITEM_CATEG.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        INPUT          = LP_ITEM-TARGET_QU
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = LP_ITEM-TARGET_QU
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

*    LP_ITEM-NET_VALUE    = LT_ITEM-NETVAL_INV.
    LP_ITEM-AUFNR        = LW_VBRP-AUFNR.
    LP_ITEM-VAT_AMOUNT   = LW_VBRP-MWSBP.
    LP_ITEM-VBELN        = LW_VBRP-AUBEL.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = LW_VBRP-PS_PSP_PNR
      IMPORTING
        OUTPUT = LP_ITEM-WBS.


* >> Sales office
    LP_HEAD-SALES_OFFICE = LW_VBRP-VKBUR.
    CLEAR LP_HEAD-SALES_OFFICE_TXT.
    SELECT SINGLE BEZEI INTO LP_HEAD-SALES_OFFICE_TXT
           FROM TVKBT
           WHERE SPRAS = 'E'
           AND   VKBUR = LP_HEAD-SALES_OFFICE.
* >> Sales group
    LP_HEAD-SALES_GROUP = LW_VBRP-VKGRP.
    CLEAR LP_HEAD-SALES_GROUP_TXT.
    SELECT SINGLE BEZEI INTO LP_HEAD-SALES_GROUP_TXT
           FROM TVGRT
           WHERE SPRAS = 'E'
           AND   VKGRP = LP_HEAD-SALES_GROUP.
* >> Get Down payment
    CLEAR LW_KONV.
    SELECT SINGLE * INTO LW_KONV
           FROM KONV
           WHERE KNUMV = GW_VBRK-KNUMV
           AND   KPOSN = LT_ITEM-ITEM_NUMBER
           AND   KINAK = ''
           AND   KSCHL IN ('ZDP2','ZDP3').

    CLEAR : LV_PRICE.
    SELECT SINGLE * INTO @LV_PRICE
      FROM PRCD_ELEMENTS
      WHERE KNUMV EQ @GW_VBRK-KNUMV
        AND KPOSN EQ @LT_ITEM-ITEM_NUMBER
        AND KAPPL EQ 'V'
        AND KSCHL EQ 'ZPR1'.
    IF LV_PRICE-KWERT IS INITIAL.
      SELECT SINGLE * INTO @LV_PRICE
      FROM PRCD_ELEMENTS
      WHERE KNUMV EQ @GW_VBRK-KNUMV
        AND KPOSN EQ @LT_ITEM-ITEM_NUMBER
        AND KAPPL EQ 'V'
        AND KSCHL EQ 'ZPR0'.
    ENDIF.

    LP_ITEM-NET_VALUE   = LV_PRICE-KWERT."LT_ITEM-NETVAL_INV.
    LP_ITEM-DISCOUNT    = LV_PRICE-KWERT - LT_ITEM-NETVAL_INV.

    LP_ITEM-DOWN_PAYMENT_A = ABS( LW_KONV-KWERT ).
    LP_ITEM-DOWN_PAYMENT_P = ABS( LW_KONV-KBETR / 10 ).
*    lp_item-kschl_down     = lw_konv-kschl.
    IF LW_KONV-KSCHL = 'ZDP2'.
      CLEAR LP_ITEM-DOWN_PAYMENT_P.
    ENDIF.
    APPEND LP_ITEM. CLEAR LP_ITEM.

* >> Get Serial no.
    REFRESH: LT_VBELN,LT_SERN,LT_ITEM_DO.
    CLEAR LW_CTRL.

    LW_CTRL-ITEM = 'X'.
    LW_CTRL-SERNO = 'X'.

    IF LT_ITEM-REF_DOC_CAT = 'J'.
      LT_VBELN-SIGN   = 'I'.
      LT_VBELN-OPTION = 'EQ'.
      LT_VBELN-DELIV_NUMB_LOW  = LT_ITEM-REF_DOC_NUMBER.
      APPEND LT_VBELN.

      CALL FUNCTION 'BAPI_DELIVERY_GETLIST'
        EXPORTING
          IS_DLV_DATA_CONTROL = LW_CTRL
        TABLES
          IT_VBELN            = LT_VBELN
          ET_DELIVERY_ITEM    = LT_ITEM_DO
          ET_ITEM_SERIAL_NO   = LT_SERN.

      LOOP AT LT_ITEM_DO WHERE UEPOS = LT_ITEM-REF_DOC_ITEM.
        LOOP AT LT_SERN WHERE ITM_NUMBER = LT_ITEM_DO-POSNR.
          LP_SERN-ITM_NUMBER = LT_SERN-ITM_NUMBER.
          LP_SERN-SERIALNO   = LT_SERN-SERIALNO.
          APPEND LP_SERN. CLEAR LP_SERN.
        ENDLOOP.
      ENDLOOP.
      IF SY-SUBRC <> 0. "No Bom Case
        LOOP AT LT_SERN WHERE ITM_NUMBER = LT_ITEM-REF_DOC_ITEM.
          LP_SERN-ITM_NUMBER = LT_SERN-ITM_NUMBER.
          LP_SERN-SERIALNO   = LT_SERN-SERIALNO.
          APPEND LP_SERN. CLEAR LP_SERN.
        ENDLOOP.
      ENDIF.
    ENDIF.
* >> PO No.
    IF LT_ITEM-SD_DOC_CAT = 'C'.
* >> PO no.
      SELECT SINGLE BSTKD INTO LP_HEAD-PURCH_NO_C
             FROM VBKD
             WHERE VBELN = LT_ITEM-SD_DOC_NUMBER.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_INVOICE
*&---------------------------------------------------------------------*
*&      Form  GET_ADDRESS_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_VBRK_VBELN  text
*      -->P_0125   text
*      <--P_LP_HEAD_SOLD_TO_CODE  text
*      <--P_LP_HEAD_ADDRESS  text
*      <--P_LP_HEAD_CITY  text
*      <--P_LP_HEAD_POST_CODE  text
*----------------------------------------------------------------------*
FORM GET_ADDRESS_DISP  USING    LP_VBELN
                                LP_PARVW
                       CHANGING LP_CODE
                                LP_NAME
                                LP_ADDRESS
                                LP_CITY
                                LP_POST_CODE
                                LP_ADDR.

  DATA: LW_VBPA   TYPE VBPA,
        LW_ADRC   TYPE ADRC,
        LW_PA0002 TYPE PA0002.

  CLEAR: LP_NAME.

  CLEAR: LW_VBPA.
  SELECT SINGLE * INTO LW_VBPA
         FROM VBPA
         WHERE VBELN = LP_VBELN
         AND   PARVW = LP_PARVW.
  IF LP_PARVW = 'VE'.
    CLEAR: LW_PA0002.
    SELECT SINGLE * INTO LW_PA0002
           FROM PA0002
           WHERE PERNR = LP_CODE.
    CONCATENATE LW_PA0002-VNAMC LW_PA0002-NCHMC
                INTO LP_NAME SEPARATED BY SPACE.
  ELSE.
    CLEAR: LP_CODE,LP_ADDRESS,LP_CITY,LP_POST_CODE.
    CLEAR: LW_ADRC.
    SELECT SINGLE * INTO LW_ADRC
           FROM ADRC
           WHERE ADDRNUMBER = LW_VBPA-ADRNR
             AND NATION     = SPACE.
    IF SY-SUBRC <> 0.
      SELECT SINGLE * INTO LW_ADRC
             FROM ADRC
             WHERE ADDRNUMBER = LW_VBPA-ADRNR
               AND NATION     = 'I'.
    ENDIF.
    CONCATENATE LW_ADRC-NAME1 LW_ADRC-NAME2
                INTO LP_NAME.
    CONCATENATE LW_ADRC-STREET
                LW_ADRC-STR_SUPPL3
                LW_ADRC-LOCATION
                LW_ADRC-CITY2
                INTO LP_ADDRESS SEPARATED BY SPACE.
    LP_CODE      = LW_VBPA-KUNNR.
    LP_CITY      = LW_ADRC-CITY1.
    LP_POST_CODE = LW_ADRC-POST_CODE1.

    PERFORM MAPPING_ADDRESS   USING LW_ADRC
                              CHANGING LP_ADDR.
  ENDIF.

ENDFORM.                    " GET_ADDRESS_DISP
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM READ_TEXT USING LP_ID
                     LP_LANGU
                     LP_NAME
                     LP_OBJ
            CHANGING LP_TEXT.

  DATA: LV_ID    TYPE THEAD-TDID,
        LV_LANGU TYPE THEAD-TDSPRAS,
        LV_NAME  TYPE THEAD-TDNAME,
        LV_OBJ   TYPE THEAD-TDOBJECT,
        LT_LINES TYPE STANDARD TABLE OF TLINE WITH HEADER LINE.

  CLEAR: LP_TEXT,LV_ID,LV_LANGU,LV_NAME,LV_OBJ.
  REFRESH: LT_LINES.

  LV_ID    = LP_ID.
  LV_LANGU = LP_LANGU.
  LV_NAME  = LP_NAME.
  LV_OBJ   = LP_OBJ.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = LV_ID
      LANGUAGE                = LV_LANGU
      NAME                    = LV_NAME
      OBJECT                  = LV_OBJ
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.


  DELETE LT_LINES WHERE TDLINE = ''.
  READ TABLE LT_LINES INDEX 1.
  LP_TEXT = LT_LINES-TDLINE.
ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*& Form MAPPING_ADDRESS
*&---------------------------------------------------------------------*
FORM MAPPING_ADDRESS  USING    US_ADRC TYPE ADRC
                      CHANGING CS_ADDR TYPE ZSDSSDS120.

  CS_ADDR-NAME1         = US_ADRC-NAME1.
  CS_ADDR-NAME2         = US_ADRC-NAME2.
  CS_ADDR-NAME3         = US_ADRC-NAME3.
  CS_ADDR-NAME4         = US_ADRC-NAME4.
  CS_ADDR-STREET1       = US_ADRC-STREET.
  CS_ADDR-STREET4       = US_ADRC-STR_SUPPL3.
  CS_ADDR-STREET5       = US_ADRC-LOCATION.
  CS_ADDR-DISTRICT      = US_ADRC-CITY2.
  CS_ADDR-CITY          = US_ADRC-CITY1.
  CS_ADDR-TAXNUMBER     = US_ADRC-TAXJURCODE.
  CS_ADDR-COUNTRY       = US_ADRC-COUNTRY.
  CS_ADDR-POSTL_COD1    = US_ADRC-POST_CODE1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DELIVERY
*&---------------------------------------------------------------------*
*& Get Delivery
*&---------------------------------------------------------------------*
FORM GET_DELIVERY  CHANGING CT_ITEM             TYPE ZSDSSDS004_TT
                            CT_ITEM_SERIAL_NO   TYPE ZSDSSDS005_TT
                            CS_HEADER           TYPE ZSDSSDS003
                            CS_ADDRESS          TYPE ZSDSSDS120.

  DATA: LV_SO    TYPE VBAK-VBELN,
        LV_KUNNR TYPE VBAK-KUNNR.

  SELECT
    LIKP~VBELN,
    LIPS~POSNR,
    LIKP~VKORG,
    LIPS~VTWEG,
    LIPS~SPART,
    LIPS~VGBEL,
    LIPS~VGPOS,
    LIPS~UEPOS,
    LIPS~MATNR,
    LIPS~PRODH,
    LIPS~KVGR2,
    LIPS~WERKS,
    LIPS~LGORT,
    LIPS~LFIMG,
    LIPS~VRKME,
    LIPS~PSTYV,
    LIPS~AUFNR,
    LIPS~PS_PSP_PNR,
    LIPS~VKBUR,
    LIPS~VKGRP,
    LIKP~KNUMV
    FROM LIKP
    INNER JOIN LIPS
    ON LIKP~VBELN EQ LIPS~VBELN
    INTO TABLE @DATA(LT_DELI)
    WHERE LIKP~VBELN = @GW_LIKP-VBELN.
  IF SY-SUBRC = 0.

    "Get serial
    SELECT
      SER01~LIEF_NR,
      SER01~POSNR,
      OBJK~SERNR
      FROM SER01
      INNER JOIN OBJK
      ON SER01~OBKNR = OBJK~OBKNR
      INTO TABLE @DATA(LT_SERIAL)
      FOR ALL ENTRIES IN @LT_DELI
      WHERE SER01~LIEF_NR = @LT_DELI-VBELN
        AND SER01~POSNR   = @LT_DELI-POSNR.
    IF SY-SUBRC = 0.
      SORT LT_SERIAL BY LIEF_NR POSNR.
    ENDIF.

    CS_HEADER-INV_NO       = GW_LIKP-VBELN.   "Delivery No.

    "Project Text
    PERFORM READ_TEXT USING 'ZH06'
                            'E'
                            GW_LIKP-VBELN
                            'VBBK'
                   CHANGING CS_HEADER-PROJECT.

    READ TABLE LT_DELI INTO DATA(LS_DELI)
                       INDEX 1.
    IF SY-SUBRC = 0.
      CS_HEADER-SALES_ORG    = GW_LIKP-VKORG.
      CS_HEADER-DISTR_CHAN   = LS_DELI-VTWEG.
      CS_HEADER-DIVISION     = LS_DELI-SPART.
      LV_SO                  = LS_DELI-VGBEL.
    ENDIF.

    IF LV_SO IS NOT INITIAL.
      SELECT SINGLE ZZPOB FROM VBAK WHERE VBELN = @LV_SO
                          INTO @CS_HEADER-POB.
    ENDIF.

    CS_HEADER-DOC_TYPE     = GW_LIKP-LFART.
    CS_HEADER-DOC_DATE     = GW_LIKP-BLDAT.

    "Get Address
    PERFORM GET_ADDRESS_DISP USING GW_LIKP-VBELN
                                   'AG'
                          CHANGING CS_HEADER-SOLD_TO_CODE
                                   CS_HEADER-SOLD_TO_NAME
                                   CS_HEADER-ADDRESS
                                   CS_HEADER-CITY
                                   CS_HEADER-POST_CODE
                                   CS_ADDRESS.
    SELECT
      VBPA~VBELN,
      VBPA~PARVW,
      VBPA~KUNNR,
      VBPA~ASSIGNED_BP,
      ADRC~NAME1,
      ADRC~NAME2,
      ADRC~NAME3,
      ADRC~NAME4
      FROM VBPA
      INNER JOIN ADRC ON VBPA~ADRNR EQ ADRC~ADDRNUMBER
      INTO TABLE @DATA(LT_VBPA)
      WHERE VBPA~VBELN    = @GW_LIKP-VBELN
        AND VBPA~POSNR    = @SPACE
        AND ADRC~NATION   = @SPACE.
    IF SY-SUBRC = 0.
      SORT LT_VBPA BY VBELN PARVW.
    ENDIF.

    "Employee
    READ TABLE LT_VBPA INTO DATA(LS_VBPA)
                       WITH KEY VBELN = GW_LIKP-VBELN
                                PARVW = 'VE'
                                BINARY SEARCH.
    IF SY-SUBRC = 0.
      SELECT SINGLE KUNNR,
               NAME1,
               NAME2
        FROM KNA1
        INTO @DATA(GS_EMP)
        WHERE KUNNR = @LS_VBPA-ASSIGNED_BP.
      IF SY-SUBRC = 0.
        CS_HEADER-SALES_NAME = |{ GS_EMP-NAME1 }{ GS_EMP-NAME2 }|.
        CONDENSE CS_HEADER-SALES_NAME.
      ENDIF.
    ENDIF.

    "GET Sales order infomation
    IF LT_DELI IS NOT INITIAL.
      SELECT VBAK~VBELN,
             VBAP~POSNR,
             VBAP~MWSBP,
             VBAK~KNUMV
        FROM VBAK
        INNER JOIN VBAP ON VBAK~VBELN = VBAP~VBELN
        INTO TABLE @DATA(LT_SO_INFO)
        FOR ALL ENTRIES IN @LT_DELI
        WHERE VBAK~VBELN = @LT_DELI-VGBEL
          AND VBAP~POSNR = @LT_DELI-VGPOS.
      IF SY-SUBRC = 0.
        SORT LT_SO_INFO BY VBELN POSNR.
      ENDIF.
    ENDIF.

    LOOP AT LT_DELI INTO LS_DELI.

      APPEND INITIAL LINE TO CT_ITEM ASSIGNING FIELD-SYMBOL(<LFS_ITEM>).
      SELECT SINGLE ZZ1_LOB_SO_SDI
      FROM VBAP
      INTO <LFS_ITEM>-LOB
      WHERE VBELN EQ LS_DELI-VGBEL
        AND POSNR EQ LS_DELI-VGPOS.

      <LFS_ITEM>-ITM_NUMBER   = LS_DELI-POSNR.
      <LFS_ITEM>-HG_LV_ITEM   = LS_DELI-UEPOS.
      <LFS_ITEM>-MATERIAL     = LS_DELI-MATNR.
      <LFS_ITEM>-PRODH        = LS_DELI-PRODH.
      <LFS_ITEM>-KVGR2        = LS_DELI-KVGR2.
      <LFS_ITEM>-PLANT        = LS_DELI-WERKS.
      <LFS_ITEM>-STORE_LOC    = LS_DELI-LGORT.
      <LFS_ITEM>-TARGET_QTY   = LS_DELI-LFIMG.
      <LFS_ITEM>-TARGET_QU    = LS_DELI-VRKME.
      <LFS_ITEM>-ITEM_CATEG   = LS_DELI-PSTYV.
      <LFS_ITEM>-AUFNR        = LS_DELI-AUFNR.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          INPUT          = <LFS_ITEM>-TARGET_QU
          LANGUAGE       = SY-LANGU
        IMPORTING
          OUTPUT         = <LFS_ITEM>-TARGET_QU
        EXCEPTIONS
          UNIT_NOT_FOUND = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

      "Read sales order information
      READ TABLE LT_SO_INFO INTO DATA(LS_SO_INFO)
                             WITH KEY VBELN = LS_DELI-VGBEL
                                      POSNR = LS_DELI-VGPOS
                                      BINARY SEARCH.
      IF SY-SUBRC = 0.
        <LFS_ITEM>-VAT_AMOUNT   = LS_SO_INFO-MWSBP.     "Vat
        <LFS_ITEM>-VBELN        = LS_SO_INFO-VBELN.

        IF CS_HEADER-PURCH_NO_C IS INITIAL.
          SELECT SINGLE BSTKD INTO CS_HEADER-PURCH_NO_C
            FROM VBKD
            WHERE VBELN = LS_SO_INFO-VBELN.
        ENDIF.

        "Get Price from sales orders
        SELECT SINGLE * INTO @DATA(LS_PRCD_ELEMENTS)
          FROM PRCD_ELEMENTS
          WHERE KNUMV EQ @LS_SO_INFO-KNUMV
            AND KPOSN EQ @LS_SO_INFO-POSNR
            AND KAPPL EQ 'V'
            AND KSCHL EQ 'ZPR1'.
        IF LS_PRCD_ELEMENTS-KWERT IS INITIAL.
          SELECT SINGLE * INTO @LS_PRCD_ELEMENTS
          FROM PRCD_ELEMENTS
          WHERE KNUMV EQ @LS_SO_INFO-KNUMV
            AND KPOSN EQ @LS_SO_INFO-POSNR
            AND KAPPL EQ 'V'
            AND KSCHL EQ 'ZPR0'.
        ENDIF.
        IF LS_PRCD_ELEMENTS-KWERT IS NOT INITIAL.
          <LFS_ITEM>-NET_VALUE   = LS_PRCD_ELEMENTS-KWERT.
        ENDIF.

        "Discount
        SELECT SUM( KWERT )
          INTO @DATA(LV_DISCOUNT)
          FROM PRCD_ELEMENTS
          WHERE KNUMV EQ @LS_SO_INFO-KNUMV
            AND KPOSN EQ @LS_SO_INFO-POSNR
            AND KAPPL EQ 'V'
            AND KSCHL LIKE 'ZD%'.
        IF SY-SUBRC = 0.
          <LFS_ITEM>-DISCOUNT = ABS( LV_DISCOUNT ).
        ELSE.
          CLEAR <LFS_ITEM>-DISCOUNT.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          INPUT  = LS_DELI-PS_PSP_PNR
        IMPORTING
          OUTPUT = <LFS_ITEM>-WBS.

* >> SALES OFFICE
      CS_HEADER-SALES_OFFICE = LS_DELI-VKBUR.
      CLEAR CS_HEADER-SALES_OFFICE_TXT.
      SELECT SINGLE BEZEI INTO CS_HEADER-SALES_OFFICE_TXT
             FROM TVKBT
             WHERE SPRAS = 'E'
             AND   VKBUR = CS_HEADER-SALES_OFFICE.

* >> Sales group
      CS_HEADER-SALES_GROUP = LS_DELI-VKGRP.
      CLEAR CS_HEADER-SALES_GROUP_TXT.
      SELECT SINGLE BEZEI INTO CS_HEADER-SALES_GROUP_TXT
             FROM TVGRT
             WHERE SPRAS = 'E'
             AND   VKGRP = CS_HEADER-SALES_GROUP.

      READ TABLE LT_SERIAL  TRANSPORTING NO FIELDS
                            WITH KEY LIEF_NR = LS_DELI-VBELN
                                     POSNR   = LS_DELI-POSNR
                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        DATA(LV_INDEX) = SY-TABIX.
        LOOP AT LT_SERIAL INTO DATA(LS_SERIAL)
                          FROM LV_INDEX.
          IF LS_SERIAL-LIEF_NR <> LS_DELI-VBELN OR
             LS_SERIAL-POSNR   <> LS_DELI-POSNR.
            EXIT.
          ELSE.
            APPEND INITIAL LINE TO CT_ITEM_SERIAL_NO ASSIGNING FIELD-SYMBOL(<LFS_SERAIL_NO>).
            <LFS_SERAIL_NO>-ITM_NUMBER = LS_SERIAL-POSNR.
            <LFS_SERAIL_NO>-SERIALNO   = LS_SERIAL-SERNR.
          ENDIF.
        ENDLOOP.

        CLEAR: LS_PRCD_ELEMENTS.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
