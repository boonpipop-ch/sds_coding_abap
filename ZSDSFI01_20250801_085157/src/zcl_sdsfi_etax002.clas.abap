CLASS ZCL_SDSFI_ETAX002 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_ETAX002
*"* do not include other source files here!!!
  PUBLIC SECTION.

    TYPES TY_BSEG TYPE BSEG .
    TYPES TY_BKPF TYPE BKPF .
    TYPES:
      BEGIN OF TY_PRCD,
        KNUMV TYPE KONV-KNUMV,
        KPOSN TYPE KONV-KPOSN,
        STUNR TYPE KONV-STUNR,
        ZAEHK TYPE KONV-ZAEHK,
        KSCHL TYPE KONV-KSCHL,
        KAWRT TYPE KONV-KAWRT,
        MWSK1 TYPE KONV-MWSK1,
        KWERT TYPE KONV-KWERT,
        KBETR TYPE KONV-KBETR,
*>>> BEGIN OF MODIFITION: <ETAX002_11> on 08.09.2020 02:11:20 <<<
        KOAID TYPE KONV-KOAID,
        VBELN TYPE VBRK-VBELN,
*>>> END OF MODIFITION: <ETAX002_11> on 08.09.2020 02:11:20 <<<
      END OF TY_PRCD .
    TYPES:
      BEGIN OF TY_DATA_ITEM,
        ITEM            TYPE I,
        MATNR           TYPE VBRP-MATNR,
        SGTXT           TYPE CHAR255,
        UNIT_PRICE      TYPE BSEG-WRBTR,
        QTY             TYPE CHAR16,
        UNIT            TYPE CHAR20, "bseg-meins,
        AMOUNT          TYPE BSEG-WRBTR,
        NO_UPRICE       TYPE FLAG,
        DISCOUNT        TYPE FLAG,
        PDIS_SGTXT      TYPE CHAR255,
        PDIS_AMOUNT     TYPE BSEG-WRBTR,
        DIS_SGTXT       TYPE CHAR255,
        DIS_UNIT_PRICE  TYPE BSEG-WRBTR,
        DIS_AMOUNT      TYPE BSEG-WRBTR,
        SGTXT_A         TYPE CHAR255,
        SGTXT_B         TYPE CHAR255,
        VBELN           TYPE VBRP-VBELN,
        POSNR           TYPE VBRP-POSNR,
        ITEM_DESC       TYPE CHAR255,
        RD_DISC_AMT     TYPE VBRP-NETWR,
        CHARGE_AMT      TYPE VBRP-NETWR,
        RD_CHARGE_FLAG  TYPE ZSDSDE_sap_rd_charge_flag,  "<--- ( + ) By Navapat.t 26.08.2020 10:51:13
        GROSS_AMT       TYPE VBRP-NETWR,
        VAT_AMT         TYPE VBRP-NETWR,
        VAT_BASE_AMT    TYPE VBRP-NETWR,
        NET_AMT_BF_VAT  TYPE VBRP-NETWR,
        NET_AMT_AFT_VAT TYPE VBRP-NETWR,
        SUM_FLAG        TYPE FLAG,            "For summary in doc item
      END OF TY_DATA_ITEM .
    TYPES:
      TTY_DATA_ITEM TYPE TABLE OF TY_DATA_ITEM WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF TY_DATA,
        BUKRS                TYPE BKPF-BUKRS,
        BELNR                TYPE BKPF-BELNR,     "เลขที่ใบแจ้งหนี้
        GJAHR                TYPE BKPF-GJAHR,
        BUZEI                TYPE BSEG-BUZEI,
        KOART                TYPE BSEG-KOART,
        COMP_NAME_TH         TYPE CHAR255,        "ชื่อและที่อยู่ บริษัท (ภาษาไทย)
        COMP_ADDR_TH         TYPE CHAR255,
        COMP_NAME_EN         TYPE CHAR255,        "ชื่อและที่อยู่ บริษัท (ภาษาอังกฤษ)
        COMP_ADDR_EN         TYPE CHAR255,
        STCEG                TYPE T001-STCEG,     "เลขประจำตัวผู้เสียภาษี
        TOPIC                TYPE CHAR1,
        PARTNER              TYPE BU_PARTNER,     "รหัสลูกค้า/เจ้าหนี้
        PARTNER_NAME1(161)   TYPE C,              "ชื่อลูกค้า/เจ้าหนี้
        PARTNER_NAME2(161)   TYPE C,              "ชื่อลูกค้า/เจ้าหนี้
        TAXNUM               TYPE C LENGTH 60,    "bptaxnumxl,     "เลขประจำตัวผู้เสียภาษี(ลูกค้า)/เจ้าหนี้
        PARTNER_ADDR1(290)   TYPE C,              "ที่อยู่ของลูกค้า/เจ้าหนี้
        PARTNER_ADDR2(290)   TYPE C,              "ที่อยู่ของลูกค้า/เจ้าหนี้
        PARTNER_ADDR3(290)   TYPE C,              "ที่อยู่ของลูกค้า/เจ้าหนี้
        PARTNER_ADRNR        TYPE AD_ADDRNUM,     "add ETAX002_08 08.9.2020
        BRANCH_T             TYPE C LENGTH 40,    "fith_desc,      "สาขา
        XBLNR                TYPE BKPF-XBLNR,     "เลขที่ใบกำกับภาษี
        BLDAT                TYPE BKPF-BLDAT,     "วันที่เอกสาร
        VTEXT                TYPE TVZBT-VTEXT,    "เงื่อนไขการรับชำระ
        DUE_DATE             TYPE DATS,           "วันที่ครบกำหนดชำระ
        BLART                TYPE BKPF-BLART,
        IHREZ                TYPE VBKD-IHREZ,     "รหัสผู้ขาย
        SHIP_NAME            TYPE CHAR255,        "สถานที่ส่ง
        SHIP_ADDR1           TYPE CHAR255,        "ที่อยู่สถานที่ส่ง
        SHIP_ADDR2           TYPE CHAR255,
        SHIP_ADDR3           TYPE CHAR255,
        ORDER_NUM            TYPE VBKD-BSTKD,     "เลขที่ใบสั่งซื้อ
        ORDER_DAT            TYPE DATS,           "วันที่ใบสั่งซื้อ
        DELIV_DAT            TYPE DATS,           "วันที่กำหนดส่งสินค้า
        DO_NUM               TYPE VBRP-VGBEL,     "เลขที่ใบจัดส่งสินค้า
        IT_ITEM              TYPE TTY_DATA_ITEM,            "Item data
        WHT                  TYPE BSEG-WRBTR,
        WHT_AMT              TYPE BSEG-WRBTR,
        TOT_AMT              TYPE BSEG-WRBTR,           "รวม
        DISC                 TYPE BSEG-WRBTR,           "ภาษีมูลค่าเพิ่ม
        TOT_AMT_VAT          TYPE BSEG-WRBTR,           "รวมเงิน
        VAT                  TYPE BSEG-WRBTR,           "ภาษีมูลค่าเพิ่ม
        GRAND_TOT            TYPE BSEG-WRBTR,           "จำนวนเงินหลังหักภาษี ณ ที่จ่าย
*         wa_detail            TYPE ztar002_detail,
*         wa_print             TYPE ztar002_print,
*         wa_reason            TYPE ztar002_reason,
        CUS_TAXNUM           TYPE C LENGTH 60,    "bptaxnumxl,
        CUR_TXT1             TYPE TCURT-KTEXT,
        CUR_TXT2             TYPE TCURT-KTEXT,
        OUTPUT               TYPE NAST-KSCHL,
        SUBDEPT_NAME(161)    TYPE C,
        SUBDEPT_TEL(30)      TYPE C,
        SUBDEPT_FAX(30)      TYPE C,
        PERIOD_TEXT          TYPE CHAR100,
        PAYCOND              TYPE STRING,
        PAYCOND01            TYPE STRING,
        CREATEBY             TYPE STRING,
        APPROVER             TYPE STRING,
        SUBAPP_POSTION       TYPE STRING,
        APP_POSTION          TYPE STRING,
        PRINTED              TYPE C LENGTH 1,
        PRINT_REASON         TYPE C LENGTH 4,
        SUBDEPT_NAME_Z1(161) TYPE C,
        WAERK                TYPE VBDKR-WAERK,
        EXRATE               TYPE VBRK-KURRF,
        DIGIT                TYPE C LENGTH 2,
        BARCD_H              TYPE C LENGTH 5,
        "Fi part
        MAKTX                TYPE MAKT-MAKTX,
        REF_BELNR            TYPE BKPF-XBLNR,
        REF_XBLNR            TYPE BKPF-XBLNR,
        REF_XBLNR_DAT        TYPE C LENGTH 20,
        TAX_FOUND            TYPE FLAG,
*>>> BEGIN OF INSERTION: <ETAX002_13> on 20.08.2020 <<<
        INCO1                TYPE VBRK-INCO1,
        REF_DOC_AMT          TYPE BSEG-WRBTR,
        CORRECT_AMT          TYPE BSEG-WRBTR,
        DIFF_AMT             TYPE BSEG-WRBTR,
        PAY_DUE_DATE         TYPE SY-DATUM,
        SUBJECT              TYPE CHAR100,
*>>> END OF INSERTION: <ETAX002_13> on 20.08.2020 <<<
*>>> BEGIN OF INSERTION: <ETAX002_02> on 31.08.2020 <<<
        RD_DOC_TYPE          TYPE CHAR3,
        RD_DOC_TYPE_GRP      TYPE CHAR3,
*>>> END OF INSERTION: <ETAX002_02> on 31.08.2020 <<<
*>>> BEGIN OF INSERTION: <ETAX002_11> on 03.09.2020 16:26:12 <<<
        BUPLA_INFO           TYPE CHAR5,
*>>> END OF INSERTION: <ETAX002_11> on 03.09.2020 16:26:12 <<<
*>>> BEGIN OF INSERTION: <ETAX002_09> on 05.09.2020 <<<
        CONTENT              TYPE CHAR100,
        SUM_FLAG             TYPE FLAG,            "For summary in doc item
*>>> END OF INSERTION: <ETAX002_09> on 05.09.2020 <<<
*>>> BEGIN OF INSERTION: <ETAX002_10> on 11.09.2020 10:20:46 <<<
        RD_CHARGE_FLAG       TYPE STRING,          "individual fix by each form
*>>> END OF INSERTION: <ETAX002_10> on 11.09.2020 10:20:46 <<<
      END OF TY_DATA .
    TYPES:
      BEGIN OF TY_VBRK,
        ERDAT TYPE  VBRK-ERDAT,
        ERZET TYPE  VBRK-ERZET,
        VBELN TYPE VBRK-VBELN,
        FKART TYPE VBRK-FKART,
        FKTYP TYPE VBRK-FKTYP,
        VBTYP TYPE VBRK-VBTYP,
        WAERK TYPE VBRK-WAERK,
        VKORG TYPE VBRK-VKORG,
        VTWEG TYPE VBRK-VTWEG,
        KALSM TYPE VBRK-KALSM,
        KNUMV TYPE VBRK-KNUMV,
        VSBED TYPE VBRK-VSBED,
        FKDAT TYPE VBRK-FKDAT,
        BELNR TYPE VBRK-BELNR,
        GJAHR TYPE VBRK-GJAHR,
        POPER TYPE VBRK-POPER,
        KONDA TYPE VBRK-KONDA,
        KDGRP TYPE VBRK-KDGRP,
        KUNRG TYPE VBRK-KUNRG,
        KUNAG TYPE VBRK-KUNAG,
*             j_1tpbupl TYPE vbrk-bupla, "c LENGTH 5,    "vbrk-j_1tpbupl,
        FKSTO TYPE VBRK-FKSTO,
        BUKRS TYPE VBRK-BUKRS,
        BUPLA TYPE VBRK-BUPLA,
      END OF TY_VBRK .
    TYPES:
      BEGIN OF TY_VBPA,
        VBELN TYPE VBPA-VBELN,
        POSNR TYPE VBPA-POSNR,
        PARVW TYPE VBPA-PARVW,
        KUNNR TYPE VBPA-KUNNR,
        LIFNR TYPE VBPA-LIFNR,
        PERNR TYPE VBPA-PERNR,
        PARNR TYPE VBPA-PARNR,
        ADRNR TYPE VBPA-ADRNR,
        ABLAD TYPE VBPA-ABLAD,
        LAND1 TYPE VBPA-LAND1,
        ADRDA TYPE VBPA-ADRDA,
        XCPDK TYPE VBPA-XCPDK,
      END OF TY_VBPA .
    TYPES:
      BEGIN OF TY_ITEM,
        ITEM            TYPE I,
        MATNR           TYPE VBRP-MATNR,
        SGTXT           TYPE CHAR255,
        UNIT_PRICE      TYPE BSEG-WRBTR,
        QTY             TYPE CHAR16,
        UNIT            TYPE CHAR20, "bseg-meins,
        AMOUNT          TYPE BSEG-WRBTR,
        NO_UPRICE       TYPE FLAG,
        DISCOUNT        TYPE FLAG,
        PDIS_SGTXT      TYPE CHAR255,
        PDIS_AMOUNT     TYPE BSEG-WRBTR,
        DIS_SGTXT       TYPE CHAR255,
        DIS_UNIT_PRICE  TYPE BSEG-WRBTR,
        DIS_AMOUNT      TYPE BSEG-WRBTR,
        SGTXT_A         TYPE CHAR255,
        SGTXT_B         TYPE CHAR255,
        VBELN           TYPE VBRP-VBELN,
        POSNR           TYPE VBRP-POSNR,
        ITEM_DESC       TYPE CHAR255,
        RD_DISC_AMT     TYPE VBRP-NETWR,
        CHARGE_AMT      TYPE VBRP-NETWR,
        RD_CHARGE_FLAG  TYPE ZSDSDE_SAP_RD_CHARGE_FLAG,     "<--- ( + ) By Navapat.t 26.08.2020 10:53:20
        GROSS_AMT       TYPE VBRP-NETWR,
        VAT_AMT         TYPE VBRP-NETWR,
        VAT_BASE_AMT    TYPE VBRP-NETWR,
        NET_AMT_BF_VAT  TYPE VBRP-NETWR,
        NET_AMT_AFT_VAT TYPE VBRP-NETWR,
        SUM_FLAG        TYPE FLAG,            "For summary in doc item
      END OF TY_ITEM .
    TYPES:
      BEGIN OF TY_COMPADDR,
        BUKRS      TYPE T001-BUKRS,
        STCEG      TYPE T001-STCEG, "ETAX002_01 01.09.2020
        NATION     TYPE ADRC-NATION,
        NAME1      TYPE ADRC-NAME1,
        NAME2      TYPE ADRC-NAME2,
        NAME3      TYPE ADRC-NAME3,
        NAME4      TYPE ADRC-NAME4,
        STREET     TYPE ADRC-STREET,
        CITY2      TYPE ADRC-CITY2,
        HOME_CITY  TYPE ADRC-HOME_CITY,
        CITY1      TYPE ADRC-CITY1,
        POST_CODE1 TYPE ADRC-POST_CODE1,
        STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
        LOCATION   TYPE ADRC-LOCATION,
      END OF TY_COMPADDR .
    TYPES:
      BEGIN OF TY_TAXRATE,
        MWSKZ TYPE A003-MWSKZ,
        KNUMH TYPE A003-KNUMH,
        KBETR TYPE KONP-KBETR,
      END OF TY_TAXRATE .
    TYPES:
      BEGIN OF TY_KNA1,
        KUNNR     TYPE KNA1-KUNNR,
        LAND1     TYPE KNA1-LAND1,
        ADRNR     TYPE KNA1-ADRNR,
        PSTLZ     TYPE KNA1-PSTLZ,
        J_1KFTIND TYPE KNA1-J_1KFTIND,
        STCD3     TYPE STCD3,
      END OF TY_KNA1 .
    TYPES:
      BEGIN OF TY_ADRC,
        BUKRS      TYPE J_1BBRANCH-BUKRS,
        BRANCH     TYPE J_1BBRANCH-BRANCH,
        ADDRNUMBER TYPE ADRC-ADDRNUMBER,
        NATION     TYPE ADRC-NATION,
        CITY1      TYPE ADRC-CITY1,
        CITY2      TYPE ADRC-CITY2,
        LOCATION   TYPE ADRC-LOCATION,
        COUNTRY    TYPE ADRC-COUNTRY,
      END OF TY_ADRC .
    TYPES:
      BEGIN OF TY_PARAM,
        ID           TYPE ZSDSFIC015-ID,
        NAME         TYPE ZSDSFIC015-NAME,
        PARAM_EXT    TYPE ZSDSFIC015-PARAM_EXT,
        SEQUENCE     TYPE ZSDSFIC015-SEQUENCE,
        ENDDA        TYPE ZSDSFIC015-ENDDA,
        BEGDA        TYPE ZSDSFIC015-BEGDA,
        PARAM_SIGN   TYPE ZSDSFIC015-PARAM_SIGN,
        PARAM_OPTION TYPE ZSDSFIC015-PARAM_OPTION,
        LOW_VALUE    TYPE ZSDSFIC015-LOW_VALUE,
        HIGH_VALUE   TYPE ZSDSFIC015-HIGH_VALUE,
        COMMENTS     TYPE ZSDSFIC015-COMMENTS,
      END OF TY_PARAM .
    TYPES:
      TTY_PARAM TYPE TABLE OF TY_PARAM .
    TYPES:
      BEGIN OF TY_VBRP,
                   MWSK2  TYPE VBRP-MWSKZ,
                   VBELN2 TYPE VBRP-VBELN,
                   POSNR2 TYPE VBRP-POSNR.
      INCLUDE TYPE : VBRP.
      TYPES        KNUMV  TYPE VBAK-KNUMV. "ADD <ETAX002_11>
    TYPES: END OF TY_VBRP .
    TYPES:
      BEGIN OF TY_SELL_ADRC,
        ADRNR      TYPE J_1BBRANCH-ADRNR,
        BUKRS      TYPE J_1BBRANCH-BUKRS,
        BRANCH     TYPE J_1BBRANCH-BRANCH,
        COUNTRY    TYPE ADRC-COUNTRY,
        ADDRNUMBER TYPE ADRC-ADDRNUMBER,
      END OF TY_SELL_ADRC .
    TYPES:
      BEGIN OF TY_DATA_FI.
        INCLUDE TYPE ZSDSFIS030.
    TYPES:
        COMP_NAME_TH     TYPE C LENGTH 160,
        COMP_NAME_EN     TYPE C LENGTH 160,
        ADDR_TH          TYPE C LENGTH 225,
        ADDR_EN          TYPE C LENGTH 225,
        COMP_TAX_BRANCH  TYPE C LENGTH 225,
        TYPE_TH          TYPE C LENGTH 100,
        TYPE_EN          TYPE C LENGTH 100,
        CUST_CODE_NAME   TYPE C LENGTH 180,
        CUST_ADDR        TYPE C LENGTH 225,
        CUST_ADDR2       TYPE C LENGTH 225,
        CUST_TAX_ID      TYPE C LENGTH 13,
        CUST_BRANCH      TYPE C LENGTH 15,
        NUMBER           TYPE BELNR_D,
        DATE             TYPE BUDAT,
        PAID_BY          TYPE C LENGTH 15,
        BANK_ACCOUNT     TYPE C LENGTH 72,
        BANK             TYPE C LENGTH 40,
        BANK_BRANCH      TYPE C LENGTH 40,
        BANK_NO          TYPE C LENGTH 40,
        BANK_DATE        TYPE VALUT,
        COND1            TYPE C LENGTH 120,
        COND2            TYPE C LENGTH 120,
        COND3            TYPE C LENGTH 120,
        COND4            TYPE C LENGTH 120,
        COND5            TYPE C LENGTH 120,
        DISCOUNT         TYPE VBRP-BRTWR, "zsetax_head_receipt-discount,
        AFTER_DISC       TYPE VBRP-BRTWR, "p ,"DECIMALS 8, "zsetax_head_receipt-after_disc,
        RECEIPT_TOTAL    TYPE VBRP-BRTWR, "p ,"DECIMALS 8, "zsetax_head_receipt-receipt_total,
        RECEIPT_VAT      TYPE VBRP-BRTWR, "p ,"DECIMALS 8, "zsetax_head_receipt-receipt_vat,
        GRAND_TOTAL      TYPE VBRP-BRTWR, "p ,"DECIMALS 8, "zsetax_head_receipt-grand_total,
        REMARK1          TYPE C LENGTH 150,
        REMARK2          TYPE C LENGTH 100,
        SPELL_TH         TYPE C LENGTH 255,
        SPELL_EN         TYPE C LENGTH 255,
        SPELL_EN2        TYPE C LENGTH 255,
        VAT_VALUE        TYPE C LENGTH 3,
        TYPE             TYPE C LENGTH 3,
*>>> BEGIN OF INSERTION: <ETAX002_02> on 31.08.2020 <<<
        RD_DOC_TYPE      TYPE CHAR3,
        RD_DOC_TYPE_GRP  TYPE CHAR3,
        PAY_DUE_DATE     TYPE SY-DATUM,
        ZTERM            TYPE BSEG-ZTERM,
        RD_FLAG          TYPE C LENGTH 1,
        EMAIL_FLAG       TYPE C LENGTH 1,
        REF_BUKRS        TYPE BKPF-BUKRS,
        REF_GJAHR        TYPE BKPF-GJAHR,
        REF_DOC_NO       TYPE BKPF-BELNR,
        REF_SAP_POST_DAT TYPE BKPF-BUDAT.
*>>> END OF INSERTION: <ETAX002_02> on 31.08.2020 <<<
    TYPES: END OF TY_DATA_FI .
    TYPES:
      BEGIN OF TY_ITEMS_FI.
        INCLUDE TYPE ZSDSFIS032.
    TYPES: COMP_CODE   TYPE BUKRS,
        DOCUMENT_NO TYPE BELNR_D,
        FISCAL_YEAR TYPE GJAHR,
        ITEM_NO     TYPE BUZEI,
        QUANTITY    TYPE C LENGTH 20,
        UNIT        TYPE MEINS,
        UNIT_PRICE  TYPE C LENGTH 25,
        AMOUNT      TYPE C LENGTH 25.
    TYPES: END OF TY_ITEMS_FI .
    TYPES:
      BEGIN OF TY_T001,
        BUKRS      TYPE T001-BUKRS,
        ADRNR      TYPE T001-ADRNR,
        STCEG      TYPE T001-STCEG,
        NAME1      TYPE ADRC-NAME1,
        NAME2      TYPE ADRC-NAME2,
        NAME3      TYPE ADRC-NAME3,
        NAME4      TYPE ADRC-NAME4,
        CITY1      TYPE ADRC-CITY1,
        CITY2      TYPE ADRC-CITY2,
        POST_CODE1 TYPE ADRC-POST_CODE1,
        STREET     TYPE ADRC-STREET,
        COUNTRY    TYPE ADRC-COUNTRY,
        MC_NAME1   TYPE ADRC-MC_NAME1,
        MC_CITY1   TYPE ADRC-MC_CITY1,
        MC_STREET  TYPE ADRC-MC_STREET,
      END OF TY_T001 .
    TYPES:
      BEGIN OF TY_ZTE_COMP_ADDR,
        BUKRS         TYPE ZSDSFIC003-BUKRS,
        BUPLA         TYPE ZSDSFIC003-BUPLA,
        HOME_NO       TYPE ZSDSFIC003-HOME_NO,
        ADDR1         TYPE ZSDSFIC003-ADDR1,
        SUB_DIST_CODE TYPE ZSDSFIC003-SUB_DIST_CODE,
        POSTAL        TYPE ZSDSFIC003-POSTAL,
        COUNTRY       TYPE ZSDSFIC003-COUNTRY,
        SCH_ID        TYPE ZSDSFIC003-SCH_ID,
        SUB_DIST_NAME TYPE ZSDSFIC020-SUB_DIST_NAME,
        DIST_CODE     TYPE ZSDSFIC020-DIST_CODE,
        PROVINCE_CODE TYPE ZSDSFIC020-PROVINCE_CODE,
        DIST_NAME     TYPE ZSDSFIC017-DIST_NAME,
        PROVINCE_NAME TYPE ZSDSFIC019-PROVINCE_NAME,
      END OF TY_ZTE_COMP_ADDR .
    TYPES:
      TTY_VBRP         TYPE TABLE OF TY_VBRP .
    TYPES:
      TTY_VBPA         TYPE TABLE OF TY_VBPA .
    TYPES:
      TTY_BKPF         TYPE TABLE OF TY_BKPF .
    TYPES:
      TTY_BSEG         TYPE TABLE OF TY_BSEG .
    TYPES:
      TTY_BSET         TYPE TABLE OF BSET .
    TYPES:
      TTY_ITEM         TYPE TABLE OF TY_ITEM .
    TYPES:
      TTY_COMPADDR     TYPE TABLE OF TY_COMPADDR .
    TYPES:
      TTY_KOMV         TYPE TABLE OF KOMV .
    TYPES:
      TTY_PRCD         TYPE TABLE OF TY_PRCD .
    TYPES:
      TTY_TAXRATE      TYPE TABLE OF TY_TAXRATE .
    TYPES:
      TTY_KNA1         TYPE TABLE OF TY_KNA1 .
    TYPES:
      TTY_ADRC         TYPE TABLE OF TY_ADRC .
    TYPES:
      TTY_DOC_HEADER   TYPE TABLE OF ZSDSFIT014 .
    TYPES:
      TTY_MAP_DOC_FI   TYPE TABLE OF ZSDSFIC009 .
    TYPES:
      TTY_MAP_DISCHG   TYPE TABLE OF ZSDSFIC008 .
    TYPES:
      TTY_MAP_DOC_RESN TYPE TABLE OF ZSDSFIC010 .
    TYPES:
      TTY_MAP_VAT_TYPE TYPE TABLE OF ZSDSFIC014 .
    TYPES:
      TTY_SELL_ADRC    TYPE TABLE OF TY_SELL_ADRC .
    TYPES:
      TTY_ITEMS_FI     TYPE TABLE OF TY_ITEMS_FI .
    TYPES:
      TTY_KNA1_ALL     TYPE TABLE OF KNA1 .
    TYPES:
      TTY_ADRC_ALL     TYPE TABLE OF ADRC .
    TYPES:
      GTTY_VBRP TYPE STANDARD TABLE OF VBRP .
    TYPES:
      TTY_T001 TYPE TABLE OF TY_T001 .
    TYPES:
      TTY_ZTE_COMP_ADDR TYPE TABLE OF TY_ZTE_COMP_ADDR .

    CONSTANTS C_FORM002_11 TYPE STRING VALUE 'ZSF_FI_RECEIPT_NEW_A4'. "#EC NOTEXT
    CONSTANTS C_KSCHL_MWST TYPE KSCHL VALUE 'MWST'.         "#EC NOTEXT
    CONSTANTS C_FORM002_10 TYPE STRING VALUE 'ZSF_SD_RECEIPT_NEW_A4'. "#EC NOTEXT
    CONSTANTS C_FORM002_08 TYPE STRING VALUE 'ZSF_BILL_INV_NEW_A4'. "#EC NOTEXT
    CONSTANTS C_RD_DOC_380 TYPE STRING VALUE '380'.         "#EC NOTEXT
    CONSTANTS C_FORM002_07 TYPE STRING VALUE 'ZSF_SD_SP_INVOICE_2014_NEW'. "#EC NOTEXT
    CONSTANTS C_FORM002_05 TYPE STRING VALUE 'ZSF_SD_INVOICE_2014_NEW_A4'. "#EC NOTEXT
    CONSTANTS C_FORM002_12 TYPE STRING VALUE 'ZSF_SD_CREDIT_NOTE_A4'. "#EC NOTEXT
    CONSTANTS C_FORM002_13 TYPE STRING VALUE 'ZSF_SD_DEBIT_NOTE_A4'. "#EC NOTEXT
    CONSTANTS C_FORM002_09 TYPE STRING VALUE 'ZSF_SD_RECEIPT_SV_CASH_NEW_A4'. "#EC NOTEXT
    CONSTANTS C_O7 TYPE STRING VALUE 'O7'.                  "#EC NOTEXT
    CONSTANTS C_O0 TYPE STRING VALUE 'O0'.                  "#EC NOTEXT
    CONSTANTS C_DS TYPE STRING VALUE 'DS'.                  "#EC NOTEXT
    CONSTANTS C_D0 TYPE STRING VALUE 'D0'.                  "#EC NOTEXT
    CONSTANTS C_FORM002_01 TYPE STRING VALUE 'ZSF_FI_RECEIPT_INV_A4'. "#EC NOTEXT
    CONSTANTS C_FORM002_02 TYPE STRING VALUE 'ZSF_FI_RECEIPT_A4'. "#EC NOTEXT
    CONSTANTS C_DV_CUST TYPE STRING VALUE 'DV_CUSTOMER'.    "#EC NOTEXT
    CONSTANTS C_DV_RD_DOCTYPE TYPE STRING VALUE 'DV_RD_DOCTYPE'. "#EC NOTEXT
    CONSTANTS C_CN_CANCEL_DOCTYPE TYPE STRING VALUE 'CDN_VF11'. "#EC NOTEXT

    CLASS-METHODS CONV_DATE_TO_INTER
      IMPORTING
        !IM_DATE TYPE ANY
      CHANGING
        !CH_DATE TYPE DATUM .
    CLASS-METHODS TRIM_NUMBER
      IMPORTING
        !IM_NUM TYPE ANY
      CHANGING
        !CH_NUM TYPE ANY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSFI_ETAX002 IMPLEMENTATION.


method CONV_DATE_TO_INTER.
  DATA: lv_date TYPE c LENGTH 2,
        lv_month TYPE c LENGTH 2,
        lv_year  TYPE c LENGTH 4.

  lv_date = im_date(2).
  CASE im_date+3(3).
    WHEN 'JAN'. lv_month = '01'.
    WHEN 'FEB'. lv_month = '02'.
    WHEN 'MAR'. lv_month = '03'.
    WHEN 'APR'. lv_month = '04'.
    WHEN 'MAY'. lv_month = '05'.
    WHEN 'JUN'. lv_month = '06'.
    WHEN 'JUL'. lv_month = '07'.
    WHEN 'AUG'. lv_month = '08'.
    WHEN 'SEP'. lv_month = '09'.
    WHEN 'OCT'. lv_month = '10'.
    WHEN 'NOV'. lv_month = '11'.
    WHEN 'DEC'. lv_month = '12'.
  ENDCASE.

  CONCATENATE '20'
              im_date+7(2)
         INTO lv_year.

 CONCATENATE
             lv_year
             lv_month
             lv_date
        INTO ch_date.

endmethod.


METHOD TRIM_NUMBER.
  DATA: LV_TMP TYPE STRING.

  LV_TMP = IM_NUM.
  REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN LV_TMP WITH SPACE.
  CH_NUM = LV_TMP.
ENDMETHOD.
ENDCLASS.
