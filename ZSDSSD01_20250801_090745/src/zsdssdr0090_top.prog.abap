*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0090_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS : TRUXS,SLIS,ICON.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: TNAPR,                         "Programs & Forms
        ITCPO,                         "Communicationarea for Spool
        ARC_PARAMS,                    "Archive PARAMETERSs
        TOA_DARA,                      "Archive PARAMETERSs
        ADDR_KEY,                      "Adressnumber for ADDRESS
        KONV,
        JEST,
        TVKBT,
        TVGRT,
        VBAK,
        VBAP,
        VBEP,
        COAS,
        KNB1.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF TYP_HEADER,
         VBELN           LIKE VBAK-VBELN,        "Sale Order
         CRTEDAT(9)      TYPE C,                 "Create Date format DD-MMM-YY
         ERDAT           LIKE VBAK-ERDAT,        "Create Date
         EDATU           LIKE VBEP-EDATU,        " Delivery Date
         DELIEDATU(9)    TYPE C,                 "Delivery Date format DD-MMM-YY
         DELI_S(20)      TYPE C,                 "Delivery Status
         KUNNR           LIKE VBAK-KUNNR,        "Customer Number
         SOLDTO(100)     TYPE C,                  "Cutomer Name
         AUFNR           LIKE VBAP-AUFNR,        "Cost Sheet No
         KTEXT           LIKE COAS-KTEXT,        "Project Name.
         SHIPTONO        LIKE VBPA-KUNNR,        "Ship to no
         SHIPTOTITLE     LIKE TSAD3T-TITLE_MEDI, "Ship to title name
         SHIPTONAME(50)  TYPE C,                 "Ship to name
         SHIPTOADDR(100) TYPE C,                 "Ship to Address
         JOBNO(15)       TYPE C,                 "Job no
         QUOTN           LIKE VBFA-VBELV,        "Quotation no
         REFERMEMO(255)  TYPE C,                 "Refer memo
         VTEXT(25)       TYPE C,                 "Payment Term
         KWERT           LIKE KONV-KWERT,        "Advance receipt
         SALEPR          LIKE P0001-ENAME,       "Sale Person
         DATNO(15)       TYPE C,                 "DAT Modifify no
         CMDONO(15)      TYPE C,                 "CM for D/O no
         BEZEI           LIKE TVAKT-BEZEI,       "Order Type
         VKBUR           LIKE VBAK-VKBUR,        "Sale Office
         VKGRP           LIKE VBAK-VKGRP,        "Sale Group
         BSTKD           LIKE VBKD-BSTKD,        "Customer PO
         CUST_ADRNR      LIKE KNA1-ADRNR,        "Address number for sold to no
         SHIP_ADRNR      LIKE VBPA-ADRNR,        "Address number for ship to no
         CUST_TERM       LIKE KNB1-ZTERM,        "Payment Term of Customer Master
         BILL_TO_NO      LIKE VBPA-KUNNR,        "Bill to no
         BILL_ADRNR      LIKE VBPA-ADRNR,        "Bill to address number
         FLG_PROCON      TYPE C,        "Flag Check Product Control
         FLG_CUSTCON     TYPE C,        "Flag Check Customer Controll
         FLG_WRONG       TYPE C,          "Flag wrong shipto empcode profit soldto
         FLG_SHIPTO      TYPE C,        "Flag shipto
         FLG_PROFIT      TYPE C,        "Flag Profit Center
         FLG_EMPCODE     TYPE C,        "Flag Empcode
         FLG_SOLDTO      TYPE C,        "Flag soldto
         FLG_IO          TYPE C,        "Flag io
         FLG_DISCON      TYPE C,        "Product Discontinue
         TXT_PER_ADV(2)  TYPE C,     "Text show case Advance receipt persent
         FLG_PAYMENT     TYPE C,     "Text show case Advance receipt persent
         FLG_S_DISTRICT  TYPE C,     "Text show case sale district
         FLG_S_CUSTGROUP TYPE C,     "Text show case customer group
         FLG_S_PRICGROUP TYPE C,     "Text show case pricing group
         FLG_S_MATGROUP  TYPE C,     "Text show case material group
         FLG_EMPSALE     TYPE C,  "Text show case employee in sale office and sale group incorrect
         FLG_ROUTE,                  "Text show case Route wrong
         FLG_DISC,                   "Check discount > 20%
         FLG_PROFIT_SALE TYPE C,            "Check Profit center and sale office and sale group
         FLG_OVER_DIS    TYPE C,  "CH25 Check over discount  20211223
         PS_PSP_PNR      TYPE CHAR24,
         BILLTOADDR(100) TYPE C,
         ORDER_REASON    TYPE C LENGTH 100,
         ZZPOB           TYPE VBAK-ZZPOB,
         KVGR5           TYPE VBAK-KVGR5,
       END OF TYP_HEADER.

* item
TYPES: BEGIN OF TYP_ITEM,
         VBELN      LIKE VBAP-VBELN,     "Sale order
         POSNR      LIKE VBAP-POSNR,     "Item
         MATNR      LIKE VBAP-MATNR,     "material
         UEPOS      LIKE VBAP-UEPOS,     "node no mother
         DESC       TYPE STRING,
         LGORT      TYPE VBAP-LGORT,     "Stor. Location "CH26+
         KWMENG     LIKE VBAP-KWMENG,    "QTY
         KWMENG2    TYPE I,
         NETPR      LIKE VBAP-NETPR,     "Amount per unit
         NETWR      LIKE VBAP-NETWR,     "Amount total
         KWMENG_MOM TYPE KWMENG,
         WMENG_MOM  TYPE VBEP-WMENG,    "Req. Qty
         PRICE      TYPE NETWR,
         RUNNO(2)   TYPE N,
         EDATU      LIKE VBEP-EDATU,     " Delivery Date
         KWERT      LIKE KONV-KWERT,     "Advance receipt
         CHECK_MOM  TYPE C,              "check mom
         ZTERM      TYPE KNB1-ZTERM,     " Term of payment for  Line item in S/O
*             sflag      TYPE C,              "FLAG STATUS
         POSEX      TYPE VBAP-POSEX,     "PO Item
         KONDM      TYPE VBAP-KONDM,     " material price group "Add by Wantanee 20120326
         C_PROCON   TYPE C,              "Check Production control "Add by Wantanee 20120229
         LGPBE      TYPE MARD-LGPBE,     "Storage Bin "CH26+
         DELIV      TYPE C LENGTH 10,
       END OF TYP_ITEM.
TYPES: TYP_ITEM_TABLE TYPE TABLE OF TYP_ITEM.

TYPES: BEGIN OF TYP_ITEM2,
         VBELN     LIKE VBAP-VBELN,
         POSNR     LIKE VBAP-POSNR,
         POSNR_1   LIKE VBAP-POSNR,
         MATNR(21) TYPE C,
         RUNNO(2)  TYPE N,
         EDATU     TYPE VBEP-EDATU,
         WMENG     TYPE I,                "Req Qty
         BMENG     TYPE I,                "Confirm QTY
         KWERT     LIKE KONV-KWERT,        "Advance receipt
         ZTERM     TYPE KNB1-ZTERM,      " Term of payment for  Line item in S/O
         POSEX     TYPE VBAP-POSEX,   "PO Item
         KONDM     TYPE VBAP-KONDM,    " material price group "Add by Wantanee 20120326
         C_PROCON  TYPE C,            "Check Production control "Add by Wantanee 20120229
         DELIV     TYPE C LENGTH 10,
       END OF TYP_ITEM2.
TYPES: TYP_ITEM_TABLE2 TYPE TABLE OF TYP_ITEM2.

TYPES: BEGIN OF TYP_MARD,
         MATNR TYPE MARD-MATNR,
         WERKS TYPE MARD-WERKS,
         LGORT TYPE MARD-LGORT,
         LGPBE TYPE MARD-LGPBE,
       END   OF TYP_MARD.
DATA : GS_MARD TYPE TYP_MARD,
       GT_MARD TYPE TABLE OF TYP_MARD.

TYPES: TYP_ITEM_VBAP TYPE TABLE OF VBAP.
TYPES: BEGIN OF TYP_VBEP,
         VBELN LIKE VBEP-VBELN,
         POSNR LIKE VBEP-POSNR,
         MATNR LIKE VBAP-MATNR,
         EDATU LIKE VBEP-EDATU,
         BMENG LIKE VBEP-BMENG,
       END OF TYP_VBEP.

* total
TYPES: BEGIN OF TYP_TOTAL,
         TOTAL      LIKE VBAK-NETWR,
         PCENT      TYPE I,
         VAT        LIKE VBAK-NETWR,
         GTOTAL     LIKE VBAK-NETWR,
         QTY(5)     TYPE C,
*             qty    LIKE vbap-kbmeng,
         TOTAL_DOWN LIKE KONV-KWERT,  "Advance Receipt
       END OF TYP_TOTAL.

* attachment
TYPES: BEGIN OF TYP_ATTCH,
         PYMNT TYPE C,
         QUOTN TYPE C,
         MAP   TYPE C,
         PO    TYPE C,
       END OF TYP_ATTCH.

* premium
TYPES: BEGIN OF TYP_PREM,
         PICHONK TYPE C,
         QTY     TYPE I,
       END OF TYP_PREM.

* manager
TYPES: BEGIN OF TYP_MGR,
         SALES_MGR      TYPE ZSDSDE_PARAM_VALUE,
         SALES_DATE(9)  TYPE C,
         CREDIT_MGR     TYPE ZSDSDE_PARAM_VALUE,
         CREDIT_DATE(9) TYPE C,
         ACCNT_MGR      TYPE ZSDSDE_PARAM_VALUE,
         ACCNT_DATE(9)  TYPE C,
         SPA_MGR        TYPE ZSDSDE_PARAM_VALUE,
         SPA_DATE(9)    TYPE C,
       END OF TYP_MGR.

TYPES: BEGIN OF TYP_VBAK,
         VBELN        LIKE VBAK-VBELN,        "Sale Order
         ERDAT        LIKE VBAK-ERDAT,        "Create Date
         KUNNR        LIKE VBAK-KUNNR,        "Customer Number
         NAME1        LIKE KNA1-NAME1,        "Customer name1
         NAME2        LIKE KNA1-NAME2,        "Customer Name2
         VKBUR        LIKE VBAK-VKBUR,        "Sale Office
         VKGRP        LIKE VBAK-VKGRP,        "Sale Group
         SHIPTONO     LIKE VBPA-KUNNR,        "Ship to number
         ADRNR        LIKE VBPA-ADRNR,        "Addruss number Ship-to no.
         SHIPTITLE    LIKE ADRC-TITLE,        "Ship to title name
         SHIPNAME1    LIKE ADRC-NAME1,        "Ship to name1
         SHIPNAME2    LIKE ADRC-NAME2,        "Ship to name2
         STREET       LIKE ADRC-STREET,       "Street ship to
         CITY2        LIKE ADRC-CITY2,        "City2 to ship to
         CITY1        LIKE ADRC-CITY1,        "City1 to ship to
         POST_CODE1   LIKE ADRC-POST_CODE1,   "Post cost
         VGBEL        LIKE VBAK-VGBEL,        "Quotation no ref
         ZTERM        LIKE VBKD-ZTERM,        "Payment term
         VTEXT        LIKE TVZBT-VTEXT,       "Description Payment
         BSTKD        LIKE VBKD-BSTKD,        "Cutomer PO No
         AUART        LIKE VBAK-AUART,        "Type for sale order
         BEZEI        LIKE TVAKT-BEZEI,       "Description Type for sale order
         KNUMV        LIKE VBAK-KNUMV,        "NO for search downpayment
         PERNR        LIKE VBPA-PERNR,        "Sale Person no.
*              ename           LIKE pa0001-ename,      "Sale Person name
         CUST_ADRNR   LIKE KNA1-ADRNR,        "Address number for sold to no
         SHIP_ADRNR   LIKE VBPA-ADRNR,        "Address number for ship to no
         CUST_TERM    LIKE KNB1-ZTERM,      " Term of payment for  Line item in S/O
         BILL_TO_NO   LIKE VBPA-KUNNR,        "Bill to no
         BILL_ADRNR   LIKE VBPA-ADRNR,        "Bill to address number
         VTWEG        LIKE VBAK-VTWEG,        "Distribution Chanel
         LOCATION     TYPE ADRC-LOCATION,
         STREET_B     LIKE ADRC-STREET,       "Street bill to
         CITY2_B      LIKE ADRC-CITY2,        "City2 to bill to
         CITY1_B      LIKE ADRC-CITY1,        "City1 to bill to
         POST_CODE1_B LIKE ADRC-POST_CODE1,   "Post cost
         LOCATION_B   TYPE ADRC-LOCATION,
         ERNAM        TYPE VBAK-ERNAM,
         BEZEI_R      TYPE TVAUT-BEZEI,
         ZZPOB        TYPE VBAK-ZZPOB,
         KVGR5        TYPE VBAK-KVGR5,        "Customer Group 5
         KLIMK        LIKE KNKK-KLIMK,        "Customer's credit limit
         CDRMN        LIKE KNKK-KLIMK,        "Credit Remaining
         SFLAG        TYPE C,                    "FLAG STATUS WCH:11072011
       END OF TYP_VBAK.

TYPES: BEGIN OF TYP_KONV,
         KNUMV LIKE KONV-KNUMV,    "
         KPOSN LIKE KONV-KPOSN,    " Item
         KBETR LIKE KONV-KBETR,    " Downpayment for header
         KWERT LIKE KONV-KWERT,    "Downpayment for line item
         KSCHL LIKE KONV-KSCHL,    "Type

       END OF TYP_KONV.

*
TYPES: BEGIN OF TYP_PA0001,
         PERNR LIKE PA0001-PERNR, "Sale no
         ENAME LIKE PA0001-ENAME,  "sale name

       END OF TYP_PA0001.

TYPES: BEGIN OF TYP_VBAP,
         VBELN      LIKE VBAP-VBELN,     "Sale order
         AUFNR      LIKE VBAP-AUFNR,     "IO
         POSNR      LIKE VBAP-POSNR,     "Item
         PSTYV      LIKE VBAP-PSTYV,     "Type for item
         OBJNR      LIKE VBAP-OBJNR,     "Object no
         MATNR      LIKE VBAP-MATNR,     "material
         ARKTX      LIKE VBAP-ARKTX,     "Description Mat
         WERKS      TYPE VBAP-WERKS,     "Plant "CH26+
         LGORT      LIKE VBAP-LGORT,     "Stor. Location "CH26+
         UEPOS      LIKE VBAP-UEPOS,     "node no mother
         UPMAT      LIKE VBAP-UPMAT,     "mother materail
         KWMENG     LIKE VBAP-KWMENG,    "(VBEP-BMENG) Confirm QTY
         WMENG      LIKE VBEP-WMENG,     "(VBEP-WMENG) Req Qty
         NETPR      LIKE VBAP-NETPR,     "Amount per unit
         NETWR      LIKE VBAP-NETWR,     "Amount total
         EDATU      LIKE VBEP-EDATU,     "Delivery Date
         KTEXT      LIKE COAS-KTEXT,     "IO name
         PRCTR      LIKE VBAP-PRCTR,     "Profit Center
         MWSBP      LIKE VBAP-MWSBP,     "Profit Center
         POSEX      LIKE VBAP-POSEX,     " PO ITEM
         KONDM      TYPE VBAP-KONDM,     " material price group
         ROUTE      TYPE VBAP-ROUTE,     "Route
         PS_PSP_PNR TYPE VBAP-PS_PSP_PNR, "WBS
         INACT      LIKE JEST-INACT,     "Status block
         C_PROCON   TYPE C,              "Check Production control
         LGPBE      TYPE MARD-LGPBE,     "Storage Bin "CH26+
       END OF TYP_VBAP.

TYPES: BEGIN OF TYP_SHOW_MOTHER,
         VBELN LIKE VBAP-VBELN,     "Sale order
         POSNR LIKE VBAP-POSNR,     "Item
         MATNR LIKE VBAP-MATNR,     "material
         UEPOS LIKE VBAP-UEPOS,     "node no mother

       END OF TYP_SHOW_MOTHER.

TYPES: BEGIN OF TYP_CHECK,
         VBELN TYPE VBAK-VBELN,
         AUFNR TYPE AUFK-AUFNR,  "Add by Wantanee

       END OF TYP_CHECK.

TYPES: BEGIN OF TYP_JEST,
         OBJNR LIKE JEST-OBJNR,
         INACT LIKE JEST-INACT,    "Status block
       END OF TYP_JEST.

TYPES: BEGIN OF TYP_VBKD,
         VBELN LIKE VBKD-VBELN,
         POSNR LIKE VBKD-POSNR,
         ZTERM LIKE VBKD-ZTERM,
         BZIRK LIKE VBKD-BZIRK,
         KDGRP LIKE VBKD-KDGRP,
         KONDA LIKE VBKD-KONDA,
         IHREZ LIKE VBKD-IHREZ,
       END OF TYP_VBKD.

TYPES: BEGIN OF TYP_PROCON,
         MATNR  LIKE ZSDSSDT003-MATNR,
         EFDAT  LIKE ZSDSSDT003-EFDAT,
         EFEDAT LIKE ZSDSSDT003-EFEDAT,
       END OF TYP_PROCON.

TYPES: BEGIN OF TYP_DISCON,
         MATNR  LIKE ZSDSSDT003-MATNR,
         EFDAT  LIKE ZSDSSDT003-EFDAT,
         EFEDAT LIKE ZSDSSDT003-EFEDAT,
       END OF TYP_DISCON.

TYPES: BEGIN OF TYP_CUSTCON,
         KUNNR LIKE ZSDSSDT004-KUNNR,
         VTWEG LIKE ZSDSSDT004-VTWEG,
         VKBUR LIKE ZSDSSDT004-VKBUR,
         NAME2 LIKE ZSDSSDT004-NAME2,
       END OF TYP_CUSTCON.

TYPES: BEGIN OF TYP_PARTNER,
         VBELN LIKE VBPA-VBELN,
         POSNR LIKE VBPA-POSNR,
         PARVW LIKE VBPA-PARVW,
         KUNNR LIKE VBPA-KUNNR,
         PERNR LIKE VBPA-PERNR,
         ADRNR LIKE VBPA-ADRNR,
       END OF TYP_PARTNER.
TYPES: BEGIN OF TYP_AUFK,
         AUFNR LIKE AUFK-AUFNR,  "IO No
         KDAUF LIKE AUFK-KDAUF,   "Quotation no
         PARVW LIKE VBPA-PARVW,   "Type partner
         KUNNR LIKE VBPA-KUNNR,   "Customer no
         ADRNR LIKE VBPA-ADRNR,   "Address no
       END OF TYP_AUFK.

TYPES: BEGIN OF TYP_GEN_C,
         CONST TYPE ZSDSCAC001-PARAM,
         VALUE TYPE ZSDSCAC001-VALUE_LOW,
       END OF TYP_GEN_C.
TYPES: BEGIN OF TYP_ZSDSEMP_SAREA,
         VKBUR TYPE ZSDSSDT005-VKBUR,
         VKGRP TYPE ZSDSSDT005-VKGRP,
         PERNR TYPE ZSDSSDT005-PERNR,
       END OF TYP_ZSDSEMP_SAREA.

TYPES : BEGIN OF TYP_TVKBT,
          VKBUR LIKE        TVKBT-VKBUR,
          BEZEI LIKE        TVKBT-BEZEI,
        END OF TYP_TVKBT.

TYPES : BEGIN OF TYP_ZTSD_PROFIT,
          VKBUR LIKE        ZSDSSDT006-VKBUR,
          VKGRP LIKE        ZSDSSDT006-VKGRP,
          PRCTR LIKE        ZSDSSDT006-PRCTR,
        END OF TYP_ZTSD_PROFIT.

*TYPES : BEGIN OF GY_T685A,
*          KSCHL TYPE T685A-KSCHL,
*          KRECH TYPE T685A-KRECH,
*        END OF GY_T685A.

*--------------------------------------------------------------------*
* Type for Pay In ALV
*--------------------------------------------------------------------*
*TYPES : BEGIN OF gy_payin_alv,
*          check    TYPE c,
*          kunnr    TYPE ztb_pay_in-kunnr,
*          name1    TYPE kna1-name1,
*          payno    TYPE ztb_pay_in-payno,
*          paydate  TYPE ztb_pay_in-paydate,
*          bank     TYPE ztb_pay_in-bank,
*          payamt   TYPE ztb_pay_in-payamt,
*          rempi    TYPE ztb_pay_in-rempi,
*          expen    TYPE ztb_pay_in-expen,
*          incom    TYPE ztb_pay_in-incom,
*          erdat    TYPE ztb_pay_in-erdat,
*          ernam    TYPE ztb_pay_in-ernam,
*          ref_412  TYPE ztb_pay_in-ref_412,
*          r412dat  TYPE ztb_pay_in-r412dat,
*          advno    TYPE ztb_pay_in-advno,
*          flacm    TYPE ztb_pay_in-flacm,
*          remark_1 TYPE ztb_pay_in-remark_1,
*          used     TYPE c. "vbak-netwr.
*TYPES END OF gy_payin_alv.
*--------------------------------------------------------------------*
* END Type for Pay In ALV
*--------------------------------------------------------------------*
TYPES : BEGIN OF GY_VBAP_PDC,
          VBELN TYPE VBAP-VBELN,
          POSNR TYPE VBAP-POSNR,
          NETWR TYPE VBAP-NETWR,
          MWSBP TYPE VBAP-MWSBP,
          KUNNR TYPE VBAK-KUNNR,
        END OF GY_VBAP_PDC,

        BEGIN OF TY_KNVV,
          KUNNR TYPE KNVV-KUNNR,
          VKORG TYPE KNVV-VKORG,
          VTWEG TYPE KNVV-VTWEG,
          SPART TYPE KNVV-SPART,
          KVGR1 TYPE KNVV-KVGR1,
          KVGR2 TYPE KNVV-KVGR2,
          KVGR3 TYPE KNVV-KVGR3,
          KVGR4 TYPE KNVV-KVGR4,
          KVGR5 TYPE KNVV-KVGR5,
        END OF TY_KNVV,

        TTY_KNVV TYPE STANDARD TABLE OF TY_KNVV.

*--------------------------------------------------------------------*
* Range
*--------------------------------------------------------------------*
RANGES: R_VKBUR         FOR VBAK-VKBUR,
        R_VKGRP         FOR VBAK-VKGRP.

DATA : GR_KSCHL   TYPE RANGE OF T685A-KSCHL.
DATA : GR_CHK_REV TYPE RANGE OF T685A-KSCHL.

DATA: GR_DISC_PERCEN TYPE RANGE OF T685A-KSCHL,
      GS_DISC_PERCEN LIKE LINE OF GR_DISC_PERCEN.

DATA: GR_DISC_VALUE TYPE RANGE OF T685A-KSCHL,
      GS_DISC_VALUE LIKE LINE OF GR_DISC_VALUE.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

DATA: GV_FORMNAME  TYPE TDSFNAME,
      GV_FM_NAME   TYPE RS38L_FNAM,
      GV_TABIX     TYPE SYST-TABIX,
      GV_LAST_ITEM TYPE SYST-TABIX.
*&-----------------------------------------------------------------------------------*
*& D A T A Variables
*&-----------------------------------------------------------------------------------*
DATA : GT_VBAP_PDC TYPE TABLE OF GY_VBAP_PDC,
       GS_VBAP_PDC TYPE GY_VBAP_PDC.

DATA: GV_VBELN          LIKE VBAK-VBELN,
      GV_KNUMV          TYPE KNUMV,
      GV_VKBUR          TYPE VKBUR,
      GV_TXTNAME_INVRMK TYPE TDOBNAME,
      GV_SPRAS_ADRC     TYPE NA_SPRAS.
DATA: LWA_HEADER TYPE TYP_HEADER,
      LWA_TOTAL  TYPE TYP_TOTAL,
      LWA_ATTCH  TYPE TYP_ATTCH,
      LWA_PREM   TYPE TYP_PREM,
      LWA_MGR    TYPE TYP_MGR.
DATA: LT_ITEM        TYPE TYP_ITEM_TABLE,
      LT_ITEM2       TYPE TYP_ITEM_TABLE2,
      LT_TEXT_Z009   TYPE TABLE OF TLINE,
      LT_VBAP        TYPE TYP_ITEM_VBAP,   "C3-15072010
      LV_TEXT_INVRMK TYPE TDOBNAME,
      LWA_HEAD_FRM   TYPE TYP_HEADER,
      LT_ITEM_FRM    TYPE TYP_ITEM_TABLE,
      LT_ITEM_FRM2   TYPE TYP_ITEM_TABLE2,
      LWA_TOTAL_FRM  TYPE TYP_TOTAL,
      LWA_ATTCH_FRM  TYPE TYP_ATTCH,
      LWA_PREM_FRM   TYPE TYP_PREM,
      LWA_MGR_FRM    TYPE TYP_MGR,
      LT_ITEM_VBAP   TYPE TYP_ITEM_VBAP.
DATA: LV_DATE              LIKE VBAK-ERDAT,
      LV_DATE_CONVERTED(9) TYPE C.

*-> internal tables
DATA: GT_HEADER           TYPE STANDARD TABLE OF TYP_HEADER,
      GT_VBAK             TYPE STANDARD TABLE OF TYP_VBAK,
      GT_KONV             TYPE STANDARD TABLE OF TYP_KONV,
      GT_VBEP             TYPE STANDARD TABLE OF TYP_VBEP,
      GT_PA0001           TYPE STANDARD TABLE OF TYP_PA0001,
      GT_ITEM             TYPE STANDARD TABLE OF TYP_VBAP,
      GT_ITEM1            TYPE STANDARD TABLE OF TYP_ITEM,
      GT_ITEM2            TYPE STANDARD TABLE OF TYP_ITEM2,
      GT_CHECK            TYPE STANDARD TABLE OF TYP_CHECK,
      GT_CHECK_MOTHER     TYPE STANDARD TABLE OF TYP_SHOW_MOTHER,
      GT_ITEM_VBAP_CHECKP TYPE STANDARD TABLE OF TYP_SHOW_MOTHER,
      GT_JEST             TYPE STANDARD TABLE OF TYP_JEST,
      GT_TOTAL            TYPE STANDARD TABLE OF TYP_TOTAL,
      GT_VBKD             TYPE STANDARD TABLE OF TYP_VBKD,
      GT_PRODCON          TYPE STANDARD TABLE OF TYP_PROCON,
      GT_DISCON           TYPE STANDARD TABLE OF TYP_PROCON,
      GT_CUSTCON          TYPE STANDARD TABLE OF TYP_CUSTCON,
      GT_PARTNER          TYPE STANDARD TABLE OF TYP_PARTNER,
      GT_AUFK             TYPE STANDARD TABLE OF TYP_AUFK,
      GT_GEN_C            TYPE STANDARD TABLE OF TYP_GEN_C,
      GT_EMP_SAREA        TYPE STANDARD TABLE OF TYP_ZSDSEMP_SAREA,
      GT_TVKBT_SSS        TYPE STANDARD TABLE OF TYP_TVKBT,
      GT_ZTSD_PROFIT      TYPE STANDARD TABLE OF TYP_ZTSD_PROFIT.
*-> range
*-> work areas
DATA: GWA_HEADER      TYPE TYP_HEADER,
      GWA_ATTCH       TYPE TYP_ATTCH,
      GWA_PREM        TYPE TYP_PREM,
      GWA_FIRST       TYPE TYP_VBAK,
      GWA_TOTAL       TYPE TYP_TOTAL,
      GW_VBAK         TYPE TYP_VBAK,
      GW_KONV         TYPE TYP_KONV,
      GA_KONV         TYPE TYP_KONV,
      GW_VBEP         TYPE TYP_VBEP,
      GW_PA0001       TYPE TYP_PA0001,
      GW_ITEM         TYPE TYP_VBAP,
      GW_ITEM1        TYPE TYP_ITEM,
      GW_ITEM2        TYPE TYP_ITEM2,
      GW_CHECK        TYPE TYP_CHECK,
      GW_CHECK_MOTHER TYPE  TYP_SHOW_MOTHER,
      GW_JEST         TYPE  TYP_JEST,
      GW_TOTAL        TYPE  TYP_TOTAL,
      GW_VBKD         TYPE  TYP_VBKD,
      GW_PRODCON      TYPE  TYP_PROCON,
      GW_CUSTCON      TYPE  TYP_CUSTCON,
      GW_PARTNER      TYPE  TYP_PARTNER,
      GW_AUFK         TYPE  TYP_AUFK,
      GW_TVKBT_SSS    TYPE  TYP_TVKBT,
      GW_ZTSD_PROFIT  TYPE TYP_ZTSD_PROFIT.

DATA: WA_VBAK         TYPE TYP_VBAK,
      WA_KONV         TYPE TYP_KONV,
      WA_VBEP         TYPE TYP_VBEP,
      WA_PA0001       TYPE TYP_PA0001,
      WA_ITEM         TYPE TYP_VBAP,
      WA_ITEM1        TYPE TYP_ITEM,
      WA_ITEM2        TYPE TYP_ITEM2,
      WA_CHECK_MOTHER TYPE  TYP_SHOW_MOTHER,
      WA_JEST         TYPE  TYP_JEST,
      WA_TOTAL        TYPE  TYP_TOTAL,
      WA_VBKD         TYPE  TYP_VBKD,
      WA_PRODCON      TYPE  TYP_PROCON,
      WA_DISCON       TYPE  TYP_DISCON,
      WA_CUSTCON      TYPE  TYP_CUSTCON,
      WA_PARTNER      TYPE  TYP_PARTNER,
      WA_AUFK         TYPE  TYP_AUFK,
      WA_GEN_C        TYPE TYP_GEN_C,
      WA_EMP_SAREA    TYPE TYP_ZSDSEMP_SAREA,
      WA_TVKBT_SSS    TYPE  TYP_TVKBT,
      FLAG_DISC,
      WA_ZTSD_PROFIT  TYPE TYP_ZTSD_PROFIT.

DATA: T_OUTPUT_INFO  TYPE SSFCRESCL.
DATA: GWA_CONTROL_PARAMETERS TYPE SSFCTRLOP,
      GWA_OUTPUT_OPTIONS     TYPE SSFCOMPOP.
DATA: LV_LANGU_NAST LIKE NAST-SPRAS.
DATA: LV_COUNT_ALL   TYPE I,
      LV_COUNT_BLOCK TYPE I,
      LV_COUNT_ITEM  TYPE I,
      IT_LINES       TYPE STANDARD TABLE OF TLINE,
      V_NAME         TYPE THEAD-TDNAME.

DATA : GV_CHECK_PROFIT TYPE C,
       GV_CHECK_TABIX  TYPE C,
       GV_QTY_ITEM     TYPE C LENGTH 15,
       GV_REQ_QTY      TYPE C LENGTH 15.

DATA: XSCREEN(1)   TYPE C.                "Output on printer or screen
DATA: LV_FM        TYPE RS38L_FNAM.
DATA: GT_KNVV      TYPE TTY_KNVV.


*-> variables
*-> reference

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : GC_YES           TYPE C    VALUE 'X',
            GC_X             TYPE C    VALUE 'X',
            GC_REPID         TYPE REPID      VALUE 'ZSDSSDR0090',
            C_LANGU_TH       LIKE SYST-LANGU VALUE '2',
            GC_FORM_NAME_NEW TYPE TDSFNAME   VALUE 'ZSDSSD006'.
CONSTANTS: LC_PTNRFUNC_SOLDTO     LIKE VBPA-PARVW VALUE 'AG',
           LC_PTNRFUNC_SHIPTO     LIKE VBPA-PARVW VALUE 'WE',
           LC_PTNRFUNC_FWDAGNT    LIKE VBPA-PARVW VALUE 'SP',
           LC_PTNRFUNC_SALEPERSON LIKE VBPA-PARVW VALUE 'VE',
           LC_PTNRFUNC_BILLTO     LIKE VBPA-PARVW VALUE 'RE',
           LC_POSNR               TYPE POSNR VALUE '000000',
           LC_KPOSN               TYPE KPOSN VALUE '000000',
           LC_TITLE_TH            LIKE TSAD3T-TITLE VALUE '0005',
           LC_SPRAS_EN            TYPE SPRAS VALUE 'E',
           LC_SPRAS_TH            TYPE SPRAS VALUE '2'.
CONSTANTS: LC_KSCHL_ZDO3 TYPE KSCHL VALUE 'ZD03'.

CONSTANTS : BEGIN OF GC_CON,
              I    TYPE C LENGTH 1 VALUE 'I',
              NE   TYPE C LENGTH 2 VALUE 'NE',
              EQ   TYPE C LENGTH 2 VALUE 'EQ',
              CP   TYPE C LENGTH 2 VALUE 'CP',
              S    TYPE C LENGTH 1 VALUE 'S',
              E    TYPE C LENGTH 1 VALUE 'E',
              ZPR0 TYPE C LENGTH 4 VALUE 'ZPR0', " price
              ZPR1 TYPE C LENGTH 4 VALUE 'ZPR1', " price
            END OF GC_CON.
*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:
