*&---------------------------------------------------------------------*
*& Include          ZSDSSDI0020_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : VBAK,
         VBRK,
         VBFA,
         CDHDR,
         CDPOS,
         SER01,
         OBJK,
         EQUI_ADDR,
         BGMKOBJ,
         ADRC,
         LIKP,
         LIPS,
         VBUK..
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF TYP_ITAB,

         LIEF_NR          TYPE SER01-LIEF_NR,   "deliver number
         POSNR            TYPE LIPS-POSNR,      "Deli Item  "Add by Wantanee 20130218
         VBELN            TYPE VBAK-VBELN,      "Sale order
         MATNR            TYPE OBJK-MATNR,      "material
         SERNR            TYPE OBJK-SERNR,      "Serial
         DI_QTY           TYPE LIPS-LFIMG,      "Deli QTY
         PK_QTY           TYPE LIPS-LFIMG,      "picking QTY
         KUNNR_SOLDTO     TYPE KNA1-KUNNR,      "Customer code soldto
         NAME_SOLDTO(200) TYPE C,               "Customer name
         KUNNR_SHIPTO     TYPE KNA1-KUNNR,      "Customer code shipto
         NAME_SHIPTO(200) TYPE C,               "Customer name
         CLASS(5)         TYPE C,                           "PH level 2
         CATEGORY(5)      TYPE C,                           "PH level 1
         ERDAT            TYPE LIKP-ERDAT,      "Create date
         KODAT            TYPE LIKP-KODAT,      "pick Date
         LDDAT            TYPE LIKP-LDDAT,      "load Date
         LFDAT            TYPE LIKP-LFDAT,      "deliv.date
         WADAT            TYPE LIKP-WADAT,      "GI Date
         KOSTK(50)        TYPE C,               "Picking Status
         VTWEG_TXT        TYPE TVTWT-VTEXT,     "Dchanel Division Text
         VKBUR_TXT        TYPE TVKBT-BEZEI,     "Sale office
         SALE_CODE        TYPE VBPA-PARNR,      "sale code
         SALE_NAME(200)   TYPE C,               "Sale name
         AUFNR            TYPE AUFK-AUFNR,     "IO Nubber    Project
         KTEXT            TYPE AUFK-KTEXT,      "IO Name Project name
         SHIPTO_ADD1(200) TYPE C,               "ship to address
         ROUTE            TYPE LIKP-ROUTE,      "route
         SHIPTO_LOC       TYPE ADRC-CITY1,       "Location ship-to
         BRGEW            TYPE LIPS-BRGEW,      "Total Wght
         GEWEI            TYPE LIPS-GEWEI,      "WUn
         VOLUM            TYPE LIPS-VOLUM,      "Volum
         VOLEH            TYPE LIPS-VOLEH,      "VUn
         LSTEL_TXT        TYPE TVLAT-VTEXT,     "LoadPt
         TKNUM(100)       TYPE C,               "shipment
         EXTI1(100)       TYPE C,               "External ID1
         EXTI2(100)       TYPE C,               "Truck drive
         VSART            TYPE VTTK-VSART,      "Shipping Type
         INV_REK(200)     TYPE C,               "Invoice remark
         REQ_REK(200)     TYPE C,               "Request remark
         SHIPTO_ADD2(200) TYPE C,               "ship to address
         SHIPTO_CITY2     TYPE ADRC-CITY2,    "ship to distric
         SHIPTO_POST_CODE TYPE ADRC-POST_CODE1,  "post code
         SOLDTO_ADD1(200) TYPE C,               "sold to address
         SOLDTO_ADD2(200) TYPE C,               "sold to address
         SOLDTO_CITY2     TYPE ADRC-CITY2,       "sold to distric
         SOLDTO_LOC       TYPE ADRC-CITY1,       "Location sold-to
         SOLDTO_POST_CODE TYPE ADRC-POST_CODE1,  "post code
         WERKS            TYPE LIPS-WERKS,       "Plant
         AUART            TYPE VBAK-AUART,       "Type sale
         LGORT            TYPE LIPS-LGORT,       "Storage location
         MAT_DOC          TYPE VBFA-VBELN,       "Mat doc.
         MAT_DOC_L        TYPE VBFA-VBELN,       "Mat doc.
         INV_NO           TYPE VBRK-VBELN,       "Invoice No Add by Wantanee 20130621 T41K915489
         NETWR            TYPE VBAP-NETWR,
         VKGRP            TYPE VBAK-VKGRP,
         SALESGROUP       TYPE C LENGTH 255,
         GWLDT            TYPE BGMKOBJ-GWLDT,
         GWLEN            TYPE BGMKOBJ-GWLEN,
         VGPOS            TYPE LIPS-VGPOS,
         VBTYP            TYPE LIKP-VBTYP,
         CHECK            TYPE C,
         ZZPOB            TYPE VBAK-ZZPOB,  "POB
         PS_PSP_PNR       TYPE VBAK-PS_PSP_PNR,
         POST1            TYPE PRPS-POST1,
         LAND_REK(200)    TYPE C,                 "Land No. Remarks.
         PROJ_REK(200)    TYPE C,                 "Project . Remarks.
         BSTKD            TYPE VBKD-BSTKD,        "Customer Reference.
         DI_ZR01          TYPE LIPS-LFIMG,        "Deli QTY ZR01
       END OF TYP_ITAB.
TYPES: BEGIN OF TYP_SERIAL,
         VBELN         TYPE LIKP-VBELN,      "outbound
         POSNR         TYPE LIPS-POSNR,      "Item
         ERDAT         TYPE LIKP-ERDAT,      "create date
         KODAT         TYPE LIKP-KODAT,      "pick Date
         LDDAT         TYPE LIKP-LDDAT,      "load Date
         LFDAT         TYPE LIKP-LFDAT,      "deliv.date
         WADAT         TYPE LIKP-WADAT,      "GI Date
         ROUTE         TYPE LIKP-ROUTE,      "route
         LSTEL         TYPE LIKP-LSTEL,      "loading point
         MATNR         TYPE LIPS-MATNR,      "material
         MEINS         TYPE LIPS-MEINS,      "unit
         BRGEW         TYPE LIPS-BRGEW,      "Total Wght
         GEWEI         TYPE LIPS-GEWEI,      "WUn
         VOLUM         TYPE LIPS-VOLUM,      "Volum
         VOLEH         TYPE LIPS-VOLEH,      "VUn
         SERAIL        TYPE LIPS-SERAIL,     "Serail Type
         PRODH         TYPE LIPS-PRODH,      "PH
         VTWEG         TYPE LIPS-VTWEG,      "Dchanel Division
         VKBUR         TYPE LIPS-VKBUR,      "Sale office
         VKGRP         TYPE LIPS-VKGRP,      "Sale group
         VGBEL         TYPE LIPS-VGBEL,      "Sale Order
         VGPOS         TYPE LIPS-VGPOS,      "item   "Add by Wantanee 20120329
         AUFNR         TYPE LIPS-AUFNR,      "IO number
         SOLD_TO       TYPE KNA1-KUNNR,      "Sold_to Code
         ADRNR_SOLD_TO TYPE VBPA-ADRNR,      "Sold to address
         SHIP_TO       TYPE KNA1-KUNNR,      "Ship to code
         ADRNR_SHIP_TO TYPE VBPA-ADRNR,      "Ship to address
         SALE_CODE     TYPE VBPA-PERNR,      "Sale code
         KOSTK         TYPE VBUK-KOSTK,      "Type status delivery
         LFIMG         TYPE LIPS-LFIMG,      "QTY
         WERKS         TYPE LIPS-WERKS,       "Plant
         RFMNG         TYPE VBFA-RFMNG,       "picking QTY
         AUART         TYPE VBAK-AUART,       "Type sale
         LGORT         TYPE LIPS-LGORT,       "Storage location
*add by Wantanee 20120314
         POSNV         TYPE VBFA-POSNV,       "item
         VBELN_VF      TYPE VBFA-VBELN,       "doc no
         POSNN         TYPE VBFA-POSNN,       "item doc
         VBTYP_N       TYPE VBFA-VBTYP_N,     "type doc column vbeln
         VBTYP         TYPE LIKP-VBTYP,
         ZZPOB         TYPE VBAK-ZZPOB,       "POB
         PS_PSP_PNR    TYPE VBAK-PS_PSP_PNR,  "WBS
       END OF TYP_SERIAL.

TYPES : BEGIN OF TYP_GET_MAT_DOC,
          VBELV   TYPE VBFA-VBELV,
          POSNV   TYPE VBFA-POSNV,
          VBELN   TYPE VBFA-VBELN,
          POSNN   TYPE VBFA-POSNN,
          VBTYP_N TYPE VBFA-VBTYP_N,
          BWART   TYPE VBFA-BWART,
        END OF TYP_GET_MAT_DOC.

TYPES: BEGIN OF TYP_NOSERIAL,
         VBELN         TYPE LIKP-VBELN,      "outbound
         POSNR         TYPE LIPS-POSNR,      "Item
         ERDAT         TYPE LIKP-ERDAT,      "create date
         KODAT         TYPE LIKP-KODAT,      "pick Date
         LDDAT         TYPE LIKP-LDDAT,      "load Date
         LFDAT         TYPE LIKP-LFDAT,      "deliv.date
         WADAT         TYPE LIKP-WADAT,      "GI Date
         ROUTE         TYPE LIKP-ROUTE,      "route
         LSTEL         TYPE LIKP-LSTEL,      "loading point
         MATNR         TYPE LIPS-MATNR,      "material
         MEINS         TYPE LIPS-MEINS,      "unit
         BRGEW         TYPE LIPS-BRGEW,      "Total Wght
         GEWEI         TYPE LIPS-GEWEI,      "WUn
         VOLUM         TYPE LIPS-VOLUM,      "Volum
         VOLEH         TYPE LIPS-VOLEH,      "VUn
         SERAIL        TYPE LIPS-SERAIL,     "Serail Type
         PRODH         TYPE LIPS-PRODH,      "PH
         VTWEG         TYPE LIPS-VTWEG,      "Dchanel Division
         VKBUR         TYPE LIPS-VKBUR,      "Sale office
         VKGRP         TYPE LIPS-VKGRP,      "Sale group
         VGBEL         TYPE LIPS-VGBEL,      "Sale Order
         VGPOS         TYPE LIPS-VGPOS,      "item   "Add by Wantanee 20120329
         AUFNR         TYPE LIPS-AUFNR,      "IO number
         SOLD_TO       TYPE KNA1-KUNNR,      "Sold_to Code
         ADRNR_SOLD_TO TYPE VBPA-ADRNR,      "Sold to address
         SHIP_TO       TYPE KNA1-KUNNR,      "Ship to code
         ADRNR_SHIP_TO TYPE VBPA-ADRNR,      "Ship to address
         SALE_CODE     TYPE VBPA-PERNR,      "Sale code
         KOSTK         TYPE VBUK-KOSTK,      "Type status delivery
         LFIMG         TYPE LIPS-LFIMG,      "QTY
         WERKS         TYPE LIPS-WERKS,       "Plant
         RFMNG         TYPE VBFA-RFMNG,       "picking QTY
         AUART         TYPE VBAK-AUART,       "Type sale
         LGORT         TYPE LIPS-LGORT,       "Storage location
*add by Wantanee 20120314
         POSNV         TYPE VBFA-POSNV,       "item
         VBELN_VF      TYPE VBFA-VBELN,       "doc no
         POSNN         TYPE VBFA-POSNN,       "item doc
         VBTYP_N       TYPE VBFA-VBTYP_N,     "type doc column vbeln
         VBTYP         TYPE LIKP-VBTYP,
         ZZPOB         TYPE VBAK-ZZPOB,  "POB
         PS_PSP_PNR    TYPE VBAK-PS_PSP_PNR,  "WBS
*End add by Wantanee 20120314
       END OF TYP_NOSERIAL.
TYPES: BEGIN OF TYP_ADRC,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,      "Address Number
         NAME1      TYPE KNA1-NAME1,      "Customer Name
         NAME2      TYPE KNA1-NAME2,      "Customer Nam
         NATION     TYPE ADRC-NATION,     "Langu.
         STREET     TYPE ADRC-STREET,     "Street
         CITY1      TYPE ADRC-CITY1,      "
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3, "
         LOCATION   TYPE ADRC-LOCATION,   "
         CITY2      TYPE ADRC-CITY2,      "
         POST_CODE1 TYPE ADRC-POST_CODE1, "

       END OF TYP_ADRC.


TYPES: BEGIN OF TYP_TVKBT,
         VKBUR TYPE VBAK-VKBUR,     "Sale office
         BEZEI TYPE TVKBT-BEZEI,    "Sale office detail
       END OF TYP_TVKBT.

TYPES: BEGIN OF TYP_TVGRT,
         VKGRP TYPE VBAK-VKGRP,          "Sale Group
         BEZEI TYPE TVGRT-BEZEI,         "Sale Group - Text
       END OF TYP_TVGRT.
TYPES: BEGIN OF TYP_TVTWT,
         VTWEG TYPE VBAK-VTWEG,          "Distribution chanel
         VTEXT TYPE TVTWT-VTEXT,         "Distribution chanel - Text
       END OF TYP_TVTWT.
TYPES: BEGIN OF TYP_TVLAT,
         LSTEL TYPE LIKP-LSTEL,      "loading point
         VTEXT TYPE TVLAT-VTEXT,         "loading point - Text
       END OF TYP_TVLAT.
TYPES: BEGIN OF TYP_PA0001,
         PERNR LIKE PA0001-PERNR, "Sale no
         ENAME LIKE PA0001-ENAME,  "sale name

       END OF TYP_PA0001.

TYPES: BEGIN OF TYP_SHIPMENT,
         VBELN TYPE VTTP-VBELN,      "Delivery no.
         TKNUM TYPE VTTP-TKNUM,      "shipment
         EXTI1 TYPE VTTK-EXTI1,      "External ID1
         EXTI2 TYPE VTTK-EXTI2,      "Truck drive
         VSART TYPE VTTK-VSART,      "Shipping Type
       END OF TYP_SHIPMENT.
TYPES: BEGIN OF TYP_DELI,
         VBELN TYPE LIKP-VBELN,      "Delivery no.
       END OF TYP_DELI.

TYPES: BEGIN OF TYP_TVBST,
         TBNAM TYPE TVBST-TBNAM,
         FDNAM TYPE TVBST-FDNAM,
         STATU TYPE TVBST-STATU,
         BEZEI TYPE TVBST-BEZEI,
       END OF TYP_TVBST.

TYPES: BEGIN OF TYP_SER01,
         LIEF_NR TYPE SER01-LIEF_NR,   "Outbound no
         POSNR   TYPE SER01-POSNR,     "Outbound item
         OBKNR   TYPE OBJK-OBKNR,      "Object lis
         MATNR   TYPE OBJK-MATNR,      "material
         SERNR   TYPE OBJK-SERNR,      "serial no
       END OF TYP_SER01.
*End Add by Wantanee 20111206

*Add by Wantanee 20120329
TYPES: BEGIN OF TYP_SALE,
         VGBEL TYPE LIPS-VGBEL,   "sale order no
         VGPOS TYPE LIPS-VGPOS,     "item
       END OF  TYP_SALE.

TYPES: BEGIN OF TYP_CHECK_INV,
         VBELV TYPE VBFA-VBELV,   "sale order no
         POSNV TYPE VBFA-POSNV,     "item
         VBELN TYPE VBFA-VBELN,   "invoice or cn
       END OF TYP_CHECK_INV.
*End Add by Wantanee 20120329

"Add by Wantanee 20130621 T41K915489
TYPES: BEGIN OF TYP_LIEF_NR,
         LIEF_NR TYPE SER01-LIEF_NR,
         POSNR   TYPE SER01-POSNR,

       END OF TYP_LIEF_NR.
*End Add by Wantanee 20130621 T41K915489

TYPE-POOLS : VRM.
TYPE-POOLS: SLIS.
*TYPES : BEGIN OF GY_RESULT,
*          TRKORR  TYPE E070-TRKORR,
*          AS4USER TYPE E070-AS4USER,
*          AS4TEXT TYPE E07T-AS4TEXT,
*          CHECK   TYPE C,
*        END OF GY_RESULT.
*&-----------------------------------------------------------------------------------*
*& D A T A
*&-----------------------------------------------------------------------------------*
DATA: GV_FORMNAME  TYPE TDSFNAME,
      GV_FM_NAME   TYPE RS38L_FNAM,
      GV_TABIX     TYPE SYST-TABIX,
      GV_LAST_ITEM TYPE SYST-TABIX.

DATA: V_POS TYPE I.

DEFINE DEF_LIST_HEAD.
  CLEAR: LW_LISTLINE.
  LW_LISTLINE-TYP  = &1.
  LW_LISTLINE-KEY  = &2.
  LW_LISTLINE-INFO = &3.
  APPEND LW_LISTLINE TO LT_LISTHEAD.
END-OF-DEFINITION.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF TYP_ITAB,
       GS_RESULT TYPE TYP_ITAB.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

DATA: GT_ITAB         TYPE STANDARD TABLE OF TYP_ITAB,
      GT_PA0001       TYPE STANDARD TABLE OF TYP_PA0001,
      GT_SERIAL       TYPE STANDARD TABLE OF TYP_SERIAL,
      GT_GET_MAT_DOC  TYPE STANDARD TABLE OF TYP_GET_MAT_DOC,
      GT_ADRC         TYPE STANDARD TABLE OF TYP_ADRC,
      GT_TVKBT        TYPE STANDARD TABLE OF TYP_TVKBT,
      GT_TVGRT        TYPE STANDARD TABLE OF TYP_TVGRT,
      GT_TVTWT        TYPE STANDARD TABLE OF TYP_TVTWT,
      GT_TVLAT        TYPE STANDARD TABLE OF TYP_TVLAT,
      GT_SHIPMENT     TYPE STANDARD TABLE OF TYP_SHIPMENT,
      GT_SHIPMENTNSR  TYPE STANDARD TABLE OF TYP_SHIPMENT,
      GT_DELISR       TYPE STANDARD TABLE OF TYP_DELI,
      GT_DELINSR      TYPE STANDARD TABLE OF TYP_DELI,
      GT_NOSERIAL     TYPE STANDARD TABLE OF TYP_NOSERIAL,
      GT_TVBST        TYPE STANDARD TABLE OF TYP_TVBST,
      GT_SER01        TYPE STANDARD TABLE OF TYP_SER01,
      GT_SERIAL_TMP   TYPE STANDARD TABLE OF TYP_SERIAL,  "Add by Wantanee 20120314
      GT_NOSERIAL_TMP TYPE STANDARD TABLE OF TYP_NOSERIAL, "Add by Wantanee 20120314
      GT_SALE_SER     TYPE STANDARD TABLE OF TYP_SALE, "Add by Wantanee 20120329
      GT_SALE_NOSER   TYPE STANDARD TABLE OF TYP_SALE, "Add by Wantanee 20120329
      GT_CHECK_INV_S  TYPE STANDARD TABLE OF TYP_CHECK_INV, "Add by Wantanee 20120329
      GT_CHECK_INV_NS TYPE STANDARD TABLE OF TYP_CHECK_INV, "Add by Wantanee 20120329
      GT_LIEF_NR      TYPE STANDARD TABLE OF TYP_LIEF_NR.

DATA: WA_ITAB          TYPE TYP_ITAB,
      WA_PA0001        TYPE TYP_PA0001,
      WA_SERIAL        TYPE TYP_SERIAL,
      WA_ADRC          TYPE TYP_ADRC,
      WA_TVKBT         TYPE TYP_TVKBT,
      WA_TVGRT         TYPE TYP_TVGRT,
      WA_TVTWT         TYPE TYP_TVTWT,
      WA_TVLAT         TYPE TYP_TVLAT,
      WA_SHIPMENT      TYPE TYP_SHIPMENT,
      WA_SHIPMENTNSR   TYPE TYP_SHIPMENT,
      WA_DELISR        TYPE TYP_DELI,
      WA_DELINSR       TYPE TYP_DELI,
      WA_NOSERIAL      TYPE TYP_NOSERIAL,
      WA_TVBST         TYPE TYP_TVBST,
      WA_SER01         TYPE TYP_SER01,
      WA_SERIAL_TMP    TYPE TYP_SERIAL,
      WA_NOSERIAL_TMP  TYPE TYP_NOSERIAL,
      WA_SALE_SER      TYPE TYP_SALE, "Add by Wantanee 20120329
      WA_SALE_NOSER    TYPE TYP_SALE, "Add by Wantanee 20120329
      WA_CHECK_INV_S   TYPE TYP_CHECK_INV, "Add by Wantanee 20120329
      WA_CHECK_INV_NS  TYPE TYP_CHECK_INV, "Add by Wantanee 20120329
      WA_CHK_SMAT_DOC  TYPE TYP_SERIAL,  "Add by Wantanee 20120503
      WA_CHK_NSMAT_DOC TYPE TYP_NOSERIAL, "Add by Wantanee 20120503
      WA_LIEF_NR       TYPE  TYP_LIEF_NR.

DATA: GW_ITAB         TYPE TYP_ITAB.


*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
*Variable
CONSTANTS: GC_CHARX TYPE C VALUE 'X',
           GC_REPID TYPE ZSDSCAC002-REPID VALUE 'ZR_SD_OUTBOUND_NOT_INV'.

CONSTANTS: GC_LANG_EN TYPE C      VALUE 'E',
           GC_LANG_TH TYPE C      VALUE 'T',
           GC_MARK_X  TYPE C      VALUE 'X'.
CONSTANTS: GC_MASK_TIME TYPE CHAR8  VALUE '__:__',
           GC_MASK_DATE TYPE CHAR10 VALUE '__/__/____'.
CONSTANTS: GC_TMTY_HEADER TYPE CHAR1  VALUE 'S'.

CONSTANTS: GC_I  TYPE C LENGTH 1 VALUE 'I',
           GC_EQ TYPE C LENGTH 2 VALUE 'EQ',
           GC_S  TYPE C LENGTH 1 VALUE 'S',
           GC_E  TYPE C LENGTH 1 VALUE 'E',
           GC_X  TYPE C LENGTH 1 VALUE 'X',
           GC_A  TYPE C LENGTH 1 VALUE 'A',
           GC_L  TYPE C LENGTH 1 VALUE 'L'.

*&---------------------------------------------------------------------*
*& Program Variables
*&---------------------------------------------------------------------*
DATA: XSCREEN(1)   TYPE C.                "Output on printer or screen
DATA: LV_FM        TYPE RS38L_FNAM.
