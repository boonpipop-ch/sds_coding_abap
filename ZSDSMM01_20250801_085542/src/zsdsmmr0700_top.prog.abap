*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0700_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : LIKP,
         LIPS,
         T001,
         VBPA,
         ADRC,
         KNA1,
         MAKT,
         SER01,
         OBJK,
         MARA,
         EKET,
         EKKO,
         ZSDSSDC002.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          TRKORR  TYPE E070-TRKORR,
          AS4USER TYPE E070-AS4USER,
          AS4TEXT TYPE E07T-AS4TEXT,
          CHECK   TYPE C,
        END OF GY_RESULT.

TYPES: BEGIN OF TYP_ZSDSSDC003,
         KUNNR        TYPE ZSDSSDC003-KUNNR,
         VKBUR        TYPE ZSDSSDC003-VKBUR,
         Z_CUST_GROUP TYPE ZSDSSDC003-Z_CUST_GROUP,
       END OF TYP_ZSDSSDC003.

*End Add by Wantanee ITR2015-3851

*Add by Wantanee 20160502
TYPES : BEGIN OF TYP_ZSDSSDC004,
          MATNR TYPE ZSDSSDC004-MATNR,
        END OF TYP_ZSDSSDC004.
*End Add by Wantanee 20160502

TYPES: BEGIN OF TYP_TVLAT,
         LSTEL TYPE TVLAT-LSTEL,
         VTEXT TYPE TVLAT-VTEXT,
       END OF TYP_TVLAT.

TYPES: BEGIN OF TYP_ZSDSSDC005,
         MATNR TYPE ZSDSSDC005-MATNR,
       END OF TYP_ZSDSSDC005.
TYPES: BEGIN OF S_MAT_AT,
         MATNR TYPE VBAP-MATNR, " Material number (BOM Header)
         IDNRK TYPE STPO-IDNRK, " Component
       END OF S_MAT_AT.

TYPES: BEGIN OF TYP_ZSDSSDC006,
         MATNR TYPE ZSDSSDC006-MATNR,
         AUART TYPE ZSDSSDC006-AUART,
       END OF TYP_ZSDSSDC006.


TYPES : BEGIN OF S_LSTEL,
          VBELN TYPE LIKP-VBELN,
          LSTEL TYPE LIKP-LSTEL,
        END OF S_LSTEL.
TYPES : ST_LSTEL TYPE STANDARD TABLE OF S_LSTEL WITH EMPTY KEY.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

DATA: BEGIN OF GT_ITAB OCCURS 0,
        LFART   LIKE LIKP-LFART,
        VBELN   LIKE LIKP-VBELN,
        LFDAT   LIKE LIKP-LFDAT,
        LGORT   LIKE LIPS-LGORT, "add by Aphai on Sep 4th, 2014

        ERDAT   LIKE LIKP-ERDAT,
        POSNR   LIKE LIPS-POSNR,
        MATNR   LIKE LIPS-MATNR,


        ARKTX   LIKE LIPS-ARKTX,
        LGMNG   LIKE LIPS-LGMNG,
        KUNNR   LIKE LIKP-KUNNR,

        UEPOS   LIKE LIPS-UEPOS,
        LIFNR   LIKE LIKP-LIFNR,
        LSTEL   LIKE LIKP-LSTEL,

        VTWEG   TYPE VBAK-VTWEG,
        VKBUR   TYPE VBAK-VKBUR,
        KNUMV   TYPE VBAK-KNUMV,

        PRODH   TYPE VBAP-PRODH,
        NETWR   TYPE VBAP-NETWR,
        ERSDA   TYPE MARA-ERSDA,

        POSNV   TYPE LIPS-POSNV,  "Add by Wantanee 20141027
        AUFNR   TYPE LIPS-AUFNR,  "Add by Wantanee 20150521

        VGBEL   TYPE LIPS-VGBEL,

        VKGRP   TYPE VBAK-VKGRP, "Sales group
        ZTERM   TYPE VBKD-ZTERM, "Term of payment
        BSTKD   TYPE VBKD-BSTKD, "PO number
        BSTKD_E TYPE VBKD-BSTKD_E, "PO Number item
        MEINS   TYPE LIPS-MEINS, "Base Unit of Measure
        KOWRR   TYPE LIPS-KOWRR, "check bom
      END OF GT_ITAB.
DATA: BEGIN OF GT_IITAB OCCURS 0.
        INCLUDE STRUCTURE GT_ITAB.
DATA: END OF GT_IITAB.

*Internal table
DATA: BEGIN OF GT_WARRANTY OCCURS 0.
        INCLUDE STRUCTURE ZSDSSDC007.
DATA: END OF GT_WARRANTY.

DATA: BEGIN OF GT_KONV OCCURS 0.
        INCLUDE STRUCTURE PRCD_ELEMENTS.
DATA: END OF GT_KONV.

DATA: BEGIN OF GT_INTAB OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),

        LFDAT(8),
        ERDAT(8),
        POSNR(4),

        MATNR(35),
        ARKTX(40),
        LGMNG(8),

        STATUS(6),
        KUNNR(10),
        CUST_NAME(40),
        DELI_TO(60),

        REMARK(30),
      END OF GT_INTAB.

DATA: BEGIN OF GT_OUTTAB OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),
        LFDAT(8),
        ERDAT(8),
        POSNR(4),
        MATNR(35),
        ARKTX(40),
        LGMNG(8),
        KUNNR(10),
        CUST_NAME(60),
        DELI_TO(60),
        REMARK(250),
        PICHON(250),
        REMARK_PICHON(1),
        LGORT(10),                                          "T41K912266
        WARRANTY(1),                                        "T41K917643  "Add by Wantanee 20140923
        SO(10),
        REQ_MAP(1),
        REQ_INV(1),
        SHIP_ADDR(220)     TYPE C,
        SHIP_PROVINCE(30),
        POSTCODE(5),
        AM_PM(2),
        LOADING_POINT(20),
        SALESMAN(40),
        DUE_DATE(8),
        PO_NO(35),
        PROJECT(40),
        TERM_OF_PAYMENT(4),
        CONTACT_NAME(35),
        REF_INV(10),
        SALES_DIV(2),
        SALES_OFFICE(20),
        SALES_GROUP(20),
        UOM(5),
        DOC_FLAG(1),
        MHA(1),
        FLAG_BOM(1),
        REFER_BOM(4),
        INSULATION(10),
        SPSOLC(6),                                          "T41K912266

      END OF GT_OUTTAB.

DATA: BEGIN OF GT_OUTTAB3 OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),

        LFDAT(8),
        ERDAT(8),
        POSNR(4),

        MATNR(35),
        ARKTX(40),
        LGMNG(8),

        KUNNR(10),
        CUST_NAME(40),
        DELI_TO(60),

        REMARK(30),

        PICHON        TYPE CHAR255,
      END OF GT_OUTTAB3.

DATA: BEGIN OF GT_ITAB2 OCCURS 0,
        VBELN LIKE LIKP-VBELN,
        POSNR LIKE LIPS-POSNR,
        MATNR LIKE LIPS-MATNR,
        LIFEX LIKE LIKP-LIFEX,
        VGBEL LIKE LIPS-VGBEL,
        VGPOS LIKE LIPS-VGPOS,
      END OF GT_ITAB2.

DATA: BEGIN OF GT_ITAB3 OCCURS 0,
        LFART LIKE LIKP-LFART,
        VBELN LIKE LIKP-VBELN,
        LFDAT LIKE LIKP-LFDAT,

        ERDAT LIKE LIKP-ERDAT,
        POSNR LIKE LIPS-POSNR,
        MATNR LIKE LIPS-MATNR,

        ARKTX LIKE LIPS-ARKTX,
        LGMNG LIKE LIPS-LGMNG,
        KUNNR LIKE LIKP-KUNNR,

        UEPOS LIKE LIPS-UEPOS,
      END OF GT_ITAB3.

DATA: BEGIN OF GT_SERNR OCCURS 0,
        SERNR LIKE OBJK-SERNR,
      END OF GT_SERNR.

DATA: BEGIN OF GT_INTAB2 OCCURS 0,
        LICHA(15),  "like eket-licha,
        EAN11(18),  "like mara-ean11,
        MATNR(35),  "like lips-matnr,
        SERNR(18),  "like objk-sernr,
        LIFEX(35),  "like likp-lifex,
      END OF GT_INTAB2.

DATA: GW_OBJK TYPE OBJK.

DATA: GT_OBJK LIKE STANDARD TABLE OF GW_OBJK.


DATA: GT_OUTTAB2    LIKE GT_OUTTAB OCCURS 0,
      WA_OUTTAB2    LIKE GT_OUTTAB,
      WA_IITAB      LIKE GT_IITAB,
      WA_IITAB2     LIKE GT_IITAB,
      WA_OUTTAB     LIKE GT_OUTTAB,
      WA_WARRANTY   LIKE GT_WARRANTY,
      WA_KONV       LIKE GT_KONV,
      DT_CONS_LGORT TYPE TABLE OF ZSDSCAC002,
      WA_CONS_LGORT TYPE ZSDSCAC002.


DATA: WA_IITAB_TEMP   LIKE GT_IITAB.

DATA: BEGIN OF GT_OUTTAB4 OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),
        LFDAT(8),
        ERDAT(8),
        POSNR(4),
        MATNR(35),
        ARKTX(40),
        LGMNG(8),
        KUNNR(10),
        CUST_NAME(40),
        DELI_TO(60),
        REMARK(30),
        PICHON           TYPE CHAR255,
        REMARK_PICHON(1),
        LGORT(10),                                          "T41K912266
        WARRANTY(1),                                        "WCH290514
        SPSOLC(6).                                          "T41K912266
DATA: END OF GT_OUTTAB4.


"Add by Wantanee 20151020

DATA: BEGIN OF GT_OUTTAB_HI OCCURS 0,
        LFART(1),
        CUST_CODE(3),
        VBELN(10),

        LFDAT(8),
        ERDAT(8),
        POSNR(4),

        MATNR(35),
        ARKTX(40),
        LGMNG(8),

        KUNNR(10),
        CUST_NAME(40),
        DELI_TO(60),

        REMARK(30),

        PICHON             TYPE CHAR255,

        REMARK_PICHON(1),
        LGORT(10),                                          "T41K912266
        WARRANTY(1),                                        "T41K917643  "Add by Wantanee 20140923
        "Add by wantanee 20151020 ITR2015-4118
        DEST_ADDR(50),
        DEST_SUB_DIS(50),
        DEST_DISTRIC(50),
        DEST_PROVIN(50),
        DEST_POST_CODE(50),
      END OF GT_OUTTAB_HI.
"Add by wantanee 20151020 ITR2015-4118
"End Add by Wantanee 20151020

TYPES: BEGIN OF TYP_ZSDSSDT002,
         VBELN   TYPE ZSDSSDT002-VBELN,
         RUN_ID  TYPE ZSDSSDT002-RUN_ID,
         REQ_INV TYPE ZSDSSDT002-REQ_INV,
         REQ_MAP TYPE ZSDSSDT002-REQ_INV,
         AM_PM   TYPE ZSDSSDT002-AM_PM,
         ERDAT   TYPE ZSDSSDT002-ERDAT,
         ERZET   TYPE ZSDSSDT002-ERZET,
       END OF TYP_ZSDSSDT002.
TYPES: BEGIN OF TYP_DO_NO,
         VBELN TYPE LIKP-VBELN,
       END OF TYP_DO_NO.


TYPES: BEGIN OF TYP_DO_REFER_BOM,
         VBELN TYPE LIPS-VBELN,
         POSNR TYPE LIPS-POSNR,
         LGMNG TYPE LIPS-LGMNG,
         UEPOS TYPE VBAP-UEPOS,

       END OF TYP_DO_REFER_BOM.



*Add by Wantanee 20150521
DATA: GT_CUST_WARRANTY TYPE STANDARD TABLE OF TYP_ZSDSSDC003,
      WA_CUST_WARRANTY TYPE TYP_ZSDSSDC003.

*End Add by Wantanee 20150521
*-T41K908432

DATA: GV_CUST_ADDR(100).
DATA: GV_SHIP_ADDR TYPE CHAR255.
DATA: GV_SHIP_NAME TYPE CHAR255.  "CH33 Add by Wantanee 20210121

DATA: GV_LOADING_POINT TYPE LIKP-LSTEL.  "Add by wantanee 20140807  T41K917437

DATA: GT_ZSDSSDC004 TYPE STANDARD TABLE OF TYP_ZSDSSDC004,
      GW_ZSDSSDC004 TYPE TYP_ZSDSSDC004,
      WA_ZSDSSDC004 TYPE TYP_ZSDSSDC004.

DATA: GT_TVLAT TYPE STANDARD TABLE OF TYP_TVLAT,
      WA_TVLAT TYPE TYP_TVLAT.
DATA: LV_CHK_LOGRT(200) TYPE C.

DATA: GT_ZSDSSDT002 TYPE STANDARD TABLE OF TYP_ZSDSSDT002,
      WA_ZSDSSDT002 TYPE ZSDSSDT002,
      GT_DO_NO      TYPE STANDARD TABLE OF TYP_DO_NO,
      WA_DO_NO      TYPE TYP_DO_NO.
DATA: GT_DO_TEMP TYPE STANDARD TABLE OF TYP_DO_NO,
      WA_DO_TEMP TYPE TYP_DO_NO.

DATA: GT_ZSDSSDC005 TYPE STANDARD TABLE OF TYP_ZSDSSDC005,
      WA_ZSDSSDC005 TYPE TYP_ZSDSSDC005.

DATA: GT_DO_REFER_BOM TYPE STANDARD TABLE OF TYP_DO_REFER_BOM,
      GW_DO_REFER_BOM TYPE TYP_DO_REFER_BOM,
      WA_DO_REFER_BOM TYPE TYP_DO_REFER_BOM.

DATA: GT_ZSDSSDC006 TYPE STANDARD TABLE OF TYP_ZSDSSDC006,
      GW_ZSDSSDC006 TYPE TYP_ZSDSSDC006,
      WA_ZSDSSDC006 TYPE TYP_ZSDSSDC006.

DATA: I_MAT_AT TYPE TABLE OF S_MAT_AT.

DATA: CHECK_1800_DIT TYPE C.
DATA: CHECK_1600_SMP TYPE C.
DATA: CHECK_1500_JPP TYPE C.  "CH3 Add by Wantanee 20230315
DATA: CHECK_1100_SONY TYPE C.
DATA: FILE_NAME(10) TYPE C.
DATA: TEXT_NAME(100) TYPE C.

*Variable
CONSTANTS: GC_LIFNR(10)   TYPE C VALUE '0000132005'.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS: GC_I  TYPE C LENGTH 1 VALUE 'I',
           GC_EQ TYPE C LENGTH 2 VALUE 'EQ',
           GC_S  TYPE C LENGTH 1 VALUE 'S',
           GC_E  TYPE C LENGTH 1 VALUE 'E',
           GC_X  TYPE C LENGTH 1 VALUE 'X',
           GC_A  TYPE C LENGTH 1 VALUE 'A',
           GC_L  TYPE C LENGTH 1 VALUE 'L'.

*-----------------------------------------------------------------------
* R A N G E S
*-----------------------------------------------------------------------
*DATA:
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
