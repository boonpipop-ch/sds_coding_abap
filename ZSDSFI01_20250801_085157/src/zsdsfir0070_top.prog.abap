*&---------------------------------------------------------------------*
*& Include          ZSDSFII0070_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : WITH_ITEM,  "Witholding tax info per W/tax type and FI line item
         T059ZT,     "Withholding tax code (enhanced functions)
         LFA1,       "Vendor Master
         BSEC,       "One-Time Account Data Document Segment
         KNA1.       "Customer Master
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF TY_RESULT,
          BELNR            TYPE  BELNR_D,     "Accounting Document Number
          GJAHR            TYPE  GJAHR,      "Fiscal Year
          BUZEI            TYPE  BUZEI,      "Number of Line Item Within Accounting Document
          WITHT            TYPE  WITHT,      "Indicator for withholding tax type
          WT_WITHCD        TYPE  WT_WITHCD,  "Withholding tax code
          WITHCD_DS(40)    TYPE  C,
          WT_QSSHH         TYPE  WT_BS,      "Withholding tax base amount (local currency)
          WT_QSSHB         TYPE  WT_BS1,     "Withholding tax base amount in document currency
          WT_BASMAN        TYPE  WT_BASMAN,  "Indicator: Withholding tax base amount entered manually
          WT_QSSHHC        TYPE  WT_BSC,      "Withholding tax base amount in LC for calculation
          WT_QSSHBC        TYPE  WT_BS1C,    "Withholding tax base amount in doc. currency for calculation
          WT_QBSHH         TYPE  WT_WT,      "Withholding tax amount (in local currency)
          WT_QBSHB         TYPE  WT_WT,      "Withholding tax amount in document currency
          WT_AMNMAN        TYPE  WT_AMNMAN,  "Indicator: Withholding tax amount entered manually
          WT_STAT          TYPE  WT_STAT,    "Line item status
          WT_QSFHH         TYPE  WT_EXMP,    "Amount exempt from withholding tax in local currency
          WT_QSFHB         TYPE  WT_EXMP,    "Amount exempt from withholding tax in document currency
          WT_WTEXMN        TYPE  WT_EXNR,    "Exemption certificate number
          KOART            TYPE  KOART,      "Account Type
          WT_ACCO          TYPE  WT_ACNO,    "Vendor/customer account number
          NAME(200)        TYPE  C,      "Vendor/customer name
          HKONT            TYPE  HKONT,      "General Ledger Account
          QSREC            TYPE  WT_QSREC,    "Type of recipient
          AUGBL            TYPE  AUGBL,      "Document Number of the Clearing Document
          AUGDT            TYPE  AUGDT,      "Clearing Date
          CTNUMBER         TYPE  CTNUMBER,   "Withholding Tax Certificate Number
          STCD3            TYPE  LFA1-STCD3,  "Tax number3
          HOUSE_NO(20)     TYPE C,  "Street/House number
          SOI(30)          TYPE C, "Soi
          STREET(30)       TYPE C, "Street
          SUB_DISTRICT(30) TYPE C, "Sub District
          DISTRICT(30)     TYPE C,  "District
          CITY1            TYPE ADRC-CITY1,  "City
          POST_CODE1       TYPE ADRC-POST_CODE1, "Post code
          TAX_RATE         TYPE I,  "Tax Rate
          TAX_TYPE(200)    TYPE C,  "Tax Type
          CHECK            TYPE C,
        END OF TY_RESULT.

TYPES: BEGIN OF TY_PND3,
         NO(5)             TYPE C,
         TAX_NUMBER(13)    TYPE C,
         BRANCH_NO(5)      TYPE C,
         TITLE_NAME(40)    TYPE C,
         NAME_TH(100)      TYPE C,
         SURNAME_TH(80)    TYPE C,
         MOO_BAN(30)       TYPE C,
         ROOM_NUMBER(10)   TYPE C,
         FLOOR(3)          TYPE C,
         HOUSE_NUMBER(20)  TYPE C,
         MOO(2)            TYPE C,
         SOI(30)           TYPE C,
         STREET(30)        TYPE C,
         SUBDISTRICT(30)   TYPE C,
         DISTRICT(30)      TYPE C,
         CITY(40)          TYPE C,
         POSTCODE(5)       TYPE C,
         PAY_DATE(8)       TYPE C,
         MONEY_TYPE(200)   TYPE C,
         TAX_RATE(6)       TYPE C,
         AMOUNT(17)        TYPE C,
         TAX_AMT(17)       TYPE C,
         TAX_CON(1)        TYPE C,
         PAY_DATE_2(8)     TYPE C,
         MONEY_TYPE_2(200) TYPE C,
         TAX_RATE_2(6)     TYPE C,
         AMOUNT_2(17)      TYPE C,
         TAX_AMT_2(17)     TYPE C,
         TAX_CON_2(1)      TYPE C,
         PAY_DATE_3(8)     TYPE C,
         MONEY_TYPE_3(200) TYPE C,
         TAX_RATE_3(6)     TYPE C,
         AMOUNT_3(17)      TYPE C,
         TAX_AMT_3(17)     TYPE C,
         TAX_CON_3(1)      TYPE C,

       END OF TY_PND3.
TYPES: BEGIN OF TY_PND53,
         NO(5)             TYPE C,
         TAX_NUMBER(13)    TYPE C,
         BRANCH_NO(5)      TYPE C,
         TITLE_NAME(40)    TYPE C,
         NAME_TH(160)      TYPE C,
         MOO_BAN(30)       TYPE C,
         ROOM_NUMBER(10)   TYPE C,
         FLOOR(3)          TYPE C,
         HOUSE_NUMBER(20)  TYPE C,
         MOO(2)            TYPE C,
         SOI(30)           TYPE C,
         STREET(30)        TYPE C,
         SUBDISTRICT(30)   TYPE C,
         DISTRICT(30)      TYPE C,
         CITY(40)          TYPE C,
         POSTCODE(5)       TYPE C,
         PAY_DATE(8)       TYPE C,
         MONEY_TYPE(200)   TYPE C,
         TAX_RATE(6)       TYPE C,
         AMOUNT(17)        TYPE C,
         TAX_AMT(17)       TYPE C,
         TAX_CON(1)        TYPE C,
         PAY_DATE_2(8)     TYPE C,
         MONEY_TYPE_2(200) TYPE C,
         TAX_RATE_2(6)     TYPE C,
         AMOUNT_2(17)      TYPE C,
         TAX_AMT_2(17)     TYPE C,
         TAX_CON_2(1)      TYPE C,
         PAY_DATE_3(8)     TYPE C,
         MONEY_TYPE_3(200) TYPE C,
         TAX_RATE_3(6)     TYPE C,
         AMOUNT_3(17)      TYPE C,
         TAX_AMT_3(17)     TYPE C,
         TAX_CON_3(1)      TYPE C,

       END OF TY_PND53.

TYPES: BEGIN OF TY_PND1,
         MONEY_TYPE(2)   TYPE C,
         NO(7)           TYPE C,
         TAX_NUMBER(13)  TYPE C,
         TITLE_NAME(100) TYPE C,
         NAME_TH(100)    TYPE C,
         SURNAME_TH(80)  TYPE C,
         PAY_DATE(8)     TYPE C,
         AMOUNT(17)      TYPE C,
         TAX_AMT(17)     TYPE C,
         TAX_CON(1)      TYPE C,
       END OF TY_PND1.
TYPES: BEGIN OF TY_PND2,
         MONEY_TYPE(2)  TYPE C,
         NO(7)          TYPE C,
         TAX_NUMBER(13) TYPE C,
         TITLE_NAME(40) TYPE C,
         NAME_TH(100)   TYPE C,
         SURNAME_TH(80) TYPE C,
         BOOK_BANK(15)  TYPE C,
         PAY_DATE(8)    TYPE C,
         TAX_RATE(6)    TYPE C,
         AMOUNT(17)     TYPE C,
         TAX_AMT(17)    TYPE C,
         TAX_CON(1)     TYPE C,
       END OF TY_PND2.

TYPES: BEGIN OF TY_SUM_PND3,
         QSREC            TYPE  WT_QSREC,    "Type of recipient
         STCD3            TYPE  LFA1-STCD3,  "Tax number3
         TAX_RATE(2)      TYPE C,
         TAX_TYPE(200)    TYPE C,  "Tax Type
         AUGDT            TYPE  AUGDT,      "Clearing Date
         NAME(200)        TYPE  C,      "Vendor/customer name
         HOUSE_NO(20)     TYPE C,  "Street/House number
         SOI(30)          TYPE C, "Soi
         STREET(30)       TYPE C, "Street
         SUB_DISTRICT(30) TYPE C, "Sub District
         DISTRICT(30)     TYPE C,  "District
         CITY1            TYPE ADRC-CITY1,  "City
         POST_CODE1       TYPE ADRC-POST_CODE1, "Post code
         WT_QSSHH         TYPE  WT_BS,      "Withholding tax base amount (local currency)
         WT_QBSHH         TYPE  WT_WT,      "Withholding tax amount (in local currency)
       END OF TY_SUM_PND3.
TYPES: BEGIN OF TYP_SUM_PND1_2,
         MONEY_TYPE(2) TYPE C,
         QSREC         TYPE  WT_QSREC,   "Type of recipient
         STCD3         TYPE  LFA1-STCD3,  "Tax number3
         TAX_RATE(2)   TYPE C,
         TAX_TYPE(200) TYPE C,  "Tax Type
         AUGDT         TYPE  AUGDT,      "Clearing Date
         NAME(200)     TYPE  C,      "Vendor/customer name
         WT_QSSHH      TYPE  WT_BS,      "Withholding tax base amount (local currency)
         WT_QBSHH      TYPE  WT_WT,      "Withholding tax amount (in local currency)
       END OF TYP_SUM_PND1_2.
TYPES: BEGIN OF TY_DFILE,
         TEXT TYPE TEXT1000,
       END OF TY_DFILE.

TYPES: BEGIN OF TY_ADRC,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,
         NAME1      TYPE ADRC-NAME1,
         NAME2      TYPE ADRC-NAME2,
         NAME3      TYPE ADRC-NAME3,
         NAME4      TYPE ADRC-NAME4,
         STREET     TYPE ADRC-STREET,
         STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
         STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
         LOCATION   TYPE ADRC-LOCATION,
         CITY2      TYPE ADRC-CITY2,
         CITY1      TYPE ADRC-CITY1,
         POST_CODE1 TYPE ADRC-POST_CODE1,
         NATION     TYPE ADRC-NATION,
       END OF TY_ADRC.
TYPES: BEGIN OF TY_LFBW,
         LIFNR TYPE LFBW-LIFNR,
         BUKRS TYPE LFBW-BUKRS,
         WITHT TYPE LFBW-WITHT,
         QSREC TYPE LFBW-QSREC,
       END OF TY_LFBW.
"Add CH03
TYPES: BEGIN OF TY_KNBW,
         KUNNR     TYPE KNBW-KUNNR,
         BUKRS     TYPE KNBW-BUKRS,
         WITHT     TYPE KNBW-WITHT,
         WT_WTSTCD TYPE KNBW-WT_WTSTCD,

       END OF TY_KNBW.
"END CH03
TYPES: BEGIN OF TY_COL_STRUC,
         COL_POS   LIKE SY-CUCOL,
         FIELDNAME TYPE SLIS_FIELDNAME,
         TABNAME   TYPE SLIS_TABNAME,
       END OF TY_COL_STRUC.
TYPES: TY_COL_QUEUE TYPE TY_COL_STRUC OCCURS 1.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF TY_RESULT,
       GS_RESULT TYPE TY_RESULT.

DATA: GT_WITH_ITEM  TYPE STANDARD TABLE OF WITH_ITEM,
      GT_LFA1       TYPE STANDARD TABLE OF LFA1,
      GT_KNA1       TYPE STANDARD TABLE OF KNA1,
      GT_BSEC       TYPE STANDARD TABLE OF BSEC,
      GT_T059ZT     TYPE STANDARD TABLE OF T059ZT,
      GT_RESULT2    TYPE STANDARD TABLE OF TY_RESULT,
      GT_DFILE      TYPE STANDARD TABLE OF TY_DFILE,
      GT_ADRC       TYPE STANDARD TABLE OF TY_ADRC,
      GT_SUM_PND3   TYPE STANDARD TABLE OF TY_SUM_PND3,
      GT_SUM_PND53  TYPE STANDARD TABLE OF TY_SUM_PND3,
      GT_SUM_PND1_2 TYPE STANDARD TABLE OF TYP_SUM_PND1_2,
      GT_PND1       TYPE STANDARD TABLE OF TY_PND1,
      GT_PND2       TYPE STANDARD TABLE OF TY_PND2,
      GT_LFBW       TYPE STANDARD TABLE OF TY_LFBW,
      GT_KNBW       TYPE STANDARD TABLE OF TY_KNBW, " Add CH03

      GS_WITH_ITEM  TYPE WITH_ITEM,
      GS_WITH_ITMK  TYPE WITH_ITEM,
      GS_WITH_ITMD  TYPE WITH_ITEM,
      GS_LFA1       TYPE LFA1,
      GS_KNA1       TYPE KNA1,
      GS_BSEC       TYPE BSEC,
      GS_T059ZT     TYPE T059ZT,
      GS_DFILE      TYPE TY_DFILE,
      GS_ADRC       TYPE TY_ADRC,
      GS_SUM_PND3   TYPE TY_SUM_PND3,
      GS_SUM_PND53  TYPE TY_SUM_PND3,
      GS_PND3       TYPE TY_PND3,
      GS_PND53      TYPE TY_PND53,
      GS_SUM_PND1_2 TYPE TYP_SUM_PND1_2,
      GS_PND1       TYPE TY_PND1,
      GS_PND2       TYPE TY_PND2,
      GS_LFBW       TYPE TY_LFBW,
      GS_KNBW       TYPE TY_KNBW, " Add CH03

      GV_NAME(100)  TYPE C,
      GV_NAME1      TYPE NAME1,
      GV_NAME2      TYPE NAME1,
      GV_NAME3      TYPE NAME1,
      GV_NAME4      TYPE NAME1,
      GV_J_1KFTBUS  TYPE BSEC-J_1KFTBUS.  "Add by Wantanee 20150831

DATA: WA_ADRC           TYPE TY_ADRC.

DATA:
  GD_REPID    LIKE SY-REPID,
  GD_SAVE(1)  TYPE C,
  GD_EXIT(1)  TYPE C,
  GS_XVARIANT LIKE DISVARIANT,
  GS_VARIANT  LIKE DISVARIANT.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.
DATA: GS_COL_QUEUE TYPE TY_COL_STRUC.
DATA: GT_COL_QUEUE LIKE TABLE OF GS_COL_QUEUE.
DATA: GS_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: GV_FILTER_AUART TYPE C.
DATA: ALV_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      ALV_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      ALV_EVENTS   TYPE SLIS_T_EVENT,
      ALV_SORT     TYPE SLIS_T_SORTINFO_ALV.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

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
