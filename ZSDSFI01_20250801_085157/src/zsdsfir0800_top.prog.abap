*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0800_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : E070.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          TRKORR  TYPE E070-TRKORR,
          AS4USER TYPE E070-AS4USER,
          AS4TEXT TYPE E07T-AS4TEXT,
          CHECK   TYPE C,
        END OF GY_RESULT.

TYPES: BEGIN OF GY_HEADER,
         DOC_NUMBER    TYPE VBRK-VBELN,
         DATE          TYPE VBRK-FKDAT,
         LANGUAGE      TYPE CHAR2,
         CUSTOMER      TYPE CHAR80,
         ADRESS1       TYPE CHAR255,
         ADRESS2       TYPE CHAR255,
         ADRESS3       TYPE CHAR255,
         ADRESS4       TYPE CHAR255,
         INVOICE_TAX   TYPE CHAR255,
         INVOICE_DATE  TYPE CHAR255,
         CUST_CODE     TYPE CHAR255,
         REMARK1       TYPE CHAR255,
         REMARK2       TYPE CHAR255,
         REMARK3       TYPE CHAR255,
         REMARK4       TYPE CHAR255,
         TOTAL_TEXT    TYPE CHAR255,
         PRICE_ORG_INV TYPE P DECIMALS 2,
         ADV_REC       TYPE P DECIMALS 2,
         TOTAL_AMT     TYPE P DECIMALS 2,
         PRICE_DUC     TYPE P DECIMALS 2,
         VAL_ADDTAX    TYPE P DECIMALS 2,
         NET_TOTAL     TYPE P DECIMALS 2,
         DOC_NAME_TH   TYPE CHAR50,
         DOC_NAME_EN   TYPE CHAR50,
         HOLDING_TAX   TYPE CHAR255,
         FI_DOC        TYPE CHAR50,
         CUSTOMER2     TYPE CHAR80,

       END OF GY_HEADER.

TYPES: BEGIN OF GY_DETAIL,
         ITEM       TYPE CHAR255,
         MODEL      TYPE CHAR255,
         DES        TYPE CHAR255,
         QTY        TYPE VBRP-FKLMG,
         UNIT_PRICE TYPE P DECIMALS 2,
         DISCOUNT   TYPE P DECIMALS 2,
         NET_UNIT   TYPE P DECIMALS 2,
         NET_AMOUNT TYPE VBRP-NETWR,
         VAT_TMP    TYPE P DECIMALS 2,
         CHECK      TYPE C,
       END OF GY_DETAIL.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GS_HEADER TYPE GY_HEADER.

DATA : gt_DETAIL TYPE TABLE OF GY_DETAIL,
       gs_DETAIL TYPE GY_DETAIL.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

CONTROLS: TC_DETAIL TYPE TABLEVIEW USING SCREEN 101.
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
