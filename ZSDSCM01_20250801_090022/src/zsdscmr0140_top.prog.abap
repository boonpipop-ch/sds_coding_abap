*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0140_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : CRMS4D_SERV_H,CRMS4D_SERV_I.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          OBJTYPE_H         TYPE CRMS4D_SERV_I-OBJTYPE_H,
          OBJECT_ID         TYPE CRMS4D_SERV_I-OBJECT_ID,
          NUMBER_INT        TYPE CRMS4D_SERV_I-NUMBER_INT,
          STAT_H            TYPE CRMS4D_SERV_H-STAT_LIFECYCLE,
          STAT_I            TYPE CRMS4D_SERV_I-STAT_LIFECYCLE,
          ORDERED_PROD      TYPE CRMS4D_SERV_I-ORDERED_PROD,
          DESCRIPTION_I     TYPE CRMS4D_SERV_I-DESCRIPTION_I,
          BILLED_VALUE      TYPE CRMS4D_SERV_I-BILLED_VALUE,
          BILLED_QUANTITY   TYPE CRMS4D_SERV_I-BILLED_QUANTITY,
          DIS_CHANNEL       TYPE CRMS4D_SERV_I-DIS_CHANNEL,
          DIVISION          TYPE CRMS4D_SERV_I-DIVISION,
          SALES_OFFICE_SD   TYPE CRMS4D_SERV_I-SALES_OFFICE_SD,
          SALES_GROUP_SD    TYPE CRMS4D_SERV_I-SALES_GROUP_SD,
          SOLD_TO_PARTY     TYPE CRMS4D_SERV_I-SOLD_TO_PARTY,
          PMNTTRMS          TYPE CRMS4D_SERV_I-PMNTTRMS,
          BASE_QTY_UNIT     TYPE CRMS4D_SERV_I-BASE_QTY_UNIT,
          PROCESS_TYPE      TYPE CRMS4D_SERV_I-PROCESS_TYPE,
          CREATED_AT_H      TYPE CRMS4D_SERV_I-CREATED_AT_H,
          CREATED_BY_H      TYPE CRMS4D_SERV_I-CREATED_BY_H,
          CHANGED_AT_H      TYPE CRMS4D_SERV_I-CHANGED_AT_H,
          CHANGED_BY_H      TYPE CRMS4D_SERV_I-CHANGED_BY_H,
          ZZ1_LOB_SRI       TYPE CRMS4D_SERV_I-ZZ1_LOB_SRI,
          ZZ1_LGORT         TYPE CRMS4D_SERV_I-ZZ1_LGORT,
          SHIP_TO_PARTY     TYPE CRMS4D_SERV_H-SHIP_TO_PARTY,
          BILL_TO_PARTY     TYPE CRMS4D_SERV_H-BILL_TO_PARTY,
          PAYER             TYPE CRMS4D_SERV_H-PAYER,
          PERSON_RESP       TYPE CRMS4D_SERV_H-PERSON_RESP,
          PRICING_PROCEDURE TYPE CRMS4D_SERV_H-PRICING_PROCEDURE,
          INCOTERMS1        TYPE CRMS4D_SERV_H-INCOTERMS1,
          INCOTERMS2        TYPE CRMS4D_SERV_H-INCOTERMS2,
          PROCESS_QTY_NUM   TYPE CRMS4D_SERV_I-PROCESS_QTY_NUM,
          PROCESS_QTY_UNIT  TYPE CRMS4D_SERV_I-PROCESS_QTY_UNIT,
          ZZ1_WERKS         TYPE CRMS4D_SERV_I-ZZ1_WERKS,
          PROD_HIERARCHY    TYPE CRMS4D_SERV_I-PROD_HIERARCHY,
          AC_ASSIGNMENT     TYPE CRMS4D_SERV_I-AC_ASSIGNMENT,
          PRICING_DOCUMENT  TYPE CRMS4D_SERV_H-PRICING_DOCUMENT,
          SALES_ORG_SD      TYPE CRMS4D_SERV_H-SALES_ORG_SD,
          STATUS            TYPE ICON-ID,
          MESSAGE           TYPE STRING,
          CHECK             TYPE C,
          HEADER_STATUS     TYPE ZSDSCMT011-HEADER_STATUS,
          DETIAL_STATUS     TYPE ZSDSCMT011-DETIAL_STATUS,
          VBELN             TYPE VBRK-VBELN,
          BDR_NUM           TYPE VBRK-VBELN,
          INV_NUM           TYPE VBRK-VBELN,
        END OF GY_RESULT.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_PROFIT TYPE TABLE OF ZSDSCMC001.

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
           GC_C  TYPE C LENGTH 1 VALUE 'C',
           GC_D  TYPE C LENGTH 1 VALUE 'D',
           GC_N  TYPE C LENGTH 1 VALUE 'N',
           GC_L  TYPE C LENGTH 1 VALUE 'L'.

CONSTANTS : GC_SUCS TYPE C LENGTH 4 VALUE '@08@',
            GC_ERRO TYPE C LENGTH 4 VALUE '@0A@',
            GC_WARN TYPE C LENGTH 4 VALUE '@09@'.
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
