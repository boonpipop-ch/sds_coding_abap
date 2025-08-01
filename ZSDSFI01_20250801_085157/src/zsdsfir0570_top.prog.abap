*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0570_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : VBRK.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          DOCNO TYPE VBAK-VBELN,
          KUNNR TYPE VBAK-KUNNR,
          NAMEA TYPE STRING,
          AMOUT TYPE ACDOCA-TSL,
          CURRN TYPE ACDOCA-RTCUR,
          ADRNR TYPE KNA1-ADRNR,
          STCD3 TYPE KNA1-STCD3,
          BUPLA TYPE BSEG-BUPLA,
          CREDT TYPE SY-DATUM,
          STATU TYPE ICON-ID,
          CHECK TYPE C,
          WHTAM TYPE PRCD_ELEMENTS-KWERT,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          KBETR TYPE P DECIMALS 2,
          QSREC TYPE LFBW-QSREC,
          BUDAT TYPE BKPF-BUDAT,
          MESSG TYPE STRING,
        END OF GY_RESULT.

TYPES: BEGIN OF GY_WITH_TYPE,
         PARAM_EXT TYPE ZSDSCAC001-PARAM_EXT,
         VALUE_LOW TYPE ZSDSCAC001-VALUE_LOW,
       END OF GY_WITH_TYPE.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_WITH_TYPE TYPE TABLE OF GY_WITH_TYPE,
       GS_WITH_TYPE TYPE GY_WITH_TYPE.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

CONSTANTS: BEGIN OF GC_CON,
             SUCCESS TYPE C LENGTH 4 VALUE '@08@',
             WARNING TYPE C LENGTH 4 VALUE '@09@',
             ERROR   TYPE C LENGTH 4 VALUE '@0A@',
           END OF GC_CON.

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
