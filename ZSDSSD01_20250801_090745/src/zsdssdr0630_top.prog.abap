*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0630_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : LIKP,VBAK,KNA1,ZSDSSDT025.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          VBELN   TYPE LIKP-VBELN,
          KUNNR   TYPE LIKP-KUNNR,
          WBSTK   TYPE LIKP-WBSTK,
          ADRNR   TYPE KNA1-ADRNR,
          SMARD   TYPE ZSDSSDT025-SMART_TRACKING_DATE,
          KVGR5   TYPE VBAK-KVGR5,
          INVNO   TYPE VBRK-VBELN,
          MHA     TYPE CHAR1,
          STATUS  TYPE ICON-ID,
          MESSAGE TYPE STRING,
          CHECK   TYPE C,
        END OF GY_RESULT.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GV_FORM TYPE TNAPR-SFORM.

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
           GC_N  TYPE C LENGTH 1 VALUE 'N',
           GC_C  TYPE C LENGTH 1 VALUE 'C',
           GC_L  TYPE C LENGTH 1 VALUE 'L'.

CONSTANTS : GC_SUCS TYPE C LENGTH 4 VALUE '@08@',
            GC_ERRO TYPE C LENGTH 4 VALUE '@0A@',
            GC_WARN TYPE C LENGTH 4 VALUE '@09@'.


CONSTANTS : BEGIN OF GC_CON,
              UPS  TYPE C LENGTH 3 VALUE 'UPS',
              S    TYPE C LENGTH 1 VALUE 'S',
              E    TYPE C LENGTH 1 VALUE 'E',
              POST TYPE C LENGTH 4 VALUE 'POST',
            END OF GC_CON.
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
