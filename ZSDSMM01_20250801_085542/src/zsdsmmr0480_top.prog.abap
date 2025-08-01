*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0480_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : EKKO.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,

          EBELN TYPE EKKO-EBELN,
          LIFNR TYPE EKKO-LIFNR,
          EBELP TYPE EKPO-EBELP,
          MATNR TYPE EKPO-MATNR,
          TXZ01 TYPE EKPO-TXZ01,
          MENGE TYPE EKPO-MENGE,
          MEINS TYPE EKPO-MEINS,
          WBSPO TYPE EKPO-PS_PSP_PNR,
          NETWR TYPE EKPO-NETWR,
          NAME1 TYPE LFA1-NAME1,
          EINDT TYPE EKET-EINDT,
          RUNNG TYPE ZSDSMMT002-RUNNG,
          STATU TYPE ZSDSMMT002-STATU,
          REMAK TYPE ZSDSMMT002-REMAK,
          ERNAM TYPE ZSDSMMT002-ERNAM,
          ERDAT TYPE ZSDSMMT002-ERDAT,
          ERZET TYPE ZSDSMMT002-ERZET,
          POSIT TYPE ZSDSMMT002-POSIT,
          CHECK TYPE C,
        END OF GY_RESULT.

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
