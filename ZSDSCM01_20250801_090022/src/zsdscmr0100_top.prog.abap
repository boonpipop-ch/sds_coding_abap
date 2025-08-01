*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0100_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : EQUI,TJ02T.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          EQUNR       TYPE ZSDSCMT009-EQUNR,
          SERNR       TYPE ZSDSCMT009-SERNR,
          MATNR       TYPE ZSDSCMT009-MATNR,
          STD_WRT_BEG TYPE ZSDSCMT009-STD_WRT_BEG,
          STD_WRT_END TYPE ZSDSCMT009-STD_WRT_END,
          ERNAM       TYPE ZSDSCMT009-ERNAM,
          ERDAT       TYPE ZSDSCMT009-ERDAT,
          ERZET       TYPE ZSDSCMT009-ERZET,
          AENAM       TYPE ZSDSCMT009-AENAM,
          AEDAT       TYPE ZSDSCMT009-AEDAT,
          AEZET       TYPE ZSDSCMT009-AEZET,
          TXT04       TYPE TJ02T-TXT04,
          CHECK       TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_REQUEST_JSON,
          I_SERNR TYPE STRING,
          I_MATNR TYPE STRING,
        END OF GY_REQUEST_JSON.
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

CONSTANTS GC_MAX_FETCH TYPE I VALUE '10000'.

CONSTANTS:
  BEGIN OF GC_CON,
    POST TYPE C LENGTH 4 VALUE 'POST',
    GET  TYPE C LENGTH 3 VALUE 'GET',
    E    TYPE C LENGTH 1 VALUE 'E',
    S    TYPE C LENGTH 1 VALUE 'S',
  END OF GC_CON .

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
