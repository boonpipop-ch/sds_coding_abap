*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0390_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : EBAN,ZSDSMMT006,SOMLRECI1.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          BANFN      TYPE EBAN-BANFN,
          BNFPO      TYPE EBAN-BNFPO,
          ERDAT      TYPE EBAN-ERDAT,
          ERNAM      TYPE EBAN-ERNAM,
          MATNR      TYPE EBAN-MATNR,
          TXZ01      TYPE EBAN-TXZ01,
          MENGE      TYPE EBAN-MENGE,
          MEINS      TYPE EBAN-MEINS,
          PREIS      TYPE EBAN-PREIS,
          WAERS      TYPE EBAN-WAERS,
          LFDAT      TYPE EBAN-LFDAT,
          SAKTO      TYPE EBKN-SAKTO,
          KOSTL      TYPE EBKN-KOSTL,
          AUFNR      TYPE EBKN-AUFNR,
          PS_PSP_PNR TYPE EBKN-PS_PSP_PNR,
          MSEHL      TYPE MSEHL,
          CHECK      TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_ATTACH,
          FILE_SEQ           TYPE STRING,
          PR_REQUEST_DOCID   TYPE STRING,
          FILE_TYPE_ID       TYPE STRING,
          FILE_NAME_ORIGINAL TYPE STRING,
          CONTENT_TYPE       TYPE STRING,
          FILE_NAME          TYPE STRING,
          FILE_PATH          TYPE STRING,
          IS_ACTIVE          TYPE STRING,
          CREATE_DATE        TYPE STRING,
          CREATE_BY          TYPE STRING,
          CREATE_BY_NAME     TYPE STRING,
          UPDATE_DATE        TYPE STRING,
          UPDATE_BY          TYPE STRING,
          UPDATE_BY_NAME     TYPE STRING,
        END OF GY_ATTACH.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_ATTACH TYPE TABLE OF GY_ATTACH,
       GS_ATTACH TYPE GY_ATTACH.

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

CONSTANTS: GC_FORM_NAME TYPE TNAPR-SFORM VALUE 'ZSDSMM007'.

CONSTANTS: BEGIN OF GC_CON,
             GET TYPE C LENGTH 3 VALUE 'GET',
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
