*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0350_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : E070,SOMLRECI1.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          TRKORR  TYPE E070-TRKORR,
          AS4USER TYPE E070-AS4USER,
          AS4TEXT TYPE E07T-AS4TEXT,
          CHECK   TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_SERIAL,
          DELIVERY TYPE C LENGTH 15,
          ITEM     TYPE C LENGTH 4,
          SERIAL   TYPE C LENGTH 18,
          MATNR    TYPE LIPS-MATNR,
          LFART    TYPE LIKP-LFART,
          FLAG     TYPE C LENGTH 1,
          PICK     TYPE C LENGTH 1,
          BLOCK    TYPE C LENGTH 1,
*   flag     TYPE c,
        END OF GY_SERIAL.
TYPES : GTY_SERIAL TYPE TABLE OF GY_SERIAL WITH EMPTY KEY.

*TYPES : BEGIN OF GY_CHK,
*        DELIVERY TYPE C LENGTH 10,
*        ITEM     TYPE C LENGTH 6,
*        SERIAL   TYPE C LENGTH 18,
*        LFART    TYPE C LENGTH 4,
*        MATNR    TYPE C LENGTH 18,
*        FLAG     TYPE C LENGTH 1,
*        PICK     TYPE C LENGTH 1,
*        BLOCK    TYPE C LENGTH 1,
*      END OF GY_CHK.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_SERIAL TYPE TABLE OF GY_SERIAL WITH EMPTY KEY,
       GS_SERIAL TYPE GY_SERIAL.

DATA : GV_FLAG TYPE CHAR1,
       GV_TAXT TYPE C LENGTH 255.

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
           GC_L  TYPE C LENGTH 1 VALUE 'L',
           GC_F  TYPE C LENGTH 1 VALUE 'f',
           GC_P  TYPE C LENGTH 1 VALUE 'p',
           GC_B  TYPE C LENGTH 1 VALUE 'b',
           GC_C  TYPE C LENGTH 1 VALUE 'C'.

CONSTANTS: GC_VL02N TYPE C LENGTH 5 VALUE 'VL02N'.
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
