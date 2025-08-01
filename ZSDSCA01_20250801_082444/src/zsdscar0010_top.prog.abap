*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0010_TOP
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

TYPES: BEGIN OF GY_GEN_C,
         REPID        TYPE  ZSDSCAC001-REPID,
         PARAM        TYPE  ZSDSCAC001-PARAM,
         PARAM_EXT    TYPE  ZSDSCAC001-PARAM_EXT,
         SEQUENCE     TYPE  ZSDSCAC001-SEQUENCE,
         PARAM_SIGN   TYPE  ZSDSCAC001-PARAM_SIGN,
         PARAM_OPTION TYPE  ZSDSCAC001-PARAM_OPTION,
         VALUE_LOW    TYPE  ZSDSCAC001-VALUE_LOW,
         VALUE_HIGH   TYPE  ZSDSCAC001-VALUE_HIGH,
         VDESC        TYPE  ZSDSCAC001-VDESC,
       END OF GY_GEN_C .
TYPES: GYT_GEN_C  TYPE STANDARD TABLE OF GY_GEN_C.

TYPES: BEGIN OF GY_K2_RES,
         K2Url   TYPE STRING,
         Status  TYPE STRING,
         Message TYPE STRING,
       END OF GY_K2_RES.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_GEN_C TYPE GYT_GEN_C,
       GS_GEN_C TYPE GY_GEN_C.

DATA : GS_K2_RES TYPE GY_K2_RES.

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

CONSTANTS : BEGIN OF GC_CON,
              USER   TYPE C LENGTH 4  VALUE 'USER',
              PASS   TYPE C LENGTH 4  VALUE 'PASS',
              DBQ    TYPE C LENGTH 1  VALUE '"',
              SGQ    TYPE C LENGTH 1  VALUE '''',
              CMA    TYPE C LENGTH 1  VALUE ',',
              COB    TYPE C LENGTH 1  VALUE '{',
              CCB    TYPE C LENGTH 1  VALUE '}',
              SOB    TYPE C LENGTH 1  VALUE '[',
              SCB    TYPE C LENGTH 1  VALUE ']',
              FOLIO  TYPE C LENGTH 8  VALUE '"folio":',
              EXPDT  TYPE C LENGTH 26 VALUE '"expectedDuration": 86400,',
              PRITY  TYPE C LENGTH 15 VALUE '"priority": 1, ',
              DATFD  TYPE C LENGTH 13 VALUE '"dataFields":',
              PARAM  TYPE C LENGTH 4  VALUE 'PARA',
              PARA1  TYPE C LENGTH 5  VALUE 'PARA1',
              PARA2  TYPE C LENGTH 5  VALUE 'PARA2',
              PARA3  TYPE C LENGTH 5  VALUE 'PARA3',
              PARA4  TYPE C LENGTH 5  VALUE 'PARA4',
              PARA5  TYPE C LENGTH 5  VALUE 'PARA5',
              PARA6  TYPE C LENGTH 5  VALUE 'PARA6',
              PARA7  TYPE C LENGTH 5  VALUE 'PARA7',
              PARA8  TYPE C LENGTH 5  VALUE 'PARA8',
              PARA9  TYPE C LENGTH 5  VALUE 'PARA9',
              PARA10 TYPE C LENGTH 6  VALUE 'PARA10',
              PARA11 TYPE C LENGTH 6  VALUE 'PARA11',
              PARA12 TYPE C LENGTH 6  VALUE 'PARA12',
              PARA13 TYPE C LENGTH 6  VALUE 'PARA13',
              PARA14 TYPE C LENGTH 6  VALUE 'PARA14',
              PARA15 TYPE C LENGTH 6  VALUE 'PARA15',
              PARA16 TYPE C LENGTH 6  VALUE 'PARA16',
              PARA17 TYPE C LENGTH 6  VALUE 'PARA17',
              PARA18 TYPE C LENGTH 6  VALUE 'PARA18',
              PARA19 TYPE C LENGTH 6  VALUE 'PARA19',
              PARA20 TYPE C LENGTH 6  VALUE 'PARA20',
              END    TYPE C LENGTH 3  VALUE 'END',
              URL    TYPE C LENGTH 3  VALUE 'URL',
              METHOD TYPE C LENGTH 6  VALUE 'METHOD',
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
