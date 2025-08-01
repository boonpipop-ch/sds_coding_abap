FUNCTION-POOL ZSDSFI23.                     "MESSAGE-ID ..
*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0710_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
*TYPES: BEGIN OF GY_K2_RES,
*         MESSAGETYPE TYPE STRING,
*         MESSAGETEXT TYPE STRING,
*       END OF GY_K2_RES.
*
TYPES:
  BEGIN OF TS_GEN_C,
    REPID        TYPE  ZSDSCAC001-REPID,
    PARAM        TYPE  ZSDSCAC001-PARAM,
    PARAM_EXT    TYPE  ZSDSCAC001-PARAM_EXT,
    SEQUENCE     TYPE  ZSDSCAC001-SEQUENCE,
    PARAM_SIGN   TYPE  ZSDSCAC001-PARAM_SIGN,
    PARAM_OPTION TYPE  ZSDSCAC001-PARAM_OPTION,
    VALUE_LOW    TYPE  ZSDSCAC001-VALUE_LOW,
    VALUE_HIGH   TYPE  ZSDSCAC001-VALUE_HIGH,
    VDESC        TYPE  ZSDSCAC001-VDESC,
  END OF TS_GEN_C .
TYPES:
  TT_GEN_C  TYPE STANDARD TABLE OF TS_GEN_C .
*
CONSTANTS : BEGIN OF GC_CON,
*              USER   TYPE C LENGTH 4  VALUE 'USER',
*              PASS   TYPE C LENGTH 4  VALUE 'PASS',
*              DBQ    TYPE C LENGTH 1  VALUE '"',
*              SGQ    TYPE C LENGTH 1  VALUE '''',
*              CMA    TYPE C LENGTH 1  VALUE ',',
*              COB    TYPE C LENGTH 1  VALUE '{',
*              CCB    TYPE C LENGTH 1  VALUE '}',
*              SOB    TYPE C LENGTH 1  VALUE '[',
*              SCB    TYPE C LENGTH 1  VALUE ']',
*              FOLIO  TYPE C LENGTH 8  VALUE '"folio":',
*              EXPDT  TYPE C LENGTH 26 VALUE '"expectedDuration": 86400,',
*              PRITY  TYPE C LENGTH 15 VALUE '"priority": 1, ',
*              DATFD  TYPE C LENGTH 13 VALUE '"dataFields":',
*              PARAM  TYPE C LENGTH 4  VALUE 'PARA',
*              PARA1  TYPE C LENGTH 5  VALUE 'PARA1',
*              PARA2  TYPE C LENGTH 5  VALUE 'PARA2',
*              PARA3  TYPE C LENGTH 5  VALUE 'PARA3',
*              PARA4  TYPE C LENGTH 5  VALUE 'PARA4',
*              PARA5  TYPE C LENGTH 5  VALUE 'PARA5',
*              PARA6  TYPE C LENGTH 5  VALUE 'PARA6',
*              PARA7  TYPE C LENGTH 5  VALUE 'PARA7',
*              PARA8  TYPE C LENGTH 5  VALUE 'PARA8',
*              PARA9  TYPE C LENGTH 5  VALUE 'PARA9',
*              PARA10 TYPE C LENGTH 6  VALUE 'PARA10',
*              PARA11 TYPE C LENGTH 6  VALUE 'PARA11',
*              PARA12 TYPE C LENGTH 6  VALUE 'PARA12',
*              PARA13 TYPE C LENGTH 6  VALUE 'PARA13',
*              PARA14 TYPE C LENGTH 6  VALUE 'PARA14',
*              PARA15 TYPE C LENGTH 6  VALUE 'PARA15',
*              PARA16 TYPE C LENGTH 6  VALUE 'PARA16',
*              PARA17 TYPE C LENGTH 6  VALUE 'PARA17',
*              PARA18 TYPE C LENGTH 6  VALUE 'PARA18',
*              PARA19 TYPE C LENGTH 6  VALUE 'PARA19',
*              PARA20 TYPE C LENGTH 6  VALUE 'PARA20',
*              END    TYPE C LENGTH 3  VALUE 'END',
*              URL    TYPE C LENGTH 3  VALUE 'URL',
*              METHOD TYPE C LENGTH 6  VALUE 'METHOD',
*
*              "Email
              REPID  TYPE ZSDSCAC001-REPID VALUE 'Z_SDSFI_JV_PROCESS',
            END OF GC_CON.
*
**-----------------------------------------------------------------------
** D A T A
**-----------------------------------------------------------------------
*DATA : GS_K2_RES TYPE GY_K2_RES.
*
*DATA : GT_GEN_C  TYPE TT_GEN_C,
*       GS_GEN_C  TYPE TS_GEN_C,
*       GS_FIT060 TYPE ZSDSFIT060,
*       GV_TEST   TYPE CHAR01,
*       GR_BLART  TYPE RANGE OF BKPF-BLART.
* INCLUDE LZSDSFI23D...                      " Local class definition
