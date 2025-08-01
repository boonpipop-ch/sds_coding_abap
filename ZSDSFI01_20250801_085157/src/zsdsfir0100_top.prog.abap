*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0100_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : ACDOCA,BKPF.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          RLDNR  TYPE ACDOCA-RLDNR,
          RBUKRS TYPE ACDOCA-RBUKRS,
          DOCLN  TYPE ACDOCA-DOCLN,
          BELNR  TYPE ACDOCA-BELNR,
          GJAHR  TYPE ACDOCA-GJAHR,
          BUDAT  TYPE ACDOCA-BUDAT,
          BLDAT  TYPE ACDOCA-BLDAT,
          MWSKZ  TYPE ACDOCA-MWSKZ, "TAX TYPE
          AUGBL  TYPE ACDOCA-AUGBL,
          AUGGJ  TYPE ACDOCA-AUGGJ,
          TSL    TYPE ACDOCA-TSL,
          RTCUR  TYPE ACDOCA-RTCUR,
          KUNNR  TYPE ACDOCA-KUNNR,
          NETDT  TYPE ACDOCA-NETDT,
          NAME1  TYPE C LENGTH 70,
          ADRNR  TYPE KNA1-ADRNR,
          BUPLA  TYPE BSEG-BUPLA,
          CHECK  TYPE C,
        END OF GY_RESULT.

TYPES: BEGIN OF GY_DETAIL,
         BELNR    TYPE ACDOCA-BELNR,
         GJAHR    TYPE ACDOCA-GJAHR,
         MWSKZ    TYPE ACDOCA-MWSKZ, "TAX TYPE
         AUGBL    TYPE ACDOCA-AUGBL,
         AUGGJ    TYPE ACDOCA-AUGGJ,
         TSL      TYPE ACDOCA-TSL,
         RTCUR    TYPE ACDOCA-RTCUR,
         SGTXT    TYPE ACDOCA-SGTXT,
         LINETYPE TYPE ACDOCA-LINETYPE, "05100 TAX
       END OF GY_DETAIL.

TYPES: GTY_DETAIL TYPE STANDARD TABLE OF GY_DETAIL WITH EMPTY KEY.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT WITH EMPTY KEY, " WITH KEY RCLNT RLDNR RBUKRS DOCLN BELNR GJAHR,
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

CONSTANTS: GC_FORM_NAME TYPE TNAPR-SFORM VALUE 'ZSDSFI003'.

CONSTANTS : BEGIN OF GC_CON,
              REPID TYPE C LENGTH 12 VALUE 'RECEIPT_PATH',
              PARAM TYPE C LENGTH 14 VALUE 'GET_CONFIG_001',
              0L    TYPE C LENGTH 2  VALUE '0L',
              D     TYPE C LENGTH 1  VALUE 'D',
              S     TYPE C LENGTH 1  VALUE 'S',
              AG    TYPE C LENGTH 2  VALUE 'AG',
              RE    TYPE C LENGTH 2  VALUE 'RE',
              05100 TYPE C LENGTH 5  VALUE '05100',
              ZZZZ  TYPE C LENGTH 4  VALUE 'ZZZZ',
              H     TYPE C LENGTH 1  VALUE 'H',
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
