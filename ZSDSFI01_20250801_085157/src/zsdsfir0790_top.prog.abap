*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0790_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : E070.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          BUKRS   TYPE BSEG-BUKRS,
          BELNR   TYPE BSEG-BELNR,
          GJAHR   TYPE BSEG-GJAHR,
          BUZEI   TYPE BSEG-BUZEI,
          SHKZG   TYPE BSEG-SHKZG,
          DMBTR   TYPE BSEG-DMBTR,
          AUGBL   TYPE BSEG-AUGBL,
          SGTXT   TYPE BSEG-SGTXT,
          HKONT   TYPE BSEG-HKONT,
          ZUONR   TYPE BSEG-ZUONR,
          PRCTR   TYPE BSEG-PRCTR,
          STATUS  TYPE ICON-ID,
          MESSAGE TYPE STRING,
          CHECK   TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_HEADER,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          WAERS TYPE BKPF-WAERS,
          BLART TYPE BKPF-BLART,
          XBLNR TYPE BKPF-XBLNR,
          BUDAT TYPE BKPF-BUDAT,
          BLDAT TYPE BKPF-BLDAT,
          USNAM TYPE BKPF-USNAM,
          CPUDT TYPE BKPF-CPUDT,
        END OF GY_HEADER.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_HEADER TYPE TABLE OF GY_HEADER,
       GS_HEADER TYPE GY_HEADER.

DATA : GV_TMP_FILE_PATH LIKE IBIPPARMS-PATH.

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
