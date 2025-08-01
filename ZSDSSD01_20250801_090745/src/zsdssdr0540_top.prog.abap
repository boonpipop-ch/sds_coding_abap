*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0540_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : VBAK,VBRK.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,

          VBELN     TYPE VBAK-VBELN,
          KUNNR     TYPE VBAK-KUNNR,
          VKGRP     TYPE VBAK-VKGRP,
          VKBUR     TYPE VBAK-VKBUR,
          ERDAT     TYPE VBAK-ERDAT,
          ERNAM     TYPE VBAK-ERNAM,
          PSP_PNR   TYPE VBAK-PS_PSP_PNR,
          AEDAT     TYPE VBAK-AEDAT,
          CHANGE_BY TYPE VBAK-LAST_CHANGED_BY_USER,
*          MATNR     TYPE VBAP-MATNR,
*          ARKTX     TYPE VBAP-ARKTX,
*          POSNR     TYPE VBAP-POSNR,
*          VRKME     TYPE VBAP-VRKME,
*          WAERK     TYPE VBAP-WAERK,
*          NETPR     TYPE VBAP-NETPR,
          STATU     TYPE ZSDSSDT022-STATU,
          REMARK_FI TYPE ZSDSSDT022-REMARK_FI,
          REMARK_SD TYPE ZSDSSDT022-REMARK_SD,
          FPLNR     TYPE FPLA-FPLNR,
          FPLTR     TYPE FPLT-FPLTR,
          FAKWR     TYPE FPLT-FAKWR,
          FAKSP     TYPE FPLT-FAKSP,
          NETWR     TYPE VBAK-NETWR,
          FKDAT     TYPE FPLT-FKDAT,
          FPART     TYPE FPLA-FPART,
          STATU_S   TYPE ZSDSSDT022-STATU,
          NAME1     TYPE KNA1-NAME1,
          BEZEI_OF  TYPE TVKBT-BEZEI,
          BEZEI_GP  TYPE TVGRT-BEZEI,
          AUART     TYPE VBAK-AUART,


          CHECK     TYPE C,
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
