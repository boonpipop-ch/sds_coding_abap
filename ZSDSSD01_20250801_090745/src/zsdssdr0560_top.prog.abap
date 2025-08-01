*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0560_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : VBAK,VBAP.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          VBELN         TYPE VBEP-VBELN,
          POSNR         TYPE VBEP-POSNR,
          ETENR         TYPE VBEP-ETENR,
          EDATU         TYPE VBEP-EDATU,
          WMENG         TYPE VBEP-WMENG,
          BMENG         TYPE VBEP-BMENG,
          MATNR         TYPE VBAP-MATNR,
          GBSTA         TYPE VBAP-GBSTA,
          KUNNR         TYPE VBAK-KUNNR,
          PARVW         TYPE VBPA-PARVW,
          PERNR         TYPE VBPA-PERNR,
          NACHN         TYPE PA0002-NACHN,
          VORNA         TYPE PA0002-VORNA,
          NAME          TYPE  CHAR255,
          QUOTA_QTY     TYPE P DECIMALS 0,
          REMAIN        TYPE P DECIMALS 0,
          VKGRP         TYPE VBAK-VKGRP,
          QUOTAGRP      TYPE ZSDSSDC013-QUOTAGRP,
          QUOTAGRP_DESC TYPE ZSDSSDC013-QUOTAGRP_DESC,
          USR02         TYPE ZSDSSDC031-USR02,
          LINE_COLOR    TYPE C LENGTH 4,
          CHECK         TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_QUOTA_MAT,
          QUOTAGRP    TYPE ZSDSSDT018-QUOTAGRP,
          MATNR       TYPE ZSDSSDT018-MATNR,
          PORTION_QTY TYPE ZSDSSDT018-PORTION_QTY,
        END OF GY_QUOTA_MAT.
TYPES : GTY_QUOTA_MAT TYPE TABLE OF GY_QUOTA_MAT WITH EMPTY KEY.

TYPES : BEGIN OF GY_MAT,
          VKGRP TYPE ZSDSSDC013-VKGRP,
          MATNR TYPE ZSDSSDT018-MATNR,
        END OF GY_MAT.
TYPES : GTY_MAT TYPE HASHED TABLE OF GY_MAT WITH UNIQUE KEY VKGRP
                                                            MATNR.


*TYPES : BEGIN OF GY_SUM_DATA,
*         MATNR TYPE MARA-MATNR,
*         BMENG TYPE VBEP-BMENG,
*       END OF GY_SUM_DATA.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_RESULT_SUM TYPE TABLE OF GY_RESULT,
       GS_RESULT_SUM TYPE GY_RESULT.

*DATA : GT_SUM_DATA TYPE TABLE OF GY_SUM_DATA,
*       GS_SUM_DATA TYPE GY_SUM_DATA.

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
