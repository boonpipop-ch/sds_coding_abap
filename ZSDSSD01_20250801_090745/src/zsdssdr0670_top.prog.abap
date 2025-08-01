*&---------------------------------------------------------------------*
*& INCLUDE          ZSDSSDR0670_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : VBRK,VBap.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          SONUM TYPE VBAP-VBELN,
          FPLNR TYPE FPLA-FPLNR,
          BILNO TYPE VBRP-VBELN,
          DONUM TYPE LIPS-VBELN,
          POSNR TYPE VBAP-POSNR,
          MATWA TYPE VBAP-MATWA,
          ARKTX TYPE VBAP-ARKTX,
          NETWR TYPE VBAP-NETWR,
          PRCTR TYPE VBRP-PRCTR,
          VKGRP TYPE VBRP-VKGRP,
          VKBUR TYPE VBRP-VKBUR,
          F_WBS TYPE VBRP-PS_PSP_PNR,
          PRODH TYPE VBRP-PRODH,
          AUFNR TYPE VBRP-AUFNR,
          VBTYP TYPE VBRK-VBTYP,
          FKART TYPE VBRK-FKART,
          FKDAT TYPE VBRK-FKDAT,
          DESGP TYPE TVGRT-BEZEI,
          DESOF TYPE TVKBT-BEZEI,
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
           GC_C  TYPE C LENGTH 1 VALUE 'C',
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
