*&---------------------------------------------------------------------*
*& Include          ZSDSSDI0030_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : ZSDSSDT001,KNA1,LIKP,VBAK,SSCRFIELDS.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          LFDAT               TYPE LIKP-LFDAT,
          BEZEI_G             TYPE TVGRT-BEZEI,
          BEZEI_O             TYPE TVKBT-BEZEI,
          VTEXT               TYPE TVTWT-VTEXT,
          VTWEG               TYPE VBAK-VTWEG,
          VKGRP               TYPE VBAK-VKGRP,
          VKBUR               TYPE VBAK-VKBUR,
          ERZETD              TYPE LIKP-ERZET,
          ERDATD              TYPE LIKP-ERDAT,
          ERZETE              TYPE LIKP-ERZET,
          ERDATE              TYPE LIKP-ERDAT,
          DOSIG_D             TYPE SY-DATUM,
          PODDA_D             TYPE SY-DATUM,
          AMPMF_S             TYPE ZSDSSDT001-AMPMF,
          REMAP_S             TYPE ZSDSSDT001-REMAP,
          REINV_S             TYPE ZSDSSDT001-REINV,
          REMAR_R             TYPE C LENGTH 255,
          REMAR_I             TYPE C LENGTH 255,
          SDS_SHPPV           TYPE ADRC-CITY1,
          SDS_CUSTNAME(100)   TYPE C,
          SDS_CUSTCODE        TYPE VBPA-KUNNR,
          SDS_SHIPADDR(250)   TYPE C,
          STATU_U             TYPE C LENGTH 2,
          LINE_COLOR          TYPE C LENGTH 4,
          CHECK               TYPE C,
          SMART_TRACKING_DATE TYPE ZSDSSDT025-SMART_TRACKING_DATE,
          SMART_TRACKING_TIME TYPE ZSDSSDT025-SMART_TRACKING_TIME,
          FLAG                TYPE ZSDSSDT025-FLAG,
          INVNO               TYPE ZSDSSDT025-INVNO,
          PETAX               TYPE ZSDSSDT025-PETAX.
          INCLUDE             TYPE ZSDSSDT001.
TYPES  END OF GY_RESULT.

TYPES : BEGIN OF GY_LIKP,
          VBELN   TYPE LIKP-VBELN,
          LFDAT   TYPE LIKP-LFDAT,
          BEZEI_G TYPE TVGRT-BEZEI,
          BEZEI_O TYPE TVKBT-BEZEI,
          VTEXT   TYPE TVTWT-VTEXT,
          VTWEG   TYPE VBAK-VTWEG,
          VKGRP   TYPE VBAK-VKGRP,
          VKBUR   TYPE VBAK-VKBUR,
          ERZET   TYPE LIKP-ERZET,
          ERDAT   TYPE LIKP-ERDAT,
          VBELN_S TYPE VBAP-VBELN,
          POSNR   TYPE VBAP-POSNR,
          LSTEL   TYPE LIKP-LSTEL,
          VSTEL   TYPE LIKP-VSTEL,
        END OF GY_LIKP.

TYPES : BEGIN OF GY_VBPA,
          VBELN      TYPE VBAK-VBELN,
          KUNNR      TYPE VBPA-KUNNR,
          NAME1      TYPE ADRC-NAME1,
          NAME2      TYPE ADRC-NAME2,
          NAME3      TYPE ADRC-NAME3,
          NAME4      TYPE ADRC-NAME4,
          STREET     TYPE ADRC-STREET,
          STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
          STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
          STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
          LOCATION   TYPE ADRC-LOCATION,
          POST_CODE1 TYPE ADRC-POST_CODE1,
          CITY1      TYPE ADRC-CITY1,
        END OF GY_VBPA.

TYPES : BEGIN OF GY_TVLAT,
          VSTEL TYPE TVLAT-VSTEL,
          LSTEL TYPE TVLAT-LSTEL,
          VTEXT TYPE TVLAT-VTEXT,
        END OF GY_TVLAT.

TYPES : BEGIN OF GY_ZSDSSDT025,
          DO_NO               TYPE ZSDSSDT025-DO_NO,
          SMART_TRACKING_DATE TYPE ZSDSSDT025-SMART_TRACKING_DATE,
          SMART_TRACKING_TIME TYPE ZSDSSDT025-SMART_TRACKING_TIME,
          FLAG                TYPE ZSDSSDT025-FLAG,
          INVNO               TYPE ZSDSSDT025-INVNO,
          PETAX               TYPE ZSDSSDT025-PETAX,
        END OF GY_ZSDSSDT025.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT WITH EMPTY KEY,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_ZTMM_DO_EXPORT TYPE TABLE OF ZSDSSDT002,
       GS_ZTMM_DO_EXPORT TYPE ZSDSSDT002.

DATA : GT_ZSDSSDT025 TYPE HASHED TABLE OF GY_ZSDSSDT025 WITH UNIQUE KEY DO_NO,
       GS_ZSDSSDT025 TYPE GY_ZSDSSDT025.

DATA : GT_LIKP TYPE TABLE OF GY_LIKP WITH EMPTY KEY,
       GS_LIKP TYPE GY_LIKP.

DATA : GT_VBPA TYPE TABLE OF GY_VBPA,
       GS_VBPA TYPE GY_VBPA.

DATA : GT_TVLAT TYPE TABLE OF GY_TVLAT,
       GS_TVLAT TYPE GY_TVLAT.
DATA:  LS_COLOR TYPE LVC_T_STYL.

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
              UPS  TYPE C LENGTH 3 VALUE 'UPS',
              S    TYPE C LENGTH 1 VALUE 'S',
              E    TYPE C LENGTH 1 VALUE 'E',
              POST TYPE C LENGTH 4 VALUE 'POST',
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
