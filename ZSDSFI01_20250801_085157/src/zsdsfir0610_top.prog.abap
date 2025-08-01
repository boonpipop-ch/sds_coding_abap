*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0610_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : ACDOCA,VBRP.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT.
          INCLUDE TYPE ZSDSFIS183.
*          PS_POSID         TYPE ACDOCA-PS_POSID,
*          RLDNR            TYPE ACDOCA-RLDNR,
*          RBUKRS           TYPE ACDOCA-RBUKRS,
*          GJAHR            TYPE ACDOCA-GJAHR,
*          BELNR            TYPE ACDOCA-BELNR,
*          DOCLN            TYPE ACDOCA-DOCLN,
*          BUDAT            TYPE ACDOCA-BUDAT,
*          BLDAT            TYPE ACDOCA-BLDAT,
*          MWSKZ            TYPE ACDOCA-MWSKZ,
*          AUGBL            TYPE ACDOCA-AUGBL,
*          AUGGJ            TYPE ACDOCA-AUGGJ,
*          TSL              TYPE ACDOCA-TSL,
*          RTCUR            TYPE ACDOCA-RTCUR,
*          MSL              TYPE ACDOCA-MSL,
*          RUNIT            TYPE ACDOCA-RUNIT,
*          KUNNR            TYPE ACDOCA-KUNNR,
*          NETDT            TYPE ACDOCA-NETDT,
*          RCNTR            TYPE ACDOCA-RCNTR,
*          PRCTR            TYPE ACDOCA-PRCTR,
*          AUFNR            TYPE ACDOCA-AUFNR,
*          MATNR            TYPE ACDOCA-MATNR,
*          RACCT            TYPE ACDOCA-RACCT,
*          FKART_v          TYPE VBRK-FKART,
*          VKORG_v          TYPE VBRK-VKORG,
*          VTWEG_v          TYPE VBRK-VTWEG,
*          VBELN_v          TYPE VBRP-VBELN,
*          POSNR_v          TYPE VBRP-POSNR,
*          VKGRP_v          TYPE VBRP-VKGRP,
*          VKBUR_v          TYPE VBRP-VKBUR,
*          FKART            TYPE VBRK-FKART,
*          VKORG            TYPE VBRK-VKORG,
*          VTWEG            TYPE VBRK-VTWEG,
*          VBELN            TYPE VBRP-VBELN,
*          POSNR            TYPE VBRP-POSNR,
*          VKGRP            TYPE VBRP-VKGRP,
*          VKBUR            TYPE VBRP-VKBUR,
*          PRODH            TYPE VBRP-PRODH,
*          SOno             TYPE VBRP-AUBEL,
*          DOno             TYPE VBRP-VGBEL,
*          SOno_v           TYPE VBRP-AUBEL,
*          DOno_v           TYPE VBRP-VGBEL,
*          TXT50            TYPE SKAT-TXT50,
*          SOQTY            TYPE ACDOCA-MSL,
*          BLART            TYPE BKPF-BLART,
*          NAMEA            TYPE C LENGTH 140,
*          PH1              TYPE CHAR5,
*          PH2              TYPE CHAR5,
*          PH3              TYPE CHAR8,
*          HSL              TYPE ACDOCA-HSL,
*          RHCUR            TYPE ACDOCA-RHCUR,
*          REGIO_PA         TYPE ACDOCA-REGIO_PA,
*          ZZ1_PROJTYPE_MSE TYPE ACDOCA-ZZ1_PROJTYPE_MSE,
*          ZZ1_ZZCAV_MSE    TYPE ACDOCA-ZZ1_ZZCAV_MSE,
*          ZZ1_ZZINNI_MSE   TYPE ACDOCA-ZZ1_ZZINNI_MSE,
*          ZZ1_ZZIUT_MSE    TYPE ACDOCA-ZZ1_ZZIUT_MSE,
*          ZZ1_ITMTYPE_MSE  TYPE ACDOCA-ZZ1_ITMTYPE_MSE,
*          ZZ1_SERVTYPE_MSE TYPE ACDOCA-ZZ1_SERVTYPE_MSE,
*          ZZ1_ACTTYPE_MSE  TYPE ACDOCA-ZZ1_ACTTYPE_MSE,
*          ZZ1_SDSDIST_MSE  TYPE ACDOCA-ZZ1_SDSDIST_MSE,
*          ZZ1_ZZPHA_MSE    TYPE ACDOCA-ZZ1_ZZPHA_MSE,
*          ZZ1_PROJ_MSE     TYPE ACDOCA-ZZ1_PROJ_MSE,
*          ZZ1_FISCYR_MSE   TYPE ACDOCA-ZZ1_FISCYR_MSE,
*          ZZ1_ZZREFTN_MSE  TYPE ACDOCA-ZZ1_ZZREFTN_MSE,
*          ZZ1_ZZPMT_MSE    TYPE ACDOCA-ZZ1_ZZPMT_MSE,
*          ZZ1_SSERIES      TYPE ACDOCA-ZZ1_SSERIES,
*          ZZ1_ZZREFT       TYPE ACDOCA-ZZ1_ZZREFT,
*          ZZ1_SHOPTYPE     TYPE ACDOCA-ZZ1_SHOPTYPE,
*          ZZ1_APPLTN       TYPE ACDOCA-ZZ1_APPLTN,
*          ZZ1_MVGR1        TYPE ACDOCA-ZZ1_MVGR1,
*          PERNR            TYPE VBPA-PERNR,
*          SALEN            TYPE CHAR70,
*          GRPDS            TYPE CHAR70,
*          OFFDS            TYPE CHAR70,
*          CHADS            TYPE CHAR70,
*          ORGDS            TYPE CHAR70,
*          DIVIS            TYPE CHAR70,
*          APPLI            TYPE CHAR70,
*          GRPDS_M          TYPE CHAR70,
*          OFFDS_M          TYPE CHAR70,
*          CHADS_M          TYPE CHAR70,
*          ORGDS_M          TYPE CHAR70,
*          DIVIS_M          TYPE CHAR70,
*          APPLI_M          TYPE CHAR70,
*          PSTYV            TYPE VBRP-PSTYV,
*          PSTYV_V          TYPE VBRP-PSTYV,
*          MATNR_BOM        TYPE VBRP-MATNR,
*          MATNR_BOM_V      TYPE VBRP-MATNR,
*TYPES :     CHECK TYPE C,
TYPES  END OF GY_RESULT.

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
