*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0650_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : MKPF,MSEG.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          MBLNR TYPE MKPF-MBLNR,
          MJAHR TYPE MKPF-MJAHR,
          ZEILE TYPE MSEG-ZEILE,
          MATNR TYPE MSEG-MATNR,
          LGORT TYPE MSEG-LGORT,
          UMLGO TYPE MSEG-UMLGO,
          SHKZG TYPE MSEG-SHKZG,
          MENGE TYPE MSEG-MENGE,
          RSNUM TYPE MSEG-RSNUM,
          RSPOS TYPE MSEG-RSPOS,
          CPUDT TYPE MKPF-CPUDT,
          BWART TYPE MSEG-BWART,
          XAUTO TYPE MSEG-XAUTO,
          MESSG TYPE ZSDSMMT027-MESSG,
          STATU TYPE ICON-ID,
          CHECK TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_TRANSFER_ITEM,
          _TRANSFER_KEY       TYPE STRING,
          _RESER_KEY          TYPE STRING,
          _MAT_DOC_NO         TYPE STRING,
          _MAT_DOC_ITEM       TYPE STRING,
          _SAP_CREATED_DATE   TYPE STRING,
          _QUANTITY           TYPE P DECIMALS 0,
          _TYPE               TYPE STRING,
          _PRODUCT_CODE       TYPE STRING,
          _FROM_LOCATION      TYPE STRING,
          _TO_LOCATION        TYPE STRING,
          _REF_SAP_RESER_NO   TYPE STRING,
          _REF_SAP_RESER_ITEM TYPE STRING,
        END OF GY_TRANSFER_ITEM.
DATA : GTY_TRANSFER_ITEM TYPE TABLE OF GY_TRANSFER_ITEM.

TYPES : BEGIN OF GY_REQUEST_JSON,
          _REF_SAP_RESER_NO TYPE STRING,
          _MAT_DOC_NO       TYPE STRING,
          _TRANS_FER_ITEM   LIKE GTY_TRANSFER_ITEM,
        END OF GY_REQUEST_JSON.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_REQUEST_JSON TYPE GY_REQUEST_JSON.

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

CONSTANTS:
  BEGIN OF GC_CON,
    POST TYPE C LENGTH 4 VALUE 'POST',
    GET  TYPE C LENGTH 3 VALUE 'GET',
    E    TYPE C LENGTH 1 VALUE 'E',
    S    TYPE C LENGTH 1 VALUE 'S',
  END OF GC_CON .

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
