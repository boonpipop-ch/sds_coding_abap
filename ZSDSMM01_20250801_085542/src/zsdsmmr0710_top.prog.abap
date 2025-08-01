*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0710_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : RESB.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          RSNUM   TYPE RESB-RSNUM,
          RSPOS   TYPE RESB-RSPOS,
          MATNR   TYPE RESB-MATNR,
          MAKTX   TYPE MAKT-MAKTX,
          LGORT   TYPE RESB-LGORT,
          UMLGO   TYPE RESB-UMLGO,
          BDMNG   TYPE RESB-BDMNG,
          MEINS   TYPE RESB-MEINS,
          LIFNR   TYPE RESB-LIFNR,
          NAMEA   TYPE CHAR70,
          STATUS  TYPE ICON-ID,
          MESSAGE TYPE STRING,
          BDTER   TYPE RESB-BDTER,
          STATU   TYPE ZSDSMMT028-STATU,
*          DOCNO   TYPE MBLNR,
*          DOCYR   TYPE MJAHR,
*          DOCLI   TYPE MBLPO,
          CHECK   TYPE C,
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
