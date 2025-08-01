*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0420_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : EBAN,ZSDSMMT006.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          WEBNO      TYPE ZSDSMMT006-WEBNO,
          BANFN      TYPE EBAN-BANFN,
          BNFPO      TYPE EBAN-BNFPO,
          ERDAT      TYPE EBAN-ERDAT,
          ERNAM      TYPE EBAN-ERNAM,
          MATNR      TYPE EBAN-MATNR,
          TXZ01      TYPE EBAN-TXZ01,
          MENGE      TYPE P DECIMALS 0,
          MEINS      TYPE EBAN-MEINS,
          PREIS      TYPE EBAN-PREIS,
          WAERS      TYPE EBAN-WAERS,
          LFDAT      TYPE EBAN-LFDAT,
          SAKTO      TYPE EBKN-SAKTO,
          KOSTL      TYPE EBKN-KOSTL,
          AUFNR      TYPE EBKN-AUFNR,
          PS_PSP_PNR TYPE EBKN-PS_PSP_PNR,
          BSMNG      TYPE P DECIMALS 0,
          EBELN      TYPE EBAN-EBELN,
          EBELP      TYPE EBAN-EBELP,
          STATU      TYPE ZSDSMMT001-STATU,
          ACTBY      TYPE ZSDSMMT001-ACTBY,
          PRSTU      TYPE C LENGTH 10,
          REMAI      TYPE P DECIMALS 0,
          CHECK      TYPE C,
        END OF GY_RESULT.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT WITH EMPTY KEY,
       GS_RESULT TYPE GY_RESULT.

DATA : GV_VENDOR    TYPE LFA1-LIFNR,
       GV_PO_TYPE   TYPE EKKO-BSART,
       GV_PUR_GROUP TYPE EKKO-EKGRP.

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
