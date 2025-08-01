*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0500_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : VBAK.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          VBELN      TYPE VBAK-VBELN,
          KUNNR      TYPE VBAK-KUNNR,
          NETWR      TYPE VBAK-NETWR,
          NAME1      TYPE CHAR255,
          STATUS     TYPE ICON-ID,

          BDR_REF    TYPE VBRK-BDR_REF,
          STREET     TYPE ADRC-STREET,
          STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
          STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
          STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
          LOCATION   TYPE ADRC-LOCATION,
          CITY2      TYPE ADRC-CITY2,
          CITY1      TYPE ADRC-CITY1,
          POST_CODE  TYPE ADRC-POST_CODE1,
          ZTERM      TYPE VBRK-ZTERM,
          ZZ1_CUS_PO TYPE CRMS4D_SERV_H-ZZ1_CUS_PO,
          EMAIL      TYPE CHAR20,
          BRANCH     TYPE CHAR20,
          BRANCH_DES TYPE CHAR20,
          TAX        TYPE CHAR20,

          MESSAGE    TYPE STRING,
          CHECK      TYPE C,
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
