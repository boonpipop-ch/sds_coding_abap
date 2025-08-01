*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0540_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0460_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : E070.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_RESULT,
          MATNR          TYPE MARA-MATNR,
          PROD_HIER      TYPE STRING,
          IN_OUT_DOOR    TYPE STRING,
          IN_DOOR_TYPR   TYPE STRING,
          CAPACITY_VAL   TYPE STRING,
          CAPACITY_UNBIT TYPE STRING,
          INVT_FLAG      TYPE STRING,
          VOLTAGE_LV     TYPE STRING,
          FREQUENCY      TYPE STRING,
          PHASE          TYPE STRING,
          COOLING_SYSTEM TYPE STRING,
          REFRIG_TYPR    TYPE STRING,
          COOLING_HEATP  TYPE STRING,
          COLOR          TYPE STRING,
          PAIR_MUL_TYPE  TYPE STRING,
          MAT_DESC       TYPE STRING,
          MAT_DESC1      TYPE STRING,
          MODEL_CHECK    TYPE STRING,
          PRODUCT_CHECK  TYPE STRING,
          MAT_GROUP_1    TYPE STRING,
          MAT_GROUP_2    TYPE STRING,
          COMMPRICE1     TYPE STRING,
          COUNTING_FLAG  TYPE STRING,
          STATUS         TYPE ICON-ID,
          MESSAGE        TYPE STRING,
          CHECK          TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_MVKE,
          MATNR TYPE MVKE-MATNR,
          VKORG TYPE MVKE-VKORG,
          VTWEG TYPE MVKE-VTWEG,
        END OF GY_MVKE.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE TABLE OF GY_RESULT WITH EMPTY KEY,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_MVKE TYPE TABLE OF GY_MVKE,
       GS_MVKE TYPE GY_MVKE.

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
