*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0730_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES : VBAP,PRPS.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
*TYPES : BEGIN OF GY_RESULT,
*          VBELN      TYPE VBAP-VBELN,
*          POSNR      TYPE VBAP-POSNR,
*          MATNR      TYPE VBAP-MATNR,
*          NETWR      TYPE VBAP-NETWR,
*          KUNNR      TYPE VBAK-KUNNR,
*          PS_PSP_PNR TYPE VBAP-PS_PSP_PNR,
*          CHECK      TYPE C,
*        END OF GY_RESULT.

*TYPES : BEGIN OF GY_RESULT2,
*          BANFN      TYPE EBAN-BANFN,
*          MATNR      TYPE EBAN-MATNR,
*          TXZ01      TYPE EBAN-TXZ01,
*          PREIS      TYPE P DECIMALS 2,
*          EBELN      TYPE EKKO-EBELN,
*          MBLNR      TYPE MSEG-MBLNR,
*          PS_PSP_PNR TYPE EBKN-PS_PSP_PNR,
*
*          CHECK   TYPE C,
*        END OF GY_RESULT2.
*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA : GT_RESULT TYPE  ZSDSFIS186_TT,
       GS_RESULT TYPE ZSDSFIS186.

DATA : GT_RESULT2 TYPE  ZSDSFIS187_TT,
       GS_RESULT2 TYPE ZSDSFIS187.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.
*--------------------------------------------------------------------*
DATA : GT_FCAT_OO   TYPE LVC_T_FCAT,
       GS_LAYOUT_OO TYPE LVC_S_LAYO,
       GT_SORT_OO   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT_OO   TYPE SLIS_SORTINFO_ALV.

DATA : GT_EVENTS TYPE SLIS_T_EVENT,
       GS_EVENTS TYPE SLIS_ALV_EVENT.

DATA : GCL_ALV      TYPE REF TO CL_GUI_ALV_GRID,
       GCL_CONT     TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : GT_TOOLBAR_EXCLUDING     TYPE UI_FUNCTIONS.
*--------------------------------------------------------------------*
DATA : GT_FCAT2_OO   TYPE LVC_T_FCAT,
       GS_LAYOUT2_OO TYPE LVC_S_LAYO,
       GT_SORT2_OO   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT2_OO   TYPE SLIS_SORTINFO_ALV.

DATA : GT_EVENTS2 TYPE SLIS_T_EVENT,
       GS_EVENTS2 TYPE SLIS_ALV_EVENT.

DATA : GCL_ALV2      TYPE REF TO CL_GUI_ALV_GRID,
       GCL_CONT2     TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : GT_TOOLBAR_EXCLUDING2     TYPE UI_FUNCTIONS.
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

CONSTANTS: GC_CON_NAME TYPE C LENGTH 6  VALUE 'CC_ALV'.



CONSTANTS: GC_CON_NAME2 TYPE C LENGTH 7  VALUE 'CC_ALV2'.

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
