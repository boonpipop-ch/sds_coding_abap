FUNCTION-POOL ZSDSSD03.                     "MESSAGE-ID ..

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TS_CONF_DATA TYPE ZCL_SDSSD_ORDER_CONFIRMATION=>TS_CONF_DATA.
TYPES: TT_CONF_DATA TYPE ZCL_SDSSD_ORDER_CONFIRMATION=>TT_CONF_DATA.
TYPES: TS_ADVREC_DATA TYPE ZCL_SDSSD_ORDER_CONFIRMATION=>TS_ADVREC_DATA.
TYPES: TT_ADVREC_DATA TYPE ZCL_SDSSD_ORDER_CONFIRMATION=>TT_ADVREC_DATA.
TYPES: TS_ADVREC_ASSIGN TYPE ZCL_SDSSD_ORDER_CONFIRMATION=>TS_ADVREC_ASSIGN.
TYPES: TT_ADVREC_ASSIGN TYPE ZCL_SDSSD_ORDER_CONFIRMATION=>TT_ADVREC_ASSIGN.

TYPES: BEGIN OF TS_HEAD,
         ITEMS_COUNT     TYPE  I,
         WAERS           TYPE  TS_CONF_DATA-WAERS,
         ITEMS_AMOUNT    TYPE  TS_CONF_DATA-AMOUNT,
         TOTAL_ADVREC    TYPE  TS_ADVREC_DATA-DMBTR,
         ASSIGN_AMOUNT   TYPE  TS_CONF_DATA-AMOUNT,
         UNASSIGN_AMOUNT TYPE  TS_CONF_DATA-AMOUNT,
         REMAIN_ADVREC   TYPE  TS_ADVREC_DATA-DMBTR,
         STATUS          TYPE  ICON-ID,
       END OF TS_HEAD.

TYPES: BEGIN OF TS_ITEM.
         INCLUDE TYPE ZSDSSDS045.
TYPES: END OF TS_ITEM.
TYPES: TT_ITEM  TYPE STANDARD TABLE OF TS_ITEM
                     WITH DEFAULT KEY.
TYPES: TT_ITEM_SORT  TYPE  SORTED TABLE OF TS_ITEM
                     WITH UNIQUE KEY BUKRS
                                     BELNR
                                     GJAHR.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE        TYPE  CHAR1 VALUE 'X'                          ##NEEDED,

  GC_FUNC_SAVE   TYPE  UI_FUNC VALUE 'SAVE'                     ##NEEDED,
  GC_FUNC_DELETE TYPE  UI_FUNC VALUE 'DELETE'                   ##NEEDED,
  GC_FUNC_CANC   TYPE  UI_FUNC VALUE 'CANC'                     ##NEEDED,
  GC_FUNC_CLOSE  TYPE  UI_FUNC VALUE 'CLOSE'                    ##NEEDED,
  GC_FUNC_RESET  TYPE  UI_FUNC VALUE 'RESET'                    ##NEEDED,

  GC_OK          TYPE  ICON-ID VALUE '@0V@'                     ##NEEDED,
  GC_NOT_OK      TYPE  ICON-ID VALUE '@0W@'                     ##NEEDED.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_ITEM   TYPE  TT_ITEM                                     ##NEEDED,
  GT_DATA   TYPE  TT_CONF_DATA                                ##NEEDED,
  GT_ADVREC TYPE  TT_ADVREC_DATA                              ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_HEAD  TYPE  TS_HEAD                                     ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  GF_FCODE        TYPE  SY-UCOMM                             ##NEEDED,
  GF_CANCEL       TYPE  FLAG                                 ##NEEDED,
  GF_DISPLAY_ONLY TYPE  FLAG                                 ##NEEDED,
  GF_DELETE_ALLOW TYPE  FLAG                                 ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Class
*----------------------------------------------------------------------*
CLASS:
  LCL_EVENT_HANDLER_1 DEFINITION DEFERRED.  "for event handling

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_SAVE_ALL    TYPE CHAR1    VALUE 'A'                       ##NEEDED,
  GC_STRUCTURE_1 TYPE TABNAME  VALUE 'ZSDSSDS045'              ##NEEDED.

DATA:
  GREF_GRID_1             TYPE REF TO CL_GUI_ALV_GRID          ##NEEDED,
  GREF_CUSTOM_CONTAINER_1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER  ##NEEDED,
  GREF_CONTAINER_GRID_1   TYPE REF TO CL_GUI_CONTAINER         ##NEEDED,
  GREF_EVENT_RECEIVER_1   TYPE REF TO LCL_EVENT_HANDLER_1      ##NEEDED.

DATA:
  GF_CONTAINER_1          TYPE SCRFNAME VALUE 'CUST_CTRL'      ##NEEDED.

DATA:
  GT_TOOL_EXC_1 TYPE UI_FUNCTIONS                              ##NEEDED,
  GT_FIELDCAT_1 TYPE LVC_T_FCAT                                ##NEEDED,
  GT_SORT_1     TYPE LVC_T_SORT                                ##NEEDED.

DATA:
  GS_VARIANT_1  TYPE DISVARIANT                                ##NEEDED,
  GS_LAYOUT_1   TYPE LVC_S_LAYO                                ##NEEDED,
  GS_FIELDCAT_1 TYPE LVC_S_FCAT                                ##NEEDED,
  GS_SORT_1     TYPE LVC_S_SORT                                ##NEEDED,
  GS_PRINT_1    TYPE LVC_S_PRNT                                ##NEEDED.

FIELD-SYMBOLS:
  <G_LIST_1> TYPE STANDARD TABLE ##FS_ASSIGN_OK.            "#EC NEEDED
