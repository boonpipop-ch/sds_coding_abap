FUNCTION-POOL ZSDSFI04.                     "MESSAGE-ID ..

* INCLUDE LZSDSFI04D...                      " Local class definition
*--------------------------------------------------------------------*
* TYPE
*--------------------------------------------------------------------*
*TYPES : BEGIN OF GY_DISPLAY,
*         SELECTION_ASSET TYPE TABLE OF ZSDSFIS099 WITH EMPTY KEY,
*         SELECTION_EMP   TYPE ANLZ-PERNR,
*         EXPORT_RETURN   TYPE ZSDSFIS098_TT,
*         EXPORT_MESTYPE  TYPE CHAR1,
*         EXPORT_MESSAGE  TYPE CHAR70,
*       END OF GY_DISPLAY.
*--------------------------------------------------------------------*
* DATA TYPE
*--------------------------------------------------------------------*
DATA : BEGIN OF GS_DISPLAY,
         SELECTION_ASSET TYPE TABLE OF ZSDSFIS099 WITH EMPTY KEY,
         SELECTION_EMP   TYPE ANLZ-PERNR,
         EXPORT_RETURN   TYPE ZSDSFIS098_TT,
         EXPORT_MESTYPE  TYPE CHAR1,
         EXPORT_MESSAGE  TYPE CHAR70,
       END OF GS_DISPLAY.
DATA : GT_DISPLAY LIKE TABLE OF GS_DISPLAY.
*--------------------------------------------------------------------*
* Constant
*--------------------------------------------------------------------*
CONSTANTS : BEGIN OF GC_CON,
              S TYPE C LENGTH 1 VALUE 'S',
              E TYPE C LENGTH 1 VALUE 'E',
            END OF GC_CON.
