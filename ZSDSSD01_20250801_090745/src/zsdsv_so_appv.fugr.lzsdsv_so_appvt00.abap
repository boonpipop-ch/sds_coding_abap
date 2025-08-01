*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_SO_APPV...................................*
TABLES: ZSDSV_SO_APPV, *ZSDSV_SO_APPV. "view work areas
CONTROLS: TCTRL_ZSDSV_SO_APPV
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_SO_APPV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_SO_APPV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_SO_APPV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_SO_APPV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_SO_APPV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_SO_APPV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_SO_APPV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_SO_APPV_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDT009                     .
