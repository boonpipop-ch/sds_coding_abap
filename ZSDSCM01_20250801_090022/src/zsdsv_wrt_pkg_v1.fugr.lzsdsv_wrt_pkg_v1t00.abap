*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_WRT_PKG_V1................................*
TABLES: ZSDSV_WRT_PKG_V1, *ZSDSV_WRT_PKG_V1. "view work areas
CONTROLS: TCTRL_ZSDSV_WRT_PKG_V1
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_WRT_PKG_V1. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_WRT_PKG_V1.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_WRT_PKG_V1_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_WRT_PKG_V1.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_WRT_PKG_V1_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_WRT_PKG_V1_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_WRT_PKG_V1.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_WRT_PKG_V1_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCMC009                     .
