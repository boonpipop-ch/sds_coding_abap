*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_WRT_PKG...................................*
TABLES: ZSDSV_WRT_PKG, *ZSDSV_WRT_PKG. "view work areas
CONTROLS: TCTRL_ZSDSV_WRT_PKG
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_WRT_PKG. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_WRT_PKG.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_WRT_PKG_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_WRT_PKG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_WRT_PKG_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_WRT_PKG_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_WRT_PKG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_WRT_PKG_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCMC002                     .
