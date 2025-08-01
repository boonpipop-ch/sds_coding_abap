*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_MAT_WRT...................................*
TABLES: ZSDSV_MAT_WRT, *ZSDSV_MAT_WRT. "view work areas
CONTROLS: TCTRL_ZSDSV_MAT_WRT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_MAT_WRT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_MAT_WRT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_MAT_WRT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_MAT_WRT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_MAT_WRT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_MAT_WRT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_MAT_WRT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_MAT_WRT_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCMC003                     .
