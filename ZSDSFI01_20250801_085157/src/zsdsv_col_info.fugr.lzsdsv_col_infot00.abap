*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_COL_INFO..................................*
TABLES: ZSDSV_COL_INFO, *ZSDSV_COL_INFO. "view work areas
CONTROLS: TCTRL_ZSDSV_COL_INFO
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_COL_INFO. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_COL_INFO.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_COL_INFO_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_COL_INFO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_COL_INFO_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_COL_INFO_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_COL_INFO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_COL_INFO_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSFIT029                     .
