*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_COL_STATUS................................*
TABLES: ZSDSV_COL_STATUS, *ZSDSV_COL_STATUS. "view work areas
CONTROLS: TCTRL_ZSDSV_COL_STATUS
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_COL_STATUS. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_COL_STATUS.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_COL_STATUS_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_COL_STATUS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_COL_STATUS_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_COL_STATUS_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_COL_STATUS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_COL_STATUS_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSFIT038                     .
