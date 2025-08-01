*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_COL_LOG...................................*
TABLES: ZSDSV_COL_LOG, *ZSDSV_COL_LOG. "view work areas
CONTROLS: TCTRL_ZSDSV_COL_LOG
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_COL_LOG. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_COL_LOG.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_COL_LOG_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_COL_LOG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_COL_LOG_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_COL_LOG_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_COL_LOG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_COL_LOG_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSFIT046                     .
