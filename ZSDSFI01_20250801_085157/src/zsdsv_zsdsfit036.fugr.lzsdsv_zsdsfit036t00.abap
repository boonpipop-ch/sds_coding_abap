*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ZSDSFIT036................................*
TABLES: ZSDSV_ZSDSFIT036, *ZSDSV_ZSDSFIT036. "view work areas
CONTROLS: TCTRL_ZSDSV_ZSDSFIT036
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ZSDSFIT036. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ZSDSFIT036.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ZSDSFIT036_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIT036.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIT036_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ZSDSFIT036_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIT036.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIT036_TOTAL.

*.........table declarations:.................................*
TABLES: BUT000                         .
TABLES: ZSDSFIT036                     .
