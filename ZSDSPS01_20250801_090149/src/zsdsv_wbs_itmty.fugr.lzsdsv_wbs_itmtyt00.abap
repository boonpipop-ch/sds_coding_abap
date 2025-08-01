*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_WBS_ITMTY.................................*
TABLES: ZSDSV_WBS_ITMTY, *ZSDSV_WBS_ITMTY. "view work areas
CONTROLS: TCTRL_ZSDSV_WBS_ITMTY
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_WBS_ITMTY. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_WBS_ITMTY.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_WBS_ITMTY_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_WBS_ITMTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_WBS_ITMTY_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_WBS_ITMTY_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_WBS_ITMTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_WBS_ITMTY_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSPSC001                     .
