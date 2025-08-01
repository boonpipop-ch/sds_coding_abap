*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_WBS_ACTTY.................................*
TABLES: ZSDSV_WBS_ACTTY, *ZSDSV_WBS_ACTTY. "view work areas
CONTROLS: TCTRL_ZSDSV_WBS_ACTTY
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_WBS_ACTTY. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_WBS_ACTTY.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_WBS_ACTTY_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_WBS_ACTTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_WBS_ACTTY_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_WBS_ACTTY_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_WBS_ACTTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_WBS_ACTTY_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSPSC002                     .
