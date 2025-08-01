*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_PJ_RESPPER................................*
TABLES: ZSDSV_PJ_RESPPER, *ZSDSV_PJ_RESPPER. "view work areas
CONTROLS: TCTRL_ZSDSV_PJ_RESPPER
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_PJ_RESPPER. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_PJ_RESPPER.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_PJ_RESPPER_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_PJ_RESPPER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_PJ_RESPPER_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_PJ_RESPPER_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_PJ_RESPPER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_PJ_RESPPER_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSPSC003                     .
