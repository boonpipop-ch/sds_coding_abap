*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_PRODH_APUP................................*
TABLES: ZSDSV_PRODH_APUP, *ZSDSV_PRODH_APUP. "view work areas
CONTROLS: TCTRL_ZSDSV_PRODH_APUP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_PRODH_APUP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_PRODH_APUP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_PRODH_APUP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_PRODH_APUP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_PRODH_APUP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_PRODH_APUP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_PRODH_APUP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_PRODH_APUP_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSPSC004                     .
