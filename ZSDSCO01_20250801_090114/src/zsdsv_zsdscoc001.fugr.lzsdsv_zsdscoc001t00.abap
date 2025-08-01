*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ZSDSCOC001................................*
TABLES: ZSDSV_ZSDSCOC001, *ZSDSV_ZSDSCOC001. "view work areas
CONTROLS: TCTRL_ZSDSV_ZSDSCOC001
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ZSDSCOC001. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ZSDSCOC001.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ZSDSCOC001_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSCOC001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSCOC001_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ZSDSCOC001_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSCOC001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSCOC001_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCOC001                     .
