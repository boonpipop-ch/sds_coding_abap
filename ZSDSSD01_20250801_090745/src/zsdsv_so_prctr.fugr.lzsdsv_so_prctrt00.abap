*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_SO_PRCTR..................................*
TABLES: ZSDSV_SO_PRCTR, *ZSDSV_SO_PRCTR. "view work areas
CONTROLS: TCTRL_ZSDSV_SO_PRCTR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_SO_PRCTR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_SO_PRCTR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_SO_PRCTR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_SO_PRCTR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_SO_PRCTR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_SO_PRCTR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_SO_PRCTR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_SO_PRCTR_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC001                     .
