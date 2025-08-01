*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_CM_PRCTR..................................*
TABLES: ZSDSV_CM_PRCTR, *ZSDSV_CM_PRCTR. "view work areas
CONTROLS: TCTRL_ZSDSV_CM_PRCTR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_CM_PRCTR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_CM_PRCTR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_CM_PRCTR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_CM_PRCTR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_CM_PRCTR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_CM_PRCTR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_CM_PRCTR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_CM_PRCTR_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCMC001                     .
