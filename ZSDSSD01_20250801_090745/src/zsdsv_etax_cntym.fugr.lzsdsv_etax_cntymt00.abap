*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_CNTYM................................*
TABLES: ZSDSV_ETAX_CNTYM, *ZSDSV_ETAX_CNTYM. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_CNTYM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_CNTYM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_CNTYM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_CNTYM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_CNTYM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_CNTYM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_CNTYM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_CNTYM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_CNTYM_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC020                     .
