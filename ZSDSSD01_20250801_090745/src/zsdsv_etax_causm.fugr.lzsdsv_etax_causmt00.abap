*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_CAUSM................................*
TABLES: ZSDSV_ETAX_CAUSM, *ZSDSV_ETAX_CAUSM. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_CAUSM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_CAUSM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_CAUSM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_CAUSM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_CAUSM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_CAUSM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_CAUSM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_CAUSM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_CAUSM_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC019                     .
