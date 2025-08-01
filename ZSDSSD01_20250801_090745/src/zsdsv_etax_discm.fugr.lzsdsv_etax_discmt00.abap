*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_DISCM................................*
TABLES: ZSDSV_ETAX_DISCM, *ZSDSV_ETAX_DISCM. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_DISCM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_DISCM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_DISCM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_DISCM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DISCM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DISCM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_DISCM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DISCM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DISCM_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC018                     .
