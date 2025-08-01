*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_DISC.................................*
TABLES: ZSDSV_ETAX_DISC, *ZSDSV_ETAX_DISC. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_DISC
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_DISC. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_DISC.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_DISC_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DISC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DISC_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_DISC_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DISC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DISC_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC016                     .
