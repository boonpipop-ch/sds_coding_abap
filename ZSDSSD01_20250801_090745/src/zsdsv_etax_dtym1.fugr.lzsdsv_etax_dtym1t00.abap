*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_DTYM1................................*
TABLES: ZSDSV_ETAX_DTYM1, *ZSDSV_ETAX_DTYM1. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_DTYM1
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_DTYM1. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_DTYM1.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_DTYM1_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DTYM1.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DTYM1_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_DTYM1_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DTYM1.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DTYM1_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC017                     .
