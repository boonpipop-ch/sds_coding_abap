*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_DOCTY................................*
TABLES: ZSDSV_ETAX_DOCTY, *ZSDSV_ETAX_DOCTY. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_DOCTY
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_DOCTY. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_DOCTY.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_DOCTY_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DOCTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DOCTY_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_DOCTY_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DOCTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DOCTY_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC014                     .
