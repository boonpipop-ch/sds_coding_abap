*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_SBDT.................................*
TABLES: ZSDSV_ETAX_SBDT, *ZSDSV_ETAX_SBDT. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_SBDT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_SBDT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_SBDT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_SBDT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_SBDT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_SBDT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_SBDT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_SBDT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_SBDT_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC024                     .
