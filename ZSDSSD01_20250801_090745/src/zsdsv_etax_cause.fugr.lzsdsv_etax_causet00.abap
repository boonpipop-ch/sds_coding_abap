*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_CAUSE................................*
TABLES: ZSDSV_ETAX_CAUSE, *ZSDSV_ETAX_CAUSE. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_CAUSE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_CAUSE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_CAUSE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_CAUSE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_CAUSE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_CAUSE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_CAUSE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_CAUSE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_CAUSE_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC015                     .
