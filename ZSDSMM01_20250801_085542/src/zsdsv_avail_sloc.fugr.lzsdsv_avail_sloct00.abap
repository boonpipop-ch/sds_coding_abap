*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_AVAIL_SLOC................................*
TABLES: ZSDSV_AVAIL_SLOC, *ZSDSV_AVAIL_SLOC. "view work areas
CONTROLS: TCTRL_ZSDSV_AVAIL_SLOC
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_AVAIL_SLOC. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_AVAIL_SLOC.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_AVAIL_SLOC_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_AVAIL_SLOC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_AVAIL_SLOC_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_AVAIL_SLOC_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_AVAIL_SLOC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_AVAIL_SLOC_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSMMC002                     .
