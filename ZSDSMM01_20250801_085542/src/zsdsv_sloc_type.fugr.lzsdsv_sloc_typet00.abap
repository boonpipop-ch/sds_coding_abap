*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_SLOC_TYPE.................................*
TABLES: ZSDSV_SLOC_TYPE, *ZSDSV_SLOC_TYPE. "view work areas
CONTROLS: TCTRL_ZSDSV_SLOC_TYPE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_SLOC_TYPE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_SLOC_TYPE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_SLOC_TYPE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_SLOC_TYPE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_SLOC_TYPE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_SLOC_TYPE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_SLOC_TYPE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_SLOC_TYPE_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSMMT016                     .
