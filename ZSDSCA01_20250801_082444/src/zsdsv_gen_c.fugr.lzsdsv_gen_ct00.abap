*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_GEN_C.....................................*
TABLES: ZSDSV_GEN_C, *ZSDSV_GEN_C. "view work areas
CONTROLS: TCTRL_ZSDSV_GEN_C
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_GEN_C. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_GEN_C.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_GEN_C_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_GEN_C.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_GEN_C_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_GEN_C_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_GEN_C.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_GEN_C_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCAC001                     .
