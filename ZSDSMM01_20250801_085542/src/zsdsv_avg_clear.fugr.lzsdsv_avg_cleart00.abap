*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_AVG_CLEAR.................................*
TABLES: ZSDSV_AVG_CLEAR, *ZSDSV_AVG_CLEAR. "view work areas
CONTROLS: TCTRL_ZSDSV_AVG_CLEAR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_AVG_CLEAR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_AVG_CLEAR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_AVG_CLEAR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_AVG_CLEAR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_AVG_CLEAR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_AVG_CLEAR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_AVG_CLEAR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_AVG_CLEAR_TOTAL.

*.........table declarations:.................................*
TABLES: T173                           .
TABLES: T173T                          .
TABLES: ZSDSMMT009                     .
