*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ZSDSSDC013................................*
TABLES: ZSDSV_ZSDSSDC013, *ZSDSV_ZSDSSDC013. "view work areas
CONTROLS: TCTRL_ZSDSV_ZSDSSDC013
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ZSDSSDC013. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ZSDSSDC013.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ZSDSSDC013_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSSDC013.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSSDC013_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ZSDSSDC013_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSSDC013.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSSDC013_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC013                     .
