*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ZSDSFIC024................................*
TABLES: ZSDSV_ZSDSFIC024, *ZSDSV_ZSDSFIC024. "view work areas
CONTROLS: TCTRL_ZSDSV_ZSDSFIC024
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ZSDSFIC024. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ZSDSFIC024.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ZSDSFIC024_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC024.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC024_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ZSDSFIC024_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC024.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC024_TOTAL.

*.........table declarations:.................................*
TABLES: KNA1                           .
TABLES: ZSDSFIC024                     .
