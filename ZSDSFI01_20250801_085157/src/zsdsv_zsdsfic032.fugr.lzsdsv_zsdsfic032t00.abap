*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ZSDSFIC032................................*
TABLES: ZSDSV_ZSDSFIC032, *ZSDSV_ZSDSFIC032. "view work areas
CONTROLS: TCTRL_ZSDSV_ZSDSFIC032
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ZSDSFIC032. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ZSDSFIC032.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ZSDSFIC032_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC032.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC032_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ZSDSFIC032_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC032.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC032_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSFIC032                     .
