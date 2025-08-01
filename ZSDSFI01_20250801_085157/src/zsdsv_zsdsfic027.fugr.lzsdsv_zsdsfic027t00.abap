*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ZSDSFIC027................................*
TABLES: ZSDSV_ZSDSFIC027, *ZSDSV_ZSDSFIC027. "view work areas
CONTROLS: TCTRL_ZSDSV_ZSDSFIC027
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ZSDSFIC027. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ZSDSFIC027.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ZSDSFIC027_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC027.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC027_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ZSDSFIC027_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC027.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC027_TOTAL.

*.........table declarations:.................................*
TABLES: PA0002                         .
TABLES: ZSDSFIC027                     .
