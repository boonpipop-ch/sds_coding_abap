*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ZSDSFIC031................................*
TABLES: ZSDSV_ZSDSFIC031, *ZSDSV_ZSDSFIC031. "view work areas
CONTROLS: TCTRL_ZSDSV_ZSDSFIC031
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ZSDSFIC031. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ZSDSFIC031.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ZSDSFIC031_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC031.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC031_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ZSDSFIC031_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC031.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC031_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSFIC031                     .
