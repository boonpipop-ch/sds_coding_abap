*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ZSDSFIC029................................*
TABLES: ZSDSV_ZSDSFIC029, *ZSDSV_ZSDSFIC029. "view work areas
CONTROLS: TCTRL_ZSDSV_ZSDSFIC029
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ZSDSFIC029. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ZSDSFIC029.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ZSDSFIC029_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC029.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC029_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ZSDSFIC029_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ZSDSFIC029.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ZSDSFIC029_TOTAL.

*.........table declarations:.................................*
TABLES: ADRP                           .
TABLES: USR21                          .
TABLES: ZSDSFIC029                     .
