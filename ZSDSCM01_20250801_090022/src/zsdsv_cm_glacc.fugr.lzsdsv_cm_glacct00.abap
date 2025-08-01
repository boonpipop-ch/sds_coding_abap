*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_CM_GLACC..................................*
TABLES: ZSDSV_CM_GLACC, *ZSDSV_CM_GLACC. "view work areas
CONTROLS: TCTRL_ZSDSV_CM_GLACC
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_CM_GLACC. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_CM_GLACC.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_CM_GLACC_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_CM_GLACC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_CM_GLACC_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_CM_GLACC_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_CM_GLACC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_CM_GLACC_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCMC005                     .
