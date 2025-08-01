*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_CM_RAKEY..................................*
TABLES: ZSDSV_CM_RAKEY, *ZSDSV_CM_RAKEY. "view work areas
CONTROLS: TCTRL_ZSDSV_CM_RAKEY
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_CM_RAKEY. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_CM_RAKEY.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_CM_RAKEY_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_CM_RAKEY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_CM_RAKEY_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_CM_RAKEY_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_CM_RAKEY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_CM_RAKEY_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCMC004                     .
