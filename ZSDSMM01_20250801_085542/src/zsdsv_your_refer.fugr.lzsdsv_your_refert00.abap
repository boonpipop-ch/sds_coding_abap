*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_YOUR_REFER................................*
TABLES: ZSDSV_YOUR_REFER, *ZSDSV_YOUR_REFER. "view work areas
CONTROLS: TCTRL_ZSDSV_YOUR_REFER
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_YOUR_REFER. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_YOUR_REFER.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_YOUR_REFER_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_YOUR_REFER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_YOUR_REFER_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_YOUR_REFER_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_YOUR_REFER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_YOUR_REFER_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSMMT023                     .
