*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_DTRC.................................*
TABLES: ZSDSV_ETAX_DTRC, *ZSDSV_ETAX_DTRC. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_DTRC
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_DTRC. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_DTRC.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_DTRC_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DTRC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DTRC_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_DTRC_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_DTRC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_DTRC_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC023                     .
