*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_DELIVERYTO................................*
TABLES: ZSDSV_DELIVERYTO, *ZSDSV_DELIVERYTO. "view work areas
CONTROLS: TCTRL_ZSDSV_DELIVERYTO
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_DELIVERYTO. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_DELIVERYTO.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_DELIVERYTO_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_DELIVERYTO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_DELIVERYTO_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_DELIVERYTO_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_DELIVERYTO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_DELIVERYTO_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSMMT017                     .
