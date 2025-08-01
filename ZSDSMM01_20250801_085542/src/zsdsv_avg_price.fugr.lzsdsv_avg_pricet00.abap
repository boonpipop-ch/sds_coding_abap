*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_AVG_PRICE.................................*
TABLES: ZSDSV_AVG_PRICE, *ZSDSV_AVG_PRICE. "view work areas
CONTROLS: TCTRL_ZSDSV_AVG_PRICE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_AVG_PRICE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_AVG_PRICE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_AVG_PRICE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_AVG_PRICE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_AVG_PRICE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_AVG_PRICE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_AVG_PRICE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_AVG_PRICE_TOTAL.

*.........table declarations:.................................*
TABLES: T173                           .
TABLES: T173T                          .
TABLES: ZSDSMMT010                     .
