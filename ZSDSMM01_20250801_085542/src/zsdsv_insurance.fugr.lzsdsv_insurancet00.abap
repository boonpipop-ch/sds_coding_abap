*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_INSURANCE.................................*
TABLES: ZSDSV_INSURANCE, *ZSDSV_INSURANCE. "view work areas
CONTROLS: TCTRL_ZSDSV_INSURANCE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_INSURANCE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_INSURANCE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_INSURANCE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_INSURANCE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_INSURANCE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_INSURANCE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_INSURANCE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_INSURANCE_TOTAL.

*.........table declarations:.................................*
TABLES: T173                           .
TABLES: T173T                          .
TABLES: ZSDSMMT008                     .
