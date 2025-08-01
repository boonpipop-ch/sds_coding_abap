*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_IMP_DUTY..................................*
TABLES: ZSDSV_IMP_DUTY, *ZSDSV_IMP_DUTY. "view work areas
CONTROLS: TCTRL_ZSDSV_IMP_DUTY
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_IMP_DUTY. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_IMP_DUTY.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_IMP_DUTY_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_IMP_DUTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_IMP_DUTY_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_IMP_DUTY_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_IMP_DUTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_IMP_DUTY_TOTAL.

*.........table declarations:.................................*
TABLES: MAKT                           .
TABLES: MARA                           .
TABLES: ZSDSMMT012                     .
