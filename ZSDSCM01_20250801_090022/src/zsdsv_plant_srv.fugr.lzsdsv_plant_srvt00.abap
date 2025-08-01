*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_PLANT_SRV.................................*
TABLES: ZSDSV_PLANT_SRV, *ZSDSV_PLANT_SRV. "view work areas
CONTROLS: TCTRL_ZSDSV_PLANT_SRV
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_PLANT_SRV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_PLANT_SRV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_PLANT_SRV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_PLANT_SRV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_PLANT_SRV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_PLANT_SRV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_PLANT_SRV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_PLANT_SRV_TOTAL.

*.........table declarations:.................................*
TABLES: CRMS4C_PLANT_SRV               .
