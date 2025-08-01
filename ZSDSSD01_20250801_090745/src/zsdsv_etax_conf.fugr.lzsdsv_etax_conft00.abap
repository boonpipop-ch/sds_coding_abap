*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_ETAX_CONF.................................*
TABLES: ZSDSV_ETAX_CONF, *ZSDSV_ETAX_CONF. "view work areas
CONTROLS: TCTRL_ZSDSV_ETAX_CONF
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_ETAX_CONF. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_ETAX_CONF.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_ETAX_CONF_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_CONF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_CONF_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_ETAX_CONF_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_ETAX_CONF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_ETAX_CONF_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSSDC011                     .
