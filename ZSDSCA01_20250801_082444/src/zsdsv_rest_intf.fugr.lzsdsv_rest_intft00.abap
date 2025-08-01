*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSV_REST_INTF.................................*
TABLES: ZSDSV_REST_INTF, *ZSDSV_REST_INTF. "view work areas
CONTROLS: TCTRL_ZSDSV_REST_INTF
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDSV_REST_INTF. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDSV_REST_INTF.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDSV_REST_INTF_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_REST_INTF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_REST_INTF_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDSV_REST_INTF_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDSV_REST_INTF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDSV_REST_INTF_TOTAL.

*.........table declarations:.................................*
TABLES: ZSDSCAC005                     .
