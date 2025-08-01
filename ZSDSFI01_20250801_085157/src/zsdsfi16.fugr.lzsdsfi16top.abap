FUNCTION-POOL zsdsfi16.                     "MESSAGE-ID ..
*----------------------------------------------------------------------*
* Function module : Z_SDSFI_POSTING_INTF_CLR_PART
* Function  Desc  : BTE imeplementation function
* WRICEF id       : FIARE05
* Start Date      : 31.10.2024
* Developer       : Apichat C.
* SAP Version     : S/4 HANA
* COPY ALL CODING OF TOP INCLUDE FROM STANDARD INCLUDE LFIPITOP
* and adjust logic with search term CH00
*----------------------------------------------------------------------*
* Modification History
***********************************************************************
* Author        :
* Date          :
* Change Request:
* Transport no. :
* Search term   : CH01
* Description   :
***********************************************************************


* INCLUDE LZSDSFI16D...                      " Local class definition

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  gty_coll_log       TYPE zsdsfit029 WITH INDICATORS col_ind TYPE abap_bool,
  gty_bank_statement TYPE zsdsfit042 WITH INDICATORS col_ind TYPE abap_bool,

  tt_coll_log        TYPE STANDARD TABLE OF gty_coll_log WITH EMPTY KEY,
  tt_bank_statement  TYPE STANDARD TABLE OF gty_bank_statement WITH EMPTY KEY.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  gc_true  TYPE  char1     VALUE 'X'.


*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  gt_coll_log       TYPE tt_coll_log        ##NEEDED,
  gt_bank_statement TYPE tt_bank_statement  ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
