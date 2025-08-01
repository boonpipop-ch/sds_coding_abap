*-----------------------------------------------------------------------
*  Program ID         : Z_SDSSD_ASSIGN_ADVREC
*  Creation Date      : 04.07.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : SDE011
*  Description        : This is Function module for assigning advance
*                       receive amount to SO schedule lines
*  Purpose            : To assign advance Receive amount
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
FUNCTION Z_SDSSD_ASSIGN_ADVREC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IF_DISPLAY_ONLY) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(IF_DELETE_ALLOW) TYPE  FLAG DEFAULT ' '
*"  CHANGING
*"     REFERENCE(CT_DATA) TYPE
*"        ZCL_SDSSD_ORDER_CONFIRMATION=>TT_CONF_DATA
*"     REFERENCE(CT_ADVREC) TYPE
*"        ZCL_SDSSD_ORDER_CONFIRMATION=>TT_ADVREC_DATA
*"  EXCEPTIONS
*"      USER_CANCEL
*"----------------------------------------------------------------------

  DATA:
    LT_ITEM  TYPE  TT_ITEM_SORT.


* Initialize Variables
  PERFORM F_INITIALIZE_DATA.

* Assign Display Only flag
  GF_DISPLAY_ONLY = IF_DISPLAY_ONLY.
  GF_DELETE_ALLOW = IF_DELETE_ALLOW.

* Assign Data
  GT_DATA   = CT_DATA.

  CLEAR: LT_ITEM.

* --------------------
* Assign Screen Data
* --------------------
  GS_HEAD-ITEMS_COUNT   = LINES( GT_DATA ).
  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    GS_HEAD-ITEMS_AMOUNT = GS_HEAD-ITEMS_AMOUNT + <L_DATA>-AMOUNT.
    GS_HEAD-WAERS        = <L_DATA>-WAERS.
    GS_HEAD-UNASSIGN_AMOUNT = GS_HEAD-UNASSIGN_AMOUNT + ( <L_DATA>-AMOUNT - <L_DATA>-AMOUNT_ADV ).
    LOOP AT <L_DATA>-ADVREC ASSIGNING FIELD-SYMBOL(<L_ADVREC>).
      PERFORM F_COLLECT_ITEM  USING  <L_ADVREC>
                                     GC_TRUE
                            CHANGING LT_ITEM.
      GS_HEAD-ASSIGN_AMOUNT = GS_HEAD-ASSIGN_AMOUNT + <L_ADVREC>-DMBTR.
      GS_HEAD-TOTAL_ADVREC  = GS_HEAD-TOTAL_ADVREC  + <L_ADVREC>-DMBTR.
    ENDLOOP.
*   Clear existing assignment data, it will be assigned on save
    CLEAR: <L_DATA>-ADVREC,
           <L_DATA>-AMOUNT_ADV.
  ENDLOOP.

  LOOP AT CT_ADVREC ASSIGNING <L_ADVREC>.
    PERFORM F_COLLECT_ITEM  USING  <L_ADVREC>
                                   SPACE
                          CHANGING LT_ITEM.
    GS_HEAD-TOTAL_ADVREC = GS_HEAD-TOTAL_ADVREC + <L_ADVREC>-DMBTR.
  ENDLOOP.
  GS_HEAD-REMAIN_ADVREC = GS_HEAD-TOTAL_ADVREC - GS_HEAD-ASSIGN_AMOUNT.
  GT_ITEM = LT_ITEM.
* Sort for FIFO Assignment
  SORT GT_ITEM BY BUKRS ASCENDING
                  BUDAT ASCENDING
                  GJAHR ASCENDING
                  BELNR ASCENDING.
  PERFORM F_UPDATE_HEADER  USING  GT_ITEM
                         CHANGING GS_HEAD.

* --------------------
* Call Maintain Screen
* --------------------
  PERFORM F_CALL_MAINTENANCE_SCREEN USING GT_ITEM.
  IF GF_DISPLAY_ONLY IS INITIAL AND
     GF_CANCEL EQ GC_TRUE.
    RAISE USER_CANCEL.
  ENDIF.

* --------------------
* Update Output
* --------------------
  CT_DATA = GT_DATA.
  CT_ADVREC = GT_ADVREC.

ENDFUNCTION.
