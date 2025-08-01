*-----------------------------------------------------------------------
*  Program ID         : Z_SDSCM_UPDATE_WARRANTY_MST_EC
*  Creation Date      : 16.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : ZCME017
*  Description        : Update Installation/Commission date into
*                       warranty master table
*  Purpose            : Update Installation/Commission date into
*                       warranty master table
*                       - The function should be triggered only when
*                         changing of I1005(CMPD-Completed) status
*                       - The activation of function is done in table
*                         CRMV_EVENT_CUST and CRMV_FUNC_ASSIGN
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
FUNCTION Z_SDSCM_UPDATE_WARRANTY_MST_EC .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_HEADER_GUID) TYPE  CRMT_OBJECT_GUID_TAB
*"----------------------------------------------------------------------

  DATA:
    LT_ORDERADM_H TYPE CRMT_ORDERADM_H_WRKT,
    LT_CUSTOMER_H TYPE CRMT_CUSTOMER_H_WRKT,
    LT_ORDERADM_I TYPE CRMT_ORDERADM_I_WRKT,
    LT_ORGMAN     TYPE CRMT_ORGMAN_WRKT,
    LT_STATUS     TYPE CRMT_STATUS_WRKT    ##NEEDED,
    LT_REFOBJ     TYPE CRMT_REFOBJ_WRKT.

  DATA:
    LF_FOUND  TYPE  CHAR1.


* Initialize Variables
  CLEAR: GR_OBJID.

* Read Constants
  PERFORM F_GET_CONSTANTS.

* Reasd service order data
  CALL FUNCTION 'CRM_ORDER_READ'
    EXPORTING
      IT_HEADER_GUID       = IT_HEADER_GUID
    IMPORTING
      ET_ORDERADM_H        = LT_ORDERADM_H
      ET_ORDERADM_I        = LT_ORDERADM_I
      ET_ORGMAN            = LT_ORGMAN
      ET_STATUS            = LT_STATUS
      ET_REFOBJ            = LT_REFOBJ
      ET_CUSTOMER_H        = LT_CUSTOMER_H
    EXCEPTIONS
      DOCUMENT_NOT_FOUND   = 1
      ERROR_OCCURRED       = 2
      DOCUMENT_LOCKED      = 3
      NO_CHANGE_AUTHORITY  = 4
      NO_DISPLAY_AUTHORITY = 5
      NO_CHANGE_ALLOWED    = 6
      OTHERS               = 7.

  IF SY-SUBRC <> 0.
*   No data found
    RETURN.
  ENDIF.

  LOOP AT IT_HEADER_GUID ASSIGNING FIELD-SYMBOL(<L_HEADER_GUID>).

*   Get Sales Org
    READ TABLE LT_ORGMAN ASSIGNING FIELD-SYMBOL(<L_ORGMAN>)
                         WITH KEY GUID = <L_HEADER_GUID>.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

*   Only Activated SalesOrg
*   Check Sales Org activated for processing?
    IF NOT ( GR_ACTIVE_VKORG IS NOT INITIAL AND
             <L_ORGMAN>-SALES_ORG_SD IN GR_ACTIVE_VKORG ).
      CONTINUE.
    ENDIF.

*   Get Header data
    READ TABLE LT_ORDERADM_H ASSIGNING FIELD-SYMBOL(<L_ORDERADM_H>)
                             WITH KEY GUID = <L_HEADER_GUID>.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

*   Only Reference Equipment Found
    CLEAR LF_FOUND.

*   ----------------
*   Commissioning
*   ----------------
    IF GR_COMM IS NOT INITIAL AND
       <L_ORDERADM_H>-PROCESS_TYPE IN GR_COMM.
      LOOP AT LT_ORDERADM_I ASSIGNING FIELD-SYMBOL(<L_ORDERADM_I>)
                            WHERE HEADER EQ <L_HEADER_GUID>. "#EC CI_SORTSEQ
        LOOP AT LT_REFOBJ TRANSPORTING NO FIELDS
                          WHERE REF_GUID  EQ  <L_ORDERADM_I>-GUID
                            AND EQUIPMENT_ID IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF SY-SUBRC EQ 0.
          LF_FOUND = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

*   ----------------
*   Installation
*   ----------------
    ELSEIF GR_INST IS NOT INITIAL AND
           <L_ORDERADM_H>-PROCESS_TYPE IN GR_INST.
*     Get Ref Delivery Number
      READ TABLE LT_CUSTOMER_H ASSIGNING FIELD-SYMBOL(<L_CUSTOMER_H>)
                               WITH KEY GUID = <L_HEADER_GUID>.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
      IF <L_CUSTOMER_H>-ZZ1_DELIVERY_ORD IS INITIAL.
        CONTINUE.
      ENDIF.
*     Check Reference Equipment found?
      SELECT B~EQUNR
        FROM SER01 AS A
               INNER JOIN OBJK AS B
                 ON  B~OBKNR = A~OBKNR
       WHERE A~LIEF_NR EQ @<L_CUSTOMER_H>-ZZ1_DELIVERY_ORD
       ORDER BY B~OBKNR ASCENDING,
                B~OBZAE ASCENDING
        INTO @DATA(LF_EQUNR) ##NEEDED
          UP TO 1 ROWS.
      ENDSELECT.
      IF SY-SUBRC EQ 0.
        LF_FOUND = 'X'.
      ENDIF.

    ENDIF.

*   Only when Reference Equipment found
    IF LF_FOUND IS INITIAL.
      CONTINUE.
    ENDIF.

*   Collect Object ID
    INSERT VALUE #( SIGN = 'I'
                    OPTION = 'EQ'
                    LOW    = <L_ORDERADM_H>-OBJECT_ID )
           INTO TABLE GR_OBJID.

  ENDLOOP.

* Only when Criteria exist
  IF GR_OBJID IS INITIAL.
    RETURN.
  ENDIF.

* Create Background Job to update Warranty from Object ID
  PERFORM F_CREATE_BG_JOB ON COMMIT.

ENDFUNCTION.
