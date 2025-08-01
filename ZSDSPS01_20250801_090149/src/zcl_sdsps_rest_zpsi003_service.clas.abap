class ZCL_SDSPS_REST_ZPSI003_SERVICE definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  methods CHANGE_STATUS
    changing
      !CS_WBS_STATUS type ZSDSPSS009 .

  methods PROCESS_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSPS_REST_ZPSI003_SERVICE IMPLEMENTATION.


  METHOD CHANGE_STATUS.
    DATA: LS_RETURN TYPE BAPIRETURN1,
          LT_STATUS TYPE TABLE OF BAPI_WBS_MNT_USER_STATUS,
          LT_RETURN TYPE TABLE OF BAPIRET2.

    CHECK CS_WBS_STATUS-OBJECT[] IS NOT INITIAL.

    LOOP AT CS_WBS_STATUS-OBJECT ASSIGNING FIELD-SYMBOL(<L_OBJECT>).
      APPEND INITIAL LINE TO LT_STATUS ASSIGNING FIELD-SYMBOL(<L_STATUS>).
      <L_STATUS>-WBS_ELEMENT = <L_OBJECT>-WBS.
      CASE <L_OBJECT>-STATUSFLAG.
        WHEN 'T'.
          <L_STATUS>-SET_USER_STATUS = 'WTEC'.
        WHEN 'R'.
          <L_STATUS>-SET_USER_STATUS = 'REL'.
        WHEN OTHERS.
      ENDCASE.

      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
      CALL FUNCTION 'BAPI_BUS2054_SET_STATUS'
        IMPORTING
          RETURN            = LS_RETURN
        TABLES
*         I_WBS_SYSTEM_STATUS       =
          I_WBS_USER_STATUS = LT_STATUS
*         E_RESULT          = LT_RESULT
        .
      IF LS_RETURN-TYPE = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        <L_OBJECT>-RESPONSESTATUS  = LS_RETURN-TYPE.
        <L_OBJECT>-RESPONSEMESSAGE = LS_RETURN-MESSAGE.
      ELSE.
        CALL FUNCTION 'BAPI_PS_PRECOMMIT'
          TABLES
            ET_RETURN = LT_RETURN.
        READ TABLE LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>) WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          <L_OBJECT>-RESPONSESTATUS  = <L_RETURN>-TYPE.
          <L_OBJECT>-RESPONSEMESSAGE = <L_RETURN>-MESSAGE.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
          <L_OBJECT>-RESPONSESTATUS  = 'S'.
          <L_OBJECT>-RESPONSEMESSAGE = 'Status set for the object' ##NO_TEXT.
        ENDIF.

      ENDIF.

      CLEAR: LS_RETURN, LT_STATUS[].
*      UNASSIGN: <L_STATUS>.
    ENDLOOP.
*

*




  ENDMETHOD .


  METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSPS_REST_ZPSI003_SERVICE
*  Creation Date      : 05.06.2024
*  Author             : Atitep B.
*  Add-on ID          : ZPSI003
*  Description        : Interface Inbound to Request to Close (WTEC)/Cancel
*                       in SAP from Salesforce
*  Purpose            : WBS Close (WTEC)/Cancel
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA:
      LS_REQUEST  TYPE  ZSDSPSS009.

    FIELD-SYMBOLS:
      <L_RESPONSE>  TYPE  ZSDSPSS009.

* Initialize Output
    CLEAR: EF_STATUS,
           EF_MESSAGE,
           EF_HTTP_ERROR.

    LS_REQUEST = IREF_REQUEST_DATA->*.

    ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
    IF SY-SUBRC NE 0.
*   Critical error
      RETURN.
    ENDIF.

* Lock for processing
* - Only 1 session for interface can be executed at a time
    IF NOT LOCK_PROCESSING( ).
*   Error: The interface is currently processing other data. Please try again later.
      EF_STATUS     = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
      MESSAGE E028(ZSDSCA01) INTO EF_MESSAGE.
      EF_HTTP_ERROR = 'X'.
      RETURN.
    ENDIF.

    CHANGE_STATUS(
      CHANGING
        CS_WBS_STATUS = LS_REQUEST ).

* Unlock Processing
    UNLOCK_PROCESSING( ).
    <L_RESPONSE> = LS_REQUEST.
    EF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_SUCCESS.
    EF_MESSAGE = 'Successfully' ##NO_TEXT.

*    EF_STATUS  = LS_REQUEST-RESPONSESTATUS.
*    EF_MESSAGE = LS_REQUEST-RESPONSEMESSAGE.
** Maintain Project Structure
*  MAINTAIN_PROJECT_STRUCTURE(
*    EXPORTING
*      IS_REQUEST = LS_REQUEST
*    IMPORTING
*      ES_RESPONSE = <L_RESPONSE> ).
*
** Unlock Processing
*  UNLOCK_PROCESSING( ).
*
** Assign Log status from Response structure
*  IF <L_RESPONSE>-RESP_STATUS EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
**   Error at header level, Nothing success => HTTP error
*    EF_STATUS  = <L_RESPONSE>-RESP_STATUS.
*    EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
*    EF_HTTP_ERROR = 'X'.
*  ELSE.
*    LOOP AT <L_RESPONSE>-WBSELEM ASSIGNING FIELD-SYMBOL(<L_WBSELEM>)
*                                 WHERE RESP_STATUS EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
**     Error at item level, Something sucess => Must not HTTP error
*      EF_STATUS  = <L_WBSELEM>-RESP_STATUS.
*      EF_MESSAGE = <L_WBSELEM>-RESP_MESSAGE.
*      EXIT.
*    ENDLOOP.
*    IF SY-SUBRC NE 0.
*      EF_STATUS  = <L_RESPONSE>-RESP_STATUS.
*      EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
*    ENDIF.
*  ENDIF.

  ENDMETHOD.
ENDCLASS.
