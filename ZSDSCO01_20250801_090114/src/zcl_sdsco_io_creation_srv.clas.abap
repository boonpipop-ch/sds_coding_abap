class ZCL_SDSCO_IO_CREATION_SRV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  methods PROCESS_DATA
    redefinition .
protected section.
private section.

  methods IO_CREATION
    importing
      !IS_REQUEST type ZSDSCOS007
    exporting
      !ES_RESPONSE type ZSDSCOS007 .
ENDCLASS.



CLASS ZCL_SDSCO_IO_CREATION_SRV IMPLEMENTATION.


  METHOD io_creation.

    DATA: ls_ordermaster TYPE zsdscos006,
          lt_planning    TYPE zsdscos004_tt,
          lt_return      TYPE bapiret2_t,
          lf_aufnr       TYPE aufk-aufnr.

    MOVE-CORRESPONDING is_request-ordermaster TO ls_ordermaster.
    lt_planning = is_request-planningtab.

    CALL FUNCTION 'Z_SDSCO_IO_CREATION'
      EXPORTING
        is_ordermaster = ls_ordermaster
      IMPORTING
        ex_aufnr       = lf_aufnr
      TABLES
        et_planning    = lt_planning
        et_return      = lt_return.

*...Return
    es_response       = is_request.
    es_response-aufnr = lf_aufnr.

    LOOP AT lt_return INTO DATA(ls_return).
      es_response-resp_status = ls_return-type.
      es_response-resp_message = ls_return-message.

      APPEND INITIAL LINE TO es_response-response ASSIGNING FIELD-SYMBOL(<l_response>).
      <l_response>-resp_status  = ls_return-type.
      <l_response>-resp_message = ls_return-message.
    ENDLOOP.

  ENDMETHOD.


METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSCO_IO_CREATION_SRV
*  Creation Date      : 28.05.2024
*  Author             : Nadtaya T.(Eviden)
*  Add-on ID          : IOI001
*  Description        :
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  FIELD-SYMBOLS: <L_RESPONSE> TYPE ZSDSCOS007.
  DATA:       LS_REQUEST   TYPE ZSDSCOS007.

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

** Lock for processing
** - Only 1 session for interface can be executed at a time
*    IF NOT lock_processing( gs_config-intfno ).
**   Error: The interface is currently processing other data. Please try again later.
*      ef_status     = zcl_sdsca_rest_intf_utility=>gc_error.
*      MESSAGE e028(zsdsca01) INTO ef_message.
*      ef_http_error = 'X'.
*      RETURN.
*    ENDIF.

* get remaining budget by Cost Element
  IO_CREATION( EXPORTING IS_REQUEST  = LS_REQUEST
                           IMPORTING ES_RESPONSE = <L_RESPONSE> ).

* Unlock Processing
*    unlock_processing( gs_config-intfno ).

** Assign Log status from Response structure
*    IF <l_response>-resp_status EQ zcl_sdsca_rest_intf_utility=>gc_error.
**   Error at header level, Nothing success => HTTP error
*      ef_status     = <l_response>-resp_status.
*      ef_message    = <l_response>-resp_message.
*      ef_http_error = 'X'.
*    ELSE.
*      ef_status     = <l_response>-resp_status.
*      ef_message    = <l_response>-resp_message.
*    ENDIF.

* Assign Log status from Response structure
  EF_STATUS     = <L_RESPONSE>-RESP_STATUS.
  EF_MESSAGE    = <L_RESPONSE>-RESP_MESSAGE.

ENDMETHOD.
ENDCLASS.
