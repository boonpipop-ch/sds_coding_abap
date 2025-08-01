class ZCL_SDSCO_IO_BUDGET_REMAIN_SRV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  methods PROCESS_DATA
    redefinition .
protected section.
private section.

  class-methods IO_AVAILABLE_BUDGET_GET
    importing
      !IS_REQUEST type ZSDSCOS008
    exporting
      !ES_RESPONSE type ZSDSCOS008 .
ENDCLASS.



CLASS ZCL_SDSCO_IO_BUDGET_REMAIN_SRV IMPLEMENTATION.


  METHOD io_available_budget_get.

    DATA: ls_io_in     TYPE zsdscos008,
          ls_io_out    TYPE zsdscos008,
*          ls_param     TYPE zsdscos001,
          lt_actual    TYPE zsdscos003_tt,
          lt_commit    TYPE zsdscos003_tt,
          lt_assigned  TYPE zsdscos003_tt,
          lt_plan      TYPE zsdscos003_tt,
          lt_available TYPE zsdscos003_tt,
          lt_return    TYPE zsdscos002_tt.

    MOVE-CORRESPONDING is_request TO ls_io_in.

    CALL FUNCTION 'Z_SDSCO_IO_AVAIL_SRV'
      EXPORTING
        im_k2_memo      = ls_io_in-param_in-k2_memo
        im_k2_bg_year   = ls_io_in-param_in-k2_bg_year
        im_k2_period_fr = ls_io_in-param_in-k2_period_fr
        im_k2_period_to = ls_io_in-param_in-k2_period_to
*      IMPORTING
*        es_param        = ls_param
      TABLES
        et_actual       = lt_actual
        et_commit       = lt_commit
        et_assigned     = lt_assigned
        et_plan         = lt_plan
        et_available    = lt_available
        et_return       = lt_return.

*...Return
    ls_io_out-param_in           =  ls_io_in-param_in.
    ls_io_out-param_out          =  ls_io_in-param_in.

    APPEND LINES OF lt_actual    TO ls_io_out-t_actual.
    APPEND LINES OF lt_commit    TO ls_io_out-t_commit.
    APPEND LINES OF lt_assigned  TO ls_io_out-t_assigned.
    APPEND LINES OF lt_plan      TO ls_io_out-t_plan.
    APPEND LINES OF lt_available TO ls_io_out-t_available.

    LOOP AT lt_return INTO DATA(ls_return).
      ls_io_out-resp_status     = ls_return-type.
      ls_io_out-resp_message    = ls_return-message.

      APPEND INITIAL LINE TO ls_io_out-response ASSIGNING FIELD-SYMBOL(<l_response>).
      <l_response>-resp_status  = ls_return-type.
      <l_response>-resp_message = ls_return-message.
    ENDLOOP.

    es_response = ls_io_out.

  ENDMETHOD.


  METHOD process_data.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSCO_IO_BUDGET_REMAIN_SRV
*  Creation Date      : 28.05.2024
*  Author             : Nadtaya T.(Eviden)
*  Add-on ID          : COE001
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

    FIELD-SYMBOLS: <l_response> TYPE zsdscos008.
    DATA:          ls_request   TYPE zsdscos008.

* Initialize Output
    CLEAR: ef_status,
           ef_message,
           ef_http_error.

    ls_request = iref_request_data->*.

    ASSIGN eref_response_data->* TO <l_response>.
    IF sy-subrc NE 0.
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
    io_available_budget_get( EXPORTING is_request  = ls_request
                             IMPORTING es_response = <l_response> ).

** Unlock Processing
*    unlock_processing( gs_config-intfno ).

* Assign Log status from Response structure
    IF <l_response>-resp_status EQ zcl_sdsca_rest_intf_utility=>gc_error.
*   Error at header level, Nothing success => HTTP error
      ef_status     = <l_response>-resp_status.
      ef_message    = <l_response>-resp_message.
      ef_http_error = 'X'.
    ELSE.
      ef_status     = <l_response>-resp_status.
      ef_message    = <l_response>-resp_message.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
