class ZCL_SDSCO_CC_BUDGET_REMAIN_SRV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  methods PROCESS_DATA
    redefinition .
protected section.
private section.

  class-methods CE_DBGET
    importing
      !IS_REQUEST type ZSDSCOS009
    exporting
      !ES_RESPONSE type ZSDSCOS009 .
ENDCLASS.



CLASS ZCL_SDSCO_CC_BUDGET_REMAIN_SRV IMPLEMENTATION.


  METHOD ce_dbget.

    DATA: ls_cca_in  TYPE zsdscos009,
          ls_cca_out TYPE zsdscos009,
*          ls_cca     TYPE zsdscos001,
          lt_data    TYPE zsdscos001_tt,
          lt_return  TYPE zsdscos002_tt.

    MOVE-CORRESPONDING is_request TO ls_cca_in.

*..Process request data
    CALL FUNCTION 'Z_SDSCO_CCA_REMAIN_BG_SRV'
      EXPORTING
        im_k2_resp_cca  = ls_cca_in-param_in-k2_resp_cca
        im_k2_bg_year   = ls_cca_in-param_in-k2_bg_year
        im_k2_period_fr = ls_cca_in-param_in-k2_period_fr
        im_k2_period_to = ls_cca_in-param_in-k2_period_to
        im_k2_gl        = ls_cca_in-param_in-k2_gl
*      IMPORTING
*        es_param        = ls_cca
      TABLES
        et_data         = lt_data
        et_return       = lt_return.

*...Return
    ls_cca_out-param_in  = ls_cca_in-param_in.
    ls_cca_out-param_out = ls_cca_in-param_in.
    APPEND LINES OF lt_data TO ls_cca_out-t_data.

    LOOP AT lt_return INTO DATA(ls_return).
      ls_cca_out-resp_status = ls_return-type.
      ls_cca_out-resp_message = ls_return-message.

      APPEND INITIAL LINE TO ls_cca_out-response ASSIGNING FIELD-SYMBOL(<l_response>).
      <l_response>-resp_status  = ls_return-type.
      <l_response>-resp_message = ls_return-message.
    ENDLOOP.

    es_response = ls_cca_out.

  ENDMETHOD.


  METHOD process_data.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSCO_CC_BUDGET_REMAIN_SRV
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

    FIELD-SYMBOLS: <l_response> TYPE zsdscos009.
    DATA:          ls_request   TYPE zsdscos009.

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

* Lock for processing
* - Only 1 session for interface can be executed at a time
*    IF NOT lock_processing( gs_config-intfno ).
**   Error: The interface is currently processing other data. Please try again later.
*      ef_status     = zcl_sdsca_rest_intf_utility=>gc_error.
*      MESSAGE e028(zsdsca01) INTO ef_message.
*      ef_http_error = 'X'.
*      RETURN.
*    ENDIF.

* get remaining budget by Cost Element
    CALL METHOD zcl_sdsco_cc_budget_remain_srv=>ce_dbget
      EXPORTING
        is_request  = ls_request
      IMPORTING
        es_response = <l_response>.

* Unlock Processing
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
