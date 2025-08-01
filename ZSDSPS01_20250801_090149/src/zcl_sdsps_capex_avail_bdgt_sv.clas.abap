class ZCL_SDSPS_CAPEX_AVAIL_BDGT_SV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  methods PROCESS_DATA
    redefinition .
  methods INITIALIZE_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSPS_CAPEX_AVAIL_BDGT_SV IMPLEMENTATION.


 METHOD INITIALIZE_DATA.
   GF_FNAME_STATUS = 'RESPONSESTATUS'.
   GF_FNAME_MESSAGE =  'RESPONSEMESSAGE'.
 ENDMETHOD.


  METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_CAPEX_AVAIL_BDGT_SV
*  Creation Date      : 24.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : PSE002
*  Description        : CAPEX budget data service
*  Purpose            : Inbound interface for CAPEX Available Budget
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
    LS_REQUEST  TYPE ZSDSPSS004.


    FIELD-SYMBOLS:
    <L_RESPONSE> TYPE ZSDSPSS004.


* Initialize Output
    CLEAR: EF_STATUS,
           EF_MESSAGE,
           EF_HTTP_ERROR.

    LS_REQUEST = IREF_REQUEST_DATA->*.

    ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.

    CALL METHOD ZCL_SDSPS_CAPEX_AVAIL_BDGT=>RETRIEVE_CAPEX_DATA
      CHANGING
        CS_DATA = LS_REQUEST.


    <L_RESPONSE> = LS_REQUEST.


* Assign Result to Log
    EF_STATUS  = <L_RESPONSE>-RESPONSESTATUS.
    EF_MESSAGE = <L_RESPONSE>-RESPONSEMESSAGE.
  ENDMETHOD.
ENDCLASS.
