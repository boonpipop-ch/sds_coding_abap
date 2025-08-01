class ZCL_SDSCA_INTERFACE_PROVIDER definition
  public
  inheriting from CL_REST_RESOURCE
  final
  create public .

public section.

  methods IF_REST_RESOURCE~GET
    redefinition .
  methods IF_REST_RESOURCE~POST
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSCA_INTERFACE_PROVIDER IMPLEMENTATION.


  method IF_REST_RESOURCE~GET.
*CALL METHOD SUPER->IF_REST_RESOURCE~GET
*    .
  endmethod.


METHOD IF_REST_RESOURCE~POST.

  DATA:
    LF_ENDPOINT  TYPE  ZSDSCAC005-ENDPOINT,
    LF_JSON      TYPE  XSTRING,
    LF_RESP_JSON TYPE  XSTRING.


* Get End Point
  DATA(LF_PATH) = IO_ENTITY->GET_HEADER_FIELD( IF_HTTP_HEADER_FIELDS_SAP=>PATH_INFO ).

* Determine Interface no for End Point
  LF_ENDPOINT = LF_PATH.

  SELECT INTFNO
    FROM ZSDSCAC005
   WHERE ENDPOINT EQ @LF_ENDPOINT
     AND ZDEL_FLG EQ @SPACE
    INTO @DATA(LF_INTFNO)
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
*   Error: Invalid Endpoint &1
    MESSAGE E023(ZSDSCA01) WITH LF_ENDPOINT.
    RETURN.
  ENDIF.

* Get Request JSON
  LF_JSON = IO_ENTITY->GET_BINARY_DATA( ).

  ZCL_SDSCA_REST_INTF_UTILITY=>CALL_REST_INTF(
    EXPORTING
      IF_INTFNO           = LF_INTFNO
      IF_REQUEST_JSON     = LF_JSON
      IF_SHOW_PROCESS_LOG = ' '
    IMPORTING
      EF_RESPONSE_JSON    = LF_RESP_JSON
    EXCEPTIONS
      INVALID_INTFNO      = 1
      ERROR_DATA_TO_JSON  = 2
      URL_ERROR           = 3
      SEND_ERROR          = 4
      REQNO_ERROR         = 5
      LOG_ERROR           = 6
      ERROR_JSON_TO_DATA  = 7
      PROCESSING_ERROR    = 8
      OTHERS              = 9 ).
  IF SY-SUBRC <> 0.
*   Error
    MESSAGE ID SY-MSGID TYPE SY-MSGTY
            NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2
                 SY-MSGV3 SY-MSGV4.
    RETURN.
  ENDIF.

* Return Response JSON
  DATA(LREF_ENTITY) =  MO_RESPONSE->CREATE_ENTITY( ).
  LREF_ENTITY->SET_BINARY_DATA( IV_DATA = LF_RESP_JSON ).
  LREF_ENTITY->SET_HEADER_FIELD( IV_NAME = 'Content-Type'
                                 IV_VALUE = 'application/json' ).

ENDMETHOD.
ENDCLASS.
