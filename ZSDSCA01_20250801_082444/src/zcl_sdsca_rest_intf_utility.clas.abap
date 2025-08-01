class ZCL_SDSCA_REST_INTF_UTILITY definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TS_LOG,
        USNAM         TYPE  ZSDSCAT001-USNAM,
        STDAT         TYPE  ZSDSCAT001-STDAT,
        STTIM         TYPE  ZSDSCAT001-STTIM,
        ENDAT         TYPE  ZSDSCAT001-ENDAT,
        ENTIM         TYPE  ZSDSCAT001-ENTIM,
        REQUEST_JSON  TYPE  ZSDSCAT001-REQUEST_JSON,
        RESPONSE_JSON TYPE  ZSDSCAT001-RESPONSE_JSON,
        HTTP_CODE     TYPE  ZSDSCAT001-HTTP_CODE,
        HTTP_REASON   TYPE  ZSDSCAT001-HTTP_REASON,
        HTML_ERROR    TYPE  ZSDSCAT001-HTML_ERROR,
        KEYDATA       TYPE  ZSDSCAT001-KEYDATA,
        STATUS        TYPE  ZSDSCAT001-STATUS,
        MESSAGE       TYPE  ZSDSCAT001-MESSAGE,
      END OF TS_LOG .
  types TS_INTF_CONFIG type ZSDSCAC005 .
  types TS_REQUEST_KEY type ZSDSCAS005 .
  types:
    BEGIN OF TS_TTZZ,
             TZONE    TYPE  TTZZ-TZONE,
             ZONERULE TYPE  TTZZ-ZONERULE,
             UTCSIGN  TYPE  TTZR-UTCSIGN,
             UTCDIFF  TYPE  TTZR-UTCDIFF,
           END OF TS_TTZZ .

  constants GC_REQUEST type CHAR1 value '1' ##NO_TEXT.
  constants GC_RESPONSE type CHAR1 value '2' ##NO_TEXT.
  constants GC_JSON type CHAR1 value '1' ##NO_TEXT.
  constants GC_HTML type CHAR1 value '2' ##NO_TEXT.
  constants GC_OUTBOUND type ZSDSDE_INTTY value 'O' ##NO_TEXT.
  constants GC_INBOUND type ZSDSDE_INTTY value 'I' ##NO_TEXT.
  constants GC_SUCCESS type ZSDSDE_REST_STATUS value 'S' ##NO_TEXT.
  constants GC_ERROR type ZSDSDE_REST_STATUS value 'E' ##NO_TEXT.

  class-methods CALL_REST_INTF
    importing
      !IF_INTFNO type ZSDSDE_INTFNO
      !IS_REQUEST type ANY optional
      !IF_REQUEST_JSON type XSTRING optional
      !IF_KEYDATA type ZSDSDE_KEYDATA optional
      !IF_SHOW_PROCESS_LOG type FLAG default ' '
    exporting
      !ES_RESPONSE type ANY
      !EF_RESPONSE_JSON type XSTRING
      !ES_REQUEST_KEY type ZSDSCAS005
    exceptions
      INVALID_INTFNO
      ERROR_DATA_TO_JSON
      URL_ERROR
      SEND_ERROR
      REQNO_ERROR
      LOG_ERROR
      ERROR_JSON_TO_DATA
      PROCESSING_ERROR .
  class-methods ADD_LOG_REST_INTF
    importing
      !IF_INTFNO type ZSDSDE_INTFNO
      !IS_REQUEST type ANY optional
      !IF_REQUEST_JSON type XSTRING optional
      !IF_KEYDATA type ZSDSDE_KEYDATA optional
      !IF_STATUS type ZSDSDE_REST_STATUS
      !IF_MESSAGE type ZSDSDE_REST_MESSAGE
    exporting
      !ES_REQUEST_KEY type ZSDSCAS005
    exceptions
      INVALID_STATUS
      INVALID_INTFNO
      ERROR_DATA_TO_JSON
      REQNO_ERROR
      LOG_ERROR
      ERROR_JSON_TO_DATA .
  class-methods READ_CONFIGURATION
    importing
      !IF_INTFNO type ZSDSDE_INTFNO
    returning
      value(RS_CONFIG) type TS_INTF_CONFIG .
  class-methods CONVERT_DATA_TO_JSON
    importing
      !IF_INTFNO type ZSDSDE_INTFNO
      !IF_TYPE type CHAR1 default '1'
      !IS_DATA type ANY
      !IF_SHOW_JSON_POPUP type FLAG default ' '
    exporting
      !EF_JSON type ZSDSDE_JSON
    exceptions
      INVALID_INTFNO
      TRANSFORM_NOT_FOUND
      TRANSFORM_ERROR .
  class-methods CONVERT_JSON_TO_DATA
    importing
      !IF_INTFNO type ZSDSDE_INTFNO
      !IF_TYPE type CHAR1 default '1'
      !IF_JSON type ZSDSDE_JSON
    exporting
      !ES_DATA type ANY
    exceptions
      INVALID_INTFNO
      TRANSFORM_NOT_FOUND
      TRANSFORM_ERROR .
  class-methods CONVERT_DATE_TIME_TO_ISO
    importing
      !IF_TIMESTAMP type TIMESTAMP optional
      !IF_DATUM type DATUM optional
      !IF_UZEIT type UZEIT optional
      !IF_TIMEZONE type TIMEZONE optional
    exporting
      !EF_OUTPUT type CLIKE .
  class-methods CONVERT_ISO_TO_DATE_TIME
    importing
      !IF_INPUT type CLIKE
      !IF_TIMEZONE type TIMEZONE default 'UTC+8'
    exporting
      !EF_DATUM type DATUM
      !EF_UZEIT type UZEIT
      !EF_TIMESTAMP type TIMESTAMP
    exceptions
      CONVERSION_ERROR .
protected section.
private section.

  class-data GF_POPUP type FLAG .
  class-data GREF_PROCESS type ref to ZCL_SDSCA_REST_SERVICE .

  class-methods GET_TIMEZONE
    importing
      !IF_UTCDIFF type TS_TTZZ-UTCDIFF
      !IF_UTCSIGN type TS_TTZZ-UTCSIGN
    exporting
      !EF_TIMEZONE type TIMEZONE .
  class-methods GET_TIMEZONE_DETAIL
    importing
      !IF_TIMEZONE type TIMEZONE
    exporting
      !ES_TTZZ type TS_TTZZ .
  class-methods GET_PROCESSING_INSTANCE
    importing
      !IS_CONFIG type TS_INTF_CONFIG
    returning
      value(RREF_PROCESS) type ref to ZCL_SDSCA_REST_SERVICE .
  class-methods TRANSFORM_DATA_TO_JSON
    importing
      !IF_TRANSFORM type ZSDSDE_TRANSFORM
      !IS_DATA type ANY
    exporting
      !EF_JSON type ZSDSDE_JSON
    exceptions
      TRANSFORM_ERROR .
  class-methods TRANSFORM_JSON_TO_DATA
    importing
      !IF_TRANSFORM type ZSDSDE_TRANSFORM
      !IF_JSON type ZSDSDE_JSON
    exporting
      !ES_DATA type ANY
    exceptions
      TRANSFORM_ERROR .
  class-methods SHOW_POPUP
    importing
      !IS_DATA type ANY
      !IF_TYPE type CHAR1
      !IF_SECTION type CLIKE .
  class-methods CONSUME_REST_API
    importing
      !IS_CONFIG type TS_INTF_CONFIG
      !IF_REQUEST_JSON type ZSDSDE_JSON
    exporting
      !EF_RESPONSE_JSON type ZSDSDE_JSON
      !EF_HTTP_CODE type TS_LOG-HTTP_CODE
      !EF_HTTP_REASON type TS_LOG-HTTP_REASON
      !EF_HTML_ERROR type TS_LOG-HTML_ERROR
    exceptions
      URL_ERROR
      SEND_ERROR .
  class-methods CALL_PROCESS_METHOD
    importing
      !IS_CONFIG type TS_INTF_CONFIG
      !IF_REQUEST_JSON type ZSDSDE_JSON
    exporting
      !EF_RESPONSE_JSON type ZSDSDE_JSON
      !EF_STATUS type ZSDSDE_REST_STATUS
      !EF_MESSAGE type ZSDSDE_REST_MESSAGE
      !EF_HTTP_ERROR type FLAG .
  class-methods SAVE_PROCESSING_LOG
    importing
      !IS_CONFIG type TS_INTF_CONFIG
      !IS_LOG type TS_LOG
    exporting
      !ES_KEY type TS_REQUEST_KEY
    exceptions
      REQNO_ERROR
      LOG_ERROR .
  class-methods ASSIGN_KEYDATA
    importing
      !IF_JSON type ZSDSDE_JSON
      !IS_CONFIG type TS_INTF_CONFIG
    exporting
      !EF_VALUE type ZSDSDE_KEYDATA .
  class-methods READ_FIELD_VALUE
    importing
      !IF_FIELD type CLIKE
      !IS_DATA type ANY   ##NEEDED
    exporting
      !EF_VALUE type CLIKE .
  class-methods ASSIGN_STATUS
    importing
      !IF_JSON type ZSDSDE_JSON
      !IS_CONFIG type TS_INTF_CONFIG
    exporting
      !EF_STATUS type ZSDSDE_REST_STATUS
      !EF_MESSAGE type ZSDSDE_REST_MESSAGE .
ENDCLASS.



CLASS ZCL_SDSCA_REST_INTF_UTILITY IMPLEMENTATION.


METHOD ADD_LOG_REST_INTF.

  DATA:
    LS_LOG    TYPE  TS_LOG.


* Initialize Output
  CLEAR: ES_REQUEST_KEY.

  IF IF_STATUS NE GC_SUCCESS AND
     IF_STATUS NE GC_ERROR.
*   Error: Invalid status. Only S or E can be used.
    MESSAGE E031(ZSDSCA01) RAISING INVALID_STATUS.
    RETURN.
  ENDIF.

* Read Interface Configuration
  DATA(LS_CONFIG) = READ_CONFIGURATION(
    EXPORTING
      IF_INTFNO = IF_INTFNO ).
  IF LS_CONFIG IS INITIAL.
*   Error: Invalid Interface number.
    MESSAGE E013(ZSDSCA01) RAISING INVALID_INTFNO.
    RETURN.
  ENDIF.

* Log Processing Time
  CLEAR LS_LOG.
  LS_LOG-USNAM = SY-UNAME.
  LS_LOG-STDAT = SY-DATUM.
  LS_LOG-STTIM = SY-UZEIT.

  IF IF_REQUEST_JSON IS INITIAL.
*   Request Missing
    IF IS_REQUEST IS NOT INITIAL.
*     Convert Request Into JSON String
      CONVERT_DATA_TO_JSON(
        EXPORTING
          IF_INTFNO           = LS_CONFIG-INTFNO
          IF_TYPE             = GC_REQUEST
          IS_DATA             = IS_REQUEST
        IMPORTING
          EF_JSON             = LS_LOG-REQUEST_JSON
        EXCEPTIONS
          INVALID_INTFNO      = 1
          TRANSFORM_NOT_FOUND = 2
          TRANSFORM_ERROR     = 3
          OTHERS              = 4 ).
      IF SY-SUBRC <> 0.
*       Error: Error on DATA to JSON transform.
        MESSAGE E014(ZSDSCA01) RAISING ERROR_DATA_TO_JSON.
        RETURN.
      ENDIF.
    ENDIF.
  ELSE.
    LS_LOG-REQUEST_JSON = IF_REQUEST_JSON.
  ENDIF.

* Update Processing Time
  LS_LOG-ENDAT = SY-DATUM.
  LS_LOG-ENTIM = SY-UZEIT.

* Update Error Log
  LS_LOG-KEYDATA = IF_KEYDATA.
  LS_LOG-STATUS  = IF_STATUS.
  LS_LOG-MESSAGE = IF_MESSAGE.

* Save Processing Log
  SAVE_PROCESSING_LOG(
    EXPORTING
      IS_CONFIG = LS_CONFIG
      IS_LOG    = LS_LOG
    IMPORTING
      ES_KEY    = ES_REQUEST_KEY ).

ENDMETHOD.


METHOD ASSIGN_KEYDATA.

  DATA:
    LREF_DATA  TYPE  REF TO DATA.

  DATA:
    LT_FIELD  TYPE  STANDARD TABLE OF STRING.

  DATA:
    LF_FIRST TYPE  FLAG,
    LF_VALUE TYPE  TEXT1000.

  FIELD-SYMBOLS:
    <L_DATA>  TYPE  ANY.


* Initialize Output
  CLEAR EF_VALUE.

* Create Data object
  CREATE DATA LREF_DATA TYPE (IS_CONFIG-REQUEST_STRUC).
  IF LREF_DATA IS INITIAL.
    RETURN.
  ENDIF.

* Assign Data
  ASSIGN LREF_DATA->* TO <L_DATA>.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Convert JSON to Data
  CONVERT_JSON_TO_DATA(
    EXPORTING
      IF_INTFNO = IS_CONFIG-INTFNO
      IF_TYPE   = GC_REQUEST
      IF_JSON   = IF_JSON
    IMPORTING
      ES_DATA   = <L_DATA>
    EXCEPTIONS
      INVALID_INTFNO      = 1
      TRANSFORM_NOT_FOUND = 2
      TRANSFORM_ERROR     = 3
      OTHERS              = 4 ).
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Only when format specified
  IF IS_CONFIG-KEYDATA IS INITIAL.
*   Get Processing Class Instance
    DATA(LREF_PROCESS) = GET_PROCESSING_INSTANCE( IS_CONFIG ).
    LREF_PROCESS->GET_KEYDATA(
      EXPORTING
        IS_DATA = <L_DATA>
      IMPORTING
        EF_VALUE = EF_VALUE ).

  ELSE.

*   Split Field name
    SPLIT IS_CONFIG-KEYDATA AT '&&' INTO TABLE LT_FIELD.

*   Process Each field
    LOOP AT LT_FIELD ASSIGNING FIELD-SYMBOL(<L_FIELD>).

      AT FIRST.
        LF_FIRST = 'X'.
      ENDAT.

      READ_FIELD_VALUE(
        EXPORTING
          IF_FIELD = <L_FIELD>
          IS_DATA  = <L_DATA>
        IMPORTING
          EF_VALUE = LF_VALUE ).

      IF LF_FIRST EQ 'X'.
        EF_VALUE = LF_VALUE.
      ELSE.
        CONCATENATE EF_VALUE LF_VALUE
               INTO EF_VALUE.
      ENDIF.

      CLEAR LF_FIRST.

    ENDLOOP.
  ENDIF.

  CONDENSE EF_VALUE.

ENDMETHOD.


METHOD ASSIGN_STATUS.

  DATA:
    LREF_DATA  TYPE REF TO DATA.

  FIELD-SYMBOLS:
    <L_DATA>  TYPE  ANY.


* Initialize Output
  CLEAR: EF_STATUS,
         EF_MESSAGE.

  IF IF_JSON IS INITIAL.
    RETURN.
  ENDIF.

* Create Data object
  CREATE DATA LREF_DATA TYPE (IS_CONFIG-RESPONSE_STRUC).
  IF LREF_DATA IS INITIAL.
    RETURN.
  ENDIF.

* Assign Data
  ASSIGN LREF_DATA->* TO <L_DATA>.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Convert JSON to Data
  CONVERT_JSON_TO_DATA(
    EXPORTING
      IF_INTFNO = IS_CONFIG-INTFNO
      IF_TYPE   = GC_RESPONSE
      IF_JSON   = IF_JSON
    IMPORTING
      ES_DATA   = <L_DATA>
    EXCEPTIONS
      INVALID_INTFNO      = 1
      TRANSFORM_NOT_FOUND = 2
      TRANSFORM_ERROR     = 3
      OTHERS              = 4 ).
  IF SY-SUBRC <> 0.
    EF_STATUS = GC_ERROR.
*   Text-e03: Response cannot transform
    EF_MESSAGE = TEXT-E03.
    RETURN.
  ENDIF.

* Get Processing Class Instance
  DATA(LREF_PROCESS) = GET_PROCESSING_INSTANCE( IS_CONFIG ).
  LREF_PROCESS->GET_STATUS(
    EXPORTING
      IS_DATA = <L_DATA>
    IMPORTING
      EF_STATUS = EF_STATUS
      EF_MESSAGE = EF_MESSAGE ).

ENDMETHOD.


METHOD CALL_PROCESS_METHOD.

  DATA:
    LREF_PROCESS TYPE REF TO ZCL_SDSCA_REST_SERVICE,
    LREF_DATA    TYPE REF TO DATA,
    LREF_RESP    TYPE REF TO DATA.

  DATA:
    LF_ASSIGNED  TYPE  FLAG.

  FIELD-SYMBOLS:
    <L_DATA> TYPE  ANY,
    <L_RESP> TYPE  ANY.


* Initialize Output
  CLEAR: EF_RESPONSE_JSON,
         EF_STATUS,
         EF_MESSAGE,
         EF_HTTP_ERROR.

  DO 1 TIMES.
    TRY.

*       Get Processing Class Instance
        LREF_PROCESS = GET_PROCESSING_INSTANCE( IS_CONFIG ).

*       Create Response Object
        IF IS_CONFIG-RESPONSE_STRUC IS NOT INITIAL.
          CREATE DATA LREF_RESP TYPE (IS_CONFIG-RESPONSE_STRUC).
          ASSIGN LREF_RESP->* TO <L_RESP>.
          IF SY-SUBRC NE 0.
*           Critical Error
            EXIT.
          ENDIF.
        ENDIF.

*       Create Request Object
        CREATE DATA LREF_DATA TYPE (IS_CONFIG-REQUEST_STRUC).
        ASSIGN LREF_DATA->* TO <L_DATA>.
        IF SY-SUBRC NE 0.
*         Text-e01: Error on request structure.
          EF_STATUS = GC_ERROR.
          EF_MESSAGE = TEXT-E01.
          LREF_PROCESS->HANDLING_ERROR(
            EXPORTING
              IREF_RESPONSE_DATA = LREF_RESP
              IF_STATUS          = EF_STATUS
              IF_MESSAGE         = EF_MESSAGE
            IMPORTING
              EF_ASSIGNED        = LF_ASSIGNED ).
*         If message not assigned to response, use HTTP error
          IF LF_ASSIGNED IS INITIAL.
            EF_HTTP_ERROR = 'X'.
          ENDIF.
          EXIT.
        ENDIF.

*       Convert JSON to Data
        CONVERT_JSON_TO_DATA(
          EXPORTING
            IF_INTFNO = IS_CONFIG-INTFNO
            IF_TYPE   = GC_REQUEST
            IF_JSON   = IF_REQUEST_JSON
          IMPORTING
            ES_DATA   = <L_DATA>
          EXCEPTIONS
            INVALID_INTFNO      = 1
            TRANSFORM_NOT_FOUND = 2
            TRANSFORM_ERROR     = 3
            OTHERS              = 4 ).
        IF SY-SUBRC <> 0.
          EF_STATUS = GC_ERROR.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY
                  NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2
                       SY-MSGV3 SY-MSGV4
                  INTO EF_MESSAGE.
          IF EF_MESSAGE IS INITIAL.
*           Text-e02: Error during transform request data.
            EF_MESSAGE = TEXT-E02.
          ENDIF.
          LREF_PROCESS->HANDLING_ERROR(
            EXPORTING
              IREF_RESPONSE_DATA = LREF_RESP
              IF_STATUS          = EF_STATUS
              IF_MESSAGE         = EF_MESSAGE
            IMPORTING
              EF_ASSIGNED        = LF_ASSIGNED ).
*         If message not assigned to response, use HTTP error
          IF LF_ASSIGNED IS INITIAL.
            EF_HTTP_ERROR = 'X'.
          ENDIF.
          EXIT.
        ENDIF.

*       Call Processing Function
        LREF_PROCESS->PROCESS_DATA(
          EXPORTING
            IREF_REQUEST_DATA = LREF_DATA
          IMPORTING
            EREF_RESPONSE_DATA = LREF_RESP
            EF_STATUS          = EF_STATUS
            EF_MESSAGE         = EF_MESSAGE
            EF_HTTP_ERROR      = EF_HTTP_ERROR ).

      CATCH CX_ROOT INTO DATA(LREF_ERROR) ##CATCH_ALL.
        DATA(LF_LONGTEXT) = LREF_ERROR->IF_MESSAGE~GET_LONGTEXT( PRESERVE_NEWLINES = ' ' ) ##NEEDED.
        DATA(LF_TEXT)     = LREF_ERROR->IF_MESSAGE~GET_TEXT( ) ##NEEDED.
*       Text-e02: Error during transform request data.
        EF_STATUS = GC_ERROR.
        EF_MESSAGE = TEXT-E02.
        LREF_PROCESS->HANDLING_ERROR(
          EXPORTING
            IREF_RESPONSE_DATA = LREF_RESP
            IF_STATUS          = EF_STATUS
            IF_MESSAGE         = EF_MESSAGE
            IMPORTING
              EF_ASSIGNED        = LF_ASSIGNED ).
*       If message not assigned to response, use HTTP error
        IF LF_ASSIGNED IS INITIAL.
          EF_HTTP_ERROR = 'X'.
        ENDIF.
        EXIT.
    ENDTRY.

  ENDDO.

  IF <L_RESP> IS ASSIGNED.
*   Convert Response to JSON
    CONVERT_DATA_TO_JSON(
      EXPORTING
        IF_INTFNO = IS_CONFIG-INTFNO
        IF_TYPE   = GC_RESPONSE
        IS_DATA   = <L_RESP>
        IF_SHOW_JSON_POPUP = SPACE
      IMPORTING
        EF_JSON   = EF_RESPONSE_JSON
      EXCEPTIONS
        INVALID_INTFNO      = 1
        TRANSFORM_NOT_FOUND = 2
        TRANSFORM_ERROR     = 3
        OTHERS              = 4 ).
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY
              NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2
                   SY-MSGV3 SY-MSGV4.
      RETURN.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD CALL_REST_INTF.

  DATA:
    LS_LOG    TYPE  TS_LOG.

  DATA:
    LF_HTTP_ERROR  TYPE  FLAG.


* Initialize Output
  CLEAR: ES_RESPONSE,
         EF_RESPONSE_JSON,
         ES_REQUEST_KEY.

* Assign Popup Flag
  GF_POPUP = IF_SHOW_PROCESS_LOG.

* Read Interface Configuration
  DATA(LS_CONFIG) = READ_CONFIGURATION(
    EXPORTING
      IF_INTFNO = IF_INTFNO ).
  IF LS_CONFIG IS INITIAL.
*   Error: Invalid Interface number.
    MESSAGE E013(ZSDSCA01) RAISING INVALID_INTFNO.
    RETURN.
  ENDIF.

* Log Processing Time
  CLEAR LS_LOG.
  LS_LOG-USNAM = SY-UNAME.
  LS_LOG-STDAT = SY-DATUM.
  LS_LOG-STTIM = SY-UZEIT.

  IF IF_REQUEST_JSON IS INITIAL.
*   Request Missing
    IF IS_REQUEST IS NOT INITIAL.
*     Convert Request Into JSON String
      CONVERT_DATA_TO_JSON(
        EXPORTING
          IF_INTFNO           = LS_CONFIG-INTFNO
          IF_TYPE             = GC_REQUEST
          IS_DATA             = IS_REQUEST
        IMPORTING
          EF_JSON             = LS_LOG-REQUEST_JSON
        EXCEPTIONS
          INVALID_INTFNO      = 1
          TRANSFORM_NOT_FOUND = 2
          TRANSFORM_ERROR     = 3
          OTHERS              = 4 ).
      IF SY-SUBRC <> 0.
*       Error: Error on DATA to JSON transform.
        MESSAGE E014(ZSDSCA01) RAISING ERROR_DATA_TO_JSON.
        RETURN.
      ENDIF.
    ENDIF.
  ELSE.
    LS_LOG-REQUEST_JSON = IF_REQUEST_JSON.
  ENDIF.

  CASE LS_CONFIG-INTTY.
*   ----------------------------------
*   Outbound Processing (Service Consumption)
*   ----------------------------------
    WHEN GC_OUTBOUND.
*     Consume REST API
      CONSUME_REST_API(
        EXPORTING
          IS_CONFIG        = LS_CONFIG
          IF_REQUEST_JSON  = LS_LOG-REQUEST_JSON
        IMPORTING
          EF_RESPONSE_JSON = LS_LOG-RESPONSE_JSON
          EF_HTTP_CODE     = LS_LOG-HTTP_CODE
          EF_HTTP_REASON   = LS_LOG-HTTP_REASON
          EF_HTML_ERROR    = LS_LOG-HTML_ERROR ).

*   ----------------------------------
*   Inbound Processing (Service Provider)
*   ----------------------------------
    WHEN GC_INBOUND.
      CALL_PROCESS_METHOD(
        EXPORTING
          IS_CONFIG        = LS_CONFIG
          IF_REQUEST_JSON  = LS_LOG-REQUEST_JSON
        IMPORTING
          EF_RESPONSE_JSON = LS_LOG-RESPONSE_JSON
          EF_STATUS        = LS_LOG-STATUS
          EF_MESSAGE       = LS_LOG-MESSAGE
          EF_HTTP_ERROR    = LF_HTTP_ERROR ).

  ENDCASE.

* Update Processing Time
  LS_LOG-ENDAT = SY-DATUM.
  LS_LOG-ENTIM = SY-UZEIT.

* Assgin HTTP Error code
  IF LF_HTTP_ERROR EQ 'X' AND
     LS_LOG-STATUS EQ GC_ERROR.
    LS_LOG-HTTP_CODE = '500'.
*   HTTP Error, no JSON Response to caller
    CLEAR LS_LOG-RESPONSE_JSON.
  ENDIF.

* Assign Specific KEYDATA for the calling
  IF IF_KEYDATA IS NOT INITIAL.
    LS_LOG-KEYDATA = IF_KEYDATA.
  ENDIF.

* Save Processing Log
  SAVE_PROCESSING_LOG(
    EXPORTING
      IS_CONFIG = LS_CONFIG
      IS_LOG    = LS_LOG
    IMPORTING
      ES_KEY    = ES_REQUEST_KEY ).

* Return Output JSON
  EF_RESPONSE_JSON = LS_LOG-RESPONSE_JSON.

* Return Output Data
  IF ES_RESPONSE IS SUPPLIED.
    CONVERT_JSON_TO_DATA(
      EXPORTING
        IF_INTFNO           = LS_CONFIG-INTFNO
        IF_TYPE             = GC_RESPONSE
        IF_JSON             = EF_RESPONSE_JSON
      IMPORTING
        ES_DATA             = ES_RESPONSE
      EXCEPTIONS
        INVALID_INTFNO      = 1
        TRANSFORM_NOT_FOUND = 2
        TRANSFORM_ERROR     = 3
        OTHERS              = 4 ).
    IF SY-SUBRC <> 0.
*     Error: Error on JSON to DATA transform.
      MESSAGE E022(ZSDSCA01) RAISING ERROR_JSON_TO_DATA.
      RETURN.
    ENDIF.

  ENDIF.

* Raise HTTP Error?
  IF LF_HTTP_ERROR EQ 'X' AND
     LS_LOG-STATUS EQ GC_ERROR.
*   Raise Error message to generate HTTP error
    MESSAGE LS_LOG-MESSAGE TYPE 'E'
             RAISING PROCESSING_ERROR.
  ENDIF.

ENDMETHOD.


METHOD CONSUME_REST_API.

  DATA:
    LREF_CLIENT  TYPE REF TO IF_HTTP_CLIENT.

  DATA:
    LF_URL         TYPE STRING,
    LF_ENCODED     TYPE STRING,
    LF_USERID      TYPE STRING,
    LF_PASSWD      TYPE STRING,
    LF_HTTP_CODE   TYPE I,
    LF_HTTP_REASON TYPE STRING,
    LF_ERROR       TYPE FLAG.


* Initialize Output
  CLEAR: EF_RESPONSE_JSON,
         EF_HTTP_CODE,
         EF_HTTP_REASON,
         EF_HTML_ERROR.

* Create URL
  LF_URL = IS_CONFIG-URL.
  CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
    EXPORTING
      URL                = LF_URL
    IMPORTING
      CLIENT             = LREF_CLIENT
    EXCEPTIONS
      ARGUMENT_NOT_FOUND = 1
      PLUGIN_NOT_ACTIVE  = 2
      INTERNAL_ERROR     = 3
      OTHERS             = 4.
  IF SY-SUBRC NE 0.
*   Error: URL Error
    MESSAGE E017(ZSDSCA01) RAISING URL_ERROR.
    RETURN.
  ENDIF.

* Set Accept Cookies
  LREF_CLIENT->PROPERTYTYPE_ACCEPT_COOKIE = IF_HTTP_CLIENT=>CO_ENABLED.
  LREF_CLIENT->PROPERTYTYPE_LOGON_POPUP = IF_HTTP_CLIENT=>CO_DISABLED.

* Set Content JSON
  LREF_CLIENT->REQUEST->SET_HEADER_FIELD(
                           EXPORTING: NAME  = 'Content-Type' ##NO_TEXT
                                      VALUE = 'application/json' ).

* Only when User password provided
  IF NOT ( IS_CONFIG-USERID IS INITIAL AND
           IS_CONFIG-PASSWD IS INITIAL ).
    CLEAR LF_PASSWD.
    IF IS_CONFIG-PASSWD IS NOT INITIAL.
*     Decrypt Password
      LF_ENCODED = IS_CONFIG-PASSWD.
      CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~DECODE_BASE64
        EXPORTING
          ENCODED = LF_ENCODED
        RECEIVING
          DECODED = LF_PASSWD.
    ENDIF.

*   Basic Authentication with user/password
    LF_USERID = IS_CONFIG-USERID.
    CALL METHOD LREF_CLIENT->AUTHENTICATE
      EXPORTING
        USERNAME = LF_USERID
        PASSWORD = LF_PASSWD.
  ENDIF.

* Set Call Method
  CASE IS_CONFIG-METHOD.
*   Get Method
    WHEN '1'.
      LREF_CLIENT->REQUEST->SET_METHOD( 'GET' ).
*   Post Method
    WHEN '2'.
      LREF_CLIENT->REQUEST->SET_METHOD( 'POST' ).
*   Default GET Method
    WHEN OTHERS.
      LREF_CLIENT->REQUEST->SET_METHOD( 'GET' ).
  ENDCASE.

* Set Data
  IF IF_REQUEST_JSON IS NOT INITIAL.
    LREF_CLIENT->REQUEST->SET_DATA(
                            EXPORTING: DATA = IF_REQUEST_JSON ).
*   Text-i01: Result JSON
    SHOW_POPUP(
      EXPORTING
        IS_DATA = IF_REQUEST_JSON
        IF_TYPE = GC_JSON
        IF_SECTION = TEXT-I01 ).
  ENDIF.

* Send Request
  CALL METHOD LREF_CLIENT->SEND
*    EXPORTING
*      timeout                    = 200
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      HTTP_INVALID_TIMEOUT       = 4
      OTHERS                     = 5.
  IF SY-SUBRC <> 0.
*   Error: Error on Sending request.
    MESSAGE E018(ZSDSCA01) RAISING SEND_ERROR.
    RETURN.
  ENDIF.

* Recieve Response
  CALL METHOD LREF_CLIENT->RECEIVE
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      OTHERS                     = 4.
  IF SY-SUBRC <> 0.
    LF_ERROR = 'X'.
  ENDIF.

* Get Response Status and Data
  CALL METHOD LREF_CLIENT->RESPONSE->GET_STATUS
    IMPORTING
      CODE   = LF_HTTP_CODE
      REASON = LF_HTTP_REASON.
  EF_HTTP_CODE   = LF_HTTP_CODE.
  EF_HTTP_REASON = LF_HTTP_REASON.

* Check Success code 200-299?
  IF ( NOT EF_HTTP_CODE BETWEEN 200 AND 299 ) OR ##NUMBER_OK
      LF_ERROR EQ 'X'.
    LF_ERROR = 'X'.
*   html result
    EF_HTML_ERROR = LREF_CLIENT->RESPONSE->GET_CDATA( ).
*   Text-i02: Receive Error HTML
    SHOW_POPUP(
      EXPORTING
        IS_DATA = EF_HTML_ERROR
        IF_TYPE = GC_HTML
        IF_SECTION = TEXT-I02 ).
  ENDIF.

* Fetch data from API in json format
  IF LF_ERROR IS INITIAL.
    EF_RESPONSE_JSON = LREF_CLIENT->RESPONSE->GET_DATA( ).
*   Text-i03: Response JSON
    SHOW_POPUP(
      EXPORTING
        IS_DATA = EF_RESPONSE_JSON
        IF_TYPE = GC_JSON
        IF_SECTION = TEXT-I03 ).
  ENDIF.

ENDMETHOD.


METHOD CONVERT_DATA_TO_JSON.

  DATA:
    LF_TRANSFORM TYPE  TS_INTF_CONFIG-TRANSFORM_REQ,
    LF_ERROR     TYPE  FLAG.


* Assign Popup Flag
  GF_POPUP = IF_SHOW_JSON_POPUP.

* Initialize Output
  CLEAR: EF_JSON.

* Read Interface Configuration
  DATA(LS_CONFIG) = READ_CONFIGURATION(
                      EXPORTING
                        IF_INTFNO = IF_INTFNO ).
  IF LS_CONFIG IS INITIAL.
*   Error: Invalid Interface number.
    MESSAGE E013(ZSDSCA01) RAISING INVALID_INTFNO.
    RETURN.
  ENDIF.

  CASE IF_TYPE.
*   Request Data
    WHEN GC_REQUEST.
      LF_TRANSFORM = LS_CONFIG-TRANSFORM_REQ.
*   Response Data
    WHEN GC_RESPONSE.
      LF_TRANSFORM = LS_CONFIG-TRANSFORM_RES.
  ENDCASE.

  IF LF_TRANSFORM IS INITIAL.
    TRY.
        CLEAR LF_ERROR.
*       Get Processing Class Instance
        DATA(LREF_PROCESS) = GET_PROCESSING_INSTANCE( LS_CONFIG ).
        LREF_PROCESS->TRANSFORM_DATA_TO_JSON(
          EXPORTING
            IS_DATA      = IS_DATA
            IF_TYPE      = IF_TYPE
          IMPORTING
            EF_JSON      = EF_JSON
          EXCEPTIONS
            TRANSFORM_ERROR = 1
            OTHERS          = 2 ).
        IF SY-SUBRC NE 0.
          LF_ERROR = 'X'.
        ENDIF.
      CATCH CX_ROOT ##CATCH_ALL.
        LF_ERROR = 'X'.
    ENDTRY.
    IF LF_ERROR EQ 'X'.
*     Error: Cannot determine Transformation.
      MESSAGE E015(ZSDSCA01) RAISING TRANSFORM_NOT_FOUND.
      RETURN.
    ENDIF.
  ELSE.
    TRANSFORM_DATA_TO_JSON(
      EXPORTING
        IF_TRANSFORM = LF_TRANSFORM
        IS_DATA      = IS_DATA
      IMPORTING
        EF_JSON      = EF_JSON
      EXCEPTIONS
        TRANSFORM_ERROR = 1
        OTHERS          = 2 ).
    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID
              TYPE SY-MSGTY
              NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2
                   SY-MSGV3 SY-MSGV4
      RAISING TRANSFORM_ERROR.
    ENDIF.
  ENDIF.


* Text-i01: Result JSON
  SHOW_POPUP(
    EXPORTING
      IS_DATA = EF_JSON
      IF_TYPE = GC_JSON
      IF_SECTION = TEXT-I01 ).

ENDMETHOD.


METHOD CONVERT_DATE_TIME_TO_ISO.

  DATA:
    LS_TTZZ     TYPE  TS_TTZZ.

  DATA:
    LF_DATUM    TYPE  SY-DATUM,
    LF_UZEIT    TYPE  SY-UZEIT,
    LF_DATE_TXT TYPE  CHAR10,
    LF_TIME_TXT TYPE  CHAR8,
    LF_ZONE_TXT TYPE  CHAR6,
    LF_TIMEZONE TYPE  TIMEZONE.


* Initialize Output
  CLEAR EF_OUTPUT.

* Assign Time Zone
  IF IF_TIMEZONE IS INITIAL.
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        TIMEZONE            = LF_TIMEZONE
      EXCEPTIONS
        CUSTOMIZING_MISSING = 1
        OTHERS              = 2.
    IF SY-SUBRC <> 0.
      CLEAR LF_TIMEZONE.
    ENDIF.
  ELSE.
    LF_TIMEZONE = IF_TIMEZONE.
  ENDIF.

  IF IF_TIMESTAMP IS SUPPLIED.
    CONVERT TIME STAMP IF_TIMESTAMP
                  TIME ZONE LF_TIMEZONE
                  INTO DATE LF_DATUM
                       TIME LF_UZEIT.
  ELSEIF IF_DATUM IS SUPPLIED.
    LF_DATUM = IF_DATUM.
    LF_UZEIT = IF_UZEIT.
  ENDIF.

* Assign Date
  CONCATENATE LF_DATUM(4)
              LF_DATUM+4(2)
              LF_DATUM+6(2)
         INTO LF_DATE_TXT
    SEPARATED BY '-'.

* Assign Time
  CONCATENATE LF_UZEIT(2)
              LF_UZEIT+2(2)
              LF_UZEIT+4(2)
         INTO LF_TIME_TXT
    SEPARATED BY ':'.

  GET_TIMEZONE_DETAIL(
    EXPORTING
      IF_TIMEZONE = LF_TIMEZONE
    IMPORTING
      ES_TTZZ     = LS_TTZZ ).
  IF LS_TTZZ IS NOT INITIAL.
    CONCATENATE LS_TTZZ-UTCSIGN
                LS_TTZZ-UTCDIFF(2) ':'
                LS_TTZZ-UTCDIFF+2(2)
           INTO LF_ZONE_TXT.
  ENDIF.

* Assign result
  IF IF_UZEIT IS SUPPLIED OR
     IF_TIMESTAMP IS SUPPLIED.
    IF IF_TIMEZONE IS SUPPLIED.
      CONCATENATE LF_DATE_TXT 'T' LF_TIME_TXT LF_ZONE_TXT
             INTO EF_OUTPUT.
    ELSE.
      CONCATENATE LF_DATE_TXT 'T' LF_TIME_TXT
             INTO EF_OUTPUT.
    ENDIF.
  ELSE.
    EF_OUTPUT = LF_DATE_TXT.
  ENDIF.

ENDMETHOD.


METHOD CONVERT_ISO_TO_DATE_TIME.

  DATA:
    LF_DATUM_TXT TYPE CHAR10,
    LF_UZEIT_TXT TYPE CHAR9,
    LF_DATUM     TYPE SY-DATUM,
    LF_UZEIT     TYPE SY-UZEIT,
    LF_UTCDIFF   TYPE TS_TTZZ-UTCDIFF,
    LF_UTCSIGN   TYPE TS_TTZZ-UTCSIGN,
    LF_TIMEZONE  TYPE TIMEZONE,
    LF_TIMESTAMP TYPE TZONREF-TSTAMPS.


* Initialize Output
  CLEAR: EF_DATUM,
         EF_UZEIT.

  TRY.

      IF STRLEN( IF_INPUT ) LT 10.
        RAISE CONVERSION_ERROR.
      ENDIF.

*     ---------------
*     Assign date
*     ---------------
      LF_DATUM_TXT = IF_INPUT(10).
      IF NOT ( LF_DATUM_TXT CO '0123456789- ' AND
               LF_DATUM_TXT+4(1) = '-' AND
               LF_DATUM_TXT+7(1) = '-' ).
        RAISE CONVERSION_ERROR.
      ENDIF.

      LF_DATUM = LF_DATUM_TXT(4) && LF_DATUM_TXT+5(2) && LF_DATUM_TXT+8(2).

*     ---------------
*     Assign Time
*     ---------------
      IF STRLEN( IF_INPUT ) GE 19 ##NUMBER_OK.
        LF_UZEIT_TXT = IF_INPUT+10(9).
        IF NOT ( LF_UZEIT_TXT CO '0123456789T: ' AND
                 LF_UZEIT_TXT+0(1) = 'T' AND
                 LF_UZEIT_TXT+3(1) = ':' AND
                 LF_UZEIT_TXT+6(1) = ':' ).
          RAISE CONVERSION_ERROR.
        ENDIF.

        LF_UZEIT = LF_UZEIT_TXT+1(2) && LF_UZEIT_TXT+4(2) && LF_UZEIT_TXT+7(2).

      ENDIF.

*     ---------------
*     Assign TimeZone
*     ---------------
      IF STRLEN( IF_INPUT ) EQ 25 ##NUMBER_OK.
        LF_UTCDIFF = IF_INPUT+20(2) && IF_INPUT+23(2) && '00'.
        LF_UTCSIGN = IF_INPUT+19(1).
        GET_TIMEZONE(
          EXPORTING
            IF_UTCDIFF  = LF_UTCDIFF
            IF_UTCSIGN  = LF_UTCSIGN
          IMPORTING
            EF_TIMEZONE = LF_TIMEZONE ).
        IF LF_TIMEZONE IS INITIAL.
          RAISE CONVERSION_ERROR.
        ENDIF.
*       Only when Timezone is different
        IF LF_TIMEZONE NE IF_TIMEZONE.
*         Convert date time to Target Timezone
          CALL FUNCTION 'IB_CONVERT_INTO_TIMESTAMP'
            EXPORTING
              I_DATLO     = LF_DATUM
              I_TIMLO     = LF_UZEIT
              I_TZONE     = LF_TIMEZONE
            IMPORTING
              E_TIMESTAMP = LF_TIMESTAMP.
          CALL FUNCTION 'IB_CONVERT_FROM_TIMESTAMP'
            EXPORTING
              I_TIMESTAMP = LF_TIMESTAMP
              I_TZONE     = IF_TIMEZONE
            IMPORTING
              E_DATLO     = LF_DATUM
              E_TIMLO     = LF_UZEIT.
        ENDIF.

      ENDIF.

    CATCH CX_ROOT ##CATCH_ALL.
      RAISE CONVERSION_ERROR.

  ENDTRY.

* Assign Output
  EF_DATUM = LF_DATUM.
  EF_UZEIT = LF_UZEIT.

  IF EF_TIMESTAMP IS SUPPLIED.
    CONVERT DATE EF_DATUM
            TIME EF_UZEIT
        INTO TIME STAMP EF_TIMESTAMP TIME ZONE IF_TIMEZONE.
  ENDIF.

ENDMETHOD.


METHOD CONVERT_JSON_TO_DATA.

  DATA:
    LF_TRANSFORM TYPE  TS_INTF_CONFIG-TRANSFORM_REQ,
    LF_ERROR     TYPE  FLAG.


* Initialize Output
  CLEAR: ES_DATA.

* Only JSON Exist
  IF IF_JSON IS INITIAL.
    RETURN.
  ENDIF.

* Read Interface Configuration
  DATA(LS_CONFIG) = READ_CONFIGURATION(
                      EXPORTING
                        IF_INTFNO = IF_INTFNO ).
  IF LS_CONFIG IS INITIAL.
*   Error: Invalid Interface number.
    MESSAGE E013(ZSDSCA01) RAISING INVALID_INTFNO.
    RETURN.
  ENDIF.

  CASE IF_TYPE.
*   Request Data
    WHEN GC_REQUEST.
      LF_TRANSFORM = LS_CONFIG-TRANSFORM_REQ.
*   Response Data
    WHEN GC_RESPONSE.
      LF_TRANSFORM = LS_CONFIG-TRANSFORM_RES.
  ENDCASE.

  IF LF_TRANSFORM IS INITIAL.
    TRY.
        CLEAR LF_ERROR.
*       Get Processing Class Instance
        DATA(LREF_PROCESS) = GET_PROCESSING_INSTANCE( LS_CONFIG ).
        LREF_PROCESS->TRANSFORM_JSON_TO_DATA(
          EXPORTING
            IF_JSON      = IF_JSON
            IF_TYPE      = IF_TYPE
          IMPORTING
            ES_DATA      = ES_DATA
          EXCEPTIONS
            TRANSFORM_ERROR = 1
            OTHERS          = 2 ).
        IF SY-SUBRC NE 0.
          LF_ERROR = 'X'.
        ENDIF.
      CATCH CX_ROOT ##CATCH_ALL.
        LF_ERROR = 'X'.
    ENDTRY.
    IF LF_ERROR EQ 'X'.
*     Error: Cannot determine Transformation.
      MESSAGE E015(ZSDSCA01) RAISING TRANSFORM_NOT_FOUND.
      RETURN.
    ENDIF.
  ENDIF.

  TRANSFORM_JSON_TO_DATA(
    EXPORTING
      IF_TRANSFORM = LF_TRANSFORM
      IF_JSON      = IF_JSON
    IMPORTING
      ES_DATA      = ES_DATA
    EXCEPTIONS
      TRANSFORM_ERROR = 1
      OTHERS          = 2 ).
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID
            TYPE SY-MSGTY
            NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2
                 SY-MSGV3 SY-MSGV4
    RAISING TRANSFORM_ERROR.
  ENDIF.

ENDMETHOD.


METHOD GET_PROCESSING_INSTANCE.

  DATA:
    LF_PROCESS_CLASS  TYPE  TS_INTF_CONFIG-PROCESS_CLASS.


  IF GREF_PROCESS IS NOT BOUND OR
     GREF_PROCESS->GS_CONFIG NE IS_CONFIG.
    IF IS_CONFIG-PROCESS_CLASS IS INITIAL.
      LF_PROCESS_CLASS = 'ZCL_SDSCA_REST_SERVICE'.
    ELSE.
      LF_PROCESS_CLASS = IS_CONFIG-PROCESS_CLASS.
    ENDIF.
    CREATE OBJECT GREF_PROCESS TYPE (LF_PROCESS_CLASS)
               EXPORTING IS_CONFIG = IS_CONFIG.
*   Call Initialize Method
    GREF_PROCESS->INITIALIZE_DATA( ).
  ENDIF.

* Return Output
  RREF_PROCESS = GREF_PROCESS.

ENDMETHOD.


METHOD GET_TIMEZONE.

  STATICS:
    LS_TTZZ  TYPE  TS_TTZZ.


* Initialize Output
  CLEAR: EF_TIMEZONE.

  IF LS_TTZZ-UTCDIFF NE IF_UTCDIFF OR
     LS_TTZZ-UTCSIGN NE IF_UTCSIGN .
    SELECT A~TZONE,
           A~ZONERULE,
           B~UTCSIGN,
           B~UTCDIFF
      FROM TTZZ AS A
             INNER JOIN TTZR AS B                      "#EC CI_BUFFJOIN
               ON  B~ZONERULE = A~ZONERULE
     WHERE B~UTCDIFF EQ @IF_UTCDIFF
       AND B~UTCSIGN EQ @IF_UTCSIGN
       AND A~TZONE LIKE 'UTC%'
     ORDER BY A~TZONE    ASCENDING,
              A~ZONERULE ASCENDING,
              B~UTCSIGN  ASCENDING,
              B~UTCDIFF  ASCENDING
      INTO @LS_TTZZ
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.

* Assign Output
  EF_TIMEZONE = LS_TTZZ-TZONE.

ENDMETHOD.


METHOD GET_TIMEZONE_DETAIL.

  STATICS:
    LS_TTZZ  TYPE  TS_TTZZ.


* Initialize Output
  CLEAR: ES_TTZZ.

  IF LS_TTZZ-TZONE NE IF_TIMEZONE.
    SELECT A~TZONE,
           A~ZONERULE,
           B~UTCSIGN,
           B~UTCDIFF
      FROM TTZZ AS A
             INNER JOIN TTZR AS B                      "#EC CI_BUFFJOIN
               ON  B~ZONERULE = A~ZONERULE
     WHERE A~TZONE EQ @IF_TIMEZONE
     ORDER BY A~TZONE    ASCENDING,
              A~ZONERULE ASCENDING,
              B~UTCSIGN  ASCENDING,
              B~UTCDIFF  ASCENDING
      INTO @LS_TTZZ
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.

* Assign Output
  ES_TTZZ = LS_TTZZ.

ENDMETHOD.


METHOD READ_CONFIGURATION.

  STATICS:
    LS_CONFIG  TYPE  TS_INTF_CONFIG.


* Initialize Output
  CLEAR: RS_CONFIG.

  IF LS_CONFIG-INTFNO NE IF_INTFNO.
    SELECT SINGLE *
      INTO @LS_CONFIG
      FROM ZSDSCAC005
     WHERE INTFNO EQ @IF_INTFNO.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.
  ENDIF.

* Assign Output
  RS_CONFIG = LS_CONFIG.

ENDMETHOD.


METHOD READ_FIELD_VALUE.

  DATA:
    LF_FNAME TYPE  CHAR100,
    LF_FROM  TYPE  I,
    LF_TO    TYPE  I,
    LF_LEN   TYPE  I,
    LF_INDEX TYPE  I,
    LF_FIELD TYPE  TEXT1000.

  FIELD-SYMBOLS:
    <L_TABLE> TYPE ANY TABLE.


* Initialize Output
  CLEAR EF_VALUE.

  TRY.

*     Check table field?
      FIND FIRST OCCURRENCE OF '[' IN IF_FIELD
                               MATCH OFFSET LF_FROM.
      IF SY-SUBRC EQ 0.
*       This is Table field
        FIND FIRST OCCURRENCE OF ']' IN IF_FIELD
                                 MATCH OFFSET LF_TO.

*       Get Table Name
        CONCATENATE 'IS_DATA-' IF_FIELD(LF_FROM)
               INTO LF_FNAME.
        ASSIGN (LF_FNAME) TO <L_TABLE>.
        IF SY-SUBRC NE 0.
          RETURN.
        ENDIF.

*       Get Index
        LF_LEN = LF_TO - LF_FROM - 1.
        IF LF_LEN LE 0.
          RETURN.
        ENDIF.
        LF_FROM = LF_FROM + 1.
        LF_INDEX = IF_FIELD+LF_FROM(LF_LEN).

*       Read table on specified index
        LOOP AT <L_TABLE> ASSIGNING FIELD-SYMBOL(<L_DATA>).
          IF SY-TABIX GE LF_INDEX.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF SY-SUBRC NE 0.
          RETURN.
        ENDIF.

*       There must be - after ], therefore, +2 index
        LF_TO = LF_TO + 2.
        LF_FIELD = IF_FIELD+LF_TO.
*       Read Field
        READ_FIELD_VALUE(
          EXPORTING
            IF_FIELD = LF_FIELD
            IS_DATA  = <L_DATA>
          IMPORTING
            EF_VALUE = EF_VALUE ).

      ELSE.
        CONCATENATE 'IS_DATA-' IF_FIELD
               INTO LF_FNAME.
        ASSIGN (LF_FNAME) TO FIELD-SYMBOL(<L_VALUE>).
        IF SY-SUBRC NE 0.
          RETURN.
        ENDIF.

*       Assign Output
        EF_VALUE = <L_VALUE>.

      ENDIF.

    CATCH CX_ROOT ##CATCH_ALL.
      RETURN.
  ENDTRY.

ENDMETHOD.


METHOD SAVE_PROCESSING_LOG.

  DATA:
    LS_SAVE  TYPE  ZSDSCAT001.


* Initialize Output
  CLEAR: ES_KEY.

* Only when Number range specified
  IF IS_CONFIG-NRNR IS INITIAL.
    RETURN.
  ENDIF.

* Assign Log Data
  CLEAR LS_SAVE.
  MOVE-CORRESPONDING IS_LOG TO LS_SAVE.
  LS_SAVE-INTFNO = IS_CONFIG-INTFNO.
  LS_SAVE-GJAHR  = SY-DATUM(4).

* Generate Request Number
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = IS_CONFIG-NRNR
      OBJECT                  = 'ZCA002'
      QUANTITY                = '1'
      TOYEAR                  = LS_SAVE-GJAHR
    IMPORTING
      NUMBER                  = LS_SAVE-REQNO
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
*   Error: Cannot Generate Request number
    MESSAGE E020(ZSDSCA01) RAISING REQNO_ERROR.
    RETURN.
  ENDIF.

  IF IS_LOG-KEYDATA IS INITIAL.
*   Assign Key data
    ASSIGN_KEYDATA(
      EXPORTING
        IF_JSON   = IS_LOG-REQUEST_JSON
        IS_CONFIG = IS_CONFIG
      IMPORTING
        EF_VALUE  = LS_SAVE-KEYDATA ).
  ELSE.
    LS_SAVE-KEYDATA = IS_LOG-KEYDATA.
  ENDIF.

  IF IS_LOG-STATUS IS INITIAL.
*   Assign Status from response
    ASSIGN_STATUS(
      EXPORTING
        IF_JSON    = IS_LOG-RESPONSE_JSON
        IS_CONFIG  = IS_CONFIG
      IMPORTING
        EF_STATUS  = LS_SAVE-STATUS
        EF_MESSAGE = LS_SAVE-MESSAGE ).
  ELSE.
    LS_SAVE-STATUS  = IS_LOG-STATUS.
    LS_SAVE-MESSAGE = IS_LOG-MESSAGE.
  ENDIF.

* Insert New Log
  INSERT ZSDSCAT001 FROM LS_SAVE.
  IF SY-SUBRC NE 0.
*   Error: Error during insert new log entry.
    MESSAGE E021(ZSDSCA01) RAISING LOG_ERROR.
    RETURN.
  ENDIF.

  COMMIT WORK AND WAIT.

* Assign Output
  ES_KEY-INTFNO = LS_SAVE-INTFNO.
  ES_KEY-REQNO  = LS_SAVE-REQNO.
  ES_KEY-GJAHR  = LS_SAVE-GJAHR.

ENDMETHOD.


METHOD SHOW_POPUP.

* Only when Popup Activated in online mode
  IF SY-BATCH EQ 'X' OR
     GF_POPUP IS INITIAL.
    RETURN.
  ENDIF.

  DATA(LREF_OUT) = CL_DEMO_OUTPUT=>NEW( )->BEGIN_SECTION( IF_SECTION ).

  CASE IF_TYPE.
    WHEN GC_JSON.
      LREF_OUT->WRITE_JSON( IS_DATA ).
    WHEN GC_HTML.
      LREF_OUT->WRITE_HTML( IS_DATA ).
    WHEN OTHERS.
      RETURN.
  ENDCASE.

* Display Popup
  LREF_OUT->DISPLAY( ).

ENDMETHOD.


METHOD TRANSFORM_DATA_TO_JSON.

  DATA:
    LREF_WRITER TYPE REF TO CL_SXML_STRING_WRITER.


* Initialize Output
  CLEAR: EF_JSON.

* Create Writer Object
  LREF_WRITER = CL_SXML_STRING_WRITER=>CREATE(
                        TYPE = IF_SXML=>CO_XT_JSON ).

  TRY.
*   Call Transformation
      CALL TRANSFORMATION (IF_TRANSFORM)
                          SOURCE DATA = IS_DATA
                          RESULT XML LREF_WRITER.
    CATCH CX_ROOT INTO DATA(LREF_ERROR) ##CATCH_ALL.
      DATA(LF_LONGTEXT) = LREF_ERROR->IF_MESSAGE~GET_LONGTEXT( PRESERVE_NEWLINES = ' ' ) ##NEEDED.
      DATA(LF_TEXT)     = LREF_ERROR->IF_MESSAGE~GET_TEXT( ) ##NEEDED.
      IF LF_LONGTEXT IS NOT INITIAL.
        MESSAGE LF_LONGTEXT TYPE 'E' RAISING TRANSFORM_ERROR.
      ELSEIF LF_TEXT IS NOT INITIAL.
        MESSAGE LF_TEXT TYPE 'E' RAISING TRANSFORM_ERROR.
      ELSE.
*       Error: Transformation to JSON error.
        MESSAGE E016(ZSDSCA01) RAISING TRANSFORM_ERROR.
      ENDIF.
      RETURN.
  ENDTRY.

* Assign JSON Output
  EF_JSON = LREF_WRITER->GET_OUTPUT( ).

ENDMETHOD.


METHOD TRANSFORM_JSON_TO_DATA.

* Initialize Output
  CLEAR ES_DATA.

  TRY.
*   Transform Data
      CALL TRANSFORMATION (IF_TRANSFORM)
                          SOURCE XML IF_JSON
                          RESULT DATA = ES_DATA.
    CATCH CX_ROOT INTO DATA(LREF_ERROR) ##CATCH_ALL.
      DATA(LF_LONGTEXT) = LREF_ERROR->IF_MESSAGE~GET_LONGTEXT( PRESERVE_NEWLINES = ' ' ) ##NEEDED.
      DATA(LF_TEXT)     = LREF_ERROR->IF_MESSAGE~GET_TEXT( ) ##NEEDED.
      IF LF_LONGTEXT IS NOT INITIAL.
        MESSAGE LF_LONGTEXT TYPE 'E' RAISING TRANSFORM_ERROR.
      ELSEIF LF_TEXT IS NOT INITIAL.
        MESSAGE LF_TEXT TYPE 'E' RAISING TRANSFORM_ERROR.
      ELSE.
*       Error: Transformation from JSON error.
        MESSAGE E019(ZSDSCA01) RAISING TRANSFORM_ERROR.
      ENDIF.
      RETURN.
  ENDTRY.

ENDMETHOD.
ENDCLASS.
