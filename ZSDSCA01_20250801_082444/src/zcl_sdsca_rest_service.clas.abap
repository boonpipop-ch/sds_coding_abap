class ZCL_SDSCA_REST_SERVICE definition
  public
  create public .

public section.

  data GS_CONFIG type ZCL_SDSCA_REST_INTF_UTILITY=>TS_INTF_CONFIG .

  methods INITIALIZE_DATA .
  methods CONSTRUCTOR
    importing
      !IS_CONFIG type ZCL_SDSCA_REST_INTF_UTILITY=>TS_INTF_CONFIG
      !IF_FNAME_STATUS type CHAR50 optional
      !IF_FNAME_MESSAGE type CHAR50 optional .
  methods PROCESS_DATA
    importing
      !IREF_REQUEST_DATA type ref to DATA
    exporting
      !EREF_RESPONSE_DATA type ref to DATA
      !EF_STATUS type ZSDSDE_REST_STATUS
      !EF_MESSAGE type ZSDSDE_REST_MESSAGE
      !EF_HTTP_ERROR type FLAG .
  methods HANDLING_ERROR
    importing
      !IREF_RESPONSE_DATA type ref to DATA
      !IF_STATUS type ZSDSDE_REST_STATUS
      !IF_MESSAGE type CLIKE
    exporting
      !EF_ASSIGNED type FLAG .
  methods TRANSFORM_DATA_TO_JSON
    importing
      !IS_DATA type ANY
      !IF_TYPE type CHAR1 optional
    exporting
      !EF_JSON type ZSDSDE_JSON
    exceptions
      TRANSFORM_ERROR .
  methods TRANSFORM_JSON_TO_DATA
    importing
      !IF_JSON type ZSDSDE_JSON
      !IF_TYPE type CHAR1 optional
    exporting
      !ES_DATA type ANY
    exceptions
      TRANSFORM_ERROR .
  methods GET_KEYDATA
    importing
      !IS_DATA type ANY
    exporting
      !EF_VALUE type ZSDSDE_KEYDATA .
  methods GET_STATUS
    importing
      !IS_DATA type ANY
    exporting
      !EF_STATUS type ZSDSDE_REST_STATUS
      !EF_MESSAGE type ZSDSDE_REST_MESSAGE .
protected section.

  data GF_FNAME_STATUS type CHAR50 value 'RESP_STATUS' ##NO_TEXT.
  data GF_FNAME_MESSAGE type CHAR50 value 'RESP_MESSAGE' ##NO_TEXT.
  data GF_WAIT_TIMES type I value 300 ##NO_TEXT.

  methods LOCK_PROCESSING
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
  methods UNLOCK_PROCESSING .
private section.
ENDCLASS.



CLASS ZCL_SDSCA_REST_SERVICE IMPLEMENTATION.


METHOD CONSTRUCTOR.
  GS_CONFIG = IS_CONFIG.
  IF IF_FNAME_STATUS IS SUPPLIED.
    GF_FNAME_STATUS = IF_FNAME_STATUS.
  ENDIF.
  IF IF_FNAME_MESSAGE IS SUPPLIED.
    GF_FNAME_MESSAGE = IF_FNAME_MESSAGE.
  ENDIF.
ENDMETHOD.


METHOD GET_KEYDATA ##NEEDED.

* This method will be processed only when CONFIG-KEYDATA is not set
* Assign EF_VALUE from Data in IS_DATA
* The value will be save in ZSDSCAT001-KEYDATA for searching

ENDMETHOD.


METHOD GET_STATUS.

  DATA:
    LF_FNAME  TYPE  CHAR50.

  FIELD-SYMBOLS:
    <L_FIELD> TYPE  ANY.


* Initialize Output
  CLEAR: EF_STATUS,
         EF_MESSAGE.

* Assign Result
  CONCATENATE 'IS_DATA-' GF_FNAME_STATUS
         INTO LF_FNAME.
  ASSIGN (LF_FNAME) TO <L_FIELD>.
  IF SY-SUBRC EQ 0.
    EF_STATUS = <L_FIELD>.
  ENDIF.

  CONCATENATE 'IS_DATA-' GF_FNAME_MESSAGE
         INTO LF_FNAME.
  ASSIGN (LF_FNAME) TO <L_FIELD>.
  IF SY-SUBRC EQ 0.
    EF_MESSAGE = <L_FIELD>.
  ENDIF.

ENDMETHOD.


METHOD HANDLING_ERROR.
****************************************
* Implement this method when need the return
* general error as response message instead
* of HTTP error message
* - After assign to response structure
*   set EF_ASSIGNED = X to avoid HTTP error
****************************************
  FIELD-SYMBOLS:
    <L_RESP>  TYPE  ANY,
    <L_FIELD> TYPE  ANY.


* Initialize Output
  CLEAR EF_ASSIGNED.

  ASSIGN IREF_RESPONSE_DATA->* TO <L_RESP>.
  IF SY-SUBRC NE 0.
*   Critical Error
    RETURN.
  ENDIF.

* ----------------------------------
* Assign Response Status and Message
* ----------------------------------
  MACRO_ASSIGN_RESP <L_RESP> GF_FNAME_STATUS IF_STATUS.
  MACRO_ASSIGN_RESP <L_RESP> GF_FNAME_MESSAGE IF_MESSAGE.

ENDMETHOD.


METHOD INITIALIZE_DATA ##NEEDED.
* Redefine and implement custom logic when new instance here...

ENDMETHOD.


METHOD LOCK_PROCESSING.

* Initialize Output
  CLEAR: RF_RESULT.

  DO GF_WAIT_TIMES TIMES.
    CALL FUNCTION 'ENQUEUE_EZSDSCAC005'
      EXPORTING
        MODE_ZSDSCAC005 = 'E'
        MANDT           = SY-MANDT
        INTFNO          = GS_CONFIG-INTFNO
      EXCEPTIONS
        FOREIGN_LOCK    = 1
        SYSTEM_FAILURE  = 2
        OTHERS          = 3.
    IF SY-SUBRC EQ 0.
      RF_RESULT = ABAP_TRUE.
      EXIT.
    ENDIF.
    WAIT UP TO 1 SECONDS.
  ENDDO.

ENDMETHOD.


METHOD PROCESS_DATA.

* Initialize Output
  CLEAR: EF_STATUS,
         EF_MESSAGE,
         EF_HTTP_ERROR,
         EREF_RESPONSE_DATA->*.

* Process data from IREF_REQUEST_DATA which structure is GS_CONFIG-REQUEST_STRUC
* Assign response data into EREF_RESPONSE_DATA which structure is GS_CONFIG-RESPONSE_STRUC

* Assign EF_STATUS & EF_MESSAGE to Log into table ZSDSCAT001 field STATUS and MESSAGE

ENDMETHOD.


METHOD TRANSFORM_DATA_TO_JSON.

  TRY.
*     Generate JSon based on Data Structure
      EF_JSON = /UI2/CL_JSON=>SERIALIZE(
                  EXPORTING
                    DATA             = IS_DATA
                    PRETTY_NAME      = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
                    COMPRESS         = 'X'
                    ASSOC_ARRAYS     = 'X'
                    ASSOC_ARRAYS_OPT = 'X'  ).
    CATCH CX_ROOT ##CATCH_ALL.
      RAISE TRANSFORM_ERROR.
  ENDTRY.

ENDMETHOD.


METHOD TRANSFORM_JSON_TO_DATA.

  TRY.
*     Generate Data based on JSON
      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSONX            = IF_JSON
          PRETTY_NAME      = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
          ASSOC_ARRAYS     = 'X'
          ASSOC_ARRAYS_OPT = 'X'
        CHANGING
          DATA             = ES_DATA.
    CATCH CX_ROOT ##CATCH_ALL.
      RAISE TRANSFORM_ERROR.
  ENDTRY.

ENDMETHOD.


METHOD UNLOCK_PROCESSING.

  CALL FUNCTION 'DEQUEUE_EZSDSCAC005'
    EXPORTING
      MODE_ZSDSCAC005 = 'E'
      MANDT           = SY-MANDT
      INTFNO          = GS_CONFIG-INTFNO.

ENDMETHOD.
ENDCLASS.
