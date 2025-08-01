class ZCL_SDSCA_CAL_API definition
  public
  final
  create public .

public section.

  constants GC_S type CHAR1 value 'S' ##NO_TEXT.
  constants GC_E type CHAR1 value 'E' ##NO_TEXT.

  class-methods CALL_API
    importing
      !I_URL type STRING
      !I_METHOD type STRING
      !I_HEADER type ZSDSCAS001_TT
      !I_BODY_TEXT type STRING optional
      !I_BODY_BIN type XSTRING optional
      !I_LEN type I optional
      !I_LEN_BIN type I optional
      !I_FROM type ZSDSCAS001_TT optional
      !I_USER type STRING optional
      !I_PASS type STRING optional
    exporting
      !E_RETURN type ANY
      !E_RETURN_BODY_TEXT type STRING
      !E_RETURN_BODY_BIN type XSTRING
      !E_MESSAGE type STRING
      !E_STATUS type CHAR1 .
  methods GET_API_END_POINT
    importing
      !I_PROCESS_NAME type ZSDSDE_PROCESS_NAME
    returning
      value(R_RETURN) type ZSDSDE_END_POINT .
  methods GET_API_HEADER
    importing
      value(I_DATA) type ZSDSCAS001_TT optional
    returning
      value(R_RETURN) type ZSDSCAS001_TT .
  methods GET_API_DETAIL
    importing
      value(I_DATA) type ANY optional
      value(I_DATA_TAB) type ANY TABLE optional
    returning
      value(R_RETURN) type STRING .
  methods GET_API_LEN
    importing
      !I_DATA type STRING
    returning
      value(R_RETURN) type I .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSCA_CAL_API IMPLEMENTATION.


  METHOD CALL_API.
*HTTP Client Abstraction
    DATA  LO_CLIENT TYPE REF TO IF_HTTP_CLIENT.

*Data variables for storing response in xstring and string
    DATA : LV_XSTRING   TYPE XSTRING,
           LV_STRING    TYPE STRING,
           LV_NODE_NAME TYPE STRING.

    DATA : L_CODE         TYPE I,
           LT_HTTP_FIELDS TYPE TIHTTPNVP,
           L_BODY_XSTRING TYPE XSTRING,
           L_BODY_STRING  TYPE STRING.

    DATA : LV_CDATA                TYPE STRING,
           LV_CONTENT_LENGTH_VALUE TYPE I.

    DATA : LV_USERNAME TYPE STRING,
           LV_PASSWORD TYPE STRING.

    DATA : LS_FROM LIKE LINE OF I_FROM.

    DATA : LV_VALUE TYPE STRING.

    DATA : LV_URL TYPE STRING.

    DATA : LV_MESSAGE TYPE C LENGTH 255.

    DATA : LV_DATA TYPE XSTRING.

    DATA : LS_HEADER LIKE LINE OF I_HEADER.

    DATA : LCL_ZGG_API TYPE REF TO LCL_ZGG_API.

    CLEAR : LV_XSTRING, LV_STRING, LV_NODE_NAME.

*Creation of New IF_HTTP_Client Object
    CL_HTTP_CLIENT=>CREATE_BY_URL(
    EXPORTING
      URL                = I_URL
    IMPORTING
      CLIENT             = LO_CLIENT
    EXCEPTIONS
      ARGUMENT_NOT_FOUND = 1
      PLUGIN_NOT_ACTIVE  = 2
      INTERNAL_ERROR     = 3
      ).
    IF SY-SUBRC IS NOT INITIAL.
* Handle errors
    ENDIF.

    LO_CLIENT->REQUEST->SET_METHOD( I_METHOD ).

    LV_USERNAME = I_USER.
    LV_PASSWORD = I_PASS.

    IF LV_USERNAME IS NOT INITIAL.
      CALL METHOD LO_CLIENT->AUTHENTICATE
        EXPORTING
          USERNAME = LV_USERNAME
          PASSWORD = LV_PASSWORD.
      IF SY-SUBRC IS NOT INITIAL.
* Handle errors
      ENDIF.
    ENDIF.

    LOOP AT I_HEADER INTO LS_HEADER.
      CALL METHOD LO_CLIENT->REQUEST->SET_HEADER_FIELD
        EXPORTING
          NAME  = LS_HEADER-NAME
          VALUE = LS_HEADER-VALUE.
    ENDLOOP.

    LOOP AT I_FROM INTO LS_FROM.
      CALL METHOD LO_CLIENT->REQUEST->IF_HTTP_ENTITY~SET_FORM_FIELD
        EXPORTING
          NAME  = LS_FROM-NAME
          VALUE = LS_FROM-VALUE.
    ENDLOOP.

    IF I_BODY_TEXT IS NOT INITIAL.
      LV_CONTENT_LENGTH_VALUE = I_LEN.
      LV_CDATA = I_BODY_TEXT.
      CALL METHOD LO_CLIENT->REQUEST->IF_HTTP_ENTITY~SET_CDATA
        EXPORTING
          DATA   = LV_CDATA
          LENGTH = LV_CONTENT_LENGTH_VALUE
          OFFSET = 0.
    ENDIF.

    IF I_BODY_BIN IS NOT INITIAL.
      LV_CONTENT_LENGTH_VALUE = I_LEN_BIN.
      LV_DATA                 = I_BODY_BIN.
      CALL METHOD LO_CLIENT->REQUEST->IF_HTTP_ENTITY~SET_DATA
        EXPORTING
          DATA   = LV_DATA
          LENGTH = LV_CONTENT_LENGTH_VALUE
          OFFSET = 0.
    ENDIF.
*Structure of HTTP Connection and Dispatch of Data
    LO_CLIENT->SEND( ).
    IF SY-SUBRC IS NOT INITIAL.
* Handle errors
    ENDIF.

*Receipt of HTTP Response
    CALL METHOD LO_CLIENT->RECEIVE
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        OTHERS                     = 4.


    IF SY-SUBRC IS NOT INITIAL.
* Handle errors
    ENDIF.
*-----------------------------------------------------------------------
*- print the results
*-----------------------------------------------------------------------
    CALL METHOD LO_CLIENT->RESPONSE->GET_STATUS
      IMPORTING
        CODE = L_CODE.
    IF L_CODE < 300.
*   HTTP-codes: 100 - 199 = informations
*   HTTP-codes: 200 - 299 = client-request successful (200 = OK)
*    WRITE: / icon_green_light AS ICON.
*      MESSAGE S000(Z_SD) WITH TEXT-999.
      LV_MESSAGE = TEXT-999.
      E_STATUS   = GC_S.
    ELSE.
*   HTTP-codes: 300 - 399 = redirected; further actions required
*   HTTP-codes: 400 - 499 = client-request incomplete
*   HTTP-codes: 500 - 599 = server errors
*    WRITE: / icon_red_light AS ICON.
      IF     L_CODE BETWEEN 300 AND 399.
        LV_MESSAGE = TEXT-998.
        E_STATUS = GC_E.
      ELSEIF L_CODE BETWEEN 400 AND 499.
        LV_MESSAGE = TEXT-997.
        E_STATUS = GC_E.
      ELSE.
        LV_MESSAGE = TEXT-996.
        E_STATUS = GC_E.
      ENDIF.

*      MESSAGE s000(z_sd) WITH lv_message DISPLAY LIKE 'E'.
    ENDIF.

    E_MESSAGE = LV_MESSAGE.

    CALL METHOD LO_CLIENT->RESPONSE->GET_HEADER_FIELDS
      CHANGING
        FIELDS = LT_HTTP_FIELDS.

    L_BODY_XSTRING = LO_CLIENT->RESPONSE->GET_DATA(  ).
    L_BODY_STRING  = LO_CLIENT->RESPONSE->GET_CDATA(  ).

    IF LCL_ZGG_API IS INITIAL.
      CREATE OBJECT LCL_ZGG_API.
    ENDIF.
    CALL METHOD LCL_ZGG_API->CONVERT_JSON
      EXPORTING
        I_JSON = L_BODY_STRING
      IMPORTING
        E_DATA = E_RETURN.

    E_RETURN_BODY_BIN  = L_BODY_XSTRING.
    E_RETURN_BODY_TEXT = L_BODY_STRING.

    LO_CLIENT->CLOSE( ).
  ENDMETHOD.


  METHOD GET_API_DETAIL.
    IF I_DATA IS NOT INITIAL.
      CALL METHOD /UI2/CL_JSON=>SERIALIZE
        EXPORTING
          DATA        = I_DATA
          PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE
        RECEIVING
          R_JSON      = R_RETURN
        EXCEPTIONS
          OTHERS      = 1.
    ELSEIF I_DATA_TAB IS NOT INITIAL.
      CALL METHOD /UI2/CL_JSON=>SERIALIZE
        EXPORTING
          DATA        = I_DATA_TAB
          PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE
        RECEIVING
          R_JSON      = R_RETURN
        EXCEPTIONS
          OTHERS      = 1.
    ENDIF.
  ENDMETHOD.


  METHOD GET_API_END_POINT.
    CONSTANTS : BEGIN OF LC_CON,
                  DEV TYPE C LENGTH 3 VALUE 'F36',
                  QAS TYPE C LENGTH 3 VALUE 'F46',
                  PRD TYPE C LENGTH 3 VALUE 'F56',
                END OF LC_CON.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        R_RETURN = LCL_DATA=>GET_ENDPOINT( I_PROCESS = I_PROCESS_NAME
                                           I_ENV     = 'DEV' ).
      WHEN LC_CON-QAS.
        R_RETURN = LCL_DATA=>GET_ENDPOINT( I_PROCESS = I_PROCESS_NAME
                                           I_ENV     = 'QAS' ).
      WHEN LC_CON-PRD.
        R_RETURN = LCL_DATA=>GET_ENDPOINT( I_PROCESS = I_PROCESS_NAME
                                           I_ENV     = 'PRD' ).
    ENDCASE.
  ENDMETHOD.


  METHOD GET_API_HEADER.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    DATA : LV_TOKEN TYPE STRING.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R_RETURN.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R_RETURN.
    CLEAR LS_HEADER.

    IF I_DATA IS NOT INITIAL.
      APPEND LINES OF I_DATA TO R_RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD GET_API_LEN.
    R_RETURN = STRLEN( I_DATA ).
  ENDMETHOD.
ENDCLASS.
