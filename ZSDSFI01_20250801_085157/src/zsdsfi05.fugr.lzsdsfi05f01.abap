*----------------------------------------------------------------------*
***INCLUDE LZSDSFI05F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PF_CHECK_VALID_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_START_DATE  text
*      -->P_IV_END_DATE  text
*      -->P_IV_CURRENCY  text
*----------------------------------------------------------------------*
FORM PF_CHECK_VALID_DATA USING UV_START_DATE TYPE CHAR10
                               UV_END_DATE    TYPE CHAR10
                               UV_CURRENCY    TYPE CHAR10.

  DATA: LREF_MATCHER  TYPE REF TO CL_ABAP_MATCHER.

  DATA: LV_CURRENCY   TYPE T001-WAERS,
        LV_PATTERN    TYPE STRING VALUE '\d{4}-\d{2}-\d{2}',
        LV_YEAR(4)    TYPE C,
        LV_MONTH(2)   TYPE C,
        LV_DAY(2)     TYPE C,
        LV_VALID_DATE TYPE STRING.

  DATA: LV_DATE       TYPE SY-DATUM,
        LV_START_DATE TYPE SY-DATUM,
        LV_END_DATE   TYPE SY-DATUM.

*.. Check Validate on Currency
  LV_CURRENCY = UV_CURRENCY.
  CALL FUNCTION 'AIA_CHECK_EXIST_CURRENCY'
    EXPORTING
      I_CURRENCY         = LV_CURRENCY
    EXCEPTIONS
      NO_CURRENCY        = 1
      CURRENCY_NOT_EXIST = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    RAISE CURRENCY_NOT_EXIST.
  ENDIF.

*.. Check Validate on Start-End Date format YYYY-MM-DD
  CONCATENATE SY-DATUM+0(4) SY-DATUM+4(2) SY-DATUM+6(2)
  INTO LV_VALID_DATE SEPARATED BY '-'.
  CONCATENATE LV_VALID_DATE 'YYYY-MM-DD'
  INTO LV_VALID_DATE SEPARATED BY SPACE.

  "Start Date Check
  LREF_MATCHER = CL_ABAP_MATCHER=>CREATE( PATTERN = LV_PATTERN
                                             TEXT = UV_START_DATE ).
  IF ( LREF_MATCHER->MATCH( ) = ABAP_TRUE ).
    CLEAR: LV_YEAR, LV_MONTH, LV_DAY.
    SPLIT UV_START_DATE AT '-' INTO LV_YEAR LV_MONTH LV_DAY.
    CONDENSE: LV_YEAR, LV_MONTH, LV_DAY.
    IF ( NOT LV_YEAR BETWEEN 0001 AND 9999 ) OR
       ( NOT LV_MONTH BETWEEN 1 AND 12 ) OR
       ( NOT LV_DAY BETWEEN 1 AND 31 ).
      MESSAGE E888(FK) WITH LV_VALID_DATE RAISING DATE_IS_INVALID.
    ELSE.
      LV_DATE = UV_START_DATE+0(4) && UV_START_DATE+5(2) && UV_START_DATE+8(2).
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          DATE                      = LV_DATE
        EXCEPTIONS
          PLAUSIBILITY_CHECK_FAILED = 1
          OTHERS                    = 2.
      IF SY-SUBRC <> 0.
        MESSAGE E888(FK) WITH LV_VALID_DATE RAISING DATE_IS_INVALID.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE E888(FK) WITH LV_VALID_DATE RAISING DATE_IS_INVALID.
  ENDIF.

  "End Date Check
  LREF_MATCHER = CL_ABAP_MATCHER=>CREATE( PATTERN = LV_PATTERN
                                             TEXT = UV_END_DATE ).
  IF ( LREF_MATCHER->MATCH( ) = ABAP_TRUE ).
    CLEAR: LV_YEAR, LV_MONTH, LV_DAY.
    SPLIT UV_END_DATE AT '-' INTO LV_YEAR LV_MONTH LV_DAY.
    CONDENSE: LV_YEAR, LV_MONTH, LV_DAY.
    IF ( NOT LV_YEAR BETWEEN 0001 AND 9999 ) OR
       ( NOT LV_MONTH BETWEEN 1 AND 12 ) OR
       ( NOT LV_DAY BETWEEN 1 AND 31 ).
      MESSAGE E888(FK) WITH LV_VALID_DATE RAISING DATE_IS_INVALID.
    ELSE.
      LV_DATE = UV_END_DATE+0(4) && UV_END_DATE+5(2) && UV_END_DATE+8(2).
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          DATE                      = LV_DATE
        EXCEPTIONS
          PLAUSIBILITY_CHECK_FAILED = 1
          OTHERS                    = 2.
      IF SY-SUBRC <> 0.
        MESSAGE E888(FK) WITH LV_VALID_DATE RAISING DATE_IS_INVALID.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE E888(FK) WITH LV_VALID_DATE RAISING DATE_IS_INVALID.
  ENDIF.

  "Check Start Date Not Greater than End Date
  LV_START_DATE = UV_START_DATE+0(4) && UV_START_DATE+5(2) && UV_START_DATE+8(2).
  LV_END_DATE = UV_END_DATE+0(4) && UV_END_DATE+5(2) && UV_END_DATE+8(2).

  IF LV_START_DATE > LV_END_DATE.
    MESSAGE E032(FK) RAISING DATE_IS_INVALID.
  ENDIF.

ENDFORM. " PF_CHECK_VALID_DATA

*&---------------------------------------------------------------------*
*&      Form  PF_GET_CONFIG_API_URL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PF_GET_API_URL USING IV_UPDATE_K2 TYPE CHAR1
                 CHANGING CV_URL       TYPE STRING.

  IF IV_UPDATE_K2 EQ ABAP_TRUE.
    CV_URL = 'http://172.31.136.252/SDS.WEB.PRD/API/BOI/BOIRequest/GetAndUpdateExchangeRate/'.
  ELSE.
    CV_URL = 'http://172.31.136.252/SDS.WEB.UAT/API/BOI/BOIRequest/GetAndUpdateExchangeRate/'.
  ENDIF.
*  CONCATENATE 'http://172.31.136.252/SDS.WEB.UAT/API/BOI/BOIRequest/GetAndUpdateExchangeRate/?'
*              'start_period='
*              uv_start_date '&'
*              'end_period='
*              uv_end_date '&'
*              'currency='
*              uv_currency INTO cv_url.

ENDFORM. " PF_GET_CONFIG_API_URL
*&---------------------------------------------------------------------*
*&      Form  PF_CALL_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_URL  text
*----------------------------------------------------------------------*
FORM PF_CALL_API  USING UV_URL           TYPE STRING
                        UV_START_DATE    TYPE CHAR10
                        UV_END_DATE      TYPE CHAR10
                        UV_CURRENCY      TYPE CHAR10
               CHANGING CT_EXCHANGE_RATE TYPE ZSDSFIS097_TT
                        CV_MST           TYPE CHAR1
                        CV_MSG           TYPE STRING.

  DATA: LT_HEADER TYPE ZSDSCAS001_TT.

  DATA: LV_URL              TYPE STRING,
        LV_METHOD           TYPE STRING,
        LV_BODY_TEXT        TYPE STRING,
        LV_BODY_BIN         TYPE XSTRING,
        LV_LEN              TYPE I,
        LV_LEN_BIN          TYPE I,
        LV_RETURN_BODY_TEXT TYPE STRING,
        LV_RETURN_BODY_BIN  TYPE XSTRING,
        LV_MESSAGE          TYPE STRING,
        LV_STATUS           TYPE C.

  DATA: LS_RETURN TYPE ZSDSFIS094.

*..URL
  LV_URL = UV_URL.

*..Method
  LV_METHOD = 'POST'.

**..API Header 1
*  PERFORM F_SET_CLIENT_ID CHANGING LT_HEADER.
**..API Header 2
  PERFORM F_GET_HEADER CHANGING LT_HEADER.
*..Call API
  PERFORM F_GET_BODY USING UV_START_DATE
                           UV_END_DATE
                           UV_CURRENCY
                  CHANGING LV_BODY_TEXT.

  PERFORM F_GET_LEN  USING LV_BODY_TEXT
                  CHANGING LV_LEN.

  CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
    EXPORTING
      I_URL              = LV_URL
      I_METHOD           = LV_METHOD
      I_HEADER           = LT_HEADER
      I_BODY_TEXT        = LV_BODY_TEXT
      I_BODY_BIN         = LV_BODY_BIN
      I_LEN              = LV_LEN
      I_LEN_BIN          = LV_LEN_BIN
    IMPORTING
      E_RETURN           = LS_RETURN
      E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
      E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
      E_MESSAGE          = LV_MESSAGE
      E_STATUS           = LV_STATUS.
  IF LV_STATUS = 'S'.
    CT_EXCHANGE_RATE = LS_RETURN-RESULT-DATA-DATA_DETAIL.
  ELSE.
    CLEAR : CT_EXCHANGE_RATE.
  ENDIF.
  CV_MST = LV_STATUS.
  CV_MSG = LV_MESSAGE.
ENDFORM. " PF_CALL_API
*&---------------------------------------------------------------------*
*& Form F_GET_BODY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UV_START_DATE
*&      --> UV_END_DATE
*&      <-- LV_BODY_TEXT
*&---------------------------------------------------------------------*
FORM F_GET_BODY  USING    UV_START_DATE TYPE CHAR10
                          UV_END_DATE   TYPE CHAR10
                          UV_CURRENCY
                 CHANGING LV_BODY_TEXT  TYPE STRING.

  DATA : LS_CURRENCY TYPE STRING.

  DATA : BEGIN OF LS_DATA,
           START_PERIOD TYPE C LENGTH 10,
           END_PERIOD   TYPE C LENGTH 10,
           CURRENCY     LIKE TABLE OF LS_CURRENCY,
         END OF LS_DATA.

  LS_DATA-START_PERIOD = UV_START_DATE.
  LS_DATA-END_PERIOD   = UV_END_DATE.
  LS_CURRENCY          = UV_CURRENCY.
  APPEND LS_CURRENCY TO  LS_DATA-CURRENCY.

  CALL METHOD /UI2/CL_JSON=>SERIALIZE
    EXPORTING
      DATA        = LS_DATA
      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE
    RECEIVING
      R_JSON      = LV_BODY_TEXT
    EXCEPTIONS
      OTHERS      = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_LEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_BODY_TEXT
*&      <-- LV_LEN
*&---------------------------------------------------------------------*
FORM F_GET_LEN  USING    LV_BODY_TEXT
                CHANGING LV_LEN.

  LV_LEN = STRLEN( LV_BODY_TEXT ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM F_GET_HEADER CHANGING LT_HEADER TYPE ZSDSCAS001_TT.

  DATA : LS_HEADER TYPE ZSDSCAS001.

  LS_HEADER-NAME  = 'Content-Type'.
  LS_HEADER-VALUE = 'application/json'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

  LS_HEADER-NAME  = 'Accept'.
  LS_HEADER-VALUE = 'application/json'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_CLIENT_ID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM F_SET_CLIENT_ID  CHANGING LT_HEADER TYPE ZSDSCAS001_TT.
  DATA : LS_HEADER TYPE ZSDSCAS001.
  LS_HEADER-NAME  = 'X-IBM-Client-Id'.
  LS_HEADER-VALUE = 'b2882771-4960-4c0e-aa26-2ffdcf8a0164'.
  APPEND LS_HEADER TO LT_HEADER.
  CLEAR LS_HEADER.
ENDFORM.
