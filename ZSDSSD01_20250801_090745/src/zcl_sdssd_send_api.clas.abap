class ZCL_SDSSD_SEND_API definition
  public
  final
  create public .

public section.

  methods SEND_DO_TO_WMS
    importing
      !IT_DATA type ZSDSMMS040_TT
    exporting
      !E_MESTYPE type CHAR1
    changing
      value(C_RETURN) type ANY optional .
  methods GET_TOKEN_SONY
    returning
      value(R_TOKEN) type STRING .
  methods SEND_DO_FG_TO_SONY
    importing
      !IT_DATA type ZSDSMMS042_TT
    exporting
      !E_MESTYPE type CHAR1
    changing
      !C_RETURN type ANY .
  methods SEND_DO_SP_TO_SONY
    importing
      !IT_DATA type ZSDSMMS041_TT
    exporting
      !E_MESTYPE type CHAR1
    changing
      !C_RETURN type ANY .
protected section.
private section.

  constants:
    BEGIN OF GC_CON,
                POST TYPE C LENGTH 4 VALUE 'POST',
                GET  TYPE C LENGTH 3 VALUE 'GET',
                E    TYPE C LENGTH 1 VALUE 'E',
                S    TYPE C LENGTH 1 VALUE 'S',
              END OF GC_CON .
ENDCLASS.



CLASS ZCL_SDSSD_SEND_API IMPLEMENTATION.


  METHOD GET_TOKEN_SONY.

    DATA : BEGIN OF LS_BODY,
             REFRESH_TOKEN TYPE STRING,
           END OF LS_BODY.

    DATA : BEGIN OF LS_RETURN,
             IS_SUCCESS    TYPE STRING,
             ACCESS_TOKEN  TYPE STRING,
             REFRESH_TOKEN TYPE STRING,
           END OF LS_RETURN.

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

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    DATA: BEGIN OF LS_DATA,
            SUCCESS      TYPE STRING,
            MESSAGE      TYPE STRING,
            ERROR_CODE   TYPE STRING,
            MESSAGE_CODE TYPE STRING,
            DATA         TYPE STRING,
            NEXT_CONTROL TYPE STRING,
            ORDER_NUMBER TYPE STRING,
            SHOW_MESSAGE TYPE STRING,
            PAGER        TYPE STRING,
          END OF LS_DATA.

    CONSTANTS : BEGIN OF LC_CON,
                  SONY TYPE C LENGTH 4 VALUE 'SONY',
                END OF LC_CON.

    SELECT SINGLE REFRESH_TOKEN
      FROM ZSDSCAC008
      INTO LS_BODY-REFRESH_TOKEN
      WHERE PROCESS EQ LC_CON-SONY.

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT_SONY_TOKEN( ).
    LT_HEADER    = LCL_DATA=>HEADER_API_GET_TOKEN( ).
    LV_BODY_TEXT = LCL_DATA=>BODY_API( I_DATA = LS_BODY ).
    LV_LEN       = LCL_DATA=>BODY_API_LEN( LV_BODY_TEXT ).

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
    IF LV_STATUS EQ GC_CON-S.
      R_TOKEN = LS_RETURN-ACCESS_TOKEN.
      IF LS_RETURN-REFRESH_TOKEN IS NOT INITIAL.
        UPDATE ZSDSCAC008 SET TOKEN         = LS_RETURN-ACCESS_TOKEN
                              REFRESH_TOKEN = LS_RETURN-REFRESH_TOKEN
                              AENAM         = SY-UNAME
                              AEDAT         = SY-DATUM
                              AEZET         = SY-UZEIT
                        WHERE PROCESS EQ LC_CON-SONY.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD SEND_DO_FG_TO_SONY.
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

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    DATA: BEGIN OF LS_DATA,
            SUCCESS      TYPE STRING,
            MESSAGE      TYPE STRING,
            ERROR_CODE   TYPE STRING,
            MESSAGE_CODE TYPE STRING,
            DATA         TYPE STRING,
            NEXT_CONTROL TYPE STRING,
            ORDER_NUMBER TYPE STRING,
            SHOW_MESSAGE TYPE STRING,
            PAGER        TYPE STRING,
          END OF LS_DATA.

    DATA : LV_TOKEN TYPE STRING.

*    LV_TOKEN     = GET_TOKEN_SONY( ).

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT_SONY_FG( ).
    LT_HEADER    = LCL_DATA=>HEADER_API_TOKEN_FG( LV_TOKEN ).
    LV_BODY_TEXT = LCL_DATA=>BODY_API( I_DATA_TAB = IT_DATA ).
    LV_LEN       = LCL_DATA=>BODY_API_LEN( LV_BODY_TEXT ).

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
        E_RETURN           = C_RETURN
        E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
        E_MESSAGE          = LV_MESSAGE
        E_STATUS           = LV_STATUS.
    IF LV_STATUS EQ GC_CON-S.
      E_MESTYPE = GC_CON-S.
    ELSE.
      E_MESTYPE = GC_CON-E.
    ENDIF.
  ENDMETHOD.


  METHOD SEND_DO_SP_TO_SONY.
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

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    DATA: BEGIN OF LS_DATA,
            SUCCESS      TYPE STRING,
            MESSAGE      TYPE STRING,
            ERROR_CODE   TYPE STRING,
            MESSAGE_CODE TYPE STRING,
            DATA         TYPE STRING,
            NEXT_CONTROL TYPE STRING,
            ORDER_NUMBER TYPE STRING,
            SHOW_MESSAGE TYPE STRING,
            PAGER        TYPE STRING,
          END OF LS_DATA.

    DATA : LV_TOKEN TYPE STRING.

*    LV_TOKEN     = GET_TOKEN_SONY( ).

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT_SONY_SP( ).
    LT_HEADER    = LCL_DATA=>HEADER_API_TOKEN_SP( LV_TOKEN ).
    LV_BODY_TEXT = LCL_DATA=>BODY_API( I_DATA_TAB = IT_DATA ).
    LV_LEN       = LCL_DATA=>BODY_API_LEN( LV_BODY_TEXT ).

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
        E_RETURN           = C_RETURN
        E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
        E_MESSAGE          = LV_MESSAGE
        E_STATUS           = LV_STATUS.
    IF LV_STATUS EQ GC_CON-S.
      E_MESTYPE = GC_CON-S.
    ELSE.
      E_MESTYPE = GC_CON-E.
    ENDIF.
  ENDMETHOD.


  METHOD SEND_DO_TO_WMS.
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

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    DATA: BEGIN OF LS_DATA,
            SUCCESS      TYPE STRING,
            MESSAGE      TYPE STRING,
            ERROR_CODE   TYPE STRING,
            MESSAGE_CODE TYPE STRING,
            DATA         TYPE STRING,
            NEXT_CONTROL TYPE STRING,
            ORDER_NUMBER TYPE STRING,
            SHOW_MESSAGE TYPE STRING,
            PAGER        TYPE STRING,
          END OF LS_DATA.

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT_WMS( ).
    LT_HEADER    = LCL_DATA=>HEADER_API_NORMAL( ).
    LV_BODY_TEXT = LCL_DATA=>BODY_API( I_DATA_TAB = IT_DATA ).
    LV_LEN       = LCL_DATA=>BODY_API_LEN( LV_BODY_TEXT ).

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
        E_RETURN           = C_RETURN
        E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
        E_MESSAGE          = LV_MESSAGE
        E_STATUS           = LV_STATUS.
    IF LV_STATUS EQ GC_CON-S.
      E_MESTYPE = GC_CON-S.
    ELSE.
      E_MESTYPE = GC_CON-E.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
