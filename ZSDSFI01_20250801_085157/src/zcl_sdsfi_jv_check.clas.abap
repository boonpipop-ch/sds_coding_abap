class ZCL_SDSFI_JV_CHECK definition
  public
  create public .

public section.

  types:
    BEGIN OF GY_K2_RES,
        AMOUNT      TYPE STRING,
        MESSAGETYPE TYPE STRING,
        MESSAGETEXT TYPE STRING,
      END OF GY_K2_RES .
  types:
    BEGIN OF TS_GEN_C,
        REPID        TYPE  ZSDSCAC001-REPID,
        PARAM        TYPE  ZSDSCAC001-PARAM,
        PARAM_EXT    TYPE  ZSDSCAC001-PARAM_EXT,
        SEQUENCE     TYPE  ZSDSCAC001-SEQUENCE,
        PARAM_SIGN   TYPE  ZSDSCAC001-PARAM_SIGN,
        PARAM_OPTION TYPE  ZSDSCAC001-PARAM_OPTION,
        VALUE_LOW    TYPE  ZSDSCAC001-VALUE_LOW,
        VALUE_HIGH   TYPE  ZSDSCAC001-VALUE_HIGH,
        VDESC        TYPE  ZSDSCAC001-VDESC,
      END OF TS_GEN_C .
  types:
    TT_GEN_C  TYPE STANDARD TABLE OF TS_GEN_C .

  constants:
    BEGIN OF GC_CON,
                  USER   TYPE C LENGTH 4  VALUE 'USER',
                  PASS   TYPE C LENGTH 4  VALUE 'PASS',
                  DBQ    TYPE C LENGTH 1  VALUE '"',
                  SGQ    TYPE C LENGTH 1  VALUE '''',
                  CMA    TYPE C LENGTH 1  VALUE ',',
                  COB    TYPE C LENGTH 1  VALUE '{',
                  CCB    TYPE C LENGTH 1  VALUE '}',
                  SOB    TYPE C LENGTH 1  VALUE '[',
                  SCB    TYPE C LENGTH 1  VALUE ']',
                  FOLIO  TYPE C LENGTH 8  VALUE '"folio":',
                  EXPDT  TYPE C LENGTH 26 VALUE '"expectedDuration": 86400,',
                  PRITY  TYPE C LENGTH 15 VALUE '"priority": 1, ',
                  DATFD  TYPE C LENGTH 13 VALUE '"dataFields":',
                  PARAM  TYPE C LENGTH 4  VALUE 'PARA',
                  PARA1  TYPE C LENGTH 5  VALUE 'PARA1',
                  PARA2  TYPE C LENGTH 5  VALUE 'PARA2',
                  PARA3  TYPE C LENGTH 5  VALUE 'PARA3',
                  PARA4  TYPE C LENGTH 5  VALUE 'PARA4',
                  PARA5  TYPE C LENGTH 5  VALUE 'PARA5',
                  PARA6  TYPE C LENGTH 5  VALUE 'PARA6',
                  PARA7  TYPE C LENGTH 5  VALUE 'PARA7',
                  PARA8  TYPE C LENGTH 5  VALUE 'PARA8',
                  PARA9  TYPE C LENGTH 5  VALUE 'PARA9',
                  PARA10 TYPE C LENGTH 6  VALUE 'PARA10',
                  PARA11 TYPE C LENGTH 6  VALUE 'PARA11',
                  PARA12 TYPE C LENGTH 6  VALUE 'PARA12',
                  PARA13 TYPE C LENGTH 6  VALUE 'PARA13',
                  PARA14 TYPE C LENGTH 6  VALUE 'PARA14',
                  PARA15 TYPE C LENGTH 6  VALUE 'PARA15',
                  PARA16 TYPE C LENGTH 6  VALUE 'PARA16',
                  PARA17 TYPE C LENGTH 6  VALUE 'PARA17',
                  PARA18 TYPE C LENGTH 6  VALUE 'PARA18',
                  PARA19 TYPE C LENGTH 6  VALUE 'PARA19',
                  PARA20 TYPE C LENGTH 6  VALUE 'PARA20',
                  END    TYPE C LENGTH 3  VALUE 'END',
                  URL    TYPE C LENGTH 3  VALUE 'URL',
                  METHOD TYPE C LENGTH 6  VALUE 'METHOD',

                  "Email
                  REPID  TYPE ZSDSCAC001-REPID VALUE 'ZCL_SDSFI_JV_CHECK',
                END OF GC_CON .
  data GS_K2_RES type GY_K2_RES .
  data GT_GEN_C type TT_GEN_C .
  data GS_GEN_C type TS_GEN_C .
  data GS_FIT060 type ZSDSFIT060 .
  data GV_TEST type CHAR01 .
  data:
    GR_BLART  TYPE RANGE OF BKPF-BLART .
  class-data LO type ref to ZCL_SDSFI_JV_CHECK .

  methods CONSTRUCTOR .
  methods START_PROCESS .
  methods GET_BODY_TEXT
    exporting
      !E_LEN type I
      !E_DATA type STRING .
  methods GET_USER_PASS
    exporting
      !E_USER type STRING
      !E_PASS type STRING .
  methods GET_URL
    returning
      value(R) type STRING .
  methods GET_METHOD
    returning
      value(R) type STRING .
  methods GET_HEADER
    returning
      value(R) type ZSDSCAS001_TT .
private section.
ENDCLASS.



CLASS ZCL_SDSFI_JV_CHECK IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    DATA : LR_PRRAM TYPE RANGE OF ZSDSCAC001-PARAM.
    ZCL_SDSCA_UTILITIES=>GET_GEN_C( EXPORTING IF_REPID  = GC_CON-REPID
                                              IRT_PARAM = LR_PRRAM
                                    IMPORTING ET_GEN_C = GT_GEN_C ).

  ENDMETHOD.


  METHOD GET_BODY_TEXT.

    DATA : LV_DATA TYPE C LENGTH 255.
    DATA : LS_GEN_C LIKE GS_GEN_C.
    DATA : LT_TMP LIKE GT_GEN_C.

    LT_TMP = GT_GEN_C.

    DELETE LT_TMP WHERE PARAM+0(4) NE GC_CON-PARAM.
*    IF P_FORIO IS NOT INITIAL.
*      CONCATENATE GC_CON-COB
*                  GC_CON-FOLIO GC_CON-DBQ P_FORIO GC_CON-DBQ GC_CON-CMA
*                  GC_CON-EXPDT
*                  GC_CON-PRITY
*                  GC_CON-DATFD INTO E_DATA.
*    ENDIF.
*

*{
*  "journalVoucherDocNo": "JV-25040001",
*  "amount": 123,
*  "userName": "boonpipop",
*  "test": "X"
*}

    LOOP AT LT_TMP INTO LS_GEN_C.
      MOVE-CORRESPONDING LS_GEN_C TO GS_GEN_C.
      AT FIRST.
        CONCATENATE E_DATA GC_CON-COB  INTO E_DATA.
      ENDAT.

      IF GS_GEN_C-PARAM = GC_CON-PARA1.
        LV_DATA = GS_FIT060-JV_DOCNO.
      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA2.
        LV_DATA = GS_FIT060-SUM_AMT.
      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA3.
        LV_DATA = SY-UNAME.
      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA4.
        LV_DATA = GV_TEST.
      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA5.
        LV_DATA = GS_FIT060-POSTED.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA6.
*        LV_DATA = P_PARA6.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA7.
*        LV_DATA = P_PARA7.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA8.
*        LV_DATA = P_PARA8.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA9.
*        LV_DATA = P_PARA9.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA10.
*        LV_DATA = P_PARA10.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA11.
*        LV_DATA = P_PARA11.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA12.
*        LV_DATA = P_PARA12.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA13.
*        LV_DATA = P_PARA13.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA14.
*        LV_DATA = P_PARA14.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA15.
*        LV_DATA = P_PARA15.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA16.
*        LV_DATA = P_PARA16.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA17.
*        LV_DATA = P_PARA17.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA18.
*        LV_DATA = P_PARA18.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA19.
*        LV_DATA = P_PARA19.
*      ELSEIF GS_GEN_C-PARAM = GC_CON-PARA20.
*        LV_DATA = P_PARA20.
      ENDIF.

      AT LAST.
        CONCATENATE E_DATA GS_GEN_C-VALUE_LOW GC_CON-DBQ LV_DATA GC_CON-DBQ GC_CON-CCB INTO E_DATA.
        EXIT.
      ENDAT.
      CONCATENATE E_DATA GS_GEN_C-VALUE_LOW GC_CON-DBQ LV_DATA GC_CON-DBQ GC_CON-CMA INTO E_DATA.
    ENDLOOP.

*    CONCATENATE E_DATA GC_CON-CCB INTO E_DATA.
    E_LEN = STRLEN( E_DATA ).
  ENDMETHOD.


  METHOD GET_HEADER.
    CONSTANTS : BEGIN OF LC_CON,
                  CONTYPE TYPE STRING VALUE 'Content-Type' ##NO_TEXT,
                  ACCEP   TYPE STRING VALUE 'Accept' ##NO_TEXT,
                  APPJSON TYPE STRING VALUE 'application/json' ##NO_TEXT,
                END OF LC_CON.

    R = VALUE #( ( NAME = LC_CON-CONTYPE VALUE = LC_CON-APPJSON )
                 ( NAME = LC_CON-ACCEP   VALUE = LC_CON-APPJSON ) ).

  ENDMETHOD.


  METHOD GET_METHOD.
    READ TABLE GT_GEN_C INTO GS_GEN_C
    WITH KEY PARAM = GC_CON-METHOD.
    IF SY-SUBRC EQ 0.
      R = GS_GEN_C-VALUE_LOW.
    ENDIF.
  ENDMETHOD.


  METHOD GET_URL.
    READ TABLE GT_GEN_C INTO GS_GEN_C
    WITH KEY PARAM = GC_CON-URL.
    IF SY-SUBRC EQ 0.
      R = GS_GEN_C-VALUE_LOW.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN R WITH ''.
    ENDIF.
  ENDMETHOD.


  METHOD GET_USER_PASS.
*    READ TABLE GT_GEN_C INTO GS_GEN_C
*    WITH KEY PARAM = GC_CON-USER.
*    IF SY-SUBRC EQ 0.
*      E_USER = GS_GEN_C-VALUE_LOW.
*    ENDIF.
*
*    READ TABLE GT_GEN_C INTO GS_GEN_C
*    WITH KEY PARAM = GC_CON-PASS.
*    IF SY-SUBRC EQ 0.
*      E_PASS = GS_GEN_C-VALUE_LOW.
*    ENDIF.
  ENDMETHOD.


  METHOD START_PROCESS.
    DATA : LCL_ZCL_SDSCA_CAL_API TYPE REF TO ZCL_SDSCA_CAL_API.

    DATA : I_URL              TYPE  STRING,
           I_METHOD           TYPE  STRING,
           I_HEADER           TYPE  ZSDSCAS001_TT,
           I_BODY_TEXT        TYPE  STRING,
           I_BODY_BIN         TYPE  XSTRING,
           I_LEN              TYPE  I,
           I_LEN_BIN          TYPE  I,
           I_FROM             TYPE  ZSDSCAS001_TT,
           I_USER             TYPE STRING,
           I_PASS             TYPE STRING,
*           E_RETURN            Type  ANY,
           E_RETURN_BODY_TEXT TYPE  STRING,
           E_RETURN_BODY_BIN  TYPE  XSTRING,
           E_MESSAGE          TYPE  STRING,
           E_STATUS           TYPE  CHAR1.

    IF LCL_ZCL_SDSCA_CAL_API IS INITIAL.
      CREATE OBJECT LCL_ZCL_SDSCA_CAL_API.
    ENDIF.

    I_URL    = GET_URL( ).
    I_METHOD = GET_METHOD( ).
    I_HEADER = GET_HEADER( ).

    GET_USER_PASS( IMPORTING E_USER = I_USER
                             E_PASS = I_PASS ).

    GET_BODY_TEXT( IMPORTING E_LEN  = I_LEN
                             E_DATA = I_BODY_TEXT ).

    CALL METHOD LCL_ZCL_SDSCA_CAL_API->CALL_API
      EXPORTING
        I_URL              = I_URL
        I_METHOD           = I_METHOD
        I_HEADER           = I_HEADER
        I_BODY_TEXT        = I_BODY_TEXT
        I_BODY_BIN         = I_BODY_BIN
        I_LEN              = I_LEN
        I_LEN_BIN          = I_LEN_BIN
        I_FROM             = I_FROM
        I_USER             = I_USER
        I_PASS             = I_PASS
      IMPORTING
        E_RETURN           = GS_K2_RES
        E_RETURN_BODY_TEXT = E_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = E_RETURN_BODY_BIN
        E_MESSAGE          = E_MESSAGE
        E_STATUS           = E_STATUS.

  ENDMETHOD.
ENDCLASS.
