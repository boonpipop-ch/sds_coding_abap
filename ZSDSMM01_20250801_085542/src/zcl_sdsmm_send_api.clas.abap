class ZCL_SDSMM_SEND_API definition
  public
  final
  create public .

public section.

  class-methods SEND_MATDOC_TO_SONY
    importing
      !IT_MSEG type TY_T_MSEG
      !I_MKPF type MKPF
    returning
      value(R_RETURN) type CHAR255 .
protected section.
PRIVATE SECTION.

  CONSTANTS:
    BEGIN OF GC_CON,
      POST TYPE C LENGTH 4 VALUE 'POST',
      GET  TYPE C LENGTH 3 VALUE 'GET',
      E    TYPE C LENGTH 1 VALUE 'E',
      S    TYPE C LENGTH 1 VALUE 'S',
    END OF GC_CON .
ENDCLASS.



CLASS ZCL_SDSMM_SEND_API IMPLEMENTATION.


  METHOD SEND_MATDOC_TO_SONY.
    DATA: LS_DATA TYPE ZSDSMMS041,
          LT_DATA TYPE ZSDSMMS041_TT.

    DATA: LR_STORAGE TYPE RANGE OF MSEG-LGORT.

    DATA : BEGIN OF LS_STO,
             WERKS TYPE MSEG-WERKS,
             UMLGO TYPE MSEG-UMLGO,
           END OF LS_STO.
    DATA : LT_STO LIKE HASHED TABLE OF LS_STO WITH UNIQUE KEY WERKS UMLGO.

    DATA : BEGIN OF LS_MAT,
             MATNR TYPE MARA-MATNR,
           END OF LS_MAT.
    DATA : LT_MAT LIKE HASHED TABLE OF LS_MAT WITH UNIQUE KEY MATNR.

    DATA : LV_MATDOC TYPE MKPF-MBLNR.

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

    DATA: BEGIN OF LS_RETURN,
            IS_SUCCESS TYPE STRING,
            STATUS     TYPE P DECIMALS 0,
            MESSAGE    TYPE STRING,
          END OF LS_RETURN.

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    CONSTANTS : BEGIN OF LC_CON,
                  I_REPID TYPE CHAR20 VALUE 'SEND_MATDOC_TO_SONY',
                  I_PARAM TYPE CHAR20 VALUE 'STORAGE',
                END OF LC_CON.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID   = LC_CON-I_REPID
                                                  I_PARAM   = LC_CON-I_PARAM
                                         CHANGING CR_RETURN = LR_STORAGE
                                       ).

    LT_MAT =  CORRESPONDING #( IT_MSEG  DISCARDING DUPLICATES ).
    LT_STO =  CORRESPONDING #( IT_MSEG DISCARDING DUPLICATES ).

    SELECT T001L~LGORT,
           T001L~LGOBE
      FROM @LT_STO AS A
      INNER JOIN T001L ON A~WERKS EQ T001L~WERKS AND
                          A~UMLGO EQ T001L~LGORT
      INTO TABLE @DATA(LT_STO_DESC).

    SELECT MAKT~MATNR,
           MAKT~MAKTX
      FROM @LT_MAT AS A
      INNER JOIN MAKT ON A~MATNR    EQ MAKT~MATNR AND
                         MAKT~SPRAS EQ @SY-LANGU
      INTO TABLE @DATA(LT_MAT_DESC).

    SELECT BWART,
           DELIVERY_TO
      FROM ZSDSMMT017
      INTO TABLE @DATA(LT_DELI).

    CLEAR : LV_MATDOC.
    LOOP AT IT_MSEG INTO DATA(LS_MSEG) WHERE LGORT IN LR_STORAGE[].
      LV_MATDOC             = I_MKPF-MBLNR.
      LS_DATA-LFART         = '0'.
      LS_DATA-CUST_CODE     = 'SDS'.
      LS_DATA-VBELN         = I_MKPF-MBLNR.
      LS_DATA-LFDAT         = I_MKPF-BUDAT.
      LS_DATA-ERDAT         = I_MKPF-CPUDT.
      LS_DATA-POSNR         = LS_MSEG-ZEILE.
      LS_DATA-MATNR         = LS_MSEG-MATNR.
      READ TABLE LT_MAT_DESC INTO DATA(LS_MAT_DESC)
      WITH KEY MATNR = LS_MSEG-MATNR.
      IF SY-SUBRC EQ 0.
        LS_DATA-ARKTX       = LS_MAT_DESC-MAKTX.
      ENDIF.
      LS_DATA-LGMNG         = LS_MSEG-MENGE.
      LS_DATA-KUNNR         = LS_MSEG-UMLGO.
      READ TABLE LT_STO_DESC INTO DATA(LS_STO_DESC)
      WITH KEY LGORT = LS_MSEG-UMLGO.
      IF SY-SUBRC EQ 0.
        LS_DATA-CUST_NAME     = LS_STO_DESC-LGOBE.
      ENDIF.
*      IF LS_MSEG-SHKZG EQ 'H'.
*        LS_DATA-DELI_TO       = 'TRF_OUT'.
*      ELSE.
*        LS_DATA-DELI_TO       = 'TRF_IN'.
*      ENDIF.

      READ TABLE LT_DELI INTO DATA(LS_DELI)
      WITH KEY BWART = LS_MSEG-BWART.
      IF SY-SUBRC EQ 0.
        LS_DATA-DELI_TO = LS_DELI-DELIVERY_TO.
      ENDIF.

      LS_DATA-REMARK        = ''.
      LS_DATA-PICHON        = ''.
      LS_DATA-REMARK_PICHON = ''.
      LS_DATA-LGORT         = LS_MSEG-LGORT.

      APPEND LS_DATA TO LT_DATA.
      CLEAR LS_DATA.
    ENDLOOP.

    R_RETURN = ABAP_TRUE.
    IF LT_DATA IS NOT INITIAL.
      LV_METHOD    = GC_CON-POST.
      LV_URL       = LCL_DATA=>ENDPOINT_API_MATDOC( ).
      LT_HEADER    = LCL_DATA=>HEADER_API_MATDOC( ).
      LV_BODY_TEXT = LCL_DATA=>BODY_API_MATDOC( I_DATA_TAB = LT_DATA ).
      LV_LEN       = LCL_DATA=>BODY_API_LEN_MATDOC( LV_BODY_TEXT ).

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
      IF LS_RETURN-IS_SUCCESS EQ ABAP_TRUE.
        LCL_DATA=>UPDATE_STATUS( I_DOC  = LV_MATDOC
                                 I_YEAR = SY-DATUM+0(4)
                                 I_STAU = 'S'
                                 I_MESS = TEXT-S01 ).
        CLEAR : R_RETURN.
      ELSE.
        IF LS_RETURN-MESSAGE IS NOT INITIAL.
          R_RETURN = LS_RETURN-MESSAGE.
        ELSE.
          R_RETURN          = TEXT-E01.
          LS_RETURN-MESSAGE = TEXT-E01.
        ENDIF.
        LCL_DATA=>UPDATE_STATUS( I_DOC  = LV_MATDOC
                                 I_YEAR = SY-DATUM+0(4)
                                 I_STAU = 'E'
                                 I_MESS = LS_RETURN-MESSAGE ).
      ENDIF.
    ELSE.
      R_RETURN          = TEXT-E02.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
