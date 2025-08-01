*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.

    CLASS-METHODS :
      ENDPOINT_API_MATDOC RETURNING VALUE(R) TYPE STRING,
      HEADER_API_MATDOC RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY_API_MATDOC IMPORTING I_DATA     TYPE ANY OPTIONAL
                                I_DATA_TAB TYPE ANY TABLE OPTIONAL
                      RETURNING VALUE(R)   TYPE STRING,
      BODY_API_LEN_MATDOC IMPORTING I_DATA   TYPE STRING
                          RETURNING VALUE(R) TYPE I,
      UPDATE_STATUS IMPORTING I_DOC  TYPE ANY
                              I_YEAR TYPE ANY
                              I_STAU TYPE ANY
                              I_MESS TYPE ANY.

    CONSTANTS : BEGIN OF LC_CON,
                  S   TYPE C LENGTH 1 VALUE 'S',
                  E   TYPE C LENGTH 1 VALUE 'E',
                  DEV TYPE C LENGTH 3 VALUE 'F36',
                  QAS TYPE C LENGTH 3 VALUE 'F46',
                  PRD TYPE C LENGTH 3 VALUE 'F56',
                END OF LC_CON.


ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD ENDPOINT_API_MATDOC.
    CONSTANTS : BEGIN OF LC_WMS,
                  REPID TYPE C LENGTH 22 VALUE 'ENDPOINT_SONY_ORDER_DO',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  SP    TYPE C LENGTH 5  VALUE 'SP',
                END OF LC_WMS.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-DEV
                                                      I_PARAM_EXT         = LC_WMS-SP
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-QAS
                                                      I_PARAM_EXT         = LC_WMS-SP
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-PRD
                                                      I_PARAM_EXT         = LC_WMS-SP
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD HEADER_API_MATDOC.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    DATA : LV_TOKEN TYPE STRING.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.
  ENDMETHOD.
  METHOD BODY_API_MATDOC.
    IF I_DATA IS NOT INITIAL.
      CALL METHOD /UI2/CL_JSON=>SERIALIZE
        EXPORTING
          DATA        = I_DATA
          PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE
        RECEIVING
          R_JSON      = R
        EXCEPTIONS
          OTHERS      = 1.
    ELSEIF I_DATA_TAB IS NOT INITIAL.
      CALL METHOD /UI2/CL_JSON=>SERIALIZE
        EXPORTING
          DATA        = I_DATA_TAB
          PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE
        RECEIVING
          R_JSON      = R
        EXCEPTIONS
          OTHERS      = 1.
    ENDIF.

  ENDMETHOD.
  METHOD  BODY_API_LEN_MATDOC.
    R = STRLEN( I_DATA ).
  ENDMETHOD.
  METHOD UPDATE_STATUS.
    DATA : LS_ZSDSMMT029 TYPE ZSDSMMT029.

    LS_ZSDSMMT029-MBLNR = I_DOC .
    LS_ZSDSMMT029-MJAHR = I_YEAR.
    LS_ZSDSMMT029-MSGTX = I_MESS.
    LS_ZSDSMMT029-STATU = I_STAU.
    LS_ZSDSMMT029-ERNAM = SY-UNAME.
    LS_ZSDSMMT029-ERDAT = SY-DATUM.
    LS_ZSDSMMT029-ERZET = SY-UZEIT.
    LS_ZSDSMMT029-AENAM = SY-UNAME.
    LS_ZSDSMMT029-AEDAT = SY-DATUM.
    LS_ZSDSMMT029-AEZET = SY-UZEIT.

    MODIFY ZSDSMMT029 FROM LS_ZSDSMMT029.
  ENDMETHOD.
ENDCLASS.
