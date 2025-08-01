*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.

    CLASS-METHODS :
      HEADER_API_NORMAL RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      HEADER_API_GET_TOKEN RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      HEADER_API_TOKEN IMPORTING I_DATA   TYPE STRING
                       RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      HEADER_API_TOKEN_FG IMPORTING I_DATA   TYPE STRING
                          RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      HEADER_API_TOKEN_SP IMPORTING I_DATA   TYPE STRING
                          RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY_API IMPORTING I_DATA     TYPE ANY OPTIONAL
                         I_DATA_TAB TYPE ANY TABLE OPTIONAL
               RETURNING VALUE(R)   TYPE STRING,
      BODY_API_LEN IMPORTING I_DATA   TYPE STRING
                   RETURNING VALUE(R) TYPE I,
      ENDPOINT_WMS  RETURNING VALUE(R) TYPE STRING,
      ENDPOINT_SONY_TOKEN RETURNING VALUE(R) TYPE STRING,
      ENDPOINT_SONY_FG RETURNING VALUE(R) TYPE STRING,
      ENDPOINT_SONY_SP RETURNING VALUE(R) TYPE STRING,
      HEADER_TOKEN RETURNING VALUE(R) TYPE STRING,
      HEADER_FG RETURNING VALUE(R) TYPE STRING,
      HEADER_SP RETURNING VALUE(R) TYPE STRING.

    CONSTANTS : BEGIN OF LC_CON,
                  S   TYPE C LENGTH 1 VALUE 'S',
                  E   TYPE C LENGTH 1 VALUE 'E',
                  DEV TYPE C LENGTH 3 VALUE 'F36',
                  QAS TYPE C LENGTH 3 VALUE 'F46',
                  PRD TYPE C LENGTH 3 VALUE 'F56',
                END OF LC_CON.

ENDCLASS.
CLASS LCL_dATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD HEADER_API_NORMAL.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
  ENDMETHOD.
  METHOD HEADER_API_GET_TOKEN.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.

    LS_HEADER-NAME  = 'X-Api-Url'.
    LS_HEADER-VALUE = HEADER_TOKEN( ).
    APPEND LS_HEADER TO R.

    LS_HEADER-NAME  = 'X-Api-Method'.
    LS_HEADER-VALUE = 'POST'.
    APPEND LS_HEADER TO R.

  ENDMETHOD.
  METHOD HEADER_TOKEN.
    CONSTANTS : BEGIN OF LC_WMS,
                  REPID TYPE C LENGTH 22 VALUE 'ENDPOINT_SONY_ORDER_DO',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  TOKEN TYPE C LENGTH 5  VALUE 'TOKEN',
                END OF LC_WMS.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-DEV
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-QAS
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-PRD
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
    ENDCASE.

  ENDMETHOD.
  METHOD HEADER_SP.
    CONSTANTS : BEGIN OF LC_WMS,
                  REPID TYPE C LENGTH 22 VALUE 'ENDPOINT_SONY_ORDER_DO',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  TOKEN TYPE C LENGTH 5  VALUE 'SP',
                END OF LC_WMS.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-DEV
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-QAS
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-PRD
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
    ENDCASE.

  ENDMETHOD.
  METHOD HEADER_FG.
    CONSTANTS : BEGIN OF LC_WMS,
                  REPID TYPE C LENGTH 22 VALUE 'ENDPOINT_SONY_ORDER_DO',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  TOKEN TYPE C LENGTH 5  VALUE 'FG',
                END OF LC_WMS.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-DEV
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-QAS
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-PRD
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN_HIGH       = R ).
    ENDCASE.

  ENDMETHOD.
  METHOD HEADER_API_TOKEN.
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

    CONCATENATE 'Bearer'
                I_DATA
         INTO LV_TOKEN SEPARATED BY SPACE.

    LS_HEADER-NAME  = 'Authorization'.
    LS_HEADER-VALUE = LV_TOKEN.
    APPEND LS_HEADER TO R.
  ENDMETHOD.
  METHOD HEADER_API_TOKEN_FG.
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

*    CONCATENATE 'Bearer'
*                I_DATA
*         INTO LV_TOKEN SEPARATED BY SPACE.
*
*    LS_HEADER-NAME  = 'Authorization'.
*    LS_HEADER-VALUE = LV_TOKEN.
*    APPEND LS_HEADER TO R.

*    LS_HEADER-NAME  = 'X-Api-Token'.
*    LS_HEADER-VALUE = I_DATA.
*    APPEND LS_HEADER TO R.
*
*    LS_HEADER-NAME  = 'X-Api-Url'.
*    LS_HEADER-VALUE = HEADER_FG( ).
*    APPEND LS_HEADER TO R.
*
*    LS_HEADER-NAME  = 'X-Api-Method'.
*    LS_HEADER-VALUE = 'POST'.
*    APPEND LS_HEADER TO R.


  ENDMETHOD.
  METHOD HEADER_API_TOKEN_SP.
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

*    CONCATENATE 'Bearer'
*                I_DATA
*         INTO LV_TOKEN SEPARATED BY SPACE.
*
*    LS_HEADER-NAME  = 'Authorization'.
*    LS_HEADER-VALUE = LV_TOKEN.
*    APPEND LS_HEADER TO R.

*    LS_HEADER-NAME  = 'X-Api-Token'.
*    LS_HEADER-VALUE = I_DATA.
*    APPEND LS_HEADER TO R.
*
*
*    LS_HEADER-NAME  = 'X-Api-Url'.
*    LS_HEADER-VALUE = HEADER_SP( ).
*    APPEND LS_HEADER TO R.
*
*    LS_HEADER-NAME  = 'X-Api-Method'.
*    LS_HEADER-VALUE = 'POST'.
*    APPEND LS_HEADER TO R.
  ENDMETHOD.
  METHOD BODY_API.
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
  METHOD BODY_API_LEN.
    R = STRLEN( I_DATA ).
  ENDMETHOD.
  METHOD ENDPOINT_WMS.
    CONSTANTS : BEGIN OF LC_WMS,
                  REPID TYPE C LENGTH 21 VALUE 'ENDPOINT_WMS_ORDER_DO',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                END OF LC_WMS.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-DEV
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-QAS
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-PRD
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD ENDPOINT_SONY_TOKEN.
    CONSTANTS : BEGIN OF LC_WMS,
                  REPID TYPE C LENGTH 22 VALUE 'ENDPOINT_SONY_ORDER_DO',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  TOKEN TYPE C LENGTH 5  VALUE 'TOKEN',
                END OF LC_WMS.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-DEV
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-QAS
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-PRD
                                                      I_PARAM_EXT         = LC_WMS-TOKEN
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD ENDPOINT_SONY_FG.
    CONSTANTS : BEGIN OF LC_WMS,
                  REPID TYPE C LENGTH 22 VALUE 'ENDPOINT_SONY_ORDER_DO',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  FG    TYPE C LENGTH 5  VALUE 'FG',
                END OF LC_WMS.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-DEV
                                                      I_PARAM_EXT         = LC_WMS-FG
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-QAS
                                                      I_PARAM_EXT         = LC_WMS-FG
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_WMS-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_WMS-PRD
                                                      I_PARAM_EXT         = LC_WMS-FG
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD ENDPOINT_SONY_SP.
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

ENDCLASS.
