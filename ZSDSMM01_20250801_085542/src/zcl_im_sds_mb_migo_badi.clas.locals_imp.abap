*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.
    CLASS-METHODS :
      SEND_GOODS_ISSUE IMPORTING IT_MSEG TYPE	TY_T_MSEG
                                 IS_MKPF TYPE	MKPF,
      CHECK_SDS        IMPORTING LV_MANDT TYPE SY-MANDT
                       RETURNING VALUE(R) LIKE ABAP_TRUE,
      GET_PATH RETURNING VALUE(R) TYPE STRING,
      GET_FILE_NAEM RETURNING VALUE(R) TYPE STRING,
      GET_AL11_PATH RETURNING VALUE(R) TYPE STRING,
      GET_VENDOR_HOLD RETURNING VALUE(R) TYPE LFA1-LIFNR.

ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD SEND_GOODS_ISSUE.



  ENDMETHOD.
  METHOD CHECK_SDS.

    DATA: BEGIN OF LS_GEN_C,
            REPID        TYPE  ZSDSCAC001-REPID,
            PARAM        TYPE  ZSDSCAC001-PARAM,
            PARAM_EXT    TYPE  ZSDSCAC001-PARAM_EXT,
            SEQUENCE     TYPE  ZSDSCAC001-SEQUENCE,
            PARAM_SIGN   TYPE  ZSDSCAC001-PARAM_SIGN,
            PARAM_OPTION TYPE  ZSDSCAC001-PARAM_OPTION,
            VALUE_LOW    TYPE  ZSDSCAC001-VALUE_LOW,
            VALUE_HIGH   TYPE  ZSDSCAC001-VALUE_HIGH,
            VDESC        TYPE  ZSDSCAC001-VDESC,
          END OF LS_GEN_C .
    DATA: LT_GEN_C  LIKE STANDARD TABLE OF LS_GEN_C .

    DATA : IV_REPID TYPE PROGRAMM,
           LT_PARAM TYPE RANGE OF ZSDSCAC001-PARAM,
           LT_EXT   TYPE RANGE OF ZSDSCAC001-PARAM_EXT.

    DATA : LS_PARAM LIKE LINE OF LT_PARAM,
           LS_EXT   LIKE LINE OF LT_EXT.

    CONSTANTS : BEGIN OF LC_CON,
                  SIGN    TYPE C LENGTH 1  VALUE 'I',
                  OPTION  TYPE C LENGTH 2  VALUE 'EQ',
                  VALUE   TYPE C LENGTH 10 VALUE 'SDS_CLIENT',
                  PROGRAM TYPE C LENGTH 14 VALUE 'SDS_PROGRAM',
                END OF LC_CON.

    CLEAR : LT_GEN_C,LT_PARAM,LS_PARAM.

    IV_REPID = LC_CON-PROGRAM.

    LS_PARAM-SIGN   = LC_CON-SIGN.
    LS_PARAM-OPTION = LC_CON-OPTION.
    LS_PARAM-LOW    = LC_CON-VALUE.
    APPEND LS_PARAM TO LT_PARAM.

    ZCL_SDSCA_UTILITIES=>GET_GEN_C( EXPORTING IF_REPID  = IV_REPID
                                              IRT_PARAM = LT_PARAM
                                              IRT_EXT   = LT_EXT
                                    IMPORTING ET_GEN_C  = LT_GEN_C ).

    IF LT_GEN_C IS NOT INITIAL.
      LOOP AT LT_GEN_C INTO LS_GEN_C WHERE VALUE_LOW EQ SY-MANDT.
        R = ABAP_TRUE.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        R = ABAP_FALSE.
      ENDIF.
    ELSE.
      R = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.
  METHOD GET_PATH.
    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 22 VALUE 'SONY_HOLD_DATA',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  F36   TYPE C LENGTH 3  VALUE 'F36',
                  F46   TYPE C LENGTH 3  VALUE 'F46',
                  F56   TYPE C LENGTH 3  VALUE 'F56',
                  TOKEN TYPE C LENGTH 20 VALUE 'WINDOW_PATH',
                END OF LC_CON.

    CASE SY-SYSID.
      WHEN LC_CON-F36.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-DEV
                                                      I_PARAM_EXT         = LC_CON-TOKEN
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-F46.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-QAS
                                                      I_PARAM_EXT         = LC_CON-TOKEN
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-F56.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-PRD
                                                      I_PARAM_EXT         = LC_CON-TOKEN
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD GET_FILE_NAEM.
    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 22 VALUE 'SONY_HOLD_DATA',
                  PARAM TYPE C LENGTH 22 VALUE 'HOLD_FILE_NAME',
                END OF LC_CON.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = LC_CON-PARAM
                                        CHANGING  C_RETURN            = R ).

  ENDMETHOD.
  METHOD GET_AL11_PATH.
    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 22 VALUE 'SONY_HOLD_DATA',
                  PARAM TYPE C LENGTH 10  VALUE 'AL11_PATH',
                END OF LC_CON.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = LC_CON-PARAM
                                        CHANGING  C_RETURN            = R ).
  ENDMETHOD.
  METHOD GET_VENDOR_HOLD.
    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 22 VALUE 'SONY_HOLD_DATA',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  F36   TYPE C LENGTH 3  VALUE 'F36',
                  F46   TYPE C LENGTH 3  VALUE 'F46',
                  F56   TYPE C LENGTH 3  VALUE 'F56',
                  PARAE TYPE C LENGTH 10 VALUE 'VENDOR',
                END OF LC_CON.

    CASE SY-SYSID.
      WHEN LC_CON-F36.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-DEV
                                                      I_PARAM_EXT         = LC_CON-PARAE
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-F46.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-QAS
                                                      I_PARAM_EXT         = LC_CON-PARAE
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-F56.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-PRD
                                                      I_PARAM_EXT         = LC_CON-PARAE
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
