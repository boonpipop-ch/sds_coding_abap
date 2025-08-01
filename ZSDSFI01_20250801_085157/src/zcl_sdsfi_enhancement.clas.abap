class ZCL_SDSFI_ENHANCEMENT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF GY_MAT,
             MATNR TYPE MAKT-MATNR,
             MAKTX TYPE MAKT-MAKTX,
           END OF GY_MAT .
  types:
    GTY_MAT TYPE STANDARD TABLE OF GY_MAT WITH EMPTY KEY .

  class-data GV_ITEM_TEXT type BSEG-SGTXT .

  methods BTE1120
    importing
      value(I_BKDF) type BKDF optional
    changing
      value(I_BKDFSUB) type BKDF_SUBST optional
      value(T_BKPF) type BKPF_T optional
      value(T_BSEG) type BSEG_T optional
      value(T_BKPFSUB) type BKPF_SUBST_T optional
      value(T_BSEGSUB) type BSEG_SUBST_T optional
      value(T_BSEC) type BSEC_T optional .
  methods CHECK_SDS
    returning
      value(R_RETURN) type CHAR1 .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_SDSFI_ENHANCEMENT IMPLEMENTATION.


  METHOD BTE1120.
    IF T_BSEG IS NOT INITIAL.
      DATA(LT_MAT) = LCL_DATA=>GET_MAT( T_BSEG ).
      LCL_DATA=>UPDATE_BSEG( EXPORTING IT_DATA = LT_MAT
                              CHANGING CT_DATA = T_BSEG
                                       CT_BSEG = T_BSEGSUB ).
    ENDIF.
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
        R_RETURN = ABAP_TRUE.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        R_RETURN = ABAP_FALSE.
      ENDIF.
    ELSE.
      R_RETURN = ABAP_FALSE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
