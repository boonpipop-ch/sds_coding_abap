*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF LY_DOC_SV,
              INVDC TYPE VBAK-VBELN,
              SODOC TYPE VBAP-VGBEL,
            END OF LY_DOC_SV.
    TYPES : LTY_DOC_SV TYPE TABLE OF LY_DOC_SV WITH EMPTY KEY.

    TYPES : BEGIN OF LY_DOC,
              "BELNR TYPE BSEG-BELNR,
              VBELN TYPE BSEG-VBELN,
            END OF LY_DOC.
    TYPES : LTY_DOC TYPE HASHED TABLE OF LY_DOC WITH UNIQUE KEY VBELN.

    METHODS :
      CONSTRUCTOR.
    CLASS-METHODS :
      CHECK_SDS   IMPORTING LV_MANDT TYPE SY-MANDT
                  RETURNING VALUE(R) LIKE ABAP_TRUE,
      GET_MAT IMPORTING I_DATA   TYPE BSEG_T
              RETURNING VALUE(R) TYPE ZCL_SDSFI_ENHANCEMENT=>GTY_MAT,
      UPDATE_BSEG IMPORTING IT_DATA TYPE ZCL_SDSFI_ENHANCEMENT=>GTY_MAT
                  CHANGING  CT_DATA TYPE BSEG_T
                            CT_BSEG TYPE BSEG_SUBST_T,
      GET_SO_SV_CON IMPORTING I_DATA   TYPE LTY_DOC
                    RETURNING VALUE(R) TYPE LTY_DOC_SV.

    CONSTANTS : BEGIN OF LC_CON,
                  E TYPE C LENGTH 1 VALUE 'E',
                  S TYPE C LENGTH 1 VALUE 'S',
                END OF LC_CON.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

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
  METHOD GET_MAT.
    SELECT MATNR,
           MAKTX
      FROM MAKT
      INTO TABLE @R
      FOR ALL ENTRIES IN @I_DATA
      WHERE MATNR EQ @I_DATA-MATNR
        AND SPRAS EQ @SY-LANGU.
  ENDMETHOD.
  METHOD UPDATE_BSEG.
    DATA : LV_TABIX TYPE SY-TABIX.

    DATA : LT_DOC TYPE LTY_DOC.

    DATA : LT_SO_SV_CONT TYPE LTY_DOC_SV.

    DATA : LV_SGTXT TYPE BSEG-SGTXT.

    DATA : LV_MEMMORY TYPE C LENGTH 50.

    FIELD-SYMBOLS : <FS_TEXT> TYPE ANY.

    LT_DOC =  CORRESPONDING #( CT_DATA  DISCARDING DUPLICATES ).
    SORT LT_DOC.
    DELETE LT_DOC WHERE VBELN EQ SPACE.

    LT_SO_SV_CONT = GET_SO_SV_CON( LT_DOC ).

    CLEAR : LV_SGTXT.
    IF SY-TCODE EQ 'FB05'.
      CONCATENATE 'ZSDSFIR0810' SY-UNAME INTO LV_MEMMORY.
      IMPORT LV_SGTXT TO LV_SGTXT FROM MEMORY ID LV_MEMMORY.
      FREE MEMORY ID LV_MEMMORY.
    ENDIF.

    LOOP AT CT_DATA ASSIGNING FIELD-SYMBOL(<LFS_DATA>)." WHERE MATNR IS NOT INITIAL.
      LV_TABIX = SY-TABIX.
*--------------------------------------------------------------------*
* Update Material Description
*--------------------------------------------------------------------*
      READ TABLE IT_DATA INTO DATA(LS_DATA)
      WITH KEY MATNR = <LFS_DATA>-MATNR.
      IF SY-SUBRC EQ 0.
        READ TABLE CT_BSEG ASSIGNING FIELD-SYMBOL(<LFS_BSEG>)
        WITH KEY TABIX = LV_TABIX.
        IF SY-SUBRC EQ 0.
          <LFS_BSEG>-SGTXT = LS_DATA-MAKTX.
        ENDIF.
      ENDIF.
*--------------------------------------------------------------------*
* End Update Material Description
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
* Update Assignment
*--------------------------------------------------------------------*
      READ TABLE LT_SO_SV_CONT INTO DATA(LS_SO_SV_CONT)
      WITH KEY INVDC = <LFS_DATA>-VBELN.
      IF SY-SUBRC EQ 0.
        READ TABLE CT_BSEG ASSIGNING FIELD-SYMBOL(<LFS_BSEG_SO>)
        WITH KEY TABIX = LV_TABIX.
        IF SY-SUBRC EQ 0.
          <LFS_BSEG_SO>-ZUONR = LS_SO_SV_CONT-SODOC.
        ENDIF.
      ENDIF.
*--------------------------------------------------------------------*
* End Update Assignment
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
* UPDATE TEXT PROGRAM ZSDSFI077
*--------------------------------------------------------------------*
      IF LV_SGTXT IS NOT INITIAL AND
         SY-TCODE EQ 'FB05'.
        READ TABLE CT_BSEG ASSIGNING FIELD-SYMBOL(<LFS_BSEG_ITEMS>)
        WITH KEY TABIX = LV_TABIX.
        IF SY-SUBRC EQ 0.
          IF <LFS_BSEG_ITEMS>-SGTXT IS INITIAL.
            <LFS_BSEG_ITEMS>-SGTXT = LV_SGTXT.
          ENDIF.
        ENDIF.
      ENDIF.
*--------------------------------------------------------------------*
* END UPDATE TEXT PROGRAM ZSDSFI077
*--------------------------------------------------------------------*
    ENDLOOP.
  ENDMETHOD.
  METHOD GET_SO_SV_CON.
    CONSTANTS : LC_ZVB3 TYPE C LENGTH 4 VALUE 'ZVB3',
                LC_CSCT TYPE C LENGTH 4 VALUE 'CSCT'.

    DATA : LS_R LIKE LINE OF R.

    FIELD-SYMBOLS : <FS>        TYPE VBRK,
                    <LFS_XVBRP> TYPE TAB_VBRPVB.

    DATA : LS_XVBRP LIKE LINE OF <LFS_XVBRP>.

    ASSIGN ('(SAPLV60B)VBRK') TO <FS>.
    IF <FS> IS ASSIGNED.
      IF <FS>-FKART EQ LC_ZVB3.
        ASSIGN ('(SAPLV60B)XVBRP[]') TO <LFS_XVBRP>.
        IF <LFS_XVBRP> IS ASSIGNED.
          LOOP AT I_DATA INTO DATA(LS_DATA).
            LS_R-INVDC = LS_DATA.
            READ TABLE <LFS_XVBRP> INTO LS_XVBRP
            WITH KEY VGTYP = LC_CSCT.
            IF SY-SUBRC EQ 0.
              LS_R-SODOC = LS_XVBRP-VGBEL.
            ELSE.
              LS_R-SODOC = LS_R-INVDC.
            ENDIF.
            APPEND LS_R TO R.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
