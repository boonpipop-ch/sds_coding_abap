class ZCL_SDSFI_ENHANCEMENT2 definition
  public
  final
  create public .

public section.

  types:
    TT_BUKRS_RANGE TYPE RANGE OF T001-BUKRS .
  types:
    TT_VKORG_RANGE TYPE RANGE OF TVKO-VKORG .

  class-methods IS_SDS
    importing
      !IF_BUKRS type T001-BUKRS optional
      !IF_VKORG type TVKO-VKORG optional
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
  class-methods EXIT_SAPLV60B_010
    importing
      !IS_XVBRP type VBRPVB
      !IS_XACCIT_DEB type ACCIT_DEB
      !IS_XKOMV type KOMV
    changing
      !CS_XACCIT_DEB type ACCIT_DEB .
protected section.
private section.

  class-data GT_BUKRS type TT_BUKRS_RANGE .
  class-data GT_VKORG type TT_VKORG_RANGE .

  class-methods GET_GENC .
ENDCLASS.



CLASS ZCL_SDSFI_ENHANCEMENT2 IMPLEMENTATION.


METHOD EXIT_SAPLV60B_010.

  CONSTANTS:
    LC_WT_FNAME  TYPE  CHAR30 VALUE '(SAPLV60B)XACCIT_WT[]'.

  FIELD-SYMBOLS:
    <L_XACCIT_WT_TAB>  TYPE  ACCIT_WT_T.


* Check Only WT_KEY exist (WTH Related)
  IF CS_XACCIT_DEB-WT_KEY IS INITIAL.
    RETURN.
  ENDIF.

* Get WT Data
  ASSIGN (LC_WT_FNAME) TO <L_XACCIT_WT_TAB>.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Read WTH Key
  READ TABLE <L_XACCIT_WT_TAB> ASSIGNING FIELD-SYMBOL(<L_XACCIT_WT>)
                               WITH KEY WT_KEY = CS_XACCIT_DEB-WT_KEY.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign WTH Base Amount
  <L_XACCIT_WT>-WT_QSSHB = <L_XACCIT_WT>-WT_QSSHB + IS_XVBRP-NETWR.

* Clear WTH Key to combine AR Lines
  CLEAR CS_XACCIT_DEB-WT_KEY.

ENDMETHOD.


METHOD GET_GENC.

  CONSTANTS:
    LC_BUKRS TYPE  ZSDSDE_PARAM_NAME VALUE 'COMPANY_IN_SCOPE',
    LC_VKORG TYPE  ZSDSDE_PARAM_NAME VALUE 'SALES_ORG_IN_SCOPE'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSFI_ENHANCEMENT2'.


* Check Already Read?
  IF LF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_BUKRS,
         GT_VKORG.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read
  LF_READ = 'X'.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Company Code in enhancement scope
*     ------------------------------------
      WHEN LC_BUKRS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_BUKRS.

*     ------------------------------------
*     Sales Org in enhancement scope
*     ------------------------------------
      WHEN LC_VKORG.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_VKORG.

    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD IS_SDS.

  DATA:
    LF_ORG_SPECIFIED TYPE  FLAG.


  CLEAR LF_ORG_SPECIFIED.

* Get Constants
  GET_GENC( ).

  IF IF_BUKRS IS SUPPLIED.
    LF_ORG_SPECIFIED = 'X'.
    IF GT_BUKRS IS NOT INITIAL AND
       IF_BUKRS IN GT_BUKRS.
      RF_RESULT = ABAP_TRUE.
    ELSE.
      RF_RESULT = ABAP_FALSE.
      RETURN.
    ENDIF.
  ENDIF.

  IF IF_VKORG IS SUPPLIED.
    LF_ORG_SPECIFIED = 'X'.
    IF GT_VKORG IS NOT INITIAL AND
       IF_VKORG IN GT_VKORG.
      RF_RESULT = ABAP_TRUE.
    ELSE.
      RF_RESULT = ABAP_FALSE.
      RETURN.
    ENDIF.
  ENDIF.

  IF LF_ORG_SPECIFIED IS INITIAL.
    RF_RESULT = ABAP_FALSE.
  ENDIF.

ENDMETHOD.
ENDCLASS.
