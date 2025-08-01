class ZCL_SDSFI_UPDATE_TAX_INV definition
  public
  final
  create public .

public section.

  types:
    TT_BUKRS_RANGE TYPE RANGE OF T001-BUKRS .
  types:
    TT_MWSKZ_RANGE TYPE RANGE OF BSEG-MWSKZ .

  class-methods UPDATE_TAX_INV_NUMBER
    importing
      !IT_BSEG type BSEG_T
    changing
      !CT_BKPF type BKPF_T
      !CF_XBLNR type FIN1_PARAM-XBLNR .
protected section.
private section.

  class-data GR_ACTIVE_BUKRS type TT_BUKRS_RANGE .
  class-data GR_ACTIVE_MWSKZ type TT_MWSKZ_RANGE .

  class-methods GET_CONSTANTS .
  class-methods IS_SDS
    importing
      !IF_BUKRS type T001-BUKRS optional
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
ENDCLASS.



CLASS ZCL_SDSFI_UPDATE_TAX_INV IMPLEMENTATION.


METHOD GET_CONSTANTS.
  CONSTANTS:
    LC_ACTIVE_BUKRS TYPE ZSDSDE_PARAM_NAME VALUE 'ACTIVATED_BUKRS',
    LC_ACTIVE_MWSKZ TYPE ZSDSDE_PARAM_NAME VALUE 'ACTIVATED_MWSKZ'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSFI_UPDATE_TAX_INV'.


* Check Already Read?
  IF LF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GR_ACTIVE_BUKRS,
         GR_ACTIVE_MWSKZ.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = 'X'.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Active Company Code
*     ------------------------------------
      WHEN LC_ACTIVE_BUKRS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_ACTIVE_BUKRS.

*     ------------------------------------
*     Active TAX Code
*     ------------------------------------
      WHEN LC_ACTIVE_MWSKZ.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_ACTIVE_MWSKZ.

    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD IS_SDS.

* Initialize Output
  CLEAR: RF_RESULT.

* Get Constants setting
  GET_CONSTANTS( ).

  IF IF_BUKRS IS SUPPLIED AND
     GR_ACTIVE_BUKRS IS NOT INITIAL AND
     IF_BUKRS IN  GR_ACTIVE_BUKRS.
    RF_RESULT = 'X'.
  ENDIF.

ENDMETHOD.


METHOD UPDATE_TAX_INV_NUMBER.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSFI_UPDATE_TAX_INV
*  Creation Date      : 02.10.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : FIARE041
*  Description        : This is class to change Tax invoice number in
*                       FI document to be FI document number
*  Purpose            : To change Tax invoice number
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  LOOP AT CT_BKPF ASSIGNING FIELD-SYMBOL(<L_BKPF>).

*   Only Activated Company code
    IF IS_SDS( IF_BUKRS = <L_BKPF>-BUKRS ) NE 'X'.
      CONTINUE.
    ENDIF.

    IF GR_ACTIVE_MWSKZ IS INITIAL.
      CONTINUE.
    ENDIF.

*   Check Tax Code Exist
    LOOP AT IT_BSEG TRANSPORTING NO FIELDS
                    WHERE BUKRS EQ <L_BKPF>-BUKRS
                      AND BELNR EQ <L_BKPF>-BELNR
                      AND GJAHR EQ <L_BKPF>-GJAHR
                      AND MWSKZ IN GR_ACTIVE_MWSKZ.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

*   Update Tax Invoice = Document number
    <L_BKPF>-XBLNR = <L_BKPF>-BELNR.
    CF_XBLNR       = <L_BKPF>-BELNR.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.
