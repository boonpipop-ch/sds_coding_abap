class ZCL_SDSCM_ENH_GI_SVO_CONF definition
  public
  final
  create public .

public section.

  types:
    TT_BUKRS_RANGE TYPE RANGE OF T001-BUKRS .
  types:
    TT_WERKS_RANGE TYPE RANGE OF T001W-WERKS .

  class-methods UPDATE_GOODSMVT_HEAD
    importing
      !IF_WERKS type T001W-WERKS
      !IF_HEADER_GUID type CRMT_OBJECT_GUID
    changing
      !CS_GM_HEAD type BAPI2017_GM_HEAD_01 .
  class-methods IS_SDS
    importing
      !IF_BUKRS type T001-BUKRS optional
      !IF_WERKS type T001W-WERKS optional
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
  class-methods UPDATE_GOODSMVT_ITEM
    importing
      !IF_BUKRS type T001-BUKRS
      !IS_MATERIAL_GM type CRMT_SERVICECONFMAT_CO
    changing
      !CS_GOODSMVT_ITEM type BAPI2017_GM_ITEM_CREATE .
protected section.
private section.

  class-data GR_ACTIVE_BUKRS type TT_BUKRS_RANGE .
  class-data GR_ACTIVE_WERKS type TT_WERKS_RANGE .

  class-methods GET_CONSTANTS .
ENDCLASS.



CLASS ZCL_SDSCM_ENH_GI_SVO_CONF IMPLEMENTATION.


METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_WBS_SUBSTITUTION / GET_CONSTANTS
*  Creation Date      : 26.07.2024
*  Author             : Wuthichai L. (Eviden)
*  Add-on ID          : PSE004
*  Description        : To get GENC constant from table ZSDSCAC001
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  CONSTANTS:
    LC_ACTIVE_BUKRS TYPE ZSDSDE_PARAM_NAME VALUE 'ACTIVATED_BUKRS',
    LC_ACTIVE_WERKS TYPE ZSDSDE_PARAM_NAME VALUE 'ACTIVATED_WERKS'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSCM_ENH_GI_SVO_CONF'.


* Check Already Read?
  IF LF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GR_ACTIVE_BUKRS,
         GR_ACTIVE_WERKS.

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
*     Active Plant
*     ------------------------------------
      WHEN LC_ACTIVE_WERKS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GR_ACTIVE_WERKS.

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

  IF IF_WERKS IS SUPPLIED AND
     GR_ACTIVE_WERKS IS NOT INITIAL AND
     IF_WERKS IN  GR_ACTIVE_WERKS.
    RF_RESULT = 'X'.
  ENDIF.

ENDMETHOD.


METHOD UPDATE_GOODSMVT_HEAD.

  CONSTANTS:
    LC_SVO  TYPE  SWO_OBJTYP VALUE 'BUS2000116'.

  DATA:
    LT_DOC_FLOW  TYPE  CRMT_DOC_FLOW_WRKT.


* Check Only SDS
  IF IS_SDS( IF_WERKS = IF_WERKS ) NE 'X'.
    RETURN.
  ENDIF.

* -----------------------------------------
* Get Service Order number from Doc Flow
* -----------------------------------------
  CALL FUNCTION 'CRM_DOC_FLOW_READ_OB'
    EXPORTING
      IV_HEADER_GUID  = IF_HEADER_GUID
    IMPORTING
      ET_DOC_FLOW_WRK = LT_DOC_FLOW.

  READ TABLE LT_DOC_FLOW ASSIGNING FIELD-SYMBOL(<L_DOC_FLOW>)
                         WITH KEY OBJTYPE_A = LC_SVO.
  IF SY-SUBRC EQ 0.
*   Get SVO Number
    SELECT SINGLE OBJECT_ID
      FROM CRMS4D_BTX_H
     WHERE GUID EQ @<L_DOC_FLOW>-OBJKEY_A
      INTO @DATA(LF_OBJECT_ID).
    IF SY-SUBRC NE 0.
      CLEAR LF_OBJECT_ID.
    ENDIF.

*   Assign Header Text = SVO number
    CS_GM_HEAD-HEADER_TXT = LF_OBJECT_ID.

  ENDIF.

ENDMETHOD.


METHOD UPDATE_GOODSMVT_ITEM.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSCM_ENH_GI_SVO_CONF=>UPDATE_GOODSMVT_ITEM
*  Creation Date      : 06.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : CME019
*  Description        : Update GI item data for SVO confirmation
*  Purpose            : Update GI item data for SVO confirmation
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA:
    LT_REFOBJ          TYPE  CRMT_REFOBJ_WRKT.

  DATA:
    LS_PRODUCT_I  TYPE  CRMT_PRODUCT_I_WRK,
    LS_CUSTOMER_I TYPE  CRMT_CUSTOMER_I_WRK.

  DATA:
    LF_GUID         TYPE  CRMT_OBJECT_GUID,
    LF_EQUIPMENT_ID TYPE  CRMT_ORDPRP_OBJL_I_D_WRK-EQUIPMENT_ID.


* Check Only SDS
  IF IS_SDS( IF_BUKRS = IF_BUKRS ) NE 'X'.
    RETURN.
  ENDIF.

* Service Confirmation Item GUID
  LF_GUID = IS_MATERIAL_GM-CURRENT_OBJECT_ID.

* Get Equipment
  CLEAR LF_EQUIPMENT_ID.
  CALL FUNCTION 'CRM_REFOBJ_READ_OW'
    EXPORTING
      IV_REF_GUID    = LF_GUID
      IV_REF_KIND    = 'B' "Item
*     IV_GUID_SET    =
*     IV_PROFILE_TYPE         =
    IMPORTING
      ET_REFOBJ_WRK  = LT_REFOBJ
*     ET_REFOBJ_WRK_OLD       =
    EXCEPTIONS
      ERROR_OCCURRED = 1
      OTHERS         = 2.
  IF SY-SUBRC EQ 0.
    LOOP AT LT_REFOBJ ASSIGNING FIELD-SYMBOL(<L_REFOBJ>)
                      WHERE EQUIPMENT_ID IS NOT INITIAL.
      LF_EQUIPMENT_ID = <L_REFOBJ>-EQUIPMENT_ID.
      EXIT.
    ENDLOOP.
  ENDIF.

* Get Plant
  CALL FUNCTION 'CRM_PRODUCT_I_READ_OW'
    EXPORTING
      IV_GUID                   = LF_GUID
    IMPORTING
      ES_PRODUCT_I              = LS_PRODUCT_I
    EXCEPTIONS
      PRODUCT_I_NOT_IN_WORKAREA = 1
      OTHERS                    = 2.
  IF SY-SUBRC <> 0.
    CLEAR LS_PRODUCT_I.
  ENDIF.

* Get SLoc
  CALL FUNCTION 'CRM_CUSTOMER_I_READ_OW'
    EXPORTING
      IV_GUID           = LF_GUID
    IMPORTING
      ES_CUSTOMER_I_WRK = LS_CUSTOMER_I
    EXCEPTIONS
      ITEM_NOT_FOUND    = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0.
    CLEAR LS_CUSTOMER_I.
  ENDIF.

* Update Ref Equipment no into item text
  WRITE LF_EQUIPMENT_ID TO CS_GOODSMVT_ITEM-ITEM_TEXT LEFT-JUSTIFIED.
  CONDENSE CS_GOODSMVT_ITEM-ITEM_TEXT.

* Update Plant from SVO Confirm header
  IF LS_PRODUCT_I-PLANT IS NOT INITIAL.
    CS_GOODSMVT_ITEM-PLANT = LS_PRODUCT_I-PLANT.
  ENDIF.

* Update Sloc from SVO Confirm header
  IF LS_CUSTOMER_I-ZZ1_LGORT IS NOT INITIAL.
    CS_GOODSMVT_ITEM-STGE_LOC = LS_CUSTOMER_I-ZZ1_LGORT.
  ENDIF.

ENDMETHOD.
ENDCLASS.
