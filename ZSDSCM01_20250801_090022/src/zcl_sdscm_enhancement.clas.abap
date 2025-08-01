class ZCL_SDSCM_ENHANCEMENT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TS_DETER_PRCTR,
        SALES_ORG_SD    TYPE CRMS4D_SERV_H-SALES_ORG_SD,
        PROCESS_TYPE    TYPE CRMS4D_SERV_H-PROCESS_TYPE,
        SALES_OFFICE_SD TYPE CRMS4D_SERV_H-SALES_OFFICE_SD,
        SALES_GROUP_SD  TYPE CRMS4D_SERV_H-SALES_GROUP_SD,
      END OF TS_DETER_PRCTR .
  types:
    BEGIN OF TS_DETER_ABGSL,
        SALES_ORG_SD TYPE CRMS4D_SERV_H-SALES_ORG_SD,
        PROCESS_TYPE TYPE CRMS4D_SERV_H-PROCESS_TYPE,
        ITM_TYPE     TYPE CRMS4D_SERV_I-ITM_TYPE,
      END OF TS_DETER_ABGSL .
  types:
    BEGIN OF TS_DETER_SAKTO,
        SALES_ORG_SD TYPE CRMS4D_SERV_H-SALES_ORG_SD,
        PROCESS_TYPE TYPE CRMS4D_SERV_H-PROCESS_TYPE,
        ITM_TYPE     TYPE CRMS4D_SERV_I-ITM_TYPE,
        BKLAS        TYPE MBEW-BKLAS,
      END OF TS_DETER_SAKTO .
  types:
    TT_KOKRS_RANGE TYPE RANGE OF COBL-KOKRS .
  types:
    TT_VKORG_RANGE TYPE RANGE OF TVKO-VKORG .
  types:
    TT_WERKS_RANGE TYPE RANGE OF T001W-WERKS .

  constants GC_ZSDS_SRV type MEMORYID value 'ZSDS_SRV' ##NO_TEXT.

  class-methods DETERMINE_GL_ACCOUNT
    importing
      !IS_COND type TS_DETER_SAKTO
    returning
      value(RF_SAKTO) type EBKN-SAKTO .
  class-methods DETERMINE_RA_KEY
    importing
      !IS_COND type TS_DETER_ABGSL
    returning
      value(RF_ABGSL) type COAS-ABGSL .
  class-methods DETERMINE_PROFIT_CENTER
    importing
      !IS_COND type TS_DETER_PRCTR
    returning
      value(RF_PRCTR) type COBL-PRCTR .
  class-methods DLC_CONFIG_CHOOSE_CONFIG
    importing
      !IV_COMPONENT type BSP_WD_COMPONENT_NAME
      !IV_VIEWNAME type O2PAGEEXT
      !IS_CONFIG_SEARCH_KEY type BSP_DLCS_CONF_SEM_KEY_VAR_PART
      !IT_CONFIG_SAP type BSP_DLCT_CONTEXT_SEMANTIC_KEY
      !IT_CONFIG_CUS type BSP_DLCT_CONTEXT_SEMANTIC_KEY
    exporting
      !ES_CONFIG_CHOOSEN type BSP_DLCS_CONTEXT_SEMANTIC_KEY
      !EV_CONFIG_CHOOSEN_ORIGIN type BSP_DLC_CONFIG_DATA_ORIGIN .
  class-methods GET_V_ZZ1_PARTNER_SEGMENT
    importing
      !IV_MODE type CHAR1 default IF_BSP_WD_MODEL_SETTER_GETTER=>RUNTIME_MODE
      !IV_INDEX type I optional
    returning
      value(RV_VALUEHELP_DESCRIPTOR) type ref to IF_BSP_WD_VALUEHELP_DESCRIPTOR .
  class-methods GET_V_ZZ1_LOB_SRH
    importing
      !IV_MODE type CHAR1 default IF_BSP_WD_MODEL_SETTER_GETTER=>RUNTIME_MODE
      !IV_INDEX type I optional
    returning
      value(RV_VALUEHELP_DESCRIPTOR) type ref to IF_BSP_WD_VALUEHELP_DESCRIPTOR .
  class-methods GET_P_ZZ1_PARTNER_SEGMENT
    importing
      !IV_PROPERTY type STRING
      !IV_INDEX type I optional
      !IV_DISPLAY_MODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_VALUE) type STRING .
  class-methods GET_P_ZZ1_LOB_SRH
    importing
      !IV_PROPERTY type STRING
      !IV_INDEX type I optional
      !IV_DISPLAY_MODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_VALUE) type STRING .
  class-methods GET_V_ZZ1_PROACTIVITY
    importing
      !IV_MODE type CHAR1 default IF_BSP_WD_MODEL_SETTER_GETTER=>RUNTIME_MODE
      !IV_INDEX type I optional
    returning
      value(RV_VALUEHELP_DESCRIPTOR) type ref to IF_BSP_WD_VALUEHELP_DESCRIPTOR .
  class-methods GET_P_ZZ1_PROACTIVITY
    importing
      !IV_PROPERTY type STRING
      !IV_INDEX type I optional
      !IV_DISPLAY_MODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_VALUE) type STRING .
  class-methods GET_V_ZZ1_BILL_METHOD
    importing
      !IV_MODE type CHAR1 default IF_BSP_WD_MODEL_SETTER_GETTER=>RUNTIME_MODE
      !IV_INDEX type I optional
    returning
      value(RV_VALUEHELP_DESCRIPTOR) type ref to IF_BSP_WD_VALUEHELP_DESCRIPTOR .
  class-methods GET_P_ZZ1_BILL_METHOD
    importing
      !IV_PROPERTY type STRING
      !IV_INDEX type I optional
      !IV_DISPLAY_MODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_VALUE) type STRING .
  class-methods IS_SDS
    importing
      !IF_KOKRS type TKA01-KOKRS optional
      !IF_VKORG type TVKO-VKORG optional
      !IF_WERKS type T001W-WERKS optional
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
  class-methods GET_I_ZZ1_LGORT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR
    returning
      value(RV_DISABLED) type STRING .
  class-methods GET_V_ZZ1_LGORT
    importing
      !IV_MODE type CHAR1 default IF_BSP_WD_MODEL_SETTER_GETTER=>RUNTIME_MODE
      !IV_INDEX type I
    returning
      value(RV_VALUEHELP_DESCRIPTOR) type ref to IF_BSP_WD_VALUEHELP_DESCRIPTOR .
  class-methods EXIT_RKECRM_PCA_40_001
    importing
      !IF_KOKRS type COBL-KOKRS
      !IT_CHARTAB type IAOM_OBJECT_ATTRIBUTES
    changing
      !CF_PRCTR type COBL-PRCTR
      !CF_CALL_SUBSTITUTION type C .
  class-methods CHANGE_IO_COSTING
    changing
      !CS_COAS type COAS .
  class-methods MB_MIGO_ITEM_BADI_ITEM_MODIFY
    importing
      !IS_GOITEM type GOITEM
      !IS_GOHEAD type GOHEAD
    changing
      !CF_STGE_LOC type GOITEM-LGORT
      !CF_ITEM_TEXT type GOITEM-SGTXT
      !CT_RETURN type TY_T_BAPIRET2 .
  class-methods UPDATE_DATA_IN_PR
    importing
      !IS_SRVORDER_DATA type CRMS4S_DATAEXCH_BTX
      !IT_PROCESS_INFO type CRMT_SRV_LOG_PROCESS_INFO_T
      !IT_ITEM type TY_BAPIMEREQITEMIMP
    changing
      !CT_ACC type TY_BAPIMEREQACCOUNT
      !CT_ACC_X type TY_BAPIMEREQACCOUNTX .
  class-methods IS_ITEM_PROCESSING
    importing
      !IF_PROCESS_TYPE type CRMT_PROCESS_TYPE_DB
      !IF_DATUM type SY-DATUM default SY-DATUM
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
protected section.
private section.

  class-data GT_KOKRS type TT_KOKRS_RANGE .
  class-data GT_VKORG type TT_VKORG_RANGE .
  class-data GT_WERKS type TT_WERKS_RANGE .

  class-methods GET_GENC .
ENDCLASS.



CLASS ZCL_SDSCM_ENHANCEMENT IMPLEMENTATION.


METHOD DETERMINE_PROFIT_CENTER.

* Initialize Output
  CLEAR: RF_PRCTR.

* Read Configuration
  SELECT SINGLE PRCTR
    FROM ZSDSCMC001
   WHERE SALES_ORG_SD    EQ @IS_COND-SALES_ORG_SD
     AND PROCESS_TYPE    EQ @IS_COND-PROCESS_TYPE
     AND SALES_OFFICE_SD EQ @IS_COND-SALES_OFFICE_SD
     AND SALES_GROUP_SD  EQ @IS_COND-SALES_GROUP_SD
     AND ZDEL_FLG EQ @SPACE
    INTO @RF_PRCTR.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.

* Read with Process Type wildcard
  SELECT PROCESS_TYPE,
         PRCTR
    FROM ZSDSCMC001
   WHERE SALES_ORG_SD    EQ  @IS_COND-SALES_ORG_SD
     AND PROCESS_TYPE   LIKE '%*%'
     AND SALES_OFFICE_SD EQ  @IS_COND-SALES_OFFICE_SD
     AND SALES_GROUP_SD  EQ  @IS_COND-SALES_GROUP_SD
     AND ZDEL_FLG EQ @SPACE
   ORDER BY PROCESS_TYPE ASCENDING,
            PRCTR ASCENDING
    INTO TABLE @DATA(LT_DATA).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Check Document type which can wildcard
  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    CHECK IS_COND-PROCESS_TYPE CP <L_DATA>-PROCESS_TYPE.
    RF_PRCTR = <L_DATA>-PRCTR.
    EXIT.
  ENDLOOP.

ENDMETHOD.


METHOD DLC_CONFIG_CHOOSE_CONFIG.

  DATA:
    LF_ROLEKEY  TYPE  BSP_DLC_ROLE_KEY.


* Initialize Output
  CLEAR: ES_CONFIG_CHOOSEN,
         EV_CONFIG_CHOOSEN_ORIGIN.

  IF IT_CONFIG_CUS IS INITIAL.
    RETURN.
  ENDIF.

* Get Rolekey ZSDS_SRV activated?
  IF IS_SDS( ).

    LF_ROLEKEY = 'ZSDS_SRV'.

*   Read Configuration of Role Key ZSDS_SRV
    READ TABLE IT_CONFIG_CUS ASSIGNING FIELD-SYMBOL(<L_CONFIG>)
                             WITH KEY ROLE_KEY = LF_ROLEKEY
                                      OBJECT_TYPE = IS_CONFIG_SEARCH_KEY-OBJECT_TYPE.
    IF SY-SUBRC NE 0.
      READ TABLE IT_CONFIG_CUS ASSIGNING <L_CONFIG>
                               WITH KEY ROLE_KEY = LF_ROLEKEY
                                        OBJECT_TYPE = '<DEFAULT>'.
    ENDIF.
    IF SY-SUBRC EQ 0.
*     Assign The configuration found
      EV_CONFIG_CHOOSEN_ORIGIN = 'C'.
      ES_CONFIG_CHOOSEN = <L_CONFIG>.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD EXIT_RKECRM_PCA_40_001.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSCM_ENHANCEMENT->EXIT_RKECRM_PCA_40_001
*  Creation Date      : 21.06.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : CME013
*  Description        : This is an processing method for EXIT_RKECRM_PCA_40_001
*  Purpose            : Implement profit center determination for internal
*                       order created from service order and service contract
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
    LT_GUID TYPE CRMT_OBJECT_GUID_TAB.

  DATA:
    LS_ORGMAN_WRK TYPE  CRMT_ORGMAN_WRK,
    LS_COND       TYPE  TS_DETER_PRCTR.

  DATA:
    LF_FNAME_PRTYP  TYPE ROLLNAME VALUE 'CRMT_PROCESS_TYPE_CO',
    LF_FNAME_OBJID  TYPE ROLLNAME VALUE 'CRMT_OBJECT_ID_CO',
    LF_PROCESS_TYPE TYPE CRMT_PROCESS_TYPE,
    LF_OBJTYPE_H    TYPE CRMT_SUBOBJECT_CATEGORY_DB,
    LF_OBJECT_ID    TYPE CRMT_OBJECT_ID_DB.


* Get Constants
  GET_GENC( ).

* Check Only activated Controlling area
  IF GT_KOKRS IS INITIAL OR
     NOT IF_KOKRS IN GT_KOKRS.
    RETURN.
  ENDIF.

* Get Business Transaction Type
  READ TABLE IT_CHARTAB ASSIGNING FIELD-SYMBOL(<L_CHARTAB>)
                        WITH KEY DATA_ELEMENT = LF_FNAME_PRTYP.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.
  LF_PROCESS_TYPE = <L_CHARTAB>-VALUE.

* Get Business Object type
  SELECT SINGLE OBJECT_TYPE
    FROM CRMC_PROC_TYPE
   WHERE PROCESS_TYPE EQ @LF_PROCESS_TYPE
    INTO @LF_OBJTYPE_H.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Get Business Object ID
  READ TABLE IT_CHARTAB ASSIGNING <L_CHARTAB>
                        WITH KEY DATA_ELEMENT = LF_FNAME_OBJID.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.
  LF_OBJECT_ID = <L_CHARTAB>-VALUE.

* Get Object GUID
  CALL FUNCTION 'CRM_ORDERADM_H_GUID_GET_OB'
    EXPORTING
      IV_OBJECT_ID       = LF_OBJECT_ID
      IV_OBJECT_TYPE     = LF_OBJTYPE_H
    IMPORTING
      ET_ORDERADM_H_GUID = LT_GUID
    EXCEPTIONS
      RECORD_NOT_FOUND   = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  READ TABLE LT_GUID ASSIGNING FIELD-SYMBOL(<L_GUID>)
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Read ORGMAN Object
  CALL FUNCTION 'CRM_ORGMAN_READ_OB'
    EXPORTING
*      IV_GUID              = <L_GUID>
      IV_REF_GUID          = <L_GUID>
      IV_REF_KIND          = 'A'
    IMPORTING
      ES_ORGMAN_WRK        = LS_ORGMAN_WRK
    EXCEPTIONS
      ENTRY_DOES_NOT_EXIST = 1
      PARAMETER_ERROR      = 2
      OTHERS               = 3.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Assign Condition
  CLEAR LS_COND.
  LS_COND-SALES_ORG_SD    = LS_ORGMAN_WRK-SALES_ORG_SD.
  LS_COND-PROCESS_TYPE    = LF_PROCESS_TYPE.
  LS_COND-SALES_OFFICE_SD = LS_ORGMAN_WRK-SALES_OFFICE_SD.
  LS_COND-SALES_GROUP_SD  = LS_ORGMAN_WRK-SALES_GROUP_SD.

* Determine Profit center
  DATA(LF_PRCTR) = DETERMINE_PROFIT_CENTER( LS_COND ).
  IF LF_PRCTR IS INITIAL.
    RETURN.
  ENDIF.

* Assign Result
  CF_PRCTR = LF_PRCTR.
  CLEAR CF_CALL_SUBSTITUTION.

ENDMETHOD.


METHOD GET_GENC.

  CONSTANTS:
    LC_KOKRS TYPE  ZSDSDE_PARAM_NAME VALUE 'CONTROL_AREA_IN_SCOPE',
    LC_VKORG TYPE  ZSDSDE_PARAM_NAME VALUE 'SALES_ORG_IN_SCOPE',
    LC_WERKS TYPE  ZSDSDE_PARAM_NAME VALUE 'PLANT_IN_SCOPE'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSCM_ENHANCEMENT'.


* Check Already Read?
  IF LF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_KOKRS,
         GT_VKORG,
         GT_WERKS.

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
*     Controlling Area in enhancement scope
*     ------------------------------------
      WHEN LC_KOKRS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_KOKRS.

*     ------------------------------------
*     Sales Org in enhancement scope
*     ------------------------------------
      WHEN LC_VKORG.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_VKORG.

*     ------------------------------------
*     Plant in enhancement scope
*     ------------------------------------
      WHEN LC_WERKS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_WERKS.

    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD GET_I_ZZ1_LGORT.

  IF IS_SDS( ).
    RV_DISABLED = ABAP_FALSE.
  ENDIF.

ENDMETHOD.


METHOD GET_P_ZZ1_BILL_METHOD.

* Only for SDS
  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

  CASE IV_PROPERTY.
    WHEN IF_BSP_WD_MODEL_SETTER_GETTER=>FP_FIELDTYPE.
*     Drop down
      RV_VALUE = CL_BSP_DLC_VIEW_DESCRIPTOR=>FIELD_TYPE_PICKLIST.
  ENDCASE.

ENDMETHOD.


METHOD GET_P_ZZ1_LOB_SRH.

* Only for SDS
  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

  CASE IV_PROPERTY.
    WHEN IF_BSP_WD_MODEL_SETTER_GETTER=>FP_FIELDTYPE.
*     Drop down
      RV_VALUE = CL_BSP_DLC_VIEW_DESCRIPTOR=>FIELD_TYPE_PICKLIST.
  ENDCASE.

ENDMETHOD.


METHOD GET_P_ZZ1_PROACTIVITY.

* Only for SDS
  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

  CASE IV_PROPERTY.
    WHEN IF_BSP_WD_MODEL_SETTER_GETTER=>FP_FIELDTYPE.
*     Drop down
      RV_VALUE = CL_BSP_DLC_VIEW_DESCRIPTOR=>FIELD_TYPE_PICKLIST.
  ENDCASE.

ENDMETHOD.


METHOD GET_V_ZZ1_BILL_METHOD.

  DATA:
    LT_DDLB TYPE BSP_WD_DROPDOWN_TABLE,
    LR_DDLB TYPE REF TO CL_CRM_UIU_DDLB.


* Only for SDS
  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

  SELECT A~DOMVALUE_L AS KEY,
         B~DDTEXT     AS VALUE
    FROM DD07L AS A
           INNER JOIN DD07T AS B                       "#EC CI_BUFFJOIN
             ON  B~DOMNAME = A~DOMNAME
             AND B~DDLANGUAGE = @SY-LANGU
             AND B~AS4LOCAL   = A~AS4LOCAL
             AND B~VALPOS     = A~VALPOS
             AND B~AS4VERS    = A~AS4VERS
   WHERE A~DOMNAME  EQ 'ZSDSDM_BILL_METHOD'
     AND A~AS4LOCAL EQ 'A'
     AND A~AS4VERS  EQ '0000'
    INTO TABLE @LT_DDLB.
  IF SY-SUBRC NE 0.
    CLEAR LT_DDLB.
  ENDIF.

  SORT LT_DDLB BY KEY.

  CREATE OBJECT LR_DDLB
    EXPORTING
      IV_SOURCE_TYPE = 'T'.

  LR_DDLB->SET_SELECTION_TABLE( LT_DDLB ).
  RV_VALUEHELP_DESCRIPTOR = LR_DDLB.

ENDMETHOD.


METHOD GET_V_ZZ1_LGORT.

  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

  CLEAR: RV_VALUEHELP_DESCRIPTOR.

ENDMETHOD.


METHOD GET_V_ZZ1_LOB_SRH.

  TYPES:
    BEGIN OF LTS_Z033,
      LOB   TYPE ZDSMMC033-LOB,
      ZDESC TYPE ZDSMMC033-ZDESC,
    END OF LTS_Z033.
  DATA:
    LT_DDLB TYPE BSP_WD_DROPDOWN_TABLE,
    LS_DDLB TYPE BSP_WD_DROPDOWN_LINE,
    LR_DDLB TYPE REF TO CL_CRM_UIU_DDLB,
    LT_Z033 TYPE STANDARD TABLE OF LTS_Z033.


* Only for SDS
  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

* Space line
  APPEND LS_DDLB TO LT_DDLB.

  SELECT LOB ZDESC
    FROM ZDSMMC033 INTO TABLE LT_Z033
   WHERE ZLANG = SY-LANGU.
  IF SY-SUBRC = 0.
    LOOP AT LT_Z033 ASSIGNING FIELD-SYMBOL(<LS_Z033>).
      LS_DDLB-KEY   = <LS_Z033>-LOB.
      LS_DDLB-VALUE = <LS_Z033>-ZDESC.
      APPEND LS_DDLB TO LT_DDLB.
    ENDLOOP.
    SORT LT_DDLB BY KEY.
  ENDIF.

  CREATE OBJECT LR_DDLB
    EXPORTING
      IV_SOURCE_TYPE = 'T'.

  LR_DDLB->SET_SELECTION_TABLE( LT_DDLB ).
  RV_VALUEHELP_DESCRIPTOR = LR_DDLB.

ENDMETHOD.


METHOD GET_V_ZZ1_PROACTIVITY.

  DATA:
    LT_DDLB TYPE BSP_WD_DROPDOWN_TABLE,
    LR_DDLB TYPE REF TO CL_CRM_UIU_DDLB.


* Only for SDS
  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

  SELECT A~DOMVALUE_L AS KEY,
         B~DDTEXT     AS VALUE
    FROM DD07L AS A
           INNER JOIN DD07T AS B                       "#EC CI_BUFFJOIN
             ON  B~DOMNAME = A~DOMNAME
             AND B~DDLANGUAGE = @SY-LANGU
             AND B~AS4LOCAL   = A~AS4LOCAL
             AND B~VALPOS     = A~VALPOS
             AND B~AS4VERS    = A~AS4VERS
   WHERE A~DOMNAME  EQ 'ZSDSDM_PROACTIVITY'
     AND A~AS4LOCAL EQ 'A'
     AND A~AS4VERS  EQ '0000'
    INTO TABLE @LT_DDLB.
  IF SY-SUBRC NE 0.
    CLEAR LT_DDLB.
  ENDIF.

  SORT LT_DDLB BY KEY.

  CREATE OBJECT LR_DDLB
    EXPORTING
      IV_SOURCE_TYPE = 'T'.

  LR_DDLB->SET_SELECTION_TABLE( LT_DDLB ).
  RV_VALUEHELP_DESCRIPTOR = LR_DDLB.

ENDMETHOD.


METHOD IS_SDS.

  DATA:
    LF_ORG_SPECIFIED TYPE  FLAG,
    LF_ACTIVE        TYPE  CHAR1.


  CLEAR LF_ORG_SPECIFIED.

* Get Constants
  GET_GENC( ).

  IF IF_KOKRS IS SUPPLIED.
    LF_ORG_SPECIFIED = 'X'.
    IF GT_KOKRS IS NOT INITIAL AND
       IF_KOKRS IN GT_KOKRS.
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

  IF IF_WERKS IS SUPPLIED.
    LF_ORG_SPECIFIED = 'X'.
    IF GT_WERKS IS NOT INITIAL AND
       IF_WERKS IN GT_WERKS.
      RF_RESULT = ABAP_TRUE.
    ELSE.
      RF_RESULT = ABAP_FALSE.
      RETURN.
    ENDIF.
  ENDIF.

  IF LF_ORG_SPECIFIED IS INITIAL.
*   Check User Parameter ZSDS_SRV = X
    GET PARAMETER ID GC_ZSDS_SRV FIELD LF_ACTIVE.
    IF LF_ACTIVE EQ 'X'.
      RF_RESULT = ABAP_TRUE.
    ELSE.
      RF_RESULT = ABAP_FALSE.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD CHANGE_IO_COSTING.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSCM_ENHANCEMENT->CHANGE_IO_COSTING
*  Creation Date      : 08.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : CME020
*  Description        : Enhancement to change attributes in IO
*  Purpose            : To determine RA Key in IO created when released
*                       service order
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
    LT_GUID         TYPE  CRMT_OBJECT_GUID_TAB.

  DATA:
    LS_ORGMAN_WRK TYPE  CRMT_ORGMAN_WRK,
    LS_COND       TYPE  TS_DETER_ABGSL.

  DATA:
    LF_FNAME        TYPE  CHAR80 VALUE '(SAPLGCC_OR)GS_CALLBACK-CLASS->EXTERNAL_OBJECT->ATTRIBUTES',
    LF_PROCESS_TYPE TYPE  CRMS4D_SERV_H-PROCESS_TYPE,
    LF_ITM_TYPE     TYPE  CRMS4D_SERV_I-ITM_TYPE,
    LF_OBJTYPE_H    TYPE  CRMS4D_SERV_H-OBJTYPE_H,
    LF_OBJECT_ID    TYPE  CRMS4D_SERV_H-OBJECT_ID.


  FIELD-SYMBOLS:
    <L_ATTRIBUTES> TYPE IAOMT_OBJECT_ATTRIBUTE_TAB,
    <L_ATTRIBUTE>  TYPE IAOM_OBJECT_ATTRIBUTE.


* Only for SDS
  IF NOT IS_SDS( IF_KOKRS = CS_COAS-KOKRS ).
    RETURN.
  ENDIF.

  TRY.
*     Get Attributes
      ASSIGN (LF_FNAME) TO <L_ATTRIBUTES>.
      IF SY-SUBRC NE 0.
*       Cannot read Attributes
        RETURN.
      ENDIF.

    CATCH CX_ROOT ##CATCH_ALL.
*     Cannot read Attributes
      RETURN.
  ENDTRY.

* Get Process Type
  READ TABLE <L_ATTRIBUTES> ASSIGNING <L_ATTRIBUTE>
                            WITH KEY DATA_ELEMENT = 'CRMT_PROCESS_TYPE_CO'.
  IF SY-SUBRC EQ 0.
    LF_PROCESS_TYPE = <L_ATTRIBUTE>-VALUE.
  ENDIF.

* Get Item Type
  READ TABLE <L_ATTRIBUTES> ASSIGNING <L_ATTRIBUTE>
                            WITH KEY DATA_ELEMENT = 'CRMT_ITEM_TYPE_CO'.
  IF SY-SUBRC EQ 0.
    LF_ITM_TYPE = <L_ATTRIBUTE>-VALUE.
  ENDIF.

* Get Business Object type
  SELECT SINGLE OBJECT_TYPE
    FROM CRMC_PROC_TYPE
   WHERE PROCESS_TYPE EQ @LF_PROCESS_TYPE
    INTO @LF_OBJTYPE_H.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Get Object ID
  READ TABLE <L_ATTRIBUTES> ASSIGNING <L_ATTRIBUTE>
                            WITH KEY DATA_ELEMENT = 'CRMT_OBJECT_ID_CO'.
  IF SY-SUBRC EQ 0.
    LF_OBJECT_ID = <L_ATTRIBUTE>-VALUE.
  ENDIF.

* Get Object GUID
  CALL FUNCTION 'CRM_ORDERADM_H_GUID_GET_OB'
    EXPORTING
      IV_OBJECT_ID       = LF_OBJECT_ID
      IV_OBJECT_TYPE     = LF_OBJTYPE_H
    IMPORTING
      ET_ORDERADM_H_GUID = LT_GUID
    EXCEPTIONS
      RECORD_NOT_FOUND   = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  READ TABLE LT_GUID ASSIGNING FIELD-SYMBOL(<L_GUID>)
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Read ORGMAN Object
  CALL FUNCTION 'CRM_ORGMAN_READ_OB'
    EXPORTING
      IV_REF_GUID          = <L_GUID>
      IV_REF_KIND          = 'A'
    IMPORTING
      ES_ORGMAN_WRK        = LS_ORGMAN_WRK
    EXCEPTIONS
      ENTRY_DOES_NOT_EXIST = 1
      PARAMETER_ERROR      = 2
      OTHERS               = 3.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Determine RA Key
  CLEAR LS_COND.
  LS_COND-SALES_ORG_SD  =  LS_ORGMAN_WRK-SALES_ORG_SD.
  LS_COND-PROCESS_TYPE  =  LF_PROCESS_TYPE.
  LS_COND-ITM_TYPE      =  LF_ITM_TYPE.

  DATA(LF_ABGSL) = DETERMINE_RA_KEY( LS_COND ).
  IF LF_ABGSL IS INITIAL.
    RETURN.
  ENDIF.

* Update RA Key
  CS_COAS-ABGSL = LF_ABGSL.

ENDMETHOD.


METHOD DETERMINE_GL_ACCOUNT.

* Initialize Output
  CLEAR: RF_SAKTO.

* Read Configuration
  SELECT SINGLE SAKTO
    FROM ZSDSCMC005
   WHERE SALES_ORG_SD    EQ @IS_COND-SALES_ORG_SD
     AND PROCESS_TYPE    EQ @IS_COND-PROCESS_TYPE
     AND ITM_TYPE        EQ @IS_COND-ITM_TYPE
     AND BKLAS           EQ @IS_COND-BKLAS
     AND ZDEL_FLG EQ @SPACE
    INTO @RF_SAKTO.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.

* Read with Item Type wildcard
  SELECT ITM_TYPE,
         SAKTO
    FROM ZSDSCMC005
   WHERE SALES_ORG_SD    EQ  @IS_COND-SALES_ORG_SD
     AND PROCESS_TYPE    EQ  @IS_COND-PROCESS_TYPE
     AND ITM_TYPE       LIKE '%*%'
     AND BKLAS           EQ  @IS_COND-BKLAS
     AND ZDEL_FLG EQ @SPACE
   ORDER BY ITM_TYPE ASCENDING,
            SAKTO    ASCENDING
    INTO TABLE @DATA(LT_DATA).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Check Item type which can wildcard
  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    CHECK IS_COND-ITM_TYPE CP <L_DATA>-ITM_TYPE.
    RF_SAKTO = <L_DATA>-SAKTO.
    EXIT.
  ENDLOOP.

ENDMETHOD.


METHOD DETERMINE_RA_KEY.

* Initialize Output
  CLEAR: RF_ABGSL.

* Read Configuration
  SELECT SINGLE ABGSL
    FROM ZSDSCMC004
   WHERE SALES_ORG_SD    EQ @IS_COND-SALES_ORG_SD
     AND PROCESS_TYPE    EQ @IS_COND-PROCESS_TYPE
     AND ITM_TYPE        EQ @IS_COND-ITM_TYPE
     AND ZDEL_FLG EQ @SPACE
    INTO @RF_ABGSL.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.

* Read with Item Type wildcard
  SELECT ITM_TYPE,
         ABGSL
    FROM ZSDSCMC004
   WHERE SALES_ORG_SD    EQ  @IS_COND-SALES_ORG_SD
     AND PROCESS_TYPE    EQ  @IS_COND-PROCESS_TYPE
     AND ITM_TYPE       LIKE '%*%'
     AND ZDEL_FLG EQ @SPACE
   ORDER BY ITM_TYPE ASCENDING,
            ABGSL    ASCENDING
    INTO TABLE @DATA(LT_DATA).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Check Item type which can wildcard
  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    CHECK IS_COND-ITM_TYPE CP <L_DATA>-ITM_TYPE.
    RF_ABGSL = <L_DATA>-ABGSL.
    EXIT.
  ENDLOOP.

ENDMETHOD.


METHOD GET_P_ZZ1_PARTNER_SEGMENT.

* Only for SDS
  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

  CASE IV_PROPERTY.
    WHEN IF_BSP_WD_MODEL_SETTER_GETTER=>FP_FIELDTYPE.
*     Drop down
      RV_VALUE = CL_BSP_DLC_VIEW_DESCRIPTOR=>FIELD_TYPE_PICKLIST.
  ENDCASE.

ENDMETHOD.


METHOD GET_V_ZZ1_PARTNER_SEGMENT.

  TYPES:
    BEGIN OF LTS_Z036,
      ZZ1_DIVCD TYPE ZDSMMC036-ZZ1_DIVCD,
      DIVNA     TYPE ZDSMMC036-DIVNA,
    END OF LTS_Z036.

  DATA:
    LT_DDLB TYPE BSP_WD_DROPDOWN_TABLE,
    LS_DDLB TYPE BSP_WD_DROPDOWN_LINE,
    LR_DDLB TYPE REF TO CL_CRM_UIU_DDLB,
    LT_Z036 TYPE STANDARD TABLE OF LTS_Z036.


* Only for SDS
  IF NOT IS_SDS( ).
    RETURN.
  ENDIF.

* Space line
  APPEND LS_DDLB TO LT_DDLB.

  SELECT ZZ1_DIVCD
         DIVNA
    FROM ZDSMMC036 INTO TABLE LT_Z036
   WHERE ZLANG = SY-LANGU.
  IF SY-SUBRC = 0.
    LOOP AT LT_Z036 ASSIGNING FIELD-SYMBOL(<LS_Z036>).
      LS_DDLB-KEY   = <LS_Z036>-ZZ1_DIVCD.
      LS_DDLB-VALUE = <LS_Z036>-DIVNA.
      APPEND LS_DDLB TO LT_DDLB.
    ENDLOOP.
    SORT LT_DDLB BY KEY.
  ENDIF.

  CREATE OBJECT LR_DDLB
    EXPORTING
      IV_SOURCE_TYPE = 'T'.

  LR_DDLB->SET_SELECTION_TABLE( LT_DDLB ).
  RV_VALUEHELP_DESCRIPTOR = LR_DDLB.

ENDMETHOD.


METHOD MB_MIGO_ITEM_BADI_ITEM_MODIFY.

* Only for SDS related
  IF NOT IS_SDS( IF_WERKS = IS_GOITEM-WERKS ).
    RETURN.
  ENDIF.

* Copy Unloading Point to Item Text
  CF_ITEM_TEXT = IS_GOITEM-ABLAD.

ENDMETHOD.


METHOD UPDATE_DATA_IN_PR.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSCM_ENHANCEMENT->PREPARE_PURCH_REQ_SVO
*  Creation Date      : 13.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : CME018 & CME023
*  Description        : Method preparing PR created from SVO
*  Purpose            : - To update equipment no to Unloading Point (CME018
*                       - To determine GL Account from ZSDSCMC005
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
    LT_REFOBJ  TYPE  CRMT_REFOBJ_WRKT.

  DATA:
    LS_COND  TYPE  TS_DETER_SAKTO.

  DATA:
    LF_MATNR        TYPE  MBEW-MATNR,
    LF_ITEM_NO      TYPE  CRMT_SRV_LOG_PROCESS_INFO-ITEM_NO,
    LF_ABLAD        TYPE  EBKN-ABLAD,
    LF_GUID         TYPE  CRMT_OBJECT_GUID,
    LF_EQUIPMENT_ID TYPE  CRMT_ORDPRP_OBJL_I_D_WRK-EQUIPMENT_ID.


* Processing Items
  LOOP AT IT_ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>).

*   Read CRM Info
    LF_ITEM_NO = <L_ITEM>-PREQ_ITEM.
    READ TABLE IT_PROCESS_INFO ASSIGNING FIELD-SYMBOL(<L_PROCESS_INFO>)
                               WITH KEY ITEM_NO = LF_ITEM_NO.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

*   Read ORDERADM_H Data
    LF_GUID = <L_PROCESS_INFO>-HEADER_GUID.
    READ TABLE IS_SRVORDER_DATA-ORDERADM_H INTO DATA(LS_ORDERADM_H)
                                           WITH KEY GUID = LF_GUID.
    IF SY-SUBRC NE 0.
      CLEAR LS_ORDERADM_H.
    ENDIF.

*   Read ORGMAN Data
    LF_GUID = <L_PROCESS_INFO>-HEADER_GUID.
    READ TABLE IS_SRVORDER_DATA-ORGMAN INTO DATA(LS_ORGMAN) "#EC CI_SORTSEQ
                                       WITH KEY REF_GUID = LF_GUID
                                                REF_KIND = 'A'.
    IF SY-SUBRC NE 0.
      CLEAR LS_ORGMAN.
    ENDIF.

*   Read ORDERADM_I Data
    LF_GUID = <L_PROCESS_INFO>-ITEM_GUID.
    READ TABLE IS_SRVORDER_DATA-ORDERADM_I INTO DATA(LS_ORDERADM_I)
                                           WITH KEY GUID = LF_GUID.
    IF SY-SUBRC NE 0.
      CLEAR LS_ORDERADM_I.
    ENDIF.

    IF <L_ITEM>-MATERIAL_LONG IS NOT INITIAL.
      LF_MATNR = <L_ITEM>-MATERIAL_LONG.
    ELSE.
      LF_MATNR = LS_ORDERADM_I-ORDERED_PROD.
    ENDIF.

*   Get ValClass
    SELECT BKLAS
      FROM MBEW
     WHERE MATNR EQ @LF_MATNR
       AND BWKEY EQ @<L_ITEM>-PLANT
     ORDER BY PRIMARY KEY
      INTO @DATA(LF_BKLAS)
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      CLEAR LF_BKLAS.
    ENDIF.

*   Determine GL Account
    CLEAR LS_COND.
    LS_COND-SALES_ORG_SD = LS_ORGMAN-SALES_ORG_SD.
    LS_COND-PROCESS_TYPE = LS_ORDERADM_H-PROCESS_TYPE.
    LS_COND-ITM_TYPE     = LS_ORDERADM_I-ITM_TYPE.
    LS_COND-BKLAS        = LF_BKLAS.

    DATA(LF_SAKTO) = DETERMINE_GL_ACCOUNT( IS_COND = LS_COND ).

*   Read Reference Equipment
    CLEAR LF_EQUIPMENT_ID.
    CLEAR LT_REFOBJ.

    LF_GUID = <L_PROCESS_INFO>-ITEM_GUID.
    CALL FUNCTION 'CRM_REFOBJ_READ_OW'
      EXPORTING
        IV_REF_GUID    = LF_GUID
        IV_REF_KIND    = 'B' "Item
      IMPORTING
        ET_REFOBJ_WRK  = LT_REFOBJ
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
    WRITE LF_EQUIPMENT_ID TO LF_ABLAD LEFT-JUSTIFIED.
    CONDENSE LF_ABLAD.

*   Update Account Assignment
    LOOP AT CT_ACC ASSIGNING FIELD-SYMBOL(<L_ACC>)
                   WHERE PREQ_ITEM EQ <L_ITEM>-PREQ_ITEM.
*     Read Flag Structure
      READ TABLE CT_ACC_X ASSIGNING FIELD-SYMBOL(<L_ACC_X>)
                          WITH KEY PREQ_ITEM = <L_ACC>-PREQ_ITEM
                                   SERIAL_NO = <L_ACC>-SERIAL_NO.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Update GL Account
      IF LF_SAKTO IS NOT INITIAL.
        <L_ACC>-GL_ACCOUNT   = LF_SAKTO.
        <L_ACC_X>-GL_ACCOUNT = 'X'.
      ENDIF.

*     Update Unloading Point
      IF LF_ABLAD IS NOT INITIAL.
        <L_ACC>-UNLOAD_PT   = LF_ABLAD.
        <L_ACC_X>-UNLOAD_PT = 'X'.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDMETHOD.


METHOD IS_ITEM_PROCESSING.

  CONSTANTS:
    LC_SINGLE TYPE IAOM_CRMSRV_CTP-CO_TYPE VALUE 'S',
    LC_ITEM   TYPE  IAOM_CRMSRV_CTP-CO_LEVEL VALUE '02'.


* Initialize Output
  CLEAR: RF_RESULT.

  SELECT CO_TYPE,
         CO_LEVEL
    FROM IAOM_CRMSRV_CTP
   WHERE PROCESS_TYPE_CTP EQ @IF_PROCESS_TYPE
     AND VALID_FROM_DATE  LE @IF_DATUM
     AND VALID_TO_DATE    GE @IF_DATUM
   ORDER BY PRIMARY KEY
    INTO @DATA(LS_CTP)
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    CLEAR LS_CTP.
  ENDIF.

  IF LS_CTP-CO_TYPE EQ LC_SINGLE AND
     LS_CTP-CO_LEVEL EQ LC_ITEM.
    RF_RESULT = 'X'.
  ENDIF.

ENDMETHOD.
ENDCLASS.
