class ZL_BT116H_S_DETAILS_CN18 definition
  public
  inheriting from CL_BT116H_S_DETAILS_CN18
  create public .

public section.

  methods GET_I_ZZ1_LOB_SRH
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_I_ZZ1_PARTNER_SEGMENT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_I_ZZ1_PROACTIVITY
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_M_ZZ1_LOB_SRH
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_M_ZZ1_PARTNER_SEGMENT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_M_ZZ1_PROACTIVITY
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_ZZ1_LOB_SRH
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods GET_ZZ1_PARTNER_SEGMENT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods GET_ZZ1_PROACTIVITY
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_ZZ1_LOB_SRH
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods SET_ZZ1_PARTNER_SEGMENT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods SET_ZZ1_PROACTIVITY
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_V_ZZ1_LOB_SRH
    importing
      !IV_MODE type CHAR1 default IF_BSP_WD_MODEL_SETTER_GETTER=>RUNTIME_MODE
      !IV_INDEX type I optional
    returning
      value(RV_VALUEHELP_DESCRIPTOR) type ref to IF_BSP_WD_VALUEHELP_DESCRIPTOR .
  methods GET_V_ZZ1_PARTNER_SEGMENT
    importing
      !IV_MODE type CHAR1 default IF_BSP_WD_MODEL_SETTER_GETTER=>RUNTIME_MODE
      !IV_INDEX type I optional
    returning
      value(RV_VALUEHELP_DESCRIPTOR) type ref to IF_BSP_WD_VALUEHELP_DESCRIPTOR .
  methods GET_V_ZZ1_PROACTIVITY
    importing
      !IV_MODE type CHAR1 default IF_BSP_WD_MODEL_SETTER_GETTER=>RUNTIME_MODE
      !IV_INDEX type I optional
    returning
      value(RV_VALUEHELP_DESCRIPTOR) type ref to IF_BSP_WD_VALUEHELP_DESCRIPTOR .
  methods GET_P_ZZ1_LOB_SRH
    importing
      !IV_PROPERTY type STRING
      !IV_INDEX type I optional
      !IV_DISPLAY_MODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_VALUE) type STRING .
  methods GET_P_ZZ1_PARTNER_SEGMENT
    importing
      !IV_PROPERTY type STRING
      !IV_INDEX type I optional
      !IV_DISPLAY_MODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_VALUE) type STRING .
  methods GET_P_ZZ1_PROACTIVITY
    importing
      !IV_PROPERTY type STRING
      !IV_INDEX type I optional
      !IV_DISPLAY_MODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_VALUE) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZL_BT116H_S_DETAILS_CN18 IMPLEMENTATION.


  method GET_I_ZZ1_LOB_SRH.
    DATA: current TYPE REF TO if_bol_bo_property_access.

    rv_disabled = 'TRUE'.
    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.

  TRY.

        IF current->is_property_readonly(
                      'ZZ1_LOB_SRH' ) = abap_false. "#EC NOTEXT
          rv_disabled = 'FALSE'.
        ENDIF.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.



  endmethod.


  method GET_I_ZZ1_PARTNER_SEGMENT.
    DATA: current TYPE REF TO if_bol_bo_property_access.

    rv_disabled = 'TRUE'.
    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.

  TRY.

        IF current->is_property_readonly(
                      'ZZ1_PARTNER_SEGMENT' ) = abap_false. "#EC NOTEXT
          rv_disabled = 'FALSE'.
        ENDIF.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.



  endmethod.


  method GET_I_ZZ1_PROACTIVITY.
    DATA: current TYPE REF TO if_bol_bo_property_access.

    rv_disabled = 'TRUE'.
    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.

  TRY.

        IF current->is_property_readonly(
                      'ZZ1_PROACTIVITY' ) = abap_false. "#EC NOTEXT
          rv_disabled = 'FALSE'.
        ENDIF.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.



  endmethod.


  method GET_M_ZZ1_LOB_SRH.

  DATA: attr    TYPE ZDSDE_LOB.

  DATA: dref    TYPE REF TO data.

  GET REFERENCE OF attr INTO dref.

  metadata ?= if_bsp_model_binding~get_attribute_metadata(
       attribute_ref  = dref
       attribute_path = attribute_path
       name           = 'ZZ1_LOB_SRH'  "#EC NOTEXT
*      COMPONENT      =
       no_getter      = 1 ).


  endmethod.


  method GET_M_ZZ1_PARTNER_SEGMENT.

  DATA: attr    TYPE ZSDSDE_CM_PARTNER_SEGMENT.

  DATA: dref    TYPE REF TO data.

  GET REFERENCE OF attr INTO dref.

  metadata ?= if_bsp_model_binding~get_attribute_metadata(
       attribute_ref  = dref
       attribute_path = attribute_path
       name           = 'ZZ1_PARTNER_SEGMENT'  "#EC NOTEXT
*      COMPONENT      =
       no_getter      = 1 ).


  endmethod.


  method GET_M_ZZ1_PROACTIVITY.

  DATA: attr    TYPE ZSDSDE_PROACTIVITY.

  DATA: dref    TYPE REF TO data.

  GET REFERENCE OF attr INTO dref.

  metadata ?= if_bsp_model_binding~get_attribute_metadata(
       attribute_ref  = dref
       attribute_path = attribute_path
       name           = 'ZZ1_PROACTIVITY'  "#EC NOTEXT
*      COMPONENT      =
       no_getter      = 1 ).


  endmethod.


METHOD GET_P_ZZ1_LOB_SRH.

  RV_VALUE = ZCL_SDSCM_ENHANCEMENT=>GET_P_ZZ1_LOB_SRH(
               EXPORTING IV_PROPERTY = IV_PROPERTY
                         IV_INDEX    = IV_INDEX
                         IV_DISPLAY_MODE = IV_DISPLAY_MODE ).

ENDMETHOD.


METHOD GET_P_ZZ1_PARTNER_SEGMENT.
*<-- SDS: Start of Insertion CME021 28.08.2024
* SDS: Set field list of values
  RV_VALUE = ZCL_SDSCM_ENHANCEMENT=>GET_P_ZZ1_PARTNER_SEGMENT(
    EXPORTING
      IV_PROPERTY     = IV_PROPERTY
      IV_INDEX        = IV_INDEX
      IV_DISPLAY_MODE = IV_DISPLAY_MODE ).
*--> SDS: End of Insertion CME021 28.08.2024
ENDMETHOD.


METHOD GET_P_ZZ1_PROACTIVITY.

  RV_VALUE = ZCL_SDSCM_ENHANCEMENT=>GET_P_ZZ1_PROACTIVITY(
               EXPORTING IV_PROPERTY = IV_PROPERTY
                         IV_INDEX    = IV_INDEX
                         IV_DISPLAY_MODE = IV_DISPLAY_MODE ).

ENDMETHOD.


METHOD GET_V_ZZ1_LOB_SRH.
  RV_VALUEHELP_DESCRIPTOR = ZCL_SDSCM_ENHANCEMENT=>GET_V_ZZ1_LOB_SRH(
                               EXPORTING IV_MODE = IV_MODE
                                         IV_INDEX = IV_INDEX ).
ENDMETHOD.


METHOD GET_V_ZZ1_PARTNER_SEGMENT.
*<-- SDS: Start of Insertion CME021 28.08.2024
* SDS: Set field list of values
  RV_VALUEHELP_DESCRIPTOR = ZCL_SDSCM_ENHANCEMENT=>GET_V_ZZ1_PARTNER_SEGMENT(
    EXPORTING
      IV_MODE  = IV_MODE
      IV_INDEX = IV_INDEX ).
*--> SDS: End of Insertion CME021 28.08.2024
ENDMETHOD.


METHOD GET_V_ZZ1_PROACTIVITY.
  RV_VALUEHELP_DESCRIPTOR = ZCL_SDSCM_ENHANCEMENT=>GET_V_ZZ1_PROACTIVITY(
                               EXPORTING IV_MODE = IV_MODE
                                         IV_INDEX = IV_INDEX ).
ENDMETHOD.


  method GET_ZZ1_LOB_SRH.

    DATA: current TYPE REF TO if_bol_bo_property_access.
    DATA: dref    TYPE REF TO data.


    value =
'BTCustomerH not bound'."#EC NOTEXT


    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.


  TRY.

    TRY.
        dref = current->get_property( 'ZZ1_LOB_SRH' ). "#EC NOTEXT
      CATCH cx_crm_cic_parameter_error.
    ENDTRY.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.

    IF dref IS NOT BOUND.

      value = 'BTCustomerH/ZZ1_LOB_SRH not bound'."#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        value = if_bsp_model_util~convert_to_string( data_ref = dref
                                    attribute_path = attribute_path ).
      CATCH cx_bsp_conv_illegal_ref.
        FIELD-SYMBOLS: <l_data> type DATA.
        assign dref->* to <l_data>.
*       please implement here some BO specific handler coding
*       conversion of currency/quantity field failed caused by missing
*       unit relation
*       Coding sample:
*       provide currency, decimals, and reference type
*       value = cl_bsp_utility=>make_string(
*                          value = <l_data>
*                          reference_value = c_currency
*                          num_decimals = decimals
*                          reference_type = reference_type
*                          ).
          value = '-CURR/QUANT REF DATA MISSING-'.
      CATCH cx_root.
        value = '-CONVERSION FAILED-'.                  "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_ZZ1_PARTNER_SEGMENT.

    DATA: current TYPE REF TO if_bol_bo_property_access.
    DATA: dref    TYPE REF TO data.


    value =
'BTCustomerH not bound'."#EC NOTEXT


    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.


  TRY.

    TRY.
        dref = current->get_property( 'ZZ1_PARTNER_SEGMENT' ). "#EC NOTEXT
      CATCH cx_crm_cic_parameter_error.
    ENDTRY.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.

    IF dref IS NOT BOUND.

      value = 'BTCustomerH/ZZ1_PARTNER_SEGMENT not bound'."#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        value = if_bsp_model_util~convert_to_string( data_ref = dref
                                    attribute_path = attribute_path ).
      CATCH cx_bsp_conv_illegal_ref.
        FIELD-SYMBOLS: <l_data> type DATA.
        assign dref->* to <l_data>.
*       please implement here some BO specific handler coding
*       conversion of currency/quantity field failed caused by missing
*       unit relation
*       Coding sample:
*       provide currency, decimals, and reference type
*       value = cl_bsp_utility=>make_string(
*                          value = <l_data>
*                          reference_value = c_currency
*                          num_decimals = decimals
*                          reference_type = reference_type
*                          ).
          value = '-CURR/QUANT REF DATA MISSING-'.
      CATCH cx_root.
        value = '-CONVERSION FAILED-'.                  "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_ZZ1_PROACTIVITY.

    DATA: current TYPE REF TO if_bol_bo_property_access.
    DATA: dref    TYPE REF TO data.


    value =
'BTCustomerH not bound'."#EC NOTEXT


    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.


  TRY.

    TRY.
        dref = current->get_property( 'ZZ1_PROACTIVITY' ). "#EC NOTEXT
      CATCH cx_crm_cic_parameter_error.
    ENDTRY.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.

    IF dref IS NOT BOUND.

      value = 'BTCustomerH/ZZ1_PROACTIVITY not bound'."#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        value = if_bsp_model_util~convert_to_string( data_ref = dref
                                    attribute_path = attribute_path ).
      CATCH cx_bsp_conv_illegal_ref.
        FIELD-SYMBOLS: <l_data> type DATA.
        assign dref->* to <l_data>.
*       please implement here some BO specific handler coding
*       conversion of currency/quantity field failed caused by missing
*       unit relation
*       Coding sample:
*       provide currency, decimals, and reference type
*       value = cl_bsp_utility=>make_string(
*                          value = <l_data>
*                          reference_value = c_currency
*                          num_decimals = decimals
*                          reference_type = reference_type
*                          ).
          value = '-CURR/QUANT REF DATA MISSING-'.
      CATCH cx_root.
        value = '-CONVERSION FAILED-'.                  "#EC NOTEXT
    ENDTRY.


  endmethod.


  method SET_ZZ1_LOB_SRH.
    DATA:
      current TYPE REF TO if_bol_bo_property_access,
      dref    TYPE REF TO data,
      copy    TYPE REF TO data.

    FIELD-SYMBOLS:
      <nval> TYPE ANY,
      <oval> TYPE ANY.

*   get current entity
    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.

*   get old value and dataref to appropriate type

  TRY.

    TRY.
        dref = current->get_property( 'ZZ1_LOB_SRH' ). "#EC NOTEXT
      CATCH cx_crm_cic_parameter_error.
    ENDTRY.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.


*   assure that attribue exists
    CHECK dref IS BOUND.

*   set <oval> to old value
    ASSIGN dref->* TO <oval>.
*   create a copy for new value
    CREATE DATA copy LIKE <oval>.
*   set <nval> to new value
    ASSIGN copy->* TO <nval>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD if_bsp_model_util~convert_from_string
          EXPORTING
            data_ref       = copy
            value          = value
            attribute_path = attribute_path.
*        CATCH cx_bsp_conv_illegal_ref.
*          FIELD-SYMBOLS: <l_data> type DATA.
*          assign copy->* to <l_data>.
*         please implement here some BO specific handler coding
*         conversion of currency/quantity field failed caused by missing
*         unit relation
*         Coding sample:
*         provide currency for currency fields or decimals for quantity (select from T006).
*          cl_bsp_utility=>instantiate_simple_data(
*                             value = value
*                             reference = c_currency
*                             num_decimals = decimals
*                             use_bsp_exceptions = abap_true
*                             data = <l_data> ).
*      ENDTRY.
      CATCH cx_sy_conversion_error.
        RAISE EXCEPTION TYPE cx_bsp_conv_failed
          EXPORTING
            name = 'ZZ1_LOB_SRH'."#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <nval> <> <oval>.

      current->set_property(
                      iv_attr_name = 'ZZ1_LOB_SRH' "#EC NOTEXT
                      iv_value     = <nval> ).

    ENDIF.


  endmethod.


  method SET_ZZ1_PARTNER_SEGMENT.
    DATA:
      current TYPE REF TO if_bol_bo_property_access,
      dref    TYPE REF TO data,
      copy    TYPE REF TO data.

    FIELD-SYMBOLS:
      <nval> TYPE ANY,
      <oval> TYPE ANY.

*   get current entity
    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.

*   get old value and dataref to appropriate type

  TRY.

    TRY.
        dref = current->get_property( 'ZZ1_PARTNER_SEGMENT' ). "#EC NOTEXT
      CATCH cx_crm_cic_parameter_error.
    ENDTRY.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.


*   assure that attribue exists
    CHECK dref IS BOUND.

*   set <oval> to old value
    ASSIGN dref->* TO <oval>.
*   create a copy for new value
    CREATE DATA copy LIKE <oval>.
*   set <nval> to new value
    ASSIGN copy->* TO <nval>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD if_bsp_model_util~convert_from_string
          EXPORTING
            data_ref       = copy
            value          = value
            attribute_path = attribute_path.
*        CATCH cx_bsp_conv_illegal_ref.
*          FIELD-SYMBOLS: <l_data> type DATA.
*          assign copy->* to <l_data>.
*         please implement here some BO specific handler coding
*         conversion of currency/quantity field failed caused by missing
*         unit relation
*         Coding sample:
*         provide currency for currency fields or decimals for quantity (select from T006).
*          cl_bsp_utility=>instantiate_simple_data(
*                             value = value
*                             reference = c_currency
*                             num_decimals = decimals
*                             use_bsp_exceptions = abap_true
*                             data = <l_data> ).
*      ENDTRY.
      CATCH cx_sy_conversion_error.
        RAISE EXCEPTION TYPE cx_bsp_conv_failed
          EXPORTING
            name = 'ZZ1_PARTNER_SEGMENT'."#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <nval> <> <oval>.

      current->set_property(
                      iv_attr_name = 'ZZ1_PARTNER_SEGMENT' "#EC NOTEXT
                      iv_value     = <nval> ).

    ENDIF.


  endmethod.


  method SET_ZZ1_PROACTIVITY.
    DATA:
      current TYPE REF TO if_bol_bo_property_access,
      dref    TYPE REF TO data,
      copy    TYPE REF TO data.

    FIELD-SYMBOLS:
      <nval> TYPE ANY,
      <oval> TYPE ANY.

*   get current entity
    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.

*   get old value and dataref to appropriate type

  TRY.

    TRY.
        dref = current->get_property( 'ZZ1_PROACTIVITY' ). "#EC NOTEXT
      CATCH cx_crm_cic_parameter_error.
    ENDTRY.

    CATCH cx_sy_ref_is_initial cx_sy_move_cast_error
          cx_crm_genil_model_error.
      RETURN.
  ENDTRY.


*   assure that attribue exists
    CHECK dref IS BOUND.

*   set <oval> to old value
    ASSIGN dref->* TO <oval>.
*   create a copy for new value
    CREATE DATA copy LIKE <oval>.
*   set <nval> to new value
    ASSIGN copy->* TO <nval>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD if_bsp_model_util~convert_from_string
          EXPORTING
            data_ref       = copy
            value          = value
            attribute_path = attribute_path.
*        CATCH cx_bsp_conv_illegal_ref.
*          FIELD-SYMBOLS: <l_data> type DATA.
*          assign copy->* to <l_data>.
*         please implement here some BO specific handler coding
*         conversion of currency/quantity field failed caused by missing
*         unit relation
*         Coding sample:
*         provide currency for currency fields or decimals for quantity (select from T006).
*          cl_bsp_utility=>instantiate_simple_data(
*                             value = value
*                             reference = c_currency
*                             num_decimals = decimals
*                             use_bsp_exceptions = abap_true
*                             data = <l_data> ).
*      ENDTRY.
      CATCH cx_sy_conversion_error.
        RAISE EXCEPTION TYPE cx_bsp_conv_failed
          EXPORTING
            name = 'ZZ1_PROACTIVITY'."#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <nval> <> <oval>.

      current->set_property(
                      iv_attr_name = 'ZZ1_PROACTIVITY' "#EC NOTEXT
                      iv_value     = <nval> ).

    ENDIF.


  endmethod.
ENDCLASS.
