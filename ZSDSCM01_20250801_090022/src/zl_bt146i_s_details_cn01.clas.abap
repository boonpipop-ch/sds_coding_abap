class ZL_BT146I_S_DETAILS_CN01 definition
  public
  inheriting from CL_BSP_WD_CONTEXT_NODE
  create public .

public section.

  constants BASE_ENTITY_NAME type CRMT_EXT_OBJ_NAME value 'BTProductI' ##NO_TEXT.

  methods GET_M_PLANT_DESCRIPTION
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PLANT_DESCRIPTION
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PLANT_DESCRIPTION
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PLANT_DESCRIPTION
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods ON_NEW_FOCUS
    for event NEW_FOCUS of CL_BSP_WD_COLLECTION_WRAPPER
    importing
      !FOCUS_BO .
  methods GET_M_GUID
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_GUID
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_GUID
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_GUID
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_GROSS_WEIGHT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_GROSS_WEIGHT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_GROSS_WEIGHT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_GROSS_WEIGHT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_NET_WEIGHT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_NET_WEIGHT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_NET_WEIGHT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_NET_WEIGHT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_WEIGHT_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_WEIGHT_UNIT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_WEIGHT_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_WEIGHT_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_VOLUME
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_VOLUME
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_VOLUME
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_VOLUME
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_VOLUME_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_VOLUME_UNIT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_VOLUME_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_VOLUME_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PRICE_PRODUCT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PRICE_PRODUCT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PRICE_PRODUCT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PRICE_PRODUCT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PROCESS_QTY_NUM
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PROCESS_QTY_NUM
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PROCESS_QTY_NUM
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PROCESS_QTY_NUM
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PROCESS_QTY_DEN
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PROCESS_QTY_DEN
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PROCESS_QTY_DEN
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PROCESS_QTY_DEN
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_EXPONENT10
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_EXPONENT10
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_EXPONENT10
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_EXPONENT10
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PROCESS_QTY_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PROCESS_QTY_UNIT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PROCESS_QTY_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PROCESS_QTY_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_COMM_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_COMM_GROUP
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_COMM_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_COMM_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_REBATE_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_REBATE_GROUP
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_REBATE_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_REBATE_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_CASH_DISC
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_CASH_DISC
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_CASH_DISC
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_CASH_DISC
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PROD_PR_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PROD_PR_GROUP
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PROD_PR_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PROD_PR_GROUP
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PRC_GROUP1
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PRC_GROUP1
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PRC_GROUP1
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PRC_GROUP1
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PRC_GROUP2
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PRC_GROUP2
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PRC_GROUP2
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PRC_GROUP2
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PRC_GROUP3
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PRC_GROUP3
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PRC_GROUP3
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PRC_GROUP3
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PRC_GROUP4
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PRC_GROUP4
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PRC_GROUP4
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PRC_GROUP4
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PRC_GROUP5
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PRC_GROUP5
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PRC_GROUP5
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PRC_GROUP5
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PRODUCT_SEGMENT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PRODUCT_SEGMENT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PRODUCT_SEGMENT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PRODUCT_SEGMENT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PROD_HIERARCHY
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PROD_HIERARCHY
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PROD_HIERARCHY
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PROD_HIERARCHY
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_RESID_VALUE_ST
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_RESID_VALUE_ST
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_RESID_VALUE_ST
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_RESID_VALUE_ST
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_RESID_VALUE_GRP
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_RESID_VALUE_GRP
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_RESID_VALUE_GRP
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_RESID_VALUE_GRP
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_BATCH_ID
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_BATCH_ID
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_BATCH_ID
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_BATCH_ID
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_INT_INSPECTION
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_INT_INSPECTION
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_INT_INSPECTION
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_INT_INSPECTION
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_RET_PROC_IND
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_RET_PROC_IND
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_RET_PROC_IND
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_RET_PROC_IND
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PLANT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PLANT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PLANT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PLANT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_PRODUCT_I_DUMMY
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_PRODUCT_I_DUMMY
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_PRODUCT_I_DUMMY
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_PRODUCT_I_DUMMY
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_BASE_QTY_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_BASE_QTY_UNIT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_BASE_QTY_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_BASE_QTY_UNIT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_INT_OBJ_NO_BT
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_INT_OBJ_NO_BT
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_INT_OBJ_NO_BT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_INT_OBJ_NO_BT
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_EXCH_BUSINESS
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_EXCH_BUSINESS
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_EXCH_BUSINESS
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_EXCH_BUSINESS
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_IS_GIFTCARD
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_IS_GIFTCARD
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_IS_GIFTCARD
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_IS_GIFTCARD
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
  methods GET_M_GC_SEND_TYPE
    importing
      !ATTRIBUTE_PATH type STRING
    returning
      value(METADATA) type ref to IF_BSP_METADATA_SIMPLE .
  methods GET_I_GC_SEND_TYPE
    importing
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(RV_DISABLED) type STRING .
  methods GET_GC_SEND_TYPE
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
    returning
      value(VALUE) type STRING .
  methods SET_GC_SEND_TYPE
    importing
      !ATTRIBUTE_PATH type STRING
      !ITERATOR type ref to IF_BOL_BO_COL_ITERATOR optional
      !VALUE type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZL_BT146I_S_DETAILS_CN01 IMPLEMENTATION.


  method GET_BASE_QTY_UNIT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'BASE_QTY_UNIT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/BASE_QTY_UNIT not bound'.         "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_BATCH_ID.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'BATCH_ID' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/BATCH_ID not bound'.              "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_CASH_DISC.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'CASH_DISC' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/CASH_DISC not bound'.             "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_COMM_GROUP.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'COMM_GROUP' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/COMM_GROUP not bound'.            "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_EXCH_BUSINESS.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'EXCH_BUSINESS' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/EXCH_BUSINESS not bound'.         "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_EXPONENT10.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'EXPONENT10' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/EXPONENT10 not bound'.            "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_GC_SEND_TYPE.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'GC_SEND_TYPE' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/GC_SEND_TYPE not bound'.          "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_GROSS_WEIGHT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'GROSS_WEIGHT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/GROSS_WEIGHT not bound'.          "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_GUID.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'GUID' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/GUID not bound'.                  "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_INT_INSPECTION.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'INT_INSPECTION' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/INT_INSPECTION not bound'.        "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_INT_OBJ_NO_BT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'INT_OBJ_NO_BT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/INT_OBJ_NO_BT not bound'.         "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_IS_GIFTCARD.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'IS_GIFTCARD' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/IS_GIFTCARD not bound'.           "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_I_BASE_QTY_UNIT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'BASE_QTY_UNIT' ) = ABAP_FALSE.       "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_BATCH_ID.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'BATCH_ID' ) = ABAP_FALSE.            "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_CASH_DISC.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'CASH_DISC' ) = ABAP_FALSE.           "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_COMM_GROUP.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'COMM_GROUP' ) = ABAP_FALSE.          "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_EXCH_BUSINESS.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'EXCH_BUSINESS' ) = ABAP_FALSE.       "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_EXPONENT10.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'EXPONENT10' ) = ABAP_FALSE.          "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_GC_SEND_TYPE.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'GC_SEND_TYPE' ) = ABAP_FALSE.        "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_GROSS_WEIGHT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'GROSS_WEIGHT' ) = ABAP_FALSE.        "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_GUID.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'GUID' ) = ABAP_FALSE.                "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_INT_INSPECTION.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'INT_INSPECTION' ) = ABAP_FALSE.      "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_INT_OBJ_NO_BT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'INT_OBJ_NO_BT' ) = ABAP_FALSE.       "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_IS_GIFTCARD.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'IS_GIFTCARD' ) = ABAP_FALSE.         "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_NET_WEIGHT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'NET_WEIGHT' ) = ABAP_FALSE.          "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PLANT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PLANT' ) = ABAP_FALSE.               "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PLANT_DESCRIPTION.
    DATA: current TYPE REF TO if_bol_bo_property_access.

    rv_disabled = 'TRUE'.
    if iterator is bound.
      current = iterator->get_current( ).
    else.
      current = collection_wrapper->get_current( ).
    endif.

     TRY.
        IF current->is_property_readonly(
                      'PLANT_DESCRIPTION' ) = abap_false. "#EC NOTEXT
          rv_disabled = 'FALSE'.
        ENDIF.
      CATCH cx_sy_ref_is_initial.
    ENDTRY.



  endmethod.


  method GET_I_PRC_GROUP1.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PRC_GROUP1' ) = ABAP_FALSE.          "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PRC_GROUP2.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PRC_GROUP2' ) = ABAP_FALSE.          "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PRC_GROUP3.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PRC_GROUP3' ) = ABAP_FALSE.          "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PRC_GROUP4.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PRC_GROUP4' ) = ABAP_FALSE.          "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PRC_GROUP5.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PRC_GROUP5' ) = ABAP_FALSE.          "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PRICE_PRODUCT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PRICE_PRODUCT' ) = ABAP_FALSE.       "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PROCESS_QTY_DEN.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PROCESS_QTY_DEN' ) = ABAP_FALSE.     "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PROCESS_QTY_NUM.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PROCESS_QTY_NUM' ) = ABAP_FALSE.     "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PROCESS_QTY_UNIT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PROCESS_QTY_UNIT' ) = ABAP_FALSE.    "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PRODUCT_I_DUMMY.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PRODUCT_I_DUMMY' ) = ABAP_FALSE.     "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PRODUCT_SEGMENT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PRODUCT_SEGMENT' ) = ABAP_FALSE.     "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PROD_HIERARCHY.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PROD_HIERARCHY' ) = ABAP_FALSE.      "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_PROD_PR_GROUP.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'PROD_PR_GROUP' ) = ABAP_FALSE.       "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_REBATE_GROUP.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'REBATE_GROUP' ) = ABAP_FALSE.        "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_RESID_VALUE_GRP.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'RESID_VALUE_GRP' ) = ABAP_FALSE.     "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_RESID_VALUE_ST.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'RESID_VALUE_ST' ) = ABAP_FALSE.      "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_RET_PROC_IND.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'RET_PROC_IND' ) = ABAP_FALSE.        "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_VOLUME.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'VOLUME' ) = ABAP_FALSE.              "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_VOLUME_UNIT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'VOLUME_UNIT' ) = ABAP_FALSE.         "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_I_WEIGHT_UNIT.

    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.

    RV_DISABLED = 'TRUE'.
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

    TRY.

        IF CURRENT->IS_PROPERTY_READONLY(
                      'WEIGHT_UNIT' ) = ABAP_FALSE.         "#EC NOTEXT
          RV_DISABLED = 'FALSE'.
        ENDIF.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.



  endmethod.


  method GET_M_BASE_QTY_UNIT.


    DATA: ATTR    TYPE COMT_PRODUCT_BASE_UOM.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'BASE_QTY_UNIT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_BATCH_ID.


    DATA: ATTR    TYPE CRMT_BATCH_ID.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'BATCH_ID'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_CASH_DISC.


    DATA: ATTR    TYPE CRMT_CASH_DISC.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'CASH_DISC'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_COMM_GROUP.


    DATA: ATTR    TYPE CRMT_COMM_GROUP.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'COMM_GROUP'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_EXCH_BUSINESS.


    DATA: ATTR    TYPE CRMT_EXCH_BUSINESS_REL.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'EXCH_BUSINESS'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_EXPONENT10.


    DATA: ATTR    TYPE CRMT_EXPONENT10.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'EXPONENT10'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_GC_SEND_TYPE.


    DATA: ATTR    TYPE STRING.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'GC_SEND_TYPE'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_GROSS_WEIGHT.


    DATA: ATTR    TYPE CRMT_GROSS_WEIGHT.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'GROSS_WEIGHT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_GUID.


    DATA: ATTR    TYPE CRMT_OBJECT_GUID.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'GUID'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_INT_INSPECTION.


    DATA: ATTR    TYPE CRMT_INT_INSPECTION.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'INT_INSPECTION'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_INT_OBJ_NO_BT.


    DATA: ATTR    TYPE CRMT_INT_OBJ_NO_BT.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'INT_OBJ_NO_BT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_IS_GIFTCARD.


    DATA: ATTR    TYPE CRMT_BOOLEAN.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'IS_GIFTCARD'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_NET_WEIGHT.


    DATA: ATTR    TYPE CRMT_NET_WEIGHT.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'NET_WEIGHT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PLANT.


    DATA: ATTR    TYPE WERKS_D.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PLANT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PLANT_DESCRIPTION.

  DATA: attr    TYPE WERKS_D.

  DATA: dref    TYPE REF TO data.

  GET REFERENCE OF attr INTO dref.

  metadata ?= if_bsp_model_binding~get_attribute_metadata(
       attribute_ref  = dref
       attribute_path = attribute_path
       name           = 'PLANT_DESCRIPTION'  "#EC NOTEXT
*      COMPONENT      =
       no_getter      = 1 ).


  endmethod.


  method GET_M_PRC_GROUP1.


    DATA: ATTR    TYPE CRMT_PRC_GROUP1.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PRC_GROUP1'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PRC_GROUP2.


    DATA: ATTR    TYPE CRMT_PRC_GROUP2.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PRC_GROUP2'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PRC_GROUP3.


    DATA: ATTR    TYPE CRMT_PRC_GROUP3.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PRC_GROUP3'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PRC_GROUP4.


    DATA: ATTR    TYPE CRMT_PRC_GROUP4.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PRC_GROUP4'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PRC_GROUP5.


    DATA: ATTR    TYPE CRMT_PRC_GROUP5.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PRC_GROUP5'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PRICE_PRODUCT.


    DATA: ATTR    TYPE CRMT_PRICE_PRODUCT.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PRICE_PRODUCT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PROCESS_QTY_DEN.


    DATA: ATTR    TYPE CRMT_PROCESS_QTY_DEN.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PROCESS_QTY_DEN'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PROCESS_QTY_NUM.


    DATA: ATTR    TYPE CRMT_PROCESS_QTY_NUM.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PROCESS_QTY_NUM'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PROCESS_QTY_UNIT.


    DATA: ATTR    TYPE CRMT_PROCESS_QTY_UNIT.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PROCESS_QTY_UNIT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PRODUCT_I_DUMMY.


    DATA: ATTR    TYPE DUMMY.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PRODUCT_I_DUMMY'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PRODUCT_SEGMENT.


    DATA: ATTR    TYPE CRMT_MKTTG_PRSEG_GUID.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PRODUCT_SEGMENT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PROD_HIERARCHY.


    DATA: ATTR    TYPE CRMT_PROD_HIERARCHY.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PROD_HIERARCHY'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_PROD_PR_GROUP.


    DATA: ATTR    TYPE CRMT_PROD_PR_GROUP.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'PROD_PR_GROUP'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_REBATE_GROUP.


    DATA: ATTR    TYPE CRMT_REBATE_GROUP.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'REBATE_GROUP'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_RESID_VALUE_GRP.


    DATA: ATTR    TYPE CRMT_RESID_VALUE_GRP.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'RESID_VALUE_GRP'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_RESID_VALUE_ST.


    DATA: ATTR    TYPE CRMT_RESID_VALUE_ST.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'RESID_VALUE_ST'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_RET_PROC_IND.


    DATA: ATTR    TYPE STRING.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'RET_PROC_IND'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_VOLUME.


    DATA: ATTR    TYPE CRMT_VOLUME.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'VOLUME'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_VOLUME_UNIT.


    DATA: ATTR    TYPE COMT_VOLUME_UNIT.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'VOLUME_UNIT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_M_WEIGHT_UNIT.


    DATA: ATTR    TYPE COMT_WEIGHT_UNIT.

    DATA: DREF    TYPE REF TO DATA.

    GET REFERENCE OF ATTR INTO DREF.

    METADATA ?= IF_BSP_MODEL_BINDING~GET_ATTRIBUTE_METADATA(
      ATTRIBUTE_REF  = DREF
      ATTRIBUTE_PATH = ATTRIBUTE_PATH
      NAME           = 'WEIGHT_UNIT'  "#EC NOTEXT
*     COMPONENT      =
      NO_GETTER      = 1 ).


  endmethod.


  method GET_NET_WEIGHT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'NET_WEIGHT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/NET_WEIGHT not bound'.            "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PLANT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PLANT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PLANT not bound'.                 "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PLANT_DESCRIPTION.

    DATA: current TYPE REF TO if_bol_bo_property_access.
    DATA: lv_plant TYPE t001w-werks.
    DATA: ls_t001w TYPE t001w.

    value = ''.                                             "#EC NOTEXT

    IF iterator IS BOUND.
      current = iterator->get_current( ).
    ELSE.
      current = collection_wrapper->get_current( ).
    ENDIF.


    TRY.
        value = current->get_property_as_string(
          EXPORTING
            iv_attr_name      = 'PLANT'    " Component Name
        ).                                                  "#EC NOTEXT
      CATCH cx_sy_ref_is_initial.
        RETURN.
    ENDTRY.

    CHECK value IS NOT INITIAL.

    lv_plant = value.

    CALL FUNCTION 'GET_PLANT_DETAILS'
      EXPORTING
        i_werks         = lv_plant
      IMPORTING
        e_t001w         = ls_t001w
      EXCEPTIONS
        not_found       = 1
        parameter_error = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    value = ls_t001w-name1.


  endmethod.


  method GET_PRC_GROUP1.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP1' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PRC_GROUP1 not bound'.            "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PRC_GROUP2.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP2' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PRC_GROUP2 not bound'.            "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PRC_GROUP3.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP3' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PRC_GROUP3 not bound'.            "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PRC_GROUP4.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP4' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PRC_GROUP4 not bound'.            "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PRC_GROUP5.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP5' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PRC_GROUP5 not bound'.            "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PRICE_PRODUCT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRICE_PRODUCT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PRICE_PRODUCT not bound'.         "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PROCESS_QTY_DEN.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROCESS_QTY_DEN' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PROCESS_QTY_DEN not bound'.       "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PROCESS_QTY_NUM.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROCESS_QTY_NUM' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PROCESS_QTY_NUM not bound'.       "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PROCESS_QTY_UNIT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROCESS_QTY_UNIT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PROCESS_QTY_UNIT not bound'.      "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PRODUCT_I_DUMMY.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRODUCT_I_DUMMY' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PRODUCT_I_DUMMY not bound'.       "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PRODUCT_SEGMENT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRODUCT_SEGMENT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PRODUCT_SEGMENT not bound'.       "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PROD_HIERARCHY.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROD_HIERARCHY' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PROD_HIERARCHY not bound'.        "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_PROD_PR_GROUP.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROD_PR_GROUP' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/PROD_PR_GROUP not bound'.         "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_REBATE_GROUP.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'REBATE_GROUP' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/REBATE_GROUP not bound'.          "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_RESID_VALUE_GRP.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'RESID_VALUE_GRP' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/RESID_VALUE_GRP not bound'.       "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_RESID_VALUE_ST.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'RESID_VALUE_ST' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/RESID_VALUE_ST not bound'.        "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_RET_PROC_IND.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'RET_PROC_IND' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/RET_PROC_IND not bound'.          "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_VOLUME.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'VOLUME' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/VOLUME not bound'.                "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_VOLUME_UNIT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'VOLUME_UNIT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/VOLUME_UNIT not bound'.           "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method GET_WEIGHT_UNIT.


    DATA: CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS.
    DATA: DREF    TYPE REF TO DATA.


    VALUE =
'BTProductI not bound'.                                     "#EC NOTEXT


    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.


    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'WEIGHT_UNIT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.

    IF DREF IS NOT BOUND.

      VALUE = 'BTProductI/WEIGHT_UNIT not bound'.           "#EC NOTEXT

      RETURN.
    ENDIF.
    TRY.
        VALUE = IF_BSP_MODEL_UTIL~CONVERT_TO_STRING( DATA_REF       = DREF
                                                     ATTRIBUTE_PATH = ATTRIBUTE_PATH ).
      CATCH CX_BSP_CONV_ILLEGAL_REF.
        FIELD-SYMBOLS: <L_DATA> TYPE DATA.
        ASSIGN DREF->* TO <L_DATA>.
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
        VALUE = '-CURR/QUANT REF DATA MISSING-'.
      CATCH CX_ROOT.
        VALUE = '-CONVERSION FAILED-'.                      "#EC NOTEXT
    ENDTRY.


  endmethod.


  method ON_NEW_FOCUS.

    DATA: LV_COLLECTION TYPE REF TO IF_BOL_BO_COL,
          ENTITY        TYPE REF TO CL_CRM_BOL_ENTITY.

*   get collection of dependent nodes
    ENTITY ?= FOCUS_BO.
    TRY.
        LV_COLLECTION = ENTITY->GET_RELATED_ENTITIES(
          IV_RELATION_NAME = 'BTItemProductExt' ).

      CATCH CX_CRM_GENIL_MODEL_ERROR.
*       should never happen
        EXIT.
      CATCH CX_SY_REF_IS_INITIAL.
    ENDTRY.
    ME->SET_COLLECTION( LV_COLLECTION ).


  endmethod.


  method SET_BASE_QTY_UNIT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'BASE_QTY_UNIT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'BASE_QTY_UNIT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'BASE_QTY_UNIT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_BATCH_ID.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'BATCH_ID' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'BATCH_ID'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'BATCH_ID' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_CASH_DISC.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'CASH_DISC' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'CASH_DISC'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'CASH_DISC' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_COMM_GROUP.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'COMM_GROUP' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'COMM_GROUP'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'COMM_GROUP' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_EXCH_BUSINESS.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'EXCH_BUSINESS' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'EXCH_BUSINESS'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'EXCH_BUSINESS' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_EXPONENT10.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'EXPONENT10' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'EXPONENT10'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'EXPONENT10' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_GC_SEND_TYPE.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'GC_SEND_TYPE' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'GC_SEND_TYPE'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'GC_SEND_TYPE' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_GROSS_WEIGHT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'GROSS_WEIGHT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'GROSS_WEIGHT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'GROSS_WEIGHT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_GUID.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'GUID' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'GUID'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'GUID' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_INT_INSPECTION.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'INT_INSPECTION' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'INT_INSPECTION'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'INT_INSPECTION' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_INT_OBJ_NO_BT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'INT_OBJ_NO_BT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'INT_OBJ_NO_BT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'INT_OBJ_NO_BT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_IS_GIFTCARD.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'IS_GIFTCARD' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'IS_GIFTCARD'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'IS_GIFTCARD' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_NET_WEIGHT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'NET_WEIGHT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'NET_WEIGHT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'NET_WEIGHT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PLANT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PLANT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PLANT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PLANT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PLANT_DESCRIPTION.
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
    dref = current->get_property( 'PLANT_DESCRIPTION' )."#EC NOTEXT
      CATCH cx_sy_ref_is_initial.
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
            name = 'PLANT_DESCRIPTION'."#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <nval> <> <oval>.

      current->set_property(
                      iv_attr_name = 'PLANT_DESCRIPTION' "#EC NOTEXT
                      iv_value     = <nval> ).

    ENDIF.

  endmethod.


  method SET_PRC_GROUP1.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP1' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PRC_GROUP1'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PRC_GROUP1' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PRC_GROUP2.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP2' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PRC_GROUP2'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PRC_GROUP2' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PRC_GROUP3.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP3' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PRC_GROUP3'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PRC_GROUP3' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PRC_GROUP4.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP4' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PRC_GROUP4'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PRC_GROUP4' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PRC_GROUP5.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRC_GROUP5' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PRC_GROUP5'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PRC_GROUP5' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PRICE_PRODUCT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRICE_PRODUCT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PRICE_PRODUCT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PRICE_PRODUCT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PROCESS_QTY_DEN.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROCESS_QTY_DEN' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PROCESS_QTY_DEN'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PROCESS_QTY_DEN' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PROCESS_QTY_NUM.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROCESS_QTY_NUM' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PROCESS_QTY_NUM'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PROCESS_QTY_NUM' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PROCESS_QTY_UNIT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROCESS_QTY_UNIT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PROCESS_QTY_UNIT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PROCESS_QTY_UNIT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PRODUCT_I_DUMMY.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRODUCT_I_DUMMY' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PRODUCT_I_DUMMY'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PRODUCT_I_DUMMY' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PRODUCT_SEGMENT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PRODUCT_SEGMENT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PRODUCT_SEGMENT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PRODUCT_SEGMENT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PROD_HIERARCHY.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROD_HIERARCHY' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PROD_HIERARCHY'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PROD_HIERARCHY' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_PROD_PR_GROUP.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'PROD_PR_GROUP' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'PROD_PR_GROUP'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'PROD_PR_GROUP' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_REBATE_GROUP.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'REBATE_GROUP' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'REBATE_GROUP'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'REBATE_GROUP' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_RESID_VALUE_GRP.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'RESID_VALUE_GRP' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'RESID_VALUE_GRP'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'RESID_VALUE_GRP' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_RESID_VALUE_ST.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'RESID_VALUE_ST' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'RESID_VALUE_ST'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'RESID_VALUE_ST' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_RET_PROC_IND.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'RET_PROC_IND' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'RET_PROC_IND'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'RET_PROC_IND' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_VOLUME.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'VOLUME' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'VOLUME'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'VOLUME' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_VOLUME_UNIT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'VOLUME_UNIT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'VOLUME_UNIT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'VOLUME_UNIT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.


  method SET_WEIGHT_UNIT.

    DATA:
      CURRENT TYPE REF TO IF_BOL_BO_PROPERTY_ACCESS,
      DREF    TYPE REF TO DATA,
      COPY    TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <NVAL> TYPE ANY,
      <OVAL> TYPE ANY.

*   get current entity
    IF ITERATOR IS BOUND.
      CURRENT = ITERATOR->GET_CURRENT( ).
    ELSE.
      CURRENT = COLLECTION_WRAPPER->GET_CURRENT( ).
    ENDIF.

*   get old value and dataref to appropriate type

    TRY.

        TRY.
            DREF = CURRENT->GET_PROPERTY( 'WEIGHT_UNIT' ). "#EC NOTEXT
          CATCH CX_CRM_CIC_PARAMETER_ERROR.
        ENDTRY.

      CATCH CX_SY_REF_IS_INITIAL CX_SY_MOVE_CAST_ERROR
            CX_CRM_GENIL_MODEL_ERROR.
        RETURN.
    ENDTRY.


*   assure that attribue exists
    CHECK DREF IS BOUND.

*   set <oval> to old value
    ASSIGN DREF->* TO <OVAL>.
*   create a copy for new value
    CREATE DATA COPY LIKE <OVAL>.
*   set <nval> to new value
    ASSIGN COPY->* TO <NVAL>.

*   fill new value using the right conversion
    TRY.
*        TRY.
        CALL METHOD IF_BSP_MODEL_UTIL~CONVERT_FROM_STRING
          EXPORTING
            DATA_REF       = COPY
            VALUE          = VALUE
            ATTRIBUTE_PATH = ATTRIBUTE_PATH.
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
      CATCH CX_SY_CONVERSION_ERROR.
        RAISE EXCEPTION TYPE CX_BSP_CONV_FAILED
          EXPORTING
            NAME = 'WEIGHT_UNIT'. "#EC NOTEXT
    ENDTRY.

*   only set new value if value has changed
    IF <NVAL> <> <OVAL>.

      CURRENT->SET_PROPERTY(
        IV_ATTR_NAME = 'WEIGHT_UNIT' "#EC NOTEXT
        IV_VALUE     = <NVAL> ).

    ENDIF.


  endmethod.
ENDCLASS.
