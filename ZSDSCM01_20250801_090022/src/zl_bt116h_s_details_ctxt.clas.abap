class ZL_BT116H_S_DETAILS_CTXT definition
  public
  inheriting from CL_BT116H_S_DETAILS_CTXT
  create public .

public section.
protected section.

  methods CONNECT_NODES
    redefinition .
  methods CREATE_CONTEXT_NODES
    redefinition .
  methods CREATE_CUSTOMERH
    redefinition .
private section.

  data ZCUSTOMERH type ref to ZL_BT116H_S_DETAILS_CN18 .
ENDCLASS.



CLASS ZL_BT116H_S_DETAILS_CTXT IMPLEMENTATION.


  method CONNECT_NODES.

    DATA: COLL_WRAPPER TYPE REF TO CL_BSP_WD_COLLECTION_WRAPPER.

    SUPER->CONNECT_NODES( IV_ACTIVATE ).


  endmethod.


  method CREATE_CONTEXT_NODES.


    SUPER->CREATE_CONTEXT_NODES( CONTROLLER ).


  endmethod.


  method CREATE_CUSTOMERH.
    DATA:
      model        TYPE REF TO if_bsp_model,
      coll_wrapper TYPE REF TO cl_bsp_wd_collection_wrapper,
      entity       TYPE REF TO cl_crm_bol_entity,    "#EC *
      entity_col   TYPE REF TO if_bol_entity_col.    "#EC *

    model = owner->create_model(

        class_name     = 'ZL_BT116H_S_DETAILS_CN18'

        model_id       = 'CustomerH' ). "#EC NOTEXT
    CustomerH ?= model.
    CLEAR model.

* initial setting for depandant model node
    coll_wrapper =
      BTADMINH->get_collection_wrapper( ).
    TRY.
        entity ?= coll_wrapper->get_current( ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.
    IF entity IS BOUND.
      CustomerH->on_new_focus(
                   focus_bo = entity ).
    ENDIF.
  endmethod.
ENDCLASS.
