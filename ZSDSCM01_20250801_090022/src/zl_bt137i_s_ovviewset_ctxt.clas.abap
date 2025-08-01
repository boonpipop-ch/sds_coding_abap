class ZL_BT137I_S_OVVIEWSET_CTXT definition
  public
  inheriting from CL_BT137I_S_OVVIEWSET_CTXT
  create public .

public section.
protected section.

  methods CREATE_CONTEXT_NODES
    redefinition .
  methods CONNECT_NODES
    redefinition .
private section.
ENDCLASS.



CLASS ZL_BT137I_S_OVVIEWSET_CTXT IMPLEMENTATION.


  method CONNECT_NODES.

    DATA: COLL_WRAPPER TYPE REF TO CL_BSP_WD_COLLECTION_WRAPPER.

    SUPER->CONNECT_NODES( IV_ACTIVATE ).


  endmethod.


  method CREATE_CONTEXT_NODES.


    SUPER->CREATE_CONTEXT_NODES( CONTROLLER ).


  endmethod.
ENDCLASS.
