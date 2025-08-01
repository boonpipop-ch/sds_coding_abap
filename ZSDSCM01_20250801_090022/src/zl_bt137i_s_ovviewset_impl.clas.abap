class ZL_BT137I_S_OVVIEWSET_IMPL definition
  public
  inheriting from CL_BT137I_S_OVVIEWSET_IMPL
  create public .

public section.
protected section.

  data ZTYPED_CONTEXT type ref to ZL_BT137I_S_OVVIEWSET_CTXT .

  methods DO_HANDLE_EVENT
    redefinition .
  methods WD_CREATE_CONTEXT
    redefinition .
private section.
ENDCLASS.



CLASS ZL_BT137I_S_OVVIEWSET_IMPL IMPLEMENTATION.


  method DO_HANDLE_EVENT.

* Eventhandler dispatching
    CASE HTMLB_EVENT_EX->EVENT_SERVER_NAME.
      WHEN OTHERS.

        GLOBAL_EVENT = SUPER->DO_HANDLE_EVENT( EVENT           = EVENT
                                               HTMLB_EVENT     = HTMLB_EVENT
                                               HTMLB_EVENT_EX  = HTMLB_EVENT_EX
                                               GLOBAL_MESSAGES = GLOBAL_MESSAGES ).

    ENDCASE.


  endmethod.


  method WD_CREATE_CONTEXT.
*   create the context
    context = cl_bsp_wd_context=>get_instance(
          iv_controller = me
          iv_type = 'ZL_BT137I_S_OVVIEWSET_CTXT' ).

    typed_context ?= context.

* Added by wizard
   ztyped_context ?= context.
  endmethod.
ENDCLASS.
