class ZCL_IM_SDS_MATERIAL_REF definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MATERIAL_REFERENCE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_MATERIAL_REF IMPLEMENTATION.


  METHOD IF_EX_MATERIAL_REFERENCE~CREATE_MATERIAL.

    IF SY-SUBRC EQ 0.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
