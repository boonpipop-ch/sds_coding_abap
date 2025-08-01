class ZCL_ZSDSSD_DO_SIGNED_MPC_EXT definition
  public
  inheriting from ZCL_ZSDSSD_DO_SIGNED_MPC
  create public .

public section.
    TYPES:
      BEGIN OF TS_DEEP_ENTITY,
        UPDATE_KEY    TYPE CHAR01,
        UPDATEXDATA   TYPE ZSDSSDT024_TT,
      END OF TS_DEEP_ENTITY.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZSDSSD_DO_SIGNED_MPC_EXT IMPLEMENTATION.


  METHOD DEFINE.
    SUPER->DEFINE( ).
    DATA:
      LO_ANNOTATION   TYPE REF TO /IWBEP/IF_MGW_ODATA_ANNOTATION,
      LO_ENTITY_TYPE  TYPE REF TO /IWBEP/IF_MGW_ODATA_ENTITY_TYP,
      LO_COMPLEX_TYPE TYPE REF TO /IWBEP/IF_MGW_ODATA_CMPLX_TYPE,
      LO_PROPERTY     TYPE REF TO /IWBEP/IF_MGW_ODATA_PROPERTY,
      LO_ENTITY_SET   TYPE REF TO /IWBEP/IF_MGW_ODATA_ENTITY_SET.

***********************************************************************************************************************************
*   ENTITY - Deep Entity
***********************************************************************************************************************************

    LO_ENTITY_TYPE = MODEL->GET_ENTITY_TYPE( IV_ENTITY_NAME = 'update' ). "#EC NOTEXT

    LO_ENTITY_TYPE->BIND_STRUCTURE( IV_STRUCTURE_NAME  = 'ZCL_ZSDSSD_DO_SIGNED_MPC_EXT=>TS_DEEP_ENTITY' ). "#EC NOTEXT
  ENDMETHOD.
ENDCLASS.
