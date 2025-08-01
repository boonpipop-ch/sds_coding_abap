CLASS ZCL_ZSDSFI_BP_PROCESS_MPC_EXT DEFINITION
  PUBLIC
  INHERITING FROM ZCL_ZSDSFI_BP_PROCESS_MPC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF TS_DEEP_ENTITY,
        ACTION            TYPE CHAR1,
        BPARTNER          TYPE BU_PARTNER,
        PARTN_CAT         TYPE BU_TYPE,
        BU_GROUP          TYPE BU_GROUP,
        K2_REFNO          TYPE CHAR20,
        SFDC_REFNO        TYPE CHAR20,
        TEST              TYPE CHAR1,
        CREATED_BY        TYPE PERSNO,
        GENERALXCENTRAL   TYPE ZSDSFIS003_TT,
        GENERALXADDRESS   TYPE ZSDSFIS010_TT,
        GENERALXCUSTOMER  TYPE ZSDSFIS078_TT,
        GENERALXROLE      TYPE ZSDSFIS089_TT,
        GENERALXCONTACT   TYPE ZSDSFIS082_TT,
        GENERALXPARTNER   TYPE ZSDSFIS083_TT,
        GENERALXVENDOR    TYPE ZSDSFIS084_TT,
        GENERALXRETURN    TYPE BAPIRET2_T,
        BLOCKPROCESSING   TYPE ZSDSFIS116_TT,
        SHIP_TO           TYPE CHAR1,
        CONTACT_PERSON    TYPE CHAR1,
        BP_DELETE         TYPE CHAR1,
        GENERALXSALESAREA TYPE ZSDSSDS123_TT,
      END OF TS_DEEP_ENTITY.

    METHODS DEFINE
        REDEFINITION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZSDSFI_BP_PROCESS_MPC_EXT IMPLEMENTATION.


  METHOD define.

    super->define( ).
    DATA:
      lo_annotation   TYPE REF TO /iwbep/if_mgw_odata_annotation,
      lo_entity_type  TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
      lo_complex_type TYPE REF TO /iwbep/if_mgw_odata_cmplx_type,
      lo_property     TYPE REF TO /iwbep/if_mgw_odata_property,
      lo_entity_set   TYPE REF TO /iwbep/if_mgw_odata_entity_set.

***********************************************************************************************************************************
*   ENTITY - Deep Entity
***********************************************************************************************************************************

    lo_entity_type = model->get_entity_type( iv_entity_name = 'general' ). "#EC NOTEXT

    lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZSDSFI_BP_PROCESS_MPC_EXT=>TS_DEEP_ENTITY' ). "#EC NOTEXT

  ENDMETHOD.
ENDCLASS.
