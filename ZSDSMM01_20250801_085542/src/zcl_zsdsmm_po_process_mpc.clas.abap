class ZCL_ZSDSMM_PO_PROCESS_MPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  types:
  begin of TS_PODATA,
     PONO type string,
     POTYPE type string,
     VENDOR type string,
     SVNO type string,
     PORJNO type string,
     INVNO type string,
     INVDT type string,
     PR type string,
     MESSAGE type string,
     MESSAGETYPE type string,
  end of TS_PODATA .
  types:
TT_PODATA type standard table of TS_PODATA .
  types:
   begin of ts_text_element,
      artifact_name  type c length 40,       " technical name
      artifact_type  type c length 4,
      parent_artifact_name type c length 40, " technical name
      parent_artifact_type type c length 4,
      text_symbol    type textpoolky,
   end of ts_text_element .
  types:
         tt_text_elements type standard table of ts_text_element with key text_symbol .
  types:
     TS_POITEM type ZSDSMMS005 .
  types:
TT_POITEM type standard table of TS_POITEM .
  types:
  begin of TS_POUPDATESTATUS,
     PONO type string,
     STATUS type string,
     POSITION type string,
     ACTIONBY type string,
     ACTIONDATE type string,
     ACTIONTIME type string,
     FLAGCOMPLETED type string,
     REMARK type string,
     MESSAGE type string,
     MESSAGETYPE type string,
  end of TS_POUPDATESTATUS .
  types:
TT_POUPDATESTATUS type standard table of TS_POUPDATESTATUS .
  types:
     TS_PONEXTSTEPLIST type ZSDSMMS031 .
  types:
TT_PONEXTSTEPLIST type standard table of TS_PONEXTSTEPLIST .

  constants GC_PODATA type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PoData' ##NO_TEXT.
  constants GC_POITEM type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PoItem' ##NO_TEXT.
  constants GC_PONEXTSTEPLIST type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PoNextStepList' ##NO_TEXT.
  constants GC_POUPDATESTATUS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PoUpdateStatus' ##NO_TEXT.

  methods LOAD_TEXT_ELEMENTS
  final
    returning
      value(RT_TEXT_ELEMENTS) type TT_TEXT_ELEMENTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .

  methods DEFINE
    redefinition .
  methods GET_LAST_MODIFIED
    redefinition .
protected section.
private section.

  constants GC_INCL_NAME type STRING value 'ZCL_ZSDSMM_PO_PROCESS_MPC=====CP' ##NO_TEXT.

  methods DEFINE_PODATA
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods DEFINE_POITEM
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods DEFINE_POUPDATESTATUS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods DEFINE_PONEXTSTEPLIST
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods DEFINE_ASSOCIATIONS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
ENDCLASS.



CLASS ZCL_ZSDSMM_PO_PROCESS_MPC IMPLEMENTATION.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*

model->set_schema_namespace( 'ZSDSMM_PO_PROCESS_SRV' ).

define_podata( ).
define_poitem( ).
define_poupdatestatus( ).
define_ponextsteplist( ).
define_associations( ).
  endmethod.


  method DEFINE_ASSOCIATIONS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*




data:
lo_annotation     type ref to /iwbep/if_mgw_odata_annotation,                   "#EC NEEDED
lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,                   "#EC NEEDED
lo_association    type ref to /iwbep/if_mgw_odata_assoc,                        "#EC NEEDED
lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr,                   "#EC NEEDED
lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set,                    "#EC NEEDED
lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop.                     "#EC NEEDED

***********************************************************************************************************************************
*   ASSOCIATIONS
***********************************************************************************************************************************

 lo_association = model->create_association(
                            iv_association_name = 'PoDataXPoItem' "#EC NOTEXT
                            iv_left_type        = 'PoData' "#EC NOTEXT
                            iv_right_type       = 'PoItem' "#EC NOTEXT
                            iv_right_card       = 'N' "#EC NOTEXT
                            iv_left_card        = 'N'  "#EC NOTEXT
                            iv_def_assoc_set    = abap_true ). "#EC NOTEXT
* Referential constraint for association - PoDataXPoItem
lo_ref_constraint = lo_association->create_ref_constraint( ).
lo_ref_constraint->add_property( iv_principal_property = 'PoNo'   iv_dependent_property = 'PoNo' ). "#EC NOTEXT
 lo_association = model->create_association(
                            iv_association_name = 'PoUpdateStatusXPoNextStepList' "#EC NOTEXT
                            iv_left_type        = 'PoUpdateStatus' "#EC NOTEXT
                            iv_right_type       = 'PoNextStepList' "#EC NOTEXT
                            iv_right_card       = 'N' "#EC NOTEXT
                            iv_left_card        = 'N'  "#EC NOTEXT
                            iv_def_assoc_set    = abap_true ). "#EC NOTEXT

***********************************************************************************************************************************
*   NAVIGATION PROPERTIES
***********************************************************************************************************************************

* Navigation Properties for entity - PoData
lo_entity_type = model->get_entity_type( iv_entity_name = 'PoData' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'PoDetail' "#EC NOTEXT
                                                              iv_abap_fieldname = 'DETAIL' "#EC NOTEXT
                                                              iv_association_name = 'PoDataXPoItem' ). "#EC NOTEXT
* Navigation Properties for entity - PoUpdateStatus
lo_entity_type = model->get_entity_type( iv_entity_name = 'PoUpdateStatus' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'NextStepList' "#EC NOTEXT
                                                              iv_abap_fieldname = 'NEXTSTEPLIST' "#EC NOTEXT
                                                              iv_association_name = 'PoUpdateStatusXPoNextStepList' ). "#EC NOTEXT
  endmethod.


  method DEFINE_PODATA.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  data:
        lo_annotation     type ref to /iwbep/if_mgw_odata_annotation,                "#EC NEEDED
        lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,                "#EC NEEDED
        lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,                "#EC NEEDED
        lo_property       type ref to /iwbep/if_mgw_odata_property,                  "#EC NEEDED
        lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set.                "#EC NEEDED

***********************************************************************************************************************************
*   ENTITY - PoData
***********************************************************************************************************************************

lo_entity_type = model->create_entity_type( iv_entity_type_name = 'PoData' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'PoNo' iv_abap_fieldname = 'PONO' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PoType' iv_abap_fieldname = 'POTYPE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'vendor' iv_abap_fieldname = 'VENDOR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'svNo' iv_abap_fieldname = 'SVNO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'porjNo' iv_abap_fieldname = 'PORJNO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'invNo' iv_abap_fieldname = 'INVNO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'invDt' iv_abap_fieldname = 'INVDT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'pr' iv_abap_fieldname = 'PR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Message' iv_abap_fieldname = 'MESSAGE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'MessageType' iv_abap_fieldname = 'MESSAGETYPE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZSDSMM_PO_PROCESS_MPC=>TS_PODATA' ). "#EC NOTEXT


***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
lo_entity_set = lo_entity_type->create_entity_set( 'PoDataSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
  endmethod.


  method DEFINE_POITEM.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  data:
        lo_annotation     type ref to /iwbep/if_mgw_odata_annotation,                "#EC NEEDED
        lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,                "#EC NEEDED
        lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,                "#EC NEEDED
        lo_property       type ref to /iwbep/if_mgw_odata_property,                  "#EC NEEDED
        lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set.                "#EC NEEDED

***********************************************************************************************************************************
*   ENTITY - PoItem
***********************************************************************************************************************************

lo_entity_type = model->create_entity_type( iv_entity_type_name = 'PoItem' iv_def_entity_set = abap_true ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Serviceorder' iv_abap_fieldname = 'SERVICEORDER' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '005' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Budgetid' iv_abap_fieldname = 'BUDGETID' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '002' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Invno' iv_abap_fieldname = 'INVNO' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '006' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PoNo' iv_abap_fieldname = 'PONO' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Invdate' iv_abap_fieldname = 'INVDATE' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '007' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PoItemNo' iv_abap_fieldname = 'POITEMNO' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_decimal( ).
lo_property->set_maxlength( iv_max_length = 5 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ItemType' iv_abap_fieldname = 'ITEMTYPE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'MaterialCode' iv_abap_fieldname = 'MATERIALCODE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Description' iv_abap_fieldname = 'DESCRIPTION' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Qty' iv_abap_fieldname = 'QTY' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_maxlength( iv_max_length = 12 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BaseUnit' iv_abap_fieldname = 'BASEUNIT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Amount' iv_abap_fieldname = 'AMOUNT' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_precison( iv_precision = 2 ). "#EC NOTEXT
lo_property->set_maxlength( iv_max_length = 23 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).

lo_entity_type->bind_structure( iv_structure_name   = 'ZSDSMMS005'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT
  endmethod.


  method DEFINE_PONEXTSTEPLIST.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  data:
        lo_annotation     type ref to /iwbep/if_mgw_odata_annotation,                "#EC NEEDED
        lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,                "#EC NEEDED
        lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,                "#EC NEEDED
        lo_property       type ref to /iwbep/if_mgw_odata_property,                  "#EC NEEDED
        lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set.                "#EC NEEDED

***********************************************************************************************************************************
*   ENTITY - PoNextStepList
***********************************************************************************************************************************

lo_entity_type = model->create_entity_type( iv_entity_type_name = 'PoNextStepList' iv_def_entity_set = abap_true ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'NextStepActionBy' iv_abap_fieldname = 'NEXTSTEPACTIONBY' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '003' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'NextStepPosition' iv_abap_fieldname = 'NEXTSTEPPOSITION' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '004' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).

lo_entity_type->bind_structure( iv_structure_name   = 'ZSDSMMS031'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT
  endmethod.


  method DEFINE_POUPDATESTATUS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  data:
        lo_annotation     type ref to /iwbep/if_mgw_odata_annotation,                "#EC NEEDED
        lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,                "#EC NEEDED
        lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,                "#EC NEEDED
        lo_property       type ref to /iwbep/if_mgw_odata_property,                  "#EC NEEDED
        lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set.                "#EC NEEDED

***********************************************************************************************************************************
*   ENTITY - PoUpdateStatus
***********************************************************************************************************************************

lo_entity_type = model->create_entity_type( iv_entity_type_name = 'PoUpdateStatus' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'PoNo' iv_abap_fieldname = 'PONO' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Status' iv_abap_fieldname = 'STATUS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Position' iv_abap_fieldname = 'POSITION' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ActionBy' iv_abap_fieldname = 'ACTIONBY' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ActionDate' iv_abap_fieldname = 'ACTIONDATE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ActionTime' iv_abap_fieldname = 'ACTIONTIME' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'FlagCompleted' iv_abap_fieldname = 'FLAGCOMPLETED' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Remark' iv_abap_fieldname = 'REMARK' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Message' iv_abap_fieldname = 'MESSAGE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'MessageType' iv_abap_fieldname = 'MESSAGETYPE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZSDSMM_PO_PROCESS_MPC=>TS_POUPDATESTATUS' ). "#EC NOTEXT


***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
lo_entity_set = lo_entity_type->create_entity_set( 'PoUpdateStatusSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  CONSTANTS: lc_gen_date_time TYPE timestamp VALUE '20241118090934'.                  "#EC NOTEXT
  rv_last_modified = super->get_last_modified( ).
  IF rv_last_modified LT lc_gen_date_time.
    rv_last_modified = lc_gen_date_time.
  ENDIF.
  endmethod.


  method LOAD_TEXT_ELEMENTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


DATA:
     ls_text_element TYPE ts_text_element.                                 "#EC NEEDED


clear ls_text_element.
ls_text_element-artifact_name          = 'Serviceorder'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'PoItem'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '005'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Budgetid'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'PoItem'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '002'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Invno'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'PoItem'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '006'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Invdate'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'PoItem'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '007'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.


clear ls_text_element.
ls_text_element-artifact_name          = 'NextStepActionBy'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'PoNextStepList'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '003'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'NextStepPosition'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'PoNextStepList'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '004'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
  endmethod.
ENDCLASS.
