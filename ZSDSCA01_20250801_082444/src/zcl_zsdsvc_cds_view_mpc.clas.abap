class ZCL_ZSDSVC_CDS_VIEW_MPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  interfaces IF_SADL_GW_MODEL_EXPOSURE_DATA .

  types:
      ESHEADER type ZSDSSDS129 .
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
  begin of TS_GETSOHEADER,
     ES_HEADER type ESHEADER,
     IV_VBELN type VBELN_VA,
  end of TS_GETSOHEADER .
  types:
TT_GETSOHEADER type standard table of TS_GETSOHEADER .
  types:
  begin of TS_GETDODATA,
     DO_NUM type C length 10,
     DO_ITM type C length 6,
     DELIVERY_DATE type C length 10,
     MATERIAL type C length 40,
     MAT_GRP type C length 9,
     LAND_NO type C length 255,
     SERNR type C length 18,
  end of TS_GETDODATA .
  types:
TT_GETDODATA type standard table of TS_GETDODATA .
  types:
    begin of TS_ZSDSVC_ACC_CLERKTYPE.
      include type ZSDSVC_ACC_CLERK.
  types:
    end of TS_ZSDSVC_ACC_CLERKTYPE .
  types:
   TT_ZSDSVC_ACC_CLERKTYPE type standard table of TS_ZSDSVC_ACC_CLERKTYPE .
  types:
    begin of TS_ZSDSVC_BNKATYPE.
      include type ZSDSVC_BNKA.
  types:
    end of TS_ZSDSVC_BNKATYPE .
  types:
   TT_ZSDSVC_BNKATYPE type standard table of TS_ZSDSVC_BNKATYPE .
  types:
    begin of TS_ZSDSVC_BP_LISTTYPE.
      include type ZSDSVC_BP_LIST.
  types:
    end of TS_ZSDSVC_BP_LISTTYPE .
  types:
   TT_ZSDSVC_BP_LISTTYPE type standard table of TS_ZSDSVC_BP_LISTTYPE .
  types:
    begin of TS_ZSDSVC_DO_GETTYPE.
      include type ZSDSVC_DO_GET.
  types:
    end of TS_ZSDSVC_DO_GETTYPE .
  types:
   TT_ZSDSVC_DO_GETTYPE type standard table of TS_ZSDSVC_DO_GETTYPE .
  types:
    begin of TS_ZSDSVC_DUNNINGTYPE.
      include type ZSDSVC_DUNNING.
  types:
    end of TS_ZSDSVC_DUNNINGTYPE .
  types:
   TT_ZSDSVC_DUNNINGTYPE type standard table of TS_ZSDSVC_DUNNINGTYPE .
  types:
    begin of TS_ZSDSVC_GET_CLRDOCTYPE.
      include type ZSDSVC_GET_CLRDOC.
  types:
    end of TS_ZSDSVC_GET_CLRDOCTYPE .
  types:
   TT_ZSDSVC_GET_CLRDOCTYPE type standard table of TS_ZSDSVC_GET_CLRDOCTYPE .
  types:
    begin of TS_ZSDSVC_INCOTYPE.
      include type ZSDSVC_INCO.
  types:
    end of TS_ZSDSVC_INCOTYPE .
  types:
   TT_ZSDSVC_INCOTYPE type standard table of TS_ZSDSVC_INCOTYPE .
  types:
    begin of TS_ZSDSVC_IO_TRANSACTIONTYPE.
      include type ZSDSVC_IO_TRANSACTION.
  types:
    end of TS_ZSDSVC_IO_TRANSACTIONTYPE .
  types:
   TT_ZSDSVC_IO_TRANSACTIONTYPE type standard table of TS_ZSDSVC_IO_TRANSACTIONTYPE .
  types:
    begin of TS_ZSDSVC_MAT_LISTTYPE.
      include type ZSDSVC_MAT_LIST.
  types:
    end of TS_ZSDSVC_MAT_LISTTYPE .
  types:
   TT_ZSDSVC_MAT_LISTTYPE type standard table of TS_ZSDSVC_MAT_LISTTYPE .
  types:
    begin of TS_ZSDSVC_PROJ_COTYPE.
      include type ZSDSVC_PROJ_CO.
  types:
    end of TS_ZSDSVC_PROJ_COTYPE .
  types:
   TT_ZSDSVC_PROJ_COTYPE type standard table of TS_ZSDSVC_PROJ_COTYPE .
  types:
    begin of TS_ZSDSVC_SALES_GRPTYPE.
      include type ZSDSVC_SALES_GRP.
  types:
    end of TS_ZSDSVC_SALES_GRPTYPE .
  types:
   TT_ZSDSVC_SALES_GRPTYPE type standard table of TS_ZSDSVC_SALES_GRPTYPE .
  types:
    begin of TS_ZSDSVC_SALES_OFFICETYPE.
      include type ZSDSVC_SALES_OFFICE.
  types:
    end of TS_ZSDSVC_SALES_OFFICETYPE .
  types:
   TT_ZSDSVC_SALES_OFFICETYPE type standard table of TS_ZSDSVC_SALES_OFFICETYPE .
  types:
    begin of TS_ZSDSVC_SALES_RESULTPARAMETE.
      include type ZSDSVC_SALES_RESULT.
  types:
      P_VKORG type VKORG,
    end of TS_ZSDSVC_SALES_RESULTPARAMETE .
  types:
   TT_ZSDSVC_SALES_RESULTPARAMETE type standard table of TS_ZSDSVC_SALES_RESULTPARAMETE .
  types:
    begin of TS_ZSDSVC_SALES_RESULTTYPE.
      include type ZSDSVC_SALES_RESULT.
  types:
      P_VKORG type VKORG,
    end of TS_ZSDSVC_SALES_RESULTTYPE .
  types:
   TT_ZSDSVC_SALES_RESULTTYPE type standard table of TS_ZSDSVC_SALES_RESULTTYPE .
  types:
    begin of TS_ZSDSVC_SEARCH_SOTYPE.
      include type ZSDSVC_SEARCH_SO.
  types:
    end of TS_ZSDSVC_SEARCH_SOTYPE .
  types:
   TT_ZSDSVC_SEARCH_SOTYPE type standard table of TS_ZSDSVC_SEARCH_SOTYPE .
  types:
    begin of TS_ZSDSVC_SHIP_CONDTYPE.
      include type ZSDSVC_SHIP_COND.
  types:
    end of TS_ZSDSVC_SHIP_CONDTYPE .
  types:
   TT_ZSDSVC_SHIP_CONDTYPE type standard table of TS_ZSDSVC_SHIP_CONDTYPE .
  types:
    begin of TS_ZSDSVC_SORT_KEYTYPE.
      include type ZSDSVC_SORT_KEY.
  types:
    end of TS_ZSDSVC_SORT_KEYTYPE .
  types:
   TT_ZSDSVC_SORT_KEYTYPE type standard table of TS_ZSDSVC_SORT_KEYTYPE .
  types:
    begin of TS_ZSDSVC_T005TTYPE.
      include type ZSDSVC_T005T.
  types:
    end of TS_ZSDSVC_T005TTYPE .
  types:
   TT_ZSDSVC_T005TTYPE type standard table of TS_ZSDSVC_T005TTYPE .
  types:
    begin of TS_ZSDSVC_T880TYPE.
      include type ZSDSVC_T880.
  types:
    end of TS_ZSDSVC_T880TYPE .
  types:
   TT_ZSDSVC_T880TYPE type standard table of TS_ZSDSVC_T880TYPE .
  types:
    begin of TS_ZSDSVC_TAX_CLASSTYPE.
      include type ZSDSVC_TAX_CLASS.
  types:
    end of TS_ZSDSVC_TAX_CLASSTYPE .
  types:
   TT_ZSDSVC_TAX_CLASSTYPE type standard table of TS_ZSDSVC_TAX_CLASSTYPE .
  types:
    begin of TS_ZSDSVC_TB003TYPE.
      include type ZSDSVC_TB003.
  types:
    end of TS_ZSDSVC_TB003TYPE .
  types:
   TT_ZSDSVC_TB003TYPE type standard table of TS_ZSDSVC_TB003TYPE .
  types:
    begin of TS_ZSDSVC_TMP_PROJTYPE.
      include type ZSDSVC_TMP_PROJ.
  types:
    end of TS_ZSDSVC_TMP_PROJTYPE .
  types:
   TT_ZSDSVC_TMP_PROJTYPE type standard table of TS_ZSDSVC_TMP_PROJTYPE .
  types:
    begin of TS_ZSDSVC_TVV1TYPE.
      include type ZSDSVC_TVV1.
  types:
    end of TS_ZSDSVC_TVV1TYPE .
  types:
   TT_ZSDSVC_TVV1TYPE type standard table of TS_ZSDSVC_TVV1TYPE .
  types:
    begin of TS_ZSDSVC_TVV2TYPE.
      include type ZSDSVC_TVV2.
  types:
    end of TS_ZSDSVC_TVV2TYPE .
  types:
   TT_ZSDSVC_TVV2TYPE type standard table of TS_ZSDSVC_TVV2TYPE .
  types:
    begin of TS_ZSDSVC_WBSTYPE.
      include type ZSDSVC_WBS.
  types:
    end of TS_ZSDSVC_WBSTYPE .
  types:
   TT_ZSDSVC_WBSTYPE type standard table of TS_ZSDSVC_WBSTYPE .
  types:
    begin of TS_ZSDSVC_WBS_L05TYPE.
      include type ZSDSVC_WBS_L05.
  types:
    end of TS_ZSDSVC_WBS_L05TYPE .
  types:
   TT_ZSDSVC_WBS_L05TYPE type standard table of TS_ZSDSVC_WBS_L05TYPE .
  types:
    begin of TS_ZSDSVC_WL_INVTYPE.
      include type ZSDSVC_WL_INV.
  types:
    end of TS_ZSDSVC_WL_INVTYPE .
  types:
   TT_ZSDSVC_WL_INVTYPE type standard table of TS_ZSDSVC_WL_INVTYPE .
  types:
    begin of TS_ZSDSVC_ZSDSFIT032TYPE.
      include type ZSDSVC_ZSDSFIT032.
  types:
    end of TS_ZSDSVC_ZSDSFIT032TYPE .
  types:
   TT_ZSDSVC_ZSDSFIT032TYPE type standard table of TS_ZSDSVC_ZSDSFIT032TYPE .

  constants GC_ZSDSVC_ZSDSFIT032TYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_ZSDSFIT032Type' ##NO_TEXT.
  constants GC_ZSDSVC_WL_INVTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_WL_INVType' ##NO_TEXT.
  constants GC_ZSDSVC_WBS_L05TYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_WBS_L05Type' ##NO_TEXT.
  constants GC_ZSDSVC_WBSTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_WBSType' ##NO_TEXT.
  constants GC_ZSDSVC_TVV2TYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_TVV2Type' ##NO_TEXT.
  constants GC_ZSDSVC_TVV1TYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_TVV1Type' ##NO_TEXT.
  constants GC_ZSDSVC_TMP_PROJTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_TMP_PROJType' ##NO_TEXT.
  constants GC_ZSDSVC_TB003TYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_TB003Type' ##NO_TEXT.
  constants GC_ZSDSVC_TAX_CLASSTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_TAX_CLASSType' ##NO_TEXT.
  constants GC_ZSDSVC_T880TYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_T880Type' ##NO_TEXT.
  constants GC_ZSDSVC_T005TTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_T005TType' ##NO_TEXT.
  constants GC_ZSDSVC_SORT_KEYTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_SORT_KEYType' ##NO_TEXT.
  constants GC_ZSDSVC_SHIP_CONDTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_SHIP_CONDType' ##NO_TEXT.
  constants GC_ZSDSVC_SEARCH_SOTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_SEARCH_SOType' ##NO_TEXT.
  constants GC_ZSDSVC_SALES_RESULTTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_SALES_RESULTType' ##NO_TEXT.
  constants GC_ZSDSVC_SALES_RESULTPARAMETE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_SALES_RESULTParameters' ##NO_TEXT.
  constants GC_ZSDSVC_SALES_OFFICETYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_SALES_OFFICEType' ##NO_TEXT.
  constants GC_ZSDSVC_SALES_GRPTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_SALES_GRPType' ##NO_TEXT.
  constants GC_ZSDSVC_PROJ_COTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_PROJ_COType' ##NO_TEXT.
  constants GC_ZSDSVC_MAT_LISTTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_MAT_LISTType' ##NO_TEXT.
  constants GC_ZSDSVC_IO_TRANSACTIONTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_IO_TRANSACTIONType' ##NO_TEXT.
  constants GC_ZSDSVC_INCOTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_INCOType' ##NO_TEXT.
  constants GC_ZSDSVC_GET_CLRDOCTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_GET_CLRDOCType' ##NO_TEXT.
  constants GC_ZSDSVC_DUNNINGTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_DUNNINGType' ##NO_TEXT.
  constants GC_ZSDSVC_DO_GETTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_DO_GETType' ##NO_TEXT.
  constants GC_ZSDSVC_BP_LISTTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_BP_LISTType' ##NO_TEXT.
  constants GC_ZSDSVC_BNKATYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_BNKAType' ##NO_TEXT.
  constants GC_ZSDSVC_ACC_CLERKTYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ZSDSVC_ACC_CLERKType' ##NO_TEXT.
  constants GC_GETSOHEADER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'getSOHeader' ##NO_TEXT.
  constants GC_GETDODATA type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'getDOData' ##NO_TEXT.
  constants GC_ESHEADER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'EsHeader' ##NO_TEXT.

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

  constants GC_INCL_NAME type STRING value 'ZCL_ZSDSVC_CDS_VIEW_MPC=======CP' ##NO_TEXT.

  methods DEFINE_COMPLEXTYPES
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods DEFINE_GETSOHEADER
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods DEFINE_GETDODATA
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods DEFINE_RDS_4
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods GET_LAST_MODIFIED_RDS_4
    returning
      value(RV_LAST_MODIFIED_RDS) type TIMESTAMP .
ENDCLASS.



CLASS ZCL_ZSDSVC_CDS_VIEW_MPC IMPLEMENTATION.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*

model->set_schema_namespace( 'ZSDSVC_CDS_VIEW_SRV' ).

define_complextypes( ).
define_getsoheader( ).
define_getdodata( ).
define_rds_4( ).
get_last_modified_rds_4( ).
  endmethod.


  method DEFINE_RDS_4.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*
*   This code is generated for Reference Data Source
*   4
*&---------------------------------------------------------------------*
    TRY.
        if_sadl_gw_model_exposure_data~get_model_exposure( )->expose( model )->expose_vocabulary( vocab_anno_model ).
      CATCH cx_sadl_exposure_error INTO DATA(lx_sadl_exposure_error).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_med_exception
          EXPORTING
            previous = lx_sadl_exposure_error.
    ENDTRY.
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  CONSTANTS: lc_gen_date_time TYPE timestamp VALUE '20250722040014'.                  "#EC NOTEXT
 DATA: lv_rds_last_modified TYPE timestamp .
  rv_last_modified = super->get_last_modified( ).
  IF rv_last_modified LT lc_gen_date_time.
    rv_last_modified = lc_gen_date_time.
  ENDIF.
 lv_rds_last_modified =  GET_LAST_MODIFIED_RDS_4( ).
 IF rv_last_modified LT lv_rds_last_modified.
 rv_last_modified  = lv_rds_last_modified .
 ENDIF .
  endmethod.


  method GET_LAST_MODIFIED_RDS_4.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*
*   This code is generated for Reference Data Source
*   4
*&---------------------------------------------------------------------*
*    @@TYPE_SWITCH:
    CONSTANTS: co_gen_date_time TYPE timestamp VALUE '20250722040015'.
    TRY.
        rv_last_modified_rds = CAST cl_sadl_gw_model_exposure( if_sadl_gw_model_exposure_data~get_model_exposure( ) )->get_last_modified( ).
      CATCH cx_root ##CATCH_ALL.
        rv_last_modified_rds = co_gen_date_time.
    ENDTRY.
    IF rv_last_modified_rds < co_gen_date_time.
      rv_last_modified_rds = co_gen_date_time.
    ENDIF.
  endmethod.


  method IF_SADL_GW_MODEL_EXPOSURE_DATA~GET_MODEL_EXPOSURE.
    CONSTANTS: co_gen_timestamp TYPE timestamp VALUE '20250722040015'.
    DATA(lv_sadl_xml) =
               |<?xml version="1.0" encoding="utf-16"?>|  &
               |<sadl:definition xmlns:sadl="http://sap.com/sap.nw.f.sadl" syntaxVersion="V2" >|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_ACC_CLERK" binding="ZSDSVC_ACC_CLERK" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_PROJ_CO" binding="ZSDSVC_PROJ_CO" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_SALES_GRP" binding="ZSDSVC_SALES_GRP" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_SALES_OFFICE" binding="ZSDSVC_SALES_OFFICE" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_SALES_RESULT" binding="ZSDSVC_SALES_RESULT" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_SEARCH_SO" binding="ZSDSVC_SEARCH_SO" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_SHIP_COND" binding="ZSDSVC_SHIP_COND" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_BNKA" binding="ZSDSVC_BNKA" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_SORT_KEY" binding="ZSDSVC_SORT_KEY" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_T005T" binding="ZSDSVC_T005T" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_T880" binding="ZSDSVC_T880" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_GET_CLRDOC" binding="ZSDSVC_GET_CLRDOC" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_TAX_CLASS" binding="ZSDSVC_TAX_CLASS" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_TB003" binding="ZSDSVC_TB003" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_TMP_PROJ" binding="ZSDSVC_TMP_PROJ" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_TVV1" binding="ZSDSVC_TVV1" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_TVV2" binding="ZSDSVC_TVV2" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_WBS" binding="ZSDSVC_WBS" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_WBS_L05" binding="ZSDSVC_WBS_L05" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_BP_LIST" binding="ZSDSVC_BP_LIST" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_WL_INV" binding="ZSDSVC_WL_INV" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_ZSDSFIT032" binding="ZSDSVC_ZSDSFIT032" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_INCO" binding="ZSDSVC_INCO" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_DO_GET" binding="ZSDSVC_DO_GET" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_DUNNING" binding="ZSDSVC_DUNNING" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_IO_TRANSACTION" binding="ZSDSVC_IO_TRANSACTION" />|  &
               | <sadl:dataSource type="CDS" name="ZSDSVC_MAT_LIST" binding="ZSDSVC_MAT_LIST" />|  &
               |<sadl:resultSet>|  &
               |<sadl:structure name="ZSDSVC_ACC_CLERK" dataSource="ZSDSVC_ACC_CLERK" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_PROJ_CO" dataSource="ZSDSVC_PROJ_CO" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_SALES_GRP" dataSource="ZSDSVC_SALES_GRP" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_SALES_OFFICE" dataSource="ZSDSVC_SALES_OFFICE" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_SALES_RESULTSet" dataSource="ZSDSVC_SALES_RESULT" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>| .
      lv_sadl_xml = |{ lv_sadl_xml }| &
               |<sadl:structure name="ZSDSVC_SEARCH_SO" dataSource="ZSDSVC_SEARCH_SO" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_SHIP_COND" dataSource="ZSDSVC_SHIP_COND" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_BNKA" dataSource="ZSDSVC_BNKA" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_SORT_KEY" dataSource="ZSDSVC_SORT_KEY" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_T005T" dataSource="ZSDSVC_T005T" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_T880" dataSource="ZSDSVC_T880" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_GET_CLRDOC" dataSource="ZSDSVC_GET_CLRDOC" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_TAX_CLASS" dataSource="ZSDSVC_TAX_CLASS" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_TB003" dataSource="ZSDSVC_TB003" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_TMP_PROJ" dataSource="ZSDSVC_TMP_PROJ" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_TVV1" dataSource="ZSDSVC_TVV1" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_TVV2" dataSource="ZSDSVC_TVV2" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_WBS" dataSource="ZSDSVC_WBS" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">| .
      lv_sadl_xml = |{ lv_sadl_xml }| &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_WBS_L05" dataSource="ZSDSVC_WBS_L05" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_BP_LIST" dataSource="ZSDSVC_BP_LIST" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_WL_INV" dataSource="ZSDSVC_WL_INV" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_ZSDSFIT032" dataSource="ZSDSVC_ZSDSFIT032" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_INCO" dataSource="ZSDSVC_INCO" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_DO_GET" dataSource="ZSDSVC_DO_GET" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_DUNNING" dataSource="ZSDSVC_DUNNING" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_IO_TRANSACTION" dataSource="ZSDSVC_IO_TRANSACTION" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |<sadl:structure name="ZSDSVC_MAT_LIST" dataSource="ZSDSVC_MAT_LIST" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |</sadl:resultSet>|  &
               |</sadl:definition>| .

   ro_model_exposure = cl_sadl_gw_model_exposure=>get_exposure_xml( iv_uuid      = CONV #( 'ZSDSVC_CDS_VIEW' )
                                                                    iv_timestamp = co_gen_timestamp
                                                                    iv_sadl_xml  = lv_sadl_xml ).
  endmethod.


  method DEFINE_COMPLEXTYPES.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


 data:
       lo_annotation     type ref to /iwbep/if_mgw_odata_annotation,             "#EC NEEDED
       lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,             "#EC NEEDED
       lo_property       type ref to /iwbep/if_mgw_odata_property.                "#EC NEEDED

***********************************************************************************************************************************
*   COMPLEX TYPE - EsHeader
***********************************************************************************************************************************
lo_complex_type = model->create_complex_type( 'EsHeader' ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************
lo_property = lo_complex_type->create_property( iv_property_name  = 'Psphi' iv_abap_fieldname = 'PSPHI' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '004' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 8 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_complex_type->create_property( iv_property_name  = 'Purchaseorder' iv_abap_fieldname = 'PURCHASEORDER' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '003' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 20 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_complex_type->create_property( iv_property_name  = 'Salesorder' iv_abap_fieldname = 'SALESORDER' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_complex_type->create_property( iv_property_name  = 'Wbs' iv_abap_fieldname = 'WBS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 24 ).
lo_property->set_conversion_exit( 'ABPSN' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_complex_type->bind_structure( iv_structure_name   = 'ZSDSSDS129'
                                 iv_bind_conversions = 'X' ). "#EC NOTEXT
  endmethod.


  method DEFINE_GETDODATA.
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
*   ENTITY - getDOData
***********************************************************************************************************************************

lo_entity_type = model->create_entity_type( iv_entity_type_name = 'getDOData' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'DoNum' iv_abap_fieldname = 'DO_NUM' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '005' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'DoItm' iv_abap_fieldname = 'DO_ITM' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '006' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 6 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'DeliveryDate' iv_abap_fieldname = 'DELIVERY_DATE' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '007' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Material' iv_abap_fieldname = 'MATERIAL' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '008' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'MatGrp' iv_abap_fieldname = 'MAT_GRP' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '009' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 9 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'LandNo' iv_abap_fieldname = 'LAND_NO' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '010' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 255 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Sernr' iv_abap_fieldname = 'SERNR' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '011' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZSDSVC_CDS_VIEW_MPC=>TS_GETDODATA' ). "#EC NOTEXT


***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
lo_entity_set = lo_entity_type->create_entity_set( 'getDODataSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
  endmethod.


  method DEFINE_GETSOHEADER.
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
*   ENTITY - getSOHeader
***********************************************************************************************************************************

lo_entity_type = model->create_entity_type( iv_entity_type_name = 'getSOHeader' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_complex_type = lo_entity_type->create_complex_property( iv_property_name = 'header'
                                                           iv_complex_type_name = 'EsHeader'
                                                           iv_abap_fieldname    = 'ES_HEADER' ). "#EC NOTEXT
lo_property = lo_entity_type->create_property( iv_property_name = 'salesOrder' iv_abap_fieldname = 'IV_VBELN' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '002' iv_text_element_container = gc_incl_name ).  "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ). "#EC NOTEXT
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' )->add(
      EXPORTING
        iv_key      = 'unicode'
        iv_value    = 'false' ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZSDSVC_CDS_VIEW_MPC=>TS_GETSOHEADER' ). "#EC NOTEXT


***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
lo_entity_set = lo_entity_type->create_entity_set( 'getSOHeaderSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
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
ls_text_element-artifact_name          = 'salesOrder'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'getSOHeader'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '002'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.


clear ls_text_element.
ls_text_element-artifact_name          = 'DoNum'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'getDOData'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '005'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'DoItm'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'getDOData'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '006'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'DeliveryDate'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'getDOData'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '007'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Material'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'getDOData'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '008'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'MatGrp'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'getDOData'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '009'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'LandNo'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'getDOData'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '010'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Sernr'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'getDOData'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                                       "#EC NOTEXT
ls_text_element-text_symbol            = '011'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.


clear ls_text_element.
ls_text_element-artifact_name          = 'Psphi'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                           "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'EsHeader'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'CTYP'.                                           "#EC NOTEXT
ls_text_element-text_symbol            = '004'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Purchaseorder'.                 "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                                           "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'EsHeader'.                            "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'CTYP'.                                           "#EC NOTEXT
ls_text_element-text_symbol            = '003'.              "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
  endmethod.
ENDCLASS.
