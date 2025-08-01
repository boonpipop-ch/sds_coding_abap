@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_DE6FA3661747'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_ONDD75ZZOTSIOVGNFYZUTLWILQ
    association [0..1] to ZSDSVI_CUSTCLASS as _ZZ1_SHOPTYPE_APM
  on  $projection.ZZ1_SHOPTYPE_APM = _ZZ1_SHOPTYPE_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SHOPTYPE_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_SHOPTYPE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_SHOPTYPE
  end as ZZ1_SHOPTYPE_APM,
  _ZZ1_SHOPTYPE_APM
}
