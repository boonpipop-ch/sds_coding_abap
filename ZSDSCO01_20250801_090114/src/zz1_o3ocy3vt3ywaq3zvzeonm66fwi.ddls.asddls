@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_8CAF86E97AC2'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_O3OCY3VT3YWAQ3ZVZEONM66FWI
    association [0..1] to ZZ1_ITMTYPE_V as _ZZ1_ITMTYPE_APM
  on  $projection.ZZ1_ITMTYPE_APM = _ZZ1_ITMTYPE_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ITMTYPE_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ITMTYPE_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ITMTYPE_MSE
  end as ZZ1_ITMTYPE_APM,
  _ZZ1_ITMTYPE_APM
}
