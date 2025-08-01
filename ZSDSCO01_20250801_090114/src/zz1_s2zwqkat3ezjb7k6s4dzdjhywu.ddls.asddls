@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_35876DC384DE'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_S2ZWQKAT3EZJB7K6S4DZDJHYWU
    association [0..1] to ZZ1_ZZIUT_V as _ZZ1_ZZIUT_APM
  on  $projection.ZZ1_ZZIUT_APM = _ZZ1_ZZIUT_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZIUT_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZIUT_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZIUT_MSE
  end as ZZ1_ZZIUT_APM,
  _ZZ1_ZZIUT_APM
}
