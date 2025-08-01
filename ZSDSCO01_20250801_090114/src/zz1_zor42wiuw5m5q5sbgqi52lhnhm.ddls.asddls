@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_4F57562320A6'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_ZOR42WIUW5M5Q5SBGQI52LHNHM
    association [0..1] to ZZ1_ZZPMT_V as _ZZ1_ZZPMT_APM
  on  $projection.ZZ1_ZZPMT_APM = _ZZ1_ZZPMT_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPMT_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZPMT_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZPMT_MSE
  end as ZZ1_ZZPMT_APM,
  _ZZ1_ZZPMT_APM
}
