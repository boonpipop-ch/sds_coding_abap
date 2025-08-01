@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_32CC88EC86FF'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_CLQBKELEZPZTHZKK4I2NGH3YW4
    association [0..1] to ZZ1_ZZPHA_V as _ZZ1_ZZPHA_APM
  on  $projection.ZZ1_ZZPHA_APM = _ZZ1_ZZPHA_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPHA_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZPHA_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZPHA_MSE
  end as ZZ1_ZZPHA_APM,
  _ZZ1_ZZPHA_APM
}
