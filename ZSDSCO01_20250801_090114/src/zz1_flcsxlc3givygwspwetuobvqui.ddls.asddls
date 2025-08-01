@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_4640C141EA15'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_FLCSXLC3GIVYGWSPWETUOBVQUI
    association [0..1] to ZZ1_FISCYR_V as _ZZ1_FISCYR_APM
  on  $projection.ZZ1_FISCYR_APM = _ZZ1_FISCYR_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_FISCYR_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_FISCYR_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_FISCYR_MSE
  end as ZZ1_FISCYR_APM,
  _ZZ1_FISCYR_APM
}
