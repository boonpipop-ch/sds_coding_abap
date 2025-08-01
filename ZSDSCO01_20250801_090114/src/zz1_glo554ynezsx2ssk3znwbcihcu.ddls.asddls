@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_372993ACD37D'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_GLO554YNEZSX2SSK3ZNWBCIHCU
    association [0..1] to ZZ1_SDSDIST_V as _ZZ1_SDSDIST_APM
  on  $projection.ZZ1_SDSDIST_APM = _ZZ1_SDSDIST_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SDSDIST_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_SDSDIST_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_SDSDIST_MSE
  end as ZZ1_SDSDIST_APM,
  _ZZ1_SDSDIST_APM
}
