@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_6A566213F596'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_QMIWD4IJ372P7ER57RER3OER34
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES_APM
  on  $projection.ZZ1_SSERIES_APM = _ZZ1_SSERIES_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_SSERIES
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_SSERIES
  end as ZZ1_SSERIES_APM,
  _ZZ1_SSERIES_APM
}
