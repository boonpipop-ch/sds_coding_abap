@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_D7FFB1297ED3'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_EPQN2QJMJXZYLJ4XKKPAPGWL74
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1_APM
  on  $projection.ZZ1_MVGR1_APM = _ZZ1_MVGR1_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_MVGR1
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_MVGR1
  end as ZZ1_MVGR1_APM,
  _ZZ1_MVGR1_APM
}
