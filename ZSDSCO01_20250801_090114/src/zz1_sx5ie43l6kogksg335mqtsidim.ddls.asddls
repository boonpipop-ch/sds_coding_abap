@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_0056C9EBB4AB'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_SX5IE43L6KOGKSG335MQTSIDIM
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES_PAM
  on  $projection.ZZ1_SSERIES_PAM = _ZZ1_SSERIES_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZSDSVI_EXTMATGRP',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_SSERIES
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_SSERIES
  end as ZZ1_SSERIES_PAM,
  _ZZ1_SSERIES_PAM
}
