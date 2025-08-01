@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_F50F286CB0A8'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_WIV3EUGVRIX3IXZKMURGREGFCY
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1_PAM
  on  $projection.ZZ1_MVGR1_PAM = _ZZ1_MVGR1_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZSDSVI_MATGRP1',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_MVGR1
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_MVGR1
  end as ZZ1_MVGR1_PAM,
  _ZZ1_MVGR1_PAM
}
