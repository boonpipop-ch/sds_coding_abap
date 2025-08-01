@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_C35A8C2463F3'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_E2YE755AKIQVRO7KYOQ4JMTYHE
    association [0..1] to ZZ1_FISCYR_V as _ZZ1_FISCYR_PAM
  on  $projection.ZZ1_FISCYR_PAM = _ZZ1_FISCYR_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_FISCYR_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_FISCYR_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_FISCYR_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_FISCYR_MSE
  end as ZZ1_FISCYR_PAM,
  _ZZ1_FISCYR_PAM
}
