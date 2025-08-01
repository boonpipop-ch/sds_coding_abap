@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_61E45EF3B5A4'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_43OAQ2WPPGCMAUOPPLOQH6AHKY
    association [0..1] to ZZ1_ACTTYPE_V as _ZZ1_ACTTYPE_PAM
  on  $projection.ZZ1_ACTTYPE_PAM = _ZZ1_ACTTYPE_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ACTTYPE_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ACTTYPE_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ACTTYPE_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ACTTYPE_MSE
  end as ZZ1_ACTTYPE_PAM,
  _ZZ1_ACTTYPE_PAM
}
