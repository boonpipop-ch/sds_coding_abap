@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_73C5181F2B74'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_DNV34QJC25TY2SL7BGRQ6I7PEE
    association [0..1] to ZZ1_SERVTYPE_V as _ZZ1_SERVTYPE_PAM
  on  $projection.ZZ1_SERVTYPE_PAM = _ZZ1_SERVTYPE_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_SERVTYPE_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_SERVTYPE_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_SERVTYPE_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_SERVTYPE_MSE
  end as ZZ1_SERVTYPE_PAM,
  _ZZ1_SERVTYPE_PAM
}
