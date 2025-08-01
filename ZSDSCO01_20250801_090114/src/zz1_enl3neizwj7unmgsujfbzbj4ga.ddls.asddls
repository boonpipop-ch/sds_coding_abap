@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_40943A73BBF8'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_ENL3NEIZWJ7UNMGSUJFBZBJ4GA
    association [0..1] to ZZ1_ITMTYPE_V as _ZZ1_ITMTYPE_PAM
  on  $projection.ZZ1_ITMTYPE_PAM = _ZZ1_ITMTYPE_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ITMTYPE_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ITMTYPE_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ITMTYPE_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ITMTYPE_MSE
  end as ZZ1_ITMTYPE_PAM,
  _ZZ1_ITMTYPE_PAM
}
