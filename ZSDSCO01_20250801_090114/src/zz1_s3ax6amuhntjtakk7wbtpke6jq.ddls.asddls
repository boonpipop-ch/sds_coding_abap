@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_C7F0600769B8'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_S3AX6AMUHNTJTAKK7WBTPKE6JQ
    association [0..1] to ZZ1_SDSDIST_V as _ZZ1_SDSDIST_PAM
  on  $projection.ZZ1_SDSDIST_PAM = _ZZ1_SDSDIST_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_SDSDIST_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_SDSDIST_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_SDSDIST_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_SDSDIST_MSE
  end as ZZ1_SDSDIST_PAM,
  _ZZ1_SDSDIST_PAM
}
