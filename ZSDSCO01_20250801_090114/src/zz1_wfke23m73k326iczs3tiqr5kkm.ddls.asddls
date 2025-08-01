@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_8EBCC767093C'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_WFKE23M73K326ICZS3TIQR5KKM
    association [0..1] to ZZ1_ZZIUT_V as _ZZ1_ZZIUT_PAM
  on  $projection.ZZ1_ZZIUT_PAM = _ZZ1_ZZIUT_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ZZIUT_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ZZIUT_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZIUT_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZIUT_MSE
  end as ZZ1_ZZIUT_PAM,
  _ZZ1_ZZIUT_PAM
}
