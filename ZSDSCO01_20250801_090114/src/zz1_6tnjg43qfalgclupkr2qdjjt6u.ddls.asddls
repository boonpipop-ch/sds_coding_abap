@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_E31E8E09245D'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_6TNJG43QFALGCLUPKR2QDJJT6U
    association [0..1] to ZZ1_PROJTYPE_V as _ZZ1_PROJTYPE_PAM
  on  $projection.ZZ1_PROJTYPE_PAM = _ZZ1_PROJTYPE_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_PROJTYPE_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_PROJTYPE_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_PROJTYPE_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_PROJTYPE_MSE
  end as ZZ1_PROJTYPE_PAM,
  _ZZ1_PROJTYPE_PAM
}
