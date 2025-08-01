@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_2700F6D87F01'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_CQCE7UZZMCTSMIGTIZQZIIUZ3A
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN_PAM
  on  $projection.ZZ1_APPLTN_PAM = _ZZ1_APPLTN_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZSDSVI_PROJTYP',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_APPLTN
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_APPLTN
  end as ZZ1_APPLTN_PAM,
  _ZZ1_APPLTN_PAM
}
