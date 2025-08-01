@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_350670D8C360'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_MBZC2QGQA2VIUBK2CNTP77SYYI
    association [0..1] to ZSDSVI_CUSTCLASS as _ZZ1_SHOPTYPE_PAM
  on  $projection.ZZ1_SHOPTYPE_PAM = _ZZ1_SHOPTYPE_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZSDSVI_CUSTCLASS',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_SHOPTYPE_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_SHOPTYPE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_SHOPTYPE
  end as ZZ1_SHOPTYPE_PAM,
  _ZZ1_SHOPTYPE_PAM
}
