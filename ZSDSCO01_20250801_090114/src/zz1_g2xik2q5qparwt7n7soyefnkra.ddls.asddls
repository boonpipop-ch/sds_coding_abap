@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_75A0600FE11E'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_G2XIK2Q5QPARWT7N7SOYEFNKRA
    association [0..1] to ZZ1_ZZPHA_V as _ZZ1_ZZPHA_PAM
  on  $projection.ZZ1_ZZPHA_PAM = _ZZ1_ZZPHA_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ZZPHA_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ZZPHA_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZPHA_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZPHA_MSE
  end as ZZ1_ZZPHA_PAM,
  _ZZ1_ZZPHA_PAM
}
