@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_9AA43B1128F0'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_L7NCTOM4RQPBBJHNWR6NW3VWIU
    association [0..1] to ZZ1_ZZPMT_V as _ZZ1_ZZPMT_PAM
  on  $projection.ZZ1_ZZPMT_PAM = _ZZ1_ZZPMT_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ZZPMT_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ZZPMT_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZPMT_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZPMT_MSE
  end as ZZ1_ZZPMT_PAM,
  _ZZ1_ZZPMT_PAM
}
