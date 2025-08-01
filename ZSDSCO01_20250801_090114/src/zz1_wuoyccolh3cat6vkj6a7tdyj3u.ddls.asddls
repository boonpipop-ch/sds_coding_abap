@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_889B727E84BF'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_WUOYCCOLH3CAT6VKJ6A7TDYJ3U
    association [0..1] to ZZ1_ZZINNI_V as _ZZ1_ZZINNI_PAM
  on  $projection.ZZ1_ZZINNI_PAM = _ZZ1_ZZINNI_PAM.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ZZINNI_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ZZINNI_PAM'
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZINNI_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZINNI_MSE
  end as ZZ1_ZZINNI_PAM,
  _ZZ1_ZZINNI_PAM
}
