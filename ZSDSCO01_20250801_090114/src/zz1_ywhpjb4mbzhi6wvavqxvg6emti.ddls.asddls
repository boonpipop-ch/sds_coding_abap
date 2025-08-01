@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_D87922BD2021'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_YWHPJB4MBZHI6WVAVQXVG6EMTI
  
{ 
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZREFTN_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZREFTN_MSE
  end as ZZ1_ZZREFTN_PAM
}
