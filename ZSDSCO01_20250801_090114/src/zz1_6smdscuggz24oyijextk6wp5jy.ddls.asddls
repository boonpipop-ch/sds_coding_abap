@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_2E06A54DA3D0'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_6SMDSCUGGZ24OYIJEXTK6WP5JY
  
{ 
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZCAV_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZCAV_MSE
  end as ZZ1_ZZCAV_PAM
}
