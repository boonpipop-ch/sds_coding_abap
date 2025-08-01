@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_DCB54168DD6C'

extend view I_ACTUALPLANJOURNALENTRYITEM with ZZ1_Q55HMZ2IJGUJEBH6SUS7GXJT6E
  
{ 
@AbapCatalog.compiler.caseJoin
  case
    when ActualPlanJournalEntryItem.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_PROJ_MSE
    when ActualPlanJournalEntryItem.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_PROJ_MSE
  end as ZZ1_PROJ_PAM
}
