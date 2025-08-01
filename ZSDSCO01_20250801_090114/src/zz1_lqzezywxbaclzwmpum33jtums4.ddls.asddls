@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_EC4019FA743F'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_LQZEZYWXBACLZWMPUM33JTUMS4
  
{ 
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZCAV_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZCAV_MSE
  end as ZZ1_ZZCAV_APM
}
