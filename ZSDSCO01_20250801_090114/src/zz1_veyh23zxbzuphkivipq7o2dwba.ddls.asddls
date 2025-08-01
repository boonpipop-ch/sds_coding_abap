@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_36ABA27274EF'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_VEYH23ZXBZUPHKIVIPQ7O2DWBA
  
{ 
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_PROJ_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_PROJ_MSE
  end as ZZ1_PROJ_APM
}
