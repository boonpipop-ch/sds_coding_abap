@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_528ED7508BDE'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_HDIJAJKBTJR3WOUQZJCATZL54U
  
{ 
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZREFTN_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZREFTN_MSE
  end as ZZ1_ZZREFTN_APM
}
