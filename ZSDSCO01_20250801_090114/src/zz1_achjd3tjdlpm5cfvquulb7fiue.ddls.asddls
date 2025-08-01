@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_6EBCB1FAEC32'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_ACHJD3TJDLPM5CFVQUULB7FIUE
    association [0..1] to ZZ1_SERVTYPE_V as _ZZ1_SERVTYPE_APM
  on  $projection.ZZ1_SERVTYPE_APM = _ZZ1_SERVTYPE_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SERVTYPE_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_SERVTYPE_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_SERVTYPE_MSE
  end as ZZ1_SERVTYPE_APM,
  _ZZ1_SERVTYPE_APM
}
