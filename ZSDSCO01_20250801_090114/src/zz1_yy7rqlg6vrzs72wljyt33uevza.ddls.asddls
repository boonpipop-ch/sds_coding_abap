@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_08C0672F908B'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_YY7RQLG6VRZS72WLJYT33UEVZA
    association [0..1] to ZZ1_ACTTYPE_V as _ZZ1_ACTTYPE_APM
  on  $projection.ZZ1_ACTTYPE_APM = _ZZ1_ACTTYPE_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ACTTYPE_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ACTTYPE_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ACTTYPE_MSE
  end as ZZ1_ACTTYPE_APM,
  _ZZ1_ACTTYPE_APM
}
