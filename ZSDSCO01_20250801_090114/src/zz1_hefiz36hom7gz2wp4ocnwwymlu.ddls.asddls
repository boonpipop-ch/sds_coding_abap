@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_DC3D6A7D24F7'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_HEFIZ36HOM7GZ2WP4OCNWWYMLU
    association [0..1] to ZZ1_ZZINNI_V as _ZZ1_ZZINNI_APM
  on  $projection.ZZ1_ZZINNI_APM = _ZZ1_ZZINNI_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZINNI_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_ZZINNI_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_ZZINNI_MSE
  end as ZZ1_ZZINNI_APM,
  _ZZ1_ZZINNI_APM
}
