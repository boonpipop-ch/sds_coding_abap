@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_F04CB84A1AC4'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_R434M5FWOLIMCMXGVS3K7OSM3A
    association [0..1] to ZZ1_PROJTYPE_V as _ZZ1_PROJTYPE_APM
  on  $projection.ZZ1_PROJTYPE_APM = _ZZ1_PROJTYPE_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_PROJTYPE_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_PROJTYPE_MSE
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_PROJTYPE_MSE
  end as ZZ1_PROJTYPE_APM,
  _ZZ1_PROJTYPE_APM
}
