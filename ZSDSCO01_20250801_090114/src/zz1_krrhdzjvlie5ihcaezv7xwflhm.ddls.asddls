@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_A66EC69E1331'

extend view I_ACTUALPLANJRNLENTRYITEMCUBE with ZZ1_KRRHDZJVLIE5IHCAEZV7XWFLHM
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN_APM
  on  $projection.ZZ1_APPLTN_APM = _ZZ1_APPLTN_APM.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN_APM'
@AbapCatalog.compiler.caseJoin
  case
    when APJEI.ActualPlanCode = 'A'
      then  _Extension_acdoca.ZZ1_APPLTN
    when APJEI.ActualPlanCode = 'P'
      then  _Extension_acdocp.ZZ1_APPLTN
  end as ZZ1_APPLTN_APM,
  _ZZ1_APPLTN_APM
}
