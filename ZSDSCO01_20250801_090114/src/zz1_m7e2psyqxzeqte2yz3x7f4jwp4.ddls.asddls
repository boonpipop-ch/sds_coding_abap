@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_DC3102ECDE51'

extend view I_GLACCTBALANCECUBE with ZZ1_M7E2PSYQXZEQTE2YZ3X7F4JWP4
    association [0..1] to ZZ1_ZZINNI_V as _ZZ1_ZZINNI_MSE
  on  $projection.ZZ1_ZZINNI_MSE = _ZZ1_ZZINNI_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZINNI_MSE'
  _Extension.ZZ1_ZZINNI_MSE as ZZ1_ZZINNI_MSE,
  _ZZ1_ZZINNI_MSE
}
