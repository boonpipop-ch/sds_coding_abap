@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_6B0AA56615E4'

extend view E_JOURNALENTRYITEM with ZZ1_AYU4DCDZWQIJCONXNRIH3MS5LE
    association [0..1] to ZZ1_ZZINNI_V as _ZZ1_ZZINNI_MSE
  on  $projection.ZZ1_ZZINNI_MSE = _ZZ1_ZZINNI_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZINNI_MSE'
  Persistence.ZZ1_ZZINNI_MSE as ZZ1_ZZINNI_MSE,
  _ZZ1_ZZINNI_MSE
}
