@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_B48E04A5FE1B'

extend view I_GLACCOUNTLINEITEMCUBE with ZZ1_4FM4YFABWKOT3DDC76U4R2SE2M
    association [0..1] to ZZ1_ZZINNI_V as _ZZ1_ZZINNI_MSE
  on  $projection.ZZ1_ZZINNI_MSE = _ZZ1_ZZINNI_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZINNI_MSE'
  _Extension.ZZ1_ZZINNI_MSE as ZZ1_ZZINNI_MSE,
  _ZZ1_ZZINNI_MSE
}
