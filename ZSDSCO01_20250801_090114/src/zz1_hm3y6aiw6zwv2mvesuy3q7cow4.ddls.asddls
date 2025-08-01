@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_EE16F31283C7'

extend view P_GLACCOUNTBALANCE with ZZ1_HM3Y6AIW6ZWV2MVESUY3Q7COW4
    association [0..1] to ZZ1_ZZINNI_V as _ZZ1_ZZINNI_MSE
  on  $projection.ZZ1_ZZINNI_MSE = _ZZ1_ZZINNI_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZINNI_MSE'
  _Extension.ZZ1_ZZINNI_MSE as ZZ1_ZZINNI_MSE,
  _ZZ1_ZZINNI_MSE
}
