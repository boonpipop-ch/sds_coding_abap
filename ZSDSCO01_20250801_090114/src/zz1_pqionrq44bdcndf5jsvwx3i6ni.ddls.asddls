@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_51DB7411504B'

extend view C_FINPLANNINGDELETEITEM with ZZ1_PQIONRQ44BDCNDF5JSVWX3I6NI
    association [0..1] to ZZ1_ZZINNI_V as _ZZ1_ZZINNI_MSE
  on  $projection.ZZ1_ZZINNI_MSE = _ZZ1_ZZINNI_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZINNI_MSE'
  _Extension.ZZ1_ZZINNI_MSE as ZZ1_ZZINNI_MSE,
  _ZZ1_ZZINNI_MSE
}
