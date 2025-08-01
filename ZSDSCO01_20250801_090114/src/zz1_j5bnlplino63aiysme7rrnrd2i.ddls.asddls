@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_EA44D57D0954'

extend view C_ALLOCATIONRUNRESULTITEM with ZZ1_J5BNLPLINO63AIYSME7RRNRD2I
    association [0..1] to ZZ1_ZZPMT_V as _ZZ1_ZZPMT_MSE
  on  $projection.ZZ1_ZZPMT_MSE = _ZZ1_ZZPMT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPMT_MSE'
  _Extension.ZZ1_ZZPMT_MSE as ZZ1_ZZPMT_MSE,
  _ZZ1_ZZPMT_MSE
}
