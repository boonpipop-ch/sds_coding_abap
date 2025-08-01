@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_0A7440DAF05F'

extend view E_FINPLANNINGUPLOADITEM with ZZ1_3JEGNGK3YWSQ6TELVONHKM3KZA
    association [0..1] to ZZ1_ZZPHA_V as _ZZ1_ZZPHA_MSE
  on  $projection.ZZ1_ZZPHA_MSE = _ZZ1_ZZPHA_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPHA_MSE'
  Persistence.ZZ1_ZZPHA_MSE as ZZ1_ZZPHA_MSE,
  _ZZ1_ZZPHA_MSE
}
