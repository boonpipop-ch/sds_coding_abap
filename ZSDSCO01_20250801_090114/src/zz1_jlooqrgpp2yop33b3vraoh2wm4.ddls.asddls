@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_FD245142121C'

extend view E_FINPLANNINGUPLOADITEM with ZZ1_JLOOQRGPP2YOP33B3VRAOH2WM4
    association [0..1] to ZZ1_ZZPMT_V as _ZZ1_ZZPMT_MSE
  on  $projection.ZZ1_ZZPMT_MSE = _ZZ1_ZZPMT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPMT_MSE'
  Persistence.ZZ1_ZZPMT_MSE as ZZ1_ZZPMT_MSE,
  _ZZ1_ZZPMT_MSE
}
