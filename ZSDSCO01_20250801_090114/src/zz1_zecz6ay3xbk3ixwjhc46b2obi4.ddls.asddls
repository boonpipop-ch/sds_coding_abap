@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_7C58008D2301'

extend view I_GLACCOUNTLINEITEMCUBE with ZZ1_ZECZ6AY3XBK3IXWJHC46B2OBI4
    association [0..1] to ZZ1_ZZPMT_V as _ZZ1_ZZPMT_MSE
  on  $projection.ZZ1_ZZPMT_MSE = _ZZ1_ZZPMT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPMT_MSE'
  _Extension.ZZ1_ZZPMT_MSE as ZZ1_ZZPMT_MSE,
  _ZZ1_ZZPMT_MSE
}
