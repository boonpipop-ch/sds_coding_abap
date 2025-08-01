@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_0D9819428C56'

extend view C_FINPLANNINGUPLOADITEM with ZZ1_GMBKZDPFR657SL7ANHMYKY42F4
    association [0..1] to ZZ1_ZZPHA_V as _ZZ1_ZZPHA_MSE
  on  $projection.ZZ1_ZZPHA_MSE = _ZZ1_ZZPHA_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPHA_MSE'
  _Extension.ZZ1_ZZPHA_MSE as ZZ1_ZZPHA_MSE,
  _ZZ1_ZZPHA_MSE
}
