extend view entity FIN_SUBVALPROFSEGMENTLINEITEM with 
    association [0..1] to ZZ1_ZZPHA_V as _ZZ1_ZZPHA_MSE
  on  $projection.ZZ1_ZZPHA_MSE = _ZZ1_ZZPHA_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPHA_MSE'
  _Extension.ZZ1_ZZPHA_MSE as ZZ1_ZZPHA_MSE,
  _ZZ1_ZZPHA_MSE
}
