@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_CE57DDFD9BA0'

extend view C_ALLOCATIONRUNRESULT with ZZ1_EOCFYGTOKTT3VMG26X2AU2JRHU
    association [0..1] to ZZ1_ZZIUT_V as _ZZ1_ZZIUT_MSE
  on  $projection.ZZ1_ZZIUT_MSE = _ZZ1_ZZIUT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZIUT_MSE'
  _Extension.ZZ1_ZZIUT_MSE as ZZ1_ZZIUT_MSE,
  _ZZ1_ZZIUT_MSE
}
