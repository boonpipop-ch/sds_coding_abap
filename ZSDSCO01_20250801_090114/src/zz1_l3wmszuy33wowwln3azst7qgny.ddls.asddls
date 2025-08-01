@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_9BF9966540ED'

extend view I_GLACCTBALANCECUBE with ZZ1_L3WMSZUY33WOWWLN3AZST7QGNY
    association [0..1] to ZZ1_ZZIUT_V as _ZZ1_ZZIUT_MSE
  on  $projection.ZZ1_ZZIUT_MSE = _ZZ1_ZZIUT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZIUT_MSE'
  _Extension.ZZ1_ZZIUT_MSE as ZZ1_ZZIUT_MSE,
  _ZZ1_ZZIUT_MSE
}
