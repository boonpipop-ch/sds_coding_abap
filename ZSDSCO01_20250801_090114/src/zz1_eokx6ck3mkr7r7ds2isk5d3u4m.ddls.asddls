@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_2EE2F7988946'

extend view C_ALLOCATIONRUNRESULT with ZZ1_EOKX6CK3MKR7R7DS2ISK5D3U4M
    association [0..1] to ZZ1_FISCYR_V as _ZZ1_FISCYR_MSE
  on  $projection.ZZ1_FISCYR_MSE = _ZZ1_FISCYR_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_FISCYR_MSE'
  _Extension.ZZ1_FISCYR_MSE as ZZ1_FISCYR_MSE,
  _ZZ1_FISCYR_MSE
}
