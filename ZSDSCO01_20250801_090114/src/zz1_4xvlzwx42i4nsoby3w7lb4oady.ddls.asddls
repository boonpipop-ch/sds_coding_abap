@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_6DC244FC216C'

extend view C_ALLOCATIONRUNRESULT with ZZ1_4XVLZWX42I4NSOBY3W7LB4OADY
    association [0..1] to ZZ1_ITMTYPE_V as _ZZ1_ITMTYPE_MSE
  on  $projection.ZZ1_ITMTYPE_MSE = _ZZ1_ITMTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ITMTYPE_MSE'
  _Extension.ZZ1_ITMTYPE_MSE as ZZ1_ITMTYPE_MSE,
  _ZZ1_ITMTYPE_MSE
}
