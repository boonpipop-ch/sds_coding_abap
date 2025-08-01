@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_4CA7546769E7'

extend view E_FINPLANNINGUPLOADITEM with ZZ1_CFUXOIA5OFDLMVJVOSLBW6DWOI
    association [0..1] to ZZ1_ACTTYPE_V as _ZZ1_ACTTYPE_MSE
  on  $projection.ZZ1_ACTTYPE_MSE = _ZZ1_ACTTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ACTTYPE_MSE'
  Persistence.ZZ1_ACTTYPE_MSE as ZZ1_ACTTYPE_MSE,
  _ZZ1_ACTTYPE_MSE
}
