@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_7170C1746F22'

extend view E_FINPLANNINGDELETEITEM with ZZ1_2PLU77VFN3J52UN2ZJGHC6W7GA
    association [0..1] to ZZ1_ACTTYPE_V as _ZZ1_ACTTYPE_MSE
  on  $projection.ZZ1_ACTTYPE_MSE = _ZZ1_ACTTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ACTTYPE_MSE'
  Persistence.ZZ1_ACTTYPE_MSE as ZZ1_ACTTYPE_MSE,
  _ZZ1_ACTTYPE_MSE
}
