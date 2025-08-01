@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_764006620187'

extend view C_FINPLANNINGUPLOADITEM with ZZ1_PEHGLXNKGFDZF62OVO3ERXEY6E
    association [0..1] to ZZ1_SERVTYPE_V as _ZZ1_SERVTYPE_MSE
  on  $projection.ZZ1_SERVTYPE_MSE = _ZZ1_SERVTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SERVTYPE_MSE'
  _Extension.ZZ1_SERVTYPE_MSE as ZZ1_SERVTYPE_MSE,
  _ZZ1_SERVTYPE_MSE
}
