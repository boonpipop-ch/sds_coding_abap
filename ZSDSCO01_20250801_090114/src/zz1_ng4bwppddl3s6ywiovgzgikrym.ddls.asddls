@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_0DB628369657'

extend view E_FINPLANNINGUPLOADITEM with ZZ1_NG4BWPPDDL3S6YWIOVGZGIKRYM
    association [0..1] to ZZ1_SERVTYPE_V as _ZZ1_SERVTYPE_MSE
  on  $projection.ZZ1_SERVTYPE_MSE = _ZZ1_SERVTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SERVTYPE_MSE'
  Persistence.ZZ1_SERVTYPE_MSE as ZZ1_SERVTYPE_MSE,
  _ZZ1_SERVTYPE_MSE
}
