@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_C1770423879F'

extend view C_ALLOCATIONRUNRESULT with ZZ1_JNPBHANRMEJEXCX6G37JQYU5LQ
    association [0..1] to ZZ1_ACTTYPE_V as _ZZ1_ACTTYPE_MSE
  on  $projection.ZZ1_ACTTYPE_MSE = _ZZ1_ACTTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ACTTYPE_MSE'
  _Extension.ZZ1_ACTTYPE_MSE as ZZ1_ACTTYPE_MSE,
  _ZZ1_ACTTYPE_MSE
}
