@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_89C9A3B8BF87'

extend view P_GLACCOUNTBALANCE with ZZ1_PQ5OOF4RI6EQ2WHIF3AHZ7Q4I4
    association [0..1] to ZZ1_SERVTYPE_V as _ZZ1_SERVTYPE_MSE
  on  $projection.ZZ1_SERVTYPE_MSE = _ZZ1_SERVTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SERVTYPE_MSE'
  _Extension.ZZ1_SERVTYPE_MSE as ZZ1_SERVTYPE_MSE,
  _ZZ1_SERVTYPE_MSE
}
