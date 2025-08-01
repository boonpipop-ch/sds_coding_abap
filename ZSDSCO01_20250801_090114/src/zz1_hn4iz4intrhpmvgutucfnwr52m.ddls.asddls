@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_2354E5F69098'

extend view E_ALLOCATIONRUNRESULT with ZZ1_HN4IZ4INTRHPMVGUTUCFNWR52M
    association [0..1] to ZZ1_SERVTYPE_V as _ZZ1_SERVTYPE_MSE
  on  $projection.ZZ1_SERVTYPE_MSE = _ZZ1_SERVTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SERVTYPE_MSE'
  Persistence.ZZ1_SERVTYPE_MSE as ZZ1_SERVTYPE_MSE,
  _ZZ1_SERVTYPE_MSE
}
