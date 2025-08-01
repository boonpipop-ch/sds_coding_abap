@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_297F87C5ED25'

extend view E_FINPLANNINGDELETEITEM with ZZ1_WO54ZURI52HHYT7GYXFRJOAE6E
    association [0..1] to ZZ1_ZZIUT_V as _ZZ1_ZZIUT_MSE
  on  $projection.ZZ1_ZZIUT_MSE = _ZZ1_ZZIUT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZIUT_MSE'
  Persistence.ZZ1_ZZIUT_MSE as ZZ1_ZZIUT_MSE,
  _ZZ1_ZZIUT_MSE
}
