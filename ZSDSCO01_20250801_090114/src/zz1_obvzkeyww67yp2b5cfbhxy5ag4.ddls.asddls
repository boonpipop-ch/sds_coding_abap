@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_B40FDD1907F5'

extend view C_FINPLANNINGDELETEITEM with ZZ1_OBVZKEYWW67YP2B5CFBHXY5AG4
    association [0..1] to ZZ1_PROJTYPE_V as _ZZ1_PROJTYPE_MSE
  on  $projection.ZZ1_PROJTYPE_MSE = _ZZ1_PROJTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_PROJTYPE_MSE'
  _Extension.ZZ1_PROJTYPE_MSE as ZZ1_PROJTYPE_MSE,
  _ZZ1_PROJTYPE_MSE
}
