@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_42CB82FA48B4'

extend view E_FINPLANNINGDELETEITEM with ZZ1_H6323IHH4R4U6GSIV5IAIMZVCI
    association [0..1] to ZZ1_PROJTYPE_V as _ZZ1_PROJTYPE_MSE
  on  $projection.ZZ1_PROJTYPE_MSE = _ZZ1_PROJTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_PROJTYPE_MSE'
  Persistence.ZZ1_PROJTYPE_MSE as ZZ1_PROJTYPE_MSE,
  _ZZ1_PROJTYPE_MSE
}
