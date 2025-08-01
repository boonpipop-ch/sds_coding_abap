@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_43B4B0AD8F3B'

extend view C_ALLOCATIONRUNRESULTITEM with ZZ1_TUSG4KFAM4NFITPYF5OMJRETPU
    association [0..1] to ZZ1_SDSDIST_V as _ZZ1_SDSDIST_MSE
  on  $projection.ZZ1_SDSDIST_MSE = _ZZ1_SDSDIST_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SDSDIST_MSE'
  _Extension.ZZ1_SDSDIST_MSE as ZZ1_SDSDIST_MSE,
  _ZZ1_SDSDIST_MSE
}
