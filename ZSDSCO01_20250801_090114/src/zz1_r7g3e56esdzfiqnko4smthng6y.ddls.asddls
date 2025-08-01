@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_B404552BB3B4'

extend view E_JOURNALENTRYITEM with ZZ1_R7G3E56ESDZFIQNKO4SMTHNG6Y
    association [0..1] to ZZ1_SDSDIST_V as _ZZ1_SDSDIST_MSE
  on  $projection.ZZ1_SDSDIST_MSE = _ZZ1_SDSDIST_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SDSDIST_MSE'
  Persistence.ZZ1_SDSDIST_MSE as ZZ1_SDSDIST_MSE,
  _ZZ1_SDSDIST_MSE
}
