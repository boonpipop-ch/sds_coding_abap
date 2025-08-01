@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_157FAE464CCF'

extend view C_ALLOCRUNJOURNALENTRYITEM with ZZ1_3UKKHBASCWZQQ26SL3NHWY3GCA
    association [0..1] to ZZ1_SDSDIST_V as _ZZ1_SDSDIST_MSE
  on  $projection.ZZ1_SDSDIST_MSE = _ZZ1_SDSDIST_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SDSDIST_MSE'
  _Extension.ZZ1_SDSDIST_MSE as ZZ1_SDSDIST_MSE,
  _ZZ1_SDSDIST_MSE
}
