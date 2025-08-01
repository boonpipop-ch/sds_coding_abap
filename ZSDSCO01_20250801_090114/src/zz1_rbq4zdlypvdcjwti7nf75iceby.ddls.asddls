@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_30E552EB8336'

extend view I_PROFITABILITYCUBE with ZZ1_RBQ4ZDLYPVDCJWTI7NF75ICEBY
    association [0..1] to ZZ1_SDSDIST_V as _ZZ1_SDSDIST_MSE
  on  $projection.ZZ1_SDSDIST_MSE = _ZZ1_SDSDIST_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SDSDIST_MSE'
  _Extension.ZZ1_SDSDIST_MSE as ZZ1_SDSDIST_MSE,
  _ZZ1_SDSDIST_MSE
}
