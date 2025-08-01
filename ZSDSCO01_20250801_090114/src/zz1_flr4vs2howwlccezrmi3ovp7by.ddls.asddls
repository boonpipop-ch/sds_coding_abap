@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_31073D5CCC64'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_FLR4VS2HOWWLCCEZRMI3OVP7BY
    association [0..1] to ZZ1_PROJTYPE_V as _ZZ1_PROJTYPE_MSE
  on  $projection.ZZ1_PROJTYPE_MSE = _ZZ1_PROJTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_PROJTYPE_MSE'
  _Extension.ZZ1_PROJTYPE_MSE as ZZ1_PROJTYPE_MSE,
  _ZZ1_PROJTYPE_MSE
}
