@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_31C4527A517C'

extend view E_JOURNALENTRYITEM with ZZ1_B3ZUZLHLGKAVU6CDXJHL43LGSU
    association [0..1] to ZZ1_FISCYR_V as _ZZ1_FISCYR_MSE
  on  $projection.ZZ1_FISCYR_MSE = _ZZ1_FISCYR_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_FISCYR_MSE'
  Persistence.ZZ1_FISCYR_MSE as ZZ1_FISCYR_MSE,
  _ZZ1_FISCYR_MSE
}
