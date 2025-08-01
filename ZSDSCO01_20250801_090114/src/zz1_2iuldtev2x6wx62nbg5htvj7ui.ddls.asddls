@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_569675239C6F'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_2IULDTEV2X6WX62NBG5HTVJ7UI
    association [0..1] to ZZ1_FISCYR_V as _ZZ1_FISCYR_MSE
  on  $projection.ZZ1_FISCYR_MSE = _ZZ1_FISCYR_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_FISCYR_MSE'
  _Extension.ZZ1_FISCYR_MSE as ZZ1_FISCYR_MSE,
  _ZZ1_FISCYR_MSE
}
