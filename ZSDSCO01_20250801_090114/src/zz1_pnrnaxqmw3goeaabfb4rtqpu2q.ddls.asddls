@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_AE41248B24CF'

extend view I_JOURNALENTRYITEM with ZZ1_PNRNAXQMW3GOEAABFB4RTQPU2Q
    association [0..1] to ZZ1_FISCYR_V as _ZZ1_FISCYR_MSE
  on  $projection.ZZ1_FISCYR_MSE = _ZZ1_FISCYR_MSE.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_FISCYR_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_FISCYR_MSE'
  _Extension.ZZ1_FISCYR_MSE as ZZ1_FISCYR_MSE,
  _ZZ1_FISCYR_MSE
}
