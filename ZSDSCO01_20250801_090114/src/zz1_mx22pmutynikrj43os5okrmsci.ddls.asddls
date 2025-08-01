@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_D018923BE779'

extend view I_JOURNALENTRYITEM with ZZ1_MX22PMUTYNIKRJ43OS5OKRMSCI
    association [0..1] to ZZ1_ITMTYPE_V as _ZZ1_ITMTYPE_MSE
  on  $projection.ZZ1_ITMTYPE_MSE = _ZZ1_ITMTYPE_MSE.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ITMTYPE_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ITMTYPE_MSE'
  _Extension.ZZ1_ITMTYPE_MSE as ZZ1_ITMTYPE_MSE,
  _ZZ1_ITMTYPE_MSE
}
