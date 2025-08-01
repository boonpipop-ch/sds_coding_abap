@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_162E998F12FC'

extend view I_JOURNALENTRYITEM with ZZ1_VSGINSC7RBIZRGLUJV2OWOUGGM
    association [0..1] to ZZ1_ACTTYPE_V as _ZZ1_ACTTYPE_MSE
  on  $projection.ZZ1_ACTTYPE_MSE = _ZZ1_ACTTYPE_MSE.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ACTTYPE_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ACTTYPE_MSE'
  _Extension.ZZ1_ACTTYPE_MSE as ZZ1_ACTTYPE_MSE,
  _ZZ1_ACTTYPE_MSE
}
