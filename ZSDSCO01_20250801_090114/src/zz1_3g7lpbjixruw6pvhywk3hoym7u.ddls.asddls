@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_036A85DFED29'

extend view I_JOURNALENTRYITEM with ZZ1_3G7LPBJIXRUW6PVHYWK3HOYM7U
    association [0..1] to ZSDSVI_CUSTCLASS as _ZZ1_SHOPTYPE
  on  $projection.ZZ1_SHOPTYPE = _ZZ1_SHOPTYPE.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZSDSVI_CUSTCLASS',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_SHOPTYPE'
  _Extension.ZZ1_SHOPTYPE as ZZ1_SHOPTYPE,
  _ZZ1_SHOPTYPE
}
