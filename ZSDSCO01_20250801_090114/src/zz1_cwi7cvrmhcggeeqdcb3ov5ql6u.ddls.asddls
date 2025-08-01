@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_7305E8A03C4E'

extend view I_JOURNALENTRYITEM with ZZ1_CWI7CVRMHCGGEEQDCB3OV5QL6U
    association [0..1] to ZZ1_ZZINNI_V as _ZZ1_ZZINNI_MSE
  on  $projection.ZZ1_ZZINNI_MSE = _ZZ1_ZZINNI_MSE.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ZZINNI_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ZZINNI_MSE'
  _Extension.ZZ1_ZZINNI_MSE as ZZ1_ZZINNI_MSE,
  _ZZ1_ZZINNI_MSE
}
