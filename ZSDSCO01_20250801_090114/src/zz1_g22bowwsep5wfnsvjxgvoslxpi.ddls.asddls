@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_9A9FD6759FEE'

extend view I_JOURNALENTRYITEM with ZZ1_G22BOWWSEP5WFNSVJXGVOSLXPI
    association [0..1] to ZZ1_ZZPMT_V as _ZZ1_ZZPMT_MSE
  on  $projection.ZZ1_ZZPMT_MSE = _ZZ1_ZZPMT_MSE.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZZ1_ZZPMT_V',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_ZZPMT_MSE'
  _Extension.ZZ1_ZZPMT_MSE as ZZ1_ZZPMT_MSE,
  _ZZ1_ZZPMT_MSE
}
