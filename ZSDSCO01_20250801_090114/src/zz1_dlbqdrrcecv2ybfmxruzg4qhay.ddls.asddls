@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_221D026C9E49'

extend view I_JOURNALENTRYITEM with ZZ1_DLBQDRRCECV2YBFMXRUZG4QHAY
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES
  on  $projection.ZZ1_SSERIES = _ZZ1_SSERIES.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZSDSVI_EXTMATGRP',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES'
  _Extension.ZZ1_SSERIES as ZZ1_SSERIES,
  _ZZ1_SSERIES
}
