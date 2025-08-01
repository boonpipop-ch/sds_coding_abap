@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_3CE67773D924'

extend view I_JOURNALENTRYITEM with ZZ1_BDLZ3F2YL6CV4F7MQF53C375BA
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZSDSVI_MATGRP1',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  _Extension.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
