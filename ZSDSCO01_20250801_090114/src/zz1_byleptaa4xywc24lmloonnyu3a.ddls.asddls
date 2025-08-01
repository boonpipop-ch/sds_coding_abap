@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_19C0389F6239'

extend view C_ALLOCRUNJOURNALENTRYITEM with ZZ1_BYLEPTAA4XYWC24LMLOONNYU3A
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES
  on  $projection.ZZ1_SSERIES = _ZZ1_SSERIES.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES'
  _Extension.ZZ1_SSERIES as ZZ1_SSERIES,
  _ZZ1_SSERIES
}
