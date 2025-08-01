@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_49857AD44C47'

extend view E_JOURNALENTRYITEM with ZZ1_P7EOW7KF2FRMEHTVBGLFJD4P3Q
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES
  on  $projection.ZZ1_SSERIES = _ZZ1_SSERIES.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES'
  Persistence.ZZ1_SSERIES as ZZ1_SSERIES,
  _ZZ1_SSERIES
}
