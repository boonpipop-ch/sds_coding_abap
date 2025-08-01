@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_901E19099814'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_OCZWXT5I4YAYF3RMGDZOCL445I
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES
  on  $projection.ZZ1_SSERIES = _ZZ1_SSERIES.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES'
  _Extension.ZZ1_SSERIES as ZZ1_SSERIES,
  _ZZ1_SSERIES
}
