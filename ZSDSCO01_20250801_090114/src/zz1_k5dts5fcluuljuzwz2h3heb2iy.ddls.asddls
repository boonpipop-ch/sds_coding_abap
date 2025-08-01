@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_CC63D0046CD2'

extend view I_GLACCOUNTLINEITEMCUBE with ZZ1_K5DTS5FCLUULJUZWZ2H3HEB2IY
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES
  on  $projection.ZZ1_SSERIES = _ZZ1_SSERIES.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES'
  _Extension.ZZ1_SSERIES as ZZ1_SSERIES,
  _ZZ1_SSERIES
}
