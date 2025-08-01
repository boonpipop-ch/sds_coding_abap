@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_8DF41B8AD308'

extend view E_FINANCIALPLANNINGENTRYITEM with ZZ1_SPYWPP5Y7BWCQ6RFNPYHELIGCA
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES
  on  $projection.ZZ1_SSERIES = _ZZ1_SSERIES.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES'
  Persistence.ZZ1_SSERIES as ZZ1_SSERIES,
  _ZZ1_SSERIES
}
