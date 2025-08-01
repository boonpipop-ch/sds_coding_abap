@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_8CD1230A899A'

extend view E_FINPLANNINGDELETEITEM with ZZ1_2JHMFJRG6LTFH4EMV5ULRTAREA
    association [0..1] to ZSDSVI_EXTMATGRP as _ZZ1_SSERIES
  on  $projection.ZZ1_SSERIES = _ZZ1_SSERIES.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SSERIES'
  Persistence.ZZ1_SSERIES as ZZ1_SSERIES,
  _ZZ1_SSERIES
}
