@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_00A03A613CBB'

extend view E_FINPLANNINGDELETEITEM with ZZ1_4XIZ2MU53R36SURDUFTZFGHVOA
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  Persistence.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
