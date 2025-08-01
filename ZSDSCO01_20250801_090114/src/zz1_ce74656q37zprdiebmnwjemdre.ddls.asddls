@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_8756C1FADBC4'

extend view E_ALLOCATIONRUNRESULT with ZZ1_CE74656Q37ZPRDIEBMNWJEMDRE
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  Persistence.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
