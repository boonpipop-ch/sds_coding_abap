@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_145A5532B484'

extend view P_GLACCOUNTBALANCE with ZZ1_BOEUI7UWEJ65BD57EKTGEA466M
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  _Extension.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
