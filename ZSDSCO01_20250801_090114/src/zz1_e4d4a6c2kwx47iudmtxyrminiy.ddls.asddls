@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_00796FBD3EBA'

extend view E_FINPLANNINGUPLOADITEM with ZZ1_E4D4A6C2KWX47IUDMTXYRMINIY
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  Persistence.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
