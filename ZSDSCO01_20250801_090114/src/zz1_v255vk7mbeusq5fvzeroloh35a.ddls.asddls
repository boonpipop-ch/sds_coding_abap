@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_622B55F6252B'

extend view I_GLACCTBALANCECUBE with ZZ1_V255VK7MBEUSQ5FVZEROLOH35A
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  _Extension.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
