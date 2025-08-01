@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_1EEBD1E3DACD'

extend view E_FINANCIALPLANNINGENTRYITEM with ZZ1_6OR3DL4DJ2F4WGF7LGBWNA3Y6M
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  Persistence.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
