@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_97AB61BD3E98'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_TRG4XWK5CYPX7WWFMOHBJGLQUE
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  _Extension.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
