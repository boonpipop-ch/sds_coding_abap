@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_F83832B1F94D'

extend view C_ALLOCRUNJOURNALENTRYITEM with ZZ1_DBPW53WVYJCEBKZBFSH2IJ7XPY
    association [0..1] to ZSDSVI_MATGRP1 as _ZZ1_MVGR1
  on  $projection.ZZ1_MVGR1 = _ZZ1_MVGR1.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_MVGR1'
  _Extension.ZZ1_MVGR1 as ZZ1_MVGR1,
  _ZZ1_MVGR1
}
