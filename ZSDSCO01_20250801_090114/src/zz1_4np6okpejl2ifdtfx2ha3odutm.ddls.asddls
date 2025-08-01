@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_1A651DD75AC4'

extend view E_JOURNALENTRYITEM with ZZ1_4NP6OKPEJL2IFDTFX2HA3ODUTM
    association [0..1] to ZSDSVI_CUSTCLASS as _ZZ1_SHOPTYPE
  on  $projection.ZZ1_SHOPTYPE = _ZZ1_SHOPTYPE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SHOPTYPE'
  Persistence.ZZ1_SHOPTYPE as ZZ1_SHOPTYPE,
  _ZZ1_SHOPTYPE
}
