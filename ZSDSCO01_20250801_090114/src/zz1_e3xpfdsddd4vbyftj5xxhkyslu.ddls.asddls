@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_AC1500600C77'

extend view E_JOURNALENTRYITEM with ZZ1_E3XPFDSDDD4VBYFTJ5XXHKYSLU
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN
  on  $projection.ZZ1_APPLTN = _ZZ1_APPLTN.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN'
  Persistence.ZZ1_APPLTN as ZZ1_APPLTN,
  _ZZ1_APPLTN
}
