@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_A8D601B736E1'

extend view E_FINPLANNINGDELETEITEM with ZZ1_JKH4URLDOEQ5DXYRKWIMHCAU4M
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN
  on  $projection.ZZ1_APPLTN = _ZZ1_APPLTN.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN'
  Persistence.ZZ1_APPLTN as ZZ1_APPLTN,
  _ZZ1_APPLTN
}
