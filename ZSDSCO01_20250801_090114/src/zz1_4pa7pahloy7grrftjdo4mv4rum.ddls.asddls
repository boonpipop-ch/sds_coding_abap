@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_1BFEA290A3AB'

extend view E_ALLOCATIONRUNRESULT with ZZ1_4PA7PAHLOY7GRRFTJDO4MV4RUM
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN
  on  $projection.ZZ1_APPLTN = _ZZ1_APPLTN.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN'
  Persistence.ZZ1_APPLTN as ZZ1_APPLTN,
  _ZZ1_APPLTN
}
