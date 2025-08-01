@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_D1356C360D49'

extend view C_ALLOCATIONRUNRESULTITEM with ZZ1_FVOHU5P3HM7MRJAT6ZZMFINSKA
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN
  on  $projection.ZZ1_APPLTN = _ZZ1_APPLTN.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN'
  _Extension.ZZ1_APPLTN as ZZ1_APPLTN,
  _ZZ1_APPLTN
}
