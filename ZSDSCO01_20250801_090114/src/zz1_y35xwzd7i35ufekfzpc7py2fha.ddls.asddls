@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_CA5AAF19B483'

extend view C_ALLOCATIONRUNRESULT with ZZ1_Y35XWZD7I35UFEKFZPC7PY2FHA
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN
  on  $projection.ZZ1_APPLTN = _ZZ1_APPLTN.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN'
  _Extension.ZZ1_APPLTN as ZZ1_APPLTN,
  _ZZ1_APPLTN
}
