@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_6723132AB986'

extend view E_FINPLANNINGUPLOADITEM with ZZ1_HYBNJNLIM5USFWT42HQTAAY54M
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN
  on  $projection.ZZ1_APPLTN = _ZZ1_APPLTN.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN'
  Persistence.ZZ1_APPLTN as ZZ1_APPLTN,
  _ZZ1_APPLTN
}
