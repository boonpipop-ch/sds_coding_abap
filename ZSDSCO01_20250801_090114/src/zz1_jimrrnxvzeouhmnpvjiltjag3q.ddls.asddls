@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_EEFEAFC55F99'

extend view I_GLACCOUNTLINEITEMSEMTAG with ZZ1_JIMRRNXVZEOUHMNPVJILTJAG3Q
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN
  on  $projection.ZZ1_APPLTN = _ZZ1_APPLTN.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN'
  _Extension.ZZ1_APPLTN as ZZ1_APPLTN,
  _ZZ1_APPLTN
}
