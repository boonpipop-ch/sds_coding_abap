@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_DCF566B08C69'

extend view I_JOURNALENTRYITEM with ZZ1_5QYDD3UJ7GW4H4UDAWIJ2BVF3U
    association [0..1] to ZSDSVI_PROJTYP as _ZZ1_APPLTN
  on  $projection.ZZ1_APPLTN = _ZZ1_APPLTN.Code 
 
{ 
@Consumption.valueHelpDefinition: [{
  entity: {
    name: 'ZSDSVI_PROJTYP',
    element: 'Code'
  }
}]
@ObjectModel.foreignKey.association: '_ZZ1_APPLTN'
  _Extension.ZZ1_APPLTN as ZZ1_APPLTN,
  _ZZ1_APPLTN
}
