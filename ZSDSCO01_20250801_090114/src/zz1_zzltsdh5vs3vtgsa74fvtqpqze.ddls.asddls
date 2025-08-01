@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_20CF538502A7'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_ZZLTSDH5VS3VTGSA74FVTQPQZE
    association [0..1] to ZSDSVI_CUSTCLASS as _ZZ1_SHOPTYPE
  on  $projection.ZZ1_SHOPTYPE = _ZZ1_SHOPTYPE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SHOPTYPE'
  _Extension.ZZ1_SHOPTYPE as ZZ1_SHOPTYPE,
  _ZZ1_SHOPTYPE
}
