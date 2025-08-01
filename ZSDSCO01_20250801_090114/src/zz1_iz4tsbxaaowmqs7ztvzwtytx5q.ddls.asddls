@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_835305137D70'

extend view E_FINPLANNINGDELETEITEM with ZZ1_IZ4TSBXAAOWMQS7ZTVZWTYTX5Q
    association [0..1] to ZSDSVI_CUSTCLASS as _ZZ1_SHOPTYPE
  on  $projection.ZZ1_SHOPTYPE = _ZZ1_SHOPTYPE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SHOPTYPE'
  Persistence.ZZ1_SHOPTYPE as ZZ1_SHOPTYPE,
  _ZZ1_SHOPTYPE
}
