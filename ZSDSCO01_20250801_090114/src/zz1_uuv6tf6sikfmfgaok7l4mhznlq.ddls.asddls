@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_E9F65CC58595'

extend view E_FINANCIALPLANNINGENTRYITEM with ZZ1_UUV6TF6SIKFMFGAOK7L4MHZNLQ
    association [0..1] to ZSDSVI_CUSTCLASS as _ZZ1_SHOPTYPE
  on  $projection.ZZ1_SHOPTYPE = _ZZ1_SHOPTYPE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SHOPTYPE'
  Persistence.ZZ1_SHOPTYPE as ZZ1_SHOPTYPE,
  _ZZ1_SHOPTYPE
}
