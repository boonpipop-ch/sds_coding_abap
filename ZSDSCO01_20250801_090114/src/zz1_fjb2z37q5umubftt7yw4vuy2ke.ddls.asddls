@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_F9D020CD0EC0'

extend view C_ALLOCATIONRUNRESULTITEM with ZZ1_FJB2Z37Q5UMUBFTT7YW4VUY2KE
    association [0..1] to ZSDSVI_CUSTCLASS as _ZZ1_SHOPTYPE
  on  $projection.ZZ1_SHOPTYPE = _ZZ1_SHOPTYPE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SHOPTYPE'
  _Extension.ZZ1_SHOPTYPE as ZZ1_SHOPTYPE,
  _ZZ1_SHOPTYPE
}
