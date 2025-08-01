@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_39787F75298B'

extend view C_ALLOCATIONRUNRESULTITEM with ZZ1_KKNWGJPW5JMZSBL535QTE6QZFU
    association [0..1] to ZZ1_PROJTYPE_V as _ZZ1_PROJTYPE_MSE
  on  $projection.ZZ1_PROJTYPE_MSE = _ZZ1_PROJTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_PROJTYPE_MSE'
  _Extension.ZZ1_PROJTYPE_MSE as ZZ1_PROJTYPE_MSE,
  _ZZ1_PROJTYPE_MSE
}
