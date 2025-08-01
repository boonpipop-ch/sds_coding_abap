@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_10F81CEFBFF8'

extend view C_FINPLANNINGDELETEITEM with ZZ1_TPXF2Q63M2T2QBGEMSPCZPSFNA
    association [0..1] to ZZ1_ITMTYPE_V as _ZZ1_ITMTYPE_MSE
  on  $projection.ZZ1_ITMTYPE_MSE = _ZZ1_ITMTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ITMTYPE_MSE'
  _Extension.ZZ1_ITMTYPE_MSE as ZZ1_ITMTYPE_MSE,
  _ZZ1_ITMTYPE_MSE
}
