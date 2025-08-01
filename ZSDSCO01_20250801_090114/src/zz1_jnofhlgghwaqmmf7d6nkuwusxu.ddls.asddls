@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_6000F1AED9A7'

extend view I_GLACCOUNTLINEITEMSEMTAG with ZZ1_JNOFHLGGHWAQMMF7D6NKUWUSXU
    association [0..1] to ZZ1_ITMTYPE_V as _ZZ1_ITMTYPE_MSE
  on  $projection.ZZ1_ITMTYPE_MSE = _ZZ1_ITMTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ITMTYPE_MSE'
  _Extension.ZZ1_ITMTYPE_MSE as ZZ1_ITMTYPE_MSE,
  _ZZ1_ITMTYPE_MSE
}
