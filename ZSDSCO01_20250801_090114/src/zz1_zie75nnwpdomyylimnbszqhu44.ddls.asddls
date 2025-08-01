@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_3A056B3AD53B'

extend view I_GLACCOUNTLINEITEMSEMTAG with ZZ1_ZIE75NNWPDOMYYLIMNBSZQHU44
    association [0..1] to ZZ1_PROJTYPE_V as _ZZ1_PROJTYPE_MSE
  on  $projection.ZZ1_PROJTYPE_MSE = _ZZ1_PROJTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_PROJTYPE_MSE'
  _Extension.ZZ1_PROJTYPE_MSE as ZZ1_PROJTYPE_MSE,
  _ZZ1_PROJTYPE_MSE
}
