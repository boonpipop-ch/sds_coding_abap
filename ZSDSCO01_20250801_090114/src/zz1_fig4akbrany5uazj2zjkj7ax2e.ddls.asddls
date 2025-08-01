@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_FF2BD4D0E751'

extend view C_ALLOCATIONRUNRESULTITEM with ZZ1_FIG4AKBRANY5UAZJ2ZJKJ7AX2E
    association [0..1] to ZZ1_ZZIUT_V as _ZZ1_ZZIUT_MSE
  on  $projection.ZZ1_ZZIUT_MSE = _ZZ1_ZZIUT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZIUT_MSE'
  _Extension.ZZ1_ZZIUT_MSE as ZZ1_ZZIUT_MSE,
  _ZZ1_ZZIUT_MSE
}
