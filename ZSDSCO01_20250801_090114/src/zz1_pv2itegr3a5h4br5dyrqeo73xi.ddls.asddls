@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_45317417CFFB'

extend view I_MARGANLYSPRODANDSRVCCUBE with ZZ1_PV2ITEGR3A5H4BR5DYRQEO73XI
    association [0..1] to ZZ1_ACTTYPE_V as _ZZ1_ACTTYPE_MSE
  on  $projection.ZZ1_ACTTYPE_MSE = _ZZ1_ACTTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ACTTYPE_MSE'
  _Extension.ZZ1_ACTTYPE_MSE as ZZ1_ACTTYPE_MSE,
  _ZZ1_ACTTYPE_MSE
}
