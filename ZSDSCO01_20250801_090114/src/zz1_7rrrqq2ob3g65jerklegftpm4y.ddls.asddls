@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_534050341CC6'

extend view I_PROFITABILITYCUBE with ZZ1_7RRRQQ2OB3G65JERKLEGFTPM4Y
    association [0..1] to ZZ1_FISCYR_V as _ZZ1_FISCYR_MSE
  on  $projection.ZZ1_FISCYR_MSE = _ZZ1_FISCYR_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_FISCYR_MSE'
  _Extension.ZZ1_FISCYR_MSE as ZZ1_FISCYR_MSE,
  _ZZ1_FISCYR_MSE
}
