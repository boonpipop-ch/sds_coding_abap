@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_0857F2E38C59'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_JNNZE2LSZDS3VMZU72I2NNWTXY
    association [0..1] to ZZ1_ACTTYPE_V as _ZZ1_ACTTYPE_MSE
  on  $projection.ZZ1_ACTTYPE_MSE = _ZZ1_ACTTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ACTTYPE_MSE'
  _Extension.ZZ1_ACTTYPE_MSE as ZZ1_ACTTYPE_MSE,
  _ZZ1_ACTTYPE_MSE
}
