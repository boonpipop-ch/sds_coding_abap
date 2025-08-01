@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_FE797FDF4D9B'

extend view E_FINPLANNINGDELETEITEM with ZZ1_RUZTJQF2AZLSEGU366OSI2CRQQ
    association [0..1] to ZZ1_ZZPHA_V as _ZZ1_ZZPHA_MSE
  on  $projection.ZZ1_ZZPHA_MSE = _ZZ1_ZZPHA_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPHA_MSE'
  Persistence.ZZ1_ZZPHA_MSE as ZZ1_ZZPHA_MSE,
  _ZZ1_ZZPHA_MSE
}
