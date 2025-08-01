@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_9FDE82559B80'

extend view I_GLACCTBALANCECUBE with ZZ1_ADWRM2OBKU7WGO3NLLK2AUFPKE
    association [0..1] to ZZ1_ZZPHA_V as _ZZ1_ZZPHA_MSE
  on  $projection.ZZ1_ZZPHA_MSE = _ZZ1_ZZPHA_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPHA_MSE'
  _Extension.ZZ1_ZZPHA_MSE as ZZ1_ZZPHA_MSE,
  _ZZ1_ZZPHA_MSE
}
