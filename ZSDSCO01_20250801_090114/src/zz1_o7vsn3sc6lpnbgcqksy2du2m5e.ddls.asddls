@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_08E8F2571643'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_O7VSN3SC6LPNBGCQKSY2DU2M5E
    association [0..1] to ZZ1_ZZPMT_V as _ZZ1_ZZPMT_MSE
  on  $projection.ZZ1_ZZPMT_MSE = _ZZ1_ZZPMT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPMT_MSE'
  _Extension.ZZ1_ZZPMT_MSE as ZZ1_ZZPMT_MSE,
  _ZZ1_ZZPMT_MSE
}
