@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_4535B4A681E4'

extend view E_JOURNALENTRYITEM with ZZ1_6BSKN7P527CGQSVDPQZXCOKY7A
    association [0..1] to ZZ1_ZZPMT_V as _ZZ1_ZZPMT_MSE
  on  $projection.ZZ1_ZZPMT_MSE = _ZZ1_ZZPMT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZPMT_MSE'
  Persistence.ZZ1_ZZPMT_MSE as ZZ1_ZZPMT_MSE,
  _ZZ1_ZZPMT_MSE
}
