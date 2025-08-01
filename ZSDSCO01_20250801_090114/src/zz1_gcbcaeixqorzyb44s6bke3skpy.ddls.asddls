@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_31706DF38001'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_GCBCAEIXQORZYB44S6BKE3SKPY
    association [0..1] to ZZ1_SERVTYPE_V as _ZZ1_SERVTYPE_MSE
  on  $projection.ZZ1_SERVTYPE_MSE = _ZZ1_SERVTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SERVTYPE_MSE'
  _Extension.ZZ1_SERVTYPE_MSE as ZZ1_SERVTYPE_MSE,
  _ZZ1_SERVTYPE_MSE
}
