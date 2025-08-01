@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_5B5966365029'

extend view E_FINANCIALPLANNINGENTRYITEM with ZZ1_4UTT5BEB56FAQGBT2HDDI5SAQE
    association [0..1] to ZZ1_ZZIUT_V as _ZZ1_ZZIUT_MSE
  on  $projection.ZZ1_ZZIUT_MSE = _ZZ1_ZZIUT_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ZZIUT_MSE'
  Persistence.ZZ1_ZZIUT_MSE as ZZ1_ZZIUT_MSE,
  _ZZ1_ZZIUT_MSE
}
