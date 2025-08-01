@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_CCF4581EB5D2'

extend view I_JOURNALENTRYITEMCUBE with ZZ1_J627BKQUGHQ2N45ZUHRWFZ23BU
    association [0..1] to ZZ1_SDSDIST_V as _ZZ1_SDSDIST_MSE
  on  $projection.ZZ1_SDSDIST_MSE = _ZZ1_SDSDIST_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_SDSDIST_MSE'
  _Extension.ZZ1_SDSDIST_MSE as ZZ1_SDSDIST_MSE,
  _ZZ1_SDSDIST_MSE
}
