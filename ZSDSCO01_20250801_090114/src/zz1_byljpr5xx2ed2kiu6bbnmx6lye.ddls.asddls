@AbapCatalog.internal.setChange: 'FLDADD_NO_ASS_INFLUENCE'
@AbapCatalog.sqlViewAppendName: 'ZZ1_5074751247FC'

extend view I_GLACCTBALANCECUBE with ZZ1_BYLJPR5XX2ED2KIU6BBNMX6LYE
    association [0..1] to ZZ1_ITMTYPE_V as _ZZ1_ITMTYPE_MSE
  on  $projection.ZZ1_ITMTYPE_MSE = _ZZ1_ITMTYPE_MSE.Code 
 
{ 
@ObjectModel.foreignKey.association: '_ZZ1_ITMTYPE_MSE'
  _Extension.ZZ1_ITMTYPE_MSE as ZZ1_ITMTYPE_MSE,
  _ZZ1_ITMTYPE_MSE
}
