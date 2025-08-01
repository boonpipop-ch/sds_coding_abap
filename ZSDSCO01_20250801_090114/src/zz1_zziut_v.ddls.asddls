@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label:       'ZZ1_ZZIUT'
@AbapCatalog.sqlViewName: 'ZZ1_EFDADF73FB93'
@Search.searchable
@ObjectModel.compositionRoot: true
@Analytics.dataCategory: #DIMENSION
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.dataClass: #CUSTOMIZING
@ObjectModel.representativeKey: 'Code'
define view ZZ1_ZZIUT_V
  as select from ZZ1_4337AEEA9EF7
  association [0..*] to ZZ1_ZZIUT_W
    as _Text
    on $projection.Code = _Text.Code
{
  @ObjectModel.text.association: '_Text'
  @Search.DefaultSearchElement: true
  key CODE as Code,
  @UI.hidden: true
  IS_DISABLED as IsDisabled,
  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
  _Text
}
