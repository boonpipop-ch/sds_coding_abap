@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label:       'ZZ1_ACTTYPE'
@AbapCatalog.sqlViewName: 'ZZ1_EF348DB126EC'
@Search.searchable
@ObjectModel.compositionRoot: true
@Analytics.dataCategory: #DIMENSION
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.dataClass: #CUSTOMIZING
@ObjectModel.representativeKey: 'Code'
define view ZZ1_ACTTYPE_V
  as select from ZZ1_D79AF14EC740
  association [0..*] to ZZ1_ACTTYPE_W
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
