@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label:       'ZZ1_ITMTYPE'
@AbapCatalog.sqlViewName: 'ZZ1_C42A960A4D6D'
@Search.searchable
@ObjectModel.compositionRoot: true
@Analytics.dataCategory: #DIMENSION
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.dataClass: #CUSTOMIZING
@ObjectModel.representativeKey: 'Code'
define view ZZ1_ITMTYPE_V
  as select from ZZ1_D7423E3502F6
  association [0..*] to ZZ1_ITMTYPE_W
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
