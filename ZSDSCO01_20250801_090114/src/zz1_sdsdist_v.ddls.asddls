@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label:       'ZZ1_SDSDIST'
@AbapCatalog.sqlViewName: 'ZZ1_0B30896AF6C3'
@Search.searchable
@ObjectModel.compositionRoot: true
@Analytics.dataCategory: #DIMENSION
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.dataClass: #CUSTOMIZING
@ObjectModel.representativeKey: 'Code'
define view ZZ1_SDSDIST_V
  as select from ZZ1_A1E815F95A56
  association [0..*] to ZZ1_SDSDIST_W
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
