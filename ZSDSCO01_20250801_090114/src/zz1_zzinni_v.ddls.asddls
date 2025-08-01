@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label:       'ZZ1_ZZINNI'
@AbapCatalog.sqlViewName: 'ZZ1_324C8F4783E1'
@Search.searchable
@ObjectModel.compositionRoot: true
@Analytics.dataCategory: #DIMENSION
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.dataClass: #CUSTOMIZING
@ObjectModel.representativeKey: 'Code'
define view ZZ1_ZZINNI_V
  as select from ZZ1_EAF1E4DF6B31
  association [0..*] to ZZ1_ZZINNI_W
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
