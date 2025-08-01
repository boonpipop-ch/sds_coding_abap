@EndUserText.label: 'ZZ1_SDSDIST'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.representativeKey: 'Code'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.dataClass: #CUSTOMIZING
@AbapCatalog.sqlViewName: 'ZZ1_3A6E5C67893B'
@Search.searchable
define view ZZ1_SDSDIST_W
  as select from ZZ1_C024CF126913
association [1..1] to ZZ1_SDSDIST_V as _Code on $projection.Code = _Code.Code
association [0..1] to I_Language as _Language on $projection.Language = _Language.Language
{
  @Search.DefaultSearchElement: true
  key CODE     as Code,
  @Semantics.language: true
  @ObjectModel.foreignKey.association: '_Language'
  key LANGUAGE as Language,
  @Search.DefaultSearchElement: true
  @Semantics.text: true
  DESCRIPTION  as Description,
  @ObjectModel.association.type: [#TO_COMPOSITION_ROOT, #TO_COMPOSITION_PARENT]
  _Code,
  _Language
}
