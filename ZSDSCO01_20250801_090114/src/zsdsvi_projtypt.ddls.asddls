@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.representativeKey: 'Code'
@ObjectModel.dataCategory: #TEXT
@EndUserText.label: 'Project Types Text'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #CUSTOMIZING
}
define view entity ZSDSVI_PROJTYPT
  as select from TCJ1T
  association [1..1] to ZSDSVI_PROJTYP as _Code     on $projection.Code = _Code.Code
  association [0..1] to I_Language     as _Language on $projection.Language = _Language.Language
{
      @Search.defaultSearchElement: true
  key PRART as Code,
      @Semantics.language: true
      @ObjectModel.foreignKey.association: '_Language'
  key LANGU as Language,
      @Search.defaultSearchElement: true
      @Semantics.text: true
      PRATX as Description,
      @ObjectModel.association.type: [#TO_COMPOSITION_ROOT, #TO_COMPOSITION_PARENT]
      _Code,
      _Language
}
