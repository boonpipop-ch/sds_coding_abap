@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.representativeKey: 'Code'
@ObjectModel.dataCategory: #TEXT
@EndUserText.label: 'Customer classification: Texts'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #CUSTOMIZING
}
define view entity ZSDSVI_CUSTCLASST
  as select from TKUKT
  association [1..1] to ZSDSVI_CUSTCLASS as _Code     on $projection.Code = _Code.Code
  association [0..1] to I_Language       as _Language on $projection.Language = _Language.Language

{
      @Search.defaultSearchElement: true
  key KUKLA as Code,
      @Semantics.language: true
      @ObjectModel.foreignKey.association: '_Language'
  key spras as Language,
      @Search.defaultSearchElement: true
      @Semantics.text: true
      VTEXT as Description,
      @ObjectModel.association.type: [#TO_COMPOSITION_ROOT, #TO_COMPOSITION_PARENT]
      _Code,
      _Language
}
