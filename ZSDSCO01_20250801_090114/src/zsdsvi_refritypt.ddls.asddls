@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.representativeKey: 'Code'
@ObjectModel.dataCategory: #TEXT
@EndUserText.label: 'Refrigerant Type Text'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #CUSTOMIZING
}
define view entity ZSDSVI_REFRITYPT
  as select from zdsmmc006
  association [1..1] to ZSDSVI_REFRITYP as _Code on $projection.Code = _Code.Code
  association [0..1] to I_Language       as _Language on $projection.Language = _Language.Language  
{
      @Search.defaultSearchElement: true
  key refri_type  as Code,
      @Semantics.language: true
      @ObjectModel.foreignKey.association: '_Language'
  key cast('E' as abap.lang) as Language,
      @Search.defaultSearchElement: true
      @Semantics.text: true
      description as Description,
      @ObjectModel.association.type: [#TO_COMPOSITION_ROOT, #TO_COMPOSITION_PARENT]
      _Code,
      _Language
      
}
