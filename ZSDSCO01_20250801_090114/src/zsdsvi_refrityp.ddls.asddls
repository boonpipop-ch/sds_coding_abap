@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Refrigerant Type'
@Search.searchable
@ObjectModel.compositionRoot: true
@Analytics.dataCategory: #DIMENSION
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #CUSTOMIZING
}
@ObjectModel.representativeKey: 'Code'
define view entity ZSDSVI_REFRITYP
  as select from zdsmmc006
  association [0..*] to ZSDSVI_REFRITYPT as _Text on $projection.Code = _Text.Code
{
      @ObjectModel.text.association: '_Text'
      @Search.defaultSearchElement: true
  key refri_type as Code,

      @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
      _Text
}
