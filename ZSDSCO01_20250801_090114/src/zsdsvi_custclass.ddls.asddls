@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Classifications'
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
define view entity ZSDSVI_CUSTCLASS as select from TKUKL
  association [0..*] to ZSDSVI_CUSTCLASST as _Text on $projection.Code = _Text.Code
{
      @ObjectModel.text.association: '_Text'
      @Search.defaultSearchElement: true
  key KUKLA as Code,

      @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
      _Text    
}
