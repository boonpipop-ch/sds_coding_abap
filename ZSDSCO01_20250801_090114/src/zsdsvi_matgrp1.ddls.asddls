@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Group 1'
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
define view entity ZSDSVI_MATGRP1 
  as select from TVM1
  association [0..*] to ZSDSVI_MATGRP1T as _Text on $projection.Code = _Text.Code
{
      @ObjectModel.text.association: '_Text'
      @Search.defaultSearchElement: true
  key MVGR1 as Code,

      @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
      _Text  
}
