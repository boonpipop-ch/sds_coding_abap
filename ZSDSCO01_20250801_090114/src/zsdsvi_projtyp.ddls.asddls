@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Project Types'
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
define view entity ZSDSVI_PROJTYP
  as select from TCJ1
  association [0..*] to ZSDSVI_PROJTYPT as _Text on $projection.Code = _Text.Code
{
      @ObjectModel.text.association: '_Text'
      @Search.defaultSearchElement: true
  key PRART as Code,

      @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
      _Text
}
