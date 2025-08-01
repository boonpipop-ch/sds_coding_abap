@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Get Advance Receipt'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_GET_ADVANCE
  as select from           zsdssdt010 as a
    left outer to one join vbrp       as b on  a.vbeln = b.aubel
                                           and a.posnr = b.aupos
{
  key a.vbeln,
  key a.posnr,
  key a.etenr,
      a.bukrs,
      a.belnr,
      a.gjahr,
      a.waers,
      @Semantics.amount.currencyCode: 'waers'
      a.dmbtr,
      b.vbeln as invno,
      b.posnr as invit
}
