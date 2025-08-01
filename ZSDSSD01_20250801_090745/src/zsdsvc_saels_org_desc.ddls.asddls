@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Organization Description'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_SAELS_ORG_DESC
  as select from vbrp as h
  inner join vbrk as a on a.vbeln = h.vbeln
  association [0..1] to tvgrt as b on  b.vkgrp = h.vkgrp
                                   and b.spras = $session.system_language
  association [0..1] to tvkbt as c on  c.vkbur = h.vkbur
                                   and c.spras = $session.system_language
  association [0..1] to tvtwt as d on  d.vtweg = a.vtweg
                                   and d.spras = $session.system_language
  association [0..1] to tvkot as e on  e.vkorg = a.vkorg
                                   and e.spras = $session.system_language
  association [0..1] to tspat as f on  f.spart = a.spart
                                   and f.spras = $session.system_language
  association [0..1] to tvlvt as g on  g.abrvw = h.abrvw
                                   and g.spras = $session.system_language
{
  key h.vbeln,
      h.posnr,
      b.bezei as grpds,
      c.bezei as offds,
      d.vtweg as chads,
      e.vtext as orgds,
      f.vtext as divis,
      g.bezei as appli
}
