@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Check Serial'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_CHECK_SERIAL 
with parameters
    p_matnr : matnr,
    p_sernr : gernr

as select from objk as a
inner join ser01 as b on b.obknr = a.obknr 
inner join lips  as c on b.lief_nr = c.vbeln and
                         b.posnr   = c.posnr
inner join vbap  as d on c.vgbel = d.vbeln and
                         c.vgpos = d.posnr
inner join vbak  as f on d.vbeln = f.vbeln 
inner join tvgrt as g on f.vkgrp = g.vkgrp and 
                         g.spras = 'E' 
inner join kna1  as h on f.kunnr = h.kunnr
association [0..1] to adrc as i on i.addrnumber = h.adrnr and
                                   i.nation     = 'I' and
                                   i.date_from <= $session.system_date
{  
  key a.sernr,
  key a.matnr,
    b.datum, 
    f.kunnr,
    concat_with_space(concat_with_space(concat_with_space(h.name1,h.name2,1),h.name3,1),h.name4,1) as namth,
    concat_with_space(concat_with_space(concat_with_space(i.name1,i.name2,1),i.name3,1),i.name4,1) as namen,
    f.vkgrp,
    g.bezei
} where a.matnr = $parameters.p_matnr 
    and a.sernr = $parameters.p_sernr
    and a.taser = 'SER01' 
