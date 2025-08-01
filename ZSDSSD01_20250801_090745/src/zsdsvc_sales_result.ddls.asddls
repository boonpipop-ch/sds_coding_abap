@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data Sales Result'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #L,
    dataClass: #TRANSACTIONAL
}
define view entity ZSDSVC_SALES_RESULT
  with parameters
    P_VKORG : vkorg
  as select from           vbrk as a
    left outer to one join bkpf as o on a.vbeln = o.awkey
    inner join             kna1 as m on a.kunag = m.kunnr
    inner join             knb1 as l on  a.kunag = l.kunnr
                                     and l.bukrs = $parameters.P_VKORG
    inner join             vbrp as b on a.vbeln = b.vbeln
    left outer to one join vbap as e on  b.aubel = e.vbeln
                                     and b.aupos = e.posnr
    left outer to one join vbak as f on b.aubel = f.vbeln
    left outer to one join vbpa as d on  b.vbeln = d.vbeln
                                     and b.posnr = d.posnr
                                     and d.parvw = 'VE'
    left outer to one join vbpa as p on  b.vbeln = p.vbeln
                                     and b.posnr = p.posnr
                                     and d.parvw = 'ZM'
    left outer to one join but000 as bp on a.kunag = bp.partner
  //    left outer to one join lips as t on b.vgbel = t.vbeln and
  //                                        b.vgpos = t.posnr
    left outer to one join likp as u on b.vgbel = u.vbeln
  //    left outer to one join vttp as v on v.vbeln = b.vgbel and
  //                                        v.tpnum = substring( b.posnr, 3, 4 )
  //    left outer to one join vttk as w on w.tknum = v.tknum
  //    left outer to one join ZSDSVC_CUSTOMER_INFO  as n on  m.kunnr = n.kunnr
    left outer to one join vbkd as c on  c.vbeln = b.vbeln
                                     and c.posnr = b.posnr
    left outer to one join prps as w on  w.pspnr = b.ps_psp_pnr
  association [0..1] to ZSDSVC_SAELS_ORG_DESC as g on  g.vbeln = b.vbeln
                                                  and  g.posnr = b.posnr
  association [0..1] to tvfkt                 as k on  k.fkart = a.fkart
                                                   and k.spras = $session.system_language
  association [0..1] to tkukt                 as i on  i.kukla = m.kukla
                                                   and i.spras = $session.system_language
  association [0..1] to tvv1t                 as n on  n.kvgr1 = f.kvgr1
                                                   and n.spras = $session.system_language
  association [0..1] to tvv2t                 as q on  q.kvgr2 = f.kvgr2
                                                   and q.spras = $session.system_language
  association [0..1] to tvaut                 as r on  r.augru = f.augru
                                                   and r.spras = $session.system_language     
  association [0..1] to t052u                 as v on  v.zterm = c.zterm
                                                   and v.ztagg is initial
                                                   and v.spras = $session.system_language
  association [0..1] to tvlat                 as t on  t.vstel = u.vstel
                                                   and t.lstel = u.lstel
                                                   and t.spras = $session.system_language
  association [0..1] to t001s                 as j on  j.busab = l.busab
                                                   and j.bukrs = $parameters.P_VKORG
  association [0..1] to vbak                  as s on  s.vbeln = e.vgbel
  association [0..1] to proj                  as x on  x.pspnr = w.psphi
  association [1..1] to mara                  as y on  y.matnr = b.matnr
{
  key b.vbeln,
  key b.posnr,
      o.belnr,
      o.gjahr,
      a.vtweg,
      b.spart,
      b.vkbur,
      b.vkgrp,
      a.kunag,
      a.fkart,
      a.fkdat,
      b.matnr,
      b.prodh,
      d.pernr,
      p.pernr as perzm,
      c.bstkd,
      b.kvgr2,
      f.bname,
      g.grpds,
      g.offds,
      g.chads,
      g.orgds,
      g.divis,
      g.appli,
      k.vtext as bitds,
      b.aubel,
      b.aupos,
      f.auart,
      f.audat,
      f.ernam,
      c.ihrez,
      c.zterm,
      b.waerk,
      c.bstkd_e,
      a.xblnr,
      m.telf2,
      m.erdat,
      l.busab,
      l.bukrs,
      m.kukla,
      i.vtext as csfds,
      j.sname as accds,
      //      n.namth,
      //      n.adr1th,
      //      n.adr2th,
      //      n.namen,
      //      n.adr1en,
      //      n.adr2en,
      //      n.CITY1TH,
      //      n.POST_CODE1TH,
      //      n.TEL_NUMBERTH,
      //      n.CITY1EN,
      //      n.POST_CODE1EN,
      //      n.TEL_NUMBEREN,
      ////      n.smtp_addr,
      //      n.partner2,
      b.uepos,
      b.arktx,
      b.vrkme,
      @Semantics.quantity.unitOfMeasure: 'VRKME'
      b.fkimg,
      b.gewei,
      @Semantics.quantity.unitOfMeasure: 'GEWEI'
      b.ntgew,
      @Semantics.quantity.unitOfMeasure: 'GEWEI'
      b.brgew,
      b.voleh,
      @Semantics.quantity.unitOfMeasure: 'VOLEH'
      b.volum,
      b.prctr,
      b.ps_psp_pnr,
      @Semantics.amount.currencyCode: 'WAERK'
      b.netwr,
      @Semantics.amount.currencyCode: 'WAERK'
      b.mwsbp,
      n.bezei as cusg1,
      q.bezei as cusg2,
      f.kvgr1,
      r.bezei as ordrs,
      e.vgbel,
      e.vgpos,
      s.auart as quoty,
      u.vbeln as donum,
      u.erdat as docre,
      u.erzet as dotim,
      u.lfdat as dodat,
      u.lstel,
      u.vstel,
      t.vtext as lodpt,
      v.text1 as paytm,
      a.knumv,
      x.post1,
      b.pstyv,
      b.upmat,
      b.kowrr,
      y.zzreft,
      y.mfrnr,
      y.zzscf,
      s.zzpob,
      //      w.tknum,
      //      w.vsart,
      //      w.dtdis,
      //      w.route,
      //      w.uzdis,
      //      w.exti1,
      //      w.exti2,
      //      w.daten,
      //      w.uaten
      bp.bu_group
}
where
      a.vkorg = $parameters.P_VKORG
  and a.vbtyp <> 'EBDR'
  and o.belnr <> ''
