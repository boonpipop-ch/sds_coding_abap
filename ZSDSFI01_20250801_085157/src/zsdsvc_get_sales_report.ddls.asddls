@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: ' Sales Report by Accounting'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_GET_SALES_REPORT as select from acdoca
left outer to one join vbrk on acdoca.awref  = vbrk.vbeln
left outer to one join vbrp on acdoca.awref  = vbrp.vbeln and
                               acdoca.awitem = vbrp.posnr
left outer to one join matdoc on acdoca.awref  = matdoc.mblnr and
                                 acdoca.aworg  = matdoc.mjahr and
                                 acdoca.awitem = matdoc.line_id
left outer to one join vbrp as A on matdoc.vbeln_im = A.vgbel   and
                                    matdoc.vbelp_im = A.vgpos   
left outer to one join vbrk as C on A.vbeln    = C.vbeln
left outer to one join vbpa as f on  vbrp.vbeln = f.vbeln and
                                     vbrp.posnr = f.posnr and
                                     f.parvw = 'VE'
left outer to one join vbpa as g on  vbrp.vbeln = g.vbeln and 
                                     vbrp.posnr = g.posnr and 
                                     g.parvw = 'ZM'
left outer to one join kna1 as e on  acdoca.kunnr = e.kunnr
left outer to one join knvv as m on  C.kunrg = m.kunnr and 
                                     C.vkorg = m.vkorg and 
                                     C.vtweg = m.vtweg and 
                                     C.spart = m.spart
left outer to one join vbak as s on s.vbeln = vbrp.aubel  
association [1..1] to skat as b on  b.saknr = acdoca.racct
                           and b.ktopl = 'RCOA'
                           and b.spras = $session.system_language
association [1..1] to bkpf as d on  d.bukrs = acdoca.rbukrs and
                                    d.belnr = acdoca.belnr and 
                                    d.gjahr = acdoca.gjahr
//association [0..1] to kna1 as e on  e.kunnr = acdoca.kunnr
association [0..1] to ZSDSVC_SAELS_ORG_DESC as h on  h.vbeln = vbrp.vbeln
                                                and  h.posnr = vbrp.posnr
association [0..1] to ZSDSVC_SAELS_ORG_DESC as i on  i.vbeln = A.vbeln
                                                and  i.posnr = A.posnr
association [0..1] to vbrp as j on  j.vbeln = vbrp.vbeln
                               and  j.posnr = vbrp.uepos     
association [0..1] to vbrp as k on  k.vbeln = A.vbeln
                               and  k.posnr = A.uepos 
association [0..1] to tkukt as l on l.spras = $session.system_language
                               and  l.kukla = e.kukla     
association [0..1] to tvv1t as n on n.spras = $session.system_language
                               and  n.kvgr1 = m.kvgr1    
association [0..1] to tvv2t as o on o.spras = $session.system_language
                               and  o.kvgr2 = m.kvgr2  
association [0..1] to mara as p on p.matnr = matdoc.matnr      
association [0..1] to prps as q on q.pspnr = A.ps_psp_pnr    
association [0..1] to vbap as r on r.vbeln = vbrp.aubel
                               and r.posnr = vbrp.aupos      
association [0..1] to tvfkt as t on t.spras = $session.system_language 
                                and t.fkart = vbrk.fkart     
association [0..1] to tvakt as u on u.spras = $session.system_language 
                                and u.auart = s.auart    
association [0..1] to zz1_c00f10a53c25 as v on v.language = $session.system_language 
                                           and v.code = acdoca.zz1_zziut_mse                   
{
          key acdoca.rldnr,
          key acdoca.rbukrs,
          key acdoca.docln,
          key acdoca.belnr,
          key acdoca.gjahr,
           acdoca.budat,
           acdoca.bldat,
           acdoca.mwskz,
           acdoca.augbl,
           acdoca.auggj,
           @Semantics.amount.currencyCode: 'RTCUR'
           acdoca.tsl,
           acdoca.rtcur,
           @Semantics.amount.currencyCode: 'RHCUR'
           acdoca.hsl,
           acdoca.rhcur,
           @Semantics.quantity.unitOfMeasure: 'RUNIT'
           acdoca.msl,
           acdoca.runit,
           acdoca.kunnr,
           acdoca.netdt,
           acdoca.ps_posid,
           acdoca.rcntr,
           acdoca.prctr,
           acdoca.aufnr,
           acdoca.matnr,
           acdoca.racct,
           C.fkart as fkart_v,
           vbrk.fkart ,
           C.vkorg as vkorg_v,
           vbrk.vkorg ,
           C.vtweg as vtweg_v,
           vbrk.vtweg,
           A.vbeln as vbeln_v,
           vbrp.vbeln,
           A.vkgrp as vkgrp_v,
           vbrp.vkgrp,
           A.vkbur as vkbur_v,
           vbrp.vkbur,
           A.aubel as SONO_v,
           vbrp.aubel as SONO,
           A.vgbel as DONO_v,
           vbrp.vgbel as DONO,
//           case
//              when C.fkart is not initial then C.fkart
//              else vbrk.fkart
//           end as fkart,
//           case
//              when C.vkorg is not initial then C.vkorg 
//              else vbrk.vkorg 
//           end as vkorg,
//           case
//              when vbrk.vtweg is not initial then vbrk.vtweg
//              else C.vtweg
//           end as vtweg,
//           case
//              when A.vbeln is not initial then A.vbeln
//              else vbrp.vbeln 
//           end as VBELN,
//           case
//              when A.posnr is not initial then A.posnr
//              else vbrp.posnr
//           end as posnr,
//           case
//              when A.vkgrp is not initial then A.vkgrp
//              else vbrp.vkgrp 
//           end as vkgrp,
//           case
//              when A.vkbur is not initial then A.vkbur
//              else vbrp.vkbur
//           end as vkbur,
           case
              when acdoca.prodh_pa is not initial then acdoca.prodh_pa
              when vbrp.prodh is not initial then vbrp.prodh
              else A.prodh
           end as prodh,
//           case
//              when A.aubel is not initial then A.aubel
//              else vbrp.aubel
//           end as sono,
//           case
//              when A.vgbel is not initial then A.vgbel
//              else vbrp.vgbel
//           end as dono,
           b.txt50,
           d.blart,
           concat_with_space(concat_with_space(concat_with_space(e.name1,e.name2,1),e.name3,1),e.name4,1) as namea,
           acdoca.regio_pa,
           acdoca.zz1_projtype_mse,
           acdoca.zz1_zzcav_mse,
           acdoca.zz1_zzinni_mse,
           acdoca.zz1_zziut_mse,
           acdoca.zz1_itmtype_mse,
           acdoca.zz1_servtype_mse,
           acdoca.zz1_acttype_mse,
           acdoca.zz1_sdsdist_mse,
           acdoca.zz1_zzpha_mse,
           acdoca.zz1_proj_mse,
           acdoca.zz1_fiscyr_mse,
           acdoca.zz1_zzreftn_mse,
           acdoca.zz1_zzpmt_mse,
           acdoca.zz1_sseries,
           acdoca.zz1_zzreft,
           acdoca.zz1_shoptype,
           acdoca.zz1_appltn,
           acdoca.zz1_mvgr1,
           case
              when f.pernr is not initial then f.pernr
              else g.pernr
           end as pernr,
           h.grpds,
           h.offds,
           h.chads,
           h.orgds,
           h.divis,
           h.appli,
           i.grpds as grpds_m,
           i.offds as offds_m,
           i.chads as chads_m,
           i.orgds as orgds_m,
           i.divis as divis_m,
           i.appli as appli_m,
           vbrp.pstyv,
           A.pstyv as pstyv_v,
           j.matnr as matnr_bom,
           k.matnr as matnr_bom_v,
           l.vtext as kukla,
           n.kvgr1,
           n.bezei as KVGR1_D,
           o.kvgr2,
           o.bezei as KVGR2_D,
           p.matkl,
           q.post1 as PROJECT_NAME,
           r.vgbel as QTNUM,
           t.vtext as BILL_TYPE_DESC,
           u.auart,
           u.bezei as SO_TYPE_DESC,
           v.description as FCU_DESC,
           acdoca.paph1_pa as PH1,
           acdoca.paph2_pa as PH2,
           acdoca.paph3_pa as PH3,
           acdoca.awref_rev,
           acdoca.xreversed
} where substring( acdoca.racct, 1, 1 ) = '4'
    and acdoca.racct <> '4021000010'  
