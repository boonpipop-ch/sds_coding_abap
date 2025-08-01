@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Available Stock Sum'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_SUM_STOCK 
with parameters
    p_plant : werks_d,
    p_chanl : vtweg
  as select from        mara as b   
    inner join mard as a on b.matnr = a.matnr
    inner join zsdsmmc002 as h on b.mtart = h.mtart and
                                  a.lgort = h.lgort and
                                  h.werks = $parameters.p_plant
    left outer to one join nsdm_e_mard_diff as m on  a.matnr = m.matnr
                                                 and a.werks = m.werks
                                                 and a.lgort = m.lgort
    left outer to one join a004 as d on b.matnr = d.matnr and
                                        d.kappl = 'V' and
                                        d.kschl = 'ZPR0' and
                                        d.vkorg = '1000' and
                                        d.vtweg = $parameters.p_chanl and
                                        d.datbi >= $session.system_date                                             
 // association [1..1] to mara as b on b.matnr = a.matnr
  association [1..1] to makt as c on c.matnr = b.matnr and
                                     c.spras = 'E'
  association [0..1] to konp as e on e.knumh = d.knumh and
                                     e.loevm_ko = ''
  association [0..1] to mbew  as f on f.matnr = b.matnr and
                                      f.bwkey = '1000' and
                                      f.bwtar = ''                             
{
  key b.matnr,
      b.mtart,
      c.maktx,
      b.zzcav,
      substring( b.prdha, 1, 5 )                     as ph1,
      substring( b.prdha, 6, 5 )                     as ph2,
      substring( b.prdha, 11, 8 )                    as ph3,
      @Semantics.amount.currencyCode: 'konwa'
      e.kbetr,
      @Semantics.amount.currencyCode: 'konwa'
      f.verpr,
      @Semantics.amount.currencyCode: 'konwa'
      f.stprs,
      e.konwa, 
      @Semantics.quantity.unitOfMeasure: 'meins'
      sum(m.umlme) as umlme,
      @Semantics.quantity.unitOfMeasure: 'meins'
      sum(m.labst) as labst,
      @Semantics.quantity.unitOfMeasure: 'meins'
      sum(m.speme) as speme,
      b.meins
}
where
  a.werks = $parameters.p_plant  and
  ( m.lgort between '1000' and '1999' )
group by b.matnr,b.mtart,c.maktx,b.zzcav,b.prdha,e.kbetr,b.meins,e.konwa,f.verpr,f.stprs
 