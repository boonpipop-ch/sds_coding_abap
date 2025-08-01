@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sum Stock SP'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_SUM_STOCK_SP
with parameters
    p_plant : werks_d,
    p_chanl : vtweg
  as select from        mara as b   
    inner join mard as a on b.matnr = a.matnr
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
  association [1..1] to t001l as g on g.werks = a.werks and
                                      g.lgort = a.lgort                          
{
  key b.matnr,
      b.mtart,
      c.maktx,
      b.zzcav,
      substring( b.prdha, 1, 5 )                     as ph1,
      substring( b.prdha, 6, 5 )                     as ph2,
      substring( b.prdha, 11, 8 )                    as ph3,
      a.lgort,
      g.lgobe,
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
  a.werks = $parameters.p_plant  and (
    m.lgort like 'A1%' or
    m.lgort like 'B1%' or
    m.lgort like 'C1%' or
    m.lgort like 'D1%' or
    m.lgort like 'E1%' or
    m.lgort like 'F1%' or
    m.lgort like 'G1%' or
    m.lgort like 'H1%' or
    m.lgort like 'I1%' or
    m.lgort like 'J1%' or
    m.lgort like 'K1%' or
    m.lgort like 'L1%' or
    m.lgort like 'M1%' or
    m.lgort like 'N1%' or
    m.lgort like 'O1%' or
    m.lgort like 'P1%' or
    m.lgort like 'Q1%' or
    m.lgort like 'R1%' or
    m.lgort like 'S1%' or
    m.lgort like 'T1%' or
    m.lgort like 'U1%' or
    m.lgort like 'V1%' or
    m.lgort like 'W1%' or
    m.lgort like 'X1%' or
    m.lgort like 'Y1%' or
    m.lgort like 'Z1%' or
    m.lgort like 'B9%'  )
group by b.matnr,b.mtart,c.maktx,b.zzcav,b.prdha,e.kbetr,b.meins,e.konwa,f.verpr,f.stprs,a.lgort,g.lgobe
