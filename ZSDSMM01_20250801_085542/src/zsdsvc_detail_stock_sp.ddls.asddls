@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Detail Stock SP'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_DETAIL_STOCK_SP
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
  association [1..1] to t001l as f on f.werks = a.werks and
                                      f.lgort = a.lgort
  association [0..1] to mbew  as g on g.matnr = b.matnr and
                                      g.bwkey = '1000' and
                                      g.bwtar = ''
{
  key b.matnr,
      b.mtart,
      c.maktx,
      b.zzcav,
      substring( b.prdha, 1, 5 )                     as ph1,
      substring( b.prdha, 6, 5 )                     as ph2,
      substring( b.prdha, 11, 8 )                    as ph3,
      a.lgort,
      f.lgobe,
      @Semantics.amount.currencyCode: 'konwa'
      e.kbetr,
      @Semantics.amount.currencyCode: 'konwa'
      g.verpr,
      @Semantics.amount.currencyCode: 'konwa'
      g.stprs,
      e.konwa,
      @Semantics.quantity.unitOfMeasure: 'meins'
      m.umlme,
      @Semantics.quantity.unitOfMeasure: 'meins'
      m.labst,
      @Semantics.quantity.unitOfMeasure: 'meins'
      m.speme,
      b.meins
}
where
  a.werks = $parameters.p_plant and
  ( 
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
