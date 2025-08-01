@AbapCatalog.sqlViewName: 'ZSDSVC_CHKSTK_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'For Check Available Stock'
define view ZSDSVC_CHKSTK
  with parameters
    p_plant : werks_d,
    p_chanl : vtweg
  as select from        mara as b   
    inner join mard as a on b.matnr = a.matnr
    left outer to one join nsdm_e_mard_diff as m on  a.mandt = m.mandt
                                                 and a.matnr = m.matnr
                                                 and a.werks = m.werks
                                                 and a.lgort = m.lgort
    left outer to one join a004 as d on b.matnr = d.matnr and
                                        d.kappl = 'V' and
                                        d.kschl = 'ZPR0' and
                                        d.vkorg = '1000' and
                                        d.vtweg = :p_chanl and
                                        d.datbi >= $session.system_date                                             
 // association [1..1] to mara as b on b.matnr = a.matnr
  association [1..1] to makt as c on c.matnr = b.matnr and
                                     c.spras = 'E'
  association [0..1] to konp as e on e.knumh = d.knumh and
                                     e.loevm_ko = ''
  association [1..1] to t001l as f on f.werks = a.werks and
                                      f.lgort = a.lgort
{
  key b.matnr,
      c.maktx,
      b.zzcav,
      substring( b.prdha, 1, 5 )                     as ph1,
      substring( b.prdha, 6, 5 )                     as ph2,
      substring( b.prdha, 11, 8 )                    as ph3,
      a.lgort,
      f.lgobe,
      e.kbetr,
      case when m.umlme is null then 0 else m.umlme end as umlme,
      case when m.labst is null then 0 else m.labst end as labst,
      case when m.speme is null then 0 else m.speme end as speme
}
where
  a.werks = :p_plant
