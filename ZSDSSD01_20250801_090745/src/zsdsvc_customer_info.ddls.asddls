@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Infomation'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_CUSTOMER_INFO
  as select from  kna1   as d
    inner join             adrc   as a on d.adrnr = a.addrnumber
    left outer to one join adrc   as b on  a.addrnumber = b.addrnumber
                                       and b.nation     = 'I'
                                       and b.date_from  <= $session.system_date
                                       and b.date_to    >= $session.system_date
    left outer to one join adr6   as c on c.addrnumber = a.addrnumber
    left outer to one join but050 as e on  d.kunnr     = e.partner1
                                       and e.date_from <= $session.system_date
                                       and e.date_to   >= $session.system_date
                                       and e.reltyp    = 'UKM001'
{
  key d.kunnr,
      a.addrnumber,
      concat_with_space(concat_with_space(concat_with_space(a.name1,a.name2,1),a.name3,1),a.name4,1)                                                                                                                                             as namth,
      concat_with_space(concat_with_space(a.street,a.str_suppl3,1),a.location,1)                                                                                                                                                                 as adr1th,
      concat_with_space(concat_with_space(concat_with_space(concat_with_space(a.str_suppl1,a.str_suppl2,1),a.city2,1),a.city1,1),a.post_code1,1)                                                                                                 as adr2th,
      a.city1                                                                                                                                                                                                                                    as CITY1TH,
      a.post_code1                                                                                                                                                                                                                               as POST_CODE1TH,
      a.tel_number                                                                                                                                                                                                                               as TEL_NUMBERTH,
      concat_with_space(concat_with_space(concat_with_space(b.name1,b.name2,1),b.name3,1),b.name4,1)                                                                                                                                             as namen,
      concat_with_space(concat_with_space(b.street,b.str_suppl3,1),b.location,1)                                                                                                                                                                 as adr1en,
      concat_with_space(concat_with_space(concat_with_space(concat_with_space(b.str_suppl1,b.str_suppl2,1),b.city2,1),b.city1,1),b.post_code1,1)                                                                                                 as adr2en,
      b.city1                                                                                                                                                                                                                                    as CITY1EN,
      b.post_code1                                                                                                                                                                                                                               as POST_CODE1EN,
      b.tel_number                                                                                                                                                                                                                               as TEL_NUMBEREN,
      c.smtp_addr,
      e.partner2,
      concat_with_space(concat_with_space(concat_with_space(concat_with_space(concat_with_space(concat_with_space(concat_with_space(a.street,a.str_suppl3,1),a.location,1),a.str_suppl1,1),a.str_suppl2,1),a.city2,1),a.city1,1),a.post_code1,1) as shpth
}
where
      a.nation    = ''
  and a.date_from <= $session.system_date
  and a.date_to   >= $session.system_date
  
//union all select from adrc as b
//{
//  key b.addrnumber,
//      b.nation,
//      concat_with_space(concat_with_space(concat_with_space(b.name1,b.name2,1),b.name3,1),b.name4,1)                                             as namth,
//      concat_with_space(concat_with_space(b.street,b.str_suppl3,1),b.location,1)                                                                 as adr1th,
//      concat_with_space(concat_with_space(concat_with_space(concat_with_space(b.str_suppl1,b.str_suppl2,1),b.city2,1),b.city1,1),b.post_code1,1) as adr2th
//}
//where
//      b.nation    = 'I'
//  and b.date_from <= $session.system_date
