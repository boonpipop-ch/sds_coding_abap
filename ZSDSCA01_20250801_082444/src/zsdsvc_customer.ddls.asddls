@AbapCatalog.sqlViewName: 'ZSDSVC_CUST_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Master'
define view ZSDSVC_CUSTOMER as select from kna1
    left outer join knvv on kna1.kunnr = knvv.kunnr
    left outer join knb5 on kna1.kunnr = knb5.kunnr
                        and knb5.bukrs = '1000'
    left outer join knb1 on kna1.kunnr = knb1.kunnr
                        and knb1.bukrs = '1000'
    left outer join knvi on kna1.kunnr = knvi.kunnr
                        and knvi.aland = 'TH'
                        and knvi.tatyp = 'MWST'
    left outer join adrc on kna1.adrnr = adrc.addrnumber
{
    key kna1.kunnr,
        kna1.adrnr,
        kna1.aufsd as soAll,
        kna1.lifsd as doAll,
        kna1.faksd as billAll,
        knvv.aufsd as soSel,
        knvv.lifsd as doSel,
        knvv.faksd as billSel,
        adrc.name1 as name1,
        adrc.name2 as name2,
        adrc.name3 as name3,
        adrc.name4 as name4,
            concat( adrc.name1,
                concat( adrc.name2,
                    concat( adrc.name3, adrc.name4 ))) as orgName,
                    
            concat_with_space( adrc.name1, adrc.name2, 1 ) as perName
}
where adrc.nation = 'I'
