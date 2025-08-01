@AbapCatalog.sqlViewName: 'ZSDSVC_VEND_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vendor Master'
define view ZSDSVC_VENDOR as select from lfa1
left outer join adrc on lfa1.adrnr = adrc.addrnumber
{
    key lfa1.lifnr,
    lfa1.adrnr,
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
