@AbapCatalog.sqlViewName: 'ZSDSVC_INCO_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Incoterm'
define view ZSDSVC_INCO as select from tinc
inner join tinct
on tinc.inco1 = tinct.inco1
{
    key tinc.inco1 as incoterm,
    tinct.bezei as description
}
where tinc.ortob = 'X'
  and tinct.spras = 'E'
