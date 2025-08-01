@AbapCatalog.sqlViewName: 'ZSDSVC_SHCOND_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Shipping Condition'
define view ZSDSVC_SHIP_COND as select from tvsbt
{
    key vsbed as shipCond,
    vtext     as description
}
where spras = 'E'
