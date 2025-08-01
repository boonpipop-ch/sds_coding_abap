@AbapCatalog.sqlViewName: 'ZSDSVC_SORTKEY_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Sort Key'
define view ZSDSVC_SORT_KEY as select from tzunt
{
    key zuawa as sortKey,
    ttext     as description
}
where tzunt.spras = 'E'
