@AbapCatalog.sqlViewName: 'ZSDSVC_T005T_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Country'
define view ZSDSVC_T005T as select from t005t
{
    key land1 as country,
    landx50   as description
}
where spras = 'E'
