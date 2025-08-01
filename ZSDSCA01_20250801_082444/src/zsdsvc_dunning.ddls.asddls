@AbapCatalog.sqlViewName: 'ZSDSVC_DUNNING_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Dunning Procedure'
define view ZSDSVC_DUNNING as select from t047t
{
    key mahna as dunning,
    textm as description
}
where spras = 'E'
