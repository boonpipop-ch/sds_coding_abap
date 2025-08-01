@AbapCatalog.sqlViewName: 'ZSDSVC_T880_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Global Company Data'
define view ZSDSVC_T880 as select from t880
{
    key rcomp as company,
    name1
}
