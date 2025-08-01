@AbapCatalog.sqlViewName: 'ZSDSVC_T006A_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base Unit Master'
define view ZSDSVC_T006A as select from t006a
{
    key msehi,
        msehl
} where spras = 'E';
