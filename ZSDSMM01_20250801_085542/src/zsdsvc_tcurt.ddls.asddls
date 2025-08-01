@AbapCatalog.sqlViewName: 'ZSDSVC_TCURT_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Currency Master'
define view ZSDSVC_TCURT as select from tcurt
{
    key waers,
        ltext
} where spras = 'E';
