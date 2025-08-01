@AbapCatalog.sqlViewName: 'ZSDSVC_CSKT_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cost Center Text'
define view ZSDSVC_CSKT as select from cskt
{
    key kostl,
        ktext,
        ltext,
        mctxt
} where spras = 'E'
    and datbi >= $session.system_date
    and kokrs = '1000'
