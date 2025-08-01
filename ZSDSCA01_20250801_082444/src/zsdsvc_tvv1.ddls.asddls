@AbapCatalog.sqlViewName: 'ZSDSVC_TVV1_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Group 1'
define view ZSDSVC_TVV1 as select from tvv1t
{
    key kvgr1 as customerGrp1,
    bezei as description
}
where spras = 'E'
