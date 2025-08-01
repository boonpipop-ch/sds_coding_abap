@AbapCatalog.sqlViewName: 'ZSDSVC_TVV2_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Group 2'
define view ZSDSVC_TVV2 as select from tvv2t
{
    key kvgr2 as customerGrp2,
    bezei as description
}
where spras = 'E'
