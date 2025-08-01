@AbapCatalog.sqlViewName: 'ZSDSV_WL_INV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for warranty letter expand invoice'
define view ZSDSVC_WL_INV as select from zsdscmt007
{
    key wl_id,
    inv_no
}
