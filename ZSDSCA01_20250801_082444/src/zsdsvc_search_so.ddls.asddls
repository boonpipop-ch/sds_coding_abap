@AbapCatalog.sqlViewName: 'ZSDSSVC_SSO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Search Sales Order'
define view ZSDSVC_SEARCH_SO as select from vbak
{
    key vbeln as salesOrder,
        bstnk as purchaseOrder
}
