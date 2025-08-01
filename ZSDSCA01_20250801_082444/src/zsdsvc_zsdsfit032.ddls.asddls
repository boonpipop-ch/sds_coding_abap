@AbapCatalog.sqlViewName: 'ZSDSFIT032_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Mapping Reconcile Account and Plan.Grp'
define view ZSDSVC_ZSDSFIT032 as select from zsdsfit032
{
    key akont as reconcileAcc,
    key fdgrv as planningGrp,
    akont_desc as Description
}
