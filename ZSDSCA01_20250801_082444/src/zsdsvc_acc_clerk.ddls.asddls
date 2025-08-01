@AbapCatalog.sqlViewName: 'ZSDSVC_CLERK_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Serach help for Accounting Clerk Abbreviation'
define view ZSDSVC_ACC_CLERK as select from t001s
{
    key busab as accClerk,
    sname as description
}
    where bukrs = '1000'
