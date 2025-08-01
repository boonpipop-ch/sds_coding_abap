@AbapCatalog.sqlViewName: 'ZSDSVC_BNKA_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Bank Master'
define view ZSDSVC_BNKA as select from bnka
{
    key banks as bankCtry,
    key bankl as bankKeys,
    banka as bankName,
    bnklz as bankNo,
    brnch as bankBranch
}
