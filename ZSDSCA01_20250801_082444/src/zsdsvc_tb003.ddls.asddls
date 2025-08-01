@AbapCatalog.sqlViewName: 'ZSDSVC_TB003_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for BP Roles'
define view ZSDSVC_TB003 as select from tb003t
{
    key role,
    rltitl,
    rltxt
}
where spras = 'E'
  and ( role = 'FLCU01'
     or role = 'FLVN01' 
     or role = 'BUP003' )
