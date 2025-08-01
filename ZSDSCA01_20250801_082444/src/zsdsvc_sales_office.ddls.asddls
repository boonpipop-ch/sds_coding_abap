@AbapCatalog.sqlViewName: 'ZSDSVC_SOFFICE_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Sales Office'
define view ZSDSVC_SALES_OFFICE as select from tvbur
inner join tvkbt
on tvbur.vkbur = tvkbt.vkbur
{
    key tvbur.vkbur as salesOffice,
    tvkbt.bezei as description
    
}
where tvbur.hide <> 'X'
  and tvkbt.spras = 'E'
