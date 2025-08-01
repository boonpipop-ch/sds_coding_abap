@AbapCatalog.sqlViewName: 'ZSDSVC_TAXC_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Tax classification for customer'
define view ZSDSVC_TAX_CLASS as select from tskd
inner join tskdt
on  tskd.tatyp = tskdt.tatyp
and tskd.taxkd = tskdt.taxkd
{
    key tskd.tatyp as taxType,
    key tskd.taxkd as taxClass,
    tskdt.vtext as description 
}
where tskdt.tatyp = 'MWST'
  and  tskdt.spras = 'E'
