@AbapCatalog.sqlViewName: 'ZSDSVC_SGRP_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Sales Group'
define view ZSDSVC_SALES_GRP as select from tvkgr
inner join tvgrt 
on tvkgr.vkgrp = tvgrt.vkgrp
{
    key tvkgr.vkgrp as salesGrp,
    tvgrt.bezei as description
}
where tvkgr.hide <> 'X'
  and tvgrt.spras = 'E'
