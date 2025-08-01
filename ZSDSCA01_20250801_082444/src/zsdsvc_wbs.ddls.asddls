@AbapCatalog.sqlViewName: 'ZSDSVC_WBS_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for WBS Master'
define view ZSDSVC_WBS as select from prps
{ 
  key pspnr,
      posid as wbs,
      post1 as description,
      stufe as wbs_level
    
}
where prps.stufe = 1

