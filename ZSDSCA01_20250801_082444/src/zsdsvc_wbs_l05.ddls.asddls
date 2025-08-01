@AbapCatalog.sqlViewName: 'ZSDSVC_WBS05'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for WBS Master'
define view ZSDSVC_WBS_L05 as select from prps as lv05
left outer join rpsco on lv05.objnr = rpsco.objnr
{ 
  key lv05.pspnr,
      lv05.posid as wbs,
      lv05.post1 as description,
      lv05.stufe as wbs_level,
      lv05.poski as short_id,
      lv05.objnr as objno,
      lv05.prctr as profitCenter,
      rpsco.wlp00,
      ( rpsco.wlp01 + 
        rpsco.wlp02 +
        rpsco.wlp03 + 
        rpsco.wlp04 + 
        rpsco.wlp05 + 
        rpsco.wlp06 + 
        rpsco.wlp07 +
        rpsco.wlp08 + 
        rpsco.wlp09 +
        rpsco.wlp10 + 
        rpsco.wlp11 + 
        rpsco.wlp12 +
        rpsco.wlp13 + 
        rpsco.wlp14 + 
        rpsco.wlp15 + 
        rpsco.wlp16 ) as total
}
where lv05.stufe = 4
  and lv05.poski like 'P%05-'
  and ( rpsco.vorga = 'RKP1' and 
        rpsco.versn = '000'  )
