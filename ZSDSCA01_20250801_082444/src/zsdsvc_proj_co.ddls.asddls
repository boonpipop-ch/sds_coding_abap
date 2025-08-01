@AbapCatalog.sqlViewName: 'ZSDSVC_PRJCO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Project info'
define view ZSDSVC_PROJ_CO as select from rpsco
inner join prps on prps.objnr = rpsco.objnr
{
  key prps.posid,
  key rpsco.objnr,
  key rpsco.lednr,
  key rpsco.wrttp,
  key rpsco.trgkz,
  key rpsco.gjahr,
  key rpsco.acpos,
  key rpsco.vorga,
  key rpsco.versn,
  key rpsco.abkat,
  key rpsco.geber,
  key rpsco.twaer,
  key rpsco.perbl,
          
  rpsco.gjahr as fiscal_yr,
  rpsco.vorga as budget_typ,
  rpsco.wlp00 as period_tt,
  rpsco.wlp01 as period_01,
  rpsco.wlp02 as period_02,
  rpsco.wlp03 as period_03,
  rpsco.wlp04 as period_04,
  rpsco.wlp05 as period_05,
  rpsco.wlp06 as period_06,
  rpsco.wlp07 as period_07,
  rpsco.wlp08 as period_08,
  rpsco.wlp09 as period_09,
  rpsco.wlp10 as period_10,
  rpsco.wlp11 as period_11,
  rpsco.wlp12 as period_12,
  rpsco.wlp13 as period_13,
  rpsco.wlp14 as period_14,
  rpsco.wlp15 as period_15,
  rpsco.wlp16 as period_16
}

