@AbapCatalog.sqlViewName: 'ZSDSVC_MAT_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material List'
define view ZSDSVC_MAT_LIST as select from mara
left outer join makt
    on mara.matnr = makt.matnr
{ 
   key mara.matnr,
       makt.spras,
       makt.maktx,
       mara.mtart,
       mara.meins
} where lvorm = ''
