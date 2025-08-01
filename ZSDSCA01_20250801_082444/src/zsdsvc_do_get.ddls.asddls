@AbapCatalog.sqlViewName: 'ZSDSV_DO_GET'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Get D/O data for Work Order'
define view ZSDSVC_DO_GET as select from likp
inner join lips on likp.vbeln = lips.vbeln
left outer join ser01 on ser01.lief_nr  = lips.vbeln and
                         ser01.posnr    = lips.posnr
left outer join objk  on objk.obknr     = ser01.obknr
{ 
    key likp.vbeln as DO_NUM,
    key lips.posnr as DO_ITM,
        likp.lfdat as DELIVERY_DATE,
    lips.matnr as MATERIAL,
    lips.matkl as MAT_GRP,
    objk.sernr
}
