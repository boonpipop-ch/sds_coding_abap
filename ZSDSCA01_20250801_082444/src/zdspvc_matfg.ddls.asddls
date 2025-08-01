@AbapCatalog.sqlViewName: 'ZDSPVC_MATFG_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Master -  Finished Goods'
@Metadata.ignorePropagatedAnnotations: true
define view ZDSPVC_MATFG 


as select from mara as matmas
inner join mvke as _mvke on matmas.matnr = _mvke.matnr
inner join makt as _makt on _mvke.matnr = _makt.matnr
                        and _makt.spras = 'E'
//inner join t179t as _t179t on _mvke.prodh = _t179t.prodh
//                       and _t179t.spras = 'E'      
inner join marc as _marc on _mvke.matnr = _marc.matnr                                                           
{

    key matmas.matnr as material,
        _makt.maktx,
        cast( 'Daikin'  as abap.char(6)) as brand, 
        _marc.werks  as plant_code
                    
}
where matmas.mtart = 'ZFG'
//and   matmas.matkl = 'BMS'
and   _mvke.prodh  <> ' '
