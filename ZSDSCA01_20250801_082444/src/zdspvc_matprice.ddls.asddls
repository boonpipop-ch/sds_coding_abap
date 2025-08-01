@AbapCatalog.sqlViewName: 'ZDSPVC_MATPRCE_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material - Parts Price'
@Metadata.ignorePropagatedAnnotations: true
define view ZDSPVC_MATPRICE 


as select from mara as matmas
inner join marc as _marc on matmas.matnr = _marc.matnr
inner join mbew as _mbew on _marc.matnr = _mbew.matnr
                        and _marc.werks = _mbew.bwkey
{
    key matmas.matnr as material,
        cast(_mbew.verpr as abap.char(16)) as mvg_avg_price, 
        _mbew.bwkey as plant
        
}
where matmas.mtart = 'ZSP'
