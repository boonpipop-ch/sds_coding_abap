@AbapCatalog.sqlViewName: 'ZSDSVC_T023T_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Group Master'
define view ZSDSVC_T023T as select from t023t
{
    key matkl,
        wgbez
} where spras = 'E';
    
    
