@AbapCatalog.sqlViewName: 'ZSDSVC_VENDOR_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Business Partner (Vendor)'
@Metadata.ignorePropagatedAnnotations: true
define view ZDSPVC_BP_VEND as select from lfa1
{
    key lifnr as VENDOR,
        tstmp_to_tims( tstmp_current_utctimestamp(),
        abap_system_timezone( $session.client,'NULL' ),
        $session.client,
        'NULL' )               as time,
        concat( name1, name2 ) as NAME,
        loevm                  as status            
}
where loevm = ' '
