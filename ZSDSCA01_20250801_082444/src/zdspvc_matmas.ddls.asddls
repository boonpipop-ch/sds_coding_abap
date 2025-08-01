@AbapCatalog.sqlViewName: 'ZDSPVC_MATMAS_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Master -  Parts'
@Metadata.ignorePropagatedAnnotations: true
define view ZDSPVC_MATMAS 



as select from mara as matmas
	inner join marc as _mar on matmas.matnr = _mar.matnr
	inner join makt as _mak on _mar.matnr   = _mak.matnr
	                       and _mak.spras   = 'E'     
	inner join mbew as _mbe on _mar.matnr   = _mbe.matnr
  	                       and _mar.werks   = _mbe.bwkey   
	inner join mvke as _mvk on matmas.matnr = _mvk.matnr                                 	                              	             		 	 	 
{
      
  key matmas.matnr as material,
      matmas.bismt,
      matmas.lvorm,
      _mak.maktx,
      matmas.matkl,
       _mar.mmsta,      
      matmas.mstae,
      matmas.mstav,
      tstmp_to_tims( tstmp_current_utctimestamp(),
                     abap_system_timezone( $session.client,'NULL' ),
                     $session.client,
                     'NULL' )     as time,
     cast(_mbe.verpr as abap.char(16)) as verpr,                    
      _mvk.vmsta,              
      _mar.werks     
//      cast( 'Test'  as abap.char(4)) as item_text1                                                   
}
where _mvk.vkorg <> ' '
