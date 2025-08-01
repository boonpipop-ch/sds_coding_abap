@AbapCatalog.sqlViewName: 'ZSDSVC_EQUIP_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Equipment Master Data'
@Metadata.ignorePropagatedAnnotations: true
define view ZDSPVC_EQUIPMENT 


as select from equi as equipment
inner join mara as _mara on equipment.matnr = _mara.matnr
inner join makt as _makt on equipment.matnr = _makt.matnr
                         and _makt.spras = 'E' 
inner join equz as _equz on equipment.equnr = _equz.equnr
inner join iloa as _iloa on _equz.iloan = _iloa.iloan
inner join adrc as _adrc on _iloa.adrnr = _adrc.addrnumber
inner join eqkt as _eqkt on equipment.equnr = _eqkt.equnr                       
inner join ihpa as _ihpa on equipment.objnr = _ihpa.objnr
inner join kna1 as _kna1 on _ihpa.parnr     = _kna1.kunnr
inner join jest as _jest on equipment.objnr = _jest.objnr                   
{

  key equipment.equnr as equipment,
      _kna1.anred,
      _eqkt.eqktx as sold_to_party,
      equipment.groes,
      equipment.herst,
      equipment.inbdt,
      equipment.invnr, 
      _kna1.land1,
      _makt.maktx,
      _equz.mapar,
      equipment.matnr, 
      cast(_adrc.street as abap.char(60)) as MC_STREET,
      _adrc.name1,
      _adrc.name2,
      _adrc.name3,
      _adrc.name4,
      _kna1.ort01,
      _ihpa.parnr, 
      _adrc.post_code1,     
      equipment.sernr,
      _jest.stat,
      cast(_adrc.tel_number as abap.char(30)) as TELF1,
      tstmp_to_tims( tstmp_current_utctimestamp(),
                     abap_system_timezone( $session.client,'NULL' ),
                     $session.client,
                     'NULL' )     as time,
      equipment.typbz
    
}
//where equipment.eqtyp = 'Z'
