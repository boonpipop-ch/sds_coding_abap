@AbapCatalog.sqlViewName: 'ZSDSVC_CLRDOC_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for finding clear document'
define view ZSDSVC_GET_CLRDOC as select from bseg
{
    key zuonr as salesOrder,
        augbl as clrDoc,
        augdt as clrDat
}
where bschl = '11'      //Posting key
  and  ( zuonr <> '' and
         zuonr like '3%' )
  and augbl <> ''
