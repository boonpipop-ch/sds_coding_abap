@AbapCatalog.sqlViewName: 'ZSDSVC_IO_ITEM_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CO Object: Line Items'
@Metadata.ignorePropagatedAnnotations: true
define view ZSDSVC_IO_TRANSACTION as 
select from coep
left outer join aufk on aufk.objnr = coep.objnr 
left outer join cobk on cobk.belnr = coep.belnr 

{
  --  key coep.kokrs,
  --  key coep.buzei,
  --  key coep.belnr,
    key 'actual' as transactionType,
    key coep.vrgng as bTran,
    key cobk.blart as docType,
    key coep.aufnr as orderNumber,
    key coep.pspnr as wbsNumber,
    key coep.gjahr as fiscalYear,
    key coep.perio as period,
    coep.rbest as poCategory,
    coep.ebeln as poDoc,
    coep.kstar as costElement,
    coep.sgtxt as description,
    coep.wkgbtr as amount,
    cobk.refbn as refDoc,
    coep.timestmp as createDate
    
}
--where aufk.aufnr='A0424011' and 
where coep.accasty ='OR' or coep.accasty ='PR'

union all

select from cooi
left outer join aufk on aufk.objnr = cooi.objnr 
left outer join prps on prps.objnr = cooi.objnr
{
  --  key coep.kokrs,
  --  key coep.buzei,
  --  key coep.belnr,
    key 'commit' as transactionType,
    key cooi.vrgng as bTran,
    key cooi.rftyp as docType,
    key aufk.aufnr as orderNumber,
    key prps.posid as wbsNumber,
    key cooi.gjahr as fiscalYear,
    key cooi.perio as period,
    cast('' as abap.numc(3))  as poCategory,
    '' as poDoc,
    cooi.sakto as costElement,
    cooi.sgtxt as description,
    cooi.wkgbtr as amount,
    cooi.refbn as refDoc,
    cooi.timestmp as createDate              
    
}
--where aufk.aufnr='A0424011' 
