@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Get Remain Serial'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSDSVC_GET_REMAIN_SERIAL as select from eqbs
inner join equi on eqbs.equnr = equi.equnr
association [0..1] to mbew as a on a.matnr = equi.matnr
                               and a.bwkey = '1000'
                               and a.bwtar = ''  
association [1..1] to t001 as b on b.bukrs = '1000'  
association [1..1] to mara as C on C.matnr = equi.matnr  
{
  key   eqbs.equnr,
        eqbs.b_lager,
        equi.matnr,
        equi.sernr,
        equi.erdat,
        @Semantics.amount.currencyCode: 'WAERS'
        a.verpr,
        b.waers,
        substring( C.prdha, 1, 5 )                     as ph1,
        substring( C.prdha, 6, 5 )                     as ph2,
        substring( C.prdha, 11, 8 )                    as ph3
}
