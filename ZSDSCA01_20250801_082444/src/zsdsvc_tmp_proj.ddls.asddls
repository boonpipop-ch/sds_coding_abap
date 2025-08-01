@AbapCatalog.sqlViewName: 'ZSDS_TMP_PROJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help for Credit temporary'
define view ZSDSVC_TMP_PROJ as select from zsdsfit036
{
    key kunnr   as partner,
    psphi       as project,
    startdate   as startDate,
    enddate     as endDate,
    seq         as seq,
    credit_limit as creditLimit,
    zterm        as zterm,
    zdel_flg     as delFlag,
    zcrt_date    as createDate,
    zcrt_time    as createTime,
    zcrt_user    as createUser,
    zcrt_pgm     as createProg,
    zupd_date    as updateDate,
    zupd_time    as updateTime,
    zupd_user    as updateUser,
    zupd_pgm     as updateProg
    }
 where zdel_flg <> 'X'
   and ( startdate <=  $session.system_date and 
         enddate >=  $session.system_date )
