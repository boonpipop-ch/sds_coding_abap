@AbapCatalog.sqlViewName: 'ZSDSVC_BP_LIS_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Business Partner List'
define view ZSDSVC_BP_LIST as select from but000
inner join but100 on but000.partner = but100.partner and 
                          ( but100.rltyp = 'FLCU01' or          //Customer 
                            but100.rltyp = 'FLVN01' or          //Vendor
                            but100.rltyp = 'FLCU00' or         // Customer FI
                            but100.rltyp = 'FLVN00')           //Vendor FI             
left outer join but020 on but020.partner = but000.partner 
left outer join adrc as _address on ( _address.addrnumber = but020.addrnumber )
left outer join fitha_pbupl_d_t as branch_c on ( branch_c.kunnr = but000.partner and
                                                 branch_c.spras = 'E') 
left outer join fitha_pbupl_k_t as branch_v on ( branch_v.lifnr = but000.partner and
                                                 branch_v.spras = 'E') 
left outer join dfkkbptaxnum as tax on (tax.partner = but000.partner)
association [0..1] to ZSDSVC_CUSTOMER as _customer on _customer.kunnr = but000.partner
association [0..1] to ZSDSVC_VENDOR as _vendor on _vendor.lifnr = but000.partner


//left outer join kna1 on kna1.kunnr = but000.partner 
//left outer join lfa1 on lfa1.lifnr = but000.partner 
//left outer join adrc as _address on ( _address.addrnumber = kna1.adrnr or 
//                                        _address.addrnumber = lfa1.adrnr )

{
    key but000.partner,
    key case but100.rltyp 
        when 'FLCU01' then 
            _customer.adrnr  
        when 'FLCU00' then 
            _customer.adrnr  
        when 'FLVN01' then
             _vendor.adrnr
        when 'FLVN00' then
             _vendor.adrnr
        else ''
    end as addrnumber,
    
    
    but100.rltyp,
    but000.type,
    
    case but000.type
        when '1' then
            concat_with_space( but000.name_first,
                               but000.name_last,
                               1 )
        when '2' then 
            concat( but000.name_org1,
                concat( but000.name_org2,
                    concat( but000.name_org3, but000.name_org4 )))
        else ''
    end as name_th,
    
    
    //International version
    //----------------------------------------------------------------------//
    
    //Name Org.
    case but100.rltyp 
        when 'FLCU01' then 
            _customer.orgName
        when 'FLCU00' then 
            _customer.orgName
        when 'FLVN01' then
             _vendor.orgName
        when 'FLVN00' then
             _vendor.orgName
        else ''
    end as nameOrg_en,
    
    
    //Name Person
    case but100.rltyp 
        when 'FLCU01' then 
            _customer.perName
        when 'FLCU00' then 
            _customer.perName
        when 'FLVN01' then
             _vendor.perName
        when 'FLVN00' then
             _vendor.perName
        else ''
    end as namePer_en,
    
    //Branch Code 
    case but100.rltyp 
        when 'FLCU01' then 
            branch_c.j_1tpbupl
        when 'FLCU00' then 
            branch_c.j_1tpbupl
        when 'FLVN01' then
            branch_v.j_1tpbupl
        when 'FLVN00' then
            branch_v.j_1tpbupl
        else ''
    end as branchCode,
    
    //Branch Description
    case but100.rltyp 
        when 'FLCU01' then 
            branch_c.description
        when 'FLCU00' then 
            branch_c.description
        when 'FLVN01' then
            branch_v.description
        when 'FLVN00' then
            branch_v.description
        else ''
    end as branchDesc,
    
    //tax number
    tax.taxnum as taxid,

    
//    case but000.type
//        when '1' then
//            concat_with_space( _address.name1,
//                               _address.name2,
//                               1 )
//        when '2' then 
//            concat( _address.name1,
//                concat( _address.name2,
//                    concat( _address.name3, _address.name4 )))
//        else ''
//    end as name_en

      cast(
            ltrim(concat_with_space( _address.house_num1,
            //   ltrim(concat_with_space( house_num2,
                  ltrim(concat_with_space( _address.street,
            //         ltrim(concat_with_space( str_suppl1,
                       ltrim(concat_with_space( _address.str_suppl2,
                          ltrim(concat_with_space( _address.str_suppl3,
                            ltrim(concat_with_space( _address.location,
                              ltrim(concat_with_space( _address.city2,
                                 ltrim(concat_with_space( _address.city1, _address.post_code1,
                                  1 ),' '),
                                1 ),' '),
                             1 ),' '),
                           1 ),' '),
                         1 ),' '),
           //            1 ),' '),
                     1 ),' '),
           //        1 ),' '),
                1 ),' ')    as zsds_de_addr )
           as address1,
           
        _customer.soAll,
        _customer.soSel,
        _customer.doAll,
        _customer.doSel,
        _customer.billAll,
        _customer.billSel

}
where but000.bu_group like 'Z%' and
       ( but000.bu_group <> 'Z040' and
          but000.bu_group <> 'Z041' and
          //but000.bu_group <> 'Z060' and
          but000.bu_group <> 'Z070' ) and
        ( _address.nation = '' ) and 
         //_address.nation = 'I' ) and
       ( but100.rltyp = 'FLCU01' or //Customer 
         but100.rltyp = 'FLCU01' or //Customer FI        
         but100.rltyp = 'FLVN01' or //Vendor  
         but100.rltyp = 'FLVN00' )  //Vendor  FI              
       
group by but000.partner,
 //       _address.addrnumber,
        but100.rltyp,
        but000.type,
        but000.name_first,
        but000.name_last,
        but000.name_org1,
        but000.name_org2,
        but000.name_org3,
        but000.name_org4,
        _customer.adrnr,
        _vendor.adrnr,
        _customer.orgName,
        _customer.perName,
        _vendor.orgName,
        _vendor.perName,
        _address.house_num1,
        _address.street,
        _address.str_suppl2,
        _address.str_suppl3,
        _address.location,
        _address.city2,
        _address.city1,
        _address.post_code1,
   //     _address.name1,
   //     _address.name2,
   //     _address.name3,
   //     _address.name4
        _customer.soAll,
        _customer.soSel,
        _customer.doAll,
        _customer.doSel,
        _customer.billAll,
        _customer.billSel,
        branch_c.j_1tpbupl,
        branch_v.j_1tpbupl,
        branch_c.description,
        branch_v.description,
        tax.taxnum
  
            
            
            
            
