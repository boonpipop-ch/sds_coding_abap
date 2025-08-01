@AbapCatalog.sqlViewName: 'ZSDSVC_ADDRESS_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Get Address data'
define view ZSDSVC_GET_ADDRESS
  with parameters
      i_ADDRNUMBER  : ad_addrnum,
      i_lang        : spras
    as select from adrc
       left outer join t005t
         on t005t.land1 = adrc.country
        and t005t.spras = $parameters.i_lang
  {
    key nation,
    key adrc.addrnumber,
      cast(
          concat( name1,
            concat( name2,
              concat ( name3, name4 ) ) ) as zsds_de_name ) as name,
      cast(
            ltrim(concat_with_space( house_num1,
            //   ltrim(concat_with_space( house_num2,
                  ltrim(concat_with_space( street,
            //         ltrim(concat_with_space( str_suppl1,
                       ltrim(concat_with_space( str_suppl2,
                          ltrim(concat_with_space( str_suppl3,
                            ltrim(concat_with_space( location,
                              ltrim(concat_with_space( city2,
                                 ltrim(concat_with_space( city1, post_code1,
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
        name1,
        name2,
        name3,
        name4,
        house_num1,
        house_num2,
        street,
        str_suppl1,
        str_suppl2,
        str_suppl3,
        location,
        city2,
        city1,
        post_code1,
        tel_number,
        fax_number,
        country,
        t005t.landx50 as country_name
  }
  where addrnumber = :i_ADDRNUMBER
