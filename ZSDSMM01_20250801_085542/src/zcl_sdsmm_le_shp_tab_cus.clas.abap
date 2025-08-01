class ZCL_SDSMM_LE_SHP_TAB_CUS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_TAB_CUST_OVER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSMM_LE_SHP_TAB_CUS IMPLEMENTATION.


  METHOD if_ex_le_shp_tab_cust_over~activate_tab_page.

    DATA: lr_client TYPE RANGE OF sy-mandt.
    CHECK is_likp-lfart = 'EL'.

    IF lr_client[] IS INITIAL.
      zcl_sdsca_utilities=>get_gen_c_range( EXPORTING if_repid = 'SDS_PROGRAM'
                                                      if_param = 'SDS_CLIENT'
                                            IMPORTING et_range = lr_client ).
    ENDIF.

    CHECK lr_client IS NOT INITIAL.
    CHECK sy-mandt IN lr_client.

    ef_caption = 'Additional 1'.
    ef_program = 'ZSDSMMR0060'.
    ef_dynpro  = '1000'.

  ENDMETHOD.


  method IF_EX_LE_SHP_TAB_CUST_OVER~PASS_FCODE_TO_SUBSCREEN.
  endmethod.


  method IF_EX_LE_SHP_TAB_CUST_OVER~TRANSFER_DATA_FROM_SUBSCREEN.
  endmethod.


  method IF_EX_LE_SHP_TAB_CUST_OVER~TRANSFER_DATA_TO_SUBSCREEN.
  endmethod.
ENDCLASS.
