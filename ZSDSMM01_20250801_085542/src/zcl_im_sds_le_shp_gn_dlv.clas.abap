class ZCL_IM_SDS_LE_SHP_GN_DLV definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_GN_DLV_CREATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_LE_SHP_GN_DLV IMPLEMENTATION.


  METHOD if_ex_le_shp_gn_dlv_create~move_komdlgn_to_likp.

    DATA: lr_client TYPE RANGE OF sy-mandt.
    IF lr_client[] IS INITIAL.
      zcl_sdsca_utilities=>get_gen_c_range( EXPORTING if_repid = 'SDS_PROGRAM'
                                                      if_param = 'SDS_CLIENT'
                                            IMPORTING et_range = lr_client ).
    ENDIF.

    CHECK lr_client IS NOT INITIAL.
    CHECK sy-mandt IN lr_client.

    cs_likp-zwadat_ist = is_xkomdlgn-zwadat_ist.
    cs_likp-zwatim_ist = is_xkomdlgn-zwatim_ist.
    cs_likp-ztruckno   = is_xkomdlgn-ztruckno.

  ENDMETHOD.


  method IF_EX_LE_SHP_GN_DLV_CREATE~MOVE_KOMDLGN_TO_LIPS.
  endmethod.
ENDCLASS.
