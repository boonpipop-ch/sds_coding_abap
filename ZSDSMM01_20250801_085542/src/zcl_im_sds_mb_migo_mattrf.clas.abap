class ZCL_IM_SDS_MB_MIGO_MATTRF definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_MB_MIGO_MATTRF IMPLEMENTATION.


  method IF_EX_MB_MIGO_BADI~CHECK_HEADER.
  endmethod.


  method IF_EX_MB_MIGO_BADI~CHECK_ITEM.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_DELETE.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_LOAD.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_SAVE.
  endmethod.


  method IF_EX_MB_MIGO_BADI~INIT.

  endmethod.


  method IF_EX_MB_MIGO_BADI~LINE_DELETE.
  endmethod.


  method IF_EX_MB_MIGO_BADI~LINE_MODIFY.
  endmethod.


  method IF_EX_MB_MIGO_BADI~MAA_LINE_ID_ADJUST.
  endmethod.


  method IF_EX_MB_MIGO_BADI~MODE_SET.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_DETAIL.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_HEADER.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_DETAIL.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_HEADER.
  endmethod.


  method IF_EX_MB_MIGO_BADI~POST_DOCUMENT.

    CALL METHOD ZCL_SDSMM_MIGO_MAT_TRNFR=>CHANGE_BKTXT_OF_REFDOC
      EXPORTING
        IF_CURRENT_MKPF = is_mkpf.

  endmethod.


  method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PUBLISH_MATERIAL_ITEM.
  endmethod.


  method IF_EX_MB_MIGO_BADI~RESET.
    CALL METHOD ZCL_SDSMM_MIGO_MAT_TRNFR=>INIT_VALUE .

  endmethod.


  method IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER.
  endmethod.
ENDCLASS.
