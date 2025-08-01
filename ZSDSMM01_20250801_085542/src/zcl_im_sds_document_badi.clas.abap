class ZCL_IM_SDS_DOCUMENT_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
PRIVATE SECTION.

  CONSTANTS : BEGIN OF GC_CON,
                MB1A TYPE C LENGTH 4 VALUE 'MB1A',
              END OF GC_CON.
ENDCLASS.



CLASS ZCL_IM_SDS_DOCUMENT_BADI IMPLEMENTATION.


  METHOD IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      CASE SY-TCODE.
        WHEN GC_CON-MB1A.
          LCL_DATA=>EXPORT_MB1A_TO_WMS( I_XMKPF  = XMKPF
                                        I_XMSEG  = XMSEG
                                        I_XVM07M = XVM07M ).
      ENDCASE.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

  ENDMETHOD.


  method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.
  endmethod.
ENDCLASS.
