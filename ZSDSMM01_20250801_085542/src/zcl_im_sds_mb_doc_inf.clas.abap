class ZCL_IM_SDS_MB_DOC_INF definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_MB_DOC_INF IMPLEMENTATION.


  METHOD IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_IM_SDS_BADI_MIGO_INTF/ POST_DOCUMENT
*  Creation Date      : 12.06.2024
*  Author             : Jutamas Y. (Eviden)
*  Add-on ID          : MMI013
*  Description        : Create outbound interface file
*  Purpose            : After MIGO ->Generate file for Saleforce & SONY
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    READ TABLE XMKPF ASSIGNING FIELD-SYMBOL(<L_MKPF>) INDEX 1 .

    CALL METHOD ZCL_SDSMM_MIGO_MAT_INTF=>CREATE_INTERFACE_FILE
      EXPORTING
        IS_MKPF = <L_MKPF>
        IT_MSEG = XMSEG.
  ENDMETHOD.


  method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.
  endmethod.
ENDCLASS.
