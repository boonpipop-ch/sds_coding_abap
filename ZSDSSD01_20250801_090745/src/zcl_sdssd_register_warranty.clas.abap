class ZCL_SDSSD_REGISTER_WARRANTY definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSSD_REGISTER_WARRANTY IMPLEMENTATION.


METHOD IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_REGISTER_WARRANTY=>IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE
*  Creation Date      : 27.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : SDE033
*  Description        : To register warranty master when post GI from
*                       delivery order
*  Purpose            : To register warranty master when post GI from
*                       delivery order
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
* Call Register Warranty
  PERFORM F_REGISTER_WARRANTY IN PROGRAM ZSDSSDR0270
                              USING XMKPF
                                    XMSEG
                              IF FOUND.
ENDMETHOD.


METHOD IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.
ENDMETHOD.
ENDCLASS.
