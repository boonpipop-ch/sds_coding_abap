class ZCL_IM_SDS_MM_CHECK_ALL_IT definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_CHECK_ALL_ITEMS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_MM_CHECK_ALL_IT IMPLEMENTATION.


METHOD IF_EX_ME_CHECK_ALL_ITEMS~RECALCULATE_ITEMS.
*-----------------------------------------------------------------------
*  Program ID         : IF_EX_ME_CHECK_ALL_ITEMS~RECALCULATE_ITEMS
*  Creation Date      : 23.07.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : To implement logic to trigger price redetermination
*                       in Purchase order
*  Purpose            : - To trigger price determination. It is used
*                         together with implementation of Badi ME_DEFINE_CALCTYPE
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Trigger all items recalculate when related fields in header
* has been changed
  PERFORM F_CHECK_EKKO_CHANGED IN PROGRAM ZSDSMMR0280
                               USING IM_X_OEKKO
                                     IM_X_EKKO
                               CHANGING CH_X_RECALCULATE
                               IF FOUND.

ENDMETHOD.
ENDCLASS.
