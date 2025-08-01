class ZCL_IM_SDS_MM_DEF_CALCTYPE definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_DEFINE_CALCTYPE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_MM_DEF_CALCTYPE IMPLEMENTATION.


METHOD IF_EX_ME_DEFINE_CALCTYPE~DEFINE_CALCTYPE.
*-----------------------------------------------------------------------
*  Program ID         : IF_EX_ME_DEFINE_CALCTYPE~DEFINE_CALCTYPE
*  Creation Date      : 23.07.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : To implement logic to trigger price redetermination
*                       in Purchase order
*  Purpose            : - To trigger price determination. It is used
*                         together with implementation of Badi ME_CHECK_ALL_ITEMS
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Update Price Trigger for SDS
  PERFORM F_CHECK_REDETERMINE_PRICE IN PROGRAM ZSDSMMR0280
                                    USING  IM_X_OEKKO
                                           IM_X_NEKKO
                                           IM_X_OEKPO
                                           IM_X_NEKPO
                                    CHANGING CH_X_LF_CALCTYPE
                                    IF FOUND.

ENDMETHOD.
ENDCLASS.
