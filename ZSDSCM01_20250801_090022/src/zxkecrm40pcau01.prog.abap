*-----------------------------------------------------------------------
*  Program ID         : ZXKECRM40PCAU01
*  Creation Date      : 21.06.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : CME013
*  Description        : This is an include program for EXIT_RKECRM_PCA_40_001
*  Purpose            : Implement profit center determination for internal
*                       order created from service order and service contract
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
* >>>>> CME013 INSERT START >>>>> *
  ZCL_SDSCM_ENHANCEMENT=>EXIT_RKECRM_PCA_40_001(
    EXPORTING
      IF_KOKRS             = IM_KOKRS
      IT_CHARTAB           = T_CHARTAB[]
    CHANGING
      CF_PRCTR             = CH_PRCTR
      CF_CALL_SUBSTITUTION = CH_CALL_SUBSTITUTION ).
* <<<<< CME013 INSERT END   <<<<< *
