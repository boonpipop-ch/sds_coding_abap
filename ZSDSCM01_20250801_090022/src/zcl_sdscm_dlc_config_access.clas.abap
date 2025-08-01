class ZCL_SDSCM_DLC_CONFIG_ACCESS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_DLC_CONFIG_ACCESS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSCM_DLC_CONFIG_ACCESS IMPLEMENTATION.


METHOD IF_DLC_CONFIG_ACCESS~CHOOSE_CONFIG.
*-----------------------------------------------------------------------
*  Program ID         : IF_DLC_CONFIG_ACCESS~CHOOSE_CONFIG
*  Creation Date      : 09.05.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : CME005
*  Description        : This is enhancement to select WEBGUI configuration
*                       of SDS
*  Purpose            : To assign SDS WEBGUI Configuration
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  ZCL_SDSCM_ENHANCEMENT=>DLC_CONFIG_CHOOSE_CONFIG(
       EXPORTING IV_COMPONENT               = IV_COMPONENT
                 IV_VIEWNAME                = IV_VIEWNAME
                 IS_CONFIG_SEARCH_KEY       = IS_CONFIG_SEARCH_KEY
                 IT_CONFIG_SAP              = IT_CONFIG_SAP
                 IT_CONFIG_CUS              = IT_CONFIG_CUS
       IMPORTING ES_CONFIG_CHOOSEN          = ES_CONFIG_CHOOSEN
                 EV_CONFIG_CHOOSEN_ORIGIN   = EV_CONFIG_CHOOSEN_ORIGIN ).

ENDMETHOD.
ENDCLASS.
