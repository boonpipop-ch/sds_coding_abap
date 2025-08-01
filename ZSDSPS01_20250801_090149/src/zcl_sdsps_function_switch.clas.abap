class ZCL_SDSPS_FUNCTION_SWITCH definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_PS_FUNCTION_SWITCH .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSPS_FUNCTION_SWITCH IMPLEMENTATION.


METHOD IF_PS_FUNCTION_SWITCH~GET_SHOW_FLAG.
ENDMETHOD.


METHOD IF_PS_FUNCTION_SWITCH~GET_SWITCH.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_FUNCTION_SWITCH
*  Creation Date      : 28.05.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : To activate PS User fields function.
*  Purpose            : To be able to update user fields using BAPI
*                       BAPI_PROJECTDEF_CREATE (Function Group 2001)
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
* Activate User Fields
  SWITCH_VALUES-PROJ_DEFN_USER_FIELDS = 'X'.
ENDMETHOD.
ENDCLASS.
