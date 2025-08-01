class ZCL_SDSSD_ADD_SUPPORT_COND_KEY definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_WCB_ADD_SUPPORTED_KEY_BADI .
protected section.
private section.

  class-data GT_ACTIVE_FIELD type WCB_FIELD_STAB .

  class-methods GET_CONSTANTS .
ENDCLASS.



CLASS ZCL_SDSSD_ADD_SUPPORT_COND_KEY IMPLEMENTATION.


METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_WBS_SUBSTITUTION / GET_CONSTANTS
*  Creation Date      : 26.07.2024
*  Author             : Wuthichai L. (Eviden)
*  Add-on ID          : PSE004
*  Description        : To get GENC constant from table ZSDSCAC001
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  CONSTANTS:
    LC_ACTIVE_FIELD TYPE ZSDSDE_PARAM_NAME VALUE 'ACTIVATED_FIELD'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID TYPE  PROGRAMM VALUE 'ZCL_SDSSD_ADD_SUPPORT_COND_KEY',
    LF_FNAME TYPE  WCB_FIELD.


* Check Already Read?
  IF LF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_ACTIVE_FIELD.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = 'X'.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Active Condition Key Field
*     ------------------------------------
      WHEN LC_ACTIVE_FIELD.
        LF_FNAME = <L_GENC>-VALUE_LOW.
        INSERT LF_FNAME INTO TABLE GT_ACTIVE_FIELD.

    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD IF_WCB_ADD_SUPPORTED_KEY_BADI~ADD_KEYS.

* Get Constant
  GET_CONSTANTS( ).

  IF GT_ACTIVE_FIELD IS INITIAL.
    RETURN.
  ENDIF.

  INSERT LINES OF GT_ACTIVE_FIELD INTO TABLE T_SUPPORTED.

ENDMETHOD.
ENDCLASS.
