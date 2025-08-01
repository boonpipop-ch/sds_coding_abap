FUNCTION Z_SDSFI_SXPG_COMMAND_EXECUTE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(COMMANDNAME) LIKE  SXPGCOLIST-NAME
*"     REFERENCE(ADDITIONAL_PARAMETERS) LIKE  SXPGCOLIST-PARAMETERS
*"     REFERENCE(OPERATINGSYSTEM) LIKE  SXPGCOLIST-OPSYSTEM DEFAULT
*"       SY-OPSYS
*"     REFERENCE(TARGETSYSTEM) LIKE  RFCDISPLAY-RFCHOST OPTIONAL
*"     REFERENCE(DESTINATION) LIKE  RFCDES-RFCDEST
*"     REFERENCE(STDOUT) LIKE  EXTCMDEXIM-STDOUT DEFAULT 'X'
*"     REFERENCE(STDERR) LIKE  EXTCMDEXIM-STDERR DEFAULT 'X'
*"     REFERENCE(TERMINATIONWAIT) LIKE  EXTCMDEXIM-TERMWAIT DEFAULT 'X'
*"     REFERENCE(TRACE) LIKE  EXTCMDEXIM-TRACE
*"     REFERENCE(DIALOG) TYPE  BTCH0000-CHAR1
*"     REFERENCE(REMARK) TYPE  CHAR10
*"  EXPORTING
*"     VALUE(STATUS) LIKE  EXTCMDEXEX-STATUS
*"     VALUE(EXITCODE) LIKE  EXTCMDEXEX-EXITCODE
*"     REFERENCE(RETURN_MESSAGE) TYPE  CHAR100
*"     REFERENCE(RETURN_TYPE) TYPE  CHAR1
*"  TABLES
*"      EXEC_PROTOCOL STRUCTURE  BTCXPM OPTIONAL
*"----------------------------------------------------------------------
  DATA: LV_RETURN_MESSAGE TYPE STRING,
        LV_RETURN_TYPE    TYPE C.

  DATA: LWA_EXEC_PROTOCAL LIKE LINE OF EXEC_PROTOCOL.

*  IF TARGETSYSTEM IS INITIAL.
*   TARGETSYSTEM = SY-HOST.
*  ENDIF.

*  IF sy-uname EQ 'FIORI'.
*
*  ELSE.
*    IF sy-sysid EQ 'S4A'.
*      return_message = 'sFTP: Connection failed'.
*      return_type    = 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.

  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
    EXPORTING
      COMMANDNAME                   = COMMANDNAME
      ADDITIONAL_PARAMETERS         = ADDITIONAL_PARAMETERS
*     OPERATINGSYSTEM               = SY-OPSYS
*     TARGETSYSTEM                  = SY-HOST
*     DESTINATION                   =
*     STDOUT                        = 'X'
*     STDERR                        = 'X'
*     TERMINATIONWAIT               = 'X'
*     TRACE                         =
*     DIALOG                        =
    IMPORTING
      STATUS                        = STATUS
      EXITCODE                      = EXITCODE
    TABLES
      EXEC_PROTOCOL                 = EXEC_PROTOCOL
    EXCEPTIONS
      NO_PERMISSION                 = 1
      COMMAND_NOT_FOUND             = 2
      PARAMETERS_TOO_LONG           = 3
      SECURITY_RISK                 = 4
      WRONG_CHECK_CALL_INTERFACE    = 5
      PROGRAM_START_ERROR           = 6
      PROGRAM_TERMINATION_ERROR     = 7
      X_ERROR                       = 8
      PARAMETER_EXPECTED            = 9
      TOO_MANY_PARAMETERS           = 10
      ILLEGAL_COMMAND               = 11
      WRONG_ASYNCHRONOUS_PARAMETERS = 12
      CANT_ENQ_TBTCO_ENTRY          = 13
      JOBCOUNT_GENERATION_ERROR     = 14
      OTHERS                        = 15.
  IF SY-SUBRC IS NOT INITIAL.
    LV_RETURN_TYPE = 'E'.
    CASE SY-SUBRC.
      WHEN 1.       LV_RETURN_MESSAGE = '$1: no_permission'.
      WHEN 2.       LV_RETURN_MESSAGE = '$1: command_not_found'.
      WHEN 3.       LV_RETURN_MESSAGE = '$1: parameters_too_long'.
      WHEN 4.       LV_RETURN_MESSAGE = '$1: security_risk'.
      WHEN 5.       LV_RETURN_MESSAGE = '$1: wrong_check_call_interface'.
      WHEN 6.       LV_RETURN_MESSAGE = '$1: program_start_error'.
      WHEN 7.       LV_RETURN_MESSAGE = '$1: program_termination_error'.
      WHEN 8.       LV_RETURN_MESSAGE = '$1: x_error'.
      WHEN 9.       LV_RETURN_MESSAGE = '$1: parameter_expected'.
      WHEN 10.      LV_RETURN_MESSAGE = '$1: too_many_parameters'.
      WHEN 11.      LV_RETURN_MESSAGE = '$1: illegal_command'.
      WHEN 12.      LV_RETURN_MESSAGE = '$1: wrong_asynchronous_parameters'.
      WHEN 13.      LV_RETURN_MESSAGE = '$1: cant_enq_tbtco_entry'.
      WHEN 14.      LV_RETURN_MESSAGE = '$1: jobcount_generation_error'.
      WHEN 15.      LV_RETURN_MESSAGE = '$1: Other'.
      WHEN OTHERS.  LV_RETURN_MESSAGE = '$1: Unknown error'.
    ENDCASE.
  ENDIF.

  IF LV_RETURN_TYPE EQ 'E'.
  ELSE.
    LOOP AT EXEC_PROTOCOL INTO LWA_EXEC_PROTOCAL. "get last message
    ENDLOOP.
    IF SY-SUBRC IS INITIAL.
      IF LWA_EXEC_PROTOCAL-MESSAGE NE '0'.
        LV_RETURN_TYPE    = 'E'.
        LV_RETURN_MESSAGE = LWA_EXEC_PROTOCAL-MESSAGE.
        CONCATENATE '$1:' LV_RETURN_MESSAGE
               INTO LV_RETURN_MESSAGE.
      ENDIF.
    ENDIF.
  ENDIF.

  IF LV_RETURN_TYPE NE 'E'.
    LV_RETURN_TYPE    = 'S'.
    LV_RETURN_MESSAGE = '$1:Completed.'.
  ENDIF.

  REPLACE '$1' IN LV_RETURN_MESSAGE WITH REMARK.

  RETURN_MESSAGE = LV_RETURN_MESSAGE.
  RETURN_TYPE    = LV_RETURN_TYPE.




ENDFUNCTION.
