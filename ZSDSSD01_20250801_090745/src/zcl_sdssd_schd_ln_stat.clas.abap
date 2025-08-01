class ZCL_SDSSD_SCHD_LN_STAT definition
  public
  final
  create public .

public section.

  class-methods CHECK_ACTIVE_SALE_ORG
    importing
      value(IF_SALES_ORG) type LIKP-VKORG
    returning
      value(CF_ACTIVE) type FLAG .
  class-methods CHECK_CRITERIA_WITH_GENC
    importing
      !IS_LIKP type LIKPVB
    returning
      value(CF_PASS) type FLAG .
  class-methods SUBMIT_PROG_TO_SEND_INTERFACE
    importing
      value(IF_DO_NUMBER) type LIKP-VBELN
      value(IF_DEL) type FLAG
      value(IF_TEST) type FLAG default ' '
      value(IF_WAIT) type FLAG default ' ' .
protected section.
private section.

  constants GC_REPID type SY-REPID value 'ZCL_SDSSD_SCHD_LN_STAT' ##NO_TEXT.
  class-data GR_VKORG type RANGE_T_VKORG .
  class-data GR_LFART type J_3RS_SO_DELIVERY .
  constants GC_JOBNAME_PREFIX type TBTCJOB-JOBNAME value 'ZSDSSDR0200' ##NO_TEXT.
  constants GC_SUBMIT_PROGRAM type SY-REPID value 'ZSDSSDR0200' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_SDSSD_SCHD_LN_STAT IMPLEMENTATION.


  METHOD CHECK_ACTIVE_SALE_ORG.
*-----------------------------------------------------------------------
*  Program ID         : class ZCL_SDSSD_SCHD_LN_STAT
*                       method CHECK_ACTIVE_SALE_ORG
*  Creation Date      : 26.06.2024
*  Author             : Boontip
*  Add-on ID          : SDI036
*  Description        : method to check active sale org
*  Purpose            : to call in badi/enhancement
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CLEAR CF_ACTIVE.
    IF GR_VKORG IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = gc_repid
          IF_PARAM = 'SALES_ORG'
        IMPORTING
          ET_RANGE = GR_VKORG.
    ENDIF.
    IF GR_VKORG IS NOT INITIAL
    AND IF_SALES_ORG IN GR_VKORG.
      CF_ACTIVE = ABAP_TRUE.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_CRITERIA_WITH_GENC.
*-----------------------------------------------------------------------
*  Program ID         : class ZCL_SDSSD_SCHD_LN_STAT
*                       method CHECK_MEET_CRITERIA
*  Creation Date      : 08.11.2024
*  Author             : Boontip
*  Add-on ID          : SDI036
*  Description        : method to validate criteria from Genc
*  Purpose            : to call in badi/enhancement
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CLEAR CF_PASS.
    IF GR_LFART IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_SUBMIT_PROGRAM
          IF_PARAM = 'DELIVERY_TYPE'
        IMPORTING
          ET_RANGE = GR_LFART.
    ENDIF.
    IF GR_LFART IS NOT INITIAL
    AND IS_LIKP-LFART IN GR_LFART.
      CF_PASS = ABAP_TRUE.
    ENDIF.
  ENDMETHOD.


  METHOD SUBMIT_PROG_TO_SEND_INTERFACE.
*-----------------------------------------------------------------------
*  Program ID         : class ZCL_SDSSD_SCHD_LN_STAT
*                       method SUBMIT_PROG_TO_SEND_INTERFACE
*  Creation Date      : 26.06.2024
*  Author             : Boontip
*  Add-on ID          : SDI036
*  Description        : method to submit program ZSDSSDR0200
*  Purpose            : to send interface DO status to salesforces
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LF_JOBNAME TYPE TBTCJOB-JOBNAME,
          LF_JOBNUM  TYPE TBTCJOB-JOBCOUNT.

* Generate JOB to call Interface
    CONCATENATE GC_JOBNAME_PREFIX
                SY-DATUM
                SY-UZEIT
           INTO LF_JOBNAME
           SEPARATED BY '_'.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LF_JOBNAME
      IMPORTING
        JOBCOUNT         = LF_JOBNUM
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.

* Call interface program
    SUBMIT ZSDSSDR0200                                   "#EC CI_SUBMIT
      WITH P_VBELN EQ IF_DO_NUMBER
      WITH P_DEL EQ IF_DEL
      WITH P_TEST EQ IF_TEST
      WITH P_WAIT EQ IF_WAIT
      VIA JOB LF_JOBNAME NUMBER LF_JOBNUM
      AND RETURN.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LF_JOBNUM
        JOBNAME              = LF_JOBNAME
        STRTIMMED            = 'X'
      EXCEPTIONS
        CANT_START_IMMEDIATE = 1
        INVALID_STARTDATE    = 2
        JOBNAME_MISSING      = 3
        JOB_CLOSE_FAILED     = 4
        JOB_NOSTEPS          = 5
        JOB_NOTEX            = 6
        LOCK_FAILED          = 7
        OTHERS               = 8.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
