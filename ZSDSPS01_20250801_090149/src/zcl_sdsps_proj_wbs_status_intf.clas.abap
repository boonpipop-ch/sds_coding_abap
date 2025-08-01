class ZCL_SDSPS_PROJ_WBS_STATUS_INTF definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TS_WBS_LEVEL_RANGE,
        SIGN   TYPE SIGN_RANGE,
        OPTION TYPE OPT_RANGE,
        LOW    TYPE PRPS-STUFE,
        HIGH   TYPE PRPS-STUFE,
      END OF TS_WBS_LEVEL_RANGE .
  types:
    TT_WBS_LEVEL_RANGE TYPE STANDARD TABLE OF TS_WBS_LEVEL_RANGE .
  types:
    BEGIN OF TS_STATUS_CLSD_RANGE,
        SIGN   TYPE SIGN_RANGE,
        OPTION TYPE OPT_RANGE,
        LOW    TYPE TJ02-ISTAT,
        HIGH   TYPE TJ02-ISTAT,
      END OF TS_STATUS_CLSD_RANGE .
  types:
    TT_STATUS_CLSD_RANGE TYPE STANDARD TABLE OF TS_STATUS_CLSD_RANGE .
  types:
    BEGIN OF TS_OBJECT_TYPE_RANGE,
        SIGN   TYPE SIGN_RANGE,
        OPTION TYPE OPT_RANGE,
        LOW    TYPE SEU_TYPE,
        HIGH   TYPE SEU_TYPE,
      END OF TS_OBJECT_TYPE_RANGE .
  types:
    TT_OBJECT_TYPE_RANGE TYPE STANDARD TABLE OF TS_OBJECT_TYPE_RANGE .

  class-methods PROCESS_DATA
    importing
      !IT_OBJECT type ZSDSPSS013_TT
      !IF_SYSTEM_STATUS type TJ02T-TXT04
      !IF_TEST type FLAG optional .
protected section.
private section.

  constants GC_JOBNAME_PREFIX type TBTCJOB-JOBNAME value 'ZSDSPSR0020' ##NO_TEXT.
  constants GC_REPID type ZSDSCAC001-REPID value 'ZCL_SDSPS_PROJ_WBS_STATUS_INTF' ##NO_TEXT.
  constants GC_SIGN_I type DDSIGN value 'I' ##NO_TEXT.
  constants GC_OPTION_EQ type DDOPTION value 'EQ' ##NO_TEXT.
  class-data GRT_PROJ_DEF type MPET_PSPID_RANG .
  class-data GRT_WBS_LEVEL type TT_WBS_LEVEL_RANGE .
  class-data GRT_OBTYP_PROJ type TT_OBJECT_TYPE_RANGE .
  class-data GRT_OBTYP_WBS type TT_OBJECT_TYPE_RANGE .
  class-data GRT_SYSTEM_STATUS type BAPI_ITOB_T_SEL_STATUS .

  class-methods GET_CONSTANTS .
  class-methods FILL_PROJECT
    importing
      !IT_OBJECT type ZSDSPSS013_TT
    changing
      !CT_PROJECT type MPET_PSPID_RANG .
  class-methods FILL_WBS
    importing
      !IT_OBJECT type ZSDSPSS013_TT
    changing
      !CT_WBS type RE_T_RSOPOSID .
  class-methods SUBMIT_INTERFACE_JOB_CLOSURE
    importing
      !IT_PROJECT type MPET_PSPID_RANG
      !IT_WBS type RE_T_RSOPOSID
      !IF_TEST type FLAG
      !IF_SYSTEM_STATUS type TJ02T-TXT04 .
  class-methods PROCESS_STATUS_CLOSED
    importing
      !IT_OBJECT type ZSDSPSS013_TT
      !IF_TEST type FLAG
      !IF_SYSTEM_STATUS type TJ02T-TXT04 .
ENDCLASS.



CLASS ZCL_SDSPS_PROJ_WBS_STATUS_INTF IMPLEMENTATION.


METHOD FILL_PROJECT.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_PROJ_WBS_STATUS_INTF / FILL_PROJECT
*  Creation Date      : 26.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : PSI010
*  Description        : Fill list of project definition for sending
*                       interface to salesforce
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  CLEAR: CT_PROJECT.

  LOOP AT IT_OBJECT ASSIGNING FIELD-SYMBOL(<L_OBJECT>)
                    WHERE OBTYP IN GRT_OBTYP_PROJ.  "Object type PD
*   Project definition in scope?
    IF <L_OBJECT>-OBJECTKEY IN GRT_PROJ_DEF.
      INSERT VALUE #( SIGN   = GC_SIGN_I
                      OPTION = GC_OPTION_EQ
                      LOW    = <L_OBJECT>-OBJECTKEY )
             INTO TABLE CT_PROJECT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD FILL_WBS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_PROJ_WBS_STATUS_INTF / FILL_WBS
*  Creation Date      : 26.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : PSI010
*  Description        : Fill list of WBS element for sending
*                       interface to salesforce
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  TYPES: BEGIN OF LTS_OBJECT,
           OBTYP     TYPE ZSDSPSS013-OBTYP,
           OBJECTKEY TYPE PRPS-POSID,
         END OF LTS_OBJECT.

  DATA: LT_OBJECT TYPE STANDARD TABLE OF LTS_OBJECT.

  CLEAR: CT_WBS.

  LT_OBJECT[] = CORRESPONDING #( IT_OBJECT ).
  DELETE LT_OBJECT WHERE OBTYP NOT IN GRT_OBTYP_WBS.

  IF LT_OBJECT IS NOT INITIAL.
*  Get WBS element which
*   - WBS level in scope (GRT_WBS_LEVEL)
*   - WBS in project definition in scope (GRT_PROJ_DEF)
    SELECT WBS~PSPNR,
           WBS~POSID,
           WBS~OBJNR,
           WBS~PSPHI,
           WBS~STUFE,
           PROJ~PSPID
      FROM PRPS AS WBS
      INNER JOIN PROJ
       ON ( WBS~PSPHI EQ PROJ~PSPNR )
      INTO TABLE @DATA(LT_PRPS)
      FOR ALL ENTRIES IN @LT_OBJECT
      WHERE WBS~POSID EQ @LT_OBJECT-OBJECTKEY
        AND WBS~STUFE IN @GRT_WBS_LEVEL
        AND PROJ~PSPID IN @GRT_PROJ_DEF.

    IF SY-SUBRC EQ 0.
      LOOP AT LT_PRPS ASSIGNING FIELD-SYMBOL(<L_WBS>).
        INSERT VALUE #( SIGN   = GC_SIGN_I
                        OPTION = GC_OPTION_EQ
                        LOW    = <L_WBS>-POSID )
               INTO TABLE CT_WBS.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_PROJ_WBS_STATUS_INTF / GET_CONSTANTS
*  Creation Date      : 26.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : PSI010
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
    LC_SYSTEM_STATUS      TYPE ZSDSDE_PARAM_NAME VALUE 'SYSTEM_STATUS',
    LC_PROJECT_DEFINITION TYPE ZSDSDE_PARAM_NAME VALUE 'PROJECT_DEFINITION',
    LC_WBS_LEVEL          TYPE ZSDSDE_PARAM_NAME VALUE 'WBS_LEVEL',
    LC_OBJECT_TYPE_PROJ   TYPE ZSDSDE_PARAM_NAME VALUE 'OBJECT_TYPE_PROJ',
    LC_OBJECT_TYPE_WBS    TYPE ZSDSDE_PARAM_NAME VALUE 'OBJECT_TYPE_WBS'.

  DATA: LT_GENC TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  CLEAR: GRT_PROJ_DEF,
         GRT_WBS_LEVEL,
         GRT_OBTYP_PROJ,
         GRT_OBTYP_WBS,
         GRT_SYSTEM_STATUS.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = GC_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).
    CASE <L_GENC>-PARAM.
*     SYSTEM_STATUS
      WHEN LC_SYSTEM_STATUS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_SYSTEM_STATUS.

*     PROJECT_DEFINITION
      WHEN LC_PROJECT_DEFINITION.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_PROJ_DEF.

*     WBS_LEVEL
      WHEN LC_WBS_LEVEL.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_WBS_LEVEL.

*     OBJECT_TYPE_PROJ
      WHEN LC_OBJECT_TYPE_PROJ.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_OBTYP_PROJ.

*     OBJECT_TYPE_WBS
      WHEN LC_OBJECT_TYPE_WBS.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_OBTYP_WBS.

    ENDCASE.
  ENDLOOP.

ENDMETHOD.


METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_PROJ_WBS_STATUS_INTF / PROCESS_DATA
*  Creation Date      : 26.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : PSI010
*  Description        : To check and process outbound interface
*                       Project/ WBS system status to Salesforce
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

* Get Constants setting
  GET_CONSTANTS( ).

* If no system status maintain in GENC, then skip processing
  IF GRT_SYSTEM_STATUS IS INITIAL.
    RETURN.
  ENDIF.

  IF IF_SYSTEM_STATUS IN GRT_SYSTEM_STATUS.
*   Processing for system status closed
    PROCESS_STATUS_CLOSED( EXPORTING IT_OBJECT = IT_OBJECT
                                     IF_TEST = IF_TEST
                                     IF_SYSTEM_STATUS = IF_SYSTEM_STATUS ).
  ENDIF.

ENDMETHOD.


METHOD PROCESS_STATUS_CLOSED.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_PROJ_WBS_STATUS_INTF / PROCESS_STATUS_CLOSED
*  Creation Date      : 26.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : PSI010
*  Description        : Processing for system status closed (CLSD)
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LRT_S_PROJ TYPE MPET_PSPID_RANG,
        LRT_S_WBS  TYPE RE_T_RSOPOSID.

* GENC Constants need to be maintained
* If GENC not found, then skip processing
  IF GRT_PROJ_DEF IS INITIAL OR
     GRT_WBS_LEVEL IS INITIAL OR
     ( GRT_OBTYP_PROJ IS INITIAL AND
       GRT_OBTYP_WBS IS INITIAL ).
    RETURN.
  ENDIF.

* Prepare Project list
  IF GRT_OBTYP_PROJ IS NOT INITIAL.
    FILL_PROJECT( EXPORTING IT_OBJECT = IT_OBJECT
                   CHANGING CT_PROJECT = LRT_S_PROJ ).
  ENDIF.

* Prepare WBS list
  IF GRT_OBTYP_WBS IS NOT INITIAL.
    FILL_WBS( EXPORTING IT_OBJECT = IT_OBJECT
               CHANGING CT_WBS = LRT_S_WBS ).
  ENDIF.

* No data to be processed
  IF LRT_S_PROJ IS INITIAL AND
     LRT_S_WBS IS INITIAL.
    RETURN.
  ENDIF.

  SUBMIT_INTERFACE_JOB_CLOSURE( EXPORTING IT_PROJECT = LRT_S_PROJ
                                          IT_WBS = LRT_S_WBS
                                          IF_TEST = IF_TEST
                                          IF_SYSTEM_STATUS = IF_SYSTEM_STATUS ).

ENDMETHOD.


METHOD SUBMIT_INTERFACE_JOB_CLOSURE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_PROJ_WBS_STATUS_INTF / SUBMIT_INTERFACE_JOB
*  Creation Date      : 26.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : PSI010
*  Description        : To submit program ZSDSPSR0020 to send outbound
*                       interface to salesforce when project or WBS
*                       system status is closed (CLSD)
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
  SUBMIT ZSDSPSR0020                             "#EC CI_SUBMIT
    WITH S_PROJ IN IT_PROJECT
    WITH S_WBS IN IT_WBS
    WITH CB_TEST EQ IF_TEST
    WITh CB_SYST EQ IF_SYSTEM_STATUS
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
