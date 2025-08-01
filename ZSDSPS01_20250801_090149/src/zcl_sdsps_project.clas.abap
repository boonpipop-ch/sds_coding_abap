class ZCL_SDSPS_PROJECT definition
  public
  final
  create public .

public section.

  types TS_PROC_STEP type TEXT30 .
  types:
    TT_PROFL_RANGE  TYPE  RANGE OF PROJ-PROFL .
  types:
    TT_ITMTY_RANGE  TYPE  RANGE OF ZSDSDE_PS_ITMTY .
  types:
    BEGIN OF TS_PLANVAL,
*       Adding new fields in this type may
*       impact on Corresponding movement
*       need to check carefully
        POSID         TYPE PRPS-POSID,
        VERSN         TYPE V_COSP_VIEW-VERSN,
        GJAHR         TYPE V_COSP_VIEW-GJAHR,
        KSTAR         TYPE V_COSP_VIEW-KSTAR,
        VAR_VAL_PER01 TYPE BAPICURR_D,
        VAR_VAL_PER02 TYPE BAPICURR_D,
        VAR_VAL_PER03 TYPE BAPICURR_D,
        VAR_VAL_PER04 TYPE BAPICURR_D,
        VAR_VAL_PER05 TYPE BAPICURR_D,
        VAR_VAL_PER06 TYPE BAPICURR_D,
        VAR_VAL_PER07 TYPE BAPICURR_D,
        VAR_VAL_PER08 TYPE BAPICURR_D,
        VAR_VAL_PER09 TYPE BAPICURR_D,
        VAR_VAL_PER10 TYPE BAPICURR_D,
        VAR_VAL_PER11 TYPE BAPICURR_D,
        VAR_VAL_PER12 TYPE BAPICURR_D,
      END OF TS_PLANVAL .
  types TS_WBSLIST type BAPI_WBS_ELEMENTS .
  types:
    TT_WBSLIST TYPE STANDARD TABLE OF TS_WBSLIST .
  types:
    BEGIN OF TS_PROJECTDEF,
        PSPID TYPE PROJ-PSPID,
        POST1 TYPE ZSDSDE_POST1,
        PLFAZ TYPE PROJ-PLFAZ,
        PLSEZ TYPE PROJ-PLSEZ,
        VERNR TYPE PROJ-VERNR,
        USR00 TYPE PROJ-USR00,
        USR01 TYPE PROJ-USR01,
        USR02 TYPE PROJ-USR02,
        USR03 TYPE PROJ-USR03,
        USR04 TYPE PROJ-USR04,
        USR10 TYPE PROJ-USR10,
        USR11 TYPE PROJ-USR11,
        PROFL TYPE PROJ-PROFL,
      END OF TS_PROJECTDEF .
  types:
    BEGIN OF TS_PROJECTDEFX,
        PSPID TYPE FLAG,
        POST1 TYPE FLAG,
        PLFAZ TYPE FLAG,
        PLSEZ TYPE FLAG,
        VERNR TYPE FLAG,
        USR00 TYPE FLAG,
        USR01 TYPE FLAG,
        USR02 TYPE FLAG,
        USR03 TYPE FLAG,
        USR04 TYPE FLAG,
        USR10 TYPE FLAG,
        USR11 TYPE FLAG,
        PROFL TYPE FLAG,
      END OF TS_PROJECTDEFX .
  types:
    BEGIN OF TS_WBSELEM,
        POSID TYPE PRPS-POSID,
        POST1 TYPE ZSDSDE_POST1,
        STUFE TYPE PRPS-STUFE,
        PSTRT TYPE PRTE-PSTRT,
        PENDE TYPE PRTE-PENDE,
        PRART TYPE PRPS-PRART,
        USR00 TYPE PRPS-USR00,
        USR01 TYPE PRPS-USR01,
        USR02 TYPE PRPS-USR02,
        USR03 TYPE PRPS-USR03,
        PRCTR TYPE PRPS-PRCTR,
        FKSTL TYPE PRPS-FKSTL,
        AKSTL TYPE PRPS-AKSTL,
      END OF TS_WBSELEM .
  types:
    TT_WBSELEM TYPE STANDARD TABLE OF TS_WBSELEM
                                                         WITH DEFAULT KEY .
  types:
    BEGIN OF TS_WBSELEMX,
        POSID TYPE FLAG,
        POST1 TYPE FLAG,
        STUFE TYPE FLAG,
        PSTRT TYPE FLAG,
        PENDE TYPE FLAG,
        PRART TYPE FLAG,
        USR00 TYPE FLAG,
        USR01 TYPE FLAG,
        USR02 TYPE FLAG,
        USR03 TYPE FLAG,
        PRCTR TYPE FLAG,
        FKSTL TYPE FLAG,
        AKSTL TYPE FLAG,
      END OF TS_WBSELEMX .
  types:
    BEGIN OF TS_WBS_UPD,
        WBSELEM  TYPE  TS_WBSELEM,
        WBSELEMX TYPE  TS_WBSELEMX,
      END OF TS_WBS_UPD .
  types:
    TT_WBS_UPD  TYPE  STANDARD TABLE OF TS_WBS_UPD
                                             WITH DEFAULT KEY .
  types:
    BEGIN OF TS_WBSLV,
        POSID     TYPE  PRPS-POSID,
        STUFE     TYPE  PRPS-STUFE,
        POSID_LV1 TYPE  PRPS-POSID,
        POSID_LV2 TYPE  PRPS-POSID,
        POSID_LV3 TYPE  PRPS-POSID,
        POSID_LV4 TYPE  PRPS-POSID,
      END OF TS_WBSLV .
  types:
    BEGIN OF TS_BUDGET_KEY,
        PSPID TYPE  PROJ-PSPID,
        GJAHR TYPE  V_COSP_VIEW-GJAHR,
        VERSN TYPE  V_COSP_VIEW-VERSN,
      END OF TS_BUDGET_KEY .
  types:
    BEGIN OF TS_BUDGET,
        POSID  TYPE  PRPS-POSID,
        OBJNR  TYPE  PRPS-OBJNR,
        KSTAR  TYPE  V_COSP_VIEW-KSTAR,
        GJAHR  TYPE  V_COSP_VIEW-GJAHR,
        MONAT  TYPE  MONAT,
        AMOUNT TYPE  WTGXXX,
      END OF TS_BUDGET .
  types:
    TT_BUDGET TYPE STANDARD TABLE OF TS_BUDGET
                                        WITH DEFAULT KEY .
  types:
    BEGIN OF TS_ADJ_EXP,
        KOSTL  TYPE  CSKS-KOSTL,
        KSTAR  TYPE  V_COSP_VIEW-KSTAR,
        GJAHR  TYPE  V_COSP_VIEW-GJAHR,
        MONAT  TYPE  MONAT,
        AMOUNT TYPE  WTGXXX,
      END OF TS_ADJ_EXP .
  types:
    TT_ADJ_EXP  TYPE SORTED TABLE OF TS_ADJ_EXP
                              WITH UNIQUE KEY KOSTL
                                              KSTAR
                                              GJAHR
                                              MONAT .

  constants GC_CURRTYP type COPLN_CURR value 'C' ##NO_TEXT.
  constants GC_LEDGER type ACDOCP-RLDNR value '0L' ##NO_TEXT.
  constants GC_LEDNR type V_COSP_VIEW-LEDNR value '00' ##NO_TEXT.
  constants GC_PLAN01 type V_COSP_VIEW-WRTTP value '01' ##NO_TEXT.
  constants GC_BUDGET41 type V_COSP_VIEW-WRTTP value '41' ##NO_TEXT.
  constants GC_PLAN10 type V_COSP_VIEW-WRTTP value '10' ##NO_TEXT.
  constants GC_ORIGINAL_VERSN_C type V_COSP_VIEW-VERSN value 'Z01' ##NO_TEXT.
  constants GC_ORIGINAL_VERSN_P type V_COSP_VIEW-VERSN value 'Z04' ##NO_TEXT.
  constants GC_BACKUP_VERSN type V_COSP_VIEW-VERSN value 'Z05' ##NO_TEXT.
  constants GC_CURRENT_VERSN type V_COSP_VIEW-VERSN value '000' ##NO_TEXT.
  constants GC_TYPE_PROJECT type CHAR1 value 'D' ##NO_TEXT.
  constants GC_TYPE_WBSELEM type CHAR1 value 'E' ##NO_TEXT.

  class-methods GET_PTIME
    importing
      !IF_BPROF type PROJ-BPROF
      !IF_GJAHR type GJAHR
      !IF_WRTTP type TBP1C-WRTTP
    exporting
      !EF_PTIME type CLIKE .
  class-methods GET_WBSELEM_LEVELS
    importing
      !IF_POSID type PRPS-POSID
    exporting
      !ES_WBSLV type TS_WBSLV
      !EF_INVALID type FLAG .
  class-methods CHECK_ORIGINAL_PLAN
    importing
      !IF_PSPID type PROJ-PSPID
      !IF_GJAHR type V_COSP_VIEW-GJAHR
    exporting
      !EF_EXIST type FLAG
      !ET_WBS type TT_WBSLIST .
  class-methods CREATE_PROJECTDEF
    importing
      !IS_PROJECTDEF type TS_PROJECTDEF
      !IF_TEST type FLAG default 'X'
    exporting
      !ES_RETURN type BAPIRETURN1 .
  class-methods CREATE_WBSELEM
    importing
      !IF_PSPID type PROJ-PSPID
      !IT_WBSELEM type TT_WBSELEM
      !IF_TEST type FLAG default 'X'
      !IF_SKIP_CHK_PSPID type FLAG default ' '
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods CREATE_BUDGET
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IS_KEY type TS_BUDGET_KEY
      !IF_CREATE_ORG_VERSN type FLAG
      !IT_BUDGET type TT_BUDGET
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB
      !EF_STEP type TS_PROC_STEP .
  class-methods CHANGE_PROJECTDEF
    importing
      !IS_PROJECTDEF type TS_PROJECTDEF
      !IS_PROJECTDEFX type TS_PROJECTDEFX
      !IF_TEST type FLAG default 'X'
    exporting
      !ES_RETURN type BAPIRETURN1 .
  class-methods CHANGE_WBSELEM
    importing
      !IF_PSPID type PROJ-PSPID
      !IT_WBSELEM type TT_WBS_UPD
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods CHANGE_BUDGET
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IS_KEY type TS_BUDGET_KEY
      !IT_BUDGET type TT_BUDGET
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB
      !EF_STEP type TS_PROC_STEP .
  class-methods VALIDATE_PSPID
    importing
      !IF_INPUT type CLIKE
      !IF_CREATE type FLAG default ' '
    exporting
      !EF_OUTPUT type PROJ-PSPID
      !EF_INVALID type FLAG .
  class-methods VALIDATE_PROJECTDEF
    importing
      !IS_PROJECTDEF type TS_PROJECTDEF
      !IS_PROJECTDEFX type TS_PROJECTDEFX optional
    exporting
      !EF_INVALID type FLAG
      !EF_MSGTX type ZSDSDE_MSGTX .
  class-methods VALIDATE_WBSELEM
    importing
      !IS_WBSELEM type TS_WBSELEM
      !IS_WBSELEMX type TS_WBSELEMX optional
      !IT_WBSELEM type TT_WBSELEM optional
    exporting
      !EF_INVALID type FLAG
      !EF_MSGTX type ZSDSDE_MSGTX
      !ES_WBSLV type TS_WBSLV .
  class-methods VALIDATE_BUDGET
    importing
      !IS_BUDGET type TS_BUDGET
    exporting
      !EF_INVALID type FLAG
      !EF_MSGTX type ZSDSDE_MSGTX
      !ES_WBSLV type TS_WBSLV .
protected section.
private section.

  class-data GT_PROFL_NOOVW type TT_PROFL_RANGE .
  class-data GT_ITMTY_EXP type TT_ITMTY_RANGE .

  class-methods COLLECT_ADJUST_EXPENSE
    importing
      !IF_POSID type PRPS-POSID
      !IF_KSTAR type V_COSP_VIEW-KSTAR
      !IF_GJAHR type V_COSP_VIEW-GJAHR
      !IF_MONAT type MONAT
      !IF_AMOUNT type WTGXXX
    changing
      !CT_ADJ_EXP type TT_ADJ_EXP .
  class-methods POST_ADJUST_EXP
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IS_KEY type TS_BUDGET_KEY
      !IT_ADJ_EXP type TT_ADJ_EXP
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods SAVE_LONGTEXT
    importing
      !IF_TYPE type CHAR1
      !IF_POSID type PS_POSID
      !IF_POST1 type ZSDSDE_POST1 .
  class-methods GET_1ST_LV3_LINE
    importing
      !IF_PSPID type PROJ-PSPID
    exporting
      !EF_POSID type PRPS-POSID
      !EF_LINE type NUMC2 .
  class-methods GET_SUM_PLAN_LV2
    importing
      !IS_KEY type TS_BUDGET_KEY
    exporting
      !EF_SUM_AMT type BP_WERT1 .
  class-methods READ_COSTELEM_PLAN
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IF_POSID type PRPS-POSID
      !IF_VERSN type V_COSP_VIEW-VERSN
      !IF_GJAHR type V_COSP_VIEW-GJAHR
      !IF_KSTAR type V_COSP_VIEW-KSTAR
    exporting
      !ES_PLANVAL type TS_PLANVAL .
  class-methods POST_COSTELEM_PLAN
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IS_KEY type TS_BUDGET_KEY
      !IT_BUDGET type TT_BUDGET
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB
      !EF_PERIOD_FROM type MONAT
      !EF_PERIOD_TO type MONAT
      !ET_ADJ_EXP type TT_ADJ_EXP .
  class-methods TOTOAL_UP_COST_PLAN
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IS_KEY type TS_BUDGET_KEY
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods TOTOAL_UP_REVENUE_PLAN
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IS_KEY type TS_BUDGET_KEY
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods COPY_BUDGET_FROM_PLAN
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IS_KEY type TS_BUDGET_KEY
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods UPDATE_PLAN_COST_TABLE
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IS_KEY type TS_BUDGET_KEY
      !IF_PERIOD_FROM type MONAT default '01'
      !IF_PERIOD_TO type MONAT default '12'
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods RELEASE_PROJECT_WBS
    importing
      !IF_PSPID type PROJ-PSPID
      !IT_WBS type TT_WBSLIST
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods REVISE_PROJECT_WBS
    importing
      !IF_PSPID type PROJ-PSPID
      !IT_WBS type TT_WBSLIST
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods COPY_PLAN_VERSION
    importing
      !IF_KOKRS type TKA01-KOKRS default '1000'
      !IF_PSPID type PROJ-PSPID
      !IF_VERSN_FROM type V_COSP_VIEW-VERSN
      !IF_VERSN_TO type V_COSP_VIEW-VERSN
      !IF_GJAHR type V_COSP_VIEW-GJAHR
      !IF_PERIOD_FROM type MONAT default '01'
      !IF_PERIOD_TO type MONAT default '12'
      !IF_TEST type FLAG default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  class-methods GET_GENC .
ENDCLASS.



CLASS ZCL_SDSPS_PROJECT IMPLEMENTATION.


METHOD CHANGE_BUDGET.

  DATA:
    LT_RETURN   TYPE BAPIRET2_TAB,
    LT_WBS_REL  TYPE STANDARD TABLE OF BAPI_WBS_ELEMENTS,
    LT_WBS      TYPE STANDARD TABLE OF BAPI_WBS_ELEMENTS,
    LT_WBS_REVS TYPE STANDARD TABLE OF BAPI_WBS_ELEMENTS,
    LT_ADJ_EXP  TYPE TT_ADJ_EXP.

  DATA:
    LS_RETURN TYPE  BAPIRET2,
    LS_KEY    TYPE  TS_BUDGET_KEY.

  DATA:
    LF_EXIST       TYPE  FLAG,
    LF_PERIOD_FROM TYPE  MONAT,
    LF_PERIOD_TO   TYPE  MONAT.


* Initialize Output
  CLEAR: ET_RETURN,
         EF_STEP.

* Get Constants
  GET_GENC( ).

* -----------------------
* Validate Version
* -----------------------
  EF_STEP = 'B1-VALIDATE'.

  IF IS_KEY-VERSN IS INITIAL.
    LS_RETURN-TYPE    = 'E'.
    LS_RETURN-ID      = 'ZSDSPS01'.
    LS_RETURN-NUMBER  = '000'.
*   Text-e12: Version is required.
    LS_RETURN-MESSAGE = TEXT-E12.
    INSERT LS_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* Check Original Plan Version exist?
* -----------------------
  EF_STEP = 'B2-ORIVERSN'.
  IF IS_KEY-PSPID(1) EQ 'P'.
*   Check Original Plan exist for project
    CHECK_ORIGINAL_PLAN( EXPORTING IF_PSPID = IS_KEY-PSPID
                                   IF_GJAHR = IS_KEY-GJAHR
                         IMPORTING EF_EXIST = LF_EXIST
                                   ET_WBS   = LT_WBS_REL ).
    IF LF_EXIST EQ SPACE.
      LS_RETURN-TYPE    = 'E'.
      LS_RETURN-ID      = 'ZSDSPS01'.
      LS_RETURN-NUMBER  = '000'.
*     Text-e15: Original Plan version does not exist for
      LS_RETURN-MESSAGE = |{ TEXT-E15 } { IS_KEY-PSPID } { IS_KEY-GJAHR }|.
      INSERT LS_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

* -----------------------
* Copy Version 0 to Backup Version
* -----------------------
  EF_STEP = 'B3-BCKVERSN'.

  COPY_PLAN_VERSION( EXPORTING IF_KOKRS       = IF_KOKRS
                               IF_PSPID       = IS_KEY-PSPID
                               IF_GJAHR       = IS_KEY-GJAHR
                               IF_PERIOD_FROM = '01'
                               IF_PERIOD_TO   = '12'
                               IF_VERSN_FROM  = GC_CURRENT_VERSN
                               IF_VERSN_TO    = GC_BACKUP_VERSN
                               IF_TEST        = IF_TEST
                     IMPORTING ET_RETURN      = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* Copy Version 0 to Processing Version
* -----------------------
  EF_STEP = 'B4-CURVERSN'.
  IF IS_KEY-VERSN NE GC_CURRENT_VERSN.
    COPY_PLAN_VERSION( EXPORTING IF_KOKRS       = IF_KOKRS
                                 IF_PSPID       = IS_KEY-PSPID
                                 IF_GJAHR       = IS_KEY-GJAHR
                                 IF_PERIOD_FROM = '01'
                                 IF_PERIOD_TO   = '12'
                                 IF_VERSN_FROM  = GC_CURRENT_VERSN
                                 IF_VERSN_TO    = IS_KEY-VERSN
                                 IF_TEST        = IF_TEST
                       IMPORTING ET_RETURN      = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

* -----------------------
* Post Cost Element Plan
* -----------------------
  EF_STEP = 'B5-CJR2'.
  POST_COSTELEM_PLAN( EXPORTING IF_KOKRS       = IF_KOKRS
                                IS_KEY         = IS_KEY
                                IT_BUDGET      = IT_BUDGET
                                IF_TEST        = IF_TEST
                      IMPORTING ET_RETURN      = LT_RETURN
                                EF_PERIOD_FROM = LF_PERIOD_FROM
                                EF_PERIOD_TO   = LF_PERIOD_TO
                                ET_ADJ_EXP     = LT_ADJ_EXP ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* Test Run Only end here...
  IF IF_TEST EQ 'X'.
    RETURN.
  ENDIF.

* -----------------------
* Post Expense Adjustment (KP06)
* -----------------------
  EF_STEP = 'B6-KP06'.
  IF LT_ADJ_EXP IS NOT INITIAL.
    POST_ADJUST_EXP( EXPORTING IF_KOKRS   = IF_KOKRS
                               IS_KEY     = IS_KEY
                               IT_ADJ_EXP = LT_ADJ_EXP
                               IF_TEST    = IF_TEST
                     IMPORTING ET_RETURN  = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

* -----------------------
* CJ40 Total up
* -----------------------
  EF_STEP = 'B7-CJ40'.
  TOTOAL_UP_COST_PLAN( EXPORTING IF_KOKRS  = IF_KOKRS
                                 IS_KEY    = IS_KEY
                                 IF_TEST   = IF_TEST
                       IMPORTING ET_RETURN = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* CJ42 Total up (Only for P project)
* -----------------------
  EF_STEP = 'B8-CJ42'.
  IF IS_KEY-PSPID(1) EQ 'P'.
    TOTOAL_UP_REVENUE_PLAN( EXPORTING IF_KOKRS  = IF_KOKRS
                                      IS_KEY    = IS_KEY
                                      IF_TEST   = IF_TEST
                            IMPORTING ET_RETURN = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

* -----------------------
* IF Processing Version is not Current, copy it to current
* -----------------------
  EF_STEP = 'B9-COPYCURVERSN'.
  IF IS_KEY-VERSN NE GC_CURRENT_VERSN.
    COPY_PLAN_VERSION( EXPORTING IF_KOKRS       = IF_KOKRS
                                 IF_PSPID       = IS_KEY-PSPID
                                 IF_GJAHR       = IS_KEY-GJAHR
                                 IF_PERIOD_FROM = LF_PERIOD_FROM
                                 IF_PERIOD_TO   = LF_PERIOD_TO
                                 IF_VERSN_FROM  = IS_KEY-VERSN
                                 IF_VERSN_TO    = GC_CURRENT_VERSN
                                 IF_TEST        = IF_TEST
                       IMPORTING ET_RETURN      = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

* -----------------------
* CJ30 Budget copy from plan
* -----------------------
  EF_STEP = 'B10-CJ30'.
* From now on processing with current version
  LS_KEY = IS_KEY.
  LS_KEY-VERSN = GC_CURRENT_VERSN.
  COPY_BUDGET_FROM_PLAN( EXPORTING IF_KOKRS  = IF_KOKRS
                                   IS_KEY    = LS_KEY
                                   IF_TEST   = IF_TEST
                         IMPORTING ET_RETURN = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* Run Update cost table
* -----------------------
  EF_STEP = 'B11-UPDCOST'.
  UPDATE_PLAN_COST_TABLE( EXPORTING IF_KOKRS       = IF_KOKRS
                                    IS_KEY         = IS_KEY
                                    IF_PERIOD_FROM = LF_PERIOD_FROM
                                    IF_PERIOD_TO   = LF_PERIOD_TO
                                    IF_TEST        = IF_TEST
                          IMPORTING ET_RETURN      = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* Release Project + WBS
* -----------------------
  EF_STEP = 'B12-RELWBS'.
  LT_WBS = CORRESPONDING #( IT_BUDGET ##ENH_OK
                              MAPPING WBS_ELEMENT = POSID ).
  SORT LT_WBS BY WBS_ELEMENT ASCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_WBS COMPARING WBS_ELEMENT.

  RELEASE_PROJECT_WBS( EXPORTING IF_PSPID  = IS_KEY-PSPID
                                 IT_WBS    = LT_WBS
                                 IF_TEST   = IF_TEST
                       IMPORTING ET_RETURN = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

  LT_WBS_REVS = LT_WBS.
* Only WBS that has plan to change to REVS status
  LOOP AT LT_WBS_REVS ASSIGNING FIELD-SYMBOL(<L_WBS_REVS>).
    READ TABLE LT_WBS_REL TRANSPORTING NO FIELDS
                          WITH KEY WBS_ELEMENT = <L_WBS_REVS>-WBS_ELEMENT
                          BINARY SEARCH.
    IF SY-SUBRC NE 0.
      DELETE LT_WBS_REVS.
    ENDIF.
  ENDLOOP.

* -----------------------
* Revise Project + WBS
* -----------------------
  EF_STEP = 'B13-REVWBS'.
  REVISE_PROJECT_WBS( EXPORTING IF_PSPID  = IS_KEY-PSPID
                                IT_WBS    = LT_WBS_REVS
                                IF_TEST   = IF_TEST
                      IMPORTING ET_RETURN = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD CHANGE_PROJECTDEF.

  DATA:
    LS_BAPI_PROJ    TYPE BAPI_PROJECT_DEFINITION,
    LS_BAPI_PROJ_UP TYPE BAPI_PROJECT_DEFINITION_UP.

  DATA:
    LF_INVALID TYPE  FLAG,
    LF_MSGTX   TYPE  ZSDSDE_MSGTX,
    LF_PSPNR   TYPE  PROJ-PSPNR.


* Initialize Output
  CLEAR ES_RETURN.

* Get Constants
  GET_GENC( ).

* Validate Project Def
  VALIDATE_PSPID( EXPORTING IF_INPUT = IS_PROJECTDEF-PSPID
                  IMPORTING EF_INVALID = LF_INVALID ).
  IF LF_INVALID IS NOT INITIAL OR
     IS_PROJECTDEF-PSPID IS INITIAL.
    ES_RETURN-TYPE    = 'E'.
    ES_RETURN-ID      = 'ZSDSPS01'.
    ES_RETURN-NUMBER  = '000'.
*   Text-e06: Project Definition does not exist:
    ES_RETURN-MESSAGE = |{ TEXT-E06 } { IS_PROJECTDEF-PSPID }|.
    RETURN.
  ENDIF.

* Assign Project Def internal format
  CALL FUNCTION 'CONVERSION_EXIT_KONPD_INPUT'
    EXPORTING
      INPUT     = IS_PROJECTDEF-PSPID
    IMPORTING
      OUTPUT    = LF_PSPNR
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
    ES_RETURN-TYPE    = 'E'.
    ES_RETURN-ID      = 'ZSDSPS01'.
    ES_RETURN-NUMBER  = '000'.
*   Text-e06: Project Definition does not exist:
    ES_RETURN-MESSAGE = |{ TEXT-E06 } { IS_PROJECTDEF-PSPID }|.
    RETURN.
  ENDIF.

* Validate Input
  VALIDATE_PROJECTDEF( EXPORTING IS_PROJECTDEF = IS_PROJECTDEF
                                 IS_PROJECTDEFX = IS_PROJECTDEFX
                       IMPORTING EF_INVALID = LF_INVALID
                                 EF_MSGTX   = LF_MSGTX ).
  IF LF_INVALID IS NOT INITIAL.
    ES_RETURN-TYPE    = 'E'.
    ES_RETURN-ID      = 'ZSDSPS01'.
    ES_RETURN-NUMBER  = '000'.
    ES_RETURN-MESSAGE = LF_MSGTX.
    RETURN.
  ENDIF.

* Stop Here in test run mode.
  IF IF_TEST EQ 'X'.
    RETURN.
  ENDIF.

  CLEAR LS_BAPI_PROJ.
  IF IS_PROJECTDEFX-PSPID EQ 'X'.
    LS_BAPI_PROJ-PROJECT_DEFINITION    = IS_PROJECTDEF-PSPID.
    LS_BAPI_PROJ_UP-PROJECT_DEFINITION = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-POST1 EQ 'X'.
    LS_BAPI_PROJ-DESCRIPTION    = IS_PROJECTDEF-POST1.
    LS_BAPI_PROJ_UP-DESCRIPTION = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-PLFAZ EQ 'X'.
    LS_BAPI_PROJ-START    = IS_PROJECTDEF-PLFAZ.
    LS_BAPI_PROJ_UP-START = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-PLSEZ EQ 'X'.
    LS_BAPI_PROJ-FINISH    = IS_PROJECTDEF-PLSEZ.
    LS_BAPI_PROJ_UP-FINISH = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-VERNR EQ 'X'.
    LS_BAPI_PROJ-RESPONSIBLE_NO    = IS_PROJECTDEF-VERNR.
    LS_BAPI_PROJ_UP-RESPONSIBLE_NO = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-USR00 EQ 'X'.
    LS_BAPI_PROJ-USR00    = IS_PROJECTDEF-USR00.
    LS_BAPI_PROJ_UP-USER_FIELD_CHAR20_1 = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-USR01 EQ 'X'.
    LS_BAPI_PROJ-USR01    = IS_PROJECTDEF-USR01.
    LS_BAPI_PROJ_UP-USER_FIELD_CHAR20_2 = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-USR02 EQ 'X'.
    LS_BAPI_PROJ-USR02    = IS_PROJECTDEF-USR02.
    LS_BAPI_PROJ_UP-USER_FIELD_CHAR10_1 = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-USR03 EQ 'X'.
    LS_BAPI_PROJ-USR03    = IS_PROJECTDEF-USR03.
    LS_BAPI_PROJ_UP-USER_FIELD_CHAR10_2 = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-USR04 EQ 'X'.
    LS_BAPI_PROJ-USR04    = IS_PROJECTDEF-USR04.
    LS_BAPI_PROJ_UP-USER_FIELD_QUAN1 = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-USR10 EQ 'X'.
    LS_BAPI_PROJ-USR10    = IS_PROJECTDEF-USR10.
    LS_BAPI_PROJ_UP-USER_FIELD_FLAG1 = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-USR11 EQ 'X'.
    LS_BAPI_PROJ-USR11    = IS_PROJECTDEF-USR11.
    LS_BAPI_PROJ_UP-USER_FIELD_FLAG2 = 'X'.
  ENDIF.
  IF IS_PROJECTDEFX-PROFL EQ 'X'.
    LS_BAPI_PROJ-PROJECT_PROFILE    = IS_PROJECTDEF-PROFL.
    LS_BAPI_PROJ_UP-PROJECT_PROFILE = 'X'.
  ENDIF.

  CALL FUNCTION 'BAPI_PROJECTDEF_UPDATE'
    EXPORTING
      CURRENTEXTERNALPROJE    = IS_PROJECTDEF-PSPID
      CURRENTINTERNALPROJE    = LF_PSPNR
      PROJECT_DEFINITION_STRU = LS_BAPI_PROJ
      PROJECT_DEFINITION_UP   = LS_BAPI_PROJ_UP
    IMPORTING
      RETURN                  = ES_RETURN.

  IF ES_RETURN-TYPE EQ 'E' OR
     ES_RETURN-TYPE EQ 'A' OR
     ES_RETURN-TYPE EQ 'X' .
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
*   Add Long Text if Description longer than 40 char
    IF STRLEN( IS_PROJECTDEF-POST1 ) GT 40 ##NUMBER_OK.
      SAVE_LONGTEXT(
        EXPORTING
          IF_TYPE  = GC_TYPE_PROJECT
          IF_POSID = IS_PROJECTDEF-PSPID
          IF_POST1 = IS_PROJECTDEF-POST1 ).
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD CHANGE_WBSELEM.

  DATA:
    LT_BAPI_WBS  TYPE  STANDARD TABLE OF BAPI_BUS2054_CHG,
    LT_BAPI_WBSX TYPE  STANDARD TABLE OF BAPI_BUS2054_UPD,
    LT_RETURN    TYPE  STANDARD TABLE OF BAPIRET2.

  DATA:
    LS_RETURN    TYPE  BAPIRET2,
    LS_BAPI_WBS  TYPE  BAPI_BUS2054_CHG,
    LS_BAPI_WBSX TYPE  BAPI_BUS2054_UPD.

  DATA:
    LF_INVALID TYPE  FLAG,
    LF_MSGTX   TYPE  ZSDSDE_MSGTX,
    LF_ERROR   TYPE  FLAG.


* Initialize Output
  CLEAR ET_RETURN.

* Get Constants
  GET_GENC( ).

* Validate Project Def
  VALIDATE_PSPID( EXPORTING IF_INPUT   = IF_PSPID
                  IMPORTING EF_INVALID = LF_INVALID ).
  IF LF_INVALID IS NOT INITIAL OR
     IF_PSPID IS INITIAL.
    LS_RETURN-TYPE    = 'E'.
    LS_RETURN-ID      = 'ZSDSPS01'.
    LS_RETURN-NUMBER  = '000'.
*   Text-e06: Project Definition does not exist:
    LS_RETURN-MESSAGE = |{ TEXT-E06 } { IF_PSPID }|.
    INSERT LS_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* Validate WBS and Assign BAPI data
  LOOP AT IT_WBSELEM ASSIGNING FIELD-SYMBOL(<L_WBSELEM>).
    VALIDATE_WBSELEM( EXPORTING IS_WBSELEM = <L_WBSELEM>-WBSELEM
                      IMPORTING EF_INVALID = LF_INVALID
                                EF_MSGTX   = LF_MSGTX ).
    IF LF_INVALID IS NOT INITIAL.
      LS_RETURN-TYPE    = 'E'.
      LS_RETURN-ID      = 'ZSDSPS01'.
      LS_RETURN-NUMBER  = '000'.
      LS_RETURN-MESSAGE = LF_MSGTX.
      INSERT LS_RETURN INTO TABLE ET_RETURN.
      EXIT.
    ENDIF.

*   Assign BAPI Input
    CLEAR LS_BAPI_WBS.
    LS_BAPI_WBS-WBS_ELEMENT           = <L_WBSELEM>-WBSELEM-POSID.
    LS_BAPI_WBSX-WBS_ELEMENT          = <L_WBSELEM>-WBSELEM-POSID.

    IF <L_WBSELEM>-WBSELEMX-POST1 EQ 'X'.
      LS_BAPI_WBS-DESCRIPTION           = <L_WBSELEM>-WBSELEM-POST1.
      LS_BAPI_WBSX-DESCRIPTION          = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-PSTRT EQ 'X'.
      LS_BAPI_WBS-WBS_BASIC_START_DATE  = <L_WBSELEM>-WBSELEM-PSTRT.
      LS_BAPI_WBSX-WBS_BASIC_START_DATE = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-PENDE EQ 'X'.
      LS_BAPI_WBS-WBS_BASIC_FINISH_DATE  = <L_WBSELEM>-WBSELEM-PENDE.
      LS_BAPI_WBSX-WBS_BASIC_FINISH_DATE = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-PRART EQ 'X'.
      LS_BAPI_WBS-PROJ_TYPE  = <L_WBSELEM>-WBSELEM-PRART.
      LS_BAPI_WBSX-PROJ_TYPE = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-USR00 EQ 'X'.
      LS_BAPI_WBS-USER_FIELD_CHAR20_1  = <L_WBSELEM>-WBSELEM-USR00.
      LS_BAPI_WBSX-USER_FIELD_CHAR20_1 = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-USR01 EQ 'X'.
      LS_BAPI_WBS-USER_FIELD_CHAR20_2  = <L_WBSELEM>-WBSELEM-USR01.
      LS_BAPI_WBSX-USER_FIELD_CHAR20_2 = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-USR02 EQ 'X'.
      LS_BAPI_WBS-USER_FIELD_CHAR10_1  = <L_WBSELEM>-WBSELEM-USR02.
      LS_BAPI_WBSX-USER_FIELD_CHAR10_1 = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-USR03 EQ 'X'.
      LS_BAPI_WBS-USER_FIELD_CHAR10_2  = <L_WBSELEM>-WBSELEM-USR03.
      LS_BAPI_WBSX-USER_FIELD_CHAR10_2 = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-PRCTR EQ 'X'.
      LS_BAPI_WBS-PROFIT_CTR  = <L_WBSELEM>-WBSELEM-PRCTR.
      LS_BAPI_WBSX-PROFIT_CTR = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-FKSTL EQ 'X'.
      LS_BAPI_WBS-RESPSBL_CCTR  = <L_WBSELEM>-WBSELEM-FKSTL.
      LS_BAPI_WBSX-RESPSBL_CCTR = 'X'.
    ENDIF.
    IF <L_WBSELEM>-WBSELEMX-AKSTL EQ 'X'.
      LS_BAPI_WBS-REQUEST_CCTR  = <L_WBSELEM>-WBSELEM-AKSTL.
      LS_BAPI_WBSX-REQUEST_CCTR = 'X'.
    ENDIF.

    INSERT LS_BAPI_WBS INTO TABLE LT_BAPI_WBS.
    INSERT LS_BAPI_WBSX INTO TABLE LT_BAPI_WBSX.

  ENDLOOP.
  IF ET_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.

  CLEAR LF_ERROR.
* Call BAPI To create WBS
  CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

  CALL FUNCTION 'BAPI_BUS2054_CHANGE_MULTI'
    EXPORTING
      I_PROJECT_DEFINITION  = IF_PSPID
    TABLES
      IT_WBS_ELEMENT        = LT_BAPI_WBS
      IT_UPDATE_WBS_ELEMENT = LT_BAPI_WBSX
      ET_RETURN             = LT_RETURN.

  LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                    WHERE TYPE = 'E' OR TYPE = 'A' OR TYPE = 'X'.
*   keep error
    LF_ERROR = 'X'.
    INSERT <L_RETURN> INTO TABLE ET_RETURN.
  ENDLOOP.

  DO 1 TIMES.

    CHECK LF_ERROR IS INITIAL.

    CLEAR LT_RETURN.

*   check data before commit
    CALL FUNCTION 'BAPI_PS_PRECOMMIT'
      TABLES
        ET_RETURN = LT_RETURN.
    LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                      WHERE TYPE = 'E' OR TYPE = 'A' OR TYPE = 'X'.
*     keep error
      LF_ERROR = 'X'.
      INSERT <L_RETURN> INTO TABLE ET_RETURN.
    ENDLOOP.
    CHECK LF_ERROR IS INITIAL.

  ENDDO.

* Stop Here in test run mode.
  IF IF_TEST EQ 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RETURN.
  ENDIF.

  IF LF_ERROR IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    LOOP AT IT_WBSELEM ASSIGNING <L_WBSELEM>.
      IF STRLEN( <L_WBSELEM>-WBSELEM-POST1 ) GT 40 AND ##NUMBER_OK
         <L_WBSELEM>-WBSELEMX-POST1 EQ 'X'.
        SAVE_LONGTEXT(
          EXPORTING
            IF_TYPE  = GC_TYPE_WBSELEM
            IF_POSID = <L_WBSELEM>-WBSELEM-POSID
            IF_POST1 = <L_WBSELEM>-WBSELEM-POST1 ).
      ENDIF.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDMETHOD.


METHOD CHECK_ORIGINAL_PLAN.

  DATA:
    LF_VERSN  TYPE  V_COSP_VIEW-VERSN.


* Initialize Output
  CLEAR: EF_EXIST,
         ET_WBS.

* Get Constants
  GET_GENC( ).

  IF IF_PSPID(1) EQ 'P'.
    LF_VERSN = GC_ORIGINAL_VERSN_P.
  ELSE.
    LF_VERSN = GC_ORIGINAL_VERSN_C.
  ENDIF.

* Check Version exist for Project + Fiscal year
  SELECT 'X' AS EXIST
    FROM PROJ AS A
           INNER JOIN PRPS AS B
             ON  B~PSPHI = A~PSPNR
           INNER JOIN V_COSP_VIEW AS C
             ON  C~OBJNR = B~OBJNR
             AND C~GJAHR = @IF_GJAHR
             AND C~LEDNR = @GC_LEDNR
             AND ( C~WRTTP = @GC_PLAN01 OR
                   C~WRTTP = @GC_PLAN10 )
             AND C~VERSN = @LF_VERSN
   WHERE A~PSPID EQ @IF_PSPID
    INTO @EF_EXIST
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Get List WBS Already Planned on Current Version
  IF ET_WBS IS SUPPLIED.
    SELECT DISTINCT B~POSID AS WBS_ELEMENT
      FROM PROJ AS A
             INNER JOIN PRPS AS B
               ON  B~PSPHI = A~PSPNR
             INNER JOIN V_COSP_VIEW AS C
               ON  C~OBJNR = B~OBJNR
               AND C~GJAHR = @IF_GJAHR
               AND C~LEDNR = @GC_LEDNR
               AND ( C~WRTTP = @GC_PLAN01 OR
                     C~WRTTP = @GC_PLAN10 )
               AND C~VERSN = @GC_CURRENT_VERSN
     WHERE A~PSPID EQ @IF_PSPID
     ORDER BY B~POSID ASCENDING
      INTO TABLE @ET_WBS.
    IF SY-SUBRC NE 0.
      CLEAR ET_WBS.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD COPY_BUDGET_FROM_PLAN.

  CONSTANTS:
    LC_CJ30    TYPE SY-TCODE VALUE 'CJ30'.

  DATA:
    LT_BDCDATA TYPE STANDARD TABLE OF BDCDATA,
    LT_MSG     TYPE TAB_BDCMSGCOLL.

  DATA:
    LS_OPT  TYPE  CTU_PARAMS.

  DATA:
    LF_LINES_PER_PAGE TYPE  I VALUE 8,
*    LF_NUMC           TYPE  N LENGTH 1,                     "-420000344
    LF_PTIME          TYPE  CHAR10,
    LF_ERROR          TYPE  FLAG,
    LF_POSID          TYPE  PRPS-POSID ##NEEDED,
    LF_LINE           TYPE  NUMC2,
    LF_FNAM           TYPE  BDCDATA-FNAM,
    LF_FVAL           TYPE  BDCDATA-FVAL.


* Initialize Output
  CLEAR: ET_RETURN.

* Read Project Profile
  SELECT SINGLE PROFL,
                BPROF,                                      "+420000344
                PPROF                                       "+420000344
    FROM PROJ
   WHERE PSPID EQ @IS_KEY-PSPID
*    INTO @DATA(LF_PROFL).                                   "-420000344
    INTO @DATA(LS_PROJ).                                    "+420000344
  IF SY-SUBRC NE 0.
*   Critical error
*    CLEAR LF_PROFL.                                         "-420000344
    CLEAR LS_PROJ.                                          "+420000344
  ENDIF.

  CLEAR: LT_BDCDATA.

* Set Controlling Area
  SET PARAMETER ID 'CAC' FIELD IF_KOKRS.

* Enter Project and press enter
  MC_BDC_DYNPRO 'SAPMKBUD' '0200'.
  MC_BDC_FIELD  'PROJ-PSPID' IS_KEY-PSPID.
  MC_BDC_FIELD  'BDC_OKCODE' '=MYENTER'.

* Assign PTIME
  IF GT_PROFL_NOOVW IS NOT INITIAL AND
*     NOT LF_PROFL IN GT_PROFL_NOOVW.                        "-420000344
     NOT LS_PROJ-PROFL IN GT_PROFL_NOOVW.                   "+420000344
    LF_PTIME = IS_KEY-GJAHR.
  ELSE.
*<-- Start of Insertion 420000344 29.01.2025 (Get Year List)
    GET_PTIME( EXPORTING IF_BPROF = LS_PROJ-BPROF
                         IF_GJAHR = IS_KEY-GJAHR
                         IF_WRTTP = GC_BUDGET41
               IMPORTING EF_PTIME = LF_PTIME ).
*--> End of Insertion 420000344 29.01.2025
*    LF_NUMC = IS_KEY-GJAHR - SY-DATUM(4).                   "-420000344
*    LF_PTIME = LF_NUMC.                                     "-420000344
  ENDIF.

* Enter Fiscal Year
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'DROPT-PTIME' LF_PTIME.
  MC_BDC_FIELD  'BDC_OKCODE' '=DROT'.

  GET_1ST_LV3_LINE(
    EXPORTING
      IF_PSPID = IS_KEY-PSPID
    IMPORTING
      EF_POSID = LF_POSID
      EF_LINE  = LF_LINE ).

  IF LF_LINE IS NOT INITIAL.

*   Page down till found needed line
    WHILE LF_LINE > LF_LINES_PER_PAGE.
*     Page down (1 Page = 8 Lines)
      MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
      MC_BDC_FIELD  'BDC_OKCODE' '=P+'.
      LF_LINE = LF_LINE - LF_LINES_PER_PAGE.
    ENDWHILE.

    LF_FVAL = |BPDY_NEU-UNTGR({ LF_LINE })|.
    LF_FNAM = |RCJ_MARKL-MARK({ LF_LINE })|.

*   Mark level 3 then collapse (Budget Only down to LV3)
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_CURSOR' LF_FVAL.
    MC_BDC_FIELD  LF_FNAM 'X'.
    MC_BDC_FIELD  'BDC_OKCODE' '=ABLS'.
  ENDIF.

* Select all
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'BDC_OKCODE' '=MRKA'.

* Copy View
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'BDC_OKCODE' '=KOPS'.

* Select Planned Total
  MC_BDC_DYNPRO 'SAPLSPO5' '0130'.
  MC_BDC_FIELD  'SPOPLI-SELFLAG(01)' SPACE.
  MC_BDC_FIELD  'SPOPLI-SELFLAG(05)' 'X'.
  MC_BDC_FIELD  'BDC_OKCODE' '=OK'.

* Enter Overwrite 100%
  MC_BDC_DYNPRO 'SAPLKBPP' '0706'.
  MC_BDC_FIELD  'BPDY-COPY_PERC' '100.00'.
  MC_BDC_FIELD  'BPDY-COPY_REPL' 'X'.
  MC_BDC_FIELD  'BDC_OKCODE' '=ENTE'.

* Process Copy Overview
  IF GT_PROFL_NOOVW IS NOT INITIAL AND
*     NOT LF_PROFL IN GT_PROFL_NOOVW.                        "-420000344
     NOT LS_PROJ-PROFL IN GT_PROFL_NOOVW.                   "+420000344

*   Select Overall
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'DROPT-PTIME' '0'.
    MC_BDC_FIELD  'BDC_OKCODE' '=DROT'.

*   Select all
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=MRKA'.

*   Copy View
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=KOPS'.

*   Select Cumulative
    MC_BDC_DYNPRO 'SAPLSPO5' '0130'.
    MC_BDC_FIELD  'SPOPLI-SELFLAG(01)' SPACE.
    MC_BDC_FIELD  'SPOPLI-SELFLAG(03)' 'X'.
    MC_BDC_FIELD  'BDC_OKCODE' '=OK'.

*   Enter Overwrite 100%
    MC_BDC_DYNPRO 'SAPLKBPP' '0706'.
    MC_BDC_FIELD  'BPDY-COPY_PERC' '100.00'.
    MC_BDC_FIELD  'BPDY-COPY_REPL' 'X'.
    MC_BDC_FIELD  'BDC_OKCODE' '=ENTE'.

  ENDIF.

  IF IF_TEST EQ 'X'.
*   Back to exit
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=BACK'.
  ELSE.
*   Save
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=POST'.
  ENDIF.

  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.

* Call CJ30
  CALL TRANSACTION LC_CJ30 WITHOUT AUTHORITY-CHECK       "#EC CI_CALLTA
                           USING LT_BDCDATA
                           OPTIONS FROM LS_OPT
                           MESSAGES INTO LT_MSG.
  IF SY-SUBRC NE 0.
    LF_ERROR = 'X'.
  ENDIF.

* Find Error Message
  LOOP AT LT_MSG ASSIGNING FIELD-SYMBOL(<L_MSG>)
                 WHERE ( MSGTYP EQ 'E' OR
                         MSGTYP EQ 'A' OR
                         MSGTYP EQ 'X' ).
    MESSAGE ID <L_MSG>-MSGID
            TYPE <L_MSG>-MSGTYP
            NUMBER <L_MSG>-MSGNR
            WITH <L_MSG>-MSGV1 <L_MSG>-MSGV2
                 <L_MSG>-MSGV3 <L_MSG>-MSGV4
            INTO DATA(LF_MSGTX).
*   Assign Result
    INSERT VALUE #( TYPE = 'E'
                    ID   = <L_MSG>-MSGID
                    NUMBER = <L_MSG>-MSGNR
                    MESSAGE = LF_MSGTX
                    MESSAGE_V1 = <L_MSG>-MSGV1
                    MESSAGE_V2 = <L_MSG>-MSGV2
                    MESSAGE_V3 = <L_MSG>-MSGV3
                    MESSAGE_V4 = <L_MSG>-MSGV4
                    PARAMETER  = IS_KEY-PSPID
                    ROW   = IS_KEY-GJAHR
                    FIELD = IS_KEY-VERSN )
                  INTO TABLE ET_RETURN.

  ENDLOOP.

* Default Error message
  IF LF_ERROR EQ 'X' AND
     ET_RETURN IS INITIAL.
*   Text-e16: Error during processing tcode
    MESSAGE E000(ZSDSPS01) WITH TEXT-E16 LC_CJ30 SPACE SPACE
                           INTO LF_MSGTX.
*   Assign Result
    INSERT VALUE #( TYPE = 'E'
                    ID   = 'ZSDSPS01'
                    NUMBER = '000'
                    MESSAGE = LF_MSGTX
                    MESSAGE_V1 = TEXT-E16
                    MESSAGE_V2 = LC_CJ30
                    PARAMETER  = IS_KEY-PSPID
                    ROW   = IS_KEY-GJAHR
                    FIELD = IS_KEY-VERSN )
                  INTO TABLE ET_RETURN.
  ENDIF.

ENDMETHOD.


METHOD COPY_PLAN_VERSION.

  DATA:
    LT_OBJNR TYPE  STANDARD TABLE OF KPU3_OBJNR,
    LT_TMP1  TYPE  STANDARD TABLE OF KPU3_OBJNR,
    LT_TMP2  TYPE  STANDARD TABLE OF KPU3_OBJNR,
    LT_LIST1 TYPE  STANDARD TABLE OF KPU3_LIST,
    LT_MESG1 TYPE  STANDARD TABLE OF MESG,
    LT_VRGNG TYPE  STANDARD TABLE OF KPU3_VRGNG,
    LT_MESG  TYPE  STANDARD TABLE OF SMESG.

  DATA:
    LS_DYNP   TYPE  RKPT3_DYNP,
    LS_SINFO  TYPE  KPU3_SINFO,
    LS_TINFO  TYPE  KPU3_TINFO,
    LS_CTRL   TYPE  KPU3_CTRL,
    LS_RESULT TYPE  KPU3_RESULT.

  DATA:
    LF_BKTXT  TYPE  COBK-BLTXT.


* Initialize Output
  CLEAR: ET_RETURN.

* Assign Source Info
  CLEAR LS_SINFO.
  LS_SINFO-GJAHR = IF_GJAHR.
  LS_SINFO-VERSN = IF_VERSN_FROM.
  LS_SINFO-PERAB = IF_PERIOD_FROM.
  LS_SINFO-PERBI = IF_PERIOD_TO.

* Assign Target Info
  CLEAR LS_TINFO.
  LS_TINFO-GJAHR = IF_GJAHR.
  LS_TINFO-VERSN = IF_VERSN_TO.
  LS_TINFO-PERAB = IF_PERIOD_FROM.
  LS_TINFO-PERBI = IF_PERIOD_TO.

* Assign Control data
  CLEAR LS_CTRL.
  LS_CTRL-PRICE_QUANT     = 'B'.
  LS_CTRL-FLG_TEST        = IF_TEST.
  LS_CTRL-FLG_UPDATE      = 'X'.
  LS_CTRL-FLG_STRUMI      = 'X'.
  LS_CTRL-FLG_SPLIT       = 'X'.
  LS_CTRL-FLG_CTWAER      = 'X'.
  LS_CTRL-FLG_PKWAER      = 'X'.
  LS_CTRL-FLG_NO_VERSENQU = 'X'.
  LS_CTRL-REVPRCT         = '100.00'.

* Fix business type (Reference from CJ9BS
* Subroutine fill_transactions_pr in Program SAPMKPT4
  INSERT VALUE #( VRGNG = 'RKP1' ) INTO TABLE LT_VRGNG.
  INSERT VALUE #( VRGNG = 'RKP8' ) INTO TABLE LT_VRGNG.
  INSERT VALUE #( VRGNG = 'RKPZ' ) INTO TABLE LT_VRGNG.
  INSERT VALUE #( VRGNG = 'RKP3' ) INTO TABLE LT_VRGNG.
  INSERT VALUE #( VRGNG = 'RKP4' ) INTO TABLE LT_VRGNG.
  INSERT VALUE #( VRGNG = 'RKP5' ) INTO TABLE LT_VRGNG.
  INSERT VALUE #( VRGNG = 'RKPW' ) INTO TABLE LT_VRGNG.

* Get List of Object number
  SELECT B~OBJNR AS OBJNR
    FROM PROJ AS A
           INNER JOIN PRPS AS B
             ON  B~PSPHI = A~PSPNR
   WHERE A~PSPID EQ @IF_PSPID
    INTO TABLE @LT_OBJNR.
  IF SY-SUBRC NE 0.
*   Text-e20: Cannot find object number for copying plan version.
    INSERT VALUE #( TYPE = 'E'
                    ID   = 'ZSDSPS01'
                    NUMBER = '000'
                    MESSAGE    = TEXT-E20
                    PARAMETER  = IF_PSPID
                    ROW        = IF_GJAHR
                    FIELD      = IF_VERSN_FROM )
           INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* --------------------------
* Copy Overall Scenario
* --------------------------
  CLEAR LS_DYNP.
  LS_DYNP-SVERSN               = IF_VERSN_FROM.
  LS_DYNP-SGJAHR               = IF_GJAHR.
  LS_DYNP-SPERAB               = IF_PERIOD_FROM.
  LS_DYNP-SPERBI               = IF_PERIOD_TO.
  LS_DYNP-TVERSN               = IF_VERSN_TO.
  LS_DYNP-TGJAHR               = IF_GJAHR.
  LS_DYNP-TPERAB               = IF_PERIOD_FROM.
  LS_DYNP-TPERBI               = IF_PERIOD_TO.
  LS_DYNP-REVPRCT              = '100.00'.
  LS_DYNP-FOVER                = 'H1'.
  LS_DYNP-FLG_VRGALL           = 'X'.
  LS_DYNP-UPDATE_YES           = 'X'.
  LS_DYNP-FLG_SOBJ_NO          = 'X'.
  LS_DYNP-FLG_TEST             = IF_TEST.
  LS_DYNP-FLG_PROTO            = 'X'.
  LS_DYNP-FLG_STRUM            = 'X'.
  LS_DYNP-FLG_SPLIT            = 'X'.
  LS_DYNP-FLG_CTWAER           = 'X'.
  LS_DYNP-FLG_PKWAER           = 'X'.
  LS_DYNP-VRGNG_RKP1           = 'X'.
  LS_DYNP-VRGNG_RKP3           = 'X'.
  LS_DYNP-VRGNG_RKP4           = 'X'.
  LS_DYNP-VRGNG_RKP5           = 'X'.
  LS_DYNP-VRGNG_RKPW           = 'X'.
  LS_DYNP-VRGNG_KSTP           = 'X'.
  LS_DYNP-VRGNG_KSTR           = 'X'.
  LS_DYNP-VRGNG_KSTE_KEKX      = 'X'.
  LS_DYNP-VRGNG_KSTP_YEAR      = 'X'.
  LS_DYNP-VRGNG_KSTR_YEAR      = 'X'.
  LS_DYNP-VRGNG_KSTE_KEKX_YEAR = 'X'.

* Add object List from Project + WBS
  LT_TMP1 = LT_OBJNR.
  SELECT OBJNR AS OBJNR
    FROM PROJ
   WHERE PSPID EQ @IF_PSPID
    APPENDING TABLE @LT_TMP1.

  CALL FUNCTION 'KOPV_CALL_COPY_SCENARIO1_PS'
    EXPORTING
      I_RKPT3_DYNP = LS_DYNP
      I_KOKRS      = IF_KOKRS
    TABLES
      IT_SOBJNR    = LT_TMP2
      CT_TOBJNR    = LT_TMP1
      ET_LIST      = LT_LIST1
      ET_MESG      = LT_MESG1.

* Delete Ignore message
* - BS013: System status & is active (&)
*       - Cause: error if WBS already TECO which the rest still copied
*                The error must be ignored to be able to continue further processing.
  DELETE LT_MESG1 WHERE ARBGB EQ 'BS'
                    AND TXTNR EQ '013'
                    AND MSGTY EQ 'E'.

* Delete Ignore message
* - BS007: "&" is not allowed (&)
*       - Cause: error if WBS already TECO which the rest still copied
*                The error must be ignored to be able to continue further processing.
  DELETE LT_MESG1 WHERE ARBGB EQ 'BS'
                    AND TXTNR EQ '007'
                    AND MSGTY EQ 'E'.

* Assign Error
  LOOP AT LT_MESG1 ASSIGNING FIELD-SYMBOL(<L_MESG1>)
                  WHERE ( MSGTY EQ 'E' OR
                          MSGTY EQ 'A' OR
                          MSGTY EQ 'X' ).
    INSERT VALUE #( TYPE = 'E'
                    MESSAGE    = <L_MESG1>-TEXT
                    MESSAGE_V1 = <L_MESG1>-MSGV1
                    MESSAGE_V2 = <L_MESG1>-MSGV2
                    MESSAGE_V3 = <L_MESG1>-MSGV3
                    MESSAGE_V4 = <L_MESG1>-MSGV4
                    PARAMETER  = IF_PSPID
                    ROW        = IF_GJAHR
                    FIELD      = IF_VERSN_FROM )
           INTO TABLE ET_RETURN.
  ENDLOOP.
  IF ET_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.

* --------------------------
* Copy Plan Cost
* --------------------------
  CALL FUNCTION 'K_PLAN_TO_PLAN_COPY'
    EXPORTING
      ID_KOKRS                  = IF_KOKRS
      IS_SINFO                  = LS_SINFO
      IS_TINFO                  = LS_TINFO
      IS_CTRL                   = LS_CTRL
      ID_BLTXT                  = LF_BKTXT
    IMPORTING
      ES_RESULT                 = LS_RESULT
    TABLES
      CT_OBJNR                  = LT_OBJNR
      CT_VRGNG                  = LT_VRGNG
      ET_MESG                   = LT_MESG
    EXCEPTIONS
      ERROR_PERIODS             = 1
      VERSION_NOT_FOUND         = 2
      NO_PLAN_VERSN_SOURCE      = 3
      TVERSN_NO_CHANGES_ALLOWED = 4
      NOTHING_TO_DO             = 5
      ERROR_CALCULATION         = 6
      VRGNG_BLOCKED             = 7
      ENQUEUED_OBJECTS          = 8
      ACTIVITIES_NOT_POSTED     = 9
      VERSN_BLOCKED             = 10
      OTHERS                    = 11.
  IF SY-SUBRC <> 0.
*   Text-e19: Error occured during copy plan version.
    INSERT VALUE #( TYPE = 'E'
                    ID   = 'ZSDSPS01'
                    NUMBER = '000'
                    MESSAGE    = TEXT-E19
                    PARAMETER  = IF_PSPID
                    ROW        = IF_GJAHR
                    FIELD      = IF_VERSN_FROM )
           INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* Delete Ignore message
* - BS013: System status & is active (&)
*       - Cause: error if WBS already TECO which the rest still copied
*                The error must be ignored to be able to continue further processing.
* Reduce Failed count
  LOOP AT LT_MESG TRANSPORTING NO FIELDS WHERE ARBGB EQ 'BS'
                                           AND TXTNR EQ '013'
                                           AND MSGTY EQ 'E'.
    LS_RESULT-NO_FAILS = LS_RESULT-NO_FAILS - 1.
    DELETE LT_MESG.
  ENDLOOP.

* Delete Ignore message
* - BS007: "&" is not allowed (&)
*       - Cause: error if WBS already TECO which the rest still copied
*                The error must be ignored to be able to continue further processing.
  DELETE LT_MESG WHERE ARBGB EQ 'BS'
                   AND TXTNR EQ '007'
                   AND MSGTY EQ 'E'.

* Assign Error
  LOOP AT LT_MESG ASSIGNING FIELD-SYMBOL(<L_MESG>)
                  WHERE ( MSGTY EQ 'E' OR
                          MSGTY EQ 'A' OR
                          MSGTY EQ 'X' ).
    INSERT VALUE #( TYPE = 'E'
                    MESSAGE    = <L_MESG>-TEXT
                    MESSAGE_V1 = <L_MESG>-MSGV1
                    MESSAGE_V2 = <L_MESG>-MSGV2
                    MESSAGE_V3 = <L_MESG>-MSGV3
                    MESSAGE_V4 = <L_MESG>-MSGV4
                    PARAMETER  = IF_PSPID
                    ROW        = IF_GJAHR
                    FIELD      = IF_VERSN_FROM )
           INTO TABLE ET_RETURN.
  ENDLOOP.

  IF LS_RESULT-NO_FAILS IS NOT INITIAL AND
     ET_RETURN IS INITIAL.
*   Text-e19: Error occured during copy plan version.
    INSERT VALUE #( TYPE = 'E'
                    ID   = 'ZSDSPS01'
                    NUMBER = '000'
                    MESSAGE    = TEXT-E19
                    PARAMETER  = IF_PSPID
                    ROW        = IF_GJAHR
                    FIELD      = IF_VERSN_FROM )
           INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD CREATE_BUDGET.

  DATA:
    LT_RETURN  TYPE BAPIRET2_TAB,
    LT_WBS     TYPE STANDARD TABLE OF BAPI_WBS_ELEMENTS,
    LT_ADJ_EXP TYPE  TT_ADJ_EXP.

  DATA:
    LS_RETURN  TYPE  BAPIRET2.

  DATA:
    LF_EXIST       TYPE  FLAG,
    LF_PERIOD_FROM TYPE  MONAT,
    LF_PERIOD_TO   TYPE  MONAT,
    LF_VERSN       TYPE  V_COSP_VIEW-VERSN.


* Initialize Output
  CLEAR: ET_RETURN,
         EF_STEP.

* Get Constants
  GET_GENC( ).

* -----------------------
* Validate Version
* -----------------------
  EF_STEP = 'A1-VALIDATE'.

  IF IS_KEY-VERSN IS INITIAL.
    LS_RETURN-TYPE    = 'E'.
    LS_RETURN-ID      = 'ZSDSPS01'.
    LS_RETURN-NUMBER  = '000'.
*   Text-e12: Version is required.
    LS_RETURN-MESSAGE = TEXT-E12.
    INSERT LS_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ELSEIF IS_KEY-VERSN NE '000'.
    LS_RETURN-TYPE    = 'E'.
    LS_RETURN-ID      = 'ZSDSPS01'.
    LS_RETURN-NUMBER  = '000'.
*   Text-e13: Version must be 0 for 1st upload plan.
    LS_RETURN-MESSAGE = TEXT-E13.
    INSERT LS_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* Check Original Plan Version exist?
* -----------------------
  EF_STEP = 'A2-ORIVERSN'.
  IF IF_CREATE_ORG_VERSN EQ 'X'.
*   Check Original Plan exist for project
    CHECK_ORIGINAL_PLAN( EXPORTING IF_PSPID = IS_KEY-PSPID
                                   IF_GJAHR = IS_KEY-GJAHR
                         IMPORTING EF_EXIST = LF_EXIST ).

    IF LF_EXIST EQ 'X'.

*     Clear Original Version with Version Z04 (Always Blank for Project C)
      IF IS_KEY-PSPID(1) EQ 'C'.
        COPY_PLAN_VERSION( EXPORTING IF_KOKRS       = IF_KOKRS
                                     IF_PSPID       = IS_KEY-PSPID
                                     IF_GJAHR       = IS_KEY-GJAHR
                                     IF_PERIOD_FROM = '01'
                                     IF_PERIOD_TO   = '12'
                                     IF_VERSN_FROM  = GC_ORIGINAL_VERSN_P
                                     IF_VERSN_TO    = IS_KEY-VERSN
                                     IF_TEST        = IF_TEST
                           IMPORTING ET_RETURN      = LT_RETURN ).
*     Error Plan already exists
      ELSE.
        LS_RETURN-TYPE    = 'E'.
        LS_RETURN-ID      = 'ZSDSPS01'.
        LS_RETURN-NUMBER  = '000'.
*       Text-e14: Original Plan version already exists for
        LS_RETURN-MESSAGE = |{ TEXT-E14 } { IS_KEY-PSPID } { IS_KEY-GJAHR }|.
        INSERT LS_RETURN INTO TABLE ET_RETURN.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

* -----------------------
* Post Cost Element Plan
* -----------------------
  EF_STEP = 'A3-CJR2'.
  POST_COSTELEM_PLAN( EXPORTING IF_KOKRS       = IF_KOKRS
                                IS_KEY         = IS_KEY
                                IT_BUDGET      = IT_BUDGET
                                IF_TEST        = IF_TEST
                      IMPORTING ET_RETURN      = LT_RETURN
                                EF_PERIOD_FROM = LF_PERIOD_FROM
                                EF_PERIOD_TO   = LF_PERIOD_TO
                                ET_ADJ_EXP     = LT_ADJ_EXP ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* Test Run Only end here...
  IF IF_TEST EQ 'X'.
    RETURN.
  ENDIF.

* -----------------------
* Post Expense Adjustment (KP06)
* -----------------------
  EF_STEP = 'A4-KP06'.
  IF LT_ADJ_EXP IS NOT INITIAL.
    POST_ADJUST_EXP( EXPORTING IF_KOKRS   = IF_KOKRS
                               IS_KEY     = IS_KEY
                               IT_ADJ_EXP = LT_ADJ_EXP
                               IF_TEST    = IF_TEST
                     IMPORTING ET_RETURN  = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

* -----------------------
* CJ40 Total up
* -----------------------
  EF_STEP = 'A5-CJ40'.
  TOTOAL_UP_COST_PLAN( EXPORTING IF_KOKRS  = IF_KOKRS
                                 IS_KEY    = IS_KEY
                                 IF_TEST   = IF_TEST
                       IMPORTING ET_RETURN = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* CJ42 Total up (Only for P project)
* -----------------------
  EF_STEP = 'A6-CJ42'.
  IF IS_KEY-PSPID(1) EQ 'P'.
    TOTOAL_UP_REVENUE_PLAN( EXPORTING IF_KOKRS  = IF_KOKRS
                                      IS_KEY    = IS_KEY
                                      IF_TEST   = IF_TEST
                            IMPORTING ET_RETURN = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

* -----------------------
* CJ30 Budget copy from plan
* -----------------------
  EF_STEP = 'A7-CJ30'.
  COPY_BUDGET_FROM_PLAN( EXPORTING IF_KOKRS  = IF_KOKRS
                                   IS_KEY    = IS_KEY
                                   IF_TEST   = IF_TEST
                         IMPORTING ET_RETURN = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* Run Update cost table
* -----------------------
  EF_STEP = 'A8-UPDCOST'.
  UPDATE_PLAN_COST_TABLE( EXPORTING IF_KOKRS       = IF_KOKRS
                                    IS_KEY         = IS_KEY
                                    IF_PERIOD_FROM = LF_PERIOD_FROM
                                    IF_PERIOD_TO   = LF_PERIOD_TO
                                    IF_TEST        = IF_TEST
                          IMPORTING ET_RETURN      = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* Release Project + WBS
* -----------------------
  EF_STEP = 'A9-RELWBS'.
  LT_WBS = CORRESPONDING #( IT_BUDGET        ##ENH_OK
                              MAPPING WBS_ELEMENT = POSID ).
  SORT LT_WBS BY WBS_ELEMENT ASCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_WBS COMPARING WBS_ELEMENT.

  RELEASE_PROJECT_WBS( EXPORTING IF_PSPID  = IS_KEY-PSPID
                                 IT_WBS    = LT_WBS
                                 IF_TEST   = IF_TEST
                       IMPORTING ET_RETURN = LT_RETURN ).
  IF LT_RETURN IS NOT INITIAL.
    INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* -----------------------
* Copy version processing to Original Version
* -----------------------
  EF_STEP = 'A10-COPYORIVERSN'.
  IF IF_CREATE_ORG_VERSN EQ 'X'.
*   Assign Original plan version
    IF IS_KEY-PSPID(1) EQ 'P'.
      LF_VERSN = GC_ORIGINAL_VERSN_P.
    ELSE.
      LF_VERSN = GC_ORIGINAL_VERSN_C.
    ENDIF.
    COPY_PLAN_VERSION( EXPORTING IF_KOKRS       = IF_KOKRS
                                 IF_PSPID       = IS_KEY-PSPID
                                 IF_GJAHR       = IS_KEY-GJAHR
                                 IF_PERIOD_FROM = LF_PERIOD_FROM
                                 IF_PERIOD_TO   = LF_PERIOD_TO
                                 IF_VERSN_FROM  = IS_KEY-VERSN
                                 IF_VERSN_TO    = LF_VERSN
                                 IF_TEST        = IF_TEST
                       IMPORTING ET_RETURN      = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
*   Also Copy to Backup Version (Z05)
*   - This is added to support if some WBS TECO before revise
*     the plan of TECO WBS will not be copied. Therefore, the
*     program must copy at create time.
    COPY_PLAN_VERSION( EXPORTING IF_KOKRS       = IF_KOKRS
                                 IF_PSPID       = IS_KEY-PSPID
                                 IF_GJAHR       = IS_KEY-GJAHR
                                 IF_PERIOD_FROM = LF_PERIOD_FROM
                                 IF_PERIOD_TO   = LF_PERIOD_TO
                                 IF_VERSN_FROM  = IS_KEY-VERSN
                                 IF_VERSN_TO    = GC_BACKUP_VERSN
                                 IF_TEST        = IF_TEST
                       IMPORTING ET_RETURN      = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD CREATE_PROJECTDEF.

  DATA:
    LS_BAPI_PROJ TYPE  BAPI_PROJECT_DEFINITION.

  DATA:
    LF_INVALID TYPE  FLAG,
    LF_MSGTX   TYPE  ZSDSDE_MSGTX.


* Initialize Output
  CLEAR ES_RETURN.

* Get Constants
  GET_GENC( ).

* Validate Input
  VALIDATE_PROJECTDEF( EXPORTING IS_PROJECTDEF = IS_PROJECTDEF
                       IMPORTING EF_INVALID    = LF_INVALID
                                 EF_MSGTX      = LF_MSGTX ).
  IF LF_INVALID IS NOT INITIAL.
    ES_RETURN-TYPE    = 'E'.
    ES_RETURN-ID      = 'ZSDSPS01'.
    ES_RETURN-NUMBER  = '000'.
    ES_RETURN-MESSAGE = LF_MSGTX.
    RETURN.
  ENDIF.

* Stop Here in test run mode.
  IF IF_TEST EQ 'X'.
    RETURN.
  ENDIF.

  CLEAR LS_BAPI_PROJ.
  LS_BAPI_PROJ-PROJECT_DEFINITION = IS_PROJECTDEF-PSPID.
  LS_BAPI_PROJ-DESCRIPTION        = IS_PROJECTDEF-POST1.
  LS_BAPI_PROJ-START              = IS_PROJECTDEF-PLFAZ.
  LS_BAPI_PROJ-FINISH             = IS_PROJECTDEF-PLSEZ.
  LS_BAPI_PROJ-RESPONSIBLE_NO     = IS_PROJECTDEF-VERNR.
  LS_BAPI_PROJ-USR00              = IS_PROJECTDEF-USR00.
  LS_BAPI_PROJ-USR01              = IS_PROJECTDEF-USR01.
  LS_BAPI_PROJ-USR02              = IS_PROJECTDEF-USR02.
  LS_BAPI_PROJ-USR03              = IS_PROJECTDEF-USR03.
  LS_BAPI_PROJ-USR04              = IS_PROJECTDEF-USR04.
  LS_BAPI_PROJ-USR10              = IS_PROJECTDEF-USR10.
  LS_BAPI_PROJ-USR11              = IS_PROJECTDEF-USR11.
  LS_BAPI_PROJ-PROJECT_PROFILE    = IS_PROJECTDEF-PROFL.

  CALL FUNCTION 'BAPI_PROJECTDEF_CREATE'
    EXPORTING
      PROJECT_DEFINITION_STRU = LS_BAPI_PROJ
    IMPORTING
      RETURN                  = ES_RETURN.

  IF ES_RETURN-TYPE EQ 'E' OR
     ES_RETURN-TYPE EQ 'A' OR
     ES_RETURN-TYPE EQ 'X' .
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

*   Add Long Text if Description longer than 40 char
    IF STRLEN( IS_PROJECTDEF-POST1 ) GT 40 ##NUMBER_OK.
      SAVE_LONGTEXT(
        EXPORTING
          IF_TYPE  = GC_TYPE_PROJECT
          IF_POSID = IS_PROJECTDEF-PSPID
          IF_POST1 = IS_PROJECTDEF-POST1 ).
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD CREATE_WBSELEM.

  DATA:
    LT_WBSELEM  TYPE  TT_WBSELEM,
    LT_BAPI_WBS TYPE  STANDARD TABLE OF BAPI_BUS2054_NEW,
    LT_RETURN   TYPE  STANDARD TABLE OF BAPIRET2.

  DATA:
    LS_RETURN   TYPE  BAPIRET2,
    LS_WBSLV    TYPE  TS_WBSLV,
    LS_BAPI_WBS TYPE  BAPI_BUS2054_NEW.

  DATA:
    LF_INVALID TYPE  FLAG,
    LF_MSGTX   TYPE  ZSDSDE_MSGTX,
    LF_LV      TYPE  CHAR1,
    LF_ERROR   TYPE  FLAG.


* Initialize Output
  CLEAR ET_RETURN.

* Get Constants
  GET_GENC( ).

  IF IF_SKIP_CHK_PSPID IS INITIAL.
*   Validate Project Def
    VALIDATE_PSPID( EXPORTING IF_INPUT   = IF_PSPID
                    IMPORTING EF_INVALID = LF_INVALID ).
    IF LF_INVALID IS NOT INITIAL OR
       IF_PSPID IS INITIAL.
      LS_RETURN-TYPE    = 'E'.
      LS_RETURN-ID      = 'ZSDSPS01'.
      LS_RETURN-NUMBER  = '000'.
*     Text-e06: Project Definition does not exist:
      LS_RETURN-MESSAGE = |{ TEXT-E06 } { IF_PSPID }|.
      INSERT LS_RETURN INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

  LT_WBSELEM = IT_WBSELEM.
* Sort data for Processing
  SORT LT_WBSELEM BY STUFE ASCENDING
                     POSID ASCENDING.

* Get Existing Structure
  SELECT C~POSID,
         D~POSID AS UP,
         E~POSID AS LEFT,
         F~POSID AS RIGHT
    FROM PROJ AS A
           INNER JOIN PRHI AS B
             ON  B~PSPHI = A~PSPNR
           INNER JOIN PRPS AS C
             ON  C~PSPNR = B~POSNR
           LEFT OUTER JOIN PRPS AS D
             ON  D~PSPNR = B~UP
           LEFT OUTER JOIN PRPS AS E
             ON  E~PSPNR = B~LEFT
           LEFT OUTER JOIN PRPS AS F
             ON  F~PSPNR = B~RIGHT
   WHERE A~PSPID EQ @IF_PSPID
    INTO TABLE @DATA(LT_PRHI).
  IF SY-SUBRC NE 0.
    CLEAR LT_PRHI.
  ENDIF.

* Validate WBS and Assign BAPI data
  LOOP AT LT_WBSELEM ASSIGNING FIELD-SYMBOL(<L_WBSELEM>).
    VALIDATE_WBSELEM( EXPORTING IS_WBSELEM = <L_WBSELEM>
                                IT_WBSELEM = LT_WBSELEM
                      IMPORTING EF_INVALID = LF_INVALID
                                EF_MSGTX   = LF_MSGTX
                                ES_WBSLV   = LS_WBSLV ).
    IF LF_INVALID IS NOT INITIAL.
      LS_RETURN-TYPE    = 'E'.
      LS_RETURN-ID      = 'ZSDSPS01'.
      LS_RETURN-NUMBER  = '000'.
      LS_RETURN-MESSAGE = LF_MSGTX.
      INSERT LS_RETURN INTO TABLE ET_RETURN.
      EXIT.
    ENDIF.

*   Assign BAPI Input
    CLEAR LS_BAPI_WBS.
    LS_BAPI_WBS-WBS_ELEMENT           = <L_WBSELEM>-POSID.
    LS_BAPI_WBS-DESCRIPTION           = <L_WBSELEM>-POST1.
    LS_BAPI_WBS-WBS_BASIC_START_DATE  = <L_WBSELEM>-PSTRT.
    LS_BAPI_WBS-WBS_BASIC_FINISH_DATE = <L_WBSELEM>-PENDE.
    LS_BAPI_WBS-PROJ_TYPE             = <L_WBSELEM>-PRART.
    LS_BAPI_WBS-USER_FIELD_CHAR20_1   = <L_WBSELEM>-USR00.
    LS_BAPI_WBS-USER_FIELD_CHAR20_2   = <L_WBSELEM>-USR01.
    LS_BAPI_WBS-USER_FIELD_CHAR10_1   = <L_WBSELEM>-USR02.
    LS_BAPI_WBS-USER_FIELD_CHAR10_2   = <L_WBSELEM>-USR03.
    LS_BAPI_WBS-PROFIT_CTR            = <L_WBSELEM>-PRCTR.
    LS_BAPI_WBS-RESPSBL_CCTR          = <L_WBSELEM>-FKSTL.
    LS_BAPI_WBS-REQUEST_CCTR          = <L_WBSELEM>-AKSTL.

*   Determine Superior WBS
    TRY.
        IF LS_WBSLV-STUFE > 1.
          LF_LV = LS_WBSLV-STUFE - 1.
          DATA(LF_FNAME) = |LS_WBSLV-POSID_LV{ LF_LV }|.
          ASSIGN (LF_FNAME) TO FIELD-SYMBOL(<L_FIELD>).
          LS_BAPI_WBS-WBS_UP = <L_FIELD>.
        ENDIF.
      CATCH CX_CONVERSION_FAILED.
        LS_RETURN-TYPE    = 'E'.
        LS_RETURN-ID      = 'ZSDSPS01'.
        LS_RETURN-NUMBER  = '000'.
*       Text-e07: Cannot determine superior WBS for
        LS_RETURN-MESSAGE = |{ TEXT-E07 } { <L_WBSELEM>-POSID }|.
        INSERT LS_RETURN INTO TABLE ET_RETURN.
        EXIT.
    ENDTRY.

*   Determine Left Adjacent WBS
    LOOP AT LT_PRHI ASSIGNING FIELD-SYMBOL(<L_PRHI>)
                              WHERE UP = LS_BAPI_WBS-WBS_UP
                                AND RIGHT IS INITIAL.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      <L_PRHI>-RIGHT = <L_WBSELEM>-POSID.
      LS_BAPI_WBS-WBS_LEFT = <L_PRHI>-POSID.
    ENDIF.

*   Insert New Hier entry
    INSERT VALUE #( POSID = LS_BAPI_WBS-WBS_ELEMENT
                    UP    = LS_BAPI_WBS-WBS_UP
                    LEFT  = LS_BAPI_WBS-WBS_LEFT )
                   INTO TABLE LT_PRHI.

    INSERT LS_BAPI_WBS INTO TABLE LT_BAPI_WBS.

  ENDLOOP.
  IF ET_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.

* Check Without Project must end here...
  IF IF_SKIP_CHK_PSPID EQ 'X'.
    RETURN.
  ENDIF.

  CLEAR LF_ERROR.
* Call BAPI To create WBS
  CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

  CALL FUNCTION 'BAPI_BUS2054_CREATE_MULTI'
    EXPORTING
      I_PROJECT_DEFINITION = IF_PSPID
    TABLES
      IT_WBS_ELEMENT       = LT_BAPI_WBS
      ET_RETURN            = LT_RETURN.

  LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                    WHERE TYPE = 'E' OR TYPE = 'A' OR TYPE = 'X'.
*   keep error
    LF_ERROR = 'X'.
    INSERT <L_RETURN> INTO TABLE ET_RETURN.
  ENDLOOP.

  DO 1 TIMES.

    CHECK LF_ERROR IS INITIAL.

    CLEAR LT_RETURN.

*   check data before commit
    CALL FUNCTION 'BAPI_PS_PRECOMMIT'
      TABLES
        ET_RETURN = LT_RETURN.
    LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                      WHERE TYPE = 'E' OR TYPE = 'A' OR TYPE = 'X'.
*     keep error
      LF_ERROR = 'X'.
      INSERT <L_RETURN> INTO TABLE ET_RETURN.
    ENDLOOP.
    CHECK LF_ERROR IS INITIAL.

  ENDDO.

* Stop Here in test run mode.
  IF IF_TEST EQ 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RETURN.
  ENDIF.

  IF LF_ERROR IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    LOOP AT LT_WBSELEM ASSIGNING <L_WBSELEM>.
      IF STRLEN( <L_WBSELEM>-POST1 ) GT 40 ##NUMBER_OK.
        SAVE_LONGTEXT(
          EXPORTING
            IF_TYPE  = GC_TYPE_WBSELEM
            IF_POSID = <L_WBSELEM>-POSID
            IF_POST1 = <L_WBSELEM>-POST1 ).
      ENDIF.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDMETHOD.


METHOD GET_WBSELEM_LEVELS.

* Initialize Output
  CLEAR: ES_WBSLV,
         EF_INVALID.

* Get Constants
  GET_GENC( ).

  CASE IF_POSID(1).
*   -------------------
*   Sales Project
*   -------------------
    WHEN 'P'.
*     LV4: P-X-XXXXXXXX-XX-XX-XXX
      IF IF_POSID CP 'P-+-++++++++-++-++-+++'.
        ES_WBSLV-POSID = IF_POSID.
        ES_WBSLV-STUFE = 4.
        ES_WBSLV-POSID_LV1 = IF_POSID(12).
        ES_WBSLV-POSID_LV2 = IF_POSID(15).
        ES_WBSLV-POSID_LV3 = IF_POSID(18).
        ES_WBSLV-POSID_LV4 = IF_POSID.
*     LV3: P-X-XXXXXXXX-XX-XX
      ELSEIF IF_POSID CP 'P-+-++++++++-++-++'.
        ES_WBSLV-POSID = IF_POSID.
        ES_WBSLV-STUFE = 3.
        ES_WBSLV-POSID_LV1 = IF_POSID(12).
        ES_WBSLV-POSID_LV2 = IF_POSID(15).
        ES_WBSLV-POSID_LV3 = IF_POSID.
*     LV2: P-X-XXXXXXXX-XX
      ELSEIF IF_POSID CP 'P-+-++++++++-++'.
        ES_WBSLV-POSID = IF_POSID.
        ES_WBSLV-STUFE = 2.
        ES_WBSLV-POSID_LV1 = IF_POSID(12).
        ES_WBSLV-POSID_LV2 = IF_POSID.
*     LV1: P-X-XXXXXXXX
      ELSEIF IF_POSID CP 'P-+-++++++++'.
        ES_WBSLV-POSID = IF_POSID.
        ES_WBSLV-STUFE = 1.
        ES_WBSLV-POSID_LV1 = IF_POSID.
      ENDIF.

*   -------------------
*   RMA & AUC Project
*   -------------------
    WHEN 'C'.
*     LV3: C-X-XX-XXXX-XXXXXXXX-XX
      IF IF_POSID CP 'C-+-++-++++-++++++++-++'.
        ES_WBSLV-POSID = IF_POSID.
        ES_WBSLV-STUFE = 3.
        ES_WBSLV-POSID_LV1 = IF_POSID(11).
        ES_WBSLV-POSID_LV2 = IF_POSID(20).
        ES_WBSLV-POSID_LV3 = IF_POSID.
*     LV2: C-X-XX-XXXX-XXXXXXXX
      ELSEIF IF_POSID CP 'C-+-++-++++-++++++++'.
        ES_WBSLV-POSID = IF_POSID.
        ES_WBSLV-STUFE = 2.
        ES_WBSLV-POSID_LV1 = IF_POSID(11).
        ES_WBSLV-POSID_LV2 = IF_POSID.
*     LV1: C-X-XX-XXXX
      ELSEIF IF_POSID CP 'C-+-++-++++'.
        ES_WBSLV-POSID = IF_POSID.
        ES_WBSLV-STUFE = 1.
        ES_WBSLV-POSID_LV1 = IF_POSID.
      ENDIF.

  ENDCASE.

* Cannot Determine Level from WBS
  IF ES_WBSLV IS INITIAL.
    EF_INVALID = 'X'.
  ENDIF.

ENDMETHOD.


METHOD POST_COSTELEM_PLAN.

  DATA:
    LT_BAPI_INDEX  TYPE  STANDARD TABLE OF BAPIACPSTRU,
    LT_BAPI_COOBJ  TYPE  STANDARD TABLE OF BAPIPCPOBJ,
    LT_BAPI_PERVAL TYPE  STANDARD TABLE OF BAPIPCPVAL,
    LT_RETURN      TYPE  STANDARD TABLE OF BAPIRET2.

  DATA:
    LS_RETURN      TYPE  BAPIRET2,
    LS_BAPI_HEAD   TYPE  BAPIPLNHDR,
    LS_BAPI_COOBJ  TYPE  BAPIPCPOBJ,
    LS_BAPI_PERVAL TYPE  BAPIPCPVAL,
    LS_PLANVAL     TYPE  TS_PLANVAL.

  DATA:
    LF_INVALID     TYPE  FLAG,
    LF_MSGTX       TYPE  ZSDSDE_MSGTX,
    LF_PERIOD_FROM TYPE  CO_PERAB,
    LF_PERIOD_TO   TYPE  CO_PERBI,
    LF_INDEX       TYPE  BAPIPCPOBJ-OBJECT_INDEX,
    LF_ADJ_AMOUNT  TYPE  TS_ADJ_EXP-AMOUNT.


* Initialize Output
  CLEAR: ET_RETURN,
         EF_PERIOD_FROM,
         EF_PERIOD_TO,
         ET_ADJ_EXP.

* Initialize Variables
  CLEAR: LT_BAPI_COOBJ,
         LT_BAPI_PERVAL,
         LT_BAPI_INDEX,
         LT_RETURN.
  CLEAR: LF_INDEX,
         LF_PERIOD_FROM,
         LF_PERIOD_TO.

* Group Data by WBS/Cost Elm/Year
  LOOP AT IT_BUDGET INTO DATA(LS_BUDGET) ##INTO_OK
                    GROUP BY ( POSID = LS_BUDGET-POSID
                               KSTAR = LS_BUDGET-KSTAR
                               GJAHR = LS_BUDGET-GJAHR ) ASCENDING
                    ASSIGNING FIELD-SYMBOL(<L_GROUP>).

*   Generate New Index
    LF_INDEX = LF_INDEX + 1.

*   Assign BAPI Data
    CLEAR LS_BAPI_COOBJ.
    LS_BAPI_COOBJ-OBJECT_INDEX  = LF_INDEX.
    LS_BAPI_COOBJ-WBS_ELEMENT   = <L_GROUP>-POSID.
    INSERT LS_BAPI_COOBJ INTO TABLE LT_BAPI_COOBJ.

    CLEAR LS_BAPI_PERVAL.
    LS_BAPI_PERVAL-VALUE_INDEX = LF_INDEX.
    LS_BAPI_PERVAL-COST_ELEM   = <L_GROUP>-KSTAR.

    INSERT VALUE #( OBJECT_INDEX = LS_BAPI_COOBJ-OBJECT_INDEX
                    VALUE_INDEX  = LS_BAPI_PERVAL-VALUE_INDEX )
                 INTO TABLE LT_BAPI_INDEX.

*   Read Current Plan
    READ_COSTELEM_PLAN( EXPORTING IF_KOKRS   = IF_KOKRS
                                  IF_POSID   = <L_GROUP>-POSID
                                  IF_VERSN   = IS_KEY-VERSN
                                  IF_GJAHR   = <L_GROUP>-GJAHR
                                  IF_KSTAR   = <L_GROUP>-KSTAR
                        IMPORTING ES_PLANVAL = LS_PLANVAL ).

*   Update Current Plan amoount into structure
    MOVE-CORRESPONDING LS_PLANVAL TO LS_BAPI_PERVAL ##ENH_OK.

    LOOP AT GROUP <L_GROUP> ASSIGNING FIELD-SYMBOL(<L_BUDGET>).

      VALIDATE_BUDGET( EXPORTING IS_BUDGET  = <L_BUDGET>
                       IMPORTING EF_INVALID = LF_INVALID
                                 EF_MSGTX   = LF_MSGTX ).
*                                 ES_WBSLV   = LS_WBSLV ).
      IF LF_INVALID IS NOT INITIAL.
        LS_RETURN-TYPE    = 'E'.
        LS_RETURN-ID      = 'ZSDSPS01'.
        LS_RETURN-NUMBER  = '000'.
        LS_RETURN-MESSAGE = LF_MSGTX.
        INSERT LS_RETURN INTO TABLE ET_RETURN.
        EXIT.
      ENDIF.

      IF LF_PERIOD_FROM IS INITIAL OR
         LF_PERIOD_FROM GT <L_BUDGET>-MONAT.
        LF_PERIOD_FROM = <L_BUDGET>-MONAT.
      ENDIF.
      IF LF_PERIOD_TO IS INITIAL OR
         LF_PERIOD_TO LT <L_BUDGET>-MONAT.
        LF_PERIOD_TO = <L_BUDGET>-MONAT.
      ENDIF.

      DATA(LF_FNAME) = |LS_BAPI_PERVAL-VAR_VAL_PER{ <L_BUDGET>-MONAT }|.
      ASSIGN (LF_FNAME) TO FIELD-SYMBOL(<L_VAL>).
      IF SY-SUBRC EQ 0.
        LF_ADJ_AMOUNT = <L_VAL> - <L_BUDGET>-AMOUNT.

*       Only Project P with Expense Item Type
        IF <L_GROUP>-POSID(1) EQ 'P' AND
           <L_GROUP>-POSID+13(2) IN GT_ITMTY_EXP AND
           GT_ITMTY_EXP IS NOT INITIAL AND
           <L_VAL> NE <L_BUDGET>-AMOUNT.
*         Collect Adjust Expense Amount
          COLLECT_ADJUST_EXPENSE(
            EXPORTING
              IF_POSID   = <L_GROUP>-POSID
              IF_KSTAR   = <L_GROUP>-KSTAR
              IF_GJAHR   = <L_GROUP>-GJAHR
              IF_MONAT   = <L_BUDGET>-MONAT
              IF_AMOUNT  = LF_ADJ_AMOUNT
            CHANGING
              CT_ADJ_EXP = ET_ADJ_EXP ).
        ENDIF.
        <L_VAL> = <L_BUDGET>-AMOUNT.
      ENDIF.

    ENDLOOP.
    IF ET_RETURN IS NOT INITIAL.
      EXIT.
    ENDIF.

    INSERT LS_BAPI_PERVAL INTO TABLE LT_BAPI_PERVAL.

  ENDLOOP.
  IF ET_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.

* Assign Header data
  CLEAR LS_BAPI_HEAD.
  LS_BAPI_HEAD-CO_AREA        = IF_KOKRS.
  LS_BAPI_HEAD-FISC_YEAR      = IS_KEY-GJAHR.
  LS_BAPI_HEAD-PERIOD_FROM    = LF_PERIOD_FROM.
  LS_BAPI_HEAD-PERIOD_TO      = LF_PERIOD_TO.
  LS_BAPI_HEAD-VERSION        = IS_KEY-VERSN.
  LS_BAPI_HEAD-PLAN_CURRTYPE  = GC_CURRTYP.

  CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
    EXPORTING
      HEADERINFO     = LS_BAPI_HEAD
      DELTA          = SPACE
    TABLES
      INDEXSTRUCTURE = LT_BAPI_INDEX
      COOBJECT       = LT_BAPI_COOBJ
      PERVALUE       = LT_BAPI_PERVAL
      RETURN         = LT_RETURN.

  IF IF_TEST EQ 'X' OR
     LT_RETURN IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ENDIF.

* Collect Result
  INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.

  IF EF_PERIOD_FROM IS SUPPLIED.
    EF_PERIOD_FROM = LF_PERIOD_FROM.
  ENDIF.

  IF EF_PERIOD_TO IS SUPPLIED.
    EF_PERIOD_TO = LF_PERIOD_TO.
  ENDIF.

ENDMETHOD.


METHOD READ_COSTELEM_PLAN.

  DATA:
    LT_BAPI_INDEX  TYPE  STANDARD TABLE OF BAPIACPSTRU,
    LT_BAPI_COOBJ  TYPE  STANDARD TABLE OF BAPIPCPOBJ,
    LT_BAPI_PERVAL TYPE  STANDARD TABLE OF BAPIPCPVAL,
    LT_RETURN      TYPE  STANDARD TABLE OF BAPIRET2.

  DATA:
    LS_BAPI_HEAD  TYPE  BAPIPLNHDR.

  DATA:
    LF_INDEX  TYPE  OBJ_INDX.


* Initialize Output
  CLEAR ES_PLANVAL.

* Assign header
  CLEAR LS_BAPI_HEAD.
  LS_BAPI_HEAD-CO_AREA       = IF_KOKRS.
  LS_BAPI_HEAD-FISC_YEAR     = IF_GJAHR.
  LS_BAPI_HEAD-PERIOD_FROM   = '001'.
  LS_BAPI_HEAD-PERIOD_TO     = '012'.
  LS_BAPI_HEAD-VERSION       = IF_VERSN.
  LS_BAPI_HEAD-PLAN_CURRTYPE = GC_CURRTYP.

  LF_INDEX = LF_INDEX + 1.

* Assign CO Object
  INSERT VALUE #( OBJECT_INDEX = LF_INDEX
                  WBS_ELEMENT  = IF_POSID )
         INTO TABLE LT_BAPI_COOBJ.

* Assign Period Value
  INSERT VALUE #( VALUE_INDEX  = LF_INDEX
                  COST_ELEM = IF_KSTAR )
         INTO TABLE LT_BAPI_PERVAL.

* Assign Index
  INSERT VALUE #( OBJECT_INDEX = LF_INDEX
                  VALUE_INDEX  = LF_INDEX )
         INTO TABLE LT_BAPI_INDEX.

* Read Cost element Plan
  CALL FUNCTION 'BAPI_COSTACTPLN_READPRIMCOST'
    EXPORTING
      HEADERINFO     = LS_BAPI_HEAD
    TABLES
      INDEXSTRUCTURE = LT_BAPI_INDEX
      COOBJECT       = LT_BAPI_COOBJ
      PERVALUE       = LT_BAPI_PERVAL
      RETURN         = LT_RETURN.
  IF LT_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.

* Read Result
  READ TABLE LT_BAPI_PERVAL ASSIGNING FIELD-SYMBOL(<L_PERVAL>)
                            INDEX 1.
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING <L_PERVAL> TO ES_PLANVAL ##ENH_OK.
    ES_PLANVAL-POSID = IF_POSID.
    ES_PLANVAL-VERSN = IF_VERSN.
    ES_PLANVAL-GJAHR = IF_GJAHR.
    ES_PLANVAL-KSTAR = IF_KSTAR.
  ENDIF.

ENDMETHOD.


METHOD RELEASE_PROJECT_WBS.

  CONSTANTS:
    LC_REL     TYPE  BAPI_SYSTEM_STATUS-SYSTEM_STATUS VALUE 'REL'.

  DATA:
    LT_STATUS     TYPE  STANDARD TABLE OF BAPI_SYSTEM_STATUS,
    LT_WBS_STATUS TYPE  STANDARD TABLE OF BAPI_WBS_SYSTEM_STATUS,
    LT_WBS_REL    TYPE  STANDARD TABLE OF BAPI_WBS_MNT_SYSTEM_STATUS,
    LT_RETURN     TYPE  STANDARD TABLE OF BAPIRET2.

  DATA:
    LS_RETURN  TYPE  BAPIRETURN1.

  DATA:
    LF_ERROR  TYPE  FLAG.


* Initialize Output
  CLEAR: ET_RETURN.

* ----------------------
* Read Project Status
* ----------------------
  CALL FUNCTION 'BAPI_BUS2001_GET_STATUS'
    EXPORTING
      PROJECT_DEFINITION = IF_PSPID
    IMPORTING
      RETURN             = LS_RETURN
    TABLES
      E_SYSTEM_STATUS    = LT_STATUS.
  IF LS_RETURN IS NOT INITIAL.
*   Error reading Project status
*   Text-e18: Error while reading Project/WBS Status.
    INSERT VALUE #( TYPE      = 'E'
                    ID        = 'ZSDSPS01'
                    NUMBER    = '000'
                    MESSAGE   = TEXT-E18
                    PARAMETER = IF_PSPID )
           INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

* ----------------------
* Release Project
* ----------------------
  IF NOT LINE_EXISTS( LT_STATUS[ SYSTEM_STATUS = LC_REL ] ).

    DO 1 TIMES.
      CLEAR LF_ERROR.

      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

      CALL FUNCTION 'BAPI_BUS2001_SET_STATUS'
        EXPORTING
          PROJECT_DEFINITION = IF_PSPID
          SET_SYSTEM_STATUS  = LC_REL
        IMPORTING
          RETURN             = LS_RETURN.
      IF LS_RETURN IS NOT INITIAL.
        LF_ERROR = 'X'.
*       Error
        INSERT INITIAL LINE INTO TABLE ET_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).
        MOVE-CORRESPONDING LS_RETURN TO <L_RETURN>.
        <L_RETURN>-PARAMETER = IF_PSPID.
        EXIT.
      ENDIF.

*     check data before commit
      CLEAR LT_RETURN.
      CALL FUNCTION 'BAPI_PS_PRECOMMIT'
        TABLES
          ET_RETURN = LT_RETURN.
      LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                        WHERE TYPE = 'E' OR TYPE = 'A' OR TYPE = 'X'.
*       keep error
        LF_ERROR = 'X'.
        INSERT <L_RETURN> INTO TABLE ET_RETURN.
      ENDLOOP.

    ENDDO.
    IF LF_ERROR IS INITIAL AND
       IF_TEST EQ SPACE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDIF.

* ----------------------
* Read WBS Status
* ----------------------
  CALL FUNCTION 'BAPI_BUS2054_GET_STATUS'
    IMPORTING
      RETURN          = LS_RETURN
    TABLES
      I_WBS_ELEMENTS  = IT_WBS
      E_SYSTEM_STATUS = LT_WBS_STATUS.
  IF LS_RETURN IS NOT INITIAL.
*   Error reading WBS status
*   Text-e18: Error while reading Project/WBS Status.
    INSERT VALUE #( TYPE      = 'E'
                    ID        = 'ZSDSPS01'
                    NUMBER    = '000'
                    MESSAGE   = TEXT-E18
                    PARAMETER = IF_PSPID )
           INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.

  LOOP AT IT_WBS ASSIGNING FIELD-SYMBOL(<L_WBS>).

    IF NOT LINE_EXISTS( LT_WBS_STATUS[ WBS_ELEMENT   = <L_WBS>-WBS_ELEMENT
                                   SYSTEM_STATUS = LC_REL ] ).

      INSERT VALUE #( WBS_ELEMENT = <L_WBS>-WBS_ELEMENT
*                      UNDO_SYSTEM_STATUS
                      SET_SYSTEM_STATUS  = LC_REL )
                     INTO TABLE LT_WBS_REL.

    ENDIF.
  ENDLOOP.

* ----------------------
* Release WBS Elements
* ----------------------
  IF LT_WBS_REL IS NOT INITIAL.

    DO 1 TIMES.
      CLEAR LF_ERROR.

      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

      CALL FUNCTION 'BAPI_BUS2054_SET_STATUS'
        IMPORTING
          RETURN              = LS_RETURN
        TABLES
          I_WBS_SYSTEM_STATUS = LT_WBS_REL.
      IF LS_RETURN IS NOT INITIAL.
        LF_ERROR = 'X'.
*       Error
        INSERT INITIAL LINE INTO TABLE ET_RETURN ASSIGNING <L_RETURN>.
        MOVE-CORRESPONDING LS_RETURN TO <L_RETURN>.
        <L_RETURN>-PARAMETER = IF_PSPID.
        EXIT.
      ENDIF.

*     check data before commit
      CLEAR LT_RETURN.
      CALL FUNCTION 'BAPI_PS_PRECOMMIT'
        TABLES
          ET_RETURN = LT_RETURN.
      LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                        WHERE TYPE = 'E' OR TYPE = 'A' OR TYPE = 'X'.
*       keep error
        LF_ERROR = 'X'.
        INSERT <L_RETURN> INTO TABLE ET_RETURN.
      ENDLOOP.

    ENDDO.
    IF LF_ERROR IS INITIAL AND
       IF_TEST EQ SPACE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD REVISE_PROJECT_WBS.

  CONSTANTS:
    LC_REVS     TYPE  BAPI_USER_STATUS_TEXT VALUE 'REVS'.

  DATA:
    LT_STATUS     TYPE  STANDARD TABLE OF BAPI_USER_STATUS,
    LT_WBS_STATUS TYPE  STANDARD TABLE OF BAPI_WBS_USER_STATUS,
    LT_WBS_REVS   TYPE  STANDARD TABLE OF BAPI_WBS_MNT_USER_STATUS,
    LT_RETURN     TYPE  STANDARD TABLE OF BAPIRET2.

  DATA:
    LS_RETURN TYPE  BAPIRETURN1,
    LS_WBSLV  TYPE  TS_WBSLV.

  DATA:
    LF_ERROR    TYPE  FLAG,
    LF_UPD_PROJ TYPE  FLAG.


* Initialize Output
  CLEAR: ET_RETURN.

* Only when WBS Revise Exist
  IF IT_WBS IS INITIAL.
    RETURN.
  ENDIF.

* For Project C, only set project status when LV2 Revised
  CLEAR LF_UPD_PROJ.
  IF IF_PSPID(1) EQ 'C'.
    LOOP AT IT_WBS ASSIGNING FIELD-SYMBOL(<L_WBS>).
*     Check WBS Lv2?
      GET_WBSELEM_LEVELS(
        EXPORTING
          IF_POSID = <L_WBS>-WBS_ELEMENT
        IMPORTING
          ES_WBSLV = LS_WBSLV ).
      IF LS_WBSLV-STUFE EQ 2.
        LF_UPD_PROJ = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
    LF_UPD_PROJ = 'X'.
  ENDIF.

  IF LF_UPD_PROJ EQ 'X'.
*   ----------------------
*   Read Project Status
*   ----------------------
    CALL FUNCTION 'BAPI_BUS2001_GET_STATUS'
      EXPORTING
        PROJECT_DEFINITION = IF_PSPID
      IMPORTING
        RETURN             = LS_RETURN
      TABLES
        E_USER_STATUS      = LT_STATUS.
    IF LS_RETURN IS NOT INITIAL.
*     Error reading Project status
*     Text-e18: Error while reading Project/WBS Status.
      INSERT VALUE #( TYPE      = 'E'
                      ID        = 'ZSDSPS01'
                      NUMBER    = '000'
                      MESSAGE   = TEXT-E18
                      PARAMETER = IF_PSPID )
             INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.

*   ----------------------
*   Revise Project
*   ----------------------
    IF NOT LINE_EXISTS( LT_STATUS[ USER_STATUS = LC_REVS ] ).

      DO 1 TIMES.
        CLEAR LF_ERROR.

        CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

        CALL FUNCTION 'BAPI_BUS2001_SET_STATUS'
          EXPORTING
            PROJECT_DEFINITION = IF_PSPID
            SET_USER_STATUS    = LC_REVS
          IMPORTING
            RETURN             = LS_RETURN.
        IF LS_RETURN IS NOT INITIAL.
          LF_ERROR = 'X'.
*         Error
          INSERT INITIAL LINE INTO TABLE ET_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).
          MOVE-CORRESPONDING LS_RETURN TO <L_RETURN>.
          <L_RETURN>-PARAMETER = IF_PSPID.
          EXIT.
        ENDIF.

*       check data before commit
        CLEAR LT_RETURN.
        CALL FUNCTION 'BAPI_PS_PRECOMMIT'
          TABLES
            ET_RETURN = LT_RETURN.
        LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                          WHERE TYPE = 'E' OR TYPE = 'A' OR TYPE = 'X'.
*         keep error
          LF_ERROR = 'X'.
          INSERT <L_RETURN> INTO TABLE ET_RETURN.
        ENDLOOP.

      ENDDO.
      IF LF_ERROR IS INITIAL AND
         IF_TEST EQ SPACE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

    ENDIF.
  ENDIF.

* ----------------------
* Read WBS Status
* ----------------------
  IF IT_WBS IS NOT INITIAL.
    CALL FUNCTION 'BAPI_BUS2054_GET_STATUS'
      IMPORTING
        RETURN         = LS_RETURN
      TABLES
        I_WBS_ELEMENTS = IT_WBS
        E_USER_STATUS  = LT_WBS_STATUS.
    IF LS_RETURN IS NOT INITIAL.
*     Error reading WBS status
*     Text-e18: Error while reading Project/WBS Status.
      INSERT VALUE #( TYPE      = 'E'
                      ID        = 'ZSDSPS01'
                      NUMBER    = '000'
                      MESSAGE   = TEXT-E18
                      PARAMETER = IF_PSPID )
             INTO TABLE ET_RETURN.
      RETURN.
    ENDIF.
  ENDIF.

  LOOP AT IT_WBS ASSIGNING <L_WBS>.

    IF NOT LINE_EXISTS( LT_WBS_STATUS[ WBS_ELEMENT = <L_WBS>-WBS_ELEMENT
                                       USER_STATUS = LC_REVS ] ).

      INSERT VALUE #( WBS_ELEMENT = <L_WBS>-WBS_ELEMENT
*                      UNDO_USER_STATUS
                      SET_USER_STATUS = LC_REVS )
                     INTO TABLE LT_WBS_REVS.

    ENDIF.
  ENDLOOP.

* ----------------------
* Revise WBS Elements
* ----------------------
  IF LT_WBS_REVS IS NOT INITIAL.

    DO 1 TIMES.
      CLEAR LF_ERROR.

      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

      CALL FUNCTION 'BAPI_BUS2054_SET_STATUS'
        IMPORTING
          RETURN            = LS_RETURN
        TABLES
          I_WBS_USER_STATUS = LT_WBS_REVS.
      IF LS_RETURN IS NOT INITIAL.
        LF_ERROR = 'X'.
*       Error
        INSERT INITIAL LINE INTO TABLE ET_RETURN ASSIGNING <L_RETURN>.
        MOVE-CORRESPONDING LS_RETURN TO <L_RETURN>.
        <L_RETURN>-PARAMETER = IF_PSPID.
        EXIT.
      ENDIF.

*     check data before commit
      CLEAR LT_RETURN.
      CALL FUNCTION 'BAPI_PS_PRECOMMIT'
        TABLES
          ET_RETURN = LT_RETURN.
      LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                        WHERE TYPE = 'E' OR TYPE = 'A' OR TYPE = 'X'.
*       keep error
        LF_ERROR = 'X'.
        INSERT <L_RETURN> INTO TABLE ET_RETURN.
      ENDLOOP.

    ENDDO.
    IF LF_ERROR IS INITIAL AND
       IF_TEST EQ SPACE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD TOTOAL_UP_COST_PLAN.

  CONSTANTS:
    LC_CJ40    TYPE SY-TCODE VALUE 'CJ40'.

  DATA:
    LT_BDCDATA TYPE STANDARD TABLE OF BDCDATA,
    LT_MSG     TYPE TAB_BDCMSGCOLL.

  DATA:
    LS_OPT  TYPE  CTU_PARAMS.

  DATA:
*    LF_NUMC    TYPE  N LENGTH 1,                            "-420000344
    LF_PTIME   TYPE  CHAR10,
    LF_SUM_AMT TYPE  BP_WERT1,
    LF_TEXT    TYPE  TEXT50,
    LF_ERROR   TYPE  FLAG.


* Initialize Output
  CLEAR: ET_RETURN.

* Read Project Profile
  SELECT SINGLE PROFL,
                BPROF,                                      "+420000344
                PPROF                                       "+420000344
    FROM PROJ
   WHERE PSPID EQ @IS_KEY-PSPID
*    INTO @DATA(LF_PROFL).                                   "-420000344
    INTO @DATA(LS_PROJ).                                    "+420000344
  IF SY-SUBRC NE 0.
*   Critical error
*    CLEAR LF_PROFL.                                         "-420000344
    CLEAR LS_PROJ.                                          "+420000344
  ENDIF.

  CLEAR: LT_BDCDATA.

* Set Controlling Area
  SET PARAMETER ID 'CAC' FIELD IF_KOKRS.

* Enter Project and press enter
  MC_BDC_DYNPRO 'SAPMKBUD' '0200'.
  MC_BDC_FIELD  'PROJ-PSPID' IS_KEY-PSPID.
  MC_BDC_FIELD  'BPDY-VERSN' IS_KEY-VERSN.
  MC_BDC_FIELD  'BDC_OKCODE' '/00'.

* Assign PTIME
  IF GT_PROFL_NOOVW IS NOT INITIAL AND
*     NOT LF_PROFL IN GT_PROFL_NOOVW.                        "-420000344
     NOT LS_PROJ-PROFL IN GT_PROFL_NOOVW.                   "+420000344
    LF_PTIME = IS_KEY-GJAHR.
  ELSE.
*<-- Start of Insertion 420000344 29.01.2025 (Get Year List)
    GET_PTIME( EXPORTING IF_BPROF = LS_PROJ-PPROF
                         IF_GJAHR = IS_KEY-GJAHR
                         IF_WRTTP = GC_PLAN01
               IMPORTING EF_PTIME = LF_PTIME ).
*--> End of Insertion 420000344 29.01.2025
*    LF_NUMC = IS_KEY-GJAHR - SY-DATUM(4).                   "-420000344
*    LF_PTIME = LF_NUMC.                                     "-420000344
  ENDIF.

* Enter Fiscal Year
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'DROPT-PTIME' LF_PTIME.
  MC_BDC_FIELD  'BDC_OKCODE' '=DROT'.

* Filtering Down to level 2 only for C Project
  IF IS_KEY-PSPID(1) EQ 'C'.

*   Sum plan amount from WBS lv2
    GET_SUM_PLAN_LV2(
      EXPORTING
        IS_KEY     = IS_KEY
      IMPORTING
        EF_SUM_AMT = LF_SUM_AMT ).

*   Get Currency Key
    SELECT SINGLE WAERS
      FROM TKA01
     WHERE KOKRS EQ @IF_KOKRS
      INTO @DATA(LF_WAERS).
    IF SY-SUBRC NE 0.
      CLEAR LF_WAERS.
    ENDIF.

    WRITE LF_SUM_AMT TO LF_TEXT NO-GROUPING LEFT-JUSTIFIED
                        CURRENCY LF_WAERS.
    CONDENSE LF_TEXT NO-GAPS.

*   Fill Summarized amount in level 1 then press enter
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BPDY-WERT1(01)' LF_TEXT.
    MC_BDC_FIELD  'BDC_OKCODE' '/00'.

  ELSE.
*   Select all
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=MRKA'.

*   Delete (To clear existing values)
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=DELL'.

*   Select all
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=MRKA'.

*   Total up
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=SYNC'.

*   Mark Annual and Total with Year then OK
    MC_BDC_DYNPRO 'SAPLKBPP' '0705'.
    MC_BDC_FIELD  'BPDY-TI_JAHR_1' 'X'.
    MC_BDC_FIELD  'BPDY-JAHR_VON' IS_KEY-GJAHR.
    MC_BDC_FIELD  'BPDY-JAHR_BIS' IS_KEY-GJAHR.
    MC_BDC_FIELD  'BPDY-TI_GES_1' 'X'.
    MC_BDC_FIELD  'BDC_OKCODE' '=ENTE'.

  ENDIF.

* Process Copy Overview
  IF GT_PROFL_NOOVW IS NOT INITIAL AND
*     NOT LF_PROFL IN GT_PROFL_NOOVW.                        "-420000344
     NOT LS_PROJ-PROFL IN GT_PROFL_NOOVW.                   "+420000344
*   Select Overall
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'DROPT-PTIME' '0'.
    MC_BDC_FIELD  'BDC_OKCODE' '=DROT'.

*   Undo Focus (Level filtering)
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=ALLE'.

*   Select all
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=MRKA'.

*   Copy View
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=KOPS'.

*   Select Cumulative
    MC_BDC_DYNPRO 'SAPLSPO5' '0130'.
    MC_BDC_FIELD  'SPOPLI-SELFLAG(01)' SPACE.
    MC_BDC_FIELD  'SPOPLI-SELFLAG(03)' 'X'.
    MC_BDC_FIELD  'BDC_OKCODE' '=OK'.

*   Enter Overwrite 100%
    MC_BDC_DYNPRO 'SAPLKBPP' '0706'.
    MC_BDC_FIELD  'BPDY-COPY_PERC' '100.00'.
    MC_BDC_FIELD  'BPDY-COPY_REPL' 'X'.
    MC_BDC_FIELD  'BDC_OKCODE' '=ENTE'.

  ENDIF.

  IF IF_TEST EQ 'X'.
*   Back to exit
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=BACK'.
  ELSE.
*   Save
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=SAVE'.
  ENDIF.

  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.

* Call CJ40
  CALL TRANSACTION LC_CJ40 WITHOUT AUTHORITY-CHECK       "#EC CI_CALLTA
                           USING LT_BDCDATA
                           OPTIONS FROM LS_OPT
                           MESSAGES INTO LT_MSG.
  IF SY-SUBRC NE 0.
    LF_ERROR = 'X'.
  ENDIF.

* Find Error Message
  LOOP AT LT_MSG ASSIGNING FIELD-SYMBOL(<L_MSG>)
                 WHERE ( MSGTYP EQ 'E' OR
                         MSGTYP EQ 'A' OR
                         MSGTYP EQ 'X' ).
    MESSAGE ID <L_MSG>-MSGID
            TYPE <L_MSG>-MSGTYP
            NUMBER <L_MSG>-MSGNR
            WITH <L_MSG>-MSGV1 <L_MSG>-MSGV2
                 <L_MSG>-MSGV3 <L_MSG>-MSGV4
            INTO DATA(LF_MSGTX).
*   Assign Result
    INSERT VALUE #( TYPE = 'E'
                    ID   = <L_MSG>-MSGID
                    NUMBER = <L_MSG>-MSGNR
                    MESSAGE = LF_MSGTX
                    MESSAGE_V1 = <L_MSG>-MSGV1
                    MESSAGE_V2 = <L_MSG>-MSGV2
                    MESSAGE_V3 = <L_MSG>-MSGV3
                    MESSAGE_V4 = <L_MSG>-MSGV4
                    PARAMETER  = IS_KEY-PSPID
                    ROW   = IS_KEY-GJAHR
                    FIELD = IS_KEY-VERSN )
                  INTO TABLE ET_RETURN.

  ENDLOOP.

* Default Error message
  IF LF_ERROR EQ 'X' AND
     ET_RETURN IS INITIAL.
*   Text-e16: Error during processing tcode
    MESSAGE E000(ZSDSPS01) WITH TEXT-E16 LC_CJ40 SPACE SPACE
                           INTO LF_MSGTX.
*   Assign Result
    INSERT VALUE #( TYPE = 'E'
                    ID   = 'ZSDSPS01'
                    NUMBER = '000'
                    MESSAGE = LF_MSGTX
                    MESSAGE_V1 = TEXT-E16
                    MESSAGE_V2 = <L_MSG>-MSGV2
                    PARAMETER  = LC_CJ40
                    ROW   = IS_KEY-GJAHR
                    FIELD = IS_KEY-VERSN )
                  INTO TABLE ET_RETURN.
  ENDIF.

ENDMETHOD.


METHOD TOTOAL_UP_REVENUE_PLAN.

  CONSTANTS:
    LC_CJ42    TYPE SY-TCODE VALUE 'CJ42'.

  DATA:
    LT_BDCDATA TYPE STANDARD TABLE OF BDCDATA,
    LT_MSG     TYPE TAB_BDCMSGCOLL.

  DATA:
    LS_OPT  TYPE  CTU_PARAMS.

  DATA:
    LF_ERROR  TYPE  FLAG.


* Initialize Output
  CLEAR: ET_RETURN.

  CLEAR: LT_BDCDATA.

* Set Controlling Area
  SET PARAMETER ID 'CAC' FIELD IF_KOKRS.

* Enter Project and press enter
  MC_BDC_DYNPRO 'SAPMKBUD' '0200'.
  MC_BDC_FIELD  'PROJ-PSPID' IS_KEY-PSPID.
  MC_BDC_FIELD  'BPDY-VERSN' IS_KEY-VERSN.
  MC_BDC_FIELD  'BDC_OKCODE' '/00'.

* Enter Fiscal Year
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'DROPT-PTIME' IS_KEY-GJAHR.
  MC_BDC_FIELD  'BDC_OKCODE' '=DROT'.

* Select all
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'BDC_OKCODE' '=MRKA'.

* Delete (To clear existing values)
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'BDC_OKCODE' '=DELL'.

* Select all
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'BDC_OKCODE' '=MRKA'.

* Total up
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'BDC_OKCODE' '=SYNC'.

* Mark Annual and Total with Year then OK
  MC_BDC_DYNPRO 'SAPLKBPP' '0705'.
  MC_BDC_FIELD  'BPDY-TI_JAHR_1' 'X'.
  MC_BDC_FIELD  'BPDY-JAHR_VON' IS_KEY-GJAHR.
  MC_BDC_FIELD  'BPDY-JAHR_BIS' IS_KEY-GJAHR.
  MC_BDC_FIELD  'BPDY-TI_GES_1' 'X'.
  MC_BDC_FIELD  'BDC_OKCODE' '=ENTE'.

* Select Overall
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'DROPT-PTIME' '0'.
  MC_BDC_FIELD  'BDC_OKCODE' '=DROT'.

* Select all
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'BDC_OKCODE' '=MRKA'.

* Copy View
  MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
  MC_BDC_FIELD  'BDC_OKCODE' '=KOPS'.

* Select Cumulative
  MC_BDC_DYNPRO 'SAPLSPO5' '0130'.
  MC_BDC_FIELD  'SPOPLI-SELFLAG(01)' SPACE.
  MC_BDC_FIELD  'SPOPLI-SELFLAG(03)' 'X'.
  MC_BDC_FIELD  'BDC_OKCODE' '=OK'.

* Enter Overwrite 100%
  MC_BDC_DYNPRO 'SAPLKBPP' '0706'.
  MC_BDC_FIELD  'BPDY-COPY_PERC' '100.00'.
  MC_BDC_FIELD  'BPDY-COPY_REPL' 'X'.
  MC_BDC_FIELD  'BDC_OKCODE' '=ENTE'.

  IF IF_TEST EQ 'X'.
*   Back to exit
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=BACK'.
  ELSE.
*   Save
    MC_BDC_DYNPRO 'SAPLKBPP' '0320'.
    MC_BDC_FIELD  'BDC_OKCODE' '=POST'.
  ENDIF.

  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.

* Call CJ40
  CALL TRANSACTION LC_CJ42 WITHOUT AUTHORITY-CHECK       "#EC CI_CALLTA
                           USING LT_BDCDATA
                           OPTIONS FROM LS_OPT
                           MESSAGES INTO LT_MSG.
  IF SY-SUBRC NE 0.
    LF_ERROR = 'X'.
  ENDIF.

* Find Error Message
  LOOP AT LT_MSG ASSIGNING FIELD-SYMBOL(<L_MSG>)
                 WHERE ( MSGTYP EQ 'E' OR
                         MSGTYP EQ 'A' OR
                         MSGTYP EQ 'X' ).
    MESSAGE ID <L_MSG>-MSGID
            TYPE <L_MSG>-MSGTYP
            NUMBER <L_MSG>-MSGNR
            WITH <L_MSG>-MSGV1 <L_MSG>-MSGV2
                 <L_MSG>-MSGV3 <L_MSG>-MSGV4
            INTO DATA(LF_MSGTX).
*   Assign Result
    INSERT VALUE #( TYPE = 'E'
                    ID   = <L_MSG>-MSGID
                    NUMBER = <L_MSG>-MSGNR
                    MESSAGE = LF_MSGTX
                    MESSAGE_V1 = <L_MSG>-MSGV1
                    MESSAGE_V2 = <L_MSG>-MSGV2
                    MESSAGE_V3 = <L_MSG>-MSGV3
                    MESSAGE_V4 = <L_MSG>-MSGV4
                    PARAMETER  = IS_KEY-PSPID
                    ROW   = IS_KEY-GJAHR
                    FIELD = IS_KEY-VERSN )
                  INTO TABLE ET_RETURN.

  ENDLOOP.

* Default Error message
  IF LF_ERROR EQ 'X' AND
     ET_RETURN IS INITIAL.
*   Text-e16: Error during processing tcode
    MESSAGE E000(ZSDSPS01) WITH TEXT-E16 LC_CJ42 SPACE SPACE
                           INTO LF_MSGTX.
*   Assign Result
    INSERT VALUE #( TYPE = 'E'
                    ID   = 'ZSDSPS01'
                    NUMBER = '000'
                    MESSAGE = LF_MSGTX
                    MESSAGE_V1 = TEXT-E16
                    MESSAGE_V2 = <L_MSG>-MSGV2
                    PARAMETER  = LC_CJ42
                    ROW   = IS_KEY-GJAHR
                    FIELD = IS_KEY-VERSN )
                  INTO TABLE ET_RETURN.
  ENDIF.

ENDMETHOD.


METHOD UPDATE_PLAN_COST_TABLE.

  CONSTANTS:
    LC_MEMID  TYPE  CHAR20 VALUE 'ZSDS_EXP_LOG'.

  DATA:
    LT_MSG  TYPE  IF_FINS_PLANNING_API_WRITE_TY=>YT_MSG.

  DATA:
    LF_EXPORT  TYPE  FLAG.


* Initialize Output
  CLEAR ET_RETURN.

* Set Export Result into memory
* The usage is in enhancement ZSDS_PS_EXPORT_LOG in program R_FINS_PLAN_TRANS_CO_ERP_2_S4H
  LF_EXPORT = 'X'.
  EXPORT EXPORT_FLAG FROM LF_EXPORT TO MEMORY ID LC_MEMID.

  SUBMIT R_FINS_PLAN_TRANS_CO_ERP_2_S4H WITH P_PLAREA = 'WBS: Costs'  ##NO_TEXT
                                        WITH P_VERSN = IS_KEY-VERSN
                                        WITH P_KOKRS = IF_KOKRS
                                        WITH P_GJAHR = IS_KEY-GJAHR
                                        WITH P_PERAB = IF_PERIOD_FROM
                                        WITH P_PERBI = IF_PERIOD_TO
                                        WITH S_PROJ EQ IS_KEY-PSPID
                                        WITH P_PLNCT = GC_CURRTYP
                                        WITH P_RLDNR = GC_LEDGER
                                        WITH P_CATEG = 'PROJ_ECP01'   ##NO_TEXT
                                        WITH PTESTRUN = IF_TEST
                                        AND RETURN.      "#EC CI_SUBMIT
  IMPORT MESSAGE TO LT_MSG FROM MEMORY ID LC_MEMID.
  IF SY-SUBRC NE 0.
    FREE MEMORY ID LC_MEMID.
*   Error cannot get result
*   Text-e17: Cannot determine result of Updating plan cost table.
    INSERT VALUE #( TYPE = 'W'
                    ID   = 'ZSDSPS01'
                    NUMBER = '000'
                    MESSAGE = TEXT-E17
                    PARAMETER = IS_KEY-PSPID
                    ROW       = IS_KEY-GJAHR
                    FIELD     = IS_KEY-VERSN )
            INTO TABLE ET_RETURN.
    RETURN.
  ENDIF.
  FREE MEMORY ID LC_MEMID.

* Assign Error Result
  LOOP AT LT_MSG ASSIGNING FIELD-SYMBOL(<L_MSG>)
                 WHERE ( TYPE EQ 'E' OR
                         TYPE EQ 'X' OR
                         TYPE EQ 'A' ).
    INSERT CORRESPONDING #( <L_MSG> )
           INTO TABLE ET_RETURN.
  ENDLOOP.

ENDMETHOD.


METHOD VALIDATE_BUDGET.

* Initialize Output
  CLEAR: EF_INVALID,
         EF_MSGTX,
         ES_WBSLV.

* Get Constants
  GET_GENC( ).

* WBS Element is required
  IF IS_BUDGET-POSID IS INITIAL.
    EF_INVALID = 'X'.
*   Text-e03: WBS Element is required.
    EF_MSGTX   = TEXT-E03.
    RETURN.
  ENDIF.

* Cost Element is required
  IF IS_BUDGET-KSTAR IS INITIAL.
    EF_INVALID = 'X'.
*   Text-e08: Cost Element is required.
    EF_MSGTX   = TEXT-E08.
    RETURN.
  ENDIF.

* Fiscal Year is required
  IF IS_BUDGET-GJAHR IS INITIAL.
    EF_INVALID = 'X'.
*   Text-e09: Fiscal Year is required.
    EF_MSGTX   = TEXT-E09.
    RETURN.
  ENDIF.

* Period is required
  IF IS_BUDGET-MONAT IS INITIAL.
    EF_INVALID = 'X'.
*   Text-e10: Period is required.
    EF_MSGTX   = TEXT-E10.
    RETURN.
  ENDIF.

* Revenue Must in Minus sign
  IF IS_BUDGET-KSTAR(2) EQ '40' AND
     IS_BUDGET-AMOUNT GT 0.
    EF_INVALID = 'X'.
*   Text-e11: Plan revenue must be in negative amount.
    EF_MSGTX   = TEXT-E11.
    RETURN.
  ENDIF.

* WBS Element Levels
  IF ES_WBSLV IS SUPPLIED.
    GET_WBSELEM_LEVELS( EXPORTING IF_POSID = IS_BUDGET-POSID
                        IMPORTING ES_WBSLV = ES_WBSLV ).
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_PROJECTDEF.

* Initialize Output
  CLEAR: EF_INVALID,
         EF_MSGTX.

* Get Constants
  GET_GENC( ).

* Project Def is required
  IF IS_PROJECTDEF-PSPID IS INITIAL.
    EF_INVALID = 'X'.
*   Text-e01: Project Definition is required.
    EF_MSGTX   = TEXT-E01.
    RETURN.
  ENDIF.

* Project Profile is required
  IF ( IS_PROJECTDEFX IS NOT SUPPLIED OR
       IS_PROJECTDEFX-PROFL EQ 'X' ) AND
     IS_PROJECTDEF-PROFL IS INITIAL.
    EF_INVALID = 'X'.
*   Text-e02: Project profile is required.
    EF_MSGTX   = TEXT-E02.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_PSPID.

  DATA:
    LF_PSPID  TYPE  PROJ-PSPID.


* Initialize Output
  CLEAR: EF_OUTPUT,
         EF_INVALID.

* Get Constants
  GET_GENC( ).

* Only Data Exist
  IF IF_INPUT IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  IF STRLEN( IF_INPUT ) GT 24 ##NUMBER_OK.
    EF_INVALID = 'X'.
    RETURN.
  ENDIF.

* Convert to Input format
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
    EXPORTING
      INPUT  = IF_INPUT
    IMPORTING
      OUTPUT = LF_PSPID.

* Read existing
  SELECT SINGLE PSPID
    FROM PROJ
   WHERE PSPID EQ @LF_PSPID
    INTO @DATA(LF_PSPID_DB).
  IF SY-SUBRC NE 0.
    CLEAR LF_PSPID_DB.
  ENDIF.

* Change with Existing --> Ok
  IF IF_CREATE IS INITIAL AND
     LF_PSPID_DB IS NOT INITIAL.
    EF_OUTPUT = LF_PSPID.
* Create without existing --> Ok
  ELSEIF IF_CREATE EQ 'X' AND
         LF_PSPID_DB IS INITIAL.
    EF_OUTPUT = LF_PSPID.
  ELSE.
    EF_OUTPUT = LF_PSPID_DB.
    EF_INVALID = 'X'.
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_WBSELEM.

  DATA:
    LS_WBSLV  TYPE  TS_WBSLV.

  DATA:
    LF_INVALID  TYPE  FLAG.


* Initialize Output
  CLEAR: EF_INVALID,
         EF_MSGTX,
         ES_WBSLV.

* Get Constants
  GET_GENC( ).

* WBS Element is required
  IF IS_WBSELEM-POSID IS INITIAL.
    EF_INVALID = 'X'.
*   Text-e03: WBS Element is required.
    EF_MSGTX   = TEXT-E03.
    RETURN.
  ENDIF.

* WBS Level is required
  IF IS_WBSELEM-STUFE IS INITIAL.
    EF_INVALID = 'X'.
*   Text-e04: WBS level is required.
    EF_MSGTX   = TEXT-E04.
    RETURN.
  ENDIF.

* WBS Element Levels
  GET_WBSELEM_LEVELS( EXPORTING IF_POSID   = IS_WBSELEM-POSID
                      IMPORTING ES_WBSLV   = LS_WBSLV
                                EF_INVALID = LF_INVALID ).
  IF LF_INVALID EQ 'X' OR
     LS_WBSLV-STUFE NE IS_WBSELEM-STUFE.
    EF_INVALID = 'X'.
*   Text-e05: WBS Element does not match with WBS level.
    EF_MSGTX   = TEXT-E05.
    RETURN.
  ENDIF.

* Check USR01 must not exist (Only P lv 4 and C lv 2)
  IF ( IS_WBSELEM-POSID(1) EQ 'P' AND IS_WBSELEM-STUFE EQ 4 ) OR
     ( IS_WBSELEM-POSID(1) EQ 'C' AND IS_WBSELEM-STUFE EQ 2 ).
    SELECT POSID
      FROM PRPS
     WHERE USR01 EQ @IS_WBSELEM-USR01
       AND POSID NE @IS_WBSELEM-POSID
       AND PBUKR EQ '1000'
     ORDER BY PRIMARY KEY
      INTO @DATA(LF_POSID)
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      EF_INVALID = 'X'.
*     Error: Reference ID &1 is already used for wbs element &2.
      MESSAGE E011(ZSDSPS01) WITH IS_WBSELEM-USR01 LF_POSID
                             INTO EF_MSGTX.
      RETURN.
    ENDIF.
*   Check duplicate within this request
    IF IT_WBSELEM IS SUPPLIED.
      LOOP AT IT_WBSELEM ASSIGNING FIELD-SYMBOL(<L_WBSELEM>)
                         WHERE POSID NE IS_WBSELEM-POSID
                           AND USR01 EQ IS_WBSELEM-USR01
                           AND STUFE EQ IS_WBSELEM-STUFE.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        EF_INVALID = 'X'.
*       Error: Reference ID &1 is already used for wbs element &2.
        MESSAGE E011(ZSDSPS01) WITH IS_WBSELEM-USR01 <L_WBSELEM>-POSID
                               INTO EF_MSGTX.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ES_WBSLV IS SUPPLIED.
    ES_WBSLV = LS_WBSLV.
  ENDIF.

ENDMETHOD.


METHOD COLLECT_ADJUST_EXPENSE.

  DATA:
    LS_ADJ_EXP  TYPE  TS_ADJ_EXP.


* Get Profit Center of WBS
  SELECT PRCTR
    FROM PRPS
   WHERE POSID EQ @IF_POSID
   ORDER BY PRIMARY KEY
    INTO @DATA(LF_PRCTR)
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    CLEAR LF_PRCTR.
  ENDIF.

* Profit Center/Cost Center Not found
  IF LF_PRCTR IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR LS_ADJ_EXP.
  LS_ADJ_EXP-KOSTL  = LF_PRCTR.
  LS_ADJ_EXP-KSTAR  = IF_KSTAR.
  LS_ADJ_EXP-GJAHR  = IF_GJAHR.
  LS_ADJ_EXP-MONAT  = IF_MONAT.

  READ TABLE CT_ADJ_EXP ASSIGNING FIELD-SYMBOL(<L_ADJ_EXP>)
                        WITH KEY KOSTL = LS_ADJ_EXP-KOSTL
                                 KSTAR = LS_ADJ_EXP-KSTAR
                                 GJAHR = LS_ADJ_EXP-GJAHR
                                 MONAT = LS_ADJ_EXP-MONAT
                        BINARY SEARCH.
  IF SY-SUBRC NE 0.
    INSERT LS_ADJ_EXP INTO TABLE CT_ADJ_EXP
                      ASSIGNING <L_ADJ_EXP>.
  ENDIF.

* Update Adjust Amount
  <L_ADJ_EXP>-AMOUNT = <L_ADJ_EXP>-AMOUNT + IF_AMOUNT.

ENDMETHOD.


METHOD GET_1ST_LV3_LINE.

  DATA:
    LF_POSID TYPE  PRPS-POSID,
    LF_FOUND TYPE  FLAG,
    LF_COUNT TYPE  I.


* Initialize output
  CLEAR: EF_POSID,
         EF_LINE.

* Get Hierarchy data
  SELECT C~POSID,
         DOWN~POSID  AS DOWN,
         RIGHT~POSID AS RIGHT
    FROM PROJ AS A
          INNER JOIN PRHI AS B
            ON  B~PSPHI = A~PSPNR
          INNER JOIN PRPS AS C
            ON  C~PSPNR = B~POSNR
          LEFT OUTER JOIN PRPS AS DOWN
            ON  DOWN~PSPNR = B~DOWN
          LEFT OUTER JOIN PRPS AS RIGHT
            ON  RIGHT~PSPNR = B~RIGHT
   WHERE A~PSPID EQ @IF_PSPID
   ORDER BY C~POSID ASCENDING
    INTO TABLE @DATA(LT_PSHI).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Get 1st LV2
  READ TABLE LT_PSHI ASSIGNING FIELD-SYMBOL(<L_PSHI>)
                     WITH KEY POSID = IF_PSPID
                     BINARY SEARCH.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LF_POSID = <L_PSHI>-DOWN.
  READ TABLE LT_PSHI ASSIGNING <L_PSHI>
                     WITH KEY POSID = LF_POSID
                     BINARY SEARCH.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Find 1st LV3
  CLEAR: LF_FOUND,
         LF_COUNT.
  DO.
    IF <L_PSHI>-DOWN IS NOT INITIAL.
      LF_FOUND = 'X'.
      EXIT.
    ELSE.
*     Next LV2
      LF_POSID = <L_PSHI>-RIGHT.
      READ TABLE LT_PSHI ASSIGNING <L_PSHI>
                         WITH KEY POSID = LF_POSID
                         BINARY SEARCH.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
      LF_COUNT = LF_COUNT + 1.
    ENDIF.
  ENDDO.
  IF LF_FOUND IS INITIAL.
    RETURN.
  ENDIF.

* Assign Result
  EF_POSID = <L_PSHI>-DOWN.
  EF_LINE  = 3 + LF_COUNT.

ENDMETHOD.


METHOD GET_GENC.

  CONSTANTS:
    LC_PROFL_NOOVW TYPE  ZSDSDE_PARAM_NAME VALUE 'PROJECT_PROFILE_NOOVW',
    LC_ITMTY_EXP   TYPE  ZSDSDE_PARAM_NAME VALUE 'EXPENSE_ITEM_TYPE'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSPS_PROJECT'.


* Check Already Read?
  IF LF_READ EQ 'X'.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GT_PROFL_NOOVW,
         GT_ITMTY_EXP.

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
*     Project Profile which no overview plan/budget
*     ------------------------------------
      WHEN LC_PROFL_NOOVW.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_PROFL_NOOVW.

*     ------------------------------------
*     Item Type for Expense
*     ------------------------------------
      WHEN LC_ITMTY_EXP.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GT_ITMTY_EXP.

    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD GET_PTIME.

  TYPES: BEGIN OF TS_PTIME_LIST,
           GJAHR TYPE  GJAHR,
           PTIME TYPE  DROP_GJAHR,
         END OF TS_PTIME_LIST.
  TYPES: TT_PTIME_LIST TYPE SORTED TABLE OF TS_PTIME_LIST
                            WITH UNIQUE KEY GJAHR.

  TYPES: BEGIN OF TS_LIST,
           BPROF TYPE  TBP1C-PROFIL,
           WRTTP TYPE  TBP1C-WRTTP,
           PTIME TYPE  TT_PTIME_LIST,
         END OF TS_LIST.
  TYPES: TT_LIST  TYPE  SORTED TABLE OF TS_LIST
                        WITH UNIQUE KEY BPROF
                                        WRTTP.

  CONSTANTS:
    LC_PROJECT TYPE  TBP1C-APPLIK VALUE 'P',
    LC_V3      TYPE  BPIN-PERIV   VALUE 'V3'.

  STATICS:
    LT_LIST TYPE TT_LIST,
    LS_LIST TYPE TS_LIST.

  DATA:
    LF_EJAHR TYPE BPIN-EJAHR,
    LF_SJAHR TYPE BPIN-SJAHR,
    LF_PTIME TYPE TS_PTIME_LIST-PTIME.


* Initialize Output
  CLEAR EF_PTIME.

* Check Buffer
  IF IF_BPROF NE LS_LIST-BPROF OR
     IF_WRTTP NE LS_LIST-WRTTP .
*   Read From Memory
    READ TABLE LT_LIST INTO LS_LIST
                       WITH KEY BPROF = IF_BPROF
                                WRTTP = IF_WRTTP
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_LIST.
      LS_LIST-BPROF = IF_BPROF.
      LS_LIST-WRTTP = IF_WRTTP.

*     Get Budget Profile
      SELECT SINGLE *
        FROM TBP1C
       WHERE PROFIL EQ @IF_BPROF
         AND APPLIK EQ @LC_PROJECT
         AND WRTTP  EQ @IF_WRTTP
        INTO @DATA(LS_TBP1C).
      IF SY-SUBRC NE 0.
        RETURN.
      ENDIF.

*     Call Function Get Period from Profile setting
      CALL FUNCTION 'KBPS_GET_GJAHR'
        EXPORTING
          IM_APPLIK   = LC_PROJECT
          IM_BP1C     = LS_TBP1C
          IM_PERIV    = LC_V3
        IMPORTING
          EX_EJAHR    = LF_EJAHR
          EX_SJAHR    = LF_SJAHR
        EXCEPTIONS
          WRONG_VJAHR = 1
          OTHERS      = 2.
      IF SY-SUBRC <> 0.
        RETURN.
      ENDIF.

      CLEAR LF_PTIME.
      WHILE LF_SJAHR LE LF_EJAHR.
        INSERT VALUE #( GJAHR = LF_SJAHR
                        PTIME = LF_PTIME )
               INTO TABLE LS_LIST-PTIME.
        LF_SJAHR = LF_SJAHR + 1.
        LF_PTIME = LF_PTIME + 1.
      ENDWHILE.

*     Save to Memory
      INSERT LS_LIST INTO TABLE LT_LIST.

    ENDIF.

  ENDIF.

* Assign Output
  READ TABLE LS_LIST-PTIME ASSIGNING FIELD-SYMBOL(<L_PTIME>)
                           WITH KEY GJAHR = IF_GJAHR
                           BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    EF_PTIME = <L_PTIME>-PTIME.
  ENDIF.

ENDMETHOD.


METHOD GET_SUM_PLAN_LV2.

* Initialize Output
  CLEAR: EF_SUM_AMT.

  SELECT B~POSID,
         SUM( C~WKG001 ) AS WKG001,
         SUM( C~WKG002 ) AS WKG002,
         SUM( C~WKG003 ) AS WKG003,
         SUM( C~WKG004 ) AS WKG004,
         SUM( C~WKG005 ) AS WKG005,
         SUM( C~WKG006 ) AS WKG006,
         SUM( C~WKG007 ) AS WKG007,
         SUM( C~WKG008 ) AS WKG008,
         SUM( C~WKG009 ) AS WKG009,
         SUM( C~WKG010 ) AS WKG010,
         SUM( C~WKG011 ) AS WKG011,
         SUM( C~WKG012 ) AS WKG012,
         SUM( C~WKG013 ) AS WKG013,
         SUM( C~WKG014 ) AS WKG014,
         SUM( C~WKG015 ) AS WKG015,
         SUM( C~WKG016 ) AS WKG016
    FROM PROJ AS A
           INNER JOIN PRPS AS B
             ON B~PSPHI = A~PSPNR
           INNER JOIN V_COSP_VIEW AS C
             ON  C~LEDNR = @GC_LEDNR
             AND C~OBJNR = B~OBJNR
             AND C~GJAHR = @IS_KEY-GJAHR
   WHERE A~PSPID EQ @IS_KEY-PSPID
     AND B~STUFE EQ 2
     AND ( C~WRTTP EQ @GC_PLAN01 OR
           C~WRTTP EQ @GC_PLAN10 )
     AND C~VERSN EQ @IS_KEY-VERSN
   GROUP BY B~POSID
    INTO TABLE @DATA(LT_COSP).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LOOP AT LT_COSP ASSIGNING FIELD-SYMBOL(<L_COSP>).
    EF_SUM_AMT = EF_SUM_AMT + <L_COSP>-WKG001 + <L_COSP>-WKG002 + <L_COSP>-WKG003 + <L_COSP>-WKG004 +
                              <L_COSP>-WKG005 + <L_COSP>-WKG006 + <L_COSP>-WKG007 + <L_COSP>-WKG008 +
                              <L_COSP>-WKG009 + <L_COSP>-WKG010 + <L_COSP>-WKG011 + <L_COSP>-WKG012 +
                              <L_COSP>-WKG013 + <L_COSP>-WKG014 + <L_COSP>-WKG015 + <L_COSP>-WKG016 .
  ENDLOOP.

ENDMETHOD.


METHOD POST_ADJUST_EXP.

  DATA:
    LT_BAPI_INDEX  TYPE  STANDARD TABLE OF BAPIACPSTRU,
    LT_BAPI_COOBJ  TYPE  STANDARD TABLE OF BAPIPCPOBJ,
    LT_BAPI_PERVAL TYPE  STANDARD TABLE OF BAPIPCPVAL,
    LT_RETURN      TYPE  STANDARD TABLE OF BAPIRET2.

  DATA:
    LS_BAPI_HEAD   TYPE  BAPIPLNHDR,
    LS_BAPI_COOBJ  TYPE  BAPIPCPOBJ,
    LS_BAPI_PERVAL TYPE  BAPIPCPVAL.

  DATA:
    LF_INDEX   TYPE  BAPIPCPOBJ-OBJECT_INDEX.


* Initialize Output
  CLEAR: ET_RETURN.

* Initialize Variables
  CLEAR: LT_BAPI_COOBJ,
         LT_BAPI_PERVAL,
         LT_BAPI_INDEX,
         LT_RETURN.
  CLEAR: LF_INDEX.

* Group Data by Cost Center/Cost Elm/Year
  LOOP AT IT_ADJ_EXP INTO DATA(LS_ADJ_EXP) ##INTO_OK
                     GROUP BY ( KOSTL = LS_ADJ_EXP-KOSTL
                                KSTAR = LS_ADJ_EXP-KSTAR
                                GJAHR = LS_ADJ_EXP-GJAHR ) ASCENDING
                     ASSIGNING FIELD-SYMBOL(<L_GROUP>).

*   Generate New Index
    LF_INDEX = LF_INDEX + 1.

*   Assign BAPI Data
    CLEAR LS_BAPI_COOBJ.
    LS_BAPI_COOBJ-OBJECT_INDEX  = LF_INDEX.
    LS_BAPI_COOBJ-COSTCENTER    = <L_GROUP>-KOSTL.
    INSERT LS_BAPI_COOBJ INTO TABLE LT_BAPI_COOBJ.

    CLEAR LS_BAPI_PERVAL.
    LS_BAPI_PERVAL-VALUE_INDEX = LF_INDEX.
    LS_BAPI_PERVAL-COST_ELEM   = <L_GROUP>-KSTAR.

    INSERT VALUE #( OBJECT_INDEX = LS_BAPI_COOBJ-OBJECT_INDEX
                    VALUE_INDEX  = LS_BAPI_PERVAL-VALUE_INDEX )
                 INTO TABLE LT_BAPI_INDEX.

    LOOP AT GROUP <L_GROUP> ASSIGNING FIELD-SYMBOL(<L_ADJ_EXP>).

      DATA(LF_FNAME) = |LS_BAPI_PERVAL-VAR_VAL_PER{ <L_ADJ_EXP>-MONAT }|.
      ASSIGN (LF_FNAME) TO FIELD-SYMBOL(<L_VAL>).
      IF SY-SUBRC EQ 0.
        <L_VAL> = <L_ADJ_EXP>-AMOUNT.
      ENDIF.

    ENDLOOP.

    INSERT LS_BAPI_PERVAL INTO TABLE LT_BAPI_PERVAL.

  ENDLOOP.

* Assign Header data
  CLEAR LS_BAPI_HEAD.
  LS_BAPI_HEAD-CO_AREA        = IF_KOKRS.
  LS_BAPI_HEAD-FISC_YEAR      = IS_KEY-GJAHR.
  LS_BAPI_HEAD-PERIOD_FROM    = 1.
  LS_BAPI_HEAD-PERIOD_TO      = 12 ##NUMBER_OK.
  LS_BAPI_HEAD-VERSION        = IS_KEY-VERSN.
  LS_BAPI_HEAD-PLAN_CURRTYPE  = GC_CURRTYP.

  CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
    EXPORTING
      HEADERINFO     = LS_BAPI_HEAD
      DELTA          = 'X'
    TABLES
      INDEXSTRUCTURE = LT_BAPI_INDEX
      COOBJECT       = LT_BAPI_COOBJ
      PERVALUE       = LT_BAPI_PERVAL
      RETURN         = LT_RETURN.

  IF IF_TEST EQ 'X' OR
     LT_RETURN IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ENDIF.

* Collect Result
  INSERT LINES OF LT_RETURN INTO TABLE ET_RETURN.

ENDMETHOD.


METHOD SAVE_LONGTEXT.

  CONSTANTS:
    LC_TDOBJECT TYPE  THEAD-TDOBJECT VALUE 'PMS',
    LC_TDID     TYPE  THEAD-TDID     VALUE 'LTXT'.

  DATA:
    LT_LINE   TYPE  STANDARD TABLE OF TLINE.

  DATA:
    LS_HEAD TYPE  THEAD,
    LS_LINE TYPE  TLINE.

  DATA:
    LF_PSPNR   TYPE  PRPS-PSPNR,
    LF_TDNAME  TYPE  THEAD-TDNAME,
    LF_TDSPRAS TYPE  THEAD-TDSPRAS,
    LF_TEXT    TYPE  ZSDSDE_POST1.


  CASE IF_TYPE.
    WHEN GC_TYPE_PROJECT.
*     Wait up to 10 Seconds for project
      DO 10 TIMES.
        CALL FUNCTION 'ENQUEUE_EC_PROJ'
          EXPORTING
            MODE_PROJ_ENQ  = 'E'
            MANDT          = SY-MANDT
            TYP            = 'P'
            PSPID          = IF_POSID
          EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.
        IF SY-SUBRC <> 0.
          WAIT UP TO 1 SECONDS.
        ENDIF.

        CALL FUNCTION 'DEQUEUE_EC_PROJ'
          EXPORTING
            MODE_PROJ_ENQ = 'E'
            MANDT         = SY-MANDT
            TYP           = 'P'
            PSPID         = IF_POSID.
        EXIT.
      ENDDO.

      CALL FUNCTION 'CONVERSION_EXIT_KONPD_INPUT'
        EXPORTING
          INPUT     = IF_POSID
        IMPORTING
          OUTPUT    = LF_PSPNR
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF SY-SUBRC <> 0.
        RETURN.
      ENDIF.

    WHEN GC_TYPE_WBSELEM.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = IF_POSID
        IMPORTING
          OUTPUT    = LF_PSPNR
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF SY-SUBRC <> 0.
        RETURN.
      ENDIF.
  ENDCASE.

  LF_TDSPRAS = SY-LANGU.
  LF_TDNAME = |{ IF_TYPE }{ LF_PSPNR } |.

  CLEAR LS_HEAD.
  LS_HEAD-TDOBJECT  =  LC_TDOBJECT.
  LS_HEAD-TDNAME    =  LF_TDNAME.
  LS_HEAD-TDID      =  LC_TDID.
  LS_HEAD-TDSPRAS   =  LF_TDSPRAS.

* Assign Text Table
  LF_TEXT = IF_POST1.
  CLEAR LS_LINE.
  LS_LINE-TDFORMAT = '*'.
  WHILE LF_TEXT IS NOT INITIAL.

    IF STRLEN( LF_TEXT ) GT 132 ##NUMBER_OK.
      LS_LINE-TDLINE = LF_TEXT(132).
      LF_TEXT = LF_TEXT+132.
    ELSE.
      LS_LINE-TDLINE = LF_TEXT.
      CLEAR LF_TEXT.
    ENDIF.

    INSERT LS_LINE INTO TABLE LT_LINE.
    CLEAR LS_LINE.
    LS_LINE-TDFORMAT = '='.

  ENDWHILE.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      HEADER          = LS_HEAD
      INSERT          = ' '
      SAVEMODE_DIRECT = 'X'
    TABLES
      LINES           = LT_LINE
    EXCEPTIONS
      ID              = 1
      LANGUAGE        = 2
      NAME            = 3
      OBJECT          = 4
      OTHERS          = 5.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  CASE IF_TYPE.
    WHEN GC_TYPE_PROJECT.
      UPDATE PROJ SET TXTSP = LF_TDSPRAS
                  WHERE PSPNR EQ LF_PSPNR.
    WHEN GC_TYPE_WBSELEM.
      UPDATE PRPS SET TXTSP = LF_TDSPRAS
                  WHERE PSPNR EQ LF_PSPNR.
  ENDCASE.

  COMMIT WORK AND WAIT.

ENDMETHOD.
ENDCLASS.
