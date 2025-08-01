CLASS ZCL_SDSPS_PROJECT_BUDGET_SERV DEFINITION
  PUBLIC
  INHERITING FROM ZCL_SDSCA_REST_SERVICE
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF TS_BUDGET_MAP,
        BUDGET     TYPE ZCL_SDSPS_PROJECT=>TS_BUDGET,
        BUDGET_REQ TYPE ZSDSPSS007,
      END OF TS_BUDGET_MAP .
    TYPES:
      TT_BUDGET_MAP TYPE STANDARD TABLE OF TS_BUDGET_MAP
                                  WITH DEFAULT KEY .
    TYPES:
      BEGIN OF TS_BUDGET_DATA,
        KEY              TYPE  ZCL_SDSPS_PROJECT=>TS_BUDGET_KEY,
        CREATE_ORG_VERSN TYPE  FLAG,
        MODE             TYPE  CHAR1,
        BUDGET           TYPE  TT_BUDGET_MAP,
      END OF TS_BUDGET_DATA .
    TYPES:
      TT_BUDGET_DATA  TYPE SORTED TABLE OF TS_BUDGET_DATA
                                    WITH UNIQUE KEY KEY
                                                    CREATE_ORG_VERSN .

    TYPES:
      BEGIN OF TS_CONT_ERROR,
        KEY              TYPE  ZCL_SDSPS_PROJECT=>TS_BUDGET_KEY,
        CREATE_ORG_VERSN TYPE  FLAG,
        MODE             TYPE  CHAR1,
        BUDGET           TYPE  TT_BUDGET_MAP,
        RETURN           TYPE  BAPIRET2_TAB,
      END OF TS_CONT_ERROR .
    TYPES:
      TT_CONT_ERROR  TYPE STANDARD TABLE OF TS_CONT_ERROR
                             WITH DEFAULT KEY.

    CONSTANTS GC_CREATE TYPE CHAR1 VALUE 'C' ##NO_TEXT.
    CONSTANTS GC_CHANGE TYPE CHAR1 VALUE 'U' ##NO_TEXT.
    CONSTANTS GC_COST TYPE CHAR1 VALUE '1' ##NO_TEXT.
    CONSTANTS GC_REVENUE TYPE CHAR1 VALUE '2' ##NO_TEXT.

    METHODS INITIALIZE_DATA
        REDEFINITION .
    METHODS PROCESS_DATA
        REDEFINITION .
    METHODS HANDLING_ERROR
        REDEFINITION .
protected section.
private section.

  data GREF_VALIDATE type ref to ZCL_SDSCA_DATA_VALIDATION .

  methods ASSIGN_KSTAR
    importing
      !IF_PRJTY type ZSDSDE_PRJTY
      !IF_ITMTY type ZSDSDE_PS_ITMTY
      !IF_ACTTY type ZSDSDE_PS_ACTTY
    exporting
      !EF_REV_KSTAR type KSTAR
      !EF_CST_KSTAR type KSTAR .
  methods ASSIGN_MODE
    importing
      !IF_PSPID type PROJ-PSPID
      !IF_GJAHR type GJAHR
      !IF_VERSN type VERSN
      !IF_CREATE_ORG_VERSN type FLAG
    exporting
      !EF_MODE type TS_BUDGET_DATA-MODE .
  methods ASSIGN_MONAT
    importing
      !IF_PSPID type PROJ-PSPID
      !IF_GJAHR type GJAHR
    exporting
      !EF_MONAT type MONAT .
  class-methods VALIDATE_CONT_ERROR
    importing
      !IS_BUDGET type TS_BUDGET_MAP
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  methods ASSIGN_BAPI_BUDGET
    importing
      !IT_BUDGET type TT_BUDGET_MAP
    exporting
      !ET_BAPI_BUDGET type ZCL_SDSPS_PROJECT=>TT_BUDGET .
  methods ASSIGN_BUDGET_RESULT
    importing
      !IF_MODE type TS_BUDGET_DATA-MODE
      !IS_KEY type TS_BUDGET_DATA-KEY
      !IT_BUDGET type TS_BUDGET_DATA-BUDGET
      !IT_RETURN type BAPIRET2_TAB
      !IF_STEP type ZCL_SDSPS_PROJECT=>TS_PROC_STEP optional
    changing
      !CS_RESPONSE type ZSDSPSS008 .
  methods MAINTAIN_PROJECT_BUDGET
    importing
      !IS_REQUEST type ZSDSPSS008
    exporting
      !ES_RESPONSE type ZSDSPSS008
      !EF_STATUS type ZSDSDE_REST_STATUS
      !EF_MESSAGE type ZSDSDE_REST_MESSAGE .
  methods VALIDATE_REQUEST
    importing
      !IS_REQUEST type ZSDSPSS008
    exporting
      !ET_BUDGET type TT_BUDGET_DATA
      !EF_STATUS type ZSDSDE_REST_STATUS
      !EF_MESSAGE type ZSDSDE_REST_MESSAGE
      !ET_CONT_ERROR type TT_CONT_ERROR .
  methods IS_FROM_SFDC
    importing
      !IF_PSPID type PROJ-PSPID
    returning
      value(RF_RESULT) type ABAP_BOOLEAN .
ENDCLASS.



CLASS ZCL_SDSPS_PROJECT_BUDGET_SERV IMPLEMENTATION.


METHOD ASSIGN_BAPI_BUDGET.

* Initialize Output
  CLEAR: ET_BAPI_BUDGET.

  LOOP AT IT_BUDGET ASSIGNING FIELD-SYMBOL(<L_BUDGET>).
    INSERT <L_BUDGET>-BUDGET INTO TABLE ET_BAPI_BUDGET.
  ENDLOOP.

ENDMETHOD.


METHOD ASSIGN_BUDGET_RESULT.

  DATA:
    LS_RETURN      TYPE  BAPIRET2,
    LS_BUDGET_RESP TYPE  ZSDSPSS007.


  CLEAR LS_RETURN.

* ----------------------
* Determine Result Message
* ----------------------
* Success
  IF IT_RETURN IS INITIAL.
    CASE IF_MODE.
      WHEN GC_CREATE.
*       Message: Create Original Plan/Budget successfully.
        LS_RETURN-TYPE   = 'S'.
        LS_RETURN-ID     = 'ZSDSPS01'.
        LS_RETURN-NUMBER = '024'.
      WHEN GC_CHANGE.
*       Message: Revise Plan/Budget successfully.
        LS_RETURN-TYPE   = 'S'.
        LS_RETURN-ID     = 'ZSDSPS01'.
        LS_RETURN-NUMBER = '025'.
    ENDCASE.
* Error
  ELSE.
*   Get 1st Error message
    LOOP AT IT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                      WHERE ( TYPE EQ 'X' OR
                              TYPE EQ 'A' OR
                              TYPE EQ 'E' ).
      EXIT.
    ENDLOOP.
    IF SY-SUBRC NE 0.
*     Get 1st Message
      READ TABLE IT_RETURN ASSIGNING <L_RETURN>
                           INDEX 1.
    ENDIF.
    IF SY-SUBRC EQ 0.
      LS_RETURN = <L_RETURN>.
    ENDIF.
  ENDIF.

* Assign WBS Result
  LOOP AT IT_BUDGET ASSIGNING FIELD-SYMBOL(<L_BUDGET>).
    CLEAR LS_BUDGET_RESP.
    IF LS_RETURN-TYPE = 'S'.
      LS_BUDGET_RESP-POSID        = <L_BUDGET>-BUDGET-POSID.
      LS_BUDGET_RESP-VERSN        = IS_KEY-VERSN.
      LS_BUDGET_RESP-KSTAR        = <L_BUDGET>-BUDGET-KSTAR.
      LS_BUDGET_RESP-GJAHR        = <L_BUDGET>-BUDGET-GJAHR.
      LS_BUDGET_RESP-MONAT        = <L_BUDGET>-BUDGET-MONAT.
      LS_BUDGET_RESP-WAERS        = <L_BUDGET>-BUDGET_REQ-WAERS.
      IF <L_BUDGET>-BUDGET-AMOUNT LT 0.
        LS_BUDGET_RESP-REVAM      = <L_BUDGET>-BUDGET-AMOUNT.
      ELSE.
        LS_BUDGET_RESP-CSTAM      = <L_BUDGET>-BUDGET-AMOUNT.
      ENDIF.
      LS_BUDGET_RESP-RESP_STATUS  = ZCL_SDSCA_REST_INTF_UTILITY=>GC_SUCCESS.
    ELSE.
      IF <L_BUDGET>-BUDGET_REQ IS INITIAL. "Not Auto Gen data
        CONTINUE.
      ENDIF.
      LS_BUDGET_RESP = <L_BUDGET>-BUDGET_REQ.
      LS_BUDGET_RESP-RESP_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
    ENDIF.
    IF LS_RETURN-MESSAGE IS NOT INITIAL.
      LS_BUDGET_RESP-RESP_MESSAGE = LS_RETURN-MESSAGE.
    ELSE.
      MESSAGE ID LS_RETURN-ID TYPE 'I'
              NUMBER LS_RETURN-NUMBER
              WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                   LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4
              INTO LS_BUDGET_RESP-RESP_MESSAGE.
    ENDIF.
*   Add Step info in error message
    IF IF_STEP IS NOT INITIAL AND
       LS_BUDGET_RESP-RESP_STATUS EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
      LS_BUDGET_RESP-RESP_MESSAGE = |({ IF_STEP }){ LS_BUDGET_RESP-RESP_MESSAGE }|.
    ENDIF.
    INSERT LS_BUDGET_RESP INTO TABLE CS_RESPONSE-BUDGET.
  ENDLOOP.

ENDMETHOD.


METHOD ASSIGN_KSTAR.

* Initialize Output
  CLEAR: EF_REV_KSTAR,
         EF_CST_KSTAR.

  SELECT A~CST_KSTAR AS CST_KSTAR1,
         B~CST_KSTAR AS CST_KSTAR2,
         A~REV_KSTAR AS REV_KSTAR1,
         B~REV_KSTAR AS REV_KSTAR2
    FROM ZSDSPSC002 AS A
           LEFT OUTER JOIN ZSDSPSC001 AS B
             ON  B~PRJTY = A~PRJTY
             AND B~ITMTY = A~ITMTY
   WHERE A~PRJTY EQ @IF_PRJTY
     AND A~ITMTY EQ @IF_ITMTY
     AND A~ACTTY EQ @IF_ACTTY
   ORDER BY A~ITMTY ASCENDING,
            A~ACTTY ASCENDING,
            A~CST_KSTAR ASCENDING
    INTO @DATA(LS_KSTAR)
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  IF LS_KSTAR-REV_KSTAR1 IS NOT INITIAL.
    EF_REV_KSTAR = LS_KSTAR-REV_KSTAR1.
  ELSE.
    EF_REV_KSTAR = LS_KSTAR-REV_KSTAR2.
  ENDIF.
  IF LS_KSTAR-CST_KSTAR1 IS NOT INITIAL.
    EF_CST_KSTAR = LS_KSTAR-CST_KSTAR1.
  ELSE.
    EF_CST_KSTAR = LS_KSTAR-CST_KSTAR2.
  ENDIF.

ENDMETHOD.


METHOD ASSIGN_MODE.

  DATA:
    LF_EXIST  TYPE  FLAG.


* Initialize Output
  CLEAR EF_MODE.

* For C WBS Level 2 in Current Version, Always in Create mode
  IF IF_PSPID(1) EQ 'C'.
    IF IF_VERSN EQ ZCL_SDSPS_PROJECT=>GC_CURRENT_VERSN AND
       IF_CREATE_ORG_VERSN EQ 'X'.
      EF_MODE = GC_CREATE.
    ELSE.
      EF_MODE = GC_CHANGE.
    ENDIF.
    RETURN.
  ENDIF.

* Check Original Plan exist
  ZCL_SDSPS_PROJECT=>CHECK_ORIGINAL_PLAN(
    EXPORTING
      IF_PSPID = IF_PSPID
      IF_GJAHR = IF_GJAHR
    IMPORTING
      EF_EXIST = LF_EXIST ).

  IF LF_EXIST IS INITIAL.
    EF_MODE = GC_CREATE.
  ELSE.
    EF_MODE = GC_CHANGE.
  ENDIF.

ENDMETHOD.


METHOD ASSIGN_MONAT.

* Initialize Output
  CLEAR EF_MONAT.

* Get Project Create date
  SELECT SINGLE VBUKR,
                ERDAT
    FROM PROJ
   WHERE PSPID EQ @IF_PSPID
    INTO @DATA(LS_PROJ).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* If not same year of create date, use period 01
  IF LS_PROJ-ERDAT(4) NE IF_GJAHR.
    EF_MONAT = '01'.
  ELSE.
*   Assign period from create date
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        COMPANYCODEID = LS_PROJ-VBUKR
        POSTING_DATE  = LS_PROJ-ERDAT
      IMPORTING
        FISCAL_PERIOD = EF_MONAT.
  ENDIF.

ENDMETHOD.


METHOD HANDLING_ERROR.

  FIELD-SYMBOLS:
    <L_RESPONSE>  TYPE  ZSDSPSS008.


* Initialize Output
  CLEAR EF_ASSIGNED.

  ASSIGN IREF_RESPONSE_DATA->* TO <L_RESPONSE>.
  IF SY-SUBRC EQ 0.
    INSERT VALUE #( RESP_STATUS = IF_STATUS
                    RESP_MESSAGE = IF_MESSAGE )
                 INTO TABLE <L_RESPONSE>-BUDGET.
    EF_ASSIGNED = 'X'.
  ENDIF.

ENDMETHOD.


METHOD INITIALIZE_DATA.

* Create Validation Instance
  CREATE OBJECT GREF_VALIDATE.

ENDMETHOD.


METHOD IS_FROM_SFDC.

  CLEAR RF_RESULT.

* Check If data is from Salesforce
* - Check Project starting with P
  IF IF_PSPID(1) EQ 'P'.
    RF_RESULT = ABAP_TRUE.
  ENDIF.

ENDMETHOD.


METHOD MAINTAIN_PROJECT_BUDGET.

  DATA:
    LT_BUDGET      TYPE  TT_BUDGET_DATA,
    LT_RETURN      TYPE BAPIRET2_TAB,
    LT_BAPI_BUDGET TYPE ZCL_SDSPS_PROJECT=>TT_BUDGET,
    LT_CONT_ERROR  TYPE TT_CONT_ERROR.

  DATA:
    LF_STEP  TYPE  ZCL_SDSPS_PROJECT=>TS_PROC_STEP.


* Initialize Output
  CLEAR: ES_RESPONSE.

* -------------------
* Validate Request
* -------------------
  VALIDATE_REQUEST(
    EXPORTING
      IS_REQUEST    = IS_REQUEST
    IMPORTING
      ET_BUDGET     = LT_BUDGET
      EF_STATUS     = EF_STATUS
      EF_MESSAGE    = EF_MESSAGE
      ET_CONT_ERROR = LT_CONT_ERROR ).
  IF EF_STATUS IS NOT INITIAL.
    RETURN.
  ENDIF.

* -------------------
* Assign Error - Which can continue further processing
* -------------------
  LOOP AT LT_CONT_ERROR ASSIGNING FIELD-SYMBOL(<L_CONT_ERROR>).
    ASSIGN_BUDGET_RESULT(
      EXPORTING
        IF_MODE     = <L_CONT_ERROR>-MODE
        IS_KEY      = <L_CONT_ERROR>-KEY
        IT_BUDGET   = <L_CONT_ERROR>-BUDGET
        IT_RETURN   = <L_CONT_ERROR>-RETURN
      CHANGING
        CS_RESPONSE = ES_RESPONSE ).
  ENDLOOP.

* -------------------
* Maintain Project Budget
* -------------------
  LOOP AT LT_BUDGET ASSIGNING FIELD-SYMBOL(<L_BUDGET>).

*   Convert data for BAPI calling
    ASSIGN_BAPI_BUDGET(
      EXPORTING
        IT_BUDGET      = <L_BUDGET>-BUDGET
      IMPORTING
        ET_BAPI_BUDGET = LT_BAPI_BUDGET ).

    CASE <L_BUDGET>-MODE.
*     -------------------
*     Create Budget
*     -------------------
      WHEN GC_CREATE.
        ZCL_SDSPS_PROJECT=>CREATE_BUDGET( EXPORTING IS_KEY              = <L_BUDGET>-KEY
                                                    IF_CREATE_ORG_VERSN = <L_BUDGET>-CREATE_ORG_VERSN
                                                    IT_BUDGET           = LT_BAPI_BUDGET
                                                    IF_TEST             = ' '
                                          IMPORTING ET_RETURN           = LT_RETURN
                                                    EF_STEP             = LF_STEP ).

*     -------------------
*     Change Budget
*     -------------------
      WHEN GC_CHANGE.
        ZCL_SDSPS_PROJECT=>CHANGE_BUDGET( EXPORTING IS_KEY    = <L_BUDGET>-KEY
                                                    IT_BUDGET = LT_BAPI_BUDGET
                                                    IF_TEST   = ' '
                                          IMPORTING ET_RETURN = LT_RETURN
                                                    EF_STEP   = LF_STEP ).

    ENDCASE.

    ASSIGN_BUDGET_RESULT(
      EXPORTING
        IF_MODE     = <L_BUDGET>-MODE
        IS_KEY      = <L_BUDGET>-KEY
        IT_BUDGET   = <L_BUDGET>-BUDGET
        IT_RETURN   = LT_RETURN
        IF_STEP     = LF_STEP
      CHANGING
        CS_RESPONSE = ES_RESPONSE ).
  ENDLOOP.

ENDMETHOD.


METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Class              : ZCL_SDSPS_PROJECT_BUDGET_SERV
*  Creation Date      : 29.05.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : PSI006, PSI007, PSI008 and PSI009
*  Description        : This is a Processing class of REST interface
*                       PSI006_2 to maintain project plan/budget
*  Purpose            : To maintain Project plan/budget
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA:
    LS_REQUEST  TYPE  ZSDSPSS008.

  FIELD-SYMBOLS:
    <L_RESPONSE>  TYPE  ZSDSPSS008.


* Initialize Output
  CLEAR: EF_STATUS,
         EF_MESSAGE,
         EF_HTTP_ERROR.

  LS_REQUEST = IREF_REQUEST_DATA->*.

  ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
  IF SY-SUBRC NE 0.
*   Critical error
    RETURN.
  ENDIF.

* Maintain Project Budget
  MAINTAIN_PROJECT_BUDGET(
    EXPORTING
      IS_REQUEST = LS_REQUEST
    IMPORTING
      ES_RESPONSE = <L_RESPONSE>
      EF_STATUS   = EF_STATUS
      EF_MESSAGE  = EF_MESSAGE ).

* Validation Error Found
  IF EF_STATUS IS NOT INITIAL.
    INSERT VALUE #( RESP_STATUS = EF_STATUS
                    RESP_MESSAGE = EF_MESSAGE )
                 INTO TABLE <L_RESPONSE>-BUDGET.
    RETURN.
  ENDIF.

* Find Any Error?
  LOOP AT <L_RESPONSE>-BUDGET ASSIGNING FIELD-SYMBOL(<L_BUDGET>)
                              WHERE RESP_STATUS EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*   Error Found, return error
    EF_STATUS  = <L_BUDGET>-RESP_STATUS.
    EF_MESSAGE = <L_BUDGET>-RESP_MESSAGE.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    LOOP AT <L_RESPONSE>-BUDGET ASSIGNING <L_BUDGET>.
*     Return 1st Message when error not found
      EF_STATUS  = <L_BUDGET>-RESP_STATUS.
      EF_MESSAGE = <L_BUDGET>-RESP_MESSAGE.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_CONT_ERROR.

  CONSTANTS:
    LC_LKD  TYPE  BAPI_SYSTEM_STATUS-SYSTEM_STATUS VALUE 'LKD',
    LC_CLSD TYPE  BAPI_SYSTEM_STATUS-SYSTEM_STATUS VALUE 'CLSD',
    LC_TECO TYPE  BAPI_SYSTEM_STATUS-SYSTEM_STATUS VALUE 'TECO'.

  DATA:
    LS_RETURN  TYPE  BAPIRETURN1,
    LS_RETURN2 TYPE  BAPIRET2.

  DATA:
    LT_WBS        TYPE  STANDARD TABLE OF BAPI_WBS_ELEMENTS,
    LT_WBS_STATUS TYPE  STANDARD TABLE OF BAPI_WBS_SYSTEM_STATUS.


* Initialize Output
  CLEAR ET_RETURN.

* ----------------------
* Read WBS Status
* ----------------------
  INSERT VALUE #( WBS_ELEMENT = IS_BUDGET-BUDGET-POSID )
         INTO TABLE LT_WBS.
  CALL FUNCTION 'BAPI_BUS2054_GET_STATUS'
    IMPORTING
      RETURN          = LS_RETURN
    TABLES
      I_WBS_ELEMENTS  = LT_WBS
      E_SYSTEM_STATUS = LT_WBS_STATUS.
  IF LS_RETURN IS NOT INITIAL.
    CLEAR LT_WBS_STATUS.
  ENDIF.

* Check WBS Status is LKD/CLSD/TECO
  LOOP AT LT_WBS_STATUS ASSIGNING FIELD-SYMBOL(<L_WBS_STATUS>)
                        WHERE WBS_ELEMENT EQ IS_BUDGET-BUDGET-POSID
                          AND ( SYSTEM_STATUS EQ LC_LKD  OR
                                SYSTEM_STATUS EQ LC_CLSD OR
                                SYSTEM_STATUS EQ LC_TECO ).
    CLEAR LS_RETURN2.
    LS_RETURN2-TYPE    = 'E'.
    LS_RETURN2-ID      = 'BS'.
    LS_RETURN2-NUMBER  = '011'.
    LS_RETURN2-MESSAGE_V1 = <L_WBS_STATUS>-SYSTEM_STATUS.
    LS_RETURN2-MESSAGE_V2 = |WBS { IS_BUDGET-BUDGET-POSID }|.
    MESSAGE ID LS_RETURN2-ID
            TYPE LS_RETURN2-TYPE
            NUMBER LS_RETURN2-NUMBER
            WITH LS_RETURN2-MESSAGE_V1 LS_RETURN2-MESSAGE_V2
                 LS_RETURN2-MESSAGE_V3 LS_RETURN2-MESSAGE_V4
            INTO LS_RETURN2-MESSAGE.
*   Assign Result
    INSERT LS_RETURN2 INTO TABLE ET_RETURN.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD VALIDATE_REQUEST.

  DATA:
    LT_RETURN TYPE  BAPIRET2_TAB,
    LT_BUDGET TYPE  TT_BUDGET_MAP.

  DATA:
    LS_KEY        TYPE  TS_BUDGET_DATA-KEY,
    LS_BUDGET     TYPE  TS_BUDGET_MAP,
    LS_BUDGET_REV TYPE  TS_BUDGET_MAP,
    LS_WBSLV      TYPE  ZCL_SDSPS_PROJECT=>TS_WBSLV.

  DATA:
    LF_INVALID          TYPE  FLAG,
    LF_CREATE_ORG_VERSN TYPE  FLAG,
    LF_REV_KSTAR        TYPE  KSTAR.


* Initialize Output
  CLEAR: ET_BUDGET,
         EF_STATUS,
         EF_MESSAGE,
         ET_CONT_ERROR.

  LOOP AT IS_REQUEST-BUDGET ASSIGNING FIELD-SYMBOL(<L_BUDGET_REQ>).

    CLEAR: LS_KEY,
           LS_BUDGET,
           LF_CREATE_ORG_VERSN.

    LS_KEY-GJAHR = <L_BUDGET_REQ>-GJAHR.

    GREF_VALIDATE->VALIDATE_CO_VERSION( EXPORTING IF_INPUT   = <L_BUDGET_REQ>-VERSN
                                        IMPORTING EF_OUTPUT  = LS_KEY-VERSN
                                                  EF_INVALID = LF_INVALID ).
    IF LF_INVALID IS NOT INITIAL.
      EF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*     Error: Version &1 is invalid.
      MESSAGE E029(ZSDSPS01) WITH <L_BUDGET_REQ>-VERSN
              INTO EF_MESSAGE.
      EXIT.
    ENDIF.

    LS_BUDGET-BUDGET_REQ = <L_BUDGET_REQ>.

*   Validate WBS Element
    GREF_VALIDATE->VALIDATE_WBSELEM( EXPORTING IF_INPUT   = <L_BUDGET_REQ>-POSID
                                     IMPORTING EF_OUTPUT  = LS_BUDGET-BUDGET-POSID
                                               EF_OBJNR   = LS_BUDGET-BUDGET-OBJNR
                                               EF_INVALID = LF_INVALID
                                               EF_PSPID   = LS_KEY-PSPID ).
    IF LF_INVALID IS NOT INITIAL.
      EF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*     Error: WBS Element &1 does not exist.
      MESSAGE E026(ZSDSPS01) WITH <L_BUDGET_REQ>-POSID
              INTO EF_MESSAGE.
      EXIT.
    ENDIF.

*   Determine WBS Level
    ZCL_SDSPS_PROJECT=>GET_WBSELEM_LEVELS( EXPORTING IF_POSID = LS_BUDGET-BUDGET-POSID
                                           IMPORTING ES_WBSLV = LS_WBSLV ).

*   Create Original Version Flag
    IF LS_KEY-PSPID(1) EQ 'P' OR
       LS_WBSLV-STUFE LE 2.
      LF_CREATE_ORG_VERSN = 'X'.
    ENDIF.

*   Add Processing Key
    READ TABLE ET_BUDGET ASSIGNING FIELD-SYMBOL(<L_BUDGET_DATA>)
                         WITH KEY KEY = LS_KEY
                                  CREATE_ORG_VERSN = LF_CREATE_ORG_VERSN
                         BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT VALUE #( KEY = LS_KEY
                      CREATE_ORG_VERSN = LF_CREATE_ORG_VERSN )
                     INTO TABLE ET_BUDGET
                     ASSIGNING <L_BUDGET_DATA>.
      ASSIGN_MODE(
        EXPORTING
          IF_PSPID            = <L_BUDGET_DATA>-KEY-PSPID
          IF_GJAHR            = <L_BUDGET_DATA>-KEY-GJAHR
          IF_VERSN            = <L_BUDGET_DATA>-KEY-VERSN
          IF_CREATE_ORG_VERSN = <L_BUDGET_DATA>-CREATE_ORG_VERSN
        IMPORTING
          EF_MODE             = <L_BUDGET_DATA>-MODE ).
    ENDIF.

    IF <L_BUDGET_REQ>-KSTAR IS NOT INITIAL.
      GREF_VALIDATE->VALIDATE_COST_ELEMENT( EXPORTING IF_INPUT   = <L_BUDGET_REQ>-KSTAR
                                            IMPORTING EF_OUTPUT  = LS_BUDGET-BUDGET-KSTAR
                                                      EF_INVALID = LF_INVALID ).
      IF LF_INVALID IS NOT INITIAL.
        EF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*       Error: Cost Element &1 does not exist.
        MESSAGE E027(ZSDSPS01) WITH <L_BUDGET_REQ>-POSID
                INTO EF_MESSAGE.
        EXIT.
      ENDIF.

*   Determine from Itmty/Actty if from SFDC
    ELSEIF IS_FROM_SFDC( LS_KEY-PSPID ) AND
           LS_WBSLV-STUFE GE 3.

      ASSIGN_KSTAR(
        EXPORTING
          IF_PRJTY     = LS_BUDGET-BUDGET-POSID+2(1)
          IF_ITMTY     = LS_BUDGET-BUDGET-POSID+13(2)
          IF_ACTTY     = LS_BUDGET-BUDGET-POSID+16(2)
        IMPORTING
          EF_REV_KSTAR = LF_REV_KSTAR
          EF_CST_KSTAR = LS_BUDGET-BUDGET-KSTAR ).

    ENDIF.

*   Error if no cost element
    IF LS_BUDGET-BUDGET-KSTAR IS INITIAL.
      EF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*     Error: Cannot determine cost element for WBS &1.
      MESSAGE E028(ZSDSPS01) WITH LS_BUDGET-BUDGET-POSID
              INTO EF_MESSAGE.
      EXIT.
    ENDIF.

    LS_BUDGET-BUDGET-GJAHR = <L_BUDGET_REQ>-GJAHR.

    IF <L_BUDGET_REQ>-MONAT IS NOT INITIAL.
      LS_BUDGET-BUDGET-MONAT = <L_BUDGET_REQ>-MONAT.
    ELSE.
      ASSIGN_MONAT(
        EXPORTING
          IF_PSPID = LS_KEY-PSPID
          IF_GJAHR = LS_BUDGET-BUDGET-GJAHR
        IMPORTING
          EF_MONAT = LS_BUDGET-BUDGET-MONAT ).
    ENDIF.

    LS_BUDGET-BUDGET-AMOUNT = <L_BUDGET_REQ>-CSTAM.

*   Check Error which continue the rest
    VALIDATE_CONT_ERROR(
      EXPORTING
        IS_BUDGET = LS_BUDGET
      IMPORTING
        ET_RETURN = LT_RETURN ).
    IF LT_RETURN IS NOT INITIAL.
      CLEAR LT_BUDGET.
      INSERT LS_BUDGET INTO TABLE LT_BUDGET.
*     Collect Continue Error
      INSERT VALUE #( KEY              = <L_BUDGET_DATA>-KEY
                      CREATE_ORG_VERSN = <L_BUDGET_DATA>-CREATE_ORG_VERSN
                      MODE             = <L_BUDGET_DATA>-MODE
                      BUDGET           = LT_BUDGET
                      RETURN           = LT_RETURN )
             INTO TABLE ET_CONT_ERROR.
      CONTINUE.
    ENDIF.

    INSERT LS_BUDGET INTO TABLE <L_BUDGET_DATA>-BUDGET.

*   Additional Entry for Revenue Amount
    CLEAR LS_BUDGET_REV.
    LS_BUDGET_REV-BUDGET-POSID  = LS_BUDGET-BUDGET-POSID.
    LS_BUDGET_REV-BUDGET-OBJNR  = LS_BUDGET-BUDGET-OBJNR.
    LS_BUDGET_REV-BUDGET-GJAHR  = LS_BUDGET-BUDGET-GJAHR.
    LS_BUDGET_REV-BUDGET-MONAT  = LS_BUDGET-BUDGET-MONAT.
    LS_BUDGET_REV-BUDGET-AMOUNT = <L_BUDGET_REQ>-REVAM.
    LS_BUDGET_REV-BUDGET-KSTAR  = LF_REV_KSTAR.

*   Error if no cost element
    IF LS_BUDGET_REV-BUDGET-KSTAR IS INITIAL AND
       <L_BUDGET_REQ>-REVAM IS NOT INITIAL.
      EF_STATUS = ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
*     Error: Cannot determine cost element for WBS &1.
      MESSAGE E028(ZSDSPS01) WITH LS_BUDGET-BUDGET-POSID
              INTO EF_MESSAGE.
      EXIT.
    ENDIF.

    IF LS_BUDGET_REV-BUDGET-KSTAR IS NOT INITIAL.
      INSERT LS_BUDGET_REV INTO TABLE <L_BUDGET_DATA>-BUDGET.
    ENDIF.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.
