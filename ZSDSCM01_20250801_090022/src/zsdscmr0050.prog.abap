*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMR0050
*  Creation Date      : 23.01.2025
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : 420000083
*  Description        : Account Assignment at item level
*  Purpose            : To update account assignment data at service
*                       order item level.
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCMR0050.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS,
  CRMS4D_SERV_H.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSCMS019.
TYPES:   AC_ASSIGN TYPE CRMT_AC_ASSIGN_COM,
       END OF TS_RESULT.
TYPES: TT_RESULT  TYPE STANDARD TABLE OF TS_RESULT.

TYPES: TT_OBJID_RANGE TYPE  RANGE OF CRMS4D_SERV_H-OBJECT_ID.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE      TYPE  CHAR1       VALUE 'X',
  GC_TCODE     TYPE  SY-TCODE    VALUE 'ZSDSCM009',

  GC_SVO       TYPE  CRMT_SUBOBJECT_CATEGORY_DB VALUE 'BUS2000116',
  GC_ORDER     TYPE  CRMT_AC_OBJECT_TYPE        VALUE '01',

  GC_LED_GREEN TYPE  ICON-ID VALUE '@5B@',
  GC_LED_RED   TYPE  ICON-ID VALUE '@5C@'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT TYPE  TT_RESULT                                   ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  GF_TIME  TYPE TZNTSTMPS                                     ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA:
  GR_OBJID  TYPE  TT_OBJID_RANGE                              ##NEEDED.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSCMS019'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 24,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 76.


*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF sy-batch IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = &1 ##NUMBER_OK
        text       = &2.
  ELSE.
*   Show message step in background
    MESSAGE i000(38) WITH &2 space space space.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01 NO INTERVALS.
  SELECT-OPTIONS:
    S_OBJID  FOR CRMS4D_SERV_H-OBJECT_ID OBLIGATORY.
  PARAMETERS:
    CB_TEST  TYPE  CHAR1 AS CHECKBOX DEFAULT 'X'.
  PARAMETERS:
    P_WAIT TYPE  ABAP_BOOLEAN NO-DISPLAY,
    P_TIME TYPE  TZNTSTMPS NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*  PERFORM F_GET_CONSTANTS.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT.
  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_RESULT.
* Update JOB Status if error found
  IF SY-BATCH EQ GC_TRUE.
    READ TABLE GT_RESULT TRANSPORTING NO FIELDS
                         WITH KEY MSGTY = 'E'.
    IF SY-SUBRC EQ 0.
*     Text-e02: Error found on processing.
      MESSAGE E000(ZSDSCM01) WITH TEXT-E02 SPACE SPACE SPACE.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection Screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN  ##NEEDED.
* Validate Selection Screen here....
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PROCESS_DATA
*----------------------------------------------------------------------*
*  Process Data
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LT_RESULT TYPE  TT_RESULT,
    LT_RETURN TYPE  BAPIRET2_TAB.

  DATA:
    LS_RESULT TYPE  TS_RESULT.

  DATA:
    LF_SUBRC  TYPE  SY-SUBRC.


* Initialize Output
  CLEAR: CT_RESULT.

* Up to 1 Minutes
  DO 60 TIMES ##NUMBER_OK.
*   Get Order List
    SELECT OBJTYPE_H,
           OBJECT_ID,
           HEADER_GUID,
           PROCESS_TYPE,
           CREATED_AT_H,
           CHANGED_AT_H
      FROM CRMS4D_SERV_H
     WHERE OBJTYPE_H EQ @GC_SVO
       AND OBJECT_ID IN @S_OBJID
       AND ( CREATED_AT_H GT @P_TIME OR
             CHANGED_AT_H GT @P_TIME )
      INTO TABLE @DATA(LT_SERV_H).
    IF SY-SUBRC NE 0.
      IF P_WAIT IS INITIAL.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  LOOP AT LT_SERV_H ASSIGNING FIELD-SYMBOL(<L_SERV_H>).

*   Only related Process type
    CHECK ZCL_SDSCM_ENHANCEMENT=>IS_ITEM_PROCESSING( IF_PROCESS_TYPE = <L_SERV_H>-PROCESS_TYPE ) EQ GC_TRUE.

    IF P_WAIT EQ GC_TRUE.

*     Wait until Order UnLock?
      PERFORM F_WAIT_ORDER  USING  <L_SERV_H>-HEADER_GUID
                          CHANGING LF_SUBRC.
      IF LF_SUBRC NE 0.
*       Error: Cannot lock order &1 for processing.
        MESSAGE I045(ZSDSCM01) DISPLAY LIKE 'E'
                               WITH <L_SERV_H>-OBJECT_ID.
        CONTINUE.
      ENDIF.

    ENDIF.

*   Get SVO data for processing
    PERFORM F_GET_SVO_DATA  USING  <L_SERV_H>-HEADER_GUID
                          CHANGING LT_RESULT.
    IF LT_RESULT IS INITIAL.
      CONTINUE.
    ENDIF.

*   Update Account Assignment data
    PERFORM F_UPDATE_AC_ASSIGN  USING  <L_SERV_H>-HEADER_GUID
                                       LT_RESULT
                                       CB_TEST
                              CHANGING LT_RETURN.

    CLEAR LS_RESULT.
*   Successfully
    IF LT_RETURN IS INITIAL.
      LS_RESULT-MSGTY = 'S'.
      LS_RESULT-STATU = GC_LED_GREEN.
      IF CB_TEST EQ GC_TRUE.
*       Text-i02: Test run successfully
        LS_RESULT-MSGTX = TEXT-I02.
      ELSE.
*       Text-i03: Account assignment data updated successfully.
        LS_RESULT-MSGTX = TEXT-I03.
      ENDIF.

*   Error occured
    ELSE.
      LS_RESULT-MSGTY = 'E'.
      LS_RESULT-STATU = GC_LED_RED.
      READ TABLE LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>)
                           INDEX 1.
      IF SY-SUBRC EQ 0.
        MESSAGE ID <L_RETURN>-ID TYPE <L_RETURN>-TYPE
                NUMBER <L_RETURN>-NUMBER
                WITH <L_RETURN>-MESSAGE_V1
                     <L_RETURN>-MESSAGE_V2
                     <L_RETURN>-MESSAGE_V3
                     <L_RETURN>-MESSAGE_V4
                INTO LS_RESULT-MSGTX.
      ENDIF.
      IF LS_RESULT-MSGTX IS INITIAL.
*       Text-e01: Error occurred during update account assignment data.
        LS_RESULT-MSGTX = TEXT-E01.
      ENDIF.
    ENDIF.

*   Update Result
    MODIFY LT_RESULT FROM LS_RESULT
                     TRANSPORTING MSGTY
                                  STATU
                                  MSGTX
                     WHERE MSGTY IS INITIAL.

    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_WAIT_ORDER
*----------------------------------------------------------------------*
*  Lock Order for processing
*----------------------------------------------------------------------*
FORM F_WAIT_ORDER  USING  UF_GUID TYPE CRMT_OBJECT_GUID
                 CHANGING CF_SUBRC TYPE SY-SUBRC.

* Initialize Output
  CF_SUBRC = 4.

* Wait up to 1 minute
  DO 60 TIMES ##NUMBER_OK.
    CALL FUNCTION 'ENQUEUE_E_CRM_ORDER'
      EXPORTING
        MODE_CRMD_ORDERADM_H = 'E'
        GUID                 = UF_GUID
      EXCEPTIONS
        FOREIGN_LOCK         = 1
        SYSTEM_FAILURE       = 2
        OTHERS               = 3.
    IF SY-SUBRC <> 0.
      WAIT UP TO 1 SECONDS.
    ELSE.
      CF_SUBRC = SY-SUBRC.
      CALL FUNCTION 'DEQUEUE_E_CRM_ORDER'
        EXPORTING
          MODE_CRMD_ORDERADM_H = 'E'
          GUID                 = UF_GUID.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_SVO_DATA
*----------------------------------------------------------------------*
*  Get Service Order Data
*----------------------------------------------------------------------*
FORM F_GET_SVO_DATA  USING  UF_GUID  TYPE  CRMT_OBJECT_GUID
                   CHANGING CT_RESULT TYPE TT_RESULT.

  CONSTANTS:
    LC_IO  TYPE  SWO_OBJTYP VALUE 'BUS2075'.

  DATA:
    LT_HEADER_GUID TYPE CRMT_OBJECT_GUID_TAB,
    LT_DOC_FLOW    TYPE CRMT_DOC_FLOW_WRKT,
    LT_AC_ASSIGN   TYPE CRMT_AC_ASSIGN_WRKT,
    LT_ORDERADM_H  TYPE CRMT_ORDERADM_H_WRKT,
    LT_ORDERADM_I  TYPE CRMT_ORDERADM_I_WRKT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.

  DATA:
    LF_AUFNR  TYPE  TS_RESULT-AUFNR.


* Initialize Output
  CLEAR: CT_RESULT.

* Initialize Variables
  CLEAR: LT_HEADER_GUID,
         LT_DOC_FLOW,
         LT_AC_ASSIGN,
         LT_ORDERADM_I.

* Assign GUID
  INSERT UF_GUID INTO TABLE LT_HEADER_GUID.

* Read SVO Data
  CALL FUNCTION 'CRM_ORDER_READ'
    EXPORTING
      IT_HEADER_GUID       = LT_HEADER_GUID
    IMPORTING
      ET_DOC_FLOW          = LT_DOC_FLOW
      ET_AC_ASSIGN         = LT_AC_ASSIGN
      ET_ORDERADM_H        = LT_ORDERADM_H
      ET_ORDERADM_I        = LT_ORDERADM_I
    EXCEPTIONS
      DOCUMENT_NOT_FOUND   = 1
      ERROR_OCCURRED       = 2
      DOCUMENT_LOCKED      = 3
      NO_CHANGE_AUTHORITY  = 4
      NO_DISPLAY_AUTHORITY = 5
      NO_CHANGE_ALLOWED    = 6
      OTHERS               = 7.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  READ TABLE LT_ORDERADM_H ASSIGNING FIELD-SYMBOL(<L_ORDERADM_H>)
                           WITH KEY GUID = UF_GUID.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* For all Item
  LOOP AT LT_ORDERADM_I ASSIGNING FIELD-SYMBOL(<L_ORDERADM_I>).

    CLEAR LS_RESULT.
    LS_RESULT-OBJECT_ID       = <L_ORDERADM_H>-OBJECT_ID.
    LS_RESULT-NUMBER_INT      = <L_ORDERADM_I>-NUMBER_INT.

*   Read current Acc Assign Data
    READ TABLE LT_AC_ASSIGN ASSIGNING FIELD-SYMBOL(<L_AC_ASSIGN>) "#EC CI_SORTSEQ
                            WITH KEY REF_GUID = <L_ORDERADM_I>-GUID
                                     REF_KIND = 'B'.
    IF SY-SUBRC EQ 0.
      LS_RESULT-AC_OBJECT_TYPE = <L_AC_ASSIGN>-AC_OBJECT_TYPE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = <L_AC_ASSIGN>-AC_ASSIGNMENT
        IMPORTING
          OUTPUT = LF_AUFNR.
      LS_RESULT-AC_ASSIGNMENT  = LF_AUFNR.
    ELSE.
      UNASSIGN <L_AC_ASSIGN>.
    ENDIF.

*   Get Reference IO
    READ TABLE LT_DOC_FLOW ASSIGNING FIELD-SYMBOL(<L_DOC_FLOW>)
                           WITH KEY OBJ_B COMPONENTS
                                    OBJKEY_B  = <L_ORDERADM_I>-NUMBER_INT  ##WARN_OK
                                    OBJTYPE_B = LC_IO
                                    OBJTYPE_A = <L_ORDERADM_I>-OBJECT_TYPE.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

*   Read IO Number from relation ID
    READ TABLE LT_DOC_FLOW ASSIGNING FIELD-SYMBOL(<L_DOC_FLOW_H>)
                           WITH KEY RELATIONID = <L_DOC_FLOW>-RELATIONID
                                    OBJTYPE_A  = <L_ORDERADM_H>-OBJECT_TYPE
                                    OBJTYPE_B  = LC_IO.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    LS_RESULT-AUFNR = <L_DOC_FLOW_H>-OBJKEY_B.

*   Acc Assign Already Sync..
    IF LS_RESULT-AUFNR EQ LS_RESULT-AC_ASSIGNMENT AND
      LS_RESULT-AC_OBJECT_TYPE EQ GC_ORDER.
      LS_RESULT-STATU = GC_LED_GREEN.
      LS_RESULT-MSGTY = 'S'.
*     Text-i01: Account Assignment was already synchnorized.
      LS_RESULT-MSGTX = TEXT-I01.
    ELSE.
*     Prepare Ac Assign Data
      LS_RESULT-AC_ASSIGN-REF_GUID = <L_ORDERADM_I>-GUID.
      LS_RESULT-AC_ASSIGN-REF_KIND = 'B'.
      LS_RESULT-AC_ASSIGN-AC_OBJECT_TYPE = GC_ORDER.
      LS_RESULT-AC_ASSIGN-AC_ASSIGNMENT  = LS_RESULT-AUFNR.
    ENDIF.


    INSERT LS_RESULT INTO TABLE CT_RESULT.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_UPDATE_AC_ASSIGN
*----------------------------------------------------------------------*
*  Update Account Assignment data
*----------------------------------------------------------------------*
FORM F_UPDATE_AC_ASSIGN  USING  UF_GUID   TYPE  CRMT_OBJECT_GUID
                                UT_RESULT TYPE TT_RESULT
                                UF_TEST   TYPE  CHAR1
                       CHANGING CT_RETURN TYPE BAPIRET2_TAB.

  DATA:
    LT_AC_ASSIGN    TYPE  CRMT_AC_ASSIGN_COMT,
    LT_EXCEPTION    TYPE  CRMT_EXCEPTION_T,
    LT_INPUT_FIELDS TYPE  CRMT_INPUT_FIELD_TAB,
    LT_FIELD        TYPE  CRMT_INPUT_FIELD_NAMES_TAB,
    LT_GUID         TYPE  CRMT_OBJECT_GUID_TAB,
    LT_SAVED        TYPE  CRMT_RETURN_OBJECTS.


* Initialize Output
  CLEAR CT_RETURN.

* Prepare Nametab
  INSERT VALUE #( FIELDNAME = 'AC_OBJECT_TYPE' )
         INTO TABLE LT_FIELD.
  INSERT VALUE #( FIELDNAME = 'AC_ASSIGNMENT' )
         INTO TABLE LT_FIELD.

  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                    WHERE STATU IS INITIAL.
    INSERT <L_RESULT>-AC_ASSIGN INTO TABLE LT_AC_ASSIGN.

*   Assign Input
    INSERT VALUE #( REF_KIND      = <L_RESULT>-AC_ASSIGN-REF_KIND
                    REF_GUID      = <L_RESULT>-AC_ASSIGN-REF_GUID
                    OBJECTNAME    = 'AC_ASSIGN'
                    FIELD_NAMES   = LT_FIELD )
           INTO TABLE LT_INPUT_FIELDS.

  ENDLOOP.

  CALL FUNCTION 'CRM_ORDER_INITIALIZE'
    EXPORTING
      IV_INITIALIZE_WHOLE_BUFFER = GC_TRUE
    EXCEPTIONS
      ERROR_OCCURRED             = 1
      OTHERS                     = 2.
  IF SY-SUBRC <> 0 ##NEEDED.
    "Do nothing
  ENDIF.

* Maintain Ac Assign in Items
  CALL FUNCTION 'CRM_ORDER_MAINTAIN'
    EXPORTING
      IT_AC_ASSIGN      = LT_AC_ASSIGN
    IMPORTING
      ET_EXCEPTION      = LT_EXCEPTION
    CHANGING
      CT_INPUT_FIELDS   = LT_INPUT_FIELDS
    EXCEPTIONS
      ERROR_OCCURRED    = 1
      DOCUMENT_LOCKED   = 2
      NO_CHANGE_ALLOWED = 3
      NO_AUTHORITY      = 4
      OTHERS            = 5.
  IF SY-SUBRC <> 0 OR
     LT_EXCEPTION IS NOT INITIAL.
    IF LT_EXCEPTION IS INITIAL.
      INSERT VALUE #( TYPE       = SY-MSGTY
                      ID         = SY-MSGID
                      NUMBER     = SY-MSGNO
                      MESSAGE_V1 = SY-MSGV1
                      MESSAGE_V2 = SY-MSGV2
                      MESSAGE_V3 = SY-MSGV3
                      MESSAGE_V4 = SY-MSGV4 )
              INTO TABLE CT_RETURN.
    ELSE.
      PERFORM F_COLLECT_EXCEPTION  USING  LT_EXCEPTION
                               CHANGING CT_RETURN.
    ENDIF.
    RETURN.
  ENDIF.

* Finish testing
  IF UF_TEST EQ GC_TRUE.
    RETURN.
  ENDIF.

* Save Document
  INSERT UF_GUID INTO TABLE LT_GUID.
  CALL FUNCTION 'CRM_ORDER_SAVE'
    EXPORTING
      IT_OBJECTS_TO_SAVE   = LT_GUID
      IV_UPDATE_TASK_LOCAL = 'X'
*     IV_NO_BDOC_SEND      = 'X'
    IMPORTING
      ET_SAVED_OBJECTS     = LT_SAVED
      ET_EXCEPTION         = LT_EXCEPTION
    EXCEPTIONS
      DOCUMENT_NOT_SAVED   = 1
      OTHERS               = 2.
  IF SY-SUBRC <> 0 OR
     LT_EXCEPTION IS NOT INITIAL.
    IF LT_EXCEPTION IS INITIAL.
      INSERT VALUE #( TYPE       = SY-MSGTY
                      ID         = SY-MSGID
                      NUMBER     = SY-MSGNO
                      MESSAGE_V1 = SY-MSGV1
                      MESSAGE_V2 = SY-MSGV2
                      MESSAGE_V3 = SY-MSGV3
                      MESSAGE_V4 = SY-MSGV4 )
              INTO TABLE CT_RETURN.
    ELSE.
      PERFORM F_COLLECT_EXCEPTION  USING  LT_EXCEPTION
                               CHANGING CT_RETURN.
    ENDIF.
    RETURN.
  ENDIF.

* Return Object ID
  READ TABLE LT_SAVED ASSIGNING FIELD-SYMBOL(<L_SAVED>)
                      WITH KEY GUID = UF_GUID.
  IF SY-SUBRC NE 0.
*   Error: Error during processing GUID &1.
    INSERT VALUE #( TYPE       = 'E'
                    ID         = 'ZSDSCM01'
                    NUMBER     = '006'
                    MESSAGE_V1 = UF_GUID )
            INTO TABLE CT_RETURN.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RETURN.
  ENDIF.

* Commit Work
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = SPACE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LF_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'OBJECT_ID'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
*       Text-c00 : Service Order
        LF_TEXT                = TEXT-C00.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'NUMBER_INT'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'AC_OBJECT_TYPE'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'AC_ASSIGNMENT'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'AUFNR'.
      WHEN 'STATU'.
*       Text-c01 : Status
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = 6.
        <L_FIELDCAT>-JUST      = 'C'.
        <L_FIELDCAT>-ICON      = GC_TRUE.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 50 ##NUMBER_OK.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'OBJECT_ID',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'NUMBER_INT'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

* Sort by Order number
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by Order Item number
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_COLLECT_EXCEPTION
*----------------------------------------------------------------------*
*  Collect Exception message
*----------------------------------------------------------------------*
FORM F_COLLECT_EXCEPTION  USING  UT_EXCEPTION TYPE CRMT_EXCEPTION_T
                        CHANGING CT_RETURN TYPE BAPIRET2_TAB.

  DATA:
    LS_MSG         TYPE BAL_S_MSG.


* Initialize Output
  CLEAR CT_RETURN.

* Get Exception Message(s)
  LOOP AT UT_EXCEPTION ASSIGNING FIELD-SYMBOL(<L_EXCEPTION>).
    CLEAR  LS_MSG.
    CALL FUNCTION 'CRM_MESSAGES_GET_MSG_INFO'
      EXPORTING
        IS_MSG_HANDLE           = <L_EXCEPTION>-MSG_HANDLE
      IMPORTING
        ES_MSG                  = LS_MSG
      EXCEPTIONS
        NOT_FOUND               = 1
        WRONG_CONTEXT_STRUCTURE = 2
        DATA_ERROR              = 3
        OTHERS                  = 4.
    IF SY-SUBRC NE 0.
      CLEAR LS_MSG.
    ENDIF.

    INSERT VALUE #( TYPE       = LS_MSG-MSGTY
                    ID         = LS_MSG-MSGID
                    NUMBER     = LS_MSG-MSGNO
                    MESSAGE_V1 = LS_MSG-MSGV1
                    MESSAGE_V2 = LS_MSG-MSGV2
                    MESSAGE_V3 = LS_MSG-MSGV3
                    MESSAGE_V4 = LS_MSG-MSGV4 )
            INTO TABLE CT_RETURN.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CALL_UPD_AC_ASSIGN
*----------------------------------------------------------------------*
*  Trigger job to update Account assignment data
*----------------------------------------------------------------------*
FORM F_CALL_UPD_AC_ASSIGN  USING  UF_OBJECT_ID TYPE CRMT_OBJECT_ID_DB.

* Collect Object ID to update account assignment
  INSERT VALUE #( SIGN = 'I'
                  OPTION = 'EQ'
                  LOW    = UF_OBJECT_ID )
         INTO TABLE GR_OBJID.

* Get Current Change Timestamp
  SELECT SINGLE CHANGED_AT_H
    FROM CRMS4D_SERV_H
   WHERE OBJTYPE_H EQ @GC_SVO
     AND OBJECT_ID EQ @UF_OBJECT_ID
    INTO @GF_TIME.
  IF SY-SUBRC NE 0.
    CLEAR GF_TIME.
  ENDIF.

* Create Background Job to update Account assignment
  PERFORM F_CREATE_BG_JOB ON COMMIT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CREATE_BG_JOB
*----------------------------------------------------------------------*
*  Create Background job to execute the program
*----------------------------------------------------------------------*
FORM F_CREATE_BG_JOB.

  DATA:
    LR_OBJID  TYPE  TT_OBJID_RANGE.

  DATA:
    LF_JOBNAME  TYPE  TBTCJOB-JOBNAME,
    LF_JOBCOUNT TYPE  TBTCJOB-JOBCOUNT.


  SORT GR_OBJID BY SIGN OPTION LOW HIGH.
  DELETE ADJACENT DUPLICATES FROM GR_OBJID COMPARING ALL FIELDS.

* Only when Criteria exist
  IF GR_OBJID IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT GR_OBJID ASSIGNING FIELD-SYMBOL(<L_OBJID>).

    CLEAR LR_OBJID.

*   Assign Job Name
    LF_JOBNAME = |ZSDSCM_UPD_ACASSIGN_{ <L_OBJID>-LOW }|.

    INSERT <L_OBJID> INTO TABLE LR_OBJID.

*   Create Job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LF_JOBNAME
      IMPORTING
        JOBCOUNT         = LF_JOBCOUNT
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

*   Submit Program
    SUBMIT ZSDSCMR0050 AND RETURN                        "#EC CI_SUBMIT
                       WITH S_OBJID IN LR_OBJID
                       WITH CB_TEST EQ SPACE
                       WITH P_WAIT EQ GC_TRUE
                       WITH P_TIME EQ GF_TIME
                       VIA JOB LF_JOBNAME
                       NUMBER LF_JOBCOUNT.

*   Close Job
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LF_JOBCOUNT
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
        INVALID_TARGET       = 8
        INVALID_TIME_ZONE    = 9
        OTHERS               = 10.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  CLEAR GR_OBJID[].

ENDFORM.
