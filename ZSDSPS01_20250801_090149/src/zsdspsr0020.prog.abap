*-----------------------------------------------------------------------
*  Program ID         : ZSDSPSR0020
*  Creation Date      : 21.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : PSI010
*  Description        : Interface Outbound to send System Status CLSD
*                       to Salesforce for Project and WBS
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*  22.07.2025  F36K921650 BoonpipopCh. ITR25070289 Send status TECO REL
*-----------------------------------------------------------------------
REPORT ZSDSPSR0020.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS,
        PROJ,
        PRPS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSPSS010.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                      WITH DEFAULT KEY.

TYPES: TS_PROJ_DEF_RANGE TYPE PSPID_RANG.
TYPES: TT_PROJ_DEF_RANGE TYPE STANDARD TABLE OF TS_PROJ_DEF_RANGE.

TYPES: BEGIN OF TS_WBS_LEVEL_RANGE,
         SIGN   TYPE SIGN_RANGE,
         OPTION TYPE OPT_RANGE,
         LOW    TYPE PRPS-STUFE,
         HIGH   TYPE PRPS-STUFE,
       END OF TS_WBS_LEVEL_RANGE.
TYPES: TT_WBS_LEVEL_RANGE TYPE STANDARD TABLE OF TS_WBS_LEVEL_RANGE.

TYPES: BEGIN OF TS_STATUS_CLSD_RANGE,
         SIGN   TYPE SIGN_RANGE,
         OPTION TYPE OPT_RANGE,
         LOW    TYPE TJ02-ISTAT,
         HIGH   TYPE TJ02-ISTAT,
       END OF TS_STATUS_CLSD_RANGE.
TYPES: TT_STATUS_CLSD_RANGE TYPE STANDARD TABLE OF TS_STATUS_CLSD_RANGE,
       TT_STATUS_REL_RANGE  TYPE STANDARD TABLE OF TS_STATUS_CLSD_RANGE,
       TT_STATUS_TECO_RANGE TYPE STANDARD TABLE OF TS_STATUS_CLSD_RANGE.

TYPES: BEGIN OF TS_INTF_LOG,
         INTFNO      TYPE ZSDSCAT001-INTFNO,
         REQNO       TYPE ZSDSCAT001-REQNO,
         GJAHR       TYPE ZSDSCAT001-GJAHR,
         HTTP_CODE   TYPE ZSDSCAT001-HTTP_CODE,
         HTTP_REASON TYPE ZSDSCAT001-HTTP_REASON,
       END OF TS_INTF_LOG.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSPS002'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT   TYPE TT_RESULT                                  ##NEEDED,
  GS_INTF_LOG TYPE TS_INTF_LOG                                ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA:
  GRT_PROJ_DEF        TYPE TT_PROJ_DEF_RANGE                    ##NEEDED,
  GRT_WBS_LEVEL       TYPE TT_WBS_LEVEL_RANGE                   ##NEEDED,
  GRT_STATUS_CLSD     TYPE TT_STATUS_CLSD_RANGE                 ##NEEDED,
  GRT_STATUS_TECO     TYPE TT_STATUS_TECO_RANGE                 ##NEEDED,
  GRT_STATUS_REL      TYPE TT_STATUS_REL_RANGE                  ##NEEDED,
  GF_STATUS_FLAG_CLSD TYPE CHAR1                                ##NEEDED,
  GF_STATUS_FLAG_REL  TYPE CHAR1                                ##NEEDED,
  GF_STATUS_FLAG_TECO TYPE CHAR1                                ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1 TYPE TABNAME  VALUE 'ZSDSPSS010',
  GC_INTFNO      TYPE ZSDSDE_INTFNO VALUE 'PSI010'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE I VALUE 20,
  GC_ALV_HEIGHT_1    TYPE I VALUE 80.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS: S_PROJ FOR PROJ-PSPID MATCHCODE OBJECT PD,
                  S_WBS FOR PRPS-POSID MODIF ID WBS.
SELECTION-SCREEN END OF BLOCK B1.

* Text-s02: Run Mode
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
  PARAMETERS: CB_TEST  TYPE CHAR1 AS CHECKBOX DEFAULT 'X'.
  PARAMETERS: CB_SYST  TYPE TJ02T-TXT04.
SELECTION-SCREEN END OF BLOCK B2.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_CONSTANTS.

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
* Processing Data
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

  IF CB_TEST EQ SPACE.
    PERFORM F_CALL_INTERFACE CHANGING GT_RESULT
                                      GS_INTF_LOG.
  ENDIF.

* Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_RESULT.

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
*---------------------------------------------------------------------*
* Form F_GET_CONSTANTS
*---------------------------------------------------------------------*
*  Get GenC Constants
*---------------------------------------------------------------------*
FORM F_GET_CONSTANTS .
  CONSTANTS:
    LC_PROJECT_DEFINITION   TYPE ZSDSDE_PARAM_NAME VALUE 'PROJECT_DEFINITION',
    LC_WBS_LEVEL            TYPE ZSDSDE_PARAM_NAME VALUE 'WBS_LEVEL',
    LC_STATUS_INTERNAL_CLSD TYPE ZSDSDE_PARAM_NAME VALUE 'STATUS_INTERNAL_CLSD',
    LC_STATUS_INTERNAL_REL  TYPE ZSDSDE_PARAM_NAME VALUE 'STATUS_INTERNAL_REL',
    LC_STATUS_INTERNAL_TECO TYPE ZSDSDE_PARAM_NAME VALUE 'STATUS_INTERNAL_TECO',
    LC_STATUS_FLAG_CLSD     TYPE ZSDSDE_PARAM_NAME VALUE 'STATUS_FLAG_CLSD',
    LC_STATUS_FLAG_REL      TYPE ZSDSDE_PARAM_NAME VALUE 'STATUS_FLAG_REL',
    LC_STATUS_FLAG_TECO     TYPE ZSDSDE_PARAM_NAME VALUE 'STATUS_FLAG_TECO'.

  STATICS:
    LF_READ TYPE FLAG.

  DATA:
    LT_GENC TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID TYPE PROGRAMM.

* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GRT_PROJ_DEF ,
         GRT_WBS_LEVEL,
         GRT_STATUS_CLSD,
         GF_STATUS_FLAG_CLSD,
         GF_STATUS_FLAG_REL,
         GF_STATUS_FLAG_TECO.

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = GC_TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).
    CASE <L_GENC>-PARAM.

*     Project definition
      WHEN LC_PROJECT_DEFINITION.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_PROJ_DEF.

*     WBS Level in scope of processing
      WHEN LC_WBS_LEVEL.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_WBS_LEVEL.

*     Status internal number for system status = CLSD
      WHEN LC_STATUS_INTERNAL_CLSD.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_STATUS_CLSD.

*     Status internal number for system status = TECO
      WHEN LC_STATUS_INTERNAL_TECO.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_STATUS_TECO.

*     Status internal number for system status = REL
      WHEN LC_STATUS_INTERNAL_REL.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_STATUS_REL.

*     Status flag in the interface
      WHEN LC_STATUS_FLAG_CLSD.
        GF_STATUS_FLAG_CLSD = <L_GENC>-VALUE_LOW.

      WHEN LC_STATUS_FLAG_REL.
        GF_STATUS_FLAG_REL = <L_GENC>-VALUE_LOW.

      WHEN LC_STATUS_FLAG_TECO.
        GF_STATUS_FLAG_TECO = <L_GENC>-VALUE_LOW.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

* Project or WBS need to be specified
  IF S_PROJ IS INITIAL AND
     S_WBS IS INITIAL.
*   Please specify Project Definition or WBS Element
    MESSAGE E000(ZSDSCA01) WITH TEXT-E01.
    RETURN.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_PROCESS_DATA
*---------------------------------------------------------------------*
* Retrieve Project / WBS Status according to creteria and
* Prepare JSON structure for calling interface ID PSI010
*---------------------------------------------------------------------*
FORM F_PROCESS_DATA  CHANGING CT_RESULT TYPE TT_RESULT.

  CLEAR: CT_RESULT.

* GenC varialbles need to be maintained
  IF GRT_PROJ_DEF IS INITIAL OR
     GRT_WBS_LEVEL IS INITIAL OR
     GRT_STATUS_CLSD IS INITIAL OR
*BOI ITR25070289
     GRT_STATUS_REL IS INITIAL OR
     GRT_STATUS_TECO IS INITIAL.
*EOI ITR25070289
    RETURN.
  ENDIF.

  PERFORM F_GET_PROJECT CHANGING CT_RESULT.
  PERFORM F_GET_WBS CHANGING CT_RESULT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_PROJECT
*---------------------------------------------------------------------*
* Get project definition, validation and prepare interface detail
* Conditions for sending interface to Salesforce
*   1. Project definition in GRT_PROJ_DEF
*   2. Project definition has system status = CLSD (I0046)
*---------------------------------------------------------------------*
FORM F_GET_PROJECT  CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LF_INTF_FLG    TYPE C,
*BOI ITR25070289
        LR_SYST        TYPE TT_STATUS_CLSD_RANGE,
        LF_STATUS_FLAG TYPE CHAR01.

  IF CB_SYST IS NOT INITIAL.
    CASE CB_SYST.
      WHEN 'REL'.
        MOVE-CORRESPONDING GRT_STATUS_REL[] TO LR_SYST[].
        MOVE GF_STATUS_FLAG_REL TO LF_STATUS_FLAG.
      WHEN 'CLSD'.
        MOVE-CORRESPONDING GRT_STATUS_CLSD[] TO LR_SYST[].
        MOVE GF_STATUS_FLAG_CLSD TO LF_STATUS_FLAG.
      WHEN 'TECO'.
        MOVE-CORRESPONDING GRT_STATUS_TECO[] TO LR_SYST[].
        MOVE GF_STATUS_FLAG_TECO TO LF_STATUS_FLAG.
      WHEN OTHERS.
    ENDCASE.
*    APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = CB_SYST ) TO LR_SYST.
  ENDIF.
*EOI ITR25070289

* Get Project Definition
  IF S_PROJ IS NOT INITIAL.
    SELECT PSPID,
           OBJNR
      FROM PROJ
      INTO TABLE @DATA(LT_PROJ)
      WHERE PSPID IN @S_PROJ
        AND PSPID IN @GRT_PROJ_DEF.

    IF SY-SUBRC EQ 0.
      LOOP AT LT_PROJ ASSIGNING FIELD-SYMBOL(<L_PROJ>).
*       Check system status in scope?
        PERFORM F_CHECK_OBJECT_STATUS_CLSD USING <L_PROJ>-OBJNR
*                                                 GRT_STATUS_CLSD   "DEL ITR25070289
                                                 LR_SYST
                                         CHANGING LF_INTF_FLG.
        IF LF_INTF_FLG EQ ABAP_TRUE.
          INSERT VALUE #( PSPID      = <L_PROJ>-PSPID
*                          CLSD_FLAG  = GF_STATUS_FLAG_CLSD )       "DEL ITR25070289
                          CLSD_FLAG  = LF_STATUS_FLAG )
               INTO TABLE CT_RESULT.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_WBS
*---------------------------------------------------------------------*
* Get WBS element, validation and prepare interface detail
* Conditions for sending interface to Salesforce
*   1. WBS is under project definition in GRT_PROJ_DEF
*   2. WBS has system status = CLSD (I0046)
*   3. WBS level in GRT_WBS_LEVEL
*---------------------------------------------------------------------*
FORM F_GET_WBS  CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LF_INTF_FLG    TYPE C,
*BOI ITR25070289
        LR_SYST        TYPE TT_STATUS_CLSD_RANGE,
        LF_STATUS_FLAG TYPE CHAR01.

  IF CB_SYST IS NOT INITIAL.
    CASE CB_SYST.
      WHEN 'REL'.
        MOVE-CORRESPONDING GRT_STATUS_REL[] TO LR_SYST[].
        MOVE GF_STATUS_FLAG_REL TO LF_STATUS_FLAG.
      WHEN 'CLSD'.
        MOVE-CORRESPONDING GRT_STATUS_CLSD[] TO LR_SYST[].
        MOVE GF_STATUS_FLAG_CLSD TO LF_STATUS_FLAG.
      WHEN 'TECO'.
        MOVE-CORRESPONDING GRT_STATUS_TECO[] TO LR_SYST[].
        MOVE GF_STATUS_FLAG_TECO TO LF_STATUS_FLAG.
      WHEN OTHERS.
    ENDCASE.
*    APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = CB_SYST ) TO LR_SYST.
  ENDIF.
*EOI ITR25070289

  IF S_WBS IS NOT INITIAL.
*   Get WBS element
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
      WHERE WBS~POSID IN @S_WBS
        AND WBS~STUFE IN @GRT_WBS_LEVEL
        AND PROJ~PSPID IN @GRT_PROJ_DEF
        AND PROJ~PSPID IN @S_PROJ
        ORDER BY WBS~PSPNR.

    IF SY-SUBRC EQ 0.
      LOOP AT LT_PRPS ASSIGNING FIELD-SYMBOL(<L_WBS>).
*     Check system status in scope?
        PERFORM F_CHECK_OBJECT_STATUS_CLSD USING <L_WBS>-OBJNR
*                                                 GRT_STATUS_CLSD       "DEL ITR25070289
                                                 LR_SYST                "ADD ITR25070289
                                        CHANGING LF_INTF_FLG.
        IF LF_INTF_FLG EQ ABAP_TRUE.
          INSERT VALUE #( PSPID      = <L_WBS>-PSPID
                          POSID      = <L_WBS>-POSID
*                          CLSD_FLAG  = GF_STATUS_FLAG_CLSD )           "DEL ITR25070289
                          CLSD_FLAG  = LF_STATUS_FLAG )                 "ADD ITR25070289
               INTO TABLE CT_RESULT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CHECK_OBJECT_STATUS_CLSD
*---------------------------------------------------------------------*
* Check object status = CLSD (I0046)
* Maintain in GENC parameter: STATUS_INTERNAL_CLSD
*---------------------------------------------------------------------*
FORM F_CHECK_OBJECT_STATUS_CLSD  USING    UF_OBJNR TYPE JSTO-OBJNR
                                          UT_STATUS_CLSD TYPE TT_STATUS_CLSD_RANGE
                                 CHANGING CF_INTF_FLG TYPE CHAR1.
  DATA: LT_STATUS TYPE STANDARD TABLE OF JSTAT.
  CLEAR: CF_INTF_FLG.

  IF UF_OBJNR IS INITIAL OR
     UT_STATUS_CLSD IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      OBJNR            = UF_OBJNR
      ONLY_ACTIVE      = 'X'
    TABLES
      STATUS           = LT_STATUS
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.

  IF SY-SUBRC EQ 0.
    LOOP AT LT_STATUS ASSIGNING FIELD-SYMBOL(<L_STATUS>)
                      WHERE STAT IN UT_STATUS_CLSD.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC EQ 0.
      CF_INTF_FLG = ABAP_TRUE.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CALL_INTERFACE
*---------------------------------------------------------------------*
* Call interface
*---------------------------------------------------------------------*
FORM F_CALL_INTERFACE  CHANGING CT_RESULT TYPE TT_RESULT
                                CS_INTF_LOG TYPE TS_INTF_LOG.

  DATA: LS_REQUEST        TYPE ZSDSPSS011,
        LS_RESPONSE       TYPE ZSDSPSS011,
        LS_REQUEST_DETAIL TYPE ZSDSPSS010,
        LT_REQUEST_DETAIL TYPE ZSDSPSS010_TT,
        LT_RESPONSE       TYPE ZSDSPSS010_TT,
        LS_REQ_KEY        TYPE ZSDSCAS005.

  DATA: LV_GROUP_NO       TYPE I.
  FIELD-SYMBOLS: <L_RESULT> TYPE TS_RESULT.

  CLEAR: CS_INTF_LOG.

  IF CT_RESULT IS INITIAL.
    RETURN.
  ENDIF.

  SORT CT_RESULT BY PSPID POSID.

  LOOP AT CT_RESULT ASSIGNING <L_RESULT>.
    CLEAR: LS_REQUEST_DETAIL.
    MOVE-CORRESPONDING <L_RESULT> TO LS_REQUEST_DETAIL.
    INSERT LS_REQUEST_DETAIL INTO TABLE LT_REQUEST_DETAIL.
  ENDLOOP.

  LS_REQUEST-OBJECT = LT_REQUEST_DETAIL.

* Call interface id PSI010
  ZCL_SDSCA_REST_INTF_UTILITY=>CALL_REST_INTF(
    EXPORTING
      IF_INTFNO           = GC_INTFNO
      IS_REQUEST          = LS_REQUEST
      IF_SHOW_PROCESS_LOG = ' '
    IMPORTING
      ES_RESPONSE         = LS_RESPONSE
      ES_REQUEST_KEY      = LS_REQ_KEY
    EXCEPTIONS
      INVALID_INTFNO      = 1
      ERROR_DATA_TO_JSON  = 2
      URL_ERROR           = 3
      SEND_ERROR          = 4
      REQNO_ERROR         = 5
      LOG_ERROR           = 6
      ERROR_JSON_TO_DATA  = 7
      OTHERS              = 8 ).
  IF SY-SUBRC <> 0.
*   Error
    MESSAGE ID SY-MSGID TYPE SY-MSGTY
            NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2
                 SY-MSGV3 SY-MSGV4.
    RETURN.
  ELSE.
*   Read inteface log from ZSDSCAT001
    PERFORM F_GET_INTF_LOG USING LS_REQ_KEY
                           CHANGING CS_INTF_LOG.

*   Read response and update to GT_RESULT
    LT_RESPONSE[] = LS_RESPONSE-OBJECT.
    LOOP AT LT_RESPONSE ASSIGNING FIELD-SYMBOL(<L_RESPONSE>).
      READ TABLE CT_RESULT ASSIGNING <L_RESULT>
                           WITH KEY PSPID = <L_RESPONSE>-PSPID
                                    POSID = <L_RESPONSE>-POSID.
      IF SY-SUBRC EQ 0.
        <L_RESULT>-RESP_STATUS = <L_RESPONSE>-RESP_STATUS.
        <L_RESULT>-RESP_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.


* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.

  IF CB_TEST EQ ABAP_TRUE.
* Disable header area when test run
    GF_ALV_HEADER_1 = SPACE.
  ELSE.
* Display Header area
    GF_ALV_HEADER_1 = ABAP_TRUE.
  ENDIF.

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
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

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
  CLEAR: CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'A'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = SPACE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'PSPID',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'POSID'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.

* Initialize Output
  CLEAR: CT_SORT.

* Sort by PSPID
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by POSID
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_INTF_LOG
*---------------------------------------------------------------------*
* Get interface log from ZSDSCAT001
*---------------------------------------------------------------------*
FORM F_GET_INTF_LOG  USING    US_REQ_KEY TYPE ZSDSCAS005
                     CHANGING CS_INTF_LOG TYPE TS_INTF_LOG.

  CLEAR: CS_INTF_LOG.
  IF US_REQ_KEY IS INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE INTFNO,
                REQNO,
                GJAHR,
                HTTP_CODE,
                HTTP_REASON
    FROM ZSDSCAT001
    INTO @CS_INTF_LOG
    WHERE INTFNO EQ @US_REQ_KEY-INTFNO
      AND REQNO EQ @US_REQ_KEY-REQNO
      AND GJAHR EQ @US_REQ_KEY-GJAHR.

  IF SY-SUBRC NE 0.
    CLEAR: CS_INTF_LOG.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '25%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '75%'.

  DATA:
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

  IF CB_TEST EQ ABAP_TRUE.
    RETURN.
  ENDIF.

* Create table
  CALL METHOD UREF_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LREF_TABLE.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LREF_COL_KEY.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LREF_COL_VAL.
* Set Key column style
  CALL METHOD LREF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*-----------------------
* Add value in Line1 : Interface ID
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-001 : Inteface ID
  LF_TEXT = TEXT-001.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

  LF_TEXT = GS_INTF_LOG-INTFNO.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2 : Request number
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-001 : Request Number
  LF_TEXT = TEXT-002.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

  LF_TEXT = GS_INTF_LOG-REQNO.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3 : Year
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-002 : Year
  LF_TEXT = TEXT-003.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

  LF_TEXT = GS_INTF_LOG-GJAHR.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4 : HTTP Code
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-003 : HTTP Code
  LF_TEXT = TEXT-004.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

  LF_TEXT = GS_INTF_LOG-HTTP_CODE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line5 : HTTP reason
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-004 : HTTP Reason
  LF_TEXT = TEXT-005.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

  LF_TEXT = GS_INTF_LOG-HTTP_REASON.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LF_COL01 TYPE  I VALUE 18,
    LF_COL02 TYPE  I VALUE 35.

  IF CB_TEST EQ ABAP_TRUE.
    RETURN.
  ENDIF.

* Line 1: Interface ID
  WRITE AT: /1(LF_COL01)  TEXT-001 INTENSIFIED ON NO-GAP,
              (LF_COL02)  GS_INTF_LOG-INTFNO NO-GAP.

* Line 2: Request No
  WRITE AT: /1(LF_COL01)  TEXT-002 INTENSIFIED ON NO-GAP,
              (LF_COL02)  GS_INTF_LOG-REQNO NO-GAP.

* Line 3: Year
  WRITE AT: /1(LF_COL01)  TEXT-003 INTENSIFIED ON NO-GAP,
              (LF_COL02)  GS_INTF_LOG-GJAHR NO-GAP.

* Line 4: HTTP Code
  WRITE AT: /1(LF_COL01)  TEXT-004 INTENSIFIED ON NO-GAP,
              (LF_COL02)  GS_INTF_LOG-HTTP_CODE LEFT-JUSTIFIED NO-GAP.

* Line 5: HTTP Reason
  WRITE AT: /1(LF_COL01) TEXT-005 INTENSIFIED ON NO-GAP,
              (LF_COL02)  GS_INTF_LOG-HTTP_REASON LEFT-JUSTIFIED NO-GAP.

ENDFORM.
