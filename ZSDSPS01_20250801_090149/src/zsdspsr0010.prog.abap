*-----------------------------------------------------------------------
*  Program ID         : ZSDSPSR0010
*  Creation Date      : 24.04.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : PSI001
*  Description        : This is a program to upload project structure
*                       and budget data.
*  Purpose            : To upload project structure and budget
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSPSR0010.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TF_FNAME  TYPE  CHAR40.
TYPES: TF_MSGTX  TYPE  ZSDSDE_MSGTX.

TYPES: BEGIN OF TS_COUNT,
         TOTAL TYPE  I,
         SUCCS TYPE  I,
         ERROR TYPE  I,
       END OF TS_COUNT.

TYPES: BEGIN OF TS_PROC_INFO,
         DATUM TYPE  SY-DATUM,
         UZEIT TYPE  SY-UZEIT,
         UNAME TYPE  SY-UNAME,
         IFILE TYPE  STRING,
         MODE  TYPE  TEXT50,
         COUNT TYPE  TS_COUNT,
       END OF TS_PROC_INFO.

TYPES: BEGIN OF TS_MESSG,
         MSGTY TYPE  SY-MSGTY,
         MSGID TYPE  SY-MSGID,
         MSGNO TYPE  SY-MSGNO,
         MSGTX TYPE  TF_MSGTX,
       END OF TS_MESSG.

TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSPSS001.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_PROJ,
         PSPID TYPE PROJ-PSPID,
       END OF TS_PROJ.
TYPES: TT_PROJ TYPE SORTED TABLE OF TS_PROJ
                      WITH UNIQUE KEY PSPID.

TYPES: TS_XLSX TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA.
TYPES: TT_XLSX TYPE ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA.

TYPES: BEGIN OF TS_PROJDEF_DATA,
         SHEET    TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA-WORKSHEET_NAME,
         ROWNO    TYPE TS_RESULT-ROWNO,
         PROJDEF  TYPE ZCL_SDSPS_PROJECT=>TS_PROJECTDEF,
         PROJDEFX TYPE ZCL_SDSPS_PROJECT=>TS_PROJECTDEFX,
       END OF TS_PROJDEF_DATA.
TYPES: TT_PROJDEF_DATA TYPE STANDARD TABLE OF TS_PROJDEF_DATA.

TYPES: BEGIN OF TS_WBS_DATA,
         SHEET TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA-WORKSHEET_NAME,
         ROWNO TYPE TS_RESULT-ROWNO,
         PSPID TYPE PROJ-PSPID,
         WBS   TYPE ZCL_SDSPS_PROJECT=>TS_WBSELEM,
         WBSX  TYPE ZCL_SDSPS_PROJECT=>TS_WBSELEMX,
         WBSLV TYPE ZCL_SDSPS_PROJECT=>TS_WBSLV,
       END OF TS_WBS_DATA.
TYPES: TT_WBS_DATA TYPE STANDARD TABLE OF TS_WBS_DATA.

TYPES: BEGIN OF TS_BUDGET_DETAIL,
         SHEET  TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA-WORKSHEET_NAME,
         ROWNO  TYPE TS_RESULT-ROWNO,
         BUDGET TYPE  ZCL_SDSPS_PROJECT=>TS_BUDGET,
         WBSLV  TYPE ZCL_SDSPS_PROJECT=>TS_WBSLV,
       END OF TS_BUDGET_DETAIL.
TYPES: TT_BUDGET_DETAIL TYPE STANDARD TABLE OF TS_BUDGET_DETAIL
                              WITH DEFAULT KEY.
TYPES: BEGIN OF TS_BUDGET_DATA,
         KEY        TYPE ZCL_SDSPS_PROJECT=>TS_BUDGET_KEY,
         MONAT_LOW  TYPE MONAT,
         MONAT_HIGH TYPE MONAT,
         LV2        TYPE FLAG,
         LV3        TYPE FLAG,
         DETAIL     TYPE TT_BUDGET_DETAIL,
       END OF TS_BUDGET_DATA.
TYPES: TT_BUDGET_DATA TYPE SORTED TABLE OF TS_BUDGET_DATA
                           WITH UNIQUE KEY KEY.

* *****************************
* Types for Excel Template
* !!!! Changing field name impact to validation logic !!!
* *****************************
TYPES: BEGIN OF TS_TEMPLATE1 ##NEEDED,
         PSPID TYPE PROJ-PSPID,
         POST1 TYPE PROJ-POST1,
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
       END OF TS_TEMPLATE1.

TYPES: BEGIN OF TS_TEMPLATE2 ##NEEDED,
         POSID TYPE PRPS-POSID,
         POST1 TYPE PRPS-POST1,
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
       END OF TS_TEMPLATE2.

TYPES: BEGIN OF TS_TEMPLATE3 ##NEEDED,
         POSID  TYPE  PRPS-POSID,
         KSTAR  TYPE  V_COSP_VIEW-KSTAR,
         VERSN  TYPE  V_COSP_VIEW-VERSN,
         GJAHR  TYPE  V_COSP_VIEW-GJAHR,
         MONAT  TYPE  MONAT,
         AMOUNT TYPE  WTGXXX,
       END OF TS_TEMPLATE3.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE          TYPE  CHAR1     VALUE 'X',
  GC_TCODE         TYPE  SY-TCODE  VALUE 'ZSDSPS001',

  GC_NOCHG         TYPE CHAR5      VALUE '-',
  GC_SHEET_PROJDEF TYPE STRING     VALUE 'Project Definition' ##NO_TEXT,
  GC_SHEET_WBS     TYPE STRING     VALUE 'WBS Element'        ##NO_TEXT.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT  TYPE  TT_RESULT                                   ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_PROC    TYPE  TS_PROC_INFO                                ##NEEDED.

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

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSPSS001'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 25,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 75.

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
* Text-s01: Program Options
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    RB_CREAT TYPE CHAR1 RADIOBUTTON GROUP G1 DEFAULT 'X',
    RB_UPDAT TYPE CHAR1 RADIOBUTTON GROUP G1,
    RB_BGCRE TYPE CHAR1 RADIOBUTTON GROUP G1,
    RB_BGUPD TYPE CHAR1 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B1.

* Text-s02: File Selection
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
  PARAMETERS:
    RB_LOCAL TYPE CHAR1 RADIOBUTTON GROUP G2 DEFAULT 'X'
                                    USER-COMMAND DMY,
    RB_APPSV TYPE CHAR1 RADIOBUTTON GROUP G2.
  PARAMETERS:
    P_IFILE TYPE  STRING LOWER CASE MODIF ID LOC,
    P_AFILE TYPE  STRING LOWER CASE MODIF ID ASV.
SELECTION-SCREEN END OF BLOCK B2.
* Text-s03: Processing Options
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.
  PARAMETERS:
    CB_TEST  TYPE CHAR1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*  PERFORM F_GET_CONSTANTS.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Set Selection Screen Format
  PERFORM F_SET_SELECTION_SCREEN_FORMAT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_IFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_IFILE.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Processing Data
  PERFORM F_PROCESS_DATA  USING  CB_TEST
                        CHANGING GT_RESULT
                                 GS_PROC.
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
*  Form F_SET_SELECTION_SCREEN_FORMAT
*----------------------------------------------------------------------*
*  Set Selection Screen Format
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .
  LOOP AT SCREEN.

    CASE SCREEN-GROUP1.
*     Local Fields
      WHEN 'LOC'.
        IF RB_LOCAL EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.

*     AppServer Fields
      WHEN 'ASV'.
        IF RB_APPSV EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_LIST_IFILE
*----------------------------------------------------------------------*
*  Popup for Input file selection
*----------------------------------------------------------------------*
FORM F_LIST_IFILE  CHANGING CF_FILENAME  TYPE  STRING.

  DATA:
    LT_FILE     TYPE  FILETABLE.

  DATA:
    LF_RC     TYPE  I,
    LF_ACTION TYPE  I.

  FIELD-SYMBOLS:
    <L_FILE>  TYPE  FILE_TABLE.


* List Local File
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      DEFAULT_EXTENSION       = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER             = 'Excel File(XLSX)|*.XLSX' ##NO_TEXT
      MULTISELECTION          = SPACE
    CHANGING
      FILE_TABLE              = LT_FILE
      RC                      = LF_RC
      USER_ACTION             = LF_ACTION
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ENDIF.

* Read Selected
  IF NOT ( LF_ACTION IS INITIAL AND
           LF_RC     EQ 1 ).
    RETURN.
  ENDIF.

  READ TABLE LT_FILE ASSIGNING <L_FILE>
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_FILENAME = <L_FILE>-FILENAME.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  IF RB_LOCAL EQ GC_TRUE.
    IF P_IFILE IS INITIAL.
      SET CURSOR FIELD 'P_IFILE'.
*     Error: Please enter a valid filename.
      MESSAGE E002(ZSDSCA01).
      RETURN.
    ENDIF.
  ELSE.
    IF P_AFILE IS INITIAL.
      SET CURSOR FIELD 'P_AFILE'.
*     Error: Please enter a valid filename.
      MESSAGE E002(ZSDSCA01).
      RETURN.
    ENDIF.
  ENDIF.

  IF SY-BATCH EQ GC_TRUE AND
     RB_LOCAL EQ GC_TRUE.
*   Error: Cannot process local file in background mode.
    MESSAGE E012(ZSDSCA01).
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PROCESS_DATA
*----------------------------------------------------------------------*
*  Processing data
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA  USING  UF_TEST   TYPE FLAG
                   CHANGING CT_RESULT TYPE TT_RESULT
                            CS_PROC   TYPE TS_PROC_INFO.

  DATA:
    LT_XLSX    TYPE  TT_XLSX,
    LT_PROJDEF TYPE  TT_PROJDEF_DATA,
    LT_WBS     TYPE  TT_WBS_DATA,
    LT_BUDGET  TYPE  TT_BUDGET_DATA,
    LT_RESULT  TYPE  TT_RESULT.

  DATA:
    LS_COUNT   TYPE  TS_COUNT.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_PROC.

* -----------------------------
* Read File into Text table
* -----------------------------
  PERFORM F_READ_FILE CHANGING LT_XLSX.
  IF LT_XLSX IS INITIAL.
    RETURN.
  ENDIF.

  CASE GC_TRUE.
*   -----------------------------
*   1. Project Structire
*   -----------------------------
    WHEN RB_CREAT OR RB_UPDAT.
*     1.1 Validate File to Input table
      PERFORM F_ASSIGN_PROJ_STRUC  USING  LT_XLSX
                                 CHANGING LT_PROJDEF
                                          LT_WBS
                                          LT_RESULT.
      IF LT_RESULT IS NOT INITIAL.
*       Collect Final Result
        INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
        CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LINES( LT_RESULT ).
      ENDIF.

*     1.2 Create Project Structure
      IF RB_CREAT EQ GC_TRUE.
        PERFORM F_CREATE_PROJ_STRUC  USING  LT_PROJDEF
                                            LT_WBS
                                            UF_TEST
                                   CHANGING LT_RESULT
                                            LS_COUNT.

*     1.3 Change Project Structure
      ELSE.
        PERFORM F_CHANGE_PROJ_STRUC  USING  LT_PROJDEF
                                            LT_WBS
                                            UF_TEST
                                   CHANGING LT_RESULT
                                            LS_COUNT.

      ENDIF.
*     Collect Final Result
      INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
      CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LS_COUNT-ERROR.
      CS_PROC-COUNT-SUCCS = CS_PROC-COUNT-SUCCS + LS_COUNT-SUCCS.

*   -----------------------------
*   2. Project Planning and Budgeting
*   -----------------------------
    WHEN RB_BGCRE OR RB_BGUPD.
*     2.1 Validate File to Input table
      PERFORM F_ASSIGN_BUDGET_DATA  USING  LT_XLSX
                                  CHANGING LT_BUDGET
                                           LT_RESULT.
      IF LT_RESULT IS NOT INITIAL.
*       Collect Final Result
        INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
        CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LINES( LT_RESULT ).
      ENDIF.

*     2.2 Create Project Plan/Budget
      IF RB_BGCRE EQ GC_TRUE.
        PERFORM F_CREATE_BUDGET  USING  LT_BUDGET
                                        UF_TEST
                               CHANGING LT_RESULT
                                        LS_COUNT.

*     2.3 Change Project Plan/Budget
      ELSE.
        PERFORM F_CHANGE_BUDGET  USING  LT_BUDGET
                                        UF_TEST
                               CHANGING LT_RESULT
                                        LS_COUNT.

      ENDIF.
*     Collect Final Result
      INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
      CS_PROC-COUNT-ERROR = CS_PROC-COUNT-ERROR + LS_COUNT-ERROR.
      CS_PROC-COUNT-SUCCS = CS_PROC-COUNT-SUCCS + LS_COUNT-SUCCS.

    WHEN OTHERS.

  ENDCASE.

* Assign Processing Info
  CS_PROC-DATUM = SY-DATUM.
  CS_PROC-UZEIT = SY-UZEIT.
  CS_PROC-UNAME = SY-UNAME.
  CASE GC_TRUE.
    WHEN RB_LOCAL.
      CS_PROC-IFILE = P_IFILE.
    WHEN RB_APPSV.
      CS_PROC-IFILE = P_AFILE.
  ENDCASE.
  CASE GC_TRUE.
    WHEN RB_CREAT.
*     Text-X03: Create Project Structure
      CS_PROC-MODE = TEXT-X03.
    WHEN RB_UPDAT.
*     Text-X04: Change Project Structure
      CS_PROC-MODE = TEXT-X04.
    WHEN RB_BGCRE.
*     Text-X05: Upload Original Plan/Budget
      CS_PROC-MODE = TEXT-X05.
    WHEN RB_BGUPD.
*     Text-X06: Revise Plan/Budget
      CS_PROC-MODE = TEXT-X06.
  ENDCASE.
  IF CB_TEST EQ GC_TRUE.
*   Text-x01: Test Run
    CS_PROC-MODE  = |{ CS_PROC-MODE }-{ TEXT-X01 }|.
  ELSE.
*   Text-x02: Production Run
    CS_PROC-MODE  = |{ CS_PROC-MODE }-{ TEXT-X02 }|.
  ENDIF.

  CS_PROC-COUNT-TOTAL = CS_PROC-COUNT-SUCCS + CS_PROC-COUNT-ERROR.

* Show Message Complete
* Text-i07: Processing completed.
  MESSAGE S000(ZSDSPS01) WITH TEXT-I07 SPACE SPACE SPACE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_READ_FILE
*----------------------------------------------------------------------*
*  Read File into Text Table
*----------------------------------------------------------------------*
FORM F_READ_FILE  CHANGING CT_XLSX TYPE  TT_XLSX.

  DATA:
    LT_SHEET   TYPE  IF_FDT_DOC_SPREADSHEET=>T_WORKSHEET_NAMES.

  DATA:
    LF_FILENAME     TYPE  STRING,
    LF_APPSV_FLAG   TYPE  FLAG,
    LF_ACTIVE_SHEET TYPE  FLAG.


* Initialize Output
  CLEAR: CT_XLSX.

* Show progress
* Text-p01 : Reading Input file . . .
  MC_SHOW_PROGRESS 15 TEXT-P01.

  CLEAR: LF_FILENAME,
         LF_APPSV_FLAG.
  CASE GC_TRUE.
    WHEN RB_LOCAL.
      LF_FILENAME = P_IFILE.
    WHEN RB_APPSV.
      LF_FILENAME   = P_AFILE.
      LF_APPSV_FLAG = GC_TRUE.
  ENDCASE.

* Assign Sheet to read
  CLEAR: LT_SHEET,
         LF_ACTIVE_SHEET.
  CASE GC_TRUE.
    WHEN RB_CREAT OR RB_UPDAT.
      APPEND GC_SHEET_PROJDEF TO LT_SHEET.
      APPEND GC_SHEET_WBS     TO LT_SHEET.
    WHEN OTHERS.
      LF_ACTIVE_SHEET = GC_TRUE.
  ENDCASE.

  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = LF_FILENAME
      IF_APPSV_FLAG               = LF_APPSV_FLAG
      IF_READ_ACTIVE_WORKSHEET    = LF_ACTIVE_SHEET
      IT_WORKSHEET                = LT_SHEET
    IMPORTING
      ET_XLSX_DATA                = CT_XLSX
    EXCEPTIONS
      MISSING_FILENAME_OR_XSTRING = 1
      ERROR_READ_FILENAME         = 2
      NO_DATA_FOUND               = 3
      OTHERS                      = 4.
  IF SY-SUBRC <> 0.
    IF SY-SUBRC EQ 2.
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO DISPLAY LIKE 'E'
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_PROJ_STRUC
*----------------------------------------------------------------------*
*  Assign Project Structure Data
*----------------------------------------------------------------------*
FORM F_ASSIGN_PROJ_STRUC  USING  UT_XLSX  TYPE  TT_XLSX
                        CHANGING CT_PROJDEF TYPE  TT_PROJDEF_DATA
                                 CT_WBS     TYPE  TT_WBS_DATA
                                 CT_RESULT  TYPE  TT_RESULT.

  DATA:
    LT_RESULT  TYPE  TT_RESULT.


* Initialize Output
  CLEAR: CT_PROJDEF,
         CT_WBS,
         CT_RESULT.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 40 TEXT-P02.

* Read Project Def Sheet
  READ TABLE UT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>)
                     WITH KEY WORKSHEET_NAME = GC_SHEET_PROJDEF.
  IF SY-SUBRC EQ 0.
*   Assign Project Def Data
    PERFORM F_ASSIGN_PROJDEF_DATA  USING  <L_SHEET>
                                 CHANGING CT_PROJDEF
                                          LT_RESULT.
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  ENDIF.

* Read WBS Sheet
  READ TABLE UT_XLSX ASSIGNING <L_SHEET>
                     WITH KEY WORKSHEET_NAME = GC_SHEET_WBS.
  IF SY-SUBRC EQ 0.
*   Assign WBS Data
    PERFORM F_ASSIGN_WBS_DATA  USING  <L_SHEET>
                             CHANGING CT_WBS
                                      LT_RESULT.
    INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_PROJDEF_DATA
*----------------------------------------------------------------------*
*  Assign Project Definition Data
*----------------------------------------------------------------------*
FORM F_ASSIGN_PROJDEF_DATA  USING  US_XLSX TYPE TS_XLSX
                          CHANGING CT_DATA    TYPE TT_PROJDEF_DATA
                                   CT_RESULT  TYPE TT_RESULT.

  DATA:
    LS_DATA  TYPE TS_PROJDEF_DATA,
    LS_MESSG TYPE TS_MESSG.

  DATA:
    LF_ROW_START TYPE I VALUE 6,
    LF_COL_START TYPE I VALUE 2,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE TF_FNAME,
    LF_INVALID   TYPE FLAG,
    LF_MSGTX     TYPE TF_MSGTX.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION.


* Initialize Output
  CLEAR: CT_DATA,
         CT_RESULT.

  ASSIGN US_XLSX-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT LREF_VALIDATE.

  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE LF_ROW_START.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LF_MSGTX.
    LS_DATA-SHEET = US_XLSX-WORKSHEET_NAME.
    LS_DATA-ROWNO = LF_ROWNO.
    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

      CHECK SY-TABIX GE LF_COL_START.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE1'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Check No change value?
      CHECK <L_FIELD> NE GC_NOCHG.

      CASE LF_FNAME.
        WHEN 'PSPID'.
          ZCL_SDSPS_PROJECT=>VALIDATE_PSPID( EXPORTING IF_INPUT = <L_FIELD>
                                                       IF_CREATE = RB_CREAT
                                             IMPORTING EF_OUTPUT = LS_DATA-PROJDEF-PSPID
                                                       EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
            IF RB_CREAT EQ GC_TRUE.
*             Text-e01: Project definition already exists:
              LF_MSGTX = |{ TEXT-E01 } { <L_FIELD> }|.
            ELSE.
*             Text-e02: Project definition does not exist:
              LF_MSGTX = |{ TEXT-E02 } { <L_FIELD> }|.
            ENDIF.
            EXIT.
          ENDIF.
          LS_DATA-PROJDEFX-PSPID = GC_TRUE.

        WHEN 'POST1'.
          LS_DATA-PROJDEF-POST1 = <L_FIELD>.
          LS_DATA-PROJDEFX-POST1 = GC_TRUE.

        WHEN 'PLFAZ'.
          LREF_VALIDATE->VALIDATE_DATE( EXPORTING IF_INPUT = <L_FIELD>
                                                  IF_FORMAT = SPACE
                                        IMPORTING EF_OUTPUT = LS_DATA-PROJDEF-PLFAZ
                                                  EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e03: Invalid date value (Format DD.MM.YYYY):
            LF_MSGTX = |{ TEXT-E03 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-PROJDEFX-PLFAZ = GC_TRUE.

        WHEN 'PLSEZ'.
          LREF_VALIDATE->VALIDATE_DATE( EXPORTING IF_INPUT = <L_FIELD>
                                                  IF_FORMAT = SPACE
                                        IMPORTING EF_OUTPUT = LS_DATA-PROJDEF-PLSEZ
                                                  EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e03: Invalid date value (Format DD.MM.YYYY):
            LF_MSGTX = |{ TEXT-E03 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-PROJDEFX-PLSEZ = GC_TRUE.

        WHEN 'VERNR'.
          LREF_VALIDATE->VALIDATE_PROJ_RESPPERS( EXPORTING IF_INPUT = <L_FIELD>
                                                 IMPORTING EF_OUTPUT = LS_DATA-PROJDEF-VERNR
                                                           EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e04: Invalid Responsible Person:
            LF_MSGTX = |{ TEXT-E04 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-PROJDEFX-VERNR = GC_TRUE.

        WHEN 'USR00'.
          LS_DATA-PROJDEF-USR00 = <L_FIELD>.
          LS_DATA-PROJDEFX-USR00 = GC_TRUE.
        WHEN 'USR01'.
          LS_DATA-PROJDEF-USR01 = <L_FIELD>.
          LS_DATA-PROJDEFX-USR01 = GC_TRUE.
        WHEN 'USR02'.
          LS_DATA-PROJDEF-USR02 = <L_FIELD>.
          LS_DATA-PROJDEFX-USR02 = GC_TRUE.
        WHEN 'USR03'.
          LS_DATA-PROJDEF-USR03 = <L_FIELD>.
          LS_DATA-PROJDEFX-USR03 = GC_TRUE.
        WHEN 'USR04'.
          LS_DATA-PROJDEF-USR04 = <L_FIELD>.
          LS_DATA-PROJDEFX-USR04 = GC_TRUE.
        WHEN 'USR10'.
          LREF_VALIDATE->VALIDATE_FLAG( EXPORTING IF_INPUT = <L_FIELD>
                                        IMPORTING EF_OUTPUT  = LS_DATA-PROJDEF-USR10
                                                  EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e05: Invalid flag:
            LF_MSGTX = |{ TEXT-E05 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-PROJDEFX-USR10 = GC_TRUE.
        WHEN 'USR11'.
          LREF_VALIDATE->VALIDATE_FLAG( EXPORTING IF_INPUT = <L_FIELD>
                                        IMPORTING EF_OUTPUT  = LS_DATA-PROJDEF-USR11
                                                  EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e05: Invalid flag:
            LF_MSGTX = |{ TEXT-E05 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-PROJDEFX-USR11 = GC_TRUE.
        WHEN 'PROFL'.
          LREF_VALIDATE->VALIDATE_PROJ_PROFILE( EXPORTING IF_INPUT = <L_FIELD>
                                                IMPORTING EF_OUTPUT = LS_DATA-PROJDEF-PROFL
                                                          EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e06: Invalid Project Profile:
            LF_MSGTX = |{ TEXT-E06 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-PROJDEFX-PROFL = GC_TRUE.

      ENDCASE.

    ENDLOOP.

    IF LF_MSGTX IS INITIAL.
*     Validate data Row Level
      ZCL_SDSPS_PROJECT=>VALIDATE_PROJECTDEF( EXPORTING IS_PROJECTDEF = LS_DATA-PROJDEF
                                              IMPORTING EF_MSGTX  = LF_MSGTX ).
    ENDIF.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.
*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT1  USING  LS_DATA
                                        LS_MESSG
                               CHANGING CT_RESULT.
      CONTINUE.
    ENDIF.

*   Collect Data
    INSERT LS_DATA INTO TABLE CT_DATA.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_target_field
*----------------------------------------------------------------------*
*  Get Target Field name based on column sequence
*----------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD  USING  UF_TYPE   TYPE  CHAR20
                                UF_INDEX  TYPE  I
                       CHANGING CF_FIELD  TYPE  TF_FNAME.

  STATICS:
    LF_TYPE   TYPE  CHAR20,
    LT_FIELDS TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
    <L_FIELD>  TYPE  ABAP_COMPDESCR.


* Initialize Output
  CLEAR: CF_FIELD.

  IF LT_FIELDS IS INITIAL OR
     LF_TYPE NE UF_TYPE.
    CLEAR: LF_TYPE,
           LT_FIELDS.
    LF_TYPE = UF_TYPE.
*   Get Template Fields Sequence
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( LF_TYPE ).
    LREF_STRUC ?= LREF_DESCR.
    LT_FIELDS = LREF_STRUC->COMPONENTS.
  ENDIF.

  READ TABLE LT_FIELDS ASSIGNING <L_FIELD>
                       INDEX UF_INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_FIELD = <L_FIELD>-NAME.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CREATE_PROJDEF
*----------------------------------------------------------------------*
*  Create Project Definition
*----------------------------------------------------------------------*
FORM F_CREATE_PROJDEF  USING  UT_DATA   TYPE  TT_PROJDEF_DATA
                              UF_TEST   TYPE  FLAG
                     CHANGING CT_RESULT TYPE  TT_RESULT
                              CS_COUNT  TYPE  TS_COUNT.

  DATA:
    LS_RETURN TYPE  BAPIRETURN1,
    LS_MESSG  TYPE  TS_MESSG.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

  LOOP AT UT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).

    ZCL_SDSPS_PROJECT=>CREATE_PROJECTDEF( EXPORTING IS_PROJECTDEF = <L_DATA>-PROJDEF
                                                    IF_TEST       = UF_TEST
                                          IMPORTING ES_RETURN     = LS_RETURN ).

*   Assign Result
    CLEAR LS_MESSG.
    IF LS_RETURN IS NOT INITIAL.
      CS_COUNT-ERROR = CS_COUNT-ERROR + 1.
      LS_MESSG-MSGTY = LS_RETURN-TYPE.
      LS_MESSG-MSGID = LS_RETURN-ID.
      LS_MESSG-MSGNO = LS_RETURN-NUMBER.
      LS_MESSG-MSGTX = LS_RETURN-MESSAGE.
    ELSE.
      CS_COUNT-SUCCS = CS_COUNT-SUCCS + 1.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      IF UF_TEST EQ GC_TRUE.
*       Text-i01: Test run successfully.
        LS_MESSG-MSGTX = TEXT-I01.
      ELSE.
*       Text-i02: Project definition has been created.
        LS_MESSG-MSGTX = TEXT-I02.
      ENDIF.
    ENDIF.

*   Collect Validation Error Result
    PERFORM F_COLLECT_RESULT1  USING  <L_DATA>
                                      LS_MESSG
                             CHANGING CT_RESULT.

  ENDLOOP.

  CS_COUNT-TOTAL = CS_COUNT-SUCCS + CS_COUNT-ERROR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_WBS_DATA
*----------------------------------------------------------------------*
*  Assign WBS Data
*----------------------------------------------------------------------*
FORM F_ASSIGN_WBS_DATA  USING  US_XLSX TYPE TS_XLSX
                      CHANGING CT_DATA  TYPE  TT_WBS_DATA
                               CT_RESULT TYPE TT_RESULT.
  DATA:
    LS_DATA  TYPE TS_WBS_DATA,
    LS_MESSG TYPE TS_MESSG.

  DATA:
    LF_ROW_START TYPE I VALUE 6,
    LF_COL_START TYPE I VALUE 2,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE TF_FNAME,
    LF_INVALID   TYPE FLAG,
    LF_MSGTX     TYPE TF_MSGTX.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION.


* Initialize Output
  CLEAR: CT_DATA,
         CT_RESULT.

  ASSIGN US_XLSX-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT LREF_VALIDATE.

  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE LF_ROW_START.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LF_MSGTX.
    LS_DATA-SHEET = US_XLSX-WORKSHEET_NAME.
    LS_DATA-ROWNO = LF_ROWNO.
    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

      CHECK SY-TABIX GE LF_COL_START.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE2'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Check No change value?
      CHECK <L_FIELD> NE GC_NOCHG.

      CASE LF_FNAME.
        WHEN 'POSID'.
          LREF_VALIDATE->VALIDATE_WBSELEM( EXPORTING IF_INPUT = <L_FIELD>
                                                     IF_CREATE = RB_CREAT
                                           IMPORTING EF_OUTPUT = LS_DATA-WBS-POSID
                                                     EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
            IF RB_CREAT EQ GC_TRUE.
*             Text-e07: WBS Element already exists:
              LF_MSGTX = |{ TEXT-E07 } { <L_FIELD> }|.
            ELSE.
*             Text-e08: WBS Element does not exist:
              LF_MSGTX = |{ TEXT-E08 } { <L_FIELD> }|.
            ENDIF.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-POSID = GC_TRUE.

        WHEN 'POST1'.
          LS_DATA-WBS-POST1 = <L_FIELD>.
          LS_DATA-WBSX-POST1 = GC_TRUE.
        WHEN 'STUFE'.
          LREF_VALIDATE->VALIDATE_NUMBER( EXPORTING IF_INPUT = <L_FIELD>
                                                    IF_FORMAT = SPACE
                                          IMPORTING EF_OUTPUT = LS_DATA-WBS-STUFE
                                                    EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e09: Invalid WBS level:
            LF_MSGTX = |{ TEXT-E09 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-STUFE = GC_TRUE.
        WHEN 'PSTRT'.
          LREF_VALIDATE->VALIDATE_DATE( EXPORTING IF_INPUT = <L_FIELD>
                                                  IF_FORMAT = SPACE
                                        IMPORTING EF_OUTPUT = LS_DATA-WBS-PSTRT
                                                  EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e03: Invalid date value (Format DD.MM.YYYY):
            LF_MSGTX = |{ TEXT-E03 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-PSTRT = GC_TRUE.
        WHEN 'PENDE'.
          LREF_VALIDATE->VALIDATE_DATE( EXPORTING IF_INPUT = <L_FIELD>
                                                  IF_FORMAT = SPACE
                                        IMPORTING EF_OUTPUT = LS_DATA-WBS-PENDE
                                                  EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e03: Invalid date value (Format DD.MM.YYYY):
            LF_MSGTX = |{ TEXT-E03 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-PENDE = GC_TRUE.
        WHEN 'PRART'.
          LREF_VALIDATE->VALIDATE_PROJECT_TYPE( EXPORTING IF_INPUT = <L_FIELD>
                                                IMPORTING EF_OUTPUT = LS_DATA-WBS-PRART
                                                          EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e10: Invalid Project type:
            LF_MSGTX = |{ TEXT-E10 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-PRART = GC_TRUE.
        WHEN 'USR00'.
          LREF_VALIDATE->VALIDATE_LOB( EXPORTING IF_INPUT = <L_FIELD>
                                       IMPORTING EF_OUTPUT = LS_DATA-WBS-USR00
                                                 EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e14: Invalid LOB:
            LF_MSGTX = |{ TEXT-E14 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-USR00 = GC_TRUE.
        WHEN 'USR01'.
          LS_DATA-WBS-USR01 = <L_FIELD>.
          LS_DATA-WBSX-USR01 = GC_TRUE.
        WHEN 'USR02'.
          LS_DATA-WBS-USR02 = <L_FIELD>.
          LS_DATA-WBSX-USR02 = GC_TRUE.
        WHEN 'USR03'.
          LS_DATA-WBS-USR03 = <L_FIELD>.
          LS_DATA-WBSX-USR03 = GC_TRUE.
        WHEN 'PRCTR'.
          LREF_VALIDATE->VALIDATE_PROFIT_CENTER( EXPORTING IF_INPUT = <L_FIELD>
                                                 IMPORTING EF_OUTPUT = LS_DATA-WBS-PRCTR
                                                           EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e11: Invalid Profit Center:
            LF_MSGTX = |{ TEXT-E11 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-PRCTR = GC_TRUE.
        WHEN 'FKSTL'.
          LREF_VALIDATE->VALIDATE_COST_CENTER( EXPORTING IF_INPUT = <L_FIELD>
                                               IMPORTING EF_OUTPUT = LS_DATA-WBS-FKSTL
                                                         EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e12: Invalid Responsible Cost Center:
            LF_MSGTX = |{ TEXT-E12 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-FKSTL = GC_TRUE.
        WHEN 'AKSTL'.
          LREF_VALIDATE->VALIDATE_COST_CENTER( EXPORTING IF_INPUT = <L_FIELD>
                                               IMPORTING EF_OUTPUT = LS_DATA-WBS-AKSTL
                                                         EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e13: Invalid Requesting Cost Center:
            LF_MSGTX = |{ TEXT-E13 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-WBSX-AKSTL = GC_TRUE.

      ENDCASE.

    ENDLOOP.

    IF LF_MSGTX IS INITIAL.
*     Validate data Row Level
      ZCL_SDSPS_PROJECT=>VALIDATE_WBSELEM( EXPORTING IS_WBSELEM = LS_DATA-WBS
                                           IMPORTING EF_MSGTX  = LF_MSGTX
                                                     ES_WBSLV  = LS_DATA-WBSLV ).
*     Project = WBS Lv1
      LS_DATA-PSPID = LS_DATA-WBSLV-POSID_LV1.
    ENDIF.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.
*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT2  USING  LS_DATA
                                        LS_MESSG
                               CHANGING CT_RESULT.
      CONTINUE.
    ENDIF.

*   Collect Data
    INSERT LS_DATA INTO TABLE CT_DATA.

  ENDLOOP.

* Sort data by Project Definition
  SORT CT_DATA BY PSPID     ASCENDING
                  WBS-POSID ASCENDING.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_collect_result1
*----------------------------------------------------------------------*
*  Collect Project Definition Result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT1  USING  US_DATA  TYPE TS_PROJDEF_DATA
                               US_MESSG TYPE TS_MESSG
                      CHANGING CT_RESULT TYPE TT_RESULT.
  DATA:
    LS_RESULT  TYPE  TS_RESULT.


  CLEAR LS_RESULT.
  LS_RESULT-SHEET = US_DATA-SHEET.
  LS_RESULT-ROWNO = US_DATA-ROWNO.
  LS_RESULT-PSPID = US_DATA-PROJDEF-PSPID.
  LS_RESULT-USR00 = US_DATA-PROJDEF-USR00.
  LS_RESULT-MSGTY = US_MESSG-MSGTY.
  LS_RESULT-MSGID = US_MESSG-MSGID.
  LS_RESULT-MSGNO = US_MESSG-MSGNO.
  LS_RESULT-MSGTX = US_MESSG-MSGTX.

  CASE LS_RESULT-MSGTY.
    WHEN 'S'.
      LS_RESULT-STATU = ICON_LED_GREEN.
    WHEN 'W'.
      LS_RESULT-STATU = ICON_LED_YELLOW.
    WHEN OTHERS.
      LS_RESULT-STATU = ICON_LED_RED.
  ENDCASE.

  INSERT LS_RESULT INTO TABLE CT_RESULT.

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
  GF_ALV_HEADER_1 = GC_TRUE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.
** No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

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
    LF_TEXT  TYPE  STRING.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'SHEET'.
        <L_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
        IF RB_CREAT EQ SPACE AND
           RB_UPDAT EQ SPACE.
          <L_FIELDCAT>-TECH = GC_TRUE.
        ENDIF.
      WHEN 'ROWNO'.
        <L_FIELDCAT>-OUTPUTLEN = 10.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
      WHEN 'STATU'.
        <L_FIELDCAT>-OUTPUTLEN = 6.
        <L_FIELDCAT>-JUST      = 'C'.
        <L_FIELDCAT>-ICON      = GC_TRUE.
*       Text-c00 : Status
        LF_TEXT                = TEXT-C00.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'PSPID'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'GJAHR'.
        IF RB_BGCRE EQ SPACE AND
           RB_BGUPD EQ SPACE.
          <L_FIELDCAT>-TECH    = GC_TRUE.
        ENDIF.
      WHEN 'MONAT'.
        IF RB_BGCRE EQ SPACE AND
           RB_BGUPD EQ SPACE.
          <L_FIELDCAT>-TECH    = GC_TRUE.
        ENDIF.
        <L_FIELDCAT>-OUTPUTLEN = 6 ##NUMBER_OK.
      WHEN 'VERSN'.
        IF RB_BGCRE EQ SPACE AND
           RB_BGUPD EQ SPACE.
          <L_FIELDCAT>-TECH    = GC_TRUE.
        ENDIF.
        <L_FIELDCAT>-OUTPUTLEN = 6 ##NUMBER_OK.
      WHEN 'POSID'.
      WHEN 'KSTAR'.
        IF RB_BGCRE EQ SPACE AND
           RB_BGUPD EQ SPACE.
          <L_FIELDCAT>-TECH    = GC_TRUE.
        ENDIF.
      WHEN 'WRBTR'.
        IF RB_BGCRE EQ SPACE AND
           RB_BGUPD EQ SPACE.
          <L_FIELDCAT>-TECH    = GC_TRUE.
        ENDIF.
        <L_FIELDCAT>-OUTPUTLEN = 18 ##NUMBER_OK.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
      WHEN 'USR00'.
        IF RB_CREAT EQ SPACE AND
           RB_UPDAT EQ SPACE.
          <L_FIELDCAT>-TECH    = GC_TRUE.
        ENDIF.
*       Text-c01 : SF Project No.
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'USR01'.
        IF RB_CREAT EQ SPACE AND
           RB_UPDAT EQ SPACE.
          <L_FIELDCAT>-TECH    = GC_TRUE.
        ENDIF.
*       Text-c02 : SF ID
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGID'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGNO'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 60 ##NUMBER_OK.

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
    LC_SORT1 TYPE  LVC_FNAME VALUE 'SHEET',
    LC_SORT2 TYPE  LVC_FNAME VALUE 'ROWNO'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

* Sort by Sheet
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by Row Number
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '18%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '82%'.

  DATA:
    LF_TEMP1     TYPE  TEXT50,
    LF_TEMP2     TYPE  TEXT50,
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

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
* Add value in Line1
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Date/Time:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-DATUM TO LF_TEMP1.
  WRITE GS_PROC-UZEIT TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }/{ LF_TEMP2 }|.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : User:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_PROC-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : Filename:
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_PROC-IFILE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Mode:
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_PROC-MODE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_TOTAL.

*-----------------------
* Add value in Line5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : Total record(s):
  LF_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-COUNT-TOTAL TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_HEADING_INT.

*-----------------------
* Add value in Line6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Success record(s):
  LF_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-COUNT-SUCCS TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

*-----------------------
* Add value in Line7
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h07 : Error record(s):
  LF_TEXT = TEXT-H07.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-COUNT-ERROR TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_NEGATIVE.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LF_COL01 TYPE  I VALUE 20,
    LF_COL02 TYPE  I VALUE 40,
    LF_TEXT  TYPE  TEXT50,
    LF_TEMP1 TYPE  TEXT50,
    LF_TEMP2 TYPE  TEXT50.


* Text-h01 : Date/Time:
  WRITE GS_PROC-DATUM TO LF_TEMP1.
  WRITE GS_PROC-UZEIT TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }/{ LF_TEMP2 }|.
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h02 : User:
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LF_COL02)    GS_PROC-UNAME NO-GAP.

* Text-h03 : Filename:
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (150)         GS_PROC-IFILE NO-GAP ##NUMBER_OK.

* Text-h04 : Mode:
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LF_COL02)    GS_PROC-MODE NO-GAP COLOR COL_TOTAL.

* Text-h05 : Total record(s):
  WRITE GS_PROC-COUNT-TOTAL TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H05 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_HEADING.

* Text-h06 : Success record(s):
  WRITE GS_PROC-COUNT-SUCCS TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H06 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_POSITIVE.

* Text-h07 : Error record(s):
  WRITE GS_PROC-COUNT-ERROR TO LF_TEXT.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H07 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_NEGATIVE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_COLLECT_RESULT2
*----------------------------------------------------------------------*
*  Collect WBS Element Result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT2  USING  US_DATA  TYPE  TS_WBS_DATA
                               US_MESSG TYPE TS_MESSG
                      CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.


  CLEAR LS_RESULT.
  LS_RESULT-SHEET = US_DATA-SHEET.
  LS_RESULT-ROWNO = US_DATA-ROWNO.
  LS_RESULT-PSPID = US_DATA-PSPID.
  LS_RESULT-USR00 = US_DATA-WBS-USR02.
  LS_RESULT-POSID = US_DATA-WBS-POSID.
  LS_RESULT-USR01 = US_DATA-WBS-USR01.
  LS_RESULT-MSGTY = US_MESSG-MSGTY.
  LS_RESULT-MSGID = US_MESSG-MSGID.
  LS_RESULT-MSGNO = US_MESSG-MSGNO.
  LS_RESULT-MSGTX = US_MESSG-MSGTX.

  CASE LS_RESULT-MSGTY.
    WHEN 'S'.
      LS_RESULT-STATU = ICON_LED_GREEN.
    WHEN 'W'.
      LS_RESULT-STATU = ICON_LED_YELLOW.
    WHEN OTHERS.
      LS_RESULT-STATU = ICON_LED_RED.
  ENDCASE.

  INSERT LS_RESULT INTO TABLE CT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CREATE_WBS
*----------------------------------------------------------------------*
*  Create WBS Elment
*----------------------------------------------------------------------*
FORM F_CREATE_WBS  USING  UT_PROJ   TYPE  TT_PROJ
                          UT_DATA   TYPE  TT_WBS_DATA
                          UF_TEST   TYPE  FLAG
                 CHANGING CT_RESULT TYPE  TT_RESULT
                          CS_COUNT  TYPE  TS_COUNT.

  DATA:
    LT_WBS    TYPE  ZCL_SDSPS_PROJECT=>TT_WBSELEM,
    LT_RETURN TYPE  BAPIRET2_TAB.

  DATA:
    LS_MESSG  TYPE  TS_MESSG.

  DATA:
    LF_LINES  TYPE  I.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

* Group Data by Project Def
  LOOP AT UT_DATA INTO DATA(LS_DATA)
                  GROUP BY ( PSPID = LS_DATA-PSPID ) ASCENDING
                  ASSIGNING FIELD-SYMBOL(<L_GROUP>).

    CLEAR: LT_WBS,
           LF_LINES.
    LOOP AT GROUP <L_GROUP> ASSIGNING FIELD-SYMBOL(<L_DATA>).
      INSERT <L_DATA>-WBS INTO TABLE LT_WBS.
      LF_LINES = LF_LINES + 1.
    ENDLOOP.

*   Check If project just create, wait unlock
    IF LINE_EXISTS( UT_PROJ[ PSPID = <L_GROUP>-PSPID ] ).
      PERFORM F_WAIT_PROJ_UNLOCK USING <L_GROUP>-PSPID.
    ENDIF.

*   Create WBS
    ZCL_SDSPS_PROJECT=>CREATE_WBSELEM( EXPORTING IF_PSPID   = <L_GROUP>-PSPID
                                                 IT_WBSELEM = LT_WBS
                                                 IF_TEST    = UF_TEST
                                                 IF_SKIP_CHK_PSPID = UF_TEST
                                       IMPORTING ET_RETURN  = LT_RETURN ).

*   Assign Result
*   ------------------
*   Error
*   ------------------
    IF LT_RETURN IS NOT INITIAL.

      CS_COUNT-ERROR = CS_COUNT-ERROR + LF_LINES.

*     Collect Validation Error Result
      LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).

        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
        LS_MESSG-MSGID = <L_RETURN>-ID.
        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE.

        CLEAR LS_DATA.
*       Map error message with input
        LOOP AT GROUP <L_GROUP> ASSIGNING <L_DATA>
                                WHERE WBS-POSID = <L_RETURN>-PARAMETER.
          EXIT.
        ENDLOOP.
        IF SY-SUBRC EQ 0.
          LS_DATA = <L_DATA>.
        ELSE.
*         Cannot map, just assign project key
          LS_DATA-PSPID = <L_GROUP>-PSPID.
        ENDIF.

        PERFORM F_COLLECT_RESULT2  USING  LS_DATA
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDLOOP.
*   ------------------
*   Success
*   ------------------
    ELSE.

      CS_COUNT-SUCCS = CS_COUNT-SUCCS + LF_LINES.

      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      IF UF_TEST EQ GC_TRUE.
*       Text-i01: Test run successfully.
        LS_MESSG-MSGTX = TEXT-I01.
      ELSE.
*       Text-i03: WBS Element has been created.
        LS_MESSG-MSGTX = TEXT-I03.
      ENDIF.

*     Assign Message to all input lines
      LOOP AT GROUP <L_GROUP> ASSIGNING <L_DATA>.
        PERFORM F_COLLECT_RESULT2  USING  <L_DATA>
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

  CS_COUNT-TOTAL = CS_COUNT-SUCCS + CS_COUNT-ERROR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHANGE_PROJDEF
*----------------------------------------------------------------------*
*  Change Project Definition
*----------------------------------------------------------------------*
FORM F_CHANGE_PROJDEF  USING  UT_DATA  TYPE  TT_PROJDEF_DATA
                              UF_TEST  TYPE  FLAG
                     CHANGING CT_RESULT  TYPE  TT_RESULT
                              CS_COUNT   TYPE  TS_COUNT.

  DATA:
    LS_RETURN TYPE  BAPIRETURN1,
    LS_MESSG  TYPE  TS_MESSG.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

  LOOP AT UT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).

    ZCL_SDSPS_PROJECT=>CHANGE_PROJECTDEF( EXPORTING IS_PROJECTDEF  = <L_DATA>-PROJDEF
                                                    IS_PROJECTDEFX = <L_DATA>-PROJDEFX
                                                    IF_TEST        = UF_TEST
                                          IMPORTING ES_RETURN     = LS_RETURN ).

*   Assign Result
    CLEAR LS_MESSG.
    IF LS_RETURN IS NOT INITIAL.
      CS_COUNT-ERROR = CS_COUNT-ERROR + 1.
      LS_MESSG-MSGTY = LS_RETURN-TYPE.
      LS_MESSG-MSGID = LS_RETURN-ID.
      LS_MESSG-MSGNO = LS_RETURN-NUMBER.
      LS_MESSG-MSGTX = LS_RETURN-MESSAGE.
    ELSE.
      CS_COUNT-SUCCS = CS_COUNT-SUCCS + 1.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      IF UF_TEST EQ GC_TRUE.
*       Text-i01: Test run successfully.
        LS_MESSG-MSGTX = TEXT-I01.
      ELSE.
*       Text-i04: Project definition has been changed.
        LS_MESSG-MSGTX = TEXT-I04.
      ENDIF.
    ENDIF.

*   Collect Validation Error Result
    PERFORM F_COLLECT_RESULT1  USING  <L_DATA>
                                      LS_MESSG
                             CHANGING CT_RESULT.

  ENDLOOP.

  CS_COUNT-TOTAL = CS_COUNT-SUCCS + CS_COUNT-ERROR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHANGE_WBS
*----------------------------------------------------------------------*
*  Change WBS Structure
*----------------------------------------------------------------------*
FORM F_CHANGE_WBS  USING  UT_PROJ   TYPE  TT_PROJ
                          UT_DATA   TYPE  TT_WBS_DATA
                          UF_TEST   TYPE  FLAG
                 CHANGING CT_RESULT TYPE  TT_RESULT
                          CS_COUNT  TYPE  TS_COUNT.

  DATA:
    LT_WBS    TYPE  ZCL_SDSPS_PROJECT=>TT_WBS_UPD,
    LT_RETURN TYPE  BAPIRET2_TAB.

  DATA:
    LS_MESSG  TYPE  TS_MESSG.

  DATA:
    LF_LINES  TYPE  I.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

* Group Data by Project Def
  LOOP AT UT_DATA INTO DATA(LS_DATA)
                  GROUP BY ( PSPID = LS_DATA-PSPID ) ASCENDING
                  ASSIGNING FIELD-SYMBOL(<L_GROUP>).

    CLEAR: LT_WBS,
           LF_LINES.
    LOOP AT GROUP <L_GROUP> ASSIGNING FIELD-SYMBOL(<L_DATA>).
      INSERT VALUE #( WBSELEM = <L_DATA>-WBS
                      WBSELEMX = <L_DATA>-WBSX )
                    INTO TABLE LT_WBS.
      LF_LINES = LF_LINES + 1.
    ENDLOOP.

*   Check If project just create, wait unlock
    IF LINE_EXISTS( UT_PROJ[ PSPID = <L_GROUP>-PSPID ] ).
      PERFORM F_WAIT_PROJ_UNLOCK USING <L_GROUP>-PSPID.
    ENDIF.

*   Create WBS
    ZCL_SDSPS_PROJECT=>CHANGE_WBSELEM( EXPORTING IF_PSPID   = <L_GROUP>-PSPID
                                                 IT_WBSELEM = LT_WBS
                                                 IF_TEST    = UF_TEST
                                       IMPORTING ET_RETURN  = LT_RETURN ).

*   Assign Result
*   ------------------
*   Error
*   ------------------
    IF LT_RETURN IS NOT INITIAL.

      CS_COUNT-ERROR = CS_COUNT-ERROR + LF_LINES.

*     Collect Validation Error Result
      LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).

        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
        LS_MESSG-MSGID = <L_RETURN>-ID.
        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE.

        CLEAR LS_DATA.
*       Map error message with input
        LOOP AT GROUP <L_GROUP> ASSIGNING <L_DATA>
                                WHERE WBS-POSID = <L_RETURN>-PARAMETER.
          EXIT.
        ENDLOOP.
        IF SY-SUBRC EQ 0.
          LS_DATA = <L_DATA>.
        ELSE.
*         Cannot map, just assign project key
          LS_DATA-PSPID = <L_GROUP>-PSPID.
        ENDIF.

        PERFORM F_COLLECT_RESULT2  USING  LS_DATA
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDLOOP.
*   ------------------
*   Success
*   ------------------
    ELSE.

      CS_COUNT-SUCCS = CS_COUNT-SUCCS + LF_LINES.

      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      IF UF_TEST EQ GC_TRUE.
*       Text-i01: Test run successfully.
        LS_MESSG-MSGTX = TEXT-I01.
      ELSE.
*       Text-i05: WBS Element has been changed.
        LS_MESSG-MSGTX = TEXT-I05.
      ENDIF.

*     Assign Message to all input lines
      LOOP AT GROUP <L_GROUP> ASSIGNING <L_DATA>.
        PERFORM F_COLLECT_RESULT2  USING  <L_DATA>
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

  CS_COUNT-TOTAL = CS_COUNT-SUCCS + CS_COUNT-ERROR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_BUDGET_DATA
*----------------------------------------------------------------------*
*  Assign Data from XLSX Sheet to Internal table
*----------------------------------------------------------------------*
FORM F_ASSIGN_BUDGET_DATA  USING  UT_XLSX  TYPE  TT_XLSX
                         CHANGING CT_DATA TYPE TT_BUDGET_DATA
                                  CT_RESULT TYPE TT_RESULT.

  DATA:
    LS_DATA   TYPE TS_BUDGET_DATA,
    LS_DETAIL TYPE TS_BUDGET_DETAIL,
    LS_MESSG  TYPE TS_MESSG.

  DATA:
    LF_ROW_START TYPE I VALUE 3,
    LF_COL_START TYPE I VALUE 1,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE TF_FNAME,
    LF_INVALID   TYPE FLAG,
    LF_MSGTX     TYPE TF_MSGTX.

  DATA:
    LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR,
    LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION.


* Initialize Output
  CLEAR: CT_DATA,
         CT_RESULT.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 40 TEXT-P02.

* Read Active Sheet
  READ TABLE UT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>)
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  ASSIGN <L_SHEET>-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT LREF_VALIDATE.

  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE LF_ROW_START.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LS_DETAIL,
           LF_MSGTX.
    LS_DETAIL-SHEET = <L_SHEET>-WORKSHEET_NAME.
    LS_DETAIL-ROWNO = LF_ROWNO.
    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

      CHECK SY-TABIX GE LF_COL_START.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE3'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Check No change value?
      CHECK <L_FIELD> NE GC_NOCHG.

      CASE LF_FNAME.
        WHEN 'POSID'.
          LREF_VALIDATE->VALIDATE_WBSELEM( EXPORTING IF_INPUT = <L_FIELD>
                                           IMPORTING EF_OUTPUT = LS_DETAIL-BUDGET-POSID
                                                     EF_OBJNR   = LS_DETAIL-BUDGET-OBJNR
                                                     EF_INVALID = LF_INVALID
                                                     EF_PSPID   = LS_DATA-KEY-PSPID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e08: WBS Element does not exist:
            LF_MSGTX = |{ TEXT-E08 } { <L_FIELD> }|.
            EXIT.
          ENDIF.

        WHEN 'KSTAR'.
          LREF_VALIDATE->VALIDATE_COST_ELEMENT( EXPORTING IF_INPUT = <L_FIELD>
                                                IMPORTING EF_OUTPUT = LS_DETAIL-BUDGET-KSTAR
                                                          EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e15: Invalid Cost Element:
            LF_MSGTX = |{ TEXT-E15 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
        WHEN 'VERSN'.
          LREF_VALIDATE->VALIDATE_CO_VERSION( EXPORTING IF_INPUT = <L_FIELD>
                                              IMPORTING EF_OUTPUT = LS_DATA-KEY-VERSN
                                                        EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e16: Invalid Version:
            LF_MSGTX = |{ TEXT-E16 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
        WHEN 'GJAHR'.
          LREF_VALIDATE->VALIDATE_NUMBER( EXPORTING IF_INPUT = <L_FIELD>
                                                    IF_FORMAT = 'G'
                                          IMPORTING EF_OUTPUT = LS_DETAIL-BUDGET-GJAHR
                                                    EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e17: Invalid Fiscal Year:
            LF_MSGTX = |{ TEXT-E17 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
          LS_DATA-KEY-GJAHR = LS_DETAIL-BUDGET-GJAHR.
        WHEN 'MONAT'.
          LREF_VALIDATE->VALIDATE_NUMBER( EXPORTING IF_INPUT = <L_FIELD>
                                                    IF_FORMAT = 'M'
                                          IMPORTING EF_OUTPUT = LS_DETAIL-BUDGET-MONAT
                                                    EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e18: Invalid Period:
            LF_MSGTX = |{ TEXT-E18 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
        WHEN 'AMOUNT'.
          LREF_VALIDATE->VALIDATE_NUMBER( EXPORTING IF_INPUT = <L_FIELD>
                                                    IF_FORMAT = 'A'
                                          IMPORTING EF_OUTPUT = LS_DETAIL-BUDGET-AMOUNT
                                                    EF_INVALID = LF_INVALID ).
          IF LF_INVALID IS NOT INITIAL.
*           Text-e19: Invalid Amount :
            LF_MSGTX = |{ TEXT-E19 } { <L_FIELD> }|.
            EXIT.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    IF LF_MSGTX IS INITIAL.
*     Validate data Row Level
      ZCL_SDSPS_PROJECT=>VALIDATE_BUDGET( EXPORTING IS_BUDGET = LS_DETAIL-BUDGET
                                          IMPORTING EF_MSGTX  = LF_MSGTX
                                                    ES_WBSLV  = LS_DETAIL-WBSLV ).
    ENDIF.

    IF LF_MSGTX IS NOT INITIAL.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'E'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      LS_MESSG-MSGTX = LF_MSGTX.
*     Collect Validation Error Result
      PERFORM F_COLLECT_RESULT3  USING  LS_DATA-KEY
                                        LS_DETAIL
                                        LS_MESSG
                               CHANGING CT_RESULT.
      CONTINUE.
    ENDIF.

*   Collect Data
    READ TABLE CT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>)
                       WITH KEY KEY = LS_DATA-KEY
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT LS_DATA INTO TABLE CT_DATA
                     ASSIGNING <L_DATA>.
    ENDIF.
    INSERT LS_DETAIL INTO TABLE <L_DATA>-DETAIL.
*   Update Low High Period
    IF <L_DATA>-MONAT_LOW IS INITIAL OR
       <L_DATA>-MONAT_LOW GT LS_DETAIL-BUDGET-MONAT.
      <L_DATA>-MONAT_LOW = LS_DETAIL-BUDGET-MONAT.
    ENDIF.
    IF <L_DATA>-MONAT_HIGH IS INITIAL OR
       <L_DATA>-MONAT_HIGH LT LS_DETAIL-BUDGET-MONAT.
      <L_DATA>-MONAT_HIGH = LS_DETAIL-BUDGET-MONAT.
    ENDIF.
*   Assign LV2 exist flag
    IF LS_DETAIL-WBSLV-STUFE = 2.
      <L_DATA>-LV2 = GC_TRUE.
    ENDIF.
*   Assign LV3 exist flag
    IF LS_DETAIL-WBSLV-STUFE = 3.
      <L_DATA>-LV3 = GC_TRUE.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_COLLECT_RESULT3
*----------------------------------------------------------------------*
*  Collect result from Budget data
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT3  USING  US_KEY    TYPE  TS_BUDGET_DATA-KEY
                               US_DETAIL TYPE  TS_BUDGET_DETAIL
                               US_MESSG  TYPE  TS_MESSG
                      CHANGING CT_RESULT TYPE  TT_RESULT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.


  CLEAR LS_RESULT.
  LS_RESULT-SHEET = US_DETAIL-SHEET.
  LS_RESULT-ROWNO = US_DETAIL-ROWNO.
  LS_RESULT-PSPID = US_KEY-PSPID.
  LS_RESULT-GJAHR = US_KEY-GJAHR.
  LS_RESULT-MONAT = US_DETAIL-BUDGET-MONAT.
  LS_RESULT-VERSN = US_KEY-VERSN.
  LS_RESULT-POSID = US_DETAIL-BUDGET-POSID.
  LS_RESULT-KSTAR = US_DETAIL-BUDGET-KSTAR.
  LS_RESULT-WRBTR = US_DETAIL-BUDGET-AMOUNT.
  LS_RESULT-MSGTY = US_MESSG-MSGTY.
  LS_RESULT-MSGID = US_MESSG-MSGID.
  LS_RESULT-MSGNO = US_MESSG-MSGNO.
  LS_RESULT-MSGTX = US_MESSG-MSGTX.

  CASE LS_RESULT-MSGTY.
    WHEN 'S'.
      LS_RESULT-STATU = ICON_LED_GREEN.
    WHEN 'W'.
      LS_RESULT-STATU = ICON_LED_YELLOW.
    WHEN OTHERS.
      LS_RESULT-STATU = ICON_LED_RED.
  ENDCASE.

  INSERT LS_RESULT INTO TABLE CT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CREATE_BUDGET
*----------------------------------------------------------------------*
*  Create Budget
*----------------------------------------------------------------------*
FORM F_CREATE_BUDGET  USING  UT_DATA  TYPE  TT_BUDGET_DATA
                             UF_TEST  TYPE  FLAG
                    CHANGING CT_RESULT  TYPE  TT_RESULT
                             CS_COUNT   TYPE  TS_COUNT.

  DATA:
    LT_BUDGET TYPE  ZCL_SDSPS_PROJECT=>TT_BUDGET,
    LT_RETURN TYPE  BAPIRET2_TAB.

  DATA:
    LS_MESSG  TYPE  TS_MESSG,
    LS_DETAIL TYPE  TS_BUDGET_DETAIL.

  DATA:
    LF_LINES     TYPE  I,
    LF_ORG_VERSN TYPE  FLAG.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

* Show progress
* Text-p03 : Uploading data . . .
  MC_SHOW_PROGRESS 70 TEXT-P03.

* Group Data by Project Def
  LOOP AT UT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).

    CLEAR: LT_BUDGET,
           LF_LINES.
    LOOP AT <L_DATA>-DETAIL ASSIGNING FIELD-SYMBOL(<L_DETAIL>).
      INSERT <L_DETAIL>-BUDGET INTO TABLE LT_BUDGET.
      LF_LINES = LF_LINES + 1.
    ENDLOOP.

*   Assign Original version flag
*   - Do not generate original version for C project with WBS lv3 only
    CLEAR LF_ORG_VERSN.
    IF <L_DATA>-KEY-PSPID(1) EQ 'P' OR
       <L_DATA>-LV2 EQ 'X'.
      LF_ORG_VERSN = GC_TRUE.
    ENDIF.

*   Create Budget
    ZCL_SDSPS_PROJECT=>CREATE_BUDGET( EXPORTING IS_KEY = <L_DATA>-KEY
                                                IF_CREATE_ORG_VERSN = LF_ORG_VERSN
                                                IT_BUDGET = LT_BUDGET
                                                IF_TEST    = UF_TEST
                                      IMPORTING ET_RETURN  = LT_RETURN ).

*   Assign Result
*   ------------------
*   Error
*   ------------------
    IF LT_RETURN IS NOT INITIAL.

      CS_COUNT-ERROR = CS_COUNT-ERROR + LF_LINES.

*     Collect Validation Error Result
      LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).

        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
        LS_MESSG-MSGID = <L_RETURN>-ID.
        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE.

*       Map error message with LS_DETAIL here if possible

        PERFORM F_COLLECT_RESULT3  USING  <L_DATA>-KEY
                                          LS_DETAIL
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDLOOP.
*   ------------------
*   Success
*   ------------------
    ELSE.

      CS_COUNT-SUCCS = CS_COUNT-SUCCS + LF_LINES.

      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      IF UF_TEST EQ GC_TRUE.
*       Text-i01: Test run successfully.
        LS_MESSG-MSGTX = TEXT-I01.
      ELSE.
*       Text-i06: Upload Original Plan/Budget successfully.
        LS_MESSG-MSGTX = TEXT-I06.
      ENDIF.

*     Assign Message to all input lines
      LOOP AT <L_DATA>-DETAIL ASSIGNING <L_DETAIL>.
        PERFORM F_COLLECT_RESULT3  USING  <L_DATA>-KEY
                                          <L_DETAIL>
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

  CS_COUNT-TOTAL = CS_COUNT-SUCCS + CS_COUNT-ERROR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_WAIT_PROJ_UNLOCK
*----------------------------------------------------------------------*
*  Wait Project Unlock
*----------------------------------------------------------------------*
FORM F_WAIT_PROJ_UNLOCK  USING  UF_PSPID TYPE TS_PROJ-PSPID.

  DATA:
    LF_WAIT_TIMES  TYPE  I VALUE 30.


  DO LF_WAIT_TIMES TIMES.
    CALL FUNCTION 'ENQUEUE_EC_PROJ'
      EXPORTING
        MODE_PROJ_ENQ  = 'E'
        MANDT          = SY-MANDT
        TYP            = 'P'
        PSPID          = UF_PSPID
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
        PSPID         = UF_PSPID.
    EXIT.
  ENDDO.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CREATE_PROJ_STRUC
*----------------------------------------------------------------------*
*  Create Project Structure
*----------------------------------------------------------------------*
FORM F_CREATE_PROJ_STRUC  USING  UT_PROJDEF  TYPE  TT_PROJDEF_DATA
                                 UT_WBS      TYPE  TT_WBS_DATA
                                 UF_TEST     TYPE  FLAG
                        CHANGING CT_RESULT   TYPE  TT_RESULT
                                 CS_COUNT    TYPE  TS_COUNT.

  DATA:
    LT_RESULT TYPE  TT_RESULT,
    LT_PROJ   TYPE  TT_PROJ.

  DATA:
    LS_COUNT   TYPE  TS_COUNT.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

* Show progress
* Text-p03 : Uploading data . . .
  MC_SHOW_PROGRESS 70 TEXT-P03.

* 1.2.1 Create Project Definition
  PERFORM F_CREATE_PROJDEF  USING  UT_PROJDEF
                                   UF_TEST
                          CHANGING LT_RESULT
                                   LS_COUNT.
* Collect Final Result
  INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  CS_COUNT-ERROR = CS_COUNT-ERROR + LS_COUNT-ERROR.
  CS_COUNT-SUCCS = CS_COUNT-SUCCS + LS_COUNT-SUCCS.
* Collect Recent Project List
  LT_PROJ = CORRESPONDING #( LT_RESULT DISCARDING DUPLICATES
                                MAPPING PSPID = PSPID ).

* 1.2.2 Create WBS
  PERFORM F_CREATE_WBS  USING  LT_PROJ
                               UT_WBS
                               UF_TEST
                      CHANGING LT_RESULT
                               LS_COUNT.
* Collect Final Result
  INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  CS_COUNT-ERROR = CS_COUNT-ERROR + LS_COUNT-ERROR.
  CS_COUNT-SUCCS = CS_COUNT-SUCCS + LS_COUNT-SUCCS.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHANGE_PROJ_STRUC
*----------------------------------------------------------------------*
*  Change Project Structure
*----------------------------------------------------------------------*
FORM F_CHANGE_PROJ_STRUC  USING  UT_PROJDEF  TYPE  TT_PROJDEF_DATA
                                 UT_WBS      TYPE  TT_WBS_DATA
                                 UF_TEST     TYPE  FLAG
                        CHANGING CT_RESULT   TYPE  TT_RESULT
                                 CS_COUNT    TYPE  TS_COUNT.

  DATA:
    LT_RESULT TYPE  TT_RESULT,
    LT_PROJ   TYPE  TT_PROJ.

  DATA:
    LS_COUNT   TYPE  TS_COUNT.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

* Show progress
* Text-p03 : Uploading data . . .
  MC_SHOW_PROGRESS 70 TEXT-P03.

* 1.3.1 Change Project Definition
  PERFORM F_CHANGE_PROJDEF  USING  UT_PROJDEF
                                   UF_TEST
                          CHANGING LT_RESULT
                                   LS_COUNT.
* Collect Final Result
  INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  CS_COUNT-ERROR = CS_COUNT-ERROR + LS_COUNT-ERROR.
  CS_COUNT-SUCCS = CS_COUNT-SUCCS + LS_COUNT-SUCCS.
* Collect Recent Project List
  LT_PROJ = CORRESPONDING #( LT_RESULT DISCARDING DUPLICATES
                                MAPPING PSPID = PSPID ).

* 1.3.2 Change WBS
  PERFORM F_CHANGE_WBS  USING  LT_PROJ
                               UT_WBS
                               UF_TEST
                      CHANGING LT_RESULT
                               LS_COUNT.
* Collect Final Result
  INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.
  CS_COUNT-ERROR = CS_COUNT-ERROR + LS_COUNT-ERROR.
  CS_COUNT-SUCCS = CS_COUNT-SUCCS + LS_COUNT-SUCCS.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHANGE_BUDGET
*----------------------------------------------------------------------*
*  Change Budget
*----------------------------------------------------------------------*
FORM F_CHANGE_BUDGET  USING  UT_DATA  TYPE  TT_BUDGET_DATA
                             UF_TEST  TYPE  FLAG
                    CHANGING CT_RESULT  TYPE  TT_RESULT
                             CS_COUNT   TYPE  TS_COUNT.

  DATA:
    LT_BUDGET TYPE  ZCL_SDSPS_PROJECT=>TT_BUDGET,
    LT_RETURN TYPE  BAPIRET2_TAB.

  DATA:
    LS_MESSG  TYPE  TS_MESSG,
    LS_DETAIL TYPE  TS_BUDGET_DETAIL.

  DATA:
    LF_LINES     TYPE  I.


* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

* Show progress
* Text-p03 : Uploading data . . .
  MC_SHOW_PROGRESS 70 TEXT-P03.

* Group Data by Project Def
  LOOP AT UT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).

    CLEAR: LT_BUDGET,
           LF_LINES.
    LOOP AT <L_DATA>-DETAIL ASSIGNING FIELD-SYMBOL(<L_DETAIL>).
      INSERT <L_DETAIL>-BUDGET INTO TABLE LT_BUDGET.
      LF_LINES = LF_LINES + 1.
    ENDLOOP.

*   Change Budget
    ZCL_SDSPS_PROJECT=>CHANGE_BUDGET( EXPORTING IS_KEY = <L_DATA>-KEY
                                                IT_BUDGET = LT_BUDGET
                                                IF_TEST    = UF_TEST
                                      IMPORTING ET_RETURN  = LT_RETURN ).

*   Assign Result
*   ------------------
*   Error
*   ------------------
    IF LT_RETURN IS NOT INITIAL.

      CS_COUNT-ERROR = CS_COUNT-ERROR + LF_LINES.

*     Collect Validation Error Result
      LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<L_RETURN>).

        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = <L_RETURN>-TYPE.
        LS_MESSG-MSGID = <L_RETURN>-ID.
        LS_MESSG-MSGNO = <L_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <L_RETURN>-MESSAGE.

*       Map error message with LS_DETAIL here if possible

        PERFORM F_COLLECT_RESULT3  USING  <L_DATA>-KEY
                                          LS_DETAIL
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDLOOP.
*   ------------------
*   Success
*   ------------------
    ELSE.

      CS_COUNT-SUCCS = CS_COUNT-SUCCS + LF_LINES.

      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZSDSPS01'.
      LS_MESSG-MSGNO = '000'.
      IF UF_TEST EQ GC_TRUE.
*       Text-i01: Test run successfully.
        LS_MESSG-MSGTX = TEXT-I01.
      ELSE.
*       Text-i08: Revise Plan/Budget successfully.
        LS_MESSG-MSGTX = TEXT-I08.
      ENDIF.

*     Assign Message to all input lines
      LOOP AT <L_DATA>-DETAIL ASSIGNING <L_DETAIL>.
        PERFORM F_COLLECT_RESULT3  USING  <L_DATA>-KEY
                                          <L_DETAIL>
                                          LS_MESSG
                                 CHANGING CT_RESULT.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

ENDFORM.
