*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0260
*  Creation Date      : 01.08.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDE029
*  Description        : This program is create/update quota plan table
*                       from input file template
*  Purpose            : Upload Quota Plan to
*                        1. ZSDSSDT016: Quota Plan Header
*                        2. ZSDSSDT018: Quota Plan Item
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0260.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_XLSX TYPE ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA.

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

TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSSDS056.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                 WITH DEFAULT KEY.

TYPES: TS_QUOTA_PLAN_ITM TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TS_QUOTA_PLAN_ITM,
       TT_QUOTA_PLAN_ITM TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TT_QUOTA_PLAN_ITM,
       TS_QUOTA_PLAN     TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TS_QUOTA_PLAN,
       TT_QUOTA_PLAN     TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TT_QUOTA_PLAN.

TYPES: BEGIN OF TS_DATA_TEXT,
         ROWNO        TYPE FPM_ROW,
         GRPNO        TYPE STRING,
         HDR_FLG      TYPE FLAG,
         MATNR        TYPE STRING,
         QUOTA_QTY    TYPE STRING,
         DAY_FIRST    TYPE STRING,
         DAY_LAST     TYPE STRING,
         QUOTAGRP     TYPE STRING,
         PORTION_PERC TYPE STRING,
         PORTION_QTY  TYPE STRING,
         ZDEL_FLG     TYPE STRING,
       END OF TS_DATA_TEXT.

TYPES: BEGIN OF TS_DATA,
         ROWNO        TYPE FPM_ROW,
         GRPNO        TYPE ZSDSDE_GRPNO,
         MATNR        TYPE MATNR,
         QUOTA_QTY    TYPE ZSDSDE_QUOTA_QTY,
         DAY_FIRST    TYPE DATUM,
         DAY_LAST     TYPE DATUM,
         QUOTAGRP     TYPE ZSDSDE_QUOTAGRP,
         PORTION_PERC TYPE ZSDSDE_PORTION,
         PORTION_QTY  TYPE ZSDSDE_PORTION_QTY,
         ZDEL_FLG     TYPE CHAR1,
         MSGTY        TYPE MSGTY,
         MSGTX        TYPE BAPI_MSG,
       END OF TS_DATA.

TYPES: BEGIN OF TS_DATA_HDR,
         ROWNO TYPE FPM_ROW,
         GRPNO TYPE ZSDSDE_GRPNO,
         MSGTY TYPE MSGTY,
         MSGTX TYPE BAPI_MSG.
         INCLUDE TYPE ZSDSSDT016.
TYPES: END OF TS_DATA_HDR.

TYPES: TT_DATA_HDR TYPE STANDARD TABLE OF TS_DATA_HDR.

TYPES: BEGIN OF TS_DATA_ITM,
         ROWNO TYPE FPM_ROW,
         GRPNO TYPE ZSDSDE_GRPNO,
         MSGTY TYPE MSGTY,
         MSGTX TYPE BAPI_MSG.
         INCLUDE TYPE ZSDSSDT018.
TYPES: END OF TS_DATA_ITM.
TYPES: TT_DATA_ITM TYPE STANDARD TABLE OF TS_DATA_ITM.

TYPES: BEGIN OF TS_HDR_KEY,
         GRPNO     TYPE ZSDSDE_GRPNO,
         MATNR     TYPE MATNR,
         DAY_FIRST TYPE DATUM,
         DAY_LAST  TYPE DATUM,
       END OF TS_HDR_KEY.

TYPES: TT_ZSDSSDT016 TYPE STANDARD TABLE OF ZSDSSDT016,
       TT_ZSDSSDT018 TYPE STANDARD TABLE OF ZSDSSDT018.

* *****************************
* Types for Excel Template
* !!!! Changing field name impact to validation logic !!!
* *****************************
TYPES: BEGIN OF TS_TEMPLATE1 ##NEEDED,
         GRPNO        TYPE ZSDSDE_GRPNO,
         MATNR        TYPE MATNR,
         QUOTA_QTY    TYPE ZSDSDE_QUOTA_QTY,
         DAY_FIRST    TYPE CHAR10,
         DAY_LAST     TYPE CHAR10,
         QUOTAGRP     TYPE ZSDSDE_QUOTAGRP,
         PORTION_PERC TYPE ZSDSDE_PORTION,
         PORTION_QTY  TYPE ZSDSDE_PORTION_QTY,
         DEL_FLG      TYPE CHAR1,
       END OF TS_TEMPLATE1.

TYPES: BEGIN OF TS_QUOTA_GROUP,
         QUOTAGRP TYPE ZSDSSDC013-QUOTAGRP,
         FLAG     TYPE C,
       END OF TS_QUOTA_GROUP.
TYPES: TT_QUOTA_GROUP TYPE TABLE OF TS_QUOTA_GROUP WITH EMPTY KEY.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE CHAR1 VALUE 'X',
  GC_TCODE TYPE SY-TCODE VALUE 'ZSDSSD018'.

* Excel Row/Column default
CONSTANTS: GC_BEGROW TYPE I VALUE 2,
           GC_BEGCOL TYPE I VALUE 1,
           GC_ENDROW TYPE I VALUE 999999,
           GC_ENDCOL TYPE I VALUE 9.

CONSTANTS: BEGIN OF GC_MSGTY,
             SUCCESS TYPE SY-MSGTY VALUE 'S',
             WARNING TYPE SY-MSGTY VALUE 'W',
             ERROR   TYPE SY-MSGTY VALUE 'E',
           END OF GC_MSGTY.

CONSTANTS: BEGIN OF GC_MODE,
             CREATE TYPE CHAR1 VALUE 'I',
             UPDATE TYPE CHAR1 VALUE 'U',
           END OF GC_MODE.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: GT_RESULT TYPE TT_RESULT                               ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA: GS_PROC TYPE TS_PROC_INFO                             ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA: GV_MODE TYPE CHAR1                                    ##NEEDED.

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
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS056'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I VALUE 15,
  GC_ALV_HEIGHT_1    TYPE  I VALUE 85.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF SY-BATCH IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = &1 ##NUMBER_OK
        TEXT       = &2.
  ELSE.
*   Show message step in background
    MESSAGE I000(38) WITH &2 SPACE SPACE SPACE.
  ENDIF.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN: FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.
  PARAMETERS: RB_NEW RADIOBUTTON GROUP G1,
              RB_UPD RADIOBUTTON GROUP G1.
  PARAMETERS: P_FILE TYPE STRING LOWER CASE.
SELECTION-SCREEN END OF BLOCK B1.
PARAMETERS: CB_TEST  TYPE CHAR1 AS CHECKBOX DEFAULT 'X'.
PARAMETERS: CB_CLEAR  TYPE CHAR1 AS CHECKBOX.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INCLUDE ZSDSSDR0260_CLASS.

INITIALIZATION.
  SSCRFIELDS-FUNCTXT_01 = TEXT-T02.

  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_FILE.

AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM F_EXPORT_EXCEL_TEMPLATE.
    WHEN 'ONLI'.
      PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDCASE.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT
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
*---------------------------------------------------------------------*
* Form F_VALIDATE_SELECTION_SCREEN
*---------------------------------------------------------------------*
* Validate selection screen
*---------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN.

  IF P_FILE IS INITIAL.
*   Error: Please enter a valid filename.
    MESSAGE E002(ZSDSCA01).
    RETURN.
  ENDIF.

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
*---------------------------------------------------------------------*
* Form F_PROCESS_DATA
*---------------------------------------------------------------------*
*  Processing data
*---------------------------------------------------------------------*
FORM F_PROCESS_DATA CHANGING CT_RESULT TYPE TT_RESULT
                             CS_PROC TYPE TS_PROC_INFO.

  DATA:
    LT_XLSX       TYPE TT_XLSX,
    LT_DATA_HDR   TYPE TT_DATA_HDR,
    LT_DATA_ITM   TYPE TT_DATA_ITM,
    LT_RESULT     TYPE TT_RESULT,
    LT_QUOTA_PLAN TYPE TT_QUOTA_PLAN.

  CLEAR: CT_RESULT,
         CS_PROC.

* -----------------------------
* Set mode create or change
* -----------------------------
  CASE GC_TRUE.
    WHEN RB_NEW.
      GV_MODE = GC_MODE-CREATE.
    WHEN RB_UPD.
      GV_MODE = GC_MODE-UPDATE.
  ENDCASE.

* -----------------------------
* Read File into Text table
* -----------------------------
  PERFORM F_READ_FILE CHANGING LT_XLSX.
  IF LT_XLSX IS INITIAL.
    RETURN.
  ENDIF.

* -----------------------------
* Prepare data to internal table Header/Item
* -----------------------------
  PERFORM F_ASSIGN_DATA USING LT_XLSX
                     CHANGING LT_DATA_HDR
                              LT_DATA_ITM.

* -----------------------------
* Get existing quota plan using key fields in header
* -----------------------------
  PERFORM F_GET_EXISTING_QUOTA_PLAN USING LT_DATA_HDR
                                 CHANGING LT_QUOTA_PLAN.

* -----------------------------
* Validate data within the same group
* If there at least one item contain error,
* then mark error for every items within the same group
* -----------------------------
  PERFORM F_VALIDATE_GROUP    USING LT_QUOTA_PLAN
                           CHANGING LT_DATA_HDR
                                    LT_DATA_ITM
                                    LT_RESULT.

* -----------------------------
* Create or Update Quota Plan
* -----------------------------
  CASE GV_MODE.
    WHEN GC_MODE-CREATE.
      PERFORM F_CREATE_QUOTA_PLAN USING LT_DATA_HDR
                                        LT_DATA_ITM
                                        LT_QUOTA_PLAN
                               CHANGING LT_RESULT.
    WHEN GC_MODE-UPDATE.
      PERFORM F_UPDATE_QUOTA_PLAN USING LT_DATA_HDR
                                        LT_DATA_ITM
                                        LT_QUOTA_PLAN
                               CHANGING LT_RESULT.
  ENDCASE.

* -----------------------------
* Collect Final Result
* -----------------------------
  INSERT LINES OF LT_RESULT INTO TABLE CT_RESULT.

  PERFORM F_ASSIGN_PROCESSING_INFO CHANGING CS_PROC.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_READ_FILE
*----------------------------------------------------------------------*
*  Read File into Text Table
*----------------------------------------------------------------------*
FORM F_READ_FILE  CHANGING CT_XLSX TYPE  TT_XLSX.

* Initialize Output
  CLEAR: CT_XLSX.

* Show progress
* Text-p01 : Reading Input file . . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = P_FILE
      IF_READ_ACTIVE_WORKSHEET    = 'X'
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
*  Form F_GET_TARGET_FIELD
*----------------------------------------------------------------------*
*  Get Target Field name based on column sequence
*----------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD  USING  UF_TYPE   TYPE  CHAR20
                                UF_INDEX  TYPE  I
                       CHANGING CF_FIELD  TYPE  CHAR40.

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
*---------------------------------------------------------------------*
* Form F_ASSIGN_DATA
*---------------------------------------------------------------------*
*  Assign Data from XLSX Sheet to Internal table
*---------------------------------------------------------------------*
FORM F_ASSIGN_DATA  USING UT_XLSX TYPE TT_XLSX
                 CHANGING CT_DATA_HDR TYPE TT_DATA_HDR
                          CT_DATA_ITM TYPE TT_DATA_ITM.

  DATA:
    LS_DATA      TYPE TS_DATA,
    LS_DATA_TEXT TYPE TS_DATA_TEXT,
    LS_DATA_HDR  TYPE TS_DATA_HDR,
    LS_DATA_ITM  TYPE TS_DATA_ITM,
    LS_PREV_HDR  TYPE TS_HDR_KEY.

  DATA:
    LF_COL_START TYPE I,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE TS_RESULT-ROWNO,
    LF_FNAME     TYPE CHAR40,
    LF_STRING    TYPE STRING,
    LF_MSGTX     TYPE BAPI_MSG.

  DATA:
  LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR.

* Initialize Output
  CLEAR: CT_DATA_HDR,
         CT_DATA_ITM.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 30 TEXT-P02.

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

  LF_COL_START = GC_BEGCOL.

  LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

*   Keep Row no
    LF_ROWNO = SY-TABIX.

    CHECK LF_ROWNO GE GC_BEGROW AND
          LF_ROWNO LE GC_ENDROW.

    CHECK <L_ROW> IS NOT INITIAL.

    IF LREF_STRUC IS INITIAL.
      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
    ENDIF.

    DATA : LV_QTY TYPE P DECIMALS 0.

*   -----------
*   Columns
*   -----------
    CLEAR: LS_DATA,
           LS_DATA_TEXT,
           LS_DATA_HDR,
           LS_DATA_ITM,
           LF_MSGTX.

    LS_DATA-ROWNO = LF_ROWNO.

    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

      CLEAR: LF_STRING.

      CHECK SY-TABIX GE GC_BEGCOL AND
            SY-TABIX LE GC_ENDCOL.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.

      PERFORM F_GET_TARGET_FIELD  USING  'TS_TEMPLATE1'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
*     Assign Source Field
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      LF_STRING = <L_FIELD>.

      CASE LF_FNAME.
        WHEN 'GRPNO'.
          IF LF_STRING IS INITIAL AND
             LS_PREV_HDR-GRPNO IS INITIAL.
*           Text-e01: Missing Record Running No in the file
            LF_MSGTX = TEXT-E01.
            EXIT.
          ENDIF.

          IF LF_STRING IS INITIAL.
            LS_DATA_TEXT-GRPNO = LS_PREV_HDR-GRPNO.
          ELSE.
            LS_DATA_TEXT-GRPNO = LF_STRING.
            LS_PREV_HDR-GRPNO = LF_STRING.
            LS_DATA_TEXT-HDR_FLG = ABAP_TRUE.
          ENDIF.

        WHEN 'MATNR'.
          LS_DATA_TEXT-MATNR = LF_STRING.

        WHEN 'QUOTA_QTY'.
          LS_DATA_TEXT-QUOTA_QTY = LF_STRING.

        WHEN 'DAY_FIRST'.
          LS_DATA_TEXT-DAY_FIRST = LF_STRING.

        WHEN 'DAY_LAST'.
          LS_DATA_TEXT-DAY_LAST = LF_STRING.

        WHEN 'QUOTAGRP'.
          LS_DATA_TEXT-QUOTAGRP = LF_STRING.

        WHEN 'PORTION_PERC'.
          LS_DATA_TEXT-PORTION_PERC = LF_STRING.

        WHEN 'PORTION_QTY'.
          LS_DATA_TEXT-PORTION_QTY = LF_STRING.

        WHEN 'DEL_FLG'.
          LS_DATA_TEXT-ZDEL_FLG = LF_STRING.

      ENDCASE.
    ENDLOOP.

*   Validate input at row level
    PERFORM F_VALIDATE_INPUT  USING LS_DATA_TEXT
                           CHANGING LS_DATA
                                    LF_MSGTX.

    IF LF_MSGTX IS NOT INITIAL.
      LS_DATA-MSGTY = 'E'.
      LS_DATA-MSGTX = LF_MSGTX.
    ENDIF.

*   Collect Data

    IF LS_DATA_TEXT-HDR_FLG IS NOT INITIAL.
      LV_QTY = LS_DATA-QUOTA_QTY.
    ENDIF.

    IF LS_DATA-PORTION_QTY IS INITIAL AND
       LS_DATA-PORTION_PERC IS NOT INITIAL.
      LS_DATA-PORTION_QTY = ( LV_QTY * LS_DATA-PORTION_PERC ) / 100.
    ENDIF.

    IF LS_DATA_TEXT-HDR_FLG IS NOT INITIAL.
      MOVE-CORRESPONDING LS_DATA TO LS_DATA_HDR.
      MOVE-CORRESPONDING LS_DATA TO LS_PREV_HDR.
      CLEAR: LS_DATA_HDR-ZDEL_FLG.
      INSERT LS_DATA_HDR INTO TABLE CT_DATA_HDR.

      MOVE-CORRESPONDING LS_DATA TO LS_DATA_ITM.
      INSERT LS_DATA_ITM INTO TABLE CT_DATA_ITM.
    ELSE.
      MOVE-CORRESPONDING LS_DATA TO LS_DATA_ITM.
      MOVE-CORRESPONDING LS_PREV_HDR TO LS_DATA_ITM.
      INSERT LS_DATA_ITM INTO TABLE CT_DATA_ITM.
    ENDIF.

  ENDLOOP.

  SORT CT_DATA_HDR BY ROWNO.
  SORT CT_DATA_ITM BY GRPNO ROWNO.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_INPUT
*---------------------------------------------------------------------*
* Validate input at row level
*---------------------------------------------------------------------*
FORM F_VALIDATE_INPUT  USING    US_DATA_TEXT TYPE TS_DATA_TEXT
                       CHANGING CS_DATA TYPE TS_DATA
                                CF_MSGTX TYPE BAPI_MSG.

  DATA: LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION,
        LF_INVALID    TYPE FLAG.

  CLEAR: CS_DATA,
         CF_MSGTX.

  CREATE OBJECT LREF_VALIDATE.

  MOVE-CORRESPONDING US_DATA_TEXT TO CS_DATA.

  PERFORM F_CONVERT_DATE_FORMAT USING US_DATA_TEXT-DAY_FIRST
                             CHANGING CS_DATA-DAY_FIRST.

  PERFORM F_CONVERT_DATE_FORMAT USING US_DATA_TEXT-DAY_LAST
                             CHANGING CS_DATA-DAY_LAST.

* --------------------
* Validate required fields
* --------------------
* Required Field: Material Number
  IF US_DATA_TEXT-HDR_FLG EQ ABAP_TRUE AND
     US_DATA_TEXT-MATNR IS INITIAL.
*   Text-e14: Missing Material Number in the file
    CF_MSGTX = TEXT-E14.
    RETURN.
  ENDIF.

* Required Field: Day First
  IF US_DATA_TEXT-HDR_FLG EQ ABAP_TRUE AND
     US_DATA_TEXT-DAY_FIRST IS INITIAL.
*   Text-e15: Missing Day First in the file
    CF_MSGTX = TEXT-E15.
    RETURN.
  ENDIF.

* Required Field: Day Last
  IF US_DATA_TEXT-HDR_FLG EQ ABAP_TRUE AND
     US_DATA_TEXT-DAY_LAST IS INITIAL.
*   Text-e17: Missing Day Last in the file
    CF_MSGTX = TEXT-E17.
    RETURN.
  ENDIF.

* Required Field: Quota Group
  IF US_DATA_TEXT-QUOTAGRP IS INITIAL.
*   Text-e09: Missing Quota Group in the file
    CF_MSGTX = TEXT-E09.
    RETURN.
  ENDIF.

* Required Field: Portion %
  IF US_DATA_TEXT-PORTION_PERC IS INITIAL.
*   Text-e09: Missing Portion(%) in the file
    CF_MSGTX = TEXT-E10.
    RETURN.
  ENDIF.

* --------------------
* Material Number
  PERFORM F_VALIDATE_MATNR USING US_DATA_TEXT-MATNR
                        CHANGING CS_DATA-MATNR
                                 CF_MSGTX.
  IF CF_MSGTX IS NOT INITIAL.
    RETURN.
  ENDIF.

* Plan Quota Stock
  IF US_DATA_TEXT-HDR_FLG EQ ABAP_TRUE AND
     US_DATA_TEXT-QUOTA_QTY IS INITIAL.
*   Text-e02: Missing Plan Quota Stock in the file
    CF_MSGTX = TEXT-E02.
    RETURN.
  ENDIF.

  PERFORM F_VALIDATE_QUAN USING US_DATA_TEXT-QUOTA_QTY
                       CHANGING CS_DATA-QUOTA_QTY
                                CF_MSGTX.
  IF CF_MSGTX IS NOT INITIAL.
    RETURN.
  ENDIF.

* Day First
  LREF_VALIDATE->VALIDATE_DATE( EXPORTING IF_INPUT = US_DATA_TEXT-DAY_FIRST
                                          IF_FORMAT = SPACE
                                IMPORTING EF_OUTPUT = CS_DATA-DAY_FIRST
                                          EF_INVALID = LF_INVALID ).

  IF LF_INVALID IS NOT INITIAL.
*   Text-e03: Invalid date value (Format DD.MM.YYYY):
    CF_MSGTX = |{ TEXT-E03 } { US_DATA_TEXT-DAY_FIRST }|.
    RETURN.
  ENDIF.

* Day Last
  LREF_VALIDATE->VALIDATE_DATE( EXPORTING IF_INPUT = US_DATA_TEXT-DAY_LAST
                                          IF_FORMAT = SPACE
                                IMPORTING EF_OUTPUT = CS_DATA-DAY_LAST
                                          EF_INVALID = LF_INVALID ).

  IF LF_INVALID IS NOT INITIAL.
*   Text-e03: Invalid date value (Format DD.MM.YYYY):
    CF_MSGTX = |{ TEXT-E03 } { US_DATA_TEXT-DAY_LAST }|.
    RETURN.
  ENDIF.

* If Day first > Day last, Then Error
  IF CS_DATA-DAY_FIRST > CS_DATA-DAY_LAST.
*   Text-e18: Day First is greater than Day Last
    CF_MSGTX = TEXT-E18.
    RETURN.
  ENDIF.

* If Day first and Day last in the different month, the error
  IF CS_DATA-DAY_FIRST(6) <> CS_DATA-DAY_LAST(6).
*   Text-e21: Day First and Day Last must be in the same month
    CF_MSGTX = TEXT-E21.
    RETURN.
  ENDIF.

* Quota Group
  PERFORM F_VALIDATE_QUOTAGRP USING US_DATA_TEXT-QUOTAGRP
                        CHANGING CS_DATA-QUOTAGRP
                                 CF_MSGTX.
  IF CF_MSGTX IS NOT INITIAL.
    RETURN.
  ENDIF.

* Portion (%)
  PERFORM F_VALIDATE_PERCENT USING US_DATA_TEXT-PORTION_PERC
                          CHANGING CS_DATA-PORTION_PERC
                                   CF_MSGTX.
  IF CF_MSGTX IS NOT INITIAL.
    RETURN.
  ENDIF.

* Delete Flag
  LREF_VALIDATE->VALIDATE_FLAG( EXPORTING IF_INPUT = US_DATA_TEXT-ZDEL_FLG
                                IMPORTING EF_OUTPUT  = CS_DATA-ZDEL_FLG
                                          EF_INVALID = LF_INVALID ).
  IF LF_INVALID IS NOT INITIAL.
*   Text-e04: Invalid flag:
    CF_MSGTX = |{ TEXT-E04 } { US_DATA_TEXT-ZDEL_FLG }|.
    RETURN.
  ENDIF.
* Upload new quota, delete flag must equal SPACE
  IF GV_MODE EQ GC_MODE-CREATE AND
     CS_DATA-ZDEL_FLG IS NOT INITIAL.
*   Text-e12: For option create new quota plan, delete indicator must be blank
    CF_MSGTX = TEXT-E12.
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_COLLECT_RESULT
*----------------------------------------------------------------------*
*  Collect result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT USING US_DATA TYPE TS_DATA
                   CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LS_RESULT TYPE TS_RESULT.

  CLEAR LS_RESULT.

  LS_RESULT-ROWNO = US_DATA-ROWNO.
  MOVE-CORRESPONDING US_DATA TO LS_RESULT.

  CASE US_DATA-MSGTY.
    WHEN GC_MSGTY-SUCCESS.
      LS_RESULT-STATU = ICON_LED_GREEN.
    WHEN GC_MSGTY-WARNING.
      LS_RESULT-STATU = ICON_LED_YELLOW.
    WHEN GC_MSGTY-ERROR.
      LS_RESULT-STATU = ICON_LED_RED.
    WHEN OTHERS.
      LS_RESULT-STATU = ICON_LED_INACTIVE.
  ENDCASE.

  INSERT LS_RESULT INTO TABLE CT_RESULT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_VALIDATE_MATNR
*----------------------------------------------------------------------*
*  Validate Material Number
*----------------------------------------------------------------------*
FORM F_VALIDATE_MATNR  USING  UF_STRING TYPE STRING
                     CHANGING CF_MATNR  TYPE MARA-MATNR
                              CF_MSGTX  TYPE BAPI_MSG.

  TYPES: BEGIN OF LTS_MARA,
           MATNR TYPE MARA-MATNR,
         END OF LTS_MARA.
  TYPES: LTT_MARA TYPE SORTED TABLE OF LTS_MARA
  WITH UNIQUE KEY MATNR.

  STATICS:
    LT_MARA TYPE LTT_MARA,
    LS_MARA TYPE LTS_MARA.

  DATA:
  LF_MATNR  TYPE  MARA-MATNR.

* Initialize Output
  CLEAR: CF_MSGTX.

* Only when not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT        = UF_STRING
    IMPORTING
      OUTPUT       = LF_MATNR
    EXCEPTIONS
      LENGTH_ERROR = 1
      OTHERS       = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            INTO CF_MSGTX.
    RETURN.
  ENDIF.

* Check Buffer
  IF LS_MARA-MATNR NE LF_MATNR.
*   Validate with Memory
    READ TABLE LT_MARA INTO LS_MARA
                       WITH KEY MATNR = LF_MATNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_MARA.
*     Validate with Database
      SELECT SINGLE MATNR                            "#EC CI_SEL_NESTED
        INTO LS_MARA
        FROM MARA
      WHERE MATNR  EQ  LF_MATNR.
      IF SY-SUBRC NE 0.
*       Text-e05: Invalid Material Number:
        CONCATENATE TEXT-E05 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_MARA INTO TABLE LT_MARA.
    ENDIF.

  ENDIF.

* Assign Output
  CF_MATNR = LF_MATNR.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_QUAN
*---------------------------------------------------------------------*
* Validate Quota Plan Stock (Quantity)
*---------------------------------------------------------------------*
FORM F_VALIDATE_QUAN  USING    UF_STRING TYPE STRING
                      CHANGING CF_QUOTA_QTY TYPE ZSDSDE_QUOTA_QTY
                               CF_MSGTX  TYPE BAPI_MSG.
  DATA: LREF_ERROR TYPE REF TO CX_SY_CONVERSION_NO_NUMBER.

  CLEAR: CF_QUOTA_QTY,
         CF_MSGTX.

  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  TRY.
      CF_QUOTA_QTY = UF_STRING.
    CATCH CX_SY_CONVERSION_NO_NUMBER INTO LREF_ERROR.
      CF_MSGTX = LREF_ERROR->GET_TEXT( ).
      RETURN.
  ENDTRY.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_PERCENT
*---------------------------------------------------------------------*
* Validate Portion Percentage ( 0.00% - 100.00%)
*---------------------------------------------------------------------*
FORM F_VALIDATE_PERCENT  USING    UF_STRING TYPE STRING
                         CHANGING CF_PERC   TYPE ZSDSDE_PORTION
                                  CF_MSGTX  TYPE BAPI_MSG.
  DATA: LREF_ERROR TYPE REF TO CX_SY_CONVERSION_NO_NUMBER.

  DATA: LF_COUNT  TYPE I,
        LF_OFFSET TYPE I ##NEEDED.

  CLEAR: CF_PERC,
         CF_MSGTX.

  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  IF NOT UF_STRING CO '1234567890. '.
*   Text-e07: Invalid Portion (%):
    CONCATENATE TEXT-E07 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Find Decimal
  CLEAR: LF_COUNT, LF_OFFSET.
  FIND ALL OCCURRENCES OF '.' IN UF_STRING MATCH COUNT LF_COUNT
                                          MATCH OFFSET LF_OFFSET.
  IF LF_COUNT GT 1.
*   Text-e07: Invalid Portion (%):
    CONCATENATE TEXT-E07 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  TRY.
      CF_PERC = UF_STRING.
    CATCH CX_SY_CONVERSION_NO_NUMBER INTO LREF_ERROR.
      CF_MSGTX = LREF_ERROR->GET_TEXT( ).
      RETURN.
  ENDTRY.

* Percent must be between 0 - 100
  IF ( CF_PERC LT 0 OR
       CF_PERC GT 100 ).
*   Text-e07: Invalid Portion (%):
    CONCATENATE TEXT-E07 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_QUOTAGRP
*&---------------------------------------------------------------------*
*& Validate Quota Group
*&---------------------------------------------------------------------*
FORM F_VALIDATE_QUOTAGRP  USING UF_STRING TYPE STRING
                       CHANGING CF_QUOTAGRP  TYPE ANY
                                CF_MSGTX  TYPE BAPI_MSG.

  TYPES: BEGIN OF LTS_QUOTAGRP,
           QUOTAGRP      TYPE ZSDSSDC013-QUOTAGRP,
           QUOTAGRP_DESC TYPE ZSDSSDC013-QUOTAGRP_DESC,
         END OF LTS_QUOTAGRP.
  TYPES: LTT_QUOTAGRP TYPE SORTED TABLE OF LTS_QUOTAGRP
  WITH UNIQUE KEY QUOTAGRP.

  STATICS:
    LS_QUOTAGRP TYPE LTS_QUOTAGRP,
    LT_QUOTAGRP TYPE LTT_QUOTAGRP.

  DATA: LF_QUOTAGRP TYPE ZSDSSDC013-QUOTAGRP.

* Initialize Output
  CLEAR: CF_QUOTAGRP,
         CF_MSGTX.

* Only when value exist
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_QUOTAGRP = UF_STRING.

  IF LS_QUOTAGRP-QUOTAGRP NE LF_QUOTAGRP.
*   Validate with Memory
    READ TABLE LT_QUOTAGRP INTO LS_QUOTAGRP
                        WITH KEY QUOTAGRP = LF_QUOTAGRP
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_QUOTAGRP.
*     Validate with Database
      SELECT QUOTAGRP QUOTAGRP_DESC
        INTO LS_QUOTAGRP
        FROM ZSDSSDC013
        UP TO 1 ROWS
       WHERE QUOTAGRP EQ LF_QUOTAGRP
         AND ZDEL_FLG EQ SPACE
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e06 : Invalid Quota Group:
        CONCATENATE TEXT-E06 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_QUOTAGRP INTO TABLE LT_QUOTAGRP.
    ENDIF.
  ENDIF.

* Assign Output
  CF_QUOTAGRP = LS_QUOTAGRP-QUOTAGRP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_EXISTING_QUOTA_PLAN
*---------------------------------------------------------------------*
* Get existing quota plan from table ZSDSSDT016
*---------------------------------------------------------------------*
FORM F_GET_EXISTING_QUOTA_PLAN  USING    UT_DATA_HDR TYPE TT_DATA_HDR
                                CHANGING CT_QUOTA_PLAN TYPE TT_QUOTA_PLAN.

  DATA: LT_DATA_HDR     TYPE TT_DATA_HDR,
        LT_KEYTAB       TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TT_KEYTAB,
        LF_INCL_DEL_FLG TYPE FLAG.

  CLEAR: CT_QUOTA_PLAN.

  IF UT_DATA_HDR IS INITIAL.
    RETURN.
  ENDIF.

** Keep only records which not error
  LT_DATA_HDR[] = UT_DATA_HDR[].
  DELETE LT_DATA_HDR WHERE MSGTY EQ GC_MSGTY-ERROR.

  IF LT_DATA_HDR IS INITIAL.
    RETURN.
  ENDIF.

* Prepare key data table
  LT_KEYTAB = CORRESPONDING #( LT_DATA_HDR ).

  CASE GV_MODE.
    WHEN GC_MODE-CREATE.
      LF_INCL_DEL_FLG = ABAP_TRUE.
    WHEN GC_MODE-UPDATE.
      LF_INCL_DEL_FLG = ABAP_FALSE.
  ENDCASE.

* Get existing records in quota plan tables
  CALL METHOD ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_BY_KEY_MULTI
    EXPORTING
      IT_KEYTAB       = LT_KEYTAB
      IF_INCL_DEL_FLG = LF_INCL_DEL_FLG
    IMPORTING
      ET_QUOTA_PLAN   = CT_QUOTA_PLAN.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CREATE_QUOTA_PLAN
*---------------------------------------------------------------------*
* Prepare to create quota plan
*---------------------------------------------------------------------*
FORM F_CREATE_QUOTA_PLAN  USING    UT_DATA_HDR TYPE TT_DATA_HDR
                                   UT_DATA_ITM TYPE TT_DATA_ITM
                                   UT_QUOTA_PLAN TYPE TT_QUOTA_PLAN ##NEEDED
                          CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LS_ZSDSSDT016 TYPE ZSDSSDT016,
        LS_ZSDSSDT018 TYPE ZSDSSDT018,
        LF_DB_UPDATED TYPE FLAG.

  DATA: LT_DATA_HDR   TYPE TT_DATA_HDR,
        LT_DATA_ITM   TYPE TT_DATA_ITM,
        LT_ZSDSSDT016 TYPE TT_ZSDSSDT016, "Header
        LT_ZSDSSDT018 TYPE TT_ZSDSSDT018. "Item

* Show progress
* Text-p03 : Uploading data...
  MC_SHOW_PROGRESS 40 TEXT-P03.

  LT_DATA_HDR[] = UT_DATA_HDR[].
  LT_DATA_ITM[] = UT_DATA_ITM[].

  DELETE LT_DATA_HDR WHERE MSGTY EQ GC_MSGTY-ERROR.

  LOOP AT LT_DATA_HDR ASSIGNING FIELD-SYMBOL(<L_HDR>).

    CLEAR: LS_ZSDSSDT016.
    MOVE-CORRESPONDING <L_HDR> TO LS_ZSDSSDT016.
    LS_ZSDSSDT016-ZCRT_DATE = SY-DATUM.
    LS_ZSDSSDT016-ZCRT_TIME = SY-UZEIT.
    LS_ZSDSSDT016-ZCRT_USER = SY-UNAME.
    LS_ZSDSSDT016-ZCRT_PGM = SY-REPID.

    APPEND LS_ZSDSSDT016 TO LT_ZSDSSDT016.

    LOOP AT LT_DATA_ITM ASSIGNING FIELD-SYMBOL(<L_ITM>)
                        WHERE GRPNO EQ <L_HDR>-GRPNO.
      CLEAR: LS_ZSDSSDT018.
      MOVE-CORRESPONDING <L_ITM> TO LS_ZSDSSDT018.
      LS_ZSDSSDT018-ZCRT_DATE   = SY-DATUM.
      LS_ZSDSSDT018-ZCRT_TIME   = SY-UZEIT.
      LS_ZSDSSDT018-ZCRT_USER   = SY-UNAME.
      LS_ZSDSSDT018-ZCRT_PGM    = SY-REPID.

      APPEND LS_ZSDSSDT018 TO LT_ZSDSSDT018.

      READ TABLE CT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                           WITH KEY GRPNO = <L_HDR>-GRPNO
                                    MATNR = <L_HDR>-MATNR
                                    DAY_FIRST = <L_HDR>-DAY_FIRST
                                    DAY_LAST = <L_HDR>-DAY_LAST
                                    QUOTAGRP = <L_ITM>-QUOTAGRP.
      IF SY-SUBRC EQ 0.
        <L_RESULT>-ZCRT_DATE = LS_ZSDSSDT018-ZCRT_DATE.
        <L_RESULT>-ZCRT_TIME = LS_ZSDSSDT018-ZCRT_TIME.
        <L_RESULT>-ZCRT_USER = LS_ZSDSSDT018-ZCRT_USER.
        <L_RESULT>-ZCRT_PGM = LS_ZSDSSDT018-ZCRT_PGM.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  CLEAR: LF_DB_UPDATED.
  IF CB_TEST EQ SPACE.
    IF LT_ZSDSSDT016 IS NOT INITIAL AND
       LT_ZSDSSDT018 IS NOT INITIAL.
      INSERT ZSDSSDT016 FROM TABLE LT_ZSDSSDT016.
      IF CB_CLEAR EQ ABAP_TRUE.
        PERFORM F_SET_CONFIRM_QTY USING LT_ZSDSSDT016.
      ENDIF.
      IF SY-SUBRC EQ 0.
        INSERT ZSDSSDT018 FROM TABLE LT_ZSDSSDT018.
        SORT LT_ZSDSSDT018 BY MATNR.
        LCL_DATA=>SET_EMPTY_QUOTA( LT_ZSDSSDT018 ).
        IF SY-SUBRC EQ 0.
          COMMIT WORK AND WAIT.
          LF_DB_UPDATED = ABAP_TRUE.
        ELSE.
          ROLLBACK WORK.                               "#EC CI_ROLLBACK
          RETURN.
        ENDIF.
      ELSE.
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  IF CB_TEST EQ ABAP_TRUE OR
     LF_DB_UPDATED EQ ABAP_TRUE.
    PERFORM F_SET_SUCESS_RESULT CHANGING CT_RESULT.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_UPDATE_QUOTA_PLAN
*---------------------------------------------------------------------*
* Prepare to update quota plan
*---------------------------------------------------------------------*
FORM F_UPDATE_QUOTA_PLAN  USING    UT_DATA_HDR TYPE TT_DATA_HDR
                                   UT_DATA_ITM TYPE TT_DATA_ITM
                                   UT_QUOTA_PLAN TYPE TT_QUOTA_PLAN
                          CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LS_ZSDSSDT016 TYPE ZSDSSDT016,
        LS_ZSDSSDT018 TYPE ZSDSSDT018,
        LF_DB_UPDATED TYPE FLAG.

  DATA: LT_DATA_HDR   TYPE TT_DATA_HDR,
        LT_DATA_ITM   TYPE TT_DATA_ITM,
        LT_ZSDSSDT016 TYPE TT_ZSDSSDT016, "Header
        LT_ZSDSSDT018 TYPE TT_ZSDSSDT018. "Item

  FIELD-SYMBOLS: <L_QUOTA_PLAN> TYPE TS_QUOTA_PLAN.

* Show progress
* Text-p03 : Uploading data...
  MC_SHOW_PROGRESS 40 TEXT-P03.

  LT_DATA_HDR[] = UT_DATA_HDR[].
  LT_DATA_ITM[] = UT_DATA_ITM[].

  DELETE LT_DATA_HDR WHERE MSGTY EQ GC_MSGTY-ERROR.

  LOOP AT LT_DATA_HDR ASSIGNING FIELD-SYMBOL(<L_HDR>).

    CLEAR: LS_ZSDSSDT016.
    UNASSIGN: <L_QUOTA_PLAN>.

    MOVE-CORRESPONDING <L_HDR> TO LS_ZSDSSDT016.
    LS_ZSDSSDT016-ZUPD_DATE = SY-DATUM.
    LS_ZSDSSDT016-ZUPD_TIME = SY-UZEIT.
    LS_ZSDSSDT016-ZUPD_USER = SY-UNAME.
    LS_ZSDSSDT016-ZUPD_PGM = SY-REPID.

*   Read existing quota plan header
    READ TABLE UT_QUOTA_PLAN ASSIGNING <L_QUOTA_PLAN>
                             WITH KEY MATNR = <L_HDR>-MATNR
                                      DAY_FIRST = <L_HDR>-DAY_FIRST
                                      DAY_LAST = <L_HDR>-DAY_LAST.
    IF SY-SUBRC EQ 0.
      LS_ZSDSSDT016-ZCRT_DATE = <L_QUOTA_PLAN>-ZCRT_DATE.
      LS_ZSDSSDT016-ZCRT_TIME = <L_QUOTA_PLAN>-ZCRT_TIME.
      LS_ZSDSSDT016-ZCRT_USER = <L_QUOTA_PLAN>-ZCRT_USER.
      LS_ZSDSSDT016-ZCRT_PGM = <L_QUOTA_PLAN>-ZCRT_PGM.
    ENDIF.

    APPEND LS_ZSDSSDT016 TO LT_ZSDSSDT016.

    LOOP AT LT_DATA_ITM ASSIGNING FIELD-SYMBOL(<L_ITM>)
                        WHERE GRPNO EQ <L_HDR>-GRPNO.
      CLEAR: LS_ZSDSSDT018.
      MOVE-CORRESPONDING <L_ITM> TO LS_ZSDSSDT018.
      LS_ZSDSSDT018-ZUPD_DATE = SY-DATUM.
      LS_ZSDSSDT018-ZUPD_TIME = SY-UZEIT.
      LS_ZSDSSDT018-ZUPD_USER = SY-UNAME.
      LS_ZSDSSDT018-ZUPD_PGM = SY-REPID.

      IF <L_QUOTA_PLAN> IS ASSIGNED.
        READ TABLE <L_QUOTA_PLAN>-ITEM ASSIGNING FIELD-SYMBOL(<L_QUOTA_PLAN_ITM>)
                                       WITH KEY QUOTAGRP = <L_ITM>-QUOTAGRP.
        IF SY-SUBRC EQ 0.                    "Update existing item
          LS_ZSDSSDT018-ZCRT_DATE = <L_QUOTA_PLAN_ITM>-ZCRT_DATE.
          LS_ZSDSSDT018-ZCRT_TIME = <L_QUOTA_PLAN_ITM>-ZCRT_TIME.
          LS_ZSDSSDT018-ZCRT_USER = <L_QUOTA_PLAN_ITM>-ZCRT_USER.
          LS_ZSDSSDT018-ZCRT_PGM = <L_QUOTA_PLAN_ITM>-ZCRT_PGM.
        ELSE.                                "Create new item
          LS_ZSDSSDT018-ZCRT_DATE = SY-DATUM.
          LS_ZSDSSDT018-ZCRT_TIME = SY-UZEIT.
          LS_ZSDSSDT018-ZCRT_USER = SY-UNAME.
          LS_ZSDSSDT018-ZCRT_PGM = SY-REPID.
        ENDIF.
      ENDIF.

      APPEND LS_ZSDSSDT018 TO LT_ZSDSSDT018.

      READ TABLE CT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                           WITH KEY GRPNO = <L_HDR>-GRPNO
                                    MATNR = <L_HDR>-MATNR
                                    DAY_FIRST = <L_HDR>-DAY_FIRST
                                    DAY_LAST = <L_HDR>-DAY_LAST
                                    QUOTAGRP = <L_ITM>-QUOTAGRP.
      IF SY-SUBRC EQ 0.
        <L_RESULT>-ZCRT_DATE = LS_ZSDSSDT018-ZCRT_DATE.
        <L_RESULT>-ZCRT_TIME = LS_ZSDSSDT018-ZCRT_TIME.
        <L_RESULT>-ZCRT_USER = LS_ZSDSSDT018-ZCRT_USER.
        <L_RESULT>-ZCRT_PGM = LS_ZSDSSDT018-ZCRT_PGM.
        <L_RESULT>-ZUPD_DATE = LS_ZSDSSDT018-ZUPD_DATE.
        <L_RESULT>-ZUPD_TIME = LS_ZSDSSDT018-ZUPD_TIME.
        <L_RESULT>-ZUPD_USER = LS_ZSDSSDT018-ZUPD_USER.
        <L_RESULT>-ZUPD_PGM = LS_ZSDSSDT018-ZUPD_PGM.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  CLEAR: LF_DB_UPDATED.
  IF CB_TEST EQ SPACE.
    IF LT_ZSDSSDT016 IS NOT INITIAL AND
       LT_ZSDSSDT018 IS NOT INITIAL.
      MODIFY ZSDSSDT016 FROM TABLE LT_ZSDSSDT016.
      IF CB_CLEAR EQ ABAP_TRUE.
        PERFORM F_SET_CONFIRM_QTY USING LT_ZSDSSDT016.
      ENDIF.
      IF SY-SUBRC EQ 0.
        MODIFY ZSDSSDT018 FROM TABLE LT_ZSDSSDT018.
        SORT LT_ZSDSSDT018 BY MATNR.
        LCL_DATA=>SET_EMPTY_QUOTA( LT_ZSDSSDT018 ).
        IF SY-SUBRC EQ 0.
          COMMIT WORK AND WAIT.
          LF_DB_UPDATED = ABAP_TRUE.
        ELSE.
          ROLLBACK WORK.                               "#EC CI_ROLLBACK
          RETURN.
        ENDIF.
      ELSE.
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  IF CB_TEST EQ ABAP_TRUE OR
     LF_DB_UPDATED EQ ABAP_TRUE.
    PERFORM F_SET_SUCESS_RESULT CHANGING CT_RESULT.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_DISPLAY_RESULT
*---------------------------------------------------------------------*
*  Display Processing Result
*---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING UT_RESULT TYPE TT_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Enable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
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

* Determine layout
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.
  CS_LAYOUT-NO_TOTLINE = GC_TRUE.

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

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<L_FCAT>).
    CASE <L_FCAT>-FIELDNAME.
      WHEN 'ROWNO'.
        <L_FCAT>-NO_OUT = ABAP_TRUE.
      WHEN 'PORTION_PERC'.
        <L_FCAT>-DO_SUM = ABAP_TRUE.
      WHEN 'PORTION_QTY'.
        <L_FCAT>-DO_SUM = ABAP_TRUE.
      WHEN 'ZCRT_PGM' OR
           'ZUPD_PGM'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN OTHERS.
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
    LC_SORT1 TYPE  LVC_FNAME VALUE 'ROWNO',
    LC_SORT2 TYPE  LVC_FNAME VALUE 'GRPNO',
    LC_SORT3 TYPE  LVC_FNAME VALUE 'MATNR',
    LC_SORT4 TYPE  LVC_FNAME VALUE 'DAY_FIRST',
    LC_SORT5 TYPE  LVC_FNAME VALUE 'DAY_LAST'.

  DATA:
  LS_SORT  TYPE  LVC_S_SORT.

* Initialize Output
  CLEAR: CT_SORT.

* Sort by ROWNO
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by GRPNO
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = GC_TRUE.
  APPEND LS_SORT TO CT_SORT.

* Sort by MATNR
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 3.
  LS_SORT-FIELDNAME = LC_SORT3.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by DAY_FIRST
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 4.
  LS_SORT-FIELDNAME = LC_SORT4.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by DAY_LAST
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 5.
  LS_SORT-FIELDNAME = LC_SORT5.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_ASSIGN_PROCESSING_INFO
*---------------------------------------------------------------------*
* Assign Processing Information
*---------------------------------------------------------------------*
FORM F_ASSIGN_PROCESSING_INFO  CHANGING CS_PROC TYPE TS_PROC_INFO.

* Assign Processing Info
  CS_PROC-DATUM = SY-DATUM.
  CS_PROC-UZEIT = SY-UZEIT.
  CS_PROC-UNAME = SY-UNAME.
  CS_PROC-IFILE = P_FILE.

  CASE GC_TRUE.
    WHEN RB_NEW.
*     Text-X03: Upload New Quota Plan
      CS_PROC-MODE = TEXT-X03.
    WHEN RB_UPD.
*     Text-X04: Update Quota Plan
      CS_PROC-MODE = TEXT-X04.
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
* Text-i01: Processing completed.
  MESSAGE S000(ZSDSCA01) WITH TEXT-I01 SPACE SPACE SPACE.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_TOP_OF_PAGE_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '15%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '85%'.

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
      WIDTH         = '80%'
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

ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_PRINT_TOP_OF_PAGE_1
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

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CONVERT_DATE_FORMAT
*---------------------------------------------------------------------*
* Convert date from DD.MM.YYYY to internal format YYYYMMDD
*---------------------------------------------------------------------*
FORM F_CONVERT_DATE_FORMAT  USING    UF_DATE_TEXT TYPE STRING
                            CHANGING CF_DATE TYPE SY-DATUM.

  CLEAR: CF_DATE.

  IF UF_DATE_TEXT IS INITIAL OR
     STRLEN( UF_DATE_TEXT ) NE 10.
    RETURN.
  ENDIF.

  CF_DATE(4) = UF_DATE_TEXT+6(4).
  CF_DATE+4(2) = UF_DATE_TEXT+3(2).
  CF_DATE+6(2) = UF_DATE_TEXT(2).

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_GROUP
*---------------------------------------------------------------------*
* Validate all records in the same group
* If there at least one record error,
* then mark error for every items within the same group
*---------------------------------------------------------------------*
FORM F_VALIDATE_GROUP  USING UT_QUOTA_PLAN TYPE TT_QUOTA_PLAN
                       CHANGING CT_DATA_HDR TYPE TT_DATA_HDR
                                CT_DATA_ITM TYPE TT_DATA_ITM
                                CT_RESULT TYPE TT_RESULT.

  DATA: LS_DATA TYPE TS_DATA.

  DATA: LF_SUM_PORTION TYPE ZSDSDE_PORTION,
        LF_MSGTX       TYPE BAPI_MSG,
        LF_FOUND_DB    TYPE FLAG,
        LF_FOUND_DUP   TYPE FLAG,
        LF_GRPNO_DUP   TYPE TS_DATA_HDR-GRPNO,
        LF_NOT_DELETED TYPE FLAG.

  DATA : LV_QTY TYPE P DECIMALS 0.

  CLEAR: CT_RESULT.

  LOOP AT CT_DATA_HDR ASSIGNING FIELD-SYMBOL(<L_HDR>).
    CLEAR: LS_DATA,
           LF_SUM_PORTION,
           LF_MSGTX,
           LF_FOUND_DB,
           LF_FOUND_DUP,
           LF_GRPNO_DUP,
           LF_NOT_DELETED.

    MOVE-CORRESPONDING <L_HDR> TO LS_DATA.

*-----------------------
* Header Validation
*-----------------------
    IF <L_HDR>-MSGTY IS INITIAL.
*   Check key fields in header cannot be duplicated or
*   overlap period with other group in the file
      PERFORM F_CHECK_DUPLICATE_RECORD USING <L_HDR>
                                             CT_DATA_HDR
                                    CHANGING LF_FOUND_DUP
                                             LF_GRPNO_DUP.

      IF LF_FOUND_DUP EQ ABAP_TRUE.
*      Text-e13: Duplicate entries found
        LF_MSGTX = TEXT-E13.
        <L_HDR>-MSGTY =  GC_MSGTY-ERROR.
      ENDIF.
    ENDIF.

    IF <L_HDR>-MSGTY IS INITIAL.
*     Check existing quota plan
*     Check quota plan already exist?
      PERFORM F_CHECK_EXISTING_QUOTA USING <L_HDR>-MATNR
                                           <L_HDR>-DAY_FIRST
                                           <L_HDR>-DAY_LAST
                                           UT_QUOTA_PLAN
                                  CHANGING LF_FOUND_DB.
      IF GV_MODE EQ GC_MODE-CREATE AND
         LF_FOUND_DB EQ ABAP_TRUE.
*       Text e16: Quota plan already exist for this material
        LF_MSGTX = TEXT-E16.
        <L_HDR>-MSGTY =  GC_MSGTY-ERROR.
      ELSEIF GV_MODE EQ GC_MODE-UPDATE AND
             LF_FOUND_DB EQ ABAP_FALSE.
*       Text e19: Quota plan not found for updating
        LF_MSGTX = TEXT-E19.
        <L_HDR>-MSGTY =  GC_MSGTY-ERROR.
      ENDIF.
    ENDIF.

*   Header has error flag, then mark all items in error
    IF <L_HDR>-MSGTY EQ GC_MSGTY-ERROR.
      PERFORM F_SET_ERROR_TO_GROUP USING <L_HDR>-GRPNO
                                         LF_MSGTX
                                CHANGING CT_DATA_HDR
                                         CT_DATA_ITM
                                         CT_RESULT.
    ENDIF.

*-----------------------
* Item Validation
*-----------------------
    CLEAR: LF_NOT_DELETED,LV_QTY.
    LOOP AT CT_DATA_ITM ASSIGNING FIELD-SYMBOL(<L_ITM>)
                        WHERE GRPNO EQ <L_HDR>-GRPNO.
      CLEAR: LF_MSGTX.

      MOVE-CORRESPONDING <L_ITM> TO LS_DATA.

      IF <L_ITM>-ZDEL_FLG IS INITIAL.
        LF_SUM_PORTION = LF_SUM_PORTION + <L_ITM>-PORTION_PERC.
        LF_NOT_DELETED = ABAP_TRUE.  "Found at least one item which delete flag = SPACE
      ENDIF.

*     Item has error flag, set error to all other item withing same group
      IF <L_ITM>-MSGTY EQ GC_MSGTY-ERROR.
        PERFORM F_SET_ERROR_TO_GROUP USING <L_HDR>-GRPNO
                                           LF_MSGTX
                                  CHANGING CT_DATA_HDR
                                           CT_DATA_ITM
                                           CT_RESULT.
      ENDIF.

      ADD <L_ITM>-PORTION_QTY TO LV_QTY.

      PERFORM F_COLLECT_RESULT USING LS_DATA
                            CHANGING CT_RESULT.
    ENDLOOP.

*-----------------------
* Check Sum Portion(%)
*-----------------------
    IF LF_SUM_PORTION EQ 0.
*   Total of Portion(%) = 0, delete flag must be specified for every items
      IF LF_NOT_DELETED EQ ABAP_TRUE.
*       Text e20: Total of Portion(%) = 0, delete flag must be marked for every items
        LF_MSGTX = TEXT-E20.
        PERFORM F_SET_ERROR_TO_GROUP USING <L_HDR>-GRPNO
                                           LF_MSGTX
                                  CHANGING CT_DATA_HDR
                                           CT_DATA_ITM
                                           CT_RESULT.
      ENDIF.
    ELSEIF LF_SUM_PORTION NE 100.
*     Text e11: Total of Portion(%) must equal 100
*      LF_MSGTX = TEXT-E11.
*      PERFORM F_SET_ERROR_TO_GROUP USING <L_HDR>-GRPNO
*                                         LF_MSGTX
*                                CHANGING CT_DATA_HDR
*                                         CT_DATA_ITM
*                                         CT_RESULT.
    ENDIF.

    IF LV_QTY GT <L_HDR>-QUOTA_QTY.
      LF_MSGTX = TEXT-E22.
      PERFORM F_SET_ERROR_TO_GROUP USING <L_HDR>-GRPNO
                                         LF_MSGTX
                                CHANGING CT_DATA_HDR
                                         CT_DATA_ITM
                                         CT_RESULT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SET_ERROR_TO_GROUP
*---------------------------------------------------------------------*
* Set error to Header/Item with same group
*---------------------------------------------------------------------*
FORM F_SET_ERROR_TO_GROUP  USING    CF_GRPNO TYPE ZSDSDE_GRPNO
                                    LF_MSGTX TYPE BAPI_MSG
                           CHANGING CT_DATA_HDR TYPE TT_DATA_HDR
                                    CT_DATA_ITM TYPE TT_DATA_ITM
                                    CT_RESULT TYPE TT_RESULT.

  DATA: LS_DATA_HDR TYPE TS_DATA_HDR,
        LS_DATA_ITM TYPE TS_DATA_ITM,
        LS_RESULT   TYPE TS_RESULT.

  LS_DATA_HDR-MSGTY = GC_MSGTY-ERROR.
  LS_DATA_HDR-MSGTX = LF_MSGTX.

  LS_DATA_ITM-MSGTY = GC_MSGTY-ERROR.
  LS_DATA_ITM-MSGTX = LF_MSGTX.

  LS_RESULT-STATU = ICON_LED_RED.
  LS_RESULT-MSGTY = GC_MSGTY-ERROR.
  LS_RESULT-MSGTX = LF_MSGTX.

* Set error to Header, Item, Result
  MODIFY CT_DATA_HDR FROM LS_DATA_HDR
    TRANSPORTING MSGTY WHERE GRPNO EQ CF_GRPNO.

  MODIFY CT_DATA_ITM FROM LS_DATA_ITM
    TRANSPORTING MSGTY WHERE GRPNO EQ CF_GRPNO.

  MODIFY CT_RESULT FROM LS_RESULT
    TRANSPORTING STATU MSGTY WHERE GRPNO EQ CF_GRPNO.

* Set message text to Header, Item, Result
  IF LF_MSGTX IS NOT INITIAL.

    MODIFY CT_DATA_HDR FROM LS_DATA_HDR
      TRANSPORTING MSGTX WHERE GRPNO EQ CF_GRPNO
                         AND MSGTX IS INITIAL.

    MODIFY CT_DATA_ITM FROM LS_DATA_ITM
      TRANSPORTING MSGTX WHERE GRPNO EQ CF_GRPNO
                         AND MSGTX IS INITIAL.

    MODIFY CT_RESULT FROM LS_RESULT
      TRANSPORTING MSGTX WHERE GRPNO EQ CF_GRPNO
                         AND MSGTX IS INITIAL.

  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CHECK_DUPLICATE_RECORD
*---------------------------------------------------------------------*
* Check duplicate key data in input file
* MATNR, DAY_FIRST, DAY_LAST cannot duplicate or in overlap period
*---------------------------------------------------------------------*
FORM F_CHECK_DUPLICATE_RECORD  USING    US_DATA TYPE TS_DATA_HDR
                                        UT_DATA_HDR TYPE TT_DATA_HDR
                               CHANGING CF_FOUND_DUP TYPE FLAG
                                        CF_GRPNO_DUP TYPE TS_DATA_HDR-GRPNO.

  CLEAR: CF_FOUND_DUP,
         CF_GRPNO_DUP.

  LOOP AT UT_DATA_HDR ASSIGNING FIELD-SYMBOL(<L_HDR>)
                      WHERE GRPNO NE US_DATA-GRPNO
                        AND MATNR EQ US_DATA-MATNR
                        AND DAY_FIRST LE US_DATA-DAY_LAST
                        AND DAY_LAST GE US_DATA-DAY_FIRST.
    CF_FOUND_DUP = ABAP_TRUE.
    CF_GRPNO_DUP = <L_HDR>-GRPNO.
    EXIT.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SET_SUCESS_RESULT
*---------------------------------------------------------------------*
* Set success status and message to result table
*---------------------------------------------------------------------*
FORM F_SET_SUCESS_RESULT  CHANGING CT_RESULT TYPE TT_RESULT.
  DATA: LS_RESULT TYPE TS_RESULT,
        LF_MSGTX  TYPE BAPI_MSG.

  IF CB_TEST EQ GC_TRUE.
*   Text s01: Test run successfully
    LF_MSGTX = TEXT-S01.
  ELSE.
*   Text s02: Upload successful
    LF_MSGTX = TEXT-S02.
  ENDIF.

  LS_RESULT-STATU = ICON_LED_GREEN.
  LS_RESULT-MSGTY = GC_MSGTY-SUCCESS.
  LS_RESULT-MSGTX = LF_MSGTX.

  MODIFY CT_RESULT FROM LS_RESULT
    TRANSPORTING STATU MSGTY MSGTX WHERE MSGTY IS INITIAL.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_CHECK_EXISTING_QUOTA
*---------------------------------------------------------------------*
* Check Quota Plan already exist for the material during the period
*---------------------------------------------------------------------*
FORM F_CHECK_EXISTING_QUOTA  USING    UF_MATNR TYPE MATNR
                                      UF_DAY_FIRST TYPE ZSDSDE_DAY_FIRST
                                      UF_DAY_LAST TYPE ZSDSDE_DAY_LAST
                                      UT_QUOTA_PLAN TYPE TT_QUOTA_PLAN
                             CHANGING CF_FOUND TYPE FLAG.

  FIELD-SYMBOLS: <L_QUOTA_PLAN> TYPE TS_QUOTA_PLAN.

  CLEAR: CF_FOUND.

  IF UT_QUOTA_PLAN IS INITIAL.
    RETURN.
  ENDIF.

  CASE GV_MODE.
    WHEN GC_MODE-CREATE.
      LOOP AT UT_QUOTA_PLAN ASSIGNING <L_QUOTA_PLAN>
                          WHERE MATNR EQ UF_MATNR
                            AND DAY_FIRST LE UF_DAY_LAST
                            AND DAY_LAST GE UF_DAY_FIRST.
        CF_FOUND = ABAP_TRUE.
        EXIT.
      ENDLOOP.
    WHEN GC_MODE-UPDATE.
      LOOP AT UT_QUOTA_PLAN ASSIGNING <L_QUOTA_PLAN>
                          WHERE MATNR EQ UF_MATNR
                            AND DAY_FIRST EQ UF_DAY_FIRST
                            AND DAY_LAST EQ UF_DAY_LAST.

        CF_FOUND = ABAP_TRUE.
        EXIT.
      ENDLOOP.
  ENDCASE.

ENDFORM.
**---------------------------------------------------------------------*
* Form F_EXPORT_EXCEL_TEMPLATE
*---------------------------------------------------------------------*
* To download template file from SAP Web Repository which created
* via t-code SMW0
*---------------------------------------------------------------------*
FORM F_EXPORT_EXCEL_TEMPLATE .
  DATA:
  LS_KEY  TYPE  WWWDATATAB.

  DATA:
    LV_OFILE       TYPE STRING,
    LV_DESTINATION TYPE RLGRAP-FILENAME,
    LV_SUBRC       TYPE SY-SUBRC.

* Popup to Get File name
  PERFORM F_LIST_OFILE CHANGING LV_OFILE.
  IF LV_OFILE IS INITIAL.
*   Operation cancelled by user.
    MESSAGE E025(ZSDSCA01) .
    RETURN.
  ENDIF.

  LV_DESTINATION = LV_OFILE.

* Call Function to Download file
  CLEAR LS_KEY.
  LS_KEY-RELID = 'MI'.
  LS_KEY-OBJID = 'ZSDSSDR0260'.
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = LS_KEY
      DESTINATION = LV_DESTINATION
    IMPORTING
      RC          = LV_SUBRC.
  IF LV_SUBRC NE 0.
*   No layout found.
    MESSAGE E030(ZSDSCA01).
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_LIST_OFILE
*----------------------------------------------------------------------*
*  Popup for Output file selection
*----------------------------------------------------------------------*
FORM F_LIST_OFILE  CHANGING PV_FILENAME  TYPE  STRING.

  DATA:
    LV_PATH     TYPE  STRING,
    LV_FILENAME TYPE  STRING.

* Initialize Output
  CLEAR PV_FILENAME.

* Get File name dialog
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      DEFAULT_EXTENSION    = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER          = 'Excel File|*.XLSX;*.XLS' ##NO_TEXT
      PROMPT_ON_OVERWRITE  = 'X'
    CHANGING
      FILENAME             = LV_FILENAME
      PATH                 = LV_PATH
      FULLPATH             = PV_FILENAME
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_CONFIRM_QTY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TT_ZSDSSDT016
*&---------------------------------------------------------------------*
FORM F_SET_CONFIRM_QTY  USING LT_DATA TYPE TT_ZSDSSDT016.

  DATA : S_MATNR TYPE RANGE OF MARA-MATNR.

  DATA : LV_JOBNAME  TYPE TBTCO-JOBNAME,
         LV_JOBCOUNT TYPE TBTCO-JOBCOUNT.

  DATA : LS_PRINT_PARAMETERS TYPE PRI_PARAMS.
  DATA : LS_SELTAB           TYPE TABLE OF RSPARAMS.

  S_MATNR = VALUE #(
                 FOR LS_TMP IN LT_DATA INDEX INTO LV_INDEX
                   (
                     SIGN   = 'I'
                     OPTION = 'EQ'
                     LOW    = LS_TMP-MATNR
                    )
                  ).

  IF S_MATNR IS NOT INITIAL.
    SORT S_MATNR.
    DELETE ADJACENT DUPLICATES FROM S_MATNR.

    CONCATENATE 'QUOTA' '_' SY-UNAME '_' SY-UZEIT INTO LV_JOBNAME.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LV_JOBNAME
      IMPORTING
        JOBCOUNT         = LV_JOBCOUNT
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.
    CASE SY-SUBRC.
      WHEN 0.
      WHEN OTHERS.
        MESSAGE E208(00) WITH 'Error'.
    ENDCASE.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
*       immediately            = 'X'
        NO_DIALOG              = 'X'
        USER                   = SY-UNAME
      IMPORTING
        OUT_PARAMETERS         = LS_PRINT_PARAMETERS
      EXCEPTIONS
        ARCHIVE_INFO_NOT_FOUND = 1
        INVALID_PRINT_PARAMS   = 2
        INVALID_ARCHIVE_PARAMS = 3
        OTHERS                 = 4.

    SUBMIT ZSDSSDR0580 TO SAP-SPOOL AND RETURN
                                   WITH SELECTION-TABLE LS_SELTAB
                                                   USER SY-UNAME
                                       SPOOL PARAMETERS LS_PRINT_PARAMETERS
                                   WITHOUT SPOOL DYNPRO
                                                VIA JOB LV_JOBNAME
                                                 NUMBER LV_JOBCOUNT
                           USING SELECTION-SCREEN  1000
                            WITH S_MATNR IN S_MATNR[]
                            WITH P_AUTO  EQ ABAP_TRUE
                            WITH P_ZERO  EQ abap_TRUE.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LV_JOBCOUNT
        JOBNAME              = LV_JOBNAME
        STRTIMMED            = ABAP_TRUE
      EXCEPTIONS
        CANT_START_IMMEDIATE = 1
        INVALID_STARTDATE    = 2
        JOBNAME_MISSING      = 3
        JOB_CLOSE_FAILED     = 4
        JOB_NOSTEPS          = 5
        JOB_NOTEX            = 6
        LOCK_FAILED          = 7
        OTHERS               = 8.
  ENDIF.

ENDFORM.
