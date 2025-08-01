*-----------------------------------------------------------------------
*  Program ID         : ZSDSCAR0110
*  Creation Date      : 25.06.2025
*  Author             : Boonpipop Ch. (SDS)
*  Add-on ID          : N/A
*  Description        : Report for SAP User
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSCAR0101
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSCAR0101 MESSAGE-ID ZSDSCA01.
CLASS LCL_HANDLE_EVENTS DEFINITION DEFERRED."Ref. Std : SALV_TEST_TABLE_EVENTS
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: USR01,
        USR02.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES:
  BEGIN OF TYP_ALV,
    BNAME         TYPE USR01-BNAME, "User
    NAME_FIRST    TYPE ADRP-NAME_FIRST,   "First Name
    NAME_LAST     TYPE ADRP-NAME_LAST,    "last Name
    PERNR         TYPE PA0000-PERNR,      "Person number (blank? mean no mapping with HR)
    GLTGV         TYPE USR02-GLTGV,  " Valid From
    GLTGB         TYPE USR02-GLTGB,  " Valid To
    UFLAG         TYPE USR02-UFLAG,
    STATUS        TYPE CHAR50,
    EMP_STAT_CURR TYPE PA0000-STAT2, "Current Employment Status (as of current date)
    EMP_STAT_LAST TYPE PA0000-STAT2, "Last Employment Status (99991231)
    BEGDA_LAST    TYPE PA0000-BEGDA, "Start Date of Last Employment
    COLOR_FLAG    TYPE ICON_D,
  END OF TYP_ALV,

  BEGIN OF TYP_EXCEL,
    USER_NAME	TYPE CHAR12,
    EMP_ID    TYPE CHAR08,
  END OF TYP_EXCEL.

TYPES: TS_XLSX  TYPE ZCL_SDSCA_UTILITIES=>TS_XLSX_DATA.
TYPES: TT_XLSX  TYPE ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA.
TYPES: TT_ALV   TYPE STANDARD TABLE OF TYP_ALV,
       TT_EXCEL TYPE STANDARD TABLE OF TYP_EXCEL.

TYPES: TS_FNAME  TYPE  CHAR40.
TYPES: TS_MSG_TXT  TYPE  ZSDSDE_MSGTX.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA:
*-> internal tables
  GT_ALV        TYPE STANDARD TABLE OF TYP_ALV,
  GT_XLSX       TYPE TT_XLSX,
  GT_EXCEL      TYPE TT_EXCEL,

*-> range
*-> work areas
  GS_ALV        TYPE TYP_ALV,
  GS_EXCEL      TYPE TYP_EXCEL,
*-> variables
  GV_LOG_HANDLE TYPE BALLOGHNDL,
*-> reference
  GO_EVENTS     TYPE REF TO LCL_HANDLE_EVENTS.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:
  GC_NOCHG      TYPE CHAR5      VALUE '-',
  GC_ICON_ALERT TYPE ICON_D VALUE '@09@',

  "Message type: S Success, E Error, W Warning, I Info, A Abort
  BEGIN OF GC_MSGTYP,
    ERROR TYPE BAPIRET2-TYPE VALUE 'E',
    ABORT TYPE BAPIRET2-TYPE VALUE 'A',
    SUCCE TYPE BAPIRET2-TYPE VALUE 'S',
  END OF GC_MSGTYP.

CONSTANTS:
  BEGIN OF GC_SUBTY,
    USERNAME TYPE PA0105-SUBTY VALUE '0001', "System user name (SY-UNAME)
  END OF GC_SUBTY.
*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
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

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS DEFINITION.
  PUBLIC SECTION.
    METHODS:
      ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
        IMPORTING E_SALV_FUNCTION.

ENDCLASS.                    "lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS IMPLEMENTATION.
  METHOD ON_USER_COMMAND.
    CASE SY-UCOMM.
      WHEN  '&POST'.
        PERFORM F_UPDATE_PA30.
        "        MESSAGE 'Update PA30 Successfuly.' TYPE 'I' DISPLAY LIKE 'S'.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.                    "on_user_command
ENDCLASS.
*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------

"Option
SELECTION-SCREEN BEGIN OF BLOCK S02 WITH FRAME TITLE TEXT-002.
  PARAMETERS: R_OPT1 RADIOBUTTON GROUP GRP1 USER-COMMAND RAD DEFAULT 'X', "Display
              R_OPT2 RADIOBUTTON GROUP GRP1.                              "Uplaod+
SELECTION-SCREEN END OF BLOCK S02.

"Display Block
SELECTION-SCREEN BEGIN OF BLOCK S03 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS:
              S_BNAME FOR USR01-BNAME MODIF ID SC1,
              S_VFROM FOR USR02-GLTGB MODIF ID SC1,
              S_VTO   FOR USR02-GLTGV MODIF ID SC1.
SELECTION-SCREEN END OF BLOCK S03.

"Upload Block
SELECTION-SCREEN BEGIN OF BLOCK S01 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    P_IFILE TYPE  STRING LOWER CASE MODIF ID SC2,
    P_BGROW TYPE I DEFAULT 1 MODIF ID SC2.
  SELECTION-SCREEN BEGIN OF LINE.

    "Infotype
    SELECTION-SCREEN COMMENT 1(10) TEXT-C01 MODIF ID SC2.
    PARAMETERS: P_INFT TYPE INFTY DEFAULT '0105' MODIF ID SC2.

    "Subtype
    SELECTION-SCREEN COMMENT 20(10) TEXT-C02 MODIF ID SC2.
    PARAMETERS: P_STYP TYPE SUBTY DEFAULT '0001' MODIF ID SC2.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK S01.


*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  IF S_BNAME[] IS INITIAL.
    APPEND VALUE #( SIGN = 'E' OPTION = 'CP' LOW = '1*' ) TO S_BNAME.
    APPEND VALUE #( SIGN = 'E' OPTION = 'CP' LOW = '2*' ) TO S_BNAME.
    APPEND VALUE #( SIGN = 'E' OPTION = 'CP' LOW = '3TEST*' ) TO S_BNAME.
    APPEND VALUE #( SIGN = 'E' OPTION = 'CP' LOW = 'ZTEST*' ) TO S_BNAME.
    APPEND VALUE #( SIGN = 'E' OPTION = 'CP' LOW = 'MIGSDS*' ) TO S_BNAME.
  ENDIF.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_IFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_IFILE.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF R_OPT1 = 'X' AND SCREEN-GROUP1 = 'SC2'.
      SCREEN-ACTIVE = 0.
    ELSEIF R_OPT2 = 'X' AND SCREEN-GROUP1 = 'SC1'.
      SCREEN-ACTIVE = 0.
    ELSE.
      SCREEN-ACTIVE = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data
  PERFORM F_PROCESS_DATA.
  IF GT_ALV[] IS INITIAL.
    MESSAGE 'Data not found.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
  PERFORM F_ALV_DISPLAY.

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA
*&---------------------------------------------------------------------*
*& Processing data
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA.

  IF R_OPT1 = ABAP_TRUE.  "Display
    PERFORM F_GET_DATA.
  ELSE.                   "Upload
    PERFORM F_READ_FILE CHANGING GT_XLSX.
    IF GT_XLSX IS INITIAL.
      RETURN.
    ENDIF.

    "Assign to ALV
    PERFORM F_ASSIGN_EXCEL_STRUC  USING GT_XLSX
                                  CHANGING GT_EXCEL.

    PERFORM F_MAPPING_ALV         USING GT_EXCEL
                                  CHANGING GT_ALV.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_XLSX
*&---------------------------------------------------------------------*
FORM F_READ_FILE  CHANGING CT_XLSX TYPE  TT_XLSX.

  DATA:
    LF_FILENAME   TYPE  STRING.

* Initialize Output
  CLEAR: CT_XLSX.

* Show progress
* Text-p01 : Reading Input file . . .
  MC_SHOW_PROGRESS 15 TEXT-P01.

  CLEAR: LF_FILENAME.
  LF_FILENAME = P_IFILE.

  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = LF_FILENAME
      IF_READ_ACTIVE_WORKSHEET    = 'X'
*     IF_XSTRING                  =
*     IT_WORKSHEET                =
    IMPORTING
      ET_XLSX_DATA                = CT_XLSX
    EXCEPTIONS
      MISSING_FILENAME_OR_XSTRING = 1
      ERROR_READ_FILENAME         = 2
      NO_DATA_FOUND               = 3
      OTHERS                      = 4.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LIST_IFILE
*&---------------------------------------------------------------------*
*& Popup for Input file selection
*&---------------------------------------------------------------------*\
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
*&---------------------------------------------------------------------*
*& Form F_ASSIGN_ALV_STRUC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_ASSIGN_EXCEL_STRUC  USING    UT_XLSX  TYPE  TT_XLSX
                          CHANGING CT_EXCEL TYPE  TT_EXCEL.

  DATA: LT_EXCEL TYPE TT_EXCEL.

* Initialize Output
  CLEAR: LT_EXCEL.

* Show progress
* Text-p02 : Validating Input file . . .
  MC_SHOW_PROGRESS 40 TEXT-P02.

  READ TABLE UT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>) INDEX 1.
  IF SY-SUBRC EQ 0.
    PERFORM F_ASSIGN_DATA USING  <L_SHEET>
                         CHANGING LT_EXCEL.
    INSERT LINES OF LT_EXCEL INTO TABLE CT_EXCEL.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_assign_data
*&---------------------------------------------------------------------*
*& Assign data
*&---------------------------------------------------------------------*
FORM F_ASSIGN_DATA  USING  US_XLSX    TYPE TS_XLSX
                 CHANGING  CT_EXCEL   TYPE TT_EXCEL.

  DATA:
    LF_ROW_START TYPE I,
    LF_COL_START TYPE I VALUE 1,
    LF_COL_INDEX TYPE I,
    LF_ROWNO     TYPE SY-TABIX,
    LF_FNAME     TYPE TS_FNAME,
    LF_MSGTX     TYPE TS_MSG_TXT.

  DATA: LREF_VALIDATE TYPE REF TO ZCL_SDSCA_DATA_VALIDATION,
        LREF_STRUC    TYPE REF TO CL_ABAP_STRUCTDESCR.

  LF_ROW_START = P_BGROW .
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

    LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>). "#EC CI_NESTED.
      CHECK SY-TABIX GE LF_COL_START.

      LF_COL_INDEX = SY-TABIX - LF_COL_START + 1.
      PERFORM F_GET_TARGET_FIELD  USING  'TYP_EXCEL'
                                         LF_COL_INDEX
                                CHANGING LF_FNAME.
      ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*     Check No change value?
      CHECK <L_FIELD> NE GC_NOCHG.

      CASE LF_FNAME.
*---Fill data from Template Header to Header Data--------
        WHEN 'USER_NAME' .
          GS_EXCEL-USER_NAME  = <L_FIELD>.
        WHEN 'EMP_ID' .
          GS_EXCEL-EMP_ID     = <L_FIELD>.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
    APPEND GS_EXCEL TO CT_EXCEL.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_TARGET_FIELD
*&---------------------------------------------------------------------*
*& Get Target Field name based on column sequence
*&---------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD USING  UF_TYPE   TYPE  CHAR20
                               UF_INDEX  TYPE  I
                      CHANGING CF_FIELD  TYPE  TS_FNAME.

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
*&---------------------------------------------------------------------*
*& Form F_MAPPING_ALV
*&---------------------------------------------------------------------*
*& Mapping ALV
*&---------------------------------------------------------------------*
FORM F_MAPPING_ALV  USING    UT_EXCEL TYPE TT_EXCEL
                    CHANGING CT_ALV   TYPE TT_ALV.


  DATA(LT_TEMP) = UT_EXCEL[].
  DELETE LT_TEMP WHERE EMP_ID IS INITIAL
                    OR EMP_ID = '#N/A'.

  CHECK LT_TEMP IS NOT INITIAL.
  "Get User Infomation
  SELECT
    USR21~BNAME,
    USR21~PERSNUMBER,
    ADRP~NAME_FIRST,
    ADRP~NAME_LAST,
    USR02~UFLAG,
    USR02~GLTGB,
    USR02~GLTGV
  FROM USR21
  INNER JOIN ADRP ON USR21~PERSNUMBER = ADRP~PERSNUMBER
  INNER JOIN USR02 ON USR02~BNAME = USR21~BNAME
  INTO TABLE @DATA(LT_USER)
  FOR ALL ENTRIES IN @LT_TEMP[]
  WHERE USR21~BNAME = @LT_TEMP-USER_NAME.
  IF SY-SUBRC = 0.
    SORT LT_USER BY BNAME.
  ENDIF.

  LOOP AT LT_TEMP INTO DATA(LS_EXCEL).
    READ TABLE LT_USER INTO DATA(LS_USER)
                       WITH KEY BNAME = LS_EXCEL-USER_NAME
                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      APPEND INITIAL LINE TO CT_ALV ASSIGNING FIELD-SYMBOL(<LFS_ALV>).
      MOVE-CORRESPONDING LS_USER TO <LFS_ALV>.
      <LFS_ALV>-PERNR = LS_EXCEL-EMP_ID.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_DISPLAY
*&---------------------------------------------------------------------*
*& Display ALV
*&---------------------------------------------------------------------*
FORM F_ALV_DISPLAY .
  DATA:
    ##NEEDED
    LR_TEXT             TYPE REF TO CL_SALV_FORM_TEXT,
    LS_ALVTABLE         TYPE REF TO CL_SALV_TABLE,
    LR_COLUMNS          TYPE REF TO CL_SALV_COLUMNS_TABLE,
    LR_CONTENT          TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
    LR_LABEL            TYPE REF TO CL_SALV_FORM_LABEL,
    LF_TEXT             TYPE STRING,
    LS_LAYOUT           TYPE REF TO CL_SALV_LAYOUT,
    LS_KEY              TYPE SALV_S_LAYOUT_KEY,
    LS_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
    LS_FUNCTIONS_LIST   TYPE REF TO CL_SALV_FUNCTIONS_LIST,
    LO_VBELN            TYPE REF TO CL_SALV_COLUMN_TABLE,
    LO_EVENTS           TYPE REF TO CL_SALV_EVENTS_TABLE.

  FIELD-SYMBOLS <LFS_TABLE> TYPE ANY TABLE.
  ASSIGN GT_ALV TO <LFS_TABLE>.

*Initialize ref to cl_salv_table
  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = LS_ALVTABLE
        CHANGING
          T_TABLE      = <LFS_TABLE> ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
*      MESSAGE TEXT-E01 TYPE 'E'.
  ENDTRY.

  LR_COLUMNS = LS_ALVTABLE->GET_COLUMNS( ).

  LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).

  TRY.
      LO_VBELN ?= LR_COLUMNS->GET_COLUMN( 'DO_NUM' ).
      LO_VBELN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
    CATCH CX_SALV_NOT_FOUND.                            "#EC NO_HANDLER
  ENDTRY.

* Set column
  PERFORM F_DETAIL_COLUMN_SETTINGS_REPT USING LR_COLUMNS.
  IF R_OPT1 = 'X'.
    LS_ALVTABLE->SET_SCREEN_STATUS(
     EXPORTING
       REPORT        =   SY-REPID               " ABAP Program: Current Master Program
       PFSTATUS      = 'STANDARD_DISPLAY'               " Screens, Current GUI Status
       SET_FUNCTIONS = CL_SALV_TABLE=>C_FUNCTIONS_ALL " ALV: Data Element for Constants
   ).
  ELSE.
    LS_ALVTABLE->SET_SCREEN_STATUS(
     EXPORTING
       REPORT        =   SY-REPID               " ABAP Program: Current Master Program
       PFSTATUS      = 'STANDARD_FULLSCREEN'               " Screens, Current GUI Status
       SET_FUNCTIONS = CL_SALV_TABLE=>C_FUNCTIONS_ALL " ALV: Data Element for Constants
   ).
  ENDIF.

  "Event
  LO_EVENTS = LS_ALVTABLE->GET_EVENT( ).
  CREATE OBJECT GO_EVENTS.
  SET HANDLER GO_EVENTS->ON_USER_COMMAND FOR LO_EVENTS.

  LS_DISPLAY_SETTINGS = LS_ALVTABLE->GET_DISPLAY_SETTINGS( ).
  LS_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( 'X' ).
  LS_DISPLAY_SETTINGS->SET_LIST_HEADER( SY-TITLE ).

  LS_FUNCTIONS_LIST = LS_ALVTABLE->GET_FUNCTIONS( ).
  LS_FUNCTIONS_LIST->SET_ALL( ).

  LS_KEY-REPORT = SY-CPROG.
  LS_KEY-HANDLE = '0001'.
  LS_LAYOUT = LS_ALVTABLE->GET_LAYOUT( ).
  LS_LAYOUT->SET_KEY( LS_KEY ).
  LS_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
  LS_LAYOUT->SET_DEFAULT( 'X' ).

* Top_of_list
  CREATE OBJECT LR_CONTENT.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 1
    COLUMN = 1
    TEXT   = SY-TITLE ).

  LF_TEXT = TEXT-T01.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT   = LF_TEXT ).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 2
    COLUMN = 2
    TEXT   = SY-DATUM ).

  LF_TEXT = TEXT-T02.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 3
    COLUMN = 1
    TEXT   = LF_TEXT ).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 3
    COLUMN = 2
    TEXT   = SY-UNAME ).

  "total records
  DATA(LV_COUNT) = LINES( GT_ALV ).
  LF_TEXT = TEXT-T03.
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 4
    COLUMN = 1
    TEXT   = LF_TEXT ).
  LR_LABEL = LR_CONTENT->CREATE_LABEL(
    ROW    = 4
    COLUMN = 2
    TEXT   = LV_COUNT ).


  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).
  LS_ALVTABLE->SET_TOP_OF_LIST( LR_CONTENT ).
*
* Display Table
  LS_ALVTABLE->DISPLAY( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DETAIL_COLUMN_SETTINGS_REPT
*&---------------------------------------------------------------------*
FORM F_DETAIL_COLUMN_SETTINGS_REPT  USING UR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.
  DATA: LR_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE,
        LF_S_TEXT TYPE SCRTEXT_S,
        LF_M_TEXT TYPE SCRTEXT_M,
        LF_L_TEXT TYPE SCRTEXT_L.

  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'OBJNR' ).
      LR_COLUMN->SET_TECHNICAL( ABAP_TRUE ).
    CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND. "#EC NO_HANDLER
  ENDTRY.

  LF_S_TEXT = LF_M_TEXT = LF_L_TEXT = TEXT-C03.
  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'STATUS' ).
      LR_COLUMN->SET_SHORT_TEXT( VALUE = LF_S_TEXT ).
      LR_COLUMN->SET_MEDIUM_TEXT( VALUE = LF_M_TEXT ).
      LR_COLUMN->SET_LONG_TEXT( VALUE = LF_L_TEXT ).
    ##NO_HANDLER   CATCH CX_SALV_WRONG_CALL CX_SALV_MSG CX_SALV_OBJECT_NOT_FOUND CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR.
  ENDTRY.

  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'EMP_STAT_CURR' ).
      LR_COLUMN->SET_LONG_TEXT( 'Current Emp Status' ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Curr Status' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Curr' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'EMP_STAT_LAST' ).
      LR_COLUMN->SET_LONG_TEXT( 'Last Emp Status' ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Last Status' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Last' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LR_COLUMN ?= UR_COLUMNS->GET_COLUMN( 'BEGDA_LAST' ).
      LR_COLUMN->SET_LONG_TEXT( 'Last Start Date' ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Start Date' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Date' ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UPDATE_PA30
*&---------------------------------------------------------------------*
*& UpdatePA30
*&---------------------------------------------------------------------*
FORM F_UPDATE_PA30.

  CONSTANTS: LC_MODIFY      TYPE PSPAR-ACTIO VALUE 'MOD', " Operation Mode (MOD, INS, DEL)
             LC_INSERT      TYPE PSPAR-ACTIO VALUE 'INS',
             LC_NDAT        TYPE SY-DATUM    VALUE '99991231',
             LC_INFTYP_0105 TYPE INFTY   VALUE '0105'.

  CONSTANTS: BEGIN OF LC_SUBTY,
               USERNAME TYPE PA0105-SUBTY VALUE '0001', "System user name (SY-UNAME)
             END OF LC_SUBTY.

  DATA: LS_P0105  TYPE P0105,
        LV_ACTION TYPE PSPAR-ACTIO,
        LS_RET1   TYPE BAPIRETURN1.

  DATA: LS_MSG        TYPE BAL_S_MSG,
        LT_LOG_HANDLE TYPE BAL_T_LOGH.

  CHECK GT_ALV[] IS NOT INITIAL.

  SELECT PERNR
    FROM PA0000
    INTO TABLE @DATA(LT_PA0000)
    FOR ALL ENTRIES IN @GT_ALV[]
    WHERE PERNR = @GT_ALV-PERNR.
  IF SY-SUBRC = 0.
    SORT LT_PA0000 BY PERNR.
  ENDIF.

  SELECT *
    FROM PA0105
    INTO TABLE @DATA(LT_PA0105)
    FOR ALL ENTRIES IN @GT_ALV
    WHERE PERNR = @GT_ALV-PERNR
      AND SUBTY = @P_STYP.
  IF SY-SUBRC = 0.
    SORT LT_PA0105 BY PERNR SUBTY.
  ENDIF.

*  PERFORM F_LOG_CREATE.

  LOOP AT GT_ALV INTO DATA(LS_ALV).

    CLEAR: LS_RET1, GV_LOG_HANDLE.

    "Check PA30 existing
    READ TABLE LT_PA0000 INTO DATA(LS_PA0000)
                         WITH KEY PERNR = LS_ALV-PERNR
                         BINARY SEARCH.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    PERFORM F_LOG_CREATE USING LS_ALV-PERNR.

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        NUMBER = LS_ALV-PERNR
      IMPORTING
        RETURN = LS_RET1.

    READ TABLE LT_PA0105 INTO DATA(LS_PA0105)
                     WITH KEY PERNR = LS_ALV-PERNR
                              SUBTY = P_STYP
                     BINARY SEARCH.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING LS_PA0105 TO LS_P0105.
      LS_P0105-AEDTM   = SY-UNAME.
      LV_ACTION        = LC_MODIFY.
    ELSE.
      LS_P0105-SUBTY   = LC_SUBTY-USERNAME.
      LS_P0105-USRTY   = LC_SUBTY-USERNAME.
      LS_P0105-BEGDA   = SY-DATUM.
      LS_P0105-ENDDA   = LC_NDAT.
      LS_P0105-AEDTM   = SY-UNAME.
      LS_P0105-UNAME   = SY-UNAME.
      LV_ACTION        = LC_INSERT.
    ENDIF.

    LS_P0105-PERNR      = LS_PA0000-PERNR.
    LS_P0105-INFTY      = LC_INFTYP_0105.
    LS_P0105-USRID      = LS_ALV-BNAME.
    LS_P0105-USRID      = LS_ALV-BNAME.

    "Update address info type 0006
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        INFTY         = LC_INFTYP_0105  " Infotype 0006: Address
        NUMBER        = LS_ALV-PERNR      " หมายเลขพนักงาน
        SUBTYPE       = LS_P0105-SUBTY   "Subtype
        VALIDITYBEGIN = LS_P0105-BEGDA  " วันที่เริ่มต้น
        VALIDITYEND   = LS_P0105-ENDDA  " วันที่สิ้นสุด
        RECORD        = LS_P0105        " ข้อมูลที่ต้องการแก้ไข
        OPERATION     = LV_ACTION       " ประเภทการดำเนินการ (MOD, INS, DEL)
      IMPORTING
        RETURN        = LS_RET1.      " ตารางผลลัพธ์

    " Collect result to SBAL
    IF LS_RET1 IS INITIAL.
      LS_MSG-MSGID = 'ZSDSCA001'.
      LS_MSG-MSGNO = '000'.
      LS_MSG-MSGTY = GC_MSGTYP-SUCCE.
      LS_MSG-MSGV1 = |{ LS_ALV-PERNR } has been updated|.
    ELSE.
      LS_MSG-MSGID = LS_RET1-ID.
      LS_MSG-MSGNO = LS_RET1-NUMBER.
      LS_MSG-MSGTY = LS_RET1-TYPE.
      LS_MSG-MSGV1 = LS_RET1-MESSAGE_V1.
      LS_MSG-MSGV2 = LS_RET1-MESSAGE_V2.
      LS_MSG-MSGV3 = LS_RET1-MESSAGE_V3.
      LS_MSG-MSGV4 = LS_RET1-MESSAGE_V4.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        I_LOG_HANDLE = GV_LOG_HANDLE
        I_S_MSG      = LS_MSG.

    IF LS_RET1-TYPE <> GC_MSGTYP-ERROR.
      PERFORM COMMIT_WORK.
    ELSE.
      PERFORM ROLL_BACK.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        NUMBER = LS_ALV-PERNR
      IMPORTING
        RETURN = LS_RET1.

    APPEND GV_LOG_HANDLE TO LT_LOG_HANDLE.
  ENDLOOP.

  PERFORM F_LOG_SHOW USING LT_LOG_HANDLE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form commit_work
*&---------------------------------------------------------------------*
FORM COMMIT_WORK .

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = ABAP_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form roll_back
*&---------------------------------------------------------------------*
FORM ROLL_BACK .

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_GET_DATA .
  DATA: LT_PA0105 TYPE TABLE OF PA0105,
        LS_PA0105 TYPE PA0105.

  DATA: LT_DOMVALUES TYPE STANDARD TABLE OF DD07V,
        LV_TEXT      TYPE DD07V-DDTEXT,
        LV_VALUE     TYPE DOMVALUE_L.

  DATA: LR_USRID      TYPE RANGE OF PA0105-USRID.

  SELECT
    USR21~BNAME,
    USR21~PERSNUMBER,
    ADRP~NAME_FIRST,
    ADRP~NAME_LAST,
    USR02~UFLAG,
    USR02~GLTGB,
    USR02~GLTGV
  FROM USR21
  LEFT OUTER JOIN ADRP ON USR21~PERSNUMBER = ADRP~PERSNUMBER
  LEFT OUTER JOIN USR02 ON USR02~BNAME = USR21~BNAME
  WHERE USR21~BNAME IN @S_BNAME[] AND
        USR02~GLTGV IN @S_VFROM   AND
        USR02~GLTGB IN @S_VTO
  INTO TABLE @DATA(LT_USER).

  CHECK LT_USER IS NOT INITIAL.

  LR_USRID = VALUE #( FOR LS_USRID IN LT_USER
                        SIGN = 'I' OPTION = 'EQ'
                      ( LOW = LS_USRID-BNAME ) ).
  SELECT       "*
    PERNR, SUBTY, USRID, BEGDA, ENDDA, UNAME, AEDTM
    FROM PA0105
    INTO CORRESPONDING FIELDS OF TABLE @LT_PA0105
    WHERE USRID IN @LR_USRID
      AND SUBTY = @GC_SUBTY-USERNAME.
  IF SY-SUBRC = 0.
    SORT LT_PA0105 BY USRID.
  ENDIF.

  "Get Domain Name UFLAG
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      DOMNAME   = 'XUUFLAG'
      LANGU     = SY-LANGU
      TEXT      = 'X'
    TABLES
      DD07V_TAB = LT_DOMVALUES.

  SORT LT_DOMVALUES BY DOMVALUE_L.

  LOOP AT LT_USER INTO DATA(LS_USER).
    CLEAR: LV_VALUE.

    APPEND INITIAL LINE TO GT_ALV[] ASSIGNING FIELD-SYMBOL(<LFS_ALV>).
    MOVE-CORRESPONDING LS_USER TO <LFS_ALV>.

    LV_VALUE = LS_USER-UFLAG.
    CONDENSE LV_VALUE.
    READ TABLE LT_DOMVALUES INTO DATA(LS_DOMVALUES)
                            WITH KEY DOMVALUE_L = LV_VALUE
                            BINARY SEARCH.
    IF SY-SUBRC = 0.
      <LFS_ALV>-STATUS = LS_DOMVALUES-DDTEXT.
    ENDIF.

    READ TABLE LT_PA0105 INTO LS_PA0105
                         WITH KEY USRID = LS_USER-BNAME
                         BINARY SEARCH.
    IF SY-SUBRC = 0.
      <LFS_ALV>-PERNR = LS_PA0105-PERNR.
    ENDIF.

  ENDLOOP.

  " Narathip
  PERFORM F_UPDATE_EMPLOYMENT_STATUS .
ENDFORM.
FORM F_UPDATE_EMPLOYMENT_STATUS.

  DATA: LT_PERNR  TYPE RANGE OF PA0000-PERNR,
        LT_PA0000 TYPE TABLE OF PA0000,
        LS_CURR   TYPE PA0000,
        LS_LAST   TYPE PA0000,
        LV_TODAY  TYPE SY-DATUM,
        LV_PERNR  TYPE PA0000-PERNR.

  LV_TODAY = SY-DATUM.

  "Build PERNR range from GT_ALV
  LT_PERNR = VALUE #( FOR <LS> IN GT_ALV
                      WHERE ( PERNR IS NOT INITIAL )
                      ( SIGN = 'I' OPTION = 'EQ' LOW = <LS>-PERNR ) ).

  IF LT_PERNR IS INITIAL.
    RETURN.
  ENDIF.

  "Select only required fields for performance
  SELECT PERNR, STAT2, BEGDA, ENDDA
    INTO CORRESPONDING FIELDS OF TABLE @LT_PA0000
    FROM PA0000
    WHERE PERNR IN @LT_PERNR
      AND ( ENDDA = '99991231' OR ( BEGDA <= @LV_TODAY AND ENDDA >= @LV_TODAY ) ).

  SORT LT_PA0000 BY PERNR BEGDA DESCENDING.

  LOOP AT GT_ALV ASSIGNING FIELD-SYMBOL(<LS_ALV>).
    LV_PERNR = <LS_ALV>-PERNR.

    "Current employment (today)
    READ TABLE LT_PA0000 INTO LS_CURR
      WITH KEY PERNR = LV_PERNR
      BINARY SEARCH.
    IF SY-SUBRC = 0 AND LS_CURR-ENDDA >= LV_TODAY AND LS_CURR-BEGDA <= LV_TODAY.
      <LS_ALV>-EMP_STAT_CURR = LS_CURR-STAT2.
    ENDIF.

    "Last employment (ENDDA = 99991231)
    LOOP AT LT_PA0000 INTO LS_LAST
      WHERE PERNR = LV_PERNR AND ENDDA = '99991231'.
      <LS_ALV>-EMP_STAT_LAST = LS_LAST-STAT2.
      <LS_ALV>-BEGDA_LAST = LS_LAST-BEGDA.
      EXIT.
    ENDLOOP.
    IF <LS_ALV>-EMP_STAT_LAST = '0'
       AND ( <LS_ALV>-GLTGV IS INITIAL OR <LS_ALV>-GLTGV > <LS_ALV>-BEGDA_LAST ).
      <LS_ALV>-COLOR_FLAG = GC_ICON_ALERT.
    ENDIF.

  ENDLOOP.

ENDFORM.
FORM F_LOG_CREATE USING UV_PERNR TYPE PA0000-PERNR.
  DATA:
    L_LOG_HANDLE TYPE BALLOGHNDL,
    L_S_LOG      TYPE BAL_S_LOG,
    L_S_MSG      TYPE BAL_S_MSG,
    L_MSGNO      TYPE SYMSGNO.

* create an initial log file
  L_S_LOG-ALDATE     = SY-DATUM.
  L_S_LOG-ALUSER     = SY-UNAME.
  L_S_LOG-ALTIME     = SY-UZEIT.
  "l_s_log-object     = 'ZHRLOG'.         " define in SLG0
  "l_s_log-subobject  = 'INFOTYPE_OP'.
  L_S_LOG-EXTNUMBER  = UV_PERNR.
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      I_S_LOG      = L_S_LOG
    IMPORTING
      E_LOG_HANDLE = GV_LOG_HANDLE
    EXCEPTIONS
      OTHERS       = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.
FORM F_LOG_SHOW USING UT_LOG_HANDLE TYPE BAL_T_LOGH.

  DATA: L_S_DISPLAY_PROFILE TYPE BAL_S_PROF,
        LT_LOG_HANDLE       TYPE BAL_T_LOGH.

  " Get standard profile
  CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
    IMPORTING
      E_S_DISPLAY_PROFILE = L_S_DISPLAY_PROFILE.

  " Optional: ใช้ ALV Grid แสดง
  L_S_DISPLAY_PROFILE-USE_GRID = ABAP_TRUE.

  " ตั้งชื่อ report และ handle เพื่อ save layout ได้
  L_S_DISPLAY_PROFILE-DISVARIANT-REPORT = SY-REPID.
  L_S_DISPLAY_PROFILE-DISVARIANT-HANDLE = 'LOG'.

  " เตรียม log handle ที่ต้องการแสดง
*  APPEND GV_LOG_HANDLE TO LT_LOG_HANDLE.
  LT_LOG_HANDLE = UT_LOG_HANDLE[].

  " แสดง log
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      I_S_DISPLAY_PROFILE = L_S_DISPLAY_PROFILE
      I_T_LOG_HANDLE      = LT_LOG_HANDLE.


ENDFORM.
