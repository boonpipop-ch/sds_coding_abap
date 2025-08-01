*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0330
*  Creation Date      : 20.06.2023
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : Upload Asset Master and Balance
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0330.

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
TYPES: GTY_RESULT TYPE  ZSDSFIS108.
TYPES: GTTY_RESULT TYPE STANDARD TABLE OF GTY_RESULT.
TYPES: BEGIN OF GTY_SUM,
         TOTAL TYPE  I, "Total Entries
         SUCCS TYPE  I, "Success Entries
         ERROR TYPE  I, "Error Entries
       END OF GTY_SUM.
TYPES: BEGIN OF GTY_RAW,
         TLINE TYPE  STRING,
       END OF GTY_RAW.
TYPES: GTTY_RAW  TYPE  STANDARD TABLE OF GTY_RAW.

TYPES: GTY_MSGTX  TYPE  BAPI_MSG.

TYPES: BEGIN OF GTY_MESSG,
         MSGTY TYPE  SY-MSGTY,
         MSGID TYPE  SY-MSGID,
         MSGNO TYPE  SY-MSGNO,
         MSGTX TYPE  GTY_MSGTX,
       END OF GTY_MESSG.
TYPES: GTTY_MESSG  TYPE  STANDARD TABLE OF GTY_MESSG
                         WITH DEFAULT KEY.

TYPES: BEGIN OF GTY_DATA,
         ROWNO        TYPE  GTY_RESULT-ROWNO.
         INCLUDE TYPE ZSDSFIS107.
TYPES:   GJAHR        TYPE  GJAHR,
         BELNR        TYPE  BELNR_D,
         MESSG        TYPE  GTTY_MESSG,
         ASSET_EXT_NO TYPE  FLAG,
       END OF GTY_DATA.
TYPES: GTTY_DATA  TYPE  STANDARD TABLE OF GTY_DATA.
TYPES: GTY_FIELD_NAME  TYPE  CHAR40.

TYPES: BEGIN OF GTY_DATA_CREATE,
         ASSET_NO_OLD  TYPE ANLA-ANLN1,
         ASSET_SUB_OLD TYPE ANLA-ANLN2,
         SUB_FLG       TYPE FLAG,
         AUC_FLG       TYPE FLAG,
         ROWNO         TYPE  GTY_RESULT-ROWNO.
         INCLUDE TYPE ZSDSFIS107.
TYPES:   GJAHR         TYPE  GJAHR,
         BELNR         TYPE  BELNR_D,
         MESSG         TYPE  GTTY_MESSG,
         ASSET_EXT_NO  TYPE  FLAG,
       END OF GTY_DATA_CREATE.
TYPES: GTTY_DATA_CREATE  TYPE  STANDARD TABLE OF GTY_DATA_CREATE.

TYPES: GTY_LOG TYPE ZSDSFIS109.

TYPES: BEGIN OF GTY_FILE_INFO,
         DIRECTORY TYPE  STRING,
         FILENAME  TYPE  STRING,
         FULLNAME  TYPE  STRING,
       END OF GTY_FILE_INFO.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE       TYPE  CHAR1     VALUE 'X',
  GC_TCODE      TYPE  SY-TCODE  VALUE 'ZSDSFI030',
* Field value translated as not filled
  GC_DUMMY_FLD  TYPE  CHAR1       VALUE ' ',
  GC_AFAPL      TYPE ANKB-AFAPL VALUE '1000',
  GC_ANLKL_9999 TYPE ANLA-ANLKL VALUE '00009999',
  GC_CREDIT     TYPE P VALUE '-1',
* Splitter used in Raw data
  GC_SPLIT      TYPE CHAR1 VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT TYPE  GTTY_RESULT                           ##NEEDED.

DATA:
  GR_ANLKL_QTYCHECK TYPE RANGE OF ANLA-ANLKL  ##NEEDED.
*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_SUM     TYPE  GTY_SUM           ##NEEDED,
  GS_LOGFILE TYPE  GTY_FILE_INFO     ##NEEDED.

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
  GV_PARAM    TYPE  CHAR10                              ##NEEDED,
  GV_RUN_TEXT TYPE  TEXT50                              ##NEEDED,
  GV_BUKRS    TYPE T001-BUKRS                           ##NEEDED.


*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1 TYPE TABNAME  VALUE 'ZSDSFIS108'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 10,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 90.

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
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.


  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.

    PARAMETERS:  P_LFILE  TYPE  STRING LOWER CASE MODIF ID LOC.
    SELECTION-SCREEN BEGIN OF LINE.
*     Text-s03: Start Row
      SELECTION-SCREEN COMMENT 1(29) TEXT-S03 FOR FIELD P_BEGROW.
      PARAMETERS:
        P_BEGROW TYPE I DEFAULT 12 MODIF ID LOC.
*     Text-s04: Start Column
      SELECTION-SCREEN COMMENT 50(15) TEXT-S04 FOR FIELD P_BEGCOL.
      PARAMETERS:
        P_BEGCOL TYPE I DEFAULT 2 MODIF ID LOC.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
*     Text-s05: End Row
      SELECTION-SCREEN COMMENT 1(29) TEXT-S05 FOR FIELD P_ENDROW.
      PARAMETERS:
        P_ENDROW TYPE I DEFAULT 65536  MODIF ID LOC.
*     Text-s06: End Column
      SELECTION-SCREEN COMMENT 50(15) TEXT-S06 FOR FIELD P_ENDCOL.
      PARAMETERS:
        P_ENDCOL TYPE I DEFAULT 62 MODIF ID LOC.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK B2.


  SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.

    PARAMETERS: RB_NEW RADIOBUTTON GROUP RAD1 DEFAULT 'X' USER-COMMAND RAD.
    PARAMETERS: RB_CHG RADIOBUTTON GROUP RAD1.
    PARAMETERS: RB_REV RADIOBUTTON GROUP RAD1.

  SELECTION-SCREEN END OF BLOCK B3.

  PARAMETERS: CB_TEST TYPE CHAR1 AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK B1.


*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_INITIAL_SELECTION_SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_LFILE.

AT SELECTION-SCREEN.

  CASE SSCRFIELDS-UCOMM.
    WHEN 'ONLI'.
      PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDCASE.


AT SELECTION-SCREEN OUTPUT.
* Set Screen format
  PERFORM F_SET_SELECTION_SCREEN_FORMAT.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get Data
* Processing data
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT
                                  GS_SUM.

  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S829(63).
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
FORM F_AUTHORIZE_CHECK USING PV_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD PV_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH PV_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_LIST_IFILE
*----------------------------------------------------------------------*
*  Popup for Input file selection
*----------------------------------------------------------------------*
FORM F_LIST_IFILE  CHANGING PV_FILENAME  TYPE  STRING.

  DATA:
    LT_FILE     TYPE  FILETABLE.

  DATA:
    LV_RC     TYPE  I,
    LV_ACTION TYPE  I.

  FIELD-SYMBOLS:
    <LFS_FILE>  TYPE  FILE_TABLE.


* List Local File
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      DEFAULT_EXTENSION       = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER             = 'Excel File|*.XLSX;*.XLS' ##NO_TEXT
      MULTISELECTION          = SPACE
    CHANGING
      FILE_TABLE              = LT_FILE
      RC                      = LV_RC
      USER_ACTION             = LV_ACTION
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
  IF NOT ( LV_ACTION IS INITIAL AND
           LV_RC     EQ 1 ).
    RETURN.
  ENDIF.

  READ TABLE LT_FILE ASSIGNING <LFS_FILE>
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  PV_FILENAME = <LFS_FILE>-FILENAME.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

*  DATA:
*    LV_FILE TYPE  BTCXPGPAR,
*    LV_DIR  TYPE  STRING.

* -----------------------
* Local File Processing
* -----------------------
*   Local file not support in Background mode.
  IF SY-BATCH EQ GC_TRUE.
*     Error: Local file does not support in background processing mode.
    MESSAGE TEXT-E57 TYPE 'E'.
    RETURN.
  ENDIF.
  PERFORM F_VALIDATE_LOCAL_PARAM.

  IF CB_TEST = 'X'.
    GV_RUN_TEXT = TEXT-H08.
  ELSE.
    GV_RUN_TEXT = TEXT-H09.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_set_selection_screen_format
*----------------------------------------------------------------------*
*  Set Selection Screen format
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .

  LOOP AT SCREEN.

    CASE SCREEN-GROUP1.

*     --------------------------
*     For Display only fields
*     --------------------------
      WHEN 'DIS'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.

    ENDCASE.

    IF RB_NEW = 'X'.
      IF SCREEN-GROUP1 = 'REL'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.


  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_local_param
*----------------------------------------------------------------------*
*  Validate Local Parameter
*----------------------------------------------------------------------*
FORM F_VALIDATE_LOCAL_PARAM .

  DATA:
    LV_EXIST TYPE  FLAG.

* Input file is required
  IF P_LFILE IS INITIAL.
    SET CURSOR FIELD 'P_LFILE'.
*   Error: Please enter a valid filename.
    MESSAGE TEXT-E60 TYPE 'E'.
    RETURN.
  ENDIF.
* Check File Exists?
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
    EXPORTING
      FILE                 = P_LFILE
    RECEIVING
      RESULT               = LV_EXIST
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      WRONG_PARAMETER      = 3
      NOT_SUPPORTED_BY_GUI = 4
      OTHERS               = 5.
  IF SY-SUBRC <> 0.
    CLEAR LV_EXIST.
  ENDIF.
  IF LV_EXIST IS INITIAL.
    SET CURSOR FIELD 'P_LFILE'.
*   Error: File does not exist.
    MESSAGE TEXT-E58 TYPE 'E'.
    RETURN.
  ENDIF.
* Ranges are required
  IF P_BEGROW IS INITIAL OR
     P_BEGCOL IS INITIAL OR
     P_ENDROW IS INITIAL OR
     P_ENDCOL IS INITIAL.
    SET CURSOR FIELD 'P_BEGROW'.
*   Error: Please specified range to read excel file.
    MESSAGE TEXT-E59 TYPE 'E'.
    RETURN.
  ENDIF.


ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  PT_RESULT TYPE GTTY_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
* Soft refresh data
  GF_SOFT_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.

* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING PS_LAYOUT  TYPE  LVC_S_LAYO
                           PS_VARIANT TYPE  DISVARIANT
                           PS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  PS_LAYOUT, PS_VARIANT, PS_PRINT.

* determine layout
  PS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  PS_LAYOUT-CWIDTH_OPT = 'X'.
  PS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  PS_VARIANT-REPORT  = SY-REPID.

  PS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING PT_FIELDCAT  TYPE LVC_T_FCAT.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING PT_FIELDCAT.

  LOOP AT PT_FIELDCAT ASSIGNING <LFS_FIELDCAT>.
    <LFS_FIELDCAT>-NO_OUT    = SPACE.
    IF <LFS_FIELDCAT>-FIELDNAME = 'STATU'.
      <LFS_FIELDCAT>-REPTEXT = TEXT-T07.
      <LFS_FIELDCAT>-SCRTEXT_S = <LFS_FIELDCAT>-SCRTEXT_M = <LFS_FIELDCAT>-SCRTEXT_L = <LFS_FIELDCAT>-REPTEXT.
    ENDIF.
    IF <LFS_FIELDCAT>-FIELDNAME+0(5) = 'AFASL' OR
       <LFS_FIELDCAT>-FIELDNAME+0(5) = 'AFABG' OR
       <LFS_FIELDCAT>-FIELDNAME+0(5) = 'AFABE'.
      CONCATENATE <LFS_FIELDCAT>-REPTEXT   <LFS_FIELDCAT>-FIELDNAME+5(2) INTO <LFS_FIELDCAT>-REPTEXT.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_L <LFS_FIELDCAT>-FIELDNAME+5(2) INTO <LFS_FIELDCAT>-SCRTEXT_L.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_M <LFS_FIELDCAT>-FIELDNAME+5(2) INTO <LFS_FIELDCAT>-SCRTEXT_M.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_S <LFS_FIELDCAT>-FIELDNAME+5(2) INTO <LFS_FIELDCAT>-SCRTEXT_S.
    ENDIF.

    IF <LFS_FIELDCAT>-FIELDNAME+0(12) = 'ORD_DEP_PREV'.
      CONCATENATE <LFS_FIELDCAT>-REPTEXT   <LFS_FIELDCAT>-FIELDNAME+12(2) INTO <LFS_FIELDCAT>-REPTEXT.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_L <LFS_FIELDCAT>-FIELDNAME+12(2) INTO <LFS_FIELDCAT>-SCRTEXT_L.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_M <LFS_FIELDCAT>-FIELDNAME+12(2) INTO <LFS_FIELDCAT>-SCRTEXT_M.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_S <LFS_FIELDCAT>-FIELDNAME+12(2) INTO <LFS_FIELDCAT>-SCRTEXT_S.
    ELSEIF <LFS_FIELDCAT>-FIELDNAME+0(7) = 'ORD_DEP'.
      CONCATENATE <LFS_FIELDCAT>-REPTEXT   <LFS_FIELDCAT>-FIELDNAME+7(2) INTO <LFS_FIELDCAT>-REPTEXT.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_L <LFS_FIELDCAT>-FIELDNAME+7(2) INTO <LFS_FIELDCAT>-SCRTEXT_L.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_M <LFS_FIELDCAT>-FIELDNAME+7(2) INTO <LFS_FIELDCAT>-SCRTEXT_M.
      CONCATENATE <LFS_FIELDCAT>-SCRTEXT_S <LFS_FIELDCAT>-FIELDNAME+7(2) INTO <LFS_FIELDCAT>-SCRTEXT_S.
    ENDIF.

    IF <LFS_FIELDCAT>-FIELDNAME+0(5) = 'NDJAR'.
      CONCATENATE TEXT-T05 <LFS_FIELDCAT>-FIELDNAME+5(2) INTO <LFS_FIELDCAT>-REPTEXT.
      <LFS_FIELDCAT>-SCRTEXT_S = <LFS_FIELDCAT>-SCRTEXT_M = <LFS_FIELDCAT>-SCRTEXT_L =   <LFS_FIELDCAT>-REPTEXT.
    ENDIF.
    IF <LFS_FIELDCAT>-FIELDNAME+0(5) = 'NDPER'.
      CONCATENATE TEXT-T06 <LFS_FIELDCAT>-FIELDNAME+5(2) INTO <LFS_FIELDCAT>-REPTEXT.
      <LFS_FIELDCAT>-SCRTEXT_S = <LFS_FIELDCAT>-SCRTEXT_M = <LFS_FIELDCAT>-SCRTEXT_L =   <LFS_FIELDCAT>-REPTEXT.
    ENDIF.

    IF <LFS_FIELDCAT>-FIELDNAME+0(5) = 'SCHRW'.
      CONCATENATE TEXT-T03 <LFS_FIELDCAT>-FIELDNAME+5(2) INTO <LFS_FIELDCAT>-REPTEXT.
      <LFS_FIELDCAT>-SCRTEXT_S = <LFS_FIELDCAT>-SCRTEXT_M = <LFS_FIELDCAT>-SCRTEXT_L =   <LFS_FIELDCAT>-REPTEXT.
    ENDIF.

    IF <LFS_FIELDCAT>-FIELDNAME+0(10) = 'SCHRW_PROZ'.
      CONCATENATE TEXT-T04 <LFS_FIELDCAT>-FIELDNAME+10(2) INTO <LFS_FIELDCAT>-REPTEXT.
      <LFS_FIELDCAT>-SCRTEXT_S = <LFS_FIELDCAT>-SCRTEXT_M = <LFS_FIELDCAT>-SCRTEXT_L =   <LFS_FIELDCAT>-REPTEXT.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING PV_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
    LV_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LV_TEMP1     TYPE  TEXT50,
    LV_TEMP2     TYPE  TEXT50,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD PV_DYNDOC_ID->ADD_TABLE
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
* Text-h01 : Program:
  LV_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  CONCATENATE SY-REPID SY-TITLE GV_RUN_TEXT
         INTO LV_TEXT
    SEPARATED BY '-'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Processing Date/Time:
  LV_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE SY-DATUM TO LV_TEMP1.
  WRITE SY-UZEIT TO LV_TEMP2.
  CONCATENATE LV_TEMP1 LV_TEMP2
         INTO LV_TEXT
    SEPARATED BY SPACE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : System/Client:
  LV_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  CONCATENATE SY-SYSID SY-MANDT
         INTO LV_TEXT
    SEPARATED BY '/'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Process By:
  LV_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  LV_TEXT = SY-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line 5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : Total Records:
  LV_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE GS_SUM-TOTAL TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

*-----------------------
* Add value in Line 6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Success Records:
  LV_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE GS_SUM-SUCCS TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LV_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

*-----------------------
* Add value in Line 7
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h07 : Failed Records:
  LV_TEXT = TEXT-H07.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.
  WRITE GS_SUM-ERROR TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LV_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_NEGATIVE.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LV_COL01 TYPE  I VALUE 18,
    LV_COL02 TYPE  I VALUE 35,
    LV_TEXT  TYPE  STRING,
    LV_TEMP1 TYPE  TEXT50,
    LV_TEMP2 TYPE  TEXT50.


  CONCATENATE SY-REPID SY-TITLE
         INTO LV_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  WRITE AT: /1(LV_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

  WRITE SY-DATUM TO LV_TEMP1.
  WRITE SY-UZEIT TO LV_TEMP2.
  CONCATENATE LV_TEMP1 LV_TEMP2
         INTO LV_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  WRITE AT: /1(LV_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LV_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  WRITE AT: /1(LV_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (LV_COL02)    LV_TEXT NO-GAP.

* Text-h04 : Process By:
  WRITE AT: /1(LV_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LV_COL02)    SY-UNAME NO-GAP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA  CHANGING PT_RESULT TYPE GTTY_RESULT
                              PS_SUM    TYPE GTY_SUM.

  DATA:
*    LT_RESULT TYPE  GTTY_RESULT,
    LT_RAW  TYPE  GTTY_RAW,
    LT_DATA TYPE  GTTY_DATA.


* --------------------------------
* Step1: Read Input file data
* --------------------------------
  PERFORM F_READ_INPUT_FILE CHANGING LT_RAW.
*                                     LT_RESULT.

* --------------------------------
* Step2: Validate Input file
* --------------------------------
  PERFORM F_VALIDATE_FILE  USING  LT_RAW
                         CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data from file
* --------------------------------
* Upload file
  PERFORM F_UPLOAD_FILE  USING  LT_DATA
                       CHANGING PT_RESULT
                                PS_SUM.

* --------------------------------
* Step4: Create Log File
* --------------------------------

  PERFORM F_CREATE_LOGFILE_FRONTEND  USING  GS_LOGFILE
                                            PT_RESULT.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_logfile_frontend
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_CREATE_LOGFILE_FRONTEND  USING PS_LOGFILE  TYPE  GTY_FILE_INFO
                                      PT_RESULT   TYPE  GTTY_RESULT.

  DATA:
    LT_LOG   TYPE  GTTY_RAW.

  DATA:
    LS_MESSG  TYPE  GTY_MESSG.


* Convert Result into Text table
  PERFORM F_CONVERT_RESULT_TO_TEXT  USING  PT_RESULT
                                  CHANGING LT_LOG.

* Generate file to local path
  PERFORM F_CREATE_LOCAL_FILE_LOG  USING PS_LOGFILE-FULLNAME
                                         LT_LOG
                                CHANGING LS_MESSG.

  IF LS_MESSG IS NOT INITIAL.
    MESSAGE LS_MESSG-MSGTX TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ELSE.
*   Message: Log file &1 has been created.
    LS_MESSG-MSGTX = TEXT-I05.
    REPLACE '&1' IN LS_MESSG-MSGTX WITH PS_LOGFILE-FILENAME .
    RETURN.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_convert_result_to_text
*----------------------------------------------------------------------*
*  Convert Result into Text table
*----------------------------------------------------------------------*
FORM F_CONVERT_RESULT_TO_TEXT  USING  PT_RESULT  TYPE  GTTY_RESULT
                             CHANGING PT_RAW     TYPE  GTTY_RAW.

  DATA:
    LS_LOG      TYPE  GTY_LOG,
    LS_RAW      TYPE  GTY_RAW,
    LV_FIELD    TYPE  GTY_FIELD_NAME,
    LV_DATE     TYPE  CHAR8,
    LV_DATE_TMP TYPE  CHAR8.

  DATA:
    LV_INDEX  TYPE  I,
    LV_INDEX2 TYPE  I.

  FIELD-SYMBOLS:
    <LFS_RESULT> TYPE  GTY_RESULT,
    <LFS_FIELD>  TYPE  CLIKE.


* Initialize Output
  CLEAR: PT_RAW.

* Insert Header Section of Log
  PERFORM F_INSERT_LOG_HEADER CHANGING PT_RAW.

  LOOP AT PT_RESULT ASSIGNING <LFS_RESULT>.

    PERFORM F_ASSIGN_LOG  USING  <LFS_RESULT>
                        CHANGING LS_LOG.

    PERFORM CONVERSION_EXIT_ALPHA_OUTPUT CHANGING LS_LOG.

    DO.
      LV_INDEX = SY-INDEX.
      ASSIGN COMPONENT LV_INDEX OF STRUCTURE LS_LOG TO <LFS_FIELD>.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
      CONDENSE <LFS_FIELD>.
      IF LV_INDEX EQ 1.
        LS_RAW-TLINE = <LFS_FIELD>.
      ELSE.

        LV_INDEX2 = LV_INDEX - 2.
        PERFORM F_GET_TARGET_FIELD  USING  LV_INDEX2
                                           'ZSDSFIS109'
                                  CHANGING LV_FIELD.
        CASE LV_FIELD.

*Change format from yyyymmdd to ddmmyyyy
          WHEN 'IVDAT' OR
               'AKTIV' OR
               'AFABG01' OR
               'AFABG12'.
            CLEAR: LV_DATE,LV_DATE_TMP.
            LV_DATE_TMP = <LFS_FIELD>.
            CONCATENATE LV_DATE_TMP+6(2) LV_DATE_TMP+4(2) LV_DATE_TMP+0(4) INTO LV_DATE.
            <LFS_FIELD> = LV_DATE.
        ENDCASE.


        CONCATENATE LS_RAW-TLINE
                    <LFS_FIELD>
               INTO LS_RAW-TLINE
          SEPARATED BY GC_SPLIT.
      ENDIF.


    ENDDO.


    INSERT LS_RAW INTO TABLE PT_RAW.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_insert_log_header
*----------------------------------------------------------------------*
*  Insert Header Section of Log file
*----------------------------------------------------------------------*
FORM F_INSERT_LOG_HEADER  CHANGING PT_RAW TYPE GTTY_RAW.

  DATA:
    LT_FIELDCAT TYPE  LVC_T_FCAT,
    LT_FIELDS   TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  DATA:
    LS_RAW  TYPE  GTY_RAW.

  DATA:
    LV_TEXT  TYPE  TEXT50,
    LV_TEMP1 TYPE  TEXT50,
    LV_TEMP2 TYPE  TEXT50,
    LV_INDEX TYPE  I.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT> TYPE  LVC_S_FCAT,
    <LFS_FIELD>    TYPE  ABAP_COMPDESCR.


* *********************
* File Header
* *********************
  CONCATENATE SY-REPID SY-TITLE
         INTO LV_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  CONCATENATE TEXT-H01
              LV_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE PT_RAW.

  WRITE SY-DATUM TO LV_TEMP1.
  WRITE SY-UZEIT TO LV_TEMP2.
  CONCATENATE LV_TEMP1 LV_TEMP2
         INTO LV_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  CONCATENATE TEXT-H02
              LV_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE PT_RAW.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LV_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  CONCATENATE TEXT-H03
              LV_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE PT_RAW.

* Text-h04 : Process By:
  CONCATENATE TEXT-H04
              SY-UNAME
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE PT_RAW.

  WRITE GS_SUM-TOTAL TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
* Text-h05 : Total Records:
  CONCATENATE TEXT-H05
              LV_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE PT_RAW.

  WRITE GS_SUM-SUCCS TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
* Text-h06 : Success Records:
  CONCATENATE TEXT-H06
              LV_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE PT_RAW.

  WRITE GS_SUM-ERROR TO LV_TEXT LEFT-JUSTIFIED.
  CONDENSE LV_TEXT NO-GAPS.
* Text-h07 : Failed Records:
  CONCATENATE TEXT-H07
              LV_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE PT_RAW.

* *********************
* Generate Column header
* *********************
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING LT_FIELDCAT.
  SORT LT_FIELDCAT BY FIELDNAME ASCENDING.

* Get List of LOG File fields
  LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'GTY_LOG' ).
  LREF_STRUC ?= LREF_DESCR.
  LT_FIELDS = LREF_STRUC->COMPONENTS.

  CLEAR LS_RAW.
  CLEAR LV_INDEX.
  LOOP AT LT_FIELDS ASSIGNING <LFS_FIELD>.

    LV_INDEX = SY-TABIX.

    CLEAR LV_TEXT.
    READ TABLE LT_FIELDCAT ASSIGNING <LFS_FIELDCAT>
                           WITH KEY FIELDNAME = <LFS_FIELD>-NAME
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IF <LFS_FIELDCAT>-SCRTEXT_M IS NOT INITIAL.
        LV_TEXT = <LFS_FIELDCAT>-SCRTEXT_M.
      ELSE.
        LV_TEXT = <LFS_FIELDCAT>-SCRTEXT_L.
      ENDIF.
    ENDIF.

    IF LV_INDEX EQ 1.
      LS_RAW-TLINE = LV_TEXT.
    ELSE.
      CONCATENATE LS_RAW-TLINE
                  LV_TEXT
             INTO LS_RAW-TLINE
        SEPARATED BY GC_SPLIT.
    ENDIF.

  ENDLOOP.
  INSERT LS_RAW INTO TABLE PT_RAW.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_assign_log
*----------------------------------------------------------------------*
*  Assign Data to Log Structure
*----------------------------------------------------------------------*
FORM F_ASSIGN_LOG  USING  PS_RESULT  TYPE  GTY_RESULT
                 CHANGING PS_LOG     TYPE  GTY_LOG.

* Initialize Output
  CLEAR: PS_LOG.
  MOVE-CORRESPONDING PS_RESULT TO PS_LOG ##ENH_OK.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_local_file_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_CREATE_LOCAL_FILE_LOG  USING  PV_FNAME  TYPE  STRING
                                     PT_RAW    TYPE  GTTY_RAW
                            CHANGING PS_MESSG  TYPE  GTY_MESSG.
  DATA:
    LT_RAW    TYPE  GTTY_RAW.

  DATA:
    LV_FILENAME TYPE  STRING.

* Initialize Output
  CLEAR: PS_MESSG.

  LV_FILENAME = PV_FNAME.
  LT_RAW[] = PT_RAW[].
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
    EXPORTING
*     bin_filesize            =
      FILENAME                = LV_FILENAME
      FILETYPE                = 'ASC'
*     write_field_separator   = SPACE
      CODEPAGE                = '4110'  "UTF-8
    CHANGING
      DATA_TAB                = LT_RAW
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      NOT_SUPPORTED_BY_GUI    = 22
      ERROR_NO_GUI            = 23
      OTHERS                  = 24.
  IF SY-SUBRC <> 0.
    PS_MESSG-MSGTX = TEXT-E35.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_initial_selection_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_INITIAL_SELECTION_SCREEN .

  CONSTANTS:
    LC_ICON1 TYPE ICON-ID VALUE '@Y4@',
    LC_ICON2 TYPE ICON-ID VALUE '@Y5@'.

  DATA:
    LS_DYNTXT TYPE SMP_DYNTXT.

  CLEAR LS_DYNTXT.
  LS_DYNTXT-ICON_ID   = LC_ICON1.
* Text-t01: Download File
  LS_DYNTXT-QUICKINFO = TEXT-T01.
  LS_DYNTXT-ICON_TEXT = TEXT-T01.
  SSCRFIELDS-FUNCTXT_01 = LS_DYNTXT.

  CLEAR LS_DYNTXT.
  LS_DYNTXT-ICON_ID   = LC_ICON2.
* Text-t02: Upload File to AppServer
  LS_DYNTXT-QUICKINFO = TEXT-T02.
  LS_DYNTXT-ICON_TEXT = TEXT-T02.
  SSCRFIELDS-FUNCTXT_02 = LS_DYNTXT.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_input_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_READ_INPUT_FILE  CHANGING PT_RAW    TYPE  GTTY_RAW.
*                                 PT_RESULT TYPE  GTTY_RESULT.
  "pt_result TYPE  gtty_result.
* Show Progress
* Text-p01 : Reading Input file. . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

  PERFORM F_READ_EXCEL USING   P_LFILE
                               P_BEGROW
                               P_BEGCOL
                               P_ENDROW
                               P_ENDCOL
                      CHANGING PT_RAW.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_read_excel
*----------------------------------------------------------------------*
*  Read Excel file into Raw internal table
*----------------------------------------------------------------------*
FORM F_READ_EXCEL  USING  PV_IFILE   TYPE CLIKE
                          PV_BEGROW  TYPE I
                          PV_BEGCOL  TYPE I
                          PV_ENDROW  TYPE I
                          PV_ENDCOL  TYPE I
                 CHANGING PT_RAW     TYPE GTTY_RAW.

  DATA:
    LT_TAB   TYPE  STANDARD TABLE OF ZSDSCAS016.

  DATA:
    LS_RAW       TYPE  GTY_RAW.

  DATA:
    LV_FILENAME TYPE  STRING,
    LV_BEGROW   TYPE  I,
    LV_BEGCOL   TYPE  I,
    LV_ROW      TYPE  I,
    LV_COL      TYPE  I.

  FIELD-SYMBOLS:
    <LFS_TAB>  TYPE  ZSDSCAS016.


* Initialize Output
  CLEAR: PT_RAW.
  "pt_result.

* Assign File name
  LV_FILENAME = PV_IFILE.

  LV_BEGCOL = PV_BEGCOL.
  LV_BEGROW = PV_BEGROW.

  CALL FUNCTION 'Z_SDSCA_EXCEL_TO_ITAB'
    EXPORTING
      FILENAME                = LV_FILENAME
      I_BEGIN_COL             = LV_BEGCOL
      I_BEGIN_ROW             = LV_BEGROW
      I_END_COL               = PV_ENDCOL
      I_END_ROW               = PV_ENDROW
    TABLES
      INTERN                  = LT_TAB
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
*   Append Error

    RETURN.
  ENDIF.

  SORT LT_TAB BY ROW ASCENDING
                 COL ASCENDING.

  LOOP AT LT_TAB ASSIGNING <LFS_TAB>.

*   -----------
*   On New Row
*   -----------
    IF <LFS_TAB>-ROW NE LV_ROW.

      IF LV_ROW IS NOT INITIAL.
        INSERT LS_RAW INTO TABLE PT_RAW.
      ENDIF.

*     Initialize New Line
      LV_ROW = <LFS_TAB>-ROW.
      LV_COL = 1.
      CLEAR LS_RAW.

    ENDIF.

*   -----------
*   Add Blank Cell
*   -----------
    WHILE LV_COL LT <LFS_TAB>-COL.
      IF LV_COL GT 1.
        CONCATENATE LS_RAW-TLINE GC_SPLIT
               INTO LS_RAW-TLINE.
      ENDIF.
      LV_COL = LV_COL + 1.
    ENDWHILE.

*   -----------
*   Assign column value
*   -----------
    IF LV_COL EQ 1.
      LS_RAW-TLINE = <LFS_TAB>-VALUE.
    ELSE.
      CONCATENATE LS_RAW-TLINE <LFS_TAB>-VALUE
             INTO LS_RAW-TLINE
        SEPARATED BY GC_SPLIT.
    ENDIF.

    LV_COL = LV_COL + 1.

*   Insert last line
    AT LAST.
      INSERT LS_RAW INTO TABLE PT_RAW.
    ENDAT.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_file
*----------------------------------------------------------------------*
*  Validate File data
*----------------------------------------------------------------------*
FORM F_VALIDATE_FILE  USING  PT_RAW     TYPE  GTTY_RAW
                    CHANGING PT_DATA    TYPE  GTTY_DATA.

  DATA: LS_DATA  TYPE GTY_DATA,
        LS_MESSG TYPE  GTY_MESSG.

  DATA:
    LV_ROWNO   TYPE  GTY_RESULT-ROWNO.

  FIELD-SYMBOLS:
    <LFS_RAW>   TYPE  GTY_RAW.


* Initialize Output
  CLEAR: PT_DATA.

* Show Progress
* Text-p02 : Validating file data. . .
  MC_SHOW_PROGRESS 20 TEXT-P02.

  LV_ROWNO = P_BEGROW - 1.

  LOOP AT PT_RAW ASSIGNING <LFS_RAW>.

    LV_ROWNO = LV_ROWNO + 1.

*   Translate Raw line into variables
    PERFORM F_TRANSLATE_RAW  USING <LFS_RAW>
                          CHANGING LS_DATA
                                   LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND  USING  LV_ROWNO
                                          LS_DATA
                                          LS_MESSG
                                 CHANGING PT_DATA.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_translate_raw
*----------------------------------------------------------------------*
*  Convert Raw to Data
*----------------------------------------------------------------------*
FORM F_TRANSLATE_RAW  USING  PS_RAW       TYPE  GTY_RAW
                    CHANGING PS_DATA      TYPE  GTY_DATA
                             PS_MESSG     TYPE  GTY_MESSG.

  DATA:
    LT_SPLIT  TYPE  STANDARD TABLE OF STRING.

  DATA:
    LS_DATA     TYPE  GTY_DATA,
    LV_INDEX    TYPE  I,
    LV_FIELD    TYPE  GTY_FIELD_NAME,
    LV_STRING   TYPE  STRING,
    LV_MSGTX    TYPE  GTY_MESSG-MSGTX,
    LV_REQUIRED TYPE  CHAR1,
    LV_ANLN1    TYPE  STRING ##NEEDED.


* Initialize Output
  CLEAR: PS_MESSG.

* Split Into Fields
  SPLIT PS_RAW-TLINE AT GC_SPLIT INTO TABLE LT_SPLIT.

  LOOP AT LT_SPLIT INTO LV_STRING.

    LV_INDEX = SY-TABIX.

*   Initialize Variables
    CLEAR: LV_MSGTX.

*   Ignore Dummy Value
    CHECK LV_STRING NE GC_DUMMY_FLD ##BLANK_OK.

*   Get Target Field name to assign value
    PERFORM F_GET_TARGET_FIELD  USING  LV_INDEX
                                       'ZSDSFIS107'
                              CHANGING LV_FIELD.
    CONDENSE LV_STRING.

    CASE LV_FIELD.
*     ---------------------------
*     Asset Class
*     ---------------------------
      WHEN 'ANLKL'.
        PERFORM F_VALIDATE_ASSET_CLASS  USING LV_STRING
                                     CHANGING LS_DATA-ANLKL
                                              LV_MSGTX.
*     ---------------------------
*     Company Code
*     ---------------------------
      WHEN 'BUKRS'.
        LS_DATA-BUKRS = LV_STRING.

*     ---------------------------
*     Asset number
*     ---------------------------
      WHEN 'ANLN1'.
        LS_DATA-ANLN1 = LV_STRING.
        LV_ANLN1 = LV_STRING.

*     ---------------------------
*     Asset Subnumber
*     ---------------------------
      WHEN 'ANLN2'.
        LS_DATA-ANLN2 = LV_STRING.


*     ---------------------------
*     Asset description 1
*     ---------------------------
      WHEN 'TXT50'.
        LS_DATA-TXT50 = LV_STRING.

*     ---------------------------
*     Asset description 2
*     ---------------------------
      WHEN 'TXA50'.
        LS_DATA-TXA50 = LV_STRING.

*     ---------------------------
*     Asset Main Number Text
*     ---------------------------
      WHEN 'ANLHTXT'.
        LS_DATA-ANLHTXT = LV_STRING.

*     ---------------------------
*     Serial number
*     ---------------------------
      WHEN 'SERNR'.
        LS_DATA-SERNR = LV_STRING.

*     ---------------------------
*     Inventory Number
*     ---------------------------
      WHEN 'INVNR'.
        PERFORM F_VALIDATE_INVENTORY_NO USING LS_DATA-BUKRS
                                              LV_STRING
                                              LS_DATA-ASSET_EXT_NO
                                         CHANGING LS_DATA-INVNR
                                                  LV_MSGTX.
*     ---------------------------
*     Quantity
*     ---------------------------
      WHEN 'MENGE'.
        PERFORM F_VALIDATE_MENGE  USING LV_STRING
                                        LS_DATA-ANLKL
                               CHANGING LS_DATA-MENGE
                                        LV_MSGTX.

*     ---------------------------
*     Uom
*     ---------------------------
      WHEN 'MEINS'.
        PERFORM F_VALIDATE_UNIT  USING  LV_STRING
                                        LS_DATA-ANLKL
                               CHANGING LS_DATA-MEINS
                                        LV_MSGTX.
*     ---------------------------
*     Last Inventory Date
*     ---------------------------
      WHEN 'IVDAT'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING LS_DATA-IVDAT
                                        LV_MSGTX.
*     ---------------------------
*     Inventory Note
*     ---------------------------
      WHEN 'INVZU'.
        LS_DATA-INVZU = LV_STRING.

*     ---------------------------
*     Capitalized On
*     ---------------------------
      WHEN 'AKTIV'.
        PERFORM F_VALIDATE_DATE  USING  LV_STRING
                               CHANGING LS_DATA-AKTIV
                                        LV_MSGTX.

*     ---------------------------
*     Cost Center
*     ---------------------------
      WHEN 'KOSTL'.
        PERFORM F_VALIDATE_COST_CENTER   USING LV_STRING
                                      CHANGING LS_DATA-KOSTL
                                               LV_MSGTX.

*     ---------------------------
*     Asset Plant
*     ---------------------------
      WHEN 'WERKS'.
        PERFORM F_VALIDATE_PLANT   USING LV_STRING      "Plant
                                  CHANGING LS_DATA-WERKS
                                           LV_MSGTX.
*     ---------------------------
*     Asset Location
*     ---------------------------
      WHEN 'STORT'.
        PERFORM F_VALIDATE_LOCATION  USING LV_STRING      "Location
                                           LS_DATA-WERKS
                                  CHANGING LS_DATA-STORT
                                           LV_MSGTX.
*     ---------------------------
*     License Plate Number
*     ---------------------------
      WHEN 'KFZKZ'.
        LS_DATA-KFZKZ = LV_STRING.

*     ---------------------------
*     Room
*     ---------------------------
      WHEN 'RAUMN'.
        LS_DATA-RAUMN = LV_STRING.


*     ---------------------------
*     Asset Status
*     ---------------------------
      WHEN 'ORD41'.
        PERFORM F_VALIDATE_ASSET_STATUS1  USING LV_STRING
                                       CHANGING LS_DATA-ORD41
                                                LV_MSGTX.
*     ---------------------------
*     Tag Barcode
*     ---------------------------
      WHEN 'ORD42'.
        PERFORM F_VALIDATE_ASSET_STATUS2  USING LV_STRING
                                       CHANGING LS_DATA-ORD42
                                                LV_MSGTX.

*     ---------------------------
*     Project Accrued
*     ---------------------------
      WHEN 'ORD43'.
        PERFORM F_VALIDATE_ASSET_STATUS3  USING LV_STRING
                                       CHANGING LS_DATA-ORD43
                                                LV_MSGTX.

*     ---------------------------
*     Asset Type
*     ---------------------------
      WHEN 'GDLGRP'.
        PERFORM F_VALIDATE_ASSET_TYPE    USING LV_STRING
                                      CHANGING LS_DATA-GDLGRP
                                               LV_MSGTX.
*     ---------------------------
*     Asset Super Number
*     ---------------------------
      WHEN 'ANLUE'.
        PERFORM F_VALIDATE_SUPER_ASSET_NUMBER    USING LV_STRING
                                              CHANGING LS_DATA-ANLUE
                                                      LV_MSGTX.

*     ---------------------------
*     Vendor Code
*     ---------------------------
      WHEN 'VENDOR_NO'.
        PERFORM F_VALIDATE_VENDOR_CODE        USING LV_STRING
                                              CHANGING LS_DATA-VENDOR_NO
                                                       LV_MSGTX.

*     ---------------------------
*     Vendor Name
*     ---------------------------
      WHEN 'VENDOR'.
        LS_DATA-VENDOR = LV_STRING.

*     ---------------------------
*     Manufacturer
*     ---------------------------
      WHEN 'HERST'.
        LS_DATA-HERST = LV_STRING.

*     ---------------------------
*     Asset Type Name
*     ---------------------------
      WHEN 'TYPBZ'.
        LS_DATA-TYPBZ = LV_STRING.

*     ---------------------------
*     WBS Element
*     ---------------------------
      WHEN 'WBS_ELEMENT'.
        LS_DATA-WBS_ELEMENT = LV_STRING.

*     ---------------------------
*     TAX Office
*     ---------------------------
      WHEN 'TAX_OFFICE'.
        LS_DATA-TAX_OFFICE = LV_STRING.

*     ---------------------------
*     TAX No
*     ---------------------------
      WHEN 'TAX_NO'.
        LS_DATA-TAX_NO = LV_STRING.

*     ---------------------------
*     Municipality
*     ---------------------------
      WHEN 'MUNICIPALITY'.
        LS_DATA-MUNICIPALITY = LV_STRING.

*     ---------------------------
*     Plot no
*     ---------------------------
      WHEN 'PLOT_NO'.
        LS_DATA-PLOT_NO = LV_STRING.

*     ---------------------------
*     Area
*     ---------------------------
      WHEN 'AREA'.
        PERFORM F_VALIDATE_AMOUNT        USING LV_STRING
                                      CHANGING LS_DATA-AREA
                                               LV_MSGTX.
*        ls_data-area = lv_string.

*     ---------------------------
*     Area UOM
*     ---------------------------
      WHEN 'AREA_UOM'.
        LS_DATA-AREA_UOM = LV_STRING.

*     ---------------------------
*     Depre Key (01)
*     ---------------------------
      WHEN 'AFASL01'.
        PERFORM F_VALIDATE_DEPRE_KEY     USING LV_STRING
                                      CHANGING LS_DATA-AFASL01
                                               LV_MSGTX.

*     ---------------------------
*     Depre Key (10)
*     ---------------------------
      WHEN 'AFASL10'.
        PERFORM F_VALIDATE_DEPRE_KEY     USING LV_STRING
                                      CHANGING LS_DATA-AFASL10
                                               LV_MSGTX.

*     ---------------------------
*     Depre Key (11)
*     ---------------------------
      WHEN 'AFASL11'.
        PERFORM F_VALIDATE_DEPRE_KEY     USING LV_STRING
                                      CHANGING LS_DATA-AFASL11
                                               LV_MSGTX.

*     ---------------------------
*     ODep. Start Date (01)
*     ---------------------------
      WHEN 'AFABG01'.
        PERFORM F_VALIDATE_DATE          USING LV_STRING
                                      CHANGING LS_DATA-AFABG01
                                               LV_MSGTX.

*     ---------------------------
*     ODep. Start Date (10)
*     ---------------------------
      WHEN 'AFABG10'.
        PERFORM F_VALIDATE_DATE          USING LV_STRING
                                      CHANGING LS_DATA-AFABG10
                                               LV_MSGTX.

*     ---------------------------
*     ODep. Start Date (11)
*     ---------------------------
      WHEN 'AFABG11'.
        PERFORM F_VALIDATE_DATE          USING LV_STRING
                                      CHANGING LS_DATA-AFABG11
                                               LV_MSGTX.

*     ---------------------------
*     Useful life year (01)
*     ---------------------------
      WHEN 'NDJAR01'.
        PERFORM F_VALIDATE_AMOUNT_USEFUL_LIFE    USING LV_STRING
                                      CHANGING LS_DATA-NDJAR01
                                               LV_MSGTX.

*     ---------------------------
*     Useful life year (10)
*     ---------------------------
      WHEN 'NDJAR10'.
        PERFORM F_VALIDATE_AMOUNT_USEFUL_LIFE    USING LV_STRING
                                      CHANGING LS_DATA-NDJAR10
                                               LV_MSGTX.

*     ---------------------------
*     Useful life year (11)
*     ---------------------------
      WHEN 'NDJAR11'.
        PERFORM F_VALIDATE_AMOUNT_USEFUL_LIFE     USING LV_STRING
                                      CHANGING LS_DATA-NDJAR11
                                               LV_MSGTX.

*     ---------------------------
*     Useful life Period (01)
*     ---------------------------
      WHEN 'NDPER01'.
        PERFORM F_VALIDATE_AMOUNT_USEFUL_LIFE    USING LV_STRING
                                      CHANGING LS_DATA-NDPER01
                                               LV_MSGTX.

*     ---------------------------
*     Useful life Period (10)
*     ---------------------------
      WHEN 'NDPER10'.
        PERFORM F_VALIDATE_AMOUNT_USEFUL_LIFE     USING LV_STRING
                                      CHANGING LS_DATA-NDPER10
                                               LV_MSGTX.

*     ---------------------------
*     Scrap Amount 01 Book
*     ---------------------------
      WHEN 'SCHRW01'.
        PERFORM F_VALIDATE_AMOUNT_SCRAP     USING LV_STRING
                                      CHANGING LS_DATA-SCHRW01
                                               LV_MSGTX.

*     ---------------------------
*     Scrap Amount 10 Book
*     ---------------------------
      WHEN 'SCHRW10'.
        PERFORM F_VALIDATE_AMOUNT_SCRAP     USING LV_STRING
                                      CHANGING LS_DATA-SCHRW10
                                               LV_MSGTX.

*     ---------------------------
*     Scrap Amount 11 Book
*     ---------------------------
      WHEN 'SCHRW11'.
        PERFORM F_VALIDATE_AMOUNT_SCRAP     USING LV_STRING
                                      CHANGING LS_DATA-SCHRW11
                                               LV_MSGTX.

*     ---------------------------
*     Percentage Scrap 01 Book
*     ---------------------------
      WHEN 'SCHRW_PROZ01'.
        PERFORM F_VALIDATE_AMOUNT_SCRAP      USING LV_STRING
                                      CHANGING LS_DATA-SCHRW_PROZ01
                                               LV_MSGTX.

*     ---------------------------
*     Percentage Scrap 12 Book
*     ---------------------------
      WHEN 'SCHRW_PROZ12'.
        PERFORM F_VALIDATE_AMOUNT_SCRAP     USING LV_STRING
                                      CHANGING LS_DATA-SCHRW_PROZ12
                                               LV_MSGTX.
*     ---------------------------
*     Acq.Value (THB) for Area 01, 12
*     ---------------------------
      WHEN 'KANSW'.
        PERFORM F_VALIDATE_AMOUNT        USING LV_STRING
                                      CHANGING LS_DATA-KANSW "#EC CI_FLDEXT_OK[2610650]
                                               LV_MSGTX.

*     ---------------------------
*     Ord.Dep previous 01
*     ---------------------------
      WHEN 'ORD_DEP_PREV01'.
        PERFORM F_VALIDATE_AMOUNT        USING LV_STRING
                                      CHANGING LS_DATA-ORD_DEP_PREV01 "#EC CI_FLDEXT_OK[2610650]
                                               LV_MSGTX.

*     ---------------------------
*     Ord.Dep 01
*     ---------------------------
      WHEN 'ORD_DEP01'.
        PERFORM F_VALIDATE_AMOUNT        USING LV_STRING
                                      CHANGING LS_DATA-ORD_DEP01 "#EC CI_FLDEXT_OK[2610650]
                                               LV_MSGTX.

*     ---------------------------
*     Ord.Dep previous 10
*     ---------------------------
      WHEN 'ORD_DEP_PREV10'.
        PERFORM F_VALIDATE_AMOUNT        USING LV_STRING
                                      CHANGING LS_DATA-ORD_DEP_PREV10 "#EC CI_FLDEXT_OK[2610650]
                                               LV_MSGTX.

*     ---------------------------
*     Ord.Dep 10
*     ---------------------------
      WHEN 'ORD_DEP10'.
        PERFORM F_VALIDATE_AMOUNT        USING LV_STRING
                                      CHANGING LS_DATA-ORD_DEP10 "#EC CI_FLDEXT_OK[2610650]
                                               LV_MSGTX.

*     ---------------------------
*     Ord.Dep previous 11
*     ---------------------------
      WHEN 'ORD_DEP_PREV11'.
        PERFORM F_VALIDATE_AMOUNT        USING LV_STRING
                                      CHANGING LS_DATA-ORD_DEP_PREV11 "#EC CI_FLDEXT_OK[2610650]
                                               LV_MSGTX.

*     ---------------------------
*     Ord.Dep 11
*     ---------------------------
      WHEN 'ORD_DEP11'.
        PERFORM F_VALIDATE_AMOUNT        USING LV_STRING
                                      CHANGING LS_DATA-ORD_DEP11 "#EC CI_FLDEXT_OK[2610650]
                                               LV_MSGTX.

      WHEN OTHERS.
        CONTINUE.

    ENDCASE.

    PS_DATA = LS_DATA.

*   If error, assign message and ignore the rest
    IF LV_MSGTX IS NOT INITIAL.
*     Do not override previous error
      IF PS_MESSG-MSGTY NE 'E'.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
        PS_MESSG-MSGTX = LV_MSGTX.
      ENDIF.
*     Processing until all required fields
      IF LV_REQUIRED EQ GC_TRUE.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.


ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_target_field
*----------------------------------------------------------------------*
*  Get Target Field name based on column sequence
*----------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD  USING  PV_INDEX  TYPE  I
                                PV_NAME   TYPE ANY
                       CHANGING PV_FIELD  TYPE  GTY_FIELD_NAME.

  STATICS:
    LT_FIELDS   TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
    <LFS_FIELD>  TYPE  ABAP_COMPDESCR.


* Initialize Output
  CLEAR: PV_FIELD.

  IF LT_FIELDS IS INITIAL.
*   Get Template Fields Sequence
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( PV_NAME ). "ZSDSFIS107
    LREF_STRUC ?= LREF_DESCR.
    LT_FIELDS = LREF_STRUC->COMPONENTS.
  ENDIF.

  READ TABLE LT_FIELDS ASSIGNING <LFS_FIELD>
                       INDEX PV_INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  PV_FIELD = <LFS_FIELD>-NAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_asset_class
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ASSET_CLASS  USING PV_STRING  TYPE  STRING
                          CHANGING PV_ANLKL   TYPE  ANLA-ANLKL
                                   PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_ANKB,
           ANLKL TYPE  ANKB-ANLKL,
         END OF LTY_ANKB.

  TYPES: LTTY_ANKB  TYPE  SORTED TABLE OF LTY_ANKB
                          WITH UNIQUE KEY ANLKL.

  STATICS:
    LT_ANKB TYPE  LTTY_ANKB,
    LS_ANKB TYPE  LTY_ANKB.

  DATA:
    LV_ANLKL  TYPE  ANKB-ANLKL,
    LV_LENGTH TYPE  I.


* Initialize Output
  CLEAR: PV_ANLKL,
         PV_MSGTX.

* Only not initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LENGTH = STRLEN( PV_STRING ).
  IF LV_LENGTH GT 10.
*   Text-e01 : Invalid Asset Class:
    CONCATENATE TEXT-E01 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_STRING
    IMPORTING
      OUTPUT = LV_ANLKL.

* Check Buffer
  IF LS_ANKB-ANLKL NE LV_ANLKL.
*   Validate with Memory
    READ TABLE LT_ANKB INTO LS_ANKB
                       WITH KEY ANLKL = LV_ANLKL
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_ANKB.
*     Validate with Database
      SELECT  ANLKL     ##WARN_OK                    "#EC CI_SEL_NESTED
        INTO LS_ANKB
        FROM ANKB
        UP TO 1 ROWS
       WHERE ANLKL  EQ LV_ANLKL
         AND AFAPL  EQ GC_AFAPL
         AND BDATU  >= SY-DATUM
         AND XSPEB  EQ SPACE
         AND XLOEV  EQ SPACE
           ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e01 : Invalid Asset Class:
        CONCATENATE TEXT-E01 PV_STRING
               INTO PV_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_ANKB INTO TABLE LT_ANKB.
    ENDIF.

  ENDIF.

* Assign Output
  PV_ANLKL = LS_ANKB-ANLKL.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_Inventory_No
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_INVENTORY_NO  USING PV_BUKRS   TYPE  ANLA-BUKRS
                                    PV_STRING  TYPE  STRING
                                    PV_ASSET_EXT_NO  TYPE FLAG
                           CHANGING PV_INVNR   TYPE  ANLA-INVNR
                                    PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_ANLA_INVNR,
           BUKRS TYPE  ANLA-BUKRS,
           INVNR TYPE  ANLA-INVNR,
         END OF LTY_ANLA_INVNR.
  TYPES: LTTY_ANLA_INVNR  TYPE  SORTED TABLE OF LTY_ANLA_INVNR
                          WITH UNIQUE KEY BUKRS INVNR.

  STATICS:
    LT_ANLA_INVNR TYPE  LTTY_ANLA_INVNR,
    LS_ANLA_INVNR TYPE  LTY_ANLA_INVNR.

  DATA:
    LV_INVNR  TYPE  ANLA-INVNR,
    LV_LENGTH TYPE  I.


* Initialize Output
  CLEAR: PV_INVNR,
         PV_MSGTX.

* Only not initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LENGTH = STRLEN( PV_STRING ).
  IF LV_LENGTH GT 25 ##NUMBER_OK.
*   Text-e06 : Invalid Inventory Number
    CONCATENATE TEXT-E06 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_INVNR = PV_STRING.

* Assign Output
  PV_INVNR = LV_INVNR.

  IF PV_ASSET_EXT_NO  EQ SPACE.

* Check Buffer
    IF LS_ANLA_INVNR-INVNR NE LV_INVNR.
*   Validate with Memory
      READ TABLE LT_ANLA_INVNR INTO LS_ANLA_INVNR
                           WITH KEY BUKRS = PV_BUKRS
                                    INVNR = LV_INVNR
                           BINARY SEARCH.
      IF SY-SUBRC NE 0.
        IF RB_NEW = GC_TRUE.
          CLEAR LS_ANLA_INVNR.
*     Validate with Database
          SELECT BUKRS INVNR       ##WARN_OK         "#EC CI_SEL_NESTED
            INTO LS_ANLA_INVNR
            FROM ANLA
            UP TO 1 ROWS
           WHERE BUKRS EQ PV_BUKRS
             AND INVNR EQ LV_INVNR
             ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF SY-SUBRC EQ 0.
*       Text-e07 : Inventory Number <Inventory Number> was created in S4 Hana
            PV_MSGTX = TEXT-E07.
            REPLACE '&' IN PV_MSGTX WITH PV_STRING.
            RETURN.
          ENDIF.
          INSERT LS_ANLA_INVNR INTO TABLE LT_ANLA_INVNR.
        ENDIF.
      ELSE.
*       Text-e07 : Inventory Number <Inventory Number> was created in S4 Hana
        PV_MSGTX = TEXT-E07.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_amount
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AMOUNT  USING PV_STRING  TYPE  STRING
                     CHANGING PV_AMOUNT  TYPE  ANY
                              PV_MSGTX   TYPE  CLIKE.

  DATA: LV_STRING TYPE STRING.

* Initialize Output
  CLEAR: PV_AMOUNT,
         PV_MSGTX.

* Only when value exist
  CHECK PV_STRING IS NOT INITIAL.

  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
    EXPORTING
      INPUT      = PV_STRING
*     INTERNAL   = 'X'
*   IMPORTING
*     OUTPUT     =
    EXCEPTIONS
      NO_NUMERIC = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
    CONCATENATE TEXT-E08 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ELSE.
    LV_STRING = PV_STRING.
    REPLACE ALL OCCURRENCES OF ',' IN LV_STRING WITH ''.
    PV_AMOUNT = LV_STRING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_menge
*&---------------------------------------------------------------------*
*& Validate Quantity
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MENGE  USING    PV_STRING  TYPE  STRING
                                PV_ANLKL   TYPE ANLA-ANLKL
                       CHANGING PV_AMOUNT  TYPE  ANY
                                PV_MSGTX   TYPE  CLIKE.

  DATA: LV_STRING TYPE STRING.

* Initialize Output
  CLEAR: PV_AMOUNT,
         PV_MSGTX.


  IF GR_ANLKL_QTYCHECK[] IS NOT INITIAL AND
     PV_ANLKL IN GR_ANLKL_QTYCHECK.
    IF PV_STRING IS INITIAL.
      CONCATENATE TEXT-E53 PV_STRING
             INTO PV_MSGTX
        SEPARATED BY SPACE.
      RETURN.
    ENDIF.
  ENDIF.


* Only when value exist
  IF PV_STRING IS NOT INITIAL.

    CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
      EXPORTING
        INPUT      = PV_STRING
*       INTERNAL   = 'X'
*   IMPORTING
*       OUTPUT     =
      EXCEPTIONS
        NO_NUMERIC = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
      CONCATENATE TEXT-E08 PV_STRING
             INTO PV_MSGTX
        SEPARATED BY SPACE.
      RETURN.
    ELSE.
      LV_STRING = PV_STRING.
      REPLACE ALL OCCURRENCES OF ',' IN LV_STRING WITH ''.
      PV_AMOUNT = LV_STRING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_amount_scrap
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AMOUNT_SCRAP  USING PV_STRING  TYPE  STRING
                           CHANGING PV_AMOUNT  TYPE  ANY
                                    PV_MSGTX   TYPE  CLIKE.

  DATA: LV_STRING TYPE STRING.

* Initialize Output
  CLEAR: PV_AMOUNT,
         LV_STRING,
         PV_MSGTX.

* Only when value exist
  CHECK PV_STRING IS NOT INITIAL.

  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
    EXPORTING
      INPUT      = PV_STRING
*     INTERNAL   = 'X'
*    IMPORTING
*     output     = lv_amount
    EXCEPTIONS
      NO_NUMERIC = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
*Please checking value scrap amount area <area> format
    PV_MSGTX = TEXT-E43.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ELSE.
    LV_STRING = PV_STRING.
    REPLACE ALL OCCURRENCES OF ',' IN LV_STRING WITH ''.
    PV_AMOUNT = LV_STRING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_amount_useful_life
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AMOUNT_USEFUL_LIFE  USING PV_STRING  TYPE  STRING
                           CHANGING PV_AMOUNT  TYPE  ANY
                                    PV_MSGTX   TYPE  CLIKE.

* Initialize Output
  CLEAR: PV_AMOUNT,
         PV_MSGTX.

* Only when value exist
  CHECK PV_STRING IS NOT INITIAL.

  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
    EXPORTING
      INPUT      = PV_STRING
*     INTERNAL   = 'X'
*   IMPORTING
*     OUTPUT     =
    EXCEPTIONS
      NO_NUMERIC = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
*Please checking value Useful life year area <area>
    PV_MSGTX = TEXT-E44.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ELSE.
    PV_AMOUNT = PV_STRING.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_validate_unit
*----------------------------------------------------------------------*
*       Validate Unit
*----------------------------------------------------------------------*
FORM F_VALIDATE_UNIT  USING  PV_STRING  TYPE  STRING
                             PV_ANLKL   TYPE ANLA-ANLKL
                    CHANGING PV_MEINS   TYPE  MEINS
                             PV_MSGTX   TYPE  CLIKE.

  DATA:
    LV_LENGTH  TYPE  I.


* Initialize Output
  CLEAR: PV_MEINS,
         PV_MSGTX.

  IF GR_ANLKL_QTYCHECK[] IS NOT INITIAL AND
     PV_ANLKL IN GR_ANLKL_QTYCHECK.
    IF PV_STRING IS INITIAL.
      CONCATENATE TEXT-E53 PV_STRING
             INTO PV_MSGTX
        SEPARATED BY SPACE.
      RETURN.
    ENDIF.
  ENDIF.


* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  LV_LENGTH = STRLEN( PV_STRING ).
  IF LV_LENGTH GT 3.
*   Text-e09 : Invalid unit:
    CONCATENATE TEXT-E09 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to input format
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      INPUT          = PV_STRING
      LANGUAGE       = SY-LANGU
    IMPORTING
      OUTPUT         = PV_MEINS
    EXCEPTIONS
      UNIT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
*   Text-e09 : Invalid unit:
    CONCATENATE TEXT-E09 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Validate in table
  SELECT SINGLE MSEHI
    INTO PV_MEINS
    FROM T006
   WHERE MSEHI EQ PV_MEINS.
  IF SY-SUBRC NE 0.
    CLEAR PV_MEINS.
*   Text-e09 : Invalid unit:
    CONCATENATE TEXT-E09 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_date
*----------------------------------------------------------------------*
*       Validate Date
*----------------------------------------------------------------------*
FORM F_VALIDATE_DATE  USING  PV_STRING  TYPE  STRING
                    CHANGING PV_DATUM   TYPE  SY-DATUM
                             PV_MSGTX   TYPE  CLIKE.

* Initialize Output
  CLEAR: PV_DATUM,
         PV_MSGTX.

* Not initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      DATE_EXTERNAL            = PV_STRING
*     ACCEPT_INITIAL_DATE      =
    IMPORTING
      DATE_INTERNAL            = PV_DATUM
    EXCEPTIONS
      DATE_EXTERNAL_IS_INVALID = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
*   Text-e10 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E10 PV_STRING
           INTO PV_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_Cost_center
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_COST_CENTER   USING PV_STRING  TYPE  STRING
                           CHANGING PV_KOSTL   TYPE  CSKS-KOSTL
                                    PV_MSGTX   TYPE  CLIKE.

  TYPES: BEGIN OF LTY_CSKS,
*           kokrs TYPE csks-kokrs,
           KOSTL TYPE CSKS-KOSTL,
         END OF LTY_CSKS.
  TYPES: LTTY_CSKS  TYPE  SORTED TABLE OF LTY_CSKS
                          WITH UNIQUE KEY KOSTL.

  STATICS:
    LT_CSKS TYPE  LTTY_CSKS,
    LS_CSKS TYPE  LTY_CSKS.

  DATA:
    LV_KOSTL  TYPE  CSKS-KOSTL,
*    LV_KOKRS  TYPE  CSKS-KOKRS,
    LV_LENGTH TYPE  I.


* Initialize Output
  CLEAR: PV_KOSTL,
         PV_MSGTX.

* Only not initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LENGTH = STRLEN( PV_STRING ).
  IF LV_LENGTH GT 10.
*   Text-e11 : Cost Center <Cost Center> doesn't exist.
    PV_MSGTX = TEXT-E11.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_STRING
    IMPORTING
      OUTPUT = LV_KOSTL.

* Assign Output
  PV_KOSTL = LV_KOSTL.

* Check Buffer
  IF LS_CSKS-KOSTL NE LV_KOSTL.
*   Validate with Memory
    READ TABLE LT_CSKS INTO LS_CSKS
                       WITH KEY KOSTL = LV_KOSTL
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_CSKS.
*     Validate with Database
      SELECT KOSTL         ##WARN_OK                 "#EC CI_SEL_NESTED
        INTO LS_CSKS
        FROM CSKS                                     "#EC CI_SGLSELECT
        UP TO 1 ROWS
       WHERE KOSTL = LV_KOSTL
         AND DATBI >= SY-DATUM
           ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC NE 0.
*       Text-e11 : Cost Center <Cost Center> doesn't exist.
        PV_MSGTX = TEXT-E11.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_CSKS INTO TABLE LT_CSKS.
    ENDIF.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_plant
*----------------------------------------------------------------------*
*       Validate Plant
*----------------------------------------------------------------------*
FORM F_VALIDATE_PLANT USING  PV_STRING TYPE STRING
                       CHANGING PV_WERKS  TYPE ANLZ-WERKS
                                PV_MSGTX  TYPE CLIKE.

  TYPES: BEGIN OF LTY_T499S,
           WERKS TYPE T499S-WERKS,
*           STAND TYPE T499S-STAND,
         END OF LTY_T499S.
  TYPES: LTTY_T499S  TYPE  SORTED TABLE OF LTY_T499S
                           WITH UNIQUE KEY WERKS.

  STATICS:
    LS_T499S TYPE  LTY_T499S,
    LT_T499S TYPE  LTTY_T499S.

  DATA:
    LV_LEN   TYPE  I,
    LV_WERKS TYPE  LTY_T499S-WERKS.


* Initialize Output
  CLEAR: PV_WERKS,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 10.
*   Text-e13 : Locaion <location> doesn't exist
    PV_MSGTX = TEXT-E13.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_WERKS = PV_STRING.


* Assign Output
  PV_WERKS = PV_STRING.

* Check Buffer
  IF LS_T499S-WERKS NE LV_WERKS.
*   Validate with Memory
    READ TABLE LT_T499S INTO LS_T499S
                        WITH KEY WERKS = LV_WERKS
*                                 STAND = LV_STORT
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_T499S.
*     Validate with Database
      SELECT SINGLE WERKS                                   "#EC WARNOK
        INTO LS_T499S                                       ##WARN_OK
        FROM T499S
       WHERE WERKS = LV_WERKS.
      IF SY-SUBRC NE 0.
*       Text-e56 : Plant <location> doesn't exist
        PV_MSGTX = TEXT-E56.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_T499S INTO TABLE LT_T499S.
    ENDIF.

  ENDIF.


ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_location
*----------------------------------------------------------------------*
*       Validate Location
*----------------------------------------------------------------------*
FORM F_VALIDATE_LOCATION USING  PV_STRING TYPE STRING
                                PV_WERKS  TYPE ANLZ-WERKS
                       CHANGING PV_STORT  TYPE ANLZ-STORT
                                PV_MSGTX  TYPE CLIKE.

  TYPES: BEGIN OF LTY_T499S,
           WERKS TYPE T499S-WERKS,
           STAND TYPE T499S-STAND,
         END OF LTY_T499S.
  TYPES: LTTY_T499S  TYPE  SORTED TABLE OF LTY_T499S
                           WITH UNIQUE KEY WERKS STAND.

  STATICS:
    LS_T499S TYPE  LTY_T499S,
    LT_T499S TYPE  LTTY_T499S.

  DATA:
    LV_LEN   TYPE  I,
    LV_STORT TYPE  LTY_T499S-STAND.


* Initialize Output
  CLEAR: PV_STORT,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 10.
*   Text-e13 : Locaion <location> doesn't exist
    PV_MSGTX = TEXT-E13.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_STORT = PV_STRING.


* Assign Output
  PV_STORT = PV_STRING.

* Check Buffer
  IF LS_T499S-STAND NE LV_STORT.
*   Validate with Memory
    READ TABLE LT_T499S INTO LS_T499S
                        WITH KEY WERKS = SPACE
                                 STAND = LV_STORT
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_T499S.
*     Validate with Database
      SELECT SINGLE WERKS STAND
        INTO LS_T499S
        FROM T499S
       WHERE WERKS = PV_WERKS
         AND STAND = LV_STORT.
      IF SY-SUBRC NE 0.
*       Text-e13 : Locaion <location> doesn't exist
        PV_MSGTX = TEXT-E13.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_T499S INTO TABLE LT_T499S.
    ENDIF.

  ENDIF.


ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_asset_status1
*----------------------------------------------------------------------*
*       Validate Asset Status 1
*----------------------------------------------------------------------*
FORM F_VALIDATE_ASSET_STATUS1  USING PV_STRING  TYPE STRING
                            CHANGING PV_ORD4X   TYPE T087-ORD4X
                                     PV_MSGTX   TYPE CLIKE.

  TYPES: BEGIN OF LTY_T087,
           ORD4X TYPE  T087-ORD4X,
         END OF LTY_T087.
  TYPES: LTTY_T087  TYPE  SORTED TABLE OF LTY_T087
                           WITH UNIQUE KEY ORD4X.

  STATICS:
    LS_T087 TYPE  LTY_T087,
    LT_T087 TYPE  LTTY_T087.

  DATA:
    LV_LEN   TYPE  I,
    LV_ORD4X TYPE  LTY_T087-ORD4X.


* Initialize Output
  CLEAR: PV_ORD4X,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 4.
*   Text-e15 : Asset Type <Asset Typer> doesn't exist.
    PV_MSGTX = TEXT-E15.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_ORD4X = PV_STRING.
* Assign Output
  PV_ORD4X = PV_STRING.

* Check Buffer
  IF LS_T087-ORD4X NE LV_ORD4X.
*   Validate with Memory
    READ TABLE LT_T087 INTO LS_T087
                        WITH KEY ORD4X = LV_ORD4X
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_T087.
*     Validate with Database
      SELECT SINGLE ORD4X
        INTO LS_T087
        FROM T087
       WHERE ORDNR = '1'
         AND ORD4X = LV_ORD4X.
      IF SY-SUBRC NE 0.
*   Text-e15 : Asset Status <Asset Typer> doesn't exist.
        PV_MSGTX = TEXT-E15.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_T087 INTO TABLE LT_T087.
    ENDIF.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_asset_status2
*----------------------------------------------------------------------*
*       Validate Asset Status 2
*----------------------------------------------------------------------*
FORM F_VALIDATE_ASSET_STATUS2  USING PV_STRING  TYPE STRING
                            CHANGING PV_ORD4X   TYPE T087-ORD4X
                                     PV_MSGTX   TYPE CLIKE.

  TYPES: BEGIN OF LTY_T087,
           ORD4X TYPE  T087-ORD4X,
         END OF LTY_T087.
  TYPES: LTTY_T087  TYPE  SORTED TABLE OF LTY_T087
                           WITH UNIQUE KEY ORD4X.

  STATICS:
    LS_T087 TYPE  LTY_T087,
    LT_T087 TYPE  LTTY_T087.

  DATA:
    LV_LEN   TYPE  I,
    LV_ORD4X TYPE  LTY_T087-ORD4X.


* Initialize Output
  CLEAR: PV_ORD4X,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 4.
*   Text-e16 : Tag Barcode <Tag Barcode> doesn't exist
    PV_MSGTX = TEXT-E16.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_ORD4X = PV_STRING.
* Assign Output
  PV_ORD4X = PV_STRING.

* Check Buffer
  IF LS_T087-ORD4X NE LV_ORD4X.
*   Validate with Memory
    READ TABLE LT_T087 INTO LS_T087
                        WITH KEY ORD4X = LV_ORD4X
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_T087.
*     Validate with Database
      SELECT SINGLE ORD4X
        INTO LS_T087
        FROM T087
       WHERE ORDNR = '2'
         AND ORD4X = LV_ORD4X.
      IF SY-SUBRC NE 0.
*   Text-e16 : Tag Barcode <Tag Barcode> doesn't exist
        PV_MSGTX = TEXT-E16.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_T087 INTO TABLE LT_T087.
    ENDIF.

  ENDIF.


ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_asset_status3
*----------------------------------------------------------------------*
*       Validate Asset Status 3
*----------------------------------------------------------------------*
FORM F_VALIDATE_ASSET_STATUS3  USING PV_STRING  TYPE STRING
                            CHANGING PV_ORD4X   TYPE T087-ORD4X
                                     PV_MSGTX   TYPE CLIKE.

  TYPES: BEGIN OF LTY_T087,
           ORD4X TYPE  T087-ORD4X,
         END OF LTY_T087.
  TYPES: LTTY_T087  TYPE  SORTED TABLE OF LTY_T087
                           WITH UNIQUE KEY ORD4X.

  STATICS:
    LS_T087 TYPE  LTY_T087,
    LT_T087 TYPE  LTTY_T087.

  DATA:
    LV_LEN   TYPE  I,
    LV_ORD4X TYPE  LTY_T087-ORD4X.


* Initialize Output
  CLEAR: PV_ORD4X,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 4.
*   Text-e48 : Project Accrued <Project Accrued> doesn't exist
    PV_MSGTX = TEXT-E48.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_ORD4X = PV_STRING.
* Assign Output
  PV_ORD4X = PV_STRING.

* Check Buffer
  IF LS_T087-ORD4X NE LV_ORD4X.
*   Validate with Memory
    READ TABLE LT_T087 INTO LS_T087
                        WITH KEY ORD4X = LV_ORD4X
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_T087.
*     Validate with Database
      SELECT SINGLE ORD4X
        INTO LS_T087
        FROM T087
       WHERE ORDNR = '3'
         AND ORD4X = LV_ORD4X.
      IF SY-SUBRC NE 0.
*   Text-e48 : Project Accrued <Project Accrued> doesn't exist
        PV_MSGTX = TEXT-E48.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_T087 INTO TABLE LT_T087.
    ENDIF.

  ENDIF.


ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_validate_asset_type
*----------------------------------------------------------------------*
*       Validate Asset type
*----------------------------------------------------------------------*
FORM F_VALIDATE_ASSET_TYPE     USING PV_STRING   TYPE STRING
                            CHANGING PV_GDLGRP   TYPE T087G-GDLGRP
                                     PV_MSGTX    TYPE CLIKE.

  TYPES: BEGIN OF LTY_T087G,
           GDLGRP TYPE  T087G-GDLGRP,
         END OF LTY_T087G.
  TYPES: LTTY_T087G  TYPE  SORTED TABLE OF LTY_T087G
                           WITH UNIQUE KEY GDLGRP.

  STATICS:
    LS_T087G TYPE  LTY_T087G,
    LT_T087G TYPE  LTTY_T087G.

  DATA:
    LV_LEN    TYPE  I,
    LV_GDLGRP TYPE  LTY_T087G-GDLGRP.


* Initialize Output
  CLEAR: PV_GDLGRP,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 8.
*   Text-e17 : "Asset Type <Asset Typer> doesn't exist
    PV_MSGTX = TEXT-E17.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_GDLGRP = PV_STRING.
* Assign Output
  PV_GDLGRP = PV_STRING.

* Check Buffer
  IF LS_T087G-GDLGRP NE LV_GDLGRP.
*   Validate with Memory
    READ TABLE LT_T087G INTO LS_T087G
                        WITH KEY GDLGRP = LV_GDLGRP
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_T087G.
*     Validate with Database
      SELECT SINGLE GDLGRP
        INTO LS_T087G
        FROM T087G
       WHERE GDLGRP = LV_GDLGRP.
      IF SY-SUBRC NE 0.
*   Text-e17 : "Asset Type <Asset Typer> doesn't exist
        PV_MSGTX = TEXT-E17.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_T087G INTO TABLE LT_T087G.
    ENDIF.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_super_asset_number
*----------------------------------------------------------------------*
*       Validate super asset number
*----------------------------------------------------------------------*
FORM F_VALIDATE_SUPER_ASSET_NUMBER     USING PV_STRING   TYPE STRING
                                    CHANGING PV_ANLUE   TYPE T087U-ANLUE
                                             PV_MSGTX    TYPE CLIKE.

  TYPES: BEGIN OF LTY_T087U,
           ANLUE TYPE  T087U-ANLUE,
         END OF LTY_T087U.
  TYPES: LTTY_T087U  TYPE  SORTED TABLE OF LTY_T087U
                           WITH UNIQUE KEY ANLUE.

  STATICS:
    LS_T087U TYPE  LTY_T087U,
    LT_T087U TYPE  LTTY_T087U.

  DATA:
    LV_LEN   TYPE  I,
    LV_ANLUE TYPE  LTY_T087U-ANLUE.


* Initialize Output
  CLEAR: PV_ANLUE,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 12 ##NUMBER_OK.
*   Text-e49 : "Asset Super Number <Asset Super Number> doesn't exist
    PV_MSGTX = TEXT-E49.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_ANLUE = PV_STRING.
* Assign Output
  PV_ANLUE = PV_STRING.

* Check Buffer
  IF LS_T087U-ANLUE NE LV_ANLUE.
*   Validate with Memory
    READ TABLE LT_T087U INTO LS_T087U
                        WITH KEY ANLUE = LV_ANLUE
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_T087U.
*     Validate with Database
      SELECT SINGLE ANLUE
        INTO LS_T087U
        FROM T087U
       WHERE ANLUE = LV_ANLUE.
      IF SY-SUBRC NE 0.
*   Text-e49 : "Asset Super Number <Asset Super Number> doesn't exist
        PV_MSGTX = TEXT-E49.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_T087U INTO TABLE LT_T087U.
    ENDIF.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_vendor_code
*----------------------------------------------------------------------*
*       Validate vendor_code
*----------------------------------------------------------------------*
FORM F_VALIDATE_VENDOR_CODE     USING PV_STRING   TYPE STRING
                              CHANGING PV_LIFNR   TYPE LFA1-LIFNR
                                      PV_MSGTX    TYPE CLIKE.

  TYPES: BEGIN OF LTY_LFA1,
           LIFNR TYPE LFA1-LIFNR,
         END OF LTY_LFA1.
  TYPES: LTTY_LFA1  TYPE  SORTED TABLE OF LTY_LFA1
                           WITH UNIQUE KEY LIFNR.

  STATICS:
    LS_LFA1 TYPE  LTY_LFA1,
    LT_LFA1 TYPE  LTTY_LFA1.

  DATA:
    LV_LEN   TYPE  I,
    LV_LIFNR TYPE  LTY_LFA1-LIFNR.


* Initialize Output
  CLEAR: PV_LIFNR,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 10.
*   Text-e50 : "Vendor Number & doesn't exist
    PV_MSGTX = TEXT-E50.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_LIFNR = PV_STRING.
* Assign Output
  PV_LIFNR = PV_STRING.

* Check Buffer
  IF LS_LFA1-LIFNR NE LV_LIFNR.
*   Validate with Memory
    READ TABLE LT_LFA1 INTO LS_LFA1
                        WITH KEY LIFNR = LV_LIFNR
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_LFA1.
*     Validate with Database
      LV_LIFNR = |{ LV_LIFNR ALPHA = IN }|.
      SELECT SINGLE LIFNR
        INTO LS_LFA1
        FROM LFA1
       WHERE LIFNR = LV_LIFNR.
      IF SY-SUBRC NE 0.
*       Text-e50 : "Vendor Number & doesn't exist
        PV_MSGTX = TEXT-E50.
        REPLACE '&' IN PV_MSGTX WITH PV_STRING.
        RETURN.
      ENDIF.
      INSERT LS_LFA1 INTO TABLE LT_LFA1.
    ENDIF.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_validate_depre_key
*----------------------------------------------------------------------*
*       Validate Depreciation Keys
*----------------------------------------------------------------------*
FORM F_VALIDATE_DEPRE_KEY      USING PV_STRING   TYPE STRING
                            CHANGING PV_AFASL    TYPE T090NAT-AFASL
                                     PV_MSGTX    TYPE CLIKE.

  TYPES: BEGIN OF LTY_T090NAT,
           AFAPL TYPE  T090NAT-AFAPL,
           AFASL TYPE  T090NAT-AFASL,
         END OF LTY_T090NAT.
  TYPES: LTTY_T090NAT  TYPE  SORTED TABLE OF LTY_T090NAT
                           WITH UNIQUE KEY AFAPL AFASL.

  STATICS:
    LS_T090NAT TYPE  LTY_T090NAT,
    LT_T090NAT TYPE  LTTY_T090NAT.

  DATA:
    LV_LEN   TYPE  I,
    LV_AFASL TYPE  LTY_T090NAT-AFASL,
    LV_AFAPL TYPE  T093C-AFAPL.


* Initialize Output
  CLEAR: PV_AFASL,
         PV_MSGTX.

* Check Not Initial
  IF PV_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LV_LEN = STRLEN( PV_STRING ).
  IF LV_LEN GT 4.
*   Text-e18 : "Depreciation Keys & doesn't exist
    PV_MSGTX = TEXT-E18.
    REPLACE '&' IN PV_MSGTX WITH PV_STRING.
    RETURN.
  ENDIF.

  LV_AFASL = PV_STRING.

* Assign Output
  PV_AFASL = PV_STRING.


* Check Buffer
  IF LS_T090NAT-AFASL NE LV_AFASL.
    SELECT SINGLE AFAPL INTO LV_AFAPL
      FROM T093C                                         "#EC CI_BYPASS
     WHERE BUKRS = GV_BUKRS.
    IF SY-SUBRC = 0.
*   Validate with Memory
      READ TABLE LT_T090NAT INTO LS_T090NAT
                          WITH KEY AFAPL = LV_AFAPL
                                   AFASL = LV_AFASL
                          BINARY SEARCH.
      IF SY-SUBRC NE 0.

        CLEAR LS_T090NAT.
*     Validate with Database
        SELECT AFASL         ##WARN_OK               "#EC CI_SEL_NESTED
          INTO LS_T090NAT
          FROM T090NAT                                  "#EC CI_GENBUFF
          UP TO 1 ROWS
         WHERE AFAPL = LV_AFAPL
           AND AFASL = LV_AFASL
           ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF SY-SUBRC NE 0.
*   Text-e18 : "Depreciation Keys & doesn't exist
          PV_MSGTX = TEXT-E18.
          REPLACE '&' IN PV_MSGTX WITH PV_STRING.
          RETURN.
        ENDIF.
        INSERT LS_T090NAT INTO TABLE LT_T090NAT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_and_append
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND  USING    PV_ROWNO TYPE  GTY_RESULT-ROWNO
                                     PS_DATA  TYPE  GTY_DATA
                                     PS_MESSG TYPE  GTY_MESSG
                            CHANGING PT_DATA  TYPE  GTTY_DATA.


  DATA:
    LS_DATA  TYPE  GTY_DATA,
    LS_MESSG TYPE  GTY_MESSG.


  CLEAR LS_DATA.
  LS_DATA = PS_DATA.
*---------------------------------
* Validate required fields
*---------------------------------
  PERFORM F_VALIDATE_REQ_FIELDS  CHANGING  LS_DATA
                                           LS_MESSG.

*---------------------------------
  LS_DATA-ROWNO   = PV_ROWNO.


* Collect Message
  IF PS_MESSG IS NOT INITIAL.
    APPEND PS_MESSG TO LS_DATA-MESSG.
  ELSEIF LS_MESSG IS NOT INITIAL.
    APPEND LS_MESSG TO LS_DATA-MESSG.
  ENDIF.

  INSERT LS_DATA INTO TABLE PT_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_req_fields
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_REQ_FIELDS  CHANGING PS_DATA  TYPE GTY_DATA
                                     PS_MESSG TYPE GTY_MESSG.

  DATA:
    LR_STRUCT         TYPE REF TO CL_ABAP_STRUCTDESCR,
    LT_COMP           TYPE ABAP_COMPONENT_TAB,
    LS_COMP           TYPE ABAP_COMPONENTDESCR,
    LV_TABIX          LIKE SY-TABIX,
    LV_ZSFAAC001_I    TYPE ZSDSFIS107,
    LV_ASSET_NO_OLD   TYPE ANLA-ANLN1 ##NEEDED,
    LV_ASSET_SUB_OLD  TYPE ANLA-ANLN2 ##NEEDED,
    LV_ASSET_NO_OLD2  TYPE ANLA-ANLN1 ##NEEDED,
    LV_ASSET_SUB_OLD2 TYPE ANLA-ANLN2 ##NEEDED.

  FIELD-SYMBOLS: <LFS_FIELD>  TYPE ANY.

  DATA:
    LV_ERR_MSG TYPE  C.


* Initialize Output
  CLEAR: PS_MESSG.

* Asset Class Required
  IF PS_DATA-ANLKL IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e02 : Missing Asset Class.
    PS_MESSG-MSGTX = TEXT-E02.
    RETURN.
  ENDIF.
  IF PS_DATA-BUKRS IS INITIAL.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e19 : Missing Company Code.
    PS_MESSG-MSGTX = TEXT-E19.
    RETURN.
  ENDIF.


  IF RB_NEW = 'X' OR RB_CHG = 'X'.

    IF PS_DATA-TXT50 = SPACE.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZTEC'.
      PS_MESSG-MSGNO = '000'.
*   Text-e22 : Asset Description does not have value
      PS_MESSG-MSGTX = TEXT-E22.
      RETURN.
    ENDIF.

    IF PS_DATA-KOSTL IS INITIAL.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZTEC'.
      PS_MESSG-MSGNO = '000'.
*   Text-e25 : Cost Center does not have value
      PS_MESSG-MSGTX = TEXT-E25.
      RETURN.
    ENDIF.

    LR_STRUCT ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( LV_ZSFAAC001_I ).
    LT_COMP = LR_STRUCT->GET_COMPONENTS( ).
*Depre Key

    LOOP AT LT_COMP INTO LS_COMP ##INTO_OK.
      IF LS_COMP-NAME+0(4) = 'AFAS'.
        LV_TABIX = SY-TABIX + 1.
        ASSIGN COMPONENT LV_TABIX OF STRUCTURE PS_DATA TO <LFS_FIELD>.
        IF SY-SUBRC = 0.
          IF <LFS_FIELD> = SPACE.
            PS_MESSG-MSGTY = 'E'.
            PS_MESSG-MSGID = 'ZTEC'.
            PS_MESSG-MSGNO = '000'.
*   Text-e28 : Depre Key & does not have value
            PS_MESSG-MSGTX = TEXT-E28.

            REPLACE '&' IN PS_MESSG-MSGTX WITH LS_COMP-NAME+5(2).
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*Useful life year 01
    CLEAR: LV_ERR_MSG.
    IF PS_DATA-NDJAR01 = SPACE AND PS_DATA-NDPER01 = SPACE.
      PERFORM F_CHECK_DEPRE_KEY_T000 USING PS_DATA-ANLKL '01' CHANGING LV_ERR_MSG.
      IF LV_ERR_MSG = 'X'.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*       Text-e30 : Useful life year or Useful life Period  & does not have value
        PS_MESSG-MSGTX = TEXT-E30.
        REPLACE '&' IN PS_MESSG-MSGTX WITH '01'.
        RETURN.
      ENDIF.
    ENDIF.

*Useful life year 10
    CLEAR: LV_ERR_MSG.
    IF PS_DATA-NDJAR10 = SPACE." AND PS_DATA-NDPER01 = SPACE.
      PERFORM F_CHECK_DEPRE_KEY_T000 USING PS_DATA-ANLKL '10' CHANGING LV_ERR_MSG.
      IF LV_ERR_MSG = 'X'.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*       Text-e30 : Useful life year or Useful life Period  & does not have value
        PS_MESSG-MSGTX = TEXT-E30.
        REPLACE '&' IN PS_MESSG-MSGTX WITH '10'.
        RETURN.
      ENDIF.
    ENDIF.

*Useful life year 11
    CLEAR: LV_ERR_MSG.
    IF PS_DATA-NDJAR11 = SPACE." AND PS_DATA-NDPER12 = SPACE.
      PERFORM F_CHECK_DEPRE_KEY_T000 USING PS_DATA-ANLKL '11' CHANGING LV_ERR_MSG.
      IF LV_ERR_MSG = 'X'.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*       Text-e30 : Useful life year or Useful life Period  & does not have value
        PS_MESSG-MSGTX = TEXT-E30.
        REPLACE '&' IN PS_MESSG-MSGTX WITH '11'.
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.


  "Validate Asset and Sub Asset number
  PERFORM F_VALIDATE_ASSET_SUBASSET CHANGING PS_DATA
                                             PS_MESSG.
  IF PS_MESSG-MSGTY EQ 'E'.
    RETURN.
  ENDIF.


  IF RB_NEW = 'X'.
*    IF PS_DATA-INVNR = SPACE.
    IF PS_DATA-INVZU = SPACE.
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZTEC'.
      PS_MESSG-MSGNO = '000'.
*   Text-e23 : Inventory Note does not have value
      PS_MESSG-MSGTX = TEXT-E23.
      RETURN.
    ENDIF.

    IF GR_ANLKL_QTYCHECK[] IS NOT INITIAL AND
       PS_DATA-ANLKL IN GR_ANLKL_QTYCHECK.

      IF PS_DATA-MENGE = SPACE.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*   Text-e51 : Quantity does not have value
        PS_MESSG-MSGTX = TEXT-E51.
        RETURN.
      ENDIF.

      IF PS_DATA-MEINS = SPACE.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*   Text-e52 : UOM does not have value
        PS_MESSG-MSGTX = TEXT-E52.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  IF RB_NEW = 'X' OR RB_REV = 'X'.
    IF PS_DATA-KANSW <> 0.
*Capitalized on
      IF PS_DATA-AKTIV IS INITIAL.
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
*   Text-e24 : Capitalized On does not have value
        PS_MESSG-MSGTX = TEXT-E24.
      ENDIF.
*ODep. Start Date
      LOOP AT LT_COMP INTO LS_COMP ##INTO_OK.
        IF LS_COMP-NAME+0(4) = 'AFAB'.
          LV_TABIX = SY-TABIX + 1.
          ASSIGN COMPONENT SY-TABIX OF STRUCTURE PS_DATA TO <LFS_FIELD>.
          IF SY-SUBRC = 0.
            IF <LFS_FIELD> = SPACE.
              PS_MESSG-MSGTY = 'E'.
              PS_MESSG-MSGID = 'ZTEC'.
              PS_MESSG-MSGNO = '000'.
*   Text-e29 : ODep. Start Date & does not have value
              PS_MESSG-MSGTX = TEXT-E29.

              REPLACE '&' IN PS_MESSG-MSGTX WITH LS_COMP-NAME+5(2).
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
*Acq.Value (THB) for Area 01, 12
      IF PS_DATA-KANSW = SPACE .
        IF PS_DATA-AKTIV <> SPACE AND PS_DATA-ANLKL <> GC_ANLKL_9999.
          PS_MESSG-MSGTY = 'E'.
          PS_MESSG-MSGID = 'ZTEC'.
          PS_MESSG-MSGNO = '000'.
*       Text-e31 : Acq.Value does not have value
          PS_MESSG-MSGTX = TEXT-E31.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  "Begin of Insertion CH01
  "Validate if change mode. cannot change cost center because it will post new document
  "Instead, show error message and ask user to change via LSMW.
  IF RB_CHG = 'X'.
    PERFORM F_VALIDATE_CHG_KOSTL CHANGING PS_DATA
                                          PS_MESSG.
    IF PS_MESSG-MSGTY EQ 'E'.
      RETURN.
    ENDIF.
  ENDIF.
  "End of Insertion CH01

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_upload_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_UPLOAD_FILE  USING    PT_DATA   TYPE  GTTY_DATA
                    CHANGING PT_RESULT TYPE  GTTY_RESULT
                             PS_SUM    TYPE  GTY_SUM.


* Initialize Output
  CLEAR: PT_RESULT,PS_SUM.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.

  CASE 'X'.
    WHEN RB_NEW. "New upload
      PERFORM F_CREATE_ASSET_NEW USING PT_DATA
                              CHANGING "lt_messg
                                       PT_RESULT
                                       PS_SUM.

    WHEN RB_CHG.
      PERFORM F_ASSET_MASTER_CHANGE USING PT_DATA
                                 CHANGING "lt_messg
                                          PT_RESULT
                                          PS_SUM.
*      ENDIF.
    WHEN RB_REV.
      PERFORM F_ASSET_REPOST USING PT_DATA
                          CHANGING "lt_messg
                                   PT_RESULT
                                   PS_SUM.

  ENDCASE.


ENDFORM.

*----------------------------------------------------------------------*
*  Form f_collect_result
*----------------------------------------------------------------------*
*  Collect Result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT  USING  PS_DATA   TYPE  GTY_DATA
                     CHANGING PT_RESULT TYPE  GTTY_RESULT.

  DATA:
    LS_RESULT  TYPE  GTY_RESULT.

  FIELD-SYMBOLS:
    <LFS_MESSG> TYPE  GTY_MESSG.


  CLEAR: LS_RESULT.

  MOVE-CORRESPONDING PS_DATA TO LS_RESULT  ##ENH_OK.
  LS_RESULT-ROWNO = PS_DATA-ROWNO.

  LOOP AT PS_DATA-MESSG ASSIGNING <LFS_MESSG>.
    LS_RESULT-MSGTY = <LFS_MESSG>-MSGTY.
    LS_RESULT-MSGID = <LFS_MESSG>-MSGID.
    LS_RESULT-MSGNO = <LFS_MESSG>-MSGNO.
    LS_RESULT-MESSAGE = <LFS_MESSG>-MSGTX.
    CASE LS_RESULT-MSGTY.
      WHEN 'S'.
        LS_RESULT-STATU = ICON_LED_GREEN.
      WHEN 'W'.
        LS_RESULT-STATU = ICON_LED_YELLOW.
      WHEN 'E' OR 'A'.
        LS_RESULT-STATU = ICON_LED_RED.
      WHEN OTHERS.
        LS_RESULT-STATU = ICON_LED_INACTIVE.
    ENDCASE.
    EXIT.
  ENDLOOP.

  INSERT LS_RESULT INTO TABLE PT_RESULT.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_check_Depre_key_T000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_CHECK_DEPRE_KEY_T000  USING    PV_ANLKL TYPE ANKB-ANLKL
                                      PV_AFABE TYPE ANKB-AFABE
                             CHANGING PV_ERR_MSG TYPE CHAR1.

  DATA: LV_AFASL TYPE ANKB-AFASL.

  SELECT  AFASL INTO LV_AFASL  ##WARN_OK
    FROM ANKB
    UP TO 1 ROWS
   WHERE ANLKL = PV_ANLKL
     AND AFAPL = '1000'
     AND AFABE = PV_AFABE
     AND BDATU >= SY-DATUM
     AND XAFBE = SPACE
           ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF SY-SUBRC = 0.
    IF LV_AFASL <> 'T000'.
      PV_ERR_MSG = 'X'.
    ENDIF.
  ELSE.
    PV_ERR_MSG = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_asset_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_CREATE_ASSET_NEW  USING PT_DATA   TYPE  GTTY_DATA
                      CHANGING "pt_messg  TYPE  gtty_messg
                               PT_RESULT TYPE  GTTY_RESULT
                               PS_SUM    TYPE  GTY_SUM.

  DATA: LT_DATA_CREATE TYPE  GTTY_DATA_CREATE,
        LS_DATA_CREATE TYPE  GTY_DATA_CREATE,
        LS_DATA        TYPE  GTY_DATA,
        LV_ASSET_MAIN  TYPE  ANLA-ANLN1,
        LV_ERROR       TYPE  FLAG.


  PERFORM F_SORT_DATA_NEW USING PT_DATA
                       CHANGING LT_DATA_CREATE.


  LOOP AT LT_DATA_CREATE INTO LS_DATA_CREATE.
    CLEAR: LV_ERROR.
    IF LS_DATA_CREATE-MESSG[] IS INITIAL.
      PERFORM F_CREATE_ASSET_NEW2 CHANGING LS_DATA_CREATE
                                           LV_ASSET_MAIN   "subnumber must have main asset first
                                           LS_DATA_CREATE-MESSG "pt_messg
                                           LV_ERROR.

      IF LV_ERROR  = 'X'.
        CB_TEST  = 'X'.
      ENDIF.
    ELSE.
      LV_ERROR = GC_TRUE.
      CB_TEST  = 'X'.
    ENDIF.

    CLEAR: LS_DATA.
    MOVE-CORRESPONDING LS_DATA_CREATE TO LS_DATA ##ENH_OK.
    LS_DATA-MESSG = LS_DATA_CREATE-MESSG.


    PS_SUM-TOTAL = PS_SUM-TOTAL + 1.

    IF LV_ERROR EQ GC_TRUE.
      PS_SUM-ERROR = PS_SUM-ERROR + 1.
    ELSE.
      PS_SUM-SUCCS = PS_SUM-SUCCS + 1.
    ENDIF.

*   Collect Result

    PERFORM F_COLLECT_RESULT  USING  LS_DATA
                            CHANGING PT_RESULT.

  ENDLOOP.
  "Processing completed
  MESSAGE TEXT-S09 TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_sort_data_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_SORT_DATA_NEW  USING    PT_DATA   TYPE  GTTY_DATA
                      CHANGING PT_DATA_CREATE TYPE GTTY_DATA_CREATE.

  DATA: LS_DATA_CREATE TYPE  GTY_DATA_CREATE,
        LS_DATA        TYPE  GTY_DATA.


  LOOP AT PT_DATA INTO LS_DATA ##INTO_OK.
    CLEAR: LS_DATA_CREATE.
    MOVE-CORRESPONDING LS_DATA TO LS_DATA_CREATE  ##ENH_OK.

    IF RB_NEW EQ 'X'.
      IF LS_DATA_CREATE-ANLN1 IS NOT INITIAL AND
         LS_DATA_CREATE-ANLN2 IS NOT INITIAL.
        "Check if main asset already exist, then mark sub flag
        SELECT SINGLE BUKRS, ANLN1, ANLN2 FROM ANLA ##WARN_OK ##NEEDED
          INTO @DATA(LS_ANLA_CHECK)
          WHERE BUKRS EQ @LS_DATA_CREATE-BUKRS
            AND ANLN1 EQ @LS_DATA_CREATE-ANLN1.
        IF SY-SUBRC EQ 0.
          LS_DATA_CREATE-SUB_FLG = 'X'.
        ENDIF.

      ENDIF.
    ENDIF.

    APPEND LS_DATA_CREATE TO PT_DATA_CREATE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_asset_master_change
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_ASSET_MASTER_CHANGE  USING PT_DATA   TYPE  GTTY_DATA
                         CHANGING "pt_messg  TYPE  gtty_messg
                                  PT_RESULT TYPE  GTTY_RESULT
                                  PS_SUM    TYPE  GTY_SUM.

  DATA: LS_RETURN               TYPE  BAPIRET2,
        LS_KEY                  TYPE  BAPI1022_KEY,
        LS_GENERALDATA          TYPE  BAPI1022_FEGLG001,
        LS_GENERALDATAX         TYPE  BAPI1022_FEGLG001X,
        LS_INVENTORY            TYPE  BAPI1022_FEGLG011,
        LS_INVENTORYX           TYPE  BAPI1022_FEGLG011X,
        LS_POSTINGINFORMATION   TYPE  BAPI1022_FEGLG002,
        LS_POSTINGINFORMATIONX  TYPE  BAPI1022_FEGLG002X,
        LS_TIMEDEPENDENTDATA    TYPE  BAPI1022_FEGLG003,
        LS_TIMEDEPENDENTDATAX   TYPE  BAPI1022_FEGLG003X,
        LS_ALLOCATIONS          TYPE  BAPI1022_FEGLG004,
        LS_ALLOCATIONSX         TYPE  BAPI1022_FEGLG004X,
        LS_ORIGIN               TYPE  BAPI1022_FEGLG009,
        LS_ORIGINX              TYPE  BAPI1022_FEGLG009X,
        LS_INVESTACCTASSIGNMNT  TYPE  BAPI1022_FEGLG010,
        LS_INVESTACCTASSIGNMNTX TYPE  BAPI1022_FEGLG010X,
        LS_REALESTATE           TYPE  BAPI1022_FEGLG007,
        LS_REALESTATEX          TYPE  BAPI1022_FEGLG007X,
        LT_DEPRECIATIONAREAS    TYPE  TABLE OF BAPI1022_DEP_AREAS,
        LS_DEPRECIATIONAREAS    TYPE  BAPI1022_DEP_AREAS,
        LT_DEPRECIATIONAREASX   TYPE  TABLE OF BAPI1022_DEP_AREASX,
        LS_DEPRECIATIONAREASX   TYPE  BAPI1022_DEP_AREASX,
        LS_DATA                 TYPE  GTY_DATA,
        LV_ERROR                TYPE  FLAG,
        LS_MESSG                TYPE  GTY_MESSG,
        LT_MESSG                TYPE  GTTY_MESSG.

  LOOP AT PT_DATA INTO LS_DATA.
    CLEAR: LS_KEY ,LV_ERROR,LS_ORIGIN,LS_ORIGINX,
           LS_INVESTACCTASSIGNMNT,LS_INVESTACCTASSIGNMNTX,
           LS_REALESTATE,LS_REALESTATEX,
           LS_ALLOCATIONS,LS_ALLOCATIONSX,
           LS_POSTINGINFORMATION,LS_POSTINGINFORMATIONX,LS_INVENTORY,LS_INVENTORYX.

    CLEAR: LT_MESSG[],LT_DEPRECIATIONAREAS[],LT_DEPRECIATIONAREASX[].

    IF LS_DATA-MESSG[] IS INITIAL.

      LS_KEY-COMPANYCODE          = LS_DATA-BUKRS.
      LS_KEY-ASSET                = LS_DATA-ANLN1.
      LS_KEY-SUBNUMBER            = |{ LS_DATA-ANLN2 ALPHA = IN }|.

      LS_GENERALDATA-ASSETCLASS    = LS_DATA-ANLKL.
      LS_GENERALDATA-DESCRIPT      = LS_DATA-TXT50.
      LS_GENERALDATA-DESCRIPT2     = LS_DATA-TXA50.
      LS_GENERALDATA-MAIN_DESCRIPT = LS_DATA-ANLHTXT.
      LS_GENERALDATA-SERIAL_NO     = LS_DATA-SERNR.
      LS_GENERALDATA-INVENT_NO     = LS_DATA-INVNR.
      LS_GENERALDATA-QUANTITY      = LS_DATA-MENGE.
      LS_GENERALDATA-BASE_UOM      = LS_DATA-MEINS.

      LS_GENERALDATA-HISTORY       = LS_DATA-XHIST.

*      ls_generaldatax-assetclass  = 'X'.
      LS_GENERALDATAX-DESCRIPT    = 'X'.
      LS_GENERALDATAX-DESCRIPT2   = 'X'.
      LS_GENERALDATAX-MAIN_DESCRIPT = 'X'.
      LS_GENERALDATAX-SERIAL_NO   = 'X'.
      LS_GENERALDATAX-INVENT_NO    = 'X'.
      LS_GENERALDATAX-QUANTITY    = 'X'.
      LS_GENERALDATAX-BASE_UOM    = 'X'.

      LS_GENERALDATAX-HISTORY     = 'X'.
*---------------------------------------------------
      LS_INVENTORY-DATE  = LS_DATA-IVDAT.
      LS_INVENTORY-NOTE  = LS_DATA-INVZU.
      LS_INVENTORYX-DATE = 'X'.
      LS_INVENTORYX-NOTE = 'X'.
*---------------------------------------------------

      LS_POSTINGINFORMATION-CAP_DATE    = LS_DATA-AKTIV.
      LS_POSTINGINFORMATIONX-CAP_DATE   = 'X'.

*---------------------------------------------------

      LS_TIMEDEPENDENTDATA-COSTCENTER    = LS_DATA-KOSTL.

      IF LS_DATA-KOSTL IS NOT INITIAL.
        "Get default profit center from CSKS
        SELECT SINGLE PRCTR FROM CSKS INTO LS_TIMEDEPENDENTDATA-PROFIT_CTR ##WARN_OK "#EC CI_SGLSELECT "#EC WARNOK
          WHERE KOSTL EQ LS_DATA-KOSTL                      "#EC WARNOK
            AND DATBI EQ '99991231'.
        LS_TIMEDEPENDENTDATAX-PROFIT_CTR    = 'X'.
      ENDIF.

      LS_TIMEDEPENDENTDATA-PLANT         = LS_DATA-WERKS.
      LS_TIMEDEPENDENTDATA-LOCATION      = LS_DATA-STORT.
      LS_TIMEDEPENDENTDATA-PLATE_NO      = LS_DATA-KFZKZ.
      LS_TIMEDEPENDENTDATA-ROOM          = LS_DATA-RAUMN.
      LS_TIMEDEPENDENTDATA-PERSON_NO     = LS_DATA-PERNR.

      LS_TIMEDEPENDENTDATAX-COSTCENTER    = 'X'.
      LS_TIMEDEPENDENTDATAX-PLANT         = 'X'.
      LS_TIMEDEPENDENTDATAX-LOCATION      = 'X'.
      LS_TIMEDEPENDENTDATAX-LICENSE_PLATE_NO  = 'X'.
      LS_TIMEDEPENDENTDATAX-ROOM          = 'X'.
      LS_TIMEDEPENDENTDATAX-PERSON_NO     = 'X'.

*---------------------------------------------------

      LS_ALLOCATIONS-EVALGROUP1         = LS_DATA-ORD41.
      LS_ALLOCATIONS-EVALGROUP2         = LS_DATA-ORD42.

      LS_ALLOCATIONSX-EVALGROUP1        = 'X'.
      LS_ALLOCATIONSX-EVALGROUP2        = 'X'.

*---------------------------------------------------

      LS_ORIGIN-VENDOR_NO               = LS_DATA-VENDOR_NO.
      LS_ORIGIN-VENDOR                  = LS_DATA-VENDOR.
      LS_ORIGIN-MANUFACTURER            = LS_DATA-HERST.
      LS_ORIGIN-TYPE_NAME               = LS_DATA-TYPBZ.
      LS_ORIGIN-ORIG_ASSET              = LS_DATA-AIBN1.
      LS_ORIGINX-VENDOR_NO              = 'X'.
      LS_ORIGINX-VENDOR                 = 'X'.
      LS_ORIGINX-MANUFACTURER           = 'X'.
      LS_ORIGINX-TYPE_NAME              = 'X'.
      LS_ORIGINX-ORIG_ASSET             = 'X'.         "

*---------------------------------------------------------------
      LS_INVESTACCTASSIGNMNT-WBS_ELEMENT   = LS_DATA-WBS_ELEMENT.
      LS_INVESTACCTASSIGNMNTX-WBS_ELEMENT  = 'X'.

*---------------------------------------------------------------

      LS_REALESTATE-TAX_OFFICE          = LS_DATA-TAX_OFFICE.
      LS_REALESTATE-TAX_NO              = LS_DATA-TAX_NO.
      LS_REALESTATE-MUNICIPALITY        = LS_DATA-MUNICIPALITY.
      LS_REALESTATE-LNDREG_PLOT_NO      = LS_DATA-PLOT_NO.
      LS_REALESTATE-AREA                = LS_DATA-AREA.
      LS_REALESTATE-AREA_UOM            = LS_DATA-AREA_UOM.
      LS_REALESTATEX-TAX_OFFICE          = 'X'.
      LS_REALESTATEX-TAX_NO              = 'X'.
      LS_REALESTATEX-MUNICIPALITY        = 'X'.
      LS_REALESTATEX-LNDREG_PLOT_NO      = 'X'.
      LS_REALESTATEX-AREA                = 'X'.
      LS_REALESTATEX-AREA_UOM            = 'X'.

*---------------------------------------------------------------
      CLEAR: LS_DEPRECIATIONAREAS.
      LS_DEPRECIATIONAREAS-AREA             = '01'.
      LS_DEPRECIATIONAREAS-DEP_KEY          = LS_DATA-AFASL01.
      LS_DEPRECIATIONAREAS-ODEP_START_DATE  = LS_DATA-AFABG01.
      LS_DEPRECIATIONAREAS-ULIFE_YRS        = LS_DATA-NDJAR01.
      LS_DEPRECIATIONAREAS-ULIFE_PRDS       = LS_DATA-NDPER01.
      LS_DEPRECIATIONAREAS-SCRAPVALUE       = LS_DATA-SCHRW01.
      LS_DEPRECIATIONAREAS-SCRAPVALUE_PRCTG = LS_DATA-SCHRW_PROZ01.
      APPEND LS_DEPRECIATIONAREAS TO LT_DEPRECIATIONAREAS.


      CLEAR: LS_DEPRECIATIONAREAS.
      LS_DEPRECIATIONAREAS-AREA             = '10'.
      LS_DEPRECIATIONAREAS-DEP_KEY          = LS_DATA-AFASL10.
      LS_DEPRECIATIONAREAS-ODEP_START_DATE  = LS_DATA-AFABG10.
      LS_DEPRECIATIONAREAS-ULIFE_YRS        = LS_DATA-NDJAR10.
      LS_DEPRECIATIONAREAS-ULIFE_PRDS       = LS_DATA-NDPER10.
      LS_DEPRECIATIONAREAS-SCRAPVALUE       = LS_DATA-SCHRW10.
      APPEND LS_DEPRECIATIONAREAS TO LT_DEPRECIATIONAREAS.


      CLEAR: LS_DEPRECIATIONAREAS.
      LS_DEPRECIATIONAREAS-AREA             = '11'.
      LS_DEPRECIATIONAREAS-DEP_KEY          = LS_DATA-AFASL11.
      LS_DEPRECIATIONAREAS-ODEP_START_DATE  = LS_DATA-AFABG11.
      LS_DEPRECIATIONAREAS-ULIFE_YRS        = LS_DATA-NDJAR11.
      LS_DEPRECIATIONAREAS-SCRAPVALUE       = LS_DATA-SCHRW11.
      APPEND LS_DEPRECIATIONAREAS TO LT_DEPRECIATIONAREAS.

*---------------------------------------------------------------
      CLEAR: LS_DEPRECIATIONAREASX.
      LS_DEPRECIATIONAREASX-AREA             = '01'.
      LS_DEPRECIATIONAREASX-DEP_KEY          = 'X'.
      LS_DEPRECIATIONAREASX-ODEP_START_DATE  = 'X'.
      LS_DEPRECIATIONAREASX-ULIFE_YRS        = 'X'.
      LS_DEPRECIATIONAREASX-ULIFE_PRDS       = 'X'.
      LS_DEPRECIATIONAREASX-SCRAPVALUE       = 'X'.
      LS_DEPRECIATIONAREASX-SCRAPVALUE_PRCTG = 'X'.
      APPEND LS_DEPRECIATIONAREASX TO LT_DEPRECIATIONAREASX.

      LS_DEPRECIATIONAREASX-AREA  = '10'.
      APPEND LS_DEPRECIATIONAREASX TO LT_DEPRECIATIONAREASX.

      LS_DEPRECIATIONAREASX-AREA  = '11'.
      APPEND LS_DEPRECIATIONAREASX TO LT_DEPRECIATIONAREASX.
*---------------------------------------------------------------


      CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
        EXPORTING
          COMPANYCODE          = LS_KEY-COMPANYCODE
          ASSET                = LS_KEY-ASSET
          SUBNUMBER            = LS_KEY-SUBNUMBER
*         GROUPASSET           =
          GENERALDATA          = LS_GENERALDATA
          GENERALDATAX         = LS_GENERALDATAX
          INVENTORY            = LS_INVENTORY
          INVENTORYX           = LS_INVENTORYX
          POSTINGINFORMATION   = LS_POSTINGINFORMATION
          POSTINGINFORMATIONX  = LS_POSTINGINFORMATIONX
          TIMEDEPENDENTDATA    = LS_TIMEDEPENDENTDATA
          TIMEDEPENDENTDATAX   = LS_TIMEDEPENDENTDATAX
          ALLOCATIONS          = LS_ALLOCATIONS
          ALLOCATIONSX         = LS_ALLOCATIONSX
          ORIGIN               = LS_ORIGIN
          ORIGINX              = LS_ORIGINX
          INVESTACCTASSIGNMNT  = LS_INVESTACCTASSIGNMNT
          INVESTACCTASSIGNMNTX = LS_INVESTACCTASSIGNMNTX
          REALESTATE           = LS_REALESTATE
          REALESTATEX          = LS_REALESTATEX
        IMPORTING
          RETURN               = LS_RETURN
        TABLES
          DEPRECIATIONAREAS    = LT_DEPRECIATIONAREAS
          DEPRECIATIONAREASX   = LT_DEPRECIATIONAREASX.


      IF LS_RETURN-TYPE = 'E' OR LS_RETURN-TYPE = 'A' .
        LS_MESSG-MSGTY = LS_RETURN-TYPE.
        LS_MESSG-MSGID = LS_RETURN-ID.
        LS_MESSG-MSGNO = LS_RETURN-NUMBER.
        LS_MESSG-MSGTX = LS_RETURN-MESSAGE.
        INSERT LS_MESSG INTO TABLE LT_MESSG.
        LV_ERROR = 'X'.
      ENDIF.

      IF CB_TEST EQ GC_TRUE.
        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = 'S'.
        LS_MESSG-MSGID = 'ZTEC'.
        LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
        LS_MESSG-MSGTX = TEXT-I01.
        INSERT LS_MESSG INTO TABLE LT_MESSG.
      ELSE.
        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = 'S'.
        LS_MESSG-MSGID = 'ZTEC'.
        LS_MESSG-MSGNO = '000'.
*     Text-i04: Asset master changed successfully.
        LS_MESSG-MSGTX = TEXT-I04.
        INSERT LS_MESSG INTO TABLE LT_MESSG.

      ENDIF.

      IF CB_TEST = SPACE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*         IMPORTING
*           RETURN        =
          .
      ENDIF.

      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LV_ERROR = GC_TRUE.
    ENDIF.


    PS_SUM-TOTAL = PS_SUM-TOTAL + 1.

    IF LV_ERROR EQ GC_TRUE.
      PS_SUM-ERROR = PS_SUM-ERROR + 1.
    ELSE.
      PS_SUM-SUCCS = PS_SUM-SUCCS + 1.
    ENDIF.

*   Collect Result
    PERFORM F_COLLECT_RESULT  USING  LS_DATA
                            CHANGING PT_RESULT.
  ENDLOOP.

  MESSAGE TEXT-S09 TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_asset_repost
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_ASSET_REPOST  USING PT_DATA   TYPE  GTTY_DATA
                  CHANGING PT_RESULT TYPE  GTTY_RESULT
                           PS_SUM    TYPE  GTY_SUM.

  DATA: LT_RETURN         TYPE  TABLE OF BAPIRET2,
        LS_RETURN         TYPE  BAPIRET2,
        LS_KEY            TYPE  BAPI1022_KEY,
        LS_CUMVAL         TYPE  BAPI1022_CUMVAL,
        LT_CUMVAL         TYPE  TABLE OF BAPI1022_CUMVAL,
        LS_POSTVAL        TYPE  BAPI1022_POSTVAL,
        LT_POSTVAL        TYPE  TABLE OF BAPI1022_POSTVAL,
        LT_POSTINGHEADERS TYPE  TABLE OF BAPI1022_POSTINGHEADER,
        LS_POSTINGHEADERS TYPE  BAPI1022_POSTINGHEADER,
        LT_TRANSACTIONS   TYPE  TABLE OF BAPI1022_TRTYPE,
        LS_TRANSACTIONS   TYPE  BAPI1022_TRTYPE,
        LV_TRANSFER_DATE  TYPE  T093C-DATUM,
        LV_CUM_POST       TYPE  CHAR1,
*        LV_POSTED_POST    TYPE  CHAR1,
        LV_TRAN_POST      TYPE  CHAR1,
        LS_DATA           TYPE  GTY_DATA,
        LS_MESSG          TYPE  GTY_MESSG,
        LV_ERROR          TYPE  FLAG,
        LT_MESSG          TYPE  GTTY_MESSG.


  TYPES: BEGIN OF LTY_ANEK,
           BUKRS TYPE ANEK-BUKRS,
           ANLN1 TYPE ANEK-ANLN1,
           ANLN2 TYPE ANEK-ANLN2,
           GJAHR TYPE ANEK-GJAHR,
           BELNR TYPE ANEK-BELNR,
           LNRAN TYPE ANEK-LNRAN,
           AWORG TYPE ANEK-AWORG,
           AWTYP TYPE ANEK-AWTYP,
           AWKEY TYPE BKPF-AWKEY,
         END OF LTY_ANEK.

  DATA: LS_ORIGINDOCUMENTKEY TYPE  BAPI6037_DOC_KEY,
        LS_REVERSALDATA      TYPE  BAPI6037_REV_DATA.
  DATA: LT_ANEK    TYPE TABLE OF LTY_ANEK,
        LS_ANEK    TYPE LTY_ANEK,
        LV_FSYEAR  TYPE  ANLC-GJAHR,
*        LV_AUC_FLG TYPE FLAG,
        LV_REV_FLG TYPE FLAG,
        LV_TABIX   LIKE SY-TABIX.

  FIELD-SYMBOLS: <LFS_ANEK>   TYPE LTY_ANEK,
                 <LFS_RETURN> TYPE BAPIRET2.

  CHECK PT_DATA[] IS NOT INITIAL.

  SELECT BUKRS ANLN1 ANLN2 GJAHR BELNR LNRAN  AWORG AWTYP  ##TOO_MANY_ITAB_FIELDS "#EC CI_SEL_DEL
    FROM ANEK
     INTO TABLE LT_ANEK
     FOR ALL ENTRIES IN PT_DATA
   WHERE BUKRS = PT_DATA-BUKRS
     AND ANLN1 = PT_DATA-ANLN1
     AND ANLN2 = PT_DATA-ANLN2
     AND LDGRP = '0L'.
  IF SY-SUBRC = 0.
    SELECT BUKRS,ANLN1,ANLN2,GJAHR,LNRAN,AFABE,ZUJHR,ZUCOD INTO TABLE @DATA(LT_ANEP) "#EC CI_NO_TRANSFORM
      FROM ANEP
       FOR ALL ENTRIES IN @LT_ANEK
     WHERE BUKRS = @LT_ANEK-BUKRS
       AND ANLN1 = @LT_ANEK-ANLN1
       AND ANLN2 = @LT_ANEK-ANLN2
       AND GJAHR = @LT_ANEK-GJAHR
       AND LNRAN = @LT_ANEK-LNRAN
       AND AUGLN <> @SPACE
      ORDER BY PRIMARY KEY.
    IF SY-SUBRC = 0.
      LOOP AT LT_ANEK ASSIGNING <LFS_ANEK>.
        LV_TABIX = SY-TABIX.
        READ TABLE LT_ANEP WITH KEY BUKRS = <LFS_ANEK>-BUKRS
                                    ANLN1 = <LFS_ANEK>-ANLN1
                                    ANLN2 = <LFS_ANEK>-ANLN2
                                    GJAHR = <LFS_ANEK>-GJAHR
                                    LNRAN = <LFS_ANEK>-LNRAN
                                    BINARY SEARCH
                                    TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          DELETE LT_ANEK INDEX LV_TABIX.
        ENDIF.
      ENDLOOP.
    ENDIF.
    SORT LT_ANEK BY BUKRS ANLN1 ANLN2 GJAHR DESCENDING BELNR DESCENDING.

    DELETE ADJACENT DUPLICATES FROM LT_ANEK COMPARING BUKRS ANLN1 ANLN2. "get fi doc only 1 line for each asset

    IF LT_ANEK[] IS NOT INITIAL.
      LOOP AT LT_ANEK ASSIGNING <LFS_ANEK>.
        CONCATENATE <LFS_ANEK>-BELNR <LFS_ANEK>-AWORG INTO <LFS_ANEK>-AWKEY.
      ENDLOOP.

      SELECT AWKEY,AWTYP,BUKRS,BELNR,GJAHR
        INTO TABLE @DATA(LT_BKPF)
        FROM BKPF
         FOR ALL ENTRIES IN @LT_ANEK
       WHERE AWKEY = @LT_ANEK-AWKEY
         AND AWTYP = @LT_ANEK-AWTYP
         AND LDGRP = '0L'
         AND XREVERSING = @SPACE
         AND XREVERSED  = @SPACE.
      IF SY-SUBRC = 0.
        SORT LT_BKPF BY AWKEY AWTYP.
      ENDIF.
    ENDIF.
  ENDIF.


  LOOP AT PT_DATA INTO LS_DATA.
    CLEAR: LS_KEY,LS_CUMVAL,LS_TRANSACTIONS ,LV_ERROR,
           LS_ORIGINDOCUMENTKEY,LS_REVERSALDATA,LV_REV_FLG.

    CLEAR: LT_MESSG[],LT_RETURN[],LT_TRANSACTIONS[],
           LT_CUMVAL[],LT_POSTVAL[],LT_POSTINGHEADERS[].

    IF LS_DATA-MESSG[] IS INITIAL.
*Fiscal year
      CALL FUNCTION 'AISCO_CALCULATE_FIRST_DAY'
        EXPORTING
          I_BUKRS                        = LS_DATA-BUKRS
        IMPORTING
          E_TO_YEAR                      = LV_FSYEAR
        EXCEPTIONS
          COMPANY_CODE_NOT_FOUND         = 1
          CANNOT_DETERMINE_TAKEOVER_YEAR = 2
          CANNOT_DETERMINE_FIRST_DAY     = 3
          CANNOT_DETERMINE_LAST_DAY      = 4
          MANY_OPEN_FYS                  = 5
          OTHERS                         = 6.
      IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
      ENDIF.

      "Get Transfer date
      SELECT SINGLE DATUM FROM T093C INTO LV_TRANSFER_DATE "#EC CI_BYPASS
        WHERE BUKRS EQ LS_DATA-BUKRS.
      IF SY-SUBRC NE 0.
        CLEAR: LV_TRANSFER_DATE.
      ENDIF.

      "Check Transfer date
      CLEAR: LV_CUM_POST, LV_TRAN_POST.
      IF LV_TRANSFER_DATE+4(4) = '1231'. " In pattern 31.12.XXXX
        "ABLDT only
        LV_CUM_POST = 'X'.
      ELSE.
        "Check capitalize date is the same year as takeover date
        IF LS_DATA-AKTIV+0(4) EQ LV_TRANSFER_DATE+0(4).
          "Same year
          "AB01 Only
          LV_TRAN_POST = 'X'.
        ELSE.
          "Different year
          "ABLDT only
          LV_CUM_POST = 'X'.
        ENDIF.
      ENDIF.


*---------------------------------------------------------------
      "For Acquisition value to current year (T-code: ABLDT)
      IF LV_CUM_POST EQ 'X'.

        CLEAR: LS_CUMVAL.
        LS_CUMVAL-FISC_YEAR    = LV_FSYEAR.
        LS_CUMVAL-AREA        = '01'.
        LS_CUMVAL-ACQ_VALUE    = LS_DATA-KANSW.
        LS_CUMVAL-ORD_DEP     = LS_DATA-ORD_DEP_PREV01 * GC_CREDIT.
        LS_CUMVAL-CURRENCY    = 'THB'.
        APPEND LS_CUMVAL TO LT_CUMVAL.

        CLEAR: LS_CUMVAL.
        LS_CUMVAL-FISC_YEAR    = LV_FSYEAR.
        LS_CUMVAL-AREA        = '10'.
        LS_CUMVAL-ACQ_VALUE    = LS_DATA-KANSW.
        LS_CUMVAL-ORD_DEP     = LS_DATA-ORD_DEP_PREV10 * GC_CREDIT.
        LS_CUMVAL-CURRENCY    = 'THB'.
        APPEND LS_CUMVAL TO LT_CUMVAL.

        CLEAR: LS_CUMVAL.
        LS_CUMVAL-FISC_YEAR    = LV_FSYEAR.
        LS_CUMVAL-AREA        = '11'.
        LS_CUMVAL-ACQ_VALUE    = LS_DATA-KANSW.
        LS_CUMVAL-ORD_DEP     = LS_DATA-ORD_DEP_PREV11 * GC_CREDIT.
        LS_CUMVAL-CURRENCY    = 'THB'.
        APPEND LS_CUMVAL TO LT_CUMVAL.

        IF LS_DATA-ORD_DEP01 IS NOT INITIAL.
          CLEAR: LS_POSTVAL.
          LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
          LS_POSTVAL-AREA        = '01'.
          LS_POSTVAL-ORD_DEP     = LS_DATA-ORD_DEP01 * GC_CREDIT.
          LS_POSTVAL-CURRENCY    = 'THB'.
          APPEND LS_POSTVAL TO LT_POSTVAL.

        ENDIF.

        IF LS_DATA-ORD_DEP10 IS NOT INITIAL.
          CLEAR: LS_POSTVAL.
          LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
          LS_POSTVAL-AREA        = '10'.
          LS_POSTVAL-ORD_DEP     = LS_DATA-ORD_DEP10 * GC_CREDIT.
          LS_POSTVAL-CURRENCY    = 'THB'.
          APPEND LS_POSTVAL TO LT_POSTVAL.
        ENDIF.

        IF LS_DATA-ORD_DEP11 IS NOT INITIAL.
          CLEAR: LS_POSTVAL.
          LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
          LS_POSTVAL-AREA        = '11'.
          LS_POSTVAL-ORD_DEP     = LS_DATA-ORD_DEP11 * GC_CREDIT.
          LS_POSTVAL-CURRENCY    = 'THB'.
          APPEND LS_POSTVAL TO LT_POSTVAL.
        ENDIF.
      ENDIF.

*---------------------------------------------------------------
      "For Transacion in current year (T-code: AB01 )
      IF LV_TRAN_POST EQ 'X'.
        IF LS_DATA-KANSW IS NOT INITIAL.
          CLEAR: LS_TRANSACTIONS.
          LS_TRANSACTIONS-FISC_YEAR     = LV_FSYEAR.
          LS_TRANSACTIONS-CURRENT_NO    = '00001'.
          LS_TRANSACTIONS-AREA          = '01'.
          LS_TRANSACTIONS-VALUEDATE     = LS_DATA-AKTIV.
          LS_TRANSACTIONS-AMOUNT        = LS_DATA-KANSW.
          LS_TRANSACTIONS-CURRENCY      = 'THB'.
          APPEND LS_TRANSACTIONS TO LT_TRANSACTIONS.

          CLEAR: LS_TRANSACTIONS.
          LS_TRANSACTIONS-FISC_YEAR     = LV_FSYEAR.
          LS_TRANSACTIONS-CURRENT_NO    = '00001'.
          LS_TRANSACTIONS-AREA          = '10'.
          LS_TRANSACTIONS-VALUEDATE     = LS_DATA-AKTIV.
          LS_TRANSACTIONS-AMOUNT        = LS_DATA-KANSW.
          LS_TRANSACTIONS-CURRENCY      = 'THB'.
          APPEND LS_TRANSACTIONS TO LT_TRANSACTIONS.

          CLEAR: LS_TRANSACTIONS.
          LS_TRANSACTIONS-FISC_YEAR     = LV_FSYEAR.
          LS_TRANSACTIONS-CURRENT_NO    = '00001'.
          LS_TRANSACTIONS-AREA          = '11'.
          LS_TRANSACTIONS-VALUEDATE     = LS_DATA-AKTIV.
          LS_TRANSACTIONS-AMOUNT        = LS_DATA-KANSW.
          LS_TRANSACTIONS-CURRENCY      = 'THB'.
          APPEND LS_TRANSACTIONS TO LT_TRANSACTIONS.


          CLEAR: LS_POSTINGHEADERS.
          LS_POSTINGHEADERS-FISC_YEAR  = LV_TRANSFER_DATE+0(4).
          LS_POSTINGHEADERS-CURRENT_NO = '00001'.
          LS_POSTINGHEADERS-PSTNG_DATE = LV_TRANSFER_DATE.
          APPEND LS_POSTINGHEADERS TO LT_POSTINGHEADERS.

          IF LS_DATA-ORD_DEP01 IS NOT INITIAL.
            CLEAR: LS_POSTVAL.
            LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
            LS_POSTVAL-AREA        = '01'.
            LS_POSTVAL-ORD_DEP     = LS_DATA-ORD_DEP01 * GC_CREDIT.
            LS_POSTVAL-CURRENCY    = 'THB'.
            APPEND LS_POSTVAL TO LT_POSTVAL.

          ENDIF.
        ENDIF.
      ENDIF.


      READ TABLE LT_ANEK INTO LS_ANEK WITH KEY BUKRS = LS_DATA-BUKRS
                                               ANLN1 = LS_DATA-ANLN1
                                               ANLN2 = LS_DATA-ANLN2
                                               BINARY SEARCH.
      IF SY-SUBRC = 0.
        READ TABLE LT_BKPF INTO DATA(LS_BKPF) WITH KEY AWKEY = LS_ANEK-AWKEY
                                                       AWTYP = LS_ANEK-AWTYP
                                                       BINARY SEARCH.
        IF SY-SUBRC = 0.
          CLEAR: LS_ORIGINDOCUMENTKEY,LS_REVERSALDATA,LS_RETURN,LT_RETURN[].

          LV_REV_FLG = 'X'.

          LS_ORIGINDOCUMENTKEY-COMP_CODE = LS_BKPF-BUKRS.
          LS_ORIGINDOCUMENTKEY-AC_DOC_NO = LS_BKPF-BELNR.
          LS_ORIGINDOCUMENTKEY-FISC_YEAR = LS_BKPF-GJAHR.

          LS_REVERSALDATA-REASON_REV = '01'.

          IF CB_TEST = SPACE.
            CALL FUNCTION 'BAPI_ASSET_REVERSAL_POST'
              EXPORTING
*               ORIGINDOCREFERENCE       =
                ORIGINDOCUMENTKEY = LS_ORIGINDOCUMENTKEY
                REVERSALDATA      = LS_REVERSALDATA
              IMPORTING
*               DOCUMENTREFERENCE =
                RETURN            = LS_RETURN
              TABLES
                RETURN_ALL        = LT_RETURN.
            IF CB_TEST = SPACE.
              READ TABLE LT_RETURN ASSIGNING <LFS_RETURN> WITH KEY TYPE = 'E'.
              IF SY-SUBRC = 0.
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*            IMPORTING
*              RETURN        =
                  .
              ELSE.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    WAIT = 'X'.
              ENDIF.
            ENDIF.
          ELSE.
            CALL FUNCTION 'BAPI_ASSET_REVERSAL_CHECK'
              EXPORTING
*               ORIGINDOCREFERENCE       =
                ORIGINDOCUMENTKEY = LS_ORIGINDOCUMENTKEY
                REVERSALDATA      = LS_REVERSALDATA
              IMPORTING
                RETURN            = LS_RETURN
              TABLES
                RETURN_ALL        = LT_RETURN.
          ENDIF.
        ENDIF.
      ENDIF.

      LS_KEY-COMPANYCODE = LS_DATA-BUKRS.
      LS_KEY-ASSET       = LS_DATA-ANLN1.
      LS_KEY-SUBNUMBER   = |{ LS_DATA-ANLN2 ALPHA = IN }|.

      IF CB_TEST = 'X' AND LV_REV_FLG = 'X'.
        "do not post
      ELSE.
        CALL FUNCTION 'BAPI_FIXEDASSET_OVRTAKE_POST'
          EXPORTING
            KEY             = LS_KEY
            TESTRUN         = CB_TEST
          TABLES
            CUMULATEDVALUES = LT_CUMVAL
            POSTEDVALUES    = LT_POSTVAL
            POSTINGHEADERS  = LT_POSTINGHEADERS
            TRANSACTIONS    = LT_TRANSACTIONS
            RETURN          = LT_RETURN.
      ENDIF.

      IF CB_TEST = SPACE.
        READ TABLE LT_RETURN ASSIGNING <LFS_RETURN> WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*            IMPORTING
*              RETURN        =
            .
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
        ENDIF.
      ENDIF.

      LOOP AT LT_RETURN ASSIGNING <LFS_RETURN>
                        WHERE TYPE = 'E' OR
                              TYPE = 'A'.
        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = <LFS_RETURN>-TYPE.
        LS_MESSG-MSGID = <LFS_RETURN>-ID.
        LS_MESSG-MSGNO = <LFS_RETURN>-NUMBER.
        LS_MESSG-MSGTX = <LFS_RETURN>-MESSAGE.
        INSERT LS_MESSG INTO TABLE LT_MESSG.
        LV_ERROR = 'X'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*            IMPORTING
*              RETURN        =
          .
        EXIT.
      ENDLOOP.

      IF SY-SUBRC NE 0.

        IF CB_TEST EQ GC_TRUE.
          CLEAR LS_MESSG.
          LS_MESSG-MSGTY = 'S'.
          LS_MESSG-MSGID = 'ZTEC'.
          LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
          LS_MESSG-MSGTX = TEXT-I01.
          INSERT LS_MESSG INTO TABLE LT_MESSG.
        ELSE.
          CLEAR LS_MESSG.
          LS_MESSG-MSGTY = 'S'.
          LS_MESSG-MSGID = 'ZTEC'.
          LS_MESSG-MSGNO = '000'.
*     Text-i02: Asset posted successfully.
          LS_MESSG-MSGTX = TEXT-I02.
          INSERT LS_MESSG INTO TABLE LT_MESSG.

        ENDIF.
      ENDIF.

*Find FI Doc
      LOOP AT  LT_RETURN INTO LS_RETURN WHERE ID = 'FAA_POST' AND NUMBER = '092' AND MESSAGE_V3 = '0L'.
        PERFORM F_GET_FI_DOCUMENT USING  LS_KEY-COMPANYCODE LS_KEY-ASSET LS_KEY-SUBNUMBER LS_RETURN
                                CHANGING LS_DATA-GJAHR
                                         LS_DATA-BELNR.
      ENDLOOP.


      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LV_ERROR = GC_TRUE.
    ENDIF.

    PS_SUM-TOTAL = PS_SUM-TOTAL + 1.

    IF LV_ERROR EQ GC_TRUE.
      PS_SUM-ERROR = PS_SUM-ERROR + 1.
    ELSE.
      PS_SUM-SUCCS = PS_SUM-SUCCS + 1.
    ENDIF.

*   Collect Result
    PERFORM F_COLLECT_RESULT  USING  LS_DATA
                            CHANGING PT_RESULT.
  ENDLOOP.

  MESSAGE TEXT-S09 TYPE 'S'.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_asset_new2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_CREATE_ASSET_NEW2  CHANGING PS_DATA_CREATE  TYPE  GTY_DATA_CREATE
                                   PV_ASSET_MAIN   TYPE  ANLA-ANLN1
                                   PT_MESSG        TYPE  GTTY_MESSG
                                   PV_ERROR        TYPE  FLAG.

  DATA: LT_RETURN                  TYPE  TABLE OF BAPIRET2,
        LS_RETURN                  TYPE  BAPIRET2,
        LS_KEY                     TYPE  BAPI1022_KEY,
        LS_GENERALDATA             TYPE  BAPI1022_FEGLG001,
        LS_GENERALDATAX            TYPE  BAPI1022_FEGLG001X,
        LS_INVENTORY               TYPE  BAPI1022_FEGLG011,
        LS_INVENTORYX              TYPE  BAPI1022_FEGLG011X,
        LS_POSTINGINFORMATION      TYPE  BAPI1022_FEGLG002,
        LS_POSTINGINFORMATIONX     TYPE  BAPI1022_FEGLG002X,
        LS_TIMEDEPENDENTDATA       TYPE  BAPI1022_FEGLG003,
        LS_TIMEDEPENDENTDATAX      TYPE  BAPI1022_FEGLG003X,
        LS_ALLOCATIONS             TYPE  BAPI1022_FEGLG004,
        LS_ALLOCATIONSX            TYPE  BAPI1022_FEGLG004X,
        LS_ORIGIN                  TYPE  BAPI1022_FEGLG009,
        LS_ORIGINX                 TYPE  BAPI1022_FEGLG009X,
        LS_INVESTACCTASSIGNMNT     TYPE  BAPI1022_FEGLG010,
        LS_INVESTACCTASSIGNMNTX    TYPE BAPI1022_FEGLG010X,
        LS_REALESTATE              TYPE  BAPI1022_FEGLG007,
        LS_REALESTATEX             TYPE  BAPI1022_FEGLG007X,
        LT_DEPRECIATIONAREAS       TYPE  TABLE OF BAPI1022_DEP_AREAS,
        LS_DEPRECIATIONAREAS       TYPE  BAPI1022_DEP_AREAS,
        LT_DEPRECIATIONAREASX      TYPE  TABLE OF BAPI1022_DEP_AREASX,
        LS_DEPRECIATIONAREASX      TYPE  BAPI1022_DEP_AREASX,
        LS_CUMVAL                  TYPE  BAPI1022_CUMVAL,
        LT_CUMVAL                  TYPE  TABLE OF BAPI1022_CUMVAL,
        LS_POSTVAL                 TYPE  BAPI1022_POSTVAL,
        LT_POSTVAL                 TYPE  TABLE OF BAPI1022_POSTVAL,
        LT_POSTINGHEADERS          TYPE  TABLE OF BAPI1022_POSTINGHEADER,
        LS_POSTINGHEADERS          TYPE  BAPI1022_POSTINGHEADER,
        LT_TRANSACTIONS            TYPE  TABLE OF BAPI1022_TRTYPE,
        LS_TRANSACTIONS            TYPE  BAPI1022_TRTYPE,
        LV_FSYEAR                  TYPE  ANLC-GJAHR,
        LV_TRANSFER_DATE           TYPE  T093C-DATUM,
        LV_YEAREND_DATE            TYPE DATUM,
        LV_YEAREND_DATE_LAST_YEAR  TYPE DATUM ##NEEDED,
        LV_CUM_POST                TYPE  CHAR1,
        LV_TRAN_POST               TYPE  CHAR1,
        LV_COMPANYCODE             TYPE  BAPI1022_1-COMP_CODE,
        LV_ASSET                   TYPE  BAPI1022_1-ASSETMAINO,
        LV_SUBNUMBER               TYPE  BAPI1022_1-ASSETSUBNO,
        LS_MESSG                   TYPE  GTY_MESSG.

  LS_KEY-COMPANYCODE          = PS_DATA_CREATE-BUKRS.
  IF PS_DATA_CREATE-SUB_FLG = 'X'.
    IF PS_DATA_CREATE-ANLN1 IS NOT INITIAL.
      LS_KEY-ASSET             = PS_DATA_CREATE-ANLN1.
    ELSE.
      LS_KEY-ASSET             = PV_ASSET_MAIN.
    ENDIF.
  ENDIF.
*  IF ps_data_create-asset_ext_no EQ 'X'.
  IF PS_DATA_CREATE-ANLN1 IS NOT INITIAL AND
     PS_DATA_CREATE-ANLN2 IS NOT INITIAL.
    LS_KEY-ASSET               = PS_DATA_CREATE-ANLN1.
    LS_KEY-SUBNUMBER           = |{ PS_DATA_CREATE-ANLN2 ALPHA = IN }|.
  ENDIF.

  LS_GENERALDATA-ASSETCLASS    = PS_DATA_CREATE-ANLKL.
  LS_GENERALDATA-DESCRIPT      = PS_DATA_CREATE-TXT50.
  LS_GENERALDATA-DESCRIPT2     = PS_DATA_CREATE-TXA50.
  LS_GENERALDATA-MAIN_DESCRIPT = PS_DATA_CREATE-ANLHTXT.
  LS_GENERALDATA-SERIAL_NO     = PS_DATA_CREATE-SERNR.
  LS_GENERALDATA-INVENT_NO     = PS_DATA_CREATE-INVNR.
  LS_GENERALDATA-QUANTITY      = PS_DATA_CREATE-MENGE.
  LS_GENERALDATA-BASE_UOM      = PS_DATA_CREATE-MEINS.

  LS_GENERALDATA-HISTORY       = PS_DATA_CREATE-XHIST.

  LS_GENERALDATAX-ASSETCLASS  = 'X'.
  LS_GENERALDATAX-DESCRIPT    = 'X'.
  LS_GENERALDATAX-DESCRIPT2   = 'X'.
  IF LS_GENERALDATA-MAIN_DESCRIPT <> SPACE.
    LS_GENERALDATAX-MAIN_DESCRIPT = 'X'.
  ENDIF.
  LS_GENERALDATAX-SERIAL_NO   = 'X'.
  LS_GENERALDATAX-INVENT_NO   = 'X'.
  LS_GENERALDATAX-QUANTITY    = 'X'.
  LS_GENERALDATAX-BASE_UOM    = 'X'.

  LS_GENERALDATAX-HISTORY     = 'X'.
*---------------------------------------------------
  LS_INVENTORY-DATE  = PS_DATA_CREATE-IVDAT.
  LS_INVENTORY-NOTE  = PS_DATA_CREATE-INVZU.

  LS_INVENTORYX-DATE = 'X'.
  LS_INVENTORYX-NOTE = 'X'.
*---------------------------------------------------

  LS_POSTINGINFORMATION-CAP_DATE    = PS_DATA_CREATE-AKTIV.

  LS_POSTINGINFORMATIONX-CAP_DATE   = 'X'.

*---------------------------------------------------

  LS_TIMEDEPENDENTDATA-COSTCENTER    = PS_DATA_CREATE-KOSTL.
  LS_TIMEDEPENDENTDATA-PLANT         = PS_DATA_CREATE-WERKS.
  LS_TIMEDEPENDENTDATA-LOCATION      = PS_DATA_CREATE-STORT.
  LS_TIMEDEPENDENTDATA-PLATE_NO      = PS_DATA_CREATE-KFZKZ.
  LS_TIMEDEPENDENTDATA-ROOM          = PS_DATA_CREATE-RAUMN.
  LS_TIMEDEPENDENTDATA-PERSON_NO     = PS_DATA_CREATE-PERNR.


  LS_TIMEDEPENDENTDATAX-COSTCENTER    = 'X'.
  LS_TIMEDEPENDENTDATAX-PLANT         = 'X'.
  LS_TIMEDEPENDENTDATAX-LOCATION      = 'X'.
  LS_TIMEDEPENDENTDATAX-LICENSE_PLATE_NO  = 'X'.
  LS_TIMEDEPENDENTDATAX-ROOM          = 'X'.
  LS_TIMEDEPENDENTDATAX-PERSON_NO     = 'X'.
  LS_TIMEDEPENDENTDATAX-FUNDS_CTR_APC = 'X'.
  LS_TIMEDEPENDENTDATAX-PERSON_NO     = 'X'.
*---------------------------------------------------

  LS_ALLOCATIONS-EVALGROUP1         = PS_DATA_CREATE-ORD41.
  LS_ALLOCATIONS-EVALGROUP2         = PS_DATA_CREATE-ORD42.

  LS_ALLOCATIONSX-EVALGROUP1        = 'X'.
  LS_ALLOCATIONSX-EVALGROUP2        = 'X'.

*---------------------------------------------------

  LS_ORIGIN-VENDOR_NO               = PS_DATA_CREATE-VENDOR_NO.
  LS_ORIGIN-VENDOR                  = PS_DATA_CREATE-VENDOR.
  LS_ORIGIN-MANUFACTURER            = PS_DATA_CREATE-HERST.
  LS_ORIGIN-TYPE_NAME               = PS_DATA_CREATE-TYPBZ.
  LS_ORIGIN-ORIG_ASSET              = PS_DATA_CREATE-AIBN1.

  LS_ORIGINX-VENDOR_NO              = 'X'.
  LS_ORIGINX-VENDOR                 = 'X'.
  LS_ORIGINX-MANUFACTURER           = 'X'.
  LS_ORIGINX-TYPE_NAME              = 'X'.
  LS_ORIGINX-ORIG_ASSET             = 'X'.

*---------------------------------------------------------------

  LS_INVESTACCTASSIGNMNT-WBS_ELEMENT   = PS_DATA_CREATE-WBS_ELEMENT.
  LS_INVESTACCTASSIGNMNTX-WBS_ELEMENT  = 'X'.

*---------------------------------------------------------------

  LS_REALESTATE-TAX_OFFICE          = PS_DATA_CREATE-TAX_OFFICE.
  LS_REALESTATE-TAX_NO              = PS_DATA_CREATE-TAX_NO.
  LS_REALESTATE-MUNICIPALITY        = PS_DATA_CREATE-MUNICIPALITY.
  LS_REALESTATE-LNDREG_PLOT_NO      = PS_DATA_CREATE-PLOT_NO.
  LS_REALESTATE-AREA                = PS_DATA_CREATE-AREA.
  LS_REALESTATE-AREA_UOM            = PS_DATA_CREATE-AREA_UOM.
  LS_REALESTATEX-TAX_OFFICE          = 'X'.
  LS_REALESTATEX-TAX_NO              = 'X'.
  LS_REALESTATEX-MUNICIPALITY        = 'X'.
  LS_REALESTATEX-LNDREG_PLOT_NO      = 'X'.
  LS_REALESTATEX-AREA                = 'X'.
  LS_REALESTATEX-AREA_UOM            = 'X'.

*---------------------------------------------------------------

  CLEAR: LS_DEPRECIATIONAREAS.
  LS_DEPRECIATIONAREAS-AREA             = '01'.
  LS_DEPRECIATIONAREAS-DEP_KEY          = PS_DATA_CREATE-AFASL01.
  LS_DEPRECIATIONAREAS-ODEP_START_DATE  = PS_DATA_CREATE-AFABG01.
  LS_DEPRECIATIONAREAS-ULIFE_YRS        = PS_DATA_CREATE-NDJAR01.
  LS_DEPRECIATIONAREAS-ULIFE_PRDS       = PS_DATA_CREATE-NDPER01.
  LS_DEPRECIATIONAREAS-SCRAPVALUE       = PS_DATA_CREATE-SCHRW01.
  LS_DEPRECIATIONAREAS-SCRAPVALUE_PRCTG = PS_DATA_CREATE-SCHRW_PROZ01.
  APPEND LS_DEPRECIATIONAREAS TO LT_DEPRECIATIONAREAS.

  CLEAR: LS_DEPRECIATIONAREAS.
  LS_DEPRECIATIONAREAS-AREA             = '10'.
  LS_DEPRECIATIONAREAS-DEP_KEY          = PS_DATA_CREATE-AFASL10.
  LS_DEPRECIATIONAREAS-ODEP_START_DATE  = PS_DATA_CREATE-AFABG10.
  LS_DEPRECIATIONAREAS-ULIFE_YRS        = PS_DATA_CREATE-NDJAR10.
  LS_DEPRECIATIONAREAS-ULIFE_PRDS       = PS_DATA_CREATE-NDPER10.
  LS_DEPRECIATIONAREAS-SCRAPVALUE       = PS_DATA_CREATE-SCHRW10.
  APPEND LS_DEPRECIATIONAREAS TO LT_DEPRECIATIONAREAS.

  CLEAR: LS_DEPRECIATIONAREAS.
  LS_DEPRECIATIONAREAS-AREA             = '11'.
  LS_DEPRECIATIONAREAS-DEP_KEY          = PS_DATA_CREATE-AFASL11.
  LS_DEPRECIATIONAREAS-ODEP_START_DATE  = PS_DATA_CREATE-AFABG11.
  LS_DEPRECIATIONAREAS-ULIFE_YRS        = PS_DATA_CREATE-NDJAR11.
  LS_DEPRECIATIONAREAS-SCRAPVALUE       = PS_DATA_CREATE-SCHRW11.
  APPEND LS_DEPRECIATIONAREAS TO LT_DEPRECIATIONAREAS.

*---------------------------------------------------------------
  CLEAR: LS_DEPRECIATIONAREASX.
  LS_DEPRECIATIONAREASX-AREA             = '01'.
  LS_DEPRECIATIONAREASX-DEP_KEY          = 'X'.
  LS_DEPRECIATIONAREASX-ODEP_START_DATE  = 'X'.
  LS_DEPRECIATIONAREASX-ULIFE_YRS        = 'X'.
  LS_DEPRECIATIONAREASX-ULIFE_PRDS       = 'X'.
  LS_DEPRECIATIONAREASX-SCRAPVALUE       = 'X'.
*  LS_DEPRECIATIONAREASX-SCRAPVALUE_PRCTG = 'X'.
  APPEND LS_DEPRECIATIONAREASX TO LT_DEPRECIATIONAREASX.

  LS_DEPRECIATIONAREASX-AREA  = '10'.
  APPEND LS_DEPRECIATIONAREASX TO LT_DEPRECIATIONAREASX.

  LS_DEPRECIATIONAREASX-AREA  = '11'.
  APPEND LS_DEPRECIATIONAREASX TO LT_DEPRECIATIONAREASX.

*---------------------------------------------------------------
  IF  PS_DATA_CREATE-KANSW IS NOT INITIAL  .

    CLEAR:LT_POSTINGHEADERS.

*Fiscal year
    CALL FUNCTION 'AISCO_CALCULATE_FIRST_DAY'
      EXPORTING
        I_BUKRS                        = PS_DATA_CREATE-BUKRS
      IMPORTING
        E_TO_YEAR                      = LV_FSYEAR
      EXCEPTIONS
        COMPANY_CODE_NOT_FOUND         = 1
        CANNOT_DETERMINE_TAKEOVER_YEAR = 2
        CANNOT_DETERMINE_FIRST_DAY     = 3
        CANNOT_DETERMINE_LAST_DAY      = 4
        MANY_OPEN_FYS                  = 5
        OTHERS                         = 6.
    IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
    ENDIF.

*---------------------------------------------------------------


*    "Get Transfer date
    SELECT SINGLE DATUM FROM T093C INTO LV_TRANSFER_DATE "#EC CI_BYPASS
      WHERE BUKRS EQ PS_DATA_CREATE-BUKRS.
    IF SY-SUBRC NE 0.
      CLEAR: LV_TRANSFER_DATE.
*    ELSE.
*      LV_TRANSFER_DATE_LAST_YEAR = |{ LV_TRANSFER_DATE+0(4) - 1 }{ LV_TRANSFER_DATE+4(4) }|.
    ENDIF.

    "Get Year End Date
    CALL FUNCTION 'BAPI_CCODE_GET_FIRSTDAY_PERIOD'
      EXPORTING
        COMPANYCODEID             = PS_DATA_CREATE-BUKRS
        FISCAL_PERIOD             = '01'
        FISCAL_YEAR               = sy-datum+0(4)
     IMPORTING
       FIRST_DAY_OF_PERIOD       = LV_YEAREND_DATE
*       RETURN                    =
              .
    LV_YEAREND_DATE = LV_YEAREND_DATE - 1.
    LV_YEAREND_DATE_LAST_YEAR = |{ LV_YEAREND_DATE+0(4) - 1 }{ LV_YEAREND_DATE+4(4) }|.


    "Check year end date
    CLEAR: LV_CUM_POST, LV_TRAN_POST.

*      "Check capitalize date is the same fiscal year as takeover date
*
*      IF PS_DATA_CREATE-AKTIV  BETWEEN LV_YEAREND_DATE_LAST_YEAR AND LV_YEAREND_DATE.
*        "prev year
**        LV_TRAN_POST = 'X'.
*        LV_CUM_POST = 'X'.
*      ELSE.
*        "current fiscal year
**        LV_CUM_POST = 'X'.
*        LV_TRAN_POST = 'X'.
*      ENDIF.
*Fiscal year
      CALL FUNCTION 'AISCO_CALCULATE_FIRST_DAY'
        EXPORTING
          I_BUKRS                        = PS_DATA_CREATE-BUKRS
        IMPORTING
          E_TO_YEAR                      = LV_FSYEAR
        EXCEPTIONS
          COMPANY_CODE_NOT_FOUND         = 1
          CANNOT_DETERMINE_TAKEOVER_YEAR = 2
          CANNOT_DETERMINE_FIRST_DAY     = 3
          CANNOT_DETERMINE_LAST_DAY      = 4
          MANY_OPEN_FYS                  = 5
          OTHERS                         = 6.
      IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
      ENDIF.

      "Get Transfer date
      SELECT SINGLE DATUM FROM T093C INTO LV_TRANSFER_DATE "#EC CI_BYPASS
        WHERE BUKRS EQ PS_DATA_CREATE-BUKRS.
      IF SY-SUBRC NE 0.
        CLEAR: LV_TRANSFER_DATE.
      ENDIF.

      "Check Transfer date
      CLEAR: LV_CUM_POST, LV_TRAN_POST.
      IF LV_TRANSFER_DATE+4(4) = '1231'. " In pattern 31.12.XXXX
        "ABLDT only
        LV_CUM_POST = 'X'.
      ELSE.
        "Check capitalize date is the same year as takeover date
*        IF PS_DATA_CREATE-AKTIV+0(4) EQ LV_TRANSFER_DATE+0(4).
        if PS_DATA_CREATE-AKTIV > LV_YEAREND_DATE. "31.03.2024
          "Same year
          "AB01 Only
          LV_TRAN_POST = 'X'.
        ELSE.
          "Different year
          "ABLDT only
          LV_CUM_POST = 'X'.
        ENDIF.
      ENDIF.
*---------------------------------------------------------------
    "For Acquisition value to current year (T-code: ABLDT)
    IF LV_CUM_POST EQ 'X'.
      CLEAR: LS_CUMVAL.
      LS_CUMVAL-FISC_YEAR    = LV_FSYEAR.
      LS_CUMVAL-AREA        = '01'.
      LS_CUMVAL-ACQ_VALUE    = PS_DATA_CREATE-KANSW.
      LS_CUMVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP_PREV01 * GC_CREDIT.
      LS_CUMVAL-CURRENCY    = 'THB'.
      APPEND LS_CUMVAL TO LT_CUMVAL.

      CLEAR: LS_CUMVAL.
      LS_CUMVAL-FISC_YEAR    = LV_FSYEAR.
      LS_CUMVAL-AREA        = '10'.
      LS_CUMVAL-ACQ_VALUE    = PS_DATA_CREATE-KANSW.
      LS_CUMVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP_PREV10 * GC_CREDIT.
      LS_CUMVAL-CURRENCY    = 'THB'.
      APPEND LS_CUMVAL TO LT_CUMVAL.

      CLEAR: LS_CUMVAL.
      LS_CUMVAL-FISC_YEAR    = LV_FSYEAR.
      LS_CUMVAL-AREA        = '11'.
      LS_CUMVAL-ACQ_VALUE    = PS_DATA_CREATE-KANSW.
      LS_CUMVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP_PREV11 * GC_CREDIT.
      LS_CUMVAL-CURRENCY    = 'THB'.
      APPEND LS_CUMVAL TO LT_CUMVAL.

      IF PS_DATA_CREATE-ORD_DEP01 IS NOT INITIAL.
        CLEAR: LS_POSTVAL.
        LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
        LS_POSTVAL-AREA        = '01'.
        LS_POSTVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP01 * GC_CREDIT.
        LS_POSTVAL-CURRENCY    = 'THB'.
        APPEND LS_POSTVAL TO LT_POSTVAL.
      ENDIF.

      IF PS_DATA_CREATE-ORD_DEP10 IS NOT INITIAL.
        CLEAR: LS_POSTVAL.
        LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
        LS_POSTVAL-AREA        = '10'.
        LS_POSTVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP10 * GC_CREDIT.
        LS_POSTVAL-CURRENCY    = 'THB'.
        APPEND LS_POSTVAL TO LT_POSTVAL.
      ENDIF.

      IF PS_DATA_CREATE-ORD_DEP11 IS NOT INITIAL.
        CLEAR: LS_POSTVAL.
        LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
        LS_POSTVAL-AREA        = '11'.
        LS_POSTVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP11 * GC_CREDIT.
        LS_POSTVAL-CURRENCY    = 'THB'.
        APPEND LS_POSTVAL TO LT_POSTVAL.
      ENDIF.
    ENDIF.
*---------------------------------------------------------------
    "For Transacion in current year (T-code: AB01 )
    IF LV_TRAN_POST EQ 'X'.
      IF PS_DATA_CREATE-KANSW IS NOT INITIAL.
        CLEAR: LS_TRANSACTIONS.
        LS_TRANSACTIONS-FISC_YEAR     = LV_FSYEAR.
        LS_TRANSACTIONS-CURRENT_NO    = '00001'.
        LS_TRANSACTIONS-AREA          = '01'.
        LS_TRANSACTIONS-ASSETTRTYP    = '100'.
        LS_TRANSACTIONS-VALUEDATE     =  LV_TRANSFER_DATE. "PS_DATA_CREATE-AKTIV.
        LS_TRANSACTIONS-AMOUNT        = PS_DATA_CREATE-KANSW.
        LS_TRANSACTIONS-CURRENCY      = 'THB'.
        APPEND LS_TRANSACTIONS TO LT_TRANSACTIONS.

        CLEAR: LS_TRANSACTIONS.
        LS_TRANSACTIONS-FISC_YEAR     = LV_FSYEAR.
        LS_TRANSACTIONS-CURRENT_NO    = '00001'.
        LS_TRANSACTIONS-AREA          = '10'.
        LS_TRANSACTIONS-ASSETTRTYP    = '100'.
        LS_TRANSACTIONS-VALUEDATE     = LV_TRANSFER_DATE. "PS_DATA_CREATE-AKTIV.
        LS_TRANSACTIONS-AMOUNT        = PS_DATA_CREATE-KANSW.
        LS_TRANSACTIONS-CURRENCY      = 'THB'.
        APPEND LS_TRANSACTIONS TO LT_TRANSACTIONS.

        CLEAR: LS_TRANSACTIONS.
        LS_TRANSACTIONS-FISC_YEAR     = LV_FSYEAR.
        LS_TRANSACTIONS-CURRENT_NO    = '00001'.
        LS_TRANSACTIONS-AREA          = '11'.
        LS_TRANSACTIONS-ASSETTRTYP    = '100'.
        LS_TRANSACTIONS-VALUEDATE     = LV_TRANSFER_DATE. "PS_DATA_CREATE-AKTIV.
        LS_TRANSACTIONS-AMOUNT        = PS_DATA_CREATE-KANSW.
        LS_TRANSACTIONS-CURRENCY      = 'THB'.
        APPEND LS_TRANSACTIONS TO LT_TRANSACTIONS.


        CLEAR: LS_POSTINGHEADERS.
        LS_POSTINGHEADERS-FISC_YEAR  = LV_TRANSFER_DATE+0(4).
        LS_POSTINGHEADERS-CURRENT_NO = '00001'.
        LS_POSTINGHEADERS-PSTNG_DATE = LV_TRANSFER_DATE.
        APPEND LS_POSTINGHEADERS TO LT_POSTINGHEADERS.


      ENDIF.

      IF PS_DATA_CREATE-ORD_DEP01 IS NOT INITIAL.
        CLEAR: LS_POSTVAL.
        LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
        LS_POSTVAL-AREA        = '01'.
        LS_POSTVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP01 * GC_CREDIT.
        LS_POSTVAL-CURRENCY    = 'THB'.
        APPEND LS_POSTVAL TO LT_POSTVAL.
      ENDIF.
      IF PS_DATA_CREATE-ORD_DEP10 IS NOT INITIAL.
        CLEAR: LS_POSTVAL.
        LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
        LS_POSTVAL-AREA        = '10'.
        LS_POSTVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP10 * GC_CREDIT.
        LS_POSTVAL-CURRENCY    = 'THB'.
        APPEND LS_POSTVAL TO LT_POSTVAL.
      ENDIF.
      IF PS_DATA_CREATE-ORD_DEP11 IS NOT INITIAL.
        CLEAR: LS_POSTVAL.
        LS_POSTVAL-FISC_YEAR    = LV_FSYEAR.
        LS_POSTVAL-AREA        = '11'.
        LS_POSTVAL-ORD_DEP     = PS_DATA_CREATE-ORD_DEP11 * GC_CREDIT.
        LS_POSTVAL-CURRENCY    = 'THB'.
        APPEND LS_POSTVAL TO LT_POSTVAL.
      ENDIF.
    ENDIF.

    CLEAR: LS_RETURN.

    CALL FUNCTION 'BAPI_FIXEDASSET_OVRTAKE_CREATE'
      EXPORTING
        KEY                  = LS_KEY
*       REFERENCE            =
        CREATESUBNUMBER      = PS_DATA_CREATE-SUB_FLG
*       CREATEGROUPASSET     =
        TESTRUN              = CB_TEST
        GENERALDATA          = LS_GENERALDATA
        GENERALDATAX         = LS_GENERALDATAX
        INVENTORY            = LS_INVENTORY
        INVENTORYX           = LS_INVENTORYX
        POSTINGINFORMATION   = LS_POSTINGINFORMATION
        POSTINGINFORMATIONX  = LS_POSTINGINFORMATIONX
        TIMEDEPENDENTDATA    = LS_TIMEDEPENDENTDATA
        TIMEDEPENDENTDATAX   = LS_TIMEDEPENDENTDATAX
        ALLOCATIONS          = LS_ALLOCATIONS
        ALLOCATIONSX         = LS_ALLOCATIONSX
        ORIGIN               = LS_ORIGIN
        ORIGINX              = LS_ORIGINX
        INVESTACCTASSIGNMNT  = LS_INVESTACCTASSIGNMNT
        INVESTACCTASSIGNMNTX = LS_INVESTACCTASSIGNMNTX
        REALESTATE           = LS_REALESTATE
        REALESTATEX          = LS_REALESTATEX
      IMPORTING
        COMPANYCODE          = LV_COMPANYCODE
        ASSET                = LV_ASSET
        SUBNUMBER            = LV_SUBNUMBER
      TABLES
        DEPRECIATIONAREAS    = LT_DEPRECIATIONAREAS
        DEPRECIATIONAREASX   = LT_DEPRECIATIONAREASX
        CUMULATEDVALUES      = LT_CUMVAL
        POSTEDVALUES         = LT_POSTVAL
        TRANSACTIONS         = LT_TRANSACTIONS
        RETURN               = LT_RETURN
        POSTINGHEADERS       = LT_POSTINGHEADERS.

    CLEAR: PV_ASSET_MAIN.
    PV_ASSET_MAIN = LV_ASSET.
    IF CB_TEST = GC_TRUE.
      "if found 'Asset main number not entered' -> Change to warning
      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'E'
                                                   ID   = 'BAPI1022'
                                                   NUMBER = '002'.
      IF SY-SUBRC = 0.
        LS_RETURN-TYPE = 'W'.
        MODIFY LT_RETURN FROM LS_RETURN INDEX SY-TABIX.
      ENDIF.

      "if found 'Main asset number & does not exist' -> Change to warning
      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'E'
                                                   ID   = 'AA'
                                                 NUMBER = '178'.
      IF SY-SUBRC = 0.
        LS_RETURN-TYPE = 'W'.
        MODIFY LT_RETURN FROM LS_RETURN INDEX SY-TABIX.
      ENDIF.
    ENDIF.
    LOOP AT LT_RETURN INTO LS_RETURN ##INTO_OK
                      WHERE TYPE = 'E' OR
                            TYPE = 'A'.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = LS_RETURN-TYPE.
      LS_MESSG-MSGID = LS_RETURN-ID.
      LS_MESSG-MSGNO = LS_RETURN-NUMBER.
      LS_MESSG-MSGTX = LS_RETURN-MESSAGE.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
      PV_ERROR = 'X'.
      EXIT.
    ENDLOOP.

    IF SY-SUBRC NE 0.

      IF CB_TEST EQ GC_TRUE.
        IF LS_RETURN-TYPE = 'W'.
          CLEAR LS_MESSG.
          LS_MESSG-MSGTY = 'W'.
          LS_MESSG-MSGID = LS_RETURN-ID.
          LS_MESSG-MSGNO = LS_RETURN-NUMBER.
          LS_MESSG-MSGTX = LS_RETURN-MESSAGE.
          INSERT LS_MESSG INTO TABLE PT_MESSG.
        ELSE.
          CLEAR LS_MESSG.
          LS_MESSG-MSGTY = 'S'.
          LS_MESSG-MSGID = 'ZTEC'.
          LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
          LS_MESSG-MSGTX = TEXT-I01.
          INSERT LS_MESSG INTO TABLE PT_MESSG.
        ENDIF.
      ELSE.
        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = 'S'.
        LS_MESSG-MSGID = 'ZTEC'.
        LS_MESSG-MSGNO = '000'.
*     Text-i02: Asset posted successfully.
        LS_MESSG-MSGTX = TEXT-I02.
        INSERT LS_MESSG INTO TABLE PT_MESSG.

      ENDIF.
    ENDIF.

  ELSE.
*Create Asset Master Only
    CALL FUNCTION 'BAPI_FIXEDASSET_CREATE1'
      EXPORTING
        KEY                  = LS_KEY
        CREATESUBNUMBER      = PS_DATA_CREATE-SUB_FLG
        TESTRUN              = CB_TEST
        GENERALDATA          = LS_GENERALDATA
        GENERALDATAX         = LS_GENERALDATAX
        INVENTORY            = LS_INVENTORY
        INVENTORYX           = LS_INVENTORYX
        POSTINGINFORMATION   = LS_POSTINGINFORMATION
        POSTINGINFORMATIONX  = LS_POSTINGINFORMATIONX
        TIMEDEPENDENTDATA    = LS_TIMEDEPENDENTDATA
        TIMEDEPENDENTDATAX   = LS_TIMEDEPENDENTDATAX
        ALLOCATIONS          = LS_ALLOCATIONS
        ALLOCATIONSX         = LS_ALLOCATIONSX
        ORIGIN               = LS_ORIGIN
        ORIGINX              = LS_ORIGINX
        INVESTACCTASSIGNMNT  = LS_INVESTACCTASSIGNMNT
        INVESTACCTASSIGNMNTX = LS_INVESTACCTASSIGNMNTX
        REALESTATE           = LS_REALESTATE
        REALESTATEX          = LS_REALESTATEX
      IMPORTING
        COMPANYCODE          = LV_COMPANYCODE
        ASSET                = LV_ASSET
        SUBNUMBER            = LV_SUBNUMBER
*       ASSETCREATED         =
        RETURN               = LS_RETURN
      TABLES
        DEPRECIATIONAREAS    = LT_DEPRECIATIONAREAS
        DEPRECIATIONAREASX   = LT_DEPRECIATIONAREASX
*       INVESTMENT_SUPPORT   =
*       EXTENSIONIN          =
      .
    CLEAR: PV_ASSET_MAIN.
    PV_ASSET_MAIN = LV_ASSET.
    IF CB_TEST = GC_TRUE.
      "if found 'Asset main number not entered' -> Change to warning
      IF LS_RETURN-TYPE = 'E' AND LS_RETURN-ID   = 'BAPI1022' AND LS_RETURN-NUMBER = '002'.
        LS_RETURN-TYPE = 'W'.
      ENDIF.
    ENDIF.
    IF LS_RETURN-TYPE = 'E' OR LS_RETURN-TYPE = 'A' .
      LS_MESSG-MSGTY = LS_RETURN-TYPE.
      LS_MESSG-MSGID = LS_RETURN-ID.
      LS_MESSG-MSGNO = LS_RETURN-NUMBER.
      LS_MESSG-MSGTX = LS_RETURN-MESSAGE.
      INSERT LS_MESSG INTO TABLE PT_MESSG.
      PV_ERROR = 'X'.
      RETURN.
    ENDIF.

    IF CB_TEST EQ GC_TRUE.
      IF LS_RETURN-TYPE = 'W'.
        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = 'W'.
        LS_MESSG-MSGID = LS_RETURN-ID.
        LS_MESSG-MSGNO = LS_RETURN-NUMBER.
        LS_MESSG-MSGTX = LS_RETURN-MESSAGE.
        INSERT LS_MESSG INTO TABLE PT_MESSG.
      ELSE.
        CLEAR LS_MESSG.
        LS_MESSG-MSGTY = 'S'.
        LS_MESSG-MSGID = 'ZTEC'.
        LS_MESSG-MSGNO = '000'.
*     Text-i01: Test upload successfully.
        LS_MESSG-MSGTX = TEXT-I01.
        INSERT LS_MESSG INTO TABLE PT_MESSG.
      ENDIF.
    ELSE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
      LS_MESSG-MSGID = 'ZTEC'.
      LS_MESSG-MSGNO = '000'.
*     Text-i03: Asset master posted successfully
      LS_MESSG-MSGTX = TEXT-I03.
      INSERT LS_MESSG INTO TABLE PT_MESSG.

    ENDIF.

  ENDIF.
*---------------------------------------------------------------

  IF CB_TEST = SPACE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    PS_DATA_CREATE-ANLN1 = LV_ASSET.
    PS_DATA_CREATE-ANLN2 = LV_SUBNUMBER.


*Find FI Doc
    IF PS_DATA_CREATE-KANSW IS NOT INITIAL.
      LOOP AT  LT_RETURN INTO LS_RETURN WHERE ID = 'FAA_POST' AND NUMBER = '092' AND MESSAGE_V3 = '0L' ##INTO_OK.
        PERFORM F_GET_FI_DOCUMENT USING LV_COMPANYCODE LV_ASSET LV_SUBNUMBER LS_RETURN
                                CHANGING PS_DATA_CREATE-GJAHR
                                         PS_DATA_CREATE-BELNR.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_fi_document
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_GET_FI_DOCUMENT  USING    PV_COMPANYCODE TYPE ANEK-BUKRS
                                 PV_ASSET       TYPE BF_ANLN1
                                 PV_SUBNUMBER   TYPE BF_ANLN2
                                 PS_RETURN      TYPE BAPIRET2
                        CHANGING PV_GJAHR       TYPE GJAHR
                                 PV_BELNR       TYPE BELNR_D.

  TYPES: BEGIN OF LTY_ANEK,
           BUKRS TYPE ANEK-BUKRS,
           ANLN1 TYPE ANEK-ANLN1,
           ANLN2 TYPE ANEK-ANLN2,
           GJAHR TYPE ANEK-GJAHR,
           BELNR TYPE ANEK-BELNR,
           LNRAN TYPE ANEK-LNRAN,
           AWORG TYPE ANEK-AWORG,
           AWTYP TYPE ANEK-AWTYP,
           AWKEY TYPE BKPF-AWKEY,
         END OF LTY_ANEK.
  DATA: LT_ANEK  TYPE TABLE OF LTY_ANEK,
        LS_ANEK  TYPE LTY_ANEK,
        LV_BUKRS TYPE BUKRS,
        LV_BELNR TYPE BELNR_D,
        LV_LDGRP TYPE LDGRP,
        LV_TABIX LIKE SY-TABIX.

  LV_BUKRS = PS_RETURN-MESSAGE_V1.
  LV_BELNR = PS_RETURN-MESSAGE_V2.
  LV_LDGRP = PS_RETURN-MESSAGE_V3.

  SELECT BUKRS ANLN1 ANLN2  ##TOO_MANY_ITAB_FIELDS      "#EC CI_SEL_DEL
         GJAHR BELNR LNRAN                           "#EC CI_SEL_NESTED
         AWORG AWTYP                                 "#EC CI_SEL_NESTED
    FROM ANEK
     INTO TABLE LT_ANEK
   WHERE BUKRS = PV_COMPANYCODE
     AND ANLN1 = PV_ASSET
     AND ANLN2 = PV_SUBNUMBER
     AND LDGRP = '0L'.
  IF SY-SUBRC = 0.
    SELECT BUKRS,ANLN1,ANLN2,GJAHR,                  "#EC CI_SEL_NESTED
           LNRAN,AFABE,ZUJHR,ZUCOD                 "#EC CI_SROFC_NESTED
      INTO TABLE @DATA(LT_ANEP)                    "#EC CI_NO_TRANSFORM
      FROM ANEP
       FOR ALL ENTRIES IN @LT_ANEK
     WHERE BUKRS = @LT_ANEK-BUKRS
       AND ANLN1 = @LT_ANEK-ANLN1
       AND ANLN2 = @LT_ANEK-ANLN2
       AND GJAHR = @LT_ANEK-GJAHR
       AND LNRAN = @LT_ANEK-LNRAN
       AND AUGLN <> @SPACE
      ORDER BY PRIMARY KEY.
    IF SY-SUBRC = 0.
      LOOP AT LT_ANEK ASSIGNING FIELD-SYMBOL(<LFS_ANEK>).
        LV_TABIX = SY-TABIX.
        READ TABLE LT_ANEP WITH KEY BUKRS = <LFS_ANEK>-BUKRS
                                    ANLN1 = <LFS_ANEK>-ANLN1
                                    ANLN2 = <LFS_ANEK>-ANLN2
                                    GJAHR = <LFS_ANEK>-GJAHR
                                    LNRAN = <LFS_ANEK>-LNRAN
                                    BINARY SEARCH
                                    TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          DELETE LT_ANEK INDEX LV_TABIX.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT LT_ANEK BY BUKRS ANLN1 ANLN2 GJAHR DESCENDING BELNR DESCENDING.
    READ TABLE LT_ANEK INTO LS_ANEK INDEX 1.
    IF SY-SUBRC = 0.
      CONCATENATE LS_ANEK-BELNR LS_ANEK-AWORG INTO LS_ANEK-AWKEY.
    ENDIF.


    SELECT  AWKEY,AWTYP,BUKRS,BELNR,GJAHR  ##WARN_OK "#EC CI_SEL_NESTED
      INTO  @DATA(LS_BKPF)
      FROM BKPF
      UP TO 1   ROWS
     WHERE AWKEY = @LS_ANEK-AWKEY
       AND AWTYP = @LS_ANEK-AWTYP
       AND LDGRP = '0L'
       AND BUKRS = @LV_BUKRS
       AND BELNR = @LV_BELNR
       AND LDGRP = @LV_LDGRP
       AND XREVERSING = @SPACE
       AND XREVERSED  = @SPACE
           ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF SY-SUBRC = 0.
      PV_BELNR = LS_BKPF-BELNR.
      PV_GJAHR = LS_BKPF-GJAHR.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CONVERSION_EXIT_ALPHA_OUTPUT  CHANGING PS_LOG TYPE GTY_LOG.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = PS_LOG-ANLKL
    IMPORTING
      OUTPUT = PS_LOG-ANLKL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = PS_LOG-KOSTL
    IMPORTING
      OUTPUT = PS_LOG-KOSTL.


  PERFORM CONV_EXIT_ALPHA_OUTPUT_DATE CHANGING PS_LOG-IVDAT.
  PERFORM CONV_EXIT_ALPHA_OUTPUT_DATE CHANGING PS_LOG-AKTIV.
  PERFORM CONV_EXIT_ALPHA_OUTPUT_DATE CHANGING PS_LOG-AFABG01.
  PERFORM CONV_EXIT_ALPHA_OUTPUT_DATE CHANGING PS_LOG-AFABG10.
  PERFORM CONV_EXIT_ALPHA_OUTPUT_DATE CHANGING PS_LOG-AFABG11.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form conv_exit_alpha_output_date
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CONV_EXIT_ALPHA_OUTPUT_DATE  CHANGING PV_DATE TYPE CHAR100.
  IF PV_DATE = '00000000'.
    PV_DATE = SPACE.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_validate_asset_subasset
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ASSET_SUBASSET  CHANGING PS_DATA  TYPE GTY_DATA
                                         PS_MESSG TYPE GTY_MESSG.


  CASE ABAP_TRUE.
    WHEN RB_NEW. "Create new

      PERFORM F_VALIDATE_ASSET_NEW CHANGING PS_DATA
                                            PS_MESSG.

    WHEN RB_CHG OR RB_REV. "Change / Reverse and Repost

      PERFORM F_VALIDATE_ASSET_CHG CHANGING PS_DATA
                                            PS_MESSG.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_asset_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ASSET_NEW  CHANGING PS_DATA  TYPE GTY_DATA
                                    PS_MESSG TYPE GTY_MESSG.

  TYPES: BEGIN OF LTY_ANLA,
           BUKRS TYPE ANLA-BUKRS,
           ANLN1 TYPE ANLA-ANLN1,
           ANLN2 TYPE ANLA-ANLN2,
         END OF LTY_ANLA.

  STATICS:
    LS_ANLA TYPE  LTY_ANLA.

  DATA:
    LV_LENGTH TYPE  I.
*    LV_MSGTX  TYPE GTY_MESSG-MSGTX.

* Only not initial
  IF PS_DATA-ANLN1 IS INITIAL.
    RETURN.
  ENDIF.

  LV_LENGTH = STRLEN( PS_DATA-ANLN1 ).
  IF LV_LENGTH GT 12 ##NUMBER_OK.
*   Text-e04 : Invalid Asset number
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
    CONCATENATE TEXT-E04 PS_DATA-ANLN1
           INTO PS_MESSG-MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PS_DATA-ANLN1
    IMPORTING
      OUTPUT = PS_DATA-ANLN1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PS_DATA-ANLN2
    IMPORTING
      OUTPUT = PS_DATA-ANLN2.

  DATA: LS_NRIV  TYPE NRIV.

  CLEAR: PS_DATA-ASSET_EXT_NO.
  "Check Asset class is external number
  IF PS_DATA-ANLN1  IS NOT INITIAL.      "// Asset no. has entered
    PS_DATA-ASSET_EXT_NO  = LS_NRIV-EXTERNIND.        "Flag External number range
  ENDIF.

  IF PS_DATA-ASSET_EXT_NO  EQ 'X'.

*     Validate with Database
    SELECT SINGLE BUKRS ANLN1 ANLN2                  "#EC CI_SEL_NESTED
      INTO LS_ANLA
      FROM ANLA
      WHERE BUKRS = PS_DATA-BUKRS
        AND ANLN1 = PS_DATA-ANLN1
        AND ANLN2 = PS_DATA-ANLN2.
    IF SY-SUBRC EQ 0.      "// Asset no. already exist
*       Text-e46 : Asset number is already exist
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZTEC'.
      PS_MESSG-MSGNO = '000'.
      CONCATENATE TEXT-E46 PS_DATA-ANLN1 PS_DATA-ANLN2
            INTO PS_MESSG-MSGTX
            SEPARATED BY SPACE.
      RETURN.
    ENDIF.
  ENDIF.


  "Auto populate sub asset number
  IF PS_DATA-ANLN1 <> SPACE AND
     PS_DATA-ANLN2 EQ SPACE.
    SELECT BUKRS, ANLN1, ANLN2                       "#EC CI_SEL_NESTED
      INTO TABLE @DATA(LT_ANLA_CHECK)
      FROM ANLA
      WHERE BUKRS = @PS_DATA-BUKRS
        AND ANLN1 = @PS_DATA-ANLN1.
    IF SY-SUBRC EQ 0.
      SORT LT_ANLA_CHECK BY ANLN2 DESCENDING.
      READ TABLE LT_ANLA_CHECK INTO DATA(LS_ANLA_CHECK) INDEX 1.
      IF SY-SUBRC EQ 0.
        PS_DATA-ANLN2 = LS_ANLA_CHECK-ANLN2 + 1.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_asset_chg
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM F_VALIDATE_ASSET_CHG  CHANGING PS_DATA  TYPE GTY_DATA
                                    PS_MESSG TYPE GTY_MESSG.

  TYPES: BEGIN OF LTY_ANLA,
           BUKRS TYPE ANLA-BUKRS,
           ANLN1 TYPE ANLA-ANLN1,
           ANLN2 TYPE ANLA-ANLN2,
         END OF LTY_ANLA.

  STATICS:
    LS_ANLA TYPE  LTY_ANLA.

  DATA:
"    LV_ANLN1  TYPE  ANLA-ANLN1,
*    LV_ANLN2  TYPE  ANLA-ANLN2,
    LV_LENGTH TYPE  I.

  IF PS_DATA-ANLN1 = SPACE.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e21 : Asset column doesn't have value
    PS_MESSG-MSGTX = TEXT-E21.
    RETURN.
  ENDIF.

  IF PS_DATA-ANLN2 = SPACE.
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
*   Text-e38 : Sub-number doesn't have value
    PS_MESSG-MSGTX = TEXT-E38.
    RETURN.
  ENDIF.

  LV_LENGTH = STRLEN( PS_DATA-ANLN1 ).
  IF LV_LENGTH GT 12 ##NUMBER_OK.
*   Text-e04 : Invalid Asset number
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
    CONCATENATE TEXT-E04 PS_DATA-ANLN1
           INTO PS_MESSG-MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

  LV_LENGTH = STRLEN( PS_DATA-ANLN2 ).
  IF LV_LENGTH GT 4 ##NUMBER_OK.
*   Text-e05 : Invalid Asset subnumber
    PS_MESSG-MSGTY = 'E'.
    PS_MESSG-MSGID = 'ZTEC'.
    PS_MESSG-MSGNO = '000'.
    CONCATENATE TEXT-E05 PS_DATA-ANLN2
           INTO PS_MESSG-MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PS_DATA-ANLN1
    IMPORTING
      OUTPUT = PS_DATA-ANLN1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PS_DATA-ANLN2
    IMPORTING
      OUTPUT = PS_DATA-ANLN2.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PS_DATA-KOSTL
    IMPORTING
      OUTPUT = PS_DATA-KOSTL.

  IF PS_DATA-ASSET_EXT_NO  EQ SPACE.

  ELSE.
*     Validate with Database
    SELECT SINGLE BUKRS ANLN1 ANLN2                  "#EC CI_SEL_NESTED
      INTO LS_ANLA
      FROM ANLA
      WHERE BUKRS = PS_DATA-BUKRS
       AND ANLN1  = PS_DATA-ANLN1
       AND ANLN2  = PS_DATA-ANLN2.
    IF SY-SUBRC EQ 0.
*       Text-e47 : Asset / Sub number is already exist
      PS_MESSG-MSGTY = 'E'.
      PS_MESSG-MSGID = 'ZTEC'.
      PS_MESSG-MSGNO = '000'.
      CONCATENATE TEXT-E47 PS_DATA-ANLN1 PS_DATA-ANLN2
             INTO PS_MESSG-MSGTX
             SEPARATED BY SPACE.
      RETURN.
    ENDIF.
  ENDIF.


ENDFORM.
"Begin of Insertion CH01
*&---------------------------------------------------------------------*
*& Form f_validate_chg_kostl
*&---------------------------------------------------------------------*
*& Validate if change mode. cannot change cost center because it will post new document
*& Instead, show error message and ask user to change via LSMW.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CHG_KOSTL  CHANGING PS_DATA  TYPE GTY_DATA
                                    PS_MESSG TYPE GTY_MESSG.

  IF PS_DATA-KOSTL IS NOT INITIAL. "If change in cost center

* Convert to internal format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = PS_DATA-ANLN1
      IMPORTING
        OUTPUT = PS_DATA-ANLN1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = PS_DATA-ANLN2
      IMPORTING
        OUTPUT = PS_DATA-ANLN2.


*     Validate with Database
    SELECT SINGLE BUKRS, ANLN1, ANLN2, BDATU, KOSTL, PRCTR   ##WARN_OK "#EC WARNOK
      INTO @DATA(LS_ANLZ)
      FROM ANLZ
      WHERE BUKRS  EQ @PS_DATA-BUKRS
       AND  ANLN1  EQ @PS_DATA-ANLN1
       AND  ANLN2  EQ @PS_DATA-ANLN2
       AND  BDATU  GE @SY-DATUM.
    IF SY-SUBRC EQ 0.
      IF LS_ANLZ-KOSTL NE PS_DATA-KOSTL.
*       Text-e47 : Asset / Sub number is already exist
        PS_MESSG-MSGTY = 'E'.
        PS_MESSG-MSGID = 'ZTEC'.
        PS_MESSG-MSGNO = '000'.
        CONCATENATE TEXT-E54 TEXT-E55
               INTO PS_MESSG-MSGTX
               SEPARATED BY SPACE.
        RETURN.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
