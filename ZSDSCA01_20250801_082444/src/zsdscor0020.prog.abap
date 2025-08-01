*-----------------------------------------------------------------------
*  Program ID         : ZSDSCOR0020
*  Creation Date      : 20.06.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : Upload Cost Center master
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

REPORT ZSDSCOR0020 NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
*--------------------------------------
* Types for File Processing
*--------------------------------------
TYPES:
  BEGIN OF GTY_RAW,
    TLINE TYPE  STRING,
  END OF GTY_RAW.
TYPES:
  GTTY_RAW  TYPE  STANDARD TABLE OF GTY_RAW.

TYPES:
  BEGIN OF GTY_FILE.
    INCLUDE TYPE ZSDSCOS011.
TYPES:
    FILENAME TYPE FILENAMECI-FILEEXTERN,
  END OF GTY_FILE.

*--------------------------------------
* Types for Data Processing
*--------------------------------------
TYPES:
  BEGIN OF GTY_DATA.
    INCLUDE  TYPE ZSDSCOS010.
TYPES:
  END OF GTY_DATA.

*--------------------------------------
* Types for master validation
*--------------------------------------
*->Key for 'FOR ALL ENTRIES'
TYPES:
  BEGIN OF GTY_BUKRS,
    BUKRS TYPE T001-BUKRS,
  END OF GTY_BUKRS.

TYPES:
  BEGIN OF GTY_WAERS,
    WAERS TYPE TCURC-WAERS,
  END OF GTY_WAERS.

TYPES:
  BEGIN OF GTY_KOSTL,
    KOSTL TYPE CSKS-KOSTL,
    KOKRS TYPE CSKS-KOKRS,
  END   OF GTY_KOSTL.

TYPES:
  BEGIN OF GTY_PRCTR,
    PRCTR TYPE CEPC-PRCTR,
    KOKRS TYPE CEPC-KOKRS,
  END OF GTY_PRCTR.

*->Master data
TYPES:
  BEGIN OF GTY_T001,
    BUKRS TYPE T001-BUKRS,
    BUTXT TYPE T001-BUTXT,
    WAERS TYPE T001-WAERS,
    LAND1 TYPE T001-LAND1,
    KTOPL TYPE T001-KTOPL,
    KOKRS TYPE TKA02-KOKRS,
    KALSM TYPE T005-KALSM,
  END   OF GTY_T001.

TYPES:
  BEGIN OF GTY_CSKS,
    KOSTL TYPE CSKS-KOSTL,
    KOKRS TYPE CSKS-KOKRS,
    DATBI TYPE CSKS-DATBI,
    DATAB TYPE CSKS-DATAB,
  END   OF GTY_CSKS.

TYPES:
  BEGIN OF GTY_CEPC,
    PRCTR TYPE CEPC-PRCTR,
    KOKRS TYPE CEPC-KOKRS,
    DATBI TYPE CEPC-DATBI,
  END OF GTY_CEPC.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE      TYPE  CHAR1       VALUE 'X',
  GC_TCODE     TYPE  SY-TCODE    VALUE 'COC001',
* Splitter used in Raw data
  GC_SPLIT     TYPE  CHAR1
                          VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,
  GC_SPLIT_APP TYPE  CHAR1
                          VALUE '|'.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_FILE TYPE TABLE OF GTY_FILE,
  GT_DATA TYPE TABLE OF GTY_DATA.

DATA:
  GT_BUKRS TYPE TABLE OF GTY_BUKRS,
  GT_WAERS TYPE TABLE OF GTY_WAERS,
  GT_KOSTL TYPE TABLE OF GTY_KOSTL,
  GT_PRCTR TYPE TABLE OF GTY_PRCTR.

DATA:
  GT_T001  TYPE TABLE OF GTY_T001,
  GT_TCURC TYPE TABLE OF TCURC,
  GT_CSKS  TYPE TABLE OF GTY_CSKS,
  GT_CEPC  TYPE TABLE OF GTY_CEPC.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 22,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 78.

*----------------------------------------------------------------------*
* BAPI Variables
*----------------------------------------------------------------------*
DATA:
  GT_COSTCTR TYPE TABLE OF BAPI0012_CCINPUTLIST.

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
*  PARAMETERS:
*    RB_LOCAL TYPE  FLAG RADIOBUTTON GROUP G1 DEFAULT 'X'
*                         USER-COMMAND DMY.
* Text-s02: Local File
  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
    PARAMETERS:
      P_LFILE  TYPE  STRING LOWER CASE MODIF ID LOC.
    SELECTION-SCREEN BEGIN OF LINE.
*     Text-s03: Start Row
      SELECTION-SCREEN COMMENT 1(29) TEXT-S03 FOR FIELD P_BEGROW.
      PARAMETERS:
        P_BEGROW TYPE I DEFAULT 2 MODIF ID LOC.
*     Text-s04: Start Column
      SELECTION-SCREEN COMMENT 50(15) TEXT-S04 FOR FIELD P_BEGCOL.
      PARAMETERS:
        P_BEGCOL TYPE I DEFAULT 1 MODIF ID LOC.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
*     Text-s05: End Row
      SELECTION-SCREEN COMMENT 1(29) TEXT-S05 FOR FIELD P_ENDROW.
      PARAMETERS:
        P_ENDROW TYPE I DEFAULT 9999 MODIF ID LOC.
*     Text-s06: End Column
      SELECTION-SCREEN COMMENT 50(15) TEXT-S06 FOR FIELD P_ENDCOL.
      PARAMETERS:
        P_ENDCOL TYPE I DEFAULT 16 MODIF ID LOC.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK B2.
*  PARAMETERS:
*    RB_APPSV TYPE  FLAG RADIOBUTTON GROUP G1.
*
** Text-s07: Application Server File
*  SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S07.
*    PARAMETERS:
*      P_AFPATH TYPE  TEXT1000 VISIBLE LENGTH 50 MODIF ID APP,
*      P_AFNAME TYPE  FILENAMECI-FILEEXTERN MODIF ID APP,
*      P_AFILE  TYPE  STRING LOWER CASE NO-DISPLAY.
*  SELECTION-SCREEN END OF BLOCK B3.

  PARAMETERS:
    CB_TEST TYPE  FLAG AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK_TCODE USING GC_TCODE.
  PERFORM F_GET_GENC.
  PERFORM F_INIT_SELECTION_SCR.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LFILE.
* List Local input File
  PERFORM F_LIST_IFILE CHANGING P_LFILE.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_AFNAME.
** List Application Server File
*  PERFORM F_LIST_AFILE  USING  P_AFPATH
*                      CHANGING P_AFNAME.

AT SELECTION-SCREEN OUTPUT.
* Set Screen format
  PERFORM F_SET_SELECTION_SCREEN_FORMAT.

AT SELECTION-SCREEN.
*  IF RB_LOCAL EQ GC_TRUE.
*    CLEAR: P_AFILE,
*           P_AFNAME.
*  ELSE.
*    CLEAR: P_LFILE.
*  ENDIF.

  CASE SSCRFIELDS-UCOMM.
    WHEN 'ONLI'.
      PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDCASE.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Upload FI documment
  PERFORM F_PROCESS_DATA.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

* Display ALV Report
  PERFORM F_DISPLAY_RESULT USING GT_DATA.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCOR0020_F01.
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* FORM Routines
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_authorize_check
*&---------------------------------------------------------------------*
*& Check Authorization on t-code
*&---------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK_TCODE  USING PV_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD PV_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH PV_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_genc
*&---------------------------------------------------------------------*
FORM F_GET_GENC .


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
*     For Local File Criteria
*     --------------------------
*      WHEN 'LOC'.
*        IF RB_LOCAL EQ GC_TRUE.
*          SCREEN-INPUT = 1.
*        ELSE.
*          SCREEN-INPUT = 0.
*        ENDIF.
*        MODIFY SCREEN.
*
**     --------------------------
**     For AppServer File Criteria
**     --------------------------
*      WHEN 'APP'.
*        IF RB_APPSV EQ GC_TRUE.
*          SCREEN-INPUT = 1.
*        ELSE.
*          SCREEN-INPUT = 0.
*        ENDIF.
*        MODIFY SCREEN.

*     --------------------------
*     For Display only fields
*     --------------------------
      WHEN 'DIS'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.

    ENDCASE.


  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_init_selection_scr
*----------------------------------------------------------------------*
*  Initialize Selection Screen
*----------------------------------------------------------------------*
FORM F_INIT_SELECTION_SCR.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_server_path
*&---------------------------------------------------------------------*
*& Get Server Path
*&---------------------------------------------------------------------*
FORM F_GET_SERVER_PATH .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_list_ifile
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*& Form f_list_afile
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_LIST_AFILE  USING    PV_AFPATH TYPE TEXT1000
                   CHANGING PV_AFNAME TYPE FILENAMECI-FILEEXTERN.

  DATA:
    LV_IPATH TYPE DXFIELDS-LONGPATH,
    LV_OPATH TYPE DXFIELDS-LONGPATH,
    LV_FNAME TYPE SDBAH-ACTID,
    LV_EXT   TYPE SDBAH-FUNCT,
    LW_HOSTS TYPE MSXXLIST,
    LT_HOSTS TYPE TABLE OF MSXXLIST.

  LV_IPATH = PV_AFPATH.

  CALL FUNCTION 'RFC_GET_LOCAL_SERVERS' ##FM_SUBRC_OK
    TABLES
      HOSTS         = LT_HOSTS
    EXCEPTIONS
      NOT_AVAILABLE = 1
      OTHERS        = 2.
  CLEAR LW_HOSTS.
  READ TABLE LT_HOSTS INTO LW_HOSTS INDEX 1.

  CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
    EXPORTING
      I_LOCATION_FLAG = 'A'
      I_PATH          = LV_IPATH
      I_SERVER        = LW_HOSTS-NAME
*     FILEMASK        = '*.*'
    IMPORTING
      O_PATH          = LV_OPATH
*     ABEND_FLAG      =
    EXCEPTIONS
      RFC_ERROR       = 1
      ERROR_WITH_GUI  = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'SPLIT_FILENAME'
    EXPORTING
      LONG_FILENAME  = LV_OPATH
    IMPORTING
      PURE_FILENAME  = LV_FNAME
      PURE_EXTENSION = LV_EXT.

  CONCATENATE LV_FNAME '.' LV_EXT
    INTO PV_AFNAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_selection_screen
*&---------------------------------------------------------------------*
*& Validate Selection Screen
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  DATA:
    LV_FILE TYPE  BTCXPGPAR.

* -----------------------
* Local File Processing
* -----------------------
*  IF RB_LOCAL EQ GC_TRUE.
*   Local file not support in Background mode.
  IF SY-BATCH EQ GC_TRUE.
*     Error: Local file does not support in background processing mode.
    MESSAGE E007(ZTEC).
    RETURN.
  ENDIF.
  PERFORM F_VALIDATE_LOCAL_PARAM.

* -----------------------
* Application File Processing
* -----------------------
*  ELSE.
**   Input file is required
*    IF P_AFNAME IS INITIAL.
*      SET CURSOR FIELD 'P_AFNAME'.
**     Error: Please enter a valid filename.
*      MESSAGE E002(ZTEC).
*      RETURN.
*    ENDIF.
*    PERFORM F_VALIDATE_APPSV_FILE  USING  P_AFPATH
*                                          P_AFNAME
*                                 CHANGING P_AFILE.
*    IF P_AFILE IS INITIAL.
*      RETURN.
*    ENDIF.
**   Check File Exists
*    LV_FILE  = P_AFILE.
*    CALL FUNCTION 'DMC_MDS_CHECK_FILE_EXISTS'
*      EXPORTING
*        IV_FILENAME     = LV_FILE
*      EXCEPTIONS
*        FILE_OPEN_ERROR = 1
*        INTERNAL_ERROR  = 2
*        OTHERS          = 3.
*    IF SY-SUBRC <> 0.
*      SET CURSOR FIELD 'P_AFNAME'.
**     Error: File does not exist.
*      MESSAGE E005(ZTEC).
*      RETURN.
*    ENDIF.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_local_param
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_VALIDATE_LOCAL_PARAM .

  DATA:
    LV_EXIST TYPE  FLAG.

* Input file is required
  IF P_LFILE IS INITIAL.
    SET CURSOR FIELD 'P_LFILE'.
*   Error: Please enter a valid filename.
    MESSAGE E002(ZTEC).
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
    MESSAGE E005(ZTEC).
    RETURN.
  ENDIF.
* Ranges are required
  IF P_BEGROW IS INITIAL OR
     P_BEGCOL IS INITIAL OR
     P_ENDROW IS INITIAL OR
     P_ENDCOL IS INITIAL.
    SET CURSOR FIELD 'P_BEGROW'.
*   Error: Please specified range to read excel file.
    MESSAGE E014(ZTEC).
    RETURN.
  ENDIF.
** User can adjust template (remove column heading) and input new start/end row
*  IF p_begrow LT gs_config-beg_row OR
*     p_endrow LT gs_config-beg_row.
*    SET CURSOR FIELD 'P_BEGROW'.
**   Error: Start/End row must be greater than &1.
*    MESSAGE e016(ztec) WITH gs_config-beg_row.
*    RETURN.
*  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_appsv_file
*----------------------------------------------------------------------*
*  Validate Application Server File
*----------------------------------------------------------------------*
FORM F_VALIDATE_APPSV_FILE  USING  PV_FPATH  TYPE TEXT1000
                                   PV_AFNAME TYPE FILENAMECI-FILEEXTERN
                          CHANGING PV_AFILE  TYPE CLIKE.

  DATA:
    LV_FILENAME  TYPE  STRING.

* Initialize Output
  CLEAR: PV_AFILE.

* Assign Output
  CONCATENATE PV_FPATH
              PV_AFNAME
    INTO LV_FILENAME.
  PV_AFILE = LV_FILENAME.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_read_input_file
*----------------------------------------------------------------------*
*  Read input file into Raw internal table
*----------------------------------------------------------------------*
FORM F_READ_INPUT_FILE.

  DATA:
    LS_FILE   TYPE GTY_FILE,
    LS_RETURN TYPE  BAPIRET2,
    LT_RAW    TYPE  GTTY_RAW.

  FIELD-SYMBOLS:
    <LFS_RAW>   TYPE  GTY_RAW.

* Show Progress
* Text-p01 : Reading Input file. . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

*  CASE GC_TRUE.
*    WHEN RB_LOCAL.
  PERFORM F_READ_EXCEL USING   P_LFILE
                               P_BEGROW
                               P_BEGCOL
                               P_ENDROW
                               P_ENDCOL
                      CHANGING LT_RAW
                               LS_RETURN.
  LOOP AT LT_RAW ASSIGNING <LFS_RAW>.
    CLEAR LS_FILE.
*       Translate Raw line into internal Structure
    PERFORM F_TRANSLATE_RAW  USING  <LFS_RAW>
                          CHANGING  LS_FILE.
    LS_FILE-FILENAME = P_LFILE.
    APPEND LS_FILE TO GT_FILE.
  ENDLOOP.
*    WHEN RB_APPSV.
*      PERFORM F_READ_APPSV_FILE  USING  P_AFILE
*                               CHANGING LT_RAW
*                                        LS_RETURN.
*      LOOP AT LT_RAW ASSIGNING <LFS_RAW>.
*        CLEAR LS_FILE.
**       Translate Raw line into internal Structure
*        PERFORM F_TRANSLATE_RAW  USING  <LFS_RAW>
*                              CHANGING  LS_FILE.
*        LS_FILE-FILENAME =  P_AFILE.
*        APPEND LS_FILE TO GT_FILE.
*      ENDLOOP.
*  ENDCASE.

  IF LS_RETURN IS NOT INITIAL.
    MESSAGE LS_RETURN-MESSAGE TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF LT_RAW[] IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZTEC).
    LEAVE LIST-PROCESSING.
  ENDIF.

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
                 CHANGING PT_RAW     TYPE GTTY_RAW
                          PS_RETURN  TYPE BAPIRET2.

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
  CLEAR PS_RETURN.

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
    PERFORM F_SET_RET_MSG USING 'E' 'ZTEC' '021'
                                SPACE
                                SPACE
                                SPACE
                                SPACE
                       CHANGING PS_RETURN.
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
*  Form f_read_appsv_file
*----------------------------------------------------------------------*
*  Read Application Server file
*----------------------------------------------------------------------*
FORM F_READ_APPSV_FILE  USING  PV_FNAME  TYPE  CLIKE
                      CHANGING PT_RAW    TYPE  GTTY_RAW
                               PS_RETURN TYPE  BAPIRET2.

  DATA:
      LS_RAW    TYPE  GTY_RAW.

  DATA:
    LREF_EXCEPT  TYPE  REF TO CX_ROOT.

  DATA:
    LV_FILENAME TYPE  STRING.


* Initialize Output
  CLEAR: PT_RAW.
  CLEAR PS_RETURN.

  LV_FILENAME = PV_FNAME.

  TRY.
*---------------------------------
*     Open file for Reading
*---------------------------------
      OPEN DATASET LV_FILENAME FOR INPUT IN TEXT MODE
                                   ENCODING DEFAULT
                                   IGNORING CONVERSION ERRORS
                                   REPLACEMENT CHARACTER SPACE.
      IF SY-SUBRC NE 0.
*       Error : Cannot open file for reading.
        PERFORM F_SET_RET_MSG USING 'E' 'ZTEC' '009'
                                    SPACE
                                    SPACE
                                    SPACE
                                    SPACE
                           CHANGING PS_RETURN.
        RETURN.
      ENDIF.

*---------------------------------
*     Read File Content
*---------------------------------
      DO.
        CLEAR LS_RAW.
        READ DATASET LV_FILENAME INTO LS_RAW-TLINE.
        IF SY-SUBRC NE 0.
          EXIT.
        ELSE.
          APPEND LS_RAW TO PT_RAW.
        ENDIF.
      ENDDO.

*---------------------------------
*     Close File
*---------------------------------
      CLOSE DATASET LV_FILENAME.

*---------------------------------
*   Error Handling
*---------------------------------
    CATCH CX_SY_FILE_OPEN
          CX_SY_CODEPAGE_CONVERTER_INIT
          CX_SY_CONVERSION_CODEPAGE
          CX_SY_FILE_AUTHORITY
          CX_SY_PIPES_NOT_SUPPORTED
          CX_SY_TOO_MANY_FILES
     INTO LREF_EXCEPT.
*     Error : Cannot open file for reading.
      PERFORM F_SET_RET_MSG USING 'E' SY-MSGID SY-MSGNO
                                  SY-MSGV1
                                  SY-MSGV2
                                  SY-MSGV3
                                  SY-MSGV4
                         CHANGING PS_RETURN.
      PS_RETURN-MESSAGE = LREF_EXCEPT->GET_TEXT( ).
      RETURN.
  ENDTRY.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_translate_raw
*----------------------------------------------------------------------*
*  Convert Raw to Data
*----------------------------------------------------------------------*
FORM F_TRANSLATE_RAW  USING  US_RAW     TYPE  GTY_RAW
                    CHANGING CS_FILE    TYPE  GTY_FILE.

  DATA:
    LT_SPLIT TYPE STANDARD TABLE OF STRING,
    LS_SPLIT LIKE LINE OF LT_SPLIT,
    LV_SPLIT TYPE CHAR1.

  FIELD-SYMBOLS:
    <LFS_VALUE>  TYPE ANY.

*  IF RB_LOCAL EQ GC_TRUE.
  LV_SPLIT = GC_SPLIT.
*  ELSE.
*    LV_SPLIT = GC_SPLIT_APP.
*  ENDIF.

* Split Into Fields
  SPLIT US_RAW-TLINE AT LV_SPLIT INTO TABLE LT_SPLIT.
  LOOP AT LT_SPLIT INTO LS_SPLIT.
    ASSIGN COMPONENT SY-TABIX OF STRUCTURE CS_FILE TO <LFS_VALUE>.
    IF SY-SUBRC EQ 0.
      <LFS_VALUE> = LS_SPLIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*       Set return message
*       1. Fill status
*       2. Fill message
*----------------------------------------------------------------------*
FORM F_SET_MESSAGE  USING PV_TYPE    TYPE BAPI_MTYPE
                          PV_ID      TYPE SYMSGID
                          PV_NUMBER  TYPE SYMSGNO
                          PV_MSG1    TYPE ANY
                          PV_MSG2    TYPE ANY
                          PV_MSG3    TYPE ANY
                          PV_MSG4    TYPE ANY
                 CHANGING PV_STATUS  TYPE ICON_D
                          PV_MESSAGE TYPE BAPI_MSG.

  MESSAGE ID PV_ID TYPE PV_TYPE NUMBER PV_NUMBER
          INTO PV_MESSAGE
          WITH PV_MSG1 PV_MSG2 PV_MSG3 PV_MSG4.

  CASE PV_TYPE.
    WHEN 'E'.
      PV_STATUS      = ICON_RED_LIGHT.
    WHEN 'W'.
      PV_STATUS      = ICON_YELLOW_LIGHT.
    WHEN 'S'.
      PV_STATUS      = ICON_GREEN_LIGHT.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " f_set_message
*&---------------------------------------------------------------------*
*&      Form  F_SET_RET_MSG
*&---------------------------------------------------------------------*
*       Set return message
*----------------------------------------------------------------------*
FORM F_SET_RET_MSG  USING    PV_MSGTYP   TYPE BAPI_MTYPE
                             PV_MSGID    TYPE SYMSGID
                             PV_MSGNO    TYPE SYMSGNO
                             PV_MSGV1    TYPE ANY
                             PV_MSGV2    TYPE ANY
                             PV_MSGV3    TYPE ANY
                             PV_MSGV4    TYPE ANY
                    CHANGING PS_RTN_MSG  TYPE BAPIRET2.

  PS_RTN_MSG-TYPE   = PV_MSGTYP.
  PS_RTN_MSG-ID     = PV_MSGID.
  PS_RTN_MSG-NUMBER = PV_MSGNO.
  MESSAGE ID PV_MSGID  TYPE PV_MSGTYP NUMBER PV_MSGNO
                      INTO PS_RTN_MSG-MESSAGE
                      WITH PV_MSGV1 PV_MSGV2 PV_MSGV3 PV_MSGV4.

ENDFORM.                    " F_SET_RET_MSG
*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_EXIT
*&---------------------------------------------------------------------*
*       Conversion Exit
*----------------------------------------------------------------------*
FORM F_CONVERSION_EXIT USING VALUE(PV_EXIT)  TYPE ANY
                             VALUE(PV_TYPE)  TYPE ANY
                             VALUE(PV_INPUT) TYPE ANY
                       CHANGING PV_OUTPUT    TYPE ANY.
  DATA:
    LV_CONEXIT TYPE CHAR30.

  CONCATENATE 'CONVERSION_EXIT_' PV_EXIT '_' PV_TYPE INTO LV_CONEXIT.

  PV_OUTPUT = PV_INPUT.
  CALL FUNCTION LV_CONEXIT
    EXPORTING
      INPUT  = PV_OUTPUT
    IMPORTING
      OUTPUT = PV_OUTPUT.

ENDFORM.                    "F_CONVERSION_EXIT
*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_DATE_INTERNAL
*&---------------------------------------------------------------------*
*       Conversion Exit
*----------------------------------------------------------------------*
FORM F_CONVERT_DATE_INTERNAL USING VALUE(PV_INPUT)  TYPE ANY
                          CHANGING PV_OUTPUT TYPE ANY.
* File Date format DD.MM.YYYY
  CLEAR PV_OUTPUT.
  PV_OUTPUT+0(4) = PV_INPUT+6(4).
  PV_OUTPUT+4(2) = PV_INPUT+3(2).
  PV_OUTPUT+6(2) = PV_INPUT+0(2).

ENDFORM.                    "F_CONVERT_DATE_INTERNAL
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CF
*&---------------------------------------------------------------------*
*       Popup to confirm to proceed furhter step
*----------------------------------------------------------------------*
FORM F_POPUP_TO_CF USING PV_QUEST  TYPE ANY
                CHANGING PV_ANSWER TYPE C.

  DATA:
    LV_ANSWER TYPE C.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = PV_QUEST
      ICON_BUTTON_1         = 'ICON_OKAY'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = SPACE
      POPUP_TYPE            = 'ICON_MESSAGE_WARNING'
    IMPORTING
      ANSWER                = LV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    PV_ANSWER = LV_ANSWER.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA.

  DATA:
    LV_QUEST  TYPE STRING,
    LV_ANSWER TYPE C.

*--------------------------------------
* Confirm 'Actual Run'
*--------------------------------------
  IF CB_TEST EQ SPACE.
    "Are you sure to confirm posting?
    LV_QUEST = TEXT-T01.
*   Confirm to &1?
    PERFORM F_POPUP_TO_CF   USING LV_QUEST
                         CHANGING LV_ANSWER.
    CASE LV_ANSWER.
      WHEN '1'.               "Yes (button1)
*       Continue execute report
      WHEN '2'.               "No
        LEAVE LIST-PROCESSING.
      WHEN 'A'.               "Cancel
        LEAVE LIST-PROCESSING.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

* --------------------------------
* Step1: Read Input file data
* --------------------------------
  PERFORM F_READ_INPUT_FILE.

* --------------------------------
* Step2: Convert data from file into SAP format
* --------------------------------
  PERFORM F_PREPARE_DATA.

* --------------------------------
* Step3: Validate file data
* --------------------------------
  PERFORM F_VALIDATE_DATA.

* --------------------------------
* Step4: Upload data ('Test Run'/'Actual Run')
* --------------------------------
  IF CB_TEST EQ 'X'.
    PERFORM F_UPLOAD_DATA USING CB_TEST.
  ELSE.
*    PERFORM f_upload_data USING 'X'.
*
**   Only post when all items are green
*    READ TABLE gt_data WITH KEY status = icon_red_light
*                                INTO lw_data.
*    IF sy-subrc NE 0.
    PERFORM F_UPLOAD_DATA USING CB_TEST.
*    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING PT_RESULT TYPE TABLE.

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
  ASSIGN PT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT USING 'ZSDSCOS010'
                            CHANGING GT_FIELDCAT_1.
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
FORM F_ALV_LAYOUT CHANGING PS_LAYOUT  TYPE  LVC_S_LAYO
                           PS_VARIANT TYPE  DISVARIANT
                           PS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  PS_LAYOUT, PS_VARIANT, PS_PRINT.

* determine layout
  PS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  PS_LAYOUT-CWIDTH_OPT = GC_TRUE.
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
FORM F_ALV_BUILD_FIELDCAT USING PV_STRUCTURE TYPE TABNAME
                       CHANGING PT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LV_TEXT      TYPE TEXT60,
    LV_FIELDNAME TYPE CHAR35,
    LV_NUMC3     TYPE NUMC3.

  FIELD-SYMBOLS:
    <LFS_FIELDCAT> TYPE  LVC_S_FCAT,
    <LFS_VALUE>    TYPE ANY.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  PV_STRUCTURE
                              CHANGING PT_FIELDCAT.

*--------------------------------------
* Assign Text Elements to Columns Texts
*--------------------------------------
* Start Column1 at Data field
* TEXT-001 for column1
* TEXT-002 for column2
* Exclude Columns (which defined below)
  LOOP AT PT_FIELDCAT ASSIGNING <LFS_FIELDCAT>.
    LV_NUMC3 = SY-TABIX.
    CONCATENATE 'TEXT-' LV_NUMC3
      INTO LV_FIELDNAME.
    ASSIGN (LV_FIELDNAME) TO <LFS_VALUE>.
    IF SY-SUBRC EQ 0.
      LV_TEXT = <LFS_VALUE>.

      <LFS_FIELDCAT>-REPTEXT   = LV_TEXT.
      <LFS_FIELDCAT>-COLTEXT   = LV_TEXT.
      <LFS_FIELDCAT>-SCRTEXT_L = LV_TEXT.
      <LFS_FIELDCAT>-SCRTEXT_M = LV_TEXT.
      <LFS_FIELDCAT>-SCRTEXT_S = LV_TEXT.
    ENDIF.
  ENDLOOP.

  LOOP AT PT_FIELDCAT ASSIGNING <LFS_FIELDCAT>.
    CASE <LFS_FIELDCAT>-FIELDNAME.
      WHEN 'STATUS'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN 'COSTCTR'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN 'VALID_FROM'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN 'VALID_TO'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN 'NAME'.
        <LFS_FIELDCAT>-KEY    = 'X'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING PT_SORT TYPE LVC_T_SORT.



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
  CONCATENATE SY-REPID SY-TITLE
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
* Add value in Line 8
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
  IF CB_TEST EQ 'X'.
*   Text-h08 : Test Run
    LV_TEXT = TEXT-H08.
  ELSE.
*   Text-h11 : Update Run
    LV_TEXT = TEXT-H11.
  ENDIF.
  CALL METHOD LREF_TABLE->SPAN_COLUMNS
    EXPORTING
      COL_START_SPAN = LREF_COL_KEY
      NO_OF_COLS     = 2.
  CALL METHOD LREF_TABLE->SET_ROW_STYLE
    EXPORTING
      ROW_NO       = 9
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LV_TEXT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_print_top_of_page_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LV_COL01 TYPE  I VALUE 25,
    LV_COL02 TYPE  I VALUE 35,
    LV_TEXT  TYPE  TEXT50,
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

  IF CB_TEST EQ 'X'.
*   Text-h08 : Test Run
    LV_TEXT = TEXT-H08.
  ELSE.
*   Text-h11 : Update Run
    LV_TEXT = TEXT-H11.
  ENDIF.
  WRITE AT: /1(20) LV_TEXT CENTERED COLOR COL_TOTAL.

ENDFORM.
