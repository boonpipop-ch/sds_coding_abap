*-----------------------------------------------------------------------
*  Program ID         : ZSDSCMD0020
*  Creation Date      : 18.12.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : Warranty Letter Migration Table and Program Upload
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
REPORT ZSDSCMD0020.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS ##NEEDED,
  ICON ##NEEDED.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSCMC008.
TYPES:   MESSAGE_ICON TYPE ICON_D,     " Message Icon
         MESSAGE_TYPE TYPE CHAR1,      " Message Type (E,S,W,I)
         MESSAGE      TYPE BAPI_MSG,   " Message Text
         UPDATE_FLAG  TYPE C,          " I for insert , U for update
       END OF TS_RESULT.

TYPES: TT_RESULT  TYPE  STANDARD TABLE OF TS_RESULT.

TYPES: TY_DATA TYPE ZSDSCMC008.


*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSCM006',
  GC_TAB   TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT TYPE  TT_RESULT                           ##NEEDED,
  GT_UPLOAD TYPE TABLE OF TY_DATA                     ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_UPLOAD     TYPE TY_DATA                          ##NEEDED.
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
  GF_PARAM           TYPE  CHAR10                              ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSCMC008'.

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
* Text-s01: File Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    P_IFILE TYPE  STRING LOWER CASE OBLIGATORY,
    CB_TEST TYPE  FLAG DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_IFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_IFILE.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM F_READ_INPUT_FILE USING P_IFILE.
  IF GT_UPLOAD IS NOT INITIAL.
    PERFORM F_VALIDATE_DATA CHANGING GT_RESULT.
    IF GT_RESULT IS NOT INITIAL AND CB_TEST = ''.
      PERFORM UPDATE_DATABASE USING GT_RESULT.
    ENDIF.
  ENDIF.
  IF GT_RESULT IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.


*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
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
      FILE_FILTER             = 'Text File(TXT)|*.TXT' ##NO_TEXT
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
*  Form f_validate_data
*----------------------------------------------------------------------*
*  validate data
*----------------------------------------------------------------------*
FORM F_VALIDATE_DATA  CHANGING CT_RESULT TYPE TT_RESULT.
  DATA: LS_RESULT TYPE TS_RESULT.

  SELECT SERNR,MATNR,EQUNR          "#EC CI_FAE_NO_LINES_OK
    FROM EQUI
     FOR ALL ENTRIES IN @GT_UPLOAD
   WHERE SERNR = @GT_UPLOAD-SERNR
     AND MATNR = @GT_UPLOAD-MATNR
    INTO TABLE @DATA(LT_SERNR_MATNR).
  IF SY-SUBRC = 0.
    SORT LT_SERNR_MATNR BY SERNR MATNR EQUNR.
  ENDIF.

  SELECT EQUNR                      "#EC CI_FAE_NO_LINES_OK
    FROM EQUI
     FOR ALL ENTRIES IN @GT_UPLOAD
   WHERE EQUNR = @GT_UPLOAD-EQUNR
    INTO TABLE @DATA(LT_EQUNR).
  IF SY-SUBRC = 0.
    SORT LT_EQUNR BY EQUNR.
  ENDIF.

  SELECT *                          "#EC CI_FAE_NO_LINES_OK
    FROM ZSDSCMC008
     FOR ALL ENTRIES IN @GT_UPLOAD
   WHERE EQUNR = @GT_UPLOAD-EQUNR
     AND MATNR = @GT_UPLOAD-MATNR
     AND SERNR = @GT_UPLOAD-SERNR
    INTO TABLE @DATA(LT_ZSDSCMC008).
  IF SY-SUBRC = 0.
    SORT LT_ZSDSCMC008 BY EQUNR MATNR SERNR.
  ENDIF.


  LOOP AT GT_UPLOAD ASSIGNING FIELD-SYMBOL(<LFS_UPLOAD>).
    CLEAR: LS_RESULT.

    LS_RESULT-EQUNR = <LFS_UPLOAD>-EQUNR.
    LS_RESULT-MATNR = <LFS_UPLOAD>-MATNR.
    LS_RESULT-SERNR = <LFS_UPLOAD>-SERNR.
    LS_RESULT-KUNNR = <LFS_UPLOAD>-KUNNR.
    LS_RESULT-IO    = <LFS_UPLOAD>-IO.

    READ TABLE LT_EQUNR ASSIGNING FIELD-SYMBOL(<LFS_EQUNR>) WITH KEY EQUNR = <LFS_UPLOAD>-EQUNR
                                                                     BINARY SEARCH.
    IF SY-SUBRC <> 0.
      READ TABLE LT_SERNR_MATNR ASSIGNING FIELD-SYMBOL(<LFS_SERNR_MATNR>) WITH KEY SERNR = <LFS_UPLOAD>-SERNR
                                                                                   MATNR = <LFS_UPLOAD>-MATNR
                                                                                   BINARY SEARCH.
      IF SY-SUBRC <> 0.
        LS_RESULT-MESSAGE_ICON = ICON_RED_LIGHT.
        LS_RESULT-MESSAGE_TYPE = 'E'.
        LS_RESULT-MESSAGE      = TEXT-E01. "No material or serial no. in Table EQUI
      ELSE.
        LS_RESULT-EQUNR = <LFS_SERNR_MATNR>-EQUNR.
        LS_RESULT-MESSAGE_ICON = ICON_YELLOW_LIGHT.
        LS_RESULT-MESSAGE_TYPE = 'W'.
      ENDIF.
    ELSE.
      LS_RESULT-MESSAGE_ICON = ICON_GREEN_LIGHT.
      LS_RESULT-MESSAGE_TYPE = 'S'.
    ENDIF.

    READ TABLE LT_ZSDSCMC008 ASSIGNING FIELD-SYMBOL(<LFS_ZSDSCMC008>) WITH KEY EQUNR = <LFS_UPLOAD>-EQUNR
                                                                               MATNR = <LFS_UPLOAD>-MATNR
                                                                               SERNR = <LFS_UPLOAD>-SERNR
                                                                               BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_RESULT-AENAM = SY-UNAME.
      LS_RESULT-AEDAT = SY-DATUM.
      LS_RESULT-AEZET = SY-UZEIT.
      LS_RESULT-UPDATE_FLAG = 'U'. "Update
    ELSE.
      LS_RESULT-ERNAM = SY-UNAME.
      LS_RESULT-ERDAT = SY-DATUM.
      LS_RESULT-ERZET = SY-UZEIT.
      LS_RESULT-UPDATE_FLAG = 'I'. "Insert
    ENDIF.

    APPEND LS_RESULT TO CT_RESULT.
  ENDLOOP.

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
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
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
    LF_TEXT TYPE  TEXT50,
    LS_FCAT TYPE  LVC_S_FCAT.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.
  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME = 'MESSAGE_ICON'.
  LS_FCAT-SCRTEXT_L = ''.
  APPEND LS_FCAT TO CT_FIELDCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME = 'MESSAGE_TYPE'.
  LS_FCAT-SCRTEXT_L = 'Message Type'(T01).
  APPEND LS_FCAT TO CT_FIELDCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME = 'MESSAGE'.
  LS_FCAT-SCRTEXT_L = 'Message Text'(T02).
  APPEND LS_FCAT TO CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'EQUNR'.
      WHEN 'MATNR'.
      WHEN 'SERNR'.
      WHEN 'KUNNR'.
      WHEN 'IO'.

      WHEN 'ERNAM'.
        LF_TEXT                  = TEXT-C01.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
      WHEN 'ERDAT'.
        LF_TEXT                  = TEXT-C02.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
      WHEN 'ERZET'.
        LF_TEXT                  = TEXT-C03.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
      WHEN 'AENAM'.
        LF_TEXT                  = TEXT-C04.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
      WHEN 'AEDAT'.
        LF_TEXT                  = TEXT-C05.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
      WHEN 'AEZET'.
        LF_TEXT                  = TEXT-C06.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
      WHEN 'MESSAGE_ICON'.
      WHEN 'MESSAGE_TYPE'.
        LF_TEXT                  = TEXT-C07.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
      WHEN 'MESSAGE'.
        LF_TEXT                  = TEXT-C08.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
      WHEN OTHERS.
        <L_FIELDCAT>-TECH = GC_TRUE.

    ENDCASE.

  ENDLOOP.

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
* Text-h01 : Report:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = SY-TITLE.
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


* Text-h01 : Report:
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    SY-TITLE NO-GAP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_READ_INPUT_FILE
*----------------------------------------------------------------------*
*  Read Input File
*----------------------------------------------------------------------*
FORM F_READ_INPUT_FILE  USING  UF_IFILE TYPE STRING.
  DATA: LT_RAW_DATA TYPE TABLE OF CHAR200.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME        = UF_IFILE
      FILETYPE        = 'ASC'
    TABLES
      DATA_TAB        = LT_RAW_DATA
    EXCEPTIONS
      FILE_OPEN_ERROR = 1
      FILE_READ_ERROR = 2
      OTHERS          = 3.

  IF SY-SUBRC <> 0.
    MESSAGE 'Error: Unable to upload file.'(E04) TYPE 'E'.
  ENDIF.

* Convert to internal table
  LOOP AT LT_RAW_DATA INTO DATA(LS_RAW_DATA) ##INTO_OK.
    CHECK LS_RAW_DATA IS NOT INITIAL.
    SPLIT LS_RAW_DATA AT GC_TAB INTO GS_UPLOAD-EQUNR
                                     GS_UPLOAD-MATNR
                                     GS_UPLOAD-SERNR
                                     GS_UPLOAD-KUNNR
                                     GS_UPLOAD-IO
                                     GS_UPLOAD-ERNAM
                                     GS_UPLOAD-ERDAT
                                     GS_UPLOAD-ERZET
                                     GS_UPLOAD-AENAM
                                     GS_UPLOAD-AEDAT
                                     GS_UPLOAD-AEZET.
    GS_UPLOAD-EQUNR = |{ GS_UPLOAD-EQUNR ALPHA = IN }|.
    GS_UPLOAD-MATNR = |{ GS_UPLOAD-MATNR ALPHA = IN }|.
    GS_UPLOAD-SERNR = |{ GS_UPLOAD-SERNR ALPHA = IN }|.
    GS_UPLOAD-KUNNR = |{ GS_UPLOAD-KUNNR ALPHA = IN }|.
    GS_UPLOAD-IO = |{ GS_UPLOAD-IO ALPHA = IN }|.

    APPEND GS_UPLOAD TO GT_UPLOAD.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_database
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM UPDATE_DATABASE  USING CT_RESULT TYPE TT_RESULT.

  LOOP AT CT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_RESULT>) WHERE MESSAGE_TYPE <> 'E'.
    IF <LFS_RESULT>-UPDATE_FLAG = 'U'.
      UPDATE ZSDSCMC008 FROM <LFS_RESULT>.
      IF SY-SUBRC = 0.
        <LFS_RESULT>-MESSAGE_ICON = ICON_GREEN_LIGHT.
        <LFS_RESULT>-MESSAGE_TYPE = 'S'.
        <LFS_RESULT>-MESSAGE  = TEXT-S01.
      ELSE.
        <LFS_RESULT>-MESSAGE_ICON = ICON_RED_LIGHT.
        <LFS_RESULT>-MESSAGE_TYPE = 'E'.
        <LFS_RESULT>-MESSAGE  = TEXT-E02.
      ENDIF.
    ELSEIF <LFS_RESULT>-UPDATE_FLAG = 'I'.
      INSERT ZSDSCMC008 FROM <LFS_RESULT>.
      IF SY-SUBRC = 0.
        <LFS_RESULT>-MESSAGE_ICON = ICON_GREEN_LIGHT.
        <LFS_RESULT>-MESSAGE_TYPE = 'S'.
        <LFS_RESULT>-MESSAGE  = TEXT-S02.
      ELSE.
        <LFS_RESULT>-MESSAGE_ICON = ICON_RED_LIGHT.
        <LFS_RESULT>-MESSAGE_TYPE = 'E'.
        <LFS_RESULT>-MESSAGE  = TEXT-E03.
      ENDIF.
    ENDIF.
  ENDLOOP.


ENDFORM.
