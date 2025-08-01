*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0120
*  Creation Date      : 02.04.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : ZMME017
*  Description        : Program to upload material serial from excel file
*  Purpose            : To upload material serial
*  Copied from        : ZMM_CREATE_SERIAL
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0120.

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
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSMMS009.
TYPES:   SELCT TYPE CHAR1,
       END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_DATA,
         MATNR TYPE  STRING,
         SERNR TYPE  STRING,
       END OF TS_DATA.
TYPES: TT_DATA  TYPE  STANDARD TABLE OF TS_DATA.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSMM003',
  GC_IQ01  TYPE  SY-TCODE  VALUE 'IQ01',

  GC_RED   TYPE  ICON-ID   VALUE '@0A@', "Red light
  GC_GREEN TYPE  ICON-ID   VALUE '@08@'. "Green light

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT  TYPE  TT_RESULT                                   ##NEEDED.

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

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS009'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 0,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 100.

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
* Text-s01: Selection Option
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    P_IFILE  TYPE  STRING LOWER CASE OBLIGATORY.
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

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Process data
  PERFORM F_PROCESS_DATA  USING  P_IFILE
                        CHANGING GT_RESULT.
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
FORM F_VALIDATE_SELECTION_SCREEN ##NEEDED.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD GC_IQ01.
  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE E172(00) WITH GC_IQ01.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PROCESS_DATA
*----------------------------------------------------------------------*
*  Process Data
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA  USING  UF_IFILE  TYPE STRING
                   CHANGING PT_RESULT TYPE TT_RESULT.

  DATA:
    LT_DATA  TYPE  TT_DATA.


* Initialize Output
  CLEAR: PT_RESULT.

* Read Excel File to Internal Table
  PERFORM F_READ_INPUT_FILE  USING  UF_IFILE
                           CHANGING LT_DATA.
  IF LT_DATA IS INITIAL.
    RETURN.
  ENDIF.

* Validate Data and Assign Result
  PERFORM F_VALIDATE_DATA  USING  LT_DATA
                         CHANGING PT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_READ_INPUT_FILE
*----------------------------------------------------------------------*
*  Read Input File into Internal Table
*----------------------------------------------------------------------*
FORM F_READ_INPUT_FILE  USING  UF_IFILE  TYPE  STRING
                      CHANGING PT_DATA TYPE TT_DATA.

  DATA:
    LT_XLSX  TYPE  ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA.

  DATA:
    LREF_STRUC  TYPE REF TO CL_ABAP_STRUCTDESCR.

  DATA:
    LS_DATA  TYPE  TS_DATA.


* Initialize Output
  CLEAR: PT_DATA.

* Read Excel File
  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = UF_IFILE
      IF_READ_ACTIVE_WORKSHEET    = 'X'
*     IF_XSTRING                  =
*     IT_WORKSHEET                =
    IMPORTING
      ET_XLSX_DATA                = LT_XLSX
    EXCEPTIONS
      MISSING_FILENAME_OR_XSTRING = 1
      ERROR_READ_FILENAME         = 2
      NO_DATA_FOUND               = 3
      OTHERS                      = 4.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* -----------
* Worksheets
* -----------
  LOOP AT LT_XLSX ASSIGNING FIELD-SYMBOL(<L_SHEET>).

    ASSIGN <L_SHEET>-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).

*   -----------
*   Rows
*   -----------
    CLEAR LS_DATA.
    LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
*     -----------
*     Columns
*     -----------
      LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

        ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
        IF SY-SUBRC NE 0.
          CONTINUE.
        ENDIF.

*       Processing field
        CASE <L_COMP>-NAME.

*         ---------------
*         Material Code
*         ---------------
          WHEN 'A'.
            LS_DATA-MATNR = <L_FIELD>.

*         ---------------
*         Serial Number
*         ---------------
          WHEN 'B'.
            LS_DATA-SERNR = <L_FIELD>.

        ENDCASE.

      ENDLOOP.

      INSERT LS_DATA INTO TABLE PT_DATA.

    ENDLOOP.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_data
*----------------------------------------------------------------------*
*  Validate Data
*----------------------------------------------------------------------*
FORM F_VALIDATE_DATA  USING  PT_DATA  TYPE  TT_DATA
                    CHANGING PT_RESULT TYPE  TT_RESULT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.


* Initialize Output
  CLEAR: PT_RESULT.

  LOOP AT PT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).

    CLEAR LS_RESULT.

*   Validate Material
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT        = <L_DATA>-MATNR
      IMPORTING
        OUTPUT       = LS_RESULT-MATNR
      EXCEPTIONS
        LENGTH_ERROR = 1
        OTHERS       = 2.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE MATNR
        FROM MARA
       WHERE MATNR EQ @LS_RESULT-MATNR
        INTO @LS_RESULT-MATNR.
    ENDIF.
*   Error
    IF SY-SUBRC NE 0.
      LS_RESULT-MSGTY = 'E'.
      LS_RESULT-STATUS = GC_RED.
*     Text-t01: Material
*     Text-e01: does not exist
      CONCATENATE TEXT-T01 <L_DATA>-MATNR TEXT-E01
             INTO LS_RESULT-MESSG
        SEPARATED BY SPACE.
    ENDIF.

*   Validate Serial
    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        INPUT  = <L_DATA>-SERNR
      IMPORTING
        OUTPUT = LS_RESULT-SERNR.

    SELECT SERNR
      FROM EQUI
      WHERE SERNR EQ @LS_RESULT-SERNR
        AND MATNR EQ @LS_RESULT-MATNR
      ORDER BY PRIMARY KEY
      INTO @LS_RESULT-SERNR
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC EQ 0 .
      LS_RESULT-MSGTY = 'E'.
      LS_RESULT-STATUS = GC_RED.
*     Text-t03: Serial number
*     Text-e03: already exists for material
      CONCATENATE TEXT-T03 <L_DATA>-SERNR
                  TEXT-E03 LS_RESULT-MATNR
             INTO LS_RESULT-MESSG
        SEPARATED BY SPACE.
    ENDIF.

    IF LS_RESULT-MSGTY IS INITIAL.
      LS_RESULT-MSGTY = 'S'.
      LS_RESULT-STATUS = GC_GREEN.
    ENDIF.

    INSERT LS_RESULT INTO TABLE PT_RESULT.

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
  GF_ALV_HEADER_1 = SPACE.
** Enable Soft Refresh only in display mode
*  GF_SOFT_REFRESH_1 = GC_TRUE.
* No auto refresh in edit mode
  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

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
  CS_LAYOUT-SEL_MODE   = 'A'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.
*  CS_LAYOUT-BOX_FNAME  = 'SELCT'.

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

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'MATNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 18 ##NUMBER_OK.
      WHEN 'SERNR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 18 ##NUMBER_OK.
      WHEN 'STATUS'.
        <L_FIELDCAT>-OUTPUTLEN = 6.
      WHEN 'MSGTY'.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'MESSG'.
        <L_FIELDCAT>-OUTPUTLEN = 70 ##NUMBER_OK.
**       Text-c01 : Code
*        LF_TEXT                  = TEXT-C01.
*        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
*        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
*        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
*        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
*        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
*        <L_FIELDCAT>-OUTPUTLEN = 10.
*
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

* Initialize Output
  CLEAR: CT_SORT.

** Sort by CARRID
*  CLEAR LS_SORT.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-FIELDNAME = LC_SORT1.
*  LS_SORT-UP        = GC_TRUE.
*  LS_SORT-SUBTOT    = SPACE.
*  APPEND LS_SORT TO CT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UREF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

  DATA:
    LS_BUTTON TYPE STB_BUTTON.


* Add Select All Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = '&ALL'.
  LS_BUTTON-ICON     = '@4B@'.
* Text-a01: Select All
  LS_BUTTON-QUICKINFO = TEXT-A01.
  INSERT LS_BUTTON INTO UREF_OBJECT->MT_TOOLBAR
                   INDEX 3.

* Add De Select All Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = '&SAL'.
  LS_BUTTON-ICON     = '@4D@'.
* Text-a02: De Select All
  LS_BUTTON-QUICKINFO = TEXT-A02.
  INSERT LS_BUTTON INTO UREF_OBJECT->MT_TOOLBAR
                   INDEX 4.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.

  CASE UF_UCOMM.
    WHEN 'UPLOAD'.
      PERFORM F_UPLOAD_SERIAL.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_upload_serial
*----------------------------------------------------------------------*
*  Upload Serial
*----------------------------------------------------------------------*
FORM F_UPLOAD_SERIAL .

  DATA:
    LT_ROID TYPE LVC_T_ROID,
    LT_MSG  TYPE TAB_BDCMSGCOLL.

  DATA:
    LS_RESULT TYPE  TS_RESULT,
    LS_STBL   TYPE LVC_S_STBL.

  DATA:
    LF_ERROR  TYPE  FLAG.


* Clear Selection
  MODIFY GT_RESULT FROM LS_RESULT TRANSPORTING SELCT
                   WHERE SELCT EQ GC_TRUE.

* Get Selected List
  CALL METHOD GREF_GRID_1->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = LT_ROID.

* Mark Selection
  CLEAR LF_ERROR.
  LOOP AT LT_ROID ASSIGNING FIELD-SYMBOL(<L_ROID>).
    READ TABLE GT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                         INDEX <L_ROID>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    <L_RESULT>-SELCT = GC_TRUE.
    IF <L_RESULT>-MSGTY EQ 'E'.
      LF_ERROR = GC_TRUE.
    ENDIF.
  ENDLOOP.

  IF LF_ERROR EQ GC_TRUE.
*   Text-t04: Error item will not be upload.
    MESSAGE I000(38) WITH TEXT-T04.
  ENDIF.

  LOOP AT GT_RESULT ASSIGNING <L_RESULT>
                    WHERE SELCT EQ GC_TRUE
                      AND MSGTY EQ 'S'.

*   Create Serial
    PERFORM F_CALL_IQ01  USING  <L_RESULT>
                       CHANGING LT_MSG.

*   Update Status and Message
    READ TABLE LT_MSG ASSIGNING FIELD-SYMBOL(<L_MSG>)
                      WITH KEY MSGTYP = 'S'.
    IF SY-SUBRC EQ 0.
      <L_RESULT>-MSGTY  = 'S'.
      <L_RESULT>-STATUS = GC_GREEN.

    ELSE.
      LOOP AT LT_MSG ASSIGNING <L_MSG>
                     WHERE MSGTYP EQ 'A'
                        OR MSGTYP EQ 'E'.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        <L_RESULT>-MSGTY  = 'E'.
        <L_RESULT>-STATUS = GC_RED.
      ENDIF.
    ENDIF.

    MESSAGE ID <L_MSG>-MSGID TYPE <L_MSG>-MSGTYP
        NUMBER <L_MSG>-MSGNR
          WITH <L_MSG>-MSGV1
               <L_MSG>-MSGV2
               <L_MSG>-MSGV3
               <L_MSG>-MSGV4
          INTO <L_RESULT>-MESSG.

  ENDLOOP.

  LS_STBL-ROW = GC_TRUE.
  LS_STBL-COL = GC_TRUE.

* Refresh Display
  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STBL
      I_SOFT_REFRESH = 'X'.

* Set Selected List
  CALL METHOD GREF_GRID_1->SET_SELECTED_ROWS
    EXPORTING
      IT_ROW_NO = LT_ROID.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_call_iq01
*----------------------------------------------------------------------*
*  Create Serial Using tcode IQ01
*----------------------------------------------------------------------*
FORM F_CALL_IQ01  USING  US_RESULT TYPE TS_RESULT
                CHANGING CT_MSG TYPE TAB_BDCMSGCOLL.

  CONSTANTS:
    LC_EQTYP  TYPE  EQTYP VALUE 'M'.

  DATA:
    LT_BDCDATA TYPE STANDARD TABLE OF BDCDATA.

  DATA:
    LF_MODE TYPE C VALUE 'N',
    LF_UPD  TYPE C VALUE 'S'.

  DEFINE MC_BDC_DYNPRO.
    APPEND VALUE bdcdata( program = &1
                          dynpro  = &2
                          dynbegin = 'X' )
           TO lt_bdcdata.
  END-OF-DEFINITION.

  DEFINE MC_BDC_FIELD.
    APPEND VALUE bdcdata( fnam = &1
                          fval = &2 )
           TO lt_bdcdata.
  END-OF-DEFINITION.

* Initialize Output
  CLEAR: CT_MSG.

  CLEAR: LT_BDCDATA.

* Enter Material and Serial in 1st screen then press enter
  MC_BDC_DYNPRO 'SAPMIEQ0' '1000'.
  MC_BDC_FIELD  'RISA0-MATNR' US_RESULT-MATNR.
  MC_BDC_FIELD  'RISA0-SERNR' US_RESULT-SERNR.
  MC_BDC_FIELD  'RM63E-EQTYP' LC_EQTYP.
  MC_BDC_FIELD  'BDC_OKCODE' '/00'.

* Press save in 2nd Screen
  MC_BDC_DYNPRO 'SAPMIEQ0' '0101'.
  MC_BDC_FIELD  'BDC_OKCODE' '=BU'.

* Call IQ01
  CALL TRANSACTION GC_IQ01 WITHOUT AUTHORITY-CHECK
                           USING LT_BDCDATA
                           UPDATE LF_UPD
                           MODE   LF_MODE
                           MESSAGES INTO CT_MSG.

ENDFORM.
