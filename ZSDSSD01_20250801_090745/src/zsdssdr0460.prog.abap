*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0460
*  Creation Date      : 22.01.2025
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : 420000097
*  Description        : Change schedule line cat. to Y1
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
*  04.03.2025  420000465  Zulkiff B.  Change schedule line cat. to Yx
*-----------------------------------------------------------------------
REPORT ZSDSSDR0460.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
*TABLES:
*  SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TS_RESULT  TYPE  ZSDSSDS122.
TYPES: TT_RESULT  TYPE  STANDARD TABLE OF TS_RESULT.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSSD038'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT TYPE  TT_RESULT                           ##NEEDED,
  GT_DATA   TYPE  TT_RESULT                           ##NEEDED.

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
  GF_PARAM           TYPE  CHAR10                              ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS122'.

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
  PARAMETERS: P_IFILE TYPE  STRING LOWER CASE,
              P_TEST  TYPE CHAR1 AS CHECKBOX DEFAULT 'X'.
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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_IFILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_IFILE.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_OFILE.
** List Output File
*  PERFORM F_LIST_OFILE CHANGING P_OFILE.

AT SELECTION-SCREEN.
*  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
*    PERFORM F_VALIDATE_SELECTION_SCREEN.
*  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Example Excel file reading
  IF P_IFILE IS NOT INITIAL.
    PERFORM F_READ_INPUT_FILE USING P_IFILE.
    IF GT_DATA IS NOT INITIAL.
*   Validate and Update Data
      PERFORM F_VALIDATE_UPDATE_DATA CHANGING GT_RESULT.
    ENDIF.
    IF GT_RESULT IS INITIAL.
*     Message: No data found.
      MESSAGE S001(ZSDSCA01).
      RETURN.
    ENDIF.
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
*  Form f_validate_update_data
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_VALIDATE_UPDATE_DATA  CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LS_RESULT   TYPE  TS_RESULT.

  DATA:
    LT_RETURN          TYPE STANDARD TABLE OF BAPIRET2,
    LT_SCHEDULE_LINES  TYPE STANDARD TABLE OF BAPISCHDL,
    LT_SCHEDULE_LINESX TYPE STANDARD TABLE OF BAPISCHDLX,
    LS_HEADERX         TYPE BAPISDH1X.

  SELECT VBELN,                                 "#EC CI_FAE_NO_LINES_OK
         POSNR,
         ETENR,
         EDATU,
         ETTYP
    FROM VBEP
     FOR ALL ENTRIES IN @GT_DATA
   WHERE VBELN = @GT_DATA-VBELN
     AND POSNR = @GT_DATA-POSNR
     AND ETENR = @GT_DATA-ETENR
    INTO TABLE @DATA(LT_VBEP).
  IF SY-SUBRC = 0.
    SORT LT_VBEP BY VBELN POSNR ETTYP.
  ENDIF.

  SELECT VBELN,                                 "#EC CI_FAE_NO_LINES_OK
         POSNR,
         RFSTA
    FROM VBAP
     FOR ALL ENTRIES IN @GT_DATA
   WHERE VBELN = @GT_DATA-VBELN
     AND POSNR = @GT_DATA-POSNR
    INTO TABLE @DATA(LT_VBAP).
  IF SY-SUBRC = 0.
    SORT LT_VBAP BY VBELN POSNR.
  ENDIF.


  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
    CLEAR:LS_RESULT,LT_SCHEDULE_LINES,LT_SCHEDULE_LINESX,LS_HEADERX,LT_RETURN.
    LS_RESULT = <LFS_DATA>.

    READ TABLE LT_VBAP ASSIGNING FIELD-SYMBOL(<LFS_VBAP>) WITH KEY VBELN = <LFS_DATA>-VBELN
                                                                   POSNR = <LFS_DATA>-POSNR
                                                                   BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF <LFS_VBAP>-RFSTA = SPACE OR <LFS_VBAP>-RFSTA = 'A' OR <LFS_VBAP>-RFSTA = 'B'.
        IF P_TEST = 'X'.
          LS_RESULT-REMARK =  TEXT-S03. "TheSchedule Line Category can be changed
        ENDIF.
      ELSE.
        LS_RESULT-REMARK = TEXT-E01. "Status is not equal to space or A
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND VALUE #( ITM_NUMBER = <LFS_DATA>-POSNR
                    SCHED_LINE = <LFS_DATA>-ETENR
                    REQ_DATE   = <LFS_DATA>-EDATU
                    SCHED_TYPE = <LFS_DATA>-ETTYP )
                    TO LT_SCHEDULE_LINES.
    APPEND VALUE #( ITM_NUMBER = <LFS_DATA>-POSNR
                    SCHED_LINE = <LFS_DATA>-ETENR
*                    REQ_DATE   = ABAP_TRUE
                    UPDATEFLAG = 'U'
                    SCHED_TYPE = ABAP_TRUE )
                    TO LT_SCHEDULE_LINESX.

    LS_HEADERX-UPDATEFLAG  = 'U'.

    CALL FUNCTION 'BAPI_CUSTOMERQUOTATION_CHANGE'
      EXPORTING
        SALESDOCUMENT        = <LFS_DATA>-VBELN
*       QUOTATION_HEADER_IN  = LS_HEADER
        QUOTATION_HEADER_INX = LS_HEADERX
*       LOGIC_SWITCH         = LOGIC_SWITCH
        SIMULATION           = P_TEST
      TABLES
        RETURN               = LT_RETURN
*       QUOTATION_ITEM_IN    = LT_ITEM
*       QUOTATION_ITEM_INX   = LT_ITEMX
        SCHEDULE_LINES       = LT_SCHEDULE_LINES
        SCHEDULE_LINESX      = LT_SCHEDULE_LINESX.
    IF LINE_EXISTS( LT_RETURN[ TYPE = 'A' ] ) OR
       LINE_EXISTS( LT_RETURN[ TYPE = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      READ TABLE LT_RETURN INTO DATA(LS_RETURN) WITH KEY TYPE = 'E'.
      IF SY-SUBRC = 0.
        LS_RESULT-REMARK = LS_RETURN-MESSAGE.
      ENDIF.

    ELSE.

      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE   = 'S'
                                                   ID     = 'V1'
                                                   NUMBER = '311'.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'
*         IMPORTING
*           RETURN        =
          .

        LS_RESULT-REMARK = LS_RETURN-MESSAGE.
      ENDIF.
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
      WHEN 'REMARK'.
        LF_TEXT                  = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.

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
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'VBELN',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'POSNR',
    LC_SORT3 TYPE  LVC_S_SORT-FIELDNAME VALUE 'ETENR'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

* Sort by CARRID
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by CONNID
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = GC_TRUE.
  APPEND LS_SORT TO CT_SORT.

* Sort by FLDATE
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 3.
  LS_SORT-FIELDNAME = LC_SORT3.
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

  DATA:
    LT_DATA     TYPE  ZCL_SDSCA_UTILITIES=>TT_XLSX_DATA,
    LS_RAW_DATA TYPE  TS_RESULT.

  DATA:
    LREF_STRUC  TYPE REF TO CL_ABAP_STRUCTDESCR.


  CALL METHOD ZCL_SDSCA_UTILITIES=>READ_XLSX_INTO_ITAB
    EXPORTING
      IF_FILENAME                 = UF_IFILE
      IF_READ_ACTIVE_WORKSHEET    = 'X'
*     IF_XSTRING                  =
*     IT_WORKSHEET                =
    IMPORTING
      ET_XLSX_DATA                = LT_DATA
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
  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_SHEET>).

    ASSIGN <L_SHEET>-DATA->* TO FIELD-SYMBOL(<L_SHEET_DATA>).

*   -----------
*   Rows
*   -----------
    LOOP AT <L_SHEET_DATA> ASSIGNING FIELD-SYMBOL(<L_ROW>).

      LREF_STRUC ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( P_DATA = <L_ROW> ).
*     -----------
*     Columns
*     -----------
      CLEAR: LS_RAW_DATA.
      LOOP AT LREF_STRUC->COMPONENTS ASSIGNING FIELD-SYMBOL(<L_COMP>).

        ASSIGN <L_ROW>-(<L_COMP>-NAME) TO FIELD-SYMBOL(<L_FIELD>).
        IF SY-SUBRC NE 0.
          CONTINUE.
        ENDIF.

        CASE <L_COMP>-NAME.
          WHEN 'A'.
            LS_RAW_DATA-VBELN = |{ <L_FIELD> ALPHA = IN }|.
          WHEN 'B'.
            LS_RAW_DATA-POSNR = <L_FIELD>.
          WHEN 'C'.
            LS_RAW_DATA-ETENR = <L_FIELD>.
          WHEN 'D'.
            IF <L_FIELD> IS NOT INITIAL.
              LS_RAW_DATA-EDATU = |{ <L_FIELD>+6(4) }{ <L_FIELD>+3(2) }{ <L_FIELD>+0(2) }|. "dd.mm.yyyy -> yyyymmdd
            ENDIF.
          WHEN 'E'.
            LS_RAW_DATA-ETTYP = <L_FIELD>.
        ENDCASE.

      ENDLOOP.

      APPEND LS_RAW_DATA TO GT_DATA.

    ENDLOOP.

  ENDLOOP.

  DELETE GT_DATA WHERE VBELN IS INITIAL.

ENDFORM.
