*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0150
*  Creation Date      : 03.04.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : ZMMR013
*  Description        : Serial number report with form printing
*  Purpose            : Serial number report and form printing
*  Copied from        : ZR_MM_EXPORT_SERIAL
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0150.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS,
  OBJK,
  SER01,
  LIKP.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSMMS010.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.

* Shared Declaration with Form
INCLUDE ZSDSMMR0150_TOP ##INCL_OK.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSMM004'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT        TYPE  TT_RESULT                            ##NEEDED,
  GT_HEADER        TYPE  TT_HEADER                            ##NEEDED,
  GT_HEADER_DETAIL TYPE  TT_HEADER_DETAIL                     ##NEEDED,
  GT_DETAIL        TYPE  TT_DETAIL                            ##NEEDED,
  GT_COUNT_PAGE    TYPE  TT_COUNT_PAGE                        ##NEEDED.

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
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS010'.

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
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS:
    S_SERNR FOR OBJK-SERNR,
    S_MATNR FOR OBJK-MATNR,
    S_LIENR FOR SER01-LIEF_NR,
    S_LFDAT FOR LIKP-LFDAT.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

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
* Get data
  PERFORM F_GET_DATA CHANGING GT_RESULT.
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
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN ##NEEDED.

* Validate Selection Screen here...

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_DATA
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_GET_DATA  CHANGING CT_RESULT TYPE TT_RESULT.

* Initialize Output
  CLEAR: CT_RESULT.

* Get Serial Data
  SELECT DISTINCT EKES~XBLNR,
                  EKPO~EBELN,
                  SER01~LIEF_NR,
                  LIKP~LFDAT,
                  EKPO~MATNR,
                  OBJK~SERNR,
                  EKPO~NETPR,
                  EKKO~WAERS,
                  LIKP~ZTRUCKNO
    FROM OBJK
           INNER JOIN SER01
             ON  OBJK~OBKNR EQ SER01~OBKNR
           INNER JOIN EKES
             ON  SER01~LIEF_NR EQ EKES~VBELN
             AND SER01~POSNR EQ EKES~VBELP
           INNER JOIN EKPO
             ON  EKES~EBELN EQ EKPO~EBELN
             AND EKES~EBELP EQ EKPO~EBELP
             AND OBJK~MATNR EQ EKPO~MATNR
           INNER JOIN EKKO
             ON  EKKO~EBELN EQ EKPO~EBELN
           INNER JOIN LIKP
             ON  SER01~LIEF_NR EQ LIKP~VBELN
    WHERE OBJK~SERNR    IN @S_SERNR
      AND OBJK~MATNR    IN @S_MATNR
      AND TASER         EQ 'SER01'
      AND SER01~LIEF_NR IN @S_LIENR
      AND LIKP~LFDAT    IN @S_LFDAT
    ORDER BY LIKP~LFDAT ASCENDING,
             EKPO~MATNR ASCENDING
    INTO TABLE @CT_RESULT.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

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
    LF_TEXT  TYPE  STRING.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'XBLNR'.
*       Text-c01 : DIT INV.
        LF_TEXT                  = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EBELN'.
      WHEN 'LIEF_NR'.
*       Text-c02 : Inbound No.
        LF_TEXT                  = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'LFDAT'.
      WHEN 'MATNR'.
      WHEN 'SERNR'.
      WHEN 'NETPR'.
      WHEN 'WAERS'.
        <L_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'ZTRUCKNO'.

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
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.

  CASE UF_UCOMM.
    WHEN 'PRINT'.
      PERFORM F_PREPARE_PRINT_DATA  USING  GT_RESULT
                                  CHANGING GT_HEADER
                                           GT_DETAIL
                                           GT_HEADER_DETAIL
                                           GT_COUNT_PAGE.

      PERFORM F_PRINT_FORM CHANGING GT_HEADER.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PRINT_FORM
*----------------------------------------------------------------------*
*  Print Form
*----------------------------------------------------------------------*
FORM F_PRINT_FORM CHANGING CT_HEADER TYPE TT_HEADER.

  CONSTANTS:
    LC_SMART_FORMS TYPE TDSFNAME VALUE 'ZSDSMM005'.

  DATA:
    LT_HEADER  TYPE  TT_HEADER.

  DATA:
    LS_COUNT   TYPE  SSFCTRLOP.

  DATA:
    LF_TOTAL_LINE TYPE  I,
    LF_FM_NAME    TYPE RS38L_FNAM,
    LF_TABIX      TYPE SY-TABIX.


* Backup Data
  LT_HEADER = CT_HEADER.

  LF_TOTAL_LINE = LINES( LT_HEADER ).

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LC_SMART_FORMS
    IMPORTING
      FM_NAME            = LF_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ENDIF.

  LOOP AT LT_HEADER ASSIGNING FIELD-SYMBOL(<L_HEADER>).
    LF_TABIX = SY-TABIX.

    CLEAR CT_HEADER[].
    APPEND <L_HEADER> TO CT_HEADER.

    IF LF_TOTAL_LINE GT 1.
      IF     LF_TABIX = 1.               "FISRT CALL
        LS_COUNT-NO_OPEN   = SPACE .
        LS_COUNT-NO_CLOSE  = 'X' .
      ELSEIF LF_TABIX   = LF_TOTAL_LINE.  "LAST CALL
        LS_COUNT-NO_OPEN   = 'X' .
        LS_COUNT-NO_CLOSE  = SPACE .
      ELSE.                          "OTHER CALLS
        LS_COUNT-NO_OPEN   = 'X' .
        LS_COUNT-NO_CLOSE  = 'X' .
      ENDIF.
    ENDIF.

    CALL FUNCTION LF_FM_NAME
      EXPORTING
        CONTROL_PARAMETERS = LS_COUNT
        USER_SETTINGS      = 'X'
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.
    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDLOOP.

* Assign Data Back
  CT_HEADER = LT_HEADER.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PRINT_FORM
*----------------------------------------------------------------------*
*  Print Form
*----------------------------------------------------------------------*
FORM F_GET_PRINT_DATA CHANGING CT_HEADER         TYPE TT_HEADER
                               CT_DETAIL         TYPE TT_DETAIL
                               CT_HEADER_DETAIL  TYPE TT_HEADER_DETAIL
                               CT_COUNT_PAGE     TYPE TT_COUNT_PAGE  ##CALLED.

* Assign Print data
  CT_HEADER[]        = GT_HEADER[].
  CT_DETAIL[]        = GT_DETAIL[].
  CT_HEADER_DETAIL[] = GT_HEADER_DETAIL[].
  CT_COUNT_PAGE[]    = GT_COUNT_PAGE[].

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_PREPARE_PRINT_DATA
*----------------------------------------------------------------------*
*  Prepare Print Data
*----------------------------------------------------------------------*
FORM F_PREPARE_PRINT_DATA  USING  UT_RESULT        TYPE TT_RESULT
                         CHANGING CT_HEADER        TYPE TT_HEADER
                                  CT_DETAIL        TYPE TT_DETAIL
                                  CT_HEADER_DETAIL TYPE TT_HEADER_DETAIL
                                  CT_COUNT_PAGE    TYPE TT_COUNT_PAGE.

  DATA:
    LT_DETAIL  TYPE  TT_DETAIL.

  DATA:
    LS_HEADER        TYPE  TS_HEADER,
    LS_DETAIL        TYPE  TS_DETAIL,
    LS_HEADER_DETAIL TYPE  TS_HEADER_DETAIL,
    LS_COUNT_PAGE    TYPE  TS_COUNT_PAGE.

  DATA:
    LF_COUNT      TYPE I,
    LF_COUNT_PAGE TYPE I.


* Initialize Output
  CLEAR: CT_HEADER,
         CT_DETAIL,
         CT_HEADER_DETAIL,
         CT_COUNT_PAGE.

  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>).

    CLEAR LS_HEADER.
    MOVE-CORRESPONDING <L_RESULT> TO LS_HEADER.
    LS_HEADER-TOTAL = 1.
    COLLECT LS_HEADER INTO CT_HEADER.

    CLEAR LS_HEADER_DETAIL.
    MOVE-CORRESPONDING <L_RESULT> TO LS_HEADER_DETAIL.
    LS_HEADER_DETAIL-TOTAL = 1.
    COLLECT LS_HEADER_DETAIL INTO CT_HEADER_DETAIL.

    CLEAR LS_DETAIL.
    MOVE-CORRESPONDING <L_RESULT> TO LS_DETAIL.
    APPEND LS_DETAIL TO LT_DETAIL.

  ENDLOOP.

  SORT LT_DETAIL BY XBLNR LIEF_NR EBELN MATNR SERNR.
  SORT CT_HEADER_DETAIL BY XBLNR LIEF_NR EBELN MATNR.
  SORT CT_HEADER BY XBLNR LIEF_NR.

  LOOP AT CT_HEADER_DETAIL ASSIGNING FIELD-SYMBOL(<L_HEADER_DETAIL>).
    CLEAR LS_COUNT_PAGE.
    MOVE-CORRESPONDING <L_HEADER_DETAIL> TO LS_COUNT_PAGE.
    LS_COUNT_PAGE-TOTAL = 1.
    COLLECT LS_COUNT_PAGE INTO CT_COUNT_PAGE.
  ENDLOOP.

  LOOP AT CT_HEADER_DETAIL ASSIGNING <L_HEADER_DETAIL>.

    CLEAR : LF_COUNT,
            LF_COUNT_PAGE.

    LOOP AT LT_DETAIL ASSIGNING FIELD-SYMBOL(<L_DETAIL>)
                      WHERE XBLNR   EQ <L_HEADER_DETAIL>-XBLNR
                        AND LIEF_NR EQ <L_HEADER_DETAIL>-LIEF_NR
                        AND EBELN   EQ <L_HEADER_DETAIL>-EBELN
                        AND MATNR   EQ <L_HEADER_DETAIL>-MATNR.

      LF_COUNT = LF_COUNT + 1.

      IF LF_COUNT EQ 11 ##NUMBER_OK.
        LF_COUNT = 1.
      ENDIF.

      LS_DETAIL-XBLNR   = <L_DETAIL>-XBLNR.
      LS_DETAIL-LIEF_NR = <L_DETAIL>-LIEF_NR.
      LS_DETAIL-EBELN   = <L_DETAIL>-EBELN.
      LS_DETAIL-MATNR   = <L_DETAIL>-MATNR.

      IF     LF_COUNT = 1.
        LS_DETAIL-SERNR = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 2.
        LS_DETAIL-SER02 = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 3.
        LS_DETAIL-SER03 = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 4.
        LS_DETAIL-SER04 = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 5.
        LS_DETAIL-SER05 = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 6.
        LS_DETAIL-SER06 = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 7.
        LS_DETAIL-SER07 = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 8.
        LS_DETAIL-SER08 = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 9.
        LS_DETAIL-SER09 = <L_DETAIL>-SERNR.
      ELSEIF LF_COUNT = 10.
        LS_DETAIL-SER10 = <L_DETAIL>-SERNR.
        LF_COUNT_PAGE = LF_COUNT_PAGE + 1.
        APPEND  LS_DETAIL TO CT_DETAIL.
        CLEAR LS_DETAIL.
      ENDIF.
    ENDLOOP.

    IF LF_COUNT NE 10.
      LF_COUNT_PAGE = LF_COUNT_PAGE + 1.
      APPEND  LS_DETAIL TO CT_DETAIL.
    ENDIF.

    READ TABLE CT_COUNT_PAGE ASSIGNING FIELD-SYMBOL(<L_COUNT_PAGE>)
                             WITH KEY XBLNR   = <L_HEADER_DETAIL>-XBLNR
                                      LIEF_NR = <L_HEADER_DETAIL>-LIEF_NR
                                      EBELN   = <L_HEADER_DETAIL>-EBELN.
    IF SY-SUBRC = 0.
      <L_COUNT_PAGE>-TOTAL = <L_COUNT_PAGE>-TOTAL + LF_COUNT_PAGE.
    ENDIF.

  ENDLOOP.

ENDFORM.
