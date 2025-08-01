*-----------------------------------------------------------------------
*  Program ID         : ZSDSCAR0040
*  Creation Date      : 24.05.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : REST Interface processing report
*  Purpose            : To view log of REST Interfaces from table
*                       ZSDSCAT001
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCAR0040.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  ZSDSCAS007,
  SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE  TYPE  ZSDSCAS007.
TYPES:   REQUEST_JSON  TYPE  ZSDSCAT001-REQUEST_JSON,
         RESPONSE_JSON TYPE  ZSDSCAT001-RESPONSE_JSON,
         HTML_ERROR    TYPE  ZSDSCAT001-HTML_ERROR,
         CELLTAB       TYPE  LVC_T_STYL,
         COLTAB        TYPE  LVC_T_SCOL,
       END OF TS_RESULT.
TYPES: TT_RESULT  TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_SUM,
         TOTAL TYPE  I, "Total Entries
         SUCCS TYPE  I, "Success Entries
         ERROR TYPE  I, "Error Entries
       END OF TS_SUM.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE           TYPE  CHAR1       VALUE 'X',
  GC_TCODE          TYPE  SY-TCODE    VALUE 'ZSDSCA001',

  GC_JSON           TYPE  CHAR1          VALUE '1',
  GC_HTML           TYPE  CHAR1          VALUE '2',
  GC_TEXT           TYPE  CHAR1          VALUE '3',

  GC_STATUS_SUCCESS TYPE  ZSDSDE_REST_STATUS VALUE 'S',

  GC_ICON_REQ       TYPE  ICON-ID VALUE '@77@',
  GC_ICON_RES       TYPE  ICON-ID VALUE '@7B@',
  GC_ICON_ERR       TYPE  ICON-ID VALUE '@DR@'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT  TYPE  TT_RESULT ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_SUM     TYPE  TS_SUM                                     ##NEEDED.

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
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSCAS007'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 24,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 76.

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
* Text-s01: Processing Type
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01 NO INTERVALS.
  PARAMETERS:
    RB_REP TYPE  CHAR1 RADIOBUTTON GROUP G1 DEFAULT 'X'
                         USER-COMMAND DMY,
    RB_DEL TYPE  CHAR1 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B1.
* Text-s02: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
  SELECT-OPTIONS:
    S_INTFNO FOR ZSDSCAS007-INTFNO MODIF ID REP NO INTERVALS
                   MATCHCODE OBJECT ZSDSH_INTFNO,
    S_GJAHR  FOR ZSDSCAS007-GJAHR MODIF ID REP NO INTERVALS,
    S_REQNO  FOR ZSDSCAS007-REQNO MODIF ID REP,
    S_KEY    FOR ZSDSCAS007-KEYDATA MODIF ID REP,
    S_USNAM  FOR ZSDSCAS007-USNAM MODIF ID REP
                   MATCHCODE OBJECT USER_COMP,
    S_STDAT  FOR ZSDSCAS007-STDAT MODIF ID REP,
    S_STTIM  FOR ZSDSCAS007-STTIM MODIF ID REP,
    S_ENDAT  FOR ZSDSCAS007-ENDAT MODIF ID REP,
    S_ENTIM  FOR ZSDSCAS007-ENTIM MODIF ID REP,
    S_HCODE  FOR ZSDSCAS007-HTTP_CODE MODIF ID REP,
    S_HRESN  FOR ZSDSCAS007-HTTP_REASON MODIF ID REP,
    S_STATU  FOR ZSDSCAS007-STATUS MODIF ID REP,
    S_MESSG  FOR ZSDSCAS007-MESSAGE MODIF ID REP.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS:
    P_ROWS  TYPE I DEFAULT 500 MODIF ID REP.
SELECTION-SCREEN END OF BLOCK B2.

* Text-s03: Delete Condition
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.
  SELECT-OPTIONS:
    S_INTFN2  FOR ZSDSCAS007-INTFNO MODIF ID DEL NO INTERVALS,
    S_GJAHR2  FOR ZSDSCAS007-GJAHR MODIF ID DEL NO INTERVALS,
    S_REQNO2  FOR ZSDSCAS007-REQNO MODIF ID DEL.
  PARAMETERS:
    P_DAYS  TYPE  I DEFAULT 90 MODIF ID DEL.
SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Format Selection Screen
  PERFORM F_SELECTION_SCREEN_FORMAT.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CASE GC_TRUE.
    WHEN RB_REP.
      PERFORM F_GET_DATA CHANGING GT_RESULT
                                  GS_SUM.
      IF GT_RESULT IS INITIAL.
*       Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.

    WHEN RB_DEL.
      PERFORM F_DELETE_LOG.
      RETURN.
  ENDCASE.

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
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2215424]
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
  CS_LAYOUT-SEL_MODE   = 'A'.
  CS_LAYOUT-NO_ROWMARK = GC_TRUE.
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.
  CS_LAYOUT-STYLEFNAME = 'CELLTAB'.
  CS_LAYOUT-CTAB_FNAME = 'COLTAB'.

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
      WHEN 'INTFNO'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'REQNO'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'GJAHR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'KEYDATA'.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
      WHEN 'USNAM'.
      WHEN 'STDAT'.
      WHEN 'STTIM'.
      WHEN 'ENDAT'.
      WHEN 'ENTIM'.
      WHEN 'EXETM'.
      WHEN 'HTTP_CODE'.
        <L_FIELDCAT>-OUTPUTLEN = 6.
        <L_FIELDCAT>-NO_ZERO   = GC_TRUE.
      WHEN 'HTTP_REASON'.
        <L_FIELDCAT>-OUTPUTLEN = 15 ##NUMBER_OK.
      WHEN 'STATUS'.
        <L_FIELDCAT>-OUTPUTLEN = 7.
      WHEN 'MESSAGE'.
        <L_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'BT_REQ_JSON'.
        <L_FIELDCAT>-ICON  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 8.
*       Text-c01 : Req. JSON
        LF_TEXT                  = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BT_RES_JSON'.
        <L_FIELDCAT>-ICON  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 8.
*       Text-c02 : Resp. JSON
        LF_TEXT                  = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BT_ERR_HTML'.
        <L_FIELDCAT>-ICON  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 8.
*       Text-c03 : Error HTML
        LF_TEXT                  = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
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
FORM F_ALV_SORT_RESULT  CHANGING PT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'INTFNO',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'GJAHR',
    LC_SORT3 TYPE  LVC_S_SORT-FIELDNAME VALUE 'REQNO'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: PT_SORT.

* Sort by Interface number
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

* Sort by Fiscal year
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

* Sort by Request Number
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT3.
  LS_SORT-DOWN      = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO PT_SORT.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '30%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '70%'.

  DATA:
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LF_TEMP1     TYPE  TEXT50,
    LF_TEMP2     TYPE  TEXT50,
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
* Text-h01 : Program:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  CONCATENATE SY-REPID SY-TITLE
         INTO LF_TEXT
    SEPARATED BY '-'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Processing Date/Time:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : System/Client:
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Process By:
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = SY-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : Total Records:
  LF_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Success Records:
  LF_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

*-----------------------
* Add value in Line 7
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h07 : Failed Records:
  LF_TEXT = TEXT-H07.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SUM-ERROR TO LF_TEXT LEFT-JUSTIFIED.
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
    LF_COL01 TYPE  I VALUE 25,
    LF_COL02 TYPE  I VALUE 35,
    LF_TEXT  TYPE  TEXT50,
    LF_TEMP1 TYPE  TEXT50,
    LF_TEMP2 TYPE  TEXT50.


  CONCATENATE SY-REPID SY-TITLE
         INTO LF_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h04 : Process By:
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LF_COL02)    SY-UNAME NO-GAP.

* Text-h05 : Total Records:
  WRITE GS_SUM-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H05 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP.

* Text-h06 : Success Records:
  WRITE GS_SUM-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H06 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_POSITIVE.

* Text-h07 : Failed Records:
  WRITE GS_SUM-ERROR TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H07 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_NEGATIVE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_selection_screen_format
*----------------------------------------------------------------------*
*  Selection Screen Format
*----------------------------------------------------------------------*
FORM F_SELECTION_SCREEN_FORMAT .

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'REP'.
      IF RB_REP EQ GC_TRUE.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF SCREEN-GROUP1 EQ 'DEL'.
      IF RB_DEL EQ GC_TRUE.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_validate_selection_screen
*----------------------------------------------------------------------*
*  Validate Selection Screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  CASE GC_TRUE.
    WHEN RB_REP.
*     Required interface no
      IF S_INTFNO[] IS INITIAL.
        SET CURSOR FIELD 'S_INTFNO-LOW'.
*       Error: Fill out all required entry fields
        MESSAGE E055(00).
        RETURN.
      ENDIF.
    WHEN RB_DEL.
*     Required Days older
      IF P_DAYS IS INITIAL.
        SET CURSOR FIELD 'P_DAYS'.
*       Error: Fill out all required entry fields
        MESSAGE E055(00).
        RETURN.
      ENDIF.
  ENDCASE.
ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_data
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_GET_DATA  CHANGING CT_RESULT TYPE  TT_RESULT
                          CS_SUM    TYPE  TS_SUM.

* Show progress
* Text-p01 : Reading Log Entries . . .
  MC_SHOW_PROGRESS 40 TEXT-P01.

* Initialize Output
  CLEAR: CT_RESULT,
         CS_SUM.

* Read Log data
  SELECT INTFNO,
         GJAHR,
         REQNO,
         KEYDATA,
         USNAM,
         STDAT,
         STTIM,
         ENDAT,
         ENTIM,
         HTTP_CODE,
         HTTP_REASON,
         REQUEST_JSON,
         RESPONSE_JSON,
         HTML_ERROR,
         STATUS,
         MESSAGE
    FROM ZSDSCAT001
   WHERE INTFNO      IN @S_INTFNO
     AND REQNO       IN @S_REQNO
     AND GJAHR       IN @S_GJAHR
     AND KEYDATA     IN @S_KEY
     AND USNAM       IN @S_USNAM
     AND STDAT       IN @S_STDAT
     AND STTIM       IN @S_STTIM
     AND ENDAT       IN @S_ENDAT
     AND ENTIM       IN @S_ENTIM
     AND HTTP_CODE   IN @S_HCODE
     AND HTTP_REASON IN @S_HRESN
     AND STATUS      IN @S_STATU
     AND MESSAGE     IN @S_MESSG
   ORDER BY ENDAT DESCENDING,
            ENTIM DESCENDING
    INTO CORRESPONDING FIELDS OF TABLE @CT_RESULT ##TOO_MANY_ITAB_FIELDS
      UP TO @P_ROWS ROWS.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  DATA:
    LS_CELLTAB TYPE  LVC_S_STYL,
    LS_COLTAB  TYPE  LVC_S_SCOL.

  LOOP AT CT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>).

*   Calculate Execution Time
    <L_RESULT>-EXETM = <L_RESULT>-ENTIM - <L_RESULT>-STTIM.
    IF <L_RESULT>-STDAT NE <L_RESULT>-ENDAT.
      <L_RESULT>-EXETM = <L_RESULT>-EXETM + ( ( <L_RESULT>-ENDAT - <L_RESULT>-STDAT ) * 86400 ) ##NUMBER_OK.
    ENDIF.

    IF <L_RESULT>-REQUEST_JSON IS NOT INITIAL.
      <L_RESULT>-BT_REQ_JSON = GC_ICON_REQ.
      CLEAR LS_CELLTAB.
      LS_CELLTAB-FIELDNAME = 'BT_REQ_JSON' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON .
      INSERT LS_CELLTAB INTO TABLE <L_RESULT>-CELLTAB.
    ENDIF.

    IF <L_RESULT>-RESPONSE_JSON IS NOT INITIAL.
      <L_RESULT>-BT_RES_JSON = GC_ICON_RES.
      CLEAR LS_CELLTAB.
      LS_CELLTAB-FIELDNAME = 'BT_RES_JSON' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON .
      INSERT LS_CELLTAB INTO TABLE <L_RESULT>-CELLTAB.
    ENDIF.

    IF <L_RESULT>-HTML_ERROR IS NOT INITIAL.
      <L_RESULT>-BT_ERR_HTML = GC_ICON_ERR.
      CLEAR LS_CELLTAB.
      LS_CELLTAB-FIELDNAME = 'BT_ERR_HTML' .
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON .
      INSERT LS_CELLTAB INTO TABLE <L_RESULT>-CELLTAB.
    ENDIF.

*   Success Message
    IF <L_RESULT>-HTTP_CODE BETWEEN 200 AND 299 ##NUMBER_OK.
*     Add Green color
      CLEAR LS_COLTAB.
      LS_COLTAB-FNAME     = 'HTTP_CODE'.
      LS_COLTAB-COLOR-COL = 5.
      LS_COLTAB-COLOR-INT = 1.
      LS_COLTAB-COLOR-INV = 0.
      INSERT LS_COLTAB INTO TABLE <L_RESULT>-COLTAB.
*   Error Message
    ELSEIF <L_RESULT>-HTTP_CODE BETWEEN 400 AND 500 ##NUMBER_OK.
*     Add Red color
      CLEAR LS_COLTAB.
      LS_COLTAB-FNAME     = 'HTTP_CODE'.
      LS_COLTAB-COLOR-COL = 6.
      LS_COLTAB-COLOR-INT = 1.
      LS_COLTAB-COLOR-INV = 0.
      INSERT LS_COLTAB INTO TABLE <L_RESULT>-COLTAB.
*   Other
    ELSEIF <L_RESULT>-HTTP_CODE IS NOT INITIAL.
*     Add Yellow color
      CLEAR LS_COLTAB.
      LS_COLTAB-FNAME     = 'HTTP_CODE'.
      LS_COLTAB-COLOR-COL = 3.
      LS_COLTAB-COLOR-INT = 1.
      LS_COLTAB-COLOR-INV = 0.
      INSERT LS_COLTAB INTO TABLE <L_RESULT>-COLTAB.
    ENDIF.

    IF <L_RESULT>-STATUS IS NOT INITIAL.
*     Success Status
      IF <L_RESULT>-STATUS EQ GC_STATUS_SUCCESS.
*       Add Green color
        CLEAR LS_COLTAB.
        LS_COLTAB-FNAME     = 'STATUS'.
        LS_COLTAB-COLOR-COL = 5.
        LS_COLTAB-COLOR-INT = 1.
        LS_COLTAB-COLOR-INV = 0.
        INSERT LS_COLTAB INTO TABLE <L_RESULT>-COLTAB.
*     Error Message
      ELSE.
*       Add Red color
        CLEAR LS_COLTAB.
        LS_COLTAB-FNAME     = 'STATUS'.
        LS_COLTAB-COLOR-COL = 6.
        LS_COLTAB-COLOR-INT = 1.
        LS_COLTAB-COLOR-INV = 0.
        INSERT LS_COLTAB INTO TABLE <L_RESULT>-COLTAB.
      ENDIF.
    ENDIF.

*   Count Entries
    CS_SUM-TOTAL = CS_SUM-TOTAL + 1.

    IF <L_RESULT>-STATUS EQ GC_STATUS_SUCCESS.
      CS_SUM-SUCCS = CS_SUM-SUCCS + 1.
    ELSE.
      CS_SUM-ERROR = CS_SUM-ERROR + 1.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_button_click_1
*----------------------------------------------------------------------*
*  ALV Event on Button click
*----------------------------------------------------------------------*
FORM F_BUTTON_CLICK_1 USING US_ROW_ID    TYPE LVC_S_ROID
                            US_COLUMN_ID TYPE LVC_S_COL ##CALLED.

  DATA:
    LF_TYPE   TYPE  CHAR1.

  FIELD-SYMBOLS:
    <L_RESULT>  TYPE  TS_RESULT.


* Read Row
  READ TABLE GT_RESULT ASSIGNING <L_RESULT>
                       INDEX US_ROW_ID-ROW_ID.
  IF SY-SUBRC NE 0.
*   No row found
    RETURN.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'BT_REQ_JSON'.
*     Text-i01: Request JSON
      PERFORM F_SHOW_POPUP  USING  <L_RESULT>-REQUEST_JSON
                                   GC_JSON
                                   TEXT-I01.
    WHEN 'BT_RES_JSON'.
*     Text-i02: Response JSON
      PERFORM F_SHOW_POPUP  USING  <L_RESULT>-RESPONSE_JSON
                                   GC_JSON
                                   TEXT-I02.
    WHEN 'BT_ERR_HTML'.
      IF ( STRLEN( <L_RESULT>-HTML_ERROR ) GE 6 AND
           <L_RESULT>-HTML_ERROR(6) = '<html>' ) OR
         ( STRLEN( <L_RESULT>-HTML_ERROR ) GE 15 AND ##NUMBER_OK
        <L_RESULT>-HTML_ERROR(15) = '<!DOCTYPE html>' ).
        LF_TYPE = GC_HTML.
      ELSEIF <L_RESULT>-HTML_ERROR(1) = '{' OR
             <L_RESULT>-HTML_ERROR(1) = '[' .
        LF_TYPE = GC_JSON.
      ELSE.
        LF_TYPE = GC_TEXT.
      ENDIF.
*     Text-i03: HTML Error
      PERFORM F_SHOW_POPUP  USING  <L_RESULT>-HTML_ERROR
                                   LF_TYPE
                                   TEXT-I03.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_show_popup
*----------------------------------------------------------------------*
*  Show Popup
*----------------------------------------------------------------------*
FORM F_SHOW_POPUP  USING  UF_DATA  TYPE  ANY
                          UF_TYPE  TYPE  CHAR1
                          UF_SECTION  TYPE  CLIKE.

  DATA(LREF_OUT) = CL_DEMO_OUTPUT=>NEW( )->BEGIN_SECTION( UF_SECTION ).

  CASE UF_TYPE.
    WHEN GC_JSON.
      LREF_OUT->WRITE_JSON( UF_DATA ).
    WHEN GC_HTML.
      LREF_OUT->WRITE_HTML( UF_DATA ).
    WHEN GC_TEXT.
      LREF_OUT->WRITE_TEXT( UF_DATA ).
    WHEN OTHERS.
      RETURN.
  ENDCASE.

* Display Popup
  LREF_OUT->DISPLAY( ).

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_delete_log
*----------------------------------------------------------------------*
*  Delete Processing Log
*----------------------------------------------------------------------*
FORM F_DELETE_LOG .

  DATA:
    LF_DATUM TYPE  SY-DATUM,
    LF_TEMP  TYPE  TEXT50,
    LF_TEXT  TYPE  TEXT1000,
    LF_ANS   TYPE  CHAR1.


  LF_DATUM = SY-DATUM - P_DAYS.

  SELECT COUNT( * )
    INTO @DATA(LF_COUNT)
    FROM ZSDSCAT001
   WHERE INTFNO IN @S_INTFN2
     AND REQNO  IN @S_REQNO2
     AND GJAHR  IN @S_GJAHR2
     AND ENDAT  LT @LF_DATUM.
  IF SY-SUBRC NE 0.
*   Message: No log deleted.
    MESSAGE S024(ZSDSCA01).
    RETURN.
  ENDIF.

  WRITE LF_COUNT TO LF_TEMP NO-GAP LEFT-JUSTIFIED.
  CONDENSE LF_TEMP NO-GAPS.

  IF SY-BATCH IS INITIAL.
*   Text-t01: Delete Log Entries
*   Text-t02: Are you sure to delete & log entries?
    LF_TEXT = TEXT-T02.
    REPLACE ALL OCCURRENCES OF '&' IN LF_TEXT WITH LF_TEMP.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = TEXT-T01
        TEXT_QUESTION         = LF_TEXT
        TEXT_BUTTON_1         = 'Yes'(001)
        TEXT_BUTTON_2         = 'No'(002)
        DEFAULT_BUTTON        = '2'
        DISPLAY_CANCEL_BUTTON = ' '
      IMPORTING
        ANSWER                = LF_ANS
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.
    IF SY-SUBRC <> 0.
      CLEAR LF_ANS.
    ENDIF.
    IF LF_ANS NE 1.
*     Message: Operation cancelled by user.
      MESSAGE S025(ZSDSCA01).
      RETURN.
    ENDIF.
  ENDIF.

  DELETE FROM ZSDSCAT001 WHERE INTFNO IN S_INTFN2
                                 AND REQNO  IN S_REQNO2
                                 AND GJAHR  IN S_GJAHR2
                                 AND ENDAT  LT LF_DATUM.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
*   Message: &1 log entries deleted.
    MESSAGE S026(ZSDSCA01) WITH LF_TEMP.
    RETURN.
  ELSE.
    ROLLBACK WORK.                                     "#EC CI_ROLLBACK
*   Message: Error occured during delete entries from log table.
    MESSAGE S027(ZSDSCA01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

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


* Add Refresh Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = '&REF'.
  LS_BUTTON-ICON     = '@42@'.
* Text-a01: Refresh
  LS_BUTTON-QUICKINFO = TEXT-A01.
  INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.

  CASE UF_UCOMM.
    WHEN '&REF'.
*     Get data
      PERFORM F_GET_DATA CHANGING GT_RESULT
                                  GS_SUM.
*     Force PBO Process
      SUPPRESS DIALOG.
  ENDCASE.

ENDFORM.
