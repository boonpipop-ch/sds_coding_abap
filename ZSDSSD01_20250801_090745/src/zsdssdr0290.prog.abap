*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0290
*  Creation Date      : 16.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : This program is to display quota plan report
*  Purpose            : Related tables
*                        1. ZSDSSDT016: Quota Plan Header
*                        2. ZSDSSDT018: Quota Plan Item
*                        3. ZSDSSDC013: Mapping Sales Group/Quota Group
*                       Program will call report ZSDSSDR0310 to display
*                       detail list of PO, Quotation and Sales Order data
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0290.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS  ##NEEDED,
        MARA,
        ZSDSSDT018.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSSDS053.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                 WITH DEFAULT KEY.

TYPES: TS_PERIOD_SCOPE TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TS_PERIOD_SCOPE.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: GT_RESULT TYPE TT_RESULT                               ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA: GS_SCOPE_DATE TYPE TS_PERIOD_SCOPE   ##NEEDED.

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
CONSTANTS:
  GC_TRUE  TYPE CHAR1 VALUE 'X',
  GC_TCODE TYPE SY-TCODE VALUE 'ZSDSSD024'.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS053'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I VALUE 10,
  GC_ALV_HEIGHT_1    TYPE  I VALUE 90.

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
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.
  SELECT-OPTIONS: S_MATNR FOR MARA-MATNR,
                  S_QTGRP FOR ZSDSSDT018-QUOTAGRP.
  PARAMETERS: P_INCDEL AS CHECKBOX DEFAULT SPACE.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_SET_PERIOD_SCOPE CHANGING GS_SCOPE_DATE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Set Selection Screen Format
  PERFORM F_SET_SELECTION_SCREEN_FORMAT.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get Data
  PERFORM F_GET_DATA CHANGING GT_RESULT.

  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S003(ZSDSSD01).
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
FORM F_AUTHORIZE_CHECK USING UF_TCODE TYPE SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
  ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SET_PERIOD_SCOPE
*---------------------------------------------------------------------*
* Set period scope
*---------------------------------------------------------------------*
FORM F_SET_PERIOD_SCOPE  CHANGING CS_SCOPE_DATE TYPE TS_PERIOD_SCOPE.

  CS_SCOPE_DATE = ZCL_SDSSD_QUOTA_PLAN_UTIL=>SET_PERIOD_SCOPE( IS_REQDAT = SY-DATUM ).

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_DATA
*---------------------------------------------------------------------*
*  Get Data
*---------------------------------------------------------------------*
FORM F_GET_DATA CHANGING CT_RESULT TYPE TT_RESULT.
  DATA:
    LT_FILTER TYPE /IWBEP/T_MGW_SELECT_OPTION.

* Initialize Output
  CLEAR: CT_RESULT.

  APPEND VALUE #( PROPERTY = 'MATNR'
                  SELECT_OPTIONS = CORRESPONDING #( S_MATNR[] ) )
            TO LT_FILTER.
  APPEND VALUE #( PROPERTY = 'QTGRP'
                  SELECT_OPTIONS = CORRESPONDING #( S_QTGRP[] ) )
            TO LT_FILTER.

  IF P_INCDEL EQ GC_TRUE.        "Include record with delete flag
    APPEND VALUE #( PROPERTY = 'INCDEL'
                    SELECT_OPTIONS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_INCDEL ) ) )
              TO LT_FILTER.
  ENDIF.

* Call Class method to retrieve quota report information
  CALL METHOD ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_QUOTA_REPORT
    EXPORTING
      IT_FILTER = LT_FILTER
    IMPORTING
      ET_RESULT = CT_RESULT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_DISPLAY_RESULT
*---------------------------------------------------------------------*
*  Display ALV report
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
    LF_COLOR_CURRENT TYPE CHAR4 VALUE 'C410',  "Blue intensified off
    LF_COLOR_NEXT    TYPE CHAR4 VALUE 'C700',  "Orange intensified on
    LF_CURRENT       TYPE TEXT50,
    LF_NEXT1         TYPE TEXT50.

  FIELD-SYMBOLS:
    <L_FCAT>  TYPE  LVC_S_FCAT.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  WRITE GS_SCOPE_DATE-CURRENT TO LF_CURRENT.
  WRITE GS_SCOPE_DATE-NEXT1 TO LF_NEXT1.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FCAT>.

    CASE <L_FCAT>-FIELDNAME.
      WHEN 'REQDAT'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'MATNR'.
        <L_FCAT>-KEY = ABAP_TRUE.
      WHEN '00_PORTION'.
        <L_FCAT>-DO_SUM = ABAP_TRUE.
      WHEN '01_PORTION'.
        <L_FCAT>-DO_SUM = ABAP_TRUE.
      WHEN '00_PERIOD' OR
           '01_PERIOD'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN '00_PO_QTY' OR
           '01_PO_QTY' OR
           '00_QT_QTY' OR
           '01_QT_QTY' OR
           '00_SO_QTY' OR
           '01_SO_QTY' OR
           '00_DO_QTY' OR
           '01_DO_QTY' .
        <L_FCAT>-HOTSPOT = ABAP_TRUE.
    ENDCASE.

*   Set color for all columns under current period
    IF <L_FCAT>-FIELDNAME CP '00_*'.
      <L_FCAT>-EMPHASIZE = LF_COLOR_CURRENT.

      CONCATENATE LF_CURRENT <L_FCAT>-REPTEXT
        INTO <L_FCAT>-REPTEXT SEPARATED BY '-'.

    ELSEIF  <L_FCAT>-FIELDNAME CP '01_*'.
*   Set color for all columns under next period
      <L_FCAT>-EMPHASIZE = LF_COLOR_NEXT.

      CONCATENATE LF_NEXT1 <L_FCAT>-REPTEXT
        INTO <L_FCAT>-REPTEXT SEPARATED BY '-'.
    ENDIF.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_FNAME VALUE 'MATNR',
    LC_SORT2 TYPE  LVC_FNAME VALUE '00_QUOTAGRP'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.

* Initialize Output
  CLEAR: CT_SORT.

* Sort by Material Number
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = GC_TRUE.
  APPEND LS_SORT TO CT_SORT.

* Sort by Sales Group
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_TOP_OF_PAGE_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '20%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '80%'.

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
  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
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
  LF_TEXT = SY-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : Current Period:
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SCOPE_DATE-CURRENT TO LF_TEXT.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Next Period:
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_SCOPE_DATE-NEXT1 TO LF_TEXT.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
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
  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }/{ LF_TEMP2 }|.
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h02 : User:
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LF_COL02)    SY-UNAME NO-GAP.

* Text-h03 : Current Period:
  WRITE GS_SCOPE_DATE-CURRENT TO LF_TEMP1.
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEMP1 NO-GAP.

* Text-h04 : Next Period:
  WRITE GS_SCOPE_DATE-NEXT1 TO LF_TEMP1.
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEMP1 NO-GAP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_HOTSPOT_CLICK_1
*----------------------------------------------------------------------*
*  ALV Event on double click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW
                             US_COLUMN_ID TYPE LVC_S_COL ##CALLED.
  DATA: LF_FIELD TYPE CHAR50.

  FIELD-SYMBOLS:
    <L_RESULT> TYPE TS_RESULT,
    <L_VALUE>  TYPE ANY.

* Read Row
  READ TABLE GT_RESULT ASSIGNING <L_RESULT>
                       INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
*   No row found
    RETURN.
  ENDIF.

* Check value of the selected cell
* If initial, then no need to drill down for detail
  CONCATENATE '<L_RESULT>' US_COLUMN_ID-FIELDNAME
    INTO LF_FIELD SEPARATED BY '-'.

  ASSIGN (LF_FIELD) TO <L_VALUE>.
  IF <L_VALUE> IS ASSIGNED.
    IF <L_VALUE> IS INITIAL.
      RETURN.
    ENDIF.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN '00_PO_QTY'.
      PERFORM F_SUBMIT_ZSDSSDR0310 USING <L_RESULT>-MATNR
                                         <L_RESULT>-00_QUOTAGRP
                                         'PO'
                                         <L_RESULT>-00_PERIOD.
    WHEN '01_PO_QTY'.
      PERFORM F_SUBMIT_ZSDSSDR0310 USING <L_RESULT>-MATNR
                                         <L_RESULT>-01_QUOTAGRP
                                         'PO'
                                         <L_RESULT>-01_PERIOD.
    WHEN '00_QT_QTY'.
      PERFORM F_SUBMIT_ZSDSSDR0310 USING <L_RESULT>-MATNR
                                         <L_RESULT>-00_QUOTAGRP
                                         'QT'
                                         <L_RESULT>-00_PERIOD.
    WHEN '01_QT_QTY'.
      PERFORM F_SUBMIT_ZSDSSDR0310 USING <L_RESULT>-MATNR
                                         <L_RESULT>-01_QUOTAGRP
                                         'QT'
                                         <L_RESULT>-01_PERIOD.
    WHEN '00_SO_QTY'.
      PERFORM F_SUBMIT_ZSDSSDR0310 USING <L_RESULT>-MATNR
                                         <L_RESULT>-00_QUOTAGRP
                                         'SO'
                                         <L_RESULT>-00_PERIOD.
    WHEN '01_SO_QTY'.
      PERFORM F_SUBMIT_ZSDSSDR0310 USING <L_RESULT>-MATNR
                                         <L_RESULT>-01_QUOTAGRP
                                         'SO'
                                         <L_RESULT>-01_PERIOD.
    WHEN '01_DO_QTY'.
      PERFORM F_SUBMIT_ZSDSSDR0310 USING <L_RESULT>-MATNR
                                         <L_RESULT>-01_QUOTAGRP
                                         'DO'
                                         <L_RESULT>-01_PERIOD.
    WHEN '01_DO_QTY'.
      PERFORM F_SUBMIT_ZSDSSDR0310 USING <L_RESULT>-MATNR
                                         <L_RESULT>-01_QUOTAGRP
                                         'DO'
                                         <L_RESULT>-01_PERIOD.
  ENDCASE.
ENDFORM.
*---------------------------------------------------------------------*
* Form F_SUBMIT_ZSDSSDR0310
*---------------------------------------------------------------------*
* Submit to ZSDSSDR0310 for detail of PO,QT,SO
*---------------------------------------------------------------------*
FORM F_SUBMIT_ZSDSSDR0310  USING UF_MATNR TYPE MARA-MATNR
                                 UF_QUOTAGRP TYPE ZSDSDE_QUOTAGRP
                                 UF_OPTION TYPE CHAR2
                                 UF_PERIOD TYPE SPMON.
  DATA: LF_PO     TYPE CHAR1,
        LF_QT     TYPE CHAR1,
        LF_SO     TYPE CHAR1,
        LF_DO     TYPE CHAR1,
        LF_DATFRM TYPE SY-DATUM,
        LF_DATTO  TYPE SY-DATUM.

  DATA: LT_RANGE_MATNR    TYPE MMPUR_T_MATNR,
        LT_RANGE_QUOTAGRP TYPE ZQUOTAGRP_R.

  IF UF_MATNR IS INITIAL OR
     UF_PERIOD IS INITIAL OR
     UF_OPTION IS INITIAL.
    RETURN.
  ENDIF.

* Prepare material number
  IF UF_MATNR IS NOT INITIAL.
    INSERT VALUE #( SIGN   = 'I'
                    OPTION = 'EQ'
                    LOW    = UF_MATNR
                    HIGH   = SPACE )
           INTO TABLE LT_RANGE_MATNR.
  ENDIF.

* Prepare sales group
  IF UF_QUOTAGRP IS NOT INITIAL.
    INSERT VALUE #( SIGN   = 'I'
                    OPTION = 'EQ'
                    LOW    = UF_QUOTAGRP
                    HIGH   = SPACE )
           INTO TABLE LT_RANGE_QUOTAGRP.
  ENDIF.

* Prepare option (PO or QT or SO)
  CASE UF_OPTION.
    WHEN 'PO'.
      LF_PO = ABAP_TRUE.
      CLEAR: LF_QT, LF_SO,LF_DO.
    WHEN 'QT'.
      LF_QT = ABAP_TRUE.
      CLEAR: LF_PO, LF_SO,LF_DO.
    WHEN 'SO'.
      LF_SO = ABAP_TRUE.
      CLEAR: LF_PO, LF_QT,LF_DO.
    WHEN 'DO'.
      LF_DO = ABAP_TRUE.
      CLEAR: LF_PO,LF_QT,LF_SO.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

* Prepare period
  CASE UF_PERIOD.
    WHEN GS_SCOPE_DATE-CURRENT.
      LF_DATFRM = GS_SCOPE_DATE-CURRENT_BEGIN.
      LF_DATTO = GS_SCOPE_DATE-CURRENT_END.

    WHEN GS_SCOPE_DATE-NEXT1.
      LF_DATFRM = GS_SCOPE_DATE-NEXT1_BEGIN.
      LF_DATTO = GS_SCOPE_DATE-NEXT1_END .
  ENDCASE.

  SUBMIT ZSDSSDR0310 WITH P_DATFRM EQ LF_DATFRM          "#EC CI_SUBMIT
                     WITH P_DATTO EQ LF_DATTO
                     WITH S_MATNR IN LT_RANGE_MATNR
                     WITH S_QTGRP IN LT_RANGE_QUOTAGRP
                     WITH RB_PO EQ LF_PO
                     WITH RB_QT EQ LF_QT
                     WITH RB_SO EQ LF_SO
                     WITH RB_DO EQ LF_DO
                     AND RETURN.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SET_SELECTION_SCREEN_FORMAT
*---------------------------------------------------------------------*
*  Set Selection Screen Format
*---------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'S_MATNR-LOW'.
      SCREEN-REQUIRED = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_SELECTION_SCREEN
*---------------------------------------------------------------------*
*  Validate Selection screen
*---------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  IF S_MATNR IS INITIAL.
*   Please specify Material Number
    MESSAGE E000(ZSDSSD01) WITH TEXT-E01.
    RETURN.
  ENDIF.

ENDFORM.
