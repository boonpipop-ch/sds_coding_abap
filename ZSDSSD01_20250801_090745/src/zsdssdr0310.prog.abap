*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0310
*  Creation Date      : 21.09.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : SDR012
*  Description        : This program is to display detail information on
*                         - Purchase order (PO)
*                         - Quotation (QT)
*                         - Sales order (SO)
*  Purpose            : This program is to be called from ZSDSSDR00290
*                       when click to show detail for PO, QT, SO
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0310.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS,
        MARA,
        ZSDSSDT018.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSSDS099.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                 WITH DEFAULT KEY.

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
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS099'.

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
  PARAMETERS: P_DATFRM TYPE SY-DATUM OBLIGATORY,
              P_DATTO  TYPE SY-DATUM OBLIGATORY.
  SELECT-OPTIONS: S_MATNR FOR MARA-MATNR OBLIGATORY,
                  S_QTGRP FOR ZSDSSDT018-QUOTAGRP.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T02.
  PARAMETERS: RB_PO RADIOBUTTON GROUP G1,
              RB_QT RADIOBUTTON GROUP G1,
              RB_SO RADIOBUTTON GROUP G1,
              RB_DO RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B2.

* For internal call from program ZSDSSDR0290,
* Mark this flag to read data from memory instead of selecting
* from database again
PARAMETERS: P_IMPORT TYPE CHAR1 DEFAULT SPACE NO-DISPLAY ##NEEDED.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'ONLI'.
      PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDCASE.

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
* Form F_VALIDATE_SELECTION_SCREEN
*---------------------------------------------------------------------*
* Validate selection screen
*---------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  IF P_DATFRM IS NOT INITIAL AND
     P_DATTO IS NOT INITIAL.
    IF P_DATTO LT P_DATFRM.
*     Error: Invalid From/To Date
      MESSAGE E000(ZSDSSD01) WITH TEXT-E01.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_DATA
*---------------------------------------------------------------------*
*  Get Data
*---------------------------------------------------------------------*
FORM F_GET_DATA  CHANGING CT_RESULT TYPE TT_RESULT.
  DATA: LT_PO_DATA TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TT_PO_DATA,
        LT_QT_DATA TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TT_SALES_ORD_DATA,
        LT_SO_DATA TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TT_SALES_ORD_DATA,
        LT_DO_DATA TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TT_SALES_ORD_DATA.
  DATA: LS_PERIOD_SCOPE TYPE ZCL_SDSSD_QUOTA_PLAN_UTIL=>TS_PERIOD_SCOPE.

  CLEAR: CT_RESULT.

  LS_PERIOD_SCOPE-CURRENT_BEGIN = P_DATFRM.
  LS_PERIOD_SCOPE-NEXT1_END = P_DATTO.

  CASE ABAP_TRUE.
    WHEN RB_PO.
*     Get PO
      ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_PO_DATA(
        EXPORTING
          IS_PERIOD_SCOPE = LS_PERIOD_SCOPE
          IT_MATNR        = S_MATNR[]
        IMPORTING
          ET_PO_DATA      = LT_PO_DATA ).

      CT_RESULT[] = CORRESPONDING #( LT_PO_DATA ).

    WHEN RB_QT.
*     Get Quotation
      ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_QUOTATION_DATA(
        EXPORTING
          IS_PERIOD_SCOPE    = LS_PERIOD_SCOPE
          IT_MATNR           = S_MATNR[]
          IT_QTGRP           = S_QTGRP[]
        IMPORTING
          ET_QUOTATION_DATA  = LT_QT_DATA ).

      CT_RESULT[] = CORRESPONDING #( LT_QT_DATA ).

    WHEN RB_SO.
*     Get Sales Order
      ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_SALES_ORDER_DATA(
        EXPORTING
          IS_PERIOD_SCOPE    = LS_PERIOD_SCOPE
          IT_MATNR           = S_MATNR[]
          IT_QTGRP           = S_QTGRP[]
        IMPORTING
          ET_SALES_ORD_DATA  = LT_SO_DATA ).

      CT_RESULT[] = CORRESPONDING #( LT_SO_DATA ).
    WHEN RB_DO.
      ZCL_SDSSD_QUOTA_PLAN_UTIL=>GET_DO_DATA(
       EXPORTING
         IS_PERIOD_SCOPE    = LS_PERIOD_SCOPE
         IT_MATNR           = S_MATNR[]
         IT_QTGRP           = S_QTGRP[]
       IMPORTING
         ET_DO_ORD_DATA     = LT_DO_DATA ).

      CT_RESULT[] = CORRESPONDING #( LT_DO_DATA ).
  ENDCASE.

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

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  CASE ABAP_TRUE.
    WHEN RB_PO.
      PERFORM F_MODIFY_FIELDCAT_PO CHANGING CT_FIELDCAT.
    WHEN RB_QT OR RB_SO.
      PERFORM F_MODIFY_FIELDCAT_SALES CHANGING CT_FIELDCAT.
  ENDCASE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_MODIFY_FIELDCAT_PO
*---------------------------------------------------------------------*
* Modify ALV Fieldcatalog for option PO
*---------------------------------------------------------------------*
FORM F_MODIFY_FIELDCAT_PO  CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.
  FIELD-SYMBOLS:
    <L_FCAT>  TYPE  LVC_S_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FCAT>.

    CASE <L_FCAT>-FIELDNAME.
      WHEN 'PERIOD'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'VKGRP' OR
           'ZZRUNNING' OR
           'QUOTAGRP' OR
           'QUOTAGRP_DESC' OR
           'VBELN' OR
           'POSNR' OR
           'EDATU' OR
           'PSTYV' OR
           'VBTYP' OR
           'KUNNR' OR
           'VKBUR' OR
           'WMENG' OR
           'VRKME'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'MENGE'.
        <L_FCAT>-DO_SUM = ABAP_TRUE.
    ENDCASE.

  ENDLOOP.
ENDFORM.
*---------------------------------------------------------------------*
* Form F_MODIFY_FIELDCAT_SALES
*---------------------------------------------------------------------*
* Modify ALV Fieldcatalog for option QT and SO
*---------------------------------------------------------------------*
FORM F_MODIFY_FIELDCAT_SALES CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  FIELD-SYMBOLS:
    <L_FCAT>  TYPE  LVC_S_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FCAT>.
    CASE <L_FCAT>-FIELDNAME.
      WHEN 'PERIOD'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'VBTYP'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'EBELN' OR
           'EBELP' OR
           'EINDT' OR
           'WERKS' OR
           'LGORT' OR
           'MENGE' OR
           'MEINS'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'WMENG'.
        <L_FCAT>-DO_SUM = ABAP_TRUE.
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
    LC_SORT1 TYPE  LVC_FNAME VALUE 'MATNR'.

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
* Text-h03 : Req. Delivery Date
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE P_DATFRM TO LF_TEMP1.
  WRITE P_DATTO TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }-{ LF_TEMP2 }|.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Option
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  CASE ABAP_TRUE.
    WHEN RB_PO.
      LF_TEXT = TEXT-001.
    WHEN RB_QT.
      LF_TEXT = TEXT-002.
    WHEN RB_SO.
      LF_TEXT = TEXT-003.
  ENDCASE.
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

* Text-h03 : Req. Delivery Date
  WRITE P_DATFRM TO LF_TEMP1.
  WRITE P_DATTO TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }-{ LF_TEMP2 }|.
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h04 : Option
  CASE ABAP_TRUE.
    WHEN RB_PO.
      LF_TEXT = TEXT-001.
    WHEN RB_QT.
      LF_TEXT = TEXT-002.
    WHEN RB_SO.
      LF_TEXT = TEXT-003.
  ENDCASE.
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.
ENDFORM.
