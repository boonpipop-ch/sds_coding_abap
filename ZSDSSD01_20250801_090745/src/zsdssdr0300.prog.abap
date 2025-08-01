*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0300
*  Creation Date      : 18.09.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : SDI038
*  Description        : This is a program to submit Tax invoice data to
*                       EZTax application.
*  Purpose            : - To submit Tax invoice data to EZTax
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  02.01.2025  F36K910519  Wuthichai L.  - Add parameter to filter only
*                                          data not yet submitted (CH01)
*-----------------------------------------------------------------------
REPORT ZSDSSDR0300.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS,
  BKPF,
  BSEG,
  VBRK.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE  TYPE  ZSDSSDS107.
TYPES:   ETAX_DATA TYPE ZCL_SDSSD_ETAX_INTERFACE=>TS_ETAX_DATA,
       END OF TS_RESULT.
TYPES: TT_RESULT  TYPE  STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_COUNT,
         TOTAL TYPE  I,
         READY TYPE  I,
         SUCCS TYPE  I,
         ERROR TYPE  I,
       END OF TS_COUNT.

TYPES: BEGIN OF TS_HEAD,
         COUNT TYPE  TS_COUNT,
       END OF TS_HEAD.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE      TYPE  CHAR1     VALUE 'X',
  GC_TCODE     TYPE  SY-TCODE  VALUE 'ZSDSSD025',

  GC_LED_GRAY  TYPE  ICON-ID VALUE '@BZ@',
  GC_LED_GREEN TYPE  ICON-ID VALUE '@5B@',
  GC_LED_RED   TYPE  ICON-ID VALUE '@5C@'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT        TYPE  TT_RESULT                           ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_HEAD            TYPE TS_HEAD                             ##NEEDED.

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
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSSDS107'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 28,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 72.

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
* Text-s01: Module
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      CB_SD TYPE  CHAR1 AS CHECKBOX DEFAULT 'X' USER-COMMAND DMY.
*   Text-s02: SD
    SELECTION-SCREEN COMMENT 2(30) TEXT-S02 FOR FIELD CB_SD.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      CB_FI TYPE  CHAR1 AS CHECKBOX DEFAULT 'X' USER-COMMAND DMY.
*   Text-s03: FI
    SELECTION-SCREEN COMMENT 2(30) TEXT-S03 FOR FIELD CB_FI.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

** Text-s04: Program Option
*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S04.
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS:
*      RB_SIGN  TYPE  CHAR1 RADIOBUTTON GROUP G1 DEFAULT 'X'.
**   Text-s05: Transfer Document to Cloud
*    SELECTION-SCREEN COMMENT 2(50) TEXT-S05 FOR FIELD RB_SIGN.
*  SELECTION-SCREEN END OF LINE.
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS:
*      RB_CANC  TYPE  CHAR1 RADIOBUTTON GROUP G1.
**   Text-s06: Reject Document (To re-transport)
*    SELECTION-SCREEN COMMENT 2(50) TEXT-S06 FOR FIELD RB_CANC.
*  SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK B2.
* Text-s07: General Data Selection
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S07.
  PARAMETERS:
    P_BUKRS TYPE  T001-BUKRS OBLIGATORY MEMORY ID BUK,
    P_GJAHR TYPE  BKPF-GJAHR OBLIGATORY DEFAULT SY-DATUM(4).
  SELECT-OPTIONS:
    S_BUDAT  FOR  BKPF-BUDAT,
    S_KUNNR  FOR  BSEG-KUNNR,
    S_BUPLA  FOR  BSEG-BUPLA.
  PARAMETERS:
    CB_SEND TYPE  FLAG AS CHECKBOX DEFAULT ' ',
    CB_NEW  TYPE  FLAG AS CHECKBOX DEFAULT ' '. "+CH01
SELECTION-SCREEN END OF BLOCK B3.
* Text-s08: Accounting Data Selection
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-S08.
  SELECT-OPTIONS:
    S_BLART   FOR BKPF-BLART MODIF ID FI,
    S_BELNR   FOR BKPF-BELNR MODIF ID FI,
    S_XBLNR1  FOR BKPF-XBLNR MODIF ID FI.
SELECTION-SCREEN END OF BLOCK B4.
* Text-s09: Sales Data Selection
SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-S09.
  SELECT-OPTIONS:
    S_VKORG  FOR VBRK-VKORG MODIF ID SD,
    S_VTWEG  FOR VBRK-VTWEG MODIF ID SD,
    S_FKART  FOR VBRK-FKART MODIF ID SD,
    S_VBELN  FOR VBRK-VBELN MODIF ID SD,
    S_XBLNR2 FOR VBRK-XBLNR MODIF ID SD.
SELECTION-SCREEN END OF BLOCK B5.
* Text-s10: Document Group
SELECTION-SCREEN BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-S10.

SELECTION-SCREEN END OF BLOCK B6.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SET_SELECTION_SCREEN.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get Data
  PERFORM F_GET_DATA CHANGING GT_RESULT
                              GS_HEAD-COUNT.
  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.
  IF CB_SEND EQ GC_TRUE.
    PERFORM F_AUTO_SEND CHANGING GT_RESULT
                                 GS_HEAD-COUNT.
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
*  Form F_SET_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Set Selection Screen
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN .

  LOOP AT SCREEN.

    IF SCREEN-GROUP1 EQ 'FI'.
      IF CB_FI EQ GC_TRUE.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'SD'.
      IF CB_SD EQ GC_TRUE.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

* Select at least 1 data source
  IF CB_SD IS INITIAL AND
     CB_FI IS INITIAL.
*   Text-e01: Please select at least 1 data source Module.
    MESSAGE E000(ZSDSSD01) WITH TEXT-E01 SPACE SPACE SPACE.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_DATA
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_GET_DATA  CHANGING CT_RESULT TYPE TT_RESULT
                          CS_COUNT  TYPE TS_COUNT.

  DATA:
    LT_COND TYPE  /IWBEP/T_MGW_SELECT_OPTION,
    LT_TMP  TYPE  ZCL_SDSSD_ETAX_INTERFACE=>TT_ETAX_DATA,
    LT_DATA TYPE  ZCL_SDSSD_ETAX_INTERFACE=>TT_ETAX_DATA.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.


* Show progress
* Text-p01 : Reading Tax Invoice Data . . .
  MC_SHOW_PROGRESS 25 TEXT-P01.

* Initialize Output
  CLEAR: CT_RESULT,
         CS_COUNT.

  IF CB_SD EQ GC_TRUE.
    CLEAR: LT_COND.
*   Assign Condition for selection
    APPEND VALUE #( PROPERTY = 'BUDAT'
                    SELECT_OPTIONS = CORRESPONDING #( S_BUDAT[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'KUNNR'
                    SELECT_OPTIONS = CORRESPONDING #( S_KUNNR[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'BUPLA'
                    SELECT_OPTIONS = CORRESPONDING #( S_BUPLA[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'VKORG'
                    SELECT_OPTIONS = CORRESPONDING #( S_VKORG[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'VTWEG'
                    SELECT_OPTIONS = CORRESPONDING #( S_VTWEG[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'FKART'
                    SELECT_OPTIONS = CORRESPONDING #( S_FKART[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'VBELN'
                    SELECT_OPTIONS = CORRESPONDING #( S_VBELN[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'VBRK_XBLNR'
                    SELECT_OPTIONS = CORRESPONDING #( S_XBLNR2[] ) )
              TO LT_COND.

*   Get ETAX Data (SD)
    ZCL_SDSSD_ETAX_INTERFACE=>GET_DATA_SD(
      EXPORTING
        IF_BUKRS    = P_BUKRS
        IF_GJAHR    = P_GJAHR
        IT_COND     = LT_COND
        IF_NEW      = CB_NEW
        IF_NEW_FILE = SPACE     "+20.01.2025 (Not include new form file)
      IMPORTING
        ET_DATA     = LT_TMP ).
    INSERT LINES OF LT_TMP INTO TABLE LT_DATA.
  ENDIF.

  IF CB_FI EQ GC_TRUE.
    CLEAR: LT_COND.
*   Assign Condition for selection
    APPEND VALUE #( PROPERTY = 'BUDAT'
                    SELECT_OPTIONS = CORRESPONDING #( S_BUDAT[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'KUNNR'
                    SELECT_OPTIONS = CORRESPONDING #( S_KUNNR[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'BUPLA'
                    SELECT_OPTIONS = CORRESPONDING #( S_BUPLA[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'BLART'
                    SELECT_OPTIONS = CORRESPONDING #( S_BLART[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'BELNR'
                    SELECT_OPTIONS = CORRESPONDING #( S_BELNR[] ) )
              TO LT_COND.
    APPEND VALUE #( PROPERTY = 'BKPF_XBLNR'
                    SELECT_OPTIONS = CORRESPONDING #( S_XBLNR1[] ) )
              TO LT_COND.

*   Get ETAX Data (FI)
    ZCL_SDSSD_ETAX_INTERFACE=>GET_DATA_FI(
      EXPORTING
        IF_BUKRS    = P_BUKRS
        IF_GJAHR    = P_GJAHR
        IT_COND     = LT_COND
        IF_NEW      = CB_NEW
        IF_NEW_FILE = SPACE     "+20.01.2025 (Not include new form file)
      IMPORTING
        ET_DATA     = LT_TMP ).
    INSERT LINES OF LT_TMP INTO TABLE LT_DATA.
  ENDIF.

  FREE LT_TMP.

* Assign Result
  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).

    CLEAR LS_RESULT.
    MOVE-CORRESPONDING <L_DATA> TO LS_RESULT ##ENH_OK.
    LS_RESULT-ETAX_DATA = <L_DATA>.

    PERFORM F_ASSIGN_MESSAGE  USING  LS_RESULT-ETAX_DATA-MSGTY
                                     LS_RESULT-ETAX_DATA-MSGTX
                            CHANGING LS_RESULT-STATU
                                     LS_RESULT-MSGTX
                                     CS_COUNT.

    INSERT LS_RESULT INTO TABLE CT_RESULT.

  ENDLOOP.

* Count
  CS_COUNT-TOTAL = LINES( CT_RESULT ).

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
* Enable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
  GF_NO_AUTO_REFRESH_1 = GC_TRUE.
* Not soft refresh but not all refresh
  CLEAR: GF_SOFT_REFRESH_1,
         GF_REFRESH_ALL_1.

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
  CS_LAYOUT-SEL_MODE   = 'C'.
  CS_LAYOUT-NO_ROWMARK = GC_TRUE.
  CS_LAYOUT-CWIDTH_OPT = SPACE.
  CS_LAYOUT-ZEBRA      = SPACE.
*  CS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.
*  CS_LAYOUT-BOX_FNAME  = 'SELCT'.
*  CS_LAYOUT-CTAB_FNAME = 'COLTAB'.

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
    LF_TEXT       TYPE  TEXT50,
    LF_AMOUNT_LEN TYPE  I VALUE 15.
*    LF_COLOR1 TYPE  LVC_S_FCAT-EMPHASIZE VALUE 'C700',
*    LF_COLOR2 TYPE  LVC_S_FCAT-EMPHASIZE VALUE 'C410'.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'SELCT'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-EDIT      = GC_TRUE.
        <L_FIELDCAT>-CHECKBOX  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 6 ##NUMBER_OK.
*       Text-c00 : Select
        LF_TEXT                = TEXT-C00.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'REPLC'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-EDIT      = GC_TRUE.
        <L_FIELDCAT>-CHECKBOX  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 6 ##NUMBER_OK.
*       Text-c13 : Replace
        LF_TEXT                = TEXT-C13.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'STATU'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
*       Text-c01 : Status
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-ICON  = GC_TRUE.
      WHEN 'TAXINV'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
*       Text-c02 : Tax Invoice no.
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CANCEL'.
        <L_FIELDCAT>-CHECKBOX  = GC_TRUE.
        <L_FIELDCAT>-OUTPUTLEN = 10 ##NUMBER_OK.
*       Text-c14 : Cancelled
        LF_TEXT                = TEXT-C14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MSGTX'.
        <L_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'VBELN'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
      WHEN 'BUKRS'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
        <L_FIELDCAT>-NO_OUT    = GC_TRUE.
      WHEN 'BELNR'.
        <L_FIELDCAT>-HOTSPOT   = GC_TRUE.
      WHEN 'GJAHR'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'XBLNR'.
      WHEN 'FKDAT'.
      WHEN 'DOCTY'.
        <L_FIELDCAT>-KEY       = GC_TRUE.
      WHEN 'DESCP_TH'.
        <L_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'SAP_MODULE'.
      WHEN 'FKART'.
*       Text-c15 : Doc.Type
        LF_TEXT                = TEXT-C15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'FKART_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BUPLA'.
      WHEN 'KUNNR'.
      WHEN 'KUNNR_NAME'.
        <L_FIELDCAT>-OUTPUTLEN = 30 ##NUMBER_OK.
      WHEN 'BRANCH'.
      WHEN 'AUGRU'.
      WHEN 'CAUSE_CD'.
      WHEN 'CAUSE_TX'.
        <L_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'ZTERM'.
      WHEN 'ZTERM_TXT'.
        <L_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'INCO1'.
      WHEN 'BSTKD'.
        <L_FIELDCAT>-OUTPUTLEN = 20 ##NUMBER_OK.
      WHEN 'BSTDK'.
      WHEN 'WAERS'.
      WHEN 'GROSS_AMT'.
*       Text-c03 : Gross Amount
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'DISC_AMT'.
*       Text-c04 : Total Disc Amount
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'CHRG_AMT'.
*       Text-c05 : Total Charge Amount
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'VATBS_AMT'.
*       Text-c06 : Total Vat Base Amount
        LF_TEXT                = TEXT-C06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'NET1_AMT'.
*       Text-c07 : Net Amount Before Vat
        LF_TEXT                = TEXT-C07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'VAT_AMT'.
*       Text-c08 : VAT Amount
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'NET2_AMT'.
*       Text-c09 : Net Amount After Vat
        LF_TEXT                = TEXT-C09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'ORG_AMT'.
*       Text-c10 : Ref. Doc Amount
        LF_TEXT                = TEXT-C10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'CORRCT_AMT'.
*       Text-c11 : Correct Amount
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'DIFF_AMT'.
*       Text-c12 : Different Amount
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
        <L_FIELDCAT>-OUTPUTLEN = LF_AMOUNT_LEN.
      WHEN 'ERNAM'.
      WHEN 'ERDAT'.

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
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'TAXINV'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: PT_SORT.

* Sort by Tax Invoice No.
  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
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
* Text-h05 : Program:
  LF_TEXT = TEXT-H05.
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
* Text-h06 : Processing Date/Time:
  LF_TEXT = TEXT-H06.
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
* Text-h07 : System/Client:
  LF_TEXT = TEXT-H07.
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
* Text-h08 : Process By:
  LF_TEXT = TEXT-H08.
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
* Text-h01 : Total Count Entries:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_HEAD-COUNT-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_TOTAL.

*-----------------------
* Add value in Line 6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : Ready Status Entries:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_HEAD-COUNT-READY TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line 7
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : Submitted Entries:
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_HEAD-COUNT-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.

*-----------------------
* Add value in Line 8
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Error Entries:
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_HEAD-COUNT-ERROR TO LF_TEXT LEFT-JUSTIFIED.
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
* Text-h05 : Program:
  WRITE AT: /1(LF_COL01)  TEXT-H05 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
* Text-h06 : Processing Date/Time:
  WRITE AT: /1(LF_COL01)  TEXT-H06 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
* Text-h07 : System/Client:
  WRITE AT: /1(LF_COL01)  TEXT-H07 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h08 : Process By:
  WRITE AT: /1(LF_COL01)  TEXT-H08 INTENSIFIED ON NO-GAP,
            (LF_COL02)    SY-UNAME NO-GAP.

* Text-h01 : Total Count Entries:
  WRITE GS_HEAD-COUNT-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_TOTAL.

* Text-h02 : Ready Status Entries:
  WRITE GS_HEAD-COUNT-READY TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP.

* Text-h03 : Submitted Entries:
  WRITE GS_HEAD-COUNT-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_POSITIVE.

* Text-h04 : Error Entries:
  WRITE GS_HEAD-COUNT-ERROR TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_NEGATIVE.

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


* Handle Toolbar as needed
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&REFRESH'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&INFO'.

* Add Select All Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = 'SALL'.
  LS_BUTTON-ICON     = '@4B@'.
* Text-a01: Select All
  LS_BUTTON-QUICKINFO = TEXT-A01.
  INSERT LS_BUTTON INTO UREF_OBJECT->MT_TOOLBAR
                   INDEX 3.

* Add De Select All Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = 'DSALL'.
  LS_BUTTON-ICON     = '@4D@'.
* Text-a02: De Select All
  LS_BUTTON-QUICKINFO = TEXT-A02.
  INSERT LS_BUTTON INTO UREF_OBJECT->MT_TOOLBAR
                   INDEX 4.

* Add Submit Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = 'SEND'.
  LS_BUTTON-ICON     = '@8W@'.
* Text-a03: Submit to EZTax
  LS_BUTTON-QUICKINFO = TEXT-A03.
  LS_BUTTON-TEXT      = TEXT-A03.
  INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

* Add Processing Log Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = 'PLOG'.
  LS_BUTTON-ICON     = '@KK@'.
* Text-a04: View Processing Log
  LS_BUTTON-QUICKINFO = TEXT-A04.
  LS_BUTTON-TEXT      = TEXT-A04.
  INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.

  DATA:
    LS_STABLE TYPE LVC_S_STBL.


  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CASE UF_UCOMM.
*   ----------------
*   Select All
*   ----------------
    WHEN 'SALL'.
      MODIFY GT_RESULT FROM VALUE #( SELCT = GC_TRUE )
                       TRANSPORTING SELCT
                       WHERE SELCT IS INITIAL.
*     Refresh Display
      CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE      = LS_STABLE
          I_SOFT_REFRESH = 'X'.

*   ----------------
*   De-Select All
*   ----------------
    WHEN 'DSALL'.
      MODIFY GT_RESULT FROM VALUE #( SELCT = SPACE )
                       TRANSPORTING SELCT
                       WHERE SELCT IS NOT INITIAL.
*     Refresh Display
      CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE      = LS_STABLE
          I_SOFT_REFRESH = 'X'.

*   ----------------
*   Submit to EzTax
*   ----------------
    WHEN 'SEND'.
      PERFORM F_SEND_TO_EZTAX CHANGING GT_RESULT
                                       GS_HEAD-COUNT.

*     Refresh Display
      CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE      = LS_STABLE
          I_SOFT_REFRESH = 'X'.

      SUPPRESS DIALOG.

*   ----------------
*   View Processing Log
*   ----------------
    WHEN 'PLOG'.
      PERFORM F_DISPLAY_PROCESS_LOG USING GT_RESULT.

*     Refresh Display
      CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE      = LS_STABLE
          I_SOFT_REFRESH = 'X'.


  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_SEND_TO_EZTAX
*----------------------------------------------------------------------*
*  Send Data to EzTax
*----------------------------------------------------------------------*
FORM F_SEND_TO_EZTAX  CHANGING CT_RESULT  TYPE  TT_RESULT
                               CS_COUNT   TYPE  TS_COUNT.

  CONSTANTS:
    LC_NOT_RD    TYPE ZCL_SDSSD_ETAX_INTERFACE=>TS_ETAX_CANC-CAUSE_CD VALUE 'CLN999'.

  DATA:
    LS_ETAX_CANC TYPE ZCL_SDSSD_ETAX_INTERFACE=>TS_ETAX_CANC,
    LS_RESPONSE  TYPE ZSDSSDS095,
    LS_RETURN    TYPE BAPIRET2.

  DATA:
    LF_PROC  TYPE  I,
    LF_COUNT TYPE  I,
    LF_TOTAL TYPE  I.


  LOOP AT CT_RESULT TRANSPORTING NO FIELDS
                  WHERE SELCT EQ GC_TRUE.
    LF_TOTAL = LF_TOTAL + 1.
  ENDLOOP.
  IF SY-SUBRC NE 0.
*   Message: Please select item(s) to be processed.
    MESSAGE S004(ZSDSSD01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR LF_COUNT.
  LOOP AT CT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                    WHERE SELCT EQ GC_TRUE.

    LF_COUNT = LF_COUNT + 1.
    LF_PROC =  ( LF_COUNT * 96 ) / LF_TOTAL ##NUMBER_OK.

*   Show progress
*   Text-p02 : Sending Tax Invoice Data to EzTax. . .
    MC_SHOW_PROGRESS LF_PROC TEXT-P02.

*   Update Input Fields
    <L_RESULT>-ETAX_DATA-REPLC = <L_RESULT>-REPLC.

    IF <L_RESULT>-CANCEL IS INITIAL.
*     Call EZTax Sign Service
      CALL METHOD ZCL_SDSSD_ETAX_INTERFACE=>CALL_SIGN_SERVICE
        EXPORTING
          IS_ETAX_DATA = <L_RESULT>-ETAX_DATA
        IMPORTING
          ES_RESPONSE  = LS_RESPONSE
          ES_RETURN    = LS_RETURN.
    ELSE.
      CLEAR LS_ETAX_CANC.
      MOVE-CORRESPONDING <L_RESULT>-ETAX_DATA TO LS_ETAX_CANC.
      LS_ETAX_CANC-CAUSE_CD = LC_NOT_RD.
*     Call EZTax Cancel Service
      CALL METHOD ZCL_SDSSD_ETAX_INTERFACE=>CALL_CANCEL_SERVICE
        EXPORTING
          IS_ETAX_CANC = LS_ETAX_CANC
        IMPORTING
          ES_RESPONSE  = LS_RESPONSE
          ES_RETURN    = LS_RETURN.
    ENDIF.

    IF LS_RETURN IS NOT INITIAL.
      <L_RESULT>-ETAX_DATA-MSGTY = LS_RETURN-TYPE.
      <L_RESULT>-ETAX_DATA-MSGTX = LS_RETURN-MESSAGE.
    ELSE.
      <L_RESULT>-ETAX_DATA-MSGTY = LS_RESPONSE-RESPONSE_CODE.
      <L_RESULT>-ETAX_DATA-MSGTX = LS_RESPONSE-RESPONSE_MSG.
    ENDIF.

*   Count Entries
    CASE <L_RESULT>-STATU.
      WHEN GC_LED_GREEN.
        CS_COUNT-SUCCS = CS_COUNT-SUCCS - 1.
      WHEN GC_LED_RED.
        CS_COUNT-ERROR = CS_COUNT-ERROR - 1.
      WHEN GC_LED_GRAY.
        CS_COUNT-READY = CS_COUNT-READY - 1.
    ENDCASE.

    PERFORM F_ASSIGN_MESSAGE  USING  <L_RESULT>-ETAX_DATA-MSGTY
                                     <L_RESULT>-ETAX_DATA-MSGTX
                            CHANGING <L_RESULT>-STATU
                                     <L_RESULT>-MSGTX
                                     CS_COUNT.

*   Modify Replace Flag when Successfully Call API
    IF <L_RESULT>-REPLC IS INITIAL AND
       <L_RESULT>-ETAX_DATA-MSGTY EQ 'S'.
      <L_RESULT>-REPLC = GC_TRUE.
    ENDIF.

  ENDLOOP.

* Message: Operation completed. Please check result in column "Message".
  MESSAGE S014(ZSDSSD01).

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_MESSAGE
*----------------------------------------------------------------------*
*  Assign Message
*----------------------------------------------------------------------*
FORM F_ASSIGN_MESSAGE  USING  UF_MSGTY TYPE ZCL_SDSSD_ETAX_INTERFACE=>TS_ETAX_DATA-MSGTY
                              UF_MSGTX TYPE ZCL_SDSSD_ETAX_INTERFACE=>TS_ETAX_DATA-MSGTX
                     CHANGING CF_STATU TYPE TS_RESULT-STATU
                              CF_MSGTX TYPE TS_RESULT-MSGTX
                              CS_COUNT TYPE TS_COUNT.

* Initialize Output
  CLEAR: CF_STATU,
         CF_MSGTX.

  CF_MSGTX = UF_MSGTX.

  CASE UF_MSGTY.
    WHEN 'E'.
      CF_STATU = GC_LED_RED.
    WHEN 'S'.
      CF_STATU = GC_LED_GREEN.
    WHEN OTHERS.
      CF_STATU = GC_LED_GRAY.
      IF UF_MSGTX IS INITIAL.
*       Text-i01: Ready for Submiting
        CF_MSGTX = TEXT-I01.
      ENDIF.
  ENDCASE.

* Count Entries
  CASE CF_STATU.
    WHEN GC_LED_GREEN.
      CS_COUNT-SUCCS = CS_COUNT-SUCCS + 1.
    WHEN GC_LED_RED.
      CS_COUNT-ERROR = CS_COUNT-ERROR + 1.
    WHEN GC_LED_GRAY.
      CS_COUNT-READY = CS_COUNT-READY + 1.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_PROCESS_LOG
*----------------------------------------------------------------------*
*  View Processing Log
*----------------------------------------------------------------------*
FORM F_DISPLAY_PROCESS_LOG USING UT_RESULT TYPE  TT_RESULT.

  DATA:
    LT_KEY  TYPE  ZCL_SDSSD_ETAX_INTERFACE=>TT_KEY.


  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                    WHERE SELCT EQ GC_TRUE.
    DATA(LF_BUKRS) = <L_RESULT>-ETAX_DATA-BUKRS.
    INSERT VALUE #( BUKRS  = <L_RESULT>-ETAX_DATA-BUKRS
                    DOCTY  = <L_RESULT>-ETAX_DATA-DOCTY
                    TAXINV = <L_RESULT>-ETAX_DATA-TAXINV )
           INTO TABLE LT_KEY.
  ENDLOOP.
  IF SY-SUBRC NE 0.
*   Message: Please select item(s) to be processed.
    MESSAGE S004(ZSDSSD01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Export Key to Memory
  EXPORT KEY FROM LT_KEY TO MEMORY ID ZCL_SDSSD_ETAX_INTERFACE=>GC_MEMID.

* Submit Report
  SUBMIT ZSDSSDR0350 WITH RB_REP   EQ GC_TRUE            "#EC CI_SUBMIT
                     WITH S_BUKRS  EQ LF_BUKRS
                     WITH P_SUBMIT EQ GC_TRUE
                     AND RETURN.

  FREE MEMORY ID ZCL_SDSSD_ETAX_INTERFACE=>GC_MEMID.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HOTSPOT_CLICK_1
*----------------------------------------------------------------------*
*  ALV Hotspot Click
*----------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW           ##CALLED
                             US_COLUMN_ID TYPE LVC_S_COL.

* Read Line
  READ TABLE GT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                       INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'VBELN'.
      PERFORM F_DISPLAY_BILLING USING  <L_RESULT>-VBELN.
    WHEN 'BELNR'.
      PERFORM F_DISPLAY_FIDOC USING  <L_RESULT>-BUKRS
                                     <L_RESULT>-BELNR
                                     <L_RESULT>-GJAHR.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_BILLING
*----------------------------------------------------------------------*
*  Display Billing Document
*----------------------------------------------------------------------*
FORM F_DISPLAY_BILLING  USING  UF_VBELN TYPE VBRK-VBELN.

  IF UF_VBELN IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'VF' FIELD UF_VBELN.
  CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.         "#EC CI_CALLTA

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DISPLAY_FIDOC
*----------------------------------------------------------------------*
*  Display FI Document
*----------------------------------------------------------------------*
FORM F_DISPLAY_FIDOC  USING  UF_BUKRS TYPE BKPF-BUKRS
                             UF_BELNR TYPE BKPF-BELNR
                             UF_GJAHR TYPE BKPF-GJAHR.

  IF UF_BUKRS IS INITIAL OR
     UF_BELNR IS INITIAL OR
     UF_GJAHR IS INITIAL.
    RETURN.
  ENDIF.

  SET PARAMETER ID 'BUK' FIELD UF_BUKRS.
  SET PARAMETER ID 'BLN' FIELD UF_BELNR.
  SET PARAMETER ID 'GJR' FIELD UF_GJAHR.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.         "#EC CI_CALLTA

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_AUTO_SEND
*----------------------------------------------------------------------*
*  Automatically Send data to Etax
*----------------------------------------------------------------------*
FORM F_AUTO_SEND  CHANGING CT_RESULT TYPE TT_RESULT
                           CS_COUNT  TYPE TS_COUNT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.


  CLEAR: LS_RESULT.
  LS_RESULT-SELCT = GC_TRUE.
* Mark all ready for submitting
  MODIFY CT_RESULT FROM LS_RESULT
                   TRANSPORTING SELCT
                   WHERE STATU = GC_LED_GRAY.

  READ TABLE CT_RESULT TRANSPORTING NO FIELDS
                       WITH KEY SELCT = GC_TRUE.
  IF SY-SUBRC NE 0.
*   Error: No data found to submit to Etax.
    MESSAGE S031(ZSDSSD01).
    RETURN.
  ENDIF.

* Send to Etax
  PERFORM F_SEND_TO_EZTAX CHANGING CT_RESULT
                                   CS_COUNT.

ENDFORM.
