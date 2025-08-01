*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0240
*  Creation Date      : 25.07.2024
*  Author             : Kittirat C.(Eviden)
*  Add-on ID          : ZMMR018
*  Description        : Stock on Posting Period Report
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
REPORT ZSDSMMR0450.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  MARA,
  MCS0,
  ZSDSMMT021,
  SSCRFIELDS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_DAY_RANGE   TYPE RANGE OF ZSDSMMT021-ZZDATE.
TYPES: TT_WEEK_RANGE  TYPE RANGE OF MCS0-SPWOC.
TYPES: TT_MONTH_RANGE TYPE RANGE OF MCS0-SPMON.

TYPES:  BEGIN OF TS_RESULT.
          INCLUDE TYPE ZSDSMMS039.
TYPES:  END OF TS_RESULT.

TYPES: BEGIN OF TS_SEARCH,
         PRODH TYPE CHAR18,
         LEVEL TYPE CHAR1,
       END OF TS_SEARCH.

TYPES: BEGIN OF TS_PERIOD,
         BEGDA              TYPE SY-DATUM,
         ENDDA              TYPE SY-DATUM,
         PERIOD             TYPE TT_DAY_RANGE,
         PERIOD_FIRST_DATE  TYPE TT_DAY_RANGE,
       END OF TS_PERIOD.

TYPES: BEGIN OF TS_MARD,
         MATNR TYPE MARD-MATNR,
         WERKS TYPE MARD-WERKS,
         LGORT TYPE MARD-LGORT,
         LFGJA TYPE MARD-LFGJA,
         LFMON TYPE MARD-LFMON,
         LABST TYPE MARD-LABST,
         SPEME TYPE MARD-SPEME,
       END OF TS_MARD.

TYPES: BEGIN OF TS_MAT_DOC,
         MBLNR TYPE MSEG-MBLNR,
         MJAHR TYPE MSEG-MJAHR,
         ZEILE TYPE MSEG-ZEILE,
         BWART TYPE MSEG-BWART,
         MATNR TYPE MSEG-MATNR,
         WERKS TYPE MSEG-WERKS,
         LGORT TYPE MSEG-LGORT,
         BUDAT TYPE MKPF-BUDAT,
         SHKZG TYPE MSEG-SHKZG,
         MENGE TYPE MSEG-MENGE,
         MEINS TYPE MSEG-MEINS,
       END OF TS_MAT_DOC.

TYPES: BEGIN OF TS_SSTK_UPD,
         MATNR TYPE MARD-MATNR,
         WERKS TYPE MARD-WERKS,
         LGORT TYPE MARD-LGORT,
         LABST TYPE MARD-LABST,
         SPEME TYPE MARD-SPEME,
         KALAB TYPE MSKA-KALAB,
       END OF TS_SSTK_UPD.

TYPES: BEGIN OF TS_SSTK_DISP.
         INCLUDE TYPE ZSDSMMS039.
TYPES: END OF TS_SSTK_DISP.

TYPES: BEGIN OF TS_SO_DISP,
         VBELN TYPE VBEP-VBELN,
         POSNR TYPE VBEP-POSNR,
         ETENR TYPE VBEP-ETENR,
         EDATU TYPE VBEP-EDATU,
         MATNR TYPE VBAP-MATNR,
         WERKS TYPE VBAP-WERKS,
         LGORT TYPE VBAP-LGORT,
         BMENG TYPE VBEP-BMENG,
         VRKME TYPE VBEP-VRKME,
       END OF TS_SO_DISP.

TYPES: BEGIN OF TS_MSKA,
         MATNR TYPE MSKA-MATNR,
         WERKS TYPE MSKA-WERKS,
         LGORT TYPE MSKA-LGORT,
         CHARG TYPE MSKA-CHARG,
         VBELN TYPE MSKA-VBELN,
         POSNR TYPE MSKA-POSNR,
         LFGJA TYPE MSKA-LFGJA,
         LFMON TYPE MSKA-LFMON,
         KALAB TYPE MSKA-KALAB,
       END OF TS_MSKA.

TYPES: BEGIN OF TS_PO,
         EBELN TYPE EKET-EBELN,
         EBELP TYPE EKET-EBELP,
         ETENR TYPE EKET-ETENR,
         EINDT TYPE EKET-EINDT,
         MATNR TYPE EKPO-MATNR,
         WERKS TYPE EKPO-WERKS,
         LGORT TYPE EKPO-LGORT,
         MENGE TYPE EKET-MENGE,
         WEMNG TYPE EKET-WEMNG,
         MEINS TYPE EKPO-MEINS,
       END OF TS_PO.

TYPES: BEGIN OF TS_DO,
         VBELN_SO  TYPE VBEP-VBELN,
         POSNR_SO  TYPE VBEP-POSNR,
         ETENR_SO  TYPE VBEP-POSNR,
         VBELN_DO  TYPE LIPS-VBELN,
         POSNR_DO  TYPE LIPS-POSNR,
         LFDAT     TYPE LIKP-LFDAT,
         MATNR     TYPE LIPS-MATNR,
         WERKS     TYPE LIPS-WERKS,
         LGORT     TYPE LIPS-LGORT,
         DLVQTY_BU TYPE VBEP-DLVQTY_BU,
         MEINS     TYPE VBEP-MEINS,
       END OF TS_DO.

TYPES: BEGIN OF TS_RESB,
         RSNUM TYPE RESB-RSNUM,
         RSPOS TYPE RESB-RSPOS,
         RSART TYPE RESB-RSART,
         MATNR TYPE RESB-MATNR,
         WERKS TYPE RESB-WERKS,
         LGORT TYPE RESB-LGORT,
         BDTER TYPE RESB-BDTER,
         BDMNG TYPE RESB-BDMNG,
         MEINS TYPE RESB-MEINS,
       END OF TS_RESB.

TYPES: BEGIN OF TS_STOCK.
         INCLUDE STRUCTURE ZSDSMMT021.
TYPES: END OF TS_STOCK.

TYPES: TT_SO_DISP TYPE SORTED TABLE OF TS_SO_DISP
                  WITH UNIQUE KEY VBELN
                                  POSNR
                                  ETENR.

TYPES: TT_SSTK_UPD TYPE SORTED TABLE OF TS_SSTK_UPD
                   WITH UNIQUE KEY MATNR
                                   WERKS
                                   LGORT.

TYPES: TT_SSTK_DISP TYPE SORTED TABLE OF TS_SSTK_DISP
                    WITH UNIQUE KEY DATE
                                    MATNR.

TYPES: TT_MARD TYPE SORTED TABLE OF TS_MARD
               WITH UNIQUE KEY MATNR
                               WERKS
                               LGORT
                               LFGJA
                               LFMON.

TYPES TT_MAT_DOC TYPE SORTED TABLE OF TS_MAT_DOC
                 WITH UNIQUE KEY MBLNR
                                 MJAHR
                                 ZEILE.

TYPES: TT_MSKA TYPE SORTED TABLE OF TS_MSKA
               WITH UNIQUE KEY MATNR
                               WERKS
                               LGORT
                               CHARG
                               VBELN
                               POSNR
                               LFGJA
                               LFMON.

TYPES: TT_PO TYPE SORTED TABLE OF TS_PO
             WITH UNIQUE KEY EBELN
                             EBELP
                             ETENR.

TYPES: TT_DO TYPE SORTED TABLE OF TS_DO
             WITH UNIQUE KEY VBELN_SO
                             POSNR_SO
                             ETENR_SO
                             VBELN_DO
                             POSNR_DO.

TYPES: TT_RESB TYPE SORTED TABLE OF TS_RESB
               WITH UNIQUE KEY RSNUM
                               RSPOS
                               RSART.

TYPES: TT_STOCK  TYPE STANDARD TABLE OF TS_STOCK.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.
TYPES: TT_DFIES  TYPE STANDARD TABLE OF DFIES.
TYPES: TT_SEARCH TYPE STANDARD TABLE OF TS_SEARCH.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE CHAR1    VALUE 'X',
  GC_TCODE TYPE SY-TCODE VALUE 'ZSDSMM034',
  GC_REPID TYPE SY-REPID VALUE 'ZSDSMMR0450'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: GT_RESULT     TYPE TT_RESULT                             ##NEEDED.
DATA: GT_SSTK_UPD   TYPE TT_SSTK_UPD                           ##NEEDED.
DATA: GT_SSTK_DISP  TYPE TT_SSTK_DISP                          ##NEEDED.
DATA: GT_STOCK      TYPE TT_STOCK                              ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA: GS_PERIOD     TYPE TS_PERIOD                             ##NEEDED.
DATA: GS_STOCK_UPD  TYPE ZSDSMMT021                            ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA: GRT_MAT_TYPE          TYPE RANGE OF MARA-MTART           ##NEEDED.
DATA: GRT_UR_SLOC           TYPE RANGE OF MARD-LGORT           ##NEEDED.
DATA: GRT_INSU_SLOC         TYPE RANGE OF MARD-LGORT           ##NEEDED.
DATA: GRT_RECEIVE_MVMNT     TYPE RANGE OF MSEG-BWART           ##NEEDED.
DATA: GRT_SALE_RESULT_MVMNT TYPE RANGE OF MSEG-BWART           ##NEEDED.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

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
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-A06.
  PARAMETERS: RB_UPD  RADIOBUTTON GROUP G_2 USER-COMMAND DUMMY DEFAULT 'X'.
  PARAMETERS: RB_DISP RADIOBUTTON GROUP G_2.
SELECTION-SCREEN END OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-A01.
  SELECT-OPTIONS: S_MTART FOR MARA-MTART  NO INTERVALS.
  SELECT-OPTIONS: S_MATKL FOR MARA-MATKL.
  SELECT-OPTIONS: S_MATNR FOR MARA-MATNR.
  SELECT-OPTIONS: S_PH1   FOR MARA-PRDHA+0(5) NO INTERVALS.
  SELECT-OPTIONS: S_PH2   FOR MARA-PRDHA+5(5) NO INTERVALS.
  SELECT-OPTIONS: S_PH3   FOR MARA-PRDHA+10   NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-A02.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      P_M RADIOBUTTON GROUP G_1  USER-COMMAND DUMMY2  ##SEL_WRONG.
*   Text-a03: Monthly
    SELECTION-SCREEN COMMENT 4(7) TEXT-A03 FOR FIELD P_M.
    SELECT-OPTIONS:
      S_MONTH FOR MCS0-SPMON  NO-EXTENSION MODIF ID MTH.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      P_W RADIOBUTTON GROUP G_1 ##SEL_WRONG.
*   Text-a04: Weekly
    SELECTION-SCREEN COMMENT 4(7) TEXT-A04 FOR FIELD P_W.
    SELECT-OPTIONS:
      S_WEEK FOR MCS0-SPWOC NO-EXTENSION MODIF ID WEK.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      P_D RADIOBUTTON GROUP G_1 DEFAULT 'X' ##SEL_WRONG.
*   Text-a05: Daily
    SELECTION-SCREEN COMMENT 4(7) TEXT-A05 FOR FIELD P_D.
    SELECT-OPTIONS:
      S_DATE FOR ZSDSMMT021-ZZDATE NO-EXTENSION MODIF ID DAY.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_CONSTANT.
  PERFORM F_DEFAULT_SELSCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SET_SELECTION_SCREEN_FORMAT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_MONTH-LOW.
  PERFORM F_LIST_MONTHS CHANGING S_MONTH-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_MONTH-HIGH.
  PERFORM F_LIST_MONTHS CHANGING S_MONTH-HIGH.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_WEEK-LOW.
  PERFORM F_LIST_WEEKS CHANGING S_WEEK-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_WEEK-HIGH.
  PERFORM F_LIST_WEEKS CHANGING S_WEEK-HIGH.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PH1-LOW.
  PERFORM F_F4IF_INT_TABLE_VALUE_REQUEST USING 'PH1'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PH2-LOW.
  PERFORM F_F4IF_INT_TABLE_VALUE_REQUEST USING 'PH2'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PH3-LOW.
  PERFORM F_F4IF_INT_TABLE_VALUE_REQUEST USING 'PH3'.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_GLOB_VAR.
  PERFORM F_MODIFY_SELOPT.

  IF RB_UPD EQ ABAP_TRUE.
    PERFORM F_GET_DATA_UPDATE.
    PERFORM F_PREPARE_UPDATE.
    IF GT_STOCK IS INITIAL.
*     Message: No data found.
      MESSAGE S001(ZSDSCA01).
      RETURN.
    ENDIF.

  ELSEIF RB_DISP EQ ABAP_TRUE.
    PERFORM F_GET_DATA_DISPLAY.
    PERFORM F_PREPARE_RESULT.
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
  IF RB_UPD EQ ABAP_TRUE.
    PERFORM F_UPDATE_STOCK_BY_DATE.
  ELSEIF RB_DISP EQ ABAP_TRUE.
*   Display Processing Result
    PERFORM F_DISPLAY_RESULT USING GT_RESULT.
  ENDIF.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
  CONSTANTS:
    GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS039',
    GC_HEADER_HEIGHT_1 TYPE  I        VALUE 0,
    GC_ALV_HEIGHT_1    TYPE  I        VALUE 100.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on T-code
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
*&---------------------------------------------------------------------*
*& Form F_GET_DATA_UPDATE
*&---------------------------------------------------------------------*
*& Get Data for Update
*&---------------------------------------------------------------------*
FORM F_GET_DATA_UPDATE.
  PERFORM F_GET_MATERIAL_STOCK.
  PERFORM F_GET_SO_STOCK_UPDATE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MODIFY_SELOPT
*&---------------------------------------------------------------------*
*& Modify Select Options
*&---------------------------------------------------------------------*
FORM F_MODIFY_SELOPT .
  LOOP AT S_PH1 ASSIGNING FIELD-SYMBOL(<L_PH1>).
    <L_PH1>-OPTION  = 'CP'.
    <L_PH1>-LOW     = |{ <L_PH1>-LOW }*|.
  ENDLOOP.

  LOOP AT S_PH2 ASSIGNING FIELD-SYMBOL(<L_PH2>).
    <L_PH2>-OPTION  = 'CP'.
    <L_PH2>-LOW     = |*{ <L_PH2>-LOW }*|.
  ENDLOOP.

  LOOP AT S_PH3 ASSIGNING FIELD-SYMBOL(<L_PH3>).
    <L_PH3>-OPTION  = 'CP'.
    <L_PH3>-LOW     = |*{ <L_PH3>-LOW }*|.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_UPDATE
*&---------------------------------------------------------------------*
*& Prepare Update Data
*&---------------------------------------------------------------------*
FORM F_PREPARE_UPDATE.
  DATA: LT_STOCK  LIKE GT_STOCK.
  DATA: LS_STOCK  LIKE LINE OF GT_STOCK.

* Show progress
* Text-p03 : Preparing Update Table ZSDSMMT021 . . .
  MC_SHOW_PROGRESS 10 TEXT-P03.

  IF NOT GT_SSTK_UPD IS INITIAL.
    SELECT FROM ZSDSMMT021
    FIELDS *
    FOR ALL ENTRIES IN @GT_SSTK_UPD
    WHERE MATNR   EQ @GT_SSTK_UPD-MATNR
      AND WERKS   EQ @GT_SSTK_UPD-WERKS
      AND LGORT   EQ @GT_SSTK_UPD-LGORT
      AND ZZDATE  EQ @SY-DATUM
    INTO TABLE @LT_STOCK.
  ENDIF.

  LOOP AT GT_SSTK_UPD ASSIGNING FIELD-SYMBOL(<L_SSTK>).
    CLEAR LS_STOCK.
    LS_STOCK-MATNR  = <L_SSTK>-MATNR.
    LS_STOCK-WERKS  = <L_SSTK>-WERKS.
    LS_STOCK-LGORT  = <L_SSTK>-LGORT.
    LS_STOCK-ZZDATE = SY-DATUM.
    LS_STOCK-LABST  = <L_SSTK>-LABST.
    LS_STOCK-SPEME  = <L_SSTK>-SPEME.
    LS_STOCK-KALAB  = <L_SSTK>-KALAB.

    "Existing found
    READ TABLE LT_STOCK ASSIGNING FIELD-SYMBOL(<L_STOCK_READ>)
    WITH KEY  MATNR   = <L_SSTK>-MATNR
              WERKS   = <L_SSTK>-WERKS
              LGORT   = <L_SSTK>-LGORT
              ZZDATE  = SY-DATUM.
    IF SY-SUBRC NE 0.
      LS_STOCK-ZCRT_DATE  = SY-DATUM.
      LS_STOCK-ZCRT_TIME  = SY-UZEIT.
      LS_STOCK-ZCRT_USER  = SY-UNAME.
      LS_STOCK-ZCRT_PGM   = SY-CPROG.
    ELSE.
      LS_STOCK-ZCRT_DATE  = <L_STOCK_READ>-ZCRT_DATE.
      LS_STOCK-ZCRT_TIME  = <L_STOCK_READ>-ZCRT_TIME.
      LS_STOCK-ZCRT_USER  = <L_STOCK_READ>-ZCRT_USER.
      LS_STOCK-ZCRT_PGM   = <L_STOCK_READ>-ZCRT_PGM.
      LS_STOCK-ZUPD_DATE  = SY-DATUM.
      LS_STOCK-ZUPD_TIME  = SY-UZEIT.
      LS_STOCK-ZUPD_USER  = SY-UNAME.
      LS_STOCK-ZUPD_PGM   = SY-CPROG.
    ENDIF.
    APPEND LS_STOCK TO GT_STOCK.
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
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.
** No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

  GF_SECOND_ALV_1 = SPACE.

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
** Call ALV Screen
  CALL SCREEN 9000.
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
      WHEN 'DATE'.
*       Text-c01 : Date
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'WEEK'.
*       Text-c02 : Week
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MONTH'.
*       Text-c03 : Month
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MATNR'.
*       Text-c04 : Material
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BEGIN_STOCK'.
*       Text-c05 : Begin Stock
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'UR_STOCK'.
*       Text-c06 : UR Sloc
        LF_TEXT                = TEXT-C06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SO_STOCK'.
*       Text-c07 : Sales Order Stock
        LF_TEXT                = TEXT-C07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'INSU_STOCK'.
*       Text-c08 : UR INSU Sloc
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'BLOCKED_STOCK'.
*       Text-c09 : Blocked Stock
        LF_TEXT                = TEXT-C09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'PO_QTY'.
*       Text-c10 : PO
        LF_TEXT                = TEXT-C10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'RECEIVED_QTY'.
*       Text-c11 : Received
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CONFIRM_SO_QTY'.
*       Text-c12 : Sales Order
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'DLV_QTY'.
*       Text-c13 : DO
        LF_TEXT                = TEXT-C13.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SALE_RESULT_QTY'.
*       Text-c14 : Sale Result
        LF_TEXT                = TEXT-C14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'RESERVED_QTY'.
*       Text-c15 : Reservation
        LF_TEXT                = TEXT-C15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'ADJUST_QTY'.
*       Text-c16 : Adjustment
        LF_TEXT                = TEXT-C16.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'END_STOCK'.
*       Text-c17 : End Stock
        LF_TEXT                = TEXT-C17.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EXPECT_END_STCK'.
*       Text-c18 : Expected End Stock
        LF_TEXT                = TEXT-C18.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'EXPECT_END_SO_PO'.
*       Text-c19 : Expected End Stock with SO and PO
        LF_TEXT                = TEXT-C19.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
    ENDCASE.
  ENDLOOP.

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
*&---------------------------------------------------------------------*
*& Form F_INIT_GLOB_VAR
*&---------------------------------------------------------------------*
*& Initialize Global Variable
*&---------------------------------------------------------------------*
FORM F_INIT_GLOB_VAR .
  CLEAR:  GT_SSTK_UPD,
          GT_SSTK_DISP,
          GT_RESULT,
          GT_STOCK.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_F4IF_INT_TABLE_VALUE_REQUEST USING UF_FIELD  TYPE CHAR5.
  TYPES: BEGIN OF LTS_T179,
           PRODH TYPE T179-PRODH,
         END OF LTS_T179.

  DATA : LF_RETFIELD    TYPE DFIES-FIELDNAME,
         LF_DYNPROFIELD TYPE  HELP_INFO-DYNPROFLD.

  DATA : LT_RETURN    TYPE STANDARD TABLE OF DDSHRETVAL,
         LT_FIELD_TAB TYPE STANDARD TABLE OF DFIES,
         LT_SEARCH    TYPE STANDARD TABLE OF TS_SEARCH,
         LS_SEARCH    LIKE LINE OF LT_SEARCH,
         LT_T179      TYPE STANDARD TABLE OF LTS_T179,
         LS_T179      LIKE LINE OF LT_T179.


  SELECT PRODH
  FROM T179
  INTO TABLE LT_T179
  WHERE STUFE EQ '3'.

  CLEAR : LT_SEARCH[], LS_SEARCH.
  LOOP AT LT_T179 INTO LS_T179.
    IF UF_FIELD EQ 'PH1'.
      LS_SEARCH-PRODH = LS_T179-PRODH+0(5).
      LS_SEARCH-LEVEL = '1'.
    ELSEIF UF_FIELD EQ 'PH2'.
      LS_SEARCH-PRODH = LS_T179-PRODH+5(5).
      LS_SEARCH-LEVEL = '2'.
    ELSEIF UF_FIELD EQ 'PH3'.
      LS_SEARCH-PRODH = LS_T179-PRODH+10(8).
      LS_SEARCH-LEVEL = '3'.
    ENDIF.
    APPEND LS_SEARCH TO LT_SEARCH.
    CLEAR : LS_T179,LS_SEARCH.
  ENDLOOP.

  SORT LT_SEARCH.
  DELETE ADJACENT DUPLICATES FROM LT_SEARCH COMPARING ALL FIELDS.

  LF_RETFIELD    = 'PRODH'.
  LF_DYNPROFIELD = UF_FIELD.

  PERFORM F_GET_FIELDS_OF_VALUE_TAB TABLES LT_SEARCH
                                           LT_FIELD_TAB
                                  CHANGING LF_RETFIELD.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      RETFIELD        = LF_RETFIELD
*     PVALKEY         = ' '
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = LF_DYNPROFIELD
*     STEPL           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      VALUE_ORG       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     MARK_TAB        =
*     IMPORTING
*     USER_RESET      =
    TABLES
      VALUE_TAB       = LT_SEARCH
      FIELD_TAB       = LT_FIELD_TAB
      RETURN_TAB      = LT_RETURN
      "dynpfld_mapping = lt_dynpfld_mapping
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0  ##NEEDED.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " F_F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*& Form F_GET_FIELDS_OF_VALUE_TAB
*&---------------------------------------------------------------------*
*& Get Fields of Value Tab
*&---------------------------------------------------------------------*
FORM F_GET_FIELDS_OF_VALUE_TAB  TABLES T_VALUE  TYPE TT_SEARCH  ##NEEDED
                                     T_FIELD  TYPE TT_DFIES
                              CHANGING CF_RETFIELD TYPE DFIES-FIELDNAME.
  DATA: LS_FIELD        LIKE LINE OF T_FIELD.
  DATA: LF_HLP(61).
  DATA: LF_OFFSET       TYPE DFIES-OFFSET.
  DATA: LF_DFIES_ZWI    TYPE DFIES.
  DATA: LF_DTELINFO_WA  TYPE DTELINFO.
  DATA: LF_TABNAME      TYPE DD03P-TABNAME.
  DATA: LF_FIELDNAME    TYPE DFIES-LFIELDNAME.
  FIELD-SYMBOLS: <L_VALUE>  TYPE ANY.
  DATA: LF_INDEX LIKE SY-INDEX.
  DATA: LF_N(4) TYPE N.

  DESCRIBE FIELD T_VALUE HELP-ID LF_HLP.
  DO.
    LF_INDEX = SY-INDEX.
    ASSIGN COMPONENT LF_INDEX OF STRUCTURE T_VALUE TO <L_VALUE>.
    IF SY-SUBRC NE 0 .
      EXIT.
    ENDIF.
    DESCRIBE FIELD <L_VALUE> HELP-ID LF_HLP.
    SPLIT LF_HLP AT '-' INTO LF_TABNAME
                             LF_FIELDNAME.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        TABNAME        = LF_TABNAME
        LFIELDNAME     = LF_FIELDNAME
        ALL_TYPES      = 'X'
      IMPORTING
*       X030L_WA       =
*       DDOBJTYPE      =
        DFIES_WA       = LF_DFIES_ZWI
*        TABLES
*       DFIES_TAB      = DFIES_ZWI
      EXCEPTIONS
        NOT_FOUND      = 1
        INTERNAL_ERROR = 2
        OTHERS         = 3.
    CHECK SY-SUBRC = 0.
    DESCRIBE DISTANCE BETWEEN T_VALUE AND <L_VALUE>
             INTO LF_DFIES_ZWI-OFFSET IN BYTE MODE.
    CLEAR LF_DFIES_ZWI-TABNAME.
    LF_DFIES_ZWI-POSITION = LF_INDEX.
    LF_N = LF_INDEX.
    CONCATENATE 'F' LF_N INTO LF_DFIES_ZWI-FIELDNAME.
    LF_DFIES_ZWI-MASK+2(1) = 'X'.         "Rollname für F1-Hilfe verantw.
*   Das Flag F4-Available muß jetzt aber aus dem DTEL kommen.
    CLEAR: LF_DFIES_ZWI-F4AVAILABL,
           LF_DTELINFO_WA.
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        TABNAME     = LF_DFIES_ZWI-ROLLNAME
        ALL_TYPES   = 'X'
      IMPORTING
        DTELINFO_WA = LF_DTELINFO_WA
      EXCEPTIONS
        OTHERS      = 0.
    LF_DFIES_ZWI-F4AVAILABL = LF_DTELINFO_WA-F4AVAILABL.

    IF LF_INDEX = 1.
      LF_DFIES_ZWI-REPTEXT     =  'Product Hierarchy'(T01).
      LF_DFIES_ZWI-SCRTEXT_S   =  'Product Hierarchy'(T01).
      LF_DFIES_ZWI-SCRTEXT_M   =  'Product Hierarchy'(T01).
      LF_DFIES_ZWI-SCRTEXT_L   =  'Product Hierarchy'(T01).
    ELSE.
      LF_DFIES_ZWI-REPTEXT     =  'Level'(T02).
      LF_DFIES_ZWI-SCRTEXT_S   =  'Level'(T02).
      LF_DFIES_ZWI-SCRTEXT_M   =  'Level'(T02).
      LF_DFIES_ZWI-SCRTEXT_L   =  'Level'(T02).
    ENDIF.

    APPEND LF_DFIES_ZWI TO T_FIELD.
  ENDDO.
  ASSIGN COMPONENT CF_RETFIELD OF STRUCTURE T_VALUE TO <L_VALUE>.
  DESCRIBE DISTANCE BETWEEN T_VALUE AND <L_VALUE>
           INTO LF_OFFSET IN BYTE MODE.
  READ TABLE T_FIELD INTO LS_FIELD
  WITH KEY OFFSET = LF_OFFSET.
  IF SY-SUBRC = 0.
    CF_RETFIELD = LS_FIELD-FIELDNAME.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN ##NEEDED.

  DATA:
    LF_DIFF TYPE I.

  CHECK RB_DISP EQ ABAP_TRUE.

  CASE GC_TRUE.
*   ------------
*   Monthly
*   ------------
    WHEN P_M.
      IF S_MONTH[] IS INITIAL.
        SET CURSOR FIELD 'S_MONTH-LOW'.
*       Text-e03: Please fill reporting period.
        MESSAGE E000(38) WITH TEXT-E03.
      ENDIF.

      READ TABLE S_MONTH INTO S_MONTH INDEX 1.
      IF SY-SUBRC = 0.
        IF S_MONTH-LOW(4) < '1959'.
*         Text-e01: Year must > 1959 (Input format is YYYYMM)
          MESSAGE E000(38) WITH TEXT-E01.
        ENDIF.
      ENDIF.

      PERFORM F_ASSIGN_PERIOD_MONTH  USING  S_MONTH[]
                                   CHANGING GS_PERIOD.

*   ------------
*   Weekly
*   ------------
    WHEN P_W.
      IF S_WEEK[] IS INITIAL.
        SET CURSOR FIELD 'S_WEEK-LOW'.
*       Text-e03: Please fill reporting period.
        MESSAGE E000(38) WITH TEXT-E03.
      ENDIF.

      PERFORM F_ASSIGN_PERIOD_WEEK  USING   S_WEEK[]
                                   CHANGING GS_PERIOD.

*   ------------
*   Daily
*   ------------
    WHEN P_D.
      IF S_DATE[] IS INITIAL.
        SET CURSOR FIELD 'S_DATE-LOW'.
*       Text-e03: Please fill reporting period.
        MESSAGE E000(38) WITH TEXT-E03.
      ENDIF.
      READ TABLE S_DATE INTO S_DATE INDEX 1.
      IF SY-SUBRC = 0.
        IF S_DATE-HIGH IS NOT INITIAL.
          LF_DIFF = S_DATE-HIGH - S_DATE-LOW.
          IF LF_DIFF > 31 ##NUMBER_OK.
*           Text-e02: Please Input Date <= 31 days
            MESSAGE E000(38) WITH TEXT-E02.
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM F_ASSIGN_PERIOD_DAILY  USING   S_DATE[]
                                   CHANGING GS_PERIOD.

  ENDCASE.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ASSIGN_PERIOD_DAILY
*----------------------------------------------------------------------*
*  Assign Period from Daily
*----------------------------------------------------------------------*
FORM F_ASSIGN_PERIOD_DAILY  USING  UT_DATE TYPE TT_DAY_RANGE
                          CHANGING CS_PERIOD TYPE TS_PERIOD.

  DATA:
    LF_DATUM_FROM TYPE  SY-DATUM,
    LF_DATUM_TO   TYPE  SY-DATUM.


* Initialize Output
  CLEAR: CS_PERIOD.

  READ TABLE UT_DATE ASSIGNING FIELD-SYMBOL(<L_DATE>)
                      INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LF_DATUM_FROM = <L_DATE>-LOW.
  IF <L_DATE>-HIGH IS INITIAL.
    LF_DATUM_TO = <L_DATE>-LOW.
  ELSE.
    LF_DATUM_TO = <L_DATE>-HIGH.
  ENDIF.

  CS_PERIOD-BEGDA  = LF_DATUM_FROM.
  CS_PERIOD-ENDDA  = LF_DATUM_TO.

* Assign Periods
  WHILE LF_DATUM_FROM LE LF_DATUM_TO.
    APPEND VALUE #( SIGN = 'I'
                    OPTION = 'BT'
                    LOW    = LF_DATUM_FROM
                    HIGH   = LF_DATUM_FROM )
           TO CS_PERIOD-PERIOD.
    APPEND VALUE #( SIGN   = 'I'
                    OPTION = 'EQ'
                    LOW    = LF_DATUM_FROM )
           TO CS_PERIOD-PERIOD_FIRST_DATE.
    LF_DATUM_FROM = LF_DATUM_FROM + 1.
  ENDWHILE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_assign_period_month
*----------------------------------------------------------------------*
*  Assig Period data from Monthly
*----------------------------------------------------------------------*
FORM F_ASSIGN_PERIOD_MONTH  USING  UT_MONTH TYPE TT_MONTH_RANGE
                          CHANGING CS_PERIOD TYPE TS_PERIOD.

  DATA:
    LF_DATUM_FROM TYPE  SY-DATUM,
    LF_DATUM_TO   TYPE  SY-DATUM,
    LF_MONTH_FROM TYPE  SPMON,
    LF_MONTH_TO   TYPE  SPMON.


* Initialize Output
  CLEAR: CS_PERIOD.

  READ TABLE UT_MONTH ASSIGNING FIELD-SYMBOL(<L_MONTH>)
                      INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Start date
  CS_PERIOD-BEGDA = <L_MONTH>-LOW && '01'.
  LF_MONTH_FROM   = <L_MONTH>-LOW.

  IF <L_MONTH>-HIGH IS INITIAL.
    LF_DATUM_FROM = <L_MONTH>-LOW && '01'.
  ELSE.
    LF_DATUM_FROM = <L_MONTH>-HIGH && '01'.
  ENDIF.
  LF_MONTH_TO = LF_DATUM_FROM(6).

* Assign End date
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = LF_DATUM_FROM
    IMPORTING
      LAST_DAY_OF_MONTH = CS_PERIOD-ENDDA
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Asign Periods
  WHILE LF_MONTH_FROM LE LF_MONTH_TO.
    LF_DATUM_FROM = LF_MONTH_FROM && '01'.
*   Assign End date
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = LF_DATUM_FROM
      IMPORTING
        LAST_DAY_OF_MONTH = LF_DATUM_TO
      EXCEPTIONS
        DAY_IN_NO_DATE    = 1
        OTHERS            = 2.
    IF SY-SUBRC <> 0.
      LF_DATUM_TO = LF_DATUM_FROM.
    ENDIF.

    APPEND VALUE #( SIGN   = 'I'
                    OPTION = 'BT'
                    LOW    = LF_DATUM_FROM
                    HIGH   = LF_DATUM_TO )
           TO CS_PERIOD-PERIOD.
    APPEND VALUE #( SIGN   = 'I'
                    OPTION = 'EQ'
                    LOW    = LF_DATUM_FROM )
           TO CS_PERIOD-PERIOD_FIRST_DATE.

*   Next Month
    IF LF_MONTH_FROM+4(2) EQ '12'.
      LF_MONTH_FROM(4)   = LF_MONTH_FROM(4) + 1.
      LF_MONTH_FROM+4(2) = '01'.
    ELSE.
      LF_MONTH_FROM = LF_MONTH_FROM + 1.
    ENDIF.

  ENDWHILE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ASSIGN_PERIOD_WEEK
*----------------------------------------------------------------------*
*  Assign Period data from Weekly
*----------------------------------------------------------------------*
FORM F_ASSIGN_PERIOD_WEEK  USING  UT_WEEK  TYPE  TT_WEEK_RANGE
                         CHANGING CS_PERIOD TYPE  TS_PERIOD.

  DATA:
    LF_DATUM_FROM TYPE  SY-DATUM,
    LF_DATUM_TO   TYPE  SY-DATUM,
    LF_WEEK_FROM  TYPE  SPWOC,
    LF_WEEK_TO    TYPE  SPWOC.


* Initialize Output
  CLEAR: CS_PERIOD.

  READ TABLE UT_WEEK ASSIGNING FIELD-SYMBOL(<L_WEEK>)
                      INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LF_WEEK_FROM   = <L_WEEK>-LOW.
  IF <L_WEEK>-HIGH IS INITIAL.
    LF_WEEK_TO = <L_WEEK>-LOW.
  ELSE.
    LF_WEEK_TO = <L_WEEK>-HIGH.
  ENDIF.

* Assign Start date
  PERFORM F_GET_WEEK_PERIOD  USING  LF_WEEK_FROM
                           CHANGING LF_DATUM_FROM
                                    LF_DATUM_TO.
  CS_PERIOD-BEGDA = LF_DATUM_FROM.

* Assign End date
  IF LF_WEEK_TO NE LF_WEEK_FROM.
    PERFORM F_GET_WEEK_PERIOD  USING  LF_WEEK_TO
                             CHANGING LF_DATUM_FROM
                                      LF_DATUM_TO.
  ENDIF.
  CS_PERIOD-ENDDA = LF_DATUM_TO.

* Assign Week Periods
  WHILE LF_WEEK_FROM LE LF_WEEK_TO.

    PERFORM F_GET_WEEK_PERIOD  USING  LF_WEEK_FROM
                             CHANGING LF_DATUM_FROM
                                      LF_DATUM_TO.

    APPEND VALUE #( SIGN = 'I'
                    OPTION = 'BT'
                    LOW    = LF_DATUM_FROM
                    HIGH   = LF_DATUM_TO )
           TO CS_PERIOD-PERIOD.
    APPEND VALUE #( SIGN = 'I'
                    OPTION = 'EQ'
                    LOW    = LF_DATUM_FROM )
           TO CS_PERIOD-PERIOD_FIRST_DATE.

    IF LF_DATUM_TO(4) NE LF_DATUM_FROM(4) OR
       LF_DATUM_TO+4(4) = '1231'.
      LF_WEEK_FROM(4) = LF_WEEK_FROM(4) + 1.
      LF_WEEK_FROM+4(2) = '01'.
    ELSE.
      LF_WEEK_FROM = LF_WEEK_FROM + 1.
    ENDIF.

  ENDWHILE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_get_week_period
*----------------------------------------------------------------------*
*  Get Week Period
*----------------------------------------------------------------------*
FORM F_GET_WEEK_PERIOD  USING  UF_WEEK  TYPE  SPWOC
                      CHANGING CF_FROM  TYPE  SY-DATUM
                               CF_TO    TYPE  SY-DATUM.
* Initialize Output
  CLEAR: CF_FROM,
         CF_TO.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      WEEK         = UF_WEEK
    IMPORTING
      DATE         = CF_FROM
    EXCEPTIONS
      WEEK_INVALID = 1
      OTHERS       = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  CF_TO = CF_FROM + 6.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MATERIAL_STOCK
*&---------------------------------------------------------------------*
*& Get Material Stock
*&---------------------------------------------------------------------*
FORM F_GET_MATERIAL_STOCK.
  DATA:
    LT_MARD   TYPE TT_MARD,
    LRT_LGORT TYPE RANGE OF MARD-LGORT,
    LF_SPMON  TYPE  SPMON  ##NEEDED.

* Show progress
* Text-p01 : Reading Material Stock data . . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

  APPEND LINES OF GRT_UR_SLOC TO LRT_LGORT.
  APPEND LINES OF GRT_INSU_SLOC  TO LRT_LGORT.

* Assign Reading Period
  IF GS_PERIOD-BEGDA+4(2) EQ '01'.
    LF_SPMON(4) = GS_PERIOD-BEGDA(4) - 1.
    LF_SPMON+4(2) = '12'.
  ELSE.
    LF_SPMON = GS_PERIOD-BEGDA(6) - 1.
  ENDIF.

* Get Current Period stock data
  SELECT A~MATNR,
         A~WERKS,
         A~LGORT,
         A~LFGJA,
         A~LFMON,
         A~LABST,
         A~SPEME
  FROM MARD AS A
  INNER JOIN MARA AS B
    ON  B~MATNR = A~MATNR
  WHERE A~MATNR IN @S_MATNR
    AND A~LGORT IN @LRT_LGORT
    AND A~LVORM IS INITIAL
    AND B~LVORM IS INITIAL
    AND B~MTART IN @S_MTART
    AND B~MATKL IN @S_MATKL
    AND B~PRDHA IN @S_PH1
    AND B~PRDHA IN @S_PH2
    AND B~PRDHA IN @S_PH3
  INTO TABLE @LT_MARD.

  LOOP AT LT_MARD ASSIGNING FIELD-SYMBOL(<L_MARD>).
    READ TABLE GT_SSTK_UPD ASSIGNING FIELD-SYMBOL(<L_SSTK>)
                       WITH KEY MATNR = <L_MARD>-MATNR
                                WERKS = <L_MARD>-WERKS
                                LGORT = <L_MARD>-LGORT
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT VALUE TS_SSTK_UPD( MATNR = <L_MARD>-MATNR
                                WERKS = <L_MARD>-WERKS
                                LGORT = <L_MARD>-LGORT )
                   INTO TABLE GT_SSTK_UPD
                   ASSIGNING <L_SSTK>.
    ENDIF.
*   Sum Qty from Mat/Plant/Sloc level
    <L_SSTK>-LABST = <L_SSTK>-LABST + <L_MARD>-LABST.
    <L_SSTK>-SPEME = <L_SSTK>-SPEME + <L_MARD>-SPEME.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SO_STOCK_UPDATE
*&---------------------------------------------------------------------*
*& Get Sales Order Stock for Update
*&---------------------------------------------------------------------*
FORM F_GET_SO_STOCK_UPDATE.
  DATA:
    LT_MSKA   TYPE TT_MSKA,
    LRT_LGORT TYPE RANGE OF MARD-LGORT,
    LF_SPMON  TYPE SPMON.
  FIELD-SYMBOLS:
    <L_MSKA>  TYPE TS_MSKA.

* Show progress
* Text-p02 : Reading Sales Order data . . .
  MC_SHOW_PROGRESS 10 TEXT-P02.

  APPEND LINES OF GRT_UR_SLOC TO LRT_LGORT.
  APPEND LINES OF GRT_INSU_SLOC  TO LRT_LGORT.

* Assign Reading Period
  IF GS_PERIOD-BEGDA+4(2) EQ '01'.
    LF_SPMON(4) = GS_PERIOD-BEGDA(4) - 1.
    LF_SPMON+4(2) = '12'.
  ELSE.
    LF_SPMON = GS_PERIOD-BEGDA(6) - 1.
  ENDIF.

* Show progress
* Text-p04 : Reading Sales Order data . . .
  MC_SHOW_PROGRESS 70 TEXT-P02.

* Get Sales Order Stock
  SELECT A~MATNR,
         A~WERKS,
         A~LGORT,
         A~CHARG,
         A~VBELN,
         A~POSNR,
         A~LFGJA,
         A~LFMON,
         A~KALAB
  FROM MSKA AS A
  INNER JOIN MARA AS B ON A~MATNR = B~MATNR
  WHERE A~MATNR IN @S_MATNR
    AND A~LGORT IN @LRT_LGORT
    AND ( ( A~LFGJA EQ @LF_SPMON(4) AND A~LFMON GE @LF_SPMON+4(2) ) OR
            A~LFGJA GT @LF_SPMON(4) )
    AND B~LVORM IS INITIAL
    AND B~MTART IN @S_MTART
    AND B~MATKL IN @S_MATKL
    AND B~PRDHA IN @S_PH1
    AND B~PRDHA IN @S_PH2
    AND B~PRDHA IN @S_PH3
  INTO TABLE @LT_MSKA.

  LOOP AT LT_MSKA ASSIGNING <L_MSKA>.
    READ TABLE GT_SSTK_UPD ASSIGNING FIELD-SYMBOL(<L_SSTK>)
                       WITH KEY MATNR = <L_MSKA>-MATNR
                                WERKS = <L_MSKA>-WERKS
                                LGORT = <L_MSKA>-LGORT
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT VALUE TS_SSTK_UPD( MATNR = <L_MSKA>-MATNR
                                WERKS = <L_MSKA>-WERKS
                                LGORT = <L_MSKA>-LGORT )
                   INTO TABLE GT_SSTK_UPD
                   ASSIGNING <L_SSTK>.
    ENDIF.
*   Sum Qty from Mat/Plant/Sloc level
    <L_SSTK>-KALAB = <L_SSTK>-KALAB + <L_MSKA>-KALAB.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UPDATE_STOCK_BY_DATE
*&---------------------------------------------------------------------*
*& Update Stock by Date
*&---------------------------------------------------------------------*
FORM F_UPDATE_STOCK_BY_DATE .
*  DATA: LT_TEST_UPD TYPE STANDARD TABLE OF TS_STOCK.  "TEST

* Show progress
* Text-p04 : Updating Table ZSDSMMT021 . . .
  MC_SHOW_PROGRESS 10 TEXT-P04.

*  MODIFY ZSDSMMT021 FROM TABLE LT_TEST_UPD. "TEST
  MODIFY ZSDSMMT021 FROM TABLE GT_STOCK.
  IF SY-SUBRC EQ 0.
    COMMIT WORK AND WAIT.
    MESSAGE TEXT-S01  TYPE 'S'.
  ELSE.
    MESSAGE TEXT-E04  TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA_DISPLAY
*&---------------------------------------------------------------------*
*& Get Data for Display
*&---------------------------------------------------------------------*
FORM F_GET_DATA_DISPLAY .
  PERFORM F_GET_STOCK_BY_DATE.
  PERFORM F_GET_PO_STOCK.
  PERFORM F_GET_SO_STOCK_DISP.
  PERFORM F_GET_DO_STOCK.
  PERFORM F_GET_RECEIVE_STOCK.
  PERFORM F_GET_SALES_RESULT_STOCK.
  PERFORM F_GET_ADJUSTMENT_STOCK.
  PERFORM F_GET_RESERVED_STOCK.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_STOCK_BY_DATE
*&---------------------------------------------------------------------*
*& Get Stock by Date
*&---------------------------------------------------------------------*
FORM F_GET_STOCK_BY_DATE .
  DATA: LRT_LGORT     TYPE RANGE OF MARD-LGORT.
  DATA: LF_UR_STOCK   TYPE ZSDSMMS039-UR_STOCK.
  DATA: LF_INSU_STOCK TYPE ZSDSMMS039-INSU_STOCK.
  DATA: LF_DATE       TYPE SCAL-DATE.
  DATA: LF_WEEK       TYPE SCAL-WEEK.

  APPEND LINES OF GRT_UR_SLOC TO LRT_LGORT.
  APPEND LINES OF GRT_INSU_SLOC  TO LRT_LGORT.

  SELECT FROM ZSDSMMT021 AS A
  INNER JOIN MARA AS B
    ON  B~MATNR = A~MATNR
  FIELDS A~*
  WHERE A~MATNR   IN @S_MATNR
    AND A~LGORT   IN @LRT_LGORT
    AND A~ZZDATE  IN @GS_PERIOD-PERIOD_FIRST_DATE
    AND B~MTART   IN @S_MTART
    AND B~MATKL   IN @S_MATKL
    AND B~PRDHA   IN @S_PH1
    AND B~PRDHA   IN @S_PH2
    AND B~PRDHA   IN @S_PH3
  INTO TABLE @GT_STOCK.

  LOOP AT GT_STOCK ASSIGNING FIELD-SYMBOL(<L_STOCK>)
    GROUP BY ( MATNR  = <L_STOCK>-MATNR
               ZZDATE = <L_STOCK>-ZZDATE )
    ASSIGNING FIELD-SYMBOL(<L_GROUP>).

    LOOP AT GROUP <L_GROUP> ASSIGNING FIELD-SYMBOL(<L_GVAL>).
      CLEAR: LF_UR_STOCK, LF_INSU_STOCK.
      CLEAR: LF_DATE, LF_WEEK.

      LF_DATE = <L_GVAL>-ZZDATE.
      CALL FUNCTION 'DATE_GET_WEEK'
        EXPORTING
          DATE         = LF_DATE
        IMPORTING
          WEEK         = LF_WEEK
        EXCEPTIONS
          DATE_INVALID = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0  ##NEEDED.
*       Implement suitable error handling here
      ENDIF.

      IF <L_GVAL>-LGORT IN GRT_UR_SLOC.
        LF_UR_STOCK   = <L_GVAL>-LABST.
      ELSEIF <L_GVAL>-LGORT IN GRT_INSU_SLOC.
        LF_INSU_STOCK = <L_GVAL>-LABST.
      ENDIF.

      READ TABLE GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>)
                        WITH KEY DATE   = <L_GVAL>-ZZDATE
                                 MATNR  = <L_GVAL>-MATNR
                        BINARY SEARCH.
      IF SY-SUBRC NE 0.
        INSERT VALUE TS_SSTK_DISP( DATE           = <L_GVAL>-ZZDATE
                                   WEEK           = LF_WEEK
                                   MONTH          = <L_GVAL>-ZZDATE+0(6)
                                   MATNR          = <L_GVAL>-MATNR
                                   BEGIN_STOCK    = LF_UR_STOCK + <L_GVAL>-KALAB + LF_INSU_STOCK + <L_GVAL>-SPEME
                                   SO_STOCK       = <L_GVAL>-KALAB
                                   BLOCKED_STOCK  = <L_GVAL>-SPEME
                                   UR_STOCK       = LF_UR_STOCK
                                   INSU_STOCK     = LF_INSU_STOCK )
                     INTO TABLE GT_SSTK_DISP
                     ASSIGNING <L_SSTK>.
      ELSE.
*       Sum Qty from Date/Mat level
        <L_SSTK>-BEGIN_STOCK    = <L_SSTK>-BEGIN_STOCK + (  LF_UR_STOCK + <L_GVAL>-KALAB + LF_INSU_STOCK + <L_GVAL>-SPEME ).
        <L_SSTK>-SO_STOCK       = <L_SSTK>-SO_STOCK + <L_GVAL>-KALAB.
        <L_SSTK>-BLOCKED_STOCK  = <L_SSTK>-BLOCKED_STOCK + <L_GVAL>-SPEME.
        IF <L_GVAL>-LGORT IN GRT_UR_SLOC.
          <L_SSTK>-UR_STOCK   = <L_SSTK>-UR_STOCK + <L_GVAL>-LABST.
        ELSEIF <L_GVAL>-LGORT IN GRT_INSU_SLOC.
          <L_SSTK>-INSU_STOCK = <L_SSTK>-INSU_STOCK + <L_GVAL>-LABST.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_list_months
*----------------------------------------------------------------------*
*  List Months
*----------------------------------------------------------------------*
FORM F_LIST_MONTHS  CHANGING CF_MONTH TYPE MCS0-SPMON.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      ACTUAL_MONTH    = SY-DATUM(6)
      START_COLUMN    = 8
      START_ROW       = 5
    IMPORTING
      SELECTED_MONTH  = CF_MONTH
    EXCEPTIONS
      MONTH_NOT_FOUND = 3
      OTHERS          = 4.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_LIST_WEEKS
*----------------------------------------------------------------------*
*  List Weeks
*----------------------------------------------------------------------*
FORM F_LIST_WEEKS  CHANGING CF_WEEK TYPE MCS0-SPWOC.

  DATA:
    LF_SELECT_DATE TYPE WORKFLDS-GKDAY,
    LF_TYPE        TYPE TPRG-PRGRS VALUE '2',
    LF_EEIND       TYPE RM06E-EEIND.


  CALL FUNCTION 'F4_DATE'
    EXPORTING
      DATE_FOR_FIRST_MONTH         = SY-DATUM
    IMPORTING
      SELECT_DATE                  = LF_SELECT_DATE
    EXCEPTIONS
      CALENDAR_BUFFER_NOT_LOADABLE = 1
      DATE_AFTER_RANGE             = 2
      DATE_BEFORE_RANGE            = 3
      DATE_INVALID                 = 4
      FACTORY_CALENDAR_NOT_FOUND   = 5
      HOLIDAY_CALENDAR_NOT_FOUND   = 6
      PARAMETER_CONFLICT           = 7
      OTHERS                       = 8.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'PERIOD_AND_DATE_CONVERT_OUTPUT'
    EXPORTING
      INTERNAL_DATE   = LF_SELECT_DATE
      INTERNAL_PERIOD = LF_TYPE
    IMPORTING
      EXTERNAL_DATE   = LF_EEIND
    EXCEPTIONS
      DATE_INVALID    = 1
      PERIODE_INVALID = 2
      OTHERS          = 3.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  CONCATENATE  LF_EEIND+3(4) LF_EEIND(2) INTO CF_WEEK.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_SELECTION_SCREEN_FORMAT
*----------------------------------------------------------------------*
*  Set Selection screen format
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'S_MTART-LOW' OR SCREEN-NAME EQ 'S_MTART-HIGH'.
      SCREEN-INPUT  = 0.
      MODIFY SCREEN.
    ENDIF.

    IF RB_DISP EQ ABAP_TRUE.
      CASE SCREEN-GROUP1.
*       -------------
*       Month Fields
*       -------------
        WHEN 'MTH'.
          IF P_M EQ GC_TRUE.
            SCREEN-INPUT = 1.
          ELSE.
            SCREEN-INPUT = 0.
          ENDIF.
          MODIFY SCREEN.

*       -------------
*       Week Fields
*       -------------
        WHEN 'WEK'.
          IF P_W EQ GC_TRUE.
            SCREEN-INPUT = 1.
          ELSE.
            SCREEN-INPUT = 0.
          ENDIF.
          MODIFY SCREEN.

*       -------------
*       Daily Fields
*       -------------
        WHEN 'DAY'.
          IF P_D EQ GC_TRUE.
            SCREEN-INPUT = 1.
          ELSE.
            SCREEN-INPUT = 0.
          ENDIF.
          MODIFY SCREEN.

      ENDCASE.

    ELSE.
      CLEAR: S_MONTH, S_MONTH[],
             S_WEEK,  S_WEEK[],
             S_DATE,  S_DATE[].

      IF SCREEN-GROUP1 EQ 'DAY' OR
         SCREEN-GROUP1 EQ 'WEK' OR
         SCREEN-GROUP1 EQ 'MTH'.

        SCREEN-INPUT  = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Initial Variables
  CASE GC_TRUE.
    WHEN P_M.
      CLEAR: S_WEEK, S_WEEK[],
             S_DATE, S_DATE[].
    WHEN P_W.
      CLEAR: S_MONTH, S_MONTH[],
             S_DATE,  S_DATE[].
    WHEN P_D.
      CLEAR: S_MONTH, S_MONTH[],
             S_WEEK,  S_WEEK[].
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PO_STOCK
*&---------------------------------------------------------------------*
*& Get PO Stock
*&---------------------------------------------------------------------*
FORM F_GET_PO_STOCK .
  DATA: LT_PO     TYPE TT_PO.
  DATA: LF_INDEX  TYPE SY-TABIX.
  DATA: LRT_EINDT TYPE RANGE OF EKET-EINDT.

  CHECK NOT GT_STOCK IS INITIAL.

  SELECT FROM EKET AS A
  INNER JOIN EKPO AS B
    ON  B~EBELN = A~EBELN
    AND B~EBELP = A~EBELP
  FIELDS  A~EBELN,
          A~EBELP,
          A~ETENR,
          A~EINDT,
          B~MATNR,
          B~WERKS,
          B~LGORT,
          A~MENGE,
          A~WEMNG,
          B~MEINS
  FOR ALL ENTRIES IN @GT_STOCK
  WHERE B~MATNR EQ @GT_STOCK-MATNR
    AND B~WERKS EQ @GT_STOCK-WERKS
    AND B~LGORT IN @GRT_UR_SLOC
    AND A~EINDT IN @GS_PERIOD-PERIOD
  INTO TABLE @LT_PO.

  LOOP AT GS_PERIOD-PERIOD_FIRST_DATE ASSIGNING FIELD-SYMBOL(<L_FIRST_DATE>).
    LF_INDEX  = SY-TABIX.

    CLEAR LRT_EINDT[].
    READ TABLE GS_PERIOD-PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>) INDEX LF_INDEX.
    IF SY-SUBRC EQ 0.
      APPEND <L_PERIOD> TO LRT_EINDT.

      LOOP AT LT_PO ASSIGNING FIELD-SYMBOL(<L_PO>) WHERE EINDT IN LRT_EINDT. "#EC CI_SORTSEQ
        READ TABLE GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>)
        WITH KEY DATE  = <L_FIRST_DATE>-LOW
                 MATNR = <L_PO>-MATNR
        BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Sum Qty from Date/Mat level
          <L_SSTK>-PO_QTY = <L_SSTK>-PO_QTY + ( <L_PO>-MENGE - <L_PO>-WEMNG ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SO_STOCK_DISP
*&---------------------------------------------------------------------*
*& Get SO Stock for Display
*&---------------------------------------------------------------------*
FORM F_GET_SO_STOCK_DISP .
  DATA: LT_SO     TYPE TT_SO_DISP.
  DATA: LF_INDEX  TYPE SY-TABIX.
  DATA: LRT_EDATU TYPE RANGE OF VBEP-EDATU.

  CHECK NOT GT_STOCK IS INITIAL.

  SELECT FROM VBEP AS A
  INNER JOIN VBAP AS B
    ON  B~VBELN = A~VBELN
    AND B~POSNR = A~POSNR
  FIELDS  A~VBELN,
          A~POSNR,
          A~ETENR,
          A~EDATU,
          B~MATNR,
          B~WERKS,
          B~LGORT,
          A~BMENG,
          A~VRKME
  FOR ALL ENTRIES IN @GT_STOCK
  WHERE B~MATNR EQ @GT_STOCK-MATNR
    AND B~WERKS EQ @GT_STOCK-WERKS
    AND A~EDATU IN @GS_PERIOD-PERIOD
  INTO TABLE @LT_SO.

  LOOP AT GS_PERIOD-PERIOD_FIRST_DATE ASSIGNING FIELD-SYMBOL(<L_FIRST_DATE>).
    LF_INDEX  = SY-TABIX.

    CLEAR LRT_EDATU[].
    READ TABLE GS_PERIOD-PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>) INDEX LF_INDEX.
    IF SY-SUBRC EQ 0.
      APPEND <L_PERIOD> TO LRT_EDATU.

      LOOP AT LT_SO ASSIGNING FIELD-SYMBOL(<L_SO>) WHERE EDATU IN LRT_EDATU. "#EC CI_SORTSEQ
        READ TABLE GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>)
        WITH KEY DATE  = <L_FIRST_DATE>-LOW
                 MATNR = <L_SO>-MATNR
        BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Sum Qty from Date/Mat level
          <L_SSTK>-CONFIRM_SO_QTY = <L_SSTK>-CONFIRM_SO_QTY + <L_SO>-BMENG.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_RECEIVE_STOCK
*&---------------------------------------------------------------------*
*& Get Receive Stock
*&---------------------------------------------------------------------*
FORM F_GET_RECEIVE_STOCK .
  DATA: LT_MAT_DOC  TYPE TT_MAT_DOC.
  DATA: LF_INDEX    TYPE SY-TABIX.
  DATA: LRT_BUDAT   TYPE RANGE OF MKPF-BUDAT.

  CHECK NOT GT_STOCK IS INITIAL.

  SELECT FROM MSEG AS A
  INNER JOIN MKPF AS B
    ON  B~MBLNR = A~MBLNR
    AND B~MJAHR = A~MJAHR
  FIELDS A~MBLNR,
         A~MJAHR,
         A~ZEILE,
         A~BWART,
         A~MATNR,
         A~WERKS,
         A~LGORT,
         B~BUDAT,
         A~SHKZG,
         A~MENGE,
         A~MEINS
  FOR ALL ENTRIES IN @GT_STOCK
  WHERE A~BWART IN @GRT_RECEIVE_MVMNT
    AND A~XAUTO IS INITIAL
    AND A~MATNR EQ @GT_STOCK-MATNR
    AND A~WERKS EQ @GT_STOCK-WERKS
    AND A~LGORT IN @GRT_UR_SLOC
    AND B~BUDAT IN @GS_PERIOD-PERIOD
  INTO TABLE @LT_MAT_DOC.

  LOOP AT GS_PERIOD-PERIOD_FIRST_DATE ASSIGNING FIELD-SYMBOL(<L_FIRST_DATE>).
    LF_INDEX  = SY-TABIX.

    CLEAR LRT_BUDAT[].
    READ TABLE GS_PERIOD-PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>) INDEX LF_INDEX.
    IF SY-SUBRC EQ 0.
      APPEND <L_PERIOD> TO LRT_BUDAT.

      LOOP AT LT_MAT_DOC ASSIGNING FIELD-SYMBOL(<L_MAT_DOC>) WHERE BUDAT IN LRT_BUDAT. "#EC CI_SORTSEQ
        READ TABLE GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>)
        WITH KEY DATE  = <L_FIRST_DATE>-LOW
                 MATNR = <L_MAT_DOC>-MATNR
        BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Sum Qty from Date/Mat level
          IF <L_MAT_DOC>-SHKZG EQ 'S'.
            <L_SSTK>-RECEIVED_QTY = <L_SSTK>-RECEIVED_QTY + <L_MAT_DOC>-MENGE.
          ELSE.
            <L_SSTK>-RECEIVED_QTY = <L_SSTK>-RECEIVED_QTY - <L_MAT_DOC>-MENGE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SALES_RESULT_STOCK
*&---------------------------------------------------------------------*
*& Get Sale Result Stock
*&---------------------------------------------------------------------*
FORM F_GET_SALES_RESULT_STOCK .
  DATA: LT_MAT_DOC  TYPE TT_MAT_DOC.
  DATA: LF_INDEX    TYPE SY-TABIX.
  DATA: LRT_BUDAT   TYPE RANGE OF MKPF-BUDAT.

  CHECK NOT GT_STOCK IS INITIAL.

  SELECT FROM MSEG AS A
  INNER JOIN MKPF AS B
    ON  B~MBLNR = A~MBLNR
    AND B~MJAHR = A~MJAHR
  FIELDS A~MBLNR,
         A~MJAHR,
         A~ZEILE,
         A~BWART,
         A~MATNR,
         A~WERKS,
         A~LGORT,
         B~BUDAT,
         A~SHKZG,
         A~MENGE,
         A~MEINS
  FOR ALL ENTRIES IN @GT_STOCK
  WHERE A~BWART IN @GRT_SALE_RESULT_MVMNT
    AND A~XAUTO IS INITIAL
    AND A~MATNR EQ @GT_STOCK-MATNR
    AND A~WERKS EQ @GT_STOCK-WERKS
    AND A~LGORT IN @GRT_UR_SLOC
    AND B~BUDAT IN @GS_PERIOD-PERIOD
  INTO TABLE @LT_MAT_DOC.

  LOOP AT GS_PERIOD-PERIOD_FIRST_DATE ASSIGNING FIELD-SYMBOL(<L_FIRST_DATE>).
    LF_INDEX  = SY-TABIX.

    CLEAR LRT_BUDAT[].
    READ TABLE GS_PERIOD-PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>) INDEX LF_INDEX.
    IF SY-SUBRC EQ 0.
      APPEND <L_PERIOD> TO LRT_BUDAT.

      LOOP AT LT_MAT_DOC ASSIGNING FIELD-SYMBOL(<L_MAT_DOC>) WHERE BUDAT IN LRT_BUDAT. "#EC CI_SORTSEQ
        READ TABLE GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>)
        WITH KEY DATE  = <L_FIRST_DATE>-LOW
                 MATNR = <L_MAT_DOC>-MATNR
        BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Sum Qty from Date/Mat level
          IF <L_MAT_DOC>-SHKZG EQ 'S'.
            <L_SSTK>-SALE_RESULT_QTY = <L_SSTK>-SALE_RESULT_QTY - <L_MAT_DOC>-MENGE.
          ELSE.
            <L_SSTK>-SALE_RESULT_QTY = <L_SSTK>-SALE_RESULT_QTY + <L_MAT_DOC>-MENGE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_ADJUSTMENT_STOCK
*&---------------------------------------------------------------------*
*& Get Adjustment Stock
*&---------------------------------------------------------------------*
FORM F_GET_ADJUSTMENT_STOCK .
  DATA: LT_MAT_DOC  TYPE TT_MAT_DOC.
  DATA: LF_INDEX    TYPE SY-TABIX.
  DATA: LRT_BUDAT   TYPE RANGE OF MKPF-BUDAT.

  CHECK NOT GT_STOCK IS INITIAL.

  SELECT FROM MSEG AS A
  INNER JOIN MKPF AS B
    ON  B~MBLNR = A~MBLNR
    AND B~MJAHR = A~MJAHR
  FIELDS A~MBLNR,
         A~MJAHR,
         A~ZEILE,
         A~BWART,
         A~MATNR,
         A~WERKS,
         A~LGORT,
         B~BUDAT,
         A~SHKZG,
         A~MENGE,
         A~MEINS
  FOR ALL ENTRIES IN @GT_STOCK
  WHERE A~BWART NOT IN @GRT_RECEIVE_MVMNT
    AND A~BWART NOT IN @GRT_SALE_RESULT_MVMNT
    AND A~MATNR EQ @GT_STOCK-MATNR
    AND A~WERKS EQ @GT_STOCK-WERKS
    AND A~LGORT IN @GRT_UR_SLOC
    AND B~BUDAT IN @GS_PERIOD-PERIOD
  INTO TABLE @LT_MAT_DOC.

  LOOP AT GS_PERIOD-PERIOD_FIRST_DATE ASSIGNING FIELD-SYMBOL(<L_FIRST_DATE>).
    LF_INDEX  = SY-TABIX.

    CLEAR LRT_BUDAT[].
    READ TABLE GS_PERIOD-PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>) INDEX LF_INDEX.
    IF SY-SUBRC EQ 0.
      APPEND <L_PERIOD> TO LRT_BUDAT.

      LOOP AT LT_MAT_DOC ASSIGNING FIELD-SYMBOL(<L_MAT_DOC>) WHERE BUDAT IN LRT_BUDAT. "#EC CI_SORTSEQ
        READ TABLE GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>)
        WITH KEY DATE  = <L_FIRST_DATE>-LOW
                 MATNR = <L_MAT_DOC>-MATNR
        BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Sum Qty from Date/Mat level
          IF <L_MAT_DOC>-SHKZG EQ 'S'.
            <L_SSTK>-ADJUST_QTY = <L_SSTK>-ADJUST_QTY + <L_MAT_DOC>-MENGE.
          ELSE.
            <L_SSTK>-ADJUST_QTY = <L_SSTK>-ADJUST_QTY - <L_MAT_DOC>-MENGE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DO_STOCK
*&---------------------------------------------------------------------*
*& Get DO Stock
*&---------------------------------------------------------------------*
FORM F_GET_DO_STOCK .
  DATA: LT_DO     TYPE TT_DO.
  DATA: LF_INDEX  TYPE SY-TABIX.
  DATA: LRT_LFDAT TYPE RANGE OF LIKP-LFDAT.

  CHECK NOT GT_STOCK IS INITIAL.

  SELECT FROM VBEP AS A
  INNER JOIN LIPS AS B
    ON  B~VGBEL = A~VBELN
    AND B~VGPOS = A~POSNR
  INNER JOIN LIKP AS C
    ON  C~VBELN = B~VBELN
  FIELDS  A~VBELN AS VBELN_SO,
          A~POSNR AS POSNR_SO,
          A~ETENR AS ETENR_SO,
          B~VBELN AS VBELN_DO,
          B~POSNR AS POSNR_DO,
          C~LFDAT,
          B~MATNR,
          B~WERKS,
          B~LGORT,
          A~DLVQTY_BU,
          A~MEINS
  FOR ALL ENTRIES IN @GT_STOCK
  WHERE B~MATNR EQ @GT_STOCK-MATNR
    AND B~WERKS EQ @GT_STOCK-WERKS
    AND B~LGORT IN @GRT_UR_SLOC
    AND C~LFDAT IN @GS_PERIOD-PERIOD
    AND C~WBSTK NE 'C'  "Not posted GI
  INTO TABLE @LT_DO.
  DELETE ADJACENT DUPLICATES FROM LT_DO COMPARING VBELN_SO POSNR_SO ETENR_SO.

  LOOP AT GS_PERIOD-PERIOD_FIRST_DATE ASSIGNING FIELD-SYMBOL(<L_FIRST_DATE>).
    LF_INDEX  = SY-TABIX.

    CLEAR LRT_LFDAT[].
    READ TABLE GS_PERIOD-PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>) INDEX LF_INDEX.
    IF SY-SUBRC EQ 0.
      APPEND <L_PERIOD> TO LRT_LFDAT.

      LOOP AT LT_DO ASSIGNING FIELD-SYMBOL(<L_DO>) WHERE LFDAT IN LRT_LFDAT. "#EC CI_SORTSEQ
        READ TABLE GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>)
        WITH KEY DATE  = <L_FIRST_DATE>-LOW
                 MATNR = <L_DO>-MATNR
        BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Sum Qty from Date/Mat level
          <L_SSTK>-DLV_QTY = <L_SSTK>-DLV_QTY + <L_DO>-DLVQTY_BU.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_RESERVED_STOCK
*&---------------------------------------------------------------------*
*& Get Reserved Stock
*&---------------------------------------------------------------------*
FORM F_GET_RESERVED_STOCK .
  DATA: LT_RESB   TYPE TT_RESB.
  DATA: LF_INDEX  TYPE SY-TABIX.
  DATA: LRT_BDTER TYPE RANGE OF RESB-BDTER.

  CHECK NOT GT_STOCK IS INITIAL.

  SELECT FROM RESB
  FIELDS  RSNUM,
          RSPOS,
          RSART,
          MATNR,
          WERKS,
          LGORT,
          BDTER,
          BDMNG,
          MEINS
  FOR ALL ENTRIES IN @GT_STOCK
  WHERE XLOEK IS INITIAL
    AND KZEAR IS INITIAL
    AND MATNR EQ @GT_STOCK-MATNR
    AND WERKS EQ @GT_STOCK-WERKS
    AND LGORT IN @GRT_UR_SLOC
    AND BDTER IN @GS_PERIOD-PERIOD
  INTO TABLE @LT_RESB.

  LOOP AT GS_PERIOD-PERIOD_FIRST_DATE ASSIGNING FIELD-SYMBOL(<L_FIRST_DATE>).
    LF_INDEX  = SY-TABIX.

    CLEAR LRT_BDTER[].
    READ TABLE GS_PERIOD-PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>) INDEX LF_INDEX.
    IF SY-SUBRC EQ 0.
      APPEND <L_PERIOD> TO LRT_BDTER.

      LOOP AT LT_RESB ASSIGNING FIELD-SYMBOL(<L_RESB>) WHERE BDTER IN LRT_BDTER. "#EC CI_SORTSEQ
        READ TABLE GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>)
        WITH KEY DATE  = <L_FIRST_DATE>-LOW
                 MATNR = <L_RESB>-MATNR
        BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*         Sum Qty from Date/Mat level
          <L_SSTK>-RESERVED_QTY = <L_SSTK>-RESERVED_QTY + <L_RESB>-BDMNG.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_RESULT
*&---------------------------------------------------------------------*
*& Prepare Result
*&---------------------------------------------------------------------*
FORM F_PREPARE_RESULT .
  CHECK NOT GT_SSTK_DISP IS INITIAL.

  "Calculate: Ending Stock,
*             Expect ending stock,
*             Expect ending stock with SO PO
  LOOP AT GT_SSTK_DISP ASSIGNING FIELD-SYMBOL(<L_SSTK>).
    <L_SSTK>-END_STOCK        = <L_SSTK>-BEGIN_STOCK
                                  + <L_SSTK>-RECEIVED_QTY
                                  + <L_SSTK>-ADJUST_QTY
                                  - <L_SSTK>-SALE_RESULT_QTY.

    <L_SSTK>-EXPECT_END_STCK  = <L_SSTK>-BEGIN_STOCK
                                  + <L_SSTK>-RECEIVED_QTY
                                  + <L_SSTK>-ADJUST_QTY
                                  - <L_SSTK>-SALE_RESULT_QTY
                                  - <L_SSTK>-DLV_QTY
                                  - <L_SSTK>-RESERVED_QTY.

    <L_SSTK>-EXPECT_END_SO_PO = <L_SSTK>-BEGIN_STOCK
                                  + <L_SSTK>-RECEIVED_QTY
                                  + <L_SSTK>-ADJUST_QTY
                                  - <L_SSTK>-SALE_RESULT_QTY
                                  - <L_SSTK>-DLV_QTY
                                  - <L_SSTK>-RESERVED_QTY
                                  - <L_SSTK>-CONFIRM_SO_QTY
                                  + <L_SSTK>-PO_QTY.
  ENDLOOP.
  APPEND LINES OF GT_SSTK_DISP TO GT_RESULT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONSTANT
*&---------------------------------------------------------------------*
*& Get Constant
*&---------------------------------------------------------------------*
FORM F_GET_CONSTANT .
  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'MAT_TYPE'
                                        IMPORTING ET_RANGE = GRT_MAT_TYPE ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'UR_SLOC'
                                        IMPORTING ET_RANGE = GRT_UR_SLOC ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'INSU_SLOC'
                                        IMPORTING ET_RANGE = GRT_INSU_SLOC ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'RECEIVE_MOVEMENT_TYPE'
                                        IMPORTING ET_RANGE = GRT_RECEIVE_MVMNT ).

  ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = GC_REPID
                                                  IF_PARAM = 'SALE_RESULT_MOVEMENT_TYPE'
                                        IMPORTING ET_RANGE = GRT_SALE_RESULT_MVMNT ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DEFAULT_SELSCREEN
*&---------------------------------------------------------------------*
*& Set Default to Selection Screen
*&---------------------------------------------------------------------*
FORM F_DEFAULT_SELSCREEN .
  DATA: LS_MTART  LIKE LINE OF S_MTART.
  LOOP AT GRT_MAT_TYPE ASSIGNING FIELD-SYMBOL(<L_MAT_TYPE>).
    MOVE-CORRESPONDING <L_MAT_TYPE> TO LS_MTART.
    APPEND LS_MTART TO S_MTART.
  ENDLOOP.
ENDFORM.
