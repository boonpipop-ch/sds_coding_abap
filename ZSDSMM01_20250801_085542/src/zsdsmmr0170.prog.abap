*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0170
*  Creation Date      : 04.04.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : ZMMR005
*  Description        : Ukeharai Report - ALV output
*  Purpose            : Ukeharai Report - ALV output
*  Copied from        : ZR_MM_UK_REPORT_NEW1
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0170.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS,
  MARA,
  MCS0.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_MONTH_RANGE  TYPE RANGE OF MCS0-SPMON.
TYPES: TT_WEEK_RANGE TYPE RANGE OF MCS0-SPWOC.
TYPES: TT_DAY_RANGE TYPE RANGE OF SY-DATUM.

TYPES: BEGIN OF TS_PERIOD,
         BEGDA1 TYPE  SY-DATUM,
         ENDDA1 TYPE  SY-DATUM,
         BEGDA  TYPE  SY-DATUM,
         ENDDA  TYPE  SY-DATUM,
         PERIOD TYPE TT_DAY_RANGE,
       END OF TS_PERIOD.

TYPES: BEGIN OF TS_RESULT,
         BEGDA TYPE  SY-DATUM,
         ENDDA TYPE  SY-DATUM.
         INCLUDE TYPE ZSDSMMS011.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_SSTK,
         MATNR TYPE  MARD-MATNR,
         MATKL TYPE  MARA-MATKL,
         PRDHA TYPE  MARA-PRDHA,
         MEINS TYPE  MARA-MEINS,
         LABST TYPE  MARD-LABST,
         SPEME TYPE  MARD-SPEME,
       END OF TS_SSTK.
TYPES: TT_SSTK TYPE SORTED TABLE OF TS_SSTK
                    WITH UNIQUE KEY MATNR.

TYPES: BEGIN OF TS_MARD,
         MATNR TYPE  MARD-MATNR,
         WERKS TYPE  MARD-WERKS,
         LGORT TYPE  MARD-LGORT,
         MATKL TYPE  MARA-MATKL,
         PRDHA TYPE  MARA-PRDHA,
         MEINS TYPE  MARA-MEINS,
         LFGJA TYPE  MARD-LFGJA,
         LFMON TYPE  MARD-LFMON,
         LABST TYPE  MARD-LABST,
         UMLME TYPE  MARD-UMLME,
         INSME TYPE  MARD-INSME,
         EINME TYPE  MARD-EINME,
         SPEME TYPE  MARD-SPEME,
         RETME TYPE  MARD-RETME,
       END OF TS_MARD.
TYPES: TT_MARD TYPE SORTED TABLE OF TS_MARD
                    WITH UNIQUE KEY MATNR
                                    WERKS
                                    LGORT
                                    LFGJA
                                    LFMON.

TYPES: BEGIN OF TS_MVMNT,
         MATNR TYPE  MSEG-MATNR,
         BUDAT TYPE  MKPF-BUDAT,
         BWART TYPE  MSEG-BWART,
         INSMK TYPE  MSEG-INSMK,
         XAUTO TYPE  MSEG-XAUTO,
         MEINS TYPE  MSEG-MEINS,
         MENGE TYPE  MSEG-MENGE,
       END OF TS_MVMNT.
TYPES: TT_MVMNT TYPE SORTED TABLE OF TS_MVMNT
                     WITH UNIQUE KEY MATNR
                                     BUDAT
                                     BWART
                                     INSMK
                                     XAUTO
                                     MEINS.

TYPES: BEGIN OF TS_PO,
         MATNR TYPE EKPO-MATNR,
         EINDT TYPE EKET-EINDT,
         MEINS TYPE EKPO-MEINS,
         WEMNG TYPE EKET-WEMNG,
         MENGE TYPE EKET-MENGE,
       END OF TS_PO.
TYPES: TT_PO TYPE SORTED TABLE OF TS_PO
                  WITH UNIQUE KEY MATNR
                                  EINDT
                                  MEINS.

TYPES: BEGIN OF TS_SO,
         MATNR TYPE VBBE-MATNR,
         EDATU TYPE VBEP-EDATU,
         MEINS TYPE VBBE-MEINS,
         MENGE TYPE VBBE-OMENG,
       END OF TS_SO.
TYPES: TT_SO TYPE SORTED TABLE OF TS_SO
                  WITH UNIQUE KEY MATNR
                                  EDATU
                                  MEINS.

TYPES: BEGIN OF TS_DO,
         MATNR TYPE LIPS-MATNR,
         LFDAT TYPE LIKP-LFDAT,
         MEINS TYPE LIPS-MEINS,
         LGMNG TYPE LIPS-LGMNG,
       END OF TS_DO.
TYPES: TT_DO TYPE SORTED TABLE OF TS_DO
                  WITH UNIQUE KEY MATNR
                                  LFDAT
                                  MEINS.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X',
  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSMM005'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT  TYPE  TT_RESULT                                   ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_PERIOD  TYPE  TS_PERIOD                                   ##NEEDED.

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
  GR_WERKS    TYPE  RANGE OF T001W-WERKS                       ##NEEDED,
  GR_LGORT    TYPE  RANGE OF T001L-LGORT                       ##NEEDED,
  GR_MVT_M    TYPE  RANGE OF MSEG-BWART                        ##NEEDED,
  GR_MVT_R    TYPE  RANGE OF MSEG-BWART                        ##NEEDED,
  GR_MVT_I    TYPE  RANGE OF MSEG-BWART                        ##NEEDED,
  GR_MVT_B    TYPE  RANGE OF MSEG-BWART                        ##NEEDED,
  GR_MVT_A    TYPE  RANGE OF MSEG-BWART                        ##NEEDED,
  GF_GRPT     TYPE  WEBAZ                                      ##NEEDED,
  GR_SO_AUART TYPE  RANGE OF VBAK-AUART                        ##NEEDED,
  GR_SO_LGORT TYPE  RANGE OF T001L-LGORT                       ##NEEDED,
  GR_DO_LGORT TYPE  RANGE OF T001L-LGORT                       ##NEEDED,
  GR_AJ_LGORT TYPE  RANGE OF T001L-LGORT                       ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS011'.

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
* Text-s01: Material Data
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS:
    S_MATKL FOR MARA-MATKL,
    S_MATNR FOR MARA-MATNR,
    S_MEINS FOR MARA-MEINS NO INTERVALS DEFAULT 'EA',
    S_MTPOS FOR MARA-MTPOS_MARA NO INTERVALS DEFAULT 'NORM'.
SELECTION-SCREEN END OF BLOCK B1.

* Text-s02: Type
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      P_M RADIOBUTTON GROUP G_1  USER-COMMAND DUMMY.
*   Text-s03: Monthly
    SELECTION-SCREEN COMMENT 4(7) TEXT-S03 FOR FIELD P_M.
    SELECT-OPTIONS:
      S_MONTH FOR MCS0-SPMON  NO-EXTENSION MODIF ID MTH.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      P_W RADIOBUTTON GROUP G_1.
*   Text-s04: Weekly
    SELECTION-SCREEN COMMENT 4(7) TEXT-S04 FOR FIELD P_W.
    SELECT-OPTIONS:
      S_WEEK FOR MCS0-SPWOC NO-EXTENSION MODIF ID WEK.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      P_D RADIOBUTTON GROUP G_1 DEFAULT 'X'.
*   Text-s05: Daily
    SELECTION-SCREEN COMMENT 4(7) TEXT-S05 FOR FIELD P_D.
    SELECT-OPTIONS:
      S_DATE FOR SY-DATUM NO-EXTENSION MODIF ID DAY.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_CONSTANTS.
  PERFORM F_DEFAULT_SELECTION.

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

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get data
  PERFORM F_GET_DATA  USING  GS_PERIOD
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
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_PLANT    TYPE  ZSDSDE_PARAM_NAME VALUE 'PLANT',
    LC_SLOC     TYPE  ZSDSDE_PARAM_NAME VALUE 'SLOC',
    LC_MVT_M    TYPE  ZSDSDE_PARAM_NAME VALUE 'MVT_M',
    LC_MVT_R    TYPE  ZSDSDE_PARAM_NAME VALUE 'MVT_R',
    LC_MVT_I    TYPE  ZSDSDE_PARAM_NAME VALUE 'MVT_I',
    LC_MVT_B    TYPE  ZSDSDE_PARAM_NAME VALUE 'MVT_B',
    LC_MVT_A    TYPE  ZSDSDE_PARAM_NAME VALUE 'ADJ_MVT',
    LC_GRPT     TYPE  ZSDSDE_PARAM_NAME VALUE 'GR_PROCESSING_TIME',
    LC_SO_AUART TYPE  ZSDSDE_PARAM_NAME VALUE 'SO_T',
    LC_SO_LGART TYPE  ZSDSDE_PARAM_NAME VALUE 'SO_STO',
    LC_DO_LGART TYPE  ZSDSDE_PARAM_NAME VALUE 'DO_STO',
    LC_AJ_LGART TYPE  ZSDSDE_PARAM_NAME VALUE 'ADJ_STO'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM.


* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GR_WERKS,
         GR_LGORT,
         GR_MVT_M,
         GR_MVT_R,
         GR_MVT_I,
         GR_MVT_B,
         GR_MVT_A,
         GF_GRPT,
         GR_SO_AUART,
         GR_SO_LGORT,
         GR_DO_LGORT,
         GR_AJ_LGORT.

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = GC_TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     Plants
*     ------------------------------------
      WHEN LC_PLANT.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_WERKS.

*     ------------------------------------
*     SLoc
*     ------------------------------------
      WHEN LC_SLOC.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_LGORT.

*     ------------------------------------
*     All Applicable Movement Types
*     ------------------------------------
      WHEN LC_MVT_M.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_MVT_M.

*     ------------------------------------
*     Receive Movement Types
*     ------------------------------------
      WHEN LC_MVT_R.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_MVT_R.

*     ------------------------------------
*     Issue Movement Types
*     ------------------------------------
      WHEN LC_MVT_I.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_MVT_I.

*     ------------------------------------
*     Block Movement Types
*     ------------------------------------
      WHEN LC_MVT_B.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_MVT_B.

*     ------------------------------------
*     Adjust Movement Types
*     ------------------------------------
      WHEN LC_MVT_A.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_MVT_A.

*     ------------------------------------
*     GR Processing Time
*     ------------------------------------
      WHEN LC_GRPT.
        GF_GRPT = <L_GENC>-VALUE_LOW.

*     ------------------------------------
*     Sales Order Types
*     ------------------------------------
      WHEN LC_SO_AUART.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_SO_AUART.

*     ------------------------------------
*     Sales Order Sloc
*     ------------------------------------
      WHEN LC_SO_LGART.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_SO_LGORT.

*     ------------------------------------
*     Delivery Order Sloc
*     ------------------------------------
      WHEN LC_DO_LGART.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_DO_LGORT.

*     ------------------------------------
*     Adjust Sloc
*     ------------------------------------
      WHEN LC_AJ_LGART.
        APPEND VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               TO GR_AJ_LGORT.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN ##NEEDED.

  DATA:
    LF_DIFF TYPE I.


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
*  Form F_SET_SELECTION_SCREEN_FORMAT
*----------------------------------------------------------------------*
*  Set Selection screen format
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .
  LOOP AT SCREEN.

    CASE SCREEN-GROUP1.
*     -------------
*     Month Fields
*     -------------
      WHEN 'MTH'.
        IF P_M EQ GC_TRUE.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.

*     -------------
*     Week Fields
*     -------------
      WHEN 'WEK'.
        IF P_W EQ GC_TRUE.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.

*     -------------
*     Daily Fields
*     -------------
      WHEN 'DAY'.
        IF P_D EQ GC_TRUE.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.

    ENDCASE.
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

*----------------------------------------------------------------------*
*  Form f_list_months
*----------------------------------------------------------------------*
*  List Months
*----------------------------------------------------------------------*
FORM F_LIST_MONTHS  CHANGING PC_MONTH TYPE MCS0-SPMON.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      ACTUAL_MONTH    = SY-DATUM(6)
      START_COLUMN    = 8
      START_ROW       = 5
    IMPORTING
      SELECTED_MONTH  = PC_MONTH
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
FORM F_LIST_WEEKS  CHANGING PC_WEEK TYPE MCS0-SPWOC.

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

  CONCATENATE  LF_EEIND+3(4) LF_EEIND(2) INTO PC_WEEK.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_default_selection
*----------------------------------------------------------------------*
*  Default Selection Screen
*----------------------------------------------------------------------*
FORM F_DEFAULT_SELECTION .

  DATA:
    LF_DATE TYPE SY-DATUM .


  CLEAR: S_DATE, S_DATE[].

  LF_DATE = SY-DATUM - 1.
  APPEND VALUE #( SIGN = 'I'
                  OPTION = 'EQ'
                  LOW    = LF_DATE )
         TO S_DATE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_assign_period_month
*----------------------------------------------------------------------*
*  Assig Period data from Monthly
*----------------------------------------------------------------------*
FORM F_ASSIGN_PERIOD_MONTH  USING  UR_MONTH TYPE TT_MONTH_RANGE
                          CHANGING CS_PERIOD TYPE TS_PERIOD.

  DATA:
    LF_DATUM_FROM TYPE  SY-DATUM,
    LF_DATUM_TO   TYPE  SY-DATUM,
    LF_MONTH_FROM TYPE  SPMON,
    LF_MONTH_TO   TYPE  SPMON.


* Initialize Output
  CLEAR: CS_PERIOD.

  READ TABLE UR_MONTH ASSIGNING FIELD-SYMBOL(<L_MONTH>)
                      INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Start date
  CS_PERIOD-BEGDA = <L_MONTH>-LOW && '01'.
  CS_PERIOD-BEGDA1 = CS_PERIOD-BEGDA.
  CS_PERIOD-ENDDA1 = CS_PERIOD-BEGDA - 1.
  IF CS_PERIOD-ENDDA1 LT CS_PERIOD-BEGDA1.
    CS_PERIOD-ENDDA1 = CS_PERIOD-BEGDA1.
  ENDIF.
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
FORM F_ASSIGN_PERIOD_WEEK  USING  UR_WEEK  TYPE  TT_WEEK_RANGE
                         CHANGING CS_PERIOD TYPE  TS_PERIOD.

  DATA:
    LF_DATUM_FROM TYPE  SY-DATUM,
    LF_DATUM_TO   TYPE  SY-DATUM,
    LF_WEEK_FROM  TYPE  SPWOC,
    LF_WEEK_TO    TYPE  SPWOC.


* Initialize Output
  CLEAR: CS_PERIOD.

  READ TABLE UR_WEEK ASSIGNING FIELD-SYMBOL(<L_WEEK>)
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
  CS_PERIOD-BEGDA1 = CS_PERIOD-BEGDA(6) && '01'.
  CS_PERIOD-ENDDA1 = CS_PERIOD-BEGDA - 1.
  IF CS_PERIOD-ENDDA1 LT CS_PERIOD-BEGDA1.
    CS_PERIOD-ENDDA1 = CS_PERIOD-BEGDA1.
  ENDIF.

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

*----------------------------------------------------------------------*
*  Form F_ASSIGN_PERIOD_DAILY
*----------------------------------------------------------------------*
*  Assign Period from Daily
*----------------------------------------------------------------------*
FORM F_ASSIGN_PERIOD_DAILY  USING  UR_DATE TYPE TT_DAY_RANGE
                          CHANGING CS_PERIOD TYPE TS_PERIOD.

  DATA:
    LF_DATUM_FROM TYPE  SY-DATUM,
    LF_DATUM_TO   TYPE  SY-DATUM.


* Initialize Output
  CLEAR: CS_PERIOD.

  READ TABLE UR_DATE ASSIGNING FIELD-SYMBOL(<L_DATE>)
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
  CS_PERIOD-BEGDA1 = CS_PERIOD-BEGDA(6) && '01'.
  CS_PERIOD-ENDDA1 = CS_PERIOD-BEGDA - 1.
  IF CS_PERIOD-ENDDA1 LT CS_PERIOD-BEGDA1.
    CS_PERIOD-ENDDA1 = CS_PERIOD-BEGDA1.
  ENDIF.
  CS_PERIOD-ENDDA  = LF_DATUM_TO.

* Assign Periods
  WHILE LF_DATUM_FROM LE LF_DATUM_TO.
    APPEND VALUE #( SIGN = 'I'
                    OPTION = 'BT'
                    LOW    = LF_DATUM_FROM
                    HIGH   = LF_DATUM_FROM )
           TO CS_PERIOD-PERIOD.
    LF_DATUM_FROM = LF_DATUM_FROM + 1.
  ENDWHILE.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_DATA
*----------------------------------------------------------------------*
*  Get Data
*----------------------------------------------------------------------*
FORM F_GET_DATA  USING  US_PERIOD TYPE TS_PERIOD
               CHANGING CT_RESULT TYPE TT_RESULT.

  DATA:
    LT_SSTK  TYPE  TT_SSTK,
    LT_MVMNT TYPE  TT_MVMNT,
    LT_ADJST TYPE  TT_MVMNT,
    LT_PO    TYPE  TT_PO,
    LT_SO    TYPE  TT_SO,
    LT_DO    TYPE  TT_DO.


* Get Material Stock
  PERFORM F_GET_MATERIAL_STOCK  USING  US_PERIOD
                              CHANGING LT_SSTK.
  IF LT_SSTK IS INITIAL.
    RETURN.
  ENDIF.

* Get Movement in Period
  PERFORM F_GET_MOVEMENT  USING  US_PERIOD
                                 LT_SSTK
                        CHANGING LT_MVMNT
                                 LT_ADJST.

* Get Purchase Order data
  PERFORM F_GET_PO_DATA  USING  US_PERIOD
                                LT_SSTK
                       CHANGING LT_PO.

* Get Sales Order data
  PERFORM F_GET_SO_DATA  USING  US_PERIOD
                                LT_SSTK
                       CHANGING LT_SO.

* Get Delivery Order data
  PERFORM F_GET_DO_DATA  USING  US_PERIOD
                                LT_SSTK
                       CHANGING LT_DO.

* Collect Result
  PERFORM F_COLLECT_RESULT  USING  US_PERIOD
                                   LT_SSTK
                                   LT_MVMNT
                                   LT_ADJST
                                   LT_PO
                                   LT_SO
                                   LT_DO
                          CHANGING CT_RESULT.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_get_material_stock
*----------------------------------------------------------------------*
*  Get Material Stock
*----------------------------------------------------------------------*
FORM F_GET_MATERIAL_STOCK  USING  US_PERIOD  TYPE  TS_PERIOD
                         CHANGING CT_SSTK    TYPE  TT_SSTK.

  DATA:
    LT_MARDH      TYPE  TT_MARD,
    LT_MARD       TYPE  TT_MARD,
    LT_MARD_FINAL TYPE  TT_MARD.

  DATA:
    LF_DATUM TYPE  SY-DATUM,
    LF_SPMON TYPE  SPMON.


* Show progress
* Text-p01 : Reading Material Stock data . . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

* Initialize Output
  CLEAR: CT_SSTK.

* Assign Reading Period
  IF US_PERIOD-BEGDA+4(2) EQ '01'.
    LF_SPMON(4) = US_PERIOD-BEGDA(4) - 1.
    LF_SPMON+4(2) = '12'.
  ELSE.
    LF_SPMON = US_PERIOD-BEGDA(6) - 1.
  ENDIF.

* Get Previous Period stock data
  LF_DATUM = SY-DATUM(6) && '01'.
  IF LF_DATUM GT US_PERIOD-BEGDA.
    SELECT A~MATNR,
           A~WERKS,
           A~LGORT,
           B~MATKL,
           B~PRDHA,
           B~MEINS,
           A~LFGJA,
           A~LFMON,
           A~LABST,
           A~UMLME,
           A~INSME,
           A~EINME,
           A~SPEME,
           A~RETME
      FROM MARDH AS A
             INNER JOIN MARA AS B
               ON  B~MATNR = A~MATNR
     WHERE A~MATNR IN @S_MATNR
       AND A~WERKS IN @GR_WERKS
       AND A~WERKS IN @GR_LGORT
       AND B~MATKL IN @S_MATKL
       AND B~MEINS IN @S_MEINS
       AND B~MATKL IN @S_MATKL
       AND B~MTPOS_MARA IN @S_MTPOS
       AND ( ( A~LFGJA EQ @LF_SPMON(4) AND A~LFMON GE @LF_SPMON+4(2) ) OR
             A~LFGJA GT @LF_SPMON(4) )
      INTO TABLE @LT_MARDH.
    IF SY-SUBRC NE 0.
      CLEAR: LT_MARDH.
    ENDIF.

*   Only Min period for each material
    DELETE ADJACENT DUPLICATES FROM LT_MARDH COMPARING MATNR
                                                       WERKS
                                                       LGORT.
  ENDIF.

* Get Current Period stock data
  SELECT A~MATNR,
         A~WERKS,
         A~LGORT,
         B~MATKL,
         B~PRDHA,
         B~MEINS,
         A~LFGJA,
         A~LFMON,
         A~LABST,
         A~UMLME,
         A~INSME,
         A~EINME,
         A~SPEME,
         A~RETME
    FROM MARD AS A
           INNER JOIN MARA AS B
             ON  B~MATNR = A~MATNR
   WHERE A~MATNR IN @S_MATNR
     AND A~WERKS IN @GR_WERKS
     AND A~WERKS IN @GR_LGORT
     AND B~MATKL IN @S_MATKL
     AND B~MEINS IN @S_MEINS
     AND B~MATKL IN @S_MATKL
     AND B~MTPOS_MARA IN @S_MTPOS
    INTO TABLE @LT_MARD.
  IF SY-SUBRC NE 0.
    CLEAR: LT_MARD.
  ENDIF.

* -------------------
* Collect Final MARD Result
* -------------------
  LT_MARD_FINAL = CORRESPONDING #( LT_MARDH ).
  IF LT_MARD_FINAL IS INITIAL.
    LT_MARD_FINAL = CORRESPONDING #( LT_MARD ).
  ELSE.
    LOOP AT LT_MARD ASSIGNING FIELD-SYMBOL(<L_MARD>).
      READ TABLE LT_MARD_FINAL TRANSPORTING NO FIELDS
                         WITH KEY MATNR = <L_MARD>-MATNR
                                  WERKS = <L_MARD>-WERKS
                                  LGORT = <L_MARD>-LGORT
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        CONTINUE.
      ENDIF.
      INSERT <L_MARD> INTO TABLE LT_MARD_FINAL.
    ENDLOOP.
  ENDIF.
  FREE: LT_MARDH, LT_MARD.

* -------------------
* Assign Result
* -------------------
  LOOP AT LT_MARD_FINAL ASSIGNING <L_MARD>.

    READ TABLE CT_SSTK ASSIGNING FIELD-SYMBOL(<L_SSTK>)
                       WITH KEY MATNR = <L_MARD>-MATNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT VALUE TS_SSTK( MATNR = <L_MARD>-MATNR
                            MATKL = <L_MARD>-MATKL
                            PRDHA = <L_MARD>-PRDHA
                            MEINS = <L_MARD>-MEINS )
                   INTO TABLE CT_SSTK
                   ASSIGNING <L_SSTK>.
    ENDIF.

*   Sum Qty from Plant/Sloc level
    <L_SSTK>-LABST = <L_SSTK>-LABST + <L_MARD>-LABST.
    <L_SSTK>-SPEME = <L_SSTK>-SPEME + <L_MARD>-SPEME.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_MOVEMENT
*----------------------------------------------------------------------*
*  Get Material Movement data
*----------------------------------------------------------------------*
FORM F_GET_MOVEMENT  USING  US_PERIOD  TYPE  TS_PERIOD
                            UT_SSTK    TYPE  TT_SSTK
                   CHANGING CT_MVMNT  TYPE  TT_MVMNT
                            CT_ADJST  TYPE  TT_MVMNT.

* Show progress
* Text-p02 : Reading Material Movement data . . .
  MC_SHOW_PROGRESS 40 TEXT-P02.

* Initialize Output
  CLEAR: CT_MVMNT,
         CT_ADJST.

* -------------------------------------
* Get material Document All Movement
* -------------------------------------
  SELECT A~MATNR,
         B~BUDAT,
         A~BWART,
         A~INSMK,
         A~XAUTO,
         A~MEINS,
         SUM( CASE A~SHKZG
                WHEN 'H' THEN A~MENGE * -1
                ELSE A~MENGE
              END ) AS MENGE
    FROM @UT_SSTK AS X
           INNER JOIN MSEG AS A
             ON  A~MATNR = X~MATNR
           INNER JOIN MKPF AS B
             ON  B~MBLNR = A~MBLNR
             AND B~MJAHR = A~MJAHR
   WHERE B~BUDAT BETWEEN @US_PERIOD-BEGDA1
                     AND @US_PERIOD-ENDDA
     AND A~WERKS IN @GR_WERKS
     AND A~LGORT IN @GR_LGORT
     AND A~BWART IN @GR_MVT_M
     AND A~SMBLN EQ @SPACE
     AND NOT EXISTS ( SELECT SMBLN
                        FROM MSEG AS Y
                       WHERE Y~SJAHR EQ A~MJAHR
                         AND Y~SMBLN EQ A~MBLNR
                         AND Y~SMBLP EQ A~ZEILE )
   GROUP BY A~MATNR,
            B~BUDAT,
            A~BWART,
            A~INSMK,
            A~XAUTO,
            A~MEINS
    INTO TABLE @CT_MVMNT.
  IF SY-SUBRC NE 0.
    CLEAR CT_MVMNT.
  ENDIF.

* -------------------------------------
* Get material Document Adjust Movement
* -------------------------------------
  SELECT A~MATNR,
         B~BUDAT,
         A~BWART,
         A~INSMK,
         A~XAUTO,
         A~MEINS,
         SUM( CASE A~SHKZG
                WHEN 'H' THEN A~MENGE * -1
                ELSE A~MENGE
              END ) AS MENGE
    FROM @UT_SSTK AS X
           INNER JOIN MSEG AS A
             ON  A~MATNR = X~MATNR
           INNER JOIN MKPF AS B
             ON  B~MBLNR = A~MBLNR
             AND B~MJAHR = A~MJAHR
   WHERE B~BUDAT BETWEEN @US_PERIOD-BEGDA
                     AND @US_PERIOD-ENDDA
     AND A~WERKS IN @GR_WERKS
     AND A~LGORT IN @GR_AJ_LGORT
     AND A~BWART IN @GR_MVT_A
     AND A~SMBLN EQ @SPACE
     AND NOT EXISTS ( SELECT SMBLN
                        FROM MSEG AS Y
                       WHERE Y~SJAHR EQ A~MJAHR
                         AND Y~SMBLN EQ A~MBLNR
                         AND Y~SMBLP EQ A~ZEILE )
   GROUP BY A~MATNR,
            B~BUDAT,
            A~BWART,
            A~INSMK,
            A~XAUTO,
            A~MEINS
    INTO TABLE @CT_ADJST.
  IF SY-SUBRC NE 0.
    CLEAR CT_ADJST.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_PO_DATA
*----------------------------------------------------------------------*
*  Get PO Data in period
*----------------------------------------------------------------------*
FORM F_GET_PO_DATA  USING  US_PERIOD  TYPE  TS_PERIOD
                           UT_SSTK    TYPE  TT_SSTK
                  CHANGING CT_PO      TYPE  TT_PO.

  DATA:
    LF_DAYS  TYPE  I,
    LF_BEGDA TYPE  SY-DATUM.


* Show progress
* Text-p03 : Reading Purchasing Order data . . .
  MC_SHOW_PROGRESS 60 TEXT-P03.

* Initialize Output
  CLEAR: CT_PO.

* Add buffer 7 Days
  LF_BEGDA = US_PERIOD-BEGDA - 7.

* Get PO Data
  SELECT A~MATNR,
         C~EINDT,
         A~MEINS,
         D~WEBAZ,
         SUM( C~MENGE ) AS MENGE,
         SUM( C~WEMNG ) AS WEMNG
    FROM @UT_SSTK AS X
           INNER JOIN EKPO AS A
             ON  A~MATNR = X~MATNR
           INNER JOIN EKKO AS B
             ON  B~EBELN = A~EBELN
           INNER JOIN EKET AS C
             ON  C~EBELN = A~EBELN
             AND C~EBELP = A~EBELP
           INNER JOIN MARC AS D
             ON  D~MATNR = A~MATNR
             AND D~WERKS = A~WERKS
   WHERE A~LOEKZ  EQ @SPACE
     AND A~WERKS  IN @GR_WERKS
     AND B~MEMORY EQ @SPACE
     AND C~EINDT BETWEEN @LF_BEGDA
                   AND   @US_PERIOD-ENDDA
   GROUP BY A~MATNR,
            C~EINDT,
            A~MEINS,
            D~WEBAZ
    INTO TABLE @DATA(LT_EKET).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Result
  LOOP AT LT_EKET ASSIGNING FIELD-SYMBOL(<L_EKET>).

*   Update Delivery Date
    IF <L_EKET>-WEBAZ GT GF_GRPT.
      LF_DAYS = <L_EKET>-WEBAZ.
      CALL FUNCTION 'FKK_ADD_WORKINGDAY'
        EXPORTING
          I_DATE      = <L_EKET>-EINDT
          I_DAYS      = LF_DAYS
          I_CALENDAR1 = 'TH'
        IMPORTING
          E_DATE      = <L_EKET>-EINDT.
    ENDIF.

*   Filtering Period
    CHECK <L_EKET>-EINDT BETWEEN US_PERIOD-BEGDA
                           AND   US_PERIOD-ENDDA.

    READ TABLE CT_PO ASSIGNING FIELD-SYMBOL(<L_PO>)
                     WITH KEY MATNR = <L_EKET>-MATNR
                              EINDT = <L_EKET>-EINDT
                              MEINS = <L_EKET>-MEINS
                     BINARY SEARCH.
    IF SY-SUBRC NE 0.
      INSERT VALUE TS_PO( MATNR = <L_EKET>-MATNR
                          EINDT = <L_EKET>-EINDT
                          MEINS = <L_EKET>-MEINS )
             INTO TABLE CT_PO
             ASSIGNING <L_PO>.
    ENDIF.

    <L_PO>-WEMNG = <L_PO>-WEMNG + <L_EKET>-WEMNG.
    <L_PO>-MENGE = <L_PO>-MENGE + <L_EKET>-MENGE - <L_EKET>-WEMNG.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_SO_DATA
*----------------------------------------------------------------------*
*  Get Sales Order Data in Period
*----------------------------------------------------------------------*
FORM F_GET_SO_DATA  USING  US_PERIOD  TYPE  TS_PERIOD
                           UT_SSTK    TYPE  TT_SSTK
                  CHANGING CT_SO      TYPE  TT_SO.

* Show progress
* Text-p04 : Reading Sales Order data . . .
  MC_SHOW_PROGRESS 70 TEXT-P04.

* Initialize Output
  CLEAR: CT_SO.

* Get Sales Order
  SELECT A~MATNR,
         B~EDATU,
         A~MEINS,
         SUM( A~OMENG ) AS MENGE
    FROM @UT_SSTK AS X
           INNER JOIN VBBE AS A
             ON  A~MATNR = X~MATNR
           INNER JOIN VBEP AS B
             ON  B~VBELN = A~VBELN
             AND B~POSNR = A~POSNR
             AND B~ETENR = A~ETENR
           INNER JOIN VBAK AS C
             ON  C~VBELN = A~VBELN
   WHERE C~AUART   IN  @GR_SO_AUART
     AND A~OMENG   NE  0
     AND B~EDATU BETWEEN @US_PERIOD-BEGDA
                   AND   @US_PERIOD-ENDDA
     AND A~LGORT NOT IN @GR_SO_LGORT
   GROUP BY A~MATNR,
            B~EDATU,
            A~MEINS
    INTO TABLE @CT_SO.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_DO_DATA
*----------------------------------------------------------------------*
*  Get Delivery Order data in period
*----------------------------------------------------------------------*
FORM F_GET_DO_DATA  USING  US_PERIOD  TYPE  TS_PERIOD
                           UT_SSTK    TYPE  TT_SSTK
                  CHANGING CT_DO      TYPE  TT_DO.

* Show progress
* Text-p05 : Reading Delivery Order data . . .
  MC_SHOW_PROGRESS 80 TEXT-P05.

* Initialize Output
  CLEAR: CT_DO.

* Get Delivery Order
  SELECT A~MATNR,
         D~LFDAT,
         A~MEINS,
         SUM( A~LGMNG ) AS LGMNG
    FROM @UT_SSTK AS X
           INNER JOIN LIPS AS A
             ON  A~MATNR = X~MATNR
           INNER JOIN VBAP AS B
             ON  B~VBELN = A~VGBEL
             AND B~POSNR = A~VGPOS
           INNER JOIN VBAK AS C
             ON  C~VBELN = A~VGBEL
           INNER JOIN LIKP AS D
             ON  D~VBELN = A~VBELN
   WHERE C~WBSTK NE 'C'
     AND C~AUART IN @GR_SO_AUART
     AND A~LGORT NOT IN @GR_DO_LGORT
     AND D~LFART EQ 'ZD01'
     AND D~LFDAT BETWEEN @US_PERIOD-BEGDA
                   AND   @US_PERIOD-ENDDA
   GROUP BY A~MATNR,
         D~LFDAT,
         A~MEINS
     INTO TABLE @CT_DO.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_COLLECT_RESULT
*----------------------------------------------------------------------*
*  Collect Result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT  USING  US_PERIOD  TYPE  TS_PERIOD
                              UT_SSTK    TYPE  TT_SSTK
                              UT_MVMNT   TYPE  TT_MVMNT
                              UT_ADJST   TYPE  TT_MVMNT
                              UT_PO      TYPE  TT_PO
                              UT_SO      TYPE  TT_SO
                              UT_DO      TYPE  TT_DO

                     CHANGING CT_RESULT  TYPE  TT_RESULT.

  DATA:
    LS_RESULT  TYPE  TS_RESULT.

  DATA:
    LF_BSTK  TYPE  TS_RESULT-BSTK,
    LF_SPEME TYPE  TS_RESULT-SPEME.


* Show progress
* Text-p06 : Collecting Result . . .
  MC_SHOW_PROGRESS 90 TEXT-P06.

* Initialze Output
  CLEAR: CT_RESULT.

* --------------------
* For All Materials
* --------------------
  LOOP AT UT_SSTK ASSIGNING FIELD-SYMBOL(<L_SSTK>).

*   Assign Begin Stock qty and block qty
    LF_BSTK  = <L_SSTK>-LABST + <L_SSTK>-SPEME.
    LF_SPEME = <L_SSTK>-SPEME.

*   Calc Qty up to start date (Only when start date is not 01)
    IF US_PERIOD-BEGDA+6(2) NE '01'.
      LOOP AT UT_MVMNT ASSIGNING FIELD-SYMBOL(<L_MVMNT>)
                       WHERE MATNR EQ <L_SSTK>-MATNR
                         AND BUDAT BETWEEN US_PERIOD-BEGDA1
                                     AND   US_PERIOD-ENDDA1.
        LF_BSTK = LF_BSTK + <L_MVMNT>-MENGE.

        IF <L_MVMNT>-INSMK = '3' OR
           <L_MVMNT>-INSMK = 'S' OR
           ( <L_MVMNT>-BWART IN GR_MVT_B AND <L_MVMNT>-XAUTO EQ SPACE ).
          LF_SPEME = LF_SPEME + <L_MVMNT>-MENGE.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   Reporting Period
    LOOP AT US_PERIOD-PERIOD ASSIGNING FIELD-SYMBOL(<L_PERIOD>).

      CLEAR LS_RESULT.
      LS_RESULT-BEGDA     = <L_PERIOD>-LOW.
      LS_RESULT-ENDDA     = <L_PERIOD>-HIGH.

      PERFORM F_ASSIGN_DATE_INFO  USING  <L_PERIOD>-LOW
                                CHANGING LS_RESULT-DATE
                                         LS_RESULT-WEEK
                                         LS_RESULT-MONTH.

      LS_RESULT-CATEGORY = <L_SSTK>-PRDHA(5).
      LS_RESULT-CLASS    = <L_SSTK>-PRDHA+5(5).
      LS_RESULT-MATNR    = <L_SSTK>-MATNR.
      LS_RESULT-MEINS    = <L_SSTK>-MEINS.
      LS_RESULT-BSTK     = LF_BSTK.

      LOOP AT UT_PO ASSIGNING FIELD-SYMBOL(<L_PO>)
                    WHERE MATNR EQ LS_RESULT-MATNR
                      AND EINDT BETWEEN <L_PERIOD>-LOW
                                  AND   <L_PERIOD>-HIGH.
        LS_RESULT-INPO = LS_RESULT-INPO + <L_PO>-MENGE.
      ENDLOOP.

      LOOP AT UT_MVMNT ASSIGNING <L_MVMNT>
                       WHERE MATNR   EQ LS_RESULT-MATNR
                         AND BUDAT BETWEEN <L_PERIOD>-LOW
                                     AND   <L_PERIOD>-HIGH.

        IF <L_MVMNT>-BWART IN GR_MVT_R.
          LS_RESULT-RECEIVE = LS_RESULT-RECEIVE + <L_MVMNT>-MENGE.
        ENDIF.

        IF <L_MVMNT>-BWART IN GR_MVT_I.
          LS_RESULT-RESULT = LS_RESULT-RESULT + <L_MVMNT>-MENGE.
        ENDIF.

        IF <L_MVMNT>-INSMK = '3' OR
           <L_MVMNT>-INSMK = 'S' OR
           ( <L_MVMNT>-BWART IN GR_MVT_B AND <L_MVMNT>-XAUTO EQ SPACE ).
          LF_SPEME = LF_SPEME + <L_MVMNT>-MENGE.
        ENDIF.

      ENDLOOP.
      LS_RESULT-SPEME   = LF_SPEME.
      LS_RESULT-TOTALPO = LS_RESULT-INPO - LS_RESULT-RECEIVE.

      LOOP AT UT_SO ASSIGNING FIELD-SYMBOL(<L_SO>)
                    WHERE MATNR EQ LS_RESULT-MATNR
                      AND EDATU BETWEEN <L_PERIOD>-LOW
                                  AND   <L_PERIOD>-HIGH.
        LS_RESULT-SOQTY = LS_RESULT-SOQTY + <L_SO>-MENGE.
      ENDLOOP.

      IF SY-DATUM BETWEEN <L_PERIOD>-LOW AND <L_PERIOD>-HIGH.
        LOOP AT UT_SO ASSIGNING <L_SO>
                      WHERE MATNR EQ LS_RESULT-MATNR
                        AND EDATU LT <L_PERIOD>-LOW.
          LS_RESULT-OUTSTANDING_SO = LS_RESULT-OUTSTANDING_SO + <L_SO>-MENGE.
        ENDLOOP.
      ENDIF.

      LOOP AT UT_DO ASSIGNING FIELD-SYMBOL(<L_DO>)
                    WHERE MATNR   EQ LS_RESULT-MATNR
                      AND LFDAT BETWEEN <L_PERIOD>-LOW
                                  AND   <L_PERIOD>-HIGH.
        LS_RESULT-DO = LS_RESULT-DO + <L_DO>-LGMNG.
      ENDLOOP.

      LOOP AT UT_ADJST ASSIGNING FIELD-SYMBOL(<L_ADJST>)
                       WHERE MATNR EQ LS_RESULT-MATNR
                         AND BUDAT BETWEEN <L_PERIOD>-LOW
                                     AND   <L_PERIOD>-HIGH.
        LS_RESULT-ADJUST = LS_RESULT-ADJUST + <L_ADJST>-MENGE.
      ENDLOOP.

      LS_RESULT-ENDSTK = LS_RESULT-BSTK    +
                         LS_RESULT-RECEIVE +
                         LS_RESULT-RESULT  +
                         LS_RESULT-ADJUST.

      LS_RESULT-ASTK = LS_RESULT-BSTK    +
                       LS_RESULT-RECEIVE -
                       LS_RESULT-SPEME.

      INSERT LS_RESULT INTO TABLE CT_RESULT.

      LF_BSTK = LS_RESULT-ENDSTK.

    ENDLOOP.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ASSIGN_DATE_INFO
*----------------------------------------------------------------------*
*  Assign Date Information
*----------------------------------------------------------------------*
FORM F_ASSIGN_DATE_INFO  USING  UF_DATUM  TYPE  SY-DATUM
                       CHANGING CF_DATE   TYPE  TS_RESULT-DATE
                                CF_WEEK   TYPE  TS_RESULT-WEEK
                                CF_MONTH  TYPE  TS_RESULT-MONTH.

  CONSTANTS:
    LC_QUOTE       TYPE C VALUE ''''.

  DATA:
    LS_MONTH TYPE T247.

  DATA:
    LF_WEEK TYPE SCAL-WEEK.


* Initialize Output
  CLEAR: CF_DATE,
         CF_WEEK,
         CF_MONTH.

* ------------
* DATE
* ------------
  CF_DATE = UF_DATUM.

* ------------
* WEEK
* ------------
* Get week number from date
  CLEAR LF_WEEK.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      DATE         = UF_DATUM
    IMPORTING
      WEEK         = LF_WEEK
    EXCEPTIONS
      DATE_INVALID = 1
      OTHERS       = 2.
  IF SY-SUBRC IS INITIAL.
    CONCATENATE LC_QUOTE LF_WEEK+4(2) '-' LF_WEEK(4)
           INTO CF_WEEK.
  ENDIF.

* ------------
* MONTH
* ------------
  CLEAR LS_MONTH.
  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      LANGU = SY-LANGU
      MONTH = UF_DATUM+4(2)
    IMPORTING
      T247  = LS_MONTH.

  CONCATENATE LS_MONTH-KTX '-' UF_DATUM(4)
           INTO CF_MONTH.

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
      WHEN 'DATE'.
*       Text-c01 : Date
        LF_TEXT                = TEXT-C01.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'WEEK'.
*       Text-C02: Week
        LF_TEXT                = TEXT-C02.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MONTH'.
*       Text-C03: Month
        LF_TEXT                = TEXT-C03.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CATEGORY'.
*       Text-C04: Category
        LF_TEXT                = TEXT-C04.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'CLASS'.
*       Text-C05: Class
        LF_TEXT                = TEXT-C05.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MATNR'.
*       Text-C06: Material
        LF_TEXT                = TEXT-C06.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'MEINS'.
        <L_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'BSTK'.
*       Text-C07: B/Stk
        LF_TEXT                = TEXT-C07.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'INPO'.
*       Text-C08: PO
        LF_TEXT                = TEXT-C08.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'RECEIVE'.
*       Text-C09: Receive
        LF_TEXT                = TEXT-C09.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SPEME'.
*       Text-C10: Blocked Stock
        LF_TEXT                = TEXT-C10.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'TOTALPO'.
*       Text-C11: Total PO
        LF_TEXT                = TEXT-C11.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'RESULT'.
*       Text-C12: Result
        LF_TEXT                = TEXT-C12.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'ENDSTK'.
*       Text-C13: E/Stk
        LF_TEXT                = TEXT-C13.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'ADJUST'.
*       Text-C14: Adjust
        LF_TEXT                = TEXT-C14.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'ASTK'.
*       Text-C15: A/Stk
        LF_TEXT                = TEXT-C15.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'DO'.
*       Text-C16: D/O
        LF_TEXT                = TEXT-C16.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'SOQTY'.
*       Text-C17: S/O
        LF_TEXT                = TEXT-C17.
        <L_FIELDCAT>-REPTEXT   = LF_TEXT.
        <L_FIELDCAT>-COLTEXT   = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_L = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_M = LF_TEXT.
        <L_FIELDCAT>-SCRTEXT_S = LF_TEXT.
      WHEN 'OUTSTANDING_SO'.
*       Text-C18: Outstanding SO
        LF_TEXT                = TEXT-C18.
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
