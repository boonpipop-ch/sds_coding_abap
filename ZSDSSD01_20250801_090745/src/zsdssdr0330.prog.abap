*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0330
*  Creation Date      : 03.10.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : SDI039
*  Description        : Program send manager approve status to Salesforce
*  Purpose            : when approve or cancel approve send scedule line to
*                       interface
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0330.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
TABLES: ZSDSSDS102,
        VBAK,
        SSCRFIELDS,
        ZSDSSDT009.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TCODE        TYPE SY-TCODE      VALUE 'ZSDSSD028',
  GC_REPID        TYPE SY-REPID      VALUE 'ZSDSSDR0330',
  GC_VBTYP_QT     TYPE VBAK-VBTYP    VALUE 'B',
  GC_INTFNO       TYPE ZSDSDE_INTFNO VALUE 'SDI039',
  GC_MSGTY_ERROR  TYPE MSGTY         VALUE 'E',
  GC_NOT_APPROVED TYPE CHAR1         VALUE 'N',
  GC_APPROVED     TYPE CHAR1         VALUE 'A'.

*----------------------------------------------------------------------*
* TYPES:
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TS_009,
    VBELN         TYPE ZSDSSDT009-VBELN,
    POSNR         TYPE ZSDSSDT009-POSNR,
    ETENR         TYPE ZSDSSDT009-ETENR,
    MGR_APPV_STAT TYPE ZSDSSDT009-MGR_APPV_STAT,
  END OF TS_009,
  TT_009 TYPE STANDARD TABLE OF TS_009,
  BEGIN OF TS_VBFA_SO_QT,
    SO_VBELN TYPE VBFA-VBELN,
    SO_POSNR TYPE VBFA-POSNN,
    QT_VBELN TYPE VBFA-VBELV,
    QT_POSNR TYPE VBFA-POSNV,
    QT_BNAME TYPE VBAK-BNAME,
  END OF TS_VBFA_SO_QT,
  TT_VBFA_SO_QT TYPE STANDARD TABLE OF TS_VBFA_SO_QT,
  TT_INPUT      TYPE STANDARD TABLE OF ZSDSSDS102,
  BEGIN OF TS_REP,
    QT_VBELN     TYPE VBAK-VBELN,
    QT_POSNR     TYPE VBAP-POSNR,
    SO_VBELN     TYPE VBEP-VBELN,
    SO_POSNR     TYPE VBEP-POSNR,
    SO_ETENR     TYPE VBEP-ETENR,
    SCH_STATUS   TYPE CHAR1,
    MSGTY        TYPE MSGTY,
    MESSAGE_CHK  TYPE TEXT200,
    REQNO        TYPE ZSDSCAT001-REQNO,
    RESP_STATUS  TYPE ZSDSDE_REST_STATUS,
    RESP_MESSAGE TYPE ZSDSDE_REST_MESSAGE,
    HTTP_CODE    TYPE ZSDSCAT001-HTTP_CODE,
    HTTP_REASON  TYPE ZSDSCAT001-HTTP_REASON,
  END OF TS_REP,
  TT_REP TYPE STANDARD TABLE OF TS_REP.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA:
  GT_INPUT TYPE STANDARD TABLE OF ZSDSSDS102 ##NEEDED,
  GT_REP   TYPE TT_REP ##NEEDED.
*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
DATA:
   GRT_AUART  TYPE  RANGE OF VBAK-AUART     ##NEEDED.
*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*
DATA:
  GF_MGR_APPV_STAT   TYPE  ZSDSSDT009-MGR_APPV_STAT ##NEEDED,
  GF_SCHEDULE_STATUS TYPE ZSDSSDS043-STATUS       ##NEEDED.

*----------------------------------------------------------------------*
* SCREEN DATA
*----------------------------------------------------------------------*
*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_INPUT' ITSELF
CONTROLS: TC_INPUT TYPE TABLEVIEW USING SCREEN 2000 ##NUMBER_OK.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_INPUT'
DATA:     G_TC_INPUT_LINES  LIKE SY-LOOPC ##NEEDED.

DATA:     OK_CODE LIKE SY-UCOMM           ##NEEDED.
*&SPWIZARD END ---

DATA: GF_OKCODE  TYPE SY-UCOMM.
*----------------------------------------------------------------------*
* ALV DATA
*----------------------------------------------------------------------*
CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE I VALUE 0,
  GC_ALV_HEIGHT_1    TYPE I VALUE 100.

INCLUDE ZSDSCAI9990 ##INCL_OK.
*-----------------------------------------------------------------------
* MACRO
*-----------------------------------------------------------------------
DEFINE %FCAT.
  <fcat>-scrtext_s = <fcat>-scrtext_m =
  <fcat>-scrtext_l = <fcat>-coltext =
  <fcat>-seltext   = <fcat>-tooltip =
  <fcat>-reptext   = &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS: RB_APPR RADIOBUTTON GROUP R1 DEFAULT 'X' MODIF ID BG,
              RB_CANC RADIOBUTTON GROUP R1 MODIF ID BG.
  PARAMETERS: P_IMPKEY TYPE ZSDSSDT020-ID NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK BLK1.
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-S02.
  PARAMETERS: RB_SO RADIOBUTTON GROUP R2 DEFAULT 'X' USER-COMMAND R2.
  SELECT-OPTIONS: S_VBELN FOR VBAK-VBELN MODIF ID SO,
                  S_APVDT FOR ZSDSSDT009-MGR_APPV_DATE MODIF ID SO,
                  S_APVBY FOR ZSDSSDT009-MGR_APPV_BY MODIF ID SO.
  PARAMETERS: RB_SCHL RADIOBUTTON GROUP R2 .
SELECTION-SCREEN END OF BLOCK BLK2 .
PARAMETERS: P_TEST AS CHECKBOX DEFAULT 'X'.

INCLUDE ZSDSSDR0330_O01.
INCLUDE ZSDSSDR0330_I01.
INCLUDE ZSDSSDR0330_F01.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SET_PROPERTIES.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF  SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_SEL_SCRN_VALIDATE.
  ENDIF.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_SELECT.
  IF P_IMPKEY IS NOT INITIAL.
    IMPORT INPUT = GT_INPUT FROM DATABASE ZSDSSDT020(IP) ID P_IMPKEY.
    DELETE FROM DATABASE ZSDSSDT020(IP) ID P_IMPKEY.
    PERFORM F_SEND_INTERFACE CHANGING GT_INPUT
                                      GT_REP.
    IF GT_REP[] IS INITIAL.
      "No data found.
      MESSAGE S003(ZSDSSD01).
    ELSE.
      PERFORM F_DISPLAY_REPORT USING GT_REP .
    ENDIF.
  ELSE.
    IF RB_SO = ABAP_TRUE.
      PERFORM F_RERUN_SO_DATA CHANGING GT_INPUT.
      PERFORM F_SEND_INTERFACE CHANGING GT_INPUT
                                        GT_REP.
      IF GT_REP[] IS INITIAL.
        "No data found.
        MESSAGE S003(ZSDSSD01).
      ELSE.
        PERFORM F_DISPLAY_REPORT USING GT_REP .
      ENDIF.

    ELSEIF RB_SCHL = ABAP_TRUE.
      CALL SCREEN 2000 . "input so, item, schedule line.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
