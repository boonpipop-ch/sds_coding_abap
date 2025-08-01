*-----------------------------------------------------------------------
*  Program ID         : ZSDSCOR0040
*  Creation Date      : 07.10.2024
*  Author             : Kittirat C.(Eviden)
*  Add-on ID          : ZCOPAR001
*  Description        : Management Profit & Loss Report
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
REPORT ZSDSCOR0040.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: T001,
        ACDOCA,
        CEPC,
        T2500,
        T179,
        KNA1,
        TVBUR,
        TVKGR,
        TKUKL,
        MARA.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA: GRT_LEDGER            TYPE RANGE OF ACDOCA-RLDNR         ##NEEDED.
DATA: GRT_GL_MARKET_SEG     TYPE RANGE OF ACDOCA-RACCT         ##NEEDED.
DATA: GRT_GL_NO_MARKET_SEG  TYPE RANGE OF MARA-MTART           ##NEEDED.

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
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.
  SELECT-OPTIONS: S_BUKRS FOR T001-BUKRS    OBLIGATORY
                                            NO INTERVALS
                                            NO-EXTENSION
                                            DEFAULT '1000'.
  SELECT-OPTIONS: S_POPER FOR ACDOCA-POPER  OBLIGATORY
                                            NO INTERVALS
                                            NO-EXTENSION
                                            DEFAULT SY-DATUM+4(2).
  SELECT-OPTIONS: S_RYEAR FOR ACDOCA-RYEAR  OBLIGATORY
                                            NO INTERVALS
                                            NO-EXTENSION
                                            DEFAULT SY-DATUM+0(4).
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.
  SELECT-OPTIONS: S_PRCTR FOR CEPC-PRCTR.
  SELECT-OPTIONS: S_LOB   FOR T2500-WWLO.
  SELECT-OPTIONS: S_SBU   FOR MARA-PRDHA+0(5).
  SELECT-OPTIONS: S_DIST  FOR ACDOCA-ZZ1_SDSDIST_MSE.
  SELECT-OPTIONS: S_KUNNR FOR KNA1-KUNNR.
  SELECT-OPTIONS: S_VKBUR FOR TVBUR-VKBUR.
  SELECT-OPTIONS: S_VKGRP FOR TVKGR-VKGRP.
  SELECT-OPTIONS: S_KUKLA FOR TKUKL-KUKLA.
SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*  PERFORM F_GET_CONSTANT.
*  PERFORM F_DEFAULT_SELSCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_GLOB_VAR.
  PERFORM F_GET_DATA.
*  IF GT_RESULT IS INITIAL.
**   Message: No data found.
*    MESSAGE S001(ZSDSCA01).
*    RETURN.
*  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
** Display Processing Result
*  PERFORM F_DISPLAY_RESULT USING GT_RESULT.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_INIT_GLOB_VAR
*&---------------------------------------------------------------------*
*& Initialize Global Variables
*&---------------------------------------------------------------------*
FORM F_INIT_GLOB_VAR .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& Get Data
*&---------------------------------------------------------------------*
FORM F_GET_DATA .
  PERFORM GET_ACCOUNT_MARKET_SEG.
  PERFORM GET_ACCOUNT_WO_MARKET_SEG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ACCOUNT_MARKET_SEG
*&---------------------------------------------------------------------*
*& Get Account Data Based on Market Segment
*&---------------------------------------------------------------------*
FORM GET_ACCOUNT_MARKET_SEG .
  SELECT FROM ACDOCA
  FIELDS *
  WHERE RLDNR     IN @GRT_LEDGER
    AND RBUKRS    IN @S_BUKRS
    AND RYEAR     IN @S_RYEAR
    AND RACCT     IN @GRT_GL_MARKET_SEG
    AND POPER     IN @S_POPER
    AND KUNNR     IN @S_KUNNR
    AND WWLO_PA   IN @S_LOB
    AND VKBUR_PA  IN @S_VKBUR
    AND VKGRP_PA  IN @S_VKGRP
    AND PAPH1_PA  IN @S_SBU
    AND ZZ1_SDSDIST_MSE IN @S_DIST
    AND ZZ1_SHOPTYPE  IN @S_KUKLA
  INTO TABLE @DATA(LT_ACDOCA).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ACCOUNT_WO_MARKET_SEG
*&---------------------------------------------------------------------*
*& Get Account Data without Market Segment
*&---------------------------------------------------------------------*
FORM GET_ACCOUNT_WO_MARKET_SEG .
  SELECT FROM ACDOCA
  FIELDS *
  WHERE RLDNR     IN @GRT_LEDGER
    AND RBUKRS    IN @S_BUKRS
    AND RYEAR     IN @S_RYEAR
    AND RACCT     IN @GRT_GL_MARKET_SEG
    AND POPER     IN @S_POPER
    AND KUNNR     IN @S_KUNNR
    AND WWLO_PA   IN @S_LOB
    AND VKBUR_PA  IN @S_VKBUR
    AND VKGRP_PA  IN @S_VKGRP
    AND PAPH1_PA  IN @S_SBU
    AND ZZ1_SDSDIST_MSE IN @S_DIST
    AND ZZ1_SHOPTYPE  IN @S_KUKLA
  INTO TABLE @DATA(LT_ACDOCA).
ENDFORM.
