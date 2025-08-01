*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0320
*  Creation Date      : 05.06.2024
*  Author             : Thanapong C. (Eviden)
*  Add-on ID          : FSCMR013
*  Description        : Customer Credit Report
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  21.01.2025  420000174   WuthichaiL. - Add columns for start/end date
*                                        and WBS status
*-----------------------------------------------------------------------

REPORT ZSDSFIR0320.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE ZSDSFIR0320_TOP.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS: S_BP    FOR ZSDSFIT036-KUNNR OBLIGATORY,
                  S_WBS   FOR ZSDSFIT036-PSPHI.

  PARAMETERS: P_CSGMT TYPE UKMBP_CMS_SGM-CREDIT_SGMNT OBLIGATORY DEFAULT '1000'.

  SELECT-OPTIONS: S_CGRP  FOR UKMBP_CMS-CREDIT_GROUP ,
                  S_RLCLS FOR UKMBP_CMS-RISK_CLASS,
                  S_CLT   FOR UKMBP_CMS_SGM-CREDIT_LIMIT,
                  S_BUSAB FOR KNB1-BUSAB.
SELECTION-SCREEN END OF BLOCK B1.

*<-- Start of Insertion 420000174 21.01.2025 (New Criteria)
*Text-S09: WBS Status
SELECTION-SCREEN BEGIN OF BLOCK SB1 WITH FRAME TITLE TEXT-S09.
  PARAMETERS: RB_OPEN  TYPE CHAR1 RADIOBUTTON GROUP G_3,
              RB_CLOSE TYPE CHAR1 RADIOBUTTON GROUP G_3,
              RB_ALL   TYPE CHAR1 RADIOBUTTON GROUP G_3 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK SB1.
*--> End of Insertion 420000174 21.01.2025

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS P_LYTCUS RADIOBUTTON GROUP G_1.
    "Text-s04: Customer Permanent Credit
    SELECTION-SCREEN: COMMENT (50) TEXT-S04 FOR FIELD P_LYTCUS.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS P_LYTWBS RADIOBUTTON GROUP G_1.
    "Text-s05: Project (WBS) Credit
    SELECTION-SCREEN: COMMENT (50) TEXT-S05 FOR FIELD P_LYTWBS.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS P_LYTALL RADIOBUTTON GROUP G_1 DEFAULT 'X'.
    "Text-s06: Customer Permanent Credit and Project (WBS) Credit
    SELECTION-SCREEN: COMMENT (50) TEXT-S06 FOR FIELD P_LYTALL.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS P_ALL RADIOBUTTON GROUP G_2 DEFAULT 'X'.
    "Text-s07: All Customer
    SELECTION-SCREEN: COMMENT (50) TEXT-S07 FOR FIELD P_ALL.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS P_EXC RADIOBUTTON GROUP G_2.
    "Text-s08: Exclude Block Customer
    SELECTION-SCREEN: COMMENT (50) TEXT-S08 FOR FIELD P_EXC.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_INIT_VARIABLE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON S_WBS.
  PERFORM F_VALIDATE_PROJECT USING S_WBS[].

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_GET_DATA CHANGING GT_RESULT.
  IF GT_RESULT IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
  PERFORM F_DISPLAY_RESULT.
*
*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSFIR0320_ALV.
  INCLUDE ZSDSFIR0320_F01.
