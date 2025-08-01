
*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0230
*  Creation Date      : 13.05.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : FIAR023
*  Description        : After Logistic dept. finish create billing,
*                       document has checked status and will send to
*                       Credit  dept. to check for compleness
*
*                       -> Related table ZSDSFIT027, ZSDSFIT028
*  Purpose            : Program Check Invoice from Logistic
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  06/01/2025  F36K910667  Boontip R.  CH01
*              F36K910884
* - IMS#420000147 - Project doest not display in column, try to get from item which contain wbs
* - IMS#420000161 - Change filter customer from sold to -> payer
* - IMS#420000161 - add column payer and payer name to all layout
* - IMS#420000161 - report in display mode does not show, change inner join with SO( vbap,vbak) to left outer join
* - IMS#420000161 - report by project in receive / display mode , remove check FKSTK
* - IMS#420000145 - add select all/ deselect all
*-----------------------------------------------------------------------
* 21/01/2025   F36K911359  Boontip R.  CH02
* IMS#420000145 remove Logic check if found send/receive by DO then unable to send invoice
*-----------------------------------------------------------------------
* 28/01/2025   F36K916416  Boontip R.  CH04
* CR420000321 - option 'display' must show all documents including not processed
*send/receive
*-----------------------------------------------------------------------
* 24/06/2025  F36K919964  Boontip R.  CH05
* IMS#420000159 add Personal ID to table ZSDSFIT027
* when invoice received for billing collector program
*-----------------------------------------------------------------------
REPORT ZSDSFIR0230.

INCLUDE ZSDSCAI9990 ##INCL_OK.
INCLUDE ZSDSFIR0230_TOP.
INCLUDE ZSDSFIR0230_O01.
INCLUDE ZSDSFIR0230_I01.
INCLUDE ZSDSFIR0230_F01.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_SEL_SCRN_SET_PROPERTIES.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF  SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_SEL_SCRN_VALIDATE.
  ENDIF.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_VALUE.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-SN_IV.
      PERFORM F_GET_SN_IV CHANGING GT_SN_IV.
      GT_SN_IV_OLD[] = GT_SN_IV[].
      IF GT_SN_IV IS INITIAL.
*     Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
*      PERFORM F_LOCK_ZSDSFIT027 USING GT_SN_IV. "CH03 420000360-
    WHEN GC_EXEC_TY-RC_IV.
      PERFORM F_GET_RC_IV CHANGING GT_RC_IV.
      GT_RC_IV_OLD[] = GT_RC_IV[].
      IF GT_RC_IV IS INITIAL.
*     Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
*      PERFORM F_LOCK_ZSDSFIT027 USING GT_RC_IV. "CH03 420000360-
    WHEN GC_EXEC_TY-DP_IV.
      PERFORM F_GET_DP_IV CHANGING GT_DP_IV.
      IF GT_DP_IV IS INITIAL.
*     Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
    WHEN GC_EXEC_TY-SN_PJ
    OR   GC_EXEC_TY-RC_PJ
    OR   GC_EXEC_TY-DP_PJ .
      PERFORM F_GET_PJ CHANGING GT_PJ.
      GT_PJ_OLD[] = GT_PJ[].
      IF GT_PJ IS INITIAL.
*     Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
*      IF GF_EDIT = ABAP_TRUE.
*        PERFORM F_LOCK_ZSDSFIT028 USING GT_PJ. "CH03 420000360-
*      ENDIF.
  ENDCASE.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-SN_IV.
      PERFORM F_DISPLAY_RESULT USING GT_SN_IV.
*      PERFORM F_UNLOCK_ZSDSFIT027 USING GT_SN_IV. "CH03 420000360-
    WHEN GC_EXEC_TY-RC_IV.
      PERFORM F_DISPLAY_RESULT USING GT_RC_IV.
*      PERFORM F_UNLOCK_ZSDSFIT027 USING GT_RC_IV. "CH03 420000360-
    WHEN GC_EXEC_TY-DP_IV.
      PERFORM F_DISPLAY_RESULT USING GT_DP_IV.
    WHEN GC_EXEC_TY-SN_PJ
    OR   GC_EXEC_TY-RC_PJ
    OR   GC_EXEC_TY-DP_PJ.
      PERFORM F_DISPLAY_RESULT USING GT_PJ.
*      IF GF_EDIT = ABAP_TRUE.
*        PERFORM F_UNLOCK_ZSDSFIT028 USING GT_PJ. "CH03 420000360-
*      ENDIF.
  ENDCASE.
