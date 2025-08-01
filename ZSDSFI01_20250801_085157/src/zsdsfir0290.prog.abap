*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0290
*  Creation Date      : 06.06.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : FIAR011
*  Description        : Program Bill Placement will select open AR document
*                       by customer with due date and bill placement date
*                       and number, print and sent to customer
*  Purpose            : Bill Placement
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  24.03.2025  F36K914638  Boontip R.  420000493 (CH01)-Add NAME3, NAME4 to form
*-----------------------------------------------------------------------
*  17.06.2025  F36K919508  Boontip R.  420000159 (CH02)-add field pernr from
*                                      selection screen -> update table ZSDSFIT033
*-----------------------------------------------------------------------
*  14.07.2025  F36K921101  Boontip R.  420000694 (CH03)- print bill placement
*                                      sorted by invoice date
*-----------------------------------------------------------------------

REPORT ZSDSFIR0290 .
INCLUDE ZSDSCAI9990 ##INCL_OK.
INCLUDE ZSDSFIR0290_TOP.
INCLUDE ZSDSFIR0290_O01.
INCLUDE ZSDSFIR0290_I01.
INCLUDE ZSDSFIR0290_F01.
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
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_GENC .
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_VALUE.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-NEW.
      PERFORM F_SELECT_AND_PREPARE_DATA_NEW.
      GT_DATA1_OLD[] = GT_DATA1[].
      IF GT_DATA1 IS INITIAL.
*     Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
    WHEN  GC_EXEC_TY-DETAIL
    OR    GC_EXEC_TY-REPRINT
    OR    GC_EXEC_TY-CANCEL.
      PERFORM F_SELECT_AND_PREPARE_DATA_EXST.
      IF GT_DATA1 IS INITIAL.
*     Message: No data found.
        MESSAGE S001(ZSDSCA01).
        RETURN.
      ENDIF.
  ENDCASE.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM F_DISPLAY_RESULT USING GT_DATA1.
