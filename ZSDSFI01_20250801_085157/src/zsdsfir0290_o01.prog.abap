*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0290_O01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       Module  STATUS_ALV  OUTPUT
*----------------------------------------------------------------------*
*       Set GUI Status & GUI Title
*----------------------------------------------------------------------*
MODULE STATUS_ALV OUTPUT.
  CLEAR GT_EXCL[].

  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-NEW.
      IF GF_EDIT = ABAP_TRUE.
        APPEND GC_PRINT_1 TO GT_EXCL.
      ELSE.
        APPEND GC_SAVE_1 TO GT_EXCL.
      ENDIF.
      APPEND GC_CANCEL_1 TO GT_EXCL.
    WHEN  GC_EXEC_TY-DETAIL.
      APPEND GC_SAVE_1 TO GT_EXCL.
      APPEND GC_PRINT_1 TO GT_EXCL.
      APPEND GC_CANCEL_1 TO GT_EXCL.
    WHEN  GC_EXEC_TY-REPRINT.
      APPEND GC_SAVE_1 TO GT_EXCL.
      APPEND GC_CANCEL_1 TO GT_EXCL.
    WHEN  GC_EXEC_TY-CANCEL.
      APPEND GC_SAVE_1 TO GT_EXCL.
      APPEND GC_PRINT_1 TO GT_EXCL.
      IF GF_EDIT = ABAP_FALSE.
        APPEND GC_CANCEL_1 TO GT_EXCL.
      ENDIF.
  ENDCASE.

SET PF-STATUS '9000' EXCLUDING GT_EXCL ##STAT_UNDEF.
SET TITLEBAR  '9000' ##TITL_UNDEF.

ENDMODULE.                 " STATUS_0100_1  OUTPUT
*----------------------------------------------------------------------*
*       Module  DISPLAY_ALV  OUTPUT
*----------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.

* Display ALV Grid
  PERFORM F_ALV_DISPLAY.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
