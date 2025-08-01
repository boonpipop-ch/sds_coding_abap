*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0390_O01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       Module  STATUS_ALV  OUTPUT
*----------------------------------------------------------------------*
*       Set GUI Status & GUI Title
*----------------------------------------------------------------------*
MODULE STATUS_ALV OUTPUT.

* Check ALV 1 in Edit Mode?
  IF GS_LAYOUT_1-EDIT EQ SPACE.
    READ TABLE GT_FIELDCAT_1 TRANSPORTING NO FIELDS
                             WITH KEY EDIT = 'X'.
    IF SY-SUBRC NE 0.
      APPEND GC_SAVE_1 TO GT_EXCL.
    ENDIF.
  ENDIF.

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
