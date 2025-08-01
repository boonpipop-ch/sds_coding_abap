*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0120_STATUS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'STATUS_0100'.



  IF TXT_PRCNT IS INITIAL.
    TXT_PRCNT = 100.
  ENDIF.

  IF TXT_VAT IS INITIAL.
    TXT_VAT = 7.
  ENDIF.

  IF TXT_WHT IS INITIAL.
    TXT_WHT = 3.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'STATUS_0200'.

  IF TXT_VAT IS INITIAL.
    TXT_VAT = 7.
  ENDIF.

  IF TXT_WHT IS INITIAL.
    TXT_WHT = 3.
  ENDIF.

ENDMODULE.                 " STATUS_0200  OUTPUT
