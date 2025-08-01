*----------------------------------------------------------------------*
***INCLUDE LZSDSFI19O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TTITLE_100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PAI_SET_CONTAINER OUTPUT
*&---------------------------------------------------------------------*
MODULE PAI_SET_CONTAINER OUTPUT.
  IF GR_CUSTOM_CONT IS NOT BOUND.

    CREATE OBJECT GR_CUSTOM_CONT
      EXPORTING
        CONTAINER_NAME = 'EDITOR' " Name of the Screen CustCtrl
        REPID          = SY-REPID
        DYNNR          = '0100'.
  ENDIF.
  IF GR_TEXT_EDIT IS NOT BOUND.

    CREATE OBJECT GR_TEXT_EDIT
      EXPORTING
        WORDWRAP_MODE = 1               " 0: OFF; 1: wrap a window border; 2: wrap at fixed pos
*       wordwrap_position = 254         " pos of wordwrap, only makes sense with wordwrap_mode=2
        PARENT        = GR_CUSTOM_CONT. " PARENT CONTAINER

  ENDIF.
*BOI CH01
  CALL METHOD GR_TEXT_EDIT->SET_TEXT_AS_STREAM ##SUBRC_OK
    EXPORTING
      TEXT            = GT_TEXT "TEXT AS STREAM WITH CARRIGE RETRUNS AND LINEFEEDS
    EXCEPTIONS
      ERROR_DP        = 1
      ERROR_DP_CREATE = 2
      OTHERS          = 3.
*EOI CH01
*BOD CH01
*  CALL METHOD GR_TEXT_EDIT->SET_TEXT_AS_R3TABLE
*    EXPORTING
*      TABLE           = GT_TEXT
*    EXCEPTIONS
*      ERROR_DP        = 1
*      ERROR_DP_CREATE = 2
*      OTHERS          = 3.
*EOD CH01
  IF SY-SUBRC <> 0 ##NEEDED.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO     "CH01+
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 . "CH01+
  ENDIF.


ENDMODULE.
