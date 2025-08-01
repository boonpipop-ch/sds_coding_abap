*----------------------------------------------------------------------*
***INCLUDE LZSDSFI19I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE GF_OKCODE.
    WHEN 'YES'.
      CALL METHOD GR_TEXT_EDIT->GET_TEXT_AS_STREAM ##SUBRC_OK
        IMPORTING
          TEXT                   = GT_TEXT "TEXT AS STREAM WITH CARRIGE RETRUNS AND LINEFEEDS
        EXCEPTIONS
          ERROR_DP               = 1
          ERROR_CNTL_CALL_METHOD = 2
          OTHERS                 = 3.
      IF SY-SUBRC <> 0 ##NEEDED.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO            "CH01-
*                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .        "CH01-
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'NO'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
