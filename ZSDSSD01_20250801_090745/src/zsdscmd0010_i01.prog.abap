*----------------------------------------------------------------------*
***INCLUDE ZSDSCMD0010_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  LEAVE_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LEAVE_SCREEN INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK.
    WHEN 'LANG'.
      PERFORM GET_WARR_CONDTEXT_DF.
      PERFORM DISPLAY_WARR_CONDTEXT_DF.
    WHEN 'SAVE'.
      PERFORM VALIDATE_9100.
      PERFORM SAVE_9100_WARR_CONDTEXT_DF.
      MESSAGE TEXT-S02 TYPE 'S'."Default Warranty Condition Text Saved
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9200 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK.
    WHEN 'SAVE'.
      PERFORM SAVE_9200_WARR_COMP_DF.
      MESSAGE TEXT-S01  TYPE 'S'."Default warranty component Saved
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK.
    WHEN 'SAVE'.
      PERFORM VALIDATE_100.

      PERFORM SAVE_100.

      MESSAGE 'Warranty Letter Saved'(S03) TYPE 'S'.
    WHEN 'RADIO'.
      PERFORM ADJUST_WARRANTY_START_100.

    WHEN 'LANG'.
      READ TABLE GT_ZSDSCMT008 INTO GS_ZSDSCMT008 WITH KEY SPRAS = GV_LANG_TH_EN.
      IF SY-SUBRC <> 0.
        CLEAR GS_ZSDSCMT008.
      ENDIF.
      GS_ZSDSCMT008-SPRAS = GV_LANG_TH_EN.
      PERFORM APPEND_WARRANTY_TEXT_100.
    WHEN 'SUBMIT'.
      IF GS_ZSDSCMT005-STATUS IS NOT INITIAL AND ( GS_ZSDSCMT005-STATUS = '1' OR GS_ZSDSCMT005-STATUS = '4' ).
        PERFORM POPUP_GET_VALUES_100.
        PERFORM SUBMIT_TO_K2.
        PERFORM SAVE_100.
      ELSEIF GS_ZSDSCMT005-STATUS IS NOT INITIAL AND NOT ( GS_ZSDSCMT005-STATUS = '1' OR  GS_ZSDSCMT005-STATUS = '4' ) ##BOOL_OK.
        "Warranty Letter already submitted
        MESSAGE TEXT-E05 TYPE 'E'.
      ELSEIF GS_ZSDSCMT005-WL_ID IS INITIAL.
        "Please save warranty letter first
        MESSAGE TEXT-E06 TYPE 'E'.
      ENDIF.
    WHEN 'PRINT'.
      IF GS_ZSDSCMT005-STATUS = '3'.
        PERFORM PRINT_FORM.
      ELSE.
        MESSAGE TEXT-E10 TYPE 'I'. "The document has not approved yet
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  APPEND_TEXT_DATA_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE APPEND_TEXT_DATA_0100 INPUT.

  PERFORM APPEND_WARRANTY_TEXT_100.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ADJUST_WARRANTY_START  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ADJUST_WARRANTY_START INPUT.
  PERFORM ADJUST_WARRANTY_START_100.
ENDMODULE.
