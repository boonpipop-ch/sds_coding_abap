*----------------------------------------------------------------------*
***INCLUDE LZSDSFI19F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_init_global
*&---------------------------------------------------------------------*
FORM F_INIT_GLOBAL .
  IF NOT GR_CUSTOM_CONT IS INITIAL.
    CALL METHOD GR_CUSTOM_CONT->FREE ##SUBRC_OK
      EXCEPTIONS
        OTHERS = 1.
    IF SY-SUBRC NE 0                         ##NEEDED.
    ENDIF.
    FREE GR_CUSTOM_CONT.
  ENDIF.

  IF NOT GR_TEXT_EDIT IS INITIAL.
    CALL METHOD GR_TEXT_EDIT->FREE ##SUBRC_OK
      EXCEPTIONS
        OTHERS = 1.
    IF SY-SUBRC <> 0                         ##NEEDED.
    ENDIF.
    FREE GR_TEXT_EDIT.
  ENDIF.

  CALL METHOD CL_GUI_CFW=>FLUSH ##SUBRC_OK
    EXCEPTIONS
      OTHERS = 1.
  IF SY-SUBRC NE 0                           ##NEEDED.
  ENDIF.

  FREE : GT_TEXT  ,
         GT_OLD_TEXT.
ENDFORM.
