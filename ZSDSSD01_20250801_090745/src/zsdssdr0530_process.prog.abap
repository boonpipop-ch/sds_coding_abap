*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0530_PROCESS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_DATA .
  PERFORM F_SELECT_AND_PREPARE_DATA CHANGING GT_DATA.
  IF GT_DATA IS INITIAL.
*     Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM F_ADDITIONAL_DATA CHANGING GT_DATA.
  PERFORM F_DISPLAY_RESULT USING GT_DATA.
