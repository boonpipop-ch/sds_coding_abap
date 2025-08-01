*&---------------------------------------------------------------------*
*& Include          ZSDSFII0030_PROCESS
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_GET_SELECTION.
*AT SELECTION-SCREEN ON HELP REQUEST FOR <field>
* *AT SELECTION-SCREEN ON VALUE REQUEST FOR <field>
* *AT SELECTION-SCREEN ON <field>
* AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  IF S_VBELN IS NOT INITIAL OR S_IHREZ IS NOT INITIAL OR S_VBELNS IS NOT INITIAL.
    PERFORM F_GET_DOC_TYPE.
    "PERFORM f_check_data.
    IF GV_ERROR NE GC_ERROR.
      PERFORM F_GET_DATA.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
  CASE GV_ERROR.
    WHEN GC_ERROR.
      MESSAGE S000 WITH TEXT-100 DISPLAY LIKE 'E'.
    WHEN OTHERS.
      IF GT_HEADER[] IS NOT INITIAL.
        COLTROL_QT-TOP_LINE = 0.
        PERFORM F_SHOW_REPORT.
      ELSE.
        MESSAGE S003 DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
