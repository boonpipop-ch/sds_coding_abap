*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0550_PROCESS
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  LCL_DATA=>CHECK_SCREEN( ).

*AT SELECTION-SCREEN ON P_START.
*  LCL_DATA=>CHECK_START( ).
*
*AT SELECTION-SCREEN ON P_END.
*  LCL_DATA=>CHECK_END( ).

*AT SELECTION-SCREEN ON HELP REQUEST FOR <field>
* *AT SELECTION-SCREEN ON VALUE REQUEST FOR <field>
* *AT SELECTION-SCREEN ON <field>
* AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  LCL_DATA=>GET_DATA( ).
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
  IF GV_ERROR IS INITIAL.
    IF GT_RESULT[] IS NOT INITIAL.
      LCL_DATA=>GET_ADDTIONAL_DATA( ).
      LCL_DATA=>SHOW_REPORT( ).
    ELSE.
      MESSAGE S003 DISPLAY LIKE GC_E.
    ENDIF.
  ELSE.
    LCL_DATA=>SHOW_MESSAGE_ERROR( GV_ERROR ).
  ENDIF.

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
