*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0630_PROCESS
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  LCL_DATA=>INIT_DATA( ).
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'REP01'.
      CLEAR SSCRFIELDS-UCOMM.
      LCL_DATA=>DELETE_DATA( ).
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
*
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
  IF GT_RESULT[] IS NOT INITIAL.
    LCL_DATA=>GET_ADDTIONAL_DATA( ).
    LCL_DATA=>SHOW_REPORT( ).
  ELSE.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ENDIF.

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
