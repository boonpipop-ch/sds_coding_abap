*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0110_PROCESS
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM SET_DATE.
*  PERFORM get_xml. "DEL by Ratchapol K. TISAD-2852
*  PERFORM GET_RATE.
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  PERFORM SET_SCREEN.
*AT SELECTION-SCREEN ON HELP REQUEST FOR <field>
* *AT SELECTION-SCREEN ON VALUE REQUEST FOR <field>
* *AT SELECTION-SCREEN ON <field>
* AT SELECTION-SCREEN.
*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data
*  PERFORM check_date.
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
*-> get data
  PERFORM F_GET_RATE.
  PERFORM GET_DATA.
  IF P_MANUAL = 'X'.
    PERFORM BUILD_FIELDCAT.
    PERFORM BUILD_EVENTS.
    PERFORM BUILD_LAYOUT.
    PERFORM LIST_DISPLAY.
  ELSEIF P_AUTO ='X'.
    PERFORM UPDATE_RATE_AUTO.
  ENDIF.
*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
