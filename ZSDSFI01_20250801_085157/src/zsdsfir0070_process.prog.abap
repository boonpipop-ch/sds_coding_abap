*&---------------------------------------------------------------------*
*& Include          ZSDSFII0070_PROCESS
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
*
*AT SELECTION-SCREEN ON HELP REQUEST FOR <field>
* *AT SELECTION-SCREEN ON VALUE REQUEST FOR <field>
* *AT SELECTION-SCREEN ON <field>
* AT SELECTION-SCREEN.
*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
  PERFORM GET_PATH_NAME CHANGING P_PATH.
*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM GET_DATA .
  PERFORM PREPARE_DATA.
  IF NOT R_RPT IS INITIAL.
     PERFORM PRINT_REPORT.
  ELSEIF NOT R_PND1 IS INITIAL.
     PERFORM EXPORT_PND1.
     PERFORM F_DOWNLOAD_FILE USING P_PATH.
  ELSEIF NOT R_PND2 IS INITIAL.
     PERFORM EXPORT_PND2.
     PERFORM F_DOWNLOAD_FILE USING P_PATH.
  ELSEIF NOT R_PND3 IS INITIAL.
     PERFORM EXPORT_PND3.
     PERFORM F_DOWNLOAD_FILE USING P_PATH.
  ELSEIF NOT R_PND53 IS INITIAL.
     PERFORM EXPORT_PND53.
     PERFORM F_DOWNLOAD_FILE USING P_PATH.
  ENDIF.
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*  IF gt_result[] IS NOT INITIAL.
*    lcl_data=>get_addtional_data( ).
*    lcl_data=>show_report( ).
*  ELSE.
*    MESSAGE s004 DISPLAY LIKE 'E'.
*  ENDIF.

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
