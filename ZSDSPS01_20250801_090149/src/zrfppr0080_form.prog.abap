
*&--------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.

  DATA:
    IT_TAB       TYPE GTT_RESULTS,
    LO_ALV       TYPE REF TO CL_SALV_TABLE,
    LF_MSG       TYPE REF TO CX_SALV_MSG,
    COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
    COLUMN       TYPE REF TO CL_SALV_COLUMN_TABLE,
    LR_DISPLAY   TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
    LR_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS.

  DATA(LO_OBJ) = NEW ZCL_ZRFPPR0080_TAB( ).

  DATA: L_TEXT TYPE STRING,
        L_ICON TYPE STRING.

*  IF gt_results IS NOT INITIAL.

  DATA LT_RESULTS TYPE GTT_RESULTS.

  LT_RESULTS = GT_RESULTS.

  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = LO_ALV
        CHANGING
          T_TABLE = LT_RESULTS ).
    CATCH CX_SALV_MSG INTO LF_MSG.
  ENDTRY.

  LO_ALV->SET_SCREEN_STATUS( REPORT = 'ZRFPPR0080'
                              PFSTATUS = 'STATUS_PCHK_2'
                              SET_FUNCTIONS = LO_ALV->C_FUNCTIONS_ALL ).

  LR_FUNCTIONS = LO_ALV->GET_FUNCTIONS( ).

  LR_FUNCTIONS->SET_ALL( ABAP_TRUE ).



  LR_DISPLAY = LO_ALV->GET_DISPLAY_SETTINGS( ).
  LR_DISPLAY->SET_LIST_HEADER( TEXT-H01 ).


  COLUMNS = LO_ALV->GET_COLUMNS( ).
  COLUMNS->SET_OPTIMIZE( ).
  "Hotspot the field after the execute button in pre check.
  TRY.
      COLUMN ?= COLUMNS->GET_COLUMN( 'AUFNR').
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.
  COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).

  SET HANDLER LO_OBJ->SET_ONCLICK FOR LO_ALV->GET_EVENT( ).
  SET HANDLER LO_OBJ->ON_LINK_CLICK FOR LO_ALV->GET_EVENT( ).

  LO_ALV->DISPLAY( ).
*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.
*  SET HANDLER LO_OBJ->SET_ONCLICK FOR LO_ALV->GET_EVENT( ).
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR '&F03'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
