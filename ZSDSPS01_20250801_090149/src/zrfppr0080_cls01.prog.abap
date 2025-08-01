*&---------------------------------------------------------------------*
*& Include          ZRFPPR0080_CLS01
*&---------------------------------------------------------------------*

CLASS ZCL_ZRFPPR0080_TAB DEFINITION.
  PUBLIC SECTION.
    DATA:
      IT_TAB       TYPE GTT_RESULTS,
      LO_ALV       TYPE REF TO CL_SALV_TABLE,
      LF_MSG       TYPE REF TO CX_SALV_MSG,
      COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
      COLUMN       TYPE REF TO CL_SALV_COLUMN_TABLE,
      LR_DISPLAY   TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
      LR_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS.

    METHODS:
      DISPLAY_ALV IMPORTING IT_TAB TYPE GTT_RESULTS,
      SET_COLORS  CHANGING CO_ALV   TYPE REF TO CL_SALV_TABLE
                           CT_TABLE TYPE GTT_RESULTS,
      SET_PF_STAT CHANGING CO_ALV   TYPE REF TO CL_SALV_TABLE,
      SET_ONCLICK FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
        IMPORTING E_SALV_FUNCTION,
      ON_LINK_CLICK FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN.

ENDCLASS.

CLASS ZCL_ZRFPPR0080_TAB IMPLEMENTATION.

  METHOD DISPLAY_ALV.
    DATA:
      LT_TAB TYPE GTT_RESULTS.

    LT_TAB = IT_TAB.

    TRY.
        CL_SALV_TABLE=>FACTORY(
          IMPORTING
            R_SALV_TABLE = LO_ALV
          CHANGING
            T_TABLE = LT_TAB ).
      CATCH CX_SALV_MSG INTO LF_MSG.
    ENDTRY.

    LR_FUNCTIONS = LO_ALV->GET_FUNCTIONS( ).

    LR_DISPLAY = LO_ALV->GET_DISPLAY_SETTINGS( ).
    LR_DISPLAY->SET_LIST_HEADER( TEXT-H00 ).

    ME->SET_PF_STAT( CHANGING CO_ALV = LO_ALV ).
    ME->SET_COLORS( CHANGING CO_ALV = LO_ALV
                         CT_TABLE = LT_TAB ).


    COLUMNS = LO_ALV->GET_COLUMNS( ).
    COLUMNS->SET_OPTIMIZE( ).
    "Hotspot the field after the execute button in pre check.
    TRY.
        COLUMN ?= COLUMNS->GET_COLUMN( 'AUFNR').
      CATCH CX_SALV_NOT_FOUND.
    ENDTRY.
    COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).

    SET HANDLER ME->SET_ONCLICK FOR LO_ALV->GET_EVENT( ).

    LO_ALV->DISPLAY( ).

  ENDMETHOD.

  METHOD SET_COLORS.
    DATA:
      LO_COLS_TAB TYPE REF TO CL_SALV_COLUMNS_TABLE,
      LO_COL_TAB  TYPE REF TO CL_SALV_COLUMN_TABLE.
    DATA:
      LS_COLOR TYPE LVC_S_COLO.

    LO_COLS_TAB = CO_ALV->GET_COLUMNS( ).

    INCLUDE <COLOR>.


    TRY.
        LO_COLS_TAB->SET_COLOR_COLUMN( 'T_COLOR' ).
      CATCH CX_SALV_DATA_ERROR.
    ENDTRY.
  ENDMETHOD.

  METHOD SET_PF_STAT.

    DATA LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST.
    DATA LR_FUNC TYPE REF TO CL_SALV_FUNCTIONS_LIST.


    CO_ALV->SET_SCREEN_STATUS( REPORT = 'ZRFPPR0080'
                               PFSTATUS = 'STAT_PCHK'
                               SET_FUNCTIONS = CO_ALV->C_FUNCTIONS_ALL ).
  ENDMETHOD.

  METHOD SET_ONCLICK.

    DATA:LV_FILENAME TYPE STRING,
         LV_PATH     TYPE STRING,
         LV_FULLPATH TYPE STRING,
         LV_RESULT   TYPE I.

    CASE E_SALV_FUNCTION.
      WHEN '&EXEC'.
        CALL SCREEN 2000.
      WHEN 'DOWNLOAD_R'.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
          EXPORTING
            WINDOW_TITLE      = 'Download File'
            DEFAULT_EXTENSION = 'XLS'
          CHANGING
            FILENAME          = LV_FILENAME
            PATH              = LV_PATH
            FULLPATH          = LV_FULLPATH
            USER_ACTION       = LV_RESULT.
        IF SY-SUBRC EQ 0.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
*             BIN_FILESIZE            =
              FILENAME                = LV_FULLPATH
              FILETYPE                = 'ASC'
*             APPEND                  = ' '
            TABLES
              DATA_TAB                = GT_RESULTS_DWN
*             FIELDNAMES              =
            EXCEPTIONS
              FILE_WRITE_ERROR        = 1
              NO_BATCH                = 2
              GUI_REFUSE_FILETRANSFER = 3
              INVALID_TYPE            = 4
              NO_AUTHORITY            = 5
              UNKNOWN_ERROR           = 6
              HEADER_NOT_ALLOWED      = 7
              SEPARATOR_NOT_ALLOWED   = 8
              FILESIZE_NOT_ALLOWED    = 9
              HEADER_TOO_LONG         = 10
              DP_ERROR_CREATE         = 11
              DP_ERROR_SEND           = 12
              DP_ERROR_WRITE          = 13
              UNKNOWN_DP_ERROR        = 14
              ACCESS_DENIED           = 15
              DP_OUT_OF_MEMORY        = 16
              DISK_FULL               = 17
              DP_TIMEOUT              = 18
              FILE_NOT_FOUND          = 19
              DATAPROVIDER_EXCEPTION  = 20
              CONTROL_FLUSH_ERROR     = 21
              OTHERS                  = 22.

          IF SY-SUBRC EQ 0.
            MESSAGE 'File saved successfully' TYPE 'S'.
          ELSE.
            MESSAGE 'File not saved successfully' TYPE 'E' DISPLAY LIKE 'S'.
          ENDIF.
        ENDIF.


      WHEN 'DOWNLOAD_E'.
        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
          EXPORTING
            WINDOW_TITLE      = 'Download File'
            DEFAULT_EXTENSION = 'XLS'
          CHANGING
            FILENAME          = LV_FILENAME
            PATH              = LV_PATH
            FULLPATH          = LV_FULLPATH
            USER_ACTION       = LV_RESULT.
        IF SY-SUBRC EQ 0.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
*             BIN_FILESIZE            =
              FILENAME                = LV_FULLPATH
              FILETYPE                = 'ASC'
*             APPEND                  = ' '
            TABLES
              DATA_TAB                = GT_ERR
*             FIELDNAMES              =
            EXCEPTIONS
              FILE_WRITE_ERROR        = 1
              NO_BATCH                = 2
              GUI_REFUSE_FILETRANSFER = 3
              INVALID_TYPE            = 4
              NO_AUTHORITY            = 5
              UNKNOWN_ERROR           = 6
              HEADER_NOT_ALLOWED      = 7
              SEPARATOR_NOT_ALLOWED   = 8
              FILESIZE_NOT_ALLOWED    = 9
              HEADER_TOO_LONG         = 10
              DP_ERROR_CREATE         = 11
              DP_ERROR_SEND           = 12
              DP_ERROR_WRITE          = 13
              UNKNOWN_DP_ERROR        = 14
              ACCESS_DENIED           = 15
              DP_OUT_OF_MEMORY        = 16
              DISK_FULL               = 17
              DP_TIMEOUT              = 18
              FILE_NOT_FOUND          = 19
              DATAPROVIDER_EXCEPTION  = 20
              CONTROL_FLUSH_ERROR     = 21
              OTHERS                  = 22.

          IF SY-SUBRC EQ 0.
            MESSAGE 'File saved successfully' TYPE 'S'.
          ELSE.
            MESSAGE 'File not saved successfully' TYPE 'E' DISPLAY LIKE 'S'.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD ON_LINK_CLICK.
    READ TABLE GT_RESULTS INDEX ROW ASSIGNING FIELD-SYMBOL(<FS_ROW>).
    IF SY-SUBRC EQ 0.

      SET PARAMETER ID 'ANR' FIELD <FS_ROW>-AUFNR.
      CALL TRANSACTION 'CO02' AND SKIP FIRST SCREEN.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
