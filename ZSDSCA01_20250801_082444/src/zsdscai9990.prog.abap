*-----------------------------------------------------------------------
*  Program ID         : ZSDSCAI9990
*  Creation Date      : 21.12.2023
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program which contains logic for
*                       displaying report as ALV Grid.
*  Purpose            : Include program for ALV Processing
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
*-- Class
*----------------------------------------------------------------------*
CLASS:
  LCL_EVENT_HANDLER_1 DEFINITION DEFERRED.  "for event handling

*----------------------------------------------------------------------*
*-- Tables
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Type definitions
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Constants
*----------------------------------------------------------------------*
CONSTANTS:
  GC_SAVE_ALL TYPE CHAR1         VALUE 'A',                 "#EC NEEDED
  GC_SAVE_1   TYPE SY-UCOMM      VALUE 'SAVE_1'.            "#EC NEEDED

*----------------------------------------------------------------------*
*-- Internal Tables
*----------------------------------------------------------------------*
DATA:
  GT_FIELDCAT_1 TYPE LVC_T_FCAT,                            "#EC NEEDED
  GT_SORT_1     TYPE LVC_T_SORT,                            "#EC NEEDED
  GT_TOOL_EXC_1 TYPE UI_FUNCTIONS,                          "#EC NEEDED
  GT_EXCL       TYPE STANDARD TABLE OF SY-UCOMM.            "#EC NEEDED

DATA:
  GT_FIELDCAT_2 TYPE LVC_T_FCAT,                            "#EC NEEDED
  GT_TOOL_EXC_2 TYPE UI_FUNCTIONS,                          "#EC NEEDED
  GT_SORT_2     TYPE LVC_T_SORT.                            "#EC NEEDED

*----------------------------------------------------------------------*
*-- Work Areas
*----------------------------------------------------------------------*
DATA:
  GS_VARIANT_1  TYPE DISVARIANT,                            "#EC NEEDED
  GS_LAYOUT_1   TYPE LVC_S_LAYO,                            "#EC NEEDED
  GS_FIELDCAT_1 TYPE LVC_S_FCAT,                            "#EC NEEDED
  GS_SORT_1     TYPE LVC_S_SORT,                            "#EC NEEDED
  GS_EXCL       TYPE SY-UCOMM,                              "#EC NEEDED
  GS_PRINT_1    TYPE LVC_S_PRNT.                            "#EC NEEDED

DATA:
  GS_VARIANT_2  TYPE DISVARIANT,                            "#EC NEEDED
  GS_LAYOUT_2   TYPE LVC_S_LAYO,                            "#EC NEEDED
  GS_FIELDCAT_2 TYPE LVC_S_FCAT,                            "#EC NEEDED
  GS_SORT_2     TYPE LVC_S_SORT,                            "#EC NEEDED
  GS_PRINT_2    TYPE LVC_S_PRNT.                            "#EC NEEDED

*----------------------------------------------------------------------*
*-- Ranges
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Variable declarations
*----------------------------------------------------------------------*
DATA:
  GF_CONTAINER_1          TYPE SCRFNAME,                    "#EC NEEDED
  GF_OK_CODE_1            TYPE SY-UCOMM,                    "#EC NEEDED
  GF_SAVE_OK              TYPE SY-UCOMM,                    "#EC NEEDED
  GREF_CUSTOM_CONTAINER_1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "#EC NEEDED
  GREF_GRID_1             TYPE REF TO CL_GUI_ALV_GRID,      "#EC NEEDED
  GREF_GRID_2             TYPE REF TO CL_GUI_ALV_GRID,      "#EC NEEDED
  GREF_DOCK_CONTAINER_1   TYPE REF TO CL_GUI_DOCKING_CONTAINER, "#EC NEEDED
  GREF_EVENT_RECEIVER_1   TYPE REF TO LCL_EVENT_HANDLER_1,  "#EC NEEDED
  GREF_SPLITTER_1         TYPE REF TO CL_GUI_SPLITTER_CONTAINER, "#EC NEEDED
  GREF_CONTAINER_GRID_1   TYPE REF TO CL_GUI_CONTAINER,     "#EC NEEDED
  GREF_CONTAINER_GRID_2   TYPE REF TO CL_GUI_CONTAINER,     "#EC NEEDED
  GREF_CONTAINER_HTML_1   TYPE REF TO CL_GUI_CONTAINER,     "#EC NEEDED
  GREF_DYNDOC_ID_1        TYPE REF TO CL_DD_DOCUMENT,       "#EC NEEDED
  GF_ALV_HEADER_1         TYPE FLAG,                        "#EC NEEDED
  GF_HEADER_HIGHT_1       TYPE I VALUE 20,                  "#EC NEEDED
  GF_SECOND_ALV_1         TYPE FLAG,                        "#EC NEEDED
  GF_ALV_HEIGHT_1         TYPE I VALUE 20,                  "#EC NEEDED
  GF_ALV_HEIGHT_2         TYPE I VALUE 20,                  "#EC NEEDED
  GF_SOFT_REFRESH_1       TYPE FLAG,                        "#EC NEEDED
  GF_REFRESH_ALL_1        TYPE FLAG VALUE 'X',              "#EC NEEDED
  GF_SOFT_REFRESH_2       TYPE FLAG,                        "#EC NEEDED
  GF_REFRESH_ALL_2        TYPE FLAG VALUE 'X',              "#EC NEEDED
  GF_NO_AUTO_REFRESH_1    TYPE FLAG,                        "#EC NEEDED
  GF_NO_AUTO_REFRESH_2    TYPE FLAG.                        "#EC NEEDED

*----------------------------------------------------------------------*
*-- Field-Symbols
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <G_LIST_1> TYPE STANDARD TABLE ##FS_ASSIGN_OK,            "#EC NEEDED
  <G_LIST_2> TYPE STANDARD TABLE                          "#EC FD_ASSGN
  .                                                         "#EC NEEDED

*----------------------------------------------------------------------*
*-- GenC Constants
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Macros
*----------------------------------------------------------------------*

************************************************************************
*-----------------------------------------------------------------------
* CLASS DEFINITION
*-----------------------------------------------------------------------
*Event Handler
CLASS LCL_EVENT_HANDLER_1 DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_PRINT_TOP_OF_PAGE_1 FOR EVENT PRINT_TOP_OF_PAGE OF CL_GUI_ALV_GRID,

      ON_TOP_OF_PAGE_1       FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
        IMPORTING E_DYNDOC_ID,

      ON_HOTSPOT_CLICK_1     FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID,

      ON_DOUBLE_CLICK_1      FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN,

      ON_BUTTON_CLICK_1      FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
        IMPORTING ES_ROW_NO ES_COL_ID,

      HANDLE_TOOLBAR_1       FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,

      HANDLE_USER_COMMAND_1  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

      ON_DATA_CHANGED_1 FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED
                  E_ONF4
                  E_ONF4_BEFORE
                  E_ONF4_AFTER
                  E_UCOMM,

      ON_PRINT_TOP_OF_PAGE_2 FOR EVENT PRINT_TOP_OF_PAGE OF CL_GUI_ALV_GRID,

      ON_HOTSPOT_CLICK_2     FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID,

      ON_DOUBLE_CLICK_2      FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN,

      ON_BUTTON_CLICK_2      FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
        IMPORTING ES_ROW_NO ES_COL_ID,

      HANDLE_TOOLBAR_2       FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,

      HANDLE_USER_COMMAND_2  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

ENDCLASS.                    "lcl_event_handler DEFINITION

*-----------------------------------------------------------------------
* CLASS IMPLEMENTATION
*-----------------------------------------------------------------------
* Event Handler
CLASS LCL_EVENT_HANDLER_1 IMPLEMENTATION.

  METHOD ON_TOP_OF_PAGE_1.
    PERFORM F_TOP_OF_PAGE_1 IN PROGRAM (SY-CPROG)
      USING E_DYNDOC_ID
      IF FOUND.
  ENDMETHOD.                    "on_print_top_of_page

  METHOD ON_PRINT_TOP_OF_PAGE_1.
    PERFORM F_PRINT_TOP_OF_PAGE_1 IN PROGRAM (SY-CPROG)
      IF FOUND.
  ENDMETHOD.                    "on_print_top_of_page

  METHOD ON_HOTSPOT_CLICK_1.
    PERFORM F_HOTSPOT_CLICK_1 IN PROGRAM (SY-CPROG)
      USING E_ROW_ID E_COLUMN_ID
      IF FOUND.
  ENDMETHOD.                    "on_hotspot_click

  METHOD ON_DOUBLE_CLICK_1.
    PERFORM F_HOTSPOT_CLICK_1 IN PROGRAM (SY-CPROG)
      USING E_ROW E_COLUMN
      IF FOUND.
  ENDMETHOD.                    "on_double_click

  METHOD ON_BUTTON_CLICK_1.
    PERFORM F_BUTTON_CLICK_1 IN PROGRAM (SY-CPROG)
      USING ES_ROW_NO ES_COL_ID
      IF FOUND.
  ENDMETHOD.                    "on_button_click

  METHOD HANDLE_TOOLBAR_1.
    PERFORM F_HANDLE_TOOLBAR_1 IN PROGRAM (SY-CPROG)
      USING E_OBJECT E_INTERACTIVE
      IF FOUND.
  ENDMETHOD.                    "handle_toolbar

  METHOD HANDLE_USER_COMMAND_1.
    PERFORM F_USER_COMMAND_1 IN PROGRAM (SY-CPROG)
      USING E_UCOMM
      IF FOUND.
  ENDMETHOD.                    "handle_user_command_1

  METHOD ON_DATA_CHANGED_1.
    PERFORM F_ON_DATA_CHANGED_1 IN PROGRAM (SY-CPROG)
      USING ER_DATA_CHANGED
            E_ONF4
            E_ONF4_BEFORE
            E_ONF4_AFTER
            E_UCOMM
      IF FOUND.
  ENDMETHOD.

  METHOD ON_PRINT_TOP_OF_PAGE_2.
    PERFORM F_PRINT_TOP_OF_PAGE_2 IN PROGRAM (SY-CPROG)
      IF FOUND.
  ENDMETHOD.                    "on_print_top_of_page

  METHOD ON_HOTSPOT_CLICK_2.
    PERFORM F_HOTSPOT_CLICK_2 IN PROGRAM (SY-CPROG)
      USING E_ROW_ID E_COLUMN_ID
      IF FOUND.
  ENDMETHOD.                    "on_hotspot_click

  METHOD ON_DOUBLE_CLICK_2.
    PERFORM F_HOTSPOT_CLICK_2 IN PROGRAM (SY-CPROG)
      USING E_ROW E_COLUMN
      IF FOUND.
  ENDMETHOD.                    "on_double_click

  METHOD ON_BUTTON_CLICK_2.
    PERFORM F_BUTTON_CLICK_2 IN PROGRAM (SY-CPROG)
      USING ES_ROW_NO ES_COL_ID
      IF FOUND.
  ENDMETHOD.                    "on_button_click

  METHOD HANDLE_TOOLBAR_2.
    PERFORM F_HANDLE_TOOLBAR_2 IN PROGRAM (SY-CPROG)
      USING E_OBJECT E_INTERACTIVE
      IF FOUND.
  ENDMETHOD.                    "handle_toolbar

  METHOD HANDLE_USER_COMMAND_2.
    PERFORM F_USER_COMMAND_2 IN PROGRAM (SY-CPROG)
      USING E_UCOMM
      IF FOUND.
  ENDMETHOD.                    "handle_user_command_2

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*----------------------------------------------------------------------*
*-- Subroutines
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       Form  f_prepare_fieldcat_o
*----------------------------------------------------------------------*
*       Prepare fieldcat method ALV used for online
*----------------------------------------------------------------------*
*  -->  PFD_STRUCTURE  Structure name of the list table
*  <--  PIT_FIELDCAT   Field catalog
*----------------------------------------------------------------------*
FORM F_PREPARE_FIELDCAT_O  USING  UF_STRUCTURE TYPE DD02L-TABNAME
                         CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT ##CALLED.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = UF_STRUCTURE
    CHANGING
      CT_FIELDCAT            = CT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " f_prepare_fieldcat_o

*----------------------------------------------------------------------*
*       Module  STATUS_ALV_1  OUTPUT
*----------------------------------------------------------------------*
*       Set GUI Status & GUI Title
*----------------------------------------------------------------------*
MODULE STATUS_ALV_1 OUTPUT.

* Check ALV 1 in Edit Mode?
  IF GS_LAYOUT_1-EDIT EQ SPACE.
    READ TABLE GT_FIELDCAT_1 TRANSPORTING NO FIELDS
                             WITH KEY EDIT = 'X'.
    IF SY-SUBRC NE 0.
      APPEND GC_SAVE_1 TO GT_EXCL.
    ENDIF.
  ENDIF.

  SET PF-STATUS '9000' EXCLUDING GT_EXCL ##STAT_UNDEF.
  SET TITLEBAR  '9000' ##TITL_UNDEF.

ENDMODULE.                 " STATUS_0100_1  OUTPUT

*----------------------------------------------------------------------*
*       Module  DISPLAY_ALV_1  OUTPUT
*----------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_1 OUTPUT.

* Display ALV Grid
  PERFORM F_ALV_DISPLAY_1.

ENDMODULE.                 " DISPLAY_ALV_1  OUTPUT

*----------------------------------------------------------------------*
*       Module  EXIT_COMMANDS_1 INPUT
*----------------------------------------------------------------------*
*       Cancel transaction and leave the program
*----------------------------------------------------------------------*
MODULE EXIT_COMMANDS_1 INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " EXIT_COMMANDS_1  INPUT

*----------------------------------------------------------------------*
*       Module  USER_COMMAND_ALV_1  INPUT
*----------------------------------------------------------------------*
*       User-Commands from ALV
*----------------------------------------------------------------------*
MODULE USER_COMMAND_ALV_1 INPUT.

* to react on oi_custom_events:
  CALL METHOD CL_GUI_CFW=>DISPATCH.

  GF_SAVE_OK = GF_OK_CODE_1.
  CLEAR GF_OK_CODE_1.

  CASE GF_SAVE_OK.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'END'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*     Call specific user command handler if it exists
      PERFORM F_USER_COMMAND_1 IN PROGRAM (SY-CPROG) USING GF_SAVE_OK
        IF FOUND.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*----------------------------------------------------------------------*
*       Form  F_ALV_DISPLAY_1
*----------------------------------------------------------------------*
*       Display ALV Grid
*----------------------------------------------------------------------*
FORM F_ALV_DISPLAY_1 .

* --------------------------------------------------
* Create First ALV Container
* --------------------------------------------------
  IF GREF_GRID_1 IS INITIAL.

*   Get First ALV Container
    PERFORM F_GET_CONTAINER_1  USING  'FIRST'
                                      GF_ALV_HEIGHT_1
                             CHANGING GREF_CONTAINER_GRID_1.

*   Create First ALV Object
    CREATE OBJECT GREF_GRID_1
      EXPORTING
        I_PARENT = GREF_CONTAINER_GRID_1.

*   Set Event Handler for First ALV
    CREATE OBJECT GREF_EVENT_RECEIVER_1.
    SET HANDLER:
      GREF_EVENT_RECEIVER_1->ON_PRINT_TOP_OF_PAGE_1  FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_HOTSPOT_CLICK_1      FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DOUBLE_CLICK_1       FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_BUTTON_CLICK_1       FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->HANDLE_TOOLBAR_1        FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->HANDLE_USER_COMMAND_1   FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DATA_CHANGED_1       FOR GREF_GRID_1.

*   Show First ALV
    CALL METHOD GREF_GRID_1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_BUFFER_ACTIVE               = 'X'
        IS_VARIANT                    = GS_VARIANT_1
        IS_LAYOUT                     = GS_LAYOUT_1
        IS_PRINT                      = GS_PRINT_1
        I_SAVE                        = GC_SAVE_ALL
        IT_TOOLBAR_EXCLUDING          = GT_TOOL_EXC_1
      CHANGING
        IT_OUTTAB                     = <G_LIST_1>
        IT_FIELDCATALOG               = GT_FIELDCAT_1
        IT_SORT                       = GT_SORT_1
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
    ENDIF.
  ELSE.

    IF GF_NO_AUTO_REFRESH_1 = SPACE.
      IF GF_SOFT_REFRESH_1 IS INITIAL.
        IF GF_REFRESH_ALL_1 EQ 'X'.
          CALL METHOD GREF_GRID_1->SET_FRONTEND_FIELDCATALOG
            EXPORTING
              IT_FIELDCATALOG = GT_FIELDCAT_1.
          CALL METHOD GREF_GRID_1->SET_FRONTEND_LAYOUT
            EXPORTING
              IS_LAYOUT = GS_LAYOUT_1.
          CALL METHOD GREF_GRID_1->SET_FRONTEND_PRINT
            EXPORTING
              IS_PRINT = GS_PRINT_1.
        ENDIF.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = ' '.
      ELSE.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

* --------------------------------------------------
* Create HTML Header Container if flag is marked
* --------------------------------------------------
  IF GF_ALV_HEADER_1 EQ 'X' AND
     CL_GUI_ALV_GRID=>OFFLINE( ) IS INITIAL.

    IF GREF_CONTAINER_HTML_1 IS INITIAL.

*     Get HTML Header Container
      PERFORM F_GET_CONTAINER_1  USING  'HEADER'
                                        GF_HEADER_HIGHT_1
                               CHANGING GREF_CONTAINER_HTML_1.

*     Bind TOP-OF-PAGE Event
      SET HANDLER:
        GREF_EVENT_RECEIVER_1->ON_TOP_OF_PAGE_1        FOR GREF_GRID_1.
*     Create TOP-Document
      CREATE OBJECT GREF_DYNDOC_ID_1
        EXPORTING
          STYLE = 'ALV_GRID'.
    ENDIF.

*   Initializing document
    CALL METHOD GREF_DYNDOC_ID_1->INITIALIZE_DOCUMENT.
*   Processing events
    CALL METHOD GREF_GRID_1->LIST_PROCESSING_EVENTS
      EXPORTING
        I_EVENT_NAME = 'TOP_OF_PAGE'
        I_DYNDOC_ID  = GREF_DYNDOC_ID_1.
*   Merge all setting
    CALL METHOD GREF_DYNDOC_ID_1->MERGE_DOCUMENT.
*   Display TOP document
    CALL METHOD GREF_DYNDOC_ID_1->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = GREF_CONTAINER_HTML_1
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.
    IF SY-SUBRC NE 0 ##NEEDED.
      "Do nothing
    ENDIF.

  ENDIF.

* --------------------------------------------------
* Create Second ALV Container if flag is marked
* --------------------------------------------------
  IF GF_SECOND_ALV_1 EQ 'X'.

    IF GREF_CONTAINER_GRID_2 IS INITIAL.
*     Get Second ALV Container
      PERFORM F_GET_CONTAINER_1  USING  'SECOND'
                                        GF_ALV_HEIGHT_2
                               CHANGING GREF_CONTAINER_GRID_2.

*     Create First ALV Object
      CREATE OBJECT GREF_GRID_2
        EXPORTING
          I_PARENT = GREF_CONTAINER_GRID_2.

*     Set Event Handler for First ALV
      CREATE OBJECT GREF_EVENT_RECEIVER_1.
      SET HANDLER:
        GREF_EVENT_RECEIVER_1->ON_PRINT_TOP_OF_PAGE_2  FOR GREF_GRID_2,
        GREF_EVENT_RECEIVER_1->ON_HOTSPOT_CLICK_2      FOR GREF_GRID_2,
        GREF_EVENT_RECEIVER_1->ON_DOUBLE_CLICK_2       FOR GREF_GRID_2,
        GREF_EVENT_RECEIVER_1->ON_BUTTON_CLICK_2       FOR GREF_GRID_2,
        GREF_EVENT_RECEIVER_1->HANDLE_TOOLBAR_2        FOR GREF_GRID_2,
        GREF_EVENT_RECEIVER_1->HANDLE_USER_COMMAND_2   FOR GREF_GRID_2.

*     Show First ALV
      CALL METHOD GREF_GRID_2->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          I_BUFFER_ACTIVE               = 'X'
          IS_VARIANT                    = GS_VARIANT_2
          IS_LAYOUT                     = GS_LAYOUT_2
          IS_PRINT                      = GS_PRINT_2
          I_SAVE                        = GC_SAVE_ALL
          IT_TOOLBAR_EXCLUDING          = GT_TOOL_EXC_2
        CHANGING
          IT_OUTTAB                     = <G_LIST_2>
          IT_FIELDCATALOG               = GT_FIELDCAT_2
          IT_SORT                       = GT_SORT_2
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        CALL METHOD GREF_GRID_2->REGISTER_EDIT_EVENT
          EXPORTING
            I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
        CALL METHOD GREF_GRID_2->REGISTER_EDIT_EVENT
          EXPORTING
            I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
      ENDIF.
    ELSE.

      IF GF_NO_AUTO_REFRESH_2 = SPACE.
        IF GF_SOFT_REFRESH_2 IS INITIAL.
          IF GF_REFRESH_ALL_2 EQ 'X'.
            CALL METHOD GREF_GRID_2->SET_FRONTEND_FIELDCATALOG
              EXPORTING
                IT_FIELDCATALOG = GT_FIELDCAT_2.
            CALL METHOD GREF_GRID_2->SET_FRONTEND_LAYOUT
              EXPORTING
                IS_LAYOUT = GS_LAYOUT_2.
            CALL METHOD GREF_GRID_2->SET_FRONTEND_PRINT
              EXPORTING
                IS_PRINT = GS_PRINT_2.
          ENDIF.
          CALL METHOD GREF_GRID_2->REFRESH_TABLE_DISPLAY
            EXPORTING
              I_SOFT_REFRESH = ' '.
        ELSE.
          CALL METHOD GREF_GRID_2->REFRESH_TABLE_DISPLAY
            EXPORTING
              I_SOFT_REFRESH = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_ALV_DISPLAY_1

*----------------------------------------------------------------------*
*       Form  F_GET_CONTAINER_1
*----------------------------------------------------------------------*
*       Get Container
*----------------------------------------------------------------------*
FORM F_GET_CONTAINER_1  USING  UF_TYPE       TYPE  CHAR10
                               UF_HIGHT      TYPE  I
                      CHANGING CREF_CONTAINER  TYPE  REF TO CL_GUI_CONTAINER.

  STATICS:
    LF_ROW  TYPE  I.


* Only when container is initial
  IF CREF_CONTAINER IS NOT INITIAL.
    RETURN.
  ENDIF.

* Assign Dock container for background processing
  IF CL_GUI_ALV_GRID=>OFFLINE( ) IS NOT INITIAL.
    CREF_CONTAINER = GREF_DOCK_CONTAINER_1.
    RETURN.
  ENDIF.

  IF GREF_CUSTOM_CONTAINER_1 IS INITIAL.
    CREATE OBJECT GREF_CUSTOM_CONTAINER_1
      EXPORTING
        CONTAINER_NAME = GF_CONTAINER_1.
  ENDIF.

* Create Splitter if needed
  IF GREF_SPLITTER_1 IS INITIAL AND
     ( GF_ALV_HEADER_1 EQ 'X' OR
       GF_SECOND_ALV_1 EQ 'X' ).

    IF GF_ALV_HEADER_1 EQ 'X' AND
       GF_SECOND_ALV_1 EQ 'X'.
      LF_ROW = 3.
    ELSE.
      LF_ROW = 2.
    ENDIF.

*   Create Splitter for custom_container
    CREATE OBJECT GREF_SPLITTER_1
      EXPORTING
        PARENT  = GREF_CUSTOM_CONTAINER_1
        ROWS    = LF_ROW
        COLUMNS = 1.

  ENDIF.

* Case container type
  CASE UF_TYPE.

    WHEN 'HEADER'.
*     Set height for alv header
      CALL METHOD GREF_SPLITTER_1->SET_ROW_HEIGHT
        EXPORTING
          ID     = 1
          HEIGHT = UF_HIGHT.
*     Get Container
      CALL METHOD GREF_SPLITTER_1->GET_CONTAINER
        EXPORTING
          ROW       = 1
          COLUMN    = 1
        RECEIVING
          CONTAINER = CREF_CONTAINER.

    WHEN 'FIRST'.
*     Set Container Height
      IF GF_ALV_HEADER_1 EQ 'X'.
*       Set height for alv header
        CALL METHOD GREF_SPLITTER_1->SET_ROW_HEIGHT
          EXPORTING
            ID     = 2
            HEIGHT = UF_HIGHT.
*       Get Container
        CALL METHOD GREF_SPLITTER_1->GET_CONTAINER
          EXPORTING
            ROW       = 2
            COLUMN    = 1
          RECEIVING
            CONTAINER = CREF_CONTAINER.
      ELSEIF GF_SECOND_ALV_1 EQ 'X'.
*       Set height for alv header
        CALL METHOD GREF_SPLITTER_1->SET_ROW_HEIGHT
          EXPORTING
            ID     = 1
            HEIGHT = UF_HIGHT.
*       Get Container
        CALL METHOD GREF_SPLITTER_1->GET_CONTAINER
          EXPORTING
            ROW       = 1
            COLUMN    = 1
          RECEIVING
            CONTAINER = CREF_CONTAINER.
      ELSE.
        CREF_CONTAINER = GREF_CUSTOM_CONTAINER_1.
      ENDIF.

    WHEN 'SECOND'.
*     Set height for alv header
      CALL METHOD GREF_SPLITTER_1->SET_ROW_HEIGHT
        EXPORTING
          ID     = LF_ROW
          HEIGHT = UF_HIGHT.
*     Get Container
      CALL METHOD GREF_SPLITTER_1->GET_CONTAINER
        EXPORTING
          ROW       = LF_ROW
          COLUMN    = 1
        RECEIVING
          CONTAINER = CREF_CONTAINER.

  ENDCASE.

ENDFORM.                    " F_GET_CONTAINER_1

*----------------------------------------------------------------------*
*  Form f_read_alv_sum_1
*----------------------------------------------------------------------*
*  Read Summarized row from Grid 1
*----------------------------------------------------------------------*
FORM F_READ_ALV_SUM_1  USING  US_ROW TYPE LVC_S_ROW
                     CHANGING CS_DATA TYPE ANY  ##CALLED.

  DATA:
    LREF_COL00 TYPE REF TO DATA,
    LREF_COL01 TYPE REF TO DATA,
    LREF_COL02 TYPE REF TO DATA,
    LREF_COL03 TYPE REF TO DATA,
    LREF_COL04 TYPE REF TO DATA,
    LREF_COL05 TYPE REF TO DATA,
    LREF_COL06 TYPE REF TO DATA,
    LREF_COL07 TYPE REF TO DATA,
    LREF_COL08 TYPE REF TO DATA,
    LREF_COL09 TYPE REF TO DATA.

  DATA:
    LT_GROUPLEVEL   TYPE LVC_T_GRPL.

  DATA:
    LS_GROUPLEVEL  TYPE LVC_S_GRPL.

  DATA:
    LF_INDEX  TYPE  SYTABIX.

  DATA: BEGIN OF LS_ROW_DATA,
          DTYPE(1)    TYPE C,
          FILLER1(1)  TYPE C,
          LEVEL(2)    TYPE N,
          FILLER2(19) TYPE C,
          INDEX(10)   TYPE N,
        END OF LS_ROW_DATA.

  FIELD-SYMBOLS:
    <L_COLLECT> TYPE ANY TABLE.


  LS_ROW_DATA = US_ROW.
  CALL METHOD GREF_GRID_1->GET_SUBTOTALS
    IMPORTING
      EP_COLLECT00   = LREF_COL00
      EP_COLLECT01   = LREF_COL01
      EP_COLLECT02   = LREF_COL02
      EP_COLLECT03   = LREF_COL03
      EP_COLLECT04   = LREF_COL04
      EP_COLLECT05   = LREF_COL05
      EP_COLLECT06   = LREF_COL06
      EP_COLLECT07   = LREF_COL07
      EP_COLLECT08   = LREF_COL08
      EP_COLLECT09   = LREF_COL09
      ET_GROUPLEVELS = LT_GROUPLEVEL.
  IF LS_ROW_DATA-DTYPE = 'T'.
    ASSIGN LREF_COL00->* TO <L_COLLECT>.
    LF_INDEX = 1.
  ELSE.
    READ TABLE LT_GROUPLEVEL INDEX LS_ROW_DATA-INDEX
                             INTO LS_GROUPLEVEL.
    LF_INDEX = LS_GROUPLEVEL-CINDX_FROM.
    CASE LS_GROUPLEVEL-COLLECT.
      WHEN '01'.
        ASSIGN LREF_COL01->* TO <L_COLLECT>.
      WHEN '02'.
        ASSIGN LREF_COL02->* TO <L_COLLECT>.
      WHEN '03'.
        ASSIGN LREF_COL03->* TO <L_COLLECT>.
      WHEN '04'.
        ASSIGN LREF_COL04->* TO <L_COLLECT>.
      WHEN '05'.
        ASSIGN LREF_COL05->* TO <L_COLLECT>.
      WHEN '06'.
        ASSIGN LREF_COL06->* TO <L_COLLECT>.
      WHEN '07'.
        ASSIGN LREF_COL07->* TO <L_COLLECT>.
      WHEN '08'.
        ASSIGN LREF_COL08->* TO <L_COLLECT>.
      WHEN '09'.
        ASSIGN LREF_COL09->* TO <L_COLLECT>.
    ENDCASE.
  ENDIF.
  CLEAR CS_DATA.
  LOOP AT <L_COLLECT> ASSIGNING FIELD-SYMBOL(<L_DATA>).
    IF SY-TABIX EQ LF_INDEX.
      CS_DATA = <L_DATA>.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
