*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0520_ALV
*  Creation Date      : 11.11.2024
*  Author             : Apichat Ch.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program logic
*  Copied from        : ZSDSFIR0520_ALV
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
  lcl_event_handler   DEFINITION DEFERRED,   "for event handling
  lcl_application_f4  DEFINITION DEFERRED.

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
  gc_save_all TYPE char1         VALUE 'A',                 "#EC NEEDED
  gc_save     TYPE sy-ucomm        VALUE 'SAVE'.            "#EC NEEDED

*----------------------------------------------------------------------*
*-- Internal Tables
*----------------------------------------------------------------------*
DATA:
  gt_fieldcat TYPE lvc_t_fcat,                              "#EC NEEDED
  gt_sort     TYPE lvc_t_sort,                              "#EC NEEDED
  gt_tool_exc TYPE ui_functions,                            "#EC NEEDED
  gt_excl     TYPE STANDARD TABLE OF sy-ucomm.              "#EC NEEDED
*----------------------------------------------------------------------*
*-- Work Areas
*----------------------------------------------------------------------*
DATA:
  gs_variant  TYPE disvariant,                              "#EC NEEDED
  gs_layout   TYPE lvc_s_layo,                              "#EC NEEDED
  gs_fieldcat TYPE lvc_s_fcat,                              "#EC NEEDED
  gs_sort     TYPE lvc_s_sort,                              "#EC NEEDED
  gs_excl     TYPE sy-ucomm,                                "#EC NEEDED
  gs_print    TYPE lvc_s_prnt.                              "#EC NEEDED

*----------------------------------------------------------------------*
*-- Ranges
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Variable declarations
*----------------------------------------------------------------------*
DATA:
  gf_container          TYPE scrfname,                      "#EC NEEDED
  gf_ok_code            TYPE sy-ucomm,                      "#EC NEEDED
  gf_save_ok            TYPE sy-ucomm,                      "#EC NEEDED
  gref_custom_container TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  gref_grid             TYPE REF TO cl_gui_alv_grid,        "#EC NEEDED
  gref_dock_container   TYPE REF TO cl_gui_docking_container, "#EC NEEDED
  gref_event_receiver   TYPE REF TO lcl_event_handler,      "#EC NEEDED
  gref_splitter         TYPE REF TO cl_gui_splitter_container, "#EC NEEDED
  gref_container_grid   TYPE REF TO cl_gui_container,       "#EC NEEDED
  gref_container_html   TYPE REF TO cl_gui_container,       "#EC NEEDED
  gref_dyndoc_id        TYPE REF TO cl_dd_document,         "#EC NEEDED
  gref_onf4             TYPE REF TO lcl_application_f4,     "#EC NEEDED
  gf_alv_header         TYPE flag,                          "#EC NEEDED
  gf_header_hight       TYPE i VALUE 30,                    "#EC NEEDED
  gf_second_alv         TYPE flag,                          "#EC NEEDED
  gf_alv_height         TYPE i VALUE 25,                    "#EC NEEDED
  gf_soft_refresh       TYPE flag,                          "#EC NEEDED
  gf_no_auto_refresh    TYPE flag.                          "#EC NEEDED

*----------------------------------------------------------------------*
*-- Field-Symbols
*----------------------------------------------------------------------*


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
CLASS lcl_event_handler DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_print_top_of_page FOR EVENT print_top_of_page OF cl_gui_alv_grid,

      on_top_of_page       FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id,

      on_hotspot_click     FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      on_double_click      FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      on_button_click      FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_row_no es_col_id,

      handle_toolbar       FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed
                  e_onf4
                  e_onf4_before
                  e_onf4_after
                  e_ucomm,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified
                  et_good_cells.


ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS lcl_application_f4 DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid ##CALLED
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells      ##NEEDED
                  e_display         ##NEEDED.

ENDCLASS.

*-----------------------------------------------------------------------
* CLASS IMPLEMENTATION
*-----------------------------------------------------------------------
* Event Handler
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_top_of_page.
    PERFORM f_top_of_page IN PROGRAM (sy-cprog)
      USING e_dyndoc_id
      IF FOUND.
  ENDMETHOD.                    "on_print_top_of_page

  METHOD on_print_top_of_page.
    PERFORM f_print_top_of_page IN PROGRAM (sy-cprog)
      IF FOUND.
  ENDMETHOD.                    "on_print_top_of_page

  METHOD on_hotspot_click.
    PERFORM f_hotspot_click IN PROGRAM (sy-cprog)
      USING e_row_id e_column_id
      IF FOUND.
  ENDMETHOD.                    "on_hotspot_click

  METHOD on_double_click.
    PERFORM f_hotspot_click IN PROGRAM (sy-cprog)
      USING e_row e_column
      IF FOUND.
  ENDMETHOD.                    "on_double_click

  METHOD on_button_click.
    PERFORM f_button_click IN PROGRAM (sy-cprog)
      USING es_row_no es_col_id
      IF FOUND.
  ENDMETHOD.                    "on_button_click

  METHOD handle_toolbar.
    PERFORM f_handle_toolbar IN PROGRAM (sy-cprog)
      USING e_object e_interactive
      IF FOUND.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
    PERFORM f_user_command IN PROGRAM (sy-cprog)
      USING e_ucomm
      IF FOUND.
  ENDMETHOD.                    "handle_user_command

  METHOD on_data_changed.
    PERFORM f_on_data_changed IN PROGRAM (sy-cprog)
      USING er_data_changed
            e_onf4
            e_onf4_before
            e_onf4_after
            e_ucomm
      IF FOUND.
  ENDMETHOD.

  METHOD on_data_changed_finished.
    PERFORM f_on_data_changed_finished IN PROGRAM (sy-cprog)
      USING e_modified
            et_good_cells
      IF FOUND.
  ENDMETHOD.


ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

CLASS lcl_application_f4 IMPLEMENTATION.

  METHOD on_f4  ##NEEDED.

*    DATA:
*      ls_modi  TYPE lvc_s_modi,
*      ls_banka TYPE bnka.
*    FIELD-SYMBOLS <itab> TYPE lvc_t_modi.
*
*    CALL FUNCTION 'SEARCH_BANK_ADDRESS'
*      EXPORTING
*        i_banks = 'TH'
*      IMPORTING
*        e_bnka  = ls_banka.
*
*    IF ls_banka IS NOT INITIAL.
*
*      ASSIGN er_event_data->m_data->* TO <itab>.
*      ls_modi-row_id    = es_row_no-row_id.
*      ls_modi-fieldname = e_fieldname.
*      ls_modi-value = ls_banka-bankl.
*      APPEND ls_modi TO <itab>.
*
*      er_event_data->m_event_handled = 'X'.
*
*    ENDIF.

  ENDMETHOD.

ENDCLASS.

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
FORM f_prepare_fieldcat_o  USING  uf_structure TYPE dd02l-tabname
                         CHANGING ct_fieldcat  TYPE lvc_t_fcat ##CALLED.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = uf_structure
    CHANGING
      ct_fieldcat            = ct_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_prepare_fieldcat_o

*----------------------------------------------------------------------*
*       Module  STATUS_ALV  OUTPUT
*----------------------------------------------------------------------*
*       Set GUI Status & GUI Title
*----------------------------------------------------------------------*
MODULE status_alv OUTPUT.

* Check ALV 1 in Edit Mode?
  IF gs_layout-edit EQ space.
    READ TABLE gt_fieldcat TRANSPORTING NO FIELDS
                             WITH KEY edit = 'X'.
    IF sy-subrc NE 0.
      APPEND gc_save TO gt_excl.
    ENDIF.
  ENDIF.

  SET PF-STATUS '9000' EXCLUDING gt_excl ##STAT_UNDEF.
  SET TITLEBAR  '9000' ##TITL_UNDEF.

ENDMODULE.                 " STATUS_0100  OUTPUT

*----------------------------------------------------------------------*
*       Module  DISPLAY_ALV  OUTPUT
*----------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.

* Display ALV Grid
  PERFORM f_alv_display.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT

*----------------------------------------------------------------------*
*       Module  EXIT_COMMANDS INPUT
*----------------------------------------------------------------------*
*       Cancel transaction and leave the program
*----------------------------------------------------------------------*
MODULE exit_commands INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " EXIT_COMMANDS  INPUT

*----------------------------------------------------------------------*
*       Module  USER_COMMAND_ALV  INPUT
*----------------------------------------------------------------------*
*       User-Commands from ALV
*----------------------------------------------------------------------*
MODULE user_command_alv INPUT.

* to react on oi_custom_events:
  CALL METHOD cl_gui_cfw=>dispatch.

  gf_save_ok = gf_ok_code.
  CLEAR gf_ok_code.

  CASE gf_save_ok.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'END'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*     Call specific user command handler if it exists
      PERFORM f_user_command IN PROGRAM (sy-cprog) USING gf_save_ok
        IF FOUND.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*----------------------------------------------------------------------*
*       Form  F_ALV_DISPLAY
*----------------------------------------------------------------------*
*       Display ALV Grid
*----------------------------------------------------------------------*
FORM f_alv_display .

* --------------------------------------------------
* Create First ALV Container
* --------------------------------------------------
  IF gref_grid IS INITIAL.

*   Get First ALV Container
    PERFORM f_get_container  USING  'FIRST'
                                      gf_alv_height
                             CHANGING gref_container_grid.

*   Create First ALV Object
    CREATE OBJECT gref_grid
      EXPORTING
        i_parent = gref_container_grid.

*   Set Event Handler for First ALV
    CREATE OBJECT gref_event_receiver.
    SET HANDLER:
      gref_event_receiver->on_print_top_of_page     FOR gref_grid,
      gref_event_receiver->on_hotspot_click         FOR gref_grid,
      gref_event_receiver->on_double_click          FOR gref_grid,
      gref_event_receiver->on_button_click          FOR gref_grid,
      gref_event_receiver->handle_toolbar           FOR gref_grid,
      gref_event_receiver->handle_user_command      FOR gref_grid,
      gref_event_receiver->on_data_changed          FOR gref_grid,
      gref_event_receiver->on_data_changed_finished FOR gref_grid.

*   Build custome F4
    PERFORM f_prepare_f4   IN PROGRAM (sy-cprog) IF FOUND.

*   Build Dropdown value
    PERFORM f_prepare_drdn IN PROGRAM (sy-cprog) IF FOUND.

*   Show First ALV
    CALL METHOD gref_grid->set_table_for_first_display
      EXPORTING
        i_buffer_active               = 'X'
        is_variant                    = gs_variant
        is_layout                     = gs_layout
        is_print                      = gs_print
        i_save                        = gc_save_all
        it_toolbar_excluding          = gt_tool_exc
      CHANGING
        it_outtab                     = <g_list>
        it_fieldcatalog               = gt_fieldcat
        it_sort                       = gt_sort
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CALL METHOD gref_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.
      CALL METHOD gref_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.
    ENDIF.
  ELSE.

    IF gf_no_auto_refresh = space.
      IF gf_soft_refresh IS INITIAL.
        CALL METHOD gref_grid->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = gt_fieldcat.
        CALL METHOD gref_grid->set_frontend_layout
          EXPORTING
            is_layout = gs_layout.
        CALL METHOD gref_grid->set_frontend_print
          EXPORTING
            is_print = gs_print.
        CALL METHOD gref_grid->refresh_table_display
          EXPORTING
            i_soft_refresh = ' '.
      ELSE.
        CALL METHOD gref_grid->refresh_table_display
          EXPORTING
            i_soft_refresh = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

* --------------------------------------------------
* Create HTML Header Container if flag is marked
* --------------------------------------------------
  IF gf_alv_header EQ 'X' AND
     cl_gui_alv_grid=>offline( ) IS INITIAL.

    IF gref_container_html IS INITIAL.

*     Get HTML Header Container
      PERFORM f_get_container  USING  'HEADER'
                                        gf_header_hight
                               CHANGING gref_container_html.

*     Bind TOP-OF-PAGE Event
      SET HANDLER:
        gref_event_receiver->on_top_of_page        FOR gref_grid.
*     Create TOP-Document
      CREATE OBJECT gref_dyndoc_id
        EXPORTING
          style = 'ALV_GRID'.
    ENDIF.

    PERFORM f_init_doc_alv_header.

  ENDIF.


ENDFORM.                    " F_ALV_DISPLAY

*----------------------------------------------------------------------*
*       Form  F_GET_CONTAINER
*----------------------------------------------------------------------*
*       Get Container
*----------------------------------------------------------------------*
FORM f_get_container  USING  uf_type       TYPE  char10
                               uf_hight      TYPE  i
                      CHANGING cref_container  TYPE  REF TO cl_gui_container.

  STATICS:
    lf_row  TYPE  i.


* Only when container is initial
  IF cref_container IS NOT INITIAL.
    RETURN.
  ENDIF.

* Assign Dock container for background processing
  IF cl_gui_alv_grid=>offline( ) IS NOT INITIAL.
    cref_container = gref_dock_container.
    RETURN.
  ENDIF.

  IF gref_custom_container IS INITIAL.
    CREATE OBJECT gref_custom_container
      EXPORTING
        container_name = gf_container.
  ENDIF.

* Create Splitter if needed
  IF gref_splitter IS INITIAL AND
     ( gf_alv_header EQ 'X' OR
       gf_second_alv EQ 'X' ).

    IF gf_alv_header EQ 'X' AND
       gf_second_alv EQ 'X'.
      lf_row = 3.
    ELSE.
      lf_row = 2.
    ENDIF.

*   Create Splitter for custom_container
    CREATE OBJECT gref_splitter
      EXPORTING
        parent  = gref_custom_container
        rows    = lf_row
        columns = 1.

  ENDIF.

* Case container type
  CASE uf_type.

    WHEN 'HEADER'.
*     Set height for alv header
      CALL METHOD gref_splitter->set_row_height
        EXPORTING
          id     = 1
          height = uf_hight.
*     Get Container
      CALL METHOD gref_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = cref_container.

    WHEN 'FIRST'.
*     Set Container Height
      IF gf_alv_header EQ 'X'.
*       Set height for alv header
        CALL METHOD gref_splitter->set_row_height
          EXPORTING
            id     = 2
            height = uf_hight.
*       Get Container
        CALL METHOD gref_splitter->get_container
          EXPORTING
            row       = 2
            column    = 1
          RECEIVING
            container = cref_container.
      ELSEIF gf_second_alv EQ 'X'.
*       Set height for alv header
        CALL METHOD gref_splitter->set_row_height
          EXPORTING
            id     = 1
            height = uf_hight.
*       Get Container
        CALL METHOD gref_splitter->get_container
          EXPORTING
            row       = 1
            column    = 1
          RECEIVING
            container = cref_container.
      ELSE.
        cref_container = gref_custom_container.
      ENDIF.

    WHEN 'SECOND'.
*     Set height for alv header
      CALL METHOD gref_splitter->set_row_height
        EXPORTING
          id     = lf_row
          height = uf_hight.
*     Get Container
      CALL METHOD gref_splitter->get_container
        EXPORTING
          row       = lf_row
          column    = 1
        RECEIVING
          container = cref_container.

  ENDCASE.

ENDFORM.                    " F_GET_CONTAINER

*----------------------------------------------------------------------*
*  Form f_read_alv_sum
*----------------------------------------------------------------------*
*  Read Summarized row from Grid 1
*----------------------------------------------------------------------*
FORM f_read_alv_sum  USING  us_row TYPE lvc_s_row
                     CHANGING cs_data TYPE any  ##CALLED.

  DATA:
    lref_col00 TYPE REF TO data,
    lref_col01 TYPE REF TO data,
    lref_col02 TYPE REF TO data,
    lref_col03 TYPE REF TO data,
    lref_col04 TYPE REF TO data,
    lref_col05 TYPE REF TO data,
    lref_col06 TYPE REF TO data,
    lref_col07 TYPE REF TO data,
    lref_col08 TYPE REF TO data,
    lref_col09 TYPE REF TO data.

  DATA:
    lt_grouplevel   TYPE lvc_t_grpl.

  DATA:
    ls_grouplevel  TYPE lvc_s_grpl.

  DATA:
    lf_index  TYPE  sytabix.

  DATA: BEGIN OF ls_row_data,
          dtype(1)    TYPE c,
          filler1(1)  TYPE c,
          level(2)    TYPE n,
          filler2(19) TYPE c,
          index(10)   TYPE n,
        END OF ls_row_data.

  FIELD-SYMBOLS:
    <l_collect> TYPE ANY TABLE.


  ls_row_data = us_row.
  CALL METHOD gref_grid->get_subtotals
    IMPORTING
      ep_collect00   = lref_col00
      ep_collect01   = lref_col01
      ep_collect02   = lref_col02
      ep_collect03   = lref_col03
      ep_collect04   = lref_col04
      ep_collect05   = lref_col05
      ep_collect06   = lref_col06
      ep_collect07   = lref_col07
      ep_collect08   = lref_col08
      ep_collect09   = lref_col09
      et_grouplevels = lt_grouplevel.
  IF ls_row_data-dtype = 'T'.
    ASSIGN lref_col00->* TO <l_collect>.
    lf_index = 1.
  ELSE.
    READ TABLE lt_grouplevel INDEX ls_row_data-index
                             INTO ls_grouplevel.
    lf_index = ls_grouplevel-cindx_from.
    CASE ls_grouplevel-collect.
      WHEN '01'.
        ASSIGN lref_col01->* TO <l_collect>.
      WHEN '02'.
        ASSIGN lref_col02->* TO <l_collect>.
      WHEN '03'.
        ASSIGN lref_col03->* TO <l_collect>.
      WHEN '04'.
        ASSIGN lref_col04->* TO <l_collect>.
      WHEN '05'.
        ASSIGN lref_col05->* TO <l_collect>.
      WHEN '06'.
        ASSIGN lref_col06->* TO <l_collect>.
      WHEN '07'.
        ASSIGN lref_col07->* TO <l_collect>.
      WHEN '08'.
        ASSIGN lref_col08->* TO <l_collect>.
      WHEN '09'.
        ASSIGN lref_col09->* TO <l_collect>.
    ENDCASE.
  ENDIF.
  CLEAR cs_data.
  LOOP AT <l_collect> ASSIGNING FIELD-SYMBOL(<l_data>).
    IF sy-tabix EQ lf_index.
      cs_data = <l_data>.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_doc_alv_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_doc_alv_header .
*   Initializing document
  CALL METHOD gref_dyndoc_id->initialize_document.
*   Processing events
  CALL METHOD gref_grid->list_processing_events
    EXPORTING
      i_event_name = 'TOP_OF_PAGE'
      i_dyndoc_id  = gref_dyndoc_id.
*   Merge all setting
  CALL METHOD gref_dyndoc_id->merge_document.
*   Display TOP document
  CALL METHOD gref_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = gref_container_html
    EXCEPTIONS
      html_display_error = 1.
  IF sy-subrc NE 0 ##NEEDED.
    "Do nothing
  ENDIF.

ENDFORM.
