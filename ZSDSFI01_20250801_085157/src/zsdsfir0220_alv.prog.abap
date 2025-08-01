*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0220_ALV
*  Creation Date      : 21.12.2023
*  Author             : Apichat Ch. ( Eviden )
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
  lcl_event_handler DEFINITION DEFERRED.  "for event handling

CLASS:
  cl_gui_column_tree DEFINITION LOAD,
  cl_gui_cfw DEFINITION LOAD.

DATA:
  gref_tree    TYPE REF TO cl_gui_alv_tree                  ##NEEDED,
  gref_toolbar TYPE REF TO cl_gui_toolbar                   ##NEEDED.


*----------------------------------------------------------------------*
*-- Tables
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Type definitions
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_xls,
    tree_desc TYPE lvc_value.
    INCLUDE TYPE zsdsfis077.
TYPES:
  END OF gty_xls.

TYPES:
  gtt_xls TYPE STANDARD TABLE OF gty_xls WITH EMPTY KEY.

*----------------------------------------------------------------------*
*-- Constants
*----------------------------------------------------------------------*
CONSTANTS:
  gc_save_all TYPE char1         VALUE 'A',                 "#EC NEEDED
  gc_save     TYPE sy-ucomm      VALUE 'SAVE_1'.            "#EC NEEDED

*----------------------------------------------------------------------*
*-- Internal Tables
*----------------------------------------------------------------------*
DATA:
  gt_fieldcat  TYPE lvc_t_fcat,                             "#EC NEEDED
  gt_sort      TYPE lvc_t_sort,                             "#EC NEEDED
  gt_tool_exc  TYPE ui_functions,                           "#EC NEEDED
  gt_excl      TYPE STANDARD TABLE OF sy-ucomm,             "#EC NEEDED
  gt_group_key TYPE lvc_t_nkey,                             "#EC NEEDED

  gt_all_nodes TYPE lvc_t_nkey.                             "#EC NEEDED

*----------------------------------------------------------------------*
*-- Work Areas
*----------------------------------------------------------------------*
DATA:
  gs_variant TYPE disvariant,                               "#EC NEEDED
  gs_layout  TYPE lvc_s_layo.                               "#EC NEEDED

*----------------------------------------------------------------------*
*-- Ranges
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Variable declarations
*----------------------------------------------------------------------*
DATA:
  gf_ok_code          TYPE sy-ucomm,                        "#EC NEEDED
  gf_save_ok          TYPE sy-ucomm,                        "#EC NEEDED
  gref_event_receiver TYPE REF TO lcl_event_handler.        "#EC NEEDED

*----------------------------------------------------------------------*
*-- Field-Symbols
*----------------------------------------------------------------------*
*FIELD-SYMBOLS:
*  <g_list_1> TYPE STANDARD TABLE ##FS_ASSIGN_OK,            "#EC NEEDED
*  .                                                         "#EC NEEDED

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

      on_link_click     FOR EVENT link_click OF cl_gui_alv_tree
        IMPORTING fieldname node_key,

      on_checkbox_change FOR EVENT checkbox_change OF cl_gui_alv_tree
        IMPORTING checked
                  fieldname
                  node_key,

      on_button_click    FOR EVENT button_click OF cl_gui_alv_tree
        IMPORTING fieldname
                  node_key,

      on_function_selected FOR EVENT function_selected OF cl_gui_toolbar
        IMPORTING fcode.


ENDCLASS.                    "lcl_event_handler DEFINITION

*-----------------------------------------------------------------------
* CLASS IMPLEMENTATION
*-----------------------------------------------------------------------
* Event Handler
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_link_click.
    PERFORM f_link_click IN PROGRAM (sy-cprog)
      USING fieldname node_key
      IF FOUND.
  ENDMETHOD.                    "on_hotspot_click

  METHOD on_checkbox_change.
    PERFORM f_checkbox_change IN PROGRAM (sy-cprog)
      USING checked
            fieldname
            node_key
      IF FOUND.
  ENDMETHOD.

  METHOD on_button_click.
    PERFORM f_button_click IN PROGRAM (sy-cprog)
      USING fieldname
            node_key
      IF FOUND.
  ENDMETHOD.

  METHOD on_function_selected.
    PERFORM f_alv_function IN PROGRAM (sy-cprog)
      USING fcode
      IF FOUND.
  ENDMETHOD.
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
*       Module  STATUS_ALV_1  OUTPUT
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

ENDMODULE.                 " STATUS_ALV  OUTPUT

*----------------------------------------------------------------------*
*       Module  DISPLAY_ALV  OUTPUT
*----------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.

* Display ALV Tree
  IF gref_tree IS INITIAL.
    PERFORM f_init_tree.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT

*----------------------------------------------------------------------*
*       Module  EXIT_COMMANDS_1 INPUT
*----------------------------------------------------------------------*
*       Cancel transaction and leave the program
*----------------------------------------------------------------------*
MODULE exit_commands INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " EXIT_COMMANDS_1  INPUT


*&---------------------------------------------------------------------*
*& Form f_init_tree
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_tree .

  DATA:
    lv_tree_container_name TYPE char30,
    lref_custom_container  TYPE REF TO cl_gui_custom_container.


* create fieldcatalog
  PERFORM f_alv_build_fieldcat CHANGING gt_fieldcat.

* create container for alv-tree
  lv_tree_container_name = 'CTL_ALV'.

  IF sy-batch IS INITIAL.
    CREATE OBJECT lref_custom_container
      EXPORTING
        container_name              = lv_tree_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.
  ENDIF.

* create tree control
  CREATE OBJECT gref_tree
    EXPORTING
      parent                      = lref_custom_container
*     node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
*     item_selection              = abap_true
      no_html_header              = gc_true
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

* repid for saving variants
  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-repid.

* create emty tree-control
  CALL METHOD gref_tree->set_table_for_first_display
    EXPORTING
*     is_hierarchy_header = lv_hierarchy_header
*     it_list_commentary  = lt_list_commentary
*     i_logo          = l_logo
*     i_background_id = 'ALV_BACKGROUND'
      i_default       = 'X'
      i_save          = 'A'
      is_variant      = ls_variant
    CHANGING
      it_outtab       = gt_output "table must be emty !!
      it_fieldcatalog = gt_fieldcat.

* create hierarchy
  PERFORM f_create_hierarchy.

* add own functioncodes to the toolbar
  PERFORM f_change_toolbar.

* register events
  PERFORM f_register_events.

* adjust column_width
  CALL METHOD gref_tree->column_optimize.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_register_events
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_register_events .

  DATA: lt_events TYPE cntl_simple_events,
        ls_event  TYPE cntl_simple_event.

  ls_event-eventid = cl_gui_column_tree=>eventid_link_click.
  APPEND ls_event TO lt_events.

  ls_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
  APPEND ls_event TO lt_events.

  ls_event-eventid = cl_gui_column_tree=>eventid_button_click.
  APPEND ls_event TO lt_events.

  gref_tree->set_registered_events(
    EXPORTING
      events                    = lt_events                 " Event Table
    EXCEPTIONS
      cntl_error                = 1                " cntl_error
      cntl_system_error         = 2                " cntl_system_error
      illegal_event_combination = 3                " ILLEGAL_EVENT_COMBINATION
      OTHERS                    = 4
  ).
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT gref_event_receiver.
  SET HANDLER:
    gref_event_receiver->on_link_click        FOR gref_tree,
    gref_event_receiver->on_checkbox_change   FOR gref_tree,
    gref_event_receiver->on_button_click      FOR gref_tree,

    gref_event_receiver->on_function_selected FOR gref_toolbar.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_ALV  INPUT
*&---------------------------------------------------------------------*
*       text
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


ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_change_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_change_toolbar .

  CALL METHOD gref_tree->get_toolbar_object
    IMPORTING
      er_toolbar = gref_toolbar.

  IF gref_toolbar IS INITIAL. "could happen if you do not use the
    RETURN.
  ENDIF.
  "standard toolbar

* add seperator to toolbar
  CALL METHOD gref_toolbar->add_button
    EXPORTING
      fcode     = ''
      icon      = ''
      butn_type = cntb_btype_sep.

* add Select all Button to toolbar
  CALL METHOD gref_toolbar->add_button
    EXPORTING
      fcode     = gc_fcode-sel_a "SEL_ALL
      icon      = '@4B@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = TEXT-f01.   "Select All

* add Select none Button to toolbar
  CALL METHOD gref_toolbar->add_button
    EXPORTING
      fcode     = gc_fcode-sel_n "SEL_NONE
      icon      = '@4D@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = TEXT-f02.   "Select All

* add seperator to toolbar
  CALL METHOD gref_toolbar->add_button
    EXPORTING
      fcode     = ''
      icon      = ''
      butn_type = cntb_btype_sep.

* add Select none Button to toolbar
  CALL METHOD gref_toolbar->add_button
    EXPORTING
      fcode     = gc_fcode-download "SEL_NONE
      icon      = '@49@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = TEXT-f03.   "Download


ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_hotspot_click_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_row_id
*&      --> uf_column_id
*&---------------------------------------------------------------------*
FORM f_link_click USING uf_fieldname   TYPE lvc_fname
                        uf_node_key TYPE lvc_nkey               ##CALLED.

  DATA ls_output TYPE zsdsfis077.

  gref_tree->get_outtab_line(
    EXPORTING
      i_node_key     = uf_node_key                " Node Key
    IMPORTING
      e_outtab_line  = ls_output                   " Line of Outtab
*      e_node_text    =                  " node text
*      et_item_layout =                  " Layout structure for items of the ALV tree control
*      es_node_layout =                  " Node Layout of ALV Tree Control
    EXCEPTIONS
      node_not_found = 1                " Node does not exist
      OTHERS         = 2
  ).
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CASE uf_fieldname.
    WHEN 'BELNR_OPN'.
      IF ls_output-belnr_opn IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_output-belnr_opn.
        SET PARAMETER ID 'BUK' FIELD p_bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-gjahr_opn.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.  "#EC CI_CALLTA.
      ENDIF.
    WHEN 'BELNR'.
      IF ls_output-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_output-belnr.
        SET PARAMETER ID 'BUK' FIELD p_bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.  "#EC CI_CALLTA.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_checkbox_change
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_row_id
*&      --> uf_column_id
*&---------------------------------------------------------------------*
FORM f_checkbox_change
      USING checked   TYPE  c
            fieldname TYPE  lvc_fname
            node_key  TYPE  lvc_nkey                  ##CALLED ##NEEDED.

  gref_tree->get_checked_items(
    IMPORTING
      et_checked_items = DATA(lt_checked_items)                ##NEEDED
  ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_button_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> uf_row_id
*&      --> uf_column_id
*&---------------------------------------------------------------------*
FORM f_button_click
      USING fieldname TYPE  lvc_fname
            node_key  TYPE  lvc_nkey                  ##CALLED ##NEEDED.

  PERFORM f_button_click_update USING fieldname node_key.
  gref_tree->frontend_update( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_alv_function
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_alv_function USING uf_fcode TYPE ui_func               ##CALLED.

  CASE uf_fcode.
    WHEN gc_fcode-sel_a.
      PERFORM f_set_check_all USING gc_true.
      gref_tree->frontend_update( ).
    WHEN gc_fcode-sel_n.
      PERFORM f_set_check_all USING ''.
      gref_tree->frontend_update( ).
    WHEN gc_fcode-download.
      PERFORM f_download_xlsx.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_check_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_set_check_all  USING uf_check TYPE char01.
  DATA: ls_out_line TYPE zsdsfis077                           ##NEEDED,
        ls_layout   TYPE lvc_s_laci.

  LOOP AT gt_group_key INTO DATA(ls_group_key).
    ls_layout-fieldname = 'SEL'.
    ls_layout-chosen    = uf_check.
    ls_layout-u_chosen  = gc_true.

    gref_tree->change_item(
      EXPORTING
        i_node_key     = ls_group_key               " Key of the Changed Item's Line
        i_fieldname    = 'SEL'                      " Fieldname of Item to Change
        i_data         = ''                         " Content of the Item to Change
*        i_u_data       = 'X'                       " update i_data
        is_item_layout = ls_layout                  " Item Layout
      EXCEPTIONS
        node_not_found = 1                          " Node Key not Found
        OTHERS         = 2
    ).
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_download_xlsx
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_download_xlsx .



  DATA:
    lv_node_text TYPE lvc_value,
    ls_output    TYPE zsdsfis077,
    ls_xls       TYPE gty_xls,
    lt_xls       TYPE gtt_xls.

  LOOP AT gt_all_nodes INTO DATA(ls_node_key).
    CLEAR ls_output.
    gref_tree->get_outtab_line(
      EXPORTING
        i_node_key     = ls_node_key      " Node Key
      IMPORTING
        e_node_text    = lv_node_text
        e_outtab_line  = ls_output        " Line of Outtab
      EXCEPTIONS
        node_not_found = 1                " Node does not exist
        OTHERS         = 2
    ).

    IF sy-subrc EQ 0.
      ls_xls = CORRESPONDING #( ls_output ).
      ls_xls-tree_desc = lv_node_text.
      APPEND ls_xls TO lt_xls.
    ENDIF.
  ENDLOOP.

  PERFORM f_export_xlsx USING lt_xls.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_export_xlsx
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_XLS
*&---------------------------------------------------------------------*
FORM f_export_xlsx  USING ut_xls TYPE gtt_xls.

  TYPES:
    BEGIN OF lty_s_cline,
      line TYPE c LENGTH 1024, " Line of type Character
    END OF lty_s_cline .

  DATA:
    lv_fname    TYPE string,
    lv_path     TYPE string,
    lv_fullpath TYPE string,

    lref_table  TYPE REF TO cl_salv_table,
    lv_xml_type TYPE salv_bs_constant,
    lv_xml      TYPE xstring,

    lt_srctab   TYPE STANDARD TABLE OF lty_s_cline,
    lv_len      TYPE i.

  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      window_title              = 'Export As'       ##NO_TEXT " Window Title
*      default_extension         =                  " Default Extension
*      default_file_name         =                  " Default File Name
*      with_encoding             =
       file_filter               = '*.xlsx'         " File Type Filter Table
*      initial_directory         =                  " Initial Directory
*      prompt_on_overwrite       = 'X'
    CHANGING
      filename                  = lv_fname          " File Name to Save
      path                      = lv_path           " Path to File
      fullpath                  = lv_fullpath       " Path + File Name
*      user_action               =                  " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*      file_encoding             =
    EXCEPTIONS
      cntl_error                = 1                " Control error
      error_no_gui              = 2                " No GUI available
      not_supported_by_gui      = 3                " GUI does not support this
      invalid_default_file_name = 4                " Invalid default file name
      OTHERS                    = 5
  ).

  IF sy-subrc NE 0 OR
        lv_fullpath IS INITIAL.
    RETURN.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lref_table
      CHANGING
        t_table      = ut_xls ).

    CATCH cx_salv_msg.                                  "#EC NO_HANDLER

  ENDTRY.

  lv_xml_type =  if_salv_bs_xml=>c_type_xlsx.
  lv_xml      = lref_table->to_xml( xml_type = lv_xml_type ).

  IF lv_xml IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_xml
*     APPEND_TO_TABLE       = ' '
    IMPORTING
      output_length = lv_len
    TABLES
      binary_tab    = lt_srctab.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
      bin_filesize              = lv_len                  " File length for binary files
      filename                  = |{ lv_fullpath }.xlsx| ##NO_TEXT " Name of file
      filetype                  = 'BIN'                " File type (ASCII, binary ...)
*    IMPORTING
*      filelength                =                     " Number of bytes transferred
    CHANGING
      data_tab                  = lt_srctab            " Transfer table
    EXCEPTIONS
      file_write_error          = 1                    " Cannot write to file
      no_batch                  = 2                    " Front-End Function Cannot Be Executed in Backgrnd
      gui_refuse_filetransfer   = 3                    " Incorrect Front End
      invalid_type              = 4                    " Invalid value for parameter FILETYPE
      no_authority              = 5                    " No Download Authorization
      unknown_error             = 6                    " Unknown error
      header_not_allowed        = 7                    " Invalid header
      separator_not_allowed     = 8                    " Invalid separator
      filesize_not_allowed      = 9                    " Invalid file size
      header_too_long           = 10                   " Header information currently restricted to 1023 bytes
      dp_error_create           = 11                   " Cannot create DataProvider
      dp_error_send             = 12                   " Error Sending Data with DataProvider
      dp_error_write            = 13                   " Error Writing Data with DataProvider
      unknown_dp_error          = 14                   " Error when calling data provider
      access_denied             = 15                   " Access to File Denied
      dp_out_of_memory          = 16                   " Not Enough Memory in DataProvider
      disk_full                 = 17                   " Storage Medium full
      dp_timeout                = 18                   " Timeout of DataProvider
      file_not_found            = 19                   " Could not find file
      dataprovider_exception    = 20                   " General Exception Error in DataProvider
      control_flush_error       = 21                   " Error in Control Framework
      not_supported_by_gui      = 22                   " GUI does not support this
      error_no_gui              = 23                   " GUI not available
      OTHERS                    = 24
  ).

  IF sy-subrc <> 0.
*   Error occurred during generate Excel file.
    MESSAGE s004(zsdsca01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.
