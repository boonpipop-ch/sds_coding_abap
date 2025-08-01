*----------------------------------------------------------------------*
***INCLUDE LZSDSFI17F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_read_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_FILENAME
*&---------------------------------------------------------------------*
FORM f_read_file.

  DATA: ls_data TYPE gty_data.

  CLEAR: gt_data.

  OPEN DATASET gv_filename FOR INPUT IN BINARY MODE.
  IF sy-subrc = 0.
    DO.
      READ DATASET gv_filename INTO ls_data.
      IF sy-subrc = 0.
        APPEND ls_data TO gt_data.
      ELSE.
        APPEND ls_data TO gt_data.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
  CLOSE DATASET gv_filename.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_pdf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DATA
*&---------------------------------------------------------------------*
FORM f_display_pdf .
  IF gt_data IS NOT INITIAL.
    CALL SCREEN 100 STARTING AT 5 1.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  PERFORM f_init_scr_100.

ENDMODULE. " STATUS_0100 OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE gv_ok_code.
    WHEN 'OK' OR 'BACK' OR 'EXIT' OR 'CANCEL'.
      CLEAR gv_ok_code.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE. " USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
*& Form f_init_scr_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_scr_100 .

  DATA:
    lv_type     TYPE text100,
    lv_sub_type TYPE text100.

  SET PF-STATUS '100'.
  CLEAR gv_ok_code.

  IF NOT go_custom_container IS INITIAL.
    CALL METHOD go_custom_container->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE internal_error  ##FM_RAISE_OK.
    ENDIF.
    FREE go_custom_container.
    CLEAR go_custom_container.
  ENDIF.

  IF NOT go_html IS INITIAL.
    FREE go_html.
    CLEAR go_html.
  ENDIF.

  gv_container = 'CT_PDF'.

  IF go_custom_container IS INITIAL.
    CREATE OBJECT go_custom_container
      EXPORTING
        container_name = gv_container.
  ENDIF.

  CREATE OBJECT go_html
    EXPORTING
      parent = go_custom_container.

  gv_alignment = go_html->align_at_left +
                 go_html->align_at_right +
                 go_html->align_at_top +
                 go_html->align_at_bottom.
  CALL METHOD go_html->set_alignment
    EXPORTING
      alignment = gv_alignment.

*  CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
*    EXPORTING
*      filename  = CONV char1024( gv_filename )
**     UPPERCASE = 'X'
*    IMPORTING
*      extension = gv_ext.

  CASE gv_ext.
    WHEN 'PDF'.
      lv_type = 'application'   ##NO_TEXT.
      lv_sub_type = 'pdf'       ##NO_TEXT.
    WHEN 'JPG'.
      lv_type = 'image'         ##NO_TEXT.
      lv_sub_type = 'jpeg'      ##NO_TEXT.
    WHEN 'PNG'.
      lv_type = 'image'         ##NO_TEXT.
      lv_sub_type = 'png'       ##NO_TEXT.
    WHEN OTHERS.
      lv_type = 'text'          ##NO_TEXT.
      lv_sub_type = 'plain'     ##NO_TEXT.
  ENDCASE.

* Load the HTML
  go_html->load_data(
    EXPORTING
      type         = lv_type
      subtype      = lv_sub_type
    IMPORTING
      assigned_url         = gv_url
    CHANGING
      data_table           = gt_data
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      OTHERS               = 4 ).
* Show it
  IF sy-subrc EQ 0.
    go_html->show_url( url = gv_url ). "in_place = 'X' ).
    go_html->do_refresh(  ).
  ENDIF.


ENDFORM.
