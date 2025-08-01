FUNCTION z_sdsfi_popup.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_START_COLUMN) TYPE  I DEFAULT 25
*"     REFERENCE(I_START_LINE) TYPE  I DEFAULT 6
*"     REFERENCE(I_END_COLUMN) TYPE  I DEFAULT 100
*"     REFERENCE(I_END_LINE) TYPE  I DEFAULT 16
*"     REFERENCE(I_TITLE) TYPE  STRING DEFAULT 'Popup'
*"     REFERENCE(I_POPUP) TYPE  FLAG DEFAULT ' '
*"  TABLES
*"      IT_DATA TYPE  STANDARD TABLE
*"----------------------------------------------------------------------

  DATA: lref_alv       TYPE REF TO cl_salv_table,
        lref_functions TYPE REF TO cl_salv_functions_list.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lref_alv
        CHANGING
          t_table      = it_data[] ).

    CATCH cx_salv_msg  ##NO_HANDLER.
  ENDTRY.

  lref_functions = lref_alv->get_functions( ).
  lref_functions->set_all( 'X' ).

  IF lref_alv IS BOUND.
    IF i_popup = 'X'.
      lref_alv->set_screen_popup(
        start_column = i_start_column
        end_column  = i_end_column
        start_line  = i_start_line
        end_line    = i_end_line ).
    ENDIF.

    lref_alv->display( ).

  ENDIF.


ENDFUNCTION.
