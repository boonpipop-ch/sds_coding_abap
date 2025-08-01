*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0880_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_data .
*  IF go_data IS INITIAL.
*    CREATE OBJECT go_data.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_EXTAB   text
*----------------------------------------------------------------------*
FORM pf_status_1 USING us_extab TYPE slis_t_extab.

  CONSTANTS lc_status TYPE c LENGTH 9 VALUE 'ZSTANDARD'.

  SET PF-STATUS lc_status EXCLUDING us_extab.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
FORM user_command USING p_ucomm TYPE sy-ucomm
                        p_selfld TYPE slis_selfield.
*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : ref_grid TYPE REF TO cl_gui_alv_grid.

  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
    CALL METHOD ref_grid->check_changed_data.
  ENDIF.
*&---------------------------------------------------------------------*

  CASE p_ucomm.
    WHEN '&IC1'.
      PERFORM f_call_transection.
    WHEN 'ALL'.
      PERFORM f_check_box USING 'X'.
    WHEN 'SAL'.
      PERFORM f_check_box USING space.
  ENDCASE.

  CALL METHOD ref_grid->refresh_table_display.
  CLEAR : ref_grid.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_call_transection.
  DATA: le_row     TYPE i,
        le_value   TYPE c,
        le_col     TYPE i,
        les_row_id TYPE lvc_s_row,
        les_col_id TYPE lvc_s_col,
        les_row_no TYPE lvc_s_roid.

  DATA : ref_grid TYPE REF TO cl_gui_alv_grid.
  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.

  CALL METHOD ref_grid->get_current_cell
    IMPORTING
      e_row     = le_row
      e_value   = le_value
      e_col     = le_col
      es_row_id = les_row_id
      es_col_id = les_col_id.

  CLEAR : bdcdata[],messtab[].

  READ TABLE gt_result INTO gs_result INDEX les_row_id-index.
  IF sy-subrc = 0.

  ENDIF.

ENDFORM.                    " F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = gc_x.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM bdc_transaction  USING    tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
  DATA: lv_opt  TYPE ctu_params.

  lv_opt-dismode  = gc_e."'A'
  lv_opt-updmode  = gc_l.
  lv_opt-nobinpt  = gc_x.
  "lv_opt-cattmode = 'A'.
  "lv_opt-defsize  = 'X'.

  CALL TRANSACTION tcode USING bdcdata
*                   MODE   'E'"N
*                   UPDATE 'L'
                   OPTIONS FROM lv_opt
                   MESSAGES INTO messtab.

ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0645   text
*----------------------------------------------------------------------*
FORM f_check_box  USING lv_check.
  gs_result-check = lv_check.
  MODIFY gt_result FROM gs_result TRANSPORTING check
                                         WHERE check NE gs_result-check.

ENDFORM.                    " F_CHECK_BOX
