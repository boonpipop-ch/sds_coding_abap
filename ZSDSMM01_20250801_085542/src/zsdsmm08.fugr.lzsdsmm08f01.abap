*----------------------------------------------------------------------*
*   INCLUDE LMEVIEWSF01                                                *
*----------------------------------------------------------------------*
DATA: call_subscreen   TYPE sy-dynnr,                       "#EC NEEDED
      call_prog        TYPE sy-repid,                       "#EC NEEDED
      call_view        TYPE REF TO cl_screen_view_mm,       "#EC NEEDED
      call_view_stack  TYPE REF TO cl_screen_view_mm OCCURS 0, "#EC NEEDED
      global_framework TYPE REF TO cl_framework_mm,         "#EC NEEDED
      global_help_view TYPE REF TO cl_screen_view_mm,       "#EC NEEDED
      global_help_prog TYPE sy-repid.                       "#EC NEEDED
DATA wkafields TYPE string40.

*---------------------------------------------------------------------*
*       FORM CALL_SCREEN                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_SCREEN                                                      *
*---------------------------------------------------------------------*
FORM call_screen USING p_screen TYPE sy-dynnr.
  CALL SCREEN p_screen.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CALL_POPUP                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM call_popup USING p_screen TYPE sy-dynnr
                      p_starting_x TYPE sy-tabix
                      p_starting_y TYPE sy-tabix
                      p_ending_x   TYPE sy-tabix
                      p_ending_y   TYPE sy-tabix.
  CALL SCREEN p_screen STARTING AT  p_starting_x p_starting_y
                       ENDING   AT  p_ending_x p_ending_y.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SET_SCREEN                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_SCREEN                                                      *
*---------------------------------------------------------------------*
FORM set_screen USING p_screen TYPE sy-dynnr.
  SET SCREEN p_screen.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PUSH_CALL_VIEW                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM push_call_view.
  APPEND call_view TO call_view_stack.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM POP_CALL_VIEW                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM pop_call_view.
  IF NOT call_view_stack[] IS INITIAL.
    DATA: last TYPE sy-tabix.
    DESCRIBE TABLE call_view_stack LINES last.
    READ TABLE call_view_stack INTO call_view INDEX last.
    DELETE call_view_stack INDEX last.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SET_VALUE                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_NAME                                                        *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM set_value  USING p_name p_value.
  FIELD-SYMBOLS <field>.
  ASSIGN (p_name) TO <field>.
  <field> = p_value.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SET_SUBSCREEN_AND_PROG                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DYNNR                                                         *
*  -->  PROG                                                          *
*  -->  VIEW                                                          *
*  -->  TO                                                            *
*  -->  CL_SCREEN_VIEW_MM                                             *
*---------------------------------------------------------------------*
FORM set_subscreen_and_prog USING dynnr TYPE sy-dynnr
                                  prog TYPE sy-repid
                                  view TYPE REF TO cl_screen_view_mm.
  call_subscreen = dynnr.
  call_prog = prog.
  call_view = view.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_OK_CODE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FCODE                                                         *
*---------------------------------------------------------------------*
FORM get_ok_code USING fcode TYPE sy-ucomm.
  fcode = ok-code.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  EVENT_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pbo OUTPUT.

  IF sy-tcode = 'ME57'.
    TRY.
        CALL METHOD cl_dynp_gui_state=>set( cl_dynp_gui_state=>edit_dirty ).
      CATCH cx_dynp_not_a_mainscreen.
      CATCH cx_dynp_event_not_pbo.
      CATCH cx_dynp_set_property_failed.
      CATCH cx_dynp_get_property_failed.
    ENDTRY.
  ENDIF.

  CALL METHOD call_view->handle_event( 'PBO' ).
  wkafields = TEXT-002.
ENDMODULE.                             " EVENT_PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PBO_TC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pbo_tc OUTPUT.
  CALL METHOD call_view->handle_event( 'PBO_TC_LINE' ).
ENDMODULE.                             " EVENT_PBO_TC  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PBO_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pbo_subscreen OUTPUT.
  PERFORM push_call_view.
  CALL METHOD call_view->handle_event( 'PBO_SUBSCREEN' ).
ENDMODULE.                             " EVENT_PBO_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PBO_POPSUBSCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pbo_popsubscreen OUTPUT.
  PERFORM pop_call_view.
ENDMODULE.                             " EVENT_PBO_SUBSCREEN  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EVENT_PBO_FINISHED  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pbo_finished OUTPUT.
  CALL METHOD call_view->handle_event( 'PBO_FINISHED' ).
ENDMODULE.                             " EVENT_PBO_FINISHED  OUTPUT

MODULE hide_rfm_switch_fld OUTPUT.
* For CLOUD system check if related Scope Item is activated
  IF cl_retail_switch_status_check=>check_switch_activation_status(
     cl_retail_switch_status_check=>gc_sfws_retailswitch ) = abap_false.

    LOOP AT SCREEN.
      IF screen-group1 = 'ROP'. "Retail Online Planning
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*Screen 3211
  IF cl_retail_switch_status_check=>check_switch_activation_status(
     cl_retail_switch_status_check=>gc_retail_char_sfws ) = abap_false.
    LOOP AT SCREEN.
      IF screen-group1   = 'CVL'.
        screen-invisible = '1'.
        screen-output    = '0'.
        screen-input     = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EVENT_PAI_POPSUBSCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pai_popsubscreen INPUT.
  PERFORM pop_call_view.
ENDMODULE.                             " EVENT_PBO_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PAI_SUBSCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pai_subscreen INPUT.
  PERFORM push_call_view.
  CALL METHOD call_view->handle_event( 'PAI_SUBSCREEN' ).
ENDMODULE.                             " EVENT_PBO_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PAI     INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pai INPUT.
  CALL METHOD call_view->handle_event( 'PAI' ).
ENDMODULE.                             " EVENT_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PAI_FINISHED  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pai_finished INPUT.


  "{ Begin ENHO AD_MPN_PUR2_LMEVIEWSF01 IS-AD-MPN AD_MPN_IC }
* clear temporary MPN system messages                 "note 916061
  PERFORM mepo_pic_delete_message IN PROGRAM saplmepo.
*
* Addition by Roger       <<< DI Note: 426616
  IF NOT ok-code IS INITIAL.
    CALL METHOD cl_framework_mm=>get_instance
      IMPORTING
        ex_instance = global_framework.
    CALL METHOD global_framework->set_fcode
      EXPORTING
        im_fcode = sy-ucomm.
    CLEAR ok-code.
  ENDIF.
  "{ End ENHO AD_MPN_PUR2_LMEVIEWSF01 IS-AD-MPN AD_MPN_IC }


ENHANCEMENT-POINT EVENT_PAI_FINISHED_01 SPOTS ES_LMEVIEWSF01 INCLUDE BOUND.
  CALL METHOD call_view->handle_event( 'BEFORE_TRANSPORT' ).

  CALL METHOD call_view->handle_event( 'PAI_FINISHED' ).
ENDMODULE.                             " EVENT_PAI_FINISHED  INPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PAI_TC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pai_tc INPUT.
  CALL METHOD call_view->handle_event( 'PAI_TC_LINE' ).
ENDMODULE.                             " EVENT_PAI_TC  INPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PBO_PREPARE OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pbo_prepare OUTPUT.
  CALL METHOD call_view->handle_event( 'PBO_PREPARE' ).
ENDMODULE.                             " EVENT_PBO_PREPARE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PAI_PREPARE INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE event_pai_prepare INPUT.
  CALL METHOD call_view->handle_event( 'PAI_PREPARE' ).
ENDMODULE.                             " EVENT_PAI_PREPARE  INPUT

*&---------------------------------------------------------------------*
*&      Module  EVENT_POV_LIST INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pov_list INPUT.

  CALL METHOD cl_framework_mm=>get_instance
    IMPORTING
      ex_instance = global_framework.
  global_help_prog = sy-repid.
  CALL METHOD global_framework->get_view
    EXPORTING
      im_prog  = global_help_prog
      im_dynnr = sy-dynnr
    IMPORTING
      ex_view  = global_help_view.
  IF NOT global_help_view IS INITIAL.
    CALL METHOD global_help_view->handle_event( 'EVENT_POV_LIST' ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EVENT_POH_LIST INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_poh_list INPUT.

  CALL METHOD cl_framework_mm=>get_instance
    IMPORTING
      ex_instance = global_framework.
  global_help_prog = sy-repid.
  CALL METHOD global_framework->get_view
    EXPORTING
      im_prog  = global_help_prog
      im_dynnr = sy-dynnr
    IMPORTING
      ex_view  = global_help_view.
  IF NOT global_help_view IS INITIAL.
    CALL METHOD global_help_view->handle_event( 'EVENT_POH_LIST' ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_FCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_fcode INPUT.
  CALL METHOD cl_framework_mm=>get_instance
    IMPORTING
      ex_instance = global_framework.

  CALL METHOD global_framework->set_fcode
    EXPORTING
      im_fcode = sy-ucomm.

  CLEAR ok-code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FCODE_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_exit INPUT.
  CALL METHOD call_view->handle_event( 'FCODE' ).
ENDMODULE.                             " FCODE_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  BEFORE_TRANSPORT INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE before_transport INPUT.
  CALL METHOD call_view->handle_event( 'BEFORE_TRANSPORT' ).
ENDMODULE.                             " BEFORE_TRANSPORT  INPUT

*&---------------------------------------------------------------------*
*&      Module  CLEAR_OKCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_okcode INPUT.
  CLEAR sy-ucomm.
ENDMODULE.                             " CLEAR_OKCODE  INPUT

*>>> OLC Project
* Core adaptation for VORNR searchhelp
*&---------------------------------------------------------------------*
*&      Module  VAL_REQ_VORNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_req_vornr INPUT.

ENHANCEMENT-POINT LMEVIEWSF01_OLC_001 SPOTS ES_LMEVIEWSF01 STATIC INCLUDE BOUND .

ENHANCEMENT-SECTION     LMEVIEWSF01_OLC_002 SPOTS ES_LMEVIEWSF01 INCLUDE BOUND .
* No OLC order => Ordinary F4 Help
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname     = 'MEACCT1000'
      fieldname   = 'VORNR'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'MEACCT1000-VORNR'
    EXCEPTIONS
      OTHERS      = 1.
END-ENHANCEMENT-SECTION.

ENDMODULE.                 " VAL_REQ_VORNR  INPUT
*<<< OLC Project

************* Extensibility : ME21N/1708 **************************
*DATA: rs_ekko type ekko.
*
*MODULE SET_EXTENSIBILITY_CONTEXT OUTPUT.
*
*  IF sy-tcode = 'ME21N'.
*   " set the relevant SAP GUI Context to display the custom fields which were added to this GUI context
*   cl_cfd_sap_gui_context_api=>set_context( 'PO_HEADER_GUI' ).
*
*   " set custom field values to generic dynpro fields of a GUI Context
*   cl_cfd_sap_gui_context_api=>set_values(
*   iv_gui_context_name = 'PO_HEADER_GUI'
*   ir_fields           = REF #( rs_ekko ) ).          " global reference of structure which must contain SAP fields (relevant for ABAP managed fields) and fields of Persistence Include
*
*  ENDIF.
*
*  IF sy-tcode = 'ME22N'.
*    " set the relevant SAP GUI Context to display the custom fields which were added to this GUI context
*   cl_cfd_sap_gui_context_api=>set_context( 'PO_HEADER_GUI' ).
*
*   " set custom field values to generic dynpro fields of a GUI Context
*   cl_cfd_sap_gui_context_api=>set_values(
*   iv_gui_context_name = 'PO_HEADER_GUI'
*   ir_fields           = REF #( rs_ekko ) ).          " global reference of structure which must contain SAP fields (relevant for ABAP managed fields) and fields of Persistence Include
*
*   " set screen properties of generic dynpro fields
*    CALL METHOD cl_cfd_sap_gui_context_api=>set_properties
*      EXPORTING
*     iv_gui_context_name = 'PO_HEADER_GUI'
*    " set read-only property for the custom fields in case of display mode
*    is_properties       = VALUE #( read_only = abap_true ).
*
*  ENDIF.
*
*  IF sy-tcode = 'ME23N'.
*    " set the relevant SAP GUI Context to display the custom fields which were added to this GUI context
*   cl_cfd_sap_gui_context_api=>set_context( 'PO_HEADER_GUI' ).
*
*   " set custom field values to generic dynpro fields of a GUI Context
*   cl_cfd_sap_gui_context_api=>set_values(
*   iv_gui_context_name = 'PO_HEADER_GUI'
*   ir_fields           = REF #( rs_ekko ) ).          " global reference of structure which must contain SAP fields (relevant for ABAP managed fields) and fields of Persistence Include
*
*   " set screen properties of generic dynpro fields
*    CALL METHOD cl_cfd_sap_gui_context_api=>set_properties
*      EXPORTING
*     iv_gui_context_name = 'PO_HEADER_GUI'
*    " set read-only property for the custom fields in case of display mode
*    is_properties       = VALUE #( read_only = abap_true ).
*
*  ENDIF.
*
*ENDMODULE.
*
*MODULE GET_CUSTOM_FIELDS_VALUES INPUT.
*  CL_CFD_SAP_GUI_CONTEXT_API=>GET_VALUES(
*  iv_gui_context_name = 'PO_HEADER_GUI'
*  ir_fields = ref #( rs_ekko ) ).
*
*ENDMODULE.
************* Extensibility : ME21N/1708 **************************
