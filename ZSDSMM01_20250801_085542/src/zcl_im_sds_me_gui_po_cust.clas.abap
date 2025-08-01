CLASS zcl_im_sds_me_gui_po_cust DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_me_gui_po_cust .

    CONSTANTS subscreen2 TYPE mepo_name VALUE 'ITEMSSCREEN1' ##NO_TEXT.
    CONSTANTS subscreen1 TYPE mepo_name VALUE 'HEADERSCREEN1' ##NO_TEXT.
    CONSTANTS did_item_screen TYPE mepo_name VALUE 'DID_ITEM_SCREEN' ##NO_TEXT.

    CLASS-DATA po_no TYPE ekko-ebeln .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA dynp_data_pbo TYPE zsdsmms020 .
    DATA dynp_data_pai TYPE zsdsmms020 .

    DATA did_dynp_data_pbo TYPE zdidmms010 .
    DATA did_dynp_data_pai LIKE did_dynp_data_pbo.
*
*  data DYNP_DATA_PBO type ZSDSMMS014 .
*  data DYNP_DATA_PAI type ZSDSMMS014 .
ENDCLASS.



CLASS ZCL_IM_SDS_ME_GUI_PO_CUST IMPLEMENTATION.


  METHOD if_ex_me_gui_po_cust~execute.
    DATA : lv_check_sds LIKE abap_true.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    lv_check_sds = lcl_data=>check_sds( sy-mandt ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF lv_check_sds EQ abap_true.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

  ENDMETHOD.


  METHOD if_ex_me_gui_po_cust~map_dynpro_fields.
*&---------------------------------------------------------------------*
*& Version/T.P No.: 1.01 (F36K916023)
*& Changed On/By:   2025/04/18 Thomas S.(IBM)
*& Description:     Add DID Requirement: CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_MAP_DYNPRO'
*&---------------------------------------------------------------------*

    DATA : lv_check_sds LIKE abap_true.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    lv_check_sds = lcl_data=>check_sds( sy-mandt ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF lv_check_sds EQ abap_true.
      FIELD-SYMBOLS: <mapping> LIKE LINE OF ch_mapping.
      LOOP AT ch_mapping ASSIGNING <mapping>.

        CASE <mapping>-fieldname.

          WHEN 'ZZ_JTEPA'.
            <mapping>-metafield = mmmfd_cust_08.
        ENDCASE.

      ENDLOOP.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

    CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_MAP_DYNPRO' CHANGING ch_mapping = ch_mapping.

  ENDMETHOD.


  METHOD if_ex_me_gui_po_cust~subscribe.
*&---------------------------------------------------------------------*
*& Version/T.P No.: 1.01 (F36K916023)
*& Changed On/By:   2025/04/18 Thomas S.(IBM)
*& Description:     Add DID Requirement: CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_SUBSCRIBE'
*&---------------------------------------------------------------------*

    DATA: ls_subscriber LIKE LINE OF re_subscribers.

    DATA : lv_check_sds LIKE abap_true.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    lv_check_sds = lcl_data=>check_sds( sy-mandt ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*

    IF lv_check_sds EQ abap_true.
      CLEAR re_subscribers[].
      IF im_application = 'PO'.

        CASE im_element.
          WHEN 'HEADER'.
            ls_subscriber-name = subscreen1.
            ls_subscriber-dynpro = '0002'.
            ls_subscriber-program = 'SAPLZSDSMM10'.
            ls_subscriber-struct_name = 'ZSDSMMS020'.
            ls_subscriber-label = TEXT-001.
            ls_subscriber-position = 24.
            ls_subscriber-height = 7.
          WHEN 'ITEM'.                 "Purchase Order Items (Customize Tab)


          WHEN OTHERS.
            EXIT.
        ENDCASE.
        APPEND ls_subscriber TO re_subscribers.
      ENDIF.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

    CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_SUBSCRIBE'
      EXPORTING
        im_application = im_application
        im_element     = im_element
      CHANGING
        ct_subscribers = re_subscribers.

  ENDMETHOD.


  METHOD if_ex_me_gui_po_cust~transport_from_dynp.
*&---------------------------------------------------------------------*
*& Version/T.P No.: 1.01 (F36K916023)
*& Changed On/By:   2025/04/18 Thomas S.(IBM)
*& Description:     Add DID Requirement: CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_FROM_DYNP'
*&---------------------------------------------------------------------*

    DATA : lv_check_sds LIKE abap_true.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    lv_check_sds = lcl_data=>check_sds( sy-mandt ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*

    IF lv_check_sds EQ abap_true.
      CASE im_name.

        WHEN subscreen1.

          CALL FUNCTION 'ZSDS_MEPOBADIEX_POP'
            IMPORTING
              ex_dynp_data = dynp_data_pai.

          IF dynp_data_pai NE dynp_data_pbo.
            re_changed = mmpur_yes.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*


    CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_FROM_DYNP'
      EXPORTING
        im_name             = im_name
        im_fcode            = im_fcode
        i_did_dynp_data_pbo = did_dynp_data_pbo
      IMPORTING
        re_changed          = re_changed
      CHANGING
        c_did_dynp_data_pai = did_dynp_data_pai.


  ENDMETHOD.


  METHOD if_ex_me_gui_po_cust~transport_from_model.
*&---------------------------------------------------------------------*
*& Version/T.P No.: 1.01 (F36K916023)
*& Changed On/By:   2025/04/18 Thomas S.(IBM)
*& Description:     Add DID Requirement: CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_FROM_MODEL'
*&---------------------------------------------------------------------*

    DATA : lv_check_sds LIKE abap_true.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    lv_check_sds = lcl_data=>check_sds( sy-mandt ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*

    IF lv_check_sds EQ abap_true.
      DATA: l_header      TYPE REF TO if_purchase_order_mm,
            ls_mepoheader TYPE mepoheader,
            ls_customer   TYPE zsdsmmt013.
*          ls_customer     TYPE ci_ekkodb.

      CASE im_name.
        WHEN subscreen1.
          mmpur_dynamic_cast l_header im_model.
          CHECK NOT l_header IS INITIAL.

          ls_mepoheader = l_header->get_data( ).
          CALL FUNCTION 'ZSDS_MEPOBADIEX_GET_DATA'
            EXPORTING
              im_ebeln = ls_mepoheader-ebeln
              im_ebelp = '00000'
            IMPORTING
              ex_data  = ls_customer.

          MOVE-CORRESPONDING ls_mepoheader TO dynp_data_pbo.
          MOVE ls_customer-zz_jtepa TO dynp_data_pbo-zz_jtepa.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*


    CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_FROM_MODEL'
      EXPORTING
        im_name              = im_name
        im_model             = im_model
      CHANGING
        cs_did_dynp_data_pbo = did_dynp_data_pbo.


  ENDMETHOD.


  METHOD if_ex_me_gui_po_cust~transport_to_dynp.
*&---------------------------------------------------------------------*
*& Version/T.P No.: 1.01 (F36K916023)
*& Changed On/By:   2025/04/18 Thomas S.(IBM)
*& Description:     Add DID Requirement: CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_TO_DYNP'
*&---------------------------------------------------------------------*

    DATA : lv_check_sds LIKE abap_true.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    lv_check_sds = lcl_data=>check_sds( sy-mandt ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF lv_check_sds EQ abap_true.
      CASE im_name.
        WHEN subscreen1.
          CALL FUNCTION 'ZSDS_MEPOBADIEX_PUSH'
            EXPORTING
              im_dynp_data = dynp_data_pbo.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*


    CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_TO_DYNP'
      EXPORTING
        im_name             = im_name
        i_did_dynp_data_pbo = did_dynp_data_pbo.



  ENDMETHOD.


  METHOD if_ex_me_gui_po_cust~transport_to_model.
*&---------------------------------------------------------------------*
*& Version/T.P No.: 1.01 (F36K916023)
*& Changed On/By:   2025/04/18 Thomas S.(IBM)
*& Description:     Add DID Requirement: CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_TO_MODEL'
*&---------------------------------------------------------------------*

    DATA : lv_check_sds LIKE abap_true.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    lv_check_sds = lcl_data=>check_sds( sy-mandt ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*

    IF lv_check_sds EQ abap_true.
      DATA: l_item        TYPE REF TO if_purchase_order_item_mm,
            l_header      TYPE REF TO if_purchase_order_mm,
            ls_mepoheader TYPE mepoheader,
            ls_customer   TYPE zsdsmmt013.

      BREAK 3sds007.

      CASE im_name.
        WHEN subscreen1.

          mmpur_dynamic_cast l_header im_model.
          CHECK NOT l_header IS INITIAL.

          ls_mepoheader = l_header->get_data( ).

          IF dynp_data_pbo-zz_jtepa NE dynp_data_pai-zz_jtepa.

            CALL FUNCTION 'ZSDS_MEPOBADIEX_GET_DATA'
              EXPORTING
                im_ebeln = ls_mepoheader-ebeln
              IMPORTING
                ex_data  = ls_customer.

            ls_customer-zz_jtepa = dynp_data_pai-zz_jtepa.

            CALL FUNCTION 'ZSDS_MEPOBADIEX_SET_DATA'
              EXPORTING
                im_data = ls_customer.

            MOVE dynp_data_pai-zz_jtepa TO ls_mepoheader-zz_jtepa.

            CALL METHOD l_header->set_data
              EXPORTING
                im_data = ls_mepoheader.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*


    CALL FUNCTION 'Z_DIDMM_MEPOBADIEX_TO_MODEL'
      EXPORTING
        im_name             = im_name
        im_model            = im_model
        i_did_dynp_data_pbo = did_dynp_data_pbo
        i_did_dynp_data_pai = did_dynp_data_pai.


  ENDMETHOD.
ENDCLASS.
