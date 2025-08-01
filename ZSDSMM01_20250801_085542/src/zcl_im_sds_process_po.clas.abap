class ZCL_IM_SDS_PROCESS_PO definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_PROCESS_PO IMPLEMENTATION.


  METHOD IF_EX_ME_PROCESS_PO_CUST~CHECK.

    DATA: LS_MEPOHEADER TYPE MEPOHEADER.

    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.

    CONSTANTS : BEGIN OF LC_CON,
*                  DIL TYPE LFA1-LIFNR VALUE '0000001001',
                  PARAM TYPE C LENGTH 5  VALUE 'JTEPA',
                  REPID TYPE C LENGTH 11 VALUE 'CHECK_JTEPA',
                END OF LC_CON.

    DATA : LR_JTEPPA TYPE RANGE OF LFA1-LIFNR.

    DATA : LV_JTEPA TYPE LFA1-LIFNR.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*

    IF LV_CHECK_SDS EQ ABAP_TRUE.

      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID = LC_CON-REPID
                                                    I_PARAM = LC_CON-PARAM
                                           CHANGING CR_RETURN = LR_JTEPPA ).
      IF LR_JTEPPA[] IS NOT INITIAL.
        CALL METHOD IM_HEADER->GET_DATA
          RECEIVING
            RE_DATA = LS_MEPOHEADER.

        LV_JTEPA = LCL_DATA=>GET_VENDOR_FOR_CHECK_JTEPA( LS_MEPOHEADER-LIFNR ).

        IF LS_MEPOHEADER-ZZ_JTEPA IS INITIAL AND
           LV_JTEPA IN LR_JTEPPA[]. " DIL
          MESSAGE E001(ZSDSMM01) WITH TEXT-E01.
        ENDIF.
      ENDIF.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~CLOSE.
  endmethod.


  METHOD if_ex_me_process_po_cust~fieldselection_header.
    DATA: l_changeable          TYPE mmpur_bool.
    FIELD-SYMBOLS:<fs_f> LIKE LINE OF ch_fieldselection.

    l_changeable = im_header->is_changeable( ).

    READ TABLE ch_fieldselection ASSIGNING <fs_f> WITH TABLE KEY metafield = mmmfd_cust_08.

    IF sy-subrc = 0.
      IF l_changeable = 'X'.
        <fs_f>-fieldstatus = '.'.
      ELSE.
        <fs_f>-fieldstatus = '*'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  METHOD IF_EX_ME_PROCESS_PO_CUST~INITIALIZE.

  ENDMETHOD.


  METHOD IF_EX_ME_PROCESS_PO_CUST~OPEN.

  ENDMETHOD.


  METHOD IF_EX_ME_PROCESS_PO_CUST~POST.




  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT.
  endmethod.


  METHOD IF_EX_ME_PROCESS_PO_CUST~PROCESS_HEADER.

  DATA: LS_MEPOHEADER TYPE MEPOHEADER.
  DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*  DATA LS_EKKO TYPE UEKKO.
  DATA LS_EBAN TYPE EBAN.
  DATA LT_EBAN TYPE TABLE OF EBAN.
  DATA: GT_CONN    TYPE TABLE OF TOAV0,
        GT_CONN_AV TYPE TABLE OF TOAV0,
        GS_CONN    TYPE TOAV0,
        GS_TOA01   TYPE TOA01,
        LW_BANFN   TYPE BANFN.

  LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).

  IF LV_CHECK_SDS EQ ABAP_TRUE.
    CALL METHOD IM_HEADER->GET_DATA
      RECEIVING
        RE_DATA = LS_MEPOHEADER.

    ZCL_IM_SDS_ME_GUI_PO_CUST=>PO_NO = LS_MEPOHEADER-EBELN.

    SELECT SINGLE ZZ_JTEPA FROM ZSDSMMT013
      INTO @LS_MEPOHEADER-ZZ_JTEPA
      WHERE EBELN = @LS_MEPOHEADER-EBELN.
*        AND EBELP = '00000'.

    IF SY-SUBRC EQ 0 AND LS_MEPOHEADER-ZZ_JTEPA IS NOT INITIAL.

      CALL METHOD IM_HEADER->SET_DATA
        EXPORTING
          IM_DATA = LS_MEPOHEADER.

    ENDIF.
  ENDIF.

  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE.
  endmethod.
ENDCLASS.
