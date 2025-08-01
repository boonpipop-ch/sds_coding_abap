class ZCL_IM_SDS_VALIDATION_PO definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .

  class-data GV_CHECK_CHANGE type CHAR1 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_VALIDATION_PO IMPLEMENTATION.


  METHOD IF_EX_ME_PROCESS_PO_CUST~CHECK.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.

    DATA : LS_POHEADER TYPE MEPOHEADER.

    DATA : LC_CHECK TYPE CHAR1.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      CALL METHOD IM_HEADER->GET_DATA
        RECEIVING
          RE_DATA = LS_POHEADER.
      CASE ZCL_IM_SDS_VALIDATION_PO=>GV_CHECK_CHANGE.
        WHEN ABAP_TRUE.
          MESSAGE E001(ZSDSMM01) WITH TEXT-E01.
          CLEAR : ZCL_IM_SDS_VALIDATION_PO=>GV_CHECK_CHANGE.
      ENDCASE.
      LC_CHECK = LCL_DATA=>CHECK_APPROVE_PO( LS_POHEADER-EBELN ).
      IF LC_CHECK EQ ABAP_TRUE.
        MESSAGE E001(ZSDSMM01) WITH TEXT-E02.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  ENDMETHOD.


  METHOD IF_EX_ME_PROCESS_PO_CUST~CLOSE.


  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~INITIALIZE.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~OPEN.
  endmethod.


  METHOD IF_EX_ME_PROCESS_PO_CUST~POST.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.

    DATA : LS_POHEADER TYPE MEPOHEADER.

    DATA : LC_CHECK TYPE CHAR1.

    CONSTANTS : BEGIN OF LC_CON,
                  R TYPE C LENGTH 1 VALUE 'R',
                  G TYPE C LENGTH 1 VALUE 'G',
                END OF LC_CON.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      CALL METHOD IM_HEADER->GET_DATA
        RECEIVING
          RE_DATA = LS_POHEADER.
      IF LS_POHEADER-FRGKE EQ LC_CON-G OR
         LS_POHEADER-FRGKE EQ LC_CON-R.
        LC_CHECK = LCL_DATA=>UNRELESE_PO( LS_POHEADER-EBELN ).
        CLEAR : ZCL_IM_SDS_VALIDATION_PO=>GV_CHECK_CHANGE.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT.
  endmethod.


  METHOD IF_EX_ME_PROCESS_PO_CUST~PROCESS_HEADER.

    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      ZCL_IM_SDS_VALIDATION_PO=>GV_CHECK_CHANGE = LCL_DATA=>CHECK_UNRELEASE_PO( IM_HEADER ).
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

*
  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE.
  endmethod.
ENDCLASS.
