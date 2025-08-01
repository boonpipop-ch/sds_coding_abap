FUNCTION Z_SDSMM_SERIAL_GET_CUST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_SERNR) TYPE  OBJK-SERNR OPTIONAL
*"     VALUE(I_MATNR) TYPE  OBJK-MATNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  ZSDSMMS001
*"----------------------------------------------------------------------

  DATA LO TYPE REF TO LCL_DATA.

  GV_MATNR = I_MATNR.
  GV_SERNR = I_SERNR.

  IF LO IS INITIAL.
    CREATE OBJECT LO.
  ENDIF.

  LO->GET_DATA( ).
  LO->GET_RESULT( CHANGING C_DATA = E_RESULT ).

ENDFUNCTION.
