FUNCTION Z_SDSSD_GET_CM.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VBELN) TYPE  VBELN_VF OPTIONAL
*"     VALUE(I_TYPE) TYPE  VBTYPL OPTIONAL
*"  EXPORTING
*"     VALUE(E_HEADER) TYPE  ZSDSSDS003
*"     VALUE(E_ADDRESS) TYPE  ZSDSSDS120
*"  CHANGING
*"     VALUE(CT_ITEM) TYPE  ZSDSSDS004_TT OPTIONAL
*"     VALUE(CT_RETURN) TYPE  BAPIRETTAB OPTIONAL
*"     VALUE(CT_ITEM_SERIAL_NO) TYPE  ZSDSSDS005_TT OPTIONAL
*"----------------------------------------------------------------------
  REFRESH: CT_RETURN, CT_ITEM, CT_ITEM_SERIAL_NO.
  CLEAR: E_HEADER.

  CLEAR GW_VBRK.

  CASE I_TYPE.
    WHEN GC_DOCTYP-INV.
      SELECT SINGLE * INTO GW_VBRK
             FROM VBRK
             WHERE VBELN = I_VBELN.

      PERFORM VALIDATE_INVOICE TABLES CT_RETURN
                                USING I_VBELN
                             CHANGING GV_SUBRC.
      CHECK GV_SUBRC = 0.
      PERFORM GET_INVOICE TABLES CT_ITEM
                                 CT_ITEM_SERIAL_NO
                        CHANGING E_HEADER
                                 E_ADDRESS.


      SORT CT_ITEM_SERIAL_NO BY ITM_NUMBER SERIALNO.
      DELETE ADJACENT DUPLICATES FROM CT_ITEM_SERIAL_NO
      COMPARING ITM_NUMBER SERIALNO.
    WHEN GC_DOCTYP-DEL.
      SELECT SINGLE * INTO GW_LIKP
        FROM LIKP
        WHERE VBELN = I_VBELN.
      IF SY-SUBRC = 0.
        PERFORM GET_DELIVERY CHANGING CT_ITEM
                                      CT_ITEM_SERIAL_NO
                                      E_HEADER
                                      E_ADDRESS.


      ELSE.
        GV_MSG = TEXT-E02.
        REPLACE '&1' WITH I_VBELN INTO GV_MSG.
        CT_RETURN[] = VALUE #( ( TYPE = 'E'
                                  MESSAGE = GV_MSG ) ).
      ENDIF.
    WHEN OTHERS.
      CT_RETURN[] = VALUE #( ( TYPE = 'E'
                                MESSAGE = TEXT-E01 ) ).

  ENDCASE.


ENDFUNCTION.
