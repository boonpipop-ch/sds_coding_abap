FUNCTION Z_SDSFI_PAYMENT_VOUCHER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_COMCODE) TYPE  CHAR4 OPTIONAL
*"     VALUE(I_DOCTYPE) TYPE  CHAR10 OPTIONAL
*"     VALUE(I_K2DOC) TYPE  CHAR50 OPTIONAL
*"     VALUE(I_HEADER) TYPE  ZSDSFIS013 OPTIONAL
*"     VALUE(IT_DETAIL) TYPE  ZSDSFIS014_TT OPTIONAL
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  BELNR_D
*"     VALUE(E_MESSAGE) TYPE  CHAR255
*"----------------------------------------------------------------------

  DATA : BEGIN OF LS_BSIK,
           BELNR TYPE BSIK_VIEW-BELNR,
*   lifnr TYPE bsik-lifnr,
*   bldat TYPE bsik-bldat,
*   xblnr TYPE bsik-xblnr,
         END OF LS_BSIK.
  DATA : LT_BSIK LIKE TABLE OF LS_BSIK.

  DATA : LS_HEADER LIKE I_HEADER.

  LS_HEADER = I_HEADER.

  IF I_DOCTYPE NE GC_CON-MIRO.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_HEADER-LIFNR
      IMPORTING
        OUTPUT = I_HEADER-LIFNR.

    IF I_HEADER-LIFNR EQ GC_CON-DIT.
      E_MESSAGE = TEXT-E01.
    ELSE.
      SELECT SINGLE BELNR
        FROM BSIK_VIEW
        INTO @LS_BSIK
        WHERE LIFNR EQ @I_HEADER-LIFNR
          AND XBLNR EQ @I_HEADER-XBLNR.
      IF SY-SUBRC NE 0.
        PERFORM F_POST_FI_PAYMETN_VOUCHER TABLES IT_DETAIL
                                           USING I_DOCTYPE
                                                 I_COMCODE
                                        CHANGING LS_HEADER
                                                 E_OUTPUT
                                                 E_MESSAGE.
      ELSE.
        CONCATENATE TEXT-E02 LS_BSIK-BELNR INTO E_MESSAGE SEPARATED BY SPACE.
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM F_POST_MIRO TABLES IT_DETAIL
                         USING I_COMCODE
                      CHANGING LS_HEADER
                               E_OUTPUT
                               E_MESSAGE.
  ENDIF.


ENDFUNCTION.
