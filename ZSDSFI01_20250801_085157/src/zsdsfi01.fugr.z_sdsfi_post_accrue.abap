FUNCTION Z_SDSFI_POST_ACCRUE.
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

  DATA : LS_HEADER LIKE I_HEADER.

  LS_HEADER = I_HEADER.

  PERFORM F_POST_FI TABLES IT_DETAIL
                     USING I_DOCTYPE
                           I_K2DOC
                           I_COMCODE
                  CHANGING LS_HEADER
                           E_OUTPUT
                           E_MESSAGE.

ENDFUNCTION.
