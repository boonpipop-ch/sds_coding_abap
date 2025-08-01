FUNCTION Z_SDSFI_TRANSFER_FIX_ASSET.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_INPUT) TYPE  ZSDSFIS087_TT OPTIONAL
*"  EXPORTING
*"     VALUE(ET_MESSAGE) TYPE  ZSDSFIS088_TT
*"----------------------------------------------------------------------
  DATA LS_INPUT TYPE ZSDSFIS087.

  LOOP AT IT_INPUT INTO LS_INPUT.
    PERFORM F_CHANGE_ASSET USING LS_INPUT
                        CHANGING ET_MESSAGE.
    CLEAR : LS_INPUT.
  ENDLOOP.
ENDFUNCTION.
