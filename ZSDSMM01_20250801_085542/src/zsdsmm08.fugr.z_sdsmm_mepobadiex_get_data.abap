FUNCTION Z_SDSMM_MEPOBADIEX_GET_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_EBELN) TYPE  EBELN
*"     REFERENCE(IM_EBELP) TYPE  EBELP
*"  EXPORTING
*"     VALUE(EX_DATA) TYPE  ZSDSMMT013
*"----------------------------------------------------------------------

  CLEAR ex_data.

  CHECK NOT im_ebelp IS INITIAL.

  READ TABLE gt_data INTO ex_data WITH TABLE KEY mandt = sy-mandt
                                                 ebeln = im_ebeln
                                                 ebelp = im_ebelp.
  IF NOT sy-subrc IS INITIAL.
    ex_data-mandt = sy-mandt.
    ex_data-ebeln = im_ebeln.
    ex_data-ebelp = im_ebelp.
    SELECT SINGLE ZZ_ETD FROM ekpo INTO @DATA(lv_zz_etd)
    WHERE ebeln = @im_ebeln AND ebelp = @im_ebelp.
    IF sy-subrc = 0.
       ex_data-zz_etd = lv_zz_etd.
    ENDIF.
    INSERT ex_data INTO TABLE gt_data.
  ENDIF.

ENDFUNCTION.
