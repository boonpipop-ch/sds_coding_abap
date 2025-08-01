FUNCTION ZSDS_MEPOBADIEX_OPEN.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_EBELN) TYPE  EBELN
*"--------------------------------------------------------------------

* read customer data from database

  CHECK NOT im_ebeln IS INITIAL.

  SELECT * FROM zsdsmmt013 INTO TABLE gt_persistent_data
                                 WHERE ebeln = im_ebeln.

  gt_data = gt_persistent_data.

ENDFUNCTION.
