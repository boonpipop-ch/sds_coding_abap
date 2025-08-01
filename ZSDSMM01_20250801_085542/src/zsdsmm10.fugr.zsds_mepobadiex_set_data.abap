FUNCTION ZSDS_MEPOBADIEX_SET_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_DATA) TYPE  ZSDSMMT013
*"     REFERENCE(IM_PHYSICAL_DELETE_REQUEST) TYPE  MMPUR_BOOL OPTIONAL
*"----------------------------------------------------------------------

* update customers data

  DATA: ls_data LIKE LINE OF gt_data.

  FIELD-SYMBOLS: <data> LIKE LINE OF gt_data.

*  CHECK NOT im_data-ebelp IS INITIAL.

  IF NOT im_physical_delete_request IS INITIAL.
* delete a line from gt_data
    DELETE TABLE gt_data WITH TABLE KEY mandt = sy-mandt
                                        ebeln = im_data-ebeln
                                        ebelp = im_data-ebelp.
  ELSE.
* update customer data
    READ TABLE gt_data ASSIGNING <data> WITH TABLE KEY
                                        mandt = sy-mandt
                                        ebeln = im_data-ebeln
                                        ebelp = im_data-ebelp.
    IF sy-subrc IS INITIAL.
* update existing data
      <data>-zz_jtepa = im_data-zz_jtepa.
    ELSE.
* make a new entry into the data table
      ls_data = im_data.
      ls_data-mandt = sy-mandt.
      INSERT ls_data INTO TABLE gt_data.
    ENDIF.
  ENDIF.

ENDFUNCTION.
