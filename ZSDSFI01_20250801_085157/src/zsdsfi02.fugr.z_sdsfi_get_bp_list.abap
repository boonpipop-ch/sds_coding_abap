FUNCTION Z_SDSFI_GET_BP_LIST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_PARTNER) TYPE  BU_PARTNER
*"     REFERENCE(IV_TYPE) TYPE  BU_TYPE
*"  EXPORTING
*"     VALUE(EX_LIST) TYPE  ZSDSFIS114_TT
*"----------------------------------------------------------------------

  "Get Data for generate list
  SELECT
    partner,
    type
    FROM but000
    INTO TABLE @DATA(lt_bp_info)
   WHERE partner = @iv_partner
     AND type = @iv_type.
  IF sy-subrc = 0.

    "Customer Address
*    SELECT
*      kunnr









  ENDIF.










ENDFUNCTION.
