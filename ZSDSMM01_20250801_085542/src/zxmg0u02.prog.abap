*&---------------------------------------------------------------------*
*& Include          ZXMG0U02
*&---------------------------------------------------------------------*

DATA: e_check TYPE c.
"WMARA-MTART
"WMARC-WERKS
SELECT SINGLE * FROM t001k INTO @DATA(ls_t001k)
  WHERE bwkey = @wmarc-werks.

CHECK sy-subrc = 0.

" 2.3
SELECT * FROM zrfmmc015 INTO TABLE @DATA(lt_zrfmmc015)
  WHERE bukrs = @ls_t001k-bukrs
    AND mtart = @wmara-mtart.

CHECK sy-subrc = 0.

" 2.4 "TBC
SELECT * FROM zrfmmc016 INTO TABLE @DATA(lt_zrfmmc016)
  WHERE bukrs = @ls_t001k-bukrs.

"3.validation
"3.1
e_check = '1'.
LOOP AT lt_zrfmmc015 INTO DATA(wa_zrfmmc015_d1) WHERE z_matnr = wmara-matnr+0(1).
  e_check = '0'.  "Pass
  EXIT.
ENDLOOP.

IF e_check <> '0'.
  MESSAGE 'Material Code did not match with Material Type' TYPE 'E' DISPLAY LIKE 'E'.
  "4.Return pop up  "Material Code did not match with Material Type", and Material Master could not be saved
ENDIF.

CHECK e_check = '0'.

"3.2
e_check = '1'.
LOOP AT lt_zrfmmc016 INTO DATA(wa_zrfmmc016_d1) WHERE z_bukrs = wmara-matnr+1(1).
  e_check = '0'.  "Pass
  EXIT.
ENDLOOP.

IF e_check <> '0'.
  MESSAGE 'Material Code did not match with Material Type' TYPE 'E' DISPLAY LIKE 'E'.
  "4.Return pop up  "Material Code did not match with Material Type", and Material Master could not be saved
ENDIF.
