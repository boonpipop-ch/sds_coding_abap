FUNCTION Z_SDSCA_DATE_TO_DDMMMYYYY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     REFERENCE(LANGUAGE) TYPE  SY-LANGU DEFAULT SY-LANGU
*"  EXPORTING
*"     REFERENCE(O_DATE) TYPE  CHAR30
*"----------------------------------------------------------------------

  DATA: GV_YEAR  TYPE NUM4,
        GV_MONTH TYPE NUM2,
        GV_DAY   TYPE NUM2,
        GV_MONTH_TEXT(15) TYPE C.

  CLEAR: GV_YEAR,
         GV_MONTH,
         GV_DAY,
         GV_MONTH_TEXT.

* Re-arrange Date
  MOVE I_DATE+6(2)  TO GV_DAY.
  MOVE I_DATE+4(2)  TO GV_MONTH.
  GV_YEAR = I_DATE+0(4) + 543.

* Get Month Text by Language
  SELECT SINGLE LTX INTO GV_MONTH_TEXT FROM  T247
         WHERE  SPRAS  = LANGUAGE
         AND    MNR    = GV_MONTH.

  CONCATENATE GV_DAY GV_MONTH_TEXT GV_YEAR INTO O_DATE SEPARATED BY SPACE.



ENDFUNCTION.
