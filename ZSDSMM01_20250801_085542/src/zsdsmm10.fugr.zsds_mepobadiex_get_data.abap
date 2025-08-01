FUNCTION ZSDS_MEPOBADIEX_GET_DATA.
*"----------------------------------------------------------------------
*"*"LOCAL INTERFACE:
*"  IMPORTING
*"     REFERENCE(IM_EBELN) TYPE  EBELN
*"     VALUE(IM_EBELP) TYPE  EBELP OPTIONAL
*"  EXPORTING
*"     VALUE(EX_DATA) TYPE  ZSDSMMT013
*"----------------------------------------------------------------------

  CLEAR EX_DATA.

*  CHECK NOT IM_EBELP IS INITIAL.

  READ TABLE GT_DATA INTO EX_DATA WITH TABLE KEY MANDT = SY-MANDT
                                                 EBELN = IM_EBELN
                                                 EBELP = IM_EBELP.
  IF NOT SY-SUBRC IS INITIAL.
    SELECT SINGLE ZZ_JTEPA FROM EKKO INTO @DATA(LV_ZZ_JTEPA)
    WHERE EBELN EQ @IM_EBELN.
    IF SY-SUBRC = 0.
      EX_DATA-ZZ_JTEPA = LV_ZZ_JTEPA.
    ELSE.
      EX_DATA-ZZ_JTEPA = 'N'.
    ENDIF.
    EX_DATA-MANDT = SY-MANDT.
    EX_DATA-EBELN = IM_EBELN.
    EX_DATA-EBELP = IM_EBELP.
    INSERT EX_DATA INTO TABLE GT_DATA.
  ENDIF.

ENDFUNCTION.
