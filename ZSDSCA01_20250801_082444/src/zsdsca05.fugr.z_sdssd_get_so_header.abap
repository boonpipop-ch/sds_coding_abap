FUNCTION Z_SDSSD_GET_SO_HEADER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VBELN) TYPE  VBELN_VA OPTIONAL
*"  EXPORTING
*"     VALUE(ES_HEADER) TYPE  ZSDSSDS129
*"----------------------------------------------------------------------

  CLEAR: ES_HEADER.

  SELECT SINGLE
    VBELN,
    BSTNK
    FROM VBAK
    INTO @DATA(LS_VBAK)
    WHERE VBELN = @IV_VBELN.
  IF SY-SUBRC = 0.
    SELECT
      VBELN,
      POSNR,
      PS_PSP_PNR
      FROM VBAP
    INTO @DATA(LS_VBAP)
    UP TO 1 ROWS
    WHERE VBELN = @IV_VBELN
      AND PS_PSP_PNR <> @SPACE
      ORDER BY POSNR.
    ENDSELECT.
    IF SY-SUBRC = 0.
      SELECT SINGLE
        PSPNR,
        PSPHI
      INTO @DATA(LS_PRPS)
      FROM PRPS
      WHERE PSPNR = @LS_VBAP-PS_PSP_PNR.
*      IF LS_PRPS-PSPHI IS NOT INITIAL.
*        SELECT SINGLE
*          PSPNR,
*          PSPHI
*        INTO @DATA(LS_WBS_LV1)
*        FROM PRPS
*        WHERE PSPNR = @LS_PRPS-PSPHI
*          AND STUFE = 1.
*      ENDIF.
    ENDIF.
  ENDIF.

  CHECK LS_VBAK IS NOT INITIAL.

  "Move to export parameter
  MOVE: LS_VBAK-VBELN TO ES_HEADER-SALESORDER,
        LS_VBAK-BSTNK TO ES_HEADER-PURCHASEORDER,
        LS_PRPS-PSPHI TO ES_HEADER-PSPHI.

  CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
    EXPORTING
      INPUT  = LS_PRPS-PSPHI
    IMPORTING
      OUTPUT = ES_HEADER-WBS.

ENDFUNCTION.
