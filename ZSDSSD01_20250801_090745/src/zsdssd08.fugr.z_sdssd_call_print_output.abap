FUNCTION Z_SDSSD_CALL_PRINT_OUTPUT.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_OUTPUT) TYPE  NA_KSCHL OPTIONAL
*"     VALUE(I_FORM) TYPE  NA_FNAME OPTIONAL
*"     VALUE(I_DOC) TYPE  CHAR10 OPTIONAL
*"     VALUE(I_CUS_CODE) TYPE  KUNWE OPTIONAL
*"     VALUE(I_ADDRESS) TYPE  ADRNR OPTIONAL
*"     VALUE(I_MHA) TYPE  CHAR3 OPTIONAL
*"----------------------------------------------------------------------

  DATA : LV_RETCODE     TYPE SY-SUBRC,
         LS_NAST        TYPE NAST,
         LV_FORM_NAME   TYPE TNAPR-SFORM,
         LV_PROC_SCREEN TYPE C,
         LV_NO_DIALOG   TYPE C,
         LV_MHA         TYPE CHAR3.

  DATA : LV_CHK_DATA TYPE C.
  IF I_DOC IS NOT INITIAL.
    DO 10 TIMES.
      SELECT COUNT( * )
        FROM VBRK
        WHERE VBELN EQ I_DOC.
      IF SY-SUBRC EQ 0.
        LV_CHK_DATA = ABAP_TRUE.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
        CLEAR : LV_CHK_DATA.
      ENDIF.
    ENDDO.

    LV_MHA = I_MHA.

    IF LV_CHK_DATA EQ ABAP_TRUE.
      LV_NO_DIALOG       = ABAP_TRUE.
      LS_NAST-KAPPL      = 'V3'.
      LS_NAST-OBJKY      = I_DOC.
      LS_NAST-KSCHL      = I_OUTPUT.
      LS_NAST-SPRAS      = '2'.
      LS_NAST-PARNR      = I_CUS_CODE.
      LS_NAST-PARVW      = 'RE'.
      LS_NAST-ERDAT      = SY-DATUM.
      LS_NAST-ERUHR      = SY-UZEIT.
      LS_NAST-ADRNR      = I_ADDRESS.
      LS_NAST-NACHA      = '1'.
      LS_NAST-ANZAL      = '1'.
      LS_NAST-VSZTP      = '3'.
      LS_NAST-MANUE      = 'X'.
      LS_NAST-DATVR      = SY-DATUM.
      LS_NAST-UHRVR      = SY-UZEIT.
      LS_NAST-USNAM      = SY-UNAME.
      LS_NAST-VSTAT      = '1'.
      LS_NAST-LDEST      = 'SDPF'.
      LS_NAST-DIMME      = SPACE.
      LS_NAST-DELET      = 'X'.
      LS_NAST-SNDBC      = '1'.
      LS_NAST-NAUTO      = 'X'.
      LS_NAST-TDRECEIVER = SY-UNAME.
      LS_NAST-TDARMOD    = '1'.
      LS_NAST-OBJTYPE    = 'VBRK'.
      LV_FORM_NAME       = I_FORM.

      PERFORM F_PRINT(ZSDSSDR0420) USING LV_RETCODE
                                         LS_NAST
                                         LV_FORM_NAME
                                         LV_PROC_SCREEN
                                         LV_NO_DIALOG
                                         LV_MHA.
      IF LV_RETCODE EQ 0.
        UPDATE ZSDSSDT025 SET PETAX  = ABAP_TRUE
                        WHERE INVNO EQ I_DOC.
        COMMIT WORK AND WAIT.
      ENDIF.
    ELSE.
      UPDATE ZSDSSDT025 SET PETAX  = 'N'
                      WHERE INVNO EQ I_DOC.
      COMMIT WORK AND WAIT.
    ENDIF.
  ELSE.
    UPDATE ZSDSSDT025 SET PETAX  = 'E'
                      WHERE INVNO EQ I_DOC.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.
