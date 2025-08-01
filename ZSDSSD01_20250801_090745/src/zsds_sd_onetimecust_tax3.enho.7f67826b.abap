"Name: \PR:SAPMV45A\FO:USEREXIT_SAVE_DOCUMENT_PREPARE\SE:BEGIN\EI
ENHANCEMENT 0 ZSDS_SD_ONETIMECUST_TAX3.
*&---------------------------------------------------------------------*
*& Version/T.P No.: 1.0 (F31K902762)
*& Changed On/By:   27.05.2024 Zulkiff B.(EVIDEN)
*& Description:     Update One-Time Customer : Tax3
*&---------------------------------------------------------------------*
      DATA: LS_VBPA3KOM TYPE VBPA3KOM,
            LT_VBPA3KOM TYPE STANDARD TABLE OF VBPA3KOM.

      IF T180-TRTYP = 'H' OR T180-TRTYP = 'V'. "Create/Change
        "Import from Class ZCL_SDSSD_SO_FROM_SF_CREATION Method: SALES_ORDER_CREATE
        IMPORT LT_VBPA3KOM TO LT_VBPA3KOM FROM  MEMORY ID 'ONETIMECUST_TAX3'.
        IF LT_VBPA3KOM IS NOT INITIAL.
          LOOP AT XVBPA INTO DATA(LS_VBPA).
            DATA(LV_TABIX) = SY-TABIX.
            READ TABLE LT_VBPA3KOM INTO LS_VBPA3KOM WITH KEY PARVW = LS_VBPA-PARVW.
            IF SY-SUBRC = 0.
              LS_VBPA-STCD3 = LS_VBPA3KOM-STCD3.
              MODIFY XVBPA FROM LS_VBPA INDEX LV_TABIX.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
ENDENHANCEMENT.
