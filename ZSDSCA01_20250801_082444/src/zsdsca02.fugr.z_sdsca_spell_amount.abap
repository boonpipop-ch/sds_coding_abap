FUNCTION Z_SDSCA_SPELL_AMOUNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(AMOUNT) DEFAULT 0
*"     VALUE(CURRENCY) TYPE  SY-WAERS DEFAULT SPACE
*"     VALUE(LANGUAGE) TYPE  SY-LANGU DEFAULT SY-LANGU
*"  EXPORTING
*"     REFERENCE(SPELL_WORD) TYPE  CHAR255
*"  EXCEPTIONS
*"      NOT_FOUND
*"      TOO_LARGE
*"----------------------------------------------------------------------

  DATA : LT_SPELL TYPE SPELL,
         V_KTEXT  LIKE TCURT-KTEXT.

  CONSTANTS : C_ENGLISH            LIKE SY-LANGU VALUE 'E',
              C_THAI               LIKE SY-LANGU VALUE '2',
              C_ONLY_E(10)         TYPE C VALUE 'BAHT ONLY',
              C_ONLY_T(10)         TYPE C VALUE 'บาทถ้วน',
              C_DECIMAL_E1(10)     TYPE C VALUE 'BAHT AND',
              C_DECIMAL_E2(10)     TYPE C VALUE 'SATANG' ,      "  '/100' ,
              C_DECIMAL_E_CENT(10) TYPE C VALUE 'CENT' ,      " IMS 300000237
              C_DECIMAL_T1(5)      TYPE C VALUE 'บาท',
              C_DECIMAL_T2(10)     TYPE C VALUE 'สตางค์'.

*** change by IMS 300000237-2 - Get curency word setting from GEN_C
  CONSTANTS: C_REPID              TYPE  SY-REPID  VALUE 'ZSD_SPELL_AMOUNT',
             C_DECIMAL_T_CENT(10) TYPE C VALUE 'เซน'.
  DATA: V_DEC_TXT TYPE ZSDSDE_VALUE VALUE 'CENT',
        V_CONST   TYPE ZSDSDE_CONST VALUE 'DEC_USD_E'.
  CONCATENATE 'DEC' CURRENCY LANGUAGE INTO V_CONST SEPARATED BY '_'.
  SELECT SINGLE VALUE
      FROM  ZSDSCAC002  INTO V_DEC_TXT
      WHERE REPID = C_REPID AND CONST = V_CONST.
  IF SY-SUBRC NE 0. " If not found Gen_C setup, will use default value.
    CASE V_CONST.
      WHEN 'DEC_USD_E'.
        V_DEC_TXT = C_DECIMAL_E_CENT.
      WHEN 'DEC_USD_2'.
        V_DEC_TXT = C_DECIMAL_T_CENT.
      WHEN 'DEC_THB_E'.
        V_DEC_TXT = C_DECIMAL_E2.
      WHEN 'DEC_THB_2'.
        V_DEC_TXT = C_DECIMAL_T2.
    ENDCASE.
  ENDIF.
*** End of change by IMS 300000237-2

  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      AMOUNT    = AMOUNT
      CURRENCY  = CURRENCY
*     FILLER    = FILLER
      LANGUAGE  = LANGUAGE
    IMPORTING
      IN_WORDS  = LT_SPELL
    EXCEPTIONS
      NOT_FOUND = 1
      TOO_LARGE = 2
      OTHERS    = 3.

  IF SY-SUBRC EQ 0 .
    IF CURRENCY = 'THB' .
      IF LT_SPELL-DECIMAL IS INITIAL  .
        CASE LANGUAGE  .
          WHEN C_ENGLISH .
            CONCATENATE LT_SPELL-WORD C_ONLY_E INTO SPELL_WORD SEPARATED BY SPACE.
          WHEN C_THAI.
            CONCATENATE LT_SPELL-WORD C_ONLY_T INTO SPELL_WORD .
            CONDENSE SPELL_WORD NO-GAPS. "INS AL080109 T41K902731
          WHEN OTHERS .
            MOVE LT_SPELL-WORD TO SPELL_WORD.
        ENDCASE .
      ELSE .
        CASE LANGUAGE.
          WHEN C_ENGLISH  .
*** change by IMS 300000237-2 - Get curency word setting from GEN_C
**            CONCATENATE lt_spell-word c_decimal_e1 lt_spell-decimal(lt_spell-currdec)
*            CONCATENATE lt_spell-word c_decimal_e1 lt_spell-decword c_decimal_e2
*                        INTO spell_word SEPARATED BY space.
**            CONCATENATE spell_word c_decimal_e2 INTO spell_word  .
            CONCATENATE LT_SPELL-WORD C_DECIMAL_E1 LT_SPELL-DECWORD V_DEC_TXT
                        INTO SPELL_WORD SEPARATED BY SPACE.
*** End of change by IMS 300000237-2

          WHEN C_THAI .
*** change by IMS 300000237-2 - Get curency word setting from GEN_C
*            CONCATENATE lt_spell-word c_decimal_t1 lt_spell-decword c_decimal_t2
*                        INTO spell_word.
            CONCATENATE LT_SPELL-WORD C_DECIMAL_T1 LT_SPELL-DECWORD V_DEC_TXT
                        INTO SPELL_WORD.
*** End of change by IMS 300000237-2
            CONDENSE SPELL_WORD NO-GAPS.

          WHEN OTHERS .
            CONCATENATE LT_SPELL-WORD LT_SPELL-DECWORD INTO SPELL_WORD SEPARATED BY SPACE.
        ENDCASE.

      ENDIF.
    ELSE.
      SELECT SINGLE KTEXT INTO V_KTEXT
       FROM TCURT
       WHERE SPRAS = LANGUAGE
        AND  WAERS = CURRENCY .
      IF SY-SUBRC EQ  0 .

        TRANSLATE V_KTEXT TO UPPER CASE.

        IF LT_SPELL-DECIMAL IS INITIAL  .
          CASE LANGUAGE  .
            WHEN C_ENGLISH .
              CONCATENATE LT_SPELL-WORD V_KTEXT 'ONLY' INTO SPELL_WORD SEPARATED BY SPACE.
            WHEN C_THAI.
              CONCATENATE LT_SPELL-WORD V_KTEXT 'ถ้วน' INTO SPELL_WORD .
            WHEN OTHERS .
              CONCATENATE LT_SPELL-WORD V_KTEXT  INTO SPELL_WORD .
          ENDCASE .
        ELSE .
          CASE LANGUAGE.
            WHEN C_ENGLISH  .
*** change by IMS 300000237  - change curency word
*              CONCATENATE lt_spell-word v_ktext 'AND' lt_spell-decimal(lt_spell-currdec)
*                          INTO spell_word SEPARATED BY space.
*              CONCATENATE spell_word c_decimal_e2 INTO spell_word  .
              CONCATENATE LT_SPELL-WORD V_KTEXT 'AND' LT_SPELL-DECWORD
                         INTO SPELL_WORD SEPARATED BY SPACE.
*              CONCATENATE spell_word c_decimal_e_cent INTO spell_word SEPARATED BY space . "300000237-2
*** end of change by IMS 300000237
*** change by IMS 300000237-2 - Get curency word setting from GEN_C
              CONCATENATE SPELL_WORD V_DEC_TXT INTO SPELL_WORD SEPARATED BY SPACE .
*** End of change by IMS 300000237-2

            WHEN C_THAI .
*              CONCATENATE lt_spell-word v_ktext lt_spell-decword c_decimal_t2
              CONCATENATE LT_SPELL-WORD V_KTEXT LT_SPELL-DECWORD
                         INTO SPELL_WORD.
*** change by IMS 300000237-2 - Get curency word setting from GEN_C
              CONCATENATE SPELL_WORD V_DEC_TXT INTO SPELL_WORD SEPARATED BY SPACE .
*** End of change by IMS 300000237-2
              CONDENSE SPELL_WORD NO-GAPS.

            WHEN OTHERS .
              CONCATENATE LT_SPELL-WORD V_KTEXT LT_SPELL-DECWORD INTO SPELL_WORD SEPARATED BY SPACE.
          ENDCASE.

        ENDIF.

      ENDIF.

    ENDIF.

    IF LANGUAGE EQ C_THAI.
      REPLACE ALL OCCURRENCES OF PCRE 'SAEN' IN  SPELL_WORD WITH 'แสน'.
    ENDIF.

  ELSEIF SY-SUBRC EQ 1 .
    RAISE NOT_FOUND .

  ELSEIF SY-SUBRC EQ 2 .
    RAISE TOO_LARGE .
  ENDIF.

ENDFUNCTION.
