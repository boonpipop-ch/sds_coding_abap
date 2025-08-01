*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0380
*  Creation Date      : 11.07.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is a subroutine pool contains routines used
*                       in cheque form ZSDSFI002 and ZSDSFI003
*                       (SAP Script)
*  Purpose            : Subroutines for Cheque form
*  Copied from        : ZS_AP_SUBROUTINES
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
PROGRAM ZSDSFIR0380.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: TT_ITCSY  TYPE  STANDARD TABLE OF ITCSY.
TYPES: TT_HKONT_RANGE TYPE RANGE OF BSEG-HKONT.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE  TYPE  CHAR1     VALUE 'X'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA:
  GR_HKONT_OT  TYPE  TT_HKONT_RANGE                            ##NEEDED,
  GR_HKONT_TAX TYPE  TT_HKONT_RANGE                            ##NEEDED,
  GR_HKONT_WHT TYPE  TT_HKONT_RANGE                            ##NEEDED.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  Form F_CHECK_VENDOR
*----------------------------------------------------------------------*
*  F_CHECK_VENDOR
*----------------------------------------------------------------------*
FORM F_CHECK_VENDOR TABLES UT_IN_TAB  TYPE TT_ITCSY             ##CALLED
                           CT_OUT_TAB TYPE TT_ITCSY.

  DATA:
    LF_LIFNR TYPE LIFNR,
    LF_KTOKK TYPE KTOKK.


  CLEAR: LF_LIFNR,
         LF_KTOKK.

  READ TABLE UT_IN_TAB ASSIGNING FIELD-SYMBOL(<L_IN_TAB>)
                       WITH KEY NAME = 'REGUH-LIFNR'.
  IF SY-SUBRC EQ 0.
    LF_LIFNR = <L_IN_TAB>-VALUE.
  ENDIF.

  IF LF_LIFNR NE SPACE.
    SELECT SINGLE KTOKK
      INTO LF_KTOKK
      FROM LFA1
     WHERE LIFNR EQ LF_LIFNR.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING FIELD-SYMBOL(<L_OUT_TAB>)
                        WITH KEY NAME = 'VENDOR'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_KTOKK.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_CHECK_VENDOR_NAME
*----------------------------------------------------------------------*
*  F_CHECK_VENDOR_NAME
*----------------------------------------------------------------------*
FORM F_CHECK_VENDOR_NAME TABLES UT_IN_TAB TYPE TT_ITCSY      ##CALLED
                                CT_OUT_TAB TYPE TT_ITCSY.

  DATA:
    LF_LIFNR TYPE LFA1-LIFNR,
    LF_KTOKK TYPE KTOKK,
    LF_ADRNR TYPE LFA1-ADRNR,
    LF_NAME1 TYPE ADRC-NAME1,
    LF_NAME2 TYPE ADRC-NAME2,
    LF_NAME3 TYPE ADRC-NAME3,
    LF_VBLNR TYPE REGUH-VBLNR,
    LF_EMPFG TYPE REGUH-EMPFG.


  CLEAR: LF_LIFNR,
         LF_KTOKK,
         LF_VBLNR,
         LF_EMPFG.

  READ TABLE UT_IN_TAB ASSIGNING FIELD-SYMBOL(<L_IN_TAB>)
                       WITH KEY NAME = 'REGUH-LIFNR'.
  IF SY-SUBRC EQ 0.
    LF_LIFNR = <L_IN_TAB>-VALUE.
  ENDIF.
  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUH-VBLNR'.
  IF SY-SUBRC EQ 0.
    LF_VBLNR = <L_IN_TAB>-VALUE.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LF_LIFNR
    IMPORTING
      OUTPUT = LF_LIFNR.

  SELECT EMPFG
    FROM REGUH
   WHERE VBLNR EQ @LF_VBLNR
     AND LIFNR EQ @LF_LIFNR
   ORDER BY PRIMARY KEY
    INTO @LF_EMPFG
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    CLEAR LF_EMPFG.
  ENDIF.

  SELECT SINGLE KTOKK
    INTO LF_KTOKK
    FROM LFA1
    WHERE LIFNR EQ LF_LIFNR.

  IF LF_EMPFG IS INITIAL.

    IF LF_LIFNR NE SPACE.
      IF LF_LIFNR+0(2) NE 'OT'.
        SELECT SINGLE ADRNR
          INTO LF_ADRNR
          FROM LFA1
          WHERE LIFNR EQ LF_LIFNR.

        SELECT NAME1,
               NAME2,
               NAME3
          FROM ADRC
         WHERE ADDRNUMBER EQ @LF_ADRNR
           AND NATION EQ ''
         ORDER BY PRIMARY KEY
          INTO (@LF_NAME1,@LF_NAME2,@LF_NAME3)
            UP TO 1 ROWS.
        ENDSELECT.
        IF SY-SUBRC NE 0.
          CLEAR: LF_NAME1,
                 LF_NAME2,
                 LF_NAME3.
        ENDIF.

      ENDIF.
    ENDIF.
  ELSE.
    SELECT ZNME1,
           ZNME2,
           ZNME3
      FROM REGUH
     WHERE VBLNR EQ @LF_VBLNR
       AND LIFNR EQ @LF_LIFNR
     ORDER BY PRIMARY KEY
      INTO (@LF_NAME1,@LF_NAME2,@LF_NAME3)
        UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      CLEAR: LF_NAME1,
             LF_NAME2,
             LF_NAME3.
    ENDIF.
  ENDIF.
  READ TABLE CT_OUT_TAB ASSIGNING FIELD-SYMBOL(<L_OUT_TAB>)
                        WITH KEY NAME = 'V_NAME1'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_NAME1.
  ENDIF.
  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'V_NAME2'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_NAME2.
  ENDIF.
  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'V_NAME3'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_NAME3.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'VENDOR'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_KTOKK.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_SPELL_AMOUNT
*----------------------------------------------------------------------*
*  F_SPELL_AMOUNT
*----------------------------------------------------------------------*
FORM F_SPELL_AMOUNT TABLES UT_IN_TAB TYPE TT_ITCSY           ##CALLED
                           CT_OUT_TAB TYPE TT_ITCSY.

  DATA:
    LF_CURR         TYPE SYWAERS,
    LF_AMOUNT_C(18) TYPE C,
    LF_SPELL(255)   TYPE C.

  CONSTANTS:
    LC_LANGU_TH TYPE SYLANGU VALUE '2'.


  CLEAR:
    LF_CURR,
    LF_AMOUNT_C.

  READ TABLE UT_IN_TAB ASSIGNING FIELD-SYMBOL(<L_IN_TAB>)
                       WITH KEY NAME = 'REGUD-WAERS'.
  IF SY-SUBRC EQ 0.
    LF_CURR = <L_IN_TAB>-VALUE.
  ENDIF.

  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUD-SWNET'.
  IF SY-SUBRC EQ 0.
    LF_AMOUNT_C = <L_IN_TAB>-VALUE.
    DO.
      REPLACE ',' WITH SPACE INTO LF_AMOUNT_C.
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.
    ENDDO.
    CONDENSE LF_AMOUNT_C NO-GAPS.
  ENDIF.

  CLEAR LF_SPELL.

  CALL FUNCTION 'Z_SDSCA_SPELL_AMOUNT'
    EXPORTING
      AMOUNT     = LF_AMOUNT_C
      CURRENCY   = LF_CURR
      LANGUAGE   = LC_LANGU_TH
    IMPORTING
      SPELL_WORD = LF_SPELL
    EXCEPTIONS
      NOT_FOUND  = 1
      TOO_LARGE  = 2
      OTHERS     = 3.
  IF SY-SUBRC NE 0.
    CLEAR LF_SPELL.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING FIELD-SYMBOL(<L_OUT_TAB>)
                        WITH KEY NAME = 'SPELL_WORD'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_SPELL.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_ALL_INVOICE
*----------------------------------------------------------------------*
*  F_GET_ALL_INVOICE
*----------------------------------------------------------------------*
FORM F_GET_ALL_INVOICE TABLES UT_IN_TAB TYPE TT_ITCSY        ##CALLED
                              CT_OUT_TAB TYPE TT_ITCSY.

  TYPES: BEGIN OF TS_BELNR,
           BELNR TYPE BELNR_D,
         END OF TS_BELNR.

  CONSTANTS:
    LC_LIFNR_INIT TYPE LIFNR VALUE '0000000000'.

  DATA:
    LT_BELNR TYPE STANDARD TABLE OF TS_BELNR.

  DATA:
    LS_BELNR TYPE TS_BELNR.

  DATA:
    LF_LAUFD_C(10) TYPE C,
    LF_LAUFD       TYPE LAUFD,
    LF_LAUFI       TYPE LAUFI,
    LF_XVORL       TYPE XVORL,
    LF_ZBUKR       TYPE DZBUKR,
    LF_LIFNR       TYPE LIFNR,
    LF_KUNNR       TYPE KUNNR,
    LF_EMPFG       TYPE EMPFG,
    LF_VBLNR       TYPE VBLNR,
    LF_INV1(126)   TYPE C,
    LF_INV2(126)   TYPE C,
    LF_INV3(126)   TYPE C,
    LF_INV4(126)   TYPE C,
    LF_INV5(126)   TYPE C,
    LF_INV6(126)   TYPE C.


  CLEAR: LF_LAUFD_C,
         LF_LAUFD,
         LF_LAUFI,
         LF_XVORL,
         LF_ZBUKR,
         LF_LIFNR,
         LF_KUNNR,
         LF_EMPFG,
         LF_VBLNR.

  READ TABLE UT_IN_TAB ASSIGNING FIELD-SYMBOL(<L_IN_TAB>)
                       WITH KEY NAME = 'REGUH-LAUFD'.
  IF SY-SUBRC EQ 0.
    LF_LAUFD_C = <L_IN_TAB>-VALUE.
    CONCATENATE LF_LAUFD_C+6(4) LF_LAUFD_C+3(2) LF_LAUFD_C+0(2) INTO LF_LAUFD.
  ENDIF.


  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUH-LAUFI'.
  IF SY-SUBRC EQ 0.
    LF_LAUFI = <L_IN_TAB>-VALUE.
    REPLACE '*' WITH '%' INTO LF_LAUFI.
  ENDIF.

  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUH-XVORL'.
  IF SY-SUBRC EQ 0.
    LF_XVORL = <L_IN_TAB>-VALUE.
  ENDIF.

  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUH-ZBUKR'.
  IF SY-SUBRC EQ 0.
    LF_ZBUKR = <L_IN_TAB>-VALUE.
  ENDIF.

  LF_LIFNR = LC_LIFNR_INIT.
  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUH-LIFNR'.
  IF SY-SUBRC EQ 0.
    LF_LIFNR = <L_IN_TAB>-VALUE.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LF_LIFNR
    IMPORTING
      OUTPUT = LF_LIFNR.

  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUH-KUNNR'.
  IF SY-SUBRC EQ 0.
    LF_KUNNR = <L_IN_TAB>-VALUE.
  ENDIF.

  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUH-EMPFG'.
  IF SY-SUBRC EQ 0.
    LF_EMPFG = <L_IN_TAB>-VALUE.
  ENDIF.

  READ TABLE UT_IN_TAB ASSIGNING <L_IN_TAB>
                       WITH KEY NAME = 'REGUH-VBLNR'.
  IF SY-SUBRC EQ 0.
    LF_VBLNR = <L_IN_TAB>-VALUE.
  ENDIF.

  CLEAR LT_BELNR.
  SELECT BELNR
    INTO TABLE LT_BELNR
    FROM REGUP
   WHERE LAUFD EQ LF_LAUFD
     AND LAUFI LIKE LF_LAUFI
     AND XVORL EQ LF_XVORL
     AND ZBUKR EQ LF_ZBUKR
     AND LIFNR EQ LF_LIFNR
     AND KUNNR EQ LF_KUNNR
     AND EMPFG EQ LF_EMPFG
     AND VBLNR EQ LF_VBLNR
   ORDER BY BELNR ASCENDING.

  IF LT_BELNR[] IS INITIAL.
    SELECT BELNR
      FROM BSAK_VIEW
     WHERE LIFNR EQ @LF_LIFNR
       AND AUGBL EQ @LF_VBLNR
       AND AUGDT EQ @LF_LAUFD
       AND BLART NE 'KZ'
     ORDER BY BELNR ASCENDING
      INTO TABLE @LT_BELNR.
  ENDIF.

  IF NOT LT_BELNR[] IS INITIAL.

    DELETE ADJACENT DUPLICATES FROM LT_BELNR COMPARING BELNR. "#EC CI_SORTED

    LF_INV1 = ' '.
    LF_INV2 = ' '.
    LF_INV3 = ' '.
    LF_INV4 = ' '.
    LF_INV5 = ' '.
    LF_INV6 = ' '.
    LOOP AT LT_BELNR INTO LS_BELNR.
      IF SY-TABIX LT 7.
        IF LF_INV1 EQ SPACE.
          LF_INV1 = LS_BELNR-BELNR.
        ELSE.
          CONCATENATE LF_INV1 ',' LS_BELNR-BELNR INTO LF_INV1 SEPARATED BY SPACE.
        ENDIF.
      ELSEIF SY-TABIX LT 13 ##NUMBER_OK.
        IF LF_INV2 EQ SPACE.
          LF_INV2 = LS_BELNR-BELNR.
        ELSE.
          CONCATENATE LF_INV2 ',' LS_BELNR-BELNR INTO LF_INV2 SEPARATED BY SPACE.
        ENDIF.
      ELSEIF SY-TABIX LT 19 ##NUMBER_OK.
        IF LF_INV3 EQ SPACE.
          LF_INV3 = LS_BELNR-BELNR.
        ELSE.
          CONCATENATE LF_INV3 ',' LS_BELNR-BELNR INTO LF_INV3 SEPARATED BY SPACE.
        ENDIF.
      ELSEIF SY-TABIX LT 25 ##NUMBER_OK.
        IF LF_INV4 EQ SPACE.
          LF_INV4 = LS_BELNR-BELNR.
        ELSE.
          CONCATENATE LF_INV4 ',' LS_BELNR-BELNR INTO LF_INV4 SEPARATED BY SPACE.
        ENDIF.
      ELSEIF SY-TABIX LT 31 ##NUMBER_OK.
        IF LF_INV5 EQ SPACE.
          LF_INV5 = LS_BELNR-BELNR.
        ELSE.
          CONCATENATE LF_INV5 ',' LS_BELNR-BELNR INTO LF_INV5 SEPARATED BY SPACE.
        ENDIF.
      ELSEIF SY-TABIX LT 37 ##NUMBER_OK.
        IF LF_INV6 EQ SPACE.
          LF_INV6 = LS_BELNR-BELNR.
        ELSE.
          CONCATENATE LF_INV6 ',' LS_BELNR-BELNR INTO LF_INV6 SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING FIELD-SYMBOL(<L_OUT_TAB>)
                        WITH KEY NAME = 'INVOICE_NO1'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_INV1.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'INVOICE_NO2'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_INV2.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'INVOICE_NO3'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_INV3.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'INVOICE_NO4'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_INV4.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'INVOICE_NO5'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_INV5.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'INVOICE_NO6'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_INV6.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_POSTING_ENTRIES
*----------------------------------------------------------------------*
*  F_GET_POSTING_ENTRIES
*----------------------------------------------------------------------*
FORM F_GET_POSTING_ENTRIES TABLES UT_IN_TAB TYPE TT_ITCSY    ##CALLED
                                  CT_OUT_TAB TYPE TT_ITCSY.

  TYPES: BEGIN OF TS_BSEG,
           SAKNR TYPE SAKNR,
           HKONT TYPE HKONT,
           DMBTR TYPE DMBTR,
           SHKZG TYPE SHKZG,
         END OF TS_BSEG.

  CONSTANTS:
    LC_SHKZG_S   TYPE SHKZG VALUE 'S',
    LC_SHKZG_H   TYPE SHKZG VALUE 'H',
    LC_CREDIT(3) TYPE C VALUE 'Cr.',
    LC_DEBIT(3)  TYPE C VALUE 'Dr.'.

  DATA:
    LT_BSEG  TYPE STANDARD TABLE OF TS_BSEG,
    LT_TLINE TYPE STANDARD TABLE OF TLINE.

  DATA:
    LS_BSEG  TYPE TS_BSEG,
    LS_LINE  TYPE TLINE,
    LS_THEAD TYPE THEAD.

  DATA:
    LF_BUKRS     TYPE BUKRS,
    LF_VBLNR     TYPE VBLNR,
    LF_GJAHR_I   TYPE GJAHR,
    LF_GJAHR     TYPE GJAHR,
    LF_BUZEI     TYPE BUZEI,
    LF_LAUFD     TYPE REGUP-LAUFD,
    LF_ZALDT     TYPE REGUH-ZALDT,
    LF_LANGU     TYPE SYLANGU,
    LF_CRDR(3)   TYPE C,
    LF_WRBTR(16) TYPE C,
    LF_TEXT_NAME TYPE TDOBNAME.


  CLEAR: LF_BUKRS,
         LF_VBLNR,
         LF_GJAHR_I,
         LF_GJAHR,
         LF_BUZEI,
         LF_LAUFD,
         LF_ZALDT,
         LF_LANGU.

  LOOP AT UT_IN_TAB INTO UT_IN_TAB.
    CASE UT_IN_TAB-NAME+6(5).
      WHEN 'BUKRS'.
        LF_BUKRS = UT_IN_TAB-VALUE.
      WHEN 'VBLNR'.
        LF_VBLNR = UT_IN_TAB-VALUE.
      WHEN 'GJAHR'.
        LF_GJAHR_I = UT_IN_TAB-VALUE.
      WHEN 'BUZEI'.
        LF_BUZEI = UT_IN_TAB-VALUE.
      WHEN 'LAUFD'.
        CONCATENATE UT_IN_TAB-VALUE+6(4) UT_IN_TAB-VALUE+3(2) UT_IN_TAB-VALUE(2) INTO LF_LAUFD.
    ENDCASE.
  ENDLOOP.

  SELECT ZALDT
    FROM REGUH
   WHERE LAUFD  EQ @LF_LAUFD
     AND VBLNR  EQ @LF_VBLNR
   ORDER BY PRIMARY KEY
    INTO @LF_ZALDT
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    CLEAR LF_ZALDT.
  ENDIF.

  SELECT SINGLE PERIV,
                WAERS
    FROM  T001
   WHERE  BUKRS  EQ @LF_BUKRS
    INTO @DATA(LS_T001).
  IF SY-SUBRC NE 0.
    CLEAR LS_T001.
  ENDIF.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      I_DATE         = LF_ZALDT
      I_PERIV        = LS_T001-PERIV
    IMPORTING
      E_GJAHR        = LF_GJAHR
    EXCEPTIONS
      INPUT_FALSE    = 1
      T009_NOTFOUND  = 2
      T009B_NOTFOUND = 3
      OTHERS         = 4.
  IF SY-SUBRC NE 0.
    CLEAR LF_GJAHR.
  ENDIF.

  IF ( LF_GJAHR IS INITIAL ) OR ( LF_GJAHR EQ '0000' ).
    LF_GJAHR = LF_GJAHR_I.
  ENDIF.

  LF_LANGU = SY-LANGU.

  CLEAR LT_BSEG.

  SELECT SAKNR HKONT DMBTR SHKZG
    INTO TABLE LT_BSEG
    FROM BSEG
   WHERE BUKRS EQ LF_BUKRS
     AND BELNR EQ LF_VBLNR
     AND GJAHR EQ LF_GJAHR
   ORDER BY PRIMARY KEY.

  IF NOT LT_BSEG[] IS INITIAL.
*  prepare text head
    CLEAR LF_TEXT_NAME.
    CONCATENATE 'Z' 'POSTING_ENTRIES' LF_BUKRS LF_VBLNR LF_GJAHR LF_BUZEI INTO LF_TEXT_NAME SEPARATED BY '_'.
    LS_THEAD-TDID = 'ST'.
    LS_THEAD-TDOBJECT = 'TEXT'.
    LS_THEAD-TDNAME = LF_TEXT_NAME.
    LS_THEAD-TDSPRAS = LF_LANGU.

*   prepare text line
    CLEAR LT_TLINE.

    LOOP AT LT_BSEG INTO LS_BSEG.
      CLEAR LF_WRBTR.
      IF LS_BSEG-SHKZG EQ LC_SHKZG_S.
        LF_CRDR = LC_DEBIT.
      ELSEIF LS_BSEG-SHKZG EQ LC_SHKZG_H.
        LF_CRDR = LC_CREDIT.
      ENDIF.
      WRITE LS_BSEG-DMBTR TO LF_WRBTR CURRENCY LS_T001-WAERS.

      CONCATENATE ',,' LF_CRDR LS_BSEG-HKONT LF_WRBTR INTO LS_LINE-TDLINE SEPARATED BY SPACE.
      LS_LINE-TDFORMAT = 'P1'.
      APPEND LS_LINE TO LT_TLINE.
    ENDLOOP.

*  save standard text
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        HEADER          = LS_THEAD
        INSERT          = 'X'
        SAVEMODE_DIRECT = 'X'
      TABLES
        LINES           = LT_TLINE
      EXCEPTIONS
        ID              = 1
        LANGUAGE        = 2
        NAME            = 3
        OBJECT          = 4
        OTHERS          = 5.
    IF SY-SUBRC NE 0 ##NEEDED.
*     Error on save text
    ENDIF.

  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING FIELD-SYMBOL(<L_OUT_TAB>)
                        WITH KEY NAME = 'POSTING_ENTRIES_TEXT'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_TEXT_NAME.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_DELETE_TEXT
*----------------------------------------------------------------------*
*  F_DELETE_TEXT
*----------------------------------------------------------------------*
FORM F_DELETE_TEXT TABLES UT_IN_TAB TYPE TT_ITCSY            ##CALLED
                          CT_OUT_TAB TYPE TT_ITCSY           ##NEEDED.

  DATA:
    LF_TEXT_NAME TYPE TDOBNAME,
    LF_LANGU     TYPE SYLANGU.


  CLEAR:
    LF_TEXT_NAME,
    LF_LANGU.

  READ TABLE UT_IN_TAB ASSIGNING FIELD-SYMBOL(<L_IN_TAB>)
                       WITH KEY NAME = 'POSTING_ENTRIES_TEXT'.
  IF SY-SUBRC EQ 0.
    LF_TEXT_NAME = <L_IN_TAB>-VALUE.
  ENDIF.

  LF_LANGU = SY-LANGU.

  IF LF_TEXT_NAME NE SPACE.

    CALL FUNCTION 'DELETE_TEXT'
      EXPORTING
        ID       = 'ST'
        LANGUAGE = LF_LANGU
        NAME     = LF_TEXT_NAME
        OBJECT   = 'TEXT'.

  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_AMT_WHT_NET
*----------------------------------------------------------------------*
*  F_GET_AMT_WHT_NET
*----------------------------------------------------------------------*
FORM F_GET_AMT_WHT_NET TABLES UT_IN_TAB TYPE TT_ITCSY        ##CALLED
                              CT_OUT_TAB TYPE TT_ITCSY.

  CONSTANTS:
    LC_KOART_K TYPE KOART VALUE 'K',
    LC_KOART_S TYPE KOART VALUE 'S'.

  DATA:
    LF_BUKRS       TYPE BUKRS,
    LF_VBLNR       TYPE VBLNR,
    LF_GJAHR       TYPE GJAHR,
    LF_GJAHR_I     TYPE GJAHR,
    LF_LAUFD       TYPE REGUP-LAUFD,
    LF_ZALDT       TYPE REGUH-ZALDT,
    LF_AMT         TYPE WRBTR,
    LF_AMT_TXT(16) TYPE C,
    LF_WHT         TYPE WRBTR,
    LF_WHT_TXT(16) TYPE C,
    LF_NET         TYPE WRBTR,
    LF_NET_TXT(16) TYPE C,
    LF_WAERS       TYPE BKPF-WAERS.


  PERFORM F_GET_CONSTANTS.

  CLEAR: LF_BUKRS,
         LF_VBLNR,
         LF_GJAHR_I,
         LF_GJAHR,
         LF_LAUFD,
         LF_ZALDT.

  LOOP AT UT_IN_TAB INTO UT_IN_TAB.
    CASE UT_IN_TAB-NAME+6(5).
      WHEN 'BUKRS'.
        LF_BUKRS = UT_IN_TAB-VALUE.
      WHEN 'VBLNR'.
        LF_VBLNR = UT_IN_TAB-VALUE.
      WHEN 'GJAHR'.
        LF_GJAHR_I = UT_IN_TAB-VALUE.
      WHEN 'LAUFD'.
        CONCATENATE UT_IN_TAB-VALUE+6(4) UT_IN_TAB-VALUE+3(2) UT_IN_TAB-VALUE(2) INTO LF_LAUFD.
    ENDCASE.
  ENDLOOP.

  SELECT ZALDT
    FROM REGUH
   WHERE LAUFD EQ @LF_LAUFD
     AND VBLNR EQ @LF_VBLNR
   ORDER BY PRIMARY KEY
    INTO @LF_ZALDT
      UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    CLEAR LF_ZALDT.
  ENDIF.

  SELECT SINGLE PERIV,
                WAERS
    FROM  T001
   WHERE  BUKRS EQ @LF_BUKRS
    INTO @DATA(LS_T001).
  IF SY-SUBRC NE 0.
    CLEAR LS_T001.
  ENDIF.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      I_DATE         = LF_ZALDT
      I_PERIV        = LS_T001-PERIV
    IMPORTING
      E_GJAHR        = LF_GJAHR
    EXCEPTIONS
      INPUT_FALSE    = 1
      T009_NOTFOUND  = 2
      T009B_NOTFOUND = 3
      OTHERS         = 4.
  IF SY-SUBRC NE 0.
    CLEAR LF_GJAHR.
  ENDIF.

  IF ( LF_GJAHR IS INITIAL ) OR ( LF_GJAHR EQ '0000' ).
    LF_GJAHR = LF_GJAHR_I.
  ENDIF.

  LF_AMT = 0.
  LF_WHT = 0.
  LF_NET = 0.

  CLEAR: LF_AMT_TXT,
         LF_WHT_TXT,
         LF_NET_TXT.

  SELECT A~BELNR,
         A~BUZEI,
         A~KOART,
         A~HKONT,
         A~WRBTR,
         A~SHKZG,
         B~WAERS
    FROM BSEG AS A
           INNER JOIN BKPF AS B
             ON  B~BUKRS = A~BUKRS
             AND B~BELNR = A~BELNR
             AND B~GJAHR = A~GJAHR
   WHERE A~BUKRS EQ @LF_BUKRS
     AND A~BELNR EQ @LF_VBLNR
     AND A~GJAHR EQ @LF_GJAHR
     AND ( A~KOART EQ @LC_KOART_K OR
           A~KOART EQ @LC_KOART_S )
   ORDER BY A~BUKRS ASCENDING,
            A~BELNR ASCENDING,
            A~GJAHR ASCENDING,
            A~BUZEI ASCENDING
    INTO TABLE @DATA(LT_BSEG).
  IF SY-SUBRC NE 0.
    CLEAR LT_BSEG.
  ENDIF.

  IF NOT LT_BSEG[] IS INITIAL.
    LOOP AT LT_BSEG ASSIGNING FIELD-SYMBOL(<L_BSEG>).
      LF_WAERS = <L_BSEG>-WAERS.
      CASE <L_BSEG>-KOART.
        WHEN LC_KOART_K.
*          AMOUNT
          IF  <L_BSEG>-SHKZG = 'H' . "Debit
            <L_BSEG>-WRBTR = <L_BSEG>-WRBTR * -1.
          ENDIF.
          LF_AMT = LF_AMT + <L_BSEG>-WRBTR.
        WHEN LC_KOART_S.
          IF  <L_BSEG>-SHKZG = 'S' . "Debit
            <L_BSEG>-WRBTR = <L_BSEG>-WRBTR * -1.
          ENDIF.
*          AMOUNT
          IF GR_HKONT_TAX IS NOT INITIAL AND
             <L_BSEG>-HKONT IN GR_HKONT_TAX.
            LF_AMT = LF_AMT + <L_BSEG>-WRBTR.
          ENDIF.
*          WITHOLDING TAX
          IF GR_HKONT_WHT IS NOT INITIAL AND
             <L_BSEG>-HKONT IN GR_HKONT_WHT.
            LF_WHT = LF_WHT + <L_BSEG>-WRBTR.
          ENDIF.
*          NET AMOUNT
          IF GR_HKONT_TAX IS NOT INITIAL AND
             <L_BSEG>-HKONT NOT IN GR_HKONT_TAX AND
             GR_HKONT_WHT IS NOT INITIAL AND
             <L_BSEG>-HKONT NOT IN GR_HKONT_WHT.
            LF_NET = LF_NET + <L_BSEG>-WRBTR.
          ENDIF.
        WHEN OTHERS.
*        do nothing.
      ENDCASE.

      IF GR_HKONT_OT IS NOT INITIAL AND
         <L_BSEG>-HKONT IN GR_HKONT_OT.

        IF <L_BSEG>-SHKZG = 'H' . "Debit
          <L_BSEG>-WRBTR = <L_BSEG>-WRBTR * -1.
        ENDIF.
        LF_NET = LF_NET + <L_BSEG>-WRBTR.
        LF_AMT = LF_AMT + <L_BSEG>-WRBTR.
      ENDIF.

    ENDLOOP.
  ENDIF.

  WRITE LF_AMT TO LF_AMT_TXT RIGHT-JUSTIFIED CURRENCY LF_WAERS.
  WRITE LF_WHT TO LF_WHT_TXT RIGHT-JUSTIFIED CURRENCY LF_WAERS.
  WRITE LF_NET TO LF_NET_TXT RIGHT-JUSTIFIED CURRENCY LF_WAERS.

  READ TABLE CT_OUT_TAB ASSIGNING FIELD-SYMBOL(<L_OUT_TAB>)
                        WITH KEY NAME = 'AMOUNT'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_AMT_TXT.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'WTHOD_TAX'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_WHT_TXT.
  ENDIF.

  READ TABLE CT_OUT_TAB ASSIGNING <L_OUT_TAB>
                        WITH KEY NAME = 'NET_AMOUNT'.
  IF SY-SUBRC EQ 0.
    <L_OUT_TAB>-VALUE = LF_NET_TXT.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_HKONT_OT  TYPE  ZSDSDE_PARAM_NAME VALUE 'HKONT_OT',
    LC_HKONT_TAX TYPE  ZSDSDE_PARAM_NAME VALUE 'HKONT_TAX',
    LC_HKONT_WHT TYPE  ZSDSDE_PARAM_NAME VALUE 'HKONT_WT'.

  STATICS:
    LF_READ       TYPE  FLAG.

  DATA:
    LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID   TYPE  PROGRAMM.


* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GR_HKONT_OT,
         GR_HKONT_TAX,
         GR_HKONT_WHT.

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = GC_TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
*     ------------------------------------
*     GL Account for OT
*     ------------------------------------
      WHEN LC_HKONT_OT.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
                INTO TABLE GR_HKONT_OT.
*     ------------------------------------
*     GL Account for TAX
*     ------------------------------------
      WHEN LC_HKONT_TAX.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
                INTO TABLE GR_HKONT_TAX.

*     ------------------------------------
*     GL Account for WHT
*     ------------------------------------
      WHEN LC_HKONT_WHT.
        INSERT VALUE #( SIGN = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
                INTO TABLE GR_HKONT_WHT.

    ENDCASE.

  ENDLOOP.

ENDFORM.
