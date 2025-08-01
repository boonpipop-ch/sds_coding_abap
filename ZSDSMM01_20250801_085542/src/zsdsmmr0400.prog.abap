*&---------------------------------------------------------------------*
*& Modulpool ZSDSMMR0400
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM ZSDSMMR0400.
*&---------------------------------------------------------------------*
*& Subroutine Pool   ZS_MM_SUBPRINTGR
*&
*&---------------------------------------------------------------------*
*&  #1
*&  Cnage by : Wantanee Prateep na thalang
*&  Change on: 17.11.2017 (DD.MM.YYYY)
*&  TR No.   : T41K928401
*&  Description: Edit shotdump case QTY wint comma
*&  Search Key:
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  W3_TOTAL_AMT_PER_ITEM
*&---------------------------------------------------------------------*
*      Total amount by line item
*----------------------------- -----------------------------------------*
FORM W3_TOTAL_AMT_PER_ITEM TABLES IN_TAB STRUCTURE ITCSY
                           OUT_TAB STRUCTURE ITCSY.
*T41K928297
  DATA: LV_NETPR TYPE EKPO-NETPR,
        LV_DMBTR TYPE MSEG-DMBTR,
        LV_MENGE TYPE MSEG-MENGE.

  CLEAR: LV_NETPR,LV_DMBTR,LV_MENGE.

  READ TABLE IN_TAB WITH KEY NAME = 'EKPO-NETPR'.
  IF SY-SUBRC EQ 0.

    REPLACE ALL OCCURRENCES OF REGEX ',' IN IN_TAB-VALUE WITH ''.
    CONDENSE IN_TAB-VALUE.
    MOVE IN_TAB-VALUE TO LV_NETPR.
  ENDIF.
  READ TABLE IN_TAB WITH KEY NAME = 'MSEG-MENGE'.
  IF SY-SUBRC EQ 0.
    REPLACE ALL OCCURRENCES OF REGEX ',' IN IN_TAB-VALUE WITH ''.
    CONDENSE IN_TAB-VALUE.
    MOVE IN_TAB-VALUE TO LV_MENGE.
  ENDIF.

  LV_DMBTR = LV_NETPR * LV_MENGE.

  READ TABLE OUT_TAB WITH KEY NAME = 'GV_NET_AMT'.
  IF SY-SUBRC EQ 0.
    OUT_TAB-VALUE = LV_DMBTR.
    CONDENSE OUT_TAB-VALUE .
    REPLACE ALL OCCURRENCES OF REGEX '(\d)(?=(\d{3})+(?!\d))' IN OUT_TAB-VALUE WITH '$1,'.
    MODIFY OUT_TAB INDEX SY-TABIX.
  ENDIF.

ENDFORM.                    "

*&---------------------------------------------------------------------*
*&      Form  W3_TOTAL_AMT
*&---------------------------------------------------------------------*
*      Total amount
*----------------------------- -----------------------------------------*
FORM W3_TOTAL_AMT TABLES IN_TAB STRUCTURE ITCSY
                           OUT_TAB STRUCTURE ITCSY.
*T41K928297
  TYPES: BEGIN OF TYP_MSEG,
           MBLNR TYPE MSEG-MBLNR,
           MJAHR TYPE MSEG-MJAHR,
           ZEILE TYPE MSEG-ZEILE,
           EBELN TYPE MSEG-EBELN,
           EBELP TYPE MSEG-EBELP,
           MENGE TYPE MSEG-MENGE,
           ERFMG TYPE MSEG-ERFMG,     "Quantity in unit of entry
         END OF TYP_MSEG.
  TYPES: BEGIN OF TYP_EKPO,
           EBELN TYPE EKPO-EBELN,
           EBELP TYPE EKPO-EBELP,
           NETPR TYPE EKPO-NETPR,
         END OF TYP_EKPO.
  DATA: GT_MSEG TYPE STANDARD TABLE OF TYP_MSEG,
        WA_MSEG TYPE  TYP_MSEG,
        GT_EKPO TYPE STANDARD TABLE OF TYP_EKPO,
        WA_EKPO TYPE  TYP_EKPO.

  DATA: LV_NETPR TYPE EKPO-NETPR,
        LV_DMBTR TYPE MSEG-DMBTR,
        LV_MENGE TYPE MSEG-MENGE,
        LV_MBLNR TYPE MKPF-MBLNR,
        LV_MJAHR TYPE MKPF-MJAHR,
        LV_QTY   TYPE I.
  DATA: LV_WAERS TYPE EKKO-WAERS.  "Add by Wantanee 20190318

  CLEAR: LV_NETPR,LV_DMBTR,LV_MENGE,LV_MBLNR,LV_MJAHR.

  READ TABLE IN_TAB WITH KEY NAME = 'MKPF-MBLNR'.
  IF SY-SUBRC EQ 0.
    MOVE IN_TAB-VALUE TO LV_MBLNR.
  ENDIF.
  READ TABLE IN_TAB WITH KEY NAME = 'MKPF-MJAHR'.
  IF SY-SUBRC EQ 0.
    MOVE IN_TAB-VALUE TO LV_MJAHR.
  ENDIF.

  SELECT MBLNR MJAHR ZEILE EBELN EBELP MENGE ERFMG
    INTO TABLE GT_MSEG
    FROM MSEG
    WHERE MBLNR EQ LV_MBLNR
    AND   MJAHR EQ LV_MJAHR
    AND   SHKZG EQ 'S'.
*break wantanee.
  IF NOT GT_MSEG IS INITIAL.
    LOOP AT GT_MSEG INTO WA_MSEG.
      "Add by Wantanee 20190318
      SELECT SINGLE WAERS
       INTO LV_WAERS
       FROM EKKO
       WHERE EBELN EQ WA_MSEG-EBELN.
      "End Add by Wantanee 20190318

      SELECT SINGLE NETPR
        INTO LV_NETPR
        FROM EKPO
        WHERE EBELN EQ WA_MSEG-EBELN
          AND EBELP EQ WA_MSEG-EBELP.
      IF LV_WAERS NE 'JPY'.
        LV_DMBTR = LV_DMBTR + ( LV_NETPR * WA_MSEG-ERFMG ) .
        LV_QTY = LV_QTY + WA_MSEG-MENGE.
      ELSE. "Add by Wantanee 20190318
        LV_DMBTR = LV_DMBTR + ( ( LV_NETPR * 100 )  * WA_MSEG-ERFMG ) .
        LV_QTY = LV_QTY + WA_MSEG-MENGE.
      ENDIF. "End Add by Wantanee 20190318
    ENDLOOP.
  ENDIF.



  READ TABLE OUT_TAB WITH KEY NAME = 'GV_TOTAL_AMT'.
  IF SY-SUBRC EQ 0.
    OUT_TAB-VALUE = LV_DMBTR.
    CONDENSE OUT_TAB-VALUE .
    REPLACE ALL OCCURRENCES OF REGEX '(\d)(?=(\d{3})+(?!\d))' IN OUT_TAB-VALUE WITH '$1,'.
    MODIFY OUT_TAB INDEX SY-TABIX.
  ENDIF.

  READ TABLE OUT_TAB WITH KEY NAME = 'GV_TOTAL_QTY'.
  IF SY-SUBRC EQ 0.
    OUT_TAB-VALUE = LV_QTY.
    CONDENSE OUT_TAB-VALUE .
*    REPLACE ALL OCCURRENCES OF REGEX '(\d)(?=(\d{3})+(?!\d))' IN out_tab-value WITH '$1,'.
    MODIFY OUT_TAB INDEX SY-TABIX.
  ENDIF.

ENDFORM.                    " CHECK_VENDOR
