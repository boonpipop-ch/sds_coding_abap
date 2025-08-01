*&---------------------------------------------------------------------*
*& Report  ZRTMMR0010
*&         Transaction Historical Report
*&---------------------------------------------------------------------*
*&
*&
*&       Please write the outline of the whole process the
*&       contents of the Program.
*&
*&       Supplementary Description matters on the run,
*&       If you have various guidelines for change
*&       Please describe in this second block.
*&
*&---------------------------------------------------------------------*
*& Version/T.P No.: 1.00 (XXXXXXXXXX)
*& Changed On/By:   23.01.2024 Jakarin S.
*& Description:     New Development
*&---------------------------------------------------------------------*

REPORT ZSDSMMR0010 LINE-COUNT 20 LINE-SIZE 200.

INCLUDE ZSDSMMR0010_TOP.

************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

*-- Supoj Delete on 02.02.2015
*PARAMETERS:
*            p_mtart LIKE mara-mtart  DEFAULT 'ZFG' OBLIGATORY.



  SELECT-OPTIONS:
             S_MTART FOR  MARA-MTART OBLIGATORY, "-- Supoj Add on 02.02.2015,
             S_DATE  FOR SY-DATUM OBLIGATORY NO-EXTENSION,
             S_WERKS FOR MARD-WERKS DEFAULT '1000' OBLIGATORY NO-EXTENSION NO INTERVALS,
             S_LGORT FOR MARD-LGORT OBLIGATORY NO INTERVALS,
             S_MATKL FOR MARA-MATKL NO-EXTENSION,
             S_MATNR FOR MARA-MATNR NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME.
*PARAMETERS: p_vari LIKE rfums_alv-variante .
*SELECTION-SCREEN : PUSHBUTTON 55(15) text-lay
*                  USER-COMMAND 'CONF' MODIF ID mc1.

  PARAMETERS : P_1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BLOCK2.
************************************************************************
*      I N I T I A L I Z A T I O N                                     *
************************************************************************
INITIALIZATION.
  PERFORM DEFAULT_VALUE.

*AT SELECTION-SCREEN.
*  IF sscrfields-ucomm CS 'CONF'.                            "568414
*    SUBMIT (sy-repid)
*                      WITH s_date   IN s_date
*                      WITH s_werks  IN s_werks
*                      WITH s_lgort  IN s_lgort
*                      WITH s_maktl  IN s_matkl
*                      WITH s_matnr  IN s_matnr
*                      AND RETURN.
*  ENDIF.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.            "568414
*  DATA: l_variant_help TYPE disvariant,
*        l_variant      TYPE disvariant.
*  l_variant-report = sy-repid.
*  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
*    EXPORTING
*      is_variant = l_variant
*      i_save     = 'A'
*    IMPORTING
*      es_variant = l_variant_help.
*  p_vari = l_variant_help-variant.

************************************************************************
*      B E G I N      S E L E C T I O N                                *
************************************************************************
START-OF-SELECTION.
  PERFORM GET_MATERIAL.
  PERFORM GET_VENDOR.
  PERFORM GET_CURRENT_STOCK.
  PERFORM GET_MVT.
  PERFORM GET_MATERIAL_DOC.
  PERFORM PREPARE_DATA.
  PERFORM DELETE_LINE.
************************************************************************
*      E N D   O F    S E L E C T I O N                                *
************************************************************************
END-OF-SELECTION.

  PERFORM FIELDCAT_BUILD.
  PERFORM LAYOUT_BUILD USING GS_LAYOUT.
  PERFORM E03_EVENTTAB_BUILD USING GT_EVENTS[].
  PERFORM SORT_TAB.
  PERFORM SHOW_REPORT.

*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFAULT_VALUE .

  DATA : LV_MONTH TYPE FCMNR,
         LV_YEAR  TYPE GJAHR.

  DATA: BEGIN OF LS_DATE,
          YEAR   TYPE GJAHR,
          MONTH  TYPE FCMNR,
          DAY(2) TYPE N,
        END OF LS_DATE.

  LS_DATE-MONTH = SY-DATUM+4(2).
  LS_DATE-YEAR  = SY-DATUM(4).
  LS_DATE-DAY  = '01'.

  LV_MONTH = SY-DATUM+4(2).
  LV_YEAR  = SY-DATUM(4).
  S_DATE-SIGN = 'I'.
  S_DATE-OPTION = 'BT'.
  S_DATE-LOW = LS_DATE.      " Start Date




  CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
    EXPORTING
      I_MONTH    = LV_MONTH
      I_YEAR     = LV_YEAR
    IMPORTING
      E_LAST_DAY = S_DATE-HIGH. " End Date

  APPEND S_DATE.

  S_MTART-SIGN = 'I'.
  S_MTART-OPTION = 'EQ'.
  S_MTART-LOW = 'ZFG'.      " Material Type

  APPEND S_MTART.


ENDFORM.                    " DEFAULT_VALUE
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MATERIAL .
* Get material data
  SELECT A~MATNR A~MATKL A~MEINS A~PRDHA A~MFRNR T~MAKTX
              M~MEINH   M~VOLUM   " Supoj Add on 12-03.2015
    INTO TABLE GT_MAT
    FROM MARA AS A
    INNER JOIN MAKT AS T
    ON A~MATNR = T~MATNR
    LEFT OUTER JOIN MARM AS M     " Supoj Add on 12-03.2015
    ON  A~MATNR = M~MATNR     "          ,,
    AND  A~MEINS = M~MEINH
    WHERE A~MATNR IN S_MATNR
    AND   A~MATKL IN S_MATKL
*    AND   a~mtart = p_mtart. Supoj Delete on  02.02.2015
       AND   A~MTART IN S_MTART. "Supoj Add on  02.02.2015


  SORT GT_MAT BY MATNR.
  DELETE ADJACENT DUPLICATES FROM GT_MAT COMPARING MATNR.

ENDFORM.                    " GET_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_STOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CURRENT_STOCK .

  DATA : FLAG(1).
  IF S_DATE-HIGH+4(2) = SY-DATUM+4(2).
* Get Current Stock
    SELECT D~MATNR D~WERKS D~LGORT LABST SPEME VERPR SALK3 PEINH LBKUM L~LGOBE
      INTO TABLE GT_CSTK
      FROM MARD AS D
      INNER JOIN T001L AS L
      ON D~LGORT = L~LGORT
         LEFT OUTER JOIN MBEW AS W  " Supoj Add on 30.12.2014
*      INNER JOIN mbew AS w    "Supoj delete on 30.12.2014
      ON D~MATNR = W~MATNR
      AND D~WERKS = W~BWKEY
      FOR ALL ENTRIES IN GT_MAT
      WHERE D~MATNR = GT_MAT-MATNR
      AND   D~WERKS  IN S_WERKS
      AND   D~LGORT  IN S_LGORT.

  ELSEIF S_DATE-HIGH+4(2) <> SY-DATUM+4(2).

* Get Preious Stock
*-- Delete on 30.12.2014
*    SELECT d~matnr d~werks d~lgort labst speme verpr salk3 peinh lbkum l~lgobe w~lfmon
*      INTO TABLE gt_cstk2
*      FROM mard AS d
*      INNER JOIN t001l AS l
*      ON d~lgort = l~lgort
*      INNER JOIN mbewh AS w " delete on 30.12.2014
*      ON d~matnr = w~matnr
*      AND d~werks = w~bwkey
*      FOR ALL ENTRIES IN gt_mat
*      WHERE d~matnr = gt_mat-matnr
*      AND   d~werks  IN s_werks
*      AND   d~lgort  IN s_lgort
*      and    w~lfgja = s_date-high(4)                         "IMS#300000255 missing data for end value qty & unit cost
*      AND   w~lfmon  = s_date-high+4(2).
*-- End Delete
*-- Add on 30.12.2014
    DATA : LT_CSTK2 LIKE TABLE OF GT_CSTK2 WITH HEADER LINE.
    SELECT D~MATNR D~WERKS D~LGORT LABST SPEME VERPR SALK3 PEINH LBKUM L~LGOBE W~LFMON
              W~LFGJA
      INTO TABLE LT_CSTK2
      FROM MARD AS D
      INNER JOIN T001L AS L
      ON D~LGORT = L~LGORT
*      INNER JOIN mbewh AS w " delete on 30.12.2014
      LEFT OUTER JOIN MBEWH AS W   " Add on 30.12.2014
      ON D~MATNR = W~MATNR
      AND D~WERKS = W~BWKEY
      FOR ALL ENTRIES IN GT_MAT
      WHERE D~MATNR = GT_MAT-MATNR
      AND   D~WERKS  IN S_WERKS
      AND   D~LGORT  IN S_LGORT.
*      and    w~lfgja = s_date-high(4)                         "IMS#300000255 missing data for end value qty & unit cost
*      AND   w~lfmon  = s_date-high+4(2).


    LOOP AT LT_CSTK2 WHERE  LFMON  EQ S_DATE-HIGH+4(2)
                           AND    LFGJA     EQ  S_DATE-HIGH(4).
      MOVE  LT_CSTK2  TO GT_CSTK2.
      APPEND  GT_CSTK2.
    ENDLOOP.

*    delete gt_cstk2 where lfmon ne s_date-high+4(2)
*                            and   lfgja  ne s_date-high(4).
*-- End Add


    SELECT D~MATNR D~WERKS D~LGORT LABST SPEME VERPR SALK3 PEINH LBKUM L~LGOBE
     INTO TABLE GT_CSTK3
     FROM MARD AS D
     INNER JOIN T001L AS L
     ON D~LGORT = L~LGORT
*     INNER JOIN mbew AS w " delete on 30.12.2014
     LEFT OUTER JOIN MBEW AS W  " Add on 30.12.2014
     ON D~MATNR = W~MATNR
     AND D~WERKS = W~BWKEY
     FOR ALL ENTRIES IN GT_MAT
     WHERE D~MATNR = GT_MAT-MATNR
     AND   D~WERKS  IN S_WERKS
     AND   D~LGORT  IN S_LGORT.

*    DELETE gt_cstk2 WHERE lfmon <> s_date-high+4(2).
    LOOP AT GT_MAT.
      CLEAR FLAG.
      LOOP AT GT_CSTK2 WHERE MATNR = GT_MAT-MATNR.
        MOVE-CORRESPONDING GT_CSTK2 TO GT_CSTK.
        APPEND GT_CSTK.
        CLEAR GT_CSTK.
        FLAG = 'X'.
      ENDLOOP.
      IF FLAG = ''.
        LOOP AT GT_CSTK3 WHERE MATNR = GT_MAT-MATNR.
          MOVE-CORRESPONDING GT_CSTK3 TO GT_CSTK.
          APPEND GT_CSTK.
          CLEAR GT_CSTK.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
*    LOOP AT gt_cstk2.
*      MOVE-CORRESPONDING gt_cstk2 TO gt_cstk.
*      APPEND gt_cstk.
*      CLEAR gt_cstk.
*    ENDLOOP.
  ENDIF.

* Calculate current value by storage location
  LOOP AT GT_CSTK.
    GT_CSTK-SALK3 = ( GT_CSTK-LABST + GT_CSTK-SPEME ) * ( GT_CSTK-SALK3 / GT_CSTK-LBKUM ).
    MODIFY GT_CSTK.
  ENDLOOP.

ENDFORM.                    " GET_CURRENT_STOCK
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MATERIAL_DOC .

* Get Material Document
  SELECT A~MBLNR ZEILE BUDAT BWART WERKS LGORT MATNR MENGE SHKZG DMBTR
  INTO TABLE GT_MSEG
  FROM  MKPF AS A INNER JOIN MSEG AS B
          ON  A~MBLNR = B~MBLNR
          AND A~MJAHR = B~MJAHR
  FOR ALL ENTRIES IN GT_MAT
  WHERE BUDAT GE S_DATE-LOW
  AND   MATNR EQ GT_MAT-MATNR
  AND   WERKS IN S_WERKS
  AND   LGORT IN S_LGORT
  AND   BWART IN R_MVT_M
  AND   SOBKZ <> 'K'
  AND   TCODE2 <> 'VL09'.


  PERFORM CHECK_REV_DOC.
  PERFORM AGREEGATE_DATA.
  PERFORM GET_FI_DOC.

ENDFORM.                    " GET_MATERIAL_DOC

*&---------------------------------------------------------------------*
*&      Form  check_rev_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHECK_REV_DOC.
  DATA : BEGIN OF LT_DOC OCCURS 0,
           MBLNR LIKE MSEG-MBLNR,
           ZEILE LIKE MSEG-ZEILE,
           SMBLN LIKE MSEG-SMBLN,
           SMBLP LIKE MSEG-SMBLP,
         END OF LT_DOC.

*Check reverse document
  SELECT  MBLNR ZEILE SMBLN SMBLP
    INTO TABLE LT_DOC
    FROM MSEG
    FOR ALL ENTRIES IN GT_MSEG
  WHERE SMBLN = GT_MSEG-MBLNR
  AND   SMBLP = GT_MSEG-ZEILE.

* delete document that was reversed
  LOOP AT LT_DOC.
    DELETE GT_MSEG WHERE MBLNR = LT_DOC-SMBLN
                   AND   ZEILE = LT_DOC-SMBLP .
*                   OR   ( mblnr = lt_doc-mblnr
*                   AND   zeile = lt_doc-zeile ).
  ENDLOOP.

ENDFORM.                    "check_rev_doc
*&---------------------------------------------------------------------*
*&      Form  AGREEGATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AGREEGATE_DATA .

  DATA : LV_LABST LIKE GT_CSTK-LABST.

* Collect material document value
  LOOP AT GT_MSEG.
    GT_MDOC-MATNR = GT_MSEG-MATNR.
    GT_MDOC-WERKS = GT_MSEG-WERKS.
    GT_MDOC-LGORT = GT_MSEG-LGORT.
    GT_MDOC-MENGE = GT_MSEG-MENGE.
    IF GT_MSEG-DMBTR IS INITIAL.
      CHECK GT_MSEG <> 'K'.
      READ TABLE GT_CSTK WITH KEY MATNR = GT_MSEG-MATNR
                                  WERKS = GT_MSEG-WERKS
                                  LGORT = GT_MSEG-LGORT.

      LV_LABST = GT_CSTK-LABST + GT_CSTK-SPEME.
      IF GT_MSEG-SHKZG = 'H'.
        GT_MDOC-MENGE = - GT_MSEG-MENGE.
        IF LV_LABST IS INITIAL.
          GT_MDOC-DMBTR =   GT_MDOC-MENGE *  GT_CSTK-VERPR.
        ELSE.
          GT_MDOC-DMBTR =   GT_MDOC-MENGE * ( GT_CSTK-SALK3 / ( GT_CSTK-LABST + GT_CSTK-SPEME ) ).
        ENDIF.
      ELSE.
        GT_MDOC-MENGE =   GT_MSEG-MENGE.
        IF LV_LABST IS INITIAL.
          GT_MDOC-DMBTR =   GT_MDOC-MENGE *  GT_CSTK-VERPR.
        ELSE.
          GT_MDOC-DMBTR =   GT_MDOC-MENGE * ( GT_CSTK-SALK3 / ( GT_CSTK-LABST + GT_CSTK-SPEME ) ).
        ENDIF.
      ENDIF.
      IF LV_LABST IS INITIAL.
        GT_MSEG-DMBTR  =  GT_MSEG-MENGE * GT_CSTK-VERPR.
      ELSE.
        GT_MSEG-DMBTR = GT_MSEG-MENGE * ( GT_CSTK-SALK3 /  ( GT_CSTK-LABST + GT_CSTK-SPEME ) ).
      ENDIF.

      MODIFY GT_MSEG.

    ELSE.
      IF GT_MSEG-SHKZG = 'H'.
        GT_MDOC-MENGE = - GT_MSEG-MENGE.
        GT_MDOC-DMBTR = - GT_MSEG-DMBTR.
      ELSE.
        GT_MDOC-MENGE = GT_MSEG-MENGE.
        GT_MDOC-DMBTR = GT_MSEG-DMBTR.
      ENDIF.
    ENDIF.

    COLLECT GT_MDOC.
    CLEAR GT_MDOC.
  ENDLOOP.

ENDFORM.                    " AGREEGATE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_MVT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MVT .

  REFRESH : GT_GENC.
  CLEAR GT_GENC.
* Get Movement type from table GEN_C
  SELECT *
  INTO TABLE GT_GENC
  FROM ZSDSCAC002
  WHERE REPID = SY-REPID.

  CHECK GT_GENC[] IS NOT INITIAL.
  LOOP AT GT_GENC.
* Append to range(Receive)
    IF GT_GENC-CONST CP GC_MVT_R.
      R_MVT_R-SIGN = 'I'.
      R_MVT_R-OPTION = 'EQ'.
      R_MVT_R-LOW = GT_GENC-VALUE.
      APPEND R_MVT_R.
      CLEAR R_MVT_R.
* Append to range(Issue)
    ELSEIF GT_GENC-CONST CP GC_MVT_I.
      R_MVT_I-SIGN = 'I'.
      R_MVT_I-OPTION = 'EQ'.
      R_MVT_I-LOW = GT_GENC-VALUE.
      APPEND R_MVT_I.
      CLEAR R_MVT_I.
    ELSEIF GT_GENC-CONST CP GC_MVT_N.
      R_MVT_N-SIGN = 'E'.
      R_MVT_N-OPTION = 'EQ'.
      R_MVT_N-LOW = GT_GENC-VALUE.
      APPEND R_MVT_N.
      CLEAR R_MVT_N.

    ELSEIF GT_GENC-CONST CP GC_MVT_M.
      R_MVT_M-SIGN = 'I'.
      R_MVT_M-OPTION = 'EQ'.
      R_MVT_M-LOW = GT_GENC-VALUE.
      APPEND R_MVT_M.
      CLEAR R_MVT_M.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_MVT
*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VENDOR .
* Get Manufacturer
  SELECT LIFNR NAME1
    INTO TABLE GT_VENDOR
    FROM LFA1
    FOR ALL ENTRIES IN GT_MAT
    WHERE LIFNR = GT_MAT-MFRNR.

ENDFORM.                    " GET_VENDOR
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARE_DATA .

  DATA : LV_LABST LIKE MARD-LABST.

* Prepare data to main internal table
  LOOP AT GT_CSTK.

* Read material data
    READ TABLE GT_MAT WITH KEY MATNR = GT_CSTK-MATNR.
    IF SY-SUBRC = 0.
      GT_RESULT-CATE = GT_MAT-PRDHA(5).
      GT_RESULT-CLASS = GT_MAT-PRDHA+5(5).
      GT_RESULT-SCLASS = GT_MAT-PRDHA+10(8).
      GT_RESULT-MATNR = GT_MAT-MATNR.
      GT_RESULT-MEINS = GT_MAT-MEINS.
      GT_RESULT-MFRNR = GT_MAT-MFRNR.
      GT_RESULT-VOLUM  = GT_MAT-VOLUM.

      SHIFT GT_RESULT-MFRNR LEFT DELETING LEADING '0'.
      GT_RESULT-MAKTX = GT_MAT-MAKTX.
    ENDIF.

* Read Manufacturer
    READ TABLE GT_VENDOR WITH KEY LIFNR = GT_MAT-MFRNR.
    IF SY-SUBRC = 0.
      GT_RESULT-NAME1 = GT_VENDOR-NAME1.
    ENDIF.

    GT_RESULT-LGORT = GT_CSTK-LGORT.
    GT_RESULT-LGOBE = GT_CSTK-LGOBE.

* Calculate begining stock
    PERFORM GET_BEGIN   USING  GT_CSTK-MATNR
                               GT_CSTK-WERKS
                               GT_CSTK-LGORT
                               GT_CSTK-LABST
                               GT_CSTK-VERPR
                               GT_CSTK-SALK3
                               GT_CSTK-SPEME
                        CHANGING
                               GT_RESULT-BQTY
                               GT_RESULT-BVAL.
* Calculate Goods receive (Qty,Value)
    PERFORM GET_REC   USING  GT_CSTK-MATNR
                             GT_CSTK-WERKS
                             GT_CSTK-LGORT
                     CHANGING
                             GT_RESULT-RQTY
                             GT_RESULT-RVAL.
* Calculate sale value,quantity
    PERFORM GET_SALE USING GT_CSTK-MATNR
                           GT_CSTK-WERKS
                           GT_CSTK-LGORT
                     CHANGING
                           GT_RESULT-SQTY
                           GT_RESULT-SVAL.
* Calculate adjust value
    PERFORM GET_OTHER USING GT_CSTK-MATNR
                          GT_CSTK-WERKS
                          GT_CSTK-LGORT
                    CHANGING
                          GT_RESULT-AQTY
                          GT_RESULT-AVAL.

* Calculate ending stock quantity,value
    GT_RESULT-EQTY = GT_RESULT-BQTY + GT_RESULT-RQTY + GT_RESULT-SQTY + GT_RESULT-AQTY.
    GT_RESULT-EVAL = GT_RESULT-BVAL + GT_RESULT-RVAL + GT_RESULT-SVAL + GT_RESULT-AVAL.

    IF GT_RESULT-EQTY IS NOT INITIAL.
      GT_RESULT-PUNT = GT_RESULT-EVAL / GT_RESULT-EQTY .
      GT_RESULT-PREIS = GT_RESULT-EVAL / GT_RESULT-EQTY .
    ENDIF.

*Check Value per unit = Current value per unit
    IF GT_RESULT-PREIS <> GT_CSTK-SALK3.
      GT_RESULT-PREIS = GT_CSTK-VERPR.
      LV_LABST = GT_CSTK-LABST + GT_CSTK-SPEME.
      IF LV_LABST = 0.
        GT_RESULT-EVAL = GT_RESULT-EQTY * GT_CSTK-VERPR .
      ELSE.
        GT_RESULT-EVAL = GT_RESULT-EQTY * GT_CSTK-SALK3 / ( GT_CSTK-LABST + GT_CSTK-SPEME ) .
      ENDIF.
    ENDIF.

* Get Stock Type A,R
    PERFORM GET_TYPE USING GT_RESULT-LGORT
                     CHANGING GT_RESULT-TYPE.

*-- Supoj Add Spic  on 12.03.2015
    PERFORM GET_SPIC USING  GT_CSTK-MATNR
                                      GT_MAT-PRDHA
                            CHANGING  GT_RESULT-ATWRT.


    GT_RESULT-T_VOLUM  = GT_RESULT-EQTY *   GT_RESULT-VOLUM .

*--EndAdd



* Append to main internal table
    APPEND GT_RESULT.
    CLEAR GT_RESULT.

  ENDLOOP.

*  PERFORM re_calculate_value.
  PERFORM RE_CALCULATE_VALUE2.

ENDFORM.                    " PREPARE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_BEGIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BEGIN USING P_MAT
                     P_WER
                     P_LGO
                     P_LGB
                     P_VER
                     P_SAL
                     P_SPE
               CHANGING
                     P_QTY
                     P_VAL.

* Calculate begining stock from material document
  READ TABLE GT_MDOC WITH KEY MATNR = P_MAT
                              WERKS = P_WER
                              LGORT = P_LGO.
  IF SY-SUBRC = 0.
    READ TABLE GT_FI_A WITH KEY MATNR = P_MAT
                                WERKS = P_WER
                                LGORT = P_LGO.
    IF SY-SUBRC = 0.
      P_VAL = P_SAL - ( GT_MDOC-DMBTR + GT_FI_A-DMBTR ).
    ELSE.
      P_VAL = P_SAL - GT_MDOC-DMBTR .
    ENDIF.
    P_QTY = P_SPE + P_LGB - GT_MDOC-MENGE .
  ELSE.
    P_QTY = P_SPE + P_LGB.
    P_VAL = P_SAL.
  ENDIF.

ENDFORM.                    " GET_BEGIN
*&---------------------------------------------------------------------*
*&      Form  SHOW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_DATA .

ENDFORM.                    " SHOW_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT_BUILD .
* Create Field Catalog
  PERFORM SET_FILEDCAT USING '1' 'CATE' 'GT_RESULT' 'Category' SPACE '5' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'CLASS' 'GT_RESULT' 'Class' SPACE '5' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'SCLASS' 'GT_RESULT' 'Sub Class' SPACE '8' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'MATNR' 'GT_RESULT' 'Material' SPACE '10' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'MAKTX' 'GT_RESULT' 'Description' SPACE '25' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'MFRNR' 'GT_RESULT' 'Manufact. Code' SPACE '7' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'NAME1' 'GT_RESULT' 'Manufact. Name' SPACE '7' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'TYPE' 'GT_RESULT' 'Grade' SPACE '5' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'LGORT' 'GT_RESULT' 'Storage Location' SPACE '7' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'LGOBE' 'GT_RESULT' 'Storage Name' SPACE '7' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'MEINS' 'GT_RESULT' 'Unit' SPACE '5' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'BQTY' 'GT_RESULT' 'Begin Stock' SPACE '18' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'BVAL' 'GT_RESULT' 'Begin Value' SPACE '18' '2' 'ZSMM_TRANS' 'BVAL'.
  PERFORM SET_FILEDCAT USING '1' 'RQTY' 'GT_RESULT' 'Receive Qty' SPACE '18' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'RVAL' 'GT_RESULT' 'Receive Value' SPACE '18' '2' 'ZSMM_TRANS' 'RVAL'.
  PERFORM SET_FILEDCAT USING '1' 'SQTY' 'GT_RESULT' 'Sales Qty' SPACE '18' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'SVAL' 'GT_RESULT' 'Sales Value' SPACE '18' '2' 'ZSMM_TRANS' 'SVAL'.
  PERFORM SET_FILEDCAT USING '1' 'AQTY' 'GT_RESULT' 'Adjust Qty' SPACE '18' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'AVAL' 'GT_RESULT' 'Adjust Value' SPACE '18' '2' 'ZSMM_TRANS' 'AVAL'.
  PERFORM SET_FILEDCAT USING '1' 'EQTY' 'GT_RESULT' 'Ending Qty' SPACE '18' '0' '' ''.
  PERFORM SET_FILEDCAT USING '1' 'EVAL' 'GT_RESULT' 'Value Qty' SPACE '18' '2' 'ZSMM_TRANS' 'EVAL'.
  PERFORM SET_FILEDCAT USING '1' 'PUNT' 'GT_RESULT' 'Unit Cost' SPACE '18' '2' 'ZSMM_TRANS' 'PUNT'.

*-- Add on 12.03.2015
  PERFORM SET_FILEDCAT USING '1' 'VOLUM' 'GT_RESULT' 'Volume' SPACE '13' '3' SPACE SPACE.
  PERFORM SET_FILEDCAT USING '1' 'T_VOLUM' 'GT_RESULT' 'Total Volume' SPACE '13' '3' SPACE SPACE.
  PERFORM SET_FILEDCAT USING '1' 'ATWRT' 'GT_RESULT' 'Sub Group' SPACE '30' '0' SPACE SPACE.
*-- End Add


ENDFORM.                    " FIELDCAT_BUILD

*&---------------------------------------------------------------------*
*&      Form  layout_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_LAYOUT  text
*----------------------------------------------------------------------*
FORM LAYOUT_BUILD  USING  PS_LAYOUT TYPE SLIS_LAYOUT_ALV.
  PS_LAYOUT-F2CODE            = '&ETA'.
  PS_LAYOUT-INFO_FIELDNAME    = 'LINE_COLOR'.
  PS_LAYOUT-GROUP_CHANGE_EDIT = 'X'.
  PS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  PS_LAYOUT-GET_SELINFOS      = 'X'.
  PS_LAYOUT-TOTALS_TEXT       = 'Grand Total'.
ENDFORM.                    " LAYOUT_BUILD

*&---------------------------------------------------------------------*
*&      Form  e03_eventtab_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->E03_LT_EVENTS  text
*----------------------------------------------------------------------*
FORM E03_EVENTTAB_BUILD USING E03_LT_EVENTS TYPE SLIS_T_EVENT.

  DATA: LW_EVENT TYPE SLIS_ALV_EVENT.
* Read Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 1
    IMPORTING
      ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                           INTO LW_EVENT.
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_list
*                             INTO lw_event.
  IF SY-SUBRC = 0.
* register top of page event
    MOVE GC_TOP_OF_PAGE TO LW_EVENT-FORM.
    APPEND LW_EVENT TO E03_LT_EVENTS.
  ENDIF.

ENDFORM.                    " E03_EVENTTAB_BUILD

*&---------------------------------------------------------------------*
*&      Form  show_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SHOW_REPORT .

* Show report
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_top_of_page = 'TOP_OF_PAGE'
      I_SAVE             = 'A'       " X: only global, U: only user-specific, A: all
      IS_LAYOUT          = GS_LAYOUT
      IT_FIELDCAT        = GS_FIELDCAT[]
*     it_sort            = gt_sort[]
      IT_EVENTS          = GT_EVENTS[]
      IS_PRINT           = GT_PRINT       " Print parameters
    TABLES
      T_OUTTAB           = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

ENDFORM.                    " SHOW_REPORT

*&---------------------------------------------------------------------*
*&      Form  set_filedcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW        text
*      -->P_FIELDNAME  text
*      -->P_TABNAME    text
*      -->P_REPTXT     text
*      -->P_EDIT       text
*      -->P_LEN        text
*----------------------------------------------------------------------*
FORM SET_FILEDCAT  USING    P_ROW
                            P_FIELDNAME
                            P_TABNAME
                            P_REPTXT
                            P_EDIT
                            P_LEN
                            P_DEC
                            P_REFT
                            P_REFF.

  DATA: FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  ADD 1 TO GW_COUNT.
  FIELDCAT-FIELDNAME     = P_FIELDNAME.
  FIELDCAT-TABNAME       = P_TABNAME.
  FIELDCAT-REF_TABNAME   = P_REFT.
  FIELDCAT-REF_FIELDNAME     = P_REFF.
  FIELDCAT-COL_POS       = GW_COUNT.
  FIELDCAT-SELTEXT_L     = P_REPTXT.
  FIELDCAT-OUTPUTLEN = P_LEN.
  FIELDCAT-DECIMALS_OUT  = P_DEC.
  APPEND FIELDCAT TO GS_FIELDCAT.

ENDFORM.                    " set_filedcat
*&---------------------------------------------------------------------*
*&      Form  GET_REC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CSTK_MATNR  text
*      -->P_GT_CSTK_WERKS  text
*      -->P_GT_CSTK_LGORT  text
*      <--P_GT_RESULT_RQTY  text
*      <--P_GT_RESULT_RVAL  text
*----------------------------------------------------------------------*
FORM GET_REC  USING  P_MAT
                     P_WER
                     P_LGO
               CHANGING
                     P_QTY
                     P_VAL.
* Calculate goods recieve from material document
  LOOP AT GT_MSEG WHERE MATNR = P_MAT
                  AND   WERKS = P_WER
                  AND   LGORT = P_LGO
                  AND   BWART IN R_MVT_R
                  AND   BUDAT IN S_DATE.

    IF GT_MSEG-SHKZG = 'H'.
      P_QTY = P_QTY - GT_MSEG-MENGE.
      P_VAL = P_VAL - GT_MSEG-DMBTR.
    ELSEIF GT_MSEG-SHKZG = 'S'.
      P_QTY = P_QTY + GT_MSEG-MENGE.
      P_VAL = P_VAL + GT_MSEG-DMBTR.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " GET_REC
*&---------------------------------------------------------------------*
*&      Form  GET_OTHER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CSTK_MATNR  text
*      -->P_GT_CSTK_WERKS  text
*      -->P_GT_CSTK_LGORT  text
*      <--P_GT_RESULT_AQTY  text
*      <--P_GT_RESULT_AVAL  text
*----------------------------------------------------------------------*
FORM GET_OTHER USING  P_MAT
                      P_WER
                      P_LGO
               CHANGING
                      P_QTY
                      P_VAL.

  LOOP AT GT_MSEG WHERE MATNR = P_MAT
                  AND   WERKS = P_WER
                  AND   LGORT = P_LGO
                  AND   BWART NOT IN R_MVT_R
                  AND   BWART NOT IN R_MVT_I
                  AND   BUDAT IN S_DATE.

    IF GT_MSEG-SHKZG = 'H'.
      P_QTY = P_QTY - GT_MSEG-MENGE.
      P_VAL = P_VAL - GT_MSEG-DMBTR.
    ELSEIF GT_MSEG-SHKZG = 'S'.
      P_QTY = P_QTY + GT_MSEG-MENGE.
      P_VAL = P_VAL + GT_MSEG-DMBTR.
    ENDIF.
  ENDLOOP.
  READ TABLE GT_FI_A WITH KEY MATNR = P_MAT
                            WERKS = P_WER
                            LGORT = P_LGO.
  IF SY-SUBRC = 0.
    P_VAL = P_VAL + GT_FI_A-DMBTR.
  ENDIF.

*    SELECT  y~matnr y~meins y~menge y~dmbtr y~shkzg
*      INTO (i_mseg-matnr, i_mseg-meins, i_mseg-menge, i_mseg-dmbtr, d_shkzg)
*      FROM bkpf AS x INNER JOIN bsim AS y
*          ON x~belnr = y~belnr AND
*             x~gjahr = y~gjahr
*          WHERE ( x~budat GE s_date-low
*            AND x~budat LE s_date-high )
**            AND x~gjahr = s_date-low(4)
*            AND x~awtyp EQ 'RMRP'
*            AND y~matnr = p_matnr
*            AND y~bwkey IN p_werks
*            AND y~buzei <> space
*
*      i_mseg-waers = 'THB'.
*      i_mseg-menge = 0.
*      IF d_shkzg <> 'S'.
*        i_mseg-menge = - i_mseg-menge.
*        i_mseg-dmbtr = - i_mseg-dmbtr.
*      ENDIF.
*      COLLECT i_mseg.
*    ENDSELECT.

ENDFORM.                    " GET_OTHER
*&---------------------------------------------------------------------*
*&      Form  GET_SALE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CSTK_MATNR  text
*      -->P_GT_CSTK_WERKS  text
*      -->P_GT_CSTK_LGORT  text
*      <--P_GT_RESULT_SQTY  text
*      <--P_GT_RESULT_SVAL  text
*----------------------------------------------------------------------*
FORM GET_SALE USING  P_MAT
                     P_WER
                     P_LGO
               CHANGING
                     P_QTY
                     P_VAL.
  LOOP AT GT_MSEG WHERE MATNR = P_MAT
                  AND   WERKS = P_WER
                  AND   LGORT = P_LGO
                  AND   BWART IN R_MVT_I
                  AND   BUDAT IN S_DATE.

    IF GT_MSEG-SHKZG = 'H'.
      P_QTY = P_QTY - GT_MSEG-MENGE.
      P_VAL = P_VAL - GT_MSEG-DMBTR.
    ELSEIF GT_MSEG-SHKZG = 'S'.
      P_QTY = P_QTY + GT_MSEG-MENGE.
      P_VAL = P_VAL + GT_MSEG-DMBTR.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " GET_SALE


*FORM top_of_list.
** Set flag that indicates that print of the ALV list has begun
*  gw_start_of_list = 'X'.
*ENDFORM.                    "TOP_OF_LIST

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  PERFORM WRITE_HEADING.
ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  write_heading
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM WRITE_HEADING.
  DATA : L_MATTYPE(50),
         L_CURREN(11),
         L_VTEXT(50),
         L_VALUES(11),
         L_VALUEN(11),
         L_MONTH       TYPE MONTH,
         L_TMONTH      TYPE T247,
         L_PAGE(3).

*--Supoj Delete on  02.02.2015
*  IF p_mtart = 'ZFG'.
*    l_mattype = 'Siam Daikin Sales (FG)'.
*  ELSEIF p_mtart = 'ZSP'.
*    l_mattype = 'Siam Daikin Sales (SP)'.
*  ENDIF.
*-- End Delete


*--Supoj Add on  02.02.2015
  L_MATTYPE = 'Siam Daikin Sales ('.
  LOOP AT S_MTART.
    IF S_MTART-OPTION EQ 'BT'.
      CONCATENATE L_MATTYPE S_MTART-LOW  'to' S_MTART-HIGH INTO L_MATTYPE SEPARATED BY SPACE.

    ELSEIF   S_MTART-OPTION EQ 'EQ'.
      CONCATENATE L_MATTYPE S_MTART-LOW  INTO  L_MATTYPE SEPARATED BY SPACE.
    ENDIF.
  ENDLOOP.
*-- End Add

  CONCATENATE L_MATTYPE ')'  INTO L_MATTYPE SEPARATED BY SPACE.


  MOVE  SY-DATUM+4(2) TO L_MONTH.
  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      LANGU = 'E'
      MONTH = L_MONTH
    IMPORTING
      T247  = L_TMONTH.
  CONCATENATE  SY-DATUM+6(2) '-' L_TMONTH-KTX '-'  SY-DATUM(4) INTO L_CURREN.


  WRITE : / L_MATTYPE,
        43 'Inventory Value and Transaction Historical Report',
        96 'Report Date :',L_CURREN.

  WRITE : /.
  WRITE : / 'Sort By : Category,Material,Storage Location',
          60 'Detailed (THB)'.

  MOVE  S_DATE-LOW+4(2) TO L_MONTH.
  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      LANGU = 'E'
      MONTH = L_MONTH
    IMPORTING
      T247  = L_TMONTH.

  CONCATENATE  S_DATE-LOW+6(2) '-' L_TMONTH-KTX '-'  S_DATE-LOW(4) INTO L_VALUES.


  MOVE  S_DATE-HIGH+4(2) TO L_MONTH.
  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      LANGU = 'E'
      MONTH = L_MONTH
    IMPORTING
      T247  = L_TMONTH.

  CONCATENATE  S_DATE-HIGH+6(2) '-' L_TMONTH-KTX '-'  S_DATE-HIGH(4) INTO L_VALUEN.

  CONCATENATE  'Value On' L_VALUES 'To' L_VALUEN
               INTO L_VTEXT SEPARATED BY SPACE.
  WRITE : /.
  WRITE : /57 L_VTEXT.

* Local variable declaration
  STATICS: L_COMM  TYPE SYUCOMM.  " Store user command* Declaration of local variables
  DATA: L_LINE_SIZE TYPE SYLINSZ,           " Line size
        L_LINE      TYPE SLIS_LISTHEADER,   " Hold list header
        L_CURRDOC   TYPE IDCN037,           " Current doc.
        L_CURRTABIX TYPE SYTABIX.           " Current index* Check for print or print preview

  PERFORM CAL_LINE.

  IF  SY-UCOMM = '&RNT'.
    CLEAR GW_PAGE_PRINT.
    PERFORM CAL_LINE.
    GW_PAGE = GW_PAGE + 1.
    MOVE GW_PAGE TO L_PAGE.
    CONCATENATE 'Page : ' L_PAGE INTO GW_PAGE_PRINT SEPARATED BY SPACE.
    WRITE : 109 GW_PAGE_PRINT.
  ENDIF.
ENDFORM.                    "write_heading
*&---------------------------------------------------------------------*
*&      Form  GET_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_RESULT_LGORT  text
*      <--P_GT_RESULT_TYPE  text
*----------------------------------------------------------------------*
FORM GET_TYPE  USING    P_LGORT
               CHANGING P_TYPE.

  READ TABLE GT_GENC WITH KEY VALUE = P_LGORT.
  IF SY-SUBRC = 0.
    P_TYPE = 'A'.
  ELSE.
    P_TYPE = 'R'.
  ENDIF.

ENDFORM.                    " GET_TYPE
*&---------------------------------------------------------------------*
*&      Form  DELETE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_LINE .
  IF P_1 = 'X'.
    DELETE GT_RESULT WHERE BQTY IS INITIAL
                     AND   EQTY IS INITIAL
                     AND   RQTY IS INITIAL
                     AND   SQTY IS INITIAL.
  ENDIF.
  SORT GT_RESULT BY CATE CLASS MATNR LGORT.
ENDFORM.                    " DELETE_LINE
*&---------------------------------------------------------------------*
*&      Form  GET_FI_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FI_DOC .

  DATA : BEGIN OF LT_MSEG OCCURS 0 ,
           MBLNR LIKE MSEG-MBLNR,
           GJAHR LIKE MSEG-GJAHR,
           ZEILE LIKE MSEG-ZEILE,
           LGORT LIKE MSEG-LGORT,
         END OF LT_MSEG.

  SELECT  Y~MATNR Y~BWKEY Y~BELNR Y~GJAHR Y~BUZEI
          Y~SHKZG Y~DMBTR Y~BUDAT Y~BLART
    INTO TABLE GT_FI_DOC
    FROM BKPF AS X INNER JOIN BSIM AS Y
        ON X~BELNR = Y~BELNR AND
           X~GJAHR = Y~GJAHR
        FOR ALL ENTRIES IN GT_MAT
        WHERE X~BUDAT IN S_DATE
          AND X~AWTYP IN ('RMRP','PRCHG')
          AND X~BUKRS =  '1000'
          AND Y~MATNR EQ GT_MAT-MATNR
          AND Y~BWKEY IN S_WERKS
          AND Y~BUZEI <> SPACE.
*          AND y~blart NE 'PR'.

  IF GT_FI_DOC[] IS NOT INITIAL.

    SELECT BELNR GJAHR BUZEI XREF3 EBELN EBELP
      INTO TABLE GT_FI_X
      FROM BSEG
      FOR ALL ENTRIES IN GT_FI_DOC
      WHERE BELNR = GT_FI_DOC-BELNR
      AND   GJAHR = GT_FI_DOC-GJAHR
      AND   BUZEI = GT_FI_DOC-BUZEI.


    LOOP AT GT_FI_X.
      IF GT_FI_X-XREF3 IS NOT INITIAL.
        SHIFT GT_FI_X-XREF3 LEFT DELETING LEADING SPACE.
        GT_FI_X-GJAHR1 = GT_FI_X-XREF3(4).
        GT_FI_X-MBLNR  = GT_FI_X-XREF3+4(10).
        GT_FI_X-ZEILE  = GT_FI_X-XREF3+14(4).
        MODIFY GT_FI_X INDEX SY-TABIX.
      ELSEIF GT_FI_X-XREF3 IS INITIAL.
        SELECT SINGLE XREF3
          INTO GT_FI_X-XREF3
          FROM BSEG
          WHERE BELNR = GT_FI_X-BELNR
          AND   GJAHR = GT_FI_X-GJAHR
          AND   EBELN = GT_FI_X-EBELN
          AND   EBELP = GT_FI_X-EBELP
          AND   BUZID = 'W'.
        IF SY-SUBRC = 0.
          SHIFT GT_FI_X-XREF3 LEFT DELETING LEADING SPACE.
          GT_FI_X-GJAHR1 = GT_FI_X-XREF3(4).
          GT_FI_X-MBLNR  = GT_FI_X-XREF3+4(10).
          GT_FI_X-ZEILE  = GT_FI_X-XREF3+14(4).
          MODIFY GT_FI_X INDEX SY-TABIX.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CHECK GT_FI_X[] IS NOT INITIAL.
    SELECT MBLNR GJAHR ZEILE LGORT
      INTO TABLE LT_MSEG
      FROM MSEG
      FOR ALL ENTRIES IN GT_FI_X
      WHERE MBLNR = GT_FI_X-MBLNR
      AND   GJAHR = GT_FI_X-GJAHR1
      AND   ZEILE = GT_FI_X-ZEILE.

    LOOP AT GT_FI_DOC.
      READ TABLE GT_FI_X WITH KEY BELNR = GT_FI_DOC-BELNR
                                  GJAHR = GT_FI_DOC-GJAHR
                                  BUZEI = GT_FI_DOC-BUZEI.
      IF SY-SUBRC = 0.
        READ TABLE LT_MSEG WITH KEY MBLNR = GT_FI_X-MBLNR
                                    GJAHR = GT_FI_X-GJAHR1
                                    ZEILE = GT_FI_X-ZEILE.
        IF SY-SUBRC = 0.
          GT_FI_A-MATNR = GT_FI_DOC-MATNR.
          GT_FI_A-WERKS = GT_FI_DOC-BWKEY.
          GT_FI_A-LGORT = LT_MSEG-LGORT.
          IF GT_FI_DOC-SHKZG = 'H'.
            GT_FI_A-DMBTR =  - GT_FI_DOC-DMBTR.
          ELSEIF GT_FI_DOC-SHKZG = 'S'.
            GT_FI_A-DMBTR = GT_FI_DOC-DMBTR.
          ENDIF.
          COLLECT GT_FI_A.
          CLEAR GT_FI_A.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_FI_DOC
*&---------------------------------------------------------------------*
*&      Form  RE_CALCULATE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALCULATE_VALUE .
  DATA : L_TABIX LIKE SY-TABIX.

  LOOP AT GT_RESULT.
    L_TABIX = SY-TABIX.
    IF GT_RESULT-BQTY IS NOT INITIAL.
      GT_RESULT-BVAL = GT_RESULT-BQTY * GT_RESULT-PREIS .
    ELSEIF GT_RESULT-BQTY IS INITIAL
      AND  GT_RESULT-BVAL IS NOT INITIAL.
      CLEAR GT_RESULT-BVAL.
    ENDIF.

    IF GT_RESULT-RQTY IS NOT INITIAL.
      GT_RESULT-RVAL = GT_RESULT-RQTY * GT_RESULT-PREIS .
    ELSEIF GT_RESULT-RQTY IS INITIAL
      AND  GT_RESULT-RVAL IS NOT INITIAL.
      CLEAR GT_RESULT-RVAL.
    ENDIF.

    IF GT_RESULT-SQTY IS NOT INITIAL.
      GT_RESULT-SVAL = GT_RESULT-SQTY * GT_RESULT-PREIS .
    ELSEIF GT_RESULT-SQTY IS INITIAL
    AND  GT_RESULT-SVAL IS NOT INITIAL.
      CLEAR GT_RESULT-SVAL.
    ENDIF.

    IF GT_RESULT-AQTY IS NOT INITIAL.
      GT_RESULT-AVAL = GT_RESULT-AQTY * GT_RESULT-PREIS .
    ELSEIF GT_RESULT-AQTY IS INITIAL
      AND  GT_RESULT-AVAL IS NOT INITIAL.
      CLEAR GT_RESULT-AVAL.
    ENDIF.

    IF GT_RESULT-EQTY IS NOT INITIAL.
      GT_RESULT-EVAL = GT_RESULT-EQTY * GT_RESULT-PREIS .
    ENDIF.
    MODIFY GT_RESULT INDEX L_TABIX.
  ENDLOOP.
ENDFORM.                    " RE_CALCULATE_VALUE
*&---------------------------------------------------------------------*
*&      Form  CAL_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_LINE .

  DATA IT_GRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA LW_REPID LIKE SY-REPID VALUE 'ZR_MM_TRANS_REPORT2'.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_REPID = LW_REPID
      E_GRID  = IT_GRID.

ENDFORM.                    " CAL_LINE
*&---------------------------------------------------------------------*
*&      Form  GET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LAYOUT .

  CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'            "616340/2
    IMPORTING
      ES_LAYOUT     = GS_LAYOUT
      ET_FIELDCAT   = GS_FIELDCAT[]
*     et_sort       = gt_sort[]
      ET_FILTER     = GT_FILT[]
*     ES_LIST_SCROLL =
*     ES_VARIANT    =
    EXCEPTIONS
      NO_INFOS      = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.
ENDFORM.                    " GET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SORT_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_TAB.
  DATA : LT_SORT TYPE SLIS_SORTINFO_ALV .

  CLEAR LT_SORT.
  LT_SORT-SPOS      = '1'.     " Sort sequence
  LT_SORT-FIELDNAME = 'CATE'.     " Document no
  LT_SORT-TABNAME   = 'GT_RESULT'.    " Table name
  LT_SORT-UP        = 'X'.       " Ascending
  LT_SORT-GROUP     = '*'.    " Group* Populate sort table
  APPEND LT_SORT TO GT_SORT.

  CLEAR LT_SORT.
  LT_SORT-SPOS      = '2'.     " Sort sequence
  LT_SORT-FIELDNAME = 'CLASS'.     " Document no
  LT_SORT-TABNAME   = 'GT_RESULT'.    " Table name
  LT_SORT-UP        = 'X'.       " Ascending
  LT_SORT-GROUP     = ''.    " Group* Populate sort table
  APPEND LT_SORT TO GT_SORT.

  CLEAR LT_SORT.
  LT_SORT-SPOS      = '3'.     " Sort sequence
  LT_SORT-FIELDNAME = 'MATNR'.     " Document no
  LT_SORT-TABNAME   = 'GT_RESULT'.    " Table name
  LT_SORT-UP        = 'X'.       " Ascending
  LT_SORT-GROUP     = ''.    " Group* Populate sort table
  APPEND LT_SORT TO GT_SORT.


  CLEAR LT_SORT.
  LT_SORT-SPOS      = '4'.     " Sort sequence
  LT_SORT-FIELDNAME = 'LGORT'.     " Document no
  LT_SORT-TABNAME   = 'GT_RESULT'.    " Table name
  LT_SORT-UP        = 'X'.       " Ascending
  LT_SORT-GROUP     = ''.    " Group* Populate sort table

  APPEND LT_SORT TO GT_SORT.

ENDFORM.                    " SORT_TAB
*&---------------------------------------------------------------------*
*&      Form  RE_CALCULATE_VALUE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALCULATE_VALUE2 .   " Adjust Begin Stock in case moving changed in month but no transaction in some storage location

  DATA : L_TABIX   LIKE SY-TABIX,
         L_MENGE   LIKE GT_RESULT-BQTY,
         L_VAL     LIKE GT_RESULT-BVAL,
         LT_RESULT LIKE GT_RESULT OCCURS 0 WITH HEADER LINE.

  LT_RESULT[] = GT_RESULT[].

  LOOP AT GT_RESULT WHERE BQTY = 0
                    AND   BVAL <> 0.
    CLEAR : L_MENGE.
    LOOP AT LT_RESULT WHERE MATNR = GT_RESULT-MATNR
                      AND   LGORT <> GT_RESULT-LGORT.
      L_MENGE = L_MENGE + LT_RESULT-BQTY .
    ENDLOOP.

    IF L_MENGE IS NOT INITIAL.
      L_VAL = GT_RESULT-BVAL / L_MENGE.
    ENDIF.
    LOOP AT LT_RESULT WHERE MATNR = GT_RESULT-MATNR
                      AND   LGORT <> GT_RESULT-LGORT
                      AND   BQTY  <> 0.
      L_TABIX = SY-TABIX.
      LT_RESULT-BVAL = LT_RESULT-BVAL - L_VAL .
      MODIFY LT_RESULT INDEX L_TABIX.
    ENDLOOP.
    LOOP AT LT_RESULT WHERE MATNR = GT_RESULT-MATNR
                      AND   LGORT = GT_RESULT-LGORT.
      L_TABIX = SY-TABIX.
      CLEAR : LT_RESULT-BVAL.
      MODIFY LT_RESULT INDEX L_TABIX.
    ENDLOOP.
  ENDLOOP.

  CLEAR:  GT_RESULT,GT_RESULT[].
  GT_RESULT[] = LT_RESULT[].
ENDFORM.                    " RE_CALCULATE_VALUE2
*&---------------------------------------------------------------------*
*&      Form  GET_SPIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CSTK_MATNR  text
*      <--P_GT_RESULT_ATWRT  text
*----------------------------------------------------------------------*
FORM GET_SPIC  USING    P_MATNR
                                     P_PRODH
               CHANGING CH_ATWRT.

  CHECK P_MATNR IS NOT INITIAL.



  DATA:LT_CLOBJDAT TYPE TABLE OF CLOBJDAT,
       LW_CLOBJDAT LIKE LINE OF LT_CLOBJDAT,
       LT_SCLASS   TYPE TABLE OF SCLASS,
       LV_OBJ      TYPE AUSP-OBJEK.

  MOVE  P_MATNR TO  LV_OBJ.


  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
    EXPORTING
*     CLASS              = ' '
*     CLASSTEXT          = 'X'
      CLASSTYPE          = '001'
*     CLINT              = 0
*     FEATURES           = 'X'
*     LANGUAGE           = SY-LANGU
      OBJECT             = LV_OBJ
*     OBJECTTABLE        = ' '
*     KEY_DATE           = SY-DATUM
*     INITIAL_CHARACT    = 'X'
*     NO_VALUE_DESCRIPT  =
*     CHANGE_SERVICE_CLF = 'X'
*     INHERITED_CHAR     = ' '
*     CHANGE_NUMBER      = ' '
    TABLES
      T_CLASS            = LT_SCLASS
      T_OBJECTDATA       = LT_CLOBJDAT
*     I_SEL_CHARACTERISTIC       =
*     T_NO_AUTH_CHARACT  =
    EXCEPTIONS
      NO_CLASSIFICATION  = 1
      NO_CLASSTYPES      = 2
      INVALID_CLASS_TYPE = 3
      OTHERS             = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  READ TABLE  LT_CLOBJDAT INTO LW_CLOBJDAT WITH KEY SMBEZ = 'SPIC'.

  IF SY-SUBRC EQ 0 AND  LW_CLOBJDAT-AUSP1 NE '?'.
    MOVE   LW_CLOBJDAT-AUSP1 TO  CH_ATWRT.
  ENDIF.

  IF CH_ATWRT IS INITIAL.
    MOVE P_PRODH+10(8) TO  CH_ATWRT.
  ENDIF.


ENDFORM.                    " GET_SPIC
