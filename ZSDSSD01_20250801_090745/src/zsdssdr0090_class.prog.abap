*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0090_CLASS
*&---------------------------------------------------------------------*
CLASS EVENT_CLASS DEFINITION.
*Handling double click
  PUBLIC SECTION.
    METHODS:
    HANDLE_DOUBLE_CLICK
    FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS. "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS EVENT_CLASS IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.

  ENDMETHOD. "handle_double_click
ENDCLASS. "lcl_event_receiver IMPLEMENTATION
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.
    CLASS-METHODS :
      VALIDATION RETURNING VALUE(R) TYPE CHAR255.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD VALIDATION.

    DATA : BEGIN OF LS_PS,
             PS_PSP_PNR TYPE VBRP-PS_PSP_PNR,
           END OF LS_PS.
    DATA : LT_PS LIKE HASHED TABLE OF LS_PS WITH UNIQUE KEY PS_PSP_PNR.

    DATA : BEGIN OF LS_DOC,
             VBELN TYPE VBAK-VBELN,
           END OF LS_DOC.
    DATA : LT_DOC LIKE HASHED TABLE OF LS_DOC WITH UNIQUE KEY VBELN.

    DATA : LV_SO TYPE C.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    CONSTANTS : BEGIN OF LC_CON,
                  I_REPID TYPE CHAR20 VALUE 'ZSDSSDR0090',
                  I_PARAM TYPE CHAR20 VALUE 'CASE_DEDUCT_REVENUE',
                END OF LC_CON.

    DATA : CR_RETURN TYPE RANGE OF PRCD_ELEMENTS-KSCHL.
    DATA : LV_ERROR    TYPE CHAR01,
           LV_CHK_INSU TYPE CHAR01,
           LV_CHK_MOD  TYPE CHAR01.

    SELECT COUNT( * )
      FROM ZSDSCAC009
      WHERE PROCESS EQ 'CHECK_WBS'
        AND STATU   EQ ABAP_TRUE.
    IF SY-SUBRC EQ 0.
      IF LCL_UTIL IS NOT BOUND.
        CREATE OBJECT LCL_UTIL.
      ENDIF.

      LCL_UTIL->GET_CONFIG_001( EXPORTING I_REPID              = LC_CON-I_REPID
                                          I_PARAM              = LC_CON-I_PARAM
                                 CHANGING CR_RETURN            = CR_RETURN ).

      LT_PS  =  CORRESPONDING #( GT_ITEM  DISCARDING DUPLICATES ).
      LT_DOC =  CORRESPONDING #( GT_ITEM  DISCARDING DUPLICATES ).

      SELECT VBAK~VBELN,
             PRCD_ELEMENTS~KPOSN,
             SUM( PRCD_ELEMENTS~KWERT ) AS KWERT
        FROM @LT_DOC AS A
        INNER JOIN VBAK ON A~VBELN EQ VBAK~VBELN
        INNER JOIN PRCD_ELEMENTS ON VBAK~KNUMV EQ PRCD_ELEMENTS~KNUMV
        WHERE PRCD_ELEMENTS~KSCHL IN @CR_RETURN[]
        GROUP BY VBAK~VBELN,PRCD_ELEMENTS~KPOSN
        INTO TABLE @DATA(LT_PRICE).

      SELECT PRPS~PSPNR,
*             RPSCO~GJAHR,
             SUM( RPSCO~WLP01 + RPSCO~WLP02 + RPSCO~WLP03 +
                  RPSCO~WLP04 + RPSCO~WLP05 + RPSCO~WLP06 +
                  RPSCO~WLP07 + RPSCO~WLP08 + RPSCO~WLP09 +
                  RPSCO~WLP10 + RPSCO~WLP11 + RPSCO~WLP12 +
                  RPSCO~WLP13 + RPSCO~WLP14 + RPSCO~WLP15 +
                  RPSCO~WLP16
                 ) AS SUM_DATA
        FROM @LT_PS AS A
        INNER JOIN PRPS  ON A~PS_PSP_PNR EQ PRPS~PSPNR
        INNER JOIN RPSCO ON PRPS~OBJNR  EQ RPSCO~OBJNR AND
                            RPSCO~VORGA EQ 'RKP5'      AND
                            RPSCO~VERSN EQ '000'       AND
                            RPSCO~BELTP EQ '2'         AND
                            ( RPSCO~ACPOS EQ '500' OR
                              RPSCO~ACPOS EQ '600' )
        GROUP BY PSPNR",GJAHR
        INTO TABLE @DATA(LT_PLAN).

      SELECT COUNT( * )
       FROM ZSDSCAC009
       WHERE PROCESS EQ 'CHECK_SO'
         AND STATU   EQ ABAP_TRUE.
      IF SY-SUBRC EQ 0.
        LV_SO = ABAP_TRUE.
        SELECT PRPS~PSPNR,
*             RPSCO~GJAHR,
               SUM( RPSCO~WLP01 + RPSCO~WLP02 + RPSCO~WLP03 +
                    RPSCO~WLP04 + RPSCO~WLP05 + RPSCO~WLP06 +
                    RPSCO~WLP07 + RPSCO~WLP08 + RPSCO~WLP09 +
                    RPSCO~WLP10 + RPSCO~WLP11 + RPSCO~WLP12 +
                    RPSCO~WLP13 + RPSCO~WLP14 + RPSCO~WLP15 +
                    RPSCO~WLP16
                   ) AS SUM_DATA
          FROM @LT_PS AS A
          INNER JOIN PRPS  ON A~PS_PSP_PNR EQ PRPS~PSPNR
          INNER JOIN RPSCO ON PRPS~OBJNR  EQ RPSCO~OBJNR AND
                              RPSCO~VORGA EQ 'SDOR'      AND
                              RPSCO~VERSN EQ '000'       AND
                              RPSCO~BELTP EQ '2'         AND
                              ( RPSCO~ACPOS EQ '500' OR
                                RPSCO~ACPOS EQ '600' )
          GROUP BY PSPNR",GJAHR
          INTO TABLE @DATA(LT_ACTUAL).
      ELSE.
        LV_SO = ABAP_FALSE.
        SELECT PRPS~PSPNR,
*             RPSCO~GJAHR,
               SUM( RPSCO~WLP01 + RPSCO~WLP02 + RPSCO~WLP03 +
                    RPSCO~WLP04 + RPSCO~WLP05 + RPSCO~WLP06 +
                    RPSCO~WLP07 + RPSCO~WLP08 + RPSCO~WLP09 +
                    RPSCO~WLP10 + RPSCO~WLP11 + RPSCO~WLP12 +
                    RPSCO~WLP13 + RPSCO~WLP14 + RPSCO~WLP15 +
                    RPSCO~WLP16
                   ) AS SUM_DATA
          FROM @LT_PS AS A
          INNER JOIN PRPS  ON A~PS_PSP_PNR EQ PRPS~PSPNR
          INNER JOIN RPSCO ON PRPS~OBJNR  EQ RPSCO~OBJNR AND
                              RPSCO~VORGA EQ 'KABG'      AND
                              RPSCO~VERSN EQ '000'       AND
                              RPSCO~BELTP EQ '2'         AND
                              ( RPSCO~ACPOS EQ '500' OR
                                RPSCO~ACPOS EQ '600' )
          GROUP BY PSPNR",GJAHR
          INTO TABLE @DATA(LT_ACTUAL_BILL).
      ENDIF.
      DATA : LV_CHECK TYPE P DECIMALS 2,
             LV_YEAR  TYPE GJAHR.

      LOOP AT GT_ITEM INTO DATA(LS_ITEM) WHERE PS_PSP_PNR IS NOT INITIAL.
        CLEAR : LV_CHECK .

        CALL FUNCTION 'Z_SDSFI_GET_PERIOD_FISCAL_YEAR'
          EXPORTING
            I_BUKRS = '1000'
            I_DATUM = LS_ITEM-EDATU
          IMPORTING
            E_GJAHR = LV_YEAR.

        READ TABLE LT_PLAN INTO DATA(LS_PLAN)
        WITH KEY PSPNR = LS_ITEM-PS_PSP_PNR.
*                 GJAHR = LV_YEAR.

        IF LV_SO EQ ABAP_TRUE.
          READ TABLE LT_ACTUAL INTO DATA(LS_ACTUAL)
          WITH KEY PSPNR = LS_ITEM-PS_PSP_PNR.
*                 GJAHR = LV_YEAR.

          LV_CHECK = ABS( LS_PLAN-SUM_DATA ) - ABS( LS_ACTUAL-SUM_DATA ).
        ELSE.
          READ TABLE LT_ACTUAL INTO DATA(LS_ACTUAL_BILL)
          WITH KEY PSPNR = LS_ITEM-PS_PSP_PNR.
*                 GJAHR = LV_YEAR.

          READ TABLE LT_PRICE INTO DATA(LS_PRICE)
          WITH KEY VBELN = LS_ITEM-VBELN
                   KPOSN = LS_ITEM-POSNR.

          LV_CHECK = ABS( LS_PLAN-SUM_DATA ) - ( ABS( LS_ACTUAL_BILL-SUM_DATA ) + ( LS_ITEM-NETWR - LS_PRICE-KWERT ) ) .
        ENDIF.

        IF LV_CHECK LT 0.
          R = TEXT-E01.
        ENDIF.
        CLEAR : LS_PLAN,LS_ACTUAL.
      ENDLOOP.
    ENDIF.

    SELECT *
      FROM ZSDSMMC009
      INTO TABLE @DATA(LT_ZSDSMMC009).
    IF SY-SUBRC = 0.
      SORT LT_ZSDSMMC009 BY FAN_COIL.
    ENDIF.

    SELECT *
      FROM ZSDSCAC009
      INTO TABLE @DATA(LT_CAC009)
      WHERE PROCESS LIKE 'CHECK_%'
        AND STATU   EQ @ABAP_TRUE.
    IF SY-SUBRC = 0.
      LOOP AT LT_CAC009 INTO DATA(LS_CAC009).
        CASE LS_CAC009-PROCESS.
          WHEN 'CHECK_INSU'.
            MOVE ABAP_TRUE TO LV_CHK_INSU.
          WHEN 'CHECK_MOD'.
            MOVE ABAP_TRUE TO LV_CHK_MOD.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    LOOP AT GT_ITEM INTO LS_ITEM
                    WHERE VBELN = LWA_HEADER-VBELN.

      "Check Case Incorrect Modify
      IF LV_CHK_MOD = ABAP_TRUE.
        IF LS_ITEM-NETWR = 0.
          READ TABLE GT_KONV INTO DATA(LS_KONV)
                             WITH KEY KPOSN = LS_ITEM-POSNR
                                      KSCHL = 'ZPS4'.
          IF SY-SUBRC = 0.
            IF LS_KONV-KWERT <> 0.
              LV_ERROR = ABAP_TRUE.
              R = TEXT-E02.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      "Check Case Material Insulation
      IF LV_CHK_INSU = ABAP_TRUE.
        READ TABLE LT_ZSDSMMC009 INTO DATA(LS_ZSDSMMC009)
                                 WITH KEY FAN_COIL = LS_ITEM-MATNR
                                 BINARY SEARCH.
        IF SY-SUBRC = 0.
          READ TABLE GT_ITEM TRANSPORTING NO FIELDS
                             WITH KEY
                              VBELN = LS_ITEM-VBELN
                              MATNR = LS_ZSDSMMC009-INSULATION.
          IF SY-SUBRC <> 0.
            LV_ERROR = ABAP_TRUE.
            R = TEXT-E03.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
