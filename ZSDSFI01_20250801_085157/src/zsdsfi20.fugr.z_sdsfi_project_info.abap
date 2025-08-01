FUNCTION Z_SDSFI_PROJECT_INFO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_WBS) TYPE  ZSDSFIS184_TT OPTIONAL
*"  EXPORTING
*"     VALUE(ET_WBS_INFO) TYPE  ZSDSFIS185_TT
*"----------------------------------------------------------------------

  DATA : BEGIN OF LS_PS,
           PSPNR TYPE PRPS-PSPNR,
         END OF LS_PS.
  DATA : LT_PS LIKE HASHED TABLE OF LS_PS WITH UNIQUE KEY PSPNR.

  DATA LV_TABIX TYPE SY-TABIX.

  SELECT DISTINCT PRPS~PSPHI
    FROM  PRPS

  WHERE PRPS~PSPNR IN @IT_WBS
   INTO TABLE @DATA(LT_TMP).


  IF LT_TMP IS NOT INITIAL.
    SELECT A~PSPNR,
           A~PSPHI,
           A~POSID,
           A~POST1,
           RPSCO~GJAHR,
           SUM( RPSCO~WLP01 + RPSCO~WLP02 + RPSCO~WLP03 +
                RPSCO~WLP04 + RPSCO~WLP05 + RPSCO~WLP06 +
                RPSCO~WLP07 + RPSCO~WLP08 + RPSCO~WLP09 +
                RPSCO~WLP10 + RPSCO~WLP11 + RPSCO~WLP12 +
                RPSCO~WLP13 + RPSCO~WLP14 + RPSCO~WLP15 +
                RPSCO~WLP16
               ) AS SUM_DATA
      FROM  @LT_TMP AS B
      INNER JOIN  PRPS AS A ON B~PSPHI = A~PSPHI
      LEFT JOIN RPSCO ON A~OBJNR EQ RPSCO~OBJNR
      AND RPSCO~VERSN = '000'
      AND RPSCO~VORGA = 'RKP1'
      AND RPSCO~BELTP = '1'
       GROUP BY A~PSPNR,A~PSPHI,A~POSID,A~POST1,RPSCO~GJAHR
       ORDER BY A~PSPNR
      INTO TABLE @ET_WBS_INFO.

    LT_PS =  CORRESPONDING #( ET_WBS_INFO  DISCARDING DUPLICATES ).
*--------------------------------------------------------------------*
* Plan Revenue
*--------------------------------------------------------------------*
    SELECT PRPS~PSPNR,
           RPSCO~GJAHR,
           SUM( RPSCO~WLP01 + RPSCO~WLP02 + RPSCO~WLP03 +
                RPSCO~WLP04 + RPSCO~WLP05 + RPSCO~WLP06 +
                RPSCO~WLP07 + RPSCO~WLP08 + RPSCO~WLP09 +
                RPSCO~WLP10 + RPSCO~WLP11 + RPSCO~WLP12 +
                RPSCO~WLP13 + RPSCO~WLP14 + RPSCO~WLP15 +
                RPSCO~WLP16
               ) AS SUM_DATA
      FROM @LT_PS AS A
      INNER JOIN PRPS  ON A~PSPNR EQ PRPS~PSPNR
      INNER JOIN RPSCO ON PRPS~OBJNR  EQ RPSCO~OBJNR AND
                          RPSCO~VORGA EQ 'RKP5'      AND
                          RPSCO~VERSN EQ '000'       AND
                          RPSCO~BELTP EQ '2'         AND
                          ( RPSCO~ACPOS EQ '500' OR
                            RPSCO~ACPOS EQ '600' )
      GROUP BY PRPS~PSPNR,RPSCO~GJAHR
      INTO TABLE @DATA(LT_PLAN).
*--------------------------------------------------------------------*
* Actule Revenue
*--------------------------------------------------------------------*
    SELECT PRPS~PSPNR,
           RPSCO~GJAHR,
           SUM( RPSCO~WLP01 + RPSCO~WLP02 + RPSCO~WLP03 +
                RPSCO~WLP04 + RPSCO~WLP05 + RPSCO~WLP06 +
                RPSCO~WLP07 + RPSCO~WLP08 + RPSCO~WLP09 +
                RPSCO~WLP10 + RPSCO~WLP11 + RPSCO~WLP12 +
                RPSCO~WLP13 + RPSCO~WLP14 + RPSCO~WLP15 +
                RPSCO~WLP16
               ) AS SUM_DATA
      FROM @LT_PS AS A
      INNER JOIN PRPS  ON A~PSPNR EQ PRPS~PSPNR
      INNER JOIN RPSCO ON PRPS~OBJNR  EQ RPSCO~OBJNR AND
                          RPSCO~VORGA EQ 'KABG'      AND
                          RPSCO~VERSN EQ '000'       AND
                          RPSCO~BELTP EQ '2'         AND
                          ( RPSCO~ACPOS EQ '500' OR
                            RPSCO~ACPOS EQ '600' )
      GROUP BY PRPS~PSPNR,RPSCO~GJAHR
      INTO TABLE @DATA(LT_ACTUAL).
*--------------------------------------------------------------------*
* Modify Data
*--------------------------------------------------------------------*
    LOOP AT ET_WBS_INFO INTO DATA(GS_RESULT).
      LV_TABIX = SY-TABIX.
      READ TABLE LT_PLAN INTO DATA(LS_PLAN)
      WITH KEY PSPNR = GS_RESULT-PSPNR
               GJAHR = GS_RESULT-GJAHR.
      IF SY-SUBRC = 0.
        GS_RESULT-PLREV = LS_PLAN-SUM_DATA.
      ENDIF.

      READ TABLE LT_ACTUAL INTO DATA(LS_ACTUAL)
      WITH KEY PSPNR = GS_RESULT-PSPNR
               GJAHR = GS_RESULT-GJAHR.
      IF SY-SUBRC = 0.
        GS_RESULT-AUREV = LS_ACTUAL-SUM_DATA.
      ENDIF.

      MODIFY ET_WBS_INFO FROM GS_RESULT INDEX LV_TABIX
                                 TRANSPORTING PLREV AUREV.
    ENDLOOP.
  ENDIF.
*--------------------------------------------------------------------*


ENDFUNCTION.
