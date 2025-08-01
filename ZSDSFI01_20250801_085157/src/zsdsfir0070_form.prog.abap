*&---------------------------------------------------------------------*
*& Include          ZSDSFII0070_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*

FORM GET_DATA .

  SELECT *
    INTO TABLE GT_WITH_ITEM
    FROM WITH_ITEM
   WHERE BUKRS    EQ    P_BUKRS
     AND GJAHR    IN    S_GJAHR
     AND BELNR    IN    S_BELNR
     AND ( ( WT_ACCO IN S_LIFNR AND KOART EQ 'K' ) OR ( WT_ACCO IN S_KUNNR AND KOART EQ 'D' ) )
     AND AUGBL    IN    S_AUGBL
     AND AUGDT    IN    S_AUGDT
     AND CTISSUEDATE IN S_WHTDT  "Add  by wantanee
     AND CTNUMBER IN    S_CTNUM
     AND CTNUMBER NE    SPACE
     AND WITHT    IN    S_WITHT
     AND WT_WITHCD IN   S_WTHCD
     AND QSREC    IN    S_QSREC.
*     AND wt_withcd NE   space.  "Remove by wantanee

  SELECT *
    INTO TABLE GT_LFA1
    FROM LFA1
   WHERE LIFNR IN S_LIFNR.

  SELECT *
    INTO TABLE GT_KNA1
    FROM KNA1
   WHERE KUNNR IN S_KUNNR.

  SELECT *
    INTO TABLE GT_T059ZT
    FROM T059ZT
   WHERE LAND1  EQ  'TH'
     AND SPRAS  EQ  SY-LANGU.

  SELECT LIFNR BUKRS WITHT QSREC
  INTO TABLE GT_LFBW
  FROM LFBW.

  SELECT KUNNR BUKRS WITHT WT_WTSTCD
  INTO TABLE GT_KNBW
  FROM KNBW.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  prepare_data
*&---------------------------------------------------------------------*

FORM PREPARE_DATA.
  DATA: GV_STCD3 TYPE LFA1-STCD3.
  DATA: GV_NO(20)          TYPE C,
        GV_SOI(30)         TYPE C,
        GV_STREET(30)      TYPE C,
        GV_SUBDISTRICT(30) TYPE C,
        GV_DISTRICT(30)    TYPE C,
        GV_CITY            TYPE ADRC-CITY1,
        GV_POST_CODE       TYPE ADRC-POST_CODE1.

  DATA: LV_PSTLZ       TYPE BSEC-PSTLZ,
        LV_ORT01       TYPE BSEC-ORT01,
        LV_STRAS       TYPE BSEC-STRAS,
        LV_STRING(200) TYPE C.
  DATA: LV_TAX_RATE TYPE I.
  DATA: LV_LFBW_QSREC TYPE LFBW-QSREC.


  LOOP AT GT_WITH_ITEM INTO GS_WITH_ITEM.
    CLEAR: GV_STCD3,LV_PSTLZ,LV_ORT01,LV_STRAS,LV_STRING,GS_SUM_PND1_2,LV_TAX_RATE,LV_LFBW_QSREC.
    READ TABLE GT_WITH_ITEM INTO GS_WITH_ITMK WITH KEY KOART = 'K'
                                                       BELNR = GS_WITH_ITEM-BELNR
                                                       GJAHR = GS_WITH_ITEM-GJAHR.

    READ TABLE GT_WITH_ITEM INTO GS_WITH_ITMD WITH KEY KOART = 'D'
                                                       BELNR = GS_WITH_ITEM-BELNR
                                                       GJAHR = GS_WITH_ITEM-GJAHR.

    READ TABLE GT_LFA1 INTO GS_LFA1 WITH KEY LIFNR = GS_WITH_ITMK-WT_ACCO.

    READ TABLE GT_KNA1 INTO GS_KNA1 WITH KEY KUNNR = GS_WITH_ITMD-WT_ACCO.

    READ TABLE GT_T059ZT INTO GS_T059ZT WITH KEY WITHT = GS_WITH_ITEM-WITHT
                                                 WT_WITHCD = GS_WITH_ITEM-WT_WITHCD.

*            stcd3       TYPE  lfa1-stcd3,  "Tax number3
*          street      TYPE  adrc-street,  "Street/House number
*          str_suppl        TYPE adrc-str_suppl1, "Street 2
*          str_suppl2       TYPE adrc-str_suppl2, "Street 3
*          str_suppl3       TYPE adrc-str_suppl3, "Street 4
*          location         TYPE adrc-location,   "Street 5
*          city2            TYPE adrc-city2,  "Distric
*          city1            TYPE adrc-city1,  "City
*          post_code1       TYPE adrc-post_code1, "Post code

    IF GS_WITH_ITEM-KOART = 'K' AND GS_WITH_ITEM-WT_ACCO(2) NE 'OT'.
*      CONCATENATE GS_LFA1-name1 GS_LFA1-name2 GS_LFA1-name3 GS_LFA1-name4 INTO gv_name SEPARATED BY space.
      GV_STCD3 = GS_LFA1-STCD3.
      PERFORM GET_VENDER USING    GS_LFA1-LIFNR GS_LFA1-ADRNR
                         CHANGING GV_NAME GV_NO GV_SOI GV_STREET GV_SUBDISTRICT GV_DISTRICT
                                  GV_CITY GV_POST_CODE.

      READ TABLE GT_LFBW INTO GS_LFBW WITH KEY LIFNR = GS_LFA1-LIFNR
                                               WITHT = GS_WITH_ITEM-WITHT.
      IF SY-SUBRC EQ 0.
        IF GS_LFBW-QSREC NE GS_WITH_ITEM-QSREC.
          GS_WITH_ITEM-QSREC = GS_LFBW-QSREC.
        ENDIF.
      ENDIF.



    ELSEIF GS_WITH_ITEM-KOART = 'D' AND GS_WITH_ITEM-WT_ACCO(2) NE 'OT'.
      " CONCATENATE GS_KNA1-name1 GS_KNA1-name2 GS_KNA1-name3 GS_KNA1-name4 INTO gv_name SEPARATED BY space.
      GV_STCD3 = GS_KNA1-STCD3.
      PERFORM GET_VENDER USING    GS_KNA1-KUNNR GS_KNA1-ADRNR
                         CHANGING GV_NAME GV_NO GV_SOI GV_STREET GV_SUBDISTRICT GV_DISTRICT
                                  GV_CITY GV_POST_CODE.

      "Add by Wantanee CH03
      READ TABLE GT_KNBW INTO GS_KNBW WITH KEY KUNNR = GS_KNA1-KUNNR
                                               WITHT = GS_WITH_ITEM-WITHT.
      IF SY-SUBRC          EQ 0 AND
         GS_KNBW-WT_WTSTCD IS NOT INITIAL.
        IF GS_KNBW-WT_WTSTCD NE GS_WITH_ITEM-QSREC.
          GS_WITH_ITEM-QSREC = GS_KNBW-WT_WTSTCD.
        ENDIF.
      ENDIF.
      "End dd by Wantanee CH03
    ELSE.
      SELECT NAME1 NAME2 NAME3 NAME4
               J_1KFTBUS  "Add by Wantanee 20150831
               STCD3
               PSTLZ ORT01 STRAS
        INTO (GV_NAME1, GV_NAME2, GV_NAME3, GV_NAME4, GV_J_1KFTBUS, GV_STCD3,LV_PSTLZ,LV_ORT01,LV_STRAS)
        FROM BSEC
       WHERE BELNR = GS_WITH_ITEM-BELNR
         AND GJAHR = GS_WITH_ITEM-GJAHR.
        IF SY-SUBRC EQ 0.
          CONCATENATE GV_NAME1 GV_NAME2 GV_NAME3 GV_NAME4 INTO GV_NAME SEPARATED BY SPACE.
          CONCATENATE LV_STRAS LV_ORT01 INTO LV_STRING SEPARATED BY SPACE.

          GV_NO = LV_STRING+0(20).
          GV_SOI = LV_STRING+20(30).
          GV_STREET = LV_STRING+50(30).
          GV_SUBDISTRICT = LV_STRING+80(30).
          GV_DISTRICT = LV_STRING+120(30).
          GV_CITY  = LV_STRING+150(40).
          GV_POST_CODE  = LV_PSTLZ.

          "Add by Wantanee 20150831
          IF GV_J_1KFTBUS+0(2) NE SPACE.
            IF GS_WITH_ITEM-QSREC NE GV_J_1KFTBUS+0(2).
              GS_WITH_ITEM-QSREC = GV_J_1KFTBUS+0(2).
            ENDIF.
          ENDIF.

          "End "Add by Wantanee 20150831

        ENDIF.
      ENDSELECT.
    ENDIF.

    ""Add by Wantanee 20150831
    IF GS_WITH_ITEM-AUGDT EQ '00000000'.
      GS_WITH_ITEM-AUGDT = GS_WITH_ITEM-CTISSUEDATE.
      "End "Add by Wantanee 20150831
    ELSEIF GS_WITH_ITEM-AUGDT NE GS_WITH_ITEM-CTISSUEDATE.
      GS_WITH_ITEM-AUGDT = GS_WITH_ITEM-CTISSUEDATE.
      "Add by Wantanee 20151005
    ENDIF.
    IF GS_WITH_ITEM-AUGBL  EQ SPACE.
      SELECT SINGLE AUGBL
      INTO (GS_WITH_ITEM-AUGBL)
      FROM WITH_ITEM
      WHERE BELNR = GS_WITH_ITEM-BELNR
      AND GJAHR = GS_WITH_ITEM-GJAHR
      AND AUGBL NE SPACE.

    ENDIF.




    "End Add by Wantanee 20151005

    IF  NOT ( GS_WITH_ITEM-WITHT = '11' OR GS_WITH_ITEM-WITHT = '12'
      OR GS_WITH_ITEM-WITHT = '13' OR GS_WITH_ITEM-WITHT = '14' ) .
      GS_WITH_ITEM-WT_QSSHH   =   GS_WITH_ITEM-WT_QSSHH * -1.
      GS_WITH_ITEM-WT_QSSHB   =   GS_WITH_ITEM-WT_QSSHB * -1.
      GS_WITH_ITEM-WT_QSSHHC  =   GS_WITH_ITEM-WT_QSSHHC * -1.
      GS_WITH_ITEM-WT_QSSHBC  =   GS_WITH_ITEM-WT_QSSHBC * -1.
      GS_WITH_ITEM-WT_QBSHH   =   GS_WITH_ITEM-WT_QBSHH * -1.
      GS_WITH_ITEM-WT_QBSHB   =   GS_WITH_ITEM-WT_QBSHB * -1.
      GS_WITH_ITEM-WT_QSFHH   =   GS_WITH_ITEM-WT_QSFHH * -1.
      GS_WITH_ITEM-WT_QSFHB   =   GS_WITH_ITEM-WT_QSFHB * -1.
    ENDIF.

*    IF  GS_WITH_ITEM-WITHT <> '11'.
*
*      GS_WITH_ITEM-WT_QSSHH   =   GS_WITH_ITEM-WT_QSSHH * -1.
*      GS_WITH_ITEM-WT_QSSHB   =   GS_WITH_ITEM-WT_QSSHB * -1.
*      GS_WITH_ITEM-WT_QSSHHC  =   GS_WITH_ITEM-WT_QSSHHC * -1.
*      GS_WITH_ITEM-WT_QSSHBC  =   GS_WITH_ITEM-WT_QSSHBC * -1.
*      GS_WITH_ITEM-WT_QBSHH   =   GS_WITH_ITEM-WT_QBSHH * -1.
*      GS_WITH_ITEM-WT_QBSHB   =   GS_WITH_ITEM-WT_QBSHB * -1.
*      GS_WITH_ITEM-WT_QSFHH   =   GS_WITH_ITEM-WT_QSFHH * -1.
*      GS_WITH_ITEM-WT_QSFHB   =   GS_WITH_ITEM-WT_QSFHB * -1.
*    ENDIF.




    MOVE: GS_WITH_ITEM-BELNR      TO    GS_RESULT-BELNR,
          GS_WITH_ITEM-GJAHR      TO    GS_RESULT-GJAHR,
          GS_WITH_ITEM-BUZEI      TO    GS_RESULT-BUZEI,
          GS_WITH_ITEM-WITHT      TO    GS_RESULT-WITHT,
          GS_WITH_ITEM-WT_WITHCD  TO    GS_RESULT-WT_WITHCD,
          GS_T059ZT-TEXT40        TO    GS_RESULT-WITHCD_DS,
          GS_WITH_ITEM-WT_QSSHH   TO    GS_RESULT-WT_QSSHH,
          GS_WITH_ITEM-WT_QSSHB   TO    GS_RESULT-WT_QSSHB,
          GS_WITH_ITEM-WT_BASMAN  TO    GS_RESULT-WT_BASMAN,
          GS_WITH_ITEM-WT_QSSHHC  TO    GS_RESULT-WT_QSSHHC,
          GS_WITH_ITEM-WT_QSSHBC  TO    GS_RESULT-WT_QSSHBC,
          GS_WITH_ITEM-WT_QBSHH   TO    GS_RESULT-WT_QBSHH,
          GS_WITH_ITEM-WT_QBSHB   TO    GS_RESULT-WT_QBSHB,
          GS_WITH_ITEM-WT_AMNMAN  TO    GS_RESULT-WT_AMNMAN,
          GS_WITH_ITEM-WT_STAT    TO    GS_RESULT-WT_STAT,
          GS_WITH_ITEM-WT_QSFHH   TO    GS_RESULT-WT_QSFHH,
          GS_WITH_ITEM-WT_QSFHB   TO    GS_RESULT-WT_QSFHB,
          GS_WITH_ITEM-WT_WTEXMN  TO    GS_RESULT-WT_WTEXMN,
          GS_WITH_ITEM-KOART      TO    GS_RESULT-KOART,
          GS_WITH_ITEM-WT_ACCO    TO    GS_RESULT-WT_ACCO,
          GV_NAME                 TO    GS_RESULT-NAME,
          GS_WITH_ITEM-HKONT      TO    GS_RESULT-HKONT,
          GS_WITH_ITEM-QSREC      TO    GS_RESULT-QSREC,
          GS_WITH_ITEM-AUGBL      TO    GS_RESULT-AUGBL,
          GS_WITH_ITEM-AUGDT      TO    GS_RESULT-AUGDT,
          GS_WITH_ITEM-CTNUMBER   TO    GS_RESULT-CTNUMBER ,
          GV_STCD3                TO    GS_RESULT-STCD3 ,
          GV_NO                   TO    GS_RESULT-HOUSE_NO ,
          GV_SOI                  TO    GS_RESULT-SOI ,
          GV_STREET               TO    GS_RESULT-STREET ,
          GV_SUBDISTRICT          TO    GS_RESULT-SUB_DISTRICT ,
          GV_DISTRICT             TO    GS_RESULT-DISTRICT ,
          GV_CITY                 TO    GS_RESULT-CITY1 ,
          GV_POST_CODE            TO    GS_RESULT-POST_CODE1 ,
          GS_T059ZT-TEXT40+0(2)   TO    GS_RESULT-TAX_RATE,
          GS_T059ZT-TEXT40+4(36)   TO    GS_RESULT-TAX_TYPE.

    APPEND GS_RESULT TO GT_RESULT.

    IF NOT R_PND3 IS INITIAL.
      IF GS_WITH_ITEM-QSREC EQ '03'.
        LV_TAX_RATE = GS_T059ZT-TEXT40+0(2).

        MOVE: GS_WITH_ITEM-QSREC      TO    GS_SUM_PND3-QSREC,
              GV_STCD3                TO    GS_SUM_PND3-STCD3 ,
              GV_NAME                 TO    GS_SUM_PND3-NAME,
              GV_NO                   TO    GS_SUM_PND3-HOUSE_NO ,
              GV_SOI                  TO    GS_SUM_PND3-SOI ,
              GV_STREET               TO    GS_SUM_PND3-STREET ,
              GV_SUBDISTRICT          TO    GS_SUM_PND3-SUB_DISTRICT ,
              GV_DISTRICT             TO    GS_SUM_PND3-DISTRICT ,
              GV_CITY                 TO    GS_SUM_PND3-CITY1 ,
              GV_POST_CODE            TO    GS_SUM_PND3-POST_CODE1 ,
              GS_WITH_ITEM-AUGDT      TO    GS_SUM_PND3-AUGDT,
              GS_T059ZT-TEXT40+4(36)  TO    GS_SUM_PND3-TAX_TYPE,
              LV_TAX_RATE             TO    GS_SUM_PND3-TAX_RATE,
              GS_WITH_ITEM-WT_QSSHH   TO    GS_SUM_PND3-WT_QSSHH,
              GS_WITH_ITEM-WT_QBSHH   TO    GS_SUM_PND3-WT_QBSHH.




        COLLECT GS_SUM_PND3 INTO GT_SUM_PND3.
      ENDIF.
    ELSEIF NOT R_PND53 IS INITIAL.
      IF GS_WITH_ITEM-QSREC EQ '53'.
        LV_TAX_RATE = GS_T059ZT-TEXT40+0(2).

        MOVE: GS_WITH_ITEM-QSREC      TO    GS_SUM_PND53-QSREC,
              GV_STCD3                TO    GS_SUM_PND53-STCD3 ,
              GV_NAME                 TO    GS_SUM_PND53-NAME,
              GV_NO                   TO    GS_SUM_PND53-HOUSE_NO ,
              GV_SOI                  TO    GS_SUM_PND53-SOI ,
              GV_STREET               TO    GS_SUM_PND53-STREET ,
              GV_SUBDISTRICT          TO    GS_SUM_PND53-SUB_DISTRICT ,
              GV_DISTRICT             TO    GS_SUM_PND53-DISTRICT ,
              GV_CITY                 TO    GS_SUM_PND53-CITY1 ,
              GV_POST_CODE            TO    GS_SUM_PND53-POST_CODE1 ,
              GS_WITH_ITEM-AUGDT      TO    GS_SUM_PND53-AUGDT,
              GS_T059ZT-TEXT40+4(36)  TO    GS_SUM_PND53-TAX_TYPE,
              LV_TAX_RATE             TO    GS_SUM_PND53-TAX_RATE,
              GS_WITH_ITEM-WT_QSSHH   TO    GS_SUM_PND53-WT_QSSHH,
              GS_WITH_ITEM-WT_QBSHH   TO    GS_SUM_PND53-WT_QBSHH.




        COLLECT GS_SUM_PND53 INTO GT_SUM_PND53.
      ENDIF.
    ELSEIF NOT R_PND1 IS INITIAL.
      IF GS_WITH_ITEM-QSREC EQ '01'.
        LV_TAX_RATE = GS_T059ZT-TEXT40+0(2).
        IF GS_WITH_ITEM-WT_WITHCD  EQ '31' OR GS_WITH_ITEM-WT_WITHCD  EQ '34'.
          GS_SUM_PND1_2-MONEY_TYPE = '3'.
        ELSE.
          GS_SUM_PND1_2-MONEY_TYPE = '4'.
        ENDIF.

        MOVE: GS_WITH_ITEM-QSREC      TO    GS_SUM_PND1_2-QSREC,
              GV_STCD3                TO    GS_SUM_PND1_2-STCD3 ,
              GV_NAME                 TO    GS_SUM_PND1_2-NAME,
              GS_WITH_ITEM-AUGDT      TO    GS_SUM_PND1_2-AUGDT,
              GS_T059ZT-TEXT40+4(36)  TO    GS_SUM_PND1_2-TAX_TYPE,
              LV_TAX_RATE             TO    GS_SUM_PND1_2-TAX_RATE,
              GS_WITH_ITEM-WT_QSSHH   TO    GS_SUM_PND1_2-WT_QSSHH,
              GS_WITH_ITEM-WT_QBSHH   TO    GS_SUM_PND1_2-WT_QBSHH.




        COLLECT GS_SUM_PND1_2 INTO GT_SUM_PND1_2.
      ENDIF.
    ELSEIF NOT R_PND2 IS INITIAL.
      IF GS_WITH_ITEM-QSREC EQ '02'.
        LV_TAX_RATE = GS_T059ZT-TEXT40+0(2).
        IF GS_WITH_ITEM-WT_WITHCD  EQ '09' .
          GS_SUM_PND1_2-MONEY_TYPE = '2'.
        ELSEIF GS_WITH_ITEM-WT_WITHCD  EQ '10'.
          GS_SUM_PND1_2-MONEY_TYPE = '3'.
        ENDIF.

        MOVE: GS_WITH_ITEM-QSREC      TO    GS_SUM_PND1_2-QSREC,
              GV_STCD3                TO    GS_SUM_PND1_2-STCD3 ,
              GV_NAME                 TO    GS_SUM_PND1_2-NAME,
              GS_WITH_ITEM-AUGDT      TO    GS_SUM_PND1_2-AUGDT,
              GS_T059ZT-TEXT40+4(36)  TO    GS_SUM_PND1_2-TAX_TYPE,
              LV_TAX_RATE             TO    GS_SUM_PND1_2-TAX_RATE,
              GS_WITH_ITEM-WT_QSSHH   TO    GS_SUM_PND1_2-WT_QSSHH,
              GS_WITH_ITEM-WT_QBSHH   TO    GS_SUM_PND1_2-WT_QBSHH.




        COLLECT GS_SUM_PND1_2 INTO GT_SUM_PND1_2.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.  "End of Form prepare_data
*&---------------------------------------------------------------------*
*&      Form  Export_pnd1
*&---------------------------------------------------------------------*

FORM EXPORT_PND1.
  DATA: LV_LINE_NO TYPE I.
  LOOP AT GT_SUM_PND1_2 INTO GS_SUM_PND1_2 WHERE WT_QSSHH NE 0.


    LV_LINE_NO = LV_LINE_NO + 1.
    GS_PND1-MONEY_TYPE = GS_SUM_PND1_2-MONEY_TYPE.
    GS_PND1-NO = LV_LINE_NO.
    GS_PND1-TAX_NUMBER = GS_SUM_PND1_2-STCD3.
    GS_PND1-TITLE_NAME = SPACE.
    GS_PND1-NAME_TH = GS_SUM_PND1_2-NAME.
    GS_PND1-SURNAME_TH = SPACE.
    GS_PND1-PAY_DATE = GS_SUM_PND1_2-AUGDT.
    GS_PND1-AMOUNT = GS_SUM_PND1_2-WT_QSSHH.
    GS_PND1-TAX_AMT = GS_SUM_PND1_2-WT_QBSHH.
    GS_PND1-TAX_CON = '1'.


    CONDENSE : GS_PND1-NO ,GS_PND1-MONEY_TYPE,GS_PND1-AMOUNT,GS_PND1-TAX_AMT.



    CONCATENATE  GS_PND1-MONEY_TYPE
                 GS_PND1-NO
                 GS_PND1-TAX_NUMBER
                 GS_PND1-TITLE_NAME
                 GS_PND1-NAME_TH
                 GS_PND1-SURNAME_TH
                 GS_PND1-PAY_DATE
                 GS_PND1-AMOUNT
                 GS_PND1-TAX_AMT
                 GS_PND1-TAX_CON
                 INTO GS_DFILE-TEXT SEPARATED BY '|' .
    APPEND GS_DFILE TO GT_DFILE .


  ENDLOOP.

ENDFORM.  "End of Form prepare_data
*&---------------------------------------------------------------------*
*&      Form  Export_pnd1
*&---------------------------------------------------------------------*

FORM EXPORT_PND2.
  DATA: LV_LINE_NO TYPE I.
  LOOP AT GT_SUM_PND1_2 INTO GS_SUM_PND1_2 WHERE WT_QSSHH NE 0.


    LV_LINE_NO = LV_LINE_NO + 1.
    GS_PND2-MONEY_TYPE = GS_SUM_PND1_2-MONEY_TYPE.
    GS_PND2-NO = LV_LINE_NO.
    GS_PND2-TAX_NUMBER = GS_SUM_PND1_2-STCD3.
    GS_PND2-TITLE_NAME = SPACE.
    GS_PND2-NAME_TH = GS_SUM_PND1_2-NAME.
    GS_PND2-SURNAME_TH = SPACE.
    GS_PND2-BOOK_BANK = SPACE.
    GS_PND2-PAY_DATE = GS_SUM_PND1_2-AUGDT.
    GS_PND2-TAX_RATE = GS_SUM_PND1_2-TAX_RATE.
    GS_PND2-AMOUNT = GS_SUM_PND1_2-WT_QSSHH.
    GS_PND2-TAX_AMT = GS_SUM_PND1_2-WT_QBSHH.
    GS_PND2-TAX_CON = '1'.


    CONDENSE : GS_PND2-NO ,GS_PND2-MONEY_TYPE,GS_PND2-AMOUNT,GS_PND2-TAX_AMT.



    CONCATENATE  GS_PND2-MONEY_TYPE
                 GS_PND2-NO
                 GS_PND2-TAX_NUMBER
                 GS_PND2-TITLE_NAME
                 GS_PND2-NAME_TH
                 GS_PND2-SURNAME_TH
                 GS_PND2-BOOK_BANK
                 GS_PND2-PAY_DATE
                 GS_PND2-TAX_RATE
                 GS_PND2-AMOUNT
                 GS_PND2-TAX_AMT
                 GS_PND2-TAX_CON
                 INTO GS_DFILE-TEXT SEPARATED BY '|' .
    APPEND GS_DFILE TO GT_DFILE .


  ENDLOOP.

ENDFORM.  "End of Form prepare_data
*&---------------------------------------------------------------------*
*&      Form  Export_pnd3
*&---------------------------------------------------------------------*

FORM EXPORT_PND3.
  DATA: LV_LINE_NO TYPE I.
  LOOP AT GT_SUM_PND3 INTO GS_SUM_PND3 WHERE WT_QSSHH NE 0.
    LV_LINE_NO = LV_LINE_NO + 1.

    GS_PND3-NO = LV_LINE_NO.
    GS_PND3-TAX_NUMBER = GS_SUM_PND3-STCD3.
    GS_PND3-BRANCH_NO = SPACE.
    GS_PND3-TITLE_NAME = SPACE.
    GS_PND3-NAME_TH = GS_SUM_PND3-NAME.
    GS_PND3-SURNAME_TH = SPACE.
    GS_PND3-MOO_BAN = SPACE.
    GS_PND3-ROOM_NUMBER = SPACE.
    GS_PND3-FLOOR = SPACE.
    GS_PND3-HOUSE_NUMBER = GS_SUM_PND3-HOUSE_NO.
    GS_PND3-MOO = SPACE.
    GS_PND3-SOI = GS_SUM_PND3-SOI.
    GS_PND3-STREET = GS_SUM_PND3-STREET.
    GS_PND3-SUBDISTRICT = GS_SUM_PND3-SUB_DISTRICT.
    GS_PND3-DISTRICT = GS_SUM_PND3-DISTRICT.
    GS_PND3-CITY = GS_SUM_PND3-CITY1.
    GS_PND3-POSTCODE = GS_SUM_PND3-POST_CODE1.
    GS_PND3-PAY_DATE = GS_SUM_PND3-AUGDT.
    GS_PND3-MONEY_TYPE = GS_SUM_PND3-TAX_TYPE.
    GS_PND3-TAX_RATE = GS_SUM_PND3-TAX_RATE.
    GS_PND3-AMOUNT = GS_SUM_PND3-WT_QSSHH.
    GS_PND3-TAX_AMT = GS_SUM_PND3-WT_QBSHH.
    GS_PND3-TAX_CON = '1'.
    GS_PND3-PAY_DATE_2 = SPACE.
    GS_PND3-MONEY_TYPE_2 = SPACE.
    GS_PND3-TAX_RATE_2 = SPACE.
    GS_PND3-AMOUNT_2 = SPACE.
    GS_PND3-TAX_AMT_2 = SPACE.
    GS_PND3-TAX_CON_2 = SPACE.
    GS_PND3-PAY_DATE_3 = SPACE.
    GS_PND3-MONEY_TYPE_3 = SPACE.
    GS_PND3-TAX_RATE_3 = SPACE.
    GS_PND3-AMOUNT_3 = SPACE.
    GS_PND3-TAX_AMT_3 = SPACE.
    GS_PND3-TAX_CON_3 = SPACE.

    CONDENSE : GS_PND3-NO ,GS_PND3-TAX_RATE,GS_PND3-AMOUNT,GS_PND3-TAX_AMT.



    CONCATENATE  GS_PND3-NO
                 GS_PND3-TAX_NUMBER
                 GS_PND3-BRANCH_NO
                 GS_PND3-TITLE_NAME
                 GS_PND3-NAME_TH
                 GS_PND3-SURNAME_TH
                 GS_PND3-MOO_BAN
                 GS_PND3-ROOM_NUMBER
                 GS_PND3-FLOOR
                 GS_PND3-HOUSE_NUMBER
                 GS_PND3-MOO
                 GS_PND3-SOI
                 GS_PND3-STREET
                 GS_PND3-SUBDISTRICT
                 GS_PND3-DISTRICT
                 GS_PND3-CITY
                 GS_PND3-POSTCODE
                 GS_PND3-PAY_DATE
                 GS_PND3-MONEY_TYPE
                 GS_PND3-TAX_RATE
                 GS_PND3-AMOUNT
                 GS_PND3-TAX_AMT
                 GS_PND3-TAX_CON
                 GS_PND3-PAY_DATE_2
                 GS_PND3-MONEY_TYPE_2
                 GS_PND3-TAX_RATE_2
                 GS_PND3-AMOUNT_2
                 GS_PND3-TAX_AMT_2
                 GS_PND3-TAX_CON_2
                 GS_PND3-PAY_DATE_3
                 GS_PND3-MONEY_TYPE_3
                 GS_PND3-TAX_RATE_3
                 GS_PND3-AMOUNT_3
                 GS_PND3-TAX_AMT_3
                 GS_PND3-TAX_CON_3
                 INTO GS_DFILE-TEXT SEPARATED BY '|' .
    APPEND GS_DFILE TO GT_DFILE .


  ENDLOOP.

ENDFORM.  "End of Form prepare_data

*&---------------------------------------------------------------------*
*&      Form  Export_pnd53
*&---------------------------------------------------------------------*

FORM EXPORT_PND53.
  DATA: LV_LINE_NO TYPE I.
  LOOP AT GT_SUM_PND53 INTO GS_SUM_PND53 WHERE WT_QSSHH NE 0.
    LV_LINE_NO = LV_LINE_NO + 1.

    GS_PND53-NO = LV_LINE_NO.
    GS_PND53-TAX_NUMBER = GS_SUM_PND53-STCD3.
    GS_PND53-BRANCH_NO = SPACE.
    GS_PND53-TITLE_NAME = SPACE.
    GS_PND53-NAME_TH = GS_SUM_PND53-NAME.
    GS_PND53-MOO_BAN = SPACE.
    GS_PND53-ROOM_NUMBER = SPACE.
    GS_PND53-FLOOR = SPACE.
    GS_PND53-HOUSE_NUMBER = GS_SUM_PND53-HOUSE_NO.
    GS_PND53-MOO = SPACE.
    GS_PND53-SOI = GS_SUM_PND53-SOI.
    GS_PND53-STREET = GS_SUM_PND53-STREET.
    GS_PND53-SUBDISTRICT = GS_SUM_PND53-SUB_DISTRICT.
    GS_PND53-DISTRICT = GS_SUM_PND53-DISTRICT.
    GS_PND53-CITY = GS_SUM_PND53-CITY1.
    GS_PND53-POSTCODE = GS_SUM_PND53-POST_CODE1.
    GS_PND53-PAY_DATE = GS_SUM_PND53-AUGDT.
    GS_PND53-MONEY_TYPE = GS_SUM_PND53-TAX_TYPE.
    GS_PND53-TAX_RATE = GS_SUM_PND53-TAX_RATE.
    GS_PND53-AMOUNT = GS_SUM_PND53-WT_QSSHH.
    GS_PND53-TAX_AMT = GS_SUM_PND53-WT_QBSHH.
    GS_PND53-TAX_CON = '1'.
    GS_PND53-PAY_DATE_2 = SPACE.
    GS_PND53-MONEY_TYPE_2 = SPACE.
    GS_PND53-TAX_RATE_2 = SPACE.
    GS_PND53-AMOUNT_2 = SPACE.
    GS_PND53-TAX_AMT_2 = SPACE.
    GS_PND53-TAX_CON_2 = SPACE.
    GS_PND53-PAY_DATE_3 = SPACE.
    GS_PND53-MONEY_TYPE_3 = SPACE.
    GS_PND53-TAX_RATE_3 = SPACE.
    GS_PND53-AMOUNT_3 = SPACE.
    GS_PND53-TAX_AMT_3 = SPACE.
    GS_PND53-TAX_CON_3 = SPACE.

    CONDENSE : GS_PND53-NO ,GS_PND53-TAX_RATE,GS_PND53-AMOUNT,GS_PND53-TAX_AMT.



    CONCATENATE  GS_PND53-NO
                 GS_PND53-TAX_NUMBER
                 GS_PND53-BRANCH_NO
                 GS_PND53-TITLE_NAME
                 GS_PND53-NAME_TH
                 GS_PND53-MOO_BAN
                 GS_PND53-ROOM_NUMBER
                 GS_PND53-FLOOR
                 GS_PND53-HOUSE_NUMBER
                 GS_PND53-MOO
                 GS_PND53-SOI
                 GS_PND53-STREET
                 GS_PND53-SUBDISTRICT
                 GS_PND53-DISTRICT
                 GS_PND53-CITY
                 GS_PND53-POSTCODE
                 GS_PND53-PAY_DATE
                 GS_PND53-MONEY_TYPE
                 GS_PND53-TAX_RATE
                 GS_PND53-AMOUNT
                 GS_PND53-TAX_AMT
                 GS_PND53-TAX_CON
                 GS_PND53-PAY_DATE_2
                 GS_PND53-MONEY_TYPE_2
                 GS_PND53-TAX_RATE_2
                 GS_PND53-AMOUNT_2
                 GS_PND53-TAX_AMT_2
                 GS_PND53-TAX_CON_2
                 GS_PND53-PAY_DATE_3
                 GS_PND53-MONEY_TYPE_3
                 GS_PND53-TAX_RATE_3
                 GS_PND53-AMOUNT_3
                 GS_PND53-TAX_AMT_3
                 GS_PND53-TAX_CON_3
                 INTO GS_DFILE-TEXT SEPARATED BY '|' .
    APPEND GS_DFILE TO GT_DFILE .


  ENDLOOP.

ENDFORM.  "End of Form prepare_data
**&---------------------------------------------------------------------*
**&      Form  VENDER
**&---------------------------------------------------------------------*
FORM GET_VENDER USING    P_LIFNR TYPE LFA1-LIFNR
                         P_ADRNR TYPE LFA1-ADRNR
*                         p_belnr TYPE bsec-belnr
*                         p_gjahr TYPE bsec-gjahr
                  CHANGING VALUE(P_NAME) TYPE C
                           VALUE(P_NO) TYPE C
                           VALUE(P_SOI) TYPE C
                           VALUE(P_STREET) TYPE C
                           VALUE(P_SUBDESTRICT) TYPE C
                           VALUE(P_DESTRICT) TYPE C
                           VALUE(P_CITY) TYPE C
                           VALUE(P_POST_CODE) TYPE ADRC-POST_CODE1.
*                           value(p_tel)  TYPE c
*                           value(p_fax)  TYPE c
*                           value(p_email) TYPE c.



  DATA: P_NAME_THA(200) TYPE C.
  DATA: LV_SPLIT1(20)   TYPE C,
        LV_SPLIT2(30)   TYPE C,
        LV_SPLIT3(30)   TYPE C,
        LV_SPLIT4(30)   TYPE C,
        LV_SPLIT5(30)   TYPE C,
        LV_SPLIT6(30)   TYPE C,
        LV_SPLIT7(30)   TYPE C,
        LV_SPLIT8(30)   TYPE C,
        LV_SPLIT9(30)   TYPE C,
        LV_SPLIT10(30)  TYPE C,
        LV_STRING(200)  TYPE C,
        LV_STRING1(200) TYPE C.

  DATA: LEN TYPE I.
  CLEAR:P_NAME_THA,P_NAME,P_POST_CODE,LEN.",p_email,p_addr.
  CLEAR: LV_SPLIT1,LV_SPLIT2,LV_SPLIT3,LV_STRING.
  DATA: LV_NAME1     TYPE BSEC-NAME1,
        LV_NAME2     TYPE BSEC-NAME2,
        LV_STREET    TYPE BSEC-STRAS,
        LV_CITY2     TYPE BSEC-ORT01,
        LV_POST_CODE TYPE BSEC-PSTLZ.
  CLEAR: LV_NAME1,LV_NAME2,LV_STREET,LV_CITY2,LV_POST_CODE.

  SELECT ADDRNUMBER NAME1 NAME2 NAME3 NAME4
         STREET STR_SUPPL1 STR_SUPPL2 STR_SUPPL3
         LOCATION CITY2  CITY1 POST_CODE1  NATION
         "tel_number fax_number nation

  INTO TABLE GT_ADRC
  FROM ADRC
  WHERE ADDRNUMBER = P_ADRNR.
*           AND nation = 'I'.

*         IF ( p_lifnr EQ 'OT01' OR p_lifnr EQ 'OT02' ).
*
*               SELECT SINGLE name1 name2 STRAS ORT01 PSTLZ
*                 INTO (lv_name1,lv_name2,lv_street,lv_city2,lv_post_code)
*                 FROM BSEC
*                 WHERE belnr EQ p_belnr
*                 AND gjahr EQ p_gjahr.
*
*                 CONCATENATE lv_name1  lv_name2 INTO p_name.
**                 CONCATENATE lv_street lv_city2 INTO p_addr SEPARATED BY SPACE.
*                 p_post_code = lv_post_code.
*         ELSEIF p_lifnr NE 'OT01'.
  READ TABLE GT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = P_ADRNR
                                           NATION = SPACE.
  IF SY-SUBRC EQ 0.
    CONCATENATE WA_ADRC-NAME1 WA_ADRC-NAME2 WA_ADRC-NAME3 WA_ADRC-NAME4 INTO P_NAME.
    CONCATENATE WA_ADRC-STREET WA_ADRC-STR_SUPPL1 WA_ADRC-STR_SUPPL2 WA_ADRC-STR_SUPPL3 WA_ADRC-LOCATION INTO LV_STRING SEPARATED BY SPACE.
    SPLIT LV_STRING AT SPACE INTO: LV_SPLIT1 LV_SPLIT2 LV_SPLIT3 LV_SPLIT4 LV_SPLIT5 LV_SPLIT6
                                        LV_SPLIT7 LV_SPLIT8 LV_SPLIT9 LV_SPLIT10.

*                     IF strlen( lv_split1 ) < 21.
*                        p_no = lv_split1.
*                        CONCATENATE p_no lv_split2 INTO lv_string1 SEPARATED BY SPACE.
*                        IF strlen( lv_string1 ) < 21.
*                           p_no = lv_string1.
*                        ELSE.
*                           IF
*                        ENDIF.
*
*                     ENDIF.

    P_NO = LV_STRING+0(20).
    P_SOI = LV_STRING+20(30).
    P_STREET = LV_STRING+50(30).
    P_SUBDESTRICT = LV_STRING+80(30).
*                    IF NOT wa_adrc-street IS INITIAL.
*                       len = strlen( wa_adrc-street ).
*                       IF len < 21 .
*                          p_no = wa_adrc-street .
*                       ELSE.
*                          SPLIT wa_adrc-street AT space INTO: lv_split1 lv_split2 lv_split3 lv_split4 lv_split5 lv_split6.
*                          p_no = lv_split1.
*                          CONCATENATE lv_split2 lv_split3 lv_split4 lv_split5 lv_split6 INTO lv_string SEPARATED BY SPACE.
*                          len = strlen( lv_string ).
*                          IF len < 31.
*                             p_soi = lv_string.
*                          ELSE.
*                               CONCATENATE lv_split2 lv_split3 lv_split4 lv_split5 INTO lv_string SEPARATED BY SPACE.
*                               len = strlen( lv_string ).
*                               IF len < 31.
*                                  p_soi = lv_string.
*                               ELSE.
*                                  CONCATENATE lv_split2 lv_split3 INTO p_soi SEPARATED BY SPACE.
*
*                               ENDIF.
*
*                             WHILE len < 31.
*                               p_soi = lv_split2.
*                             ENDWHILE.
*                             CONCATENATE lv_split2 lv_split3 lv_split4 lv_split5 lv_split6 INTO lv_string SEPARATED BY SPACE.
*                          ENDIF .
*
*                       ENDIF.
*
*                    ENDIF.

*                    CONCATENATE  wa_adrc-street  wa_adrc-str_suppl3
*                                 wa_adrc-location  wa_adrc-city2
*                                  wa_adrc-city1 INTO p_addr SEPARATED BY SPACE.
*                    p_tel = wa_adrc-tel_number.
*                    p_fax = wa_adrc-fax_number.
    P_DESTRICT = WA_ADRC-CITY2.
    P_CITY = WA_ADRC-CITY1.
    P_POST_CODE = WA_ADRC-POST_CODE1.
  ENDIF.


*          ENDIF.

*          SELECT SINGLE smtp_addr
*          INTO (p_email)
*          FROM adr6
*          WHERE addrnumber = p_adrnr.

ENDFORM. "

*&---------------------------------------------------------------------*
*&      Form  print_report
*&---------------------------------------------------------------------*

FORM PRINT_REPORT.
  PERFORM PREPARE_FIELDCAT USING 'GT_RESULT'.
  PERFORM BUILD_EVENT      USING ALV_EVENTS.
  PERFORM F_BUILD_LAYOUT   USING ALV_LAYOUT.
  PERFORM F_BUILD_SORT     USING ALV_SORT.
  PERFORM DISPLAY_OUTPUT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1473   text
*----------------------------------------------------------------------*
FORM PREPARE_FIELDCAT  USING TABNAME TYPE SLIS_TABNAME.
*** Prepare table fields
* Accounting Document No.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BELNR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Doc.No.'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* YEAR
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'GJAHR'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'YEAR'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* LINE ITEM WITHIN ACCOUNTING
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'BUZEI'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Item'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* INDICATOR FOR WITHHOLDING TAX TYPE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WITHT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'WHT Type'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* WITHHOLDING TAX CODE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_WITHCD'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'WHT Code'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* WITHHOLDING TAX DESCRIPTION
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WITHCD_DS'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'WHT Description'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* WITHHOLDING TAX BASE AMOUNT (LOCAL)
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_QSSHH'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'BASE AMOUNT(Local)'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* WITHHOLDING TAX BASE AMOUNT IN DOCUMENT CURRENCY
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_QSSHB'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'BASE AMOUNT(Doc.)'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
*INDICATOR: WITHHOLDING TAX BASE AMOUNT ENTER MANUALLY
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_AMNMAN'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Ind.BASE(MANUAL)'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* LINE ITEM STATUS
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_STAT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'ITEM STAT.'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* AMOUNT EXEMPT FROM WITHHOLDING TAX IN LOCAL CURRENCY
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_QBSHH'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'TAX AMOUNT(Local)'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* AMOUNT EXEMPT FROM WITHHOLDING TAX IN DOC. CURRENCY
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_QBSHB'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'TAX AMOUNT(Doc)'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* EXEMPTION CERTIFICATE NUMBER
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_WTEXMN'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Extemp.Cert.'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* ACCOUNT TYPE
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'KOART'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'ACC.Type'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
*WT_ACCO
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'WT_ACCO'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'CODE'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* VENDOR/CUSTOMER NAME
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'NAME'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'NAME'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* GENERAL LEDGER ACCOUNT
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'HKONT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'GL ACC.'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* TYPE OF RECIPIENT
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'QSREC'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'RECIPIENT'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* CLEARING DOC.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'AUGBL'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'CLEAR DOC.'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* CLEARING DATE.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'AUGDT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'CLEAR Date'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* WITHHOLDING TAX CERTIFICATION NUMBER
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'CTNUMBER'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'CER.No.'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* Tax Number3
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'STCD3'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Tax Number'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* House No
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'HOUSE_NO'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Address1'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* Soi
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SOI'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Address2'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* Street
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'STREET'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Address3'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* Sub District
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'SUB_DISTRICT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Address4'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* District
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'DISTRICT'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'District'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* City
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'CITY1'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'City'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* Post Code
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'POST_CODE1'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Post Code'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* tax Rate
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'TAX_RATE'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Tax Rate'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
* tax Type
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = 'TAX_TYPE'.
  GS_FIELDCAT-TABNAME   = TABNAME.
  GS_FIELDCAT-REPTEXT_DDIC = 'Tax Type'.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " PREPARE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_EVENT
*&---------------------------------------------------------------------*
FORM BUILD_EVENT USING  P_GTYP_EVENT TYPE SLIS_T_EVENT.
  FIELD-SYMBOLS <FS_EVENTS> LIKE LINE OF P_GTYP_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      ET_EVENTS       = P_GTYP_EVENT
    EXCEPTIONS
      LIST_TYPE_WRONG = 0
      OTHERS          = 0.

*  LOOP AT p_gtyp_event ASSIGNING <fs_events>
*                       WHERE name = 'TOP_OF_PAGE'.
*    <fs_events>-form = 'REPORT_HEADER'.
*  ENDLOOP.

ENDFORM.                    "build_event
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM F_BUILD_LAYOUT USING  P_GTYP_LAYOUT TYPE SLIS_LAYOUT_ALV.
  P_GTYP_LAYOUT-WINDOW_TITLEBAR = SY-TITLE.
  P_GTYP_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
ENDFORM.                    " F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_SORT
*&---------------------------------------------------------------------*
FORM F_BUILD_SORT USING  P_GTYP_SORT TYPE SLIS_T_SORTINFO_ALV.
  DATA: LW_SORT TYPE SLIS_SORTINFO_ALV.

* company
  LW_SORT-FIELDNAME = 'AUGDT'.
  LW_SORT-UP        = 'X'.
  APPEND LW_SORT TO P_GTYP_SORT.

ENDFORM.                    " F_BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
FORM DISPLAY_OUTPUT.

  GD_REPID = SY-REPID.
  CLEAR GS_VARIANT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = 'ZFI_WITHHOLD_CER'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IS_LAYOUT               = ALV_LAYOUT
      IT_FIELDCAT             = GT_FIELDCAT
      I_DEFAULT               = 'X'
      I_SAVE                  = 'A'
      IS_VARIANT              = GS_VARIANT
      IT_EVENTS               = ALV_EVENTS
      IT_SORT                 = ALV_SORT
    TABLES
      T_OUTTAB                = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DISPLAY_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DOWNLOAD_FILE USING P_P_PATH TYPE LOCALFILE.
  DATA: LV_FILE TYPE STRING.

  LV_FILE = P_P_PATH.

*        LOOP AT gt_regis INTO wa_regis.
**             CONCATENATE '"' wa_regis-payeename '"' INTO wa_regis-payeename.
*            CONCATENATE '"' wa_regis-debitbranchcd '"' INTO wa_regis-debitbranchcd.
*             CONCATENATE  wa_regis-debitbranchcd wa_regis-transfertype
*                          wa_regis-payeeno wa_regis-payeename
*                          wa_regis-address wa_regis-phoneno
*                          wa_regis-message wa_regis-bankcode
*                          wa_regis-bankname wa_regis-brancheregioncd
*                          wa_regis-branchregionname wa_regis-branchname
*                          wa_regis-accountno wa_regis-emailaddress
*                          wa_regis-zipcd wa_regis-faxno
*                          wa_regis-attention wa_regis-whtid
*                          wa_regis-whtformid wa_regis-cardid
*                          wa_regis-pickupid wa_regis-transactioncd
*                          wa_regis-inputkana wa_regis-ankcountryname
*                          wa_regis-countryname wa_regis-useregpayeematrix
*                          wa_regis-emailnotification wa_regis-faxnotification
*                          INTO GS_DFILE-text SEPARATED BY ','.
*                          APPEND GS_DFILE TO gt_dfile.
*          ENDLOOP.





  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE            =
      FILENAME                = LV_FILE
      FILETYPE                = 'DAT'
*     APPEND                  = ' '
*     WRITE_FIELD_SEPARATOR   = ' '
*     HEADER                  = '00'
*     TRUNC_TRAILING_BLANKS   = ' '
*     WRITE_LF                = 'X'
*     COL_SELECT              = ' '
*     COL_SELECT_MASK         = ' '
*     DAT_MODE                = ' '
*     CONFIRM_OVERWRITE       = ' '
*     NO_AUTH_CHECK           = ' '
      CODEPAGE                = '8600'
*     IGNORE_CERR             = ABAP_TRUE
*     REPLACEMENT             = '#'
*     WRITE_BOM               = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT            = ' '
*     WK1_N_SIZE              = ' '
*     WK1_T_FORMAT            = ' '
*     WK1_T_SIZE              = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS    = ABAP_TRUE
* IMPORTING
*     FILELENGTH              =
    TABLES
      DATA_TAB                = GT_DFILE
*     FIELDNAMES              =
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_DOWNLOAD_file

*&---------------------------------------------------------------------*
*&      Form  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM GET_PATH_NAME  CHANGING PATH.
  DATA: L_LENGTH TYPE I.
  DATA: L_MASK(20) TYPE C.

* S = Save, O = Open
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.txt,*.txt.'
      MODE             = 'O'
    IMPORTING
      FILENAME         = PATH
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDFORM.                    " GET_PATH_NAME
