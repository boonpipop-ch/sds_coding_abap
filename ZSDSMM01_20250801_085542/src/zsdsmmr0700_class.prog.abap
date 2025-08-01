*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0700_CLASS
*&---------------------------------------------------------------------*
CLASS LCL_UTIL DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.
    CLASS-METHODS :
      CONVERT_ALPHA_IN  IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY,
      CONVERT_ALPHA_OUT IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY.

ENDCLASS.
CLASS LCL_UTIL IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD CONVERT_ALPHA_IN.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = E_DATA.

  ENDMETHOD.
  METHOD CONVERT_ALPHA_OUT.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_outPUT'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = E_DATA.

  ENDMETHOD.
ENDCLASS.
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR,
      START_PROCESS.
    CLASS-METHODS :
      GET_DATA,
      GET_ADDTIONAL_DATA,
      SHOW_REPORT,
      SET_LAYOUT_OUTPUT,
      BUILD_FCAT,
      SET_SORT,
      SET_ALV_GRID,
      HTML_TOP_OF_PAGE,
      GET_BOM_MATERIAL_GROUP_AT RETURNING VALUE(R) LIKE I_MAT_AT,
      SEND_DATA,
      SEND_DATA_SMP,
      GET_SEND_TO RETURNING VALUE(R) TYPE CHAR7,
      GET_ZSDSSDC005,
      GET_GT_TVLAT,
      GET_ZSDSSDC009 CHANGING CR_DATA TYPE ANY TABLE,
      GET_ZSDSSDC004,
      GET_ZSDSCAC002 CHANGING CT_DATA  TYPE ANY TABLE
                              CT_DATA1 TYPE ANY TABLE,
      GET_VBELN_LSTEL RETURNING VALUE(R) TYPE ST_LSTEL,
      GET_ZSDSSDC003,
      GET_ZSDSSDC007,
      GET_ZSDSSDC006,
      GET_GT_IITAB,
      GET_GT_DO_REFER_BOM,
      GET_PRICE,
      GET_DATA_INSU IMPORTING I_DATA   TYPE MARA-MATNR
                    RETURNING VALUE(R) TYPE CHAR10,
      GET_LOAD_POINT IMPORTING I_DATA   TYPE LIKP-LSTEL
                     RETURNING VALUE(R) TYPE CHAR20,
      GET_MAP   RETURNING VALUE(R) TYPE CHAR1,
      REQ_INV   RETURNING VALUE(R) TYPE CHAR1,
      GET_AM_PM RETURNING VALUE(R) TYPE CHAR1,
      GET_MAKT  CHANGING CT_DATA TYPE MAKT_ITAB,
      READ_MAKT IMPORTING IT_DATA  TYPE MAKT_ITAB
                          I_DATA   TYPE MAKT-MATNR
                RETURNING VALUE(R) TYPE MAKT,
      GET_KNA1 CHANGING CT_DATA TYPE MM_T_KNA1,
      READ_KNA1 IMPORTING IT_DATA  TYPE MM_T_KNA1
                          I_DATA   TYPE KNA1-KUNNR
                RETURNING VALUE(R) TYPE KNA1,
      GET_VBPA CHANGING CT_DATA TYPE TAB_VBPA,
      GET_ADRC IMPORTING IT_DATA TYPE TAB_VBPA
               CHANGING  CT_DATA TYPE TTY_ADRC,
      CHECK_CHANGE_PROVINCE,
      UPDATE_STATUS.
    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD GET_DATA.
    IF LO IS INITIAL.
      CREATE OBJECT LO.
    ENDIF.

    LO->START_PROCESS( ).
  ENDMETHOD.
  METHOD START_PROCESS.

    DATA : DT_CONST TYPE TABLE OF ZSDSCAC002 .

    DATA : TB_LSTEL TYPE TABLE OF S_LSTEL,
           WA_LSTEL TYPE S_LSTEL,
           F_TO(7)  TYPE C.
    FIELD-SYMBOLS <FS_LSTEL> TYPE S_LSTEL.

    DATA: LV_CHECT_MAT_AT TYPE C.

    DATA: LV_ZBD1T TYPE BSEG-ZBD1T.
    DATA: LV_DUE TYPE SY-DATUM.
    DATA: LV_EZFBDT TYPE BSEG-ZFBDT.
    DATA: LV_INTERNAL_ORDER(40) TYPE C,
          LV_PROJECT(40)        TYPE C.
    DATA: LV_CHECK_TEXT_SITE    TYPE C.

    DATA: LV_IO_AUART   TYPE AUFK-AUART,
          LV_CHECK_CUST TYPE C.

    DATA : LV_KUNNR TYPE KNA1-KUNNR.

    DATA : LS_OUTTAB LIKE LINE OF GT_OUTTAB.

    DATA : LR_KUNNR TYPE RANGE OF KNA1-KUNNR.

    DATA : LS_OUTTAB_HI LIKE LINE OF GT_OUTTAB_HI.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA: LV_PICHON(18) TYPE C,
          LV_PICHON1    TYPE VBRP-FKIMG,
          LV_PICHON2    TYPE VBRP-FKIMG,
          PERCENT       TYPE P DECIMALS 2.

    DATA: LV_SO_NO   TYPE VBAK-VBELN,
          LV_SO_TYPE TYPE VBAK-AUART,
          PH1        TYPE ZSDSSDC007-PH1,
          PH2        TYPE ZSDSSDC007-PH2,
          PH3        TYPE ZSDSSDC007-PH3.

    DATA: LV_ITEM_TEXT(4)    TYPE C,
          LV_VBELN_POSNR(16) TYPE C.

    DATA: LV_SUM_QTY_SPILT TYPE LIPS-LGMNG,
          LV_QTY_1700      TYPE LIPS-LGMNG.

    DATA: LV_SO_AUART TYPE VBAK-AUART.


    DATA: DT_CONS_CLEW TYPE TABLE OF ZSDSCAC002,
          WA_CONS_CLEW TYPE ZSDSCAC002.
    DATA: LV_CLEW_MAT TYPE MARA-MATNR.

    DATA: LV_CHK_WARRANTY TYPE C.
    DATA: LV_SO_ITEM(16) TYPE C.
    DATA: LV_TEXT(510) TYPE C.
    DATA: LV_TEXT_RETURN(255) TYPE C.
    DATA: LV_TEXT_INVREMARK(250) TYPE C.
    DATA: LV_MATNR_INSU TYPE ZSDSSDC005-MATNR.
    DATA: LT_MAKT TYPE TABLE OF MAKT,
          MAKT    TYPE MAKT.

    DATA: LT_KNA1 TYPE TABLE OF KNA1,
          LS_KNA1 TYPE KNA1.

    DATA: LT_ADRC TYPE TABLE OF ADRC,
          LS_ADRC TYPE ADRC.

    DATA: LT_VBPA TYPE TABLE OF VBPA,
          LS_VBPA TYPE VBPA.

    IF LCL_UTIL IS NOT BOUND.
      CREATE OBJECT LCL_UTIL.
    ENDIF.

    TB_LSTEL = GET_VBELN_LSTEL( ).
    LCL_DATA=>GET_ZSDSCAC002( CHANGING CT_DATA  = DT_CONST
                                       CT_DATA1 = DT_CONS_CLEW ).
    LCL_DATA=>GET_ZSDSSDC004( ).
    LCL_DATA=>GET_ZSDSSDC009( CHANGING CR_DATA = LR_KUNNR ).
    LCL_DATA=>GET_ZSDSSDC005( ).
    LCL_DATA=>GET_GT_TVLAT( ).
    F_TO = LCL_DATA=>GET_SEND_TO( ).
    LCL_DATA=>GET_ZSDSSDC003( ).
    LCL_DATA=>GET_ZSDSSDC007( ).
    LCL_DATA=>GET_ZSDSSDC006( ).
    LCL_DATA=>GET_GT_IITAB( ).
    CHECK GT_IITAB[] IS NOT INITIAL.
    LCL_DATA=>GET_GT_DO_REFER_BOM( ).
    LCL_DATA=>GET_PRICE( ).
    LCL_DATA=>GET_MAKT( CHANGING  CT_DATA = LT_MAKT ).
    LCL_DATA=>GET_KNA1( CHANGING  CT_DATA = LT_KNA1 ).
    LCL_DATA=>GET_VBPA( CHANGING  CT_DATA = LT_VBPA ).
    LCL_DATA=>GET_ADRC( EXPORTING IT_DATA = LT_VBPA
                         CHANGING CT_DATA = LT_ADRC ).

*$-----------------------------------------------------------------*
*    Werathep Ch. to additional storage location to Sony Grade A
*    AND b~lgort IN ('1100','1150'). "SONY Grade A
*$-----------------------------------------------------------------*
    CLEAR: GV_LOADING_POINT.
    I_MAT_AT = GET_BOM_MATERIAL_GROUP_AT( ).
    CLEAR: CHECK_1800_DIT.
    CLEAR: CHECK_1600_SMP.
    CLEAR: CHECK_1500_JPP.
    CLEAR: CHECK_1100_SONY.
    CLEAR: LV_TEXT_RETURN.
    LOOP AT GT_IITAB INTO DATA(LS_ITAB).
      CLEAR: LV_CHECK_CUST,GV_SHIP_ADDR,LV_ITEM_TEXT,LV_VBELN_POSNR,LV_CHECT_MAT_AT.
      CLEAR: LV_SO_AUART,LV_CHK_WARRANTY,LV_SO_ITEM.
      CLEAR: GV_SHIP_NAME.
      CLEAR: LV_TEXT,LV_TEXT_INVREMARK,LV_MATNR_INSU.

      PH1   = LS_ITAB-PRODH+0(5).
      PH2   = LS_ITAB-PRODH+5(5).
      PH3   = LS_ITAB-PRODH+10(8).

      IF LS_ITAB-VBELN(2) = '40' OR
         LS_ITAB-VBELN(2) = '42' OR
         LS_ITAB-VBELN(2) = '41'.

        IF LS_ITAB-VBELN(2) = '40'.
          LS_OUTTAB-LFART = '1'.
        ELSEIF LS_ITAB-VBELN(2) = '42' OR
               LS_ITAB-VBELN(2) = '41'.
          LS_OUTTAB-LFART = '0'.
        ENDIF.

        LS_OUTTAB-CUST_CODE  = 'SDS'.
        LS_OUTTAB-VBELN         = LS_ITAB-VBELN.
        LS_OUTTAB-LFDAT         = LS_ITAB-LFDAT.
        LS_OUTTAB-ERDAT         = LS_ITAB-ERDAT.
        LS_OUTTAB-POSNR         = LS_ITAB-POSNR+2(4).
        LS_OUTTAB-MATNR         = LS_ITAB-MATNR.
        LS_OUTTAB-INSULATION    = LCL_DATA=>GET_DATA_INSU( LS_ITAB-MATNR ).
        LS_OUTTAB-SO            = LS_ITAB-VGBEL.
        LS_OUTTAB-FLAG_BOM      = LS_ITAB-KOWRR.
        LS_OUTTAB-REFER_BOM     = LS_ITAB-UEPOS+2(4).
        LS_OUTTAB-LOADING_POINT = LCL_DATA=>GET_LOAD_POINT( LS_ITAB-LSTEL ).
        LS_OUTTAB-REQ_MAP       = LCL_DATA=>GET_MAP( ).
        LS_OUTTAB-REQ_INV       = LCL_DATA=>REQ_INV( ).
        LS_OUTTAB-AM_PM         = LCL_DATA=>GET_AM_PM( ).
        CLEAR: T001.
        MAKT = LCL_DATA=>READ_MAKT( IT_DATA = LT_MAKT
                                    I_DATA  = LS_ITAB-MATNR ).
        LS_OUTTAB-ARKTX         = MAKT-MAKTX.
        WRITE LS_ITAB-LGMNG TO LS_OUTTAB-LGMNG LEFT-JUSTIFIED DECIMALS 0.
        LS_OUTTAB-KUNNR         = LS_ITAB-KUNNR.
        SHIFT LS_OUTTAB-KUNNR LEFT DELETING LEADING '0'.

        LS_KNA1 = LCL_DATA=>READ_KNA1( IT_DATA = LT_KNA1
                                       I_DATA  = LS_ITAB-KUNNR ).

        IF LS_ITAB-KUNNR NE 'OT01'.
          READ TABLE LT_ADRC INTO ADRC
          WITH KEY ADDRNUMBER = LS_KNA1-ADRNR
                   NATION     = SPACE.
          IF SY-SUBRC EQ 0.
            CONCATENATE ADRC-NAME1
                        ADRC-NAME2
                        ADRC-NAME3
                        ADRC-NAME4
                   INTO LS_OUTTAB-CUST_NAME SEPARATED BY SPACE.
          ENDIF.
        ELSE.
          READ TABLE LT_VBPA INTO VBPA
          WITH KEY VBELN = LS_ITAB-VBELN
                   PARVW = 'AG'.
          IF SY-SUBRC EQ 0.
            READ TABLE LT_ADRC INTO ADRC
            WITH KEY ADDRNUMBER = VBPA-ADRNR
                     NATION     = SPACE.
            IF SY-SUBRC EQ 0.
              CONCATENATE ADRC-NAME1
               ADRC-NAME2
               ADRC-NAME3
               ADRC-NAME4
              INTO LS_OUTTAB-CUST_NAME SEPARATED BY SPACE.
            ENDIF.
          ENDIF.
        ENDIF.

        CONCATENATE LS_KNA1-STRAS LS_KNA1-ORT01
               INTO GV_CUST_ADDR.

        READ TABLE LT_VBPA INTO VBPA
        WITH KEY VBELN = LS_ITAB-VBELN
                 PARVW = 'WE'.
        IF SY-SUBRC EQ 0.
          READ TABLE LT_ADRC INTO ADRC
          WITH KEY ADDRNUMBER = VBPA-ADRNR
                   NATION     = SPACE.
          IF SY-SUBRC EQ 0.
            CONCATENATE ADRC-STREET ADRC-STR_SUPPL1 ADRC-STR_SUPPL2
                        ADRC-STR_SUPPL3 ADRC-LOCATION
                        ADRC-CITY2
                   INTO GV_SHIP_ADDR SEPARATED BY SPACE.
            CONCATENATE '[' ADRC-NAME1 ADRC-NAME2 ']' INTO GV_SHIP_NAME SEPARATED BY SPACE.
            LS_OUTTAB-SHIP_PROVINCE = ADRC-CITY1 .
            LS_OUTTAB-POSTCODE = ADRC-POST_CODE1 .
          ENDIF.
        ENDIF.

        LS_OUTTAB-DELI_TO = GV_CUST_ADDR(60).
        LS_OUTTAB-SHIP_ADDR = GV_SHIP_ADDR(255).

        CONCATENATE LS_ITAB-VBELN LS_ITAB-POSNR INTO LV_VBELN_POSNR.
        IF LS_ITAB-VBELN(2) = '40'.
          LS_OUTTAB-REMARK = LCL_UTIL->GET_TEXT( I_ID       = 'ZH10'
                                                 I_NAME     = LS_ITAB-VBELN
                                                 I_OBJECT   = 'VBBK'
                                                 I_LANGUAGE = SY-LANGU ).

          LV_TEXT_INVREMARK = LCL_UTIL->GET_TEXT( I_ID       = 'ZH13'
                                                 I_NAME     = LS_ITAB-VBELN
                                                 I_OBJECT   = 'VBBK'
                                                 I_LANGUAGE = SY-LANGU ).

          IF LV_TEXT_INVREMARK EQ LS_OUTTAB-REMARK.
            CLEAR: LV_TEXT_INVREMARK.
          ENDIF.

          CONCATENATE LS_OUTTAB-REMARK LV_TEXT_INVREMARK INTO LS_OUTTAB-REMARK SEPARATED BY SPACE.

          IF LS_ITAB-LSTEL EQ 'ZM'.
            CONCATENATE GV_SHIP_NAME LS_OUTTAB-REMARK INTO LS_OUTTAB-REMARK.
          ENDIF.

        ELSEIF LS_ITAB-VBELN(2) = '42' OR
               LS_ITAB-VBELN(2) = '41'.
          BREAK WANTANEE.

          LV_TEXT = LCL_UTIL->GET_TEXT( I_ID       = 'ZH09'
                                        I_NAME     = LS_ITAB-VBELN
                                        I_OBJECT   = 'VBBK'
                                        I_LANGUAGE = SY-LANGU ).

          IF LV_TEXT IS NOT INITIAL.
            LS_OUTTAB-REMARK = LV_TEXT+0(190).
            LS_OUTTAB-PICHON = LV_TEXT+190(190).
          ENDIF.
        ENDIF.
*--------------------------------------------------------------------*
* Below if I have a time, I will edit it again
*--------------------------------------------------------------------*
        SELECT SINGLE LGORT VGBEL
        INTO (LIPS-LGORT, LV_SO_NO)
        FROM LIPS
        WHERE VBELN EQ LS_ITAB-VBELN AND
              POSNR EQ LS_ITAB-POSNR.
        IF SY-SUBRC EQ 0.
          CONCATENATE LS_ITAB-VBELN LS_ITAB-POSNR INTO LV_VBELN_POSNR.
          IF LIPS-LGORT IS INITIAL.

            IF LS_ITAB-LSTEL EQ 'Z1'.
              CHECK_1100_SONY = 'X'.
            ELSE.
              IF LS_OUTTAB-LGORT+0(2) = '11'.
                CHECK_1100_SONY = 'X'.
              ENDIF.
            ENDIF.

            IF LS_ITAB-LSTEL EQ 'ZD'.
              LS_OUTTAB-LGORT = '1800' .
              CHECK_1800_DIT = 'X'.
              READ TABLE GT_DO_REFER_BOM INTO WA_DO_REFER_BOM WITH KEY VBELN = LS_ITAB-VBELN
                                                                       POSNR = LS_ITAB-POSNR.
              IF SY-SUBRC EQ 0.
                READ TABLE GT_DO_REFER_BOM INTO GW_DO_REFER_BOM WITH KEY VBELN = LS_ITAB-VBELN
                                                                         POSNR = WA_DO_REFER_BOM-UEPOS.
                IF SY-SUBRC EQ 0.
                  CLEAR: LV_SUM_QTY_SPILT,LV_QTY_1700.

                  IF LS_ITAB-LGMNG EQ GW_DO_REFER_BOM-LGMNG.
                    WRITE LS_ITAB-LGMNG TO LS_OUTTAB-LGMNG LEFT-JUSTIFIED DECIMALS 0.
                  ELSE.
                    LOOP AT GT_IITAB INTO WA_IITAB_TEMP WHERE VGBEL EQ LS_ITAB-VGBEL
                                                      AND POSNV EQ LS_ITAB-POSNV.
                      IF ( NOT WA_IITAB_TEMP-LGORT IS INITIAL ) AND
*                                      wa_iitab_temp-lgort NE '1700' . "Remove for project DDD  CH34 T41K939523 CH35 T41K939532
                           WA_IITAB_TEMP-LGORT NE '1800' . "Add for project DDD  T41K934404 T41K934432  CH34 T41K939523 CH35 T41K939532
                        LV_SUM_QTY_SPILT =  LV_SUM_QTY_SPILT + WA_IITAB_TEMP-LGMNG.
                      ENDIF.

                    ENDLOOP.
                    LV_QTY_1700 = GW_DO_REFER_BOM-LGMNG - LV_SUM_QTY_SPILT.
                    WRITE LV_QTY_1700 TO LS_OUTTAB-LGMNG LEFT-JUSTIFIED DECIMALS 0.
                  ENDIF.

                ENDIF.

              ENDIF.


            ELSE.

              IF LS_ITAB-KOWRR NE 'X'.
                LV_TEXT = LCL_UTIL->GET_TEXT( I_ID       = 'ZI03'
                                              I_NAME     = LS_ITAB-VBELN
                                              I_OBJECT   = 'VBBP'
                                              I_LANGUAGE = SY-LANGU ).
              ENDIF.

              IF LS_OUTTAB-LGORT IS INITIAL.
                IF LS_ITAB-KOWRR NE 'X'.
                  IF LS_ITAB-MATNR CP '*TRANSPORT*'.
                  ELSE.
                    CONCATENATE 'Please input location DO item: ' LS_ITAB-VBELN  LS_ITAB-POSNR INTO LV_CHK_LOGRT SEPARATED BY SPACE.
                  ENDIF.
                ENDIF.
              ELSE.
                CONDENSE LS_OUTTAB-LGORT.
              ENDIF.
              IF LS_OUTTAB-LGORT+0(2) = '16'.
                CHECK_1600_SMP = 'X'.
              ENDIF.
              IF LS_OUTTAB-LGORT+0(2) = '15'.
                CHECK_1500_JPP = 'X'.
              ENDIF.

              IF LS_OUTTAB-LGORT+0(2) = '11'.
                CHECK_1100_SONY = 'X'.
              ENDIF.
            ENDIF.
          ELSE.
            WRITE: LIPS-LGORT TO LS_OUTTAB-LGORT LEFT-JUSTIFIED.
            IF LS_OUTTAB-LGORT = '1800'.
              CHECK_1800_DIT = 'X'.
            ENDIF.

            IF LS_OUTTAB-LGORT+0(2) = '16'.
              CHECK_1600_SMP = 'X'.
            ENDIF.

            IF LS_OUTTAB-LGORT+0(2) = '15'.
              CHECK_1500_JPP = 'X'.
            ENDIF.


            IF LS_ITAB-LSTEL EQ 'Z1'.
              CHECK_1100_SONY = 'X'.
            ELSE.
              IF LS_OUTTAB-LGORT+0(2) = '11'.
                CHECK_1100_SONY = 'X'.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
        SELECT SINGLE AUART
        INTO LV_SO_TYPE
        FROM VBAK
        WHERE VBELN EQ LV_SO_NO.

        CLEAR: LV_CHECT_MAT_AT.
        IF I_MAT_AT[] IS NOT INITIAL.
          READ TABLE I_MAT_AT WITH KEY IDNRK = LS_ITAB-MATNR TRANSPORTING NO FIELDS.
          IF SY-SUBRC EQ 0.
            LV_CHECT_MAT_AT = 'X'.
          ENDIF.
        ENDIF.

        SELECT * FROM ZSDSSDC002 WHERE LSTEL = @LS_ITAB-LSTEL AND
                                       LGORT = @LS_OUTTAB-LGORT
                                  INTO @ZSDSSDC002.
          IF SY-SUBRC EQ 0.

            READ TABLE GT_ZSDSSDC006 INTO GW_ZSDSSDC006 WITH KEY MATNR =  LS_ITAB-MATNR
                                                                           AUART =  LV_SO_TYPE.
            IF SY-SUBRC EQ 0.
              LS_OUTTAB-WARRANTY = 'W'.
            ENDIF.


            IF LS_OUTTAB-WARRANTY NE 'W'.

              SELECT SINGLE AUART
              INTO LV_IO_AUART
              FROM AUFK
              WHERE AUFNR EQ LS_ITAB-AUFNR.

              IF LV_IO_AUART EQ 'H004'.
                READ TABLE GT_CUST_WARRANTY INTO WA_CUST_WARRANTY WITH KEY KUNNR = LS_ITAB-KUNNR
                                                                            VKBUR = LS_ITAB-VKBUR.
                IF SY-SUBRC EQ 0.
                  LV_CHECK_CUST = 'X'.
                ENDIF.

              ENDIF.


              IF NOT LV_CHECK_CUST IS INITIAL.
                IF PH2 EQ 'CDU'.
                  LS_OUTTAB-WARRANTY = 'W'.
                ENDIF.

              ELSE.
                CASE F_TO.
                  WHEN 'SONY'.
                    CASE LS_ITAB-VTWEG.
                      WHEN 10.
                        READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = LS_ITAB-VTWEG
                                                                         VKBUR = LS_ITAB-VKBUR
                                                                         PH1   = PH1
                                                                         PH2   = PH2.
*

                        IF SY-SUBRC EQ 0 AND LS_ITAB-ERSDA GE WA_WARRANTY-WDATE.
                          IF LV_SO_TYPE NE 'ZOK1'.
                            LS_OUTTAB-WARRANTY = 'W'.
                          ENDIF.
                        ENDIF.

*
                        IF LS_OUTTAB-WARRANTY IS INITIAL.
                          READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = LS_ITAB-KNUMV
                                                                   KPOSN = LS_ITAB-POSNV.
                          IF SY-SUBRC EQ 0.
                            CALL FUNCTION 'Z_SDSMM_CHK_WARRANTY_SP'
                              EXPORTING
                                P_MATNR    = LS_ITAB-MATNR
                                P_VTWEG    = LS_ITAB-VTWEG
                                P_VKBUR    = LS_ITAB-VKBUR
                              IMPORTING
                                ISWARRANTY = LS_OUTTAB-WARRANTY
                              TABLES
                                ITAB       = DT_CONST[].

                          ENDIF.
                        ENDIF.

                        SELECT SINGLE AUART
                          INTO LV_SO_AUART
                          FROM VBAK
                          WHERE VBELN = LS_OUTTAB-SO.


*
                        IF LV_SO_AUART EQ 'ZO07'.

                          CONCATENATE LS_OUTTAB-SO LS_ITAB-POSNV INTO LV_SO_ITEM.

                          LV_CHK_WARRANTY = LCL_UTIL->GET_TEXT( I_ID       = 'ZI13'
                                                                I_NAME     = LS_ITAB-VBELN
                                                                I_OBJECT   = 'VBBP'
                                                                I_LANGUAGE = SY-LANGU ).

                          IF LV_CHK_WARRANTY = 'X' OR LV_CHK_WARRANTY = 'x'.
                            LS_OUTTAB-WARRANTY = 'W'.
                          ENDIF.

                        ENDIF.
                      WHEN 20.

                        READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = LS_ITAB-VTWEG
                                                                         VKBUR = LS_ITAB-VKBUR
                                                                         PH1   = PH1
                                                                         PH2   = PH2
                                                                         PH3   = PH3.
                        IF SY-SUBRC EQ 0.
                          CLEAR PERCENT.
                          READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = LS_ITAB-KNUMV
                                                                   KPOSN = LS_ITAB-POSNV.
                          IF SY-SUBRC EQ 0 AND LS_ITAB-ERSDA GE WA_WARRANTY-WDATE.
                            PERCENT = ( LS_ITAB-NETWR * 100 ) / WA_KONV-KWERT.
                            IF PERCENT GE WA_WARRANTY-LP_PERCENT.
                              IF LV_SO_TYPE NE 'ZOK1'.
                                LS_OUTTAB-WARRANTY = 'W'.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.

                        IF LS_OUTTAB-DELI_TO+0(1) NE 'W'.
                          READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = LS_ITAB-VTWEG
                                                                           VKBUR = LS_ITAB-VKBUR
                                                                           PH1   = PH1
                                                                           PH2   = PH2
                                                                           PH3   = PH3.
                          IF SY-SUBRC EQ 0.
                            CLEAR PERCENT.
                            READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = LS_ITAB-KNUMV
                                                                     KPOSN = LS_ITAB-POSNV.
                            IF SY-SUBRC EQ 0 AND LS_ITAB-ERSDA GE WA_WARRANTY-WDATE.
                              PERCENT = ( LS_ITAB-NETWR * 100 ) / WA_KONV-KWERT.
                              IF PERCENT GE WA_WARRANTY-LP_PERCENT.
                                IF LV_SO_TYPE NE 'ZOK1'.
                                  LS_OUTTAB-WARRANTY = 'W'.
                                ENDIF.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.

                        IF LS_OUTTAB-WARRANTY NE 'W'.
                          READ TABLE GT_WARRANTY INTO WA_WARRANTY WITH KEY VTWEG = LS_ITAB-VTWEG
                                                                           VKBUR = LS_ITAB-VKBUR
                                                                           PH1   = PH1
                                                                           PH2   = PH2
                                                                           PH3   = SPACE.
                          IF SY-SUBRC EQ 0.
                            CLEAR PERCENT.
                            READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = LS_ITAB-KNUMV
                                                                     KPOSN = LS_ITAB-POSNV.
                            IF SY-SUBRC EQ 0 AND LS_ITAB-ERSDA GE WA_WARRANTY-WDATE.
                              PERCENT = ( LS_ITAB-NETWR * 100 ) / WA_KONV-KWERT.
                              IF PERCENT GE WA_WARRANTY-LP_PERCENT.
                                IF LV_SO_TYPE NE 'ZOK1'.
                                  LS_OUTTAB-WARRANTY = 'W'.
                                ENDIF.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                    ENDCASE.

                    IF LS_OUTTAB-WARRANTY IS INITIAL.
                      READ TABLE GT_KONV INTO WA_KONV WITH KEY KNUMV = LS_ITAB-KNUMV
                                                               KPOSN = LS_ITAB-POSNV.
                      IF SY-SUBRC EQ 0.
                        CALL FUNCTION 'ZSD_CHK_WARRANTY_SP'
                          EXPORTING
                            P_MATNR    = LS_ITAB-MATNR
                            P_NETWR    = LS_ITAB-NETWR
                            P_KWERT    = WA_KONV-KWERT
                            P_VTWEG    = LS_ITAB-VTWEG
                            P_VKBUR    = LS_ITAB-VKBUR
                          IMPORTING
                            ISWARRANTY = LS_OUTTAB-WARRANTY
                          TABLES
                            ITAB       = DT_CONST[].

                      ENDIF.
                    ENDIF.

                    READ TABLE GT_ZSDSSDC005 INTO WA_ZSDSSDC005 WITH KEY MATNR = LS_ITAB-MATNR.
                    IF SY-SUBRC EQ 0.
                      LS_OUTTAB-REMARK_PICHON = 'X'.
                    ENDIF.

                ENDCASE.
              ENDIF.

              READ TABLE GT_ZSDSSDC004 INTO GW_ZSDSSDC004 WITH KEY MATNR = LS_ITAB-MATNR.
              IF SY-SUBRC = 0.
                LS_OUTTAB-WARRANTY = ''.
              ENDIF.

              IF LV_CHECT_MAT_AT EQ 'X'.
                LS_OUTTAB-WARRANTY = ''.
              ENDIF.

              IF LS_ITAB-AUFNR = 'SDS'.
                LS_OUTTAB-WARRANTY = ''.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDSELECT.

        GT_OUTTAB_HI                = GT_OUTTAB.
        LS_OUTTAB_HI-DEST_ADDR      = ADRC-STREET.
        LS_OUTTAB_HI-DEST_SUB_DIS   = ADRC-LOCATION.
        LS_OUTTAB_HI-DEST_DISTRIC   = ADRC-CITY2.
        LS_OUTTAB_HI-DEST_PROVIN    = ADRC-CITY1.
        LS_OUTTAB_HI-DEST_POST_CODE = ADRC-POST_CODE1.

        IF GT_OUTTAB_HI IS NOT INITIAL.

          LV_KUNNR = LS_OUTTAB_HI-KUNNR.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = LV_KUNNR
            IMPORTING
              OUTPUT = LV_KUNNR.

          IF LV_KUNNR IN LR_KUNNR.
            CLEAR LS_OUTTAB_HI-WARRANTY.
          ENDIF.
          APPEND LS_OUTTAB_HI TO GT_OUTTAB_HI.
          CLEAR: LS_OUTTAB_HI.
        ENDIF.

        DATA: LV_PERNR TYPE PA0002-PERNR,
              LV_VORNA TYPE PA0002-VORNA.
        DATA: LV_PARNR TYPE VBPA-PARNR,
              LV_NAMEV TYPE KNVK-NAMEV,
              LV_NAME1 TYPE KNVK-NAME1.

        SELECT SINGLE PERNR
          INTO LV_PERNR
          FROM VBPA
          WHERE VBELN EQ LS_ITAB-VBELN
          AND PARVW EQ 'VE'.

        IF LV_PERNR IS NOT INITIAL.
          SELECT SINGLE VORNA
            INTO LV_VORNA
            FROM PA0002
            WHERE PERNR EQ LV_PERNR.

          LS_OUTTAB-SALESMAN = LV_VORNA.
        ENDIF.

        CLEAR: LV_ZBD1T,LV_EZFBDT,LV_DUE.
        DATA: LV_DATE TYPE SY-DATLO.
        DATA: LV_INV_REF TYPE VBFA-VBELV.

        LV_DATE = LS_ITAB-LFDAT.

        CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
          EXPORTING
            I_BLDAT         = LV_DATE
            I_BUDAT         = LV_DATE
            I_CPUDT         = LV_DATE
            I_ZFBDT         = LV_DATE
            I_ZTERM         = LS_ITAB-ZTERM
          IMPORTING
            E_ZBD1T         = LV_ZBD1T
            E_ZFBDT         = LV_EZFBDT
          EXCEPTIONS
            TERMS_NOT_FOUND = 1.

        IF LV_ZBD1T IS NOT INITIAL.
          LV_DUE = LV_DATE + LV_ZBD1T.

        ELSEIF LV_EZFBDT IS NOT INITIAL.
          LV_DUE = LV_EZFBDT.
        ENDIF.
        LS_OUTTAB-DUE_DATE = LV_DUE.
        IF LS_ITAB-VBELN(2) = '40'.
          LS_OUTTAB-PO_NO = LS_ITAB-BSTKD.
        ELSEIF LS_ITAB-VBELN(2) = '42' OR
               LS_ITAB-VBELN(2) = '41'.
          LS_OUTTAB-PO_NO = LS_ITAB-BSTKD_E.
        ENDIF.

        SELECT SINGLE KTEXT INTO LV_INTERNAL_ORDER
        FROM COAS
        WHERE AUFNR = LS_ITAB-AUFNR  .

        LS_OUTTAB-PROJECT = LV_INTERNAL_ORDER.
        LS_OUTTAB-TERM_OF_PAYMENT = LS_ITAB-ZTERM.

        SELECT SINGLE PARNR
          INTO LV_PARNR
          FROM VBPA
          WHERE VBELN EQ LS_ITAB-VBELN
          AND PARVW EQ 'AP'.

        IF LV_PERNR IS NOT INITIAL.
          SELECT SINGLE NAMEV NAME1
            INTO (LV_NAMEV,LV_NAME1)
            FROM KNVK
            WHERE PARNR EQ LV_PARNR.

          CONCATENATE LV_NAMEV LV_NAME1 INTO LS_OUTTAB-CONTACT_NAME SEPARATED BY SPACE.
        ENDIF.


        IF LS_ITAB-VBELN(2) = '42' OR
           LS_ITAB-VBELN(2) = '41'.
          SELECT SINGLE VBELV
            INTO LV_INV_REF
            FROM VBFA
            WHERE VBELN = LS_ITAB-VBELN
            AND VBTYP_V = 'M'.

          LS_OUTTAB-REF_INV = LV_INV_REF.
        ENDIF.

        IF LS_ITAB-VTWEG EQ '10'.
          LS_OUTTAB-SALES_DIV = 'DS'.
        ELSEIF LS_ITAB-VTWEG EQ '20'.
          LS_OUTTAB-SALES_DIV = 'GS'.
        ELSEIF LS_ITAB-VTWEG EQ '30'.
          LS_OUTTAB-SALES_DIV = 'EN'.
        ELSEIF LS_ITAB-VTWEG EQ '40'.
          LS_OUTTAB-SALES_DIV = 'SV'.
        ENDIF.

        SELECT SINGLE BEZEI
        INTO LS_OUTTAB-SALES_OFFICE
        FROM TVKBT
        WHERE VKBUR = LS_ITAB-VKBUR
          AND SPRAS EQ 'E'.

        IF LS_ITAB-VBELN(2) = '42' OR
           LS_ITAB-VBELN(2) = '41'.
          CONCATENATE LS_OUTTAB-SALESMAN '/' LS_OUTTAB-SALES_OFFICE INTO LS_OUTTAB-SALESMAN.
        ENDIF.

        SELECT SINGLE BEZEI
        INTO LS_OUTTAB-SALES_GROUP
        FROM TVGRT
        WHERE VKGRP = LS_ITAB-VKGRP
          AND SPRAS EQ 'E'.

        LS_OUTTAB-UOM = LS_ITAB-MEINS.
        IF R_NEW IS NOT INITIAL.
          LS_OUTTAB-DOC_FLAG = 'A'.
        ELSEIF R_UP IS NOT INITIAL.
          LS_OUTTAB-DOC_FLAG = 'E'.
        ENDIF.

        LV_CHECK_TEXT_SITE = LCL_UTIL->GET_TEXT( I_ID       = 'ZH18'
                                                 I_NAME     = LS_ITAB-VBELN
                                                 I_OBJECT   = 'VBBK'
                                                 I_LANGUAGE = SY-LANGU ).

        IF LV_CHECK_TEXT_SITE EQ 'X' OR LV_CHECK_TEXT_SITE EQ 'x'.
          LS_OUTTAB-MHA = 'X'.
        ELSE.
          LS_OUTTAB-MHA = ''.
        ENDIF.

        IF LS_OUTTAB IS NOT INITIAL.
          LV_KUNNR = LS_OUTTAB-KUNNR.
          IF NOT R_1700 IS  INITIAL.
            IF LS_OUTTAB-LGORT EQ '1800'.
              LS_OUTTAB-LGORT = '1700'.
              CHECK_1800_DIT = ''.
            ENDIF.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = LV_KUNNR
            IMPORTING
              OUTPUT = LV_KUNNR.
          IF LV_KUNNR IN LR_KUNNR.
            CLEAR LS_OUTTAB-WARRANTY.
          ENDIF.
          APPEND LS_OUTTAB TO GT_OUTTAB.
          CLEAR: LS_OUTTAB.   "
        ENDIF.

      ENDIF.
    ENDLOOP.

    SORT GT_OUTTAB  BY LFART CUST_CODE VBELN LFDAT ERDAT POSNR MATNR.
    SORT GT_OUTTAB4 BY LFART CUST_CODE VBELN LFDAT ERDAT POSNR MATNR.


  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.

    CHECK_CHANGE_PROVINCE( ).

*    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
*    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
*
*    ENDLOOP.
  ENDMETHOD.
  METHOD SHOW_REPORT.
    SET_LAYOUT_OUTPUT( ).
    BUILD_FCAT( ).
    SET_SORT( ).
    SET_ALV_GRID( ).
  ENDMETHOD.
  METHOD SET_LAYOUT_OUTPUT.
*    CONSTANTS : BEGIN OF LC_CON,
*                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
*                END OF LC_CON.
    GS_LAYOUT-ZEBRA             = GC_X.
    GS_LAYOUT-COLWIDTH_OPTIMIZE = GC_X.
*    GS_LAYOUT-BOX_FIELDNAME     = LC_CON-CHK_FILED.
  ENDMETHOD.
  METHOD BUILD_FCAT.
    DATA:
       LS_FCAT TYPE SLIS_FIELDCAT_ALV.

*    CONSTANTS : BEGIN OF LC_CON,
*                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
*                  CHK_NAME  TYPE C LENGTH 3 VALUE 'CHK',
*                END OF LC_CON.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = LC_CON-CHK_FILED.
*    LS_FCAT-SELTEXT_S   = LC_CON-CHK_NAME.
*    LS_FCAT-SELTEXT_M   = LC_CON-CHK_NAME.
*    LS_FCAT-SELTEXT_L   = LC_CON-CHK_FILED.
*    LS_FCAT-CHECKBOX    = ABAP_TRUE.
*    LS_FCAT-INPUT       = ABAP_TRUE.
*    LS_FCAT-EDIT        = ABAP_TRUE.
*    APPEND LS_FCAT TO GT_FCAT.

    DATA : LV_RUNNING  TYPE I,
           LV_DATA     TYPE C LENGTH 6 VALUE 'TEXT-',
           LV_RUN_TEXT TYPE C LENGTH 2.

    CONSTANTS : LC_F TYPE C VALUE 'F',
                LC_T TYPE C VALUE 'T',
                LC_d TYPE C VALUE 'D'.

    FIELD-SYMBOLS <LFS> TYPE ANY.

    DATA : LV_TEXT TYPE C LENGTH 8.
*Field
    CLEAR : LS_FCAT.
    DO 99 TIMES.
      ADD 1 TO LV_RUNNING.
      LV_RUN_TEXT = LV_RUNNING.

      LCL_UTIL=>CONVERT_ALPHA_IN( EXPORTING I_DATA = LV_RUN_TEXT
                                  IMPORTING E_Data = LV_RUN_TEXT ).

      IF <LFS> IS ASSIGNED.
        UNASSIGN <LFS>.
      ENDIF.
      CONCATENATE LV_DATA LC_F LV_RUN_TEXT INTO LV_TEXT.
      ASSIGN (LV_TEXT) TO <LFS>.
      IF <LFS> IS NOT ASSIGNED.
        EXIT.
      ENDIF.
      LS_FCAT-FIELDNAME = <LFS>.
*Teble Ref
      IF <LFS> IS ASSIGNED.
        UNASSIGN <LFS>.
      ENDIF.
      CONCATENATE LV_DATA LC_T LV_RUN_TEXT INTO LV_TEXT.
      ASSIGN (LV_TEXT) TO <LFS>.
      IF <LFS> IS ASSIGNED.
        LS_FCAT-REF_TABNAME = <LFS>.
      ENDIF.
*Description
      IF <LFS> IS ASSIGNED.
        UNASSIGN <LFS>.
      ENDIF.
      CONCATENATE LV_DATA LC_D LV_RUN_TEXT INTO LV_TEXT.
      ASSIGN (LV_TEXT) TO <LFS>.
      IF <LFS> IS ASSIGNED.
        LS_FCAT-SELTEXT_S = <LFS>.
        LS_FCAT-SELTEXT_M = <LFS>.
        LS_FCAT-SELTEXT_L = <LFS>.
      ENDIF.
      APPEND LS_FCAT TO GT_FCAT.
      CLEAR LS_FCAT.
    ENDDO.

  ENDMETHOD.
  METHOD SET_SORT.
**  CLEAR gs_sort.
**  gs_sort-fieldname = 'LIFNR'.
**  gs_sort-spos = '1'.
**  gs_sort-up = 'X'.
***  gs_sort-subtot = 'X'.
**  APPEND gs_sort TO gt_sort.
  ENDMETHOD.
  METHOD SET_ALV_GRID.
*SAPLKKBL
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = SY-REPID
        "I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
        "I_callback_user_command  = 'USER_COMMAND'
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       i_html_height_top  = 12
*       I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
*       I_GRID_TITLE       =
*       I_GRID_SETTINGS    =
        IS_LAYOUT          = GS_LAYOUT
        IT_FIELDCAT        = GT_FCAT
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
        IT_SORT            = GT_SORT
*       IT_FILTER          =
*       IS_SEL_HIDE        =
        I_DEFAULT          = GC_X
        I_SAVE             = GC_A
*       IS_VARIANT         =
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  = 0
*       I_HTML_HEIGHT_END  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        T_OUTTAB           = GT_RESULT
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.
  METHOD HTML_TOP_OF_PAGE.
*  DATA: text TYPE sdydo_text_element.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 100.
*  text =  'Company Code Data'.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'HEADING'.
*
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
*
*  text = 'User Name : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-uname.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 50.
*
*
*  text = 'Date : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-datum.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 50.
*
*  text = 'Time : '.
*  CALL METHOD document->add_text
*    EXPORTING
*      text         = text
*      sap_emphasis = 'Strong'.
*
*  CALL METHOD document->add_gap
*    EXPORTING
*      width = 6.
*
*  text = sy-uzeit.
*  CALL METHOD document->add_text
*    EXPORTING
*      text      = text
*      sap_style = 'Key'.
*
*  CALL METHOD document->new_line.
*  CALL METHOD document->new_line.
  ENDMETHOD.
  METHOD GET_BOM_MATERIAL_GROUP_AT.
    SELECT MAST~MATNR
           STPO~IDNRK
      INTO TABLE R
      FROM STKO
      INNER JOIN STPO ON STPO~STLNR EQ STKO~STLNR  AND STPO~STLTY EQ STKO~STLTY
      INNER JOIN MAST ON MAST~STLNR EQ STKO~STLNR AND MAST~STLAL EQ STKO~STLAL
      FOR ALL ENTRIES IN GT_IITAB
      WHERE STPO~IDNRK EQ GT_IITAB-MATNR
      AND MAST~STLAN EQ '5'
      AND MAST~WERKS EQ '1000'.

    SORT R BY MATNR IDNRK.
    DELETE ADJACENT DUPLICATES FROM R COMPARING ALL FIELDS.
    DELETE R WHERE MATNR+0(3) NE 'SAT'.
    SORT R BY IDNRK.
  ENDMETHOD.
  METHOD SEND_DATA.
    DATA : LCL_API TYPE REF TO ZCL_SDSSD_SEND_API.

    DATA : LS_DATA TYPE ZSDSMMS042,
           LT_DATA TYPE TABLE OF ZSDSMMS042.

    DATA: BEGIN OF LS_RETURN,
            IS_SUCCESS TYPE STRING,
            STATUS     TYPE P DECIMALS 0,
            MESSAGE    TYPE STRING,
          END OF LS_RETURN.

    DATA : LV_MESTYPE TYPE CHAR1.

    DATA : LS_OUTTAB LIKE LINE OF GT_OUTTAB.

    DATA : LR_WARANTY_GROUP TYPE RANGE OF ZSDSCAC001-VALUE_LOW.

    DATA : LCL_FORM TYPE REF TO ZCL_SDSSD_GEN_DATA_SD_FORM.

    DATA : LT_WARRANTY TYPE ZSDSSDS113_TT,
           LS_WARRANTY TYPE ZSDSSDS113.

    DATA : LT_TMP LIKE LT_WARRANTY.

    DATA : LT_TMP_DO LIKE GT_OUTTAB[].

    DATA : LV_POSNR LIKE LS_WARRANTY-POSNR.

    IF LCL_API IS NOT BOUND.
      CREATE OBJECT LCL_API.
    ENDIF.

    IF LCL_FORM IS NOT BOUND.
      CREATE OBJECT LCL_FORM.
    ENDIF.

    LT_TMP_DO = GT_OUTTAB[].
    SORT LT_TMP_DO BY VBELN.
    DELETE ADJACENT DUPLICATES FROM LT_TMP_DO COMPARING VBELN.

    LOOP AT LT_TMP_DO INTO DATA(LS_TMP_DO).
      LT_TMP = LCL_FORM->GET_LINE_ITEM_WARRANTY( LS_TMP_DO-VBELN ).
      APPEND LINES OF LT_TMP TO LT_WARRANTY.
    ENDLOOP.

*  PERFORM F_GET_CONFIG TABLES LR_WARANTY_GROUP.

    LOOP AT GT_OUTTAB INTO LS_OUTTAB.
      LS_DATA-LFART           = LS_OUTTAB-LFART.
      LS_DATA-CUST_CODE       = LS_OUTTAB-CUST_CODE.
      LS_DATA-VBELN           = LS_OUTTAB-VBELN.
      LS_DATA-LFDAT           = LS_OUTTAB-LFDAT.
      LS_DATA-ERDAT           = LS_OUTTAB-ERDAT.
      LS_DATA-POSNR           = LS_OUTTAB-POSNR.
      LS_DATA-MATNR           = LS_OUTTAB-MATNR.
      LS_DATA-ARKTX           = LS_OUTTAB-ARKTX.
      LS_DATA-LGMNG           = LS_OUTTAB-LGMNG.
      LS_DATA-KUNNR           = LS_OUTTAB-KUNNR.
      LS_DATA-CUST_NAME       = LS_OUTTAB-CUST_NAME.
      LS_DATA-DELI_TO         = LS_OUTTAB-DELI_TO.
      LS_DATA-REMARK          = LS_OUTTAB-REMARK.
      LS_DATA-PICHON          = LS_OUTTAB-PICHON.
      LS_DATA-REMARK_PICHON   = LS_OUTTAB-REMARK_PICHON.
      LS_DATA-LGORT           = LS_OUTTAB-LGORT.

      LV_POSNR = LS_DATA-POSNR.
      LV_POSNR = |{ LV_POSNR ALPHA = IN }|.
      READ TABLE LT_WARRANTY INTO LS_WARRANTY
      WITH KEY VBELN = LS_DATA-VBELN
               POSNR = LV_POSNR.
      IF SY-SUBRC EQ 0.
        LS_DATA-WARRANTY      = 'W'.
      ELSE.
        LS_DATA-WARRANTY      = LS_OUTTAB-WARRANTY.
      ENDIF.
      LS_DATA-SO              = LS_OUTTAB-SO.
      LS_DATA-REQ_MAP         = LS_OUTTAB-REQ_MAP.
      LS_DATA-REQ_INV         = LS_OUTTAB-REQ_INV.
      LS_DATA-SHIP_ADDR       = LS_OUTTAB-SHIP_ADDR.
      LS_DATA-SHIP_PROVINCE   = LS_OUTTAB-SHIP_PROVINCE.
      LS_DATA-POSTCODE        = LS_OUTTAB-POSTCODE.
      LS_DATA-AM_PM           = LS_OUTTAB-AM_PM.
      LS_DATA-LOADING_POINT   = LS_OUTTAB-LOADING_POINT.
      LS_DATA-SALESMAN        = LS_OUTTAB-SALESMAN.
      LS_DATA-DUE_DATE        = LS_OUTTAB-DUE_DATE.
      LS_DATA-PO_NO           = LS_OUTTAB-PO_NO.
      LS_DATA-PROJECT         = LS_OUTTAB-PROJECT.
      LS_DATA-TERM_OF_PAYMENT = LS_OUTTAB-TERM_OF_PAYMENT.
      LS_DATA-CONTACT_NAME    = LS_OUTTAB-CONTACT_NAME.
      LS_DATA-REF_INV         = LS_OUTTAB-REF_INV.
      LS_DATA-SALES_DIV       = LS_OUTTAB-SALES_DIV.
      LS_DATA-SALES_OFFICE    = LS_OUTTAB-SALES_OFFICE.
      LS_DATA-SALES_GROUP     = LS_OUTTAB-SALES_GROUP.
      LS_DATA-UOM             = LS_OUTTAB-UOM.
      LS_DATA-DOC_FLAG        = LS_OUTTAB-DOC_FLAG.
      LS_DATA-MHA             = LS_OUTTAB-MHA.
      LS_DATA-FLAG_BOM        = LS_OUTTAB-FLAG_BOM.
      LS_DATA-REFER_BOM       = LS_OUTTAB-REFER_BOM.
*    LS_DATA-INSULATION      = LS_OUTTAB-INSULATION.
      LS_DATA-SPSOLC          = LS_OUTTAB-SPSOLC.

*    LS_DATA-SPSOLC          = '1100'.
*    LS_DATA-MHA             = 'X'.
*    LS_DATA-SALES_DIV       = '00'.
*    LS_DATA-REF_INV         = '512000000'.
*    LS_DATA-CONTACT_NAME    = 'Test1234'.
*    LS_DATA-PROJECT         = 'Test Project'.
*    LS_DATA-SALESMAN        = 'Test'.
*    LS_DATA-LOADING_POINT   = 'Z0'.
*    LS_DATA-AM_PM           = 'AM'.
*    LS_DATA-REQ_MAP         = 'X'.
*    LS_DATA-REQ_INV         = 'X'.
*    LS_DATA-WARRANTY        = 'X'.
*    LS_DATA-LGORT           = '1100'.
*    LS_DATA-REMARK_PICHON   = 'TEST'.
*    LS_DATA-PICHON          = 'TESt'.
*    LS_DATA-REMARK          = 'TEST'.
*    LS_DATA-REFER_BOM       = 'X'.
*    LS_DATA-FLAG_BOM        = 'X'.
      APPEND LS_DATA TO LT_DATA.
      CLEAR LS_DATA.
    ENDLOOP.

    LCL_API->SEND_DO_FG_TO_SONY( EXPORTING IT_DATA   = LT_DATA
                                 IMPORTING E_MESTYPE = LV_MESTYPE
                                 CHANGING  C_RETURN  = LS_RETURN ).
    IF LS_RETURN-IS_SUCCESS EQ ABAP_TRUE.
      UPDATE_STATUS( ).
      MESSAGE I001 WITH TEXT-S01 DISPLAY LIKE 'S'.
    ELSE.
      IF LS_RETURN-MESSAGE IS INITIAL.
        MESSAGE I001 WITH TEXT-E01 DISPLAY LIKE 'E'.
      ELSE.
        MESSAGE I001 WITH LS_RETURN-MESSAGE DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD SEND_DATA_SMP.
    DATA : LCL_API TYPE REF TO ZCL_SDSSD_SEND_API.

    DATA : LS_DATA TYPE ZSDSMMS040,
           LT_DATA TYPE TABLE OF ZSDSMMS040.

    DATA: BEGIN OF LS_RETURN,
            SUCCESS      TYPE STRING,
            MESSAGE      TYPE STRING,
            ERROR_CODE   TYPE STRING,
            MESSAGE_CODE TYPE STRING,
            DATA         TYPE STRING,
            NEXT_CONTROL TYPE STRING,
            ORDER_NUMBER TYPE STRING,
            SHOW_MESSAGE TYPE STRING,
            PAGER        TYPE STRING,
          END OF LS_RETURN.

    DATA : LV_MESTYPE TYPE CHAR1.

    DATA : LS_OUTTAB LIKE LINE OF GT_OUTTAB.

    IF LCL_API IS NOT BOUND.
      CREATE OBJECT LCL_API.
    ENDIF.

    LOOP AT GT_OUTTAB INTO LS_OUTTAB.
      LS_DATA-LFART           = LS_OUTTAB-LFART.
      LS_DATA-CUST_CODE       = LS_OUTTAB-CUST_CODE.
      LS_DATA-VBELN           = LS_OUTTAB-VBELN.
      LS_DATA-LFDAT           = LS_OUTTAB-LFDAT.
      LS_DATA-ERDAT           = LS_OUTTAB-ERDAT.
      LS_DATA-POSNR           = LS_OUTTAB-POSNR.
      LS_DATA-MATNR           = LS_OUTTAB-MATNR.
      LS_DATA-ARKTX           = LS_OUTTAB-ARKTX.
      LS_DATA-LGMNG           = LS_OUTTAB-LGMNG.
      LS_DATA-KUNNR           = LS_OUTTAB-KUNNR.
      LS_DATA-CUST_NAME       = LS_OUTTAB-CUST_NAME.
      LS_DATA-DELI_TO         = LS_OUTTAB-DELI_TO.
      LS_DATA-REMARK          = LS_OUTTAB-REMARK.
      LS_DATA-PICHON          = LS_OUTTAB-PICHON.
      LS_DATA-REMARK_PICHON   = LS_OUTTAB-REMARK_PICHON.
      LS_DATA-LGORT           = LS_OUTTAB-LGORT.
      LS_DATA-WARRANTY        = LS_OUTTAB-WARRANTY.
      LS_DATA-SO              = LS_OUTTAB-SO.
      LS_DATA-REQ_MAP         = LS_OUTTAB-REQ_MAP.
      LS_DATA-REQ_INV         = LS_OUTTAB-REQ_INV.
      LS_DATA-SHIP_ADDR       = LS_OUTTAB-SHIP_ADDR.
      LS_DATA-SHIP_PROVINCE   = LS_OUTTAB-SHIP_PROVINCE.
      LS_DATA-POSTCODE        = LS_OUTTAB-POSTCODE.
      LS_DATA-AM_PM           = LS_OUTTAB-AM_PM.
      LS_DATA-LOADING_POINT   = LS_OUTTAB-LOADING_POINT.
      LS_DATA-SALESMAN        = LS_OUTTAB-SALESMAN.
      LS_DATA-DUE_DATE        = LS_OUTTAB-DUE_DATE.
      LS_DATA-PO_NO           = LS_OUTTAB-PO_NO.
      LS_DATA-PROJECT         = LS_OUTTAB-PROJECT.
      LS_DATA-TERM_OF_PAYMENT = LS_OUTTAB-TERM_OF_PAYMENT.
      LS_DATA-CONTACT_NAME    = LS_OUTTAB-CONTACT_NAME.
      LS_DATA-REF_INV         = LS_OUTTAB-REF_INV.
      LS_DATA-SALES_DIV       = LS_OUTTAB-SALES_DIV.
      LS_DATA-SALES_OFFICE    = LS_OUTTAB-SALES_OFFICE.
      LS_DATA-SALES_GROUP     = LS_OUTTAB-SALES_GROUP.
      LS_DATA-UOM             = LS_OUTTAB-UOM.
      LS_DATA-DOC_FLAG        = LS_OUTTAB-DOC_FLAG.
      LS_DATA-MHA             = LS_OUTTAB-MHA.
      LS_DATA-FLAG_BOM        = LS_OUTTAB-FLAG_BOM.
      LS_DATA-REFER_BOM       = LS_OUTTAB-REFER_BOM.
      LS_DATA-INSULATION      = LS_OUTTAB-INSULATION.
      LS_DATA-SPSOLC          = LS_OUTTAB-SPSOLC.
      APPEND LS_DATA TO LT_DATA.
      CLEAR LS_DATA.
    ENDLOOP.

    LCL_API->SEND_DO_TO_WMS( EXPORTING IT_DATA   = LT_DATA
                             IMPORTING E_MESTYPE = LV_MESTYPE
                             CHANGING  C_RETURN  = LS_RETURN ).

    IF LS_RETURN-SUCCESS EQ ABAP_TRUE.
      MESSAGE I001 WITH TEXT-S02 DISPLAY LIKE 'S'.
    ELSE.
      IF LS_RETURN-MESSAGE IS INITIAL.
        MESSAGE I001 WITH TEXT-E02 DISPLAY LIKE 'E'.
      ELSE.
        MESSAGE I001 WITH LS_RETURN-MESSAGE DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD GET_SEND_TO.
    R = 'SONY'.
  ENDMETHOD.
  METHOD GET_ZSDSSDC005.
    SELECT MATNR
    INTO TABLE GT_ZSDSSDC005
    FROM ZSDSSDC005.
  ENDMETHOD.
  METHOD GET_GT_TVLAT.
    SELECT LSTEL VTEXT
    INTO TABLE GT_TVLAT
    FROM TVLAT
    WHERE SPRAS EQ 'E'
      AND VSTEL EQ '1000'.
  ENDMETHOD.
  METHOD GET_ZSDSSDC009.
    DATA : LR_KUNNR TYPE RANGE OF KNA1-KUNNR,
           LS_KUNNR LIKE LINE OF LR_KUNNR.

    DATA : BEGIN OF LS_ZSDSSDC009,
             COND1 TYPE ZSDSSDC009-COND1,
           END OF LS_ZSDSSDC009.
    DATA LT_ZSDSSDC009 LIKE TABLE OF LS_ZSDSSDC009.

    SELECT COND1
      FROM ZSDSSDC009
      INTO TABLE LT_ZSDSSDC009
      WHERE COND1 NE SPACE.

    LOOP AT LT_ZSDSSDC009 INTO LS_ZSDSSDC009.
      CLEAR LS_KUNNR.
      LS_KUNNR-SIGN   = 'I'.
      LS_KUNNR-OPTION = 'EQ'.
      LS_KUNNR-LOW    = LS_ZSDSSDC009-COND1.
      APPEND LS_KUNNR TO LR_KUNNR.
      CLEAR LS_ZSDSSDC009.
    ENDLOOP.

    IF LR_KUNNR[] IS INITIAL.
      CLEAR LS_KUNNR.
      LS_KUNNR-SIGN   = 'I'.
      LS_KUNNR-OPTION = 'EQ'.
      LS_KUNNR-LOW    = 'NO_CHECK'.
      APPEND LS_KUNNR TO LR_KUNNR.
    ENDIF.

    CR_DATA = LR_KUNNR.
  ENDMETHOD.
  METHOD GET_ZSDSSDC004.
    SELECT MATNR
    INTO TABLE GT_ZSDSSDC004
    FROM ZSDSSDC004.
  ENDMETHOD.
  METHOD GET_ZSDSCAC002.
    DATA : WA_LGORT LIKE S_LGORT,
           LGORT_1  TYPE MARD-LGORT,
           LGORT_2  TYPE MARD-LGORT.

    SELECT *
      FROM ZSDSCAC002
      WHERE REPID EQ   @SY-REPID
        AND CONST LIKE 'STOR%'
      INTO TABLE @DT_CONS_LGORT.
    SORT DT_CONS_LGORT[] BY VALUE.

    LOOP AT DT_CONS_LGORT INTO WA_CONS_LGORT WHERE CONST+5(2) EQ 'SO'.
      SPLIT WA_CONS_LGORT-VALUE AT ':' INTO LGORT_1 LGORT_2.
      WA_LGORT-LOW = LGORT_1.
      WA_LGORT-HIGH = LGORT_2.
      WA_LGORT-SIGN = 'I'.
      WA_LGORT-OPTION = 'BT'.
      APPEND WA_LGORT TO S_LGORT.
      CLEAR :WA_CONS_LGORT,WA_LGORT,LGORT_1,LGORT_2.
    ENDLOOP.

    SELECT *
      FROM ZSDSCAC002
      INTO TABLE CT_DATA
     WHERE REPID EQ SY-REPID
       AND CONST LIKE 'SPM%'
    ORDER BY VALUE.
*    SELECT * FROM ZSDSCAC002 INTO TABLE DT_CONST WHERE REPID EQ SY-REPID AND CONST LIKE 'SPM%'.
*    SORT DT_CONST BY VALUE.

    SELECT *
      FROM ZSDSCAC002
     WHERE REPID EQ @SY-REPID
       AND CONST LIKE 'CLEW%'
      INTO TABLE @CT_DATA1.
  ENDMETHOD.
  METHOD GET_VBELN_LSTEL.
    SELECT VBELN,
           LSTEL
      FROM LIKP
      WHERE VBELN IN @S_VBELN
      INTO TABLE @R.
  ENDMETHOD.
  METHOD GET_ZSDSSDC003.
    SELECT KUNNR VKBUR
    INTO TABLE GT_CUST_WARRANTY
    FROM ZSDSSDC003.
  ENDMETHOD.
  METHOD GET_ZSDSSDC007.
    SELECT *
      FROM ZSDSSDC007
      INTO TABLE GT_WARRANTY.
  ENDMETHOD.
  METHOD GET_ZSDSSDC006.
    SELECT MATNR AUART
      INTO TABLE GT_ZSDSSDC006
      FROM ZSDSSDC006.
  ENDMETHOD.
  METHOD GET_GT_IITAB.
    CLEAR: GT_IITAB.

    SELECT A~LFART A~VBELN A~LFDAT A~ERDAT B~LGORT
           B~POSNR B~MATNR B~ARKTX B~LGMNG
           A~KUNNR B~UEPOS A~LSTEL C~VTWEG
           C~VKBUR C~KNUMV D~PRODH D~NETWR
           E~ERSDA
           D~POSNR AS POSNV
           B~AUFNR
           B~VGBEL
           C~VKGRP
           G~ZTERM
           G~BSTKD
           G~BSTKD_E
           B~MEINS B~KOWRR
      INTO CORRESPONDING FIELDS OF TABLE GT_IITAB
      FROM LIKP AS A INNER JOIN LIPS AS B ON ( A~VBELN = B~VBELN )
                     INNER JOIN VBAK AS C ON ( C~VBELN = B~VGBEL )
                     INNER JOIN VBAP AS D ON ( D~VBELN = B~VGBEL AND
                                               D~POSNR = B~VGPOS )
                     INNER JOIN MARA AS E ON ( B~MATNR = E~MATNR )
                     INNER JOIN VBKD AS G ON ( G~VBELN = B~VGBEL AND G~POSNR EQ '000000' )
     WHERE A~VBELN IN S_VBELN
       AND A~LFDAT IN S_BLDAT.
  ENDMETHOD.
  METHOD GET_GT_DO_REFER_BOM.
    SELECT A~VBELN A~POSNR A~LGMNG
           B~UEPOS
    INTO TABLE GT_DO_REFER_BOM
    FROM LIPS AS A INNER JOIN VBAP AS B
                   ON ( A~VGBEL EQ B~VBELN
                   AND  A~VGPOS EQ B~POSNR )
    FOR ALL ENTRIES IN GT_IITAB
    WHERE A~VBELN EQ GT_IITAB-VBELN.
  ENDMETHOD.
  METHOD GET_PRICE.
    DATA : BEGIN OF LS_PRICE,
             KNUMV TYPE VBAK-KNUMV,
           END OF LS_PRICE.
    DATA : LT_PRICE LIKE HASHED TABLE OF LS_PRICE WITH UNIQUE KEY KNUMV.

    LT_PRICE = CORRESPONDING #( GT_IITAB[]  DISCARDING DUPLICATES ).

    SELECT PRCD_ELEMENTS~*
      FROM @LT_PRICE AS A
      INNER JOIN PRCD_ELEMENTS ON A~KNUMV EQ PRCD_ELEMENTS~KNUMV AND
                                  PRCD_ELEMENTS~KSCHL EQ 'ZPR0'
      INTO TABLE @GT_KONV.
  ENDMETHOD.
  METHOD GET_DATA_INSU.
    READ TABLE GT_ZSDSSDC005
    WITH KEY MATNR = I_DATA TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      R = 'Insulation'.
    ENDIF.
  ENDMETHOD.
  METHOD GET_LOAD_POINT.
    READ TABLE GT_TVLAT INTO WA_TVLAT
    WITH KEY LSTEL = I_DATA.
    IF SY-SUBRC EQ 0.
      R = WA_TVLAT-VTEXT.
    ENDIF.
  ENDMETHOD.
  METHOD GET_MAP.
    IF NOT R_MAP IS INITIAL.
      R      = 'Y'.
    ENDIF.
  ENDMETHOD.
  METHOD REQ_INV.
    IF NOT R_INV IS INITIAL.
      R      = 'Y'.
    ENDIF.
  ENDMETHOD.
  METHOD GET_AM_PM.
    IF NOT R_AM IS INITIAL.
      R      = 'AM'.
    ENDIF.

    IF NOT R_PM IS INITIAL.
      R      = 'PM'.
    ENDIF.
  ENDMETHOD.
  METHOD GET_MAKT.
    DATA : BEGIN OF LS_MAT,
             MATNR TYPE MARA-MATNR,
           END OF LS_MAT.
    DATA : LT_MAT LIKE HASHED TABLE OF LS_MAT WITH UNIQUE KEY MATNR.

    LT_MAT = CORRESPONDING #( GT_IITAB[]  DISCARDING DUPLICATES ).

    SELECT MAKT~*
      FROM @LT_MAT AS A
      INNER JOIN MAKT ON A~MATNR    EQ MAKT~MATNR AND
                         MAKT~SPRAS EQ @SY-LANGU
      INTO TABLE @CT_DATA.
  ENDMETHOD.
  METHOD READ_MAKT.
    READ TABLE IT_DATA INTO R
    WITH KEY MATNR = I_DATA.
  ENDMETHOD.
  METHOD GET_KNA1.
    DATA : BEGIN OF LS_CUST,
             KUNNR TYPE KNA1-KUNNR,
           END OF LS_CUST.
    DATA : LT_CUST LIKE HASHED TABLE OF LS_CUST WITH UNIQUE KEY KUNNR.

    LT_CUST = CORRESPONDING #( GT_IITAB[]  DISCARDING DUPLICATES ).

    SELECT KNA1~*
      FROM @LT_CUST AS A
      INNER JOIN KNA1 ON A~KUNNR EQ KNA1~KUNNR
      INTO TABLE @CT_DATA.
  ENDMETHOD.
  METHOD READ_KNA1.
    READ TABLE IT_DATA INTO R
    WITH KEY KUNNR = I_DATA.
  ENDMETHOD.
  METHOD GET_VBPA.
    DATA : BEGIN OF LS_DOC,
             VBELN TYPE VBAK-VBELN,
           END OF LS_DOC.
    DATA : LT_DOC LIKE HASHED TABLE OF LS_DOC WITH UNIQUE KEY VBELN.

    LT_DOC = CORRESPONDING #( GT_IITAB[]  DISCARDING DUPLICATES ).

    SELECT VBPA~*
      FROM @LT_DOC AS A
      INNER JOIN VBPA ON A~VBELN    EQ VBPA~VBELN
*                                                                             AND VBPA~PARVW EQ 'AG'
       INTO TABLE @CT_DATA.

  ENDMETHOD.
  METHOD GET_ADRC.
    DATA : BEGIN OF LS_CUST,
             KUNNR TYPE KNA1-KUNNR,
           END OF LS_CUST.
    DATA : LT_CUST    LIKE HASHED TABLE OF LS_CUST WITH UNIQUE KEY KUNNR.

    DATA : BEGIN OF LS_CUST_VB,
             ADRNR TYPE VBPA-ADRNR,
           END OF LS_CUST_VB.
    DATA : LT_CUST_VB LIKE HASHED TABLE OF LS_CUST_VB WITH UNIQUE KEY ADRNR.

    LT_CUST    = CORRESPONDING #( GT_IITAB[] DISCARDING DUPLICATES ).
    LT_CUST_VB = CORRESPONDING #( IT_DATA[] DISCARDING DUPLICATES ).

    SELECT ADRC~*
      FROM @LT_CUST AS A
      INNER JOIN KNA1 ON A~KUNNR EQ KNA1~KUNNR
      INNER JOIN ADRC ON KNA1~ADRNR EQ ADRC~ADDRNUMBER
      INTO TABLE @CT_DATA.

    SELECT ADRC~*
      FROM @LT_CUST_VB AS A
      INNER JOIN ADRC ON A~ADRNR EQ ADRC~ADDRNUMBER
      APPENDING TABLE @CT_DATA.

  ENDMETHOD.
  METHOD UPDATE_STATUS.
    DATA : ls_ZSDSSDT001 TYPE ZSDSSDT001,
           ls_ZSDSSDT002 TYPE ZSDSSDT002.

    DATA : LT_TMP LIKE GT_OUTTAB[].

    LT_TMP = GT_OUTTAB[].

    SORT LT_TMP BY VBELN.
    DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING VBELN.

    LOOP AT LT_TMP INTO DATA(LS_TMP).
      LS_ZSDSSDT001-DONUM = LS_TMP-VBELN.
      LS_ZSDSSDT001-WMSDT = ''.
      LS_ZSDSSDT001-WMSTM = ''.
      LS_ZSDSSDT001-STAPC = ''.
      LS_ZSDSSDT001-STAPT = ''.
      LS_ZSDSSDT001-LOADD = ''.
      LS_ZSDSSDT001-LOADT = ''.
      LS_ZSDSSDT001-CONTD = ''.
      LS_ZSDSSDT001-CONTS = ''.
      LS_ZSDSSDT001-CONTP = ''.
      LS_ZSDSSDT001-CONTT = ''.
      LS_ZSDSSDT001-DOSIG = ''.
      LS_ZSDSSDT001-PODDA = ''.
      LS_ZSDSSDT001-PODRE = ''.
      LS_ZSDSSDT001-DOSDS = ''.
      LS_ZSDSSDT001-DOSDT = ''.
      LS_ZSDSSDT001-ORDDA = ''.
      LS_ZSDSSDT001-DEALC = ''.
      LS_ZSDSSDT001-DEALN = ''.
      LS_ZSDSSDT001-REMAK = LS_TMP-REMARK.
      LS_ZSDSSDT001-SHPAD = LS_TMP-SHIP_ADDR.
      LS_ZSDSSDT001-SHPPV = LS_TMP-SHIP_PROVINCE.
      LS_ZSDSSDT001-AMPMF = LS_TMP-AM_PM.
      LS_ZSDSSDT001-LOADP = LS_TMP-LOADING_POINT.
      LS_ZSDSSDT001-REMAP = LS_TMP-REQ_MAP.
      LS_ZSDSSDT001-REINV = LS_TMP-REQ_INV.
      LS_ZSDSSDT001-SERNF = ''.
      LS_ZSDSSDT001-MAPST = ''.
      LS_ZSDSSDT001-EDOTM = ''.
      LS_ZSDSSDT001-DIVNM = ''.
      LS_ZSDSSDT001-PLTNO = ''.
      LS_ZSDSSDT001-CARNA = ''.
      LS_ZSDSSDT001-TUKTY = ''.
      LS_ZSDSSDT001-SHPNO = ''.
      LS_ZSDSSDT001-ERNAM = SY-UNAME.
      LS_ZSDSSDT001-ERDAT = SY-DATUM.
      LS_ZSDSSDT001-ERZET = SY-UZEIT.
      LS_ZSDSSDT001-AENAM = SY-UNAME.
      LS_ZSDSSDT001-AEDAT = SY-DATUM.
      LS_ZSDSSDT001-AEZET = SY-UZEIT.
      MODIFY ZSDSSDT001 FROM LS_ZSDSSDT001.

      LS_ZSDSSDT002-VBELN   = LS_TMP-VBELN.
      LS_ZSDSSDT002-RUN_ID  = 1.
      LS_ZSDSSDT002-REQ_INV = LS_TMP-REQ_INV.
      LS_ZSDSSDT002-REQ_MAP = LS_TMP-REQ_MAP.
      LS_ZSDSSDT002-AM_PM   = LS_TMP-AM_PM  .
      LS_ZSDSSDT002-ERDAT   = SY-DATUM.
      LS_ZSDSSDT002-ERZET   = SY-UZEIT.
      MODIFY ZSDSSDT002 FROM LS_ZSDSSDT002.
    ENDLOOP.
  ENDMETHOD.
  METHOD CHECK_CHANGE_PROVINCE.
    DATA LS_OUTTAB LIKE LINE OF GT_OUTTAB.

    DATA LV_TABIX TYPE SY-TABIX.

    LOOP AT GT_OUTTAB INTO LS_OUTTAB.
      LV_TABIX = SY-TABIX.

      SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'จังหวัด'.
      IF SY-SUBRC = 0.
        LS_OUTTAB-SHIP_PROVINCE+SY-FDPOS(7) = SPACE.
      ENDIF.

      SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'จ.'.
      IF SY-SUBRC = 0.
        LS_OUTTAB-SHIP_PROVINCE+SY-FDPOS(2) = SPACE.
      ENDIF.

      SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'กรุงเทพ'.
      IF SY-SUBRC = 0.
        CLEAR LS_OUTTAB-SHIP_PROVINCE.
        LS_OUTTAB-SHIP_PROVINCE = 'กรุงเทพมหานคร'.
      ENDIF.

      SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'อยุธยา'.
      IF SY-SUBRC = 0.
        CLEAR LS_OUTTAB-SHIP_PROVINCE.
        LS_OUTTAB-SHIP_PROVINCE = 'พระนครศรีอยุธยา'.
      ENDIF.

      SEARCH LS_OUTTAB-SHIP_PROVINCE FOR 'กทม'.
      IF SY-SUBRC = 0.
        CLEAR LS_OUTTAB-SHIP_PROVINCE.
        LS_OUTTAB-SHIP_PROVINCE = 'กรุงเทพมหานคร'.
      ENDIF.

      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_OUTTAB-SHIP_PROVINCE WITH ''.
      MODIFY GT_OUTTAB FROM LS_OUTTAB INDEX LV_TABIX
                               TRANSPORTING SHIP_PROVINCE.

      CLEAR LS_OUTTAB.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
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
