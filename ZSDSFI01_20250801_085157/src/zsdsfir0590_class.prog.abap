*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0590_CLASS
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
      HTML_TOP_OF_PAGE.
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

    DATA : BEGIN OF LS_PAY,
             ZBUKR TYPE PAYR-ZBUKR,
             GJAHR TYPE PAYR-GJAHR,
             ZALDT TYPE PAYR-ZALDT,
             VBLNR TYPE PAYR-VBLNR,
             LIFNR TYPE PAYR-LIFNR,
           END OF LS_PAY.
    DATA : LT_PAY LIKE HASHED TABLE OF LS_PAY WITH UNIQUE KEY ZBUKR
                                                              GJAHR
                                                              ZALDT
                                                              VBLNR
                                                              LIFNR.

    DATA : BEGIN OF LS_BSAK_VIEW,
             BUKRS TYPE BSAK_VIEW-BUKRS,
             BELNR TYPE BSAK_VIEW-BELNR,
             GJAHR TYPE BSAK_VIEW-GJAHR,
             AUGBL TYPE BSAK_VIEW-AUGBL,
             AUGGJ TYPE BSAK_VIEW-AUGGJ,
           END OF LS_BSAK_VIEW.
    DATA : LT_BSAK_VIEW LIKE HASHED TABLE OF LS_BSAK_VIEW WITH UNIQUE KEY BUKRS
                                                                          BELNR
                                                                          GJAHR
                                                                          AUGBL
                                                                          AUGGJ.
    LT_PAY =  CORRESPONDING #( GT_PAYM  DISCARDING DUPLICATES ).

    IF S_CHECT IS NOT INITIAL.
      SELECT *
      INTO TABLE GT_PAYM
      FROM PAYR
     WHERE ZBUKR EQ P_BUKRS
       AND GJAHR IN S_GJAHR
       AND ZALDT IN S_AUGDT
       AND CHECT IN S_CHECT.
      IF SY-SUBRC EQ 0.
        LT_PAY =  CORRESPONDING #( GT_PAYM  DISCARDING DUPLICATES ).
        SELECT BSAK_VIEW~*
          FROM @LT_PAY AS A
          INNER JOIN BSAK_VIEW ON  A~ZBUKR EQ BSAK_VIEW~BUKRS AND
                                   A~GJAHR EQ BSAK_VIEW~AUGGJ AND
                                   A~ZALDT EQ BSAK_VIEW~AUGDT AND
                                   A~VBLNR EQ BSAK_VIEW~AUGBL AND
                                   A~LIFNR EQ BSAK_VIEW~LIFNR
          WHERE BSAK_VIEW~BUKRS EQ @P_BUKRS
            AND BSAK_VIEW~AUGGJ IN @S_GJAHR
            AND BSAK_VIEW~BELNR IN @S_BELNR
            AND BSAK_VIEW~AUGDT IN @S_AUGDT
            AND BSAK_VIEW~AUGBL IN @S_AUGBL
            AND BSAK_VIEW~LIFNR IN @S_LIFNR
            AND BSAK_VIEW~XBLNR IN @S_XBLNR
          INTO TABLE @GT_BSAK.
      ENDIF.
    ELSE.
      SELECT *
      INTO TABLE @GT_BSAK
      FROM BSAK_VIEW
      WHERE BUKRS EQ @P_BUKRS
        AND AUGGJ IN @S_GJAHR
        AND BELNR IN @S_BELNR
        AND AUGDT IN @S_AUGDT
        AND AUGBL IN @S_AUGBL
        AND LIFNR IN @S_LIFNR
        AND XBLNR IN @S_XBLNR.
    ENDIF.

    IF GT_BSAK[] IS NOT INITIAL.
      LT_BSAK_VIEW =  CORRESPONDING #( GT_BSAK DISCARDING DUPLICATES ).

      SELECT BSAK~*
        FROM @LT_BSAK_VIEW AS A
        INNER JOIN BSAK_VIEW AS BSAK ON A~BUKRS EQ BSAK~BUKRS AND
                                        A~BELNR EQ BSAK~BELNR AND
                                        A~GJAHR EQ BSAK~GJAHR AND
                                        A~BELNR NE BSAK~AUGBL AND
                                        A~AUGBL EQ BSAK~AUGBL AND
                                        A~AUGGJ EQ BSAK~AUGGJ
        INTO TABLE @GT_BSAK1.
    ENDIF.

    IF GT_BSAK1 IS NOT INITIAL.
      GT_BSAK_CHECK =  CORRESPONDING #( GT_BSAK DISCARDING DUPLICATES ).
      SELECT *
        INTO TABLE @GT_BSAD
        FROM BSAD_VIEW
      FOR ALL ENTRIES IN @GT_BSAK_CHECK
        WHERE BUKRS EQ @P_BUKRS
          AND AUGBL EQ @GT_BSAK_CHECK-AUGBL
          AND GJAHR EQ @GT_BSAK_CHECK-AUGGJ.

      SELECT *
        INTO TABLE @GT_BSAS
        FROM BSAS_VIEW
      FOR ALL ENTRIES IN @GT_BSAK
       WHERE BUKRS EQ @P_BUKRS
         AND AUGBL EQ @GT_BSAK-AUGBL
         AND AUGGJ EQ @GT_BSAK-AUGGJ
         AND AUGDT EQ @GT_BSAK-AUGDT
         AND BELNR NE @GT_BSAK-AUGBL
         AND BELNR EQ @GT_BSAK-BELNR
         AND GJAHR EQ @GT_BSAK-GJAHR.

      IF GT_BSAS IS NOT INITIAL.
        SELECT *
          INTO TABLE @GT_BSIS
          FROM BSIS_VIEW
        FOR ALL ENTRIES IN @GT_BSAS
         WHERE BUKRS EQ @P_BUKRS
           AND BELNR EQ @GT_BSAS-AUGBL
           AND GJAHR EQ @GT_BSAS-AUGGJ
           AND BLDAT EQ @GT_BSAS-AUGDT
           AND HKONT EQ @GT_BSAS-HKONT.
      ENDIF.
    ENDIF.

    IF NOT GT_BSAK IS INITIAL.
      SELECT *
        INTO TABLE GT_LFA1
        FROM LFA1
      FOR ALL ENTRIES IN GT_BSAK
       WHERE LIFNR  EQ GT_BSAK-LIFNR.
    ENDIF.
    IF NOT GT_LFA1 IS INITIAL.
      SELECT *
        INTO TABLE GT_LFBK
        FROM LFBK
      FOR ALL ENTRIES IN GT_LFA1
        WHERE LIFNR EQ GT_LFA1-LIFNR.
    ENDIF.
    IF NOT GT_LFBK IS INITIAL.
      SELECT *
        INTO TABLE GT_BANKA
        FROM BNKA
      FOR ALL ENTRIES IN GT_LFBK
        WHERE BANKL EQ GT_LFBK-BANKL
          AND BANKS EQ 'TH'.
    ENDIF.

    IF NOT  GT_BSAK IS INITIAL.
      SELECT *
        INTO TABLE GT_BSEC
        FROM BSEC
      FOR ALL ENTRIES IN GT_BSAK
        WHERE BELNR EQ GT_BSAK-BELNR
          AND GJAHR EQ GT_BSAK-GJAHR.
    ENDIF.
    DATA: GJAHR_D       TYPE P DECIMALS 2,
          GJAHR_P       TYPE P DECIMALS 2,
          GV_GROUP(300) TYPE C,
          GV_NAME(250)  TYPE C,
          GV_CONCT(300) TYPE C,
          GV_PAID       TYPE DMBTR.

    LOOP AT GT_BSAK1 INTO GW_BSAK1.
      CLEAR : GW_PAYM,
              GW_LFA1, GW_LFBK, GW_BANKA,
              GW_BSEC.
*   IF gw_bsak1-bschl = '21' or gw_bsak1-bschl = '25'.
      IF GW_BSAK1-SHKZG = 'S'.
        GW_BSAK1-DMBTR  = GW_BSAK1-DMBTR * -1.
        GW_BSAK1-WRBTR  = GW_BSAK1-WRBTR * -1.
        GW_BSAK1-MWSTS  = GW_BSAK1-MWSTS * -1.
        GW_BSAK1-WMWST  = GW_BSAK1-WMWST * -1.
        GW_BSAK1-QSSHB  = GW_BSAK1-QSSHB * -1.
        GW_BSAK1-QBSHB  = GW_BSAK1-QBSHB * -1.
        GV_PAID         = ( GW_BSAK1-DMBTR - GW_BSAK1-QBSHB ).
*     ENDIF.
      ELSE.
        GW_BSAK1-DMBTR  = GW_BSAK1-DMBTR * 1.
        GW_BSAK1-WRBTR  = GW_BSAK1-WRBTR * 1.
        GW_BSAK1-MWSTS  = GW_BSAK1-MWSTS * 1.
        GW_BSAK1-WMWST  = GW_BSAK1-WMWST * 1.
        GW_BSAK1-QSSHB  = GW_BSAK1-QSSHB * 1.
        GW_BSAK1-QBSHB  = GW_BSAK1-QBSHB * 1.
        GV_PAID         = ( GW_BSAK1-DMBTR - GW_BSAK1-QBSHB ).
      ENDIF.

      READ TABLE GT_LFA1 INTO GW_LFA1 WITH KEY LIFNR = GW_BSAK1-LIFNR.

      READ TABLE GT_LFBK INTO GW_LFBK WITH KEY LIFNR = GW_BSAK1-LIFNR.

      READ TABLE GT_BANKA INTO GW_BANKA WITH KEY BANKL = GW_LFBK-BANKL.

      READ TABLE GT_BSEC INTO GW_BSEC WITH KEY BELNR = GW_BSAK1-BELNR
                                               GJAHR = GW_BSAK1-GJAHR.

      READ TABLE GT_PAYM INTO GW_PAYM WITH KEY VBLNR = GW_BSAK1-AUGBL
                                              GJAHR = GW_BSAK1-AUGGJ
*                                             gjahr = gw_bsak1-gjahr
                                               LIFNR = GW_BSAK1-LIFNR.
      CLEAR: GV_NAME, GV_GROUP.
      GJAHR_D = GW_BSAK1-GJAHR.
      GJAHR_P = GW_PAYM-GJAHR.
      IF GW_BSAK1-LIFNR(2) EQ 'OT' OR GW_BSAK1-LIFNR+2 EQ 'PT' .
        CONCATENATE GW_BSAK1-AUGBL '-' GW_BSAK1-AUGDT+6(2) GW_BSAK1-AUGDT+4(2) GW_BSAK1-AUGDT(4) ': ' GW_LFA1-LIFNR GW_BSEC-NAME1 GW_BSEC-NAME2 INTO GV_GROUP SEPARATED BY SPACE.
        CONCATENATE GW_BSEC-NAME1 GW_BSEC-NAME2 INTO GV_NAME SEPARATED BY SPACE.
      ELSE.
        CONCATENATE GW_BSAK1-AUGBL '-' GW_BSAK1-AUGDT+6(2) GW_BSAK1-AUGDT+4(2) GW_BSAK1-AUGDT(4) ': ' GW_LFA1-LIFNR GW_LFA1-NAME1 GW_LFA1-NAME2 INTO GV_GROUP SEPARATED BY SPACE.
        CONCATENATE GW_LFA1-NAME1 GW_LFA1-NAME2 INTO GV_NAME SEPARATED BY SPACE.
      ENDIF.
      GV_CONCT = GV_GROUP.
      MOVE: GV_CONCT        TO  GW_IPAY2-GROUP,
            GW_BSAK1-BUKRS  TO  GW_IPAY2-BUKRS,
            GW_LFA1-LIFNR   TO  GW_IPAY2-LIFNR,
            GV_NAME         TO  GW_IPAY2-NAME1,
            GW_BSAK1-AUGBL  TO  GW_IPAY2-AUGBL,
            GW_BSAK1-AUGDT  TO  GW_IPAY2-AUGDT,
            GW_PAYM-VBLNR   TO  GW_IPAY2-VBLNR,
            GW_PAYM-CHECT   TO  GW_IPAY2-CHECT ,
            GW_PAYM-CHECF   TO  GW_IPAY2-CHECF  ,
            GW_PAYM-CHECV   TO  GW_IPAY2-CHECV,
            GW_PAYM-VOIDR   TO  GW_IPAY2-VOIDR ,
            GJAHR_D         TO  GW_IPAY2-GJAHR_D,
            GJAHR_P         TO  GW_IPAY2-GJAHR_P,
            GW_BSAK1-BELNR  TO  GW_IPAY2-BELNR,
            GW_BSAK1-BUZEI  TO  GW_IPAY2-BUZEI,
            GW_BSAK1-BUDAT  TO  GW_IPAY2-BUDAT,
            GW_BSAK1-BLDAT  TO  GW_IPAY2-BLDAT,
            GW_BSAK1-WAERS  TO  GW_IPAY2-WAERS,
            GW_BSAK1-XBLNR  TO  GW_IPAY2-XBLNR,
            GW_BSAK1-BLART  TO  GW_IPAY2-BLART,
            GW_BSAK1-SHKZG  TO  GW_IPAY2-SHKZG,
            GW_BSAK1-MWSKZ  TO  GW_IPAY2-MWSKZ,
            GW_BSAK1-DMBTR  TO  GW_IPAY2-DMBTR,
            GW_BSAK1-WRBTR  TO  GW_IPAY2-WRBTR,
            GW_BSAK1-MWSTS  TO  GW_IPAY2-MWSTS,
            GW_BSAK1-WMWST  TO  GW_IPAY2-WMWST,
            GV_PAID         TO  GW_IPAY2-RWBTR,
            GW_PAYM-HBKID   TO  GW_IPAY2-HBKID,
            GW_LFBK-BANKL   TO  GW_IPAY2-BANKL,
            GW_LFBK-BANKN   TO  GW_IPAY2-BANKN,
            GW_LFBK-BKREF   TO  GW_IPAY2-BKREF,
            GW_BANKA-BANKA  TO  GW_IPAY2-BANKA,
            GW_BSAK1-QSSKZ  TO  GW_IPAY2-QSSKZ,
            GW_BSAK1-QSSHB  TO  GW_IPAY2-QSSHB,
            GW_BSAK1-QBSHB  TO  GW_IPAY2-QBSHB,
            GW_BSAK1-QSZNR  TO  GW_IPAY2-QSZNR,
            GW_BSAK1-BSCHL  TO  GW_IPAY2-BSCHL,
            GW_BSAK1-ZUMSK  TO  GW_IPAY2-ZUMSK,
            GW_BSAK1-SGTXT  TO  GW_IPAY2-SGTXT.
      APPEND GW_IPAY2 TO GT_IPAY.
    ENDLOOP.

    LOOP AT GT_BSAD INTO GW_BSAD.
      DATA: GV_QSSHB TYPE BSEG-QSSHB,
            GV_QBSHB TYPE BSEG-QBSHB.

      CLEAR : GW_PAYM,GV_CONCT ,
              GW_LFA1, GW_LFBK, GW_BANKA,
              GW_BSAK, GW_BSAK1, GV_QSSHB, GV_QBSHB,
              GW_BSEC.
      SELECT QSSHB QBSHB
        INTO (GV_QSSHB, GV_QBSHB)
        FROM BSEG
        WHERE BELNR = GW_BSAD-BELNR
          AND GJAHR = GW_BSAD-GJAHR
          AND AUGBL = GW_BSAD-AUGBL.
      ENDSELECT.
      IF GW_BSAD-SHKZG = 'S'.
        GW_BSAD-DMBTR  = GW_BSAD-DMBTR * -1.
        GW_BSAD-WRBTR  = GW_BSAD-WRBTR * -1.
        GW_BSAD-MWSTS  = GW_BSAD-MWSTS * -1.
        GW_BSAD-WMWST  = GW_BSAD-WMWST * -1.
        GV_QSSHB       = GV_QSSHB * -1.
        GV_QBSHB       = GV_QBSHB * -1.
        GV_PAID        = ( GW_BSAD-DMBTR - GV_QBSHB ).
      ELSE.
        GW_BSAD-DMBTR  = GW_BSAD-DMBTR * 1.
        GW_BSAD-WRBTR  = GW_BSAD-WRBTR * 1.
        GW_BSAD-MWSTS  = GW_BSAD-MWSTS * 1.
        GW_BSAD-WMWST  = GW_BSAD-WMWST * 1.
        GV_QSSHB       = GV_QSSHB * 1.
        GV_QBSHB       = GV_QBSHB * 1.
        GV_PAID        = ( GW_BSAD-DMBTR - GV_QBSHB ).
      ENDIF.

      READ TABLE GT_BSAK1 INTO GW_BSAK1 WITH KEY AUGBL = GW_BSAD-AUGBL
                                                 GJAHR = GW_BSAD-GJAHR.

      READ TABLE GT_PAYM INTO GW_PAYM WITH KEY VBLNR = GW_BSAD-AUGBL
                                             GJAHR = GW_BSAD-AUGGJ.
*                                            gjahr = gw_bsad-gjahr.

      READ TABLE GT_BSEC INTO GW_BSEC WITH KEY BELNR = GW_BSAK1-BELNR
                                                GJAHR = GW_BSAK1-GJAHR.

      READ TABLE GT_LFA1 INTO GW_LFA1 WITH KEY LIFNR = GW_BSAK1-LIFNR.

      CLEAR: GV_NAME, GV_GROUP.
      IF GW_BSAK1-LIFNR(2) EQ 'OT' OR GW_BSAK1-LIFNR+2 EQ 'PT' .
        CONCATENATE GW_BSAK1-AUGBL '-' GW_BSAK1-AUGDT+6(2) GW_BSAK1-AUGDT+4(2) GW_BSAK1-AUGDT(4) ': ' GW_LFA1-LIFNR GW_BSEC-NAME1 GW_BSEC-NAME2 INTO GV_GROUP SEPARATED BY SPACE.
        CONCATENATE GW_BSEC-NAME1 GW_BSEC-NAME2 INTO GV_NAME SEPARATED BY SPACE.
      ELSE.
        CONCATENATE GW_BSAK1-AUGBL '-' GW_BSAK1-AUGDT+6(2) GW_BSAK1-AUGDT+4(2) GW_BSAK1-AUGDT(4) ': ' GW_LFA1-LIFNR GW_LFA1-NAME1 GW_LFA1-NAME2 INTO GV_GROUP SEPARATED BY SPACE.
        CONCATENATE GW_LFA1-NAME1 GW_LFA1-NAME2 INTO GV_NAME SEPARATED BY SPACE.
      ENDIF.
      READ TABLE GT_LFBK INTO GW_LFBK WITH KEY LIFNR = GW_BSAK1-LIFNR.

      READ TABLE GT_BANKA INTO GW_BANKA WITH KEY BANKL = GW_LFBK-BANKL.
      GV_CONCT = GV_GROUP.
      MOVE: GV_CONCT        TO  GW_IPAY2-GROUP,
            GW_BSAD-BUKRS   TO  GW_IPAY2-BUKRS,
            GW_LFA1-LIFNR   TO  GW_IPAY2-LIFNR,
            GV_NAME         TO  GW_IPAY2-NAME1,
            GW_BSAD-AUGBL   TO  GW_IPAY2-AUGBL,
            GW_BSAD-AUGDT   TO  GW_IPAY2-AUGDT,
            GW_PAYM-VBLNR   TO  GW_IPAY2-VBLNR,
            GW_PAYM-CHECT   TO  GW_IPAY2-CHECT,
            GW_PAYM-CHECF   TO  GW_IPAY2-CHECF,
            GW_PAYM-CHECV   TO  GW_IPAY2-CHECV,
            GW_PAYM-VOIDR   TO  GW_IPAY2-VOIDR,
            GJAHR_D         TO  GW_IPAY2-GJAHR_D,
            GJAHR_P         TO  GW_IPAY2-GJAHR_P,
            GW_BSAD-VBELN   TO  GW_IPAY2-BELNR,
            GW_BSAD-BUZEI   TO  GW_IPAY2-BUZEI,
            GW_BSAD-BLDAT   TO  GW_IPAY2-BUDAT,
            GW_BSAD-BUDAT   TO  GW_IPAY2-BLDAT,
            GW_BSAD-WAERS   TO  GW_IPAY2-WAERS,
            GW_BSAD-SHKZG   TO  GW_IPAY2-SHKZG,
            GW_BSAD-MWSKZ   TO  GW_IPAY2-MWSKZ,
            GW_BSAD-DMBTR   TO  GW_IPAY2-DMBTR,
            GW_BSAD-WRBTR   TO  GW_IPAY2-WRBTR,
            GW_BSAD-MWSTS   TO  GW_IPAY2-MWSTS,
            GW_BSAD-WMWST   TO  GW_IPAY2-WMWST,
            GV_PAID         TO  GW_IPAY2-RWBTR,
            GW_PAYM-HBKID   TO  GW_IPAY2-HBKID,
            GW_LFBK-BANKL   TO  GW_IPAY2-BANKL,
            GW_LFBK-BANKN   TO  GW_IPAY2-BANKN,
            GW_LFBK-BKREF   TO  GW_IPAY2-BKREF,
            GW_BANKA-BANKA  TO  GW_IPAY2-BANKA,
            GW_BSAD-QSSKZ   TO  GW_IPAY2-QSSKZ,
            GV_QSSHB        TO  GW_IPAY2-QSSHB,
            GV_QBSHB        TO  GW_IPAY2-QBSHB,
*          gw_bsad-QSZNR   TO  gw_ipay2-QSZNR,
            GW_BSAD-BSCHL   TO  GW_IPAY2-BSCHL,
            GW_BSAD-ZUMSK   TO  GW_IPAY2-ZUMSK,
            GW_BSAD-SGTXT   TO  GW_IPAY2-SGTXT.
      APPEND GW_IPAY2 TO GT_IPAY.
    ENDLOOP.
    DELETE GT_IPAY WHERE BELNR EQ SPACE.
* Selected only Cheque payment
    IF S_CHECT NE SPACE.
      DELETE GT_IPAY WHERE CHECT = SPACE.
    ENDIF.

*BOI by Janejira Modify Log #9
    LOOP AT GT_BSAS INTO GW_BSAS.
*   data: gv_qsshb TYPE bseg-qsshb,
*         gv_qbshb TYPE bseg-qbshb.

      CLEAR : GW_PAYM,GV_CONCT ,
              GW_LFA1, GW_LFBK, GW_BANKA,
              GW_BSAK, GW_BSAK1, GV_QSSHB, GV_QBSHB,
              GW_BSEC.
      SELECT QSSHB QBSHB
        INTO (GV_QSSHB, GV_QBSHB)
        FROM BSEG
        WHERE BELNR = GW_BSAS-BELNR
          AND GJAHR = GW_BSAS-GJAHR
          AND AUGBL = GW_BSAS-AUGBL.
      ENDSELECT.
      IF GW_BSAS-SHKZG = 'S'.
        GW_BSAS-DMBTR  = GW_BSAS-DMBTR * -1.
        GW_BSAS-WRBTR  = GW_BSAS-WRBTR * -1.
        GW_BSAS-MWSTS  = GW_BSAS-MWSTS * -1.
        GW_BSAS-WMWST  = GW_BSAS-WMWST * -1.
        GV_QSSHB       = GV_QSSHB * -1.
        GV_QBSHB       = GV_QBSHB * -1.
        GV_PAID        = ( GW_BSAS-DMBTR - GV_QBSHB ).
      ELSE.
        GW_BSAD-DMBTR  = GW_BSAD-DMBTR * 1.
        GW_BSAD-WRBTR  = GW_BSAD-WRBTR * 1.
        GW_BSAD-MWSTS  = GW_BSAD-MWSTS * 1.
        GW_BSAD-WMWST  = GW_BSAD-WMWST * 1.
        GV_QSSHB       = GV_QSSHB * 1.
        GV_QBSHB       = GV_QBSHB * 1.
        GV_PAID        = ( GW_BSAD-DMBTR - GV_QBSHB ).
      ENDIF.
      CLEAR GW_BSIS.
      READ TABLE GT_BSIS INTO GW_BSIS WITH KEY BELNR = GW_BSAS-AUGBL
                                               GJAHR = GW_BSAS-GJAHR
                                               HKONT = GW_BSAS-HKONT.

      IF GW_BSIS-SHKZG = 'S'.
        GW_BSIS-DMBTR  = GW_BSAS-DMBTR - ( GW_BSIS-DMBTR * -1 ).
        GW_BSIS-WRBTR  = GW_BSAS-WRBTR - ( GW_BSIS-WRBTR * -1 ).
        GW_BSIS-MWSTS  = GW_BSAS-MWSTS - ( GW_BSIS-MWSTS * -1 ).
        GW_BSIS-WMWST  = GW_BSAS-WMWST - ( GW_BSIS-WMWST * -1 ).
        GV_QSSHB       = GV_QSSHB - ( GV_QSSHB * -1 ).
        GV_QBSHB       = GV_QBSHB - ( GV_QBSHB * -1 ).
        GV_PAID        = ( GW_BSIS-DMBTR - GV_QBSHB ).
      ELSE.
        GW_BSIS-DMBTR  = GW_BSAS-DMBTR - ( GW_BSIS-DMBTR * 1 ).
        GW_BSIS-WRBTR  = GW_BSAS-WRBTR - ( GW_BSIS-WRBTR * 1 ).
        GW_BSIS-MWSTS  = GW_BSAS-MWSTS - ( GW_BSIS-MWSTS * 1 ).
        GW_BSIS-WMWST  = GW_BSAS-WMWST - ( GW_BSIS-WMWST * 1 ).
        GV_QSSHB       = GV_QSSHB * 1.
        GV_QBSHB       = GV_QBSHB * 1.
        GV_PAID        = ( GW_BSIS-DMBTR - GV_QBSHB ).
      ENDIF.

      READ TABLE GT_BSAK INTO GW_BSAK1 WITH KEY AUGBL = GW_BSAS-AUGBL
                                                 GJAHR = GW_BSAS-GJAHR.

      READ TABLE GT_PAYM INTO GW_PAYM WITH KEY VBLNR = GW_BSAS-AUGBL
                                               GJAHR = GW_BSAS-AUGGJ.
*                                               gjahr = gw_bsas-gjahr.

      READ TABLE GT_BSEC INTO GW_BSEC WITH KEY BELNR = GW_BSAK1-BELNR
                                               GJAHR = GW_BSAK1-GJAHR.

      READ TABLE GT_LFA1 INTO GW_LFA1 WITH KEY LIFNR =  GW_BSAK1-LIFNR.

      CLEAR: GV_NAME, GV_GROUP.
      IF GW_BSAK1-LIFNR(2) EQ 'OT' OR GW_BSAK1-LIFNR+2 EQ 'PT' .
        CONCATENATE GW_BSAK1-AUGBL '-' GW_BSAK1-AUGDT+6(2) GW_BSAK1-AUGDT+4(2) GW_BSAK1-AUGDT(4) ': ' GW_LFA1-LIFNR GW_BSEC-NAME1 GW_BSEC-NAME2 INTO GV_GROUP SEPARATED BY SPACE.
        CONCATENATE GW_BSEC-NAME1 GW_BSEC-NAME2 INTO GV_NAME SEPARATED BY SPACE.
      ELSE.
        CONCATENATE GW_BSAK1-AUGBL '-' GW_BSAK1-AUGDT+6(2) GW_BSAK1-AUGDT+4(2) GW_BSAK1-AUGDT(4) ': ' GW_LFA1-LIFNR GW_LFA1-NAME1 GW_LFA1-NAME2 INTO GV_GROUP SEPARATED BY SPACE.
        CONCATENATE GW_LFA1-NAME1 GW_LFA1-NAME2 INTO GV_NAME SEPARATED BY SPACE.
      ENDIF.
      READ TABLE GT_LFBK INTO GW_LFBK WITH KEY LIFNR = GW_BSAK1-LIFNR.

      READ TABLE GT_BANKA INTO GW_BANKA WITH KEY BANKL = GW_LFBK-BANKL.
      GV_CONCT = GV_GROUP.
      MOVE: GV_CONCT        TO  GW_IPAY2-GROUP,
            GW_BSAS-BUKRS   TO  GW_IPAY2-BUKRS,
            GW_LFA1-LIFNR   TO  GW_IPAY2-LIFNR,
            GV_NAME         TO  GW_IPAY2-NAME1,
            GW_BSAS-AUGBL   TO  GW_IPAY2-AUGBL,
            GW_BSAS-AUGDT   TO  GW_IPAY2-AUGDT,
            GW_PAYM-VBLNR   TO  GW_IPAY2-VBLNR,
            GW_PAYM-CHECT   TO  GW_IPAY2-CHECT,
            GW_PAYM-CHECF   TO  GW_IPAY2-CHECF,
            GW_PAYM-CHECV   TO  GW_IPAY2-CHECV,
            GW_PAYM-VOIDR   TO  GW_IPAY2-VOIDR,
            GJAHR_D         TO  GW_IPAY2-GJAHR_D,
            GJAHR_P         TO  GW_IPAY2-GJAHR_P,
            GW_BSAS-BELNR   TO  GW_IPAY2-BELNR,
            GW_BSAS-BUZEI   TO  GW_IPAY2-BUZEI,
            GW_BSAS-BLDAT   TO  GW_IPAY2-BUDAT,
            GW_BSAS-BUDAT   TO  GW_IPAY2-BLDAT,
            GW_BSAS-WAERS   TO  GW_IPAY2-WAERS,
            GW_BSAS-SHKZG   TO  GW_IPAY2-SHKZG,
            GW_BSAS-MWSKZ   TO  GW_IPAY2-MWSKZ,
            GW_BSIS-DMBTR   TO  GW_IPAY2-DMBTR,
            GW_BSIS-WRBTR   TO  GW_IPAY2-WRBTR,
            GW_BSIS-MWSTS   TO  GW_IPAY2-MWSTS,
            GW_BSIS-WMWST   TO  GW_IPAY2-WMWST,
            GV_PAID         TO  GW_IPAY2-RWBTR,
            GW_PAYM-HBKID   TO  GW_IPAY2-HBKID,
            GW_LFBK-BANKL   TO  GW_IPAY2-BANKL,
            GW_LFBK-BANKN   TO  GW_IPAY2-BANKN,
            GW_LFBK-BKREF   TO  GW_IPAY2-BKREF,
            GW_BANKA-BANKA  TO  GW_IPAY2-BANKA,
            GW_BSIS-QSSKZ   TO  GW_IPAY2-QSSKZ,
            GV_QSSHB        TO  GW_IPAY2-QSSHB,
            GV_QBSHB        TO  GW_IPAY2-QBSHB,
*          gw_bsad-QSZNR   TO  gw_ipay2-QSZNR,
            GW_BSAS-BSCHL   TO  GW_IPAY2-BSCHL,
*          gw_bsas-ZUMSK   TO  gw_ipay2-ZUMSK,
            GW_BSAS-SGTXT   TO  GW_IPAY2-SGTXT.
      APPEND GW_IPAY2 TO GT_IPAY.
    ENDLOOP.
    DELETE GT_IPAY WHERE BELNR EQ SPACE .

    GT_RESULT = GT_IPAY.
  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
*    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
*    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
*
*    ENDLOOP.
    DATA LV_TABIX TYPE SY-TABIX.

    DATA LS_IPAY TYPE GY_RESULT.

    DATA : BEGIN OF LS_BSEG,
             BUKRS TYPE BSEG-BUKRS,
             BELNR TYPE BSEG-BELNR,
             DMBTR TYPE BSEG-DMBTR,
             GJAHR TYPE BSEG-GJAHR,
             BUZEI TYPE BSEG-BUZEI,
           END OF LS_BSEG.
    DATA LT_BSEG LIKE TABLE OF LS_BSEG.

    DATA : BEGIN OF LS_BSEG_COL,
             BELNR TYPE BSEG-BELNR,
             DMBTR TYPE BSEG-DMBTR,
             GJAHR TYPE BSEG-GJAHR,
           END OF LS_BSEG_COL.
    DATA : LT_BSEG_COL LIKE TABLE OF LS_BSEG_COL.

    SELECT BUKRS
           BELNR
           DMBTR
           GJAHR
           BUZEI
      FROM BSEG
      INTO TABLE LT_BSEG
      FOR ALL ENTRIES IN GT_IPAY
      WHERE BELNR EQ GT_IPAY-BELNR
        AND BUKRS EQ P_BUKRS
        AND GJAHR EQ GT_IPAY-GJAHR_D
        AND MWART EQ 'A'.

    LOOP AT LT_BSEG INTO LS_BSEG.
      MOVE-CORRESPONDING LS_BSEG TO LS_BSEG_COL.
      COLLECT LS_BSEG_COL INTO LT_BSEG_COL.
      CLEAR : LS_BSEG_COL,LS_BSEG.
    ENDLOOP.

    LOOP AT GT_RESULT INTO LS_IPAY.
      LV_TABIX = SY-TABIX.

      READ TABLE LT_BSEG_COL INTO LS_BSEG_COL
      WITH KEY BELNR = LS_IPAY-BELNR
               GJAHR = LS_IPAY-GJAHR_D.
      IF SY-SUBRC = 0.
        LS_IPAY-DMBTR_V  = LS_BSEG_COL-DMBTR.
      ENDIF.

      LS_IPAY-DMBTR_BV = LS_IPAY-DMBTR - LS_IPAY-DMBTR_V .

      MODIFY GT_RESULT FROM LS_IPAY INDEX LV_TABIX
                     TRANSPORTING DMBTR_BV DMBTR_V.
      CLEAR LS_IPAY.
    ENDLOOP.
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
