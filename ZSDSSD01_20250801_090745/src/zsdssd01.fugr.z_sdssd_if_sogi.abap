FUNCTION Z_SDSSD_IF_SOGI.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_MESTYP) TYPE  EDIDC-MESTYP OPTIONAL
*"     VALUE(IS_PATHNAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(IS_FILENAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(IS_DATE) TYPE  ZRANGE_T_DATS OPTIONAL
*"     VALUE(IS_UNAME) TYPE  ZRANGE_T_ERNAM OPTIONAL
*"     VALUE(I_AL11) TYPE  CHAR255 OPTIONAL
*"  EXPORTING
*"     VALUE(E_STATUS) TYPE  FLAG
*"  CHANGING
*"     VALUE(CT_RESULT) TYPE  EREC_T_STRING OPTIONAL
*"----------------------------------------------------------------------
  DATA: GV_FIELD(80),
        GV_FIELD_OUT(80),
        GV_DEC             TYPE P DECIMALS 0,
        GCL_TABLEDESCR_REF TYPE REF TO CL_ABAP_TABLEDESCR,
        GCL_DESCR_REF      TYPE REF TO CL_ABAP_STRUCTDESCR,
        GS_IDETAILS        TYPE ABAP_COMPDESCR_TAB,
        GS_XDETAILS        TYPE ABAP_COMPDESCR.
* >> Outbound SO (GI)
  DATA: GT_VBFA  TYPE STANDARD TABLE OF VBFA,
        GS_VBFA  LIKE LINE OF GT_VBFA,
        GS_VBFA2 LIKE LINE OF GT_VBFA,
        GT_VBRK  TYPE STANDARD TABLE OF VBRK,
        GS_VBRK  LIKE LINE OF GT_VBRK,
        GT_LIKP  TYPE STANDARD TABLE OF LIKP,
        GS_LIKP  LIKE LINE OF GT_LIKP,
        GT_VBAK  TYPE STANDARD TABLE OF VBAK,
        GS_VBAK  LIKE LINE OF GT_VBAK.
*        gt_lips TYPE STANDARD TABLE OF lips,  "Add by Wantanee 20170906
*        lw_lips LIKE LINE OF gt_lips."Add by Wantanee 20170906

  DATA: GV_DATE  TYPE SY-DATUM,
        GV_TABIX TYPE SY-TABIX.

* <<
  DATA : GT_MKPF     TYPE TABLE OF MKPF WITH HEADER LINE,
         GT_LIPS     TYPE TABLE OF LIPS WITH HEADER LINE,
         GS_COMWA    TYPE VBCO6,
         GT_VBFA_TAB TYPE TABLE OF VBFA WITH HEADER LINE,
         GS_MSEG     TYPE MSEG,
         BEGIN OF GT_MATDOC OCCURS 0,
           MBLNR      TYPE MSEG-MBLNR,
           MJAHR      TYPE MSEG-MJAHR,
           VBTYP_N(1),
         END OF GT_MATDOC.
  FIELD-SYMBOLS: <FS>,<FS_OUT>.

*  CLEAR: gs_idoc_control.
  REFRESH: GT_GI_OUT_H,GT_GI_OUT_I,GS_IDETAILS.
  ",gt_edidc,gt_edidd.

  "Get component GT_OUT
  GCL_TABLEDESCR_REF ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( GT_GI_OUT_H ).
  GCL_DESCR_REF ?= GCL_TABLEDESCR_REF->GET_TABLE_LINE_TYPE( ).
  GS_IDETAILS[] = GCL_DESCR_REF->COMPONENTS[].

* >> Get Data
*  REFRESH: gt_vbrk,gt_likp.
*
*  SELECT * INTO TABLE gt_vbrk
*           FROM vbrk
*           WHERE erdat IN i_date
*           AND   vbtyp = 'M'
*           AND   fksto = space.
*
*  SELECT * INTO TABLE gt_likp
*           FROM likp
*           WHERE erdat IN i_date.
*
*  IF NOT gt_vbrk[] IS INITIAL.
*    SELECT * INTO TABLE gt_vbfa
*             FROM vbfa
*             FOR ALL ENTRIES IN gt_vbrk
*             WHERE vbeln   = gt_vbrk-vbeln
*             AND   vbtyp_v = 'C'.
*  ENDIF.
*  IF NOT gt_likp[] IS INITIAL.
*    SELECT * APPENDING TABLE gt_vbfa
*             FROM vbfa
*             FOR ALL ENTRIES IN gt_likp
*             WHERE vbeln   = gt_likp-vbeln
*             AND   vbtyp_v = 'C'.
*  ENDIF.
*
*  IF NOT gt_vbfa[] IS INITIAL.
*    SELECT * INTO TABLE gt_vbak
*             FROM vbak
*             FOR ALL ENTRIES IN gt_vbfa
*             WHERE vbeln = gt_vbfa-vbelv
*             AND   ernam IN i_uname. "WEB_ITF
*  ENDIF.
*
*  IF NOT gt_vbak[] IS INITIAL.
*    SELECT * INTO TABLE gt_vbfa
*             FROM vbfa
*             FOR ALL ENTRIES IN gt_vbak
*             WHERE vbelv = gt_vbak-vbeln
*             AND   vbtyp_n IN ('J','M').
*  ENDIF.
*
*  LOOP AT gt_vbfa INTO gs_vbfa WHERE vbtyp_n = 'M'.
*    gv_tabix = sy-tabix.
*    CLEAR: gs_vbrk.
*    SELECT SINGLE * INTO gs_vbrk
*           FROM vbrk
*           WHERE vbeln = gs_vbfa-vbeln
*           AND   fksto = ''.
*    IF sy-subrc <> 0.
*      DELETE gt_vbfa INDEX gv_tabix.
*    ENDIF.
*  ENDLOOP.

*break 3SDS006.

  REFRESH : GT_MKPF,
            GT_LIKP,
            GT_MATDOC,
            GT_LIPS.
  CLEAR : GS_COMWA.

  SELECT * FROM MKPF
    INTO TABLE GT_MKPF
    WHERE CPUDT IN IS_DATE
    AND   LE_VBELN NE SPACE.
  IF SY-SUBRC = 0.

    SORT GT_MKPF BY LE_VBELN.
    DELETE ADJACENT DUPLICATES FROM GT_MKPF
    COMPARING LE_VBELN.

**>> ins ISS CH 05.05.2017 14:09:21
*    "exclude if matnr SPTRANSPORT existed in DO
*    SELECT * FROM lips INTO TABLE gt_lips
*      FOR ALL ENTRIES IN gt_mkpf
*      WHERE vbeln = gt_mkpf-le_vbeln.
*    IF sy-subrc = 0.
*      LOOP AT gt_mkpf.
*        READ TABLE gt_lips WITH KEY vbeln = gt_mkpf-le_vbeln
*                                    matnr = 'SPTRANSPORT'.
*        IF sy-subrc = 0.
*          DELETE gt_mkpf WHERE le_vbeln = gt_mkpf-le_vbeln.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
**<< ins ISS CH| 05.05.2017 14:09:21

    SELECT * FROM LIKP
      INTO TABLE GT_LIKP
      FOR ALL ENTRIES IN GT_MKPF
      WHERE VBELN = GT_MKPF-LE_VBELN.
*      AND   LFART IN ('ZDS1','ZDR2','ZDR1','ZDR2'). "added on 05/05/2017

*      AND   lfart = 'ZDS1'.
    IF SY-SUBRC = 0.

      LOOP AT GT_MKPF.

        REFRESH : GT_VBFA_TAB.
        CLEAR : GS_COMWA.

        GS_COMWA-VBELN = GT_MKPF-LE_VBELN.

        CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
          EXPORTING
            COMWA         = GS_COMWA
          TABLES
            VBFA_TAB      = GT_VBFA_TAB
          EXCEPTIONS
            NO_VBFA       = 1
            NO_VBUK_FOUND = 2
            OTHERS        = 3.
        IF SY-SUBRC = 0 AND GT_VBFA_TAB[] IS NOT INITIAL.

          REFRESH GT_MATDOC.

          "keep mat doc
          LOOP AT GT_VBFA_TAB WHERE VBTYP_N = 'R'
                              OR VBTYP_N = 'h'. "loop by mat doc
            MOVE : GT_VBFA_TAB-VBELN TO GT_MATDOC-MBLNR,
                   GT_VBFA_TAB-MJAHR TO GT_MATDOC-MJAHR,
                   GT_VBFA_TAB-VBTYP_N TO GT_MATDOC-VBTYP_N.
            COLLECT GT_MATDOC. CLEAR GT_MATDOC.
          ENDLOOP.

          LOOP AT GT_MATDOC. "loop mat doc

            CLEAR GV_SKIP.

            "Header
            CLEAR GS_GI_OUT_H.
            GS_GI_OUT_H-IND   = 'H'.

            READ TABLE GT_VBFA_TAB INTO GS_VBFA
            WITH KEY VBELN = GT_MKPF-LE_VBELN
                     VBTYP_V = 'C'.
            IF SY-SUBRC = 0.
              GS_GI_OUT_H-VBELN = GS_VBFA-VBELV. "so number
              GS_GI_OUT_H-ID = GS_VBFA-VBELV. "ID

              CLEAR GS_VBAK.
              SELECT SINGLE * FROM VBAK
                INTO GS_VBAK
                WHERE VBELN = GS_VBFA-VBELV
                AND   ERNAM IN IS_UNAME "WEB_ITF..
                AND   AUART IN ('ZO01','ZOR1').
              IF SY-SUBRC = 0.
                GS_GI_OUT_H-WEBNO = GS_VBAK-BNAME. "web no.
              ELSE.
                GV_SKIP = 'X'. "skip this line
              ENDIF.
            ELSE.
**>> ins ISS CH 05.05.2017 11:57:44
              READ TABLE GT_VBFA_TAB INTO GS_VBFA
              WITH KEY VBELN = GT_MKPF-LE_VBELN
                       VBTYP_V = 'H'. "Return
              IF SY-SUBRC = 0.
                GS_GI_OUT_H-VBELN = GS_VBFA-VBELV. "so number
                GS_GI_OUT_H-ID = GS_VBFA-VBELV. "ID

                CLEAR GS_VBAK.
                SELECT SINGLE * FROM VBAK
                  INTO GS_VBAK
                  WHERE VBELN = GS_VBFA-VBELV
                  AND   ERNAM IN IS_UNAME. "WEB_ITF.
*                  AND   AUART IN ('ZO01','ZOR1').
                IF SY-SUBRC = 0.
                  GS_GI_OUT_H-WEBNO = GS_VBAK-BNAME. "web no.
                ELSE.
                  GV_SKIP = 'X'. "skip this line
                ENDIF.

                GS_GI_OUT_H-ABGRU = 'C'. "Status Return

              ENDIF.
**<< ins ISS CH| 05.05.2017 11:57:44
            ENDIF.

            CLEAR GS_LIKP.
            READ TABLE GT_LIKP INTO GS_LIKP
            WITH KEY VBELN = GT_MKPF-LE_VBELN.
            IF SY-SUBRC = 0.
              GS_GI_OUT_H-VBELN_DO = GS_LIKP-VBELN. "DO no.
              GS_GI_OUT_H-LFDAT    = GS_LIKP-LFDAT. "Delivery date
              GS_GI_OUT_H-KUNUM    = GS_LIKP-KUNNR. "ship-to
            ENDIF.

*            CLEAR gs_vbfa.
*            READ TABLE gt_vbfa_tab INTO gs_vbfa
*            WITH KEY vbelv = gt_mkpf-le_vbeln
*                     vbtyp_n = 'R'.
*            IF sy-subrc = 0.
*              gs_gi_out_h-mblnr = gs_vbfa-vbeln. "mat doc
*
*              CLEAR gs_mseg.
*              SELECT SINGLE * FROM mseg
*                INTO gs_mseg
*                WHERE smbln = gs_vbfa-vbeln
*                AND   sjahr = gs_vbfa-mjahr.
*              IF sy-subrc = 0.
*                IF gs_mseg-smbln IS NOT INITIAL.
*                  gs_gi_out_h-abgru = 'C'. "Status
*                ENDIF.
*              ENDIF.
*
*            ENDIF.

            GS_GI_OUT_H-MBLNR = GT_MATDOC-MBLNR. "mat doc

            "check mat doc status
            CLEAR GS_MSEG.
            SELECT SINGLE * FROM MSEG
              INTO GS_MSEG
              WHERE SMBLN = GT_MATDOC-MBLNR
              AND   SJAHR = GT_MATDOC-MJAHR.
            IF SY-SUBRC = 0.
              IF GS_MSEG-SMBLN IS NOT INITIAL.
                GS_GI_OUT_H-ABGRU = 'C'. "Status
              ENDIF.
            ELSE.
              IF GT_MATDOC-VBTYP_N = 'H'.
                GS_GI_OUT_H-ABGRU = 'C'. "Status
              ENDIF.
            ENDIF.

**>> ins ISS CH 05.05.2017 16:14:11
            "exclude if matnr SPTRANSPORT existed in DO
            LOOP AT GT_VBFA_TAB INTO GS_VBFA WHERE ( VBTYP_N = 'R' OR VBTYP_N = 'H' )
                                AND VBELN = GT_MATDOC-MBLNR
                                AND MJAHR = GT_MATDOC-MJAHR
                                AND MATNR = 'SPTRANSPORT'.
              GV_SKIP = 'X'.
              EXIT.
            ENDLOOP.

            IF GV_SKIP = 'X' OR GS_GI_OUT_H-VBELN IS INITIAL.
              CLEAR GS_GI_OUT_H.
              CONTINUE.
            ENDIF.
**<< ins ISS CH| 05.05.2017 16:14:11

            APPEND GS_GI_OUT_H TO GT_GI_OUT_H.

* >> Add to IDOC
*            gs_edidd-segnam = 'ZDCS12H'.
*            gs_edidd-sdata = gs_gi_out_h.
*
*            APPEND gs_edidd TO gt_edidd. CLEAR gs_edidd.
            CLEAR: GS_GI_OUT_H.
* << Add to IDOC

            CLEAR GS_VBFA.
            LOOP AT GT_VBFA_TAB INTO GS_VBFA WHERE ( VBTYP_N = 'R' OR VBTYP_N = 'h' )
                                            AND VBELN = GT_MATDOC-MBLNR
                                            AND MJAHR = GT_MATDOC-MJAHR.

              "Item
              CLEAR GS_GI_OUT_I.
              GS_GI_OUT_I-IND   = 'I'.

              CLEAR GS_VBFA2.
              READ TABLE GT_VBFA_TAB INTO GS_VBFA2
              WITH KEY VBELN = GT_MKPF-LE_VBELN
                       VBTYP_V = 'C'.
              IF SY-SUBRC = 0.
                GS_GI_OUT_I-VBELN = GS_VBFA2-VBELV. "so number
                GS_GI_OUT_I-ID = GS_VBFA2-VBELV. "so number
              ELSE.
                READ TABLE GT_VBFA_TAB INTO GS_VBFA2
                WITH KEY VBELN = GT_MKPF-LE_VBELN
                         VBTYP_V = 'H'.
                IF SY-SUBRC = 0.
                  GS_GI_OUT_I-VBELN = GS_VBFA2-VBELV. "so number
                  GS_GI_OUT_I-ID = GS_VBFA2-VBELV. "so number
                ENDIF.
              ENDIF.

              GS_GI_OUT_I-MATNR = GS_VBFA-MATNR. "material no.
              GS_GI_OUT_I-MBLNR = GS_VBFA-VBELN. "mat doc
              GS_GI_OUT_I-ZEILE = GS_VBFA-POSNN+2(4). "item in mat doc
              GS_GI_OUT_I-RFMNG = GS_VBFA-RFMNG. "qty added on 05/05/2017

              APPEND GS_GI_OUT_I TO GT_GI_OUT_I.

* >> Add to IDOC
*              READ TABLE gt_edidd WITH KEY segnam = 'ZDCS12I'
*                                           sdata  = gs_gi_out_i
*                                           TRANSPORTING NO FIELDS.
*              IF sy-subrc NE 0.
*              gs_edidd-segnam = 'ZDCS12I'.
*              gs_edidd-sdata = gs_gi_out_i.
*
*              APPEND gs_edidd TO gt_edidd. CLEAR gs_edidd.
*              ENDIF.
              CLEAR: GS_GI_OUT_I.
* << Add to IDOC

            ENDLOOP.

          ENDLOOP.

        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDIF.
*<-------Change by SDS 06/09/2017 Wantanee T41K927725
  SELECT * FROM LIKP
  INTO TABLE GT_LIKP
  WHERE ERDAT IN IS_DATE.
*  AND   LFART IN ('ZDS1','ZDR2'). "added on 05/05/2017
*        AND   lfart = 'ZDS1'.

  LOOP AT   GT_LIKP INTO GS_LIKP.
    READ TABLE GT_MKPF INTO GT_MKPF WITH KEY LE_VBELN = GS_LIKP-VBELN.
    IF SY-SUBRC NE 0.
      REFRESH : GT_VBFA_TAB,GT_LIPS.



      CLEAR : GS_COMWA.

      GS_COMWA-VBELN = GS_LIKP-VBELN.

      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
        EXPORTING
          COMWA         = GS_COMWA
        TABLES
          VBFA_TAB      = GT_VBFA_TAB
        EXCEPTIONS
          NO_VBFA       = 1
          NO_VBUK_FOUND = 2
          OTHERS        = 3.


      IF SY-SUBRC = 0 AND GT_VBFA_TAB[] IS NOT INITIAL.

        SELECT *
          FROM LIPS
         INTO TABLE GT_LIPS
          WHERE VBELN = GS_LIKP-VBELN.

        CLEAR GV_SKIP.

        "Header
        CLEAR GS_GI_OUT_H.
        GS_GI_OUT_H-IND   = 'H'.

        READ TABLE GT_VBFA_TAB INTO GS_VBFA
        WITH KEY VBELN = GS_LIKP-VBELN
                 VBTYP_V = 'C'.
        IF SY-SUBRC = 0.
          GS_GI_OUT_H-VBELN = GS_VBFA-VBELV. "so number
          GS_GI_OUT_H-ID = GS_VBFA-VBELV. "ID

          CLEAR GS_VBAK.
          SELECT SINGLE * FROM VBAK
            INTO GS_VBAK
            WHERE VBELN = GS_VBFA-VBELV
            AND   ERNAM IN IS_UNAME. "WEB_ITF..
*            AND   AUART IN ('ZS04','ZS05').
          IF SY-SUBRC = 0.
            GS_GI_OUT_H-WEBNO = GS_VBAK-BNAME. "web no.
          ELSE.
            GV_SKIP = 'X'. "skip this line
          ENDIF.
        ELSE.
*                        *>> ins ISS CH 05.05.2017 11:57:44
          READ TABLE GT_VBFA_TAB INTO GS_VBFA
          WITH KEY VBELN = GT_MKPF-LE_VBELN
                   VBTYP_V = 'H'. "Return
          IF SY-SUBRC = 0.
            GS_GI_OUT_H-VBELN = GS_VBFA-VBELV. "so number
            GS_GI_OUT_H-ID = GS_VBFA-VBELV. "so number

            CLEAR GS_VBAK.
            SELECT SINGLE * FROM VBAK
              INTO GS_VBAK
              WHERE VBELN = GS_VBFA-VBELV
              AND   ERNAM IN IS_UNAME. "WEB_ITF.
*              AND   AUART IN ('ZS04','ZS05').
            IF SY-SUBRC = 0.
              GS_GI_OUT_H-WEBNO = GS_VBAK-BNAME. "web no.
            ELSE.
              GV_SKIP = 'X'. "skip this line
            ENDIF.

            GS_GI_OUT_H-ABGRU = 'C'. "Status Return

          ENDIF.
*                        *<< ins ISS CH| 05.05.2017 11:57:44
        ENDIF.

*                                    CLEAR gs_likp.

        GS_GI_OUT_H-VBELN_DO = GS_LIKP-VBELN. "DO no.
        GS_GI_OUT_H-MBLNR = GS_LIKP-VBELN. "ใส่ DO แทน Mat doc
        GS_GI_OUT_H-LFDAT    = GS_LIKP-LFDAT. "Delivery date
        GS_GI_OUT_H-KUNUM    = GS_LIKP-KUNNR. "ship-to

*                                      gs_gi_out_h-abgru = 'C'. "Status

*                        *>> ins ISS CH 05.05.2017 16:14:11

        IF GV_SKIP = 'X' OR GS_GI_OUT_H-VBELN IS INITIAL.
          CLEAR GS_GI_OUT_H.
          CONTINUE.
        ENDIF.
*                        *<< ins ISS CH| 05.05.2017 16:14:11

        APPEND GS_GI_OUT_H TO GT_GI_OUT_H.

*                         >> Add to IDOC
*                                    gs_edidd-segnam = 'ZDCS12H'.
*                                    gs_edidd-sdata = gs_gi_out_h.
*
*                                    APPEND gs_edidd TO gt_edidd. CLEAR gs_edidd.
        CLEAR: GS_GI_OUT_H.
*                         << Add to IDOC

        CLEAR GS_VBFA.

        LOOP AT GT_LIPS WHERE VBELN EQ GS_LIKP-VBELN.
          CLEAR GS_GI_OUT_I.
          GS_GI_OUT_I-IND   = 'I'.

          CLEAR GS_VBFA2.
          READ TABLE GT_VBFA_TAB INTO GS_VBFA2
          WITH KEY VBELN = GS_LIKP-VBELN
                   VBTYP_V = 'C'.
          IF SY-SUBRC = 0.
            GS_GI_OUT_I-VBELN = GS_VBFA2-VBELV. "so number
            GS_GI_OUT_I-ID = GS_VBFA2-VBELV. "ID
          ELSE.
            READ TABLE GT_VBFA_TAB INTO GS_VBFA2
            WITH KEY VBELN = GS_LIKP-VBELN
                     VBTYP_V = 'H'.
            IF SY-SUBRC = 0.
              GS_GI_OUT_I-VBELN = GS_VBFA2-VBELV. "so number
              GS_GI_OUT_I-ID = GS_VBFA2-VBELV. "so number
            ENDIF.
          ENDIF.

          GS_GI_OUT_I-MATNR = GT_LIPS-MATNR. "material no.
          GS_GI_OUT_I-MBLNR = GT_LIPS-VBELN. "mat doc
          GS_GI_OUT_I-ZEILE = GT_LIPS-POSNR+2(4). "item posnr

          GS_GI_OUT_I-RFMNG = GT_LIPS-LFIMG. "qty added on 05/05/2017

          APPEND GS_GI_OUT_I TO GT_GI_OUT_I.

*                               >> Add to IDOC
*                                            READ TABLE gt_edidd WITH KEY segnam = 'ZDCS12I'
*                                                                         sdata  = gs_gi_out_i
*                                                                         TRANSPORTING NO FIELDS.
*                                            IF sy-subrc NE 0.
*                                            gs_edidd-segnam = 'ZDCS12I'.
*                                            gs_edidd-sdata = gs_gi_out_i.
*
*                                            APPEND gs_edidd TO gt_edidd. CLEAR gs_edidd.
*                                            ENDIF.
          CLEAR: GS_GI_OUT_I.
*                               << Add to IDOC
        ENDLOOP.



      ENDIF.
    ENDIF.



  ENDLOOP.

*  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_ITEM      = gt_out
*                                                             I_SEPARATOR = '","'
*                                                             I_START_END_VALUE = '"').
*
*
*DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.
*
*DATA : LV_STATUS TYPE C.
*
*DATA : LV_FILE TYPE EVE_TT_STRING.
*
*DATA : LT_FILE TYPE EREC_T_STRING,
*       LS_FILE LIKE LINE OF LT_FILE.
*
*IF LCL_FTP IS NOT BOUND.
*  CREATE OBJECT LCL_FTP.
*ENDIF.
*
*  LS_FILE = 'Hello Wold'.
*  APPEND LS_FILE TO LT_FILE.
*  LS_FILE = 'Hello Wold1'.
*  APPEND LS_FILE TO LT_FILE.
*
*  LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'SONY_LOGISTIC_FG/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                     I_FILE_NAME   = 'TEST_FTP_FILE.txt'
*                                     I_USER        = 'ds'
*                                     I_PASS        = 'ds=20240521'
*                                     I_IP          = '172.31.136.249'
*                                     I_PORT        = '21'
*                                     IT_DATA       = LT_FILE ).
*  IF LV_STATUS EQ 'S'.
*    " SUCCESS FTP FILE
*  ELSE.
*    " CANNOT FTP FILE
*  ENDIF.
* << Get Data
*
*  SORT gt_edidd BY segnam ASCENDING.
*
*
** >> Process Data
*  IF gt_edidd[] IS INITIAL.
*    MESSAGE s000(38) WITH 'No data found'.
*  ELSE.
*    gs_path-pathname = IS_PATHNAME.
*    gs_path-filename = IS_FILENAME.
*    gs_edidd-segnam = 'ZSDSSDS000'.
*    gs_edidd-sdata = gs_path.
*    INSERT gs_edidd INTO gt_edidd INDEX 1.
*
*    CLEAR: gs_idoc_control.
*    SELECT SINGLE * INTO gs_edp13
*           FROM edp13
*           WHERE mestyp = IS_MESTYP.
*    gs_idoc_control-mestyp = gs_edp13-mestyp.
*    gs_idoc_control-idoctp = gs_edp13-idoctyp.
*    gs_idoc_control-rcvpor = gs_edp13-rcvpor.
*    gs_idoc_control-rcvprn = gs_edp13-rcvprn.
*    gs_idoc_control-rcvprt = gs_edp13-rcvprt.
*
*    CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
*      EXPORTING
*        master_idoc_control                  = gs_idoc_control
**     OBJ_TYPE                             = ''
**     CHNUM                                = ''
*      TABLES
*        communication_idoc_control           = gt_edidc
*        master_idoc_data                     = gt_edidd
*      EXCEPTIONS
*        error_in_idoc_control                = 1
*        error_writing_idoc_status            = 2
*        error_in_idoc_data                   = 3
*        sending_logical_system_unknown       = 4
*        OTHERS                               = 5 .
*
*    COMMIT WORK AND WAIT.
*
*    READ TABLE gt_edidc INTO gs_edidc INDEX 1.
*    CALL FUNCTION 'EDI_DOCUMENT_DEQUEUE_LATER'
*      EXPORTING
*        docnum                 = gs_edidc-docnum
*      EXCEPTIONS
*        idoc_is_not_to_dequeue = 1.
*
*    ct_edidc[] = gt_edidc[].
*    ct_edidd[] = gt_edidd[].
*
*  ENDIF.

  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_HEADER = GT_GI_OUT_H
                                                              I_ITEM      = GT_GI_OUT_I
                                                              I_GROUP_BY_DETAIL_FILED1 = 'X'
                                                              I_SEPARATOR = '","'
                                                              I_START_END_VALUE = '"').
  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.

  DATA: LV_PATH_FILE TYPE STRING.

*  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = 'Z_SDSSD_IF_SOGI' "LC_CON-REPID
*                                                 I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> ต้องการค่าเดียว ใส่ ABAP_TRUE ต้องการหลายค่าให้ Comment
*                                                 I_PARAM             = SY-SYSID "LC_CON-SEPARATOR
**                                                  I_PARAM_EXT      =
*                                                 CHANGING  C_RETURN  = LV_PATH_FILE ).


* IF sy-sysid = 'F36'.
*     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZSDI029'.
*  ELSEIF sy-sysid = 'F46'.
*     LV_PATH_FILE = '/interface/Z_DS/SDS/20_OUTBOUND/DSS/ZSDI029'.
*  ELSE.
**     LV_PATH_FILE_SF = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
**     LV_PATH_FILE_SONY_SP = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/Sony_Logi/ZMMI014'.
*  ENDIF.


  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.

*  LS_FILE = 'Hello Wold'.
*  APPEND LS_FILE TO LT_FILE.
*  LS_FILE = 'Hello Wold1'.
*  APPEND LS_FILE TO LT_FILE..
  LV_PATH_FILE = I_AL11.
  CONCATENATE 'gso_' SY-DATUM SY-TIMLO  '.csv' INTO LV_PATH.
  LV_STATUS = LCL_FTP->FTP_FILE_PUT("I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                      I_AL11_PATH   = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZSDI029'
                                     I_AL11_PATH = LV_PATH_FILE
                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                    " I_USER        = 'ds'
                                     "I_PASS        = 'ds=20240521'
                                    " I_IP          = '172.31.136.250'
                                    " I_PORT        = '21'
                                      I_DATA_SPIDER = 'X'
                                     IT_DATA       = CT_RESULT ).




*   LV_STATUS = LCL_FTP->FTP_FILE_PUT(  I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
*                                     I_USER        = 'ds'
*                                     I_PASS        = 'ds=20240521'
*                                     I_IP          = '172.31.136.250'
*                                     I_PORT        = '21'
*                                     IT_DATA       = LT_FILE ).


  IF LV_STATUS EQ 'S'.
    E_STATUS = 'S'.
  ELSE.
    E_STATUS = 'E'.
  ENDIF.

ENDFUNCTION.
