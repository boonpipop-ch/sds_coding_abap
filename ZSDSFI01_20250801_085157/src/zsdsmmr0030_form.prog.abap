*&---------------------------------------------------------------------*
*& Include          ZSDSFII0030_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_END_OF_SELECTION .
  CASE GV_ERROR.
    WHEN GC_ERROR.
      MESSAGE S000 WITH TEXT-100 DISPLAY LIKE 'E'.
    WHEN OTHERS.
      IF GT_HEADER[] IS NOT INITIAL.
        COLTROL_QT-TOP_LINE = 0.
        PERFORM F_SHOW_REPORT.
      ELSE.
        MESSAGE S003 DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
ENDFORM.                    " F_END_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_DATA.
  SELECT VBAK~VKBUR
         VBAK~VBELN
         VBAK~VKGRP
         VBAK~AUDAT
         VBAK~KUNNR
         VBAK~NETWR
         VBAK~AUFNR
         VBAK~VTWEG
         VBAK~IHREZ
         VBAK~BSTDK
         VBAK~BSTNK
         KNA1~NAME1
         VBAK~AUART
    FROM VBAK
    LEFT JOIN KNA1 ON VBAK~KUNNR = KNA1~KUNNR
    INTO CORRESPONDING FIELDS OF TABLE GT_HEADER
    WHERE   VBELN IN S_VBELN
      AND   IHREZ IN S_IHREZ.

  IF GT_HEADER IS NOT INITIAL.
    PERFORM F_GET_DATA_HEADER.
    PERFORM F_GET_DATA_DETAIL.
  ENDIF.
ENDFORM.                    "f_get_data
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SHOW_REPORT.
  DATA : LV_CONFIRMATION.
  IF GV_STATUS EQ 'C'.
    PERFORM F_GET_REASON CHANGING LV_CONFIRMATION.
  ENDIF.
  CALL SCREEN 0100.
ENDFORM.                    " F_SHOW_REPORT

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DATA_DETAIL .
  DATA : BEGIN OF LS_MAKT,
     MATNR TYPE MAKT-MATNR,
     MAKTX TYPE MAKT-MAKTX,
  END OF LS_MAKT.
  DATA : LT_MAKT LIKE TABLE OF LS_MAKT.

  DATA : LT_VBAP TYPE TABLE OF GY_VBAP,
         LS_VBAP TYPE GY_VBAP.

  DATA : LV_TAX TYPE I.

  DATA : LV_COUNT TYPE I.

  SELECT VBELN
         POSNR
         MATNR
         KWMENG
         VRKME
         NETWR
         WAERK
         MATKL
         PRODH
         ERDAT
         ERNAM
         ERZET
         AUFNR
         MWSBP
         MEINS
         NETPR
         KOWRR
         UEPOS
         UPMAT
    FROM VBAP
    INTO TABLE LT_VBAP
    FOR ALL ENTRIES IN GT_HEADER
    WHERE VBELN EQ GT_HEADER-VBELN.

*--------------------------------------------------------------------*
  IF LT_VBAP[] IS NOT INITIAL.
    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO TABLE LT_MAKT
      FOR ALL ENTRIES IN LT_VBAP
      WHERE MATNR EQ LT_VBAP-MATNR
        AND SPRAS EQ SY-LANGU.
  ENDIF.
*--------------------------------------------------------------------*
  CLEAR COLTROL_QT-LINES.
  LOOP AT LT_VBAP INTO LS_VBAP.
    AT NEW VBELN.
      ADD 1 TO LV_COUNT.
    ENDAT.

    ADD 1 TO COLTROL_QT-LINES.
    MOVE-CORRESPONDING LS_VBAP TO GS_DETAIL.
    ADD LS_VBAP-MWSBP TO GS_HEADER-VTAMT.
    READ TABLE LT_MAKT INTO LS_MAKT
    WITH KEY MATNR = LS_VBAP-MATNR.
    IF SY-SUBRC = 0.
      GS_DETAIL-MAKTX = LS_MAKT-MAKTX.
    ENDIF.

    PERFORM F_GET_POSNR_INIT.

    GS_DETAIL-NETWR_V = LS_VBAP-MWSBP + GS_DETAIL-NETWR.

    IF LS_VBAP-AUFNR IS NOT INITIAL AND GS_HEADER-AUFNR IS INITIAL.
      GS_HEADER-AUFNR = LS_VBAP-AUFNR.
    ENDIF.
    ADD GS_DETAIL-NETWR_V TO GS_HEADER-NETWR_V.
    GV_POSNR = GS_DETAIL-POSNR.

    PERFORM F_GET_SUM_BOM USING GS_DETAIL.

    GS_DETAIL-LAND = GS_HEADER-LAND.

    APPEND GS_DETAIL TO GT_DETAIL.
    CLEAR : LS_VBAP,LS_MAKT.
  ENDLOOP.

  IF LV_COUNT > 1.
    CLEAR GT_SUM_BOM[].
    PERFORM F_CHAGE_POSNR.
  ENDIF.
  PERFORM F_GET_DESC_IO.
  PERFORM F_UPDATE_PRICE_BOM.

  GS_HEADER-TOTAL = GS_HEADER-VTAMT + GS_HEADER-NETWR.
*  IF gs_header-netwr NE 0.
    LV_TAX  = ( GS_HEADER-VTAMT / GS_HEADER-NETWR ) * 100.
    GS_HEADER-VTPCT = 7."lv_tax.
*  ENDIF.

  CLEAR GS_DETAIL.
  DO 200 TIMES.
    ADD 1 TO COLTROL_QT-LINES.
    APPEND GS_DETAIL TO GT_DETAIL.
  ENDDO.
ENDFORM.                    " F_GET_DATA_DETAIL
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DATA_HEADER .
  DATA : LV_NETWR TYPE VBAK-NETWR.
  PERFORM F_GET_PARTNER.
  PERFORM F_GET_TERM.
  PERFORM F_GET_ORI_PO.

  LOOP AT GT_HEADER INTO GS_HEADER.
    ADD GS_HEADER-NETWR TO LV_NETWR.
    PERFORM F_GET_RESULT_PARTNER.
    PERFORM F_GET_RESULT_TERM.
    PERFORM F_GET_DIST.
    PERFORM F_READ_TEXT.
    PERFORM F_GET_RESULT_ORI_PO.
    PERFORM F_GET_SUM_AMOUNT USING LV_NETWR.
  ENDLOOP.

  PERFORM F_GET_DESC_SLAES_G_O.
  PERFORM F_GET_PROJECT.

ENDFORM.                    " F_GET_DATA_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_GET_PARTNER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_PARTNER .
  CONSTANTS : BEGIN OF LC_PARVW,
    SH TYPE C LENGTH 2 VALUE 'WE',
    PE TYPE C LENGTH 2 VALUE 'VE',
  END OF LC_PARVW.

  SELECT VBPA~VBELN
         VBPA~PARVW
         VBPA~KUNNR
         VBPA~PERNR
         KNA1~NAME1
    FROM VBPA
    LEFT JOIN KNA1 ON VBPA~KUNNR = KNA1~KUNNR
    INTO TABLE GT_VBPA
    FOR ALL ENTRIES IN GT_HEADER
    WHERE VBELN = GT_HEADER-VBELN
      AND ( PARVW = LC_PARVW-SH
       OR   PARVW = LC_PARVW-PE ).

  IF GT_VBPA[] IS NOT INITIAL.
    SELECT PERNR
           SNAME
      FROM PA0001
      INTO TABLE GT_PA0001
      FOR ALL ENTRIES IN GT_VBPA
      WHERE PERNR = GT_VBPA-PERNR.
  ENDIF.
ENDFORM.                    " F_GET_PARTNER
*&---------------------------------------------------------------------*
*&      Form  G_GET_RESULT_PARTNER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_RESULT_PARTNER .
  LOOP AT GT_VBPA INTO GS_VBPA WHERE VBELN = GS_HEADER-VBELN.
    CASE GS_VBPA-PARVW.
      WHEN 'WE'.
        GS_HEADER-KUNNR1     = GS_VBPA-KUNNR.
        GS_HEADER-NAME1_SHIP = GS_VBPA-NAME1.
      WHEN 'VE'.
        READ TABLE GT_PA0001 INTO GS_PA0001
        WITH KEY PERNR = GS_VBPA-PERNR.
        IF SY-SUBRC = 0.
          GS_HEADER-PERNR      = GS_VBPA-PERNR.
          GS_HEADER-SNAME      = GS_PA0001-SNAME.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " G_GET_RESULT_PARTNER
*&---------------------------------------------------------------------*
*&      Form  F_GET_TERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_TERM .
  CONSTANTS LC_BUKRS TYPE C LENGTH 4 VALUE '1000'.

  SELECT KUNNR
         ZTERM
    FROM KNB1
    INTO TABLE GT_KNB1
    FOR ALL ENTRIES IN GT_HEADER
    WHERE KUNNR = GT_HEADER-KUNNR
      AND BUKRS = LC_BUKRS.

ENDFORM.                    " F_GET_TERM
*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT_TERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_RESULT_TERM .
  READ TABLE GT_KNB1 INTO GS_KNB1
  WITH KEY KUNNR = GS_HEADER-KUNNR.
  IF SY-SUBRC = 0.
    GS_HEADER-ZTERM = GS_KNB1-ZTERM.
  ENDIF.
ENDFORM.                    " F_GET_RESULT_TERM
*&---------------------------------------------------------------------*
*&      Form  F_GET_DESC_SLAES_G_O
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DESC_SLAES_G_O .

  SELECT SINGLE BEZEI
    FROM TVKBT
    INTO GS_HEADER-SALEO
    WHERE VKBUR = GS_HEADER-VKBUR
      AND SPRAS = SY-LANGU.
*--------------------------------------------------------------------*
  SELECT SINGLE BEZEI
    FROM TVGRT
    INTO GS_HEADER-SALEG
    WHERE VKGRP = GS_HEADER-VKGRP
      AND SPRAS = SY-LANGU.
ENDFORM.                    " F_GET_DESC_SLAES_G_O
*&---------------------------------------------------------------------*
*&      Form  F_GET_PROJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_PROJECT .
  DATA : LV_ID       TYPE THEAD-TDID,
         LV_LANGUAGE TYPE THEAD-TDSPRAS,
         LV_NAME     TYPE THEAD-TDNAME,
         LV_OBJECT   TYPE THEAD-TDOBJECT,
         LV_TEXT     TYPE STRING.

  DATA : LT_LINES TYPE TABLE OF TLINE,
         LS_LINES TYPE TLINE.

  DATA : BEGIN OF LS_AUFK,
   AUFNR TYPE AUFK-AUFNR,
   KTEXT TYPE AUFK-KTEXT,
  END OF LS_AUFK.
  DATA LT_AUFK LIKE TABLE OF LS_AUFK.

  CONSTANTS : BEGIN OF LC_READ_TEXT,
    ID TYPE C LENGTH 4 VALUE 'Z002',
    OB TYPE C LENGTH 4 VALUE 'VBBK',
  END OF LC_READ_TEXT.
*--------------------------------------------------------------------*
  LV_ID       = LC_READ_TEXT-ID.
  LV_LANGUAGE = SY-LANGU.
  LV_NAME     = GS_HEADER-VBELN.
  LV_OBJECT   = LC_READ_TEXT-OB.
*--------------------------------------------------------------------*
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = LV_ID
      LANGUAGE                = LV_LANGUAGE
      NAME                    = LV_NAME
      OBJECT                  = LV_OBJECT
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR LV_TEXT.
  IF LT_LINES[] IS NOT INITIAL.
    LOOP AT LT_LINES INTO LS_LINES.
      IF LV_TEXT IS NOT INITIAL.
        CONCATENATE LV_TEXT LS_LINES-TDLINE INTO LV_TEXT.
      ELSE.
        LV_TEXT = LS_LINES-TDLINE.
      ENDIF.
    ENDLOOP.
    GS_HEADER-PROJTXT = LV_TEXT.
  ELSE.

*    READ TABLE lt_aufk INTO ls_aufk
*    WITH KEY aufnr = gs_header-vbeln.
*    IF sy-subrc = 0.
*       = ls_aufk-ktext.
*    ENDIF.
  ENDIF.
ENDFORM.                    " F_GET_PROJECT

*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_HEADER_AUFNR  text
*      -->P_ENDLOOP  text
*----------------------------------------------------------------------*
FORM F_CONVERSION_EXIT_ALPHA_INPUT USING LV_INPUT
                                CHANGING LV_OUTPUT.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LV_INPUT
    IMPORTING
      OUTPUT = LV_OUTPUT.


ENDFORM.                    " F_CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  F_GET_DIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DIST .
  SELECT SINGLE VTEXT
    FROM TVTWT
    INTO GS_HEADER-VTEXT
    WHERE VTWEG EQ GS_HEADER-VTWEG
      AND SPRAS EQ SY-LANGU.
ENDFORM.                    " F_GET_DIST
*&---------------------------------------------------------------------*
*&      Form  F_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_READ_TEXT.
  DATA : LS_ID       TYPE THEAD-TDID,
         LS_LANGUAGE TYPE THEAD-TDSPRAS,
         LS_NAME     TYPE THEAD-TDNAME,
         LS_OBJECT   TYPE THEAD-TDOBJECT.

  DATA : LT_LINES	TYPE TABLE OF	TLINE,
         LS_LINES TYPE TLINE.

  CONSTANTS : BEGIN OF LC_PROJECT,
    ID     TYPE C LENGTH 4 VALUE 'Z002',
    OBJECT TYPE C LENGTH 4 VALUE 'VBBK',
  END OF LC_PROJECT.
  CONSTANTS : BEGIN OF LC_REMARK,
    ID     TYPE C LENGTH 4 VALUE 'Z007',
    OBJECT TYPE C LENGTH 4 VALUE 'VBBK',
  END OF LC_REMARK.
  CONSTANTS : BEGIN OF LC_LAN_NO,
    ID     TYPE C LENGTH 4 VALUE 'Z040',
    OBJECT TYPE C LENGTH 4 VALUE 'VBBK',
  END OF LC_LAN_NO.

*--------------------------------------------------------------------*
* Project
*--------------------------------------------------------------------*
  LS_ID       = LC_PROJECT-ID.
  LS_LANGUAGE = SY-LANGU.
  LS_NAME     = GS_HEADER-VBELN.
  LS_OBJECT   = LC_PROJECT-OBJECT.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = LS_ID
      LANGUAGE                = LS_LANGUAGE
      NAME                    = LS_NAME
      OBJECT                  = LS_OBJECT
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_LINES INTO LS_LINES.
    IF GS_HEADER-PROJTXT IS INITIAL.
      GS_HEADER-PROJTXT = LS_LINES-TDLINE.
    ELSE.
      CONCATENATE GS_HEADER-PROJTXT LS_LINES-TDLINE INTO GS_HEADER-PROJTXT SEPARATED BY SPACE.
    ENDIF.
  ENDLOOP.

*--------------------------------------------------------------------*
* Remark
*--------------------------------------------------------------------*
  CLEAR : LS_ID,LS_LANGUAGE,LS_NAME,LS_OBJECT,LT_LINES[].
  LS_ID       = LC_REMARK-ID.
  LS_LANGUAGE = SY-LANGU.
  LS_NAME     = GS_HEADER-VBELN.
  LS_OBJECT   = LC_REMARK-OBJECT.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = LS_ID
      LANGUAGE                = LS_LANGUAGE
      NAME                    = LS_NAME
      OBJECT                  = LS_OBJECT
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_LINES INTO LS_LINES.
    IF GS_HEADER-PROJTXT IS INITIAL.
      GS_HEADER-HDRRMK = LS_LINES-TDLINE.
    ELSE.
      CONCATENATE GS_HEADER-HDRRMK LS_LINES-TDLINE INTO GS_HEADER-HDRRMK SEPARATED BY SPACE.
    ENDIF.
  ENDLOOP.
*--------------------------------------------------------------------*
* Lan No.
*--------------------------------------------------------------------*
  CLEAR : LS_ID,LS_LANGUAGE,LS_NAME,LS_OBJECT,LT_LINES[].
  LS_ID       = LC_LAN_NO-ID.
  LS_LANGUAGE = SY-LANGU.
  LS_NAME     = GS_HEADER-VBELN.
  LS_OBJECT   = LC_LAN_NO-OBJECT.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = SY-MANDT
      ID                      = LS_ID
      LANGUAGE                = LS_LANGUAGE
      NAME                    = LS_NAME
      OBJECT                  = LS_OBJECT
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_LINES INTO LS_LINES.
    IF GS_HEADER-LAND IS INITIAL.
      GS_HEADER-LAND = LS_LINES-TDLINE.
    ELSE.
      CONCATENATE GS_HEADER-LAND LS_LINES-TDLINE INTO GS_HEADER-LAND SEPARATED BY SPACE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  F_GET_DESC_IO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DESC_IO .
  CLEAR GS_DETAIL.
  SELECT SINGLE KTEXT
    FROM AUFK
    INTO GS_DETAIL-KTEXT
    WHERE AUFNR EQ GS_HEADER-AUFNR.

  IF GS_HEADER-PROJTXT IS INITIAL.
    GS_HEADER-PROJTXT = GS_DETAIL-KTEXT.
  ENDIF.
  GV_KTEXT = GS_DETAIL-KTEXT.

  MODIFY GT_DETAIL FROM GS_DETAIL TRANSPORTING KTEXT
                                  WHERE AUFNR EQ GS_HEADER-AUFNR.

ENDFORM.                    " F_GET_DESC_IO

*&---------------------------------------------------------------------*
*&      Form  F_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAVE_DATA.
  PERFORM F_SAVE_HEADER.
  PERFORM F_SAVE_DETAIL.
  PERFORM F_SAVE_HEADER_LOG.
  PERFORM F_SAVE_DETAIL_LOG.
ENDFORM.                    " F_SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAVE_HEADER .
  DATA : LS_ZTSD_PO_H   TYPE ZSDSMMT003,
         LS_ZTSD_PO_H_1 TYPE ZSDSMMT003.

  MOVE-CORRESPONDING GS_HEADER TO LS_ZTSD_PO_H.
  LS_ZTSD_PO_H-BSTKD   = GS_HEADER-BSTNK.
  LS_ZTSD_PO_H-PARNR   = GS_HEADER-PERNR.
  LS_ZTSD_PO_H-DOCTYP  = GV_DOCTYPE.
  LS_ZTSD_PO_H-PROJNAM = GS_HEADER-PROJTXT.
  LS_ZTSD_PO_H-KBETR   = GS_HEADER-VTPCT.
  LS_ZTSD_PO_H-MWSBP   = GS_HEADER-VTAMT.
  LS_ZTSD_PO_H-MARK    = GV_REASON.

  IF GV_STATUS IS INITIAL.
    LS_ZTSD_PO_H-ERDAT = SY-DATUM.
    LS_ZTSD_PO_H-ERTIM = SY-UZEIT.
    LS_ZTSD_PO_H-ERNAM = SY-UNAME.
    INSERT ZSDSMMT003 FROM LS_ZTSD_PO_H.
  ELSEIF GV_STATUS = 'C'.

    SELECT SINGLE ERDAT
                  ERTIM
                  ERNAM
      FROM ZSDSMMT003
      INTO CORRESPONDING FIELDS OF LS_ZTSD_PO_H_1
      WHERE KUNNR = GS_HEADER-KUNNR
        AND BSTKD = GV_OLD_BSTNK
        AND VBELN = GS_HEADER-VBELN
        AND IHREZ = GS_HEADER-IHREZ.
    IF SY-SUBRC = 0.
      LS_ZTSD_PO_H-ERDAT = LS_ZTSD_PO_H_1-ERDAT.
      LS_ZTSD_PO_H-ERTIM = LS_ZTSD_PO_H_1-ERTIM.
      LS_ZTSD_PO_H-ERNAM = LS_ZTSD_PO_H_1-ERNAM.
      LS_ZTSD_PO_H-AEDAT = SY-DATUM.
      LS_ZTSD_PO_H-AETIM = SY-UZEIT.
      LS_ZTSD_PO_H-AENAM = SY-UNAME.
      DELETE FROM ZSDSMMT003 WHERE KUNNR = GS_HEADER-KUNNR
                               AND BSTKD = GV_OLD_BSTNK.
      INSERT ZSDSMMT003 FROM LS_ZTSD_PO_H.
    ELSE.
*      INSERT ztsd_po_h FROM ls_ztsd_po_h.
    ENDIF.

  ENDIF.
ENDFORM.                    " F_SAVE_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAVE_DETAIL.
  DATA LS_ZTSD_PO_D TYPE ZSDSMMT004.

  IF GV_STATUS = 'C'.
    DELETE FROM ZSDSMMT004 WHERE KUNNR = GS_HEADER-KUNNR
                             AND BSTKD = GV_OLD_BSTNK.
    COMMIT WORK.
  ENDIF.

  LOOP AT GT_DETAIL INTO GS_DETAIL WHERE VBELN IS NOT INITIAL.
    MOVE-CORRESPONDING GS_DETAIL TO LS_ZTSD_PO_D.
    LS_ZTSD_PO_D-KUNNR   = GS_HEADER-KUNNR.
    LS_ZTSD_PO_D-BSTKD   = GS_HEADER-BSTNK.
    LS_ZTSD_PO_D-IHREZ   = GS_HEADER-IHREZ.
    LS_ZTSD_PO_D-ERDAT   = SY-DATUM.
    LS_ZTSD_PO_D-ERTIM   = SY-UZEIT.
    LS_ZTSD_PO_D-ERNAM   = SY-UNAME.
    GV_POSNR             = GS_DETAIL-POSNR.
    IF GV_STATUS IS INITIAL.
      INSERT ZSDSMMT004 FROM LS_ZTSD_PO_D.
    ELSEIF GV_STATUS = 'C'.
      READ TABLE GT_ZTSD_PO_D INTO GS_ZTSD_PO_D
      WITH KEY KUNNR = LS_ZTSD_PO_D-KUNNR
               BSTKD = GV_OLD_BSTNK
               VBELN = LS_ZTSD_PO_D-VBELN
               IHREZ = LS_ZTSD_PO_D-IHREZ
               POSNR = LS_ZTSD_PO_D-POSNR.
      IF SY-SUBRC = 0.
        LS_ZTSD_PO_D-ERDAT = GS_ZTSD_PO_D-ERDAT.
        LS_ZTSD_PO_D-ERTIM = GS_ZTSD_PO_D-ERTIM.
        LS_ZTSD_PO_D-ERNAM = GS_ZTSD_PO_D-ERNAM.
        LS_ZTSD_PO_D-AEDAT = SY-DATUM.
        LS_ZTSD_PO_D-AETIM = SY-UZEIT.
        LS_ZTSD_PO_D-AENAM = SY-UNAME.
        INSERT ZSDSMMT004 FROM LS_ZTSD_PO_D.
      ELSE.
        LS_ZTSD_PO_D-AEDAT = SY-DATUM.
        LS_ZTSD_PO_D-AETIM = SY-UZEIT.
        LS_ZTSD_PO_D-AENAM = SY-UNAME.
        INSERT ZSDSMMT004 FROM LS_ZTSD_PO_D.
      ENDIF.
    ENDIF.
    CLEAR GS_ZTSD_PO_D.
  ENDLOOP.

  IF GV_STATUS EQ 'C'.
    LOOP AT GT_ZTSD_PO_D INTO GS_ZTSD_PO_D.
      READ TABLE GT_DETAIL INTO GS_DETAIL
      WITH KEY VBELN = GS_ZTSD_PO_D-VBELN
               POSNR = GS_ZTSD_PO_D-POSNR.
      IF SY-SUBRC NE 0.
        DELETE ZSDSMMT004 FROM GS_ZTSD_PO_D.
      ENDIF.
      CLEAR GS_ZTSD_PO_D.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_SAVE_DETAIL
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_HEADER_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAVE_HEADER_LOG .
*  DATA : LS_ZTSD_PO_H_LOG TYPE ZTSD_PO_H_LOG.
*  DATA : LV_CHGNR TYPE ZTSD_PO_D_LOG-CHGNR.
*
*  CONSTANTS : LC_CUSTOMER TYPE DD07L-DOMNAME VALUE 'Z_CUSTOMER_REASON',
*              LC_USER     TYPE DD07L-DOMNAME VALUE 'Z_USER_REASON'.
*
*  SELECT MAX( CHGNR )
*        FROM ZTSD_PO_D_LOG
*        INTO LV_CHGNR
*        WHERE KUNNR = GS_HEADER-KUNNR AND
*              BSTKD = GS_HEADER-BSTNK AND
*              VBELN = GS_HEADER-VBELN AND
*              IHREZ = GS_HEADER-IHREZ.
*
*  MOVE-CORRESPONDING GS_HEADER TO LS_ZTSD_PO_H_LOG.
*  LS_ZTSD_PO_H_LOG-BSTKD   = GS_HEADER-BSTNK.
*  LS_ZTSD_PO_H_LOG-PARNR   = GS_HEADER-PERNR.
*  LS_ZTSD_PO_H_LOG-DOCTYP  = GV_DOCTYPE.
*  LS_ZTSD_PO_H_LOG-PROJNAM = GS_HEADER-PROJTXT.
*  LS_ZTSD_PO_H_LOG-KBETR   = GS_HEADER-VTPCT.
*  LS_ZTSD_PO_H_LOG-MWSBP   = GS_HEADER-VTAMT.
*  LS_ZTSD_PO_H_LOG-ERDAT   = SY-DATUM.
*  LS_ZTSD_PO_H_LOG-ERTIM   = SY-UZEIT.
*  LS_ZTSD_PO_H_LOG-ERNAM   = SY-UNAME.
*  LS_ZTSD_PO_H_LOG-BSTKD_O = GV_OLD_BSTNK.
*  LS_ZTSD_PO_H_LOG-MARK    = GV_REASON.
*  IF GV_ZCUSTOMER_REASON IS NOT INITIAL.
*    PERFORM F_GET_DETAIL USING LC_CUSTOMER
*                      CHANGING GV_ZCUSTOMER_REASON.
*    LS_ZTSD_PO_H_LOG-REASON = GV_ZCUSTOMER_REASON.
*  ELSE.
*    PERFORM F_GET_DETAIL USING LC_USER
*                      CHANGING GV_ZUSER_REASON.
*    LS_ZTSD_PO_H_LOG-REASON = GV_ZUSER_REASON.
*  ENDIF.
*
*  IF GV_STATUS IS INITIAL.
*    LS_ZTSD_PO_H_LOG-ACTION  = 'CRE'.
*    LS_ZTSD_PO_H_LOG-CHGNR   = '1'.
*    INSERT ZTSD_PO_H_LOG FROM LS_ZTSD_PO_H_LOG.
*  ELSEIF GV_STATUS = 'C'.
*    LS_ZTSD_PO_H_LOG-ACTION  = 'CHG'.
*    LS_ZTSD_PO_H_LOG-CHGNR   = LV_CHGNR + 1.
*    INSERT ZTSD_PO_H_LOG FROM LS_ZTSD_PO_H_LOG.
*  ENDIF.
ENDFORM.                    " F_SAVE_HEADER_LOG
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_DETAIL_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAVE_DETAIL_LOG .
*  DATA : LS_ZTSD_PO_D_LOG TYPE ZTSD_PO_D_LOG.
*  DATA : LV_CHGNR TYPE ZTSD_PO_D_LOG-CHGNR.
*
*  SELECT MAX( CHGNR )
*        FROM ZTSD_PO_D_LOG
*        INTO LV_CHGNR
*        WHERE KUNNR = GS_HEADER-KUNNR AND
*              BSTKD = GS_HEADER-BSTNK AND
*              VBELN = GS_HEADER-VBELN AND
*              IHREZ = GS_HEADER-IHREZ.
*
*  LOOP AT GT_DETAIL INTO GS_DETAIL WHERE VBELN IS NOT INITIAL.
*    MOVE-CORRESPONDING GS_DETAIL TO LS_ZTSD_PO_D_LOG.
*    LS_ZTSD_PO_D_LOG-KUNNR   = GS_HEADER-KUNNR.
*    LS_ZTSD_PO_D_LOG-BSTKD   = GS_HEADER-BSTNK.
*    LS_ZTSD_PO_D_LOG-IHREZ   = GS_HEADER-IHREZ.
*    LS_ZTSD_PO_D_LOG-ERDAT   = SY-DATUM.
*    LS_ZTSD_PO_D_LOG-ERTIM   = SY-UZEIT.
*    LS_ZTSD_PO_D_LOG-ERNAM   = SY-UNAME.
*
*    IF GV_STATUS IS INITIAL.
*      LS_ZTSD_PO_D_LOG-ACTION  = 'CRE'.
*      LS_ZTSD_PO_D_LOG-CHGNR   = '1'.
*      INSERT ZTSD_PO_D_LOG FROM LS_ZTSD_PO_D_LOG.
*    ELSEIF GV_STATUS = 'C'.
*      LS_ZTSD_PO_D_LOG-CHGNR = LV_CHGNR + 1.
*      READ TABLE GT_ZTSD_PO_D INTO GS_ZTSD_PO_D
*      WITH KEY KUNNR = LS_ZTSD_PO_D_LOG-KUNNR
*               BSTKD = LS_ZTSD_PO_D_LOG-BSTKD
*               VBELN = LS_ZTSD_PO_D_LOG-VBELN
*               IHREZ = LS_ZTSD_PO_D_LOG-IHREZ
*               POSNR = LS_ZTSD_PO_D_LOG-POSNR.
*      IF SY-SUBRC = 0.
*        LS_ZTSD_PO_D_LOG-ACTION  = 'CHG'.
*        INSERT ZTSD_PO_D_LOG FROM LS_ZTSD_PO_D_LOG.
*      ELSE.
*        LS_ZTSD_PO_D_LOG-ACTION  = 'CRE'.
*        INSERT ZTSD_PO_D_LOG FROM LS_ZTSD_PO_D_LOG.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  CLEAR : LS_ZTSD_PO_D_LOG,GS_ZTSD_PO_D.
*  IF GV_STATUS EQ 'C'.
*    LOOP AT GT_ZTSD_PO_D INTO GS_ZTSD_PO_D.
*      READ TABLE GT_DETAIL INTO GS_DETAIL
*      WITH KEY VBELN = GS_ZTSD_PO_D-VBELN
*               POSNR = GS_ZTSD_PO_D-POSNR.
*      IF SY-SUBRC NE 0.
*        MOVE-CORRESPONDING GS_ZTSD_PO_D TO LS_ZTSD_PO_D_LOG.
*        LS_ZTSD_PO_D_LOG-CHGNR   = LV_CHGNR + 1.
*        LS_ZTSD_PO_D_LOG-ACTION  = 'DEL'.
*        INSERT ZTSD_PO_D_LOG FROM LS_ZTSD_PO_D_LOG.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    " F_SAVE_DETAIL_LOG
*&---------------------------------------------------------------------*
*&      Form  F_GET_DOC_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DOC_TYPE .
  IF     S_VBELN[]  IS NOT INITIAL.
    GV_DOCTYPE = 'SAPQT'.
    IF S_VBELN-LOW+0(1) NE '2'.
      MESSAGE E000 WITH 'Please check your Quotation'.
    ENDIF.
  ELSEIF S_VBELNS[] IS NOT INITIAL.
    GV_DOCTYPE = 'SAPSO'.
    S_VBELN[] = S_VBELNS[].
    S_VBELN   = S_VBELNS.
    IF S_VBELNS-LOW+0(1) NE '3'.
      CLEAR : S_VBELN[],S_VBELN.
      MESSAGE E000 WITH 'Please check your Sales Order'.
    ENDIF.
  ELSEIF S_IHREZ[]  IS NOT INITIAL.
    GV_DOCTYPE = 'SFQT'.
  ELSE.
    GV_DOCTYPE = 'SAPQT'.
  ENDIF.
ENDFORM.                    " F_GET_DOC_TYPE
*&---------------------------------------------------------------------*
*&      Form  F_GET_PRICE_DEALER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_PRICE_DEALER USING FT_MATNR TYPE VBAP-MATNR.
*  DATA : BEGIN OF LS_A550,
*    KNUMH TYPE A304-KNUMH,
*  END OF LS_A550.
*  DATA LT_A550 LIKE TABLE OF LS_A550.
*
*  SELECT SINGLE KNUMH
*    FROM A550
*    INTO LS_A550
*    WHERE KAPPL EQ 'V'             AND
*          KSCHL EQ 'PR00'          AND
*          VKORG EQ '1000'          AND
*          VTWEG EQ GS_HEADER-VTWEG AND
*          VKBUR EQ GS_HEADER-VKBUR AND
*          MATNR EQ FT_MATNR        AND
*          DATBI GE SY-DATUM        AND
*          DATAB LE SY-DATUM.
*
*  SELECT SINGLE KBETR
*                KMEIN
*    FROM KONP
*    INTO (GS_DETAIL-NETPR,GS_DETAIL-MEINS)
*    WHERE KNUMH EQ LS_A550-KNUMH.
*  IF SY-SUBRC NE 0.
*    CLEAR : GS_DETAIL-NETPR,GS_DETAIL-MEINS.
*  ENDIF.

  GS_DETAIL-NETWR   = GS_DETAIL-NETPR * GS_DETAIL-KWMENG.
  GS_DETAIL-MWSBP   = ( ( GS_DETAIL-NETWR * GS_HEADER-VTPCT ) / 100 ).
  GS_DETAIL-NETWR_V = GS_DETAIL-NETWR + ( ( GS_DETAIL-NETWR * GS_HEADER-VTPCT ) / 100 ).
  GS_DETAIL-AUFNR   = GS_HEADER-AUFNR.
  GS_DETAIL-KTEXT   = GV_KTEXT.

ENDFORM.                    " F_GET_PRICE_DEALER
*&---------------------------------------------------------------------*
*&      Form  F_GET_PRICE_GEN_SER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_PRICE_GEN_SER USING FT_MATNR TYPE VBAP-MATNR.

  DATA : BEGIN OF LS_A304,
    KNUMH TYPE A304-KNUMH,
  END OF LS_A304.
  DATA LT_A304 LIKE TABLE OF LS_A304.

  SELECT SINGLE KNUMH
    FROM A304
    INTO LS_A304
    WHERE KAPPL EQ 'V'             AND
          KSCHL EQ 'PR00'          AND
          VKORG EQ '1000'          AND
          VTWEG EQ GS_HEADER-VTWEG AND
          MATNR EQ FT_MATNR        AND
          DATBI GE SY-DATUM        AND
          DATAB LE SY-DATUM.

  SELECT SINGLE KBETR
                KMEIN
    FROM KONP
    INTO (GS_DETAIL-NETPR,GS_DETAIL-MEINS)
    WHERE KNUMH EQ LS_A304-KNUMH.
  IF SY-SUBRC NE 0.
    CLEAR : GS_DETAIL-NETPR,GS_DETAIL-MEINS.
  ENDIF.

  GS_DETAIL-NETWR   = GS_DETAIL-NETPR * GS_DETAIL-KWMENG.
  GS_DETAIL-MWSBP   = ( ( GS_DETAIL-NETWR * GS_HEADER-VTPCT ) / 100 ).
  GS_DETAIL-NETWR_V = GS_DETAIL-NETWR + ( ( GS_DETAIL-NETWR * GS_HEADER-VTPCT ) / 100 ).
  GS_DETAIL-AUFNR   = GS_HEADER-AUFNR.
  GS_DETAIL-KTEXT   = GV_KTEXT.
ENDFORM.                    " F_GET_PRICE_GEN_SER
*&---------------------------------------------------------------------*
*&      Form  F_GET_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_MATERIAL  text
*      -->P_LT_BOM_ITEM  text
*----------------------------------------------------------------------*
FORM F_GET_BOM  USING FV_MATERIAL
                      FT_BOM_ITEM.
  DATA : LT_BOM_ITEM  TYPE TABLE OF STPO_API02,
         LS_BOM_ITEM  TYPE STPO_API02.

  DATA : LS_DETAIL    LIKE GS_DETAIL.

  DATA : LV_MATERIAL  TYPE RC29L-MATNR,
         LV_POSNR     TYPE I,
         LV_LINE      TYPE I,
         LV_KWMENG    TYPE VBAP-KWMENG,
         LV_POSNR_UP  TYPE VBAP-POSNR.

  LT_BOM_ITEM[]   = FT_BOM_ITEM.
  LV_MATERIAL     = FV_MATERIAL.
  GS_DETAIL-MATNR = FV_MATERIAL.

  CLEAR : LV_POSNR,LV_LINE,GS_DETAIL-NETPR,GS_DETAIL-MEINS.

  GS_DETAIL-KOWRR = GC_TRUE.
  LV_LINE = COLTROL_QT-CURRENT_LINE.
  PERFORM F_CHECK_LINE CHANGING LV_LINE.
*--------------------------------------------------------------------*
* Bom
*--------------------------------------------------------------------*
  IF     GS_HEADER-VTWEG = '10'.
    PERFORM F_GET_PRICE_DEALER USING LV_MATERIAL.
    PERFORM F_GET_POSNR USING LV_LINE.
    PERFORM F_GET_MAT_DEST USING LV_MATERIAL.
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX LV_LINE
                                    TRANSPORTING MATNR NETPR MEINS MWSBP
                                                 NETWR_V AUFNR KTEXT POSNR KOWRR KWMENG.
  ELSEIF GS_HEADER-VTWEG = '20' OR GS_HEADER-VTWEG = '40'.
    PERFORM F_GET_PRICE_GEN_SER USING LV_MATERIAL.
    PERFORM F_GET_POSNR USING LV_LINE.
    PERFORM F_GET_MAT_DEST USING LV_MATERIAL.
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX LV_LINE
                                    TRANSPORTING MATNR NETPR MEINS MWSBP
                                                 NETWR_V AUFNR KTEXT POSNR KOWRR KWMENG.
  ENDIF.

  LV_POSNR_UP = GS_DETAIL-POSNR.
  GET CURSOR LINE GV_LINE.
  CLEAR GV_FLAG.
  LV_KWMENG = GS_DETAIL-KWMENG.
  CLEAR : GS_DETAIL,LV_LINE.
*--------------------------------------------------------------------*
* Loop Companance
*--------------------------------------------------------------------*
  LOOP AT LT_BOM_ITEM INTO LS_BOM_ITEM.
    ADD 1 TO LV_POSNR.
    LV_LINE = COLTROL_QT-CURRENT_LINE + LV_POSNR.
    IF     GS_HEADER-VTWEG = '10'.
      PERFORM F_GET_PRICE_DEALER USING LS_BOM_ITEM-COMPONENT.
      PERFORM F_CHECK_LINE CHANGING LV_LINE.
      PERFORM F_GET_POSNR USING LV_LINE.
      MOVE-CORRESPONDING GS_DETAIL TO LS_DETAIL.
      LS_DETAIL-MATNR  = LS_BOM_ITEM-COMPONENT.
      GV_POSNR         = GV_POSNR - 10.
      LS_DETAIL-UPMAT  = LV_MATERIAL.
      LS_DETAIL-UEPOS  = LS_DETAIL-POSNR - 10.
      LS_DETAIL-POSNR  = ( LS_DETAIL-POSNR - 10 )  + LV_POSNR.
      GS_DETAIL-POSNR  = GS_DETAIL-POSNR - 10.
      LS_DETAIL-KWMENG = LV_KWMENG.
      LS_DETAIL-NETWR  = LS_DETAIL-KWMENG * LS_DETAIL-NETPR.
      GS_HEADER-NETWR  = GS_HEADER-NETWR + LS_DETAIL-NETWR.
      GS_HEADER-VTAMT  = ( ( GS_HEADER-NETWR * GS_HEADER-VTPCT ) / 100 ).
      GS_HEADER-TOTAL  = GS_HEADER-NETWR + GS_HEADER-VTAMT.
      PERFORM F_GET_MAT_DEST USING LS_DETAIL-MATNR.
      MODIFY GT_DETAIL FROM LS_DETAIL INDEX LV_LINE
                                      TRANSPORTING MATNR NETPR MEINS MWSBP NETWR_V
                                                   AUFNR KTEXT POSNR UEPOS UPMAT KWMENG NETWR.
*--------------------------------------------------------------------*
    ELSEIF GS_HEADER-VTWEG = '20' OR GS_HEADER-VTWEG = '40'.
      PERFORM F_GET_PRICE_GEN_SER USING LS_BOM_ITEM-COMPONENT.
      PERFORM F_CHECK_LINE CHANGING LV_LINE.
      PERFORM F_GET_POSNR USING LV_LINE.
      MOVE-CORRESPONDING GS_DETAIL TO LS_DETAIL.
      LS_DETAIL-MATNR  = LS_BOM_ITEM-COMPONENT.
      GV_POSNR         = GV_POSNR - 10.
      LS_DETAIL-UPMAT  = LV_MATERIAL.
      LS_DETAIL-UEPOS  = LS_DETAIL-POSNR - 10.
      LS_DETAIL-POSNR  = ( LS_DETAIL-POSNR - 10 )  + LV_POSNR.
      GS_DETAIL-POSNR  = GS_DETAIL-POSNR - 10.
      LS_DETAIL-KWMENG = LV_KWMENG.
      LS_DETAIL-NETWR  = LS_DETAIL-KWMENG * LS_DETAIL-NETPR.
      GS_HEADER-NETWR  = GS_HEADER-NETWR + LS_DETAIL-NETWR.
      GS_HEADER-VTAMT  = ( ( GS_HEADER-NETWR * GS_HEADER-VTPCT ) / 100 ).
      GS_HEADER-TOTAL  = GS_HEADER-NETWR + GS_HEADER-VTAMT.
      PERFORM F_GET_MAT_DEST USING LS_DETAIL-MATNR.
      MODIFY GT_DETAIL FROM LS_DETAIL INDEX LV_LINE
                                      TRANSPORTING MATNR NETPR MEINS MWSBP NETWR_V
                                                   AUFNR KTEXT POSNR UEPOS UPMAT KWMENG NETWR.
    ENDIF.
    PERFORM F_GET_SUM_BOM USING LS_DETAIL.
  ENDLOOP.

  PERFORM F_UPDATE_PRICE_BOM.

ENDFORM.                    " F_GET_BOM
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_LINE  text
*----------------------------------------------------------------------*
FORM F_CHECK_LINE  CHANGING FV_LINE.
  DATA LS_DETAIL LIKE GS_DETAIL.
  DO.
    READ TABLE GT_DETAIL INTO LS_DETAIL INDEX FV_LINE.
    IF LS_DETAIL-MATNR IS INITIAL.
      EXIT.
    ELSEIF SY-SUBRC EQ '4'.
      EXIT.
    ELSE.
      FV_LINE = FV_LINE + 1.
    ENDIF.
  ENDDO.
ENDFORM.                    " F_CHECK_LINE
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECT_ALL.
  DATA LS_DETAIL LIKE GS_DETAIL.
  LOOP AT GT_DETAIL INTO LS_DETAIL WHERE MATNR IS NOT INITIAL.
    LS_DETAIL-SEL = GC_TRUE.
    MODIFY GT_DETAIL FROM LS_DETAIL.
  ENDLOOP.
ENDFORM.                    " F_SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  F_NON_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_NON_SELECT .
  DATA LS_DETAIL LIKE GS_DETAIL.
  LOOP AT GT_DETAIL INTO LS_DETAIL WHERE MATNR IS NOT INITIAL.
    CLEAR LS_DETAIL-SEL.
    MODIFY GT_DETAIL FROM LS_DETAIL.
  ENDLOOP.
ENDFORM.                    " F_NON_SELECT

*&---------------------------------------------------------------------*
*&      Form  F_DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DELETE_DATA.
  DATA LS_DETAIL LIKE GS_DETAIL.
  RANGES LR_POSNR FOR VBAP-POSNR.
  DATA LV_LINE TYPE I.
  DATA LV_AMOUNT TYPE NETWR.

  CLEAR : LV_LINE,LV_AMOUNT.
  LOOP AT GT_DETAIL INTO LS_DETAIL WHERE SEL EQ GC_TRUE.
    ADD 1 TO LV_LINE.
    CLEAR LR_POSNR.
    LR_POSNR-SIGN   = 'I'.
    LR_POSNR-OPTION = 'EQ'.
    LR_POSNR-LOW    = LS_DETAIL-POSNR.
    IF LS_DETAIL-POSNR IS NOT INITIAL.
      APPEND LR_POSNR.
    ENDIF.
  ENDLOOP.

  IF LR_POSNR[] IS NOT INITIAL.
    LOOP AT GT_DETAIL INTO LS_DETAIL WHERE UEPOS IN LR_POSNR.
      ADD 1 TO LV_LINE.
      ADD LS_DETAIL-NETWR TO LV_AMOUNT.
      LS_DETAIL-SEL = GC_TRUE.
      MODIFY GT_DETAIL FROM LS_DETAIL.
    ENDLOOP.
  ENDIF.

  GS_HEADER-NETWR = GS_HEADER-NETWR - LV_AMOUNT.
  GS_HEADER-VTAMT = ( ( GS_HEADER-NETWR * GS_HEADER-VTPCT ) / 100 ).
  GS_HEADER-TOTAL = GS_HEADER-NETWR + GS_HEADER-VTAMT.

  DELETE GT_DETAIL WHERE SEL EQ GC_TRUE.

  CLEAR LS_DETAIL.
  DO LV_LINE TIMES.
    APPEND LS_DETAIL TO GT_DETAIL.
  ENDDO.
ENDFORM.                    " F_DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INSERT_DATA .
  CLEAR GS_DETAIL.
  ADD 30 TO COLTROL_QT-LINES.
  ADD 1  TO GV_INSET.
  DO 30 TIMES.
    APPEND GS_DETAIL TO GT_DETAIL.
  ENDDO.
ENDFORM.                    " F_INSERT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_MAT_DEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DETAIL_MATNR  text
*----------------------------------------------------------------------*
FORM F_GET_MAT_DEST USING FV_MATNR.
  CLEAR GR_MATNR.
  GR_MATNR-SIGN   = 'I'.
  GR_MATNR-OPTION = 'EQ'.
  GR_MATNR-LOW    = FV_MATNR.
  APPEND GR_MATNR.
ENDFORM.                    " F_GET_MAT_DEST

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_DATA .
  DATA LS_VBELN TYPE VBAP-VBELN.

  SELECT SINGLE VBELN
    FROM ZSDSMMT003
    INTO LS_VBELN
    WHERE VBELN IN S_VBELN
      AND IHREZ IN S_IHREZ
      AND DFLAG NE GC_TRUE.
  IF SY-SUBRC = 0.
    GV_ERROR = GC_ERROR.
  ENDIF.

ENDFORM.                    " F_CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_VBELN  text
*      -->P_S_IHREZ  text
*----------------------------------------------------------------------*
FORM F_CHANGE_DATA USING FV_KUNNR
                         FV_BSTKD
                         FV_VBELN
                         FV_IHREZ.

  DATA : LV_KUNNR	TYPE KUNNR,
         LV_BSTKD	TYPE BSTKD,
         LV_VBELN	TYPE VBELN_VA,
         LV_IHREZ	TYPE IHREZ.

  DATA : LT_HEADER TYPE TABLE OF ZSDSMMT003,
         LT_DETAIL TYPE TABLE OF ZSDSMMT004.

  RANGES : LR_KUNNR FOR KNA1-KUNNR,
           LR_BSTKD FOR VBAK-BSTNK,
           LR_VBELN FOR VBAK-VBELN,
           LR_IHREZ FOR VBAK-IHREZ.

  LR_KUNNR = FV_KUNNR.
  LR_BSTKD = FV_BSTKD.
  LR_VBELN = FV_VBELN.
  LR_IHREZ = FV_IHREZ.

  LV_KUNNR = LR_KUNNR-LOW.
  LV_BSTKD = LR_BSTKD-LOW.
  LV_VBELN = LR_VBELN-LOW.
  LV_IHREZ = LR_IHREZ-LOW.

  CALL FUNCTION 'ZBAPI_GET_CUSTOMER_PO'
    EXPORTING
      P_KUNNR   = LV_KUNNR
      P_BSTKD   = LV_BSTKD
      P_VBELN   = LV_VBELN
      P_IHREZ   = LV_IHREZ
    TABLES
      IT_HEADER = LT_HEADER
      IT_DETAIL = LT_DETAIL.

  GT_ZTSD_PO_D[] = LT_DETAIL.
  GT_ZTSD_PO_H[] = LT_HEADER.

  PERFORM F_GET_DATA_CHANGE USING LT_HEADER
                                  LT_DETAIL
                                  FV_KUNNR
                                  FV_BSTKD
                                  FV_VBELN
                                  FV_IHREZ.
  PERFORM F_GET_TERM_CHA.
  PERFORM F_GET_DIST.
  PERFORM F_GET_DESC_SLAES_G_O.
  PERFORM F_GET_PARTNER_CHA.
  CLEAR GS_DETAIL.
  DO 200 TIMES.
    ADD 1 TO COLTROL_QT-LINES.
    APPEND GS_DETAIL TO GT_DETAIL.
  ENDDO.
  IF GS_HEADER IS NOT INITIAL.
    COLTROL_QT-TOP_LINE = 0.
    PERFORM F_SHOW_REPORT.
  ELSE.
    MESSAGE S003 DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " F_CHANGE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DATA_CHANGE USING FT_HEADER
                             FT_DETAIL
                             FV_KUNNR
                             FV_BSTKD
                             FV_VBELN
                             FV_IHREZ.

  RANGES : LR_KUNNR FOR KNA1-KUNNR,
           LR_BSTKD FOR VBAK-BSTNK,
           LR_VBELN FOR VBAK-VBELN,
           LR_IHREZ FOR VBAK-IHREZ.

  LR_KUNNR = FV_KUNNR.
  LR_BSTKD = FV_BSTKD.
  LR_VBELN = FV_VBELN.
  LR_IHREZ = FV_IHREZ.

  DATA : LT_HEADER TYPE TABLE OF ZSDSMMT003,
         LT_DETAIL TYPE TABLE OF ZSDSMMT004,
         LS_HEADER TYPE ZSDSMMT003,
         LS_DETAIL TYPE ZSDSMMT004.

  CONSTANTS LC_SET TYPE C LENGTH 3 VALUE 'SET'.

  LT_HEADER[] = FT_HEADER.
  LT_DETAIL[] = FT_DETAIL.

  CLEAR : GT_HEADER[],GS_HEADER,GS_DETAIL.
*--------------------------------------------------------------------*
* Loop Header Data
*--------------------------------------------------------------------*
  LOOP AT LT_HEADER INTO LS_HEADER WHERE KUNNR IN LR_KUNNR
                                     AND BSTKD IN LR_BSTKD
                                     AND VBELN IN LR_VBELN
                                     AND IHREZ IN LR_IHREZ
                                     AND DFLAG NE GC_TRUE.
    GV_OLD_BSTNK = LS_HEADER-BSTKD.
    MOVE-CORRESPONDING LS_HEADER TO GS_HEADER.
    GS_HEADER-BSTNK   = LS_HEADER-BSTKD.
    GS_HEADER-PERNR   = LS_HEADER-PARNR.
    GV_DOCTYPE        = LS_HEADER-DOCTYP.
    GS_HEADER-PROJTXT = LS_HEADER-PROJNAM.
    GS_HEADER-VTPCT   = 7."ls_header-kbetr.
    GS_HEADER-VTAMT   = LS_HEADER-MWSBP.
    GS_HEADER-TOTAL   = LS_HEADER-NETWR + LS_HEADER-MWSBP.
    GS_HEADER-AUDAT   = LS_HEADER-BSTDK.
    GV_REASON         = LS_HEADER-MARK.
  ENDLOOP.
  CLEAR COLTROL_QT-LINES.
*--------------------------------------------------------------------*
* Loop Detail
*--------------------------------------------------------------------*
  LOOP AT LT_DETAIL INTO LS_DETAIL WHERE KUNNR IN LR_KUNNR
                                     AND BSTKD IN LR_BSTKD
                                     AND VBELN IN LR_VBELN
                                     AND IHREZ IN LR_IHREZ
                                     AND POSNR IS NOT INITIAL
                                     AND DFLAG NE GC_TRUE.

    ADD 1 TO COLTROL_QT-LINES.
    MOVE-CORRESPONDING LS_DETAIL TO GS_DETAIL.
    GS_HEADER-AUFNR = GS_DETAIL-AUFNR.
    IF LS_DETAIL-UEPOS IS INITIAL AND LS_DETAIL-MEINS = LC_SET.
      GS_DETAIL-KOWRR = GC_TRUE .
    ENDIF.
    GV_POSNR = GS_DETAIL-POSNR.
    PERFORM F_GET_SUM_BOM USING GS_DETAIL.
    APPEND GS_DETAIL TO GT_DETAIL.
    CLEAR GS_DETAIL.
  ENDLOOP.

  PERFORM F_UPDATE_PRICE_BOM.
*--------------------------------------------------------------------*
* Get Description IO
*--------------------------------------------------------------------*
  PERFORM F_GET_DESC_IO.
*--------------------------------------------------------------------*
* Sort detail by line tiem
*--------------------------------------------------------------------*
  SORT GT_DETAIL BY POSNR.

ENDFORM.                    " F_GET_DATA_CHANGE
*&---------------------------------------------------------------------*
*&      Form  F_GET_FORM_MEMORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_FORM_MEMORY USING FV_KUNNR
                             FV_BSTKD
                             FV_VBELN
                             FV_IHREZ.

  DATA : LT_HEADER   TYPE TABLE OF ZSDSMMT003,
         LT_DETAIL   TYPE TABLE OF ZSDSMMT004,
         I_HEADER    TYPE TABLE OF ZSDSMMT003,
         I_DETAIL_DB TYPE TABLE OF ZSDSMMT004,
         LS_HEADER   TYPE ZSDSMMT003,
         LS_DETAIL   TYPE ZSDSMMT004.

  IMPORT I_HEADER    FROM MEMORY ID 'PO_HEADER'.
  IMPORT I_DETAIL_DB FROM MEMORY ID 'PO_DETAIL'.

  LT_HEADER[] = I_HEADER[].
  LT_DETAIL[] = I_DETAIL_DB[].

  FREE MEMORY ID 'PO_HEADER'.
  FREE MEMORY ID 'PO_DETAIL'.
  FREE MEMORY ID 'PO_STATUS'.
*--------------------------------------------------------------------*
* gt_ztsd_po_d will used when delete data.
*--------------------------------------------------------------------*
  GT_ZTSD_PO_D[] = LT_DETAIL.
*--------------------------------------------------------------------*
* Convert data to format screen
*--------------------------------------------------------------------*
  PERFORM F_GET_DATA_CHANGE USING LT_HEADER
                                  LT_DETAIL
                                  FV_KUNNR
                                  FV_BSTKD
                                  FV_VBELN
                                  FV_IHREZ.

  PERFORM F_GET_TERM_CHA.
  PERFORM F_GET_DIST.
  PERFORM F_GET_DESC_SLAES_G_O.
  PERFORM F_GET_PARTNER_CHA.
  CLEAR GS_DETAIL.
  DO 200 TIMES.
    ADD 1 TO COLTROL_QT-LINES.
    APPEND GS_DETAIL TO GT_DETAIL.
  ENDDO.
  IF GS_HEADER IS NOT INITIAL.
    COLTROL_QT-TOP_LINE = 0.
    PERFORM F_SHOW_REPORT.
  ELSE.
    MESSAGE S003 DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " F_GET_FORM_MEMORY
*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND_1000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1000 .
  DATA P_STATUS TYPE C.
  PERFORM F_CLEAR_DATA.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'OUT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'CRE'.
      SET TITLEBAR 'Z_CRE'.
      CLEAR : GV_STATUS,S_KUNNR,S_KUNNR[],S_BSTNK,S_BSTNK[].
    WHEN 'CHE'.
      SET TITLEBAR 'Z_CHE'.
      GV_STATUS = 'C'.
      CLEAR : S_VBELN,S_VBELN[],S_VBELNS,S_VBELNS[],S_IHREZ,S_IHREZ[].
    WHEN 'DIS'.
      SET TITLEBAR 'Z_DIS'.
      GV_STATUS = 'D'.
      CLEAR : S_VBELN,S_VBELN[],S_VBELNS,S_VBELNS[],S_IHREZ,S_IHREZ[].
    WHEN 'ONLI'.
*--------------------------------------------------------------------*
* Cheange Sales Document From Program P'Get
*--------------------------------------------------------------------*
      IMPORT P_STATUS FROM MEMORY ID 'PO_STATUS'.
      GV_STATUS = P_STATUS.
      IF GV_STATUS = 'C' OR GV_STATUS = 'D'.
        CASE GV_STATUS.
          WHEN 'C'.
            SET TITLEBAR 'Z_CHE'.
          WHEN 'D'.
            SET TITLEBAR 'Z_DIS'.
        ENDCASE.
        PERFORM F_GET_DOC_TYPE.
        PERFORM F_GET_FORM_MEMORY USING S_KUNNR
                                        S_BSTNK
                                        S_VBELN
                                        S_IHREZ.
      ENDIF.
      PERFORM F_CHECK_SELECTION_SCREEN.
  ENDCASE.

ENDFORM.                    " F_USER_COMMAND_1000
*&---------------------------------------------------------------------*
*&      Form  F_DELETE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DELETE_DOCUMENT.

  DELETE FROM ZSDSMMT004 WHERE KUNNR = GS_HEADER-KUNNR AND
                              BSTKD = GS_HEADER-BSTNK AND
                              IHREZ = GS_HEADER-IHREZ.

  DELETE FROM ZSDSMMT003 WHERE KUNNR = GS_HEADER-KUNNR AND
                              BSTKD = GS_HEADER-BSTNK AND
                              IHREZ = GS_HEADER-IHREZ.

  PERFORM F_INSERT_LOG_HEADER.
  PERFORM F_INSERT_LOG_DETAIL.

ENDFORM.                    " F_DELETE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_LOG_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INSERT_LOG_HEADER .
*  DATA : LS_ZTSD_PO_H_LOG TYPE ZTSD_PO_H_LOG.
*  DATA : LV_CHGNR TYPE ZTSD_PO_D_LOG-CHGNR.
*
*  SELECT MAX( CHGNR )
*        FROM ZTSD_PO_D_LOG
*        INTO LV_CHGNR
*        WHERE KUNNR = GS_HEADER-KUNNR AND
*              BSTKD = GS_HEADER-BSTNK AND
*              VBELN = GS_HEADER-VBELN AND
*              IHREZ = GS_HEADER-IHREZ.
*
*  MOVE-CORRESPONDING GS_HEADER TO LS_ZTSD_PO_H_LOG.
*  LS_ZTSD_PO_H_LOG-BSTKD   = GS_HEADER-BSTNK.
*  LS_ZTSD_PO_H_LOG-PARNR   = GS_HEADER-PERNR.
*  LS_ZTSD_PO_H_LOG-DOCTYP  = GV_DOCTYPE.
*  LS_ZTSD_PO_H_LOG-PROJNAM = GS_HEADER-PROJTXT.
*  LS_ZTSD_PO_H_LOG-KBETR   = GS_HEADER-VTPCT.
*  LS_ZTSD_PO_H_LOG-MWSBP   = GS_HEADER-VTAMT.
*  LS_ZTSD_PO_H_LOG-ERDAT   = SY-DATUM.
*  LS_ZTSD_PO_H_LOG-ERTIM   = SY-UZEIT.
*  LS_ZTSD_PO_H_LOG-ERNAM   = SY-UNAME.
*  LS_ZTSD_PO_H_LOG-ACTION  = 'DEL'.
*  LS_ZTSD_PO_H_LOG-CHGNR   = LV_CHGNR + 1.
*  INSERT ZTSD_PO_H_LOG FROM LS_ZTSD_PO_H_LOG.


ENDFORM.                    " F_INSERT_LOG_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_LOG_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INSERT_LOG_DETAIL .
*  DATA : LS_ZTSD_PO_D_LOG TYPE ZTSD_PO_D_LOG.
*  DATA : LV_CHGNR TYPE ZTSD_PO_D_LOG-CHGNR.
*
*  SELECT MAX( CHGNR )
*        FROM ZTSD_PO_D_LOG
*        INTO LV_CHGNR
*        WHERE KUNNR = GS_HEADER-KUNNR AND
*              BSTKD = GS_HEADER-BSTNK AND
*              VBELN = GS_HEADER-VBELN AND
*              IHREZ = GS_HEADER-IHREZ.
*
*  LOOP AT GT_DETAIL INTO GS_DETAIL WHERE VBELN IS NOT INITIAL.
*    MOVE-CORRESPONDING GS_DETAIL TO LS_ZTSD_PO_D_LOG.
*    LS_ZTSD_PO_D_LOG-KUNNR  = GS_HEADER-KUNNR.
*    LS_ZTSD_PO_D_LOG-BSTKD  = GS_HEADER-BSTNK.
*    LS_ZTSD_PO_D_LOG-IHREZ  = GS_HEADER-IHREZ.
*    LS_ZTSD_PO_D_LOG-ERDAT  = SY-DATUM.
*    LS_ZTSD_PO_D_LOG-ERTIM  = SY-UZEIT.
*    LS_ZTSD_PO_D_LOG-ERNAM  = SY-UNAME.
*    LS_ZTSD_PO_D_LOG-CHGNR  = LV_CHGNR + 1.
*    LS_ZTSD_PO_D_LOG-ACTION = 'DEL'.
*    INSERT ZTSD_PO_D_LOG FROM LS_ZTSD_PO_D_LOG.
*  ENDLOOP.
*
*  CLEAR : LS_ZTSD_PO_D_LOG,GS_ZTSD_PO_D.
*  IF GV_STATUS EQ 'C'.
*    LOOP AT GT_ZTSD_PO_D INTO GS_ZTSD_PO_D.
*      READ TABLE GT_DETAIL INTO GS_DETAIL
*      WITH KEY VBELN = GS_ZTSD_PO_D-VBELN
*               POSNR = GS_ZTSD_PO_D-POSNR.
*      IF SY-SUBRC NE 0.
*        MOVE-CORRESPONDING GS_ZTSD_PO_D TO LS_ZTSD_PO_D_LOG.
*        LS_ZTSD_PO_D_LOG-CHGNR   = LV_CHGNR + 1.
*        LS_ZTSD_PO_D_LOG-ACTION  = 'DEL'.
*        INSERT ZTSD_PO_D_LOG FROM LS_ZTSD_PO_D_LOG.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    " F_INSERT_LOG_DETAIL
*&---------------------------------------------------------------------*
*&      Form  F_GET_TERM_CHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_TERM_CHA .
  CONSTANTS LC_BUKRS TYPE C LENGTH 4 VALUE '1000'.

  SELECT SINGLE ZTERM
    FROM KNB1
    INTO GS_HEADER-ZTERM
    WHERE KUNNR = GS_HEADER-KUNNR
      AND BUKRS = LC_BUKRS.

ENDFORM.                    " F_GET_TERM_CHA
*&---------------------------------------------------------------------*
*&      Form  F_GET_PARTNER_CHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_PARTNER_CHA .
  CONSTANTS : BEGIN OF LC_PARVW,
    SH TYPE C LENGTH 2 VALUE 'WE',
    PE TYPE C LENGTH 2 VALUE 'VE',
  END OF LC_PARVW.

  SELECT VBPA~VBELN
         VBPA~PARVW
         VBPA~KUNNR
         VBPA~PERNR
         KNA1~NAME1
    FROM VBPA
    LEFT JOIN KNA1 ON VBPA~KUNNR = KNA1~KUNNR
    INTO TABLE GT_VBPA
    WHERE VBELN = GS_HEADER-VBELN
      AND ( PARVW = LC_PARVW-SH
       OR   PARVW = LC_PARVW-PE ).

  IF GT_VBPA[] IS NOT INITIAL.
    SELECT PERNR
           SNAME
      FROM PA0001
      INTO TABLE GT_PA0001
      FOR ALL ENTRIES IN GT_VBPA
      WHERE PERNR = GT_VBPA-PERNR.
  ENDIF.

  PERFORM F_GET_RESULT_PARTNER.
  PERFORM F_GET_NAME_CUST.

ENDFORM.                    " F_GET_PARTNER_CHA
*&---------------------------------------------------------------------*
*&      Form  F_GET_NAME_CUST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_NAME_CUST .
  SELECT SINGLE NAME1
    FROM KNA1
    INTO GS_HEADER-NAME1
    WHERE KUNNR EQ GS_HEADER-KUNNR.
ENDFORM.                    " F_GET_NAME_CUST

*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_SELECTION.
  IF GV_STATUS IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'MOD'.
        SCREEN-INTENSIFIED = '1'.
        SCREEN-ACTIVE      = '0'.
        SCREEN-DISPLAY_3D  = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'CRE'.
        SCREEN-INTENSIFIED = '1'.
        SCREEN-ACTIVE      = '0'.
        SCREEN-DISPLAY_3D  = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_GET_SELECTION
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_SELECTION_SCREEN .
  IF S_VBELNS[] IS NOT INITIAL.
    CLEAR S_VBELN[].
  ENDIF.
ENDFORM.                    " F_CHECK_SELECTION_SCREEN

*&---------------------------------------------------------------------*
*&      Form  GET_CURSOR_1002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CURSOR  text
*----------------------------------------------------------------------*
FORM GET_CURSOR  USING P_CURSOR.
  CASE P_CURSOR .
    WHEN 'GS_HEADER-BSTNK' .
      P_CURSOR = 'GS_HEADER-BSTDK' .
    WHEN 'GS_HEADER-BSTDK' .
      P_CURSOR = 'GS_HEADER-KUNNR' .
    WHEN 'GS_HEADER-KUNNR' .
      P_CURSOR = 'GS_HEADER-VTWEG' .
    WHEN 'GS_HEADER-VTWEG' .
      P_CURSOR = 'GS_HEADER-VKBUR' .
    WHEN 'GS_HEADER-VKBUR' .
      P_CURSOR = 'GS_HEADER-VKGRP' ."p_cursor = 'GS_HEADER-IHREZ' .
    WHEN 'GS_HEADER-VKGRP'.
      P_CURSOR = 'GS_HEADER-PERNR'  .
    WHEN 'GS_HEADER-PERNR'.
      P_CURSOR = 'GS_HEADER-IHREZ' ."p_cursor = 'GS_HEADER-PERNR' .
    WHEN 'GS_HEADER-IHREZ' .
      P_CURSOR = 'GS_HEADER-CONTACT_NAME' .
    WHEN 'GS_HEADER-CONTACT_NAME' .
      P_CURSOR = 'GS_HEADER-CONTACT_TEL' .
    WHEN 'GS_HEADER-CONTACT_TEL' .
      P_CURSOR = 'GS_HEADER-AUFNR'.
    WHEN 'GS_HEADER-AUFNR' .
      P_CURSOR = 'GS_HEADER-PROJTXT' .
    WHEN 'GS_HEADER-PROJTXT' .
      P_CURSOR = 'GS_HEADER-REFNO' .
    WHEN 'GS_HEADER-REFNO' .
      P_CURSOR = 'GS_HEADER-VTPCT' .
    WHEN 'GS_HEADER-VTPCT' .
      P_CURSOR = 'GS_HEADER-HDRRMK' .
    WHEN 'GS_HEADER-HDRRMK' .
      P_CURSOR = 'MATNR' .
    WHEN OTHERS.
      P_CURSOR = 'DETAIL'.
  ENDCASE.
ENDFORM.                    " GET_CURSOR_1002

*&---------------------------------------------------------------------*
*&      Form  F_GET_SUM_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_SUM_BOM USING FS_DETAIL.
  DATA LS_DETAIL LIKE GS_DETAIL.
  LS_DETAIL = FS_DETAIL.
  GS_SUM_BOM-MATNR = LS_DETAIL-UPMAT.
  GS_SUM_BOM-UEPOS = LS_DETAIL-UEPOS.
  GS_SUM_BOM-PRBOM = LS_DETAIL-NETPR.
  COLLECT GS_SUM_BOM INTO GT_SUM_BOM.
ENDFORM.                    " F_GET_SUM_BOM
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_PRICE_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UPDATE_PRICE_BOM .
  DATA LS_DETAIL LIKE GS_DETAIL.
  CLEAR GS_SUM_BOM.

  LOOP AT GT_SUM_BOM INTO GS_SUM_BOM .
    MOVE-CORRESPONDING GS_SUM_BOM TO LS_DETAIL.
    MODIFY GT_DETAIL FROM LS_DETAIL TRANSPORTING PRBOM
                                           WHERE POSNR = GS_SUM_BOM-UEPOS
                                             AND MATNR = GS_SUM_BOM-MATNR
                                             AND POSNR IS NOT INITIAL.
  ENDLOOP.

  CLEAR : GS_SUM_BOM.

ENDFORM.                    " F_UPDATE_PRICE_BOM

*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CLEAR_DATA .
  CLEAR : GT_SUM_BOM.
ENDFORM.                    " F_CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_SAVE CHANGING FV_CHECK_SAVE.
  IF GV_STATUS IS INITIAL.
    SELECT SINGLE *
      FROM ZSDSMMT003
      WHERE KUNNR EQ GS_HEADER-KUNNR
        AND BSTKD EQ GS_HEADER-BSTNK.
    IF SY-SUBRC = 0.
      FV_CHECK_SAVE = GC_ERROR.
    ELSE.
      CLEAR FV_CHECK_SAVE.
    ENDIF.
  ELSE.
    CLEAR FV_CHECK_SAVE.
  ENDIF.

ENDFORM.                    " F_CHECK_SAVE
*&---------------------------------------------------------------------*
*&      Form  F_GET_POSNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_POSNR USING FV_LINE.
  GV_POSNR = GV_POSNR + 10.
  IF GV_POSNR LT 100.
    CONCATENATE GV_POSNR+0(5) '0' INTO GS_DETAIL-POSNR.
  ELSEIF GV_POSNR GT 100 AND GV_POSNR LT 1000.
    CONCATENATE GV_POSNR+0(5) '0' INTO GS_DETAIL-POSNR.
  ELSEIF GV_POSNR GT 1000 AND GV_POSNR LT 10000.
    CONCATENATE GV_POSNR+0(5) '0' INTO GS_DETAIL-POSNR.
  ELSEIF GV_POSNR GT 10000 AND GV_POSNR LT 100000.
    CONCATENATE GV_POSNR+0(5) '0' INTO GS_DETAIL-POSNR.
  ELSE.
    CONCATENATE GV_POSNR+0(5) '0' INTO GS_DETAIL-POSNR.
  ENDIF.
ENDFORM.                    " F_GET_POSNR

*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_RETURN  text
*----------------------------------------------------------------------*
FORM F_MODIFY_LINE TABLES FT_BOM_ITEM
                    USING FS_RETURN
                          FV_MATERIAL.
  DATA : LS_RETURN TYPE BAPIRET2.

  DATA : LT_BOM_ITEM  TYPE TABLE OF STPO_API02.
  LT_BOM_ITEM[] = FT_BOM_ITEM[].
  LS_RETURN     = FS_RETURN.

  IF LS_RETURN-TYPE   = 'E'   AND
     LS_RETURN-ID     = 'M3'  AND
     LS_RETURN-NUMBER = '305'.
*--------------------------------------------------------------------*
    MESSAGE E000 WITH LS_RETURN-MESSAGE.
*--------------------------------------------------------------------*
  ELSEIF LS_RETURN IS INITIAL.
*--------------------------------------------------------------------*
    PERFORM F_GET_BOM_LINE USING FV_MATERIAL
                                 LT_BOM_ITEM.
*--------------------------------------------------------------------*
  ELSE.
*--------------------------------------------------------------------*
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE TRANSPORTING MATNR.
    CLEAR GS_DETAIL-MAKTX.

    DELETE GT_DETAIL  WHERE UEPOS EQ GS_DETAIL-POSNR.
    DELETE GT_SUM_BOM WHERE UEPOS EQ GS_DETAIL-POSNR.

    IF     GS_HEADER-VTWEG = '10'.
      PERFORM F_GET_PRICE_DEALER USING GS_DETAIL-MATNR.
      PERFORM F_GET_MAT_DEST USING GS_DETAIL-MATNR.
      MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                                      TRANSPORTING NETPR MEINS MWSBP NETWR_V AUFNR KTEXT POSNR MAKTX.

    ELSEIF GS_HEADER-VTWEG = '20' OR GS_HEADER-VTWEG = '40'.
      PERFORM F_GET_PRICE_GEN_SER USING GS_DETAIL-MATNR.
      PERFORM F_GET_MAT_DEST USING GS_DETAIL-MATNR.
      MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                                      TRANSPORTING NETPR MEINS MWSBP NETWR_V AUFNR KTEXT POSNR MAKTX.
    ENDIF.

    GET CURSOR LINE GV_LINE.
    CLEAR GV_FLAG.
*--------------------------------------------------------------------*
  ENDIF.

ENDFORM.                    " F_MODIFY_LINE
*&---------------------------------------------------------------------*
*&      Form  F_GET_BOM_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FV_MATERIAL  text
*      -->P_FT_BOM_ITEM  text
*----------------------------------------------------------------------*
FORM F_GET_BOM_LINE USING FV_MATERIAL
                          FT_BOM_ITEM.

  DATA : LT_BOM_ITEM  TYPE TABLE OF STPO_API02,
         LS_BOM_ITEM  TYPE STPO_API02.

  DATA : LS_DETAIL    LIKE GS_DETAIL.

  DATA : LV_MATERIAL  TYPE RC29L-MATNR,
         LV_POSNR     TYPE I,
         LV_LINE      TYPE I,
         LV_KWMENG    TYPE VBAP-KWMENG,
         LV_POSNR_UP  TYPE VBAP-POSNR.

  LT_BOM_ITEM[]   = FT_BOM_ITEM.
  LV_MATERIAL     = FV_MATERIAL.
  GS_DETAIL-MATNR = FV_MATERIAL.

  CLEAR : LV_POSNR,LV_LINE,GS_DETAIL-NETPR,GS_DETAIL-MEINS,GS_DETAIL-MAKTX.

  GS_DETAIL-KOWRR = GC_TRUE.
*--------------------------------------------------------------------*
* Bom
*--------------------------------------------------------------------*
  IF     GS_HEADER-VTWEG = '10'.
    PERFORM F_GET_PRICE_DEALER USING LV_MATERIAL.
    PERFORM F_GET_MAT_DEST USING LV_MATERIAL.
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                                    TRANSPORTING MATNR NETPR MEINS MWSBP
                                                 NETWR_V AUFNR KTEXT POSNR KOWRR KWMENG MAKTX.
  ELSEIF GS_HEADER-VTWEG = '20' OR GS_HEADER-VTWEG = '40'.
    PERFORM F_GET_PRICE_GEN_SER USING LV_MATERIAL.
    PERFORM F_GET_MAT_DEST USING LV_MATERIAL.
    MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                                    TRANSPORTING MATNR NETPR MEINS MWSBP
                                                 NETWR_V AUFNR KTEXT POSNR KOWRR KWMENG MAKTX.
  ENDIF.

  LV_POSNR_UP = GS_DETAIL-POSNR.
  GET CURSOR LINE GV_LINE.
  CLEAR GV_FLAG.
  LV_KWMENG = GS_DETAIL-KWMENG.
  DELETE GT_DETAIL  WHERE UEPOS EQ GS_DETAIL-POSNR.
  DELETE GT_SUM_BOM WHERE UEPOS EQ GS_DETAIL-POSNR.
  CLEAR : LV_LINE.
*--------------------------------------------------------------------*
* Loop Companance
*--------------------------------------------------------------------*
  LOOP AT LT_BOM_ITEM INTO LS_BOM_ITEM.
    ADD 1 TO LV_POSNR.
    LV_LINE = COLTROL_QT-CURRENT_LINE + LV_POSNR.
    IF     GS_HEADER-VTWEG = '10'.
      PERFORM F_GET_PRICE_DEALER USING LS_BOM_ITEM-COMPONENT.
      PERFORM F_CHECK_LINE CHANGING LV_LINE.
      MOVE-CORRESPONDING GS_DETAIL TO LS_DETAIL.
      LS_DETAIL-MATNR  = LS_BOM_ITEM-COMPONENT.
      LS_DETAIL-UPMAT  = LV_MATERIAL.
      LS_DETAIL-UEPOS  = LS_DETAIL-POSNR.
      LS_DETAIL-POSNR  = LS_DETAIL-POSNR + LV_POSNR.
      LS_DETAIL-KWMENG = LV_KWMENG.
      LS_DETAIL-NETWR  = LS_DETAIL-KWMENG * LS_DETAIL-NETPR.
      GS_HEADER-NETWR  = GS_HEADER-NETWR + LS_DETAIL-NETWR.
      GS_HEADER-VTAMT  = ( ( GS_HEADER-NETWR * GS_HEADER-VTPCT ) / 100 ).
      GS_HEADER-TOTAL  = GS_HEADER-NETWR + GS_HEADER-VTAMT.
      PERFORM F_GET_MAT_DEST USING LS_DETAIL-MATNR.
      MODIFY GT_DETAIL FROM LS_DETAIL INDEX LV_LINE
                                      TRANSPORTING MATNR NETPR MEINS MWSBP NETWR_V
                                                   AUFNR KTEXT POSNR UEPOS UPMAT KWMENG NETWR.
*--------------------------------------------------------------------*
    ELSEIF GS_HEADER-VTWEG = '20' OR GS_HEADER-VTWEG = '40'.
      PERFORM F_GET_PRICE_GEN_SER USING LS_BOM_ITEM-COMPONENT.
      PERFORM F_CHECK_LINE CHANGING LV_LINE.
      MOVE-CORRESPONDING GS_DETAIL TO LS_DETAIL.
      LS_DETAIL-MATNR  = LS_BOM_ITEM-COMPONENT.
      LS_DETAIL-UPMAT  = LV_MATERIAL.
      LS_DETAIL-UEPOS  = LS_DETAIL-POSNR.
      LS_DETAIL-POSNR  = LS_DETAIL-POSNR + LV_POSNR.
      LS_DETAIL-KWMENG = LV_KWMENG.
      LS_DETAIL-NETWR  = LS_DETAIL-KWMENG * LS_DETAIL-NETPR.
      GS_HEADER-NETWR  = GS_HEADER-NETWR + LS_DETAIL-NETWR.
      GS_HEADER-VTAMT  = ( ( GS_HEADER-NETWR * GS_HEADER-VTPCT ) / 100 ).
      GS_HEADER-TOTAL  = GS_HEADER-NETWR + GS_HEADER-VTAMT.
      PERFORM F_GET_MAT_DEST USING LS_DETAIL-MATNR.
      MODIFY GT_DETAIL FROM LS_DETAIL INDEX LV_LINE
                                      TRANSPORTING MATNR NETPR MEINS MWSBP NETWR_V
                                                   AUFNR KTEXT POSNR UEPOS UPMAT KWMENG NETWR.
    ENDIF.
    PERFORM F_GET_SUM_BOM USING LS_DETAIL.
    CLEAR LS_DETAIL.
  ENDLOOP.
  CLEAR LS_DETAIL.
  LS_DETAIL-POSNR = '999999'.
  MODIFY GT_DETAIL FROM LS_DETAIL TRANSPORTING POSNR
                                         WHERE POSNR IS INITIAL.
  SORT GT_DETAIL BY POSNR.
  CLEAR LS_DETAIL.
  MODIFY GT_DETAIL FROM LS_DETAIL TRANSPORTING POSNR
                                         WHERE POSNR = '999999'.
  PERFORM F_UPDATE_PRICE_BOM.

ENDFORM.                    " F_GET_BOM_LINE

*&---------------------------------------------------------------------*
*&      Form  F_GET_ORI_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_ORI_PO.
  SELECT VBELN
         POSNR
         BSTKD_E
    FROM VBKD
    INTO TABLE GT_VBKD
    FOR ALL ENTRIES IN GT_HEADER
    WHERE VBELN = GT_HEADER-VBELN.
ENDFORM.                    " F_GET_ORI_PO
*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT_ORI_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_RESULT_ORI_PO .
  READ TABLE GT_VBKD INTO GS_VBKD
  WITH KEY VBELN = GS_HEADER-VBELN.
  IF SY-SUBRC = 0.
    GS_HEADER-BSTKD_E = GS_VBKD-BSTKD_E.
  ENDIF.
ENDFORM.                    " F_GET_RESULT_ORI_PO

*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_AMOUNT_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UPDATE_AMOUNT_HEADER .
  CLEAR : GS_DETAIL,GS_HEADER-NETWR,GS_HEADER-VTAMT,GS_HEADER-TOTAL.

  LOOP AT GT_DETAIL INTO GS_DETAIL WHERE POSNR IS NOT INITIAL
                                     AND CM    NE 'X'.
    GS_HEADER-NETWR   = GS_HEADER-NETWR + GS_DETAIL-NETWR.
  ENDLOOP.

  GS_HEADER-VTAMT   = ( ( GS_HEADER-NETWR * GS_HEADER-VTPCT ) / 100 ).
  GS_HEADER-TOTAL   = GS_HEADER-NETWR + GS_HEADER-VTAMT.
ENDFORM.                    " F_UPDATE_AMOUNT_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_GET_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_REASON CHANGING FV_CONFIRMATION.
  DATA : LV_CONFIRMATION.
  CONSTANTS : BEGIN OF LC_CONFIRMATION,
    YES    TYPE C VALUE 'J',
    NO     TYPE C VALUE 'N',
    CANCEL TYPE C VALUE 'A',
  END OF LC_CONFIRMATION.

  CONSTANTS : LC_DISPLAY TYPE C VALUE 'D',
              LC_CHANGE  TYPE C VALUE 'C'.

  CLEAR : GV_ZCUSTOMER_REASON,GV_ZUSER_REASON.

  "gv_old_bstnk = gs_header-bstnk.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TEXTLINE1 = TEXT-108
      TITEL     = TEXT-107
    IMPORTING
      ANSWER    = LV_CONFIRMATION.
  IF     LV_CONFIRMATION = LC_CONFIRMATION-YES.
    CALL SCREEN 101 STARTING AT 20 5.
  ELSEIF LV_CONFIRMATION = LC_CONFIRMATION-NO.
    CALL SCREEN 102 STARTING AT 20 5.
  ELSEIF LV_CONFIRMATION = LC_CONFIRMATION-CANCEL.
    GV_STATUS  = LC_DISPLAY.
    GV_STA_LOG = LC_CHANGE.
  ENDIF.

  FV_CONFIRMATION = LV_CONFIRMATION.

ENDFORM.                    " F_GET_REASON
*&---------------------------------------------------------------------*
*&      Form  F_GET_POSNR_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_POSNR_INIT .

  GS_DETAIL-POSNR_T = GS_DETAIL-POSNR.
  GS_DETAIL-KOWRR_T = GS_DETAIL-KOWRR.
  GS_DETAIL-UEPOS_T = GS_DETAIL-UEPOS.
  GS_DETAIL-UPMAT_T = GS_DETAIL-UPMAT.

ENDFORM.                    " F_GET_POSNR_INIT
*&---------------------------------------------------------------------*
*&      Form  F_CHAGE_POSNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHAGE_POSNR.
  DATA : LS_DETAIL LIKE GS_DETAIL,
         LS_DETAIL_S LIKE GS_DETAIL.

  DATA : LV_TABIX TYPE SY-TABIX.

  CLEAR GV_POSNR_INIT.
  LOOP AT GT_DETAIL INTO LS_DETAIL.
    LV_TABIX = SY-TABIX.
    IF LS_DETAIL-KOWRR EQ GC_TRUE.  "Bom Material
      IF GV_POSNR_INIT IS INITIAL.
        GV_POSNR_INIT  = 10.
      ELSE.
        GV_POSNR_INIT  = GV_POSNR_INIT + 10.
      ENDIF.
      LS_DETAIL-POSNR = GV_POSNR_INIT.
    ELSEIF LS_DETAIL-UEPOS IS INITIAL AND "NON BOM Material
           LS_DETAIL-UPMAT IS INITIAL.
      IF GV_POSNR_INIT  IS INITIAL.
        GV_POSNR_INIT  = 10.
      ELSE.
        GV_POSNR_INIT  = GV_POSNR_INIT + 10.
      ENDIF.
      LS_DETAIL-POSNR = GV_POSNR_INIT.
    ELSE. "Companance
      READ TABLE GT_DETAIL INTO LS_DETAIL_S
      WITH KEY VBELN   = LS_DETAIL-VBELN
               POSNR_T = LS_DETAIL-UEPOS.
      IF SY-SUBRC = 0.
        CONCATENATE LS_DETAIL_S-POSNR+0(5) LS_DETAIL-POSNR+5(1) INTO LS_DETAIL-POSNR.
        LS_DETAIL-UEPOS = LS_DETAIL_S-POSNR.
      ENDIF.
    ENDIF.

    PERFORM F_GET_SUM_BOM USING LS_DETAIL.
    GV_POSNR = LS_DETAIL-POSNR.
    MODIFY GT_DETAIL FROM LS_DETAIL INDEX LV_TABIX
                             TRANSPORTING POSNR UEPOS.

    CLEAR LS_DETAIL.
  ENDLOOP.

ENDFORM.                    " F_CHAGE_POSNR
*&---------------------------------------------------------------------*
*&      Form  F_GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_ZCUSTOMER_REASON  text
*      <--P_GV_ZCUSTOMER_REASON  text
*----------------------------------------------------------------------*
FORM F_GET_DETAIL  USING    LV_DOMAIN TYPE DD07L-DOMNAME
                   CHANGING LV_REASON.

  DATA : LT_DD07V_TAB	TYPE TABLE OF DD07V,
         LS_DD07V_TAB	TYPE DD07V.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      DOMNAME        = LV_DOMAIN
      TEXT           = 'X'
      LANGU          = SY-LANGU
    TABLES
      DD07V_TAB      = LT_DD07V_TAB
    EXCEPTIONS
      WRONG_TEXTFLAG = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_DD07V_TAB INTO LS_DD07V_TAB WHERE DOMVALUE_L = LV_REASON.
    LV_REASON = LS_DD07V_TAB-DDTEXT.
  ENDLOOP.

ENDFORM.                    " F_GET_DETAIL
*&---------------------------------------------------------------------*
*&      Form  F_GET_SUM_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_SUM_AMOUNT USING LV_NETWR.
  GS_HEADER-NETWR = LV_NETWR.
ENDFORM.                    " F_GET_SUM_AMOUNT

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_CM .
  DATA : LS_DETAIL LIKE GS_DETAIL.
  IF GS_DETAIL-KOWRR = 'X' AND GS_DETAIL-CM = 'X'.
    MODIFY GT_DETAIL FROM GS_DETAIL TRANSPORTING CM
                                    WHERE ( UEPOS = GS_DETAIL-POSNR OR
                                            POSNR = GS_DETAIL-POSNR ).

  ELSEIF GS_DETAIL-KOWRR = 'X' AND GS_DETAIL-CM NE 'X'.
    MODIFY GT_DETAIL FROM GS_DETAIL TRANSPORTING CM
                                    WHERE ( UEPOS = GS_DETAIL-POSNR OR
                                            POSNR = GS_DETAIL-POSNR ).
  ELSEIF GS_DETAIL-KOWRR NE 'X'.
    READ TABLE GT_DETAIL INTO LS_DETAIL
    WITH KEY POSNR = GS_DETAIL-UEPOS
                CM = 'X'.
    IF SY-SUBRC NE 0.
      MODIFY GT_DETAIL FROM GS_DETAIL INDEX COLTROL_QT-CURRENT_LINE
                                      TRANSPORTING CM.
    ENDIF.
    CLEAR LS_DETAIL.
  ENDIF.
ENDFORM.                    " F_CHECK_CM
