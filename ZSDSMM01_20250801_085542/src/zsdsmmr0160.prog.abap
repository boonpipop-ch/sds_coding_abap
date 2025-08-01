*&---------------------------------------------------------------------*
*& Report ZSDSMMR0160
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*REPORT ZSDSMMR0160 MESSAGE-ID ZSDSMM01.

INCLUDE: M07DRTOP.                   "Tabellen und Datendeklarationen

FORM ENTRY_WA03 USING ENT_RETCO ENT_SCREEN.

  XSCREEN = ENT_SCREEN.
  CLEAR ENT_RETCO.
  CLEAR LGORTSPLIT.
  PERFORM LESEN_WAS USING NAST-OBJKY LGORTSPLIT.
  ENT_RETCO = RETCO.

ENDFORM.
FORM LESEN_WAS USING OBJKY LGORTSPLIT.

  DATA: LF_FM_NAME        TYPE RS38L_FNAM,
        LS_CONTROL_PARAM  TYPE SSFCTRLOP,
        LS_COMPOSER_PARAM TYPE SSFCOMPOP,
        LS_RECIPIENT      TYPE SWOTOBJID,
        LS_SENDER         TYPE SWOTOBJID,
        LF_FORMNAME       TYPE TDSFNAME,
        LS_ADDR_KEY       LIKE ADDR_KEY,
        LS_DLV_LAND       LIKE VBRK-LAND1,
        LS_JOB_INFO       TYPE SSFCRESCL.

  DATA: LS_DATA   TYPE ZSDSMMS007,
        LS_DETIAL TYPE ZSDSMMS008.

  DATA: LV_QTY TYPE P DECIMALS 0.

  DATA: BEGIN OF LS_MSEG,
          MBLNR TYPE MSEG-MBLNR,
          MJAHR TYPE MSEG-MJAHR,
          ZEILE TYPE MSEG-ZEILE,
          MATNR TYPE MSEG-MATNR,
          WERKS TYPE MSEG-WERKS,
          LGORT TYPE MSEG-LGORT,
          ERFMG TYPE MSEG-ERFMG,
          ERFME TYPE MSEG-ERFME,
          MENGE TYPE MSEG-MENGE,
          MEINS TYPE MSEG-MEINS,
          NAME1 TYPE T001W-NAME1,
          MAKTX TYPE MAKT-MAKTX,
        END OF LS_MSEG.
  DATA LT_MSEG LIKE TABLE OF LS_MSEG WITH EMPTY KEY.
* SmartForm from customizing table TNAPR
  LF_FORMNAME = TNAPR-FONAM.

  REFRESH TRAPTAB.
  NAST_KEY = OBJKY.
  CLEAR RETCO.
  CLEAR: XKOPFDR, NEW_PAGE.
  SELECT SINGLE * FROM MKPF WHERE MBLNR = NAST_KEY-MBLNR
                            AND   MJAHR = NAST_KEY-MJAHR.
*  MOVE-CORRESPONDING MKPF TO TRAPTAB.

  LS_DATA-DOCNO = MKPF-MBLNR.
  LS_DATA-DOCDT = MKPF-BUDAT.
  LS_DATA-PRIDT = MKPF-CPUDT.

  LS_DATA-PPTAN = SPACE.
  LS_DATA-CREBY = MKPF-USNAM.
  LS_DATA-RECBY = SPACE.
  LS_DATA-TANBY = SPACE.

  ZAEHLER_M = 1.
*  SELECT * FROM MSEG WHERE MBLNR = MKPF-MBLNR
*                     AND   MJAHR = MKPF-MJAHR.

  SELECT SER03~MBLNR,
         SER03~MJAHR,
         SER03~ZEILE,
         OBJK~SERNR
    FROM SER03
    INNER JOIN OBJK ON SER03~OBKNR EQ OBJK~OBKNR
    WHERE MBLNR EQ @MKPF-MBLNR
      AND MJAHR EQ @MKPF-MJAHR
    INTO TABLE @DATA(LT_SER).

  SELECT MSEG~MBLNR,
         MSEG~MJAHR,
         MSEG~ZEILE,
         MSEG~MATNR,
         MSEG~WERKS,
         MSEG~LGORT,
         MSEG~ERFMG,
         MSEG~ERFME,
         MSEG~MENGE,
         MSEG~MEINS,
         T001W~NAME1,
         MAKT~MAKTX
    FROM MSEG
    INNER JOIN MAKT ON MSEG~MATNR EQ MAKT~MATNR AND
                       MAKT~SPRAS EQ @SY-LANGU
    INNER JOIN T001W ON T001W~WERKS EQ MSEG~WERKS
    INTO TABLE @LT_MSEG
    WHERE MSEG~MBLNR EQ @MKPF-MBLNR
      AND MSEG~MJAHR EQ @MKPF-MJAHR
      AND MSEG~XAUTO EQ @SPACE.

  IF LT_MSEG IS NOT INITIAL.
    SELECT T001L~WERKS,
           T001L~LGORT,
           T001L~LGOBE
      FROM T001L
      INNER JOIN @LT_MSEG AS A ON T001L~WERKS EQ A~WERKS AND  ##needed
                                  T001L~LGORT EQ A~LGORT
      INTO TABLE @DATA(LT_LOCATION).
  ENDIF.

  DATA : LS_LOCATION LIKE LINE OF LT_LOCATION.

  LOOP AT LT_MSEG INTO LS_MSEG.
    LS_DATA-WERKS = LS_MSEG-WERKS .
    LS_DATA-PLNDT = LS_MSEG-NAME1.
    READ TABLE LT_LOCATION INTO LS_LOCATION
    WITH KEY WERKS = LS_MSEG-WERKS
             LGORT = LS_MSEG-LGORT.
    IF SY-SUBRC EQ 0.
      CONCATENATE LS_MSEG-LGORT LS_LOCATION-LGOBE INTO LS_DATA-LGORT SEPARATED BY SPACE.
    ENDIF.
    LS_DETIAL-DOCNO = LS_MSEG-MBLNR.
    LS_DETIAL-LINIT = LS_MSEG-ZEILE.
    LS_DETIAL-MATCD = LS_MSEG-MATNR.
    LS_DETIAL-MATDC = LS_MSEG-MAKTX.
    LS_DETIAL-BATCH = SPACE.
    LS_DETIAL-LGORT = LS_MSEG-LGORT.
    LS_DETIAL-PLACE = SPACE.
    LS_DETIAL-POINT = SPACE.
    LV_QTY          = LS_MSEG-ERFMG.
    WRITE LV_QTY TO LS_DETIAL-MENGE.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETIAL-MENGE WITH ''.
    LS_DETIAL-MEINS = LS_MSEG-ERFME.

    LOOP AT LT_SER INTO DATA(LS_SER) WHERE MBLNR EQ LS_MSEG-MBLNR
                                       AND MJAHR EQ LS_MSEG-MJAHR
                                       AND ZEILE EQ LS_MSEG-ZEILE.
      IF LS_DETIAL-SERIAL IS INITIAL.
        LS_DETIAL-SERIAL = LS_SER-SERNR.
      ELSE.
        CONCATENATE LS_DETIAL-SERIAL LS_SER-SERNR INTO LS_DETIAL-SERIAL SEPARATED BY SPACE.
      ENDIF.
    ENDLOOP.

    APPEND LS_DETIAL TO LS_DATA-T_DETAIL.
    CLEAR : LS_DETIAL.
  ENDLOOP.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LF_FORMNAME
*     variant            = ' '
*     direct_call        = ' '
    IMPORTING
      FM_NAME            = LF_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.

    PERFORM PROTOCOL_UPDATE.
  ENDIF.

  CALL FUNCTION LF_FM_NAME
    EXPORTING
      CONTROL_PARAMETERS = LS_CONTROL_PARAM
*     mail_appl_obj      =
      MAIL_RECIPIENT     = LS_RECIPIENT
      MAIL_SENDER        = LS_SENDER
      OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
      USER_SETTINGS      = SPACE
      GS_INIT_DATA       = LS_DATA
    IMPORTING
      JOB_OUTPUT_INFO    = LS_JOB_INFO
*     document_output_info =
*     job_output_options =
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
    PERFORM PROTOCOL_UPDATE.
  ELSE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROTOCOL_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROTOCOL_UPDATE .
  CHECK XSCREEN = SPACE.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      MSG_ARBGB = SYST-MSGID
      MSG_NR    = SYST-MSGNO
      MSG_TY    = SYST-MSGTY
      MSG_V1    = SYST-MSGV1
      MSG_V2    = SYST-MSGV2
      MSG_V3    = SYST-MSGV3
      MSG_V4    = SYST-MSGV4
    EXCEPTIONS
      OTHERS    = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form TAB001W_LESEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM TAB001W_LESEN .
  IF NOT T001W-WERKS = MSEG-WERKS.
    SELECT SINGLE * FROM T001W WHERE WERKS = MSEG-WERKS.
  ENDIF.
  R_WERKS = T001W-WERKS.
  R_NAME1 = T001W-NAME1.
* Sprache für Formular aus Kondition, sonst aus Werk
  IF NOT NAST-SPRAS IS INITIAL.
    LANGUAGE = NAST-SPRAS.
  ELSE.
    LANGUAGE = T001W-SPRAS.
  ENDIF.
  SET LANGUAGE LANGUAGE.
ENDFORM.
