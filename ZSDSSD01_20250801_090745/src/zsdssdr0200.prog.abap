*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0200
*  Creation Date      : 01.07.2024
*  Author             : Boontip R.(Eviden)
*  Add-on ID          : SDI036
*  Description        : Send Schedule line status to Salesforces
*  Purpose            : when create/delete/change DO send interface
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  10.03.2025  F36K912029  Boontip R.  Change checked field LIKP-LFDAT TO LIKP-WADAT
*                                      420000461(CH01)
*-----------------------------------------------------------------------
REPORT ZSDSSDR0200.
*-----------------------------------------------------------------------
* MACRO
*-----------------------------------------------------------------------
DEFINE %FCAT.
  <fcat>-scrtext_s = <fcat>-scrtext_m =
  <fcat>-scrtext_l = <fcat>-coltext =
  <fcat>-seltext   = <fcat>-tooltip =
  <fcat>-reptext   = &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* TYPES:
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TS_VBEP_UPD_ITEM,
    VBELN      TYPE VBEP-VBELN,
    POSNR      TYPE VBEP-POSNR,
    ETENR      TYPE VBEP-ETENR,
    FSH_PQR_RC TYPE VBEP-FSH_PQR_RC,
  END OF TS_VBEP_UPD_ITEM,
  TT_VBEP_UPD_ITEM TYPE STANDARD TABLE OF TS_VBEP_UPD_ITEM WITH DEFAULT KEY,
  BEGIN OF TS_VBEP_UPD,
    QT_VBELN TYPE VBAK-VBELN,
    ITEM     TYPE TT_VBEP_UPD_ITEM,
  END OF TS_VBEP_UPD,
  TT_VBEP_UPD TYPE STANDARD TABLE OF TS_VBEP_UPD,
  BEGIN OF TS_VBFA_SO_DO,
    SO_VBELN TYPE VBFA-VBELV,
    SO_POSNR TYPE VBFA-POSNV,
    SO_VBTYP TYPE VBFA-VBTYP_V,
    DO_VBELN TYPE VBFA-VBELN,
    DO_POSNR TYPE VBFA-POSNN,
    DO_VBTYP TYPE VBFA-VBTYP_N,
    RFMNG    TYPE VBFA-RFMNG,
    MEINS    TYPE VBFA-MEINS,
  END OF TS_VBFA_SO_DO,
  TT_VBFA_SO_DO TYPE SORTED TABLE OF TS_VBFA_SO_DO WITH NON-UNIQUE KEY SO_VBELN SO_POSNR DO_VBELN DO_POSNR,
  BEGIN OF TS_VBFA_QT_SO,
    QT_VBELN TYPE VBFA-VBELV,
    QT_POSNR TYPE VBFA-POSNV,
    QT_VBTYP TYPE VBFA-VBTYP_V,
    QT_BNAME TYPE VBAK-BNAME,
    SO_VBELN TYPE VBFA-VBELN,
    SO_POSNR TYPE VBFA-POSNN,
    SO_VBTYP TYPE VBFA-VBTYP_N,
  END OF TS_VBFA_QT_SO,
  TT_VBFA_QT_SO TYPE SORTED TABLE OF TS_VBFA_QT_SO WITH NON-UNIQUE KEY QT_VBELN QT_POSNR SO_VBELN SO_POSNR,
  BEGIN OF TS_LIKP,
    VBELN TYPE LIKP-VBELN,
*    LFDAT TYPE LIKP-LFDAT,  "CH01-
    WADAT TYPE LIKP-WADAT,   "CH01+
    DEL   TYPE FLAG,
  END OF TS_LIKP,
  TT_LIKP TYPE STANDARD TABLE OF TS_LIKP,
  BEGIN OF TS_VBEP,
    VBELN TYPE VBEP-VBELN,
    POSNR TYPE VBEP-POSNR,
    ETENR TYPE VBEP-ETENR,
    LMENG TYPE VBEP-LMENG,
    EDATU TYPE VBEP-EDATU,
    LIFSP TYPE VBEP-LIFSP,
  END OF TS_VBEP,
  TT_VBEP TYPE SORTED TABLE OF TS_VBEP WITH NON-UNIQUE KEY VBELN POSNR ETENR,
  BEGIN OF TS_REP,
    QT_VBELN     TYPE VBAK-VBELN,
    QT_POSNR     TYPE VBAP-POSNR,
    SO_VBELN     TYPE VBEP-VBELN,
    SO_POSNR     TYPE VBEP-POSNR,
    SO_ETENR     TYPE VBEP-ETENR,
    DO_VBELN     TYPE LIPS-VBELN,
    DO_POSNR     TYPE LIPS-POSNR,
    STATUS       TYPE CHAR1,
    REQNO        TYPE ZSDSCAT001-REQNO,
    RESP_STATUS  TYPE ZSDSDE_REST_STATUS,
    RESP_MESSAGE TYPE ZSDSDE_REST_MESSAGE,
    HTTP_CODE    TYPE ZSDSCAT001-HTTP_CODE,
    HTTP_REASON  TYPE ZSDSCAT001-HTTP_REASON,
  END OF TS_REP,
  TT_REP  TYPE STANDARD TABLE OF TS_REP,
  TT_DATA TYPE STANDARD TABLE OF ZSDSSDS041.
*----------------------------------------------------------------------*
* CONSTANTS:
*----------------------------------------------------------------------*
CONSTANTS:
  GC_VBTYP_QT    TYPE VBAK-VBTYP VALUE 'B',
  GC_VBTYP_SO    TYPE VBAK-VBTYP VALUE 'C',
  GC_VBTYP_DO    TYPE VBAK-VBTYP VALUE 'J',
  GC_STATUS_OPEN TYPE VBEP-FSH_PQR_RC VALUE 'O',
  GC_STATUS_DO   TYPE VBEP-FSH_PQR_RC VALUE 'D',
  GC_STATUS_CANC TYPE VBEP-FSH_PQR_RC VALUE 'C',
  GC_INTFNO      TYPE ZSDSDE_INTFNO VALUE 'SDI036',
  GC_ERROR       TYPE ZSDSDE_REST_STATUS VALUE 'E',
  GC_SUCCESS     TYPE ZSDSDE_REST_STATUS VALUE 'S',
  GC_REPID       TYPE SY-REPID      VALUE 'ZSDSSDR0200'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE I VALUE 0,
  GC_ALV_HEIGHT_1    TYPE I VALUE 100.
*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*
DATA:
   GF_KEYDATA_SUFFIX TYPE TEXT50 ##NEEDED.
*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
DATA: GRT_LFART TYPE RANGE OF LIKP-LFART ##NEEDED.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA: GT_DATA     TYPE TT_DATA     ##NEEDED,
      GT_VBEP_UPD TYPE TT_VBEP_UPD ##NEEDED,
      GT_REP      TYPE TT_REP      ##NEEDED.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.
  PARAMETERS P_VBELN TYPE LIKP-VBELN OBLIGATORY.
  PARAMETERS P_DEL TYPE C NO-DISPLAY.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS P_TEST AS CHECKBOX DEFAULT 'X'.
  PARAMETERS P_WAIT TYPE C NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.


*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_SELECT.
  PERFORM F_SELECT_AND_PREPARE_DATA CHANGING GT_DATA          ##PERF_GLOBAL_PAR_OK
                                             GT_VBEP_UPD
                                             GT_REP.
  IF GT_REP[] IS INITIAL.
    MESSAGE S001(ZSDSCA01) . "no data found
    LEAVE LIST-PROCESSING.
  ENDIF.
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF P_TEST = ABAP_FALSE.
    PERFORM F_SEND_INTF_AND_UPD_VBEP USING     GT_DATA
                                               GT_VBEP_UPD
                                               GT_REP.
  ENDIF.
  PERFORM F_DISPLAY_REPORT USING GT_REP.

  INCLUDE ZSDSCAI9990 ##INCL_OK.

*&---------------------------------------------------------------------*
*& Form F_SELECT_AND_PREPARE_DATA
*&---------------------------------------------------------------------*
FORM F_SELECT_AND_PREPARE_DATA  CHANGING CT_DATA TYPE TT_DATA
                                         CT_VBEP_UPD TYPE TT_VBEP_UPD
                                         CT_REP TYPE TT_REP.
  DATA: LT_VBFA_SO_DO       TYPE TT_VBFA_SO_DO,
        LT_VBFA_QT_SO       TYPE TT_VBFA_QT_SO,
        LT_LIKP             TYPE TT_LIKP,
        LT_VBEP             TYPE TT_VBEP,
        LS_DATA             LIKE LINE OF CT_DATA,
        LS_VBEP_UPD_ITEM    TYPE TS_VBEP_UPD_ITEM,
        LS_VBEP_UPD         TYPE TS_VBEP_UPD,
        LS_QUOTEITEM        TYPE ZSDSSDS042,
        LS_SCHEDULELINE     TYPE ZSDSSDS043,
        LS_REP              TYPE TS_REP,
        LF_FOUND            TYPE FLAG,
        LF_FOUND_VALIDATE   TYPE FLAG,
        LF_DO_TIME          TYPE I VALUE 10,
        LF_DEL_FLAG         TYPE FLAG,
        LF_MESSAGE          TYPE ZSDSDE_REST_MESSAGE,
        LF_MESSAGE_VALIDATE TYPE MSGTX,
        LF_KEYDATA          TYPE ZSDSDE_KEYDATA,
        LS_REQUEST_KEY      TYPE ZSDSCAS005.

  DO LF_DO_TIME TIMES.
    IF LF_FOUND_VALIDATE = ABAP_FALSE.
      PERFORM F_VALIDATE_CRITERIA CHANGING LF_FOUND_VALIDATE
                                           LF_MESSAGE_VALIDATE.
      IF LF_MESSAGE_VALIDATE IS NOT INITIAL.
        MESSAGE S000(ZSDSCA01) WITH LF_MESSAGE_VALIDATE.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
    IF LF_FOUND = ABAP_FALSE.
      PERFORM F_SELECT_DATA CHANGING LT_VBFA_SO_DO
                                     LT_VBFA_QT_SO
                                     LT_LIKP
                                     LT_VBEP
                                     LF_FOUND
                                     LF_DEL_FLAG
                                     LF_MESSAGE.
    ENDIF.
    IF ( LF_FOUND = ABAP_TRUE AND LF_FOUND_VALIDATE = ABAP_TRUE )
    OR P_WAIT = ABAP_FALSE.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
      IF SY-BATCH = ABAP_TRUE.
        MESSAGE S000(ZSDSCA01) WITH 'Wait #'(m01) SY-INDEX 'for selecting again'(m02) .
      ENDIF.
    ENDIF.
  ENDDO.

  IF LF_FOUND = ABAP_FALSE.
    IF LF_MESSAGE IS NOT INITIAL.
      LF_KEYDATA = P_VBELN && GF_KEYDATA_SUFFIX.
      CALL METHOD ZCL_SDSCA_REST_INTF_UTILITY=>ADD_LOG_REST_INTF
        EXPORTING
          IF_INTFNO          = GC_INTFNO
          IF_KEYDATA         = LF_KEYDATA
          IF_STATUS          = GC_ERROR
          IF_MESSAGE         = LF_MESSAGE
        IMPORTING
          ES_REQUEST_KEY     = LS_REQUEST_KEY
        EXCEPTIONS
          INVALID_STATUS     = 1
          INVALID_INTFNO     = 2
          ERROR_DATA_TO_JSON = 3
          REQNO_ERROR        = 4
          LOG_ERROR          = 5
          ERROR_JSON_TO_DATA = 6
          OTHERS             = 7.
      IF SY-SUBRC = 0.
        LS_REP-REQNO = LS_REQUEST_KEY-REQNO.
        LS_REP-RESP_STATUS  = GC_ERROR.
        LS_REP-RESP_MESSAGE = LF_MESSAGE.
        APPEND LS_REP TO GT_REP.
      ENDIF.
    ENDIF.
    RETURN.
  ENDIF.

  LOOP AT LT_VBFA_QT_SO ASSIGNING FIELD-SYMBOL(<L_VBFA_QT_SO>).
    AT NEW  QT_VBELN.
      CLEAR LS_DATA.
    ENDAT.
    LS_DATA-SAPQUOTATIONNO = <L_VBFA_QT_SO>-QT_VBELN.
    LS_DATA-SFQUOTATIONNO  = <L_VBFA_QT_SO>-QT_BNAME.

    CLEAR LS_QUOTEITEM.
    LS_QUOTEITEM-ITEMNO = <L_VBFA_QT_SO>-QT_POSNR.
    LOOP AT LT_VBEP INTO DATA(LS_VBEP) WHERE VBELN = <L_VBFA_QT_SO>-SO_VBELN
                                       AND   POSNR = <L_VBFA_QT_SO>-SO_POSNR.
      CLEAR LS_SCHEDULELINE.
      LS_SCHEDULELINE-SCHEDULELINENUMBER = LS_VBEP-ETENR.
      LS_SCHEDULELINE-SONUMBER = LS_VBEP-VBELN.
      LS_SCHEDULELINE-SOITEM   = LS_VBEP-POSNR.
      CLEAR LF_FOUND.
      IF LS_VBEP-LIFSP IS NOT INITIAL. "block
        LS_SCHEDULELINE-STATUS = GC_STATUS_OPEN.
      ELSE.
*     === compare schedule line and DO data with
*       1. do      ( from doc from so->do)
*       2. do item ( from doc from so->do)
*       3. qty  ( vbfa-rfmng = vbep-lmeng )
*       4. date ( likp-lfdat = vbep-edatu )
        LOOP AT LT_VBFA_SO_DO INTO DATA(LS_VBFA_SO_DO) WHERE SO_VBELN = <L_VBFA_QT_SO>-SO_VBELN
                                                       AND   SO_POSNR = <L_VBFA_QT_SO>-SO_POSNR
                                                       AND   RFMNG = LS_VBEP-LMENG.

          "---find LFDAT---
          READ TABLE LT_LIKP INTO DATA(LS_LIKP) WITH KEY VBELN = LS_VBFA_SO_DO-DO_VBELN
                                                         BINARY SEARCH.

          IF SY-SUBRC = 0.

            "--- commpare likp-lfdat = vbep-edatu
*            IF LS_LIKP-LFDAT = LS_VBEP-EDATU. "CH01-
             IF LS_LIKP-WADAT = LS_VBEP-EDATU. "CH01+
              LF_FOUND = ABAP_TRUE.

              LS_SCHEDULELINE-DONUMBER = LS_VBFA_SO_DO-DO_VBELN.
              LS_SCHEDULELINE-DOITEM   = LS_VBFA_SO_DO-DO_POSNR.

              IF LS_LIKP-DEL = ABAP_TRUE.
                LS_SCHEDULELINE-STATUS = GC_STATUS_CANC.
              ELSE.
                LS_SCHEDULELINE-STATUS = GC_STATUS_DO.
              ENDIF.

              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF LF_FOUND = ABAP_FALSE.
          CLEAR LS_VBFA_SO_DO.
          LS_SCHEDULELINE-STATUS = GC_STATUS_OPEN.
        ENDIF.
      ENDIF.

      "====== update vbep ( GT_VBEP_UPD)
      CLEAR LS_VBEP_UPD_ITEM.
      LS_VBEP_UPD_ITEM-VBELN = LS_VBEP-VBELN.
      LS_VBEP_UPD_ITEM-POSNR = LS_VBEP-POSNR.
      LS_VBEP_UPD_ITEM-ETENR = LS_VBEP-ETENR.
      LS_VBEP_UPD_ITEM-FSH_PQR_RC = LS_SCHEDULELINE-STATUS.
      APPEND LS_VBEP_UPD_ITEM TO LS_VBEP_UPD-ITEM.

      "====== report ( GT_REP )
      LS_REP-QT_VBELN  = <L_VBFA_QT_SO>-QT_VBELN.
      LS_REP-QT_POSNR  = <L_VBFA_QT_SO>-QT_POSNR.
      LS_REP-SO_VBELN  = <L_VBFA_QT_SO>-SO_VBELN.
      LS_REP-SO_POSNR  = <L_VBFA_QT_SO>-SO_POSNR.
      LS_REP-SO_ETENR  = LS_VBEP-ETENR.
      LS_REP-DO_VBELN  = LS_VBFA_SO_DO-DO_VBELN.
      LS_REP-DO_POSNR  = LS_VBFA_SO_DO-DO_POSNR.
      LS_REP-STATUS = LS_SCHEDULELINE-STATUS.
      APPEND LS_REP TO CT_REP.

      APPEND  LS_SCHEDULELINE TO  LS_QUOTEITEM-SCHEDULELINE.
    ENDLOOP.

    APPEND LS_QUOTEITEM TO LS_DATA-QUOTEITEM.
    AT END OF QT_VBELN.
      APPEND LS_DATA TO CT_DATA.
      APPEND LS_VBEP_UPD TO CT_VBEP_UPD.
    ENDAT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_SO_DO
*&---------------------------------------------------------------------*
FORM F_SELECT_SO_DO  CHANGING CT_VBFA_SO_DO TYPE TT_VBFA_SO_DO
                              CF_DEL_FLAG TYPE FLAG.

*--- Select SO-DO of current DO

  IF P_DEL = ABAP_FALSE.
    SELECT VBELV,  "SO
           POSNV,
           VBTYP_V,
           VBELN,   "DO
           POSNN,
           VBTYP_N,
           RFMNG,
           MEINS
    INTO TABLE @CT_VBFA_SO_DO
    FROM VBFA
    WHERE VBELN = @P_VBELN "DO
    AND   VBTYP_N = @GC_VBTYP_DO
    AND   VBTYP_V = @GC_VBTYP_SO.
  ENDIF.
  IF P_DEL = ABAP_TRUE
  OR CT_VBFA_SO_DO IS INITIAL.
    SELECT VBELV,  "SO
           POSNV,
           VBTYP_V,
           VBELN,   "DO
           POSNN,
           VBTYP_N,
           RFMNG,
           MEINS
    INTO TABLE @CT_VBFA_SO_DO
    FROM ZSDSSDT012  "VBFA Deleted data
    WHERE VBELN = @P_VBELN "DO
    AND   VBTYP_N = @GC_VBTYP_DO
    AND   VBTYP_V = @GC_VBTYP_SO.
    IF SY-SUBRC = 0.
      CF_DEL_FLAG = ABAP_TRUE.
    ENDIF.
  ENDIF.

  IF CT_VBFA_SO_DO[] IS INITIAL.
    RETURN.
  ENDIF.
  "Select SO-DO of all DO related
  SELECT VBELV,                                    "#EC CI_NO_TRANSFORM
         POSNV,
         VBTYP_V,
         VBELN,
         POSNN,
         VBTYP_N,
         RFMNG,
         MEINS
  INTO TABLE @DATA(LT_VBFA_SO_DO)
  FROM VBFA
  FOR ALL ENTRIES IN @CT_VBFA_SO_DO
  WHERE VBELV = @CT_VBFA_SO_DO-SO_VBELN
  AND   VBTYP_N = @GC_VBTYP_DO
  AND   VBTYP_V = @GC_VBTYP_SO.

  IF  CF_DEL_FLAG = ABAP_TRUE.
    INSERT LINES OF LT_VBFA_SO_DO INTO TABLE CT_VBFA_SO_DO.
  ELSE.
    CT_VBFA_SO_DO = LT_VBFA_SO_DO .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_QT_SO
*&---------------------------------------------------------------------*
FORM F_SELECT_QT_SO  USING    UT_VBFA_SO_DO  TYPE TT_VBFA_SO_DO
                     CHANGING CT_VBFA_QT_SO TYPE TT_VBFA_QT_SO.
  IF UT_VBFA_SO_DO[] IS NOT INITIAL.
    SELECT VBFA~VBELV,                             "#EC CI_NO_TRANSFORM
           VBFA~POSNV,
           VBFA~VBTYP_V,
           VBAK~BNAME,
           VBFA~VBELN,
           VBFA~POSNN,
           VBFA~VBTYP_N
    INTO TABLE @CT_VBFA_QT_SO
    FROM VBFA INNER JOIN VBAK
    ON VBFA~VBELV = VBAK~VBELN
    FOR ALL ENTRIES IN  @UT_VBFA_SO_DO
    WHERE VBFA~VBELN = @UT_VBFA_SO_DO-SO_VBELN
    AND   VBFA~VBTYP_V = @GC_VBTYP_QT
    AND   VBFA~VBTYP_N = @GC_VBTYP_SO
    AND   VBFA~POSNV <> 0 .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_do
*&---------------------------------------------------------------------*
FORM F_SELECT_DO  USING CT_VBFA_SO_DO TYPE TT_VBFA_SO_DO
                        CF_DEL_FLAG TYPE FLAG
                  CHANGING CT_LIKP TYPE TT_LIKP.
  IF CT_VBFA_SO_DO IS NOT INITIAL.
    SELECT VBELN ,                                 "#EC CI_NO_TRANSFORM
*           LFDAT "CH01-
           WADAT  "CH01+
    INTO TABLE @CT_LIKP
    FROM LIKP
    FOR ALL ENTRIES IN @CT_VBFA_SO_DO
    WHERE VBELN = @CT_VBFA_SO_DO-DO_VBELN ##TOO_MANY_ITAB_FIELDS.
  ENDIF.
  IF CF_DEL_FLAG = ABAP_TRUE.
    SELECT VBELN,
*           LFDAT,"CH01-
           WADAT, "CH01+
           'X'
    APPENDING TABLE @CT_LIKP
    FROM ZSDSSDT011
    WHERE VBELN = @P_VBELN.
    IF SY-SUBRC <> 0.
      CLEAR CT_LIKP. " mean not found -> for trying to select again
    ENDIF.
  ENDIF.
  SORT CT_LIKP BY VBELN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_VBEP
*&---------------------------------------------------------------------*
FORM F_SELECT_VBEP  USING    CT_VBFA_SO_DO TYPE TT_VBFA_SO_DO
                    CHANGING CT_VBEP TYPE TT_VBEP.
  IF CT_VBFA_SO_DO IS NOT INITIAL.
    SELECT VBELN,                                  "#EC CI_NO_TRANSFORM
           POSNR,
           ETENR,
           LMENG,
           EDATU,
           LIFSP
    INTO TABLE @CT_VBEP
    FROM VBEP
    FOR ALL ENTRIES IN @CT_VBFA_SO_DO
    WHERE VBELN = @CT_VBFA_SO_DO-SO_VBELN
    AND   LIFSP = ''.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_send_intf_and_upd_vbep
*&---------------------------------------------------------------------*
FORM F_SEND_INTF_AND_UPD_VBEP  USING CT_DATA TYPE TT_DATA
                                     CT_VBEP_UPD TYPE TT_VBEP_UPD
                                     CT_REP TYPE TT_REP.
  DATA: LF_KEYDATA  TYPE ZSDSDE_KEYDATA,
        LS_REQUEST  TYPE ZSDSSDS041,
        LS_RESPONSE TYPE ZSDSSDS041,
        LS_REP      TYPE TS_REP,
        LS_REQ_KEY  TYPE ZSDSCAS005.

  LOOP AT CT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    CLEAR LS_REP.
    LF_KEYDATA = P_VBELN && GF_KEYDATA_SUFFIX.
    LS_REQUEST = <L_DATA>.
    ZCL_SDSCA_REST_INTF_UTILITY=>CALL_REST_INTF(
      EXPORTING
        IF_INTFNO           = GC_INTFNO
        IS_REQUEST          = LS_REQUEST
        IF_KEYDATA          = LF_KEYDATA
        IF_SHOW_PROCESS_LOG = ''
      IMPORTING
        ES_RESPONSE         = LS_RESPONSE
        ES_REQUEST_KEY      = LS_REQ_KEY
      EXCEPTIONS
        INVALID_INTFNO      = 1
        ERROR_DATA_TO_JSON  = 2
        URL_ERROR           = 3
        SEND_ERROR          = 4
        REQNO_ERROR         = 5
        LOG_ERROR           = 6
        ERROR_JSON_TO_DATA  = 7
        OTHERS              = 8 ).
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY
              NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2
                   SY-MSGV3 SY-MSGV4
      INTO LS_REP-RESP_MESSAGE.
      LS_REP-RESP_STATUS = GC_ERROR.
    ELSE.
      SELECT SINGLE REQNO,
                    HTTP_CODE,
                    HTTP_REASON
      FROM ZSDSCAT001
      WHERE INTFNO = @LS_REQ_KEY-INTFNO
      AND   REQNO  = @LS_REQ_KEY-REQNO
      AND   GJAHR  = @LS_REQ_KEY-GJAHR
      INTO  ( @LS_REP-REQNO,
              @LS_REP-HTTP_CODE,
              @LS_REP-HTTP_REASON ).

      LS_REP-RESP_STATUS = LS_RESPONSE-RESP_STATUS.
      LS_REP-RESP_MESSAGE = LS_RESPONSE-RESP_MESSAGE.
    ENDIF.
    MODIFY CT_REP FROM LS_REP TRANSPORTING REQNO RESP_STATUS RESP_MESSAGE HTTP_CODE HTTP_REASON
                  WHERE QT_VBELN = <L_DATA>-SAPQUOTATIONNO.
    IF LS_RESPONSE-RESP_STATUS = GC_SUCCESS.
      READ TABLE CT_VBEP_UPD INTO DATA(LS_VBEP_UPD) WITH KEY QT_VBELN = <L_DATA>-SAPQUOTATIONNO.
      IF SY-SUBRC = 0.
        PERFORM F_UPDATE_VBEP USING  LS_VBEP_UPD-ITEM.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_vbep
*&---------------------------------------------------------------------*
FORM F_UPDATE_VBEP  USING   UT_VBEP_UPD TYPE TT_VBEP_UPD_ITEM.
  LOOP AT UT_VBEP_UPD INTO DATA(LS_VBEP_UPD).
    UPDATE VBEP
    SET FSH_PQR_RC = LS_VBEP_UPD-FSH_PQR_RC
    WHERE VBELN = LS_VBEP_UPD-VBELN
      AND POSNR = LS_VBEP_UPD-POSNR
      AND ETENR = LS_VBEP_UPD-ETENR.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_REPORT  USING  UT_REP TYPE TT_REP.
* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = SPACE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_REP TO <G_LIST_1>.               "#EC CI_FLDEXT_OK[2610650]

* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT  USING UT_REP
                                CHANGING GT_FIELDCAT_1.

* Call ALV Screen
  CALL SCREEN 9000.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR: CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'A'.
  CS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  CS_LAYOUT-ZEBRA      = ABAP_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = SPACE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT  USING UT_DATA TYPE  TT_REP
                           CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.
  PERFORM F_ALV_GEN_FIELDCAT    USING UT_DATA
                                  CHANGING CT_FIELDCAT.

  PERFORM F_ALV_MODIFY_FC   CHANGING CT_FIELDCAT.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_GEN_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_GEN_FIELDCAT USING UT_DATA TYPE ANY TABLE
                       CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  DATA: LF_COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
        LF_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
        LF_SALV_TABLE   TYPE REF TO CL_SALV_TABLE,
        LF_TABLE        TYPE REF TO DATA.

  FIELD-SYMBOLS: <TABLE> TYPE ANY TABLE.

* create unprotected table from import data
  CREATE DATA LF_TABLE LIKE  UT_DATA.
  ASSIGN LF_TABLE->* TO <TABLE>.

*...New ALV Instance ...............................................
  TRY.
      CL_SALV_TABLE=>FACTORY(
        EXPORTING
          LIST_DISPLAY = ABAP_FALSE
        IMPORTING
          R_SALV_TABLE = LF_SALV_TABLE
        CHANGING
          T_TABLE      = <TABLE> ).
    CATCH CX_SALV_MSG ##NO_HANDLER.
  ENDTRY.
  LF_COLUMNS  = LF_SALV_TABLE->GET_COLUMNS( ).
  LF_AGGREGATIONS = LF_SALV_TABLE->GET_AGGREGATIONS( ).

  CALL METHOD CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG
    EXPORTING
      R_COLUMNS      = LF_COLUMNS
      R_AGGREGATIONS = LF_AGGREGATIONS
    RECEIVING
      T_FIELDCATALOG = CT_FIELDCAT[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_NEW
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'QT_VBELN'.
        %FCAT 'Quot no.'(c01).
      WHEN 'QT_POSNR'.
        %FCAT 'Quot item'(c02).
      WHEN 'SO_VBELN'.
        %FCAT 'SO No.'(c03).
      WHEN 'SO_POSNR'.
        %FCAT 'SO Item'(c04).
      WHEN 'SO_ETENR'.
        %FCAT 'Sched.Line'(c05).
      WHEN 'DO_VBELN'.
        %FCAT 'DO No.'(c06).
      WHEN 'DO_POSNR'.
        %FCAT 'DO Item'(c07).
      WHEN 'STATUS'.
        %FCAT 'Sched. Stat'(c08).
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_data
*&---------------------------------------------------------------------*
FORM F_SELECT_DATA  CHANGING CT_VBFA_SO_DO TYPE TT_VBFA_SO_DO
                             CT_VBFA_QT_SO TYPE TT_VBFA_QT_SO
                             CT_LIKP TYPE TT_LIKP
                             CT_VBEP TYPE TT_VBEP
                             CV_FOUND TYPE FLAG
                             CF_DEL_FLAG TYPE FLAG
                             CF_MESSAGE TYPE ZSDSDE_REST_MESSAGE.
  CLEAR:  CT_VBFA_SO_DO,
          CT_VBFA_QT_SO ,
          CT_LIKP ,
          CT_VBEP,
          CV_FOUND.
  PERFORM F_SELECT_SO_DO CHANGING CT_VBFA_SO_DO
                                  CF_DEL_FLAG.
  IF CT_VBFA_SO_DO IS INITIAL.
    CF_MESSAGE = 'Not found SO related'(m10).
    RETURN.
  ENDIF.
  PERFORM F_SELECT_QT_SO USING CT_VBFA_SO_DO
                         CHANGING CT_VBFA_QT_SO.
  IF CT_VBFA_QT_SO IS INITIAL.
    CF_MESSAGE = 'Not found quotation related'(m11).
    RETURN.
  ENDIF.
  PERFORM F_SELECT_DO  USING CT_VBFA_SO_DO
                             CF_DEL_FLAG
                       CHANGING CT_LIKP.
  IF CT_LIKP[] IS INITIAL.
    CF_MESSAGE = 'Not found DO'(m12).
    RETURN.
  ENDIF.
  PERFORM F_SELECT_VBEP USING CT_VBFA_SO_DO
                        CHANGING CT_VBEP.
  IF CT_VBEP[] IS  INITIAL.
    CF_MESSAGE = 'Not found Schedule line'(m13).
    RETURN.
  ENDIF.
  CV_FOUND = ABAP_TRUE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_CRITERIA
*&---------------------------------------------------------------------*
FORM F_VALIDATE_CRITERIA  CHANGING CF_FOUND_VALIDATE TYPE FLAG
                                   CF_MESSAGE_VALIDATE TYPE MSGTX.

  SELECT SINGLE
         VBELN ,
         LFART
  INTO @DATA(LS_LIKP)
  FROM LIKP
  WHERE VBELN = @P_VBELN .

  IF SY-SUBRC <> 0
  OR P_DEL = ABAP_TRUE.
    SELECT SINGLE
           VBELN ,
           LFART
    INTO @LS_LIKP
    FROM ZSDSSDT011
    WHERE VBELN = @P_VBELN .
  ENDIF.

  IF LS_LIKP IS INITIAL.
    RETURN.
  ENDIF.
  IF LS_LIKP-LFART NOT IN GRT_LFART.
    CF_MESSAGE_VALIDATE = 'Delivery Type is not in scope'(m14).
  ENDIF.


  CF_FOUND_VALIDATE  = ABAP_TRUE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_select
*&---------------------------------------------------------------------*
FORM F_INIT_SELECT .
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
    EXPORTING
      IF_REPID = GC_REPID
      IF_PARAM = 'DELIVERY_TYPE'
    IMPORTING
      ET_RANGE = GRT_LFART.

  IF P_WAIT = ABAP_FALSE. "REPROCESS (NOT FROM SUBMITTED)
    GF_KEYDATA_SUFFIX = '|REPROCESS'.
  ENDIF.
ENDFORM.
