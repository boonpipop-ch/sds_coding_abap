*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0750_CLASS
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
      SELECT_KNA1              IMPORTING I_DATA TYPE   TT_CUST_KEY
                               CHANGING  C_DATA TYPE   TT_KNA1,
      SELECT_KNA1_027          IMPORTING I_DATA TYPE   TT_CUST_KEY
                               CHANGING  C_DATA TYPE   TT_KNA1_027,
      SELECT_KNVV_027          IMPORTING I_DATA TYPE   TT_CUST_KEY
                               CHANGING  C_DATA TYPE   TT_KNVV_027,
      SELECT_KNB1              IMPORTING I_DATA TYPE   TT_CUST_KEY
                               CHANGING  C_DATA TYPE   TT_KNB1,
      SELECT_FIDOC             CHANGING  C_DATA TYPE   TT_FIDOC,
      SELECT_BSEC              IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_BSEC,
      SELECT_CLEAR_DOC         IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_CLEAR_DOC,
      SELECT_CLEAR_MIGRATE_DOC IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_CLEAR_MGRTE,
      SELECT_PA0002_PIC        IMPORTING I_KNA1_027 TYPE   TT_KNA1_027
                                         I_KNVV_027 TYPE   TT_KNVV_027
                               CHANGING  C_DATA     TYPE   TT_PA0002_PIC,
      SELECT_PRPS              IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_PRPS,
      SELECT_SO_DATA           IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_VBKD_SO,
      SELECT_VBFA_INV_DO       IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_VBFA_INV_DO,
      SELECT_ZSDSFIT040        IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_040,
      SELECT_ZSDSFIC024        IMPORTING I_DATA TYPE   TT_CUST_KEY
                               CHANGING  C_DATA TYPE   TT_024,
      SELECT_ZSDSFIT033_035    IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_BILLPL,
      SELECT_ZSDSFIT029        IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_BILLPL,
      SELECT_ZSDSFIT038        IMPORTING I_DATA TYPE   TT_FIDOC
                               CHANGING  C_DATA TYPE   TT_BILLPL,
      SELECT_GL_DATA           IMPORTING I_DATA  TYPE  TT_FIDOC
                               CHANGING  C_SKAT  TYPE  TT_SKAT
                                         C_T074T TYPE  TT_T074T,
      GET_PO_FROM_SV_ORDER     IMPORTING I_VBELN TYPE VBRP-VBELN
                               CHANGING  C_BSTKD TYPE GY_RESULT-BSTKD,
      READ_TEXT                IMPORTING I_ID     TYPE ANY
                                         I_NAME   TYPE ANY
                                         I_OBJECT TYPE ANY
                               CHANGING  C_TEXT   TYPE ANY,
      NET_DUE_DATE_GET         IMPORTING I_ZFBDT TYPE BSID_VIEW-ZFBDT
                                         I_ZBD1T TYPE BSID_VIEW-ZBD1T
                                         I_ZBD2T TYPE BSID_VIEW-ZBD2T
                                         I_ZBD3T TYPE BSID_VIEW-ZBD3T
                                         I_SHKZG TYPE BSID_VIEW-SHKZG
                                         I_REBZG TYPE BSID_VIEW-REBZG
                               CHANGING  C_DUEDT TYPE DATUM,
    SET_DAY_RANGE              IMPORTING I_DAY   TYPE  I
                               CHANGING  C_DAY_RANGE  TYPE  TEXT20,
    SET_WITHIN_OR_OVER         IMPORTING I_DAY   TYPE  I
                               CHANGING  C_WITHIN_OR_OVER TYPE  TEXT20.





    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    SELECT SINGLE
             BUKRS,
             WAERS,
             KTOPL
      INTO @GS_T001
      FROM T001
      WHERE BUKRS IN @S_BUKRS ##WARN_OK.


  ENDMETHOD.
  METHOD GET_DATA.
    IF LO IS INITIAL.
      CREATE OBJECT LO.
    ENDIF.

    LO->START_PROCESS( ).
  ENDMETHOD.
  METHOD START_PROCESS.
    DATA : LT_CUST_KEY TYPE TT_CUST_KEY.

    DATA:
      LT_KNA1            TYPE TT_KNA1,
      LT_KNA1_027        TYPE TT_KNA1_027,
      LT_KNVV_027        TYPE TT_KNVV_027,
      LT_KNB1            TYPE TT_KNB1,
      LT_FIDOC           TYPE TT_FIDOC,
      LT_BSEC            TYPE TT_BSEC,
      LT_CLEAR_DOC       TYPE TT_CLEAR_DOC,
      LT_CLEAR_CHK       TYPE TT_CLEAR_DOC,
      LT_CLEAR_MGRTE     TYPE TT_CLEAR_MGRTE,
      LT_CLEAR_MGRTE_CHK TYPE TT_CLEAR_MGRTE_CHK,
      LT_PA0002_PIC      TYPE TT_PA0002_PIC,
      LT_PRPS            TYPE TT_PRPS,
      LT_VBKD_SO         TYPE TT_VBKD_SO,
      LT_VBFA_INV_DO     TYPE TT_VBFA_INV_DO,
      LT_040             TYPE TT_040,
      LT_024             TYPE TT_024,
      LT_033_035         TYPE TT_BILLPL,
      LT_029             TYPE TT_BILLPL,
      LT_038             TYPE TT_BILLPL,
      LT_SKAT            TYPE TT_SKAT,
      LT_T074T           TYPE TT_T074T,
      LT_VBPA            TYPE TT_VBPA,
      LT_PA0002          TYPE TT_PA0002_PIC,
      LF_COUNT_XBLNR     TYPE I,
      LF_DMBTR_CLR       TYPE BSID_VIEW-DMBTR,
      LF_TDNAME          TYPE THEAD-TDNAME,
      LT_TVGRT           TYPE TT_TVGRT,
      LT_TVKBT           TYPE TT_TVKBT,
      LF_INDX             TYPE SY-TABIX.


    DATA LS_FIDOC LIKE LINE OF LT_FIDOC.




    SELECT_FIDOC( CHANGING  C_DATA = LT_FIDOC ).

    CHECK LT_FIDOC IS NOT INITIAL.

    LT_CUST_KEY =  CORRESPONDING #( LT_FIDOC  DISCARDING DUPLICATES ).

    SELECT_KNA1( EXPORTING I_DATA = LT_CUST_KEY
                  CHANGING C_DATA = LT_KNA1 ).

    SELECT_KNA1_027( EXPORTING I_DATA = LT_CUST_KEY
                      CHANGING C_DATA = LT_KNA1_027 ).
    SELECT_KNVV_027( EXPORTING I_DATA = LT_CUST_KEY
                      CHANGING C_DATA = LT_KNVV_027 ).
    SELECT_KNB1( EXPORTING I_DATA = LT_CUST_KEY
                      CHANGING C_DATA = LT_KNB1 ).
    SELECT_BSEC( EXPORTING I_DATA = LT_FIDOC
                      CHANGING C_DATA = LT_BSEC ).
    SELECT_CLEAR_DOC( EXPORTING I_DATA = LT_FIDOC
                      CHANGING C_DATA = LT_CLEAR_DOC ).

    LT_CLEAR_CHK = LT_CLEAR_DOC.

    SELECT_CLEAR_MIGRATE_DOC( EXPORTING I_DATA =  LT_FIDOC
                               CHANGING C_DATA =  LT_CLEAR_MGRTE ) .
    LT_CLEAR_MGRTE_CHK = LT_CLEAR_MGRTE.
    SELECT_PA0002_PIC( EXPORTING I_KNA1_027 =  LT_KNA1_027
                                 I_KNVV_027 =  LT_KNVV_027
                               CHANGING C_DATA = LT_PA0002_PIC ) .
    SELECT_PRPS( EXPORTING I_DATA =  LT_FIDOC
                              CHANGING C_DATA = LT_PRPS ) .
    SELECT_SO_DATA( EXPORTING I_DATA =  LT_FIDOC
                           CHANGING C_DATA = LT_VBKD_SO ) .
    SELECT_VBFA_INV_DO( EXPORTING I_DATA =  LT_FIDOC
                        CHANGING C_DATA = LT_VBFA_INV_DO ) .
    SELECT_ZSDSFIT040( EXPORTING I_DATA =  LT_FIDOC
                        CHANGING C_DATA = LT_040 ) .
    SELECT_ZSDSFIC024( EXPORTING I_DATA =  LT_CUST_KEY
                        CHANGING C_DATA = LT_024 ) .
    SELECT_ZSDSFIT033_035( EXPORTING I_DATA =  LT_FIDOC
                         CHANGING C_DATA = LT_033_035 ) .
    SELECT_ZSDSFIT029( EXPORTING I_DATA =  LT_FIDOC
                         CHANGING C_DATA = LT_029 ) .
    SELECT_ZSDSFIT038( EXPORTING I_DATA =  LT_FIDOC
                         CHANGING C_DATA = LT_038 ).
    SELECT_GL_DATA( EXPORTING I_DATA =  LT_FIDOC
                         CHANGING C_SKAT = LT_SKAT
                                  C_T074T = LT_T074T ).

    LOOP AT LT_FIDOC INTO LS_FIDOC.

      READ TABLE LT_CLEAR_CHK TRANSPORTING NO FIELDS
                                  WITH  KEY BUKRS = LS_FIDOC-BUKRS
                                            BELNR = LS_FIDOC-BELNR
                                            GJAHR = LS_FIDOC-GJAHR
                                            BUZEI = LS_FIDOC-BUZEI.
      IF SY-SUBRC = 0.
        CONTINUE.
      ENDIF.
*--------------------------------------------------------------------*
      IF LS_FIDOC-BLART = GC_BLART_MIGRATE
        AND LS_FIDOC-XREF1 <> LS_FIDOC-XBLNR .
        READ TABLE LT_CLEAR_MGRTE_CHK INTO DATA(LS_MGRTE_CHK) WITH KEY
                                                               BUKRS = LS_FIDOC-BUKRS
                                                               BELNR = LS_FIDOC-BELNR
                                                               GJAHR = LS_FIDOC-GJAHR
                                                               BUZEI = LS_FIDOC-BUZEI.
        IF SY-SUBRC = 0.
          LF_COUNT_XBLNR = REDUCE I( INIT LF_I = 0 FOR LS_FIDOC1 IN LT_FIDOC WHERE ( XBLNR = LS_MGRTE_CHK-XREF1 ) NEXT LF_I = LF_I + 1 ). "#EC CI_SORTSEQ  "CH06+
          IF LF_COUNT_XBLNR > 1.
            DELETE LT_CLEAR_MGRTE WHERE BUKRS = LS_FIDOC-BUKRS
                                  AND   XREF1 = LS_MGRTE_CHK-XREF1.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.

      ENDIF.
*--------------------------------------------------------------------*
      CLEAR: GS_RESULT,
            LF_DMBTR_CLR.
*  --- CLERK
      READ TABLE LT_KNB1 INTO DATA(LS_KNB1) WITH KEY KUNNR = LS_FIDOC-KUNNR
                                                     BUKRS = LS_FIDOC-BUKRS.
      IF SY-SUBRC = 0.
        GS_RESULT-BUSAB = LS_KNB1-BUSAB.
      ELSE.
        IF S_BUSAB[] IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
*--------------------------------------------------------------------*
      READ TABLE LT_VBPA INTO DATA(LS_VBPA) WITH KEY VBELN = LS_FIDOC-VBELN.
      IF SY-SUBRC = 0.
        GS_RESULT-SALEMAN = LS_VBPA-PERNR.
        READ TABLE LT_PA0002 INTO DATA(LS_PA0002) WITH KEY PERNR = LS_VBPA-PERNR.
        IF SY-SUBRC = 0.
          GS_RESULT-SALEMAN_NAME = |{ LS_PA0002-VORNA } { LS_PA0002-NACHN }|.
        ENDIF.
      ELSE.
        IF S_SALEM[] IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
*--------------------------------------------------------------------*
      IF LS_FIDOC-VBELN IS INITIAL.
        READ TABLE LT_KNA1_027 INTO DATA(LS_KNA1_027) WITH KEY KUNNR = LS_FIDOC-KUNNR
                                                               BUSAB = GS_RESULT-BUSAB.
        IF SY-SUBRC  = 0.
          GS_RESULT-PIC_PERNR = LS_KNA1_027-PIC_PERNR.
        ELSE.
          IF S_PIC[] IS NOT INITIAL.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE LT_KNVV_027 INTO DATA(LS_KNVV_027) WITH KEY KUNNR = LS_FIDOC-KUNNR
                                                               VKORG = LS_FIDOC-VKORG
                                                               VTWEG = LS_FIDOC-VTWEG
                                                               SPART = LS_FIDOC-SPART
                                                               VKBUR = LS_FIDOC-VKBUR
                                                               VKGRP = LS_FIDOC-VKGRP
                                                               BUSAB = GS_RESULT-BUSAB.
        IF SY-SUBRC  = 0.
          GS_RESULT-PIC_PERNR = LS_KNVV_027-PIC_PERNR.
        ELSE.
          IF S_PIC[] IS NOT INITIAL.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
*--------------------------------------------------------------------*
      IF GS_RESULT-PIC_PERNR IS NOT INITIAL.
        READ TABLE LT_PA0002_PIC INTO LS_PA0002 WITH KEY PERNR = GS_RESULT-PIC_PERNR.
        IF SY-SUBRC = 0.
          GS_RESULT-PIC_NAME1 = |{ LS_PA0002-VORNA } { LS_PA0002-NACHN }|.
        ENDIF.
      ENDIF.

      GS_RESULT-VBELN_BILL = LS_FIDOC-VBELN.
      GS_RESULT-BUKRS      = LS_FIDOC-BUKRS.
      GS_RESULT-BELNR      = LS_FIDOC-BELNR.
      GS_RESULT-GJAHR      = LS_FIDOC-GJAHR.
      GS_RESULT-BUZEI      = LS_FIDOC-BUZEI.
      GS_RESULT-XBLNR      = LS_FIDOC-XBLNR.
      GS_RESULT-XREF1      = LS_FIDOC-XREF1. "CH04 420000219+
      GS_RESULT-KUNNR      = LS_FIDOC-KUNNR.
*--------------------------------------------------------------------*
      READ TABLE LT_KNA1 INTO DATA(LS_KNA1) WITH KEY KUNNR = LS_FIDOC-KUNNR.
      IF SY-SUBRC = 0.
        GS_RESULT-KUNNR_NAME1    = LS_KNA1-NAME1.
        IF LS_KNA1-XCPDK = ABAP_TRUE.
          READ TABLE LT_BSEC INTO DATA(LS_BSEC) WITH KEY BUKRS = LS_FIDOC-BUKRS
                                                         BELNR = LS_FIDOC-BELNR
                                                         GJAHR = LS_FIDOC-GJAHR
                                                         BUZEI = LS_FIDOC-BUZEI.
          IF SY-SUBRC = 0.
            GS_RESULT-KUNNR_NAME1 = LS_BSEC-NAME1.
          ENDIF.
        ENDIF.

      ENDIF.

      GS_RESULT-SGTXT          = LS_FIDOC-SGTXT.
*--------------------------------------------------------------------*
      READ TABLE LT_PRPS INTO DATA(LS_PRPS) WITH KEY POSID = LS_FIDOC-SGTXT ##WARN_OK.
      IF SY-SUBRC = 0.
        GS_RESULT-POST1 = LS_PRPS-POST1.
      ELSE.
        CLEAR GS_RESULT-SGTXT.
      ENDIF.

*--------------------------------------------------------------------*
      READ TABLE LT_VBKD_SO INTO DATA(LS_VBKD_SO) WITH KEY VBELN = LS_FIDOC-AUBEL.
      IF SY-SUBRC = 0.
        GS_RESULT-BSTKD = LS_VBKD_SO-BSTKD.
*BOI CH03
      ELSE.
        GET_PO_FROM_SV_ORDER( EXPORTING I_VBELN = LS_FIDOC-VBELN
                               CHANGING C_BSTKD = GS_RESULT-BSTKD ).
*EOI CH03
      ENDIF.
*--------------------------------------------------------------------*
      IF LS_FIDOC-VBELN IS NOT INITIAL.
        READ_TEXT( EXPORTING I_ID ='ZH09'
                              I_NAME = LS_FIDOC-VBELN
                              I_OBJECT =  'VBBK'
                             CHANGING C_TEXT = GS_RESULT-INV_REMARK ).

      ENDIF.
*--------------------------------------------------------------------*
      READ TABLE LT_VBFA_INV_DO INTO DATA(LS_VBFA_INV_DO) WITH KEY VBELN =  LS_FIDOC-VBELN.
      IF SY-SUBRC = 0.
        GS_RESULT-VBELN_DO = LS_VBFA_INV_DO-VBELV.
      ENDIF.
*EOI CH04 420000327
*    LS_DATA1-VBELN_DO  = <L_FIDOC>-VGBEL. "CH04 420000327-
      GS_RESULT-RPDAT     = P_RPDAT.
*    LS_DATA1-BUDAT     = <L_FIDOC>-BUDAT. "<<F36K909945--
      GS_RESULT-RPDAT    = LS_FIDOC-BLDAT.  "<<F36K909945++
*--------------------------------------------------------------------*
      NET_DUE_DATE_GET( EXPORTING I_ZFBDT = LS_FIDOC-ZFBDT
                                  I_ZBD1T = LS_FIDOC-ZBD1T
                                  I_ZBD2T = LS_FIDOC-ZBD2T
                                  I_ZBD3T = LS_FIDOC-ZBD3T
                                  I_SHKZG = LS_FIDOC-SHKZG
                                  I_REBZG = LS_FIDOC-REBZG
                             CHANGING C_DUEDT = GS_RESULT-DUEDT1 ).
      READ TABLE LT_033_035 INTO DATA(LS_BILLPL) WITH KEY BUKRS = LS_FIDOC-BUKRS
                                                             BELNR = LS_FIDOC-BELNR
                                                             GJAHR = LS_FIDOC-GJAHR
                                                             BUZEI = LS_FIDOC-BUZEI.
      IF SY-SUBRC <> 0.
        READ TABLE LT_029 INTO LS_BILLPL WITH KEY BUKRS = LS_FIDOC-BUKRS
                                                  BELNR = LS_FIDOC-BELNR
                                                  GJAHR = LS_FIDOC-GJAHR
                                                  BUZEI = LS_FIDOC-BUZEI.
        IF SY-SUBRC <> 0.
          READ TABLE LT_038 INTO LS_BILLPL WITH KEY BUKRS = LS_FIDOC-BUKRS
                                                    BELNR = LS_FIDOC-BELNR
                                                    GJAHR = LS_FIDOC-GJAHR
                                                    BUZEI = LS_FIDOC-BUZEI.
          IF SY-SUBRC <> 0.
            CLEAR LS_BILLPL.
*BOI CH02
          ELSE.
            IF LS_BILLPL-BILLPL_DATE IS INITIAL.
              LS_BILLPL-BILLPL_DATE = LS_BILLPL-WORK_DATE.
            ENDIF.
*EOI CH02
          ENDIF.
*BOI CH02
        ELSE.
          IF LS_BILLPL-BILLPL_DATE IS INITIAL.
            LS_BILLPL-BILLPL_DATE = LS_BILLPL-WORK_DATE.
          ENDIF.
*EOI CH02
        ENDIF.
      ENDIF.

*--------------------------------------------------------------------*
      LF_TDNAME = LS_FIDOC-BUKRS && LS_FIDOC-BELNR && LS_FIDOC-GJAHR && LS_FIDOC-BUZEI.
      READ_TEXT( EXPORTING  I_ID = 'FI01'
                            I_NAME = LF_TDNAME
                            I_OBJECT = GC_TDOBJECT_DETAIL
                 CHANGING   C_TEXT  = GS_RESULT-TEXT1 ).
      READ_TEXT( EXPORTING  I_ID =  'FI02'
                            I_NAME =  LF_TDNAME
                            I_OBJECT  = GC_TDOBJECT_DETAIL
                 CHANGING   C_TEXT   =   GS_RESULT-TEXT2 ).
      READ_TEXT( EXPORTING  I_ID = 'FI03'
                            I_NAME = LF_TDNAME
                            I_OBJECT  = GC_TDOBJECT_DETAIL
                 CHANGING   C_TEXT = GS_RESULT-TEXT3 ).
*--------------------------------------------------------------------*
      READ TABLE LT_040 INTO DATA(LS_040) WITH KEY BUKRS = LS_FIDOC-BUKRS
                                                       BELNR = LS_FIDOC-BELNR
                                                       GJAHR = LS_FIDOC-GJAHR
                                                       BUZEI = LS_FIDOC-BUZEI.
      IF SY-SUBRC = 0.
        GS_RESULT-NEW_DUEDT2 =  LS_040-NEW_DUEDT2.
        GS_RESULT-NEW_DUEDT3 =  LS_040-NEW_DUEDT3.
        GS_RESULT-ISSUE_LIST =  LS_040-ISSUE_LIST.
        READ TABLE GT_ISLIST INTO DATA(LS_ISLIST) WITH KEY VALUE = LS_040-ISSUE_LIST.
        IF SY-SUBRC = 0.
          GS_RESULT-ISSUE_LIST_TX = LS_ISLIST-TEXT.
        ENDIF.
        GS_RESULT-FOLLOWDT =  LS_040-FOLLOWDT.
*      LS_DATA1-DETAIL1  =  LS_040-DETAIL1 .  "CH04 420000332-
*      LS_DATA1-DETAIL2  =  LS_040-DETAIL2.   "CH04 420000332-
*      LS_DATA1-DETAIL3  =  LS_040-DETAIL3.   "CH04 420000332-

        GS_RESULT-UPD_DATE =  LS_040-UPD_DATE.
        GS_RESULT-UPD_TIME =  LS_040-UPD_TIME.
        GS_RESULT-UPD_USER =  LS_040-UPD_USER.
        GS_RESULT-SEQNO    = LS_040-SEQNO.
        GS_RESULT-EXIST    = ABAP_TRUE.
      ELSE.
        IF S_FLWDT[] IS NOT INITIAL
        OR S_UPDDT[] IS NOT INITIAL.
          CONTINUE.
        ENDIF.
 READ TABLE LT_024 INTO DATA(LS_024) WITH KEY KUNNR = LS_FIDOC-KUNNR.
      IF SY-SUBRC = 0.
        GS_RESULT-NEW_DUEDT2 = GS_RESULT-DUEDT1 + LS_024-ZDAYS.
      ELSE.
        GS_RESULT-NEW_DUEDT2 = GS_RESULT-DUEDT1.
      ENDIF.
      GS_RESULT-NEW_DUEDT3   =  LS_BILLPL-BILLPL_PMTDT.
    ENDIF.
*--------------------------------------------------------------------*
GS_RESULT-INV_AGE = GS_RESULT-RPDAT - GS_RESULT-BUDAT + 1.

SET_DAY_RANGE( EXPORTING  I_DAY = GS_RESULT-INV_AGE
              CHANGING    C_DAY_RANGE =  GS_RESULT-RNG_INV_AGE ).
*--------------------------------------------------------------------*
    IF GS_RESULT-DUEDT1 IS NOT INITIAL.
      GS_RESULT-OVDUE1 = GS_RESULT-RPDAT - GS_RESULT-DUEDT1.

      SET_WITHIN_OR_OVER( EXPORTING I_DAY = GS_RESULT-OVDUE1
                          CHANGING  C_WITHIN_OR_OVER = GS_RESULT-WITHIN_DUE1 ).

      SET_DAY_RANGE( EXPORTING I_DAY = GS_RESULT-OVDUE1
                              CHANGING C_DAY_RANGE =  GS_RESULT-RNG_OVDUE1 ).
    ENDIF.
*--------------------------------------------------------------------*
IF GS_RESULT-NEW_DUEDT2 IS NOT INITIAL.
      GS_RESULT-OVDUE2 = GS_RESULT-RPDAT - GS_RESULT-NEW_DUEDT2.

      SET_WITHIN_OR_OVER( EXPORTING I_DAY = GS_RESULT-OVDUE2
                                   CHANGING C_WITHIN_OR_OVER = GS_RESULT-WITHIN_DUE2 ).

      SET_DAY_RANGE( EXPORTING I_DAY =  GS_RESULT-OVDUE2
                              CHANGING C_DAY_RANGE =   GS_RESULT-RNG_OVDUE2 ).
    ENDIF.
*--------------------------------------------------------------------*
IF GS_RESULT-NEW_DUEDT3 IS NOT INITIAL.
      GS_RESULT-OVDUE3 = GS_RESULT-RPDAT - GS_RESULT-NEW_DUEDT3.
       SET_WITHIN_OR_OVER( EXPORTING I_DAY = GS_RESULT-OVDUE3
                                   CHANGING  C_WITHIN_OR_OVER = GS_RESULT-WITHIN_DUE3 ).
       SET_DAY_RANGE( EXPORTING I_DAY =  GS_RESULT-OVDUE3
                              CHANGING C_DAY_RANGE = GS_RESULT-RNG_OVDUE3 ).
    ENDIF.
*--------------------------------------------------------------------*
GS_RESULT-DMBTR = LS_FIDOC-DMBTR.
    IF LS_FIDOC-SHKZG = GC_SHKZG_CREDIT.
      GS_RESULT-DMBTR = GS_RESULT-DMBTR * -1.
    ENDIF.
    "-- find clearing document
    READ TABLE LT_CLEAR_DOC TRANSPORTING NO FIELDS WITH KEY
                            BUKRS = LS_FIDOC-BUKRS
                            REBZG = LS_FIDOC-BELNR
                            REBZJ = LS_FIDOC-GJAHR
                            REBZZ = LS_FIDOC-BUZEI.
    IF SY-SUBRC = 0.
      DATA(LV_TABIX) = SY-TABIX.
      LOOP AT LT_CLEAR_DOC ASSIGNING FIELD-SYMBOL(<L_CLEAR_DOC>) FROM LV_TABIX.
        IF  <L_CLEAR_DOC>-BUKRS <> LS_FIDOC-BUKRS
        OR  <L_CLEAR_DOC>-REBZG <> LS_FIDOC-BELNR
        OR  <L_CLEAR_DOC>-REBZJ <> LS_FIDOC-GJAHR
        OR  <L_CLEAR_DOC>-REBZZ <> LS_FIDOC-BUZEI.
          EXIT.
        ENDIF.
*BOI CH06  420000219
        IF <L_CLEAR_DOC>-USED = ABAP_TRUE.
          CONTINUE.
        ENDIF.
*EOI CH06  420000219
        IF <L_CLEAR_DOC>-SHKZG = GC_SHKZG_DEBIT.
          <L_CLEAR_DOC>-DMBTR = <L_CLEAR_DOC>-DMBTR * -1.
        ENDIF.
        <L_CLEAR_DOC>-USED = ABAP_TRUE. "CH06  420000219       +
        LF_DMBTR_CLR = LF_DMBTR_CLR + <L_CLEAR_DOC>-DMBTR.
      ENDLOOP.
    ENDIF.
*--------------------------------------------------------------------*
    IF LS_FIDOC-BLART = GC_BLART_MIGRATE
    AND  LS_FIDOC-XBLNR IS NOT INITIAL.
      READ TABLE LT_CLEAR_MGRTE TRANSPORTING NO FIELDS WITH KEY      ##WARN_OK
                                BUKRS = LS_FIDOC-BUKRS
                                XREF1 = LS_FIDOC-XBLNR.
      IF SY-SUBRC = 0.
        LV_TABIX  = SY-TABIX.
*BOI CH06  420000219
        "Check whether found the same XREF1 in other doc?
        LF_COUNT_XBLNR = REDUCE I( INIT LF_I = 0 FOR LS_FIDOC1 IN LT_FIDOC WHERE ( XBLNR = LS_FIDOC-XBLNR ) NEXT LF_I = LF_I + 1 ). "#EC CI_SORTSEQ
        IF LF_COUNT_XBLNR <= 1.
*EOI CH06  420000219
          LOOP AT LT_CLEAR_MGRTE ASSIGNING FIELD-SYMBOL(<L_CLEAR_MGRTE>) FROM LV_TABIX.
            IF <L_CLEAR_MGRTE>-BUKRS <> LS_FIDOC-BUKRS
            OR <L_CLEAR_MGRTE>-XREF1 <> LS_FIDOC-XBLNR.
              EXIT.
            ENDIF.
*BOI CH06  420000219
            IF <L_CLEAR_MGRTE>-USED = ABAP_TRUE.
              CONTINUE.
            ENDIF.
*EOI CH06  420000219
            IF <L_CLEAR_MGRTE>-SHKZG = GC_SHKZG_DEBIT.
              <L_CLEAR_MGRTE>-DMBTR = <L_CLEAR_MGRTE>-DMBTR * -1.
            ENDIF.
            LF_DMBTR_CLR = LF_DMBTR_CLR + <L_CLEAR_MGRTE>-DMBTR.
            <L_CLEAR_MGRTE>-USED = ABAP_TRUE.  "CH06  420000219+
*BOI CH06 420000219
            "--- clearing doc of migrate doc
            LOOP AT LT_CLEAR_DOC ASSIGNING <L_CLEAR_DOC> WHERE BUKRS = <L_CLEAR_MGRTE>-BUKRS
                                                         AND REBZG = <L_CLEAR_MGRTE>-BELNR
                                                         AND REBZJ = <L_CLEAR_MGRTE>-GJAHR
                                                         AND REBZZ = <L_CLEAR_MGRTE>-BUZEI.
              IF <L_CLEAR_DOC>-USED = ABAP_TRUE.
                CONTINUE.
              ENDIF.
              IF <L_CLEAR_DOC>-SHKZG = GC_SHKZG_DEBIT.
                <L_CLEAR_DOC>-DMBTR = <L_CLEAR_DOC>-DMBTR * -1.
              ENDIF.
              LF_DMBTR_CLR = LF_DMBTR_CLR + <L_CLEAR_DOC>-DMBTR.
              <L_CLEAR_DOC>-USED = ABAP_TRUE.
            ENDLOOP.
*EOI CH06 420000219
          ENDLOOP.
        ENDIF.  "CH06  420000219
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
GS_RESULT-DMBTR_REMAIN  = GS_RESULT-DMBTR - LF_DMBTR_CLR.
GS_RESULT-WAERS = GS_T001-WAERS.

    IF LS_FIDOC-UMSKZ IS INITIAL.
      READ TABLE LT_SKAT INTO DATA(LS_SKAT) WITH KEY SAKNR = LS_FIDOC-HKONT.
      IF SY-SUBRC = 0.
        GS_RESULT-HKONT_TX = LS_SKAT-TXT20.
      ENDIF.
    ELSE.
      READ TABLE LT_T074T INTO DATA(LS_T074T) WITH KEY SHBKZ = LS_FIDOC-UMSKZ .
      IF SY-SUBRC = 0.
        GS_RESULT-HKONT_TX = LS_T074T-LTEXT.
      ENDIF.
    ENDIF.
    GS_RESULT-VKGRP = LS_FIDOC-VKGRP.
    IF LS_FIDOC-VKGRP IS NOT INITIAL.
      READ TABLE LT_TVGRT INTO DATA(LS_TVGRT) WITH KEY VKGRP = LS_FIDOC-VKGRP.
      IF SY-SUBRC = 0.
        GS_RESULT-VKGRP_TX  = LS_TVGRT-BEZEI.
      ENDIF.
    ENDIF.
    GS_RESULT-VKBUR = LS_FIDOC-VKBUR.
    IF LS_FIDOC-VKBUR IS NOT INITIAL.
      READ TABLE LT_TVKBT INTO DATA(LS_TVKBT) WITH KEY VKBUR = LS_FIDOC-VKBUR.
      IF SY-SUBRC = 0.
        GS_RESULT-VKBUR_TX  = LS_TVKBT-BEZEI.
      ENDIF.
    ENDIF.
    GS_RESULT-VKBUR = LS_FIDOC-VKBUR.
    GS_RESULT-BILLPL_NO = LS_BILLPL-BILLPL_NO.
    GS_RESULT-BILLPL_DATE = LS_BILLPL-BILLPL_DATE.
    GS_RESULT-ZTERM = LS_FIDOC-ZTERM.
    GS_RESULT-ZUONR = LS_FIDOC-ZUONR.
    GS_RESULT-UMSKZ = LS_FIDOC-UMSKZ.
    LF_INDX = LF_INDX + 1.
    GS_RESULT-INDX = LF_INDX.
    APPEND GS_RESULT TO GT_RESULT.




*      GS_RESULT-PIC_PERNR      = ''.
*      GS_RESULT-PIC_NAME1      = ''.
*      GS_RESULT-VBELN_BILL     = ''.
*      GS_RESULT-BUKRS          = ''.
*      GS_RESULT-BELNR          = ''.
*      GS_RESULT-GJAHR          = ''.
*      GS_RESULT-BUZEI          = ''.
*      GS_RESULT-XBLNR          = ''.
*      GS_RESULT-XREF1          = ''.
*      GS_RESULT-KUNNR          = ''.

*      GS_RESULT-BUSAB          = ''.
*      GS_RESULT-SGTXT          = ''.
*      GS_RESULT-POST1          = ''.
*      GS_RESULT-BSTKD          = ''.
*      GS_RESULT-INV_REMARK     = ''.
*      GS_RESULT-VBELN_DO       = ''.
*      GS_RESULT-RPDAT          = ''.
*      GS_RESULT-BUDAT          = ''.
*        GS_RESULT-DUEDT1         = ''.
*        GS_RESULT-NEW_DUEDT2     = ''.
*        GS_RESULT-NEW_DUEDT3     = ''.
*        GS_RESULT-INV_AGE        = ''.
*        GS_RESULT-RNG_INV_AGE    = ''.
*        GS_RESULT-OVDUE1         = ''.
*        GS_RESULT-OVDUE2         = ''.
*        GS_RESULT-OVDUE3         = ''.
*        GS_RESULT-WITHIN_DUE1    = ''.
*        GS_RESULT-RNG_OVDUE1     = ''.
*        GS_RESULT-WITHIN_DUE2    = ''.
*        GS_RESULT-RNG_OVDUE2     = ''.
*        GS_RESULT-WITHIN_DUE3    = ''.
*        GS_RESULT-RNG_OVDUE3     = ''.
*        GS_RESULT-DMBTR          = ''.
*        GS_RESULT-DMBTR_REMAIN   = ''.
*        GS_RESULT-WAERS          = ''.
*        GS_RESULT-HKONT_TX       = ''.
*        GS_RESULT-VKGRP          = ''.
*        GS_RESULT-VKGRP_TX       = ''.
*        GS_RESULT-VKBUR          = ''.
*        GS_RESULT-VKBUR_TX       = ''.
*        GS_RESULT-BILLPL_NO      = ''.
*        GS_RESULT-BILLPL_DATE    = ''.
*        GS_RESULT-ISSUE_LIST     = ''.
*        GS_RESULT-ISSUE_LIST_TX  = ''.
*        GS_RESULT-FOLLOWDT       = ''.
*      GS_RESULT-TEXT1          = ''.
*      GS_RESULT-TEXT2          = ''.
*      GS_RESULT-TEXT3          = ''.
*      GS_RESULT-SALEMAN        = ''.
*        GS_RESULT-SALEMAN_NAME   = ''.
*        GS_RESULT-ZTERM          = ''.
*        GS_RESULT-ZUONR          = ''.
*        GS_RESULT-UMSKZ          = ''.
*        GS_RESULT-UPD_DATE       = ''.
*        GS_RESULT-UPD_TIME       = ''.
*        GS_RESULT-UPD_USER       = ''.
*        GS_RESULT-INDX           = ''.
*        GS_RESULT-EXIST          = ''.
*        GS_RESULT-SEQNO          = ''.
**        GS_RESULT-CHANGE_TX      = ''.
*        GS_RESULT-CHECK          = ''.

*        APPEND GS_RESULT TO GT_RESULT.
      ENDLOOP.

    ENDMETHOD.
    METHOD GET_ADDTIONAL_DATA.
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
*         I_CALLBACK_TOP_OF_PAGE            = ' '
*         i_html_height_top  = 12
*         I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
*         I_CALLBACK_HTML_END_OF_LIST       = ' '
*         I_STRUCTURE_NAME   =
*         I_BACKGROUND_ID    = ' '
*         I_GRID_TITLE       =
*         I_GRID_SETTINGS    =
          IS_LAYOUT          = GS_LAYOUT
          IT_FIELDCAT        = GT_FCAT
*         IT_EXCLUDING       =
*         IT_SPECIAL_GROUPS  =
          IT_SORT            = GT_SORT
*         IT_FILTER          =
*         IS_SEL_HIDE        =
          I_DEFAULT          = GC_X
          I_SAVE             = GC_A
*         IS_VARIANT         =
*         IT_EVENTS          =
*         IT_EVENT_EXIT      =
*         IS_PRINT           =
*         IS_REPREP_ID       =
*         I_SCREEN_START_COLUMN             = 0
*         I_SCREEN_START_LINE               = 0
*         I_SCREEN_END_COLUMN               = 0
*         I_SCREEN_END_LINE  = 0
*         I_HTML_HEIGHT_TOP  = 0
*         I_HTML_HEIGHT_END  = 0
*         IT_ALV_GRAPHICS    =
*         IT_HYPERLINK       =
*         IT_ADD_FIELDCAT    =
*         IT_EXCEPT_QINFO    =
*         IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*         E_EXIT_CAUSED_BY_CALLER           =
*         ES_EXIT_CAUSED_BY_USER            =
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
    METHOD SELECT_KNA1.
      IF I_DATA[] IS INITIAL.
        RETURN.
      ENDIF.
*    SELECT KUNNR,
*           NAME1,
*           XCPDK "CH04 420000327+
*    INTO TABLE @C_DATA
*    FROM KNA1
*    FOR  ALL ENTRIES IN @I_DATA
*    WHERE KUNNR = @I_DATA-KUNNR.
      SELECT KNA1~KUNNR,
             KNA1~NAME1,
             KNA1~XCPDK
        FROM @I_DATA AS A
        INNER JOIN KNA1 ON A~KUNNR EQ KNA1~KUNNR
        INTO TABLE @C_DATA.
      ENDMETHOD.
      METHOD SELECT_KNA1_027.

        IF I_DATA[] IS INITIAL.
          RETURN.
        ENDIF.
*  SELECT  KNB1~KUNNR,
*          KNB1~BUSAB,
*          ZSDSFIC027~PIC_PERNR,
*          ZSDSFIC027~FR_KUNNR,
*          ZSDSFIC027~TO_KUNNR
*  INTO TABLE @C_DATA
*  FROM KNB1 LEFT JOIN ZSDSFIC027
*  ON KNB1~KUNNR  GE ZSDSFIC027~FR_KUNNR
*  AND KNB1~KUNNR LE ZSDSFIC027~TO_KUNNR
*  AND KNB1~BUSAB GE ZSDSFIC027~FR_BUSAB
*  AND KNB1~BUSAB LE ZSDSFIC027~TO_BUSAB
*  FOR ALL ENTRIES IN @I_DATA
*  WHERE KNB1~BUKRS IN @S_BUKRS
*  AND   KNB1~KUNNR = @I_DATA-KUNNR
*  AND   ZSDSFIC027~VKORG = ''
*  AND   ZSDSFIC027~PIC_PERNR IN @S_PIC
*  AND   ZSDSFIC027~FR_BUSAB  IN @S_BUSAB


        SELECT KNB1~KUNNR,
               KNB1~BUSAB,
               ZSDSFIC027~PIC_PERNR,
               ZSDSFIC027~FR_KUNNR,
               ZSDSFIC027~TO_KUNNR
          FROM @I_DATA AS A

          INNER JOIN KNB1 ON A~KUNNR EQ KNB1~KUNNR
          LEFT JOIN ZSDSFIC027
       ON KNB1~KUNNR  GE ZSDSFIC027~FR_KUNNR
       AND KNB1~KUNNR LE ZSDSFIC027~TO_KUNNR
       AND KNB1~BUSAB GE ZSDSFIC027~FR_BUSAB
       AND KNB1~BUSAB LE ZSDSFIC027~TO_BUSAB
        WHERE KNB1~BUKRS IN @S_BUKRS
       AND   ZSDSFIC027~VKORG = ''
       AND   ZSDSFIC027~PIC_PERNR IN @S_PIC
       AND   ZSDSFIC027~FR_BUSAB  IN @S_BUSAB
       INTO TABLE @C_DATA.

*  IF S_PIC[] IS NOT INITIAL.
*    DELETE CT_KNA1_027 WHERE PIC_PERNR NOT IN S_PIC.    "#EC CI_SORTSEQ
*  ENDIF.
*  IF S_BUSAB[] IS NOT INITIAL.
*    DELETE CT_KNA1_027 WHERE BUSAB NOT IN S_BUSAB.      "#EC CI_SORTSEQ
*  ENDIF.
        ENDMETHOD.

        METHOD SELECT_KNVV_027.
          DATA: LR_VKORG TYPE RANGE OF VBAK-VKORG.
          IF I_DATA[] IS INITIAL.
            RETURN.
          ENDIF.
          CASE GF_EXEC_TY.
            WHEN GC_EXEC_TY-FI.
              LR_VKORG = S_VKORGF[].
            WHEN GC_EXEC_TY-DO.
              LR_VKORG =    S_VKORGD[].
          ENDCASE.
          SELECT  KNVV~KUNNR,
                  KNVV~VKORG,
                  KNVV~VTWEG,
                  KNVV~SPART,
                  KNVV~VKBUR,
                  KNVV~VKGRP,
                  ZSDSFIC027~PIC_PERNR,
                  KNB1~BUSAB,
                  ZSDSFIC027~FR_KUNNR,
                  ZSDSFIC027~TO_KUNNR,
                  ZSDSFIC027~FR_BUSAB,
                  ZSDSFIC027~TO_BUSAB
          FROM @I_DATA AS A


           INNER JOIN KNVV            ON  A~KUNNR EQ KNVV~KUNNR
           INNER JOIN KNB1            ON  KNVV~KUNNR EQ KNB1~KUNNR
           LEFT JOIN ZSDSFIC027 ON KNVV~KUNNR GE ZSDSFIC027~FR_KUNNR
                                 AND KNVV~KUNNR LE ZSDSFIC027~TO_KUNNR
                                 AND KNVV~VKORG EQ ZSDSFIC027~VKORG
                                 AND KNVV~VTWEG GE ZSDSFIC027~FR_VTWEG
                                 AND KNVV~VTWEG LE ZSDSFIC027~TO_VTWEG
                                 AND KNVV~SPART GE ZSDSFIC027~FR_SPART
                                 AND KNVV~SPART LE ZSDSFIC027~TO_SPART
                                 AND KNVV~VKBUR EQ  ZSDSFIC027~VKBUR
                                 AND KNVV~VKGRP GE ZSDSFIC027~FR_VKGRP
                                 AND KNVV~VKGRP LE ZSDSFIC027~TO_VKGRP
                                 AND KNB1~BUSAB GE ZSDSFIC027~FR_BUSAB
                                 AND KNB1~BUSAB LE ZSDSFIC027~TO_BUSAB

          WHERE KNB1~BUKRS IN @S_BUKRS
          AND   KNVV~VKORG IN @LR_VKORG
          AND   KNVV~VTWEG IN @S_VTWEG
          AND   KNVV~SPART IN @S_SPART
          AND   KNVV~VKBUR IN @S_VKBUR
          AND   KNVV~VKGRP IN @S_VKGRP
          AND   KNB1~BUSAB IN @S_BUSAB
          AND   ZSDSFIC027~PIC_PERNR IN @S_PIC
*  IF S_PIC[] IS NOT INITIAL.
*    DELETE CT_KNVV_027 WHERE PIC_PERNR NOT IN S_PIC    "#EC CI_SORTSEQ
*
          INTO TABLE @C_DATA.
          ENDMETHOD.
          METHOD SELECT_KNB1.

            IF I_DATA[] IS INITIAL.
              RETURN.
            ENDIF.
            SELECT KNB1~KUNNR,
                   KNB1~BUKRS,
                   KNB1~BUSAB
              FROM @I_DATA AS A

            INNER JOIN  KNB1 ON  A~KUNNR EQ KNB1~KUNNR
            WHERE  KNB1~BUSAB IN @S_BUSAB

             INTO TABLE @C_DATA.
            ENDMETHOD.

            METHOD SELECT_FIDOC.
              SELECT   BSID_VIEW~BUKRS,
                         BSID_VIEW~BELNR,
                         BSID_VIEW~GJAHR,
                         BSID_VIEW~BUZEI,
                         BSID_VIEW~VBELN,
                         BSID_VIEW~BLART,
                         BSID_VIEW~KUNNR,
                         BSID_VIEW~XBLNR,
                         BSID_VIEW~SGTXT,
                         BSID_VIEW~DMBTR,
                         BSID_VIEW~HKONT,
                         BSID_VIEW~UMSKZ,
                         BSID_VIEW~BUDAT,
                         BSID_VIEW~BLDAT, "<<F36K909945++
                         BSID_VIEW~ZFBDT,
                         BSID_VIEW~ZBD1T,
                         BSID_VIEW~ZBD2T,
                         BSID_VIEW~ZBD3T,
                         BSID_VIEW~SHKZG,
                         BSID_VIEW~REBZG,
                         BSID_VIEW~ZTERM,
                         BSID_VIEW~ZUONR,
                         BSID_VIEW~XREF1, "CH04 420000219+
                         VBRK~VKORG,
                         VBRK~VTWEG,
                         VBRK~SPART,
                         VBRP~POSNR,
                         VBRP~VKGRP,
                         VBRP~VKBUR,
                         VBRP~AUBEL,
                         VBRP~VGBEL
                  FROM BKPF  INNER JOIN BSID_VIEW
                  ON   BKPF~BELNR = BSID_VIEW~BELNR
                                     LEFT OUTER JOIN VBRK
                  ON   BSID_VIEW~VBELN = VBRK~VBELN
                                      LEFT OUTER JOIN VBRP
                  ON   BSID_VIEW~VBELN = VBRP~VBELN
                  WHERE BSID_VIEW~BUKRS IN @S_BUKRS
                  AND   BSID_VIEW~KUNNR IN @S_KUNNR
                  AND   BSID_VIEW~BELNR IN @S_BELNR
                  AND   BSID_VIEW~VBELN IN @S_BILL

                  AND   BSID_VIEW~BLART IN @S_BLART
                  AND   BSID_VIEW~XBLNR IN @S_XBLNR
                  AND   BSID_VIEW~UMSKZ IN @S_UMSKZ

                  AND   BSID_VIEW~BUDAT IN @GRT_RPDAT

                  AND   BKPF~BSTAT <> 'S'

                   INTO TABLE @C_DATA.

                SELECT BSAD_VIEW~BUKRS,
                      BSAD_VIEW~BELNR,
                      BSAD_VIEW~GJAHR,
                      BSAD_VIEW~BUZEI,
                      BSAD_VIEW~VBELN,
                      BSAD_VIEW~BLART,
                      BSAD_VIEW~KUNNR,
                      BSAD_VIEW~XBLNR,
                      BSAD_VIEW~SGTXT,
                      BSAD_VIEW~DMBTR,
                      BSAD_VIEW~HKONT,
                      BSAD_VIEW~UMSKZ,
                      BSAD_VIEW~BUDAT,
                      BSAD_VIEW~BLDAT, "<<F36K909945++
                      BSAD_VIEW~ZFBDT,
                      BSAD_VIEW~ZBD1T,
                      BSAD_VIEW~ZBD2T,
                      BSAD_VIEW~ZBD3T,
                      BSAD_VIEW~SHKZG,
                      BSAD_VIEW~REBZG,
                      BSAD_VIEW~ZTERM,
                      BSAD_VIEW~ZUONR,
                      BSAD_VIEW~XREF1, "CH04 420000219+
                      VBRK~VKORG,
                      VBRK~VTWEG,
                      VBRK~SPART,
                      VBRP~POSNR,
                      VBRP~VKGRP,
                      VBRP~VKBUR,
                      VBRP~AUBEL,
                      VBRP~VGBEL
               FROM BKPF  INNER JOIN BSAD_VIEW
               ON   BKPF~BELNR = BSAD_VIEW~BELNR
                                  LEFT OUTER JOIN VBRK
               ON   BSAD_VIEW~VBELN = VBRK~VBELN
                                   LEFT OUTER JOIN VBRP
               ON   BSAD_VIEW~VBELN = VBRP~VBELN
               WHERE BSAD_VIEW~BUKRS IN @S_BUKRS
               AND   BSAD_VIEW~KUNNR IN @S_KUNNR
               AND   BSAD_VIEW~BELNR IN @S_BELNR
               AND   BSAD_VIEW~VBELN IN @S_BILL

               AND   BSAD_VIEW~BLART IN @S_BLART
               AND   BSAD_VIEW~XBLNR IN @S_XBLNR
               AND   BSAD_VIEW~UMSKZ IN @S_UMSKZ
*
               AND   BSAD_VIEW~BUDAT IN @GRT_RPDAT
               AND   BSAD_VIEW~AUGDT > @P_RPDAT

               AND   BKPF~BSTAT <> 'S'
               AND   BSAD_VIEW~BELNR  <> BSAD_VIEW~AUGBL

               APPENDING TABLE @C_DATA.
                ENDMETHOD.
                METHOD SELECT_BSEC.
                  IF I_DATA[] IS NOT INITIAL.

                    DATA : BEGIN OF LS_FI,
                             BUKRS TYPE TS_FIDOC-BUKRS,
                             BELNR TYPE TS_FIDOC-BELNR,
                             GJAHR TYPE TS_FIDOC-GJAHR,
                             BUZEI TYPE TS_FIDOC-BUZEI,
                           END OF LS_FI.
                    DATA : LT_FI LIKE HASHED TABLE OF LS_FI WITH UNIQUE KEY   BUKRS
                                                                              BELNR
                                                                              GJAHR
                                                                              BUZEI.


                    LT_FI =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

                    SELECT  BSEC~BUKRS,                 "#EC CI_NOORDER
                            BSEC~BELNR,
                            BSEC~GJAHR,
                            BSEC~BUZEI,
                            ADRC~NATION,
                            ADRC~NAME1
                    FROM @LT_FI AS A

                    INNER JOIN BSEC ON A~BUKRS EQ BSEC~BUKRS
                                   AND A~BELNR EQ BSEC~BELNR
                                   AND A~GJAHR EQ BSEC~GJAHR
                                   AND A~BUZEI EQ BSEC~BUZEI
                    INNER JOIN ADRC ON BSEC~ADRNR = ADRC~ADDRNUMBER


*    WHERE BUKRS = @I_DATA-BUKRS
*    AND   BELNR = @I_DATA-BELNR
*    AND   GJAHR = @I_DATA-GJAHR
*    AND   BUZEI = @I_DATA-BUZEI

                      INTO TABLE @C_DATA.
                    ENDIF.
                  ENDMETHOD.
                  METHOD SELECT_CLEAR_DOC.
                    IF I_DATA[] IS INITIAL.
                      RETURN.
                    ENDIF.

                    DATA : BEGIN OF LS_FI,
                             BUKRS TYPE BSEG-BUKRS,
                             BELNR TYPE BSEG-REBZG,
                             GJAHR TYPE BSEG-REBZJ,
                             BUZEI TYPE BSEG-REBZZ,
                           END OF LS_FI.
                    DATA : LT_FI LIKE HASHED TABLE OF LS_FI WITH UNIQUE KEY BUKRS
                                                                            BELNR
                                                                            GJAHR
                                                                            BUZEI.

                    LT_FI =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).



                    SELECT BSEG~BUKRS,                  "#EC CI_NOORDER
                           BSEG~REBZG,
                           BSEG~REBZJ,
                           BSEG~REBZZ,
                           BSEG~GJAHR,
                           BSEG~BELNR,
                           BSEG~BUZEI,
                           BSEG~SHKZG,
                           BSEG~DMBTR,
                           BSEG~AUGBL
                  FROM @LT_FI AS A
                     INNER JOIN BSEG   ON A~BUKRS EQ BSEG~BUKRS
                                      AND A~BELNR EQ BSEG~REBZG
                                      AND A~GJAHR EQ BSEG~REBZJ
                                      AND A~BUZEI EQ BSEG~REBZZ

                     INNER JOIN BKPF ON BSEG~BUKRS  EQ BKPF~BUKRS
                                     AND BSEG~BELNR EQ BKPF~BELNR
                                     AND BSEG~GJAHR EQ BKPF~GJAHR
                  WHERE BKPF~BUDAT IN @GRT_RPDAT
                    AND ( ( BSEG~AUGBL IS INITIAL AND BSEG~AUGDT IS INITIAL ) OR
                          ( BSEG~AUGBL NE BSEG~BELNR AND BSEG~AUGDT GT @P_RPDAT ) )
*  WHERE BSEG~BUKRS IN @S_BUKRS
*  AND   REBZG = @UT_FI_DOC-BELNR
*  AND   REBZJ = @UT_FI_DOC-GJAHR
*  AND   REBZZ = @UT_FI_DOC-BUZEI
**<<F36K909945 start ins
*  AND   BUDAT IN @GRT_RPDAT
*  AND ( ( AUGBL IS INITIAL AND AUGDT IS INITIAL ) OR ( A~AUGBL <> A~BELNR AND AUGDT > @P_RPDAT ) )
**<<F36K909945 end ins
**  AND   B~XREVERSAL = ''  "CH02 420000219-
                    INTO TABLE @C_DATA.



                    ENDMETHOD.
                    METHOD SELECT_CLEAR_MIGRATE_DOC.

                      IF I_DATA IS INITIAL.
                        RETURN.
                      ENDIF.

                      DATA : BEGIN OF LS_FI,
                               BUKRS TYPE BSEG-BUKRS,
                               XREF1 TYPE BSEG-XREF1,
                               GJAHR TYPE BSEG-GJAHR,
                               BELNR TYPE BSEG-BELNR,
                               XBLNR TYPE BKPF-XBLNR,
                               BLART TYPE BKPF-BLART,
                             END OF LS_FI.
                      DATA : LT_FI LIKE HASHED TABLE OF LS_FI WITH UNIQUE KEY BUKRS
                                                                              XREF1
                                                                              GJAHR
                                                                              BELNR
                                                                              XBLNR
                                                                              BLART.
                      LT_FI =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

                      DATA(LT_FI_DOC) = LT_FI .
                      DELETE LT_FI_DOC WHERE XBLNR = '' "#EC CI_SORTSEQ
                                       OR XREF1 = ''
                                       OR BLART <> GC_BLART_MIGRATE.
                      LOOP AT LT_FI_DOC INTO DATA(LS_FI_DOC)                ##INTO_OK.
                        IF LS_FI_DOC-XBLNR <> LS_FI_DOC-XREF1.
                          DELETE LT_FI_DOC WHERE BUKRS = LS_FI_DOC-BUKRS AND
                                                 XREF1 = LS_FI_DOC-XREF1 AND
                                                 GJAHR = LS_FI_DOC-GJAHR AND
                                                 BELNR = LS_FI_DOC-BELNR AND
                                                 XBLNR = LS_FI_DOC-XBLNR AND
                                                 BLART = LS_FI_DOC-BLART.
                        ENDIF.
                      ENDLOOP.
                      IF LT_FI_DOC IS NOT INITIAL.
                        SELECT  BSEG~BUKRS,             "#EC CI_NOORDER
                                BSEG~XREF1 ,
                                BKPF~XBLNR ,
                                BSEG~GJAHR ,
                                BSEG~BELNR ,
                                BSEG~BUZEI ,
                                BSEG~SHKZG,
                                BSEG~UMSKZ ,
                                BSEG~DMBTR
                          FROM @LT_FI_DOC  AS A
                          INNER JOIN BSEG ON A~BUKRS EQ BSEG~BUKRS
                                         AND A~XREF1 EQ BSEG~XREF1
                                         AND A~GJAHR EQ BSEG~GJAHR
                                         AND A~BELNR EQ BSEG~BELNR
                          INNER JOIN BKPF ON BSEG~BUKRS EQ BKPF~BUKRS
                       AND BSEG~BELNR EQ BKPF~BELNR
                       AND BSEG~GJAHR EQ BKPF~GJAHR

                       WHERE BSEG~BUKRS IN @S_BUKRS
                       AND   KOART = 'D' "CUSTOMER
                       AND   BUDAT IN @GRT_RPDAT
                       AND   BKPF~BLART = @GC_BLART_MIGRATE
                       AND   BSEG~XREF1 <> BKPF~XBLNR
                       AND ( ( AUGBL IS INITIAL AND AUGDT IS INITIAL ) OR ( BSEG~AUGBL NE BSEG~BELNR AND AUGDT GT @P_RPDAT ) )
                       INTO TABLE @C_DATA.
                        ENDIF.

                      ENDMETHOD.
                      METHOD SELECT_PA0002_PIC.

                        TYPES: BEGIN OF  LTY_PIC,
                                 PIC_PERNR TYPE ZSDSFIC027-PIC_PERNR,
                               END OF LTY_PIC.

                        DATA: LT_PIC TYPE TABLE OF LTY_PIC WITH EMPTY KEY.
                        MOVE-CORRESPONDING I_KNA1_027 TO LT_PIC.
                        MOVE-CORRESPONDING I_KNVV_027 TO LT_PIC KEEPING TARGET LINES.
                        SORT LT_PIC BY PIC_PERNR.
                        DELETE LT_PIC WHERE PIC_PERNR IS INITIAL.
                        DELETE ADJACENT DUPLICATES FROM LT_PIC COMPARING PIC_PERNR.
                        IF LT_PIC[] IS NOT INITIAL.
                          SELECT PA0002~PERNR,
                                 PA0002~VORNA,
                                 PA0002~NACHN
                           FROM  @LT_PIC AS A
                            INNER JOIN PA0002 ON A~PIC_PERNR EQ PA0002~PERNR
                          WHERE BEGDA LE @SY-DATUM
                          AND   ENDDA GE @SY-DATUM
                            INTO TABLE @C_DATA.

                          ENDIF.
                        ENDMETHOD.
                        METHOD SELECT_PRPS .

                          DATA : BEGIN OF LS_FIDOC,

                                   VBELN TYPE BSID_VIEW-VBELN,
                                   SGTXT TYPE BSID_VIEW-SGTXT,


                                 END OF LS_FIDOC.
                          DATA : LT_FIDOC LIKE HASHED TABLE OF LS_FIDOC WITH UNIQUE KEY
                                                                                      VBELN
                                                                                      SGTXT.
                          LT_FIDOC =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

                          DELETE LT_FIDOC WHERE VBELN IS INITIAL
                                          OR SGTXT IS INITIAL.
                          SORT LT_FIDOC BY SGTXT.
                          DELETE ADJACENT DUPLICATES FROM LT_FIDOC COMPARING SGTXT.
                          IF LT_FIDOC[] IS NOT INITIAL.

                            SELECT POSID,
                                   POST1
                            FROM @LT_FIDOC AS A
                            INNER JOIN   PRPS ON A~SGTXT EQ PRPS~POSID
*   WHERE POSID = @LT_FIDOC-SGTXT(24)

                             INTO TABLE  @C_DATA.
                            ENDIF.
                          ENDMETHOD.
                          METHOD SELECT_SO_DATA .

                            DATA : BEGIN OF LS_FIDOC,
                                     AUBEL TYPE VBRP-AUBEL,
                                   END OF LS_FIDOC.
                            DATA : LT_FIDOC LIKE HASHED TABLE OF LS_FIDOC WITH UNIQUE KEY
                                                                                        AUBEL.
                            LT_FIDOC =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

                            DELETE LT_FIDOC WHERE AUBEL IS INITIAL.
                            IF LT_FIDOC[] IS NOT INITIAL.
                              SELECT VBELN,
                                     BSTKD
                             FROM @LT_FIDOC AS A
                             INNER JOIN VBKD ON A~AUBEL EQ VBKD~VBELN

                              WHERE BSTKD  <> ''
                              AND   POSNR = '000000'
                              INTO TABLE @C_DATA.

                              ENDIF.

                            ENDMETHOD.
                            METHOD SELECT_VBFA_INV_DO .

                              DATA : BEGIN OF LS_FIDOC,
                                       VBELN TYPE BSID_VIEW-VBELN,

                                     END OF LS_FIDOC.
                              DATA : LT_FIDOC LIKE HASHED TABLE OF LS_FIDOC WITH UNIQUE KEY
                                                                                          VBELN.

                              LT_FIDOC =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

                              IF LT_FIDOC[] IS NOT INITIAL.
                                SELECT VBFA~VBELN,
                                       VBFA~VBELV

                                FROM   @LT_FIDOC AS A
                                INNER JOIN VBFA ON A~VBELN EQ  VBFA~VBELN

                                WHERE VBTYP_V = 'J'

*   WHERE VBELN = @UT_FIDOC-VBELN "INV , CANCEL INV, CN DN
*  AND VBTYP_N = 'M' ."INV

                                  INTO TABLE @C_DATA.
                                ENDIF.

                              ENDMETHOD.
                              METHOD SELECT_ZSDSFIT040 .

                                DATA : BEGIN OF LS_FIDOC,
                                         BUKRS TYPE BSID_VIEW-BUKRS,
                                         BELNR TYPE BSID_VIEW-BELNR,
                                         GJAHR TYPE BSID_VIEW-GJAHR,
                                         BUZEI TYPE BSID_VIEW-BUZEI,

                                       END OF LS_FIDOC.
                                DATA : LT_FIDOC LIKE HASHED TABLE OF LS_FIDOC WITH UNIQUE KEY
                                                                                            BUKRS
                                                                                            BELNR
                                                                                            GJAHR
                                                                                            BUZEI.
                                LT_FIDOC =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).


                                IF LT_FIDOC IS NOT INITIAL.
                                  SELECT ZSDSFIT040~*
                                  FROM @LT_FIDOC AS A
                                  INNER JOIN  ZSDSFIT040 ON A~BUKRS EQ ZSDSFIT040~BUKRS
                                                        AND A~BELNR EQ ZSDSFIT040~BELNR
                                                        AND A~GJAHR EQ ZSDSFIT040~GJAHR
                                                        AND A~BUZEI EQ ZSDSFIT040~BUZEI
                                  WHERE  FOLLOWDT IN @S_FLWDT
                                  AND   UPD_DATE IN @S_UPDDT
                                  AND   LATEST = @ABAP_TRUE

                                    INTO TABLE @C_DATA.
                                  ENDIF.

                                ENDMETHOD.
                                METHOD SELECT_ZSDSFIC024.

                                  IF I_DATA[] IS NOT INITIAL.
                                    SELECT ZSDSFIC024~*
                                    FROM @I_DATA AS A
                                    INNER JOIN   ZSDSFIC024 ON A~KUNNR EQ  ZSDSFIC024~KUNNR

                                INTO TABLE @C_DATA .

                                    ENDIF.
                                  ENDMETHOD.
                                  METHOD SELECT_ZSDSFIT033_035.

                                    DATA : BEGIN OF LS_FIDOC,
                                             BUKRS TYPE BSID_VIEW-BUKRS,
                                             BELNR TYPE BSID_VIEW-BELNR,
                                             GJAHR TYPE BSID_VIEW-GJAHR,
                                             BUZEI TYPE BSID_VIEW-BUZEI,

                                           END OF LS_FIDOC.
                                    DATA : LT_FIDOC LIKE HASHED TABLE OF LS_FIDOC WITH UNIQUE KEY
                                                                                                BUKRS
                                                                                                BELNR
                                                                                                GJAHR
                                                                                                BUZEI.
                                    LT_FIDOC =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).




                                    IF LT_FIDOC[] IS NOT INITIAL.
                                      SELECT ZSDSFIT033~BUKRS,
                                             ZSDSFIT035~BELNR,
                                             ZSDSFIT035~GJAHR,
                                             ZSDSFIT035~BUZEI,
                                             ZSDSFIT033~BILLPL_NO,
                                             ZSDSFIT033~BILLPL_DATE,
                                             ZSDSFIT033~BILLPL_PMTDT
                                      FROM @LT_FIDOC AS A

                                      INNER JOIN ZSDSFIT035 ON A~BELNR EQ ZSDSFIT035~BELNR
                                                           AND A~GJAHR EQ ZSDSFIT035~GJAHR
                                                           AND A~GJAHR EQ ZSDSFIT035~GJAHR
                                      INNER JOIN ZSDSFIT033 ON A~BUKRS EQ ZSDSFIT033~BUKRS
                                                           AND ZSDSFIT033~BILLPL_NO EQ ZSDSFIT035~BILLPL_NO
                                                           AND ZSDSFIT033~DELFG = ''
                                                           AND ZSDSFIT035~DELFG = ''


*    FROM ZSDSFIT033 INNER JOIN ZSDSFIT035
*
*    ON   ZSDSFIT033~BUKRS = ZSDSFIT035~BUKRS
*    AND  ZSDSFIT033~BILLPL_NO = ZSDSFIT035~BILLPL_NO
*    FOR ALL ENTRIES IN @UT_FIDOC
*    WHERE ZSDSFIT033~BUKRS = @UT_FIDOC-BUKRS
*    AND   ZSDSFIT035~BELNR = @UT_FIDOC-BELNR
*    AND   ZSDSFIT035~GJAHR = @UT_FIDOC-GJAHR
*    AND   ZSDSFIT035~BUZEI = @UT_FIDOC-BUZEI
*    AND   ZSDSFIT033~DELFG = ''
*    AND   ZSDSFIT035~DELFG = ''
                                        INTO TABLE @C_DATA.
                                      ENDIF.


                                    ENDMETHOD.

                                    METHOD SELECT_ZSDSFIT029  .
                                      IF I_DATA[] IS NOT INITIAL.

                                        DATA : BEGIN OF LS_FIDOC,
                                                 BUKRS TYPE BSID_VIEW-BUKRS,
                                                 BELNR TYPE BSID_VIEW-BELNR,
                                                 GJAHR TYPE BSID_VIEW-GJAHR,
                                                 BUZEI TYPE BSID_VIEW-BUZEI,

                                               END OF LS_FIDOC.
                                        DATA : LT_FIDOC LIKE HASHED TABLE OF LS_FIDOC WITH UNIQUE KEY
                                                                                                    BUKRS
                                                                                                    BELNR
                                                                                                    GJAHR
                                                                                                    BUZEI.
                                        LT_FIDOC =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).


                                        SELECT ZSDSFIT029~BUKRS,
                                               ZSDSFIT029~BELNR,
                                               ZSDSFIT029~GJAHR,
                                               ZSDSFIT029~BUZEI,
                                               ZSDSFIT029~BILLPL_NO,
                                               ZSDSFIT029~BILLPL_DATE,
                                               ZSDSFIT029~PAYMENT_DATE AS BILLPL_PMTDT,
                                               ZSDSFIT029~WORK_DATE
                                        FROM  @LT_FIDOC AS A
                                        INNER JOIN  ZSDSFIT029 ON   A~BUKRS EQ ZSDSFIT029~BUKRS
                                                              AND   A~BELNR EQ ZSDSFIT029~BELNR
                                                              AND   A~GJAHR EQ ZSDSFIT029~GJAHR
                                                              AND   A~BUZEI EQ ZSDSFIT029~BUZEI
                                                              AND   ZSDSFIT029~DATA_TYPE EQ @SPACE
                                                              AND   ZSDSFIT029~BILLPL_NO NE @SPACE
                                          INTO TABLE @C_DATA.
                                        ENDIF.


                                      ENDMETHOD.
                                      METHOD SELECT_ZSDSFIT038 .

                                        IF I_DATA[] IS NOT INITIAL.

                                          DATA : BEGIN OF LS_FIDOC,
                                                   BUKRS TYPE BSID_VIEW-BUKRS,
                                                   BELNR TYPE BSID_VIEW-BELNR,
                                                   GJAHR TYPE BSID_VIEW-GJAHR,
                                                   BUZEI TYPE BSID_VIEW-BUZEI,

                                                 END OF LS_FIDOC.
                                          DATA : LT_FIDOC LIKE HASHED TABLE OF LS_FIDOC WITH UNIQUE KEY
                                                                                                      BUKRS
                                                                                                      BELNR
                                                                                                      GJAHR
                                                                                                      BUZEI.
                                          LT_FIDOC =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

                                          SELECT ZSDSFIT038~BUKRS,
                                                 ZSDSFIT038~BELNR,
                                                 ZSDSFIT038~GJAHR,
                                                 ZSDSFIT038~BUZEI,
                                                 ZSDSFIT038~BILLPL_NO,
                                                 ZSDSFIT038~BILLPL_DATE,
                                                 ZSDSFIT038~PAYMENT_DATE AS BILLPL_PMTDT,
                                                 ZSDSFIT038~WORK_DATE
                                          FROM   @LT_FIDOC AS A
                                          INNER JOIN   ZSDSFIT038    ON    A~BUKRS EQ ZSDSFIT038~BUKRS
                                                                    AND    A~BELNR EQ ZSDSFIT038~BELNR
                                                                    AND    A~GJAHR EQ ZSDSFIT038~GJAHR
                                                                    AND    A~BUZEI EQ ZSDSFIT038~BUZEI
                                                                    AND    ZSDSFIT038~DATA_TYPE EQ @SPACE
                                                                    AND    ZSDSFIT038~BILLPL_NO NE @SPACE
                                            INTO TABLE @C_DATA .
                                          ENDIF.

                                        ENDMETHOD.
                                        METHOD SELECT_GL_DATA .

                                          IF I_DATA[] IS NOT INITIAL.
                                            DATA : BEGIN OF LS_FIDOC,
                                                     HKONT TYPE BSID_VIEW-HKONT,
                                                   END OF LS_FIDOC.
                                            DATA : LT_FIDOC LIKE HASHED TABLE OF LS_FIDOC WITH UNIQUE KEY HKONT.

                                            LT_FIDOC =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

                                            SELECT SKAT~SAKNR,
                                                   SKAT~TXT20
                                            FROM @LT_FIDOC AS A
                                            INNER JOIN SKAT ON A~HKONT EQ SKAT~SAKNR
                                            WHERE SPRAS EQ @SY-LANGU
                                              AND KTOPL EQ @GS_T001-KTOPL
                                             INTO TABLE @C_SKAT.
                                            ENDIF.

                                            DATA : BEGIN OF LS_FIDOC1,
                                                     UMSKZ TYPE BSID_VIEW-UMSKZ,

                                                   END OF LS_FIDOC1.
                                            DATA : LT_FIDOC1 LIKE HASHED TABLE OF LS_FIDOC1 WITH UNIQUE KEY UMSKZ.

                                            LT_FIDOC1 =  CORRESPONDING #( I_DATA  DISCARDING DUPLICATES ).

                                            IF LT_FIDOC1[] IS NOT INITIAL.
                                              SELECT SHBKZ,
                                                     LTEXT
                                              FROM  @LT_FIDOC1 AS A
                                              INNER JOIN  T074T ON A~UMSKZ EQ T074T~SHBKZ
                                              WHERE SPRAS EQ @SY-LANGU
                                              AND   KOART EQ @GC_KOART_CUST
                                              INTO TABLE @C_T074T. "#EC CI_NOORDER
                                              ENDIF.

                                            ENDMETHOD.

                                            METHOD GET_PO_FROM_SV_ORDER.
                                              CONSTANTS: LC_VBTYP_SVORDER TYPE VBFA-VBTYP_N VALUE 'CSVO',
                                                         LC_VBTYP_SVCONTR TYPE VBFA-VBTYP_N VALUE 'CSCT'.
                                              DATA: LT_DOCFLOW  TYPE TDT_DOCFLOW,
                                                    LR_VBTYP_SV TYPE RANGE OF VBFA-VBTYP_N.

                                              CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
                                                EXPORTING
                                                  IV_DOCNUM     = I_VBELN
                                                IMPORTING
                                                  ET_DOCFLOW    = LT_DOCFLOW
                                                EXCEPTIONS
                                                  ERROR_MESSAGE = 01.
                                              IF SY-SUBRC <> 0.
                                                RETURN.
                                              ENDIF.
                                              APPEND VALUE #( SIGN = 'I'
                                                             OPTION = 'EQ'
                                                             LOW = LC_VBTYP_SVORDER ) TO LR_VBTYP_SV.
                                              APPEND VALUE #( SIGN = 'I'
                                                             OPTION = 'EQ'
                                                             LOW = LC_VBTYP_SVCONTR ) TO LR_VBTYP_SV.
                                              DELETE LT_DOCFLOW WHERE VBTYP_N NOT IN LR_VBTYP_SV.
                                              IF LT_DOCFLOW IS NOT INITIAL.
                                                SELECT OBJECT_ID,
                                                       ZZ1_CUS_PO
                                                INTO TABLE @DATA(LT_SV)
                                                FROM CRMS4D_SERV_H
                                                FOR ALL ENTRIES IN @LT_DOCFLOW
                                                WHERE OBJECT_ID = @LT_DOCFLOW-DOCNUM
                                                AND   ZZ1_CUS_PO <> ''.
                                                  LOOP AT LT_SV ASSIGNING FIELD-SYMBOL(<L_SV>).
                                                    IF C_BSTKD IS INITIAL.
                                                      C_BSTKD  = <L_SV>-ZZ1_CUS_PO.
                                                    ELSE.
                                                      C_BSTKD = |{ C_BSTKD }, { <L_SV>-ZZ1_CUS_PO }|.
                                                    ENDIF.
                                                  ENDLOOP.
                                                ENDIF.
                                              ENDMETHOD.
                                              METHOD READ_TEXT.
                                                DATA: LF_TDID     TYPE THEAD-TDID,
                                                      LF_TDSPRAS  TYPE THEAD-TDSPRAS,
                                                      LF_TDNAME   TYPE THEAD-TDNAME,
                                                      LF_TDOBJECT TYPE THEAD-TDOBJECT,
                                                      LT_TLINE    TYPE TABLE OF TLINE.

                                                LF_TDID = I_ID.
                                                LF_TDSPRAS = SY-LANGU.
                                                LF_TDNAME = I_NAME.
                                                LF_TDOBJECT = I_OBJECT.
                                                CALL FUNCTION 'READ_TEXT'
                                                  EXPORTING
                                                    CLIENT                  = SY-MANDT
                                                    ID                      = LF_TDID
                                                    LANGUAGE                = LF_TDSPRAS
                                                    NAME                    = LF_TDNAME
                                                    OBJECT                  = LF_TDOBJECT
                                                  TABLES
                                                    LINES                   = LT_TLINE
                                                  EXCEPTIONS
                                                    ID                      = 1
                                                    LANGUAGE                = 2
                                                    NAME                    = 3
                                                    NOT_FOUND               = 4
                                                    OBJECT                  = 5
                                                    REFERENCE_CHECK         = 6
                                                    WRONG_ACCESS_TO_ARCHIVE = 7
                                                    OTHERS                  = 8.
                                                IF SY-SUBRC = 0.
                                                  READ TABLE LT_TLINE INTO DATA(LS_TLINE) INDEX 1.
                                                  IF SY-SUBRC = 0.
                                                    C_TEXT  = LS_TLINE-TDLINE.
                                                  ENDIF.
                                                ENDIF.
                                              ENDMETHOD.
                                              METHOD NET_DUE_DATE_GET .

                                                CALL FUNCTION 'NET_DUE_DATE_GET'
                                                  EXPORTING
                                                    I_ZFBDT = I_ZFBDT
                                                    I_ZBD1T = I_ZBD1T
                                                    I_ZBD2T = I_ZBD2T
                                                    I_ZBD3T = I_ZBD3T
                                                    I_SHKZG = I_SHKZG
                                                    I_REBZG = I_REBZG
                                                  IMPORTING
                                                    E_FAEDT = C_DUEDT.


                                              ENDMETHOD.
METHOD SET_DAY_RANGE .

  LOOP AT GT_DAY_RANGE ASSIGNING FIELD-SYMBOL(<L_DAY_RANGE>).
    IF I_DAY IN <L_DAY_RANGE>-RANGE.
      C_DAY_RANGE = <L_DAY_RANGE>-DESC.
      EXIT.
    ENDIF.
  ENDLOOP.



ENDMETHOD.
METHOD SET_WITHIN_OR_OVER  .
  IF I_DAY <= 0.
    C_WITHIN_OR_OVER = 'Within Due'(V01).
  ELSE.
    C_WITHIN_OR_OVER = 'Over Due'(V02).
  ENDIF.


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
