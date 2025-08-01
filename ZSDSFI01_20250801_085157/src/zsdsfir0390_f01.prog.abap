*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0390_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_SEL_SCRN_SET_PROPERTIES
*&---------------------------------------------------------------------*
FORM F_SEL_SCRN_SET_PROPERTIES .
  LOOP AT SCREEN.
    CASE ABAP_TRUE.
      WHEN RB_BILL.
        IF SCREEN-GROUP1 = 'FI'.
          SCREEN-INPUT = 1.
          SCREEN-OUTPUT = 1.
          SCREEN-ACTIVE = 1.
        ELSEIF SCREEN-GROUP1 = 'DO'.
          SCREEN-INPUT = 0.
          SCREEN-OUTPUT = 1.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN RB_DO.
        IF SCREEN-GROUP1 = 'DO'.
          SCREEN-INPUT = 1.
          SCREEN-OUTPUT = 1.
          SCREEN-ACTIVE = 1.
        ELSEIF SCREEN-GROUP1 = 'FI'.
          SCREEN-INPUT = 0.
          SCREEN-OUTPUT = 1.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_GENC
*&---------------------------------------------------------------------*
FORM F_GET_GENC .
  DATA: LT_GEN_C   TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C,
        LT_RANGE_I TYPE TT_RANGE_I.

  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = SY-REPID
    IMPORTING
      ET_GEN_C = LT_GEN_C.
  LOOP AT LT_GEN_C ASSIGNING FIELD-SYMBOL(<L_GEN_C>).
    CASE <L_GEN_C>-PARAM.
*      WHEN 'DOCUMENT_TYPE'.
*        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
*                        OPTION =  <L_GEN_C>-PARAM_OPTION
*                        LOW    =  <L_GEN_C>-VALUE_LOW
*                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
*               INTO TABLE GRT_BLART.
*      WHEN 'DEBIT/CREDIT'.
*        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
*                        OPTION =  <L_GEN_C>-PARAM_OPTION
*                        LOW    =  <L_GEN_C>-VALUE_LOW
*                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
*               INTO TABLE GRT_SHKZG.
      WHEN 'OVERALL_STATUS'.
        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                        OPTION =  <L_GEN_C>-PARAM_OPTION
                        LOW    =  <L_GEN_C>-VALUE_LOW
                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
               INTO TABLE GRT_GBSTK.
      WHEN 'DAY_RANGE'.
        CLEAR LT_RANGE_I.
        INSERT VALUE #( SIGN   =  <L_GEN_C>-PARAM_SIGN
                        OPTION =  <L_GEN_C>-PARAM_OPTION
                        LOW    =  <L_GEN_C>-VALUE_LOW
                        HIGH   =  <L_GEN_C>-VALUE_HIGH )
               INTO TABLE LT_RANGE_I.


        INSERT VALUE #( SEQNO = <L_GEN_C>-SEQUENCE
                        RANGE = LT_RANGE_I
                        DESC = <L_GEN_C>-VDESC )
               INTO TABLE GT_DAY_RANGE.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INIT_VALUE
*&---------------------------------------------------------------------*
FORM F_INIT_VALUE .
  CONSTANTS: LC_ACTIVATE TYPE DD07T-AS4LOCAL VALUE 'A'.
  CASE ABAP_TRUE.
    WHEN RB_BILL.
      GF_EXEC_TY = GC_EXEC_TY-FI.
    WHEN RB_DO.
      GF_EXEC_TY = GC_EXEC_TY-DO.
  ENDCASE.

  IF P_RPDAT IS NOT INITIAL.
    INSERT VALUE #( SIGN = 'I'
                    OPTION = 'LE'
                    LOW = P_RPDAT
                    HIGH = '' )
           INTO TABLE GRT_RPDAT.
  ENDIF.

  SELECT SINGLE
         BUKRS,
         WAERS,
         KTOPL
  INTO @GS_T001
  FROM T001
  WHERE BUKRS IN @S_BUKRS ##WARN_OK.                    "#EC CI_NOORDER

  SELECT VALPOS,
         DOMVALUE_L,
         DDTEXT
  INTO TABLE @DATA(LT_DD07T)
  FROM DD07T
  WHERE DOMNAME = @GC_DOMNAME_ISLIST
  AND   DDLANGUAGE = @SY-LANGU
  AND   AS4LOCAL = @LC_ACTIVATE.

  SORT  LT_DD07T BY VALPOS.

  LOOP AT LT_DD07T ASSIGNING FIELD-SYMBOL(<L_DD07T>).
    INSERT VALUE #( VALUE = <L_DD07T>-DOMVALUE_L
                    TEXT =  <L_DD07T>-DDTEXT )
           INTO TABLE GT_ISLIST.
  ENDLOOP.

  IF CB_OPEN = ABAP_TRUE.
    IF CB_SPEC = ABAP_FALSE.
      CLEAR: S_UMSKZ[].
      INSERT VALUE #( SIGN = 'I'
                      OPTION = 'EQ'
                      LOW = ' '
                      HIGH = '' )
      INTO TABLE S_UMSKZ.
    ENDIF.
  ELSEIF CB_OPEN = ABAP_FALSE.
    IF CB_SPEC = ABAP_TRUE.
      INSERT VALUE #( SIGN = 'E'
                      OPTION = 'EQ'
                      LOW = ' '
                      HIGH = '' )
      INTO TABLE S_UMSKZ.
    ELSEIF CB_SPEC = ABAP_FALSE.
      CLEAR: S_UMSKZ[].
      INSERT VALUE #( SIGN = 'I'
                      OPTION = 'EQ'
                      LOW = ' '
                      HIGH = '' )
      INTO TABLE S_UMSKZ.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_AND_PREPARE_BILL
*&---------------------------------------------------------------------*
FORM F_SELECT_AND_PREPARE_BILL CHANGING CT_DATA1 TYPE TT_DATA1.
  DATA:
    LT_KNA1            TYPE TT_KNA1,
    LT_KNA1_027        TYPE TT_KNA1_027,
    LT_KNVV_027        TYPE TT_KNVV_027,
    LT_CUST_KEY        TYPE TT_CUST_KEY,
    LT_CUSTSD_KEY      TYPE TT_CUSTSD_KEY , "ch08+
    LT_FIDOC           TYPE TT_FIDOC,
    LT_PA0002_PIC      TYPE TT_PA0002,
    LT_PRPS            TYPE TT_PRPS,
    LT_VBKD_SO         TYPE TT_VBKD_SO,
    LT_040             TYPE TT_040,
    LT_024             TYPE TT_024,
    LT_033_035         TYPE TT_BILLPL,
    LT_029             TYPE TT_BILLPL,
    LT_038             TYPE TT_BILLPL,
    LT_SKAT            TYPE TT_SKAT,
    LT_T074T           TYPE TT_T074T,
    LT_VBELN_KEY       TYPE TT_VBELN_KEY,
    LT_VBPA            TYPE TT_VBPA,
    LT_PA0002          TYPE TT_PA0002,
    LT_CLEAR_DOC       TYPE TT_CLEAR_DOC,
    LT_VKGRP_KEY       TYPE TT_TVGRT,
    LT_VKBUR_KEY       TYPE TT_TVKBT,
    LT_TVGRT           TYPE TT_TVGRT,
    LT_TVKBT           TYPE TT_TVKBT,
    LT_KNB1            TYPE TT_KNB1,
    LT_CLEAR_CHK       TYPE TT_CLEAR_CHK,
    LT_BSEC            TYPE TT_BSEC, "CH04 420000327+
    LT_VBFA_INV_DO     TYPE TT_VBFA_INV_DO,  "CH04 420000327+
    LT_CLEAR_MGRTE     TYPE TT_CLEAR_MGRTE,  "CH04 420000219+
    LT_CLEAR_MGRTE_CHK TYPE TT_CLEAR_MGRTE_CHK. "CH04 420000219+
  DATA:
    LS_DATA1       TYPE TS_DATA1,
    LF_INDX        TYPE SY-TABIX,
    LF_DMBTR_CLR   TYPE BSID_VIEW-DMBTR,
    LF_TDNAME      TYPE THEAD-TDNAME,       "CH04 420000332+
    LF_COUNT_XBLNR TYPE I.          "CH06 420000219+
*BOI CH04 420000332
  CALL FUNCTION 'FREE_TEXT_MEMORY' ##FM_SUBRC_OK
    EXPORTING
      LOCAL_CAT = ' '
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.

*EOI CH04 420000332
  PERFORM F_SELECT_FI_DOC CHANGING LT_FIDOC.
  IF LT_FIDOC[] IS INITIAL.
    RETURN.
  ENDIF.
  MOVE-CORRESPONDING LT_FIDOC TO LT_CUST_KEY.
  DELETE ADJACENT DUPLICATES FROM LT_CUST_KEY COMPARING KUNNR.

  MOVE-CORRESPONDING LT_FIDOC TO LT_VKBUR_KEY ##ENH_OK.
  DELETE LT_VKBUR_KEY WHERE VKBUR IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM LT_VKBUR_KEY COMPARING VKBUR.

  IF LT_CUST_KEY IS INITIAL.
    RETURN.
  ENDIF.

*BOI CH08
  MOVE-CORRESPONDING LT_FIDOC TO LT_CUSTSD_KEY.
  DELETE LT_CUSTSD_KEY WHERE VBELN IS INITIAL.  "#EC CI_SORTSEQ
  DELETE ADJACENT DUPLICATES FROM LT_CUSTSD_KEY COMPARING KUNNR VKORG VTWEG SPART VKGRP VKBUR.
*EOI CH08

  PERFORM F_SELECT_KNA1    USING LT_CUST_KEY
                           CHANGING LT_KNA1.
  PERFORM F_SELECT_KNA1_027 USING LT_CUST_KEY
                            CHANGING LT_KNA1_027.
*BOD CH08
*  PERFORM F_SELECT_KNVV_027 USING LT_CUST_KEY
*                                  LT_VKBUR_KEY "CH07+
*                            CHANGING LT_KNVV_027.
*EOD CH08
*BOI CH08
  PERFORM F_SELECT_SD_027 USING LT_CUSTSD_KEY
                          CHANGING LT_KNVV_027.
*EOI CH08
  PERFORM F_SELECT_KNB1 USING LT_CUST_KEY
                        CHANGING LT_KNB1.
*BOI CH04 420000327
  PERFORM F_SELECT_BSEC USING LT_FIDOC
                        CHANGING LT_BSEC.
*EOI CH04 420000327
  PERFORM F_SELECT_CLEAR_DOC  USING   LT_FIDOC
                         CHANGING LT_CLEAR_DOC .
  LT_CLEAR_CHK = LT_CLEAR_DOC.  "CH02 IMS 420000219

*BOI CH04 420000219
  PERFORM F_SELECT_CLEAR_MIGRATE_DOC  USING   LT_FIDOC
                                      CHANGING LT_CLEAR_MGRTE .
  LT_CLEAR_MGRTE_CHK = LT_CLEAR_MGRTE.
*EOI CH04 420000219
  PERFORM F_SELECT_PA0002_PIC USING LT_KNA1_027
                                    LT_KNVV_027
                          CHANGING LT_PA0002_PIC.
  PERFORM F_SELECT_PRPS USING LT_FIDOC
                        CHANGING LT_PRPS.
  PERFORM F_SELECT_SO_DATA USING LT_FIDOC
                           CHANGING LT_VBKD_SO.
*BOI CH04 420000327
  PERFORM F_SELECT_VBFA_INV_DO USING LT_FIDOC
                               CHANGING LT_VBFA_INV_DO.
*EOI CH04 420000327
  PERFORM F_SELECT_ZSDSFIT040 USING LT_FIDOC
                              CHANGING LT_040.
  PERFORM F_SELECT_ZSDSFIC024 USING LT_CUST_KEY
                              CHANGING LT_024.
  PERFORM F_SELECT_ZSDSFIT033_035 USING LT_FIDOC
                                  CHANGING LT_033_035.
  PERFORM F_SELECT_ZSDSFIT029 USING LT_FIDOC
                              CHANGING LT_029.
  PERFORM F_SELECT_ZSDSFIT038 USING LT_FIDOC
                              CHANGING LT_038.
  PERFORM F_SELECT_GL_DATA USING LT_FIDOC
                           CHANGING LT_SKAT
                                    LT_T074T.
  MOVE-CORRESPONDING LT_FIDOC TO LT_VKGRP_KEY ##ENH_OK.
  DELETE LT_VKGRP_KEY WHERE VKGRP IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM LT_VKGRP_KEY COMPARING VKGRP.
  PERFORM F_SELECT_TVGRT USING LT_VKGRP_KEY
                         CHANGING LT_TVGRT.


  PERFORM F_SELECT_TVKBT USING LT_VKBUR_KEY
                         CHANGING LT_TVKBT.

  MOVE-CORRESPONDING LT_FIDOC[] TO LT_VBELN_KEY ##ENH_OK.
  DELETE LT_VBELN_KEY WHERE VBELN IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM LT_VBELN_KEY COMPARING VBELN.
  PERFORM F_SELECT_VBPA USING LT_VBELN_KEY
                        CHANGING LT_VBPA
                                 LT_PA0002.
*==========================prepare data =====================
  LOOP AT LT_FIDOC ASSIGNING FIELD-SYMBOL(<L_FIDOC>).
* BOI CH02 IMS 420000219
    READ TABLE LT_CLEAR_CHK TRANSPORTING NO FIELDS
                            WITH  KEY BUKRS = <L_FIDOC>-BUKRS
                                      BELNR = <L_FIDOC>-BELNR
                                      GJAHR = <L_FIDOC>-GJAHR
                                      BUZEI = <L_FIDOC>-BUZEI.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.
*EOI "CH02 IMS 420000219
*BOI CH04 420000219
    "migrate case
    IF <L_FIDOC>-BLART = GC_BLART_MIGRATE
    AND <L_FIDOC>-XREF1 <> <L_FIDOC>-XBLNR .
      READ TABLE LT_CLEAR_MGRTE_CHK INTO DATA(LS_MGRTE_CHK) WITH KEY
                                                             BUKRS = <L_FIDOC>-BUKRS
                                                             BELNR = <L_FIDOC>-BELNR
                                                             GJAHR = <L_FIDOC>-GJAHR
                                                             BUZEI = <L_FIDOC>-BUZEI.
      IF SY-SUBRC = 0.
        LF_COUNT_XBLNR = REDUCE I( INIT LF_I = 0 FOR LS_FIDOC IN LT_FIDOC WHERE ( XBLNR = LS_MGRTE_CHK-XREF1 ) NEXT LF_I = LF_I + 1 ). "#EC CI_SORTSEQ  "CH06+
        IF LF_COUNT_XBLNR > 1.
          DELETE LT_CLEAR_MGRTE WHERE BUKRS = <L_FIDOC>-BUKRS
                                AND   XREF1 = LS_MGRTE_CHK-XREF1.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

    ENDIF.
*EOI CH04 420000219
    CLEAR: LS_DATA1,
          LF_DMBTR_CLR.
*  --- CLERK
    READ TABLE LT_KNB1 INTO DATA(LS_KNB1) WITH KEY KUNNR = <L_FIDOC>-KUNNR
                                                   BUKRS = <L_FIDOC>-BUKRS.
    IF SY-SUBRC = 0.
      LS_DATA1-BUSAB = LS_KNB1-BUSAB.
    ELSE.
      IF S_BUSAB[] IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
*   ---SALEMAN
*   ---SALEMAN_NAME
    READ TABLE LT_VBPA INTO DATA(LS_VBPA) WITH KEY VBELN = <L_FIDOC>-VBELN.
    IF SY-SUBRC = 0.
      LS_DATA1-SALEMAN = LS_VBPA-PERNR.
      READ TABLE LT_PA0002 INTO DATA(LS_PA0002) WITH KEY PERNR = LS_VBPA-PERNR.
      IF SY-SUBRC = 0.
        LS_DATA1-SALEMAN_NAME = |{ LS_PA0002-VORNA } { LS_PA0002-NACHN }|.
      ENDIF.
    ELSE.
      IF S_SALEM[] IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
*   --- PIC_PERNR
    IF <L_FIDOC>-VBELN IS INITIAL.
      READ TABLE LT_KNA1_027 INTO DATA(LS_KNA1_027) WITH KEY KUNNR = <L_FIDOC>-KUNNR
                                                             BUSAB =  LS_DATA1-BUSAB.
      IF SY-SUBRC  = 0.
        LS_DATA1-PIC_PERNR = LS_KNA1_027-PIC_PERNR.
      ELSE.
        IF S_PIC[] IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
    ELSE.
*BOI CH08
*BOD CH07
      READ TABLE LT_KNVV_027 INTO DATA(LS_KNVV_027) WITH KEY KUNNR = <L_FIDOC>-KUNNR
                                                             VKORG = <L_FIDOC>-VKORG
                                                             VTWEG = <L_FIDOC>-VTWEG
                                                             SPART = <L_FIDOC>-SPART
                                                             VKBUR = <L_FIDOC>-VKBUR
                                                             VKGRP = <L_FIDOC>-VKGRP
                                                             BUSAB = LS_DATA1-BUSAB.
*EOD CH07
*EOI CH08
*BOD CH08
**BOI CH07
*      LOOP AT LT_KNVV_027 INTO DATA(LS_KNVV_027) WHERE KUNNR = <L_FIDOC>-KUNNR
*                                                 AND   VKORG = <L_FIDOC>-VKORG
*                                                 AND   VTWEG = <L_FIDOC>-VTWEG
*                                                 AND   SPART = <L_FIDOC>-SPART
*                                                 AND   VKBUR = <L_FIDOC>-VKBUR
*                                                 AND   FR_VKGRP <= <L_FIDOC>-VKGRP
*                                                 AND   TO_VKGRP >= <L_FIDOC>-VKGRP
*                                                 AND   BUSAB = LS_DATA1-BUSAB.
*        EXIT.
*      ENDLOOP.
**EOI CH07
*EOI CH08
      IF SY-SUBRC  = 0.
        LS_DATA1-PIC_PERNR = LS_KNVV_027-PIC_PERNR.
      ELSE.
        IF S_PIC[] IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

*   ---PIC_NAME1
    IF LS_DATA1-PIC_PERNR IS NOT INITIAL.
      READ TABLE LT_PA0002_PIC INTO LS_PA0002 WITH KEY PERNR = LS_DATA1-PIC_PERNR.
      IF SY-SUBRC = 0.
        LS_DATA1-PIC_NAME1 = |{ LS_PA0002-VORNA } { LS_PA0002-NACHN }|.
      ENDIF.
    ENDIF.

    LS_DATA1-VBELN_BILL = <L_FIDOC>-VBELN.
    LS_DATA1-BUKRS      = <L_FIDOC>-BUKRS.
    LS_DATA1-BELNR      = <L_FIDOC>-BELNR.
    LS_DATA1-GJAHR      = <L_FIDOC>-GJAHR.
    LS_DATA1-BUZEI      = <L_FIDOC>-BUZEI.
    LS_DATA1-XBLNR      = <L_FIDOC>-XBLNR.
    LS_DATA1-XREF1      = <L_FIDOC>-XREF1. "CH04 420000219+
    LS_DATA1-KUNNR      = <L_FIDOC>-KUNNR.

*   ---NAME1
    READ TABLE LT_KNA1 INTO DATA(LS_KNA1) WITH KEY KUNNR = <L_FIDOC>-KUNNR.
    IF SY-SUBRC = 0.
      LS_DATA1-KUNNR_NAME1 = LS_KNA1-NAME1.
*BOI CH04 420000327
      IF LS_KNA1-XCPDK = ABAP_TRUE.
        READ TABLE LT_BSEC INTO DATA(LS_BSEC) WITH KEY BUKRS = <L_FIDOC>-BUKRS
                                                       BELNR = <L_FIDOC>-BELNR
                                                       GJAHR = <L_FIDOC>-GJAHR
                                                       BUZEI = <L_FIDOC>-BUZEI.
        IF SY-SUBRC = 0.
          LS_DATA1-KUNNR_NAME1 = LS_BSEC-NAME1.
        ENDIF.
      ENDIF.
*EOI CH04 420000327
    ENDIF.

    LS_DATA1-SGTXT      = <L_FIDOC>-SGTXT.

*   --- POST1
    READ TABLE LT_PRPS INTO DATA(LS_PRPS) WITH KEY POSID = <L_FIDOC>-SGTXT ##WARN_OK.
    IF SY-SUBRC = 0.
      LS_DATA1-POST1 = LS_PRPS-POST1.
    ELSE.
      CLEAR LS_DATA1-SGTXT.
    ENDIF.

*   --- BSTKD
    READ TABLE LT_VBKD_SO INTO DATA(LS_VBKD_SO) WITH KEY VBELN = <L_FIDOC>-AUBEL.
    IF SY-SUBRC = 0.
      LS_DATA1-BSTKD = LS_VBKD_SO-BSTKD.
*BOI CH03
    ELSE.
      PERFORM F_GET_PO_FROM_SV_ORDER USING <L_FIDOC>-VBELN
                                     CHANGING LS_DATA1-BSTKD.
*EOI CH03
    ENDIF.

*   --- INV_REMARK
    IF <L_FIDOC>-VBELN IS NOT INITIAL.
      PERFORM F_READ_TEXT USING 'ZH09'
                                <L_FIDOC>-VBELN
                                'VBBK'
                          CHANGING LS_DATA1-INV_REMARK .

    ENDIF.
*BOI CH04 420000327
    READ TABLE LT_VBFA_INV_DO INTO DATA(LS_VBFA_INV_DO) WITH KEY VBELN =  <L_FIDOC>-VBELN.
    IF SY-SUBRC = 0.
      LS_DATA1-VBELN_DO = LS_VBFA_INV_DO-VBELV.
    ENDIF.
*EOI CH04 420000327
*    LS_DATA1-VBELN_DO  = <L_FIDOC>-VGBEL. "CH04 420000327-
    LS_DATA1-RPDAT     = P_RPDAT.
*    LS_DATA1-BUDAT     = <L_FIDOC>-BUDAT. "<<F36K909945--
    LS_DATA1-BUDAT     = <L_FIDOC>-BLDAT.  "<<F36K909945++

*   ---DUEDT1
    PERFORM F_NET_DUE_DATE_GET USING <L_FIDOC>-ZFBDT
                                     <L_FIDOC>-ZBD1T
                                     <L_FIDOC>-ZBD2T
                                     <L_FIDOC>-ZBD3T
                                     <L_FIDOC>-SHKZG
                                     <L_FIDOC>-REBZG
                               CHANGING LS_DATA1-DUEDT1.

    READ TABLE LT_033_035 INTO DATA(LS_BILLPL) WITH KEY BUKRS = <L_FIDOC>-BUKRS
                                                        BELNR = <L_FIDOC>-BELNR
                                                        GJAHR = <L_FIDOC>-GJAHR
                                                        BUZEI = <L_FIDOC>-BUZEI.
    IF SY-SUBRC <> 0.
      READ TABLE LT_029 INTO LS_BILLPL WITH KEY BUKRS = <L_FIDOC>-BUKRS
                                                BELNR = <L_FIDOC>-BELNR
                                                GJAHR = <L_FIDOC>-GJAHR
                                                BUZEI = <L_FIDOC>-BUZEI.
      IF SY-SUBRC <> 0.
        READ TABLE LT_038 INTO LS_BILLPL WITH KEY BUKRS = <L_FIDOC>-BUKRS
                                                  BELNR = <L_FIDOC>-BELNR
                                                  GJAHR = <L_FIDOC>-GJAHR
                                                  BUZEI = <L_FIDOC>-BUZEI.
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
*BOI CH04 420000332
    LF_TDNAME = <L_FIDOC>-BUKRS && <L_FIDOC>-BELNR && <L_FIDOC>-GJAHR && <L_FIDOC>-BUZEI.
    PERFORM F_READ_TEXT USING 'FI01'
                              LF_TDNAME
                              GC_TDOBJECT_DETAIL
                        CHANGING LS_DATA1-TEXT1.
    PERFORM F_READ_TEXT USING 'FI02'
                              LF_TDNAME
                              GC_TDOBJECT_DETAIL
                        CHANGING LS_DATA1-TEXT2.
    PERFORM F_READ_TEXT USING 'FI03'
                              LF_TDNAME
                              GC_TDOBJECT_DETAIL
                        CHANGING LS_DATA1-TEXT3.

*EOI CH04 420000332
*    ---DUEDT2
*    ---DUEDT3
*    ---ISSUE_LIST
*    ---FOLLOWDT
*    ---DETAIL1
*    ---DETAIL2
*    ---DETAIL3
*    ---UPD_DATE
*    ---UPD_TIME
*    ---UPD_USER

    READ TABLE LT_040 INTO DATA(LS_040) WITH KEY BUKRS = <L_FIDOC>-BUKRS
                                                 BELNR = <L_FIDOC>-BELNR
                                                 GJAHR = <L_FIDOC>-GJAHR
                                                 BUZEI = <L_FIDOC>-BUZEI.
    IF SY-SUBRC = 0.
      LS_DATA1-NEW_DUEDT2 =  LS_040-NEW_DUEDT2.
      LS_DATA1-NEW_DUEDT3 =  LS_040-NEW_DUEDT3.
      LS_DATA1-ISSUE_LIST =  LS_040-ISSUE_LIST.
      READ TABLE GT_ISLIST INTO DATA(LS_ISLIST) WITH KEY VALUE = LS_040-ISSUE_LIST.
      IF SY-SUBRC = 0.
        LS_DATA1-ISSUE_LIST_TX = LS_ISLIST-TEXT.
      ENDIF.
      LS_DATA1-FOLLOWDT =  LS_040-FOLLOWDT.
*      LS_DATA1-DETAIL1  =  LS_040-DETAIL1 .  "CH04 420000332-
*      LS_DATA1-DETAIL2  =  LS_040-DETAIL2.   "CH04 420000332-
*      LS_DATA1-DETAIL3  =  LS_040-DETAIL3.   "CH04 420000332-

      LS_DATA1-UPD_DATE =  LS_040-UPD_DATE.
      LS_DATA1-UPD_TIME =  LS_040-UPD_TIME.
      LS_DATA1-UPD_USER =  LS_040-UPD_USER.
      LS_DATA1-SEQNO = LS_040-SEQNO.
      LS_DATA1-EXIST = ABAP_TRUE.
    ELSE.
      IF S_FLWDT[] IS NOT INITIAL
      OR S_UPDDT[] IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE LT_024 INTO DATA(LS_024) WITH KEY KUNNR = <L_FIDOC>-KUNNR.
      IF SY-SUBRC = 0.
        LS_DATA1-NEW_DUEDT2 = LS_DATA1-DUEDT1 + LS_024-ZDAYS.
      ELSE.
        LS_DATA1-NEW_DUEDT2 = LS_DATA1-DUEDT1.
      ENDIF.
      LS_DATA1-NEW_DUEDT3   =  LS_BILLPL-BILLPL_PMTDT.
    ENDIF.

*   ---INV_AGE
    LS_DATA1-INV_AGE = LS_DATA1-RPDAT - LS_DATA1-BUDAT + 1.
*   ---RNG_INV_AGE
    PERFORM F_SET_DAY_RANGE USING LS_DATA1-INV_AGE
                            CHANGING  LS_DATA1-RNG_INV_AGE.
*   ---OVDUE1
*   ---WITHIN_DUE1
*   ---RNG_OVDUE1
    IF LS_DATA1-DUEDT1 IS NOT INITIAL.
      LS_DATA1-OVDUE1 = LS_DATA1-RPDAT - LS_DATA1-DUEDT1.
      PERFORM F_SET_WITHIN_OR_OVER USING LS_DATA1-OVDUE1
                                   CHANGING LS_DATA1-WITHIN_DUE1.
      PERFORM F_SET_DAY_RANGE USING LS_DATA1-OVDUE1
                              CHANGING  LS_DATA1-RNG_OVDUE1.
    ENDIF.
*   ---OVDUE2
*   ---WITHIN_DUE2
*   ---RNG_OVDUE2
    IF LS_DATA1-NEW_DUEDT2 IS NOT INITIAL.
      LS_DATA1-OVDUE2 = LS_DATA1-RPDAT - LS_DATA1-NEW_DUEDT2.
      PERFORM F_SET_WITHIN_OR_OVER USING LS_DATA1-OVDUE2
                                   CHANGING LS_DATA1-WITHIN_DUE2.
      PERFORM F_SET_DAY_RANGE USING LS_DATA1-OVDUE2
                              CHANGING  LS_DATA1-RNG_OVDUE2.
    ENDIF.
*   ---OVDUE3
*   ---WITHIN_DUE3
*   ---RNG_OVDUE3
    IF LS_DATA1-NEW_DUEDT3 IS NOT INITIAL.
      LS_DATA1-OVDUE3 = LS_DATA1-RPDAT - LS_DATA1-NEW_DUEDT3.
      PERFORM F_SET_WITHIN_OR_OVER USING LS_DATA1-OVDUE3
                                   CHANGING LS_DATA1-WITHIN_DUE3.
      PERFORM F_SET_DAY_RANGE USING LS_DATA1-OVDUE3
                              CHANGING  LS_DATA1-RNG_OVDUE3.
    ENDIF.

    LS_DATA1-DMBTR = <L_FIDOC>-DMBTR.
    IF <L_FIDOC>-SHKZG = GC_SHKZG_CREDIT.
      LS_DATA1-DMBTR = LS_DATA1-DMBTR * -1.
    ENDIF.
    "-- find clearing document
    READ TABLE LT_CLEAR_DOC TRANSPORTING NO FIELDS WITH KEY
                            BUKRS = <L_FIDOC>-BUKRS
                            REBZG = <L_FIDOC>-BELNR
                            REBZJ = <L_FIDOC>-GJAHR
                            REBZZ = <L_FIDOC>-BUZEI.
    IF SY-SUBRC = 0.
      DATA(LV_TABIX) = SY-TABIX.
      LOOP AT LT_CLEAR_DOC ASSIGNING FIELD-SYMBOL(<L_CLEAR_DOC>) FROM LV_TABIX.
        IF  <L_CLEAR_DOC>-BUKRS <> <L_FIDOC>-BUKRS
        OR  <L_CLEAR_DOC>-REBZG <> <L_FIDOC>-BELNR
        OR  <L_CLEAR_DOC>-REBZJ <> <L_FIDOC>-GJAHR
        OR  <L_CLEAR_DOC>-REBZZ <> <L_FIDOC>-BUZEI.
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

*  BOI CH04 420000219
    "-- migrate case
    IF <L_FIDOC>-BLART = GC_BLART_MIGRATE
    AND  <L_FIDOC>-XBLNR IS NOT INITIAL.
      READ TABLE LT_CLEAR_MGRTE TRANSPORTING NO FIELDS WITH KEY      ##WARN_OK
                                BUKRS = <L_FIDOC>-BUKRS
                                XREF1 = <L_FIDOC>-XBLNR.
      IF SY-SUBRC = 0.
        LV_TABIX  = SY-TABIX.
*BOI CH06  420000219
        "Check whether found the same XREF1 in other doc?
        LF_COUNT_XBLNR = REDUCE I( INIT LF_I = 0 FOR LS_FIDOC IN LT_FIDOC WHERE ( XBLNR = <L_FIDOC>-XBLNR ) NEXT LF_I = LF_I + 1 ). "#EC CI_SORTSEQ
        IF LF_COUNT_XBLNR <= 1.
*EOI CH06  420000219
          LOOP AT LT_CLEAR_MGRTE ASSIGNING FIELD-SYMBOL(<L_CLEAR_MGRTE>) FROM LV_TABIX.
            IF <L_CLEAR_MGRTE>-BUKRS <> <L_FIDOC>-BUKRS
            OR <L_CLEAR_MGRTE>-XREF1 <> <L_FIDOC>-XBLNR.
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
*  EOI CH04 420000219
    LS_DATA1-DMBTR_REMAIN  = LS_DATA1-DMBTR - LF_DMBTR_CLR.

    LS_DATA1-WAERS = GS_T001-WAERS.
*   ---HKONT_TX
    IF <L_FIDOC>-UMSKZ IS INITIAL.
      READ TABLE LT_SKAT INTO DATA(LS_SKAT) WITH KEY SAKNR = <L_FIDOC>-HKONT.
      IF SY-SUBRC = 0.
        LS_DATA1-HKONT_TX = LS_SKAT-TXT20.
      ENDIF.
    ELSE.
      READ TABLE LT_T074T INTO DATA(LS_T074T) WITH KEY SHBKZ = <L_FIDOC>-UMSKZ .
      IF SY-SUBRC = 0.
        LS_DATA1-HKONT_TX = LS_T074T-LTEXT.
      ENDIF.
    ENDIF.
    LS_DATA1-VKGRP = <L_FIDOC>-VKGRP.
    IF <L_FIDOC>-VKGRP IS NOT INITIAL.
      READ TABLE LT_TVGRT INTO DATA(LS_TVGRT) WITH KEY VKGRP = <L_FIDOC>-VKGRP.
      IF SY-SUBRC = 0.
        LS_DATA1-VKGRP_TX  = LS_TVGRT-BEZEI.
      ENDIF.
    ENDIF.
    LS_DATA1-VKBUR = <L_FIDOC>-VKBUR.
    IF <L_FIDOC>-VKBUR IS NOT INITIAL.
      READ TABLE LT_TVKBT INTO DATA(LS_TVKBT) WITH KEY VKBUR = <L_FIDOC>-VKBUR.
      IF SY-SUBRC = 0.
        LS_DATA1-VKBUR_TX  = LS_TVKBT-BEZEI.
      ENDIF.
    ENDIF.
    LS_DATA1-VKBUR = <L_FIDOC>-VKBUR.
    LS_DATA1-BILLPL_NO = LS_BILLPL-BILLPL_NO.
    LS_DATA1-BILLPL_DATE = LS_BILLPL-BILLPL_DATE.
    LS_DATA1-ZTERM = <L_FIDOC>-ZTERM.
    LS_DATA1-ZUONR = <L_FIDOC>-ZUONR.
    LS_DATA1-UMSKZ = <L_FIDOC>-UMSKZ.
    LF_INDX = LF_INDX + 1.
    LS_DATA1-INDX = LF_INDX.
    APPEND LS_DATA1 TO CT_DATA1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_KNA1_027
*&---------------------------------------------------------------------*
FORM F_SELECT_KNA1_027   USING UT_CUST_KEY TYPE TT_CUST_KEY
                         CHANGING CT_KNA1_027 TYPE TT_KNA1_027.
  IF UT_CUST_KEY[] IS INITIAL.
    RETURN.
  ENDIF.
  SELECT  A~KUNNR,
          A~BUSAB,
          B~PIC_PERNR,
          B~FR_KUNNR,
          B~TO_KUNNR
  INTO TABLE @CT_KNA1_027
  FROM KNB1 AS A LEFT OUTER JOIN ZSDSFIC027 AS B
  ON A~KUNNR >= B~FR_KUNNR
  AND A~KUNNR <= B~TO_KUNNR
  AND A~BUSAB >= B~FR_BUSAB
  AND A~BUSAB <= B~TO_BUSAB
  FOR ALL ENTRIES IN @UT_CUST_KEY
  WHERE A~BUKRS IN @S_BUKRS
  AND   A~KUNNR = @UT_CUST_KEY-KUNNR
  AND   B~VKORG = ''.

  IF S_PIC[] IS NOT INITIAL.
    DELETE CT_KNA1_027 WHERE PIC_PERNR NOT IN S_PIC.    "#EC CI_SORTSEQ
  ENDIF.
  IF S_BUSAB[] IS NOT INITIAL.
    DELETE CT_KNA1_027 WHERE BUSAB NOT IN S_BUSAB.      "#EC CI_SORTSEQ
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_kna1
*&---------------------------------------------------------------------*
FORM F_SELECT_KNA1  USING UT_CUST_KEY TYPE TT_CUST_KEY
                    CHANGING CT_KNA1 TYPE TT_KNA1.
  IF UT_CUST_KEY[] IS INITIAL.
    RETURN.
  ENDIF.
  SELECT KUNNR,
         NAME1,
         XCPDK "CH04 420000327+
  INTO TABLE @CT_KNA1
  FROM KNA1
  FOR  ALL ENTRIES IN @UT_CUST_KEY
  WHERE KUNNR = @UT_CUST_KEY-KUNNR.

ENDFORM.
*BOD CH08
**&---------------------------------------------------------------------*
**& Form f_select_knvv_027
**&---------------------------------------------------------------------*
*FORM F_SELECT_KNVV_027 USING UT_CUST_KEY TYPE TT_CUST_KEY
*                             UT_VKBUR_KEY TYPE TT_TVKBT  "CH07+
*                       CHANGING CT_KNVV_027 TYPE TT_KNVV_027.
*  DATA: LR_VKORG TYPE RANGE OF VBAK-VKORG.
*  DATA: LR_VKBUR TYPE RANGE OF VBAK-VKBUR. "CH07+
*  IF UT_CUST_KEY[] IS INITIAL.
*    RETURN.
*  ENDIF.
**BOI CH07
*  IF UT_VKBUR_KEY[] IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  LOOP AT UT_VKBUR_KEY INTO DATA(LS_VKBUR_KEY).
*    INSERT VALUE #( SIGN   =  'I'
*                    OPTION =  'EQ'
*                    LOW    =  LS_VKBUR_KEY-VKBUR
*                    HIGH   =  '' )
*           INTO TABLE LR_VKBUR.
*  ENDLOOP.
**EOI CH07
*  CASE GF_EXEC_TY.
*    WHEN GC_EXEC_TY-FI.
*      LR_VKORG = S_VKORGF[].
*    WHEN GC_EXEC_TY-DO.
*      LR_VKORG = S_VKORGD[].
*  ENDCASE.
*  SELECT  A~KUNNR,
*          A~VKORG,
*          A~VTWEG,
*          A~SPART,
**          A~VKBUR,  "CH07-
**          A~VKGRP,  "CH07-
*          B~VKBUR,    "CH07+
*          B~FR_VKGRP, "CH07+
*          B~TO_VKGRP, "CH07+
*          B~PIC_PERNR,
*          A2~BUSAB,
*          B~FR_KUNNR,
*          B~TO_KUNNR,
*          B~FR_BUSAB,
*          B~TO_BUSAB
*  INTO TABLE @CT_KNVV_027
*  FROM KNVV AS A  INNER JOIN KNB1 AS A2
*  ON   A~KUNNR = A2~KUNNR
**                   LEFT OUTER JOIN ZSDSFIC027 AS B "CH07-
*                  INNER JOIN ZSDSFIC027 AS B        "CH07+
*  ON A~KUNNR >= B~FR_KUNNR
*  AND A~KUNNR <= B~TO_KUNNR
*  AND A~VKORG = B~VKORG
*  AND A~VTWEG >= B~FR_VTWEG
*  AND A~VTWEG <= B~TO_VTWEG
*  AND A~SPART >= B~FR_SPART
*  AND A~SPART <= B~TO_SPART
**  AND A~VKBUR = B~VKBUR          "CH07-
**  AND A~VKGRP >= B~FR_VKGRP      "CH07-
**  AND A~VKGRP <= B~TO_VKGRP      "CH07-
*  AND A2~BUSAB >= B~FR_BUSAB
*  AND A2~BUSAB <= B~TO_BUSAB
*  FOR ALL ENTRIES IN @UT_CUST_KEY
*  WHERE A2~BUKRS IN @S_BUKRS
*  AND   A~KUNNR = @UT_CUST_KEY-KUNNR
*  AND   A~VKORG IN @LR_VKORG
*  AND   A~VTWEG IN @S_VTWEG
*  AND   A~SPART IN @S_SPART
**  AND   A~VKBUR IN @S_VKBUR "CH07-
*  AND   B~VKBUR IN @S_VKBUR "CH07+
*  AND   B~VKBUR IN @LR_VKBUR "CH07+
**  AND  A~VKGRP IN @S_VKGRP "CH07-
*  AND   A2~BUSAB IN @S_BUSAB.
*
*  IF S_PIC[] IS NOT INITIAL.
*    DELETE CT_KNVV_027 WHERE PIC_PERNR NOT IN S_PIC.    "#EC CI_SORTSEQ
*  ENDIF.
*
*
*ENDFORM.
*EOD CH08
*&---------------------------------------------------------------------*
*& Form f_select_fi_doc
*&---------------------------------------------------------------------*
FORM F_SELECT_FI_DOC CHANGING CT_FIDOC TYPE TT_FIDOC.
*  DATA:
*    LT_FIDOC2 TYPE TABLE OF TS_FIDOC,
*    LF_DMBTR  TYPE BSID_VIEW-DMBTR,
*    LS_FIDOC2 TYPE TS_FIDOC,
*    LF_NEW    TYPE FLAG.

  SELECT A~BUKRS,
         A~BELNR,
         A~GJAHR,
         A~BUZEI,
         A~VBELN,
         A~BLART,
         A~KUNNR,
         A~XBLNR,
         A~SGTXT,
         A~DMBTR,
         A~HKONT,
         A~UMSKZ,
         A~BUDAT,
         A~BLDAT, "<<F36K909945++
         A~ZFBDT,
         A~ZBD1T,
         A~ZBD2T,
         A~ZBD3T,
         A~SHKZG,
         A~REBZG,
         A~ZTERM,
         A~ZUONR,
         A~XREF1, "CH04 420000219+
         B~VKORG,
         B~VTWEG,
         B~SPART,
         C~POSNR,
         C~VKGRP,
         C~VKBUR,
         C~AUBEL,
         C~VGBEL
  FROM BKPF AS A0 INNER JOIN BSID_VIEW AS A
  ON   A0~BELNR = A~BELNR
                     LEFT OUTER JOIN VBRK AS B
  ON   A~VBELN = B~VBELN
                      LEFT OUTER JOIN VBRP AS C
  ON   A~VBELN = C~VBELN
  WHERE A~BUKRS IN @S_BUKRS
  AND   A~KUNNR IN @S_KUNNR
  AND   A~BELNR IN @S_BELNR
  AND   A~VBELN IN @S_BILL
*    AND   A~BLART IN @GRT_BLART
  AND   A~BLART IN @S_BLART
  AND   A~XBLNR IN @S_XBLNR
  AND   A~UMSKZ IN @S_UMSKZ
*    AND   A~SHKZG IN @GRT_SHKZG
  AND   A~BUDAT IN @GRT_RPDAT
*  AND   A0~XREVERSAL = ''  "CH02 420000219-
  AND   A0~BSTAT <> 'S'
*BOD CH02 IMS 420000219
*  AND   A~REBZG NOT IN ( SELECT BELNR                  "exclude payment doc
*                           FROM BSID_VIEW
*                           WHERE BELNR = A~REBZG
*                           AND   GJAHR = A~REBZJ )
**<<F36K909945 start ins
*  AND ( A~XZAHL <> 'X'
*        OR ( A~XZAHL = 'X' AND A~REBZG IN ( SELECT BELNR
*                                                            FROM BSAD_VIEW
*                                                            WHERE BELNR = A~REBZG
*                                                            AND   GJAHR = A~REBZJ
*                                                            AND AUGBL <> ''
*                                                            AND AUGDT < @P_RPDAT ) )
*        OR ( A~XZAHL = 'X' AND A~REBZG = '' ) )
**<<F36K909945 end ins
*EOD CH02 IMS 420000219
  INTO TABLE @DATA(LT_FIDOC).


  SELECT A~BUKRS,
         A~BELNR,
         A~GJAHR,
         A~BUZEI,
         A~VBELN,
         A~BLART,
         A~KUNNR,
         A~XBLNR,
         A~SGTXT,
         A~DMBTR,
         A~HKONT,
         A~UMSKZ,
         A~BUDAT,
         A~BLDAT, "<<F36K909945++
         A~ZFBDT,
         A~ZBD1T,
         A~ZBD2T,
         A~ZBD3T,
         A~SHKZG,
         A~REBZG,
         A~ZTERM,
         A~ZUONR,
         A~XREF1, "CH04 420000219+
         B~VKORG,
         B~VTWEG,
         B~SPART,
         C~POSNR,
         C~VKGRP,
         C~VKBUR,
         C~AUBEL,
         C~VGBEL
  FROM BKPF AS A0 INNER JOIN BSAD_VIEW AS A
  ON   A0~BELNR = A~BELNR
                     LEFT OUTER JOIN VBRK AS B
  ON   A~VBELN = B~VBELN
                      LEFT OUTER JOIN VBRP AS C
  ON   A~VBELN = C~VBELN
  WHERE A~BUKRS IN @S_BUKRS
  AND   A~KUNNR IN @S_KUNNR
  AND   A~BELNR IN @S_BELNR
  AND   A~VBELN IN @S_BILL
*    AND   A~BLART IN @GRT_BLART
  AND   A~BLART IN @S_BLART
  AND   A~XBLNR IN @S_XBLNR
  AND   A~UMSKZ IN @S_UMSKZ
*    AND   A~SHKZG IN @GRT_SHKZG
  AND   A~BUDAT IN @GRT_RPDAT
  AND   A~AUGDT > @P_RPDAT
*  AND   A0~XREVERSAL = ''  "CH02 420000219-
  AND   A0~BSTAT <> 'S'
*  AND   A~BELNR  <> A~AUGBL "CH06 420000219+
*BOD CH02 IMS 420000219
*  AND   A~XZAHL <> 'X' "<<F36K909945++
*  AND   A~REBZG NOT IN ( SELECT BELNR                  "exclude payment doc
*                           FROM BSAD_VIEW
*                           WHERE BELNR = A~REBZG
*                           AND   GJAHR = A~REBZJ )
*EOD CH02 IMS 420000219
  APPENDING TABLE @LT_FIDOC.


  SORT LT_FIDOC BY BUKRS BELNR GJAHR BUZEI VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_FIDOC COMPARING BUKRS BELNR GJAHR BUZEI VBELN.
  CT_FIDOC = LT_FIDOC .
*  "sum amount bsid-dmbtr per fi doc
*  LOOP AT LT_FIDOC INTO DATA(LS_FIDOC).
*    AT NEW GJAHR.
*      CLEAR LF_DMBTR.
*      LF_NEW = ABAP_TRUE.
*    ENDAT.
*    IF LF_NEW = ABAP_TRUE.
*      LS_FIDOC2 = LS_FIDOC.
*      CLEAR LF_NEW.
*    ENDIF.
*    IF LS_FIDOC-SHKZG = 'H'.
*      LF_DMBTR = LF_DMBTR - LS_FIDOC-DMBTR.
*    ELSE.
*      LF_DMBTR = LF_DMBTR + LS_FIDOC-DMBTR.
*    ENDIF.
*    AT END OF GJAHR.
*      LS_FIDOC2-DMBTR = LF_DMBTR.
*      APPEND LS_FIDOC2 TO LT_FIDOC2.
*    ENDAT.
*  ENDLOOP.

  IF S_VKORGF IS NOT INITIAL
  OR S_VTWEG IS NOT INITIAL
  OR S_SPART IS NOT INITIAL
  OR S_VKGRP IS NOT INITIAL
  OR S_VKBUR IS NOT INITIAL.
    DELETE CT_FIDOC WHERE VKORG NOT IN S_VKORGF
                    OR   VTWEG NOT IN S_VTWEG
                    OR   SPART NOT IN S_SPART
                    OR   VKGRP NOT IN S_VKGRP
                    OR   VKBUR NOT IN S_VKBUR.          "#EC CI_SORTSEQ
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_pa0002_PIC
*&---------------------------------------------------------------------*
FORM F_SELECT_PA0002_PIC  USING    UT_KNA1_027 TYPE TT_KNA1_027
                               UT_KNVV_027 TYPE TT_KNVV_027
                      CHANGING CT_PA0002 TYPE TT_PA0002.
  TYPES: BEGIN OF  LTY_PIC,
           PIC_PERNR TYPE ZSDSFIC027-PIC_PERNR,
         END OF LTY_PIC.
  DATA: LT_PIC TYPE TABLE OF LTY_PIC.
  MOVE-CORRESPONDING UT_KNA1_027 TO LT_PIC.
  MOVE-CORRESPONDING UT_KNVV_027 TO LT_PIC KEEPING TARGET LINES.
  SORT LT_PIC BY PIC_PERNR.
  DELETE LT_PIC WHERE PIC_PERNR IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM LT_PIC COMPARING PIC_PERNR.
  IF LT_PIC[] IS NOT INITIAL.
    SELECT PERNR,
           VORNA,
           NACHN
    INTO TABLE @CT_PA0002
    FROM PA0002
    FOR ALL ENTRIES IN @LT_PIC
    WHERE PERNR = @LT_PIC-PIC_PERNR
    AND   BEGDA <= @SY-DATUM
    AND   ENDDA >= @SY-DATUM.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_prps
*&---------------------------------------------------------------------*
FORM F_SELECT_PRPS  USING    PT_FIDOC TYPE TT_FIDOC
                    CHANGING CT_PRPS TYPE TT_PRPS.
  DATA: LT_FIDOC TYPE TABLE OF TS_FIDOC.
  LT_FIDOC[] = PT_FIDOC[].

  DELETE LT_FIDOC WHERE VBELN IS INITIAL
                  OR SGTXT IS INITIAL.
  SORT LT_FIDOC BY SGTXT.
  DELETE ADJACENT DUPLICATES FROM LT_FIDOC COMPARING SGTXT.
  IF LT_FIDOC[] IS NOT INITIAL.
    SELECT POSID,
           POST1
    INTO TABLE @CT_PRPS
    FROM PRPS
    FOR ALL ENTRIES IN @LT_FIDOC
    WHERE POSID = @LT_FIDOC-SGTXT(24).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_SO_DATA
*&---------------------------------------------------------------------*
FORM F_SELECT_SO_DATA  USING    UT_FIDOC TYPE TT_FIDOC
                       CHANGING CT_VBKD_SO TYPE TT_VBKD_SO.
  DATA: LT_FIDOC TYPE TABLE OF TS_FIDOC.

  LT_FIDOC[] = UT_FIDOC[].
  DELETE LT_FIDOC WHERE AUBEL IS INITIAL.
  SORT LT_FIDOC BY AUBEL.
  DELETE ADJACENT DUPLICATES FROM LT_FIDOC COMPARING AUBEL.
  IF LT_FIDOC[] IS NOT INITIAL.
    SELECT VBELN,
           BSTKD
    FROM VBKD
    FOR ALL ENTRIES IN @LT_FIDOC
    WHERE VBELN = @LT_FIDOC-AUBEL
    AND   BSTKD  <> ''
    AND   POSNR = '000000'
    INTO TABLE @CT_VBKD_SO.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_text
*&---------------------------------------------------------------------*
FORM F_READ_TEXT  USING    UF_ID   TYPE ANY
                           UF_NAME TYPE ANY
                           UF_OBJECT TYPE ANY
                  CHANGING CF_TEXT TYPE ANY.
  DATA: LF_TDID     TYPE THEAD-TDID,
        LF_TDSPRAS  TYPE THEAD-TDSPRAS,
        LF_TDNAME   TYPE THEAD-TDNAME,
        LF_TDOBJECT TYPE THEAD-TDOBJECT,
        LT_TLINE    TYPE TABLE OF TLINE.

  LF_TDID = UF_ID.
  LF_TDSPRAS = SY-LANGU.
  LF_TDNAME = UF_NAME.
  LF_TDOBJECT = UF_OBJECT.
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
      CF_TEXT  = LS_TLINE-TDLINE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_NET_DUE_DATE_GET
*&---------------------------------------------------------------------*
FORM F_NET_DUE_DATE_GET  USING    UF_ZFBDT TYPE BSID_VIEW-ZFBDT
                                  UF_ZBD1T TYPE BSID_VIEW-ZBD1T
                                  UF_ZBD2T TYPE BSID_VIEW-ZBD2T
                                  UF_ZBD3T TYPE BSID_VIEW-ZBD3T
                                  UF_SHKZG TYPE BSID_VIEW-SHKZG
                                  UF_REBZG TYPE BSID_VIEW-REBZG
                         CHANGING CF_DUEDT TYPE DATUM.
  CALL FUNCTION 'NET_DUE_DATE_GET'
    EXPORTING
      I_ZFBDT = UF_ZFBDT
      I_ZBD1T = UF_ZBD1T
      I_ZBD2T = UF_ZBD2T
      I_ZBD3T = UF_ZBD3T
      I_SHKZG = UF_SHKZG
      I_REBZG = UF_REBZG
    IMPORTING
      E_FAEDT = CF_DUEDT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_ZSDSFIT040
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIT040  USING    UT_FIDOC TYPE TT_FIDOC
                          CHANGING CT_ZSDSFIT040 TYPE TT_040.
  IF UT_FIDOC IS NOT INITIAL.
    SELECT *
    INTO TABLE @CT_ZSDSFIT040
    FROM ZSDSFIT040
    FOR ALL ENTRIES IN @UT_FIDOC
    WHERE BUKRS = @UT_FIDOC-BUKRS
    AND   BELNR = @UT_FIDOC-BELNR
    AND   GJAHR = @UT_FIDOC-GJAHR
    AND   BUZEI = @UT_FIDOC-BUZEI
    AND   FOLLOWDT IN @S_FLWDT
    AND   UPD_DATE IN @S_UPDDT
    AND   LATEST = @ABAP_TRUE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_ZSDSFIC024
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIC024  USING    UT_CUST_KEY TYPE TT_CUST_KEY
                          CHANGING CT_024 TYPE TT_024.
  IF UT_CUST_KEY[] IS NOT INITIAL.
    SELECT *
    INTO TABLE @CT_024
    FROM ZSDSFIC024
    FOR ALL ENTRIES IN @UT_CUST_KEY
    WHERE KUNNR = @UT_CUST_KEY-KUNNR.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_zsdsfit033_035
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIT033_035  USING    UT_FIDOC TYPE TT_FIDOC
                              CHANGING CT_033_035 TYPE TT_BILLPL.
  IF UT_FIDOC[] IS NOT INITIAL.
    SELECT A~BUKRS,
           B~BELNR,
           B~GJAHR,
           B~BUZEI,
           A~BILLPL_NO,
           A~BILLPL_DATE,
           A~BILLPL_PMTDT
    INTO TABLE @CT_033_035
    FROM ZSDSFIT033 AS A INNER JOIN ZSDSFIT035 AS B
    ON   A~BUKRS = B~BUKRS
    AND  A~BILLPL_NO = B~BILLPL_NO
    FOR ALL ENTRIES IN @UT_FIDOC
    WHERE A~BUKRS = @UT_FIDOC-BUKRS
    AND   B~BELNR = @UT_FIDOC-BELNR
    AND   B~GJAHR = @UT_FIDOC-GJAHR
    AND   B~BUZEI = @UT_FIDOC-BUZEI
    AND   A~DELFG = ''
    AND   B~DELFG = ''          ##TOO_MANY_ITAB_FIELDS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_ZSDSFIT029
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIT029 USING    UT_FIDOC TYPE TT_FIDOC
                              CHANGING CT_029 TYPE TT_BILLPL.
  IF UT_FIDOC[] IS NOT INITIAL.
    SELECT A~BUKRS,
           A~BELNR,
           A~GJAHR,
           A~BUZEI,
           A~BILLPL_NO,
           A~BILLPL_DATE,
           A~PAYMENT_DATE AS BILLPL_PMTDT,
           A~WORK_DATE     "CH02+
    INTO TABLE @CT_029
    FROM ZSDSFIT029 AS A
    FOR ALL ENTRIES IN @UT_FIDOC
    WHERE A~DATA_TYPE = ''
    AND   A~BUKRS = @UT_FIDOC-BUKRS
    AND   A~BELNR = @UT_FIDOC-BELNR
    AND   A~GJAHR = @UT_FIDOC-GJAHR
    AND   A~BUZEI = @UT_FIDOC-BUZEI
    AND   A~BILLPL_NO <> @SPACE. "CH02+
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_ZSDSFIT039
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIT038 USING    UT_FIDOC TYPE TT_FIDOC
                              CHANGING CT_038 TYPE TT_BILLPL.
  IF UT_FIDOC[] IS NOT INITIAL.
    SELECT A~BUKRS,
           A~BELNR,
           A~GJAHR,
           A~BUZEI,
           A~BILLPL_NO,
           A~BILLPL_DATE,
           A~PAYMENT_DATE AS BILLPL_PMTDT,
           A~WORK_DATE   "CH02+
    INTO TABLE @CT_038
    FROM ZSDSFIT038 AS A
    FOR ALL ENTRIES IN @UT_FIDOC
    WHERE A~DATA_TYPE = ''
    AND   A~BUKRS = @UT_FIDOC-BUKRS
    AND   A~BELNR = @UT_FIDOC-BELNR
    AND   A~GJAHR = @UT_FIDOC-GJAHR
    AND   A~BUZEI = @UT_FIDOC-BUZEI
    AND   A~BILLPL_NO <> @SPACE. "CH02+
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_day_range
*&---------------------------------------------------------------------*
FORM F_SET_DAY_RANGE  USING    UF_DAY TYPE I
                      CHANGING CF_DAY_RANGE TYPE TEXT20.
  LOOP AT GT_DAY_RANGE ASSIGNING FIELD-SYMBOL(<L_DAY_RANGE>).
    IF UF_DAY IN <L_DAY_RANGE>-RANGE.
      CF_DAY_RANGE = <L_DAY_RANGE>-DESC.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_within_or_over
*&---------------------------------------------------------------------*
FORM F_SET_WITHIN_OR_OVER  USING    UF_DAY TYPE I
                           CHANGING CF_WITHIN_OR_OVER TYPE TEXT20.
  IF UF_DAY <= 0.
    CF_WITHIN_OR_OVER = 'Within Due'(V01).
  ELSE.
    CF_WITHIN_OR_OVER = 'Over Due'(V02).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_gl_data
*&---------------------------------------------------------------------*
FORM F_SELECT_GL_DATA  USING    UT_FIDOC TYPE TT_FIDOC
                       CHANGING CT_SKAT TYPE TT_SKAT
                                CT_T074T TYPE TT_T074T.

  DATA: LT_FIDOC TYPE STANDARD TABLE OF TS_FIDOC.
  LT_FIDOC[] = UT_FIDOC[].
  SORT LT_FIDOC BY HKONT .
  DELETE ADJACENT DUPLICATES FROM LT_FIDOC COMPARING HKONT.
  IF LT_FIDOC[] IS NOT INITIAL.
    SELECT SAKNR,
           TXT20
    INTO TABLE @CT_SKAT
    FROM SKAT
    FOR ALL ENTRIES IN @LT_FIDOC
    WHERE SPRAS = @SY-LANGU
    AND   KTOPL = @GS_T001-KTOPL
    AND   SAKNR = @LT_FIDOC-HKONT.
  ENDIF.
  LT_FIDOC[] = UT_FIDOC[].
  SORT LT_FIDOC BY UMSKZ.
  DELETE LT_FIDOC WHERE UMSKZ = ''.
  DELETE ADJACENT DUPLICATES FROM LT_FIDOC COMPARING UMSKZ.
  IF LT_FIDOC[] IS NOT INITIAL.
    SELECT SHBKZ,
           LTEXT
    FROM T074T
    FOR ALL ENTRIES IN @LT_FIDOC
    WHERE SPRAS = @SY-LANGU
    AND   KOART = @GC_KOART_CUST
    AND   SHBKZ = @LT_FIDOC-UMSKZ
    INTO TABLE @CT_T074T.                               "#EC CI_NOORDER


  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_VBPA
*&---------------------------------------------------------------------*
FORM F_SELECT_VBPA  USING    UT_VBELN_KEY TYPE TT_VBELN_KEY
                    CHANGING CT_VBPA TYPE TT_VBPA
                             CT_PA0002 TYPE TT_PA0002.
  DATA: LT_VBPA TYPE STANDARD TABLE OF TS_VBPA.
  IF UT_VBELN_KEY[] IS NOT INITIAL.
    SELECT  VBELN,
            POSNR,
            PARVW,
            PERNR
    INTO TABLE @CT_VBPA
    FROM VBPA
    FOR ALL ENTRIES IN @UT_VBELN_KEY
    WHERE VBELN = @UT_VBELN_KEY-VBELN
    AND   PARVW = @GC_PARVW_SALESMAN
    AND   PERNR IN @S_SALEM.

    IF CT_VBPA[] IS NOT INITIAL.
      LT_VBPA[] = CT_VBPA[].
      SORT LT_VBPA BY PERNR.
      DELETE ADJACENT DUPLICATES FROM LT_VBPA COMPARING PERNR.
      SELECT PERNR,
             VORNA,
             NACHN
      INTO TABLE @CT_PA0002
      FROM PA0002
      FOR ALL ENTRIES IN @LT_VBPA
      WHERE PERNR = @LT_VBPA-PERNR
      AND   BEGDA <= @SY-DATUM
      AND   ENDDA >= @SY-DATUM.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_DATA TYPE ANY TABLE.
* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
  IF GF_EDIT EQ SPACE.
*   Enable Soft Refresh only in display mode
    GF_SOFT_REFRESH_1 = GC_TRUE.
  ELSE.
*   No auto refresh in edit mode
    GF_NO_AUTO_REFRESH_1 = GC_TRUE.
  ENDIF.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_DATA TO <G_LIST_1>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat, sort
  PERFORM F_ALV_BUILD_FIELDCAT    USING UT_DATA
                                  CHANGING GT_FIELDCAT_1.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-FI.
      PERFORM F_ALV_MODIFY_FC_FI   CHANGING GT_FIELDCAT_1.
    WHEN GC_EXEC_TY-DO.
      PERFORM F_ALV_MODIFY_FC_DO   CHANGING GT_FIELDCAT_1.
  ENDCASE.
* Call ALV Screen
  CALL SCREEN 9000.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT USING UT_DATA TYPE ANY TABLE
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

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'B'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.


* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
  CS_VARIANT-LOG_GROUP = GF_EXEC_TY.
  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_ALV_DISPLAY
*----------------------------------------------------------------------*
*       Display ALV Grid
*----------------------------------------------------------------------*
FORM F_ALV_DISPLAY.

* --------------------------------------------------
* Create First ALV Container
* --------------------------------------------------
  IF GREF_GRID_1 IS INITIAL.

*   Get First ALV Container
    PERFORM F_GET_CONTAINER_1  USING  'FIRST'
                                      GF_ALV_HEIGHT_1
                             CHANGING GREF_CONTAINER_GRID_1.

*   Create First ALV Object
    CREATE OBJECT GREF_GRID_1
      EXPORTING
        I_PARENT = GREF_CONTAINER_GRID_1.

*   Set Event Handler for First ALV
    CREATE OBJECT GREF_EVENT_RECEIVER_1.
    SET HANDLER:
      GREF_EVENT_RECEIVER_1->ON_PRINT_TOP_OF_PAGE_1  FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_HOTSPOT_CLICK_1      FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DOUBLE_CLICK_1       FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->HANDLE_TOOLBAR_1        FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->HANDLE_USER_COMMAND_1   FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DATA_CHANGED_1       FOR GREF_GRID_1.


*   Show First ALV
    CALL METHOD GREF_GRID_1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_BUFFER_ACTIVE               = 'X'
        IS_VARIANT                    = GS_VARIANT_1
        IS_LAYOUT                     = GS_LAYOUT_1
        IS_PRINT                      = GS_PRINT_1
        I_SAVE                        = GC_SAVE_ALL
        IT_TOOLBAR_EXCLUDING          = GT_TOOL_EXC_1
      CHANGING
        IT_OUTTAB                     = <G_LIST_1>
        IT_FIELDCATALOG               = GT_FIELDCAT_1
        IT_SORT                       = GT_SORT_1
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
    ENDIF.
  ELSE.

    IF GF_NO_AUTO_REFRESH_1 = SPACE.
      IF GF_SOFT_REFRESH_1 IS INITIAL.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_FIELDCATALOG
          EXPORTING
            IT_FIELDCATALOG = GT_FIELDCAT_1.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_LAYOUT
          EXPORTING
            IS_LAYOUT = GS_LAYOUT_1.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_PRINT
          EXPORTING
            IS_PRINT = GS_PRINT_1.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = ' '.
      ELSE.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

* --------------------------------------------------
* Create HTML Header Container if flag is marked
* --------------------------------------------------
  IF GF_ALV_HEADER_1 EQ 'X' AND
     CL_GUI_ALV_GRID=>OFFLINE( ) IS INITIAL.

    IF GREF_CONTAINER_HTML_1 IS INITIAL.

*     Get HTML Header Container
      PERFORM F_GET_CONTAINER_1  USING  'HEADER'
                                        GF_HEADER_HIGHT_1
                               CHANGING GREF_CONTAINER_HTML_1.

*     Bind TOP-OF-PAGE Event
      SET HANDLER:
        GREF_EVENT_RECEIVER_1->ON_TOP_OF_PAGE_1        FOR GREF_GRID_1.
*     Create TOP-Document
      CREATE OBJECT GREF_DYNDOC_ID_1
        EXPORTING
          STYLE = 'ALV_GRID'.
    ENDIF.

*   Initializing document
    CALL METHOD GREF_DYNDOC_ID_1->INITIALIZE_DOCUMENT.
*   Processing events
    CALL METHOD GREF_GRID_1->LIST_PROCESSING_EVENTS
      EXPORTING
        I_EVENT_NAME = 'TOP_OF_PAGE'
        I_DYNDOC_ID  = GREF_DYNDOC_ID_1.
*   Merge all setting
    CALL METHOD GREF_DYNDOC_ID_1->MERGE_DOCUMENT.
*   Display TOP document
    CALL METHOD GREF_DYNDOC_ID_1->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = GREF_CONTAINER_HTML_1
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.
    IF SY-SUBRC NE 0 ##NEEDED.
      "Do nothing
    ENDIF.

  ENDIF.

ENDFORM.                    " F_ALV_DISPLAY
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_EXIT
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_EXIT .
  DATA:
    LF_VALID   TYPE CHAR01,
    LF_REFRESH TYPE CHAR01,
    LF_ANS     TYPE CHAR01,
    LF_CHANGED TYPE FLAG.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).
*     Continue processing only when valid
  IF LF_VALID IS INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.
  PERFORM F_CHECK_CHANGED_DATA CHANGING LF_CHANGED.
  IF LF_CHANGED = ''.
    LEAVE TO SCREEN 0.
  ENDIF.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Please confirm'(M05)
      TEXT_QUESTION         = 'Data was changed. Save before exiting editor ?'(M06)
      TEXT_BUTTON_1         = 'Yes'(M07)
      ICON_BUTTON_1         = 'ICON_CHECKED'
      TEXT_BUTTON_2         = 'No'(M08)
      ICON_BUTTON_2         = 'ICON_CANCEL'
      DISPLAY_CANCEL_BUTTON = ' '
      POPUP_TYPE            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      ANSWER                = LF_ANS
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    LEAVE TO SCREEN 0.
  ELSE.
    IF LF_ANS = 1.
      PERFORM F_USER_COMMAND_SAVE.
      LEAVE TO SCREEN 0 .
    ELSE.
      LEAVE TO SCREEN 0 .
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
FORM F_CHECK_CHANGED_DATA CHANGING CF_CHANGED TYPE FLAG.
  DATA: LT_DATA1     TYPE TT_DATA1,
        LT_DATA1_OLD TYPE TT_DATA1,
        LT_DATA2     TYPE TT_DATA2,
        LT_DATA2_OLD TYPE TT_DATA2.
  LT_DATA1 = GT_DATA1.
  LT_DATA1_OLD = GT_DATA1_OLD.
  SORT LT_DATA1 BY BUKRS BELNR GJAHR.
  SORT LT_DATA1_OLD BY BUKRS BELNR GJAHR.

  LT_DATA2 = GT_DATA2.
  LT_DATA2_OLD = GT_DATA2_OLD.
  SORT LT_DATA2 BY VBELN_DO.
  SORT LT_DATA2_OLD BY VBELN_DO.

  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-FI.
      IF LT_DATA1[] <> LT_DATA1_OLD[].
        CF_CHANGED = GC_TRUE.
      ENDIF.
    WHEN GC_EXEC_TY-DO.
      IF LT_DATA2[] <> LT_DATA2_OLD[].
        CF_CHANGED = GC_TRUE.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE .
  DATA: LS_STABLE  TYPE LVC_S_STBL.
  DATA:
    LF_VALID   TYPE CHAR01  ##NEEDED,
    LF_REFRESH TYPE CHAR01  ##NEEDED.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).
  CASE GF_EXEC_TY .
    WHEN GC_EXEC_TY-FI.
      PERFORM F_USER_COMMAND_SAVE_DB_FI CHANGING GT_DATA1.
    WHEN GC_EXEC_TY-DO.
      PERFORM F_USER_COMMAND_SAVE_DB_DO CHANGING GT_DATA2.
  ENDCASE.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = ''.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = ''.
ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_lock_ZSDSFIT040
**&---------------------------------------------------------------------*
*FORM F_LOCK_ZSDSFIT040  USING    UT_DATA1 TYPE ANY TABLE .
*  TYPES: BEGIN OF LTS_LOCK,
*           BUKRS TYPE ZSDSFIT040-BUKRS,
*           BELNR TYPE ZSDSFIT040-BELNR,
*           GJAHR TYPE ZSDSFIT040-GJAHR,
*         END OF LTS_LOCK.
*  DATA: LT_LOCK  TYPE STANDARD TABLE OF  LTS_LOCK.
*  MOVE-CORRESPONDING UT_DATA1 TO LT_LOCK.
*  SORT LT_LOCK BY BUKRS BELNR GJAHR.
*  DELETE ADJACENT DUPLICATES FROM LT_LOCK COMPARING BUKRS BELNR GJAHR.
*
*  LOOP AT LT_LOCK INTO DATA(LS_LOCK).
*    CALL FUNCTION 'ENQUEUE_EZSDSFIT040'
*      EXPORTING
*        MODE_ZSDSFIT040 = 'E'
*        MANDT           = SY-MANDT
*        BUKRS           = LS_LOCK-BUKRS
*        BELNR           = LS_LOCK-BELNR
*        GJAHR           = LS_LOCK-GJAHR
*      EXCEPTIONS
*        FOREIGN_LOCK    = 1
*        SYSTEM_FAILURE  = 2
*        OTHERS          = 3.
*    IF SY-SUBRC <> 0.
*      DATA(LF_USRID) = SY-MSGV1.
*      MESSAGE S000(ZSDSCA01) WITH 'FI document'(m01) LS_LOCK-BELNR 'is locked by'(m02) LF_USRID
*      DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_unlock_ZSDSFIT040
**&---------------------------------------------------------------------*
*FORM F_UNLOCK_ZSDSFIT040  USING    UT_DATA1 TYPE ANY TABLE .
*  TYPES: BEGIN OF LTS_LOCK,
*           BUKRS TYPE ZSDSFIT040-BUKRS,
*           BELNR TYPE ZSDSFIT040-BELNR,
*           GJAHR TYPE ZSDSFIT040-GJAHR,
*         END OF LTS_LOCK.
*  DATA: LT_LOCK  TYPE STANDARD TABLE OF  LTS_LOCK.
*  MOVE-CORRESPONDING UT_DATA1 TO LT_LOCK.
*  SORT LT_LOCK BY BUKRS BELNR GJAHR.
*  DELETE ADJACENT DUPLICATES FROM LT_LOCK COMPARING BUKRS BELNR GJAHR.
*
*  LOOP AT LT_LOCK INTO DATA(LS_LOCK).
*    CALL FUNCTION 'DEQUEUE_EZSDSFIT040'
*      EXPORTING
*        MODE_ZSDSFIT040 = 'E'
*        MANDT           = SY-MANDT
*        BUKRS           = LS_LOCK-BUKRS
*        BELNR           = LS_LOCK-BELNR
*        GJAHR           = LS_LOCK-GJAHR.
*
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_FI
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC_FI CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'WAERS'
      OR   'INDX'
      OR   'EXIST'
      OR   'SEQNO'
      OR   'BUKRS'.
        <FCAT>-TECH = GC_TRUE.
      WHEN 'PIC_NAME1'.
        %FCAT 'PIC Name'(C02).
      WHEN 'BELNR'.
        %FCAT 'FI Doc'(C03).
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
        <FCAT>-HOTSPOT = GC_TRUE.
      WHEN 'VBELN_BILL'.
        <FCAT>-HOTSPOT = GC_TRUE.
      WHEN 'SGTXT'.
        %FCAT 'Project (WBS)'(C04).
      WHEN 'POST1'.
        %FCAT 'Project Name'(C05).
      WHEN 'BSTKD'.
        %FCAT 'PO No.'(C06).
      WHEN 'INV_REMARK'.
        %FCAT 'Invoice Remark'(C07).
      WHEN 'RPDAT'.
        %FCAT 'As of Date'(C08).
      WHEN 'BUDAT'.
        %FCAT 'Doc Date'(C09).
      WHEN 'DUEDT1'.
        %FCAT 'Std due date 1'(C10).
      WHEN 'NEW_DUEDT2'.
        %FCAT 'Net due date 2'(C11).
        <FCAT>-EDIT = ABAP_TRUE.
        <FCAT>-REF_TABLE  = 'ZSDSFIT040'.
        <FCAT>-REF_FIELD  = 'NEW_DUEDT2'.
      WHEN 'NEW_DUEDT3'.
        %FCAT 'Net due date 3'(C12).
        <FCAT>-EDIT = ABAP_TRUE.
        <FCAT>-REF_TABLE  = 'ZSDSFIT040'.
        <FCAT>-REF_FIELD  = 'NEW_DUEDT3'.
      WHEN 'INV_AGE'.
        %FCAT 'Aging Invoice'(C13).
      WHEN 'RNG_INV_AGE'.
        %FCAT 'Range Aging Invoice'(C14).
      WHEN 'OVDUE1'.
        %FCAT 'Over Due Date 1 std'(C15).
      WHEN 'OVDUE2'.
        %FCAT 'Over Due Date 2'(C16).
      WHEN 'OVDUE3'.
        %FCAT 'Over Due Date 3'(C17).
      WHEN 'WITHIN_DUE1'.
        %FCAT 'Within due/Overdue 1'(C18).
      WHEN 'RNG_OVDUE1'.
        %FCAT 'Range Overdue 1'(C19).
      WHEN 'WITHIN_DUE2'.
        %FCAT 'Within due/Overdue 2'(C20).
      WHEN 'RNG_OVDUE2'.
        %FCAT 'Range Overdue 2'(C21).
      WHEN 'WITHIN_DUE3'.
        %FCAT 'Within due/Overdue 3'(C22).
      WHEN 'RNG_OVDUE3'.
        %FCAT 'Range Overdue 3'(C23).
      WHEN 'DMBTR'.
        %FCAT 'Original Invoice Amount'(C35).
        <FCAT>-CFIELDNAME = 'WAERS'.
        <FCAT>-DO_SUM = ABAP_TRUE.
      WHEN 'DMBTR_REMAIN'.
        <FCAT>-CFIELDNAME = 'WAERS'.
        %FCAT 'Remaining Invoice Amount'(C32).
      WHEN 'HKONT_TX'.
        %FCAT 'GL.Desc'(C24).
      WHEN 'ISSUE_LIST'.
        <FCAT>-EDIT = ABAP_TRUE.
        <FCAT>-REF_TABLE  = 'ZSDSFIT040'.
        <FCAT>-REF_FIELD  = 'ISSUE_LIST'.
      WHEN 'ISSUE_LIST_TX'.
        %FCAT 'Issue list description'(C30).
      WHEN 'FOLLOWDT'.
        <FCAT>-EDIT = ABAP_TRUE.
        <FCAT>-REF_TABLE  = 'ZSDSFIT040'.
        <FCAT>-REF_FIELD  = 'FOLLOWDT'.
*BOI CH04 420000332
      WHEN 'TEXT1'.
        %FCAT 'Detail 1'(C36).
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-EMPHASIZE = 'C300'.  "CH05+
      WHEN 'TEXT2'.
        %FCAT 'Detail 2'(C37).
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-EMPHASIZE = 'C300'. "CH05+
      WHEN 'TEXT3'.
        %FCAT 'Detail 3'(C38).
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-EMPHASIZE = 'C300'. "CH05+
*EOI CH04 420000332
*BOD CH04 420000332
*      WHEN 'DETAIL1'.
*        <FCAT>-EDIT = ABAP_TRUE.
*        <FCAT>-REF_TABLE  = 'ZSDSFIT040'.
*        <FCAT>-REF_FIELD  = 'DETAIL1'.
*      WHEN 'DETAIL2'.
*        <FCAT>-EDIT = ABAP_TRUE.
*        <FCAT>-REF_TABLE  = 'ZSDSFIT040'.
*        <FCAT>-REF_FIELD  = 'DETAIL2'.
*      WHEN 'DETAIL3'.
*        <FCAT>-EDIT = ABAP_TRUE.
*        <FCAT>-REF_TABLE  = 'ZSDSFIT040'.
*        <FCAT>-REF_FIELD  = 'DETAIL3'.
*EOD CH04 420000332
      WHEN 'SALEMAN'.
        %FCAT 'Salesman'(C25).
      WHEN 'SALEMAN_NAME'.
        %FCAT 'Salesman Name'(C26).
      WHEN 'XBLNR'.
        %FCAT 'Tax Invoice No.'(C31).
      WHEN 'VKGRP_TX'.
        %FCAT 'Sales Group desc'(C33).
      WHEN 'VKBUR_TX'.
        %FCAT 'Sales Office desc'(C34).
    ENDCASE.
    CASE <FCAT>-FIELDNAME.
      WHEN 'PIC_PERNR'
      OR   'PIC_NAME1'
      OR   'VBELN_BILL'
      OR   'BUKRS'
      OR   'BELNR'
      OR   'GJAHR'
      OR   'XBLNR'.
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE_DB_FI
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE_DB_FI  CHANGING CT_DATA1 TYPE TT_DATA1.

  DATA: LS_MOD TYPE ZSDSFIT040,
        LT_MOD TYPE STANDARD TABLE OF ZSDSFIT040.
  DATA: LF_CHANGE_TX TYPE FLAG. "CH04 420000332
  DATA(LF_DATUM) = SY-DATUM.
  DATA(LF_UZEIT) = SY-UZEIT.
  SORT GT_DATA1_OLD BY INDX.
  LOOP AT CT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>).
*BOI CH04 420000332
    IF  <L_DATA1>-CHANGE_TX = ABAP_TRUE.
      LF_CHANGE_TX = ABAP_TRUE.
*      CLEAR <L_DATA1>-CHANGE_TX. "CH06 420000332 LONG TEXT MUST UPDATE TABLE -
    ENDIF.
*EOI CH04 420000332
    READ TABLE GT_DATA1_OLD INTO DATA(LS_OLD) WITH KEY INDX = <L_DATA1>-INDX
                                                       BINARY SEARCH.
    IF SY-SUBRC <> 0.
*    OR LS_OLD = <L_DATA1>. "CH04 420000332-
      CONTINUE.
*BOI CH04 420000332
    ELSE.
*BOD CH06 420000332 LONG TEXT MUST UPDATE TABLE
*      CLEAR: LS_OLD-TEXT1,
*             LS_OLD-TEXT2,
*             LS_OLD-TEXT3.
*      DATA(LS_DATA1) = <L_DATA1>.
*      CLEAR: LS_DATA1-TEXT1,
*             LS_DATA1-TEXT2,
*             LS_DATA1-TEXT3.
*      IF LS_OLD = LS_DATA1
*EOD CH06 420000332 LONG TEXT MUST UPDATE TABLE
*BOI CH06 420000332 LONG TEXT MUST UPDATE TABLE
      IF  LS_OLD = <L_DATA1>
      AND <L_DATA1>-CHANGE_TX = ABAP_FALSE.
*EOI CH06 - LONG TEXT MUST UPDATE TABLE
        CONTINUE.
      ENDIF.
*EOI CH04 420000332
    ENDIF.
    IF <L_DATA1>-EXIST = ABAP_TRUE.
      CLEAR LS_MOD.
      MOVE-CORRESPONDING LS_OLD TO LS_MOD.
      CLEAR LS_MOD-LATEST.
      APPEND LS_MOD TO LT_MOD.

      CLEAR LS_MOD.
      MOVE-CORRESPONDING <L_DATA1> TO LS_MOD.
      LS_MOD-LATEST = ABAP_TRUE.
      LS_MOD-SEQNO = <L_DATA1>-SEQNO + 1.
      LS_MOD-UPD_DATE = LF_DATUM.
      LS_MOD-UPD_TIME = LF_UZEIT.
      LS_MOD-UPD_USER = SY-UNAME.
      APPEND LS_MOD TO LT_MOD.
    ELSE.
      CLEAR LS_MOD.
      MOVE-CORRESPONDING <L_DATA1> TO LS_MOD.
      LS_MOD-LATEST = ABAP_TRUE.
      LS_MOD-SEQNO =  1.
      LS_MOD-UPD_DATE = LF_DATUM.
      LS_MOD-UPD_TIME = LF_UZEIT.
      LS_MOD-UPD_USER = SY-UNAME.
      APPEND LS_MOD TO LT_MOD.
    ENDIF.
    MOVE-CORRESPONDING LS_MOD TO <L_DATA1>.
    <L_DATA1>-EXIST = ABAP_TRUE.
    CLEAR <L_DATA1>-CHANGE_TX. "CH06 - LONG TEXT MUST UPDATE TABLE +
  ENDLOOP.
  IF LT_MOD[] IS NOT INITIAL.
    MODIFY ZSDSFIT040 FROM TABLE LT_MOD.
  ENDIF.

*BOI CH04 420000332
  IF LF_CHANGE_TX = ABAP_TRUE.
    CALL FUNCTION 'COMMIT_TEXT'
      EXPORTING
        OBJECT            = GC_TDOBJECT_DETAIL
        NAME              = '*'
        ID                = '*'
        LANGUAGE          = '*'
        SAVEMODE_DIRECT   = 'X'
        KEEP              = ' '
        LOCAL_CAT         = ' '
        KEEP_LAST_CHANGED = ' '.

  ENDIF.
*EOI CH04 420000332

  COMMIT WORK AND WAIT.
*BOI CH04 420000332
  CALL FUNCTION 'FREE_TEXT_MEMORY' ##FM_SUBRC_OK
    EXPORTING
      LOCAL_CAT = ' '
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.

*BOI CH04 420000332



  GT_DATA1_OLD[] = CT_DATA1[].

* Follow-up status has been created sucessfully
  MESSAGE S013(ZSDSFI01).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_AND_PREPARE_DO
*&---------------------------------------------------------------------*
FORM F_SELECT_AND_PREPARE_DO  CHANGING CT_DATA2 TYPE TT_DATA2.
  DATA:
    LT_KNA1         TYPE TT_KNA1,
    LT_KNA1_027     TYPE TT_KNA1_027,
    LT_KNVV_027     TYPE TT_KNVV_027,
    LT_CUST_KEY     TYPE TT_CUST_KEY,
    LT_CUSTSD_KEY      TYPE TT_CUSTSD_KEY , "ch08+
    LT_DO           TYPE TT_DO,
    LT_PRPS         TYPE TT_PRPS2,
    LT_041          TYPE TT_041,
    LT_SO_KEY       TYPE TT_VBELN_KEY,
    LT_VBPA         TYPE TT_VBPA,
    LT_PA0002       TYPE TT_PA0002,
    LT_PA0002_PIC   TYPE TT_PA0002,
    LT_VBKD_SO      TYPE TT_VBKD_SO,
    LT_VBFA_DO_BILL TYPE TT_VBFA,
    LT_BILL         TYPE TT_BILL,
    LT_033_035      TYPE TT_033_035_2,
    LT_029          TYPE TT_033_035_2,
    LT_038          TYPE TT_033_035_2,
    LT_VKGRP_KEY    TYPE TT_TVGRT,
    LT_VKBUR_KEY    TYPE TT_TVKBT,
    LT_TVGRT        TYPE TT_TVGRT,
    LT_TVKBT        TYPE TT_TVKBT,
    LT_KNB1         TYPE TT_KNB1,
    LT_CUST_ADRC    TYPE TT_CUST_ADRC. "CH04 420000327
  DATA:
    LS_DATA2     TYPE TS_DATA2,
    LF_INDX      TYPE SY-TABIX,
    LF_END       TYPE FLAG,
    LF_NETWR_SUM TYPE P LENGTH 16 DECIMALS 4,
    LF_TDNAME    TYPE THEAD-TDNAME.       "CH04 420000332+

*BOI CH04 420000332
  CALL FUNCTION 'FREE_TEXT_MEMORY' ##FM_SUBRC_OK
    EXPORTING
      LOCAL_CAT = ' '
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.
*EOI CH04 420000332
  PERFORM F_SELECT_DO_DATA CHANGING LT_DO.
  IF LT_DO[] IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT LT_DO ASSIGNING FIELD-SYMBOL(<L_DO>).
*    INSERT VALUE #( KUNNR = <L_DO>-KUNAG ) "CH04 420000327-
    INSERT VALUE #( KUNNR = <L_DO>-KUNNR ) "CH04 420000327+
    INTO TABLE LT_CUST_KEY.

    INSERT VALUE #( VBELN = <L_DO>-VBELN_SO )
    INTO TABLE LT_SO_KEY.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM LT_CUST_KEY COMPARING KUNNR.
  DELETE ADJACENT DUPLICATES FROM LT_SO_KEY COMPARING VBELN.

  IF LT_CUST_KEY IS INITIAL.
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING LT_DO TO LT_VKBUR_KEY ##ENH_OK.
  DELETE LT_VKBUR_KEY WHERE VKBUR IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM LT_VKBUR_KEY COMPARING VKBUR.
  PERFORM F_SELECT_KNA1    USING LT_CUST_KEY
                           CHANGING LT_KNA1.
*BOI CH08
  MOVE-CORRESPONDING LT_DO TO LT_CUSTSD_KEY.
  DELETE ADJACENT DUPLICATES FROM LT_CUSTSD_KEY COMPARING KUNNR VKORG VTWEG SPART VKGRP VKBUR.
*EOI CH08
*BOD CH08
*  PERFORM F_SELECT_KNVV_027 USING LT_CUST_KEY
*                                  LT_VKBUR_KEY "CH07+
*                            CHANGING LT_KNVV_027.
*EOD CH08
*BOI CH08
  PERFORM F_SELECT_SD_027 USING LT_CUSTSD_KEY
                          CHANGING LT_KNVV_027.
*EOI CH08
  PERFORM F_SELECT_KNB1 USING LT_CUST_KEY
                        CHANGING LT_KNB1.

  PERFORM F_SELECT_PRPS2 USING LT_DO
                         CHANGING LT_PRPS.
  PERFORM F_SELECT_ZSDSFIT041  USING    LT_DO
                               CHANGING LT_041 .

  PERFORM F_SELECT_PA0002_PIC USING LT_KNA1_027
                                    LT_KNVV_027
                          CHANGING LT_PA0002_PIC.
  PERFORM F_SELECT_BILL_DATA USING LT_DO
                             CHANGING LT_VBFA_DO_BILL
                                      LT_BILL.
  PERFORM F_SELECT_ZSDSFIT033_035_2 USING LT_BILL
                                    CHANGING LT_033_035.
  PERFORM F_SELECT_ZSDSFIT029_2 USING LT_BILL
                                CHANGING LT_029.
  PERFORM F_SELECT_ZSDSFIT038_2 USING LT_BILL
                              CHANGING LT_038.
*BOI CH04 420000327
  PERFORM F_SELECT_CUST_ADRC USING LT_DO
                             CHANGING LT_CUST_ADRC.
*EOI CH04 420000327
  MOVE-CORRESPONDING LT_DO TO LT_VKGRP_KEY ##ENH_OK.
  DELETE LT_VKGRP_KEY WHERE VKGRP IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM LT_VKGRP_KEY COMPARING VKGRP.
  PERFORM F_SELECT_TVGRT USING LT_VKGRP_KEY
                         CHANGING LT_TVGRT.


  PERFORM F_SELECT_TVKBT USING LT_VKBUR_KEY
                         CHANGING LT_TVKBT.



  PERFORM F_SELECT_VBPA USING LT_SO_KEY
                        CHANGING LT_VBPA
                                 LT_PA0002.
  PERFORM F_SELECT_SO_DATA2 USING LT_SO_KEY
                         CHANGING LT_VBKD_SO .
  LOOP AT LT_DO ASSIGNING <L_DO>.
    CLEAR: LS_DATA2,
           LF_END .
    AT NEW VBELN_DO.
      CLEAR: LF_END,
             LF_NETWR_SUM.
    ENDAT.
    IF <L_DO>-KWMENG_SO <> 0.
      LF_NETWR_SUM = LF_NETWR_SUM + ( <L_DO>-NETWR_SO /  <L_DO>-KWMENG_SO * <L_DO>-LFIMG ).
    ENDIF.
    AT END OF VBELN_DO.
      LF_END = ABAP_TRUE.
    ENDAT.
    "assign value at last record of each DO ( display report only 1 line per DO)
    IF LF_END = ABAP_FALSE.
      CONTINUE.
    ENDIF.
*BOI CH04 420000332
    LF_TDNAME = <L_DO>-VBELN_DO.
    PERFORM F_READ_TEXT USING 'DO01'
                              LF_TDNAME
                              GC_TDOBJECT_DETAIL
                        CHANGING LS_DATA2-TEXT1.
    PERFORM F_READ_TEXT USING 'DO02'
                              LF_TDNAME
                              GC_TDOBJECT_DETAIL
                        CHANGING LS_DATA2-TEXT2.
    PERFORM F_READ_TEXT USING 'DO03'
                              LF_TDNAME
                              GC_TDOBJECT_DETAIL
                        CHANGING LS_DATA2-TEXT3.

*EOI CH04 420000332
*   ---CMPFG
*   ---ISSUE_LIST
*   ---FOLLOWDT
*   ---DETAIL1
*   ---DETAIL2
*   ---DETAIL3
*   ---UPD_DATE
*   ---UPD_TIME
*   ---UPD_USER
*   ---SEQNO
*   ---EXIST
    READ TABLE LT_041 INTO DATA(LS_041) WITH KEY VBELN = <L_DO>-VBELN_DO.
    IF SY-SUBRC = 0.

      IF P_COMP = ABAP_FALSE
      AND LS_041-CMPFG = ABAP_TRUE.
        CONTINUE.
      ENDIF.
      LS_DATA2-CMPFG = LS_041-CMPFG.
      LS_DATA2-ISSUE_LIST = LS_041-ISSUE_LIST.
      READ TABLE GT_ISLIST INTO DATA(LS_ISLIST) WITH KEY VALUE = LS_041-ISSUE_LIST.
      IF SY-SUBRC = 0.
        LS_DATA2-ISSUE_LIST_TX = LS_ISLIST-TEXT.
      ENDIF.
      LS_DATA2-FOLLOWDT  = LS_041-FOLLOWDT.
*BOI CH04 420000332
*      LS_DATA2-DETAIL1   = LS_041-DETAIL1.
*      LS_DATA2-DETAIL2   = LS_041-DETAIL2.
*      LS_DATA2-DETAIL3   = LS_041-DETAIL3.
*EOI CH04 420000332
      LS_DATA2-UPD_DATE  = LS_041-UPD_DATE.
      LS_DATA2-UPD_TIME  = LS_041-UPD_TIME.
      LS_DATA2-UPD_USER  = LS_041-UPD_USER.
      LS_DATA2-SEQNO     = LS_041-SEQNO.
      LS_DATA2-EXIST     = ABAP_TRUE.
    ELSE.
      IF S_FLWDT[] IS NOT INITIAL
      OR S_UPDDT[] IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
*   ---SALEMAN
*   ---SALEMAN_NAME
    READ TABLE LT_VBPA INTO DATA(LS_VBPA) WITH KEY VBELN = <L_DO>-VBELN_SO.
    IF SY-SUBRC = 0.
      LS_DATA2-SALEMAN = LS_VBPA-PERNR.
      READ TABLE LT_PA0002 INTO DATA(LS_PA0002) WITH KEY PERNR = LS_VBPA-PERNR.
      IF SY-SUBRC = 0.
        LS_DATA2-SALEMAN_NAME = |{ LS_PA0002-VORNA } { LS_PA0002-NACHN }|.
      ENDIF.
    ELSE.
      IF S_SALEM[] IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
*  --- CLERK
    READ TABLE LT_KNB1 INTO DATA(LS_KNB1) WITH KEY KUNNR = <L_DO>-KUNNR. "KUNAG. "CH04 420000327+-
    IF SY-SUBRC = 0.
      LS_DATA2-BUSAB = LS_KNB1-BUSAB.
    ELSE.
      CLEAR LS_KNB1.
    ENDIF.
*    ---PIC_PERNR
*BOD CH07
*    READ TABLE LT_KNVV_027 INTO DATA(LS_KNVV_027) WITH KEY KUNNR = <L_DO>-KUNNR "KUNAG "CH04 420000327+-
*                                                           VKORG = <L_DO>-VKORG
*                                                           VTWEG = <L_DO>-VTWEG
*                                                           SPART = <L_DO>-SPART
*                                                           VKBUR = <L_DO>-VKBUR
*                                                           VKGRP = <L_DO>-VKGRP
*                                                           BUSAB = LS_DATA2-BUSAB.
*EOD CH07
*BOI CH07
    LOOP AT LT_KNVV_027 INTO DATA(LS_KNVV_027) WHERE KUNNR = <L_DO>-KUNNR
                                               AND   VKORG = <L_DO>-VKORG
                                               AND   VTWEG = <L_DO>-VTWEG
                                               AND   SPART = <L_DO>-SPART
                                               AND   VKBUR = <L_DO>-VKBUR
                                               AND   FR_VKGRP <= <L_DO>-VKGRP
                                               AND   TO_VKGRP >= <L_DO>-VKGRP
                                               AND   BUSAB = LS_DATA2-BUSAB.
      EXIT.
    ENDLOOP.
*EOI CH07
    IF SY-SUBRC  = 0.
      LS_DATA2-PIC_PERNR = LS_KNVV_027-PIC_PERNR.
    ELSE.
      IF S_PIC[] IS NOT INITIAL
      OR S_BUSAB[] IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
*   ---PIC_NAME1
    IF LS_DATA2-PIC_PERNR IS NOT INITIAL.
      READ TABLE LT_PA0002_PIC INTO LS_PA0002 WITH KEY PERNR = LS_DATA2-PIC_PERNR.
      IF SY-SUBRC = 0.
        LS_DATA2-PIC_NAME1 = |{ LS_PA0002-VORNA } { LS_PA0002-NACHN }|.
      ENDIF.
    ENDIF.
    LS_DATA2-VBELN_DO = <L_DO>-VBELN_DO.
    LS_DATA2-KUNNR    = <L_DO>-KUNNR. "KUNAG. "CH04 420000327+-
*   ---KUNNR_NAME1
    READ TABLE LT_KNA1 INTO DATA(LS_KNA1) WITH KEY KUNNR = <L_DO>-KUNNR. "KUNAG. "CH04 420000327+-
    IF SY-SUBRC = 0.
      LS_DATA2-KUNNR_NAME1 = LS_KNA1-NAME1.
*BOI CH04 420000327
      IF LS_KNA1-XCPDK = ABAP_TRUE.
        READ TABLE LT_CUST_ADRC INTO DATA(LS_CUST_ADRC) WITH KEY ADDRNUMBER =  <L_DO>-ADRNR.
        IF SY-SUBRC = 0.
          LS_DATA2-KUNNR_NAME1 = LS_CUST_ADRC-NAME1.
        ENDIF.
      ENDIF.
    ENDIF.
    LS_DATA2-PSPHI = <L_DO>-PSPHI.
*   ---POSID
    READ TABLE LT_PRPS INTO DATA(LS_PRPS) WITH KEY PSPNR = <L_DO>-PSPHI.
    IF SY-SUBRC = 0.
      LS_DATA2-POST1 = LS_PRPS-POST1.
    ENDIF.
*   ---BSTKD
    READ TABLE LT_VBKD_SO INTO DATA(LS_VBKD_SO) WITH KEY VBELN = <L_DO>-VBELN_SO.
    IF SY-SUBRC = 0.
      LS_DATA2-BSTKD = LS_VBKD_SO-BSTKD.
    ENDIF.
*   ---INV_REMARK
    PERFORM F_READ_TEXT USING 'ZH09'
                              <L_DO>-VBELN_DO
                              'VBBK'
                        CHANGING LS_DATA2-INV_REMARK .
    LS_DATA2-RPDAT     = P_RPDAT.
    LS_DATA2-BLDAT     = <L_DO>-BLDAT.

*   ---DO_AGE
    LS_DATA2-DO_AGE = LS_DATA2-RPDAT - LS_DATA2-BLDAT + 1.
*   ---RNG_DO_AGE
    PERFORM F_SET_DAY_RANGE USING LS_DATA2-DO_AGE
                            CHANGING  LS_DATA2-RNG_DO_AGE.
    LS_DATA2-NETWR = LF_NETWR_SUM.
    LS_DATA2-VKGRP = <L_DO>-VKGRP.
    IF <L_DO>-VKGRP IS NOT INITIAL.
      READ TABLE LT_TVGRT INTO DATA(LS_TVGRT) WITH KEY VKGRP = <L_DO>-VKGRP.
      IF SY-SUBRC = 0.
        LS_DATA2-VKGRP_TX  = LS_TVGRT-BEZEI.
      ENDIF.
    ENDIF.
    LS_DATA2-VKBUR = <L_DO>-VKBUR.
    IF <L_DO>-VKBUR IS NOT INITIAL.
      READ TABLE LT_TVKBT INTO DATA(LS_TVKBT) WITH KEY VKBUR = <L_DO>-VKBUR.
      IF SY-SUBRC = 0.
        LS_DATA2-VKBUR_TX  = LS_TVKBT-BEZEI.
      ENDIF.
    ENDIF.

*   ---VBELN_BILL
*   ---FKDAT
*   ---BILLPL_NO
*   ---BILLPL_DATE
    READ TABLE LT_VBFA_DO_BILL INTO DATA(LS_VBFA) WITH KEY VBELV = <L_DO>-VBELN_DO.
    IF SY-SUBRC = 0.
      LS_DATA2-VBELN_BILL = LS_VBFA-VBELN.
      READ TABLE LT_BILL INTO DATA(LS_BILL) WITH KEY VBELN = LS_DATA2-VBELN_BILL.
      IF SY-SUBRC = 0.
        LS_DATA2-FKDAT = LS_BILL-FKDAT.
      ENDIF.
      READ TABLE LT_033_035 INTO DATA(LS_BILLPLACE) WITH KEY VBELN = LS_DATA2-VBELN_BILL.
      IF SY-SUBRC <> 0.
        READ TABLE LT_029 INTO LS_BILLPLACE WITH KEY VBELN = LS_DATA2-VBELN_BILL.
        IF SY-SUBRC <> 0.
          READ TABLE LT_038 INTO LS_BILLPLACE WITH KEY VBELN = LS_DATA2-VBELN_BILL.
          IF SY-SUBRC <> 0.
            CLEAR LS_BILLPLACE.
          ENDIF.
        ENDIF.
      ENDIF.
      LS_DATA2-BILLPL_NO = LS_BILLPLACE-BILLPL_NO.
      LS_DATA2-BILLPL_DATE = LS_BILLPLACE-BILLPL_DATE.
    ENDIF.
*   ---INDX
    LF_INDX = LF_INDX + 1.
    LS_DATA2-INDX = LF_INDX.
    APPEND LS_DATA2 TO CT_DATA2.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_do_data
*&---------------------------------------------------------------------*
FORM F_SELECT_DO_DATA  CHANGING CT_DO TYPE TT_DO.
  SELECT A~VBELN AS VBELN_DO,                          "#EC CI_BUFFJOIN
         B~POSNR AS POSNR_DO,
*         A~KUNAG,  "CH04 420000327-
         D2~KUNNR,   "CH04 420000327+
         A~BLDAT,
         A~VKORG,
         B~VTWEG,
         B~SPART,
         B~VKBUR,
         B~VKGRP,
         B~LFIMG,
         C~VBELN AS VBELN_SO,
         D~POSNR AS POSNR_SO,
         D~NETWR AS NETWR_SO,
         D~KWMENG AS KWMENG_SO,
         C~WAERK,
         D~PS_PSP_PNR, "wbs
         F~PSPHI,       "project
         D2~ADRNR      "CH04 420000327+
  INTO TABLE @CT_DO
  FROM LIKP AS A INNER JOIN LIPS AS B
  ON   A~VBELN = B~VBELN
                 INNER JOIN VBAK AS C
  ON   B~VGBEL = C~VBELN
                 INNER JOIN VBAP AS D
  ON   B~VGBEL = D~VBELN
  AND  B~VGPOS = D~POSNR
                 INNER JOIN VBPA AS D2      "CH04 420000327+
  ON   D2~VBELN = D~VBELN                   "CH04 420000327+
  AND  D2~PARVW = 'RG'  "payer              "CH04 420000327+
                 INNER JOIN PRPS AS F
  ON   D~PS_PSP_PNR =  F~PSPNR
  WHERE A~VBELN IN @S_DO
*  AND   A~KUNAG IN @S_KUNNR "CH04 420000327-
  AND   D2~KUNNR IN @S_KUNNR "CH04 420000327+
  AND   A~VKORG IN @S_VKORGD
  AND   B~VTWEG IN @S_VTWEG
  AND   B~SPART IN @S_SPART
  AND   B~VKGRP IN @S_VKGRP
  AND   B~VKBUR IN @S_VKBUR
  AND   A~BLDAT IN @GRT_RPDAT
  AND   D~PS_PSP_PNR <> ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_sel_screen
*&---------------------------------------------------------------------*
FORM F_VALIDATE_SEL_SCREEN .
  IF RB_DO = ABAP_TRUE.
    IF S_VKORGD[] IS INITIAL.
      MESSAGE E000(ZSDSCA01) WITH 'Sales Org is required'(M03).
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_prps
*&---------------------------------------------------------------------*
FORM F_SELECT_PRPS2  USING   PT_DO TYPE TT_DO
                    CHANGING CT_PRPS TYPE TT_PRPS2.
  DATA: LT_DO TYPE TABLE OF TS_DO.
  LT_DO[] = PT_DO[].

  DELETE LT_DO WHERE PSPHI IS INITIAL.
  SORT  LT_DO BY  PSPHI.
  DELETE ADJACENT DUPLICATES FROM LT_DO  COMPARING PSPHI.
  IF LT_DO[] IS NOT INITIAL.
    SELECT PSPNR,
           POST1
    INTO TABLE @CT_PRPS
*    FROM PRPS
    FROM PROJ
    FOR ALL ENTRIES IN @LT_DO
    WHERE PSPNR = @LT_DO-PSPHI.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_ZSDSFIT041
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIT041  USING    UT_DO TYPE TT_DO
                          CHANGING CT_ZSDSFIT041 TYPE TT_041.

  IF UT_DO IS NOT INITIAL.
    SELECT *
    INTO TABLE @CT_ZSDSFIT041
    FROM ZSDSFIT041
    FOR ALL ENTRIES IN @UT_DO
    WHERE VBELN = @UT_DO-VBELN_DO
    AND   FOLLOWDT IN @S_FLWDT
    AND   UPD_DATE IN @S_UPDDT
    AND   LATEST = @ABAP_TRUE.                     "#EC CI_NO_TRANSFORM
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_SO_DATA2
*&---------------------------------------------------------------------*
FORM F_SELECT_SO_DATA2 USING    UT_SO_KEY TYPE TT_VBELN_KEY
                       CHANGING CT_VBKD_SO TYPE TT_VBKD_SO.

  IF UT_SO_KEY[] IS NOT INITIAL.
    SELECT VBELN,
           BSTKD
    FROM VBKD
    FOR ALL ENTRIES IN @UT_SO_KEY
    WHERE VBELN = @UT_SO_KEY-VBELN
    AND   BSTKD  <> ''
    AND   POSNR = '000000'
    INTO TABLE @CT_VBKD_SO.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_bill_data
*&---------------------------------------------------------------------*
FORM F_SELECT_BILL_DATA  USING    UT_DO TYPE TT_DO
                         CHANGING CT_VBFA TYPE TT_VBFA
                                  CT_BILL TYPE TT_BILL.
  IF UT_DO[] IS NOT INITIAL.
    SELECT VBELV,
           POSNV,
           VBTYP_V,
           VBELN,
           POSNN,
           VBTYP_N
    INTO TABLE @CT_VBFA
    FROM VBFA
    FOR ALL ENTRIES IN @UT_DO
    WHERE VBELV = @UT_DO-VBELN_DO
    AND   POSNV = @UT_DO-POSNR_DO "first
    AND   VBTYP_V = @GC_VBTYP_DO
    AND   VBTYP_N = @GC_VBTYP_BILL.                "#EC CI_NO_TRANSFORM
  ENDIF.
  IF CT_VBFA[] IS NOT INITIAL.
    SELECT VBELN,
           FKDAT
    INTO TABLE @CT_BILL
    FROM VBRK
    FOR ALL ENTRIES IN @CT_VBFA
    WHERE VBELN = @CT_VBFA-VBELN.                  "#EC CI_NO_TRANSFORM
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_zsdsfit033_035_2
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIT033_035_2  USING    UT_BILL TYPE TT_BILL
                                CHANGING CT_033_035_2 TYPE TT_033_035_2.
  IF UT_BILL[] IS NOT INITIAL.
    SELECT B~VBELN,
           A~BILLPL_NO,
           A~BILLPL_DATE
    INTO TABLE @CT_033_035_2
    FROM ZSDSFIT033 AS A INNER JOIN ZSDSFIT035 AS B
    ON   A~BUKRS = B~BUKRS
    AND  A~BILLPL_NO = B~BILLPL_NO
    FOR ALL ENTRIES IN @UT_BILL
    WHERE A~BUKRS IN @S_BUKRS
    AND B~VBELN = @UT_BILL-VBELN                   "#EC CI_NO_TRANSFORM
    AND A~DELFG = ''
    AND B~DELFG = ''.
  ENDIF.
ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_lock_ZSDSFIT041
**&---------------------------------------------------------------------*
*FORM F_LOCK_ZSDSFIT041  USING    UT_DATA2 TYPE ANY TABLE .
*  TYPES: BEGIN OF LTS_LOCK,
*           VBELN_DO TYPE LIKP-VBELN,
*         END OF LTS_LOCK.
*  DATA: LT_LOCK  TYPE STANDARD TABLE OF  LTS_LOCK.
*  MOVE-CORRESPONDING UT_DATA2 TO LT_LOCK.
*  SORT LT_LOCK BY VBELN_DO.
*  DELETE ADJACENT DUPLICATES FROM LT_LOCK COMPARING VBELN_DO.
*
*  LOOP AT LT_LOCK INTO DATA(LS_LOCK).
*    CALL FUNCTION 'ENQUEUE_EZSDSFIT041'
*      EXPORTING
*        MODE_ZSDSFIT041 = 'E'
*        MANDT           = SY-MANDT
*        VBELN           = LS_LOCK-VBELN_DO
*      EXCEPTIONS
*        FOREIGN_LOCK    = 1
*        SYSTEM_FAILURE  = 2
*        OTHERS          = 3.
*    IF SY-SUBRC <> 0.
*      DATA(LF_USRID) = SY-MSGV1.
*      MESSAGE S000(ZSDSCA01) WITH 'DO'(m04) LS_LOCK-VBELN_DO 'is locked by'(m02) LF_USRID
*      DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_unlock_ZSDSFIT041
**&---------------------------------------------------------------------*
*FORM F_UNLOCK_ZSDSFIT041  USING    UT_DATA2 TYPE ANY TABLE .
*  TYPES: BEGIN OF LTS_LOCK,
*           VBELN_DO TYPE LIKP-VBELN,
*         END OF LTS_LOCK.
*  DATA: LT_LOCK  TYPE STANDARD TABLE OF  LTS_LOCK.
*  MOVE-CORRESPONDING UT_DATA2 TO LT_LOCK.
*  SORT LT_LOCK BY VBELN_DO.
*  DELETE ADJACENT DUPLICATES FROM LT_LOCK COMPARING VBELN_DO.
*
*  LOOP AT LT_LOCK INTO DATA(LS_LOCK).
*    CALL FUNCTION 'DEQUEUE_EZSDSFIT041'
*      EXPORTING
*        MODE_ZSDSFIT041 = 'E'
*        MANDT           = SY-MANDT
*        VBELN           = LS_LOCK-VBELN_DO.
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_DO
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC_DO CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'WAERK'
      OR   'INDX'
      OR   'EXIST'
      OR   'SEQNO'.
        <FCAT>-TECH = GC_TRUE.
      WHEN 'CMPFG'.
        %FCAT 'Completed Flag'(C27).
        <FCAT>-EDIT = ABAP_TRUE.
        <FCAT>-CHECKBOX = ABAP_TRUE.
      WHEN 'PIC_NAME1'.
        %FCAT 'PIC Name'(C02).
      WHEN 'VBELN_DO'.
        <FCAT>-HOTSPOT = GC_TRUE.
      WHEN 'VBELN_SO'.
        <FCAT>-HOTSPOT = GC_TRUE.
      WHEN 'PSPHI'.
        %FCAT 'Project (WBS)'(C04).
        <FCAT>-REF_TABLE  = 'PRPS'.
        <FCAT>-REF_FIELD  = 'POSID'.
      WHEN 'POST1'.
        %FCAT 'Project Name'(C05).
      WHEN 'BSTKD'.
        %FCAT 'PO No.'(C06).
      WHEN 'INV_REMARK'.
        %FCAT 'Invoice Remark'(C07).
      WHEN 'RPDAT'.
        %FCAT 'As of Date'(C08).
      WHEN 'BLDAT'.
        %FCAT 'Doc Date'(C09).
      WHEN 'DO_AGE'.
        %FCAT 'Aging DO'(C28).
      WHEN 'RNG_DO_AGE'.
        %FCAT 'Range Aging DO'(C29).
      WHEN 'NETWR'.
        <FCAT>-CFIELDNAME = 'WAERK'.
        <FCAT>-DO_SUM = ABAP_TRUE.
      WHEN 'ISSUE_LIST'.
        <FCAT>-EDIT = ABAP_TRUE.
        <FCAT>-REF_TABLE  = 'ZSDSFIT041'.
        <FCAT>-REF_FIELD  = 'ISSUE_LIST'.
      WHEN 'ISSUE_LIST_TX'.
        %FCAT 'Issue list description'(C30).
      WHEN 'FOLLOWDT'.
        <FCAT>-EDIT = ABAP_TRUE.
        <FCAT>-REF_TABLE  = 'ZSDSFIT041'.
        <FCAT>-REF_FIELD  = 'FOLLOWDT'.
*BOI CH04 420000332
      WHEN 'TEXT1'.
        %FCAT 'Detail 1'(C36).
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-EMPHASIZE = 'C300'.  "CH05+
      WHEN 'TEXT2'.
        %FCAT 'Detail 2'(C37).
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-EMPHASIZE = 'C300'.  "CH05+
      WHEN 'TEXT3'.
        %FCAT 'Detail 3'(C38).
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-EMPHASIZE = 'C300'.  "CH05+
*EOI CH04 420000332
*BOD CH04 420000332
*      WHEN 'DETAIL1'.
*        <FCAT>-EDIT = ABAP_TRUE.
*        <FCAT>-REF_TABLE  = 'ZSDSFIT041'.
*        <FCAT>-REF_FIELD  = 'DETAIL1'.
*      WHEN 'DETAIL2'.
*        <FCAT>-EDIT = ABAP_TRUE.
*        <FCAT>-REF_TABLE  = 'ZSDSFIT041'.
*        <FCAT>-REF_FIELD  = 'DETAIL2'.
*      WHEN 'DETAIL3'.
*        <FCAT>-EDIT = ABAP_TRUE.
*        <FCAT>-REF_TABLE  = 'ZSDSFIT041'.
*        <FCAT>-REF_FIELD  = 'DETAIL3'.
*EOD CH04 420000332
      WHEN 'SALEMAN'.
        %FCAT 'Salesman'(C25).
      WHEN 'SALEMAN_NAME'.
        %FCAT 'Salesman Name'(C26).
      WHEN 'VKGRP_TX'.
        %FCAT 'Sales Group desc'(C33).
      WHEN 'VKBUR_TX'.
        %FCAT 'Sales Office desc'(C34).
    ENDCASE.
    CASE <FCAT>-FIELDNAME.
      WHEN 'CMPFG'
      OR   'PIC_PERNR'
      OR   'PIC_NAME1'
      OR   'VBELN_DO'.
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.
  IF GF_EXEC_TY = GC_EXEC_TY-DO.
    INSERT VALUE #( FUNCTION = 'ZALL'
                    ICON = '@4B@'
                    QUICKINFO = 'Select All'(001) )
           INTO TABLE UF_OBJECT->MT_TOOLBAR.

    INSERT VALUE #( FUNCTION = 'ZSAL'
                    ICON = '@4D@'
                    QUICKINFO = 'Deselect All'(002) )
           INTO TABLE UF_OBJECT->MT_TOOLBAR.
  ENDIF.
* Handle Toolbar as needed

  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&REFRESH'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&PRINT_BACK'.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.
  DATA: LS_STABLE  TYPE LVC_S_STBL.

  CASE UF_UCOMM.
    WHEN 'ZALL'.
      CASE GF_EXEC_TY.
        WHEN GC_EXEC_TY-DO.
          PERFORM F_SELECT_CHECK_BOX USING  GT_DATA2
                                          'CMPFG'
                                          'X'.
      ENDCASE.
    WHEN 'ZSAL'.
      CASE GF_EXEC_TY.
        WHEN GC_EXEC_TY-DO.
          PERFORM F_SELECT_CHECK_BOX USING  GT_DATA2
                                          'CMPFG'
                                          ''.
      ENDCASE.

  ENDCASE.

  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_check_box
*&---------------------------------------------------------------------*
FORM F_SELECT_CHECK_BOX  USING UT_DATA TYPE ANY TABLE
                             UF_NAME TYPE ANY
                             UF_X TYPE C.
  LOOP AT UT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) .
    ASSIGN COMPONENT UF_NAME OF STRUCTURE <L_DATA> TO FIELD-SYMBOL(<L_VALUE>).
    IF <L_VALUE> IS ASSIGNED.
      <L_VALUE> = UF_X.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE_DB_DO
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE_DB_DO  CHANGING CT_DATA2 TYPE TT_DATA2.
*  DATA: LS_UPD TYPE ZSDSFIT041,
*        LS_INS TYPE ZSDSFIT041,
*        LT_UPD TYPE STANDARD TABLE OF ZSDSFIT041,
*        LT_INS TYPE STANDARD TABLE OF ZSDSFIT041.
  DATA: LS_MOD TYPE   ZSDSFIT041,
        LT_MOD TYPE STANDARD TABLE OF ZSDSFIT041.
  DATA: LF_CHANGE_TX TYPE FLAG. "CH04 420000332+

  DATA(LF_DATUM) = SY-DATUM.
  DATA(LF_UZEIT) = SY-UZEIT.
  SORT GT_DATA2_OLD BY INDX.
  LOOP AT CT_DATA2 ASSIGNING FIELD-SYMBOL(<L_DATA2>).
*BOI CH04 420000332
    IF  <L_DATA2>-CHANGE_TX = ABAP_TRUE.
      LF_CHANGE_TX = ABAP_TRUE.
*      CLEAR <L_DATA2>-CHANGE_TX. "CH06 420000332 LONG TEXT MUST UPDATE TABLE -
    ENDIF.
*EOI CH04 420000332
    READ TABLE GT_DATA2_OLD INTO DATA(LS_OLD) WITH KEY INDX = <L_DATA2>-INDX
                                                       BINARY SEARCH.
    IF SY-SUBRC <> 0.
*    OR LS_OLD = <L_DATA2>. "CH04 420000332-
      CONTINUE.
*BOI CH04 420000332
    ELSE.
*BOD CH06 420000332 LONG TEXT MUST UPDATE TABLE
*      CLEAR: LS_OLD-TEXT1,
*             LS_OLD-TEXT2,
*             LS_OLD-TEXT3.
*      DATA(LS_DATA2) = <L_DATA2>.
*      CLEAR: LS_DATA2-TEXT1,
*             LS_DATA2-TEXT2,
*             LS_DATA2-TEXT3.
*      IF LS_OLD = LS_DATA2. "COMPARE ALL FIELDS EXCEPT LONG TEXT FIELDS
*EOD CH06 420000332 LONG TEXT MUST UPDATE TABLE
*BOI CH06 420000332 LONG TEXT MUST UPDATE TABLE
      IF  LS_OLD = <L_DATA2>
      AND <L_DATA2>-CHANGE_TX = ABAP_FALSE.
*EOI CH06 - LONG TEXT MUST UPDATE TABLE
        CONTINUE.
      ENDIF.
*EOI CH04 420000332
    ENDIF.
    IF <L_DATA2>-EXIST = ABAP_TRUE.
      CLEAR LS_MOD.
      MOVE-CORRESPONDING LS_OLD TO LS_MOD.
      LS_MOD-VBELN  = LS_OLD-VBELN_DO.
      CLEAR LS_MOD-LATEST.
      APPEND LS_MOD TO LT_MOD.

      CLEAR LS_MOD.
      MOVE-CORRESPONDING <L_DATA2> TO LS_MOD.
      LS_MOD-VBELN  = <L_DATA2>-VBELN_DO.
      LS_MOD-LATEST = ABAP_TRUE.
      LS_MOD-SEQNO = <L_DATA2>-SEQNO + 1.
      LS_MOD-UPD_DATE = LF_DATUM.
      LS_MOD-UPD_TIME = LF_UZEIT.
      LS_MOD-UPD_USER = SY-UNAME.
      APPEND LS_MOD TO LT_MOD.
    ELSE.
      CLEAR LS_MOD.
      MOVE-CORRESPONDING <L_DATA2> TO LS_MOD.
      LS_MOD-VBELN  = <L_DATA2>-VBELN_DO.
      LS_MOD-LATEST = ABAP_TRUE.
      LS_MOD-SEQNO =  1.
      LS_MOD-UPD_DATE = LF_DATUM.
      LS_MOD-UPD_TIME = LF_UZEIT.
      LS_MOD-UPD_USER = SY-UNAME.
      APPEND LS_MOD TO LT_MOD.
    ENDIF.
    MOVE-CORRESPONDING LS_MOD TO <L_DATA2>.
    <L_DATA2>-EXIST = ABAP_TRUE.
  ENDLOOP.
  IF LT_MOD[] IS NOT INITIAL.
    MODIFY ZSDSFIT041 FROM TABLE LT_MOD.
  ENDIF.
*BOI CH04 420000332
  IF LF_CHANGE_TX = ABAP_TRUE.
    CALL FUNCTION 'COMMIT_TEXT'
      EXPORTING
        OBJECT            = GC_TDOBJECT_DETAIL
        NAME              = '*'
        ID                = '*'
        LANGUAGE          = '*'
        SAVEMODE_DIRECT   = 'X'
        KEEP              = ' '
        LOCAL_CAT         = ' '
        KEEP_LAST_CHANGED = ' '.

  ENDIF.
*EOI CH04 420000332
  COMMIT WORK AND WAIT.
  GT_DATA2_OLD[] = CT_DATA2[].
*BOI CH04 420000332
  CALL FUNCTION 'FREE_TEXT_MEMORY' ##FM_SUBRC_OK
    EXPORTING
      LOCAL_CAT = ' '
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.

*BOI CH04 420000332

* Follow-up status has been created sucessfully
  MESSAGE S013(ZSDSFI01).
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_1
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_1 USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                               UF_ONF4 TYPE  CHAR01          ##NEEDED
                               UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                               UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                               UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-FI.
      PERFORM F_ON_DATA_CHANGED_FI
              USING UF_DATA_CHANGED
                    UF_ONF4
                    UF_ONF4_BEFORE
                    UF_ONF4_AFTER
                    UF_UCOMM .
    WHEN GC_EXEC_TY-DO.
      PERFORM F_ON_DATA_CHANGED_DO
              USING UF_DATA_CHANGED
                    UF_ONF4
                    UF_ONF4_BEFORE
                    UF_ONF4_AFTER
                    UF_UCOMM .

  ENDCASE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_FI
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM   F_ON_DATA_CHANGED_FI  USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                                    UF_ONF4 TYPE  CHAR01          ##NEEDED
                                    UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                                    UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                                    UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  DATA: LS_STABLE  TYPE LVC_S_STBL,
        LF_REFRESH TYPE FLAG.

* For all valid changing cells
  LOOP AT UF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>)
                         INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.

      WHEN 'ISSUE_LIST'.
        IF  <L_GOOD_CELL>-VALUE IS NOT INITIAL.
          READ TABLE GT_ISLIST INTO DATA(LS_LIST)
                                WITH KEY VALUE = <L_GOOD_CELL>-VALUE ##WARN_OK.
          IF SY-SUBRC = 0.
            <L_DATA1>-ISSUE_LIST_TX = LS_LIST-TEXT.
            LF_REFRESH = GC_TRUE.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDLOOP.
  IF LF_REFRESH = GC_TRUE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.

    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = 'X'.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_FI
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM   F_ON_DATA_CHANGED_DO  USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                                    UF_ONF4 TYPE  CHAR01          ##NEEDED
                                    UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                                    UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                                    UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  DATA: LS_STABLE  TYPE LVC_S_STBL,
        LF_REFRESH TYPE FLAG.

* For all valid changing cells
  LOOP AT UF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_DATA2 ASSIGNING FIELD-SYMBOL(<L_DATA2>)
                         INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.

      WHEN 'ISSUE_LIST'.
        IF  <L_GOOD_CELL>-VALUE IS NOT INITIAL.
          READ TABLE GT_ISLIST INTO DATA(LS_LIST)
                                WITH KEY VALUE = <L_GOOD_CELL>-VALUE ##WARN_OK.
          IF SY-SUBRC = 0.
            <L_DATA2>-ISSUE_LIST_TX = LS_LIST-TEXT.
            LF_REFRESH = GC_TRUE.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDLOOP.
  IF LF_REFRESH = GC_TRUE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.

    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_unlock_ZSDSFIT028
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW
                             US_COLUMN_ID TYPE LVC_S_COL ##CALLED.
  DATA: BEGIN OF LS_DATA,
          BUKRS      TYPE BKPF-BUKRS,
          BELNR      TYPE BKPF-BELNR,
          GJAHR      TYPE BKPF-GJAHR,
          VBELN_BILL TYPE VBRK-VBELN,
          VBELN_DO   TYPE LIKP-VBELN,
        END OF LS_DATA.
*BOI  CH04 420000332
  IF  US_COLUMN_ID-FIELDNAME = 'TEXT1'
  OR  US_COLUMN_ID-FIELDNAME = 'TEXT2'
  OR  US_COLUMN_ID-FIELDNAME = 'TEXT3'  .
    PERFORM F_HOTSPOT_LONG_TEXT USING US_ROW_ID
                                      US_COLUMN_ID.
    RETURN.
  ENDIF.
*EOI CH04 420000332

  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-FI.
      READ TABLE GT_DATA1 INTO DATA(LS_DATA1) INDEX US_ROW_ID-INDEX.
      MOVE-CORRESPONDING LS_DATA1 TO LS_DATA.

    WHEN GC_EXEC_TY-DO.
      READ TABLE GT_DATA2 INTO DATA(LS_DATA2) INDEX US_ROW_ID-INDEX.
      MOVE-CORRESPONDING LS_DATA2 TO LS_DATA.
  ENDCASE.
  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'BELNR'.
      IF LS_DATA-BELNR IS NOT INITIAL
      AND LS_DATA-GJAHR IS NOT INITIAL
      AND LS_DATA-BUKRS IS NOT INITIAL  .
        SET PARAMETER ID 'BLN' FIELD LS_DATA-BELNR.
        SET PARAMETER ID 'BUK' FIELD LS_DATA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD LS_DATA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
    WHEN 'VBELN_BILL'.
      IF LS_DATA-VBELN_BILL IS NOT INITIAL.
        SET PARAMETER ID 'VF' FIELD LS_DATA-VBELN_BILL.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
    WHEN 'VBELN_DO'.
      IF LS_DATA-VBELN_DO IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD LS_DATA-VBELN_DO.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.  "#EC CI_CALLTA
      ENDIF.
  ENDCASE.


ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '22%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '78%'.

  DATA:
    LF_TEMP1     TYPE  TEXT50,
    LF_TEMP2     TYPE  TEXT50,
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* TEXT-H00 : Follow up aging report
  CASE ABAP_TRUE.
    WHEN RB_BILL.
* Text-h02 : Billing & FI Document
      LF_TEXT = |{ TEXT-H00 } ({ TEXT-H02 })|.
    WHEN RB_DO.
* Text-h03 : DO Document
      LF_TEXT = |{ TEXT-H00 } ({ TEXT-H03 })|.
  ENDCASE.
  CALL METHOD UREF_DYNDOC_ID->ADD_TEXT_AS_HEADING
    EXPORTING
      TEXT = LF_TEXT.


* Create table
  CALL METHOD UREF_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LREF_TABLE.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LREF_COL_KEY.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LREF_COL_VAL.
* Set Key column style
  CALL METHOD LREF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.


*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Report Date/Time:
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE SY-DATUM TO LF_TEMP1.
  WRITE SY-UZEIT TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }/{ LF_TEMP2 }|.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h07 : User Name:
  LF_TEXT = TEXT-H07.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = SY-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : As of Date:
  LF_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE P_RPDAT TO LF_TEXT.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Account Clerk:
  LF_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  IF S_BUSAB[] IS INITIAL.
    LF_TEXT = '*'.
  ELSEIF S_BUSAB-HIGH IS NOT INITIAL.
    LF_TEXT = |{ S_BUSAB-LOW } to { S_BUSAB-HIGH }|.
  ELSE.
    LF_TEXT = S_BUSAB-LOW .
  ENDIF.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_clear_doc
*&---------------------------------------------------------------------*
FORM F_SELECT_CLEAR_DOC  USING   UT_FI_DOC TYPE TT_FIDOC
                         CHANGING CT_CLEAR_DOC TYPE TT_CLEAR_DOC.
  IF UT_FI_DOC IS INITIAL.
    RETURN.
  ENDIF.
  SELECT A~BUKRS,                                       "#EC CI_NOORDER
         A~REBZG,
         A~REBZJ,
         A~REBZZ,
         A~GJAHR,
         A~BELNR,
         A~BUZEI,
         A~SHKZG,
         A~DMBTR,
         A~AUGBL
  FROM BSEG AS A INNER JOIN BKPF AS  B
  ON A~BUKRS = B~BUKRS
  AND A~BELNR = B~BELNR
  AND A~GJAHR = B~GJAHR
  FOR ALL ENTRIES IN @UT_FI_DOC
  WHERE A~BUKRS IN @S_BUKRS
  AND   REBZG = @UT_FI_DOC-BELNR
  AND   REBZJ = @UT_FI_DOC-GJAHR
  AND   REBZZ = @UT_FI_DOC-BUZEI
*<<F36K909945 start ins
  AND   BUDAT IN @GRT_RPDAT
*   AND ( ( AUGBL IS INITIAL AND AUGDT IS INITIAL ) OR ( A~AUGBL <> A~BELNR AND AUGDT > @P_RPDAT ) ) "CH06 420000219-
    AND ( ( AUGBL IS INITIAL AND AUGDT IS INITIAL ) OR ( AUGBL IS NOT INITIAL AND AUGDT > @P_RPDAT ) ) "CH06 420000219+
*<<F36K909945 end ins
*  AND   B~XREVERSAL = ''  "CH02 420000219-
  INTO TABLE @CT_CLEAR_DOC ##TOO_MANY_ITAB_FIELDS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_TVGRT
*&---------------------------------------------------------------------*
FORM F_SELECT_TVGRT  USING    UT_VKGRP_KEY TYPE TT_TVGRT
                     CHANGING CT_TVGRT TYPE TT_TVGRT.
  IF UT_VKGRP_KEY[] IS INITIAL.
    RETURN.
  ENDIF.
  SELECT *
  INTO TABLE @CT_TVGRT
  FROM TVGRT
  FOR ALL ENTRIES IN @UT_VKGRP_KEY
  WHERE SPRAS = @SY-LANGU
  AND VKGRP = @UT_VKGRP_KEY-VKGRP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_TVkbT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*

FORM F_SELECT_TVKBT  USING    UT_VKBUR_KEY TYPE TT_TVKBT
                     CHANGING CT_TVKBT TYPE TT_TVKBT.
  IF UT_VKBUR_KEY[] IS INITIAL.
    RETURN.
  ENDIF.
  SELECT *
  INTO TABLE @CT_TVKBT
  FROM TVKBT
  FOR ALL ENTRIES IN @UT_VKBUR_KEY
  WHERE SPRAS = @SY-LANGU
  AND VKBUR = @UT_VKBUR_KEY-VKBUR.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_knb1
*&---------------------------------------------------------------------*
FORM F_SELECT_KNB1  USING    UT_CUST_KEY TYPE TT_CUST_KEY
                    CHANGING CT_KNB1 TYPE TT_KNB1.
  IF UT_CUST_KEY[] IS INITIAL.
    RETURN.
  ENDIF.
  SELECT KUNNR,
         BUKRS,
         BUSAB
  INTO TABLE @CT_KNB1
  FROM KNB1
  FOR ALL ENTRIES IN @UT_CUST_KEY
  WHERE KUNNR = @UT_CUST_KEY-KUNNR
  AND   BUSAB IN @S_BUSAB.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_ZSDSFIT029_2
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIT029_2 USING    UT_BILL TYPE TT_BILL
                         CHANGING CT_029 TYPE TT_033_035_2.
  IF UT_BILL[] IS NOT INITIAL.
    SELECT A~VBELN,
           A~BILLPL_NO,
           A~BILLPL_DATE
    INTO TABLE @CT_029
    FROM ZSDSFIT029 AS A
    FOR ALL ENTRIES IN @UT_BILL
    WHERE A~DATA_TYPE = ''
    AND A~VBELN_VF = @UT_BILL-VBELN .              "#EC CI_NO_TRANSFORM

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_ZSDSFIT038_2
*&---------------------------------------------------------------------*
FORM F_SELECT_ZSDSFIT038_2 USING    UT_BILL TYPE TT_BILL
                         CHANGING CT_038 TYPE TT_033_035_2.
  IF UT_BILL[] IS NOT INITIAL.
    SELECT A~VBELN,
           A~BILLPL_NO,
           A~BILLPL_DATE
    INTO TABLE @CT_038
    FROM ZSDSFIT038 AS A
    FOR ALL ENTRIES IN @UT_BILL
    WHERE A~DATA_TYPE = ''
    AND A~VBELN_VF = @UT_BILL-VBELN .              "#EC CI_NO_TRANSFORM

  ENDIF.
ENDFORM.
*BOI CH02
*&---------------------------------------------------------------------*
*& Form f_get_po_from_sv_order
*&---------------------------------------------------------------------*
FORM F_GET_PO_FROM_SV_ORDER  USING    UF_VBELN TYPE VBRP-VBELN
                             CHANGING CF_BSTKD TYPE TS_DATA1-BSTKD.
  CONSTANTS: LC_VBTYP_SVORDER TYPE VBFA-VBTYP_N VALUE 'CSVO',
             LC_VBTYP_SVCONTR TYPE VBFA-VBTYP_N VALUE 'CSCT'.
  DATA: LT_DOCFLOW  TYPE TDT_DOCFLOW,
        LR_VBTYP_SV TYPE RANGE OF VBFA-VBTYP_N.

  CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
    EXPORTING
      IV_DOCNUM     = UF_VBELN
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
      IF CF_BSTKD IS INITIAL.
        CF_BSTKD  = <L_SV>-ZZ1_CUS_PO.
      ELSE.
        CF_BSTKD = |{ CF_BSTKD }, { <L_SV>-ZZ1_CUS_PO }|.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*EOI CH02
*BOI CH04 420000327
*&---------------------------------------------------------------------*
*& Form f_select_bsec
*&---------------------------------------------------------------------*
FORM F_SELECT_BSEC  USING    UT_FIDOC TYPE TT_FIDOC
                    CHANGING CT_BSEC TYPE TT_BSEC.
  IF UT_FIDOC[] IS NOT INITIAL.
    SELECT  BUKRS,                                      "#EC CI_NOORDER
            BELNR,
            GJAHR,
            BUZEI,
            ADRC~NATION,
            ADRC~NAME1
    INTO TABLE @CT_BSEC
    FROM BSEC INNER JOIN ADRC
    ON BSEC~ADRNR = ADRC~ADDRNUMBER
    FOR ALL ENTRIES IN @UT_FIDOC
    WHERE BUKRS = @UT_FIDOC-BUKRS
    AND   BELNR = @UT_FIDOC-BELNR
    AND   GJAHR = @UT_FIDOC-GJAHR
    AND   BUZEI = @UT_FIDOC-BUZEI.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECT_CUST_ADRC
*&---------------------------------------------------------------------*
FORM F_SELECT_CUST_ADRC  USING    UT_DO TYPE TT_DO
                         CHANGING CT_CUST_ADRC TYPE TT_CUST_ADRC.
  IF UT_DO[] IS NOT INITIAL.
    SELECT ADDRNUMBER,
           NATION,
           NAME1
    INTO TABLE @CT_CUST_ADRC
    FROM ADRC
    FOR ALL ENTRIES IN @UT_DO
    WHERE ADDRNUMBER = @UT_DO-ADRNR.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SELECT_VBFA_INV_DO
*&---------------------------------------------------------------------*

FORM F_SELECT_VBFA_INV_DO  USING    UT_FIDOC TYPE TT_FIDOC
                           CHANGING CT_VBFA_INV_DO TYPE  TT_VBFA_INV_DO.
  IF UT_FIDOC[] IS NOT INITIAL.
    SELECT VBELN,
           VBELV
    INTO TABLE @CT_VBFA_INV_DO
    FROM VBFA
    FOR ALL ENTRIES IN @UT_FIDOC
    WHERE VBELN = @UT_FIDOC-VBELN "INV , CANCEL INV, CN DN
    AND VBTYP_V = 'J' ."DO
*  AND VBTYP_N = 'M' ."INV
  ENDIF.
ENDFORM.
*EOI CH04 420000327
*BOI CH04 420000219
*&---------------------------------------------------------------------*
*& Form F_SELECT_CLEAR_MIGRATE_DOC
*&---------------------------------------------------------------------*
FORM F_SELECT_CLEAR_MIGRATE_DOC  USING   UT_FI_DOC TYPE TT_FIDOC
                         CHANGING CT_CLEAR_MGRTE TYPE TT_CLEAR_MGRTE.
  IF UT_FI_DOC IS INITIAL.
    RETURN.
  ENDIF.
  DATA(LT_FI_DOC) = UT_FI_DOC .
  DELETE LT_FI_DOC WHERE XBLNR = ''                     "#EC CI_SORTSEQ
                   OR XREF1 = ''
                   OR BLART <> GC_BLART_MIGRATE.
  LOOP AT LT_FI_DOC INTO DATA(LS_FI_DOC)                ##INTO_OK.
    IF LS_FI_DOC-XBLNR <> LS_FI_DOC-XREF1.
      DELETE LT_FI_DOC.
    ENDIF.
  ENDLOOP.
  IF LT_FI_DOC IS NOT INITIAL.
    SELECT  A~BUKRS,                                    "#EC CI_NOORDER
            A~XREF1 ,
            B~XBLNR ,
            A~GJAHR ,
            A~BELNR ,
            A~BUZEI ,
            A~SHKZG,
            A~UMSKZ ,
            A~DMBTR
   FROM BSEG AS A INNER JOIN BKPF AS  B
   ON A~BUKRS = B~BUKRS
   AND A~BELNR = B~BELNR
   AND A~GJAHR = B~GJAHR
   FOR ALL ENTRIES IN @LT_FI_DOC
   WHERE A~BUKRS IN @S_BUKRS
   AND   KOART = 'D' "CUSTOMER
   AND   A~XREF1 = @LT_FI_DOC-XREF1
   AND   BUDAT IN @GRT_RPDAT
   AND   B~BLART = @GC_BLART_MIGRATE
   AND   A~XREF1 <> B~XBLNR
   AND   A~BELNR <> @LT_FI_DOC-BELNR
*   AND ( ( AUGBL IS INITIAL AND AUGDT IS INITIAL ) OR ( A~AUGBL <> A~BELNR AND AUGDT > @P_RPDAT ) ) "CH06 420000219-
    AND ( ( AUGBL IS INITIAL AND AUGDT IS INITIAL ) OR ( AUGBL IS NOT INITIAL AND  AUGDT > @P_RPDAT ) ) "CH06 420000219+
   INTO TABLE @CT_CLEAR_MGRTE ##TOO_MANY_ITAB_FIELDS.


  ENDIF.
ENDFORM.
*EOI CH04 420000219
*BOI CH04 420000332
*&---------------------------------------------------------------------*
*& Form f_hotspot_long_text
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_LONG_TEXT  USING  US_ROW_ID TYPE LVC_S_ROW
                                 US_COLUMN_ID TYPE LVC_S_COL ##CALLED.
  DATA: LS_THEAD   TYPE THEAD,
        LS_STABLE  TYPE LVC_S_STBL,
        LF_TDLINE  TYPE TDLINE,
        LF_CHANGED TYPE FLAG.

  LS_THEAD-TDOBJECT = GC_TDOBJECT_DETAIL.
  LS_THEAD-TDSPRAS = SY-LANGU.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-FI.
      READ TABLE GT_DATA1 ASSIGNING FIELD-SYMBOL(<L_DATA1>) INDEX US_ROW_ID-INDEX.
      IF <L_DATA1> IS NOT ASSIGNED.
        RETURN.
      ENDIF.
      LS_THEAD-TDNAME = <L_DATA1>-BUKRS && <L_DATA1>-BELNR && <L_DATA1>-GJAHR && <L_DATA1>-BUZEI.
      LS_THEAD-TDID = 'FI0'.
    WHEN GC_EXEC_TY-DO.
      READ TABLE GT_DATA2 ASSIGNING FIELD-SYMBOL(<L_DATA2>) INDEX US_ROW_ID-INDEX.
      IF <L_DATA2> IS NOT ASSIGNED.
        RETURN.
      ENDIF.
      LS_THEAD-TDNAME = <L_DATA2>-VBELN_DO.
      LS_THEAD-TDID = 'DO0'.
  ENDCASE.

  LS_THEAD-TDID = LS_THEAD-TDID && US_COLUMN_ID-FIELDNAME+4(1).
  IF LS_THEAD-TDNAME IS NOT INITIAL.

    CALL FUNCTION 'ZSDS_FI_MAINTAIN_LONG_TEXT'
      EXPORTING
        IM_THEAD        = LS_THEAD
      IMPORTING
        EX_TDLINE       = LF_TDLINE
        EX_DATA_CHANGED = LF_CHANGED.

  ENDIF.
  IF LF_CHANGED = ABAP_TRUE.
    CASE GF_EXEC_TY.
      WHEN GC_EXEC_TY-FI.
        ASSIGN COMPONENT US_COLUMN_ID-FIELDNAME OF STRUCTURE  <L_DATA1> TO FIELD-SYMBOL(<L_FIELDFI>).
        IF <L_FIELDFI> IS  ASSIGNED.
          <L_FIELDFI> = LF_TDLINE.
        ENDIF.
        <L_DATA1>-CHANGE_TX = ABAP_TRUE.
      WHEN GC_EXEC_TY-DO.
        ASSIGN COMPONENT US_COLUMN_ID-FIELDNAME OF STRUCTURE  <L_DATA2> TO FIELD-SYMBOL(<L_FIELDDO>).
        IF <L_FIELDDO> IS  ASSIGNED.
          <L_FIELDDO> = LF_TDLINE.
        ENDIF.
        <L_DATA2>-CHANGE_TX = ABAP_TRUE.
    ENDCASE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.

    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = 'X'.
  ENDIF.
ENDFORM.
*EOI CH04 420000332
*BOI CH08
*&---------------------------------------------------------------------*
*& Form f_select_SD_027
*&---------------------------------------------------------------------*
FORM F_SELECT_SD_027 USING UT_CUSTSD_KEY TYPE TT_CUSTSD_KEY
                     CHANGING CT_KNVV_027 TYPE TT_KNVV_027.


  IF UT_CUSTSD_KEY IS INITIAL.
    RETURN.
  ENDIF.

  SELECT  A~KUNNR,
          A~VKORG,
          A~VTWEG,
          A~SPART,
          A~VKBUR,
          A~VKGRP,
          B~FR_VKGRP,
          B~TO_VKGRP,
          B~PIC_PERNR,
          A2~BUSAB,
          B~FR_KUNNR,
          B~TO_KUNNR,
          B~FR_BUSAB,
          B~TO_BUSAB
  FROM @UT_CUSTSD_KEY AS A  INNER JOIN KNB1 AS A2
  ON   A~KUNNR = A2~KUNNR
                  INNER JOIN ZSDSFIC027 AS B
  ON A~KUNNR >= B~FR_KUNNR
  AND A~KUNNR <= B~TO_KUNNR
  AND A~VKORG = B~VKORG
  AND A~VTWEG >= B~FR_VTWEG
  AND A~VTWEG <= B~TO_VTWEG
  AND A~SPART >= B~FR_SPART
  AND A~SPART <= B~TO_SPART
  AND A~VKBUR = B~VKBUR
  AND A~VKGRP >= B~FR_VKGRP
  AND A~VKGRP <= B~TO_VKGRP
  AND A2~BUSAB >= B~FR_BUSAB
  AND A2~BUSAB <= B~TO_BUSAB
  WHERE A2~BUKRS IN @S_BUKRS
  AND   A2~BUSAB IN @S_BUSAB
  INTO TABLE @CT_KNVV_027.


  IF S_PIC[] IS NOT INITIAL.
    DELETE CT_KNVV_027 WHERE PIC_PERNR NOT IN S_PIC.    "#EC CI_SORTSEQ
  ENDIF.


ENDFORM.
*EOI CH08
