*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0530_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_ADDITIONAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_DATA
*&---------------------------------------------------------------------*
FORM F_ADDITIONAL_DATA  CHANGING CT_DATA TYPE TT_DATA.

  DATA : BEGIN OF LS_QT,
           VBELN_QT TYPE VBFA-VBELV,
         END OF LS_QT.
  DATA : LT_QT LIKE HASHED TABLE OF LS_QT WITH UNIQUE KEY VBELN_QT.

  DATA : BEGIN OF LS_DO,
           VBELN_DO TYPE VBFA-VBELV,
         END OF LS_DO.
  DATA : LT_DO LIKE HASHED TABLE OF LS_DO WITH UNIQUE KEY VBELN_DO.

  LT_QT =  CORRESPONDING #( CT_DATA  DISCARDING DUPLICATES ).
  LT_DO =  CORRESPONDING #( CT_DATA  DISCARDING DUPLICATES ).

  SORT LT_DO.
  DELETE LT_DO WHERE VBELN_DO IS INITIAL.

  SELECT   VBAK~VBELN,
           VBAK~BNAME,
           VBKD~IHREZ,
           FARR_C_EVNT_TY_T~DESCRIPTION
    FROM  @LT_QT AS A
    INNER JOIN VBAK ON A~VBELN_QT EQ VBAK~VBELN
    LEFT JOIN  VBKD ON A~VBELN_QT EQ VBKD~VBELN
                   AND VBKD~POSNR EQ '000000'
    LEFT JOIN FARR_C_EVNT_TY_T ON VBAK~ZZPOB EQ FARR_C_EVNT_TY_T~EVENT_TYPE
                              AND FARR_C_EVNT_TY_T~LANGUAGE EQ @SY-LANGU
    INTO TABLE  @DATA(LT_TMP).

  SELECT ZZ1_DELIVERY_ORD,
         OBJECT_ID,
         PO_NUMBER_SOLD
    FROM @LT_DO AS A
    INNER JOIN CRMS4D_SERV_H ON A~VBELN_DO EQ CRMS4D_SERV_H~ZZ1_DELIVERY_ORD
    INTO TABLE @DATA(LT_SER).

  LOOP AT CT_DATA ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
    READ TABLE LT_TMP INTO DATA(LS_TMP)
    WITH KEY VBELN = <LFS_DATA>-VBELN_QT.
    IF SY-SUBRC = 0.
      <LFS_DATA>-BNAME_QT    = LS_TMP-BNAME.
      <LFS_DATA>-IHREZ       = LS_TMP-IHREZ.
      <LFS_DATA>-DESCRIPTION = LS_TMP-DESCRIPTION.
    ENDIF.

    READ TABLE LT_SER INTO DATA(LS_SER)
    WITH KEY ZZ1_DELIVERY_ORD = <LFS_DATA>-VBELN_DO.
    IF SY-SUBRC EQ 0.
      <LFS_DATA>-OBJECT_ID      = LS_SER-OBJECT_ID.
      <LFS_DATA>-PO_NUMBER_SOLD = LS_SER-PO_NUMBER_SOLD.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0230_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
  ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_and_prepare_data
*&---------------------------------------------------------------------*
FORM F_SELECT_AND_PREPARE_DATA CHANGING CT_DATA TYPE TT_DATA.
  DATA: LT_SO            TYPE TT_SO,
        LT_VBFA_SO_DO    TYPE TT_VBFA_AFT,
        LT_VBFA_SO_IV    TYPE TT_VBFA_AFT,
        LT_DO            TYPE TT_DO,
        LT_VBFA_SO_QT    TYPE TT_VBFA_BF,
        LT_T001L         TYPE TT_T001L,
        LT_MVKE          TYPE TT_MVKE,
        LT_TVV1T         TYPE TT_TVV1T,
        LT_TVV2T         TYPE TT_TVV2T,
        LT_ZSDSFIT036    TYPE TT_ZSDSFIT036,
        LT_UKMBP_CMS_SGM TYPE TT_UKMBP_CMS_SGM,
        LT_VBPA          TYPE TT_VBPA,
        LT_CHLOG_CMGST   TYPE TT_CHLOG,
        LT_ZSDSSDT009    TYPE TT_ZSDSSDT009,
        LT_PRCD_ELEMENTS TYPE TT_PRCD_ELEMENTS,
        LT_TVAUT         TYPE TT_TVAUT,
        LT_QT            TYPE TT_QT,
        LT_TVAGT         TYPE TT_TVAGT,
        LT_TVGRT         TYPE TT_TVGRT,
        LT_TVKBT         TYPE TT_TVKBT,
        LT_PA0002        TYPE TT_PA0002,
        LT_INV           TYPE TT_INV,
        LT_T247          TYPE TT_T247,
        LT_T052U         TYPE TT_T052U,
        LT_PROJ          TYPE TT_PROJ,
        LT_ZSDSSDT010    TYPE TT_ZSDSSDT010,
        LT_DD07T         TYPE TT_DD07T,
        LT_INV_BY_DO     TYPE TT_INV_BY_REF,
        LT_INV_BY_SO     TYPE TT_INV_BY_REF,
        LT_VBFA_INV_REF  TYPE TT_VBFA_BF.



  DATA: LF_RUNNO           TYPE SY-TABIX,
        LF_POSNR_ZERO      TYPE VBPA-POSNR VALUE 0,
        LF_FOUND_BOM       TYPE FLAG,
        LF_CREDIT_CURRENCY TYPE UKMCRED_SGM0C-CURRENCY,
        LF_MONTH           TYPE T247-MNR,
        LF_VBTYP_N         TYPE VBFA-VBTYP_N.

  DATA: LRT_VBTYP_N        TYPE RANGE OF VBFA-VBTYP_N.

  CLEAR CT_DATA.
  DATA: LS_DATA TYPE TS_DATA.

  PERFORM F_GET_SO_DATA CHANGING LT_SO.
  IF LT_SO[] IS INITIAL.
    RETURN.
  ENDIF.
  "add related cn request, dn request, return to lt_so
  PERFORM F_GET_CN_DN_RETURN CHANGING LT_SO  .

  PERFORM F_GET_VBFA_SO_QT USING LT_SO
                           CHANGING LT_VBFA_SO_QT.
  IF S_QT[] IS NOT INITIAL
  AND LT_VBFA_SO_QT[] IS INITIAL.
    RETURN.
  ENDIF.
  IF LT_VBFA_SO_QT IS NOT INITIAL.
    PERFORM F_GET_QT USING LT_VBFA_SO_QT
                     CHANGING LT_QT.
  ENDIF.

  PERFORM F_GET_VBFA_SO_DO  USING LT_SO
                            CHANGING LT_VBFA_SO_DO.
  IF LT_VBFA_SO_DO IS NOT INITIAL.
    PERFORM F_GET_DO USING LT_VBFA_SO_DO
                     CHANGING LT_DO.
  ENDIF.

  PERFORM F_GET_VBFA_SO_INV USING LT_SO
                            CHANGING LT_VBFA_SO_IV.
  IF LT_VBFA_SO_IV IS NOT INITIAL.
    PERFORM F_GET_INV USING LT_VBFA_SO_IV
                      CHANGING LT_INV.
    PERFORM F_GET_INV_REF USING LT_INV
                          CHANGING LT_VBFA_INV_REF.
  ENDIF.

  PERFORM F_GET_INV_BY_DO USING LT_DO
                            CHANGING LT_INV_BY_DO.

  PERFORM F_GET_INV_BY_SO USING LT_SO
                            CHANGING LT_INV_BY_SO.

  PERFORM F_GET_T001L USING  LT_SO
                      CHANGING LT_T001L.
  PERFORM F_GET_MVKE USING LT_SO
                     CHANGING LT_MVKE.
  PERFORM F_GET_TVV1T USING LT_SO
                      CHANGING LT_TVV1T.
  PERFORM F_GET_TVV2T USING LT_SO
                      CHANGING LT_TVV2T.
  PERFORM F_GET_CREDIT_LIMIT USING LT_SO
                             CHANGING LT_ZSDSFIT036
                                      LF_CREDIT_CURRENCY
                                      LT_UKMBP_CMS_SGM.
  PERFORM F_GET_VBPA USING LT_SO
                     CHANGING LT_VBPA.
  IF S_PERNR[] IS NOT INITIAL.
    LOOP AT LT_VBPA INTO DATA(LS_VBPA) WHERE PERNR IN S_PERNR. "#EC CI_SORTSEQ
      EXIT.
    ENDLOOP.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.
  ENDIF.
  PERFORM F_GET_CHANGELOG USING LT_SO 'VBAK' 'CMGST'
                          CHANGING LT_CHLOG_CMGST.
  PERFORM F_GET_ZSDSSDT009 USING LT_SO
                           CHANGING LT_ZSDSSDT009.
  PERFORM F_GET_PRCD_ELEMENTS USING LT_SO
                              CHANGING LT_PRCD_ELEMENTS.
  PERFORM F_GET_TVAUT USING LT_SO
                      CHANGING LT_TVAUT.
  PERFORM F_GET_TVAGT  USING LT_SO
                       CHANGING LT_TVAGT.
  PERFORM F_GET_TVGRT USING LT_SO
                      CHANGING LT_TVGRT   .
  PERFORM F_GET_TVKBT USING LT_SO
                      CHANGING LT_TVKBT   .
  PERFORM F_GET_PA0002 USING LT_VBPA
                      CHANGING LT_PA0002  .
  PERFORM F_GET_T427 CHANGING LT_T247.
  PERFORM F_GET_T052U USING LT_SO
                      CHANGING LT_T052U.
  PERFORM F_GET_PROJ USING LT_SO
                     CHANGING LT_PROJ.
  PERFORM F_GET_ZSDSSDT010 USING LT_SO
                           CHANGING LT_ZSDSSDT010.
  PERFORM F_GET_DD07T CHANGING LT_DD07T.


  "----------------prepare data ---------------
  LOOP AT LT_SO ASSIGNING FIELD-SYMBOL(<L_SO>).

    CLEAR LS_DATA.
    READ TABLE LT_VBFA_SO_QT INTO DATA(LS_VBFA_SO_QT) WITH KEY VBELN = <L_SO>-VBELN
                                                               POSNN = <L_SO>-POSNR.
    IF SY-SUBRC = 0.
      LS_DATA-VBELN_QT  =  LS_VBFA_SO_QT-VBELV.
      READ TABLE LT_QT INTO DATA(LS_QT) WITH KEY VBELN = LS_VBFA_SO_QT-VBELV.
      IF SY-SUBRC = 0.
        LS_DATA-ERDAT_QT = LS_QT-ERDAT.
      ENDIF.
    ELSE.
      CLEAR LS_VBFA_SO_QT.
      IF S_QT IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    AT NEW VBELN_MAIN.
      LF_RUNNO = LF_RUNNO + 1.
    ENDAT.
    LS_DATA-RUNNO = LF_RUNNO.
    LS_DATA-VBELN_SO         = <L_SO>-VBELN.

    "SO-->DO
    CLEAR LF_VBTYP_N.
    IF <L_SO>-VBTYP = GC_VBTYP-SO.
      LF_VBTYP_N = GC_VBTYP-DO.
    ELSEIF  <L_SO>-VBTYP = GC_VBTYP-RT.
      LF_VBTYP_N = GC_VBTYP-DO.
    ELSE.
      CLEAR LF_VBTYP_N.
    ENDIF.
    IF LF_VBTYP_N IS NOT INITIAL.
      READ TABLE LT_VBFA_SO_DO INTO DATA(LS_VBFA_SO_DO) WITH KEY VBELV = <L_SO>-VBELN
                                                                 POSNV = <L_SO>-POSNR
                                                                 VBTYP_V = <L_SO>-VBTYP
                                                                 VBTYP_N = LF_VBTYP_N.
      IF SY-SUBRC = 0.
        READ TABLE LT_DO INTO DATA(LS_DO) WITH KEY VBELN = LS_VBFA_SO_DO-VBELN
                                                   WADAT = <L_SO>-EDATU.
        IF SY-SUBRC = 0.
          LS_DATA-VBELN_DO = LS_VBFA_SO_DO-VBELN.
          LS_DATA-ERDAT_DO = LS_DO-ERDAT.
          LS_DATA-ERZET_DO = LS_DO-ERZET.
          LS_DATA-BLDAT_DO = LS_DO-BLDAT.
        ENDIF.
      ELSE.
        CLEAR LS_VBFA_SO_DO.
      ENDIF.
    ENDIF.

    CLEAR LRT_VBTYP_N.

    "==== inv ======
    IF <L_SO>-VBTYP = GC_VBTYP-SO . "link inv from vbrk-vgbel
      "SO-->INV or DO-->INV
*      do -> inv
      READ TABLE LT_INV_BY_DO INTO DATA(LS_INV_BY_DO) WITH KEY VGBEL = LS_DATA-VBELN_DO.
      IF SY-SUBRC = 0.
        LS_DATA-VBELN_IV = LS_INV_BY_DO-VBELN.
        LS_DATA-FKDAT = LS_INV_BY_DO-FKDAT.
        LS_DATA-XBLNR = LS_INV_BY_DO-XBLNR.
        LS_DATA-BELNR = LS_INV_BY_DO-BELNR.
      ELSE.
        CLEAR LS_INV_BY_DO.
*       so -> inv
        READ TABLE LT_INV_BY_SO INTO DATA(LS_INV_BY_SO) WITH KEY VGBEL = <L_SO>-VBELN
                                                                 VGPOS = <L_SO>-POSNR.
        IF SY-SUBRC = 0.
          LS_DATA-VBELN_IV = LS_INV_BY_SO-VBELN.
          LS_DATA-FKDAT = LS_INV_BY_SO-FKDAT.
          LS_DATA-XBLNR = LS_INV_BY_SO-XBLNR.
          LS_DATA-BELNR = LS_INV_BY_SO-BELNR.
        ELSE.
          CLEAR LS_INV_BY_SO.
        ENDIF.
      ENDIF.
    ELSE.                       "link inv by doc flow vbfa
      IF  <L_SO>-VBTYP = GC_VBTYP-CN_REQ
      OR  <L_SO>-VBTYP = GC_VBTYP-RT.
        INSERT VALUE #( SIGN = 'I'
                        OPTION = 'EQ'
                        LOW = GC_VBTYP-CN )
        INTO TABLE LRT_VBTYP_N.

        INSERT VALUE #( SIGN = 'I'
                        OPTION = 'EQ'
                        LOW = GC_VBTYP-CN_CANC )
        INTO TABLE LRT_VBTYP_N.
      ELSEIF  <L_SO>-VBTYP = GC_VBTYP-DN_REQ.
        INSERT VALUE #( SIGN = 'I'
                        OPTION = 'EQ'
                        LOW = GC_VBTYP-DN )
        INTO TABLE LRT_VBTYP_N.

        INSERT VALUE #( SIGN = 'I'
                        OPTION = 'EQ'
                        LOW = GC_VBTYP-CN_CANC )
        INTO TABLE LRT_VBTYP_N.
      ELSE.
        CLEAR LRT_VBTYP_N.
      ENDIF.
      IF LRT_VBTYP_N IS NOT INITIAL.
        LOOP AT LT_VBFA_SO_IV INTO DATA(LS_VBFA_SO_IV) WHERE VBELV = <L_SO>-VBELN
                                                       AND   POSNV = <L_SO>-POSNR
                                                       AND   VBTYP_V = <L_SO>-VBTYP
                                                       AND   VBTYP_N IN LRT_VBTYP_N.
          EXIT.
        ENDLOOP.
        IF SY-SUBRC = 0.
          LS_DATA-VBELN_IV = LS_VBFA_SO_IV-VBELN.
          READ TABLE LT_INV INTO DATA(LS_INV) WITH KEY VBELN = LS_VBFA_SO_IV-VBELN.
          IF SY-SUBRC = 0.
            LS_DATA-FKDAT = LS_INV-FKDAT.
            LS_DATA-XBLNR = LS_INV-XBLNR.
            LS_DATA-BELNR = LS_INV-BELNR.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR LS_VBFA_SO_IV.
      ENDIF.
      IF LS_DATA-VBELN_IV IS NOT INITIAL.
        READ TABLE LT_VBFA_INV_REF INTO DATA(LS_VBFA_INV_REF) WITH KEY VBELN = LS_DATA-VBELN_IV.
        IF SY-SUBRC = 0.
          LS_DATA-INV_REF = LS_VBFA_INV_REF-VBELV.
        ELSE.
          PERFORM F_READ_TEXT USING 'ZH26'
                                    LS_DATA-VBELN_IV
                                    'VBBK'
                              CHANGING LS_DATA-INV_REF .
        ENDIF.
      ENDIF.
    ENDIF.


    IF <L_SO>-VBEP_VBELN IS NOT INITIAL.
      IF <L_SO>-LIFSP IS INITIAL
      AND LS_DATA-VBELN_DO IS NOT INITIAL.
        LS_DATA-LIFSP = 'Full Delivery'(V11).
      ELSE.
        LS_DATA-LIFSP = 'Not Delivery'(V12).
      ENDIF.
    ENDIF.
    LS_DATA-AUART = <L_SO>-AUART.
    LS_DATA-POSNR = <L_SO>-POSNR.
    LS_DATA-UEPOS = <L_SO>-UEPOS.

    LS_DATA-ETENR = <L_SO>-ETENR.
    LS_DATA-EDATU = <L_SO>-EDATU.
    LS_DATA-ERDAT = <L_SO>-ERDAT.
    LS_DATA-MATNR = <L_SO>-MATNR.
    LS_DATA-WMENG = <L_SO>-WMENG.
    LS_DATA-BMENG = <L_SO>-BMENG.
    LS_DATA-VRKME  = <L_SO>-VRKME.
    LS_DATA-MEINS  = <L_SO>-MEINS.
    LS_DATA-WAERK  = <L_SO>-WAERK.
    READ TABLE LT_T001L INTO DATA(LS_T001L) WITH KEY WERKS = <L_SO>-WERKS
                                                     LGORT = <L_SO>-LGORT.
    IF SY-SUBRC = 0.
      LS_DATA-LGOBE = LS_T001L-LGOBE.
    ELSE.
      CLEAR LS_T001L.
    ENDIF.
    READ TABLE LT_MVKE INTO DATA(LS_MVKE) WITH KEY MATNR = <L_SO>-MATNR
                                                   VKORG = <L_SO>-VKORG.
    IF SY-SUBRC = 0.
      LS_DATA-PRODH_MAT = LS_MVKE-PRODH.
    ELSE.
      CLEAR LS_MVKE.
    ENDIF.
    LS_DATA-PRODH  = <L_SO>-PRODH.
    LS_DATA-PRODH1 = <L_SO>-PRODH(5).
    IF S_PAPH1[] IS NOT INITIAL
    AND LS_DATA-PRODH1 NOT IN S_PAPH1.
      CONTINUE.
    ENDIF.
    LS_DATA-PRODH2 = <L_SO>-PRODH+5(5).
    IF S_PAPH2[] IS NOT INITIAL
    AND LS_DATA-PRODH2 NOT IN S_PAPH2.
      CONTINUE.
    ENDIF.
    LS_DATA-PRODH3 = <L_SO>-PRODH+10(8).
    IF S_PAPH3[] IS NOT INITIAL
    AND LS_DATA-PRODH3 NOT IN S_PAPH3.
      CONTINUE.
    ENDIF.
    LS_DATA-MVGR1  = <L_SO>-MVGR1.
    LS_DATA-AUDAT  = <L_SO>-AUDAT.
    LS_DATA-KVGR1  = <L_SO>-KVGR1.
    READ TABLE LT_TVV1T INTO DATA(LS_TVV1T) WITH KEY KVGR1 = <L_SO>-KVGR1.
    IF SY-SUBRC = 0.
      LS_DATA-KVGR1_TX = LS_TVV1T-BEZEI.
    ENDIF.
    LS_DATA-KVGR2  = <L_SO>-KVGR2.
    READ TABLE LT_TVV2T INTO DATA(LS_TVV2T) WITH KEY KVGR2 = <L_SO>-KVGR2.
    IF SY-SUBRC = 0.
      LS_DATA-KVGR2_TX = LS_TVV2T-BEZEI.
    ENDIF.

*   ----CREDIT_LIMIT
    IF <L_SO>-PSPHI IS NOT INITIAL.
      LOOP AT LT_ZSDSFIT036 INTO DATA(LS_036) WHERE KUNNR = <L_SO>-KUNNR
                                              AND   PSPHI = <L_SO>-PSPHI
                                              AND   STARTDATE <= <L_SO>-PRSDT
                                              AND   ENDDATE >= <L_SO>-PRSDT.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC = 0.
        LS_DATA-CREDIT_LIMIT = LS_036-CREDIT_LIMIT.
        LS_DATA-CREDIT_CURRENCY = LF_CREDIT_CURRENCY.
      ELSE.
        CLEAR LS_036.
      ENDIF.
    ELSE.
      READ TABLE LT_UKMBP_CMS_SGM INTO DATA(LS_UKMBP_CMS_SGM) WITH KEY PARTNER = <L_SO>-KUNNR.
      IF SY-SUBRC = 0.
        LS_DATA-CREDIT_LIMIT = LS_UKMBP_CMS_SGM-CREDIT_LIMIT.
        LS_DATA-CREDIT_CURRENCY = LF_CREDIT_CURRENCY.
      ELSE.
        CLEAR LS_UKMBP_CMS_SGM.
      ENDIF.
    ENDIF.
    IF <L_SO>-ABSTK = 'B' OR <L_SO>-ABSTK = 'C'.
      LS_DATA-CMGST_TX = 'Rejected'(V02).
    ELSEIF ( <L_SO>-CMGST = 'B' OR <L_SO>-CMGST = 'C' )
    AND <L_SO>-ABSTK = 'A'.
      LS_DATA-CMGST_TX = 'Pending Approve'(V13) .
    ELSEIF <L_SO>-CMGST = 'D' OR <L_SO>-CMGST = ''.
      LS_DATA-CMGST_TX = 'Approved'(V01).
    ENDIF.

    READ TABLE LT_CHLOG_CMGST INTO DATA(LS_CHLOG_CMGST) WITH KEY OBJECTID = <L_SO>-VBELN.
    IF SY-SUBRC = 0.
      LS_DATA-CMGST_USR = LS_CHLOG_CMGST-USERNAME.
      LS_DATA-CMGST_DATE = LS_CHLOG_CMGST-UDATE.
      LS_DATA-CMGST_TIME = LS_CHLOG_CMGST-UTIME.
    ELSE.
      CLEAR LS_CHLOG_CMGST.
    ENDIF.

**   xxxx  " Bank Name
    READ TABLE LT_VBPA INTO LS_VBPA WITH KEY VBELN = <L_SO>-VBELN
                                             PARVW = GC_PARVW-SALES.
    IF SY-SUBRC = 0.
      LS_DATA-PERNR = LS_VBPA-PERNR .
      READ TABLE LT_PA0002 INTO DATA(LS_PA0002) WITH KEY PERNR = LS_VBPA-PERNR.
      IF SY-SUBRC = 0.
        LS_DATA-PERNR_NAME = |{ LS_PA0002-VORNA } { LS_PA0002-NACHN }|.
      ENDIF.
    ENDIF.
    IF S_PERNR[] IS NOT INITIAL.
      IF LS_DATA-PERNR NOT IN S_PERNR.
        CONTINUE.
      ENDIF.
    ENDIF.
    LOOP AT LT_VBPA INTO LS_VBPA WHERE VBELN = <L_SO>-VBELN      ##INTO_OK
                                 AND   POSNR = LF_POSNR_ZERO
                                 AND   PARVW = GC_PARVW-BILL_TO .
      LS_DATA-BILLTO  = LS_VBPA-KUNNR.
      IF LS_VBPA-NATION = 'I'.
        LS_DATA-BILLTO_NAME_EN = |{ LS_VBPA-NAME1 } { LS_VBPA-NAME2 } { LS_VBPA-NAME3 } { LS_VBPA-NAME4 }|.
      ELSE.
        LS_DATA-BILLTO_NAME_TH = |{ LS_VBPA-NAME1 } { LS_VBPA-NAME2 } { LS_VBPA-NAME3 } { LS_VBPA-NAME4 }|.
        LS_DATA-BILLTO_CITY1 = LS_VBPA-CITY1.
        LS_DATA-BILLTO_STREET = LS_VBPA-STREET.
      ENDIF.
    ENDLOOP.

    CLEAR LF_FOUND_BOM.
    LOOP AT LT_SO INTO DATA(LS_SO_BOM) WHERE VBELN =  <L_SO>-VBELN  ##INTO_OK "#EC CI_SORTSEQ
                                       AND   UEPOS =  <L_SO>-POSNR
                                       AND   ETENR =  1 .
      LS_DATA-AMT_BOM   = LS_DATA-AMT_BOM + LS_SO_BOM-KZWI3.
      LS_DATA-TOTAL_BOM = LS_DATA-TOTAL_BOM + LS_SO_BOM-KZWI3 + LS_SO_BOM-MWSBP.
      LS_DATA-TAX_AMT   = LS_DATA-TAX_AMT + LS_SO_BOM-MWSBP.
      LS_DATA-HEAD_DISC = LS_DATA-HEAD_DISC + LS_SO_BOM-KZWI5.
      LF_FOUND_BOM      = ABAP_TRUE.
    ENDLOOP.
    IF LF_FOUND_BOM = ABAP_FALSE
    OR <L_SO>-UEPOS IS NOT INITIAL.
      IF <L_SO>-KWMENG <> 0.
        LS_DATA-PRICE_UNIT = <L_SO>-KZWI1 /  <L_SO>-KWMENG.
        LS_DATA-NETPRICE_UNIT = <L_SO>-KZWI3 /  <L_SO>-KWMENG.
      ENDIF.
      LS_DATA-NET_AMT = <L_SO>-KZWI3.
      LS_DATA-TOTAL_AMT = <L_SO>-KZWI3 +  <L_SO>-MWSBP.
      IF <L_SO>-UEPOS IS INITIAL.
        LS_DATA-TAX_AMT   = <L_SO>-MWSBP.
        LS_DATA-HEAD_DISC = <L_SO>-KZWI5.
      ENDIF.
      LS_DATA-TAX_AMT_ITEM = <L_SO>-MWSBP.
    ENDIF.
    READ TABLE LT_PRCD_ELEMENTS INTO DATA(LS_PRCD_ELEMENTS) WITH KEY KNUMV = <L_SO>-KNUMV
                                                                     KSCHL = GC_KSCHL-CASH_DISCOUNT.
    IF SY-SUBRC = 0.
      LS_DATA-CSH_DISC = LS_PRCD_ELEMENTS-KBETR.
    ENDIF.



    READ TABLE LT_ZSDSSDT009 INTO DATA(LS_009) WITH KEY  VBELN = <L_SO>-VBELN
                                                         POSNR = <L_SO>-POSNR
                                                         ETENR = <L_SO>-ETENR.
    IF SY-SUBRC = 0.
      IF LS_009-CO_APPV_STAT = 'Y'.
        LS_DATA-CO_APPV_STAT = 'X'.
      ENDIF.
      LS_DATA-CO_APPV_BY     = LS_009-CO_APPV_BY.
      LS_DATA-CO_APPV_DATE   = LS_009-CO_APPV_DATE.
      LS_DATA-CO_APPV_TIME   = LS_009-CO_APPV_TIME.
      IF LS_009-MGR_APPV_STAT = 'Y'.
        LS_DATA-MGR_APPV_STAT = 'X'.
      ENDIF.
      IF LS_DATA-MGR_APPV_STAT IS INITIAL.
        LS_DATA-WAITING_APPV = 'Waiting for MGR Approve'(V03).
      ENDIF.
      LS_DATA-MGR_APPV_BY   =  LS_009-MGR_APPV_BY.
      LS_DATA-MGR_APPV_DATE =  LS_009-MGR_APPV_DATE.
      LS_DATA-MGR_APPV_TIME =  LS_009-MGR_APPV_TIME.
    ENDIF.
    READ TABLE LT_TVAUT INTO DATA(LS_TVAUT) WITH KEY AUGRU = <L_SO>-AUGRU.
    IF SY-SUBRC = 0.
      LS_DATA-AUGRU_TX = LS_TVAUT-BEZEI.
    ENDIF.
    LS_DATA-BSTKD    = <L_SO>-BSTKD.
    PERFORM F_READ_TEXT USING 'ZH01'
                              <L_SO>-VBELN
                              'VBBK'
                        CHANGING LS_DATA-REASON .

    LS_DATA-BSTKD_E    = <L_SO>-BSTKD_E.
    LS_DATA-POSEX_E    = <L_SO>-POSEX_E.
    LS_DATA-ABGRU      = <L_SO>-ABGRU.
    READ TABLE LT_TVAGT INTO DATA(LS_TVAGT) WITH KEY ABGRU = <L_SO>-ABGRU.
    IF SY-SUBRC = 0.
      LS_DATA-ABGRU_TX = |{ <L_SO>-ABGRU } { LS_TVAGT-BEZEI }|.
    ENDIF.
    PERFORM F_READ_TEXT USING 'ZH10'
                              <L_SO>-VBELN
                              'VBBK'
                        CHANGING LS_DATA-REQUEST_REMARK .
    LS_DATA-VKGRP     = <L_SO>-VKGRP.
    READ TABLE LT_TVGRT INTO DATA(LS_TVGRT) WITH KEY VKGRP = <L_SO>-VKGRP.
    IF SY-SUBRC = 0.
      LS_DATA-VKGRP_TX = LS_TVGRT-BEZEI.
    ELSE.
      CLEAR LS_TVGRT.
    ENDIF.


    LS_DATA-VKBUR     = <L_SO>-VKBUR.
    READ TABLE LT_TVKBT INTO DATA(LS_TVKBT) WITH KEY VKBUR = <L_SO>-VKBUR.
    IF SY-SUBRC = 0.
      LS_DATA-VKBUR_TX = LS_TVKBT-BEZEI.
    ENDIF.

    LOOP AT LT_VBPA INTO LS_VBPA WHERE VBELN = <L_SO>-VBELN          ##INTO_OK
                                 AND   POSNR = LF_POSNR_ZERO
                                 AND   PARVW = GC_PARVW-SHIP_TO.
      LS_DATA-SHIPTO = LS_VBPA-KUNNR.
      IF LS_VBPA-NATION = ''.
        PERFORM F_SET_ADDRESS USING LS_VBPA
                        CHANGING LS_DATA-SHIPTO_H_ADR.
      ENDIF.
      IF LS_VBPA-NATION = 'I'.
        LS_DATA-SHIPTO_NAME_EN = |{ LS_VBPA-NAME1 } { LS_VBPA-NAME2 } { LS_VBPA-NAME3 } { LS_VBPA-NAME4 }|.
      ELSE.
        LS_DATA-SHIPTO_NAME_TH = |{ LS_VBPA-NAME1 } { LS_VBPA-NAME2 } { LS_VBPA-NAME3 } { LS_VBPA-NAME4 }|.
      ENDIF.
    ENDLOOP.
    READ TABLE LT_VBPA INTO LS_VBPA WITH KEY VBELN = <L_SO>-VBELN
                                             POSNR = <L_SO>-POSNR
                                             PARVW = GC_PARVW-SHIP_TO
                                             NATION = ''.
    IF SY-SUBRC = 0.
      PERFORM F_SET_ADDRESS USING LS_VBPA
                            CHANGING LS_DATA-SHIPTO_I_ADR.

    ENDIF.
    LOOP AT LT_VBPA INTO LS_VBPA WHERE VBELN = <L_SO>-VBELN          ##INTO_OK
                                 AND   POSNR = <L_SO>-POSNR
                                 AND   PARVW = GC_PARVW-SHIP_TO.
      IF LS_VBPA-NATION = ''.
        PERFORM F_SET_ADDRESS USING LS_VBPA
                        CHANGING LS_DATA-SHIPTO_I_ADR.
      ENDIF.
    ENDLOOP.

    LS_DATA-ERNAM     = <L_SO>-ERNAM.
    LS_DATA-BNAME     = <L_SO>-BNAME.
    LS_DATA-ZZ1_LOB_SO_SDI  = <L_SO>-ZZ1_LOB_SO_SDI.
    LS_DATA-PS_PSP_PNR  = <L_SO>-PS_PSP_PNR.
    LS_DATA-PSPHI       = <L_SO>-PSPHI.
    READ TABLE LT_PROJ INTO DATA(LS_PROJ) WITH KEY PSPNR = <L_SO>-PSPHI.
    IF SY-SUBRC = 0.
      LS_DATA-POST1 = LS_PROJ-POST1.
    ELSE.
      CLEAR LS_PROJ.
    ENDIF.
    LF_MONTH = <L_SO>-EDATU+4(2).
    READ TABLE LT_T247 INTO DATA(LS_T247) WITH KEY MNR = LF_MONTH.
    IF SY-SUBRC = 0.
      LS_DATA-MONTH = LS_T247-LTX.
    ELSE.
      CLEAR LS_T247.
    ENDIF.
    LS_DATA-YEAR   = <L_SO>-EDATU(4).
    LS_DATA-UPMAT  = <L_SO>-UPMAT.

    PERFORM F_READ_TEXT USING 'ZH04'
                              <L_SO>-VBELN
                              'VBBK'
                        CHANGING LS_DATA-CM_NO  .
    READ TABLE LT_ZSDSSDT010 INTO DATA(LS_010) WITH KEY VBELN = <L_SO>-VBELN
                                                        POSNR = <L_SO>-POSNR
                                                        ETENR = <L_SO>-ETENR.
    IF SY-SUBRC = 0.
      LS_DATA-ADV_DMBTR = LS_010-DMBTR.
      LS_DATA-ADV_BELNR = LS_010-BELNR.
    ENDIF.

    LS_DATA-ZTERM  =  <L_SO>-ZTERM.
    READ TABLE LT_T052U INTO DATA(LS_T052U) WITH KEY ZTERM = <L_SO>-ZTERM.
    IF SY-SUBRC = 0.
      LS_DATA-ZTERM_TX = LS_T052U-TEXT1.
    ENDIF.

    PERFORM F_READ_DDTEXT USING <L_SO>-LFSTK 'STATV' LT_DD07T
                          CHANGING LS_DATA-LFSTK_TX.

    PERFORM F_READ_DDTEXT USING <L_SO>-FKSAK 'STATV' LT_DD07T
                          CHANGING LS_DATA-FKSAK_TX.

    PERFORM F_READ_DDTEXT USING <L_SO>-SPSTG 'STATV' LT_DD07T
                          CHANGING LS_DATA-SPSTG_TX.
    PERFORM F_READ_DDTEXT USING <L_SO>-GBSTK 'STATV' LT_DD07T
                          CHANGING LS_DATA-GBSTK_TX.

    PERFORM F_READ_TEXT USING 'ZH13'
                              <L_SO>-VBELN
                              'VBBK'
                        CHANGING LS_DATA-INV_REMARK  .

**  Item Remark
    PERFORM F_READ_TEXT USING 'ZH11'
                              <L_SO>-VBELN
                              'VBBK'
                        CHANGING LS_DATA-LAND_NO   .
    LS_DATA-ZZPOB = <L_SO>-ZZPOB.

    APPEND LS_DATA TO CT_DATA.
  ENDLOOP.
  SORT CT_DATA BY RUNNO VBELN_SO EDATU POSNR.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SO_DATA
*&---------------------------------------------------------------------*
FORM F_GET_SO_DATA  CHANGING CT_SO TYPE TT_SO.
  SELECT A~VBELN AS VBELN_MAIN,
         A~VBELN,
         B~POSNR,
         C~ETENR,
         A~VBTYP,
         A~AUART,
         A~VKORG,
         A~AUDAT,
         A~KVGR1,
         A~KVGR2,
         A~AUGRU,
         A~ERNAM,
         A~BNAME,
         A~FKSAK,
         A~LFSTK,
         A~SPSTG,
         A~GBSTK,
         A~CMGST,
         A~ABSTK,
         A~VKGRP,
         A~VKBUR,
         A~KUNNR,
         A~CMPS_CM,
         A~KNUMV,
         A~ZZPOB,
         B~UEPOS,
         B~ERDAT,
         B~MATNR,
         B~KWMENG,
         B~VRKME,
         B~KLMENG,
         B~MEINS,
         B~WERKS,
         B~LGORT,
         B~PRODH,
         B~MVGR1,
         B~KZWI3,
         B~MWSBP,
         B~KZWI1,
         B~KZWI5,
         B~PS_PSP_PNR,
         E~PSPHI,
         B~ZZ1_LOB_SO_SDI,
         B~UPMAT,
         B~ABGRU,
         B~WAERK,
         B~LFGSA,
         C~VBELN,
         C~LIFSP,
         C~EDATU,
         C~BMENG,
         C~WMENG,
         D~PRSDT,
         D~BSTKD,
         D~BSTKD_E,
         D~POSEX_E,
         D~ZTERM
  FROM VBAK AS A INNER JOIN VBAP AS B
  ON A~VBELN = B~VBELN
                 LEFT OUTER JOIN VBEP AS C
  ON B~VBELN = C~VBELN
  AND B~POSNR = C~POSNR
                 LEFT OUTER JOIN VBKD AS D
  ON B~VBELN = D~VBELN
  AND D~POSNR = 0
                 LEFT OUTER JOIN PRPS AS E
  ON B~PS_PSP_PNR = E~PSPNR
  WHERE A~VKORG IN @S_VKORG
  AND   A~VTWEG IN @S_VTWEG
  AND   A~SPART IN @S_SPART
  AND   A~VKBUR IN @S_VKBUR
  AND   A~VKGRP IN @S_VKGRP
  AND   A~KUNNR IN @S_KUNNR
  AND   A~AUART IN @S_AUART
  AND   A~AUDAT IN @S_AUDAT
  AND   B~ERDAT IN @S_ERDAT
  AND   A~VBELN IN @S_VBELN
  AND   B~ABGRU IN @S_ABGRU
  AND   B~MATNR IN @S_MATNR
  AND   B~LGORT IN @S_LGORT
  AND   B~AUFNR IN @S_AUFNR
  AND   A~BNAME IN @S_BNAME
  AND   A~KVGR2 IN @S_KVGR2
  AND   A~ZZPOB IN @S_ZZPOB
  AND   B~PRODH IN @S_PRODH
  AND   C~EDATU IN @S_EDATU
  AND   A~VBTYP = @GC_VBTYP-SO
  AND   B~PSTYV IN @GR_PSTYV
  INTO TABLE @CT_SO.

  IF S_BSTKD IS NOT INITIAL.
    DELETE CT_SO WHERE BSTKD NOT IN S_BSTKD.            "#EC CI_SORTSEQ
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_vbfa_so_do
*&---------------------------------------------------------------------*
FORM F_GET_VBFA_SO_DO   USING UT_SO TYPE TT_SO
                            CHANGING CT_VBFA_SO_DO TYPE TT_VBFA_AFT.
  IF UT_SO[] IS NOT INITIAL.
    SELECT VBELV,
           POSNV,
           VBTYP_V,
           VBELN,
           POSNN,
           VBTYP_N
    FROM VBFA
    FOR ALL ENTRIES IN @UT_SO
    WHERE VBELV = @UT_SO-VBELN
    AND   POSNV = @UT_SO-POSNR
    AND   ( VBTYP_V = @GC_VBTYP-SO
    OR      VBTYP_V = @GC_VBTYP-RT )
    AND   ( VBTYP_N = @GC_VBTYP-DO
    OR      VBTYP_V = @GC_VBTYP-RT_DO )
    INTO TABLE @CT_VBFA_SO_DO.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_VBFA_SO_INV
*&---------------------------------------------------------------------*
FORM F_GET_VBFA_SO_INV  USING    UT_SO TYPE TT_SO
                        CHANGING CT_VBFA_DO_INV TYPE TT_VBFA_AFT.
  IF UT_SO IS NOT INITIAL.
    SELECT VBFA~VBELV,
           VBFA~POSNV,
           VBFA~VBTYP_V,
           VBFA~VBELN,
           VBFA~POSNN,
           VBFA~VBTYP_N
    FROM VBFA INNER JOIN VBRK
    ON VBFA~VBELN = VBRK~VBELN
    AND VBRK~FKSTO = ''
    FOR ALL ENTRIES IN @UT_SO
    WHERE VBELV = @UT_SO-VBELN
    AND   POSNV = @UT_SO-POSNR
    AND  ( VBTYP_V = @GC_VBTYP-SO
    OR     VBTYP_V = @GC_VBTYP-CN_REQ
    OR     VBTYP_V = @GC_VBTYP-DN_REQ
    OR     VBTYP_V = @GC_VBTYP-RT )
    AND  ( VBTYP_N = @GC_VBTYP-INV
    OR     VBTYP_N = @GC_VBTYP-CN
    OR     VBTYP_N = @GC_VBTYP-DN
    OR     VBTYP_N = @GC_VBTYP-CN_CANC )
    INTO TABLE @CT_VBFA_DO_INV.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_do
*&---------------------------------------------------------------------*
FORM F_GET_DO  USING   UT_VBFA_SO_DO TYPE TT_VBFA_AFT
               CHANGING CT_DO TYPE TT_DO.
  IF UT_VBFA_SO_DO IS NOT INITIAL.
    SELECT VBELN,
           WADAT,
           ERDAT,
           ERZET,
           BLDAT
    FROM LIKP
    FOR ALL ENTRIES IN @UT_VBFA_SO_DO
    WHERE VBELN = @UT_VBFA_SO_DO-VBELN
      INTO TABLE @CT_DO.                           "#EC CI_NO_TRANSFORM
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_get_vbfa_so_qt
*&---------------------------------------------------------------------*
FORM F_GET_VBFA_SO_QT  USING    UT_SO TYPE TT_SO
                       CHANGING CT_VBFA_SO_QT TYPE TT_VBFA_BF.
  IF UT_SO  IS NOT INITIAL.
    SELECT  VBELN,
            POSNN ,
            VBTYP_N,
            VBELV,
            POSNV,
            VBTYP_V
    FROM VBFA
    FOR ALL ENTRIES IN @UT_SO
    WHERE VBELN = @UT_SO-VBELN
    AND   POSNN = @UT_SO-POSNR
    AND   VBTYP_V = @GC_VBTYP-QUOT
    AND   VBELV   IN @S_QT
    INTO TABLE @CT_VBFA_SO_QT.                     "#EC CI_NO_TRANSFORM
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_t001l
*&---------------------------------------------------------------------*
FORM F_GET_T001L  USING    UT_SO TYPE TT_SO
                  CHANGING CT_T001L TYPE TT_T001L.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.
  LT_SO[] = UT_SO[].
  SORT LT_SO BY WERKS LGORT.
  DELETE LT_SO WHERE WERKS IS INITIAL
               OR LGORT IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING WERKS LGORT.
  IF LT_SO[] IS NOT INITIAL.
    SELECT WERKS,
           LGORT,
           LGOBE
    FROM T001L
    FOR ALL ENTRIES IN @LT_SO
    WHERE WERKS = @LT_SO-WERKS
    AND   LGORT = @LT_SO-LGORT
    INTO TABLE @CT_T001L.                          "#EC CI_NO_TRANSFORM
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_mvke
*&---------------------------------------------------------------------*
FORM F_GET_MVKE  USING    UT_SO TYPE TT_SO
                 CHANGING CT_MVKE TYPE TT_MVKE.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.
  LT_SO[] = UT_SO[].
  SORT LT_SO BY MATNR VKORG.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING MATNR VKORG.
  IF LT_SO[] IS NOT INITIAL.
    SELECT MATNR,                                  "#EC CI_NO_TRANSFORM
           VKORG,
           PRODH
    FROM MVKE
    FOR ALL ENTRIES IN @LT_SO
    WHERE MATNR = @LT_SO-MATNR
    AND   VKORG = @LT_SO-VKORG
    INTO TABLE @CT_MVKE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_tvv1t
*&---------------------------------------------------------------------*
FORM F_GET_TVV1T  USING    UT_SO TYPE TT_SO
                  CHANGING CT_TVV1T TYPE TT_TVV1T.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.
  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE KVGR1 IS INITIAL.
  SORT LT_SO BY KVGR1.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING KVGR1.
  IF LT_SO IS NOT INITIAL.
    SELECT KVGR1,
           BEZEI
    FROM TVV1T
    FOR ALL ENTRIES IN @LT_SO
    WHERE SPRAS = @SY-LANGU
    AND   KVGR1 = @LT_SO-KVGR1
    INTO TABLE @CT_TVV1T.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_tvv2t
*&---------------------------------------------------------------------*
FORM F_GET_TVV2T  USING    UT_SO TYPE TT_SO
                  CHANGING CT_TVV2T TYPE TT_TVV2T.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.
  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE KVGR2 IS INITIAL.
  SORT LT_SO BY KVGR1.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING KVGR2.
  IF LT_SO IS NOT INITIAL.
    SELECT KVGR2,
           BEZEI
    FROM TVV2T
    FOR ALL ENTRIES IN @LT_SO
    WHERE SPRAS = @SY-LANGU
    AND   KVGR2 = @LT_SO-KVGR2
    INTO TABLE @CT_TVV2T.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_credit_limit
*&---------------------------------------------------------------------*
FORM F_GET_CREDIT_LIMIT  USING    UT_SO TYPE TT_SO
                         CHANGING CT_ZSDSFIT036 TYPE TT_ZSDSFIT036
                                  CF_CREDIT_CURRENCY TYPE UKMCRED_SGM0C-CURRENCY
                                  CT_UKMBP_CMS_SGM TYPE TT_UKMBP_CMS_SGM.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.

  SELECT SINGLE CURRENCY
  FROM UKMCRED_SGM0C
  WHERE CREDIT_SGMNT = @GC_CREDIT_SGMNT
  INTO  @CF_CREDIT_CURRENCY .

  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE KUNNR IS INITIAL
               OR PSPHI IS INITIAL.
  SORT LT_SO BY KUNNR PSPHI.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING KUNNR PSPHI.
  IF LT_SO[] IS NOT INITIAL.
    SELECT KUNNR,
           PSPHI,
           STARTDATE,
           ENDDATE  ,
           SEQ      ,
           CREDIT_LIMIT
    FROM ZSDSFIT036
    FOR ALL ENTRIES IN @LT_SO
    WHERE KUNNR = @LT_SO-KUNNR
    AND   PSPHI = @LT_SO-PSPHI
    AND   ZDEL_FLG = '' "CH01+
    INTO TABLE @CT_ZSDSFIT036.
  ENDIF.
  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE KUNNR IS INITIAL.
  SORT LT_SO BY KUNNR.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING KUNNR.
  IF LT_SO[] IS NOT INITIAL.
    SELECT PARTNER,
           CREDIT_LIMIT
    FROM UKMBP_CMS_SGM
    FOR ALL ENTRIES IN @LT_SO
    WHERE PARTNER = @LT_SO-KUNNR
    AND CREDIT_SGMNT = @GC_CREDIT_SGMNT
    INTO TABLE @CT_UKMBP_CMS_SGM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_vbpa
*&---------------------------------------------------------------------*
FORM F_GET_VBPA  USING    UT_SO TYPE TT_SO
                 CHANGING CT_VBPA TYPE TT_VBPA.
  IF UT_SO[] IS NOT INITIAL.
    SELECT  A~VBELN,
            A~POSNR,
            A~PARVW,
            A~KUNNR,
            A~PERNR,
            A~ADRNR,
            B~NATION,
            B~NAME1,
            B~NAME2,
            B~NAME3,
            B~NAME4,
            B~STREET,
            B~STR_SUPPL3,
            B~LOCATION,
            B~STR_SUPPL1,
            B~STR_SUPPL2,
            B~CITY2,
            B~CITY1,
            B~POST_CODE1
    FROM VBPA AS A LEFT OUTER JOIN ADRC AS B
    ON A~ADRNR = B~ADDRNUMBER
    FOR ALL ENTRIES IN @UT_SO
    WHERE VBELN = @UT_SO-VBELN
    AND   POSNR = '000000'
    AND (  PARVW = @GC_PARVW-BILL_TO OR
           PARVW = @GC_PARVW-SALES OR
           PARVW = @GC_PARVW-SHIP_TO )
    INTO TABLE @CT_VBPA.                           "#EC CI_NO_TRANSFORM
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_changelog
*&---------------------------------------------------------------------*
FORM F_GET_CHANGELOG  USING    UT_SO TYPE TT_SO
                               UF_TABNAME TYPE CDPOS-TABNAME
                               UF_FNAME TYPE CDPOS-FNAME
                      CHANGING CT_CHLOG TYPE TT_CHLOG.
  TYPES: BEGIN OF LTS_KEY,
           VBELN TYPE CDHDR-OBJECTID,
         END OF LTS_KEY.
  DATA: LT_KEY TYPE STANDARD TABLE OF LTS_KEY .
  MOVE-CORRESPONDING UT_SO TO LT_KEY.
  SORT LT_KEY BY VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_KEY COMPARING VBELN.
  IF LT_KEY IS NOT INITIAL.
    SELECT A~OBJECTID,
           A~CHANGENR,
           A~USERNAME,
           A~UDATE,
           A~UTIME,
           A~TCODE,
           B~FNAME,
           B~VALUE_NEW,
           B~VALUE_OLD
     FROM CDHDR AS A INNER JOIN CDPOS AS B
     ON A~OBJECTCLAS  = B~OBJECTCLAS
     AND A~OBJECTID   = B~OBJECTID
     AND A~CHANGENR   = B~CHANGENR
     FOR ALL ENTRIES IN @LT_KEY
     WHERE A~OBJECTCLAS = @GC_OBJECTCLAS_SALE
     AND   A~OBJECTID = @LT_KEY-VBELN
     AND   B~TABNAME = @UF_TABNAME
     AND   B~FNAME = @UF_FNAME
     INTO TABLE @DATA(LT_CHLOG).

    SORT  LT_CHLOG BY OBJECTID UDATE DESCENDING UTIME DESCENDING.
    DELETE ADJACENT DUPLICATES FROM LT_CHLOG COMPARING OBJECTID .
    CT_CHLOG = LT_CHLOG.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_GET_ZSDSSDT009
*&---------------------------------------------------------------------*
FORM F_GET_ZSDSSDT009  USING    UT_SO TYPE TT_SO
                       CHANGING CT_ZSDSSDT009 TYPE TT_ZSDSSDT009.
  IF UT_SO[] IS NOT INITIAL.
    SELECT *
    FROM  ZSDSSDT009
    FOR ALL ENTRIES IN @UT_SO
    WHERE VBELN = @UT_SO-VBELN
    AND   POSNR = @UT_SO-POSNR
    AND   ETENR = @UT_SO-ETENR
    INTO TABLE @CT_ZSDSSDT009.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_GET_PRCD_ELEMENTS
*&---------------------------------------------------------------------*
FORM F_GET_PRCD_ELEMENTS  USING    UT_SO TYPE TT_SO
                          CHANGING CT_PRCD_ELEMENTS TYPE TT_PRCD_ELEMENTS.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.

  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE KNUMV IS INITIAL.
  SORT LT_SO BY KNUMV .
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING KNUMV .
  IF LT_SO IS NOT INITIAL.
    SELECT KNUMV,
           KPOSN,
           STUNR,
           ZAEHK,
           KSCHL,
           KBETR
    FROM PRCD_ELEMENTS
    FOR ALL ENTRIES IN @LT_SO
    WHERE KNUMV = @LT_SO-KNUMV
    AND   KSCHL = @GC_KSCHL-CASH_DISCOUNT
    INTO TABLE @CT_PRCD_ELEMENTS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_TVAUT
*&---------------------------------------------------------------------*
FORM F_GET_TVAUT  USING    UT_SO TYPE TT_SO
                  CHANGING CT_TVAUT TYPE TT_TVAUT.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.

  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE AUGRU IS INITIAL.
  SORT LT_SO BY AUGRU .
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING AUGRU .
  IF LT_SO IS NOT INITIAL.
    SELECT AUGRU,
           BEZEI
    FROM TVAUT
    FOR ALL ENTRIES IN @LT_SO
    WHERE AUGRU = @LT_SO-AUGRU
    AND   SPRAS = @SY-LANGU
    INTO TABLE @CT_TVAUT.
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
*& Form F_GET_QT
*&---------------------------------------------------------------------*
FORM F_GET_QT  USING    UT_VBFA_SO_QT TYPE TT_VBFA_BF
               CHANGING CT_QT TYPE TT_QT.
  IF UT_VBFA_SO_QT[] IS NOT INITIAL.
    SELECT VBELN,                                  "#EC CI_NO_TRANSFORM
           ERDAT
    FROM VBAK
    FOR ALL ENTRIES IN @UT_VBFA_SO_QT
    WHERE VBELN = @UT_VBFA_SO_QT-VBELV
    INTO TABLE @CT_QT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_TVAGT
*&---------------------------------------------------------------------*
FORM F_GET_TVAGT  USING    UT_SO TYPE TT_SO
                  CHANGING CT_TVAGT TYPE TT_TVAGT.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.

  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE ABGRU IS INITIAL.
  SORT LT_SO BY ABGRU .
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING ABGRU .
  IF LT_SO IS NOT INITIAL.
    SELECT ABGRU,
           BEZEI
    FROM TVAGT
    FOR ALL ENTRIES IN @LT_SO
    WHERE ABGRU = @LT_SO-ABGRU
    AND   SPRAS = @SY-LANGU
    INTO TABLE @CT_TVAGT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_TVGRT
*&---------------------------------------------------------------------*
FORM F_GET_TVGRT  USING    UT_SO TYPE TT_SO
                  CHANGING CT_TVGRT TYPE TT_TVGRT.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.

  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE VKGRP IS INITIAL.
  SORT LT_SO BY VKGRP.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING VKGRP .
  IF LT_SO IS NOT INITIAL.
    SELECT VKGRP,
           BEZEI
    FROM TVGRT
    FOR ALL ENTRIES IN @LT_SO
    WHERE VKGRP = @LT_SO-VKGRP
    AND   SPRAS = @SY-LANGU
    INTO TABLE @CT_TVGRT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_TVKBT
*&---------------------------------------------------------------------*
FORM F_GET_TVKBT  USING    UT_SO TYPE TT_SO
                  CHANGING CT_TVKBT TYPE TT_TVKBT.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.

  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE VKBUR IS INITIAL.
  SORT LT_SO BY VKBUR.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING VKBUR .
  IF LT_SO IS NOT INITIAL.
    SELECT VKBUR,
           BEZEI
    FROM TVKBT
    FOR ALL ENTRIES IN @LT_SO
    WHERE VKBUR = @LT_SO-VKBUR
    AND   SPRAS = @SY-LANGU
    INTO TABLE @CT_TVKBT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_pa0002
*&---------------------------------------------------------------------*
FORM F_GET_PA0002  USING    UT_VBPA TYPE TT_VBPA
                   CHANGING CT_PA0002 TYPE TT_PA0002.
  DATA: LT_VBPA TYPE STANDARD TABLE OF TS_VBPA.

  LT_VBPA[] = UT_VBPA[].
  DELETE LT_VBPA WHERE PERNR IS INITIAL.
  SORT LT_VBPA BY PERNR.
  DELETE ADJACENT DUPLICATES FROM LT_VBPA COMPARING PERNR .
  IF LT_VBPA IS NOT INITIAL.
    SELECT PERNR,
           BEGDA,
           ENDDA,
           VORNA,
           NACHN
    FROM PA0002
    FOR ALL ENTRIES IN @LT_VBPA
    WHERE PERNR = @LT_VBPA-PERNR
    AND   BEGDA <= @SY-DATUM
    AND   ENDDA >= @SY-DATUM
    INTO TABLE @CT_PA0002.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_t427
*&---------------------------------------------------------------------*
FORM F_GET_T427  CHANGING CT_T247 TYPE TT_T247.
  SELECT MNR,
         LTX
  FROM T247
  INTO TABLE @CT_T247
  WHERE SPRAS = @SY-LANGU.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_T052U
*&---------------------------------------------------------------------*
FORM F_GET_T052U  USING    UT_SO TYPE TT_SO
                  CHANGING CT_T052U TYPE TT_T052U.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.

  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE ZTERM IS INITIAL.
  SORT LT_SO BY ZTERM.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING ZTERM .
  IF LT_SO IS NOT INITIAL.
    SELECT ZTERM,
           TEXT1
    FROM T052U
    FOR ALL ENTRIES IN @LT_SO
    WHERE ZTERM = @LT_SO-ZTERM
    AND   SPRAS = @SY-LANGU
    INTO TABLE @CT_T052U.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_proj
*&---------------------------------------------------------------------*
FORM F_GET_PROJ  USING    UT_SO TYPE TT_SO
                 CHANGING CT_PROJ TYPE TT_PROJ.
  DATA: LT_SO TYPE STANDARD TABLE OF TS_SO.

  LT_SO[] = UT_SO[].
  DELETE LT_SO WHERE PSPHI IS INITIAL.
  SORT LT_SO BY PSPHI.
  DELETE ADJACENT DUPLICATES FROM LT_SO COMPARING PSPHI .
  IF LT_SO IS NOT INITIAL.
    SELECT PSPNR,
           POST1
    FROM PRoj
    FOR ALL ENTRIES IN @LT_SO
    WHERE PSPNR = @LT_SO-PSPHI
    INTO TABLE @CT_PROJ.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_ZSDSSDT010
*&---------------------------------------------------------------------*
FORM F_GET_ZSDSSDT010  USING    UT_SO TYPE TT_SO
                       CHANGING CT_ZSDSSDT010 TYPE TT_ZSDSSDT010.
  IF UT_SO IS NOT INITIAL.
    SELECT VBELN,
           POSNR,
           ETENR,
           DMBTR,
           WAERS,
           BELNR
    FROM ZSDSSDT010
    FOR ALL ENTRIES IN @UT_SO
    WHERE VBELN = @UT_SO-VBELN
    AND   POSNR = @UT_SO-POSNR
    AND   ETENR = @UT_SO-ETENR
    INTO TABLE @CT_ZSDSSDT010.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING   UT_DATA TYPE TT_DATA.
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

  PERFORM F_ALV_MODIFY_FC   CHANGING GT_FIELDCAT_1.

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
  CS_PRINT-NO_COLWOPT = GC_TRUE.
  CS_VARIANT-REPORT  = SY-REPID.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<L_FCAT>) .
    CASE <L_FCAT>-FIELDNAME.
      WHEN 'CREDIT_CURRENCY'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'VBELN_QT'.
        %FCAT 'Q/T No.'(C01).
        <L_FCAT>-HOTSPOT = GC_TRUE.
      WHEN 'VBELN_SO'.
        %FCAT 'S/O No.'(C02).
        <L_FCAT>-HOTSPOT = GC_TRUE.
      WHEN 'VBELN_DO'.
        %FCAT 'D/O No.'(C03).
        <L_FCAT>-HOTSPOT = GC_TRUE.
      WHEN 'VBELN_IV'.
        %FCAT 'Inv. No.'(C04).
        <L_FCAT>-HOTSPOT = GC_TRUE.
      WHEN 'LIFSP'.
        %FCAT 'Delivery block'(C05).
      WHEN 'AUART'.
        %FCAT 'Order Type'(D07).
      WHEN 'POSNR'.
        %FCAT 'Item'(C06).
      WHEN 'UEPOS'.
        %FCAT 'H-Item'(C07).
      WHEN 'ETENR'.
        %FCAT 'Schedule Line'(C08).
      WHEN 'EDATU'.
        %FCAT 'First Deli. Date'(C09).
      WHEN 'ERDAT'.
        %FCAT 'Item Created Date'(C10).
      WHEN 'MATNR'.
        %FCAT 'Material No.'(C11).
      WHEN 'WMENG'.
        %FCAT 'Qty.'(C12).
        <L_FCAT>-QFIELDNAME = 'VRKME'.
      WHEN 'BMENG'.
        %FCAT 'Confirm Qty.'(C13).
        <L_FCAT>-QFIELDNAME = 'VRKME'.
      WHEN 'VRKME'.
        %FCAT 'UOM'(C14).
      WHEN 'MEINS'.
        %FCAT 'Base Unit'(D06).
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'LGOBE'.
        %FCAT 'Storage Location'(C15).
      WHEN 'PRODH_MAT'.
        %FCAT 'Material Prodh.'(C16).
      WHEN 'PRODH'.
        %FCAT 'Prod. Hierarchy'(C17).
      WHEN 'PRODH1'.
        %FCAT 'Prodh. Lv.1'(C18).
      WHEN 'PRODH2'.
        %FCAT 'Prodh. Lv.2'(C19).
      WHEN 'PRODH3'.
        %FCAT 'Prodh. Lv.3'(C20).
      WHEN 'MVGR1'.
        %FCAT 'Material group1'(C21).
      WHEN 'AUDAT'.
        %FCAT 'Create Date'(C22).
      WHEN 'KVGR1'.
        %FCAT 'Cust. Grp1'(C23).
      WHEN 'KVGR1_TX'.
        %FCAT 'Cust. Grp1 Description'(C24).
      WHEN 'KVGR2'.
        %FCAT 'Cust. Grp2'(C25).
      WHEN 'KVGR2_TX'.
        %FCAT 'Cust. Grp2 Description'(C26).
      WHEN 'CREDIT_LIMIT'.
        %FCAT 'Credit Limit'(C27).
        <L_FCAT>-CFIELDNAME = 'CREDIT_CURRENCY'.
      WHEN 'CMGST_TX'.
        %FCAT 'Credit Status'(C28).
      WHEN 'CMGST_USR'.
        %FCAT 'Credit Status Name'(C29).
      WHEN 'CMGST_DATE'.
        %FCAT 'Credit Status Date'(C30).
      WHEN 'CMGST_TIME'.
        %FCAT 'Credit Status Time'(C31).
      WHEN 'BILLTO'.
        %FCAT 'Bill to Code'(C33).
      WHEN 'BILLTO_NAME_EN'.
        %FCAT 'Bill to Name EN'(C34).
      WHEN 'BILLTO_NAME_TH'.
        %FCAT 'Bill to Name TH'(C35).
      WHEN 'BILLTO_CITY1'.
        %FCAT 'City'(C36).
      WHEN 'BILLTO_STREET'.
        %FCAT 'Street'(C37).
      WHEN 'AMT_BOM'.
        %FCAT 'Amount Of BOM'(C38).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'TOTAL_BOM'.
        %FCAT 'Total Amount Of BOM'(C39).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'PRICE_UNIT'.
        %FCAT 'Price/Unit'(C40).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'NETPRICE_UNIT'.
        %FCAT 'Net Price/Unit'(C41).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'NET_AMT'.
        %FCAT 'Net Amount'(C42).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'TOTAL_AMT'.
        %FCAT 'Total Amount'(C43).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'TAX_AMT'.
        %FCAT 'Tax Amount'(C44).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'HEAD_DISC'.
        %FCAT 'Header Discount %'(C45).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'TAX_AMT_ITEM'.
        %FCAT 'Item Tax Amount'(C46).
        <L_FCAT>-CFIELDNAME = 'WAERK'.
      WHEN 'CSH_DISC'.
        %FCAT 'Cash Discount %'(C47).
      WHEN 'ERDAT_DO'.
        %FCAT 'Created Date D/O'(C48).
      WHEN 'ERZET_DO'.
        %FCAT 'Created Time D/O'(C49).
      WHEN 'BLDAT_DO'.
        %FCAT 'D/O. Date'(C50).
      WHEN 'CO_APPV_STAT'.
        %FCAT 'H. CRED Status'(C51).
        <L_FCAT>-CHECKBOX = ABAP_TRUE.
      WHEN 'CO_APPV_BY'.
        %FCAT 'H. CRED Name'(C52).
      WHEN 'CO_APPV_DATE'.
        %FCAT 'H. CRED Date'(C53).
      WHEN 'CO_APPV_TIME'.
        %FCAT 'H. CRED Time'(C54).
      WHEN 'MGR_APPV_STAT'.
        %FCAT 'MGR Status'(C55).
        <L_FCAT>-CHECKBOX = ABAP_TRUE.
      WHEN 'WAITING_APPV'.
        %FCAT 'Waiting for Manager Approve'(C56) .
      WHEN 'MGR_APPV_BY'.
        %FCAT 'MGR Name'(C57).
      WHEN 'MGR_APPV_DATE'.
        %FCAT 'MGR Date'(C58).
      WHEN 'MGR_APPV_TIME'.
        %FCAT 'MGR Time'(C59).
      WHEN 'AUGRU_TX'.
        %FCAT 'Order Reason'(C60).
      WHEN 'BSTKD'.
        %FCAT 'Main P/O No.'(C61).
      WHEN 'REASON'.
        %FCAT 'Reason'(C62).
      WHEN 'BSTKD_E'.
        %FCAT 'Sub P/O No.'(C63).
      WHEN 'POSEX_E'.
        %FCAT 'PO item'(C64).
      WHEN 'ERDAT_QT'.
        %FCAT 'Q/T Created Date'(C65).
      WHEN 'ABGRU'.
        %FCAT 'Rejection status'(C66).
      WHEN 'ABGRU_TX'.
        %FCAT 'Reason for Reject'(C67).
      WHEN 'REQUEST_REMARK'.
        %FCAT 'Request Remark'(C68).
      WHEN 'VKGRP'.
        %FCAT 'Sales Group'(C69).
      WHEN 'VKGRP_TX'.
        %FCAT 'Sales Group Desc.'(C70).
      WHEN 'PERNR'.
        %FCAT 'Sales Emp.'(C71).
      WHEN 'PERNR_NAME'.
        %FCAT 'Sales Name'(C72).
      WHEN 'VKBUR'.
        %FCAT 'Sales Office'(C73).
      WHEN 'VKBUR_TX'.
        %FCAT 'Sales Office Desc.'(C74).
      WHEN 'SHIPTO_H_ADR'.
        %FCAT 'Ship to address H.'(C75).
      WHEN 'SHIPTO_I_ADR'.
        %FCAT 'Ship to address I.'(C76).
      WHEN 'SHIPTO'.
        %FCAT 'Ship to Code'(C77).
      WHEN 'SHIPTO_NAME_EN'.
        %FCAT 'Ship to Name EN'(C78).
      WHEN 'SHIPTO_NAME_TH'.
        %FCAT 'Ship to Name TH'(C79).
      WHEN 'ERNAM'.
        %FCAT 'S/O Created By'(C80).
      WHEN 'BNAME'.
        %FCAT 'Web Number'(C81).
      WHEN 'ZZ1_LOB_SO_SDI'.
        %FCAT 'LOB'(C82).
      WHEN 'PS_PSP_PNR'.
        %FCAT 'WBS'(C83).
        <L_FCAT>-REF_TABLE = 'PRPS'.
        <L_FCAT>-REF_FIELD = 'PSPNR'.
      WHEN 'PSPHI'.
        %FCAT 'Cost Sheet No.'(C84).
        <L_FCAT>-REF_TABLE = 'PRPS'.
        <L_FCAT>-REF_FIELD = 'PSPNR'.
      WHEN 'POST1'.
        %FCAT 'Cost Sheet Desc.'(C85).
      WHEN 'MONTH'.
        %FCAT 'Month'(C86).
      WHEN 'YEAR'.
        %FCAT 'Year'(C87).
      WHEN 'UPMAT'.
        %FCAT 'UP Material'(C88).
      WHEN 'FKDAT'.
        %FCAT 'Inv. Date'(C89).
      WHEN 'XBLNR'.
        %FCAT 'Ref. Tax number'(C90).
      WHEN 'BELNR'.
        %FCAT 'Ref. FI document no.'(C91).
      WHEN 'CM_NO'.
        %FCAT 'CM. no.'(C92).
      WHEN 'ADV_DMBTR'.
        %FCAT 'Advance Receipt'(C93).
      WHEN 'ADV_BELNR'.
        %FCAT 'Advance Doc.'(C94).
      WHEN 'ZTERM'.
        %FCAT 'Payment Term'(C95).
      WHEN 'ZTERM_TX'.
        %FCAT 'Payment Term Desc.'(C96).
      WHEN 'FKSAK_TX'.
        %FCAT 'Bill.stat.order-rel.'(C97).
      WHEN 'LFSTK_TX'.
        %FCAT 'Delivery status'(C98).
      WHEN 'SPSTG_TX'.
        %FCAT 'Overall blkd status'(C99).
      WHEN 'GBSTK_TX'.
        %FCAT 'Overall status'(D01).
      WHEN 'INV_REMARK'.
        %FCAT 'Invoice Remark'(D02).
      WHEN 'ITEM_REMARK'.
        %FCAT 'Item Remark'(D03).
      WHEN 'LAND_NO'.
        %FCAT 'Land No'(D04).
      WHEN 'ZZPOB'.
        %FCAT 'POB'(D08).
      WHEN 'INV_REF'.
        %FCAT 'Reference Invoice'(D09).
      WHEN 'RUNNO'.
        %FCAT 'No.'(D05).
      WHEN 'BNAME_QT'.
        %FCAT 'Name QT.'(D10).
      WHEN 'IHREZ'.
        %FCAT 'Reference QT'(D11).
      WHEN 'DESCRIPTION'.
        %FCAT 'POB Description.'(D12).
      WHEN 'OBJECT_ID '.
        %FCAT 'Service Order'(D13).
      WHEN 'PO_NUMBER_SOLD'.
        %FCAT 'SF Number'(D14).
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_ADDRESS
*&---------------------------------------------------------------------*
FORM F_SET_ADDRESS  USING    US_VBPA TYPE TS_VBPA
                    CHANGING CF_ADDR TYPE ANY.

  CF_ADDR
   =    |{ US_VBPA-STREET } { US_VBPA-STR_SUPPL3 } { US_VBPA-LOCATION }|
   &&   | { US_VBPA-STR_SUPPL1 } { US_VBPA-STR_SUPPL2 } { US_VBPA-CITY2 }|
   &&   | { US_VBPA-CITY1 } { US_VBPA-POST_CODE1 }|.
  CONDENSE CF_ADDR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_inv
*&---------------------------------------------------------------------*
FORM F_GET_INV  USING    UT_VBFA TYPE TT_VBFA_AFT
                CHANGING CT_INV TYPE TT_INV.
  IF UT_VBFA[] IS NOT INITIAL.
    SELECT VBELN,
           FKDAT,
           XBLNR,
           BELNR,
           VBTYP
    FROM VBRK
    FOR ALL ENTRIES IN @UT_VBFA
    WHERE VBELN = @UT_VBFA-VBELN
    INTO TABLE @CT_INV.                            "#EC CI_NO_TRANSFORM
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

  INSERT VALUE #( FUNCTION = 'ZREFRESH'
                  ICON = '@UN@'
                  QUICKINFO = 'Refresh'(001) )
         INTO TABLE UF_OBJECT->MT_TOOLBAR.


ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.
  DATA: LS_STABLE  TYPE LVC_S_STBL.

  CASE UF_UCOMM.
    WHEN 'ZREFRESH'.
      PERFORM F_SELECT_AND_PREPARE_DATA CHANGING GT_DATA.
  ENDCASE.

  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_unlock_ZSDSFIT028
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW
                             US_COLUMN_ID TYPE LVC_S_COL ##CALLED.

  READ TABLE GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) INDEX US_ROW_ID-INDEX.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.
  CASE US_COLUMN_ID-FIELDNAME.
    WHEN 'VBELN_QT'.
      IF <L_DATA>-VBELN_SO IS NOT INITIAL.
        SET PARAMETER ID 'AGN' FIELD <L_DATA>-VBELN_QT.
        CALL TRANSACTION 'VA23' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
    WHEN 'VBELN_SO'.
      IF <L_DATA>-VBELN_SO IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD <L_DATA>-VBELN_SO.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
    WHEN 'VBELN_DO'.
      IF <L_DATA>-VBELN_DO IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD <L_DATA>-VBELN_DO.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.  "#EC CI_CALLTA
      ENDIF.
    WHEN 'VBELN_IV'.
      IF <L_DATA>-VBELN_IV IS NOT INITIAL.
        SET PARAMETER ID 'VF' FIELD <L_DATA>-VBELN_IV.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ENDIF.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DD07T
*&---------------------------------------------------------------------*

FORM F_GET_DD07T  CHANGING CT_DD07T TYPE TT_DD07T.
  SELECT *
  FROM DD07T
  WHERE DOMNAME = 'STATV'
  AND   DDLANGUAGE = @SY-LANGU
  AND   AS4LOCAL = 'A'
  INTO TABLE @CT_DD07T.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_DDTEXT
*&---------------------------------------------------------------------*
FORM F_READ_DDTEXT  USING    UF_VALUE TYPE ANY
                             UF_DOMNAME TYPE ANY
                             UT_DD07T TYPE TT_DD07T
                    CHANGING CF_VALUE_TX TYPE DD07T-DDTEXT.
  READ TABLE UT_DD07T INTO DATA(LS_DD07T) WITH KEY DOMNAME = UF_DOMNAME
                                                   DOMVALUE_L = UF_VALUE.
  IF SY-SUBRC = 0.
    CF_VALUE_TX = |{ UF_VALUE } { LS_DD07T-DDTEXT }|.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_cn_dn_return
*&---------------------------------------------------------------------*
FORM F_GET_CN_DN_RETURN  CHANGING CT_SO TYPE TT_SO.
  DATA: LT_REF_SO TYPE TT_SO.
  IF CT_SO[] IS INITIAL.
    RETURN.
  ENDIF.
  SELECT Z~VBELV AS VBELN_MAIN,
         A~VBELN,
         B~POSNR,
         C~ETENR,
         A~VBTYP,
         A~AUART,
         A~VKORG,
         A~AUDAT,
         A~KVGR1,
         A~KVGR2,
         A~AUGRU,
         A~ERNAM,
         A~BNAME,
         A~FKSAK,
         A~LFSTK,
         A~SPSTG,
         A~GBSTK,
         A~CMGST,
         A~ABSTK,
         A~VKGRP,
         A~VKBUR,
         A~KUNNR,
         A~CMPS_CM,
         A~KNUMV,
         A~ZZPOB,
         B~UEPOS,
         B~ERDAT,
         B~MATNR,
         B~KWMENG,
         B~VRKME,
         B~KLMENG,
         B~MEINS,
         B~WERKS,
         B~LGORT,
         B~PRODH,
         B~MVGR1,
         B~KZWI3,
         B~MWSBP,
         B~KZWI1,
         B~KZWI5,
         B~PS_PSP_PNR,
         E~PSPHI,
         B~ZZ1_LOB_SO_SDI,
         B~UPMAT,
         B~ABGRU,
         B~WAERK,
         B~LFGSA,
         C~VBELN,
         C~LIFSP,
         C~EDATU,
         C~BMENG,
         C~WMENG,
         D~PRSDT,
         D~BSTKD,
         D~BSTKD_E,
         D~POSEX_E,
         D~ZTERM
  FROM VBAK AS A INNER JOIN VBAP AS B
  ON A~VBELN = B~VBELN
                 LEFT OUTER JOIN VBEP AS C
  ON B~VBELN = C~VBELN
  AND B~POSNR = C~POSNR
                 LEFT OUTER JOIN VBKD AS D
  ON B~VBELN = D~VBELN
  AND D~POSNR = 0
                 LEFT OUTER JOIN PRPS AS E
  ON B~PS_PSP_PNR = E~PSPNR
                 INNER JOIN VBFA AS Z
  ON B~VBELN = Z~VBELN
  AND B~POSNR = Z~POSNN
  FOR ALL ENTRIES IN @CT_SO
  WHERE   Z~VBELV = @CT_SO-VBELN   "so
  AND     Z~POSNV = @CT_SO-POSNR   "so
  AND     Z~VBTYP_V = @GC_VBTYP-SO
  AND   ( Z~VBTYP_N = @GC_VBTYP-CN_REQ
   OR     Z~VBTYP_N = @GC_VBTYP-DN_REQ
   OR     Z~VBTYP_N = @GC_VBTYP-RT )
  INTO TABLE @LT_REF_SO.

  IF LT_REF_SO[] IS NOT INITIAL.
    INSERT LINES OF LT_REF_SO INTO TABLE CT_SO.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_INV_BY_DO
*&---------------------------------------------------------------------*
FORM F_GET_INV_BY_DO  USING    UT_DO TYPE TT_DO
                      CHANGING UT_INV_BY_DO TYPE TT_INV_BY_REF.
  IF UT_DO[] IS NOT INITIAL.
    SELECT VGBEL,                                  "#EC CI_NO_TRANSFORM
           VGPOS,
           VGTYP,
           VBRP~VBELN,
           FKDAT,
           XBLNR,
           BELNR
    FROM VBRP  INNER JOIN VBRK
    ON VBRP~VBELN = VBRK~VBELN
    FOR ALL ENTRIES IN @UT_DO
    WHERE VGBEL = @UT_DO-VBELN
    AND   VGTYP = @GC_VBTYP-DO
    AND   FKSTO = ''
    AND   VBTYP <> @GC_VBTYP-INV_CANC
    INTO TABLE @DATA(LT_INV_BY_DO).

    SORT LT_INV_BY_DO BY VGBEL VGPOS VBELN DESCENDING.
    DELETE ADJACENT DUPLICATES FROM LT_INV_BY_DO COMPARING VGBEL VGPOS.
    UT_INV_BY_DO = LT_INV_BY_DO.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_INV_BY_SO
*&---------------------------------------------------------------------*
FORM F_GET_INV_BY_SO  USING    UT_SO TYPE TT_SO
                      CHANGING UT_INV_BY_SO TYPE TT_INV_BY_REF.
  IF UT_SO[] IS NOT INITIAL.
    SELECT VGBEL,
           VGPOS,
           VGTYP,
           VBRP~VBELN,
           FKDAT,
           XBLNR,
           BELNR
    FROM VBRP  INNER JOIN VBRK
    ON VBRP~VBELN = VBRK~VBELN
    FOR ALL ENTRIES IN @UT_SO
    WHERE VGBEL = @UT_SO-VBELN
    AND   VGPOS = @UT_SO-POSNR
    AND   VGTYP = @GC_VBTYP-SO
    AND   FKSTO = ''
    AND   VBTYP <> @GC_VBTYP-INV_CANC
    INTO TABLE @DATA(LT_INV_BY_SO).

    SORT LT_INV_BY_SO BY VGBEL VGPOS VBELN DESCENDING.
    DELETE ADJACENT DUPLICATES FROM LT_INV_BY_SO COMPARING VGBEL VGPOS.
    UT_INV_BY_SO = LT_INV_BY_SO.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_INV_REF
*&---------------------------------------------------------------------*
FORM F_GET_INV_REF  USING    UT_INV TYPE TT_INV
                    CHANGING CT_IV_REF TYPE TT_VBFA_BF.
  DATA: LT_INV TYPE TT_INV.
  LT_INV[] = UT_INV.
  DELETE LT_INV WHERE VBTYP = GC_VBTYP-INV .            "#EC CI_SORTSEQ
  IF LT_INV[] IS NOT INITIAL.
    SELECT  VBFA~VBELN,
            POSNN ,
            VBTYP_N,
            VBELV,
            POSNV,
            VBTYP_V
    FROM VBFA INNER JOIN VBRK
    ON  VBFA~VBELV = VBRK~VBELN
    AND VBRK~FKSTO = ''
    FOR ALL ENTRIES IN @UT_INV
    WHERE VBFA~VBELN = @UT_INV-VBELN
    AND   VBTYP_N = @UT_INV-VBTYP
    AND   VBTYP_V = @GC_VBTYP-INV
    INTO TABLE @CT_IV_REF.                         "#EC CI_NO_TRANSFORM
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_data
*&---------------------------------------------------------------------*
FORM F_INIT_DATA .
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
    EXPORTING
      IF_REPID = SY-REPID
      IF_PARAM = 'ITEM_CAT'
    IMPORTING
      ET_RANGE = GR_PSTYV.
ENDFORM.
