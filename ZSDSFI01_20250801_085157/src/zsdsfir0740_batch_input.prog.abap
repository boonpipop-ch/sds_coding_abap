*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0740_BATCH_INPUT
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
***INCLUDE RFIDTRBOE1_BATCH_INPUT .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  batchdynpro
*&---------------------------------------------------------------------*
*       Start new dynpro in batch input
*----------------------------------------------------------------------*
*      --> program  name of program called by batch input
*      --> dynpro   name of dynpro called by btch input
*----------------------------------------------------------------------*
FORM BATCHDYNPRO USING PROGRAM TYPE CLIKE DYNPRO  TYPE CLIKE.
  CLEAR BDCTAB.
  BDCTAB-PROGRAM = PROGRAM.
  BDCTAB-DYNPRO = DYNPRO.
  BDCTAB-DYNBEGIN = 'X'.
  APPEND BDCTAB.
ENDFORM.                               " batchdynpro

*&---------------------------------------------------------------------*
*&      Form  batchfield
*&---------------------------------------------------------------------*
*       Insert new field in batch input
*----------------------------------------------------------------------*
*      --> fnam   name of batch input variable
*      --> fval   value of batch input variable
*----------------------------------------------------------------------*
FORM BATCHFIELD USING FNAM TYPE CLIKE FVAL TYPE CLIKE.
  CLEAR BDCTAB.
  BDCTAB-FNAM = FNAM.
  BDCTAB-FVAL = FVAL.
  APPEND BDCTAB.
ENDFORM.                    "batchfield
" batchfield
*&---------------------------------------------------------------------*
*&      Form  BATCH1
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'Outgoing checks/BoE to the bank'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH1 USING LT_WTAB LIKE GT_WTAB[].

  FIELD-SYMBOLS:  <FS_FM_PAYM_PUR> TYPE ANY. "Note 1787965

  DATA: LT_PRINTAB        TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_WTAB           TYPE BOE_TR_LIST,
        LS_DATA           TYPE BOE_GLOBAL_TURKEY,
        LF_BUPLA          LIKE BSEG-BUPLA,
        LF_BRNCH          LIKE BKPF-BRNCH,
        LF_VALUT          TYPE BSEG-VALUT, "Note 1000376
*   Note 1787965
*\        lf_dummy TYPE c.
        LF_DUMMY          TYPE C,
        LF_FIKRS          TYPE FM01-FIKRS,
        L_FLG_BKAKT       TYPE C,
        L_F_CONTROL_DATA  TYPE FMFI_CONTROL_DATA,
        C_FLG_PUR_ACTIVE  TYPE C,
        LS_BSEG           LIKE BSEG,
        LS_BSED           LIKE BSED,
        LF_SEL01          LIKE RF05A-SEL01,
        LF_BELNR          LIKE BSEG-BELNR,
        LF_GJAHR          LIKE BSEG-GJAHR,
        LF_BUZEI          LIKE BSEG-BUZEI,
        LF_PRTF           LIKE BSED-PORTF,
        L_FM_PAYM_PUR(30) TYPE C VALUE 'L_F_CONTROL_DATA-FM_PAYM_PUR'.
*   Note 1787965
* data for file creation:
  DATA: LT_FILEOUT  TYPE IDBOEINTDME OCCURS 0 WITH HEADER LINE,
        LT_DME_FILE TYPE TABLE OF DMEE_OUTPUT_FILE,
        BEGIN OF BOE_ADD_FILEDATA,
          BUKRS  LIKE T001-BUKRS,
          FLAGCB LIKE T045P-FLAGCB,
          BUPLA  LIKE BSEG-BUPLA,
          BRNCH  LIKE BKPF-BRNCH,
          BANKK  LIKE T012K-BANKN,
          HBKID  LIKE T012K-HBKID,
        END OF BOE_ADD_FILEDATA.
*  DATA: wa_fileout LIKE lt_fileout,
  DATA: WA_FILEOUT   TYPE IDBOEINTDME,
        WA_DME_FILE  LIKE LINE OF LT_DME_FILE,
        LWA_DME_FILE LIKE DMEE_OUTPUT_FILE,
        LWA_PC_FILE  LIKE DMEE_OUTPUT_FILE-LINE,
        LPC_FILE     LIKE LWA_PC_FILE OCCURS 0,
        WA_SADR      TYPE SADR.
  DATA: TREE_TYPE    LIKE DMEE_TREE_HEAD-TREE_TYPE VALUE 'BOE1',
        L_FILENAME   TYPE STRING,
        LP_XML       TYPE XFELD,
        L_SAVE       TYPE XFELD,
        L_DISPLAY(1) TYPE C.



* clear required global fields
* ----------------------------
  CLEAR: CHARWRBTR, CHARSGTXT, CHARZFBDT, CHARBUDAT, CHARBLDAT,
         CHARKURSF, CHARREF, BORDRO, COUNT1, COUNT2, BLART1, ZZUONR.

* prepare posting date and document date for batch input
* ------------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* get next free bordronumber and document type of posting
* -------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '02'.
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-34'.

*   Note 1787965
* check online payment update is active in FM
* -------------------------------------------
  CLEAR C_FLG_PUR_ACTIVE.
  CALL FUNCTION 'FMFK_BUKRS_CHECK_FMAKTIV'
    EXPORTING
      IP_BUKRS           = P_BUKRS
      IP_APPLC           = 'A'
    IMPORTING
      OP_IS_ACTIVE       = L_FLG_BKAKT
      OP_FIKRS           = LF_FIKRS
    EXCEPTIONS
      NO_FIKRS_FOR_BUKRS = 1.
  IF SY-SUBRC = 0.
    IF L_FLG_BKAKT = 'J'.
      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          FUNCNAME           = 'FMCA_GET_INIT_INFO'
        EXCEPTIONS
          FUNCTION_NOT_EXIST = 1
          OTHERS             = 2.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'FMCA_GET_INIT_INFO' ##EXISTS
          EXPORTING
            I_FIKRS          = LF_FIKRS
          CHANGING
            C_F_CONTROL_DATA = L_F_CONTROL_DATA.
        IF SY-SUBRC = 0.
          ASSIGN (L_FM_PAYM_PUR) TO <FS_FM_PAYM_PUR>.
          IF SY-SUBRC = 0 AND <FS_FM_PAYM_PUR> = 'X'.
            C_FLG_PUR_ACTIVE = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*   Note 1787965

* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.
* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        CLEAR: LF_BUPLA, LF_BRNCH.
        SELECT SINGLE BUPLA FROM BSEG INTO LF_BUPLA
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUZEI = LS_WTAB-BUZEI
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'BUPLA' FIELD LF_BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD LF_BUPLA.

        SELECT SINGLE BRNCH FROM BKPF INTO LF_BRNCH
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'JEA' FIELD LF_BRNCH.
      ENDIF.
*Note 1000376 START
      "Begin of T012K Replacement, CM80672
*      SELECT * FROM t012k INTO gs_t012k
*         WHERE bukrs = p_bukrs
*           AND wikon = gf_bankn.
*        exit.
*      ENDSELECT.
      TRY.
          DATA(LT_HBA_INSTANCE) = CL_FCLM_OBJECT_FACTORY=>GET_HBA_INSTANCES(
                                  IT_BUKRS = VALUE #( ( P_BUKRS ) ) ).
        CATCH CX_FCLM_BAM_HOUSE_BANK_ACCOUNT ##NO_HANDLER .
      ENDTRY.
      LOOP AT LT_HBA_INSTANCE ASSIGNING FIELD-SYMBOL(<FS_HBA_INSTANCE>).
        <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_VALIDITY_PERIOD(
          IMPORTING
            EV_VALID_FROM = DATA(LV_HBA_VALID_FROM)
            EV_VALID_TO   = DATA(LV_HBA_VALID_TO)
        ).
        IF LV_HBA_VALID_FROM > SY-DATUM OR LV_HBA_VALID_TO < SY-DATUM.
          CONTINUE.
        ENDIF.
        DATA(LT_ACLINK2) = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_BANK_ACCOUNT~GET_ACLINK2_FIELDS(
                             IT_REQ_FIELDS = VALUE #(
                                                    ( 'FDGRP' ) ( 'ABWAE' ) ( 'WEKON' ) ( 'MINDT' )
                                                    ( 'HBID1' ) ( 'HKID1' ) ( 'HBID2' ) ( 'HKID2' )
                                                    ( 'WKKON' ) ( 'WIKON' )
                                                  )
                          ).
        DATA(LS_ACLINK2) = LT_ACLINK2[ ACC_ID   = <FS_HBA_INSTANCE>-ACC_ID
                                       GUID     = <FS_HBA_INSTANCE>-GUID
                                       REVISION = IF_FCLM_BANK_ACCOUNT=>GC_ACTIVE_REVISION ].
        IF LS_ACLINK2-WIKON EQ GF_BANKN.
          GS_T012K-BUKRS = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~M_COMPANY_CODE.
          GS_T012K-HBKID = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~M_HOUSEBANK_ID.
          GS_T012K-HKTID = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~M_ACCOUNT_ID.
          GS_T012K-BANKN = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_BANK_ACCT( ).
          GS_T012K-BKONT = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_CTRL_KEY( ).
          GS_T012K-WAERS = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_CURRENCY( ).
          GS_T012K-REFZL = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_REFZL( ).
          GS_T012K-DTAAI = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_DTAAI( ).
          GS_T012K-BNKN2 = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_BANK_ACCT_2( ).
          GS_T012K-HKONT = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_GL_ACCOUNT( ).
          GS_T012K-FDGRP = LS_ACLINK2-FDGRP.
          GS_T012K-ABWAE = LS_ACLINK2-ABWAE.
          GS_T012K-WEKON = LS_ACLINK2-WEKON.
          GS_T012K-MINDT = LS_ACLINK2-MINDT.
          GS_T012K-HBID1 = LS_ACLINK2-HBID1.
          GS_T012K-HKID1 = LS_ACLINK2-HKID1.
          GS_T012K-HBID2 = LS_ACLINK2-HBID2.
          GS_T012K-HKID2 = LS_ACLINK2-HKID2.
          GS_T012K-WKKON = LS_ACLINK2-WKKON.
          GS_T012K-WIKON = LS_ACLINK2-WIKON.
          EXIT.
        ENDIF.
      ENDLOOP.
      "End of T012K Replacement, CM80672
*==> Start of Note 3085192
      IF GF_HBKID IS NOT INITIAL.
        GS_T012K-HBKID = GF_HBKID.
        GS_T012K-HKTID = GF_HKTID.
      ENDIF.
*==> End of Note 3085192
* get value date and prepare for batch input
      LF_VALUT = LS_WTAB-ZFBDT.
      PERFORM GET_VALUE_DATE USING P_BUKRS
                                   GS_T012K-HBKID
                                   GS_T012K-HKTID
                                   LS_WTAB-BANK
                          CHANGING LF_VALUT.
      IF GF_DATFM = '2' OR GF_DATFM = '3'.
        CONCATENATE LF_VALUT+4(2) LF_VALUT+6(2) LF_VALUT+0(4) INTO
                                                    CHARZFBDT.
      ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
        CONCATENATE LF_VALUT+0(4) LF_VALUT+4(2) LF_VALUT+6(2) INTO
                                                    CHARZFBDT.
      ELSE.
        CONCATENATE LF_VALUT+6(2) LF_VALUT+4(2) LF_VALUT+0(4) INTO
                                                    CHARZFBDT.
      ENDIF.
*Note 1000376 END
*   Note 1787965
* If Online payment update is active in FM
      IF C_FLG_PUR_ACTIVE = 'X'.
        SELECT SINGLE * FROM *BSED INTO LS_BSED
                                  WHERE BUKRS = P_BUKRS
                                    AND BELNR = LS_WTAB-BELNR
                                    AND GJAHR = LS_WTAB-GJAHR
                                    AND BUZEI = LS_WTAB-BUZEI.
        SELECT SINGLE * FROM *BSEG INTO LS_BSEG
                                  WHERE BUKRS = P_BUKRS
                                    AND BELNR = LS_WTAB-BELNR
                                    AND GJAHR = LS_WTAB-GJAHR
                                    AND BUZEI = LS_WTAB-BUZEI.
* Check for any unfinished credit postings to the portfolio
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR INTO LF_SEL01.
*==> Start of Note 3111672
*  replace bsid with bsid_view for Remove SQL View Usages
*        SELECT single belnr gjahr buzei FROM  BSID
*               INTO (lf_belnr,lf_gjahr,lf_buzei)
*               WHERE  BUKRS  = p_bukrs
*               AND    KUNNR  = ls_bseg-kunnr
*               AND    UMSKS  = ls_bseg-umsks
*               AND    UMSKZ  = ls_bseg-umskz
*               AND    ZUONR  = lf_sel01
*               AND    SHKZG  = 'H'.

        SELECT SINGLE BELNR, GJAHR, BUZEI FROM BSID_VIEW
               INTO (@LF_BELNR,@LF_GJAHR,@LF_BUZEI)
               WHERE  BUKRS  = @P_BUKRS
               AND    KUNNR  = @LS_BSEG-KUNNR
               AND    UMSKS  = @LS_BSEG-UMSKS
               AND    UMSKZ  = @LS_BSEG-UMSKZ
               AND    ZUONR  = @LF_SEL01
               AND    SHKZG  = 'H'.
*==> Start of Note 3111672
        IF SY-SUBRC = 0.
          SELECT SINGLE PORTF  FROM  BSED
                 INTO   LF_PRTF
                 WHERE  BUKRS  = P_BUKRS
                 AND    BELNR  = LF_BELNR
                 AND    GJAHR  = LF_GJAHR
                 AND    BUZEI  = LF_BUZEI
                 AND    PORTF  = LS_BSED-PORTF.
          IF SY-SUBRC = 0.
* create error message
*---------------------------------------------------------------
            MESSAGE E213 WITH LF_BELNR LF_GJAHR INTO LF_DUMMY.
            PERFORM ADD_LINE USING SY GT_SYMESS.
            ADD 1 TO COUNT1.
            LS_WTAB-BOX = '2'.
            MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
            PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                  LS_WTAB-BELNR
                            CHANGING LF_DUMMY.
            IF NOT ( LF_DUMMY IS INITIAL ).
              PERFORM ADD_LINE USING SY GT_SYMESS.
            ENDIF.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
*   Note 1787965

* call transaction: check/BoE usage posting
* -----------------------------------------
      REFRESH BDCTAB.
      "WRITE ls_wtab-wrbtr TO charwrbtr CURRENCY ls_wtab-waers.  "AFLE Change
      PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-WRBTR LS_WTAB-WAERS
                                       CHANGING CHARWRBTR.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '101'.
      PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO ZZUONR.
      PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
      IF LS_WTAB-BANK IS INITIAL.
        CHARSGTXT = LS_WTAB-BOENO.
      ELSE.
        CONCATENATE LS_WTAB-BOENO '/' LS_WTAB-BANK INTO CHARSGTXT.
      ENDIF.
      IF P_WAERS NE GS_T001-WAERS AND
         NOT ( GF_KURSF IS INITIAL ).
        WRITE GF_KURSF TO CHARKURSF.
        PERFORM BATCHFIELD USING 'BKPF-KURSF' CHARKURSF.
      ENDIF.
      PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' LS_WTAB-WAERS.
      IF NOT LS_WTAB-GSBER IS INITIAL.
        PERFORM BATCHFIELD USING 'BSEG-GSBER' LS_WTAB-GSBER.
      ENDIF.
      PERFORM BATCHFIELD USING 'RF05A-KONTO' GF_BANKN.
*==> Start of Note 3085192
      IF GF_BANK_FLAG = ABAP_TRUE.
        PERFORM BATCHFIELD USING 'BSEG-HBKID' GF_HBKID.
        PERFORM BATCHFIELD USING 'BSEG-HKTID' GF_HKTID.
      ENDIF.
*==> End of Note 3085192
*      WRITE ls_wtab-zfbdt TO charzfbdt. "Note 1000376
      PERFORM BATCHFIELD USING 'BSEG-VALUT' CHARZFBDT.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'SW'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '705'.
      PERFORM BATCHFIELD USING '*BKPF-BELNR(1)' LS_WTAB-BELNR.
      PERFORM BATCHFIELD USING '*BSEG-BUZEI(1)' LS_WTAB-BUZEI.
      PERFORM BATCHFIELD USING '*BKPF-GJAHR(1)' LS_WTAB-GJAHR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-34' USING BDCTAB MODE MODE UPDATE UPDATE
                                   MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ELSE.
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
        ADD 1 TO COUNT2.
        LS_WTAB-BOX    = '1'.
        LS_WTAB-CHEC   = '1'.
        LS_WTAB-XBLNR  = BORDRO.
        LS_WTAB-PORTF1 = P_PORT1.
        LS_WTAB-BANKN  = GF_BANKN.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                                 PORTF1 BANKN.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ---------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

* at least one BoE successfully posted?
* clear status of SAPScript print for next postings
* -------------------------------------
  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1' OR
       LS_WTAB-BOX = '1'.          "file re-creation
      IF LS_WTAB-CHEC = '1'.
        TEST = '1'.
      ENDIF.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660


* SAPScript print
* ---------------
  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-003.
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.

***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    IF PSCRIPT = 'X'  .
*ENDFORM.                    " form reprint
***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660

      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

    ELSE.
      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
**  move ls_data-user into output_header_table-tran.
*    output_header_table-status = '@0V@'.
*    output_header_table-expand = s_single.
      BOE_GLOBAL-USER = SY-UNAME.

      PERFORM OPEN_FORM_PDF.

      PERFORM ITEM_DATA_PDF.
    ENDIF.
***********END of PDF conversion ,Date: 26/06/2008 ,C5112660

  ENDIF.

* ----------------------------------------------------
* File creation
* ----------------
  IF GF_FILE EQ 'X' AND TEST = '1'.

    CLEAR: WA_SADR,
           WA_FILEOUT,
           LT_FILEOUT,
           BOE_ADD_FILEDATA,
           WA_DME_FILE,
           L_FILENAME,
           LP_XML.
    REFRESH: LT_FILEOUT,
             LT_DME_FILE.
    .

* fill idboehead with address data
    SELECT SINGLE * FROM SADR INTO CORRESPONDING FIELDS OF WA_SADR
                     WHERE ADRNR EQ GS_T001-ADRNR.

    MOVE-CORRESPONDING WA_SADR TO LT_FILEOUT-IDBOEHEAD.
    LT_FILEOUT-IDBOEHEAD-BUKRS = GS_T001-BUKRS.

* fill idboeitem from lt_printab and get additional data
    BOE_ADD_FILEDATA-BUKRS  = GS_T001-BUKRS.
    BOE_ADD_FILEDATA-FLAGCB = GF_FLAGCB1.      "t045p-flagcb
    BOE_ADD_FILEDATA-BUPLA  = LF_BUPLA.        "bseg-bupla
    BOE_ADD_FILEDATA-BRNCH  = LF_BRNCH.        "bkpf-brnch

*Note 1000376 START
    BOE_ADD_FILEDATA-BANKK =  GS_T012K-BANKN.
    BOE_ADD_FILEDATA-HBKID =  GS_T012K-HBKID.

*    SELECT * FROM t012k INTO gs_t012k
*       WHERE bukrs = p_bukrs
*         AND wikon = gf_bankn.
*      boe_add_filedata-bankk =  gs_t012k-bankn.
*      boe_add_filedata-hbkid =  gs_t012k-hbkid.
*    ENDSELECT.
*Note 1000376 END

    CALL FUNCTION 'GET_BOE_FILE_DATA'
      EXPORTING
        I_MORE_DATA = BOE_ADD_FILEDATA
      TABLES
        FILE_OUT    = LT_FILEOUT
        BOE_DATA    = LT_PRINTAB[].

    READ TABLE LT_FILEOUT INDEX 1.
    IF SY-SUBRC EQ '0'.
      WA_FILEOUT = LT_FILEOUT.
    ENDIF.

* initialize DME Engine
    CALL FUNCTION 'DMEE_START'
      EXPORTING
        I_TREE_TYPE = TREE_TYPE
        I_TREE_ID   = GF_TREEID
        ITEM        = WA_FILEOUT
      TABLES
        FILE_OUTPUT = LT_DME_FILE.

* Create DTA_File with DME Engine
    CLEAR WA_FILEOUT.
    LOOP AT LT_FILEOUT INTO WA_FILEOUT.
      CALL FUNCTION 'DMEE_PUT_ITEM'
        EXPORTING
          ITEM        = WA_FILEOUT
        TABLES
          FILE_OUTPUT = LT_DME_FILE.
    ENDLOOP.

* Close DMEEngine
    CALL FUNCTION 'DMEE_END'
      IMPORTING
        E_XML       = LP_XML
      TABLES
        FILE_OUTPUT = LT_DME_FILE.

* DMEE tree for flat file output?
    IF LP_XML IS INITIAL.
* download flat file to presentation server

      IF GF_LOCATION EQ '1'.     "download to PC
        LOOP AT LT_DME_FILE INTO LWA_DME_FILE.
          MOVE LWA_DME_FILE-LINE TO LWA_PC_FILE.
          APPEND LWA_PC_FILE TO LPC_FILE.
        ENDLOOP.
        L_FILENAME = GF_FILENAME.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            FILENAME         = L_FILENAME
            FILETYPE         = 'ASC'
          TABLES
*           data_tab         = lt_dme_file
            DATA_TAB         = LPC_FILE
          EXCEPTIONS
            FILE_WRITE_ERROR = 1
            OTHERS           = 22.
        IF SY-SUBRC EQ '0'.
          MESSAGE S113(IDFI) WITH GF_FILENAME.
        ENDIF.
      ELSE.

* download flat file to application server
* begin of note                                           "1509654
        CALL FUNCTION 'FILE_VALIDATE_NAME'
          EXPORTING
*           CLIENT                     = SY-MANDT
            LOGICAL_FILENAME           = GC_FILENAME
            PARAMETER_1                = SY-CPROG
*           PARAMETER_2                = ' '
*           PARAMETER_3                = ' '
*           WITH_FILE_EXTENSION        = ' '
*           USE_BUFFER                 = ' '
*           ELIMINATE_BLANKS           = 'X'
* IMPORTING
*           VALIDATION_ACTIVE          =
*           TS_ALIAS                   =
          CHANGING
            PHYSICAL_FILENAME          = GF_FILENAME
          EXCEPTIONS
            LOGICAL_FILENAME_NOT_FOUND = 1
            VALIDATION_FAILED          = 2
            OTHERS                     = 3.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
* end of note                                             "1509654
        OPEN DATASET GF_FILENAME FOR OUTPUT IN TEXT MODE
                                            ENCODING NON-UNICODE
                                            IGNORING CONVERSION ERRORS.
        LOOP AT LT_DME_FILE INTO WA_DME_FILE.
          TRANSFER WA_DME_FILE TO GF_FILENAME.
        ENDLOOP.
        CLOSE DATASET GF_FILENAME.
        IF SY-SUBRC = 0.
          MESSAGE S113(IDFI) WITH GF_FILENAME.
        ENDIF.
      ENDIF.
    ENDIF.                                             " flat file

* DMEE tree for xml file creation?
    IF LP_XML EQ 'X' AND
       GF_LOCATION EQ '1'.                         "download to PC

* Display XML file and download XML file to presentation server
      L_FILENAME = GF_FILENAME.
      L_SAVE = 'X'.
      L_DISPLAY = 'X'.
      CALL FUNCTION 'DMEE_HANDLE_XML_DOC_PC'
        EXPORTING
          I_FILENAME = L_FILENAME
          I_SAVE     = L_SAVE
          I_DISPLAY  = L_DISPLAY.
      CALL FUNCTION 'DMEE_HANDLE_XML_DOCUMENT'
        EXPORTING
          I_FILENAME = L_FILENAME
          I_SAVE     = L_SAVE
          I_DISPLAY  = L_DISPLAY.
    ENDIF.

* Display XML file and download it to application server
    IF LP_XML EQ 'X' AND
       GF_LOCATION EQ '2'.                    "download to applic.serv.
      L_FILENAME = GF_FILENAME.
      L_SAVE = 'X'.
      L_DISPLAY = 'X'.
      CALL FUNCTION 'DMEE_HANDLE_XML_DOCUMENT'
        EXPORTING
          I_FILENAME = L_FILENAME
          I_SAVE     = L_SAVE
          I_DISPLAY  = L_DISPLAY.
    ENDIF.
  ENDIF.
ENDFORM.                                                    " BATCH1

*&---------------------------------------------------------------------*
*&      Form  BATCH2
*&---------------------------------------------------------------------*
*       Batch Input table for transaction 'FBWE'
*       'Outgoing checks/BoE to the vendor'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH2 USING LT_WTAB LIKE GT_WTAB[].

  FIELD-SYMBOLS:  <FS_FM_PAYM_PUR> TYPE ANY. "Note 1787965

  DATA: LT_PRINTAB        TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_WTAB           TYPE BOE_TR_LIST,
        LS_DATA           TYPE BOE_GLOBAL_TURKEY,
        LF_BELNR          LIKE BSEG-BELNR,
        LF_KONTO          LIKE BSEG-HKONT,
        LF_GJAHR          LIKE BSEG-GJAHR,
        LF_LINE(2)        TYPE C,
        LF_LINECOUNT      TYPE I,
        LF_DUMMY          TYPE C,
        LF_BUPLA          LIKE BSEG-BUPLA,
        LF_BRNCH          LIKE BKPF-BRNCH,
*   Note 1787965
*\        lf_selzu LIKE bseg-zuonr.
        LF_SELZU          LIKE BSEG-ZUONR,
        LF_FIKRS          TYPE FM01-FIKRS,
        L_FLG_BKAKT       TYPE C,
        L_F_CONTROL_DATA  TYPE FMFI_CONTROL_DATA,
        C_FLG_PUR_ACTIVE  TYPE C,
        LS_BSEG           LIKE BSEG,
        LS_BSED           LIKE BSED,
        LF_SEL01          LIKE RF05A-SEL01,
        LF_BELNRA         LIKE BSEG-BELNR,
        LF_GJAHRA         LIKE BSEG-GJAHR,
        LF_BUZEIA         LIKE BSEG-BUZEI,
        LF_PRTF           LIKE BSED-PORTF,
        L_FM_PAYM_PUR(30) TYPE C VALUE 'L_F_CONTROL_DATA-FM_PAYM_PUR'.
*   Note 1787965

* clear required global fields
* ----------------------------
*  CLEAR: charwrbtr, charsgtxt, charzfbdt, charbudat, charbldat,
*         charxpos, charref, bordro, count1, count2, blart1, blart2,
*         bschl1, zzuonr.
  CLEAR: CHARWRBTR,CHARKURSF, CHARSGTXT, CHARZFBDT, CHARBUDAT, CHARBLDAT,CHARXPOS, CHARREF, BORDRO, COUNT1, COUNT2, BLART1, BLART2,
         BSCHL1, ZZUONR.

* prepare posting date and document date for batch input
* ------------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* field bsed-vendr must be changeable by FB02
* -------------------------------------------
  PERFORM CHECK_DOC_CHANGE_RULES USING 'BSED-VENDR'.

* get line of radiobutton 'Allocation' on screen SAPMF05A 710
* ----------------------------------------------------------------
  PERFORM GET_LINE_SCREEN710 USING 'ZUONR' LF_LINE.

* get next free bordronumber and document type of postings
* --------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '03'.
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-35'.
  PERFORM GET_DOCUMENT_TYPE USING BLART2 'F-51'.

* get posting key for vendor debit posting
* ---------------------------------------------
  SELECT SINGLE BSKSO FROM T041A INTO BSCHL1
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL1 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

*   Note 1787965
* check online payment update is active in FM
* -------------------------------------------
  CLEAR C_FLG_PUR_ACTIVE.
  CALL FUNCTION 'FMFK_BUKRS_CHECK_FMAKTIV'
    EXPORTING
      IP_BUKRS           = P_BUKRS
      IP_APPLC           = 'A'
    IMPORTING
      OP_IS_ACTIVE       = L_FLG_BKAKT
      OP_FIKRS           = LF_FIKRS
    EXCEPTIONS
      NO_FIKRS_FOR_BUKRS = 1.
  IF SY-SUBRC = 0.
    IF L_FLG_BKAKT = 'J'.
      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          FUNCNAME           = 'FMCA_GET_INIT_INFO'
        EXCEPTIONS
          FUNCTION_NOT_EXIST = 1
          OTHERS             = 2.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'FMCA_GET_INIT_INFO' ##EXISTS
          EXPORTING
            I_FIKRS          = LF_FIKRS
          CHANGING
            C_F_CONTROL_DATA = L_F_CONTROL_DATA.
        IF SY-SUBRC = 0.
          ASSIGN (L_FM_PAYM_PUR) TO <FS_FM_PAYM_PUR>.
          IF SY-SUBRC = 0 AND <FS_FM_PAYM_PUR> = 'X'.
            C_FLG_PUR_ACTIVE = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*   Note 1787965
* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.
* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        CLEAR: LF_BUPLA, LF_BRNCH.
        SELECT SINGLE BUPLA FROM BSEG INTO LF_BUPLA
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUZEI = LS_WTAB-BUZEI
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'BUPLA' FIELD LF_BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD LF_BUPLA.

        SELECT SINGLE BRNCH FROM BKPF INTO LF_BRNCH
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'JEA' FIELD LF_BRNCH.

      ENDIF.

* check lock of vendor sub account
* --------------------------------
      PERFORM CHECK_LOCKED_ACCOUNT  USING P_BUKRS
                                    GF_BANKM
                                    LF_DUMMY.
      IF NOT LF_DUMMY IS INITIAL.
* create error message
* --------------------
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* Note 971590 START
* test: is Customer account blocked for posting
*--------------------------------
      SELECT SINGLE * FROM LFA1 WHERE LIFNR = GF_LIFNA .
      SELECT SINGLE * FROM LFB1 WHERE LIFNR = GF_LIFNA AND
                                      BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0 OR LFA1-SPERR EQ 'X' OR lfB1-SPERR EQ 'X'.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
        MESSAGE E351(F5) WITH GF_LIFNA INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* Note 971590 END

*   Note 1787965
* If Online payment update is active in FM
      IF C_FLG_PUR_ACTIVE = 'X'.
        SELECT SINGLE * FROM *BSED INTO LS_BSED
                                  WHERE BUKRS = P_BUKRS
                                    AND BELNR = LS_WTAB-BELNR
                                    AND GJAHR = LS_WTAB-GJAHR
                                    AND BUZEI = LS_WTAB-BUZEI.
        SELECT SINGLE * FROM *BSEG INTO LS_BSEG
                                  WHERE BUKRS = P_BUKRS
                                    AND BELNR = LS_WTAB-BELNR
                                    AND GJAHR = LS_WTAB-GJAHR
                                    AND BUZEI = LS_WTAB-BUZEI.
* Check for any unfinished credit postings to the portfolio
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR INTO LF_SEL01.
*==> Start of Note 3111672
*  replace bsid with bsid_view for Remove SQL View Usages
*        SELECT single belnr gjahr buzei FROM  BSID
*               INTO (lf_belnra,lf_gjahra,lf_buzeia)
*               WHERE  BUKRS  = p_bukrs
*               AND    KUNNR  = ls_bseg-kunnr
*               AND    UMSKS  = ls_bseg-umsks
*               AND    UMSKZ  = ls_bseg-umskz
*               AND    ZUONR  = lf_sel01
*               AND    SHKZG  = 'H'.

        SELECT SINGLE BELNR, GJAHR, BUZEI FROM  BSID_VIEW
               INTO (@LF_BELNRA,@LF_GJAHRA,@LF_BUZEIA)
               WHERE  BUKRS  = @P_BUKRS
               AND    KUNNR  = @LS_BSEG-KUNNR
               AND    UMSKS  = @LS_BSEG-UMSKS
               AND    UMSKZ  = @LS_BSEG-UMSKZ
               AND    ZUONR  = @LF_SEL01
               AND    SHKZG  = 'H'.
*==> End of Note 3111672
        IF SY-SUBRC = 0.
          SELECT SINGLE PORTF  FROM  BSED
                 INTO   LF_PRTF
                 WHERE  BUKRS  = P_BUKRS
                 AND    BELNR  = LF_BELNRA
                 AND    GJAHR  = LF_GJAHRA
                 AND    BUZEI  = LF_BUZEIA
                 AND    PORTF  = LS_BSED-PORTF.
          IF SY-SUBRC = 0.
* create error message
*---------------------------------------------------------------
            MESSAGE E213 WITH LF_BELNRA LF_GJAHRA INTO LF_DUMMY.
            PERFORM ADD_LINE USING SY GT_SYMESS.
            ADD 1 TO COUNT1.
            LS_WTAB-BOX = '2'.
            MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
            PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                  LS_WTAB-BELNR
                            CHANGING LF_DUMMY.
            IF NOT ( LF_DUMMY IS INITIAL ).
              PERFORM ADD_LINE USING SY GT_SYMESS.
            ENDIF.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
*   Note 1787965

* call transaction: check/BoE usage posting
* -----------------------------------------
      REFRESH BDCTAB.
*      WRITE ls_wtab-wrbtr TO charwrbtr CURRENCY ls_wtab-waers. "AFLE Change
      PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-WRBTR LS_WTAB-WAERS
                                       CHANGING CHARWRBTR.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '101'.
      PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      IF NOT LS_WTAB-GSBER IS INITIAL.
        PERFORM BATCHFIELD USING 'BSEG-GSBER' LS_WTAB-GSBER.
      ENDIF.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                      INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                      INTO ZZUONR.
      LF_SELZU = ZZUONR.
      PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
      IF LS_WTAB-BANK IS INITIAL.
        CHARSGTXT = LS_WTAB-BOENO.
      ELSE.
        CONCATENATE LS_WTAB-BOENO '/' LS_WTAB-BANK INTO CHARSGTXT.
      ENDIF.
      PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' LS_WTAB-WAERS.
*Begin of change :Note 1293111
      IF LS_WTAB-WAERS NE LS_WTAB-HWAER AND
         NOT ( GF_KURSF IS INITIAL ).
        WRITE GF_KURSF TO CHARKURSF.
        PERFORM BATCHFIELD USING 'BKPF-KURSF' CHARKURSF.
      ENDIF.
* End of change : Note 1293111
      PERFORM BATCHFIELD USING 'RF05A-KONTO' GF_BANKM.
      WRITE LS_WTAB-ZFBDT TO CHARZFBDT.
      PERFORM BATCHFIELD USING 'BSEG-VALUT' CHARZFBDT.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'SW'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '705'.
      PERFORM BATCHFIELD USING '*BKPF-BELNR(1)' LS_WTAB-BELNR.
      PERFORM BATCHFIELD USING '*BSEG-BUZEI(1)' LS_WTAB-BUZEI.
      PERFORM BATCHFIELD USING '*BKPF-GJAHR(1)' LS_WTAB-GJAHR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-35' USING BDCTAB MODE MODE UPDATE UPDATE
                                    MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ELSE.
* determine document and check-to-bank account of BoE usage posting
* -----------------------------------------------------------------
        SELECT SINGLE * FROM *BSEG WHERE BELNR = LS_WTAB-BELNR
                                     AND BUZEI = LS_WTAB-BUZEI
                                     AND BUKRS = P_BUKRS
                                     AND GJAHR = LS_WTAB-GJAHR.
        LF_BELNR = *BSEG-DISBN.
        LF_GJAHR = *BSEG-DISBJ.

        SELECT SINGLE * FROM *BSEG WHERE BELNR = LF_BELNR
                                     AND BUKRS = P_BUKRS
                                     AND GJAHR = LF_GJAHR
                                     AND ZUONR = LF_SELZU
                                     AND SHKZG = 'S'.
        LF_KONTO = *BSEG-HKONT.

        IF SY-SUBRC NE 0 OR LF_BELNR IS INITIAL OR
           LF_KONTO IS INITIAL.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
* --------------------
          MESSAGE E181 INTO LF_DUMMY.
          PERFORM ADD_LINE USING SY GT_SYMESS.

          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
          CONTINUE.
        ENDIF.
* call transaction: posting:  acc. pay. to check/BoE-to-vendor account
* --------------------------------------------------------------------
        REFRESH BDCTAB.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '122'.
        PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
        PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
        PERFORM BATCHFIELD USING 'BKPF-BLART' BLART2.
        PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
        PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
        PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
        PERFORM BATCHFIELD USING 'BKPF-WAERS' P_WAERS.
*Changes begin :  Note 1293111
        IF P_WAERS NE GS_T001-WAERS AND
           NOT ( GF_KURSF IS INITIAL ).
          WRITE GF_KURSF TO CHARKURSF.
          PERFORM BATCHFIELD USING 'BKPF-KURSF' CHARKURSF.
        ENDIF.
* Changes end : Note 1293111
        PERFORM BATCHFIELD USING 'RF05A-NEWBS' BSCHL1.
        PERFORM BATCHFIELD USING 'RF05A-NEWKO' GF_LIFNA.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ENTR'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '302'.
        PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
        WRITE LS_WTAB-ZFBDT TO CHARZFBDT.
        PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHARZFBDT.
        IF NOT LS_WTAB-GSBER IS INITIAL.
          PERFORM BATCHFIELD USING 'BSEG-GSBER' LS_WTAB-GSBER.
        ENDIF.
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                INTO ZZUONR.
        PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
        PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'SL'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '710'.
        PERFORM BATCHFIELD USING 'RF05A-AGBUK' P_BUKRS.
        PERFORM BATCHFIELD USING 'RF05A-AGKON' LF_KONTO.
        PERFORM BATCHFIELD USING 'RF05A-AGKOA' 'S'.
        CONCATENATE 'RF05A-XPOS1(' LF_LINE ')' INTO CHARXPOS.
        PERFORM BATCHFIELD USING CHARXPOS 'X'.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PA'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '731'.
        PERFORM BATCHFIELD USING 'RF05A-SEL01(01)' LF_SELZU.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
        CALL TRANSACTION 'F-51' USING BDCTAB MODE MODE UPDATE UPDATE
                                      MESSAGES INTO GT_SYMESS.
        IF SY-SUBRC NE 0.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ELSE.
* call transaction: change receipt document with new vendor name
* --------------------------------------------------------------
          REFRESH BDCTAB.
          PERFORM BATCHDYNPRO USING 'SAPMF05L' '100'.
          PERFORM BATCHFIELD USING 'RF05L-BELNR' LS_WTAB-BELNR.
          PERFORM BATCHFIELD USING 'RF05L-BUKRS' P_BUKRS.
          PERFORM BATCHFIELD USING 'RF05L-GJAHR' LS_WTAB-GJAHR.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
          PERFORM BATCHDYNPRO USING 'SAPMF05L' '700'.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=ABZL'.
          PERFORM BATCHDYNPRO USING 'SAPMF05L' '1130'.
          PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05L-BUZEI'.
          PERFORM BATCHFIELD USING 'RF05L-BUZEI' LS_WTAB-BUZEI.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=ENTR'.
          PERFORM BATCHDYNPRO USING 'SAPMF05L' '700'.
          PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05L-ANZDT(01)'.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=PK'.
*==> Start of Note 2784912
*          PERFORM batchdynpro USING 'SAPMF05L' '320'.

          IF T001-BUVAR = '2'.
            PERFORM BATCHDYNPRO USING 'SAPMF05L' '2320'.
          ELSE.
            PERFORM BATCHDYNPRO USING 'SAPMF05L' '320'.
          ENDIF.
*==> End of Note 2784912
          PERFORM BATCHFIELD USING 'BDC_CURSOR' 'BSED-VENDR'.
          PERFORM BATCHFIELD USING 'BSED-VENDR' GF_LIFNA.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=AE'.
          CALL TRANSACTION 'FB02' USING BDCTAB MODE MODE UPDATE
                                  UPDATE MESSAGES INTO GT_SYMESS.
          IF SY-SUBRC NE 0.
            ADD 1 TO COUNT1.
            LS_WTAB-BOX = '2'.
            MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
            PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                  LS_WTAB-BELNR
                            CHANGING LF_DUMMY.
            IF NOT ( LF_DUMMY IS INITIAL ).
              PERFORM ADD_LINE USING SY GT_SYMESS.
            ENDIF.
          ELSE.
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
            ADD 1 TO COUNT2.
            LS_WTAB-BOX    = '1'.
            LS_WTAB-CHEC   = '1'.
            LS_WTAB-XBLNR  = BORDRO.
            LS_WTAB-PORTF1 = P_PORT1.
            LS_WTAB-LIFNA  = GF_LIFNA.
*              ls_wtab-bankn  = lf_konto.
            MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                                     PORTF1 LIFNA.
            PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                  LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
            IF NOT ( LF_DUMMY IS INITIAL ).
              PERFORM ADD_LINE USING SY GT_SYMESS.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ----------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

* at least one BoE successfully posted?
* clear status of SAPScript print for next postings
* -------------------------------------
  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1'.
      TEST = '1'.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660


* SAPScript print
* ---------------
  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-004.
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    IF PSCRIPT = 'X'  .
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].

***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    ELSE.
      S_SINGLE = 'X'.
      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
**move ls_data-user into output_header_table-tran.
      OUTPUT_HEADER_TABLE-STATUS = '@0V@'.
      OUTPUT_HEADER_TABLE-EXPAND = S_SINGLE.
      BOE_GLOBAL-USER = SY-UNAME.

      PERFORM OPEN_FORM_PDF.

      PERFORM ITEM_DATA_PDF.

    ENDIF.

*ENDFORM.                    " form reprint

***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660

  ENDIF.

ENDFORM.                                                    " BATCH2

*&---------------------------------------------------------------------*
*&      Form  BATCH3
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'Payment at the bank'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH3 USING LT_WTAB LIKE GT_WTAB[].

  DATA: LS_WTAB       TYPE BOE_TR_LIST,
        LF_BELNR      LIKE BSEG-BELNR,
        LF_KONTO      LIKE BSEG-HKONT,
        LF_DAT        LIKE SY-DATUM,
        LF_GJAHR      LIKE BSEG-GJAHR,
        LF_LINE(2)    TYPE C,
        LF_DUMMY      TYPE C,
        LF_BRNCH      LIKE BKPF-BRNCH,
        LF_WAERS      LIKE BKPF-WAERS,
        LF_VALUT      TYPE BSEG-VALUT, "Note 1000376
        LF_HBKID      TYPE T012K-HBKID, "Note 1000376
        LF_HKTID      TYPE T012K-HKTID,  "Note 1000376
        LF_BSEG_HBKID TYPE T012K-HBKID, "Add in Note 3085192
        LF_BSEG_HKTID TYPE T012K-HKTID, "Add in Note 3085192
        LV_BANK_FLAG  TYPE ABAP_BOOL,    "Add in Note 3085192
        LF_SELZU      LIKE BSEG-ZUONR.

* clear required global fields
* ----------------------------
  CLEAR: CHARWRBTR, CHARSGTXT, CHARZFBDT, CHARBUDAT, CHARBLDAT,
         CHARVALUT, CHARXPOS, CHAR_DAT, CHARREF, CHARKURSF,
         ABPOS1, ABPOS2, ABCOUNT, BORDRO, COUNT1, COUNT2,
         BLART1, BLART2, BSCHL1,
         ZZUONR.

* prepare posting and document date for batch input
* ------------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* prepare value date for batch input
* ----------------------------------
  IF NOT GF_VALUT IS INITIAL.
    IF GF_DATFM = '2' OR GF_DATFM = '3'.
      CONCATENATE GF_VALUT+4(2) GF_VALUT+6(2) GF_VALUT+0(4)
                                              INTO CHARVALUT.
    ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
      CONCATENATE GF_VALUT+0(4) GF_VALUT+4(2) GF_VALUT+6(2)
                                              INTO CHARVALUT.
    ELSE.
      CONCATENATE GF_VALUT+6(2) GF_VALUT+4(2) GF_VALUT+0(4)
                                              INTO CHARVALUT.
    ENDIF.
  ENDIF.

* get line of radiobutton 'Allocation' on screen SAPMF05A 710
* ----------------------------------------------------------------
  PERFORM GET_LINE_SCREEN710 USING 'ZUONR' LF_LINE.

* get next free bordronumber and document type of postings
* --------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '04'.
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-04'.
  PERFORM GET_DOCUMENT_TYPE USING BLART2 'F-20'.

* get posting key for G/L debit posting
* ---------------------------------------------
  SELECT SINGLE BSSSO FROM T041A INTO BSCHL1
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL1 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.
* determine document and Boe-to-bank account of BoE usage posting
* -----------------------------------------------------------------
      SELECT SINGLE * FROM *BSEG WHERE BELNR = LS_WTAB-BELNR
                                     AND BUKRS = P_BUKRS
                                     AND BUZEI = LS_WTAB-BUZEI
                                     AND GJAHR = LS_WTAB-GJAHR.
      LF_BELNR = *BSEG-DISBN.
      LF_GJAHR = *BSEG-DISBJ.
* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        SET PARAMETER ID 'BUPLA' FIELD *BSEG-BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD *BSEG-BUPLA.
        CLEAR: LF_BRNCH.
        SELECT SINGLE BRNCH FROM BKPF INTO LF_BRNCH
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'JEA' FIELD LF_BRNCH.
      ENDIF.

      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO LF_SELZU.
      SELECT SINGLE * FROM *BSEG WHERE BELNR = LF_BELNR
                                   AND BUKRS = P_BUKRS
                                   AND GJAHR = LF_GJAHR
                                   AND ZUONR = LF_SELZU
                                   AND SHKZG = 'S'.
      LF_KONTO = *BSEG-HKONT.
      LF_SELZU = *BSEG-ZUONR.
      LF_BSEG_HBKID = *BSEG-HBKID.  " add in note 3085192
      LF_BSEG_HKTID = *BSEG-HKTID.  " add in note 3085192

      IF SY-SUBRC NE 0 OR LF_BELNR IS INITIAL OR
         LF_KONTO IS INITIAL OR LF_SELZU IS INITIAL.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.

* create error message
* --------------------
        MESSAGE E181 INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* get bank account, if necessary
* ------------------------------
      IF P_BANKN EQ SPACE.
        CLEAR: GF_COUNT, GS_T012K.
        "Begin of T012K Replacement, CM80672
*        SELECT * FROM t012k INTO gs_t012k
*                            WHERE bukrs = p_bukrs.
*          IF gs_t012k-wikon EQ lf_konto.
*            gf_count = gf_count + 1.
*            gf_banko = gs_t012k-hkont.
**Note 1000376 START
*            lf_hbkid = gs_t012k-hbkid.
*            lf_hktid = gs_t012k-hktid.
**Note 1000376 END
*          ENDIF.
*        ENDSELECT.
        TRY.
            DATA(LT_HBA_INSTANCE) = CL_FCLM_OBJECT_FACTORY=>GET_HBA_INSTANCES(
                                    IT_BUKRS = VALUE #( ( P_BUKRS ) ) ).
          CATCH CX_FCLM_BAM_HOUSE_BANK_ACCOUNT ##NO_HANDLER .
        ENDTRY.
        LOOP AT LT_HBA_INSTANCE ASSIGNING FIELD-SYMBOL(<FS_HBA_INSTANCE>).
          <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_VALIDITY_PERIOD(
            IMPORTING
              EV_VALID_FROM = DATA(LV_HBA_VALID_FROM)
              EV_VALID_TO   = DATA(LV_HBA_VALID_TO)
          ).
          IF LV_HBA_VALID_FROM > SY-DATUM OR LV_HBA_VALID_TO < SY-DATUM.
            CONTINUE.
          ENDIF.
          DATA(LT_ACLINK2) = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_BANK_ACCOUNT~GET_ACLINK2_FIELDS(
                               IT_REQ_FIELDS = VALUE #( ( 'WIKON' ) )
                             ).
          DATA(LS_ACLINK2) = LT_ACLINK2[ ACC_ID   = <FS_HBA_INSTANCE>-ACC_ID
                                         GUID     = <FS_HBA_INSTANCE>-GUID
                                         REVISION = IF_FCLM_BANK_ACCOUNT=>GC_ACTIVE_REVISION ].
          IF LF_BSEG_HBKID IS INITIAL. " ADD IN NOTE 3085192
            IF LS_ACLINK2-WIKON EQ LF_KONTO.
              GF_COUNT = GF_COUNT + 1.
              GF_BANKO = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_GL_ACCOUNT( ).
              LF_HBKID = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~M_HOUSEBANK_ID.
              LF_HKTID = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~M_ACCOUNT_ID.
            ENDIF.
*==> Start of Note 3085192
          ELSE.
            IF LS_ACLINK2-WIKON EQ LF_KONTO
           AND LF_BSEG_HBKID = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~M_HOUSEBANK_ID
           AND LF_BSEG_HKTID = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~M_ACCOUNT_ID.
              GF_COUNT = GF_COUNT + 1.
              GF_BANKO = <FS_HBA_INSTANCE>-ACCOUNT->IF_FCLM_HOUSE_BANK_ACCOUNT~GET_GL_ACCOUNT( ).
            ENDIF.
          ENDIF.
*==> End of Note 3085192
        ENDLOOP.
        "End of T012K Replacement, CM80672
        IF GF_COUNT NE 1.
* create error message
* --------------------
          MESSAGE E193 INTO LF_DUMMY.
          PERFORM ADD_LINE USING SY GT_SYMESS.

          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.

*Note 1000376 START
* get value date
      IF GF_VALUT IS INITIAL.
        LF_VALUT = LS_WTAB-ZFBDT.
        PERFORM GET_VALUE_DATE USING P_BUKRS
                                     LF_HBKID               "1000376
                                     LF_HKTID               "1000376
                                     LS_WTAB-BANK
                            CHANGING LF_VALUT.
        IF GF_DATFM = '2' OR GF_DATFM = '3'.
          CONCATENATE LF_VALUT+4(2) LF_VALUT+6(2) LF_VALUT+0(4)
                                                  INTO CHARZFBDT.
        ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
          CONCATENATE LF_VALUT+0(4) LF_VALUT+4(2) LF_VALUT+6(2)
                                                  INTO CHARZFBDT.
        ELSE.
          CONCATENATE LF_VALUT+6(2) LF_VALUT+4(2) LF_VALUT+0(4)
                                                  INTO CHARZFBDT.
        ENDIF.
      ENDIF.
*Note 1000376 END

* test: is BoE-to-vendor account only open for internal postings?
* ---------------------------------------------------------------
      SELECT SINGLE * FROM SKB1 WHERE BUKRS = P_BUKRS
                                  AND SAKNR = LF_KONTO.
      IF SY-SUBRC NE 0 OR SKB1-XINTB NE ' '.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.

* create error message
* --------------------
        MESSAGE E182 WITH LF_KONTO INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* Note 971590 START
* test: is Customer account blocked for posting
*--------------------------------
      SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-KUNNR.
      SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-KUNNR AND
                                      BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
        MESSAGE E351(F5) WITH LS_WTAB-KUNNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* Note 971590 END

* check lock of check G/L account
* -------------------------------
      PERFORM CHECK_LOCKED_ACCOUNT  USING P_BUKRS
                                    LF_KONTO "Note 1178040
                                    LF_DUMMY.
      IF NOT LF_DUMMY IS INITIAL.
* create error message
* --------------------
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ELSE.
*Note  1178040 START
*        PERFORM enqueue_skb1 USING ls_wtab-bukrs
*                                   ls_wtab-saknr
*                             CHANGING lf_dummy.
*        IF NOT ( lf_dummy IS INITIAL ).
*          PERFORM add_line USING sy gt_symess.
*          CONTINUE.
*        ENDIF.
*Note  1178040 END
      ENDIF.

*Note  1178040 START
* check lock of liability account
* -------------------------------
*      SELECT SINGLE * FROM bsix WHERE bukrs = ls_wtab-bukrs
*                             AND hkont = ls_wtab-saknr
*                             AND zuonr = ls_wtab-zuonr
*                             AND belnr = ls_wtab-belnr
*                             AND gjahr = ls_wtab-gjahr
*                             AND buzei = ls_wtab-buzei.
*      IF sy-subrc EQ 0.
*        PERFORM check_locked_account  USING p_bukrs
*                                      bsix-hkontdis
*                                      lf_dummy.
*        IF NOT lf_dummy IS INITIAL.
** create error message
** --------------------
*          PERFORM add_line USING sy gt_symess.
*
*          ADD 1 TO count1.
*          ls_wtab-box = '2'.
*          MODIFY lt_wtab FROM ls_wtab TRANSPORTING box.
*          PERFORM enqueue USING ls_wtab-bukrs ls_wtab-gjahr
*                                ls_wtab-belnr
*                          CHANGING lf_dummy.
*          IF NOT ( lf_dummy IS INITIAL ).
*            PERFORM add_line USING sy gt_symess.
*          ENDIF.
*          CONTINUE.
*        ELSE.
*          PERFORM enqueue_skb1 USING ls_wtab-bukrs
*                                     bsix-hkontdis
*                               CHANGING lf_dummy.
*          IF NOT ( lf_dummy IS INITIAL ).
*            PERFORM add_line USING sy gt_symess.
*            PERFORM dequeue_skb1 USING ls_wtab-bukrs
*                                       ls_wtab-saknr.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*      ENDIF.
* set date for selection of BoE: sum of due date and protest period
*
      IF LS_WTAB-ZFBDT LE NULL.
        LF_DAT = P_BUDAT.
      ELSE.
        LF_DAT = LS_WTAB-ZFBDT + LS_WTAB-WELGF.
      ENDIF.
* read open line items in table bsix due till lf_dat
*
      PERFORM READ_BSIX USING GT_BELDATA[] LS_WTAB P_BUKRS LF_DAT
                              LF_WAERS 'I'.
* get line number of current BoE in table with open line items
*
      PERFORM GET_LINE USING GT_BELDATA[] LS_WTAB ABPOS1 ABPOS2.
* call transaction: reverse contingent liability of current BoE
*
      REFRESH BDCTAB.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '102'.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART2.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' LF_WAERS.
      IF GF_DATFM = '2' OR GF_DATFM = '3'.
        CONCATENATE LF_DAT+4(2) LF_DAT+6(2) LF_DAT+0(4)
                                        INTO CHAR_DAT.
      ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
        CONCATENATE LF_DAT+0(4) LF_DAT+4(2) LF_DAT+6(2)
                                        INTO CHAR_DAT.
      ELSE.
        CONCATENATE LF_DAT+6(2) LF_DAT+4(2) LF_DAT+0(4)
                                            INTO CHAR_DAT.
      ENDIF.
      PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHAR_DAT.
      PERFORM BATCHFIELD USING 'RF05A-AGKON' LS_WTAB-SAKNR.
      PERFORM BATCHFIELD USING 'RF05A-WVERW' 'I'.
      PERFORM BATCHFIELD USING 'RF05A-SEL01' LS_WTAB-BELNR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PB'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSCOM(01)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '602'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-XPOS1(04)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO+'.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(01)' ' '.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(04)' 'X'.
      ABCOUNT = 1.
      WHILE ABCOUNT LE ABPOS1.
        IF NOT ( ABCOUNT = ABPOS2 ).
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'RF05A-ABPOS' ABCOUNT.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSBET(01)'.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=PI4'.
        ENDIF.
        ADD 1 TO ABCOUNT.
      ENDWHILE.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-20' USING BDCTAB MODE MODE UPDATE
                              UPDATE MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ELSE.
*==> Start of Note 3085192
        CLEAR LV_BANK_FLAG.
        PERFORM BANK_ACC_DETERMINTE USING GF_BANKO
                                   CHANGING LV_BANK_FLAG.
*==> End of Note 3085192
*Note  1178040 END
* call transaction: posting: bank account to check/BoE-to-bank account
* --------------------------------------------------------------------
        REFRESH BDCTAB.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '122'.
        PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
        PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
        PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
        PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
        PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
        PERFORM BATCHFIELD USING 'BKPF-WAERS' P_WAERS.
        IF P_WAERS NE GS_T001-WAERS AND
           NOT ( GF_KURSF IS INITIAL ).
          WRITE GF_KURSF TO CHARKURSF.
          PERFORM BATCHFIELD USING 'BKPF-KURSF' CHARKURSF.
        ENDIF.
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                INTO CHARREF.
        PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
        PERFORM BATCHFIELD USING 'RF05A-NEWBS' BSCHL1.
        PERFORM BATCHFIELD USING 'RF05A-NEWKO' GF_BANKO.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ENTR'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '300'.
*      WRITE ls_wtab-wrbtr TO charwrbtr CURRENCY ls_wtab-waers.  "AFLE Change
        PERFORM WRITE_AMT_TO_CHARAMT     USING LS_WTAB-WRBTR LS_WTAB-WAERS
                                         CHANGING CHARWRBTR.
        PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                INTO ZZUONR.
        PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
        IF LS_WTAB-BANK IS INITIAL.
          CHARSGTXT = LS_WTAB-BOENO.
        ELSE.
          CONCATENATE LS_WTAB-BOENO '/' LS_WTAB-BANK INTO CHARSGTXT.
        ENDIF.
*==> Start of Note 3085192
        IF LV_BANK_FLAG = ABAP_TRUE.
          PERFORM BATCHFIELD USING 'BSEG-HBKID' LF_BSEG_HBKID.
          PERFORM BATCHFIELD USING 'BSEG-HKTID' LF_BSEG_HKTID.
        ENDIF.
*==> End of Note 3085192
        PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
*      WRITE ls_wtab-zfbdt TO charzfbdt. "Note 1000376
        IF CHARVALUT IS INITIAL.
          PERFORM BATCHFIELD USING 'BSEG-VALUT' CHARZFBDT.
        ELSE.
          PERFORM BATCHFIELD USING 'BSEG-VALUT' CHARVALUT.
        ENDIF.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'SL'.
        PERFORM BATCHDYNPRO USING 'SAPLKACB' '0002'.
        IF NOT LS_WTAB-GSBER IS INITIAL.
          PERFORM BATCHFIELD USING 'COBL-GSBER' LS_WTAB-GSBER.
        ENDIF.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ENTE'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '710'.
        PERFORM BATCHFIELD USING 'RF05A-AGBUK' P_BUKRS.
        PERFORM BATCHFIELD USING 'RF05A-AGKON' LF_KONTO.
        PERFORM BATCHFIELD USING 'RF05A-AGKOA' 'S'.
        CONCATENATE 'RF05A-XPOS1(' LF_LINE ')' INTO CHARXPOS.
        PERFORM BATCHFIELD USING CHARXPOS 'X'.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PA'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '731'.
        PERFORM BATCHFIELD USING 'RF05A-SEL01(01)' LF_SELZU.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
        CALL TRANSACTION 'F-51' USING BDCTAB MODE MODE UPDATE UPDATE
                                      MESSAGES INTO GT_SYMESS.
        IF SY-SUBRC NE 0.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ELSE.
*Note  1178040 START
** unlock check GL account and liability account
** ---------------------------------------------
*        PERFORM dequeue_skb1 USING ls_wtab-bukrs
*                                   ls_wtab-saknr.
*        PERFORM dequeue_skb1 USING ls_wtab-bukrs
*                                   bsix-hkontdis.
** set date for selection of BoE: sum of due date and protest period
** -----------------------------------------------------------------
*        IF ls_wtab-zfbdt LE null.
*          lf_dat = p_budat.
*        ELSE.
*          lf_dat = ls_wtab-zfbdt + ls_wtab-welgf.
*        ENDIF.
** read open line items in table bsix due till lf_dat
** -------------------------------------------------
*        PERFORM read_bsix USING gt_beldata[] ls_wtab p_bukrs lf_dat
*                                lf_waers 'I'.
** get line number of current BoE in table with open line items
** ------------------------------------------------------------
*        PERFORM get_line USING gt_beldata[] ls_wtab abpos1 abpos2.
** call transaction: reverse contingent liability of current BoE
** -------------------------------------------------------------
*        REFRESH bdctab.
*        PERFORM batchdynpro USING 'SAPMF05A' '102'.
*        PERFORM batchfield USING 'BKPF-BLDAT' charbldat.
*        PERFORM batchfield USING 'BKPF-BUKRS' p_bukrs.
*        PERFORM batchfield USING 'BKPF-XBLNR' bordro.
*        PERFORM batchfield USING 'BKPF-BKTXT' charref.
*        PERFORM batchfield USING 'BKPF-BLART' blart2.
*        PERFORM batchfield USING 'BKPF-BUDAT' charbudat.
*        PERFORM batchfield USING 'BKPF-WAERS' lf_waers.
*        IF gf_datfm = '2' OR gf_datfm = '3'.
*          CONCATENATE lf_dat+4(2) lf_dat+6(2) lf_dat+0(4)
*                                          INTO char_dat.
*        ELSEIF gf_datfm = '4' OR gf_datfm = '5' OR gf_datfm = '6'.
*          CONCATENATE lf_dat+0(4) lf_dat+4(2) lf_dat+6(2)
*                                          INTO char_dat.
*        ELSE.
*          CONCATENATE lf_dat+6(2) lf_dat+4(2) lf_dat+0(4)
*                                              INTO char_dat.
*        ENDIF.
*        PERFORM batchfield USING 'BSEG-ZFBDT' char_dat.
*        PERFORM batchfield USING 'RF05A-AGKON' ls_wtab-saknr.
*        PERFORM batchfield USING 'RF05A-WVERW' 'I'.
*        PERFORM batchfield USING 'RF05A-SEL01' ls_wtab-belnr.
*        PERFORM batchfield USING 'BDC_OKCODE' 'PB'.
*        PERFORM batchdynpro USING 'SAPMF05A' '716'.
*        PERFORM batchfield USING 'BDC_CURSOR' 'RF05A-PSCOM(01)'.
*        PERFORM batchfield USING 'BDC_OKCODE' '=SO'.
*        PERFORM batchfield USING 'RF05A-ABPOS' '1'.
*        PERFORM batchdynpro USING 'SAPMF05A' '602'.
*        PERFORM batchfield USING 'BDC_CURSOR' 'RF05A-XPOS1(04)'.
*        PERFORM batchfield USING 'BDC_OKCODE' '=SO+'.
*        PERFORM batchfield USING 'RF05A-XPOS1(01)' ' '.
*        PERFORM batchfield USING 'RF05A-XPOS1(04)' 'X'.
*        abcount = 1.
*        WHILE abcount LE abpos1.
*          IF NOT ( abcount = abpos2 ).
*            PERFORM batchdynpro USING 'SAPMF05A' '716'.
*            PERFORM batchfield USING 'RF05A-ABPOS' abcount.
*            PERFORM batchfield USING 'BDC_OKCODE' '/00'.
*            PERFORM batchdynpro USING 'SAPMF05A' '716'.
*            PERFORM batchfield USING 'BDC_CURSOR' 'RF05A-PSBET(01)'.
*            PERFORM batchfield USING 'BDC_OKCODE' '=PI4'.
*          ENDIF.
*          ADD 1 TO abcount.
*        ENDWHILE.
*        PERFORM batchdynpro USING 'SAPMF05A' '716'.
*        PERFORM batchfield USING 'RF05A-ABPOS' '1'.
*        PERFORM batchfield USING 'BDC_OKCODE' '/00'.
*        PERFORM batchdynpro USING 'SAPMF05A' '716'.
*        PERFORM batchfield USING 'BDC_OKCODE' 'BU'.
*        CALL TRANSACTION 'F-20' USING bdctab MODE mode UPDATE
*                                update MESSAGES INTO gt_symess.
*        IF sy-subrc NE 0.
*          ADD 1 TO count1.
*          ls_wtab-box = '2'.
*          MODIFY lt_wtab FROM ls_wtab TRANSPORTING box.
*          PERFORM enqueue USING ls_wtab-bukrs ls_wtab-gjahr
*                                ls_wtab-belnr
*                          CHANGING lf_dummy.
*          IF NOT ( lf_dummy IS INITIAL ).
*            PERFORM add_line USING sy gt_symess.
*          ENDIF.
*        ELSE.
*Note  1178040 END
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
          ADD 1 TO COUNT2.
          LS_WTAB-BOX = '1'.
          LS_WTAB-CHEC = '1'.
          LS_WTAB-XBLNR = BORDRO.
          LS_WTAB-BANKN  = LF_KONTO.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                                   BANKN.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ----------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

*==> Start of New Output Management for OP & Cloud 247924(2018)
  DATA: LT_PRINTAB TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_DATA    TYPE BOE_GLOBAL_TURKEY.

  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1'.
      TEST = '1'.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.

  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-005.  "Cleared at Bank
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.

    IF PSCRIPT = 'X'  .
      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].
    ELSE.


      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      BOE_GLOBAL-USER = SY-UNAME.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
      PERFORM ITEM_DATA_PDF.
    ENDIF.


  ENDIF.

*==> End of 247924(2018)

ENDFORM.                                                    " BATCH3

*&---------------------------------------------------------------------*
*&      Form  BATCH4
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'Payment at the vendor'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH4 USING LT_WTAB LIKE GT_WTAB[].

  DATA: LS_WTAB  TYPE BOE_TR_LIST,
        LF_DAT   LIKE SY-DATUM,
        LF_BUPLA LIKE BSEG-BUPLA,
        LF_BRNCH LIKE BKPF-BRNCH,
        LF_WAERS LIKE BKPF-WAERS,
        LF_DUMMY TYPE C.

* clear required global fields
* ----------------------------
  CLEAR: CHARWRBTR, CHARBUDAT, CHARBLDAT, CHAR_DAT, CHARREF,
         ABPOS1, ABPOS2, ABCOUNT, BORDRO, COUNT1, COUNT2, BLART1.

* prepare posting,document and due date for batch input
* -----------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* get next free bordronumber and document type of posting
* -------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '05'.
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-20'.

* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.

* set date for selection of BoE: sum of due date and protest period
* -----------------------------------------------------------------
      IF LS_WTAB-ZFBDT LE NULL.
        LF_DAT = P_BUDAT.
      ELSE.
        LF_DAT = LS_WTAB-ZFBDT + LS_WTAB-WELGF.
      ENDIF.
* read open line items in table bsix due till lf_dat
* -------------------------------------------------
      PERFORM READ_BSIX USING GT_BELDATA[] LS_WTAB P_BUKRS LF_DAT
                              LF_WAERS 'F'.
* get line number of current BoE in table with open line items
* ------------------------------------------------------------
      PERFORM GET_LINE USING GT_BELDATA[] LS_WTAB ABPOS1 ABPOS2.
* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        CLEAR: LF_BUPLA, LF_BRNCH.
        SELECT SINGLE BUPLA FROM BSEG INTO LF_BUPLA
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUZEI = LS_WTAB-BUZEI
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'BUPLA' FIELD LF_BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD LF_BUPLA.
        SELECT SINGLE BRNCH FROM BKPF INTO LF_BRNCH
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'JEA' FIELD LF_BRNCH.
      ENDIF.
* call transaction: reverse contingent liability of current BoE
* -------------------------------------------------------------
      REFRESH BDCTAB.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '102'.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' LF_WAERS.
      IF GF_DATFM = '2' OR GF_DATFM = '3'.
        CONCATENATE LF_DAT+4(2) LF_DAT+6(2) LF_DAT+0(4)
                                        INTO CHAR_DAT.
      ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
        CONCATENATE LF_DAT+0(4) LF_DAT+4(2) LF_DAT+6(2)
                                        INTO CHAR_DAT.
      ELSE.
        CONCATENATE LF_DAT+6(2) LF_DAT+4(2) LF_DAT+0(4)
                                            INTO CHAR_DAT.
      ENDIF.
      PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHAR_DAT.
      PERFORM BATCHFIELD USING 'RF05A-AGKON' LS_WTAB-SAKNR.
      PERFORM BATCHFIELD USING 'RF05A-WVERW' 'F'.
      PERFORM BATCHFIELD USING 'RF05A-SEL01' LS_WTAB-BELNR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PB'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSCOM(01)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '602'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-XPOS1(04)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO+'.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(01)' ' '.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(04)' 'X'.
      ABCOUNT = 1.
      WHILE ABCOUNT LE ABPOS1.
        IF NOT ( ABCOUNT = ABPOS2 ).
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'RF05A-ABPOS' ABCOUNT.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSBET(01)'.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=PI4'.
        ENDIF.
        ADD 1 TO ABCOUNT.
      ENDWHILE.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-20' USING BDCTAB MODE MODE UPDATE UPDATE
                                    MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ELSE.
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
        ADD 1 TO COUNT2.
        LS_WTAB-BOX    = '1'.
        LS_WTAB-CHEC   = '1'.
        LS_WTAB-XBLNR  = BORDRO.
        LS_WTAB-PORTF1 = P_PORT1.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                                 PORTF1.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ----------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

*==> Start of New Output Management for OP & Cloud 247924(2018)
  DATA: LT_PRINTAB TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_DATA    TYPE BOE_GLOBAL_TURKEY.

  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1'.
      TEST = '1'.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.

  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-006.  "Cleared at Bank
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.

    IF PSCRIPT = 'X'  .
      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].
    ELSE.


      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      BOE_GLOBAL-USER = SY-UNAME.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
      PERFORM ITEM_DATA_PDF.
    ENDIF.


  ENDIF.

*==> End of 247924(2018)
ENDFORM.                                                    " BATCH4

*&---------------------------------------------------------------------*
*&      Form  BATCH5
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'Bounced checks/BoE in the bank'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH5 USING LT_WTAB LIKE GT_WTAB[].

  DATA: LT_PRINTAB   TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_WTAB      TYPE BOE_TR_LIST,
        LS_DATA      TYPE BOE_GLOBAL_TURKEY,
        LS_BKPF      LIKE BKPF, LS_BSEG LIKE BSEG, LS_BSED LIKE BSED,
        LS_T031      LIKE T031,                             "#EC NEEDED
        LF_DAT       LIKE SY-DATUM,
        LF_KONTO     LIKE BSEG-HKONT,
        LF_BELNR     LIKE BSEG-BELNR,
        LF_KONTO2    LIKE BSEG-HKONT,
        LF_FLAGCB    LIKE T045P-FLAGCB,
        LF_PORTF     LIKE T045P-PORTFO,
        LF_GJAHR     LIKE BSEG-GJAHR,
        LF_LINE(2)   TYPE C,
        LF_AKONT     LIKE KNB1-AKONT,
        LF_FLAG(1)   TYPE C,
        LF_MODE(1)   TYPE C,
        LF_UMSKZ     LIKE BSEG-UMSKZ,
        LF_RESULT(1) TYPE C,
        LF_DUMMY     TYPE C,
        LF_WAERS     LIKE BKPF-WAERS,
        LF_KKBER     LIKE T001-KKBER,                       "1292178
        LF_XKKBI     LIKE T001-XKKBI,                       "1292178
        LF_SELZU     LIKE BSEG-ZUONR.

* clear required global fields
* ----------------------------
  CLEAR: CHARWRBTR, CHARSGTXT, CHARZFBDT, CHARBUDAT, CHARBLDAT,
         CHARXPOS, CHAR_DAT, CHARWDATE, CHARREF, ABPOS1,
         ABPOS2, ABCOUNT, BORDRO, COUNT1, COUNT2, BLART1, BLART2,
         BLART3, BSCHL1, BSCHL2, BSCHL3, ZZUONR,
         CHARDMBTR, CHARDMBE2, CHARDMBE3.

* prepare posting, document, system and due date for batch input
* ------------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* get line of radiobutton 'Allocation' on screen SAPMF05A 710
* ----------------------------------------------------------------
  PERFORM GET_LINE_SCREEN710 USING 'ZUONR' LF_LINE.

* get next free bordronumber and document type of postings
* --------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '06'.
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-20'.
  PERFORM GET_DOCUMENT_TYPE USING BLART2 'F-30'.
  PERFORM GET_DOCUMENT_TYPE USING BLART3 'F-22'.

* get posting key for special G/L debit posting
* ---------------------------------------------
  SELECT SINGLE BSDSS FROM T041A INTO BSCHL1
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL1 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* get posting key for G/L credit posting
* ---------------------------------------------
  SELECT SINGLE BSSHA FROM T041A INTO BSCHL2
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL2 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* get posting key for customer debit posting
* ---------------------------------------------
  SELECT SINGLE RPDSO FROM T041A INTO BSCHL3
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL3 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* determine portfolio for bounced BoE
* -----------------------------------
  LF_PORTF = GF_PORT5.
  LF_FLAGCB = GF_FLAGCB5.
  IF LF_FLAGCB IS INITIAL.
    MESSAGE E172 WITH GF_PORT5.
  ENDIF.

* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.
* set correct special G/L indicator
* ---------------------------------
      IF P_UMSKZ IS INITIAL.
        LF_UMSKZ = LS_WTAB-UMSKZ.
      ELSE.
        LF_UMSKZ = P_UMSKZ.
      ENDIF.

* check table TBSLT for necessary entry of special G/L indicator
* --------------------------------------------------------------
      LF_MODE = '-'.
      PERFORM CHECK_TBSLT USING SY-LANGU BSCHL1 LF_UMSKZ LF_MODE.
      IF LF_MODE = 'X'.
* create error message
* --------------------
        MESSAGE E156 WITH LF_UMSKZ INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check tables T074* for necessary entry of special G/L indicator
* ---------------------------------------------------------------
      PERFORM CHECK_T074* USING SY-LANGU LF_UMSKZ LF_RESULT.
      IF LF_RESULT = '1' OR LF_RESULT = '2' OR LF_RESULT = '3'.
* create error message
* --------------------
        IF LF_RESULT EQ '1'.
          MESSAGE E183 WITH LF_UMSKZ 'T074A' INTO LF_DUMMY.
        ELSEIF LF_RESULT EQ '2'.
          MESSAGE E183 WITH LF_UMSKZ 'T074T' INTO LF_DUMMY.
        ELSEIF LF_RESULT EQ '3'.
          MESSAGE E183 WITH LF_UMSKZ 'T074U' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* check table T074 for necessary entry of special G/L indicator
* ---------------------------------------------------------------
      LF_MODE = '+'.
      PERFORM CHECK_T074 USING GS_T001-KTOPL LF_UMSKZ LS_WTAB-KUNNR
                               LS_WTAB-BUKRS LF_MODE.
      IF LF_MODE = '1' OR LF_MODE = '2' OR LF_MODE = '3'.
* create error message
* --------------------
        IF LF_MODE EQ '1'.
          MESSAGE E162 WITH LS_WTAB-KUNNR INTO LF_DUMMY.
        ELSEIF LF_MODE EQ '2'.
          MESSAGE E160 WITH LF_UMSKZ INTO LF_DUMMY.
        ELSEIF LF_MODE EQ '3'.
          MESSAGE E161 WITH LF_UMSKZ INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* Begin of Note 1324719
* test: is Customer branch account blocked for posting
*--------------------------------
      IF LS_WTAB-FILKD NE SPACE.
        SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-FILKD.
        SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-FILKD AND
                                        BUKRS = P_BUKRS.
        IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
          MESSAGE E351(F5) WITH LS_WTAB-FILKD INTO LF_DUMMY.
          PERFORM ADD_LINE USING SY GT_SYMESS.

          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.

* End Of Note 1324719

* Begin of Note 1293111
* test: is Customer account blocked for posting
*--------------------------------
      SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-KUNNR.
      SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-KUNNR AND
                                      BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
        MESSAGE E351(F5) WITH LS_WTAB-KUNNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* * End of Note 1293111



* copy data of current BoE of tables bkpf, bseg and bsed
* ------------------------------------------------------
      SELECT SINGLE * FROM *BKPF INTO LS_BKPF
                                   WHERE BUKRS = P_BUKRS
                                     AND BELNR = LS_WTAB-BELNR
                                     AND GJAHR = LS_WTAB-GJAHR.
      SELECT SINGLE * FROM *BSEG INTO LS_BSEG
                                   WHERE BUKRS = P_BUKRS
                                     AND BELNR = LS_WTAB-BELNR
                                     AND GJAHR = LS_WTAB-GJAHR
                                     AND BUZEI = LS_WTAB-BUZEI.
* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        SET PARAMETER ID 'BUPLA' FIELD LS_BSEG-BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD LS_BSEG-BUPLA.
        SET PARAMETER ID 'JEA' FIELD LS_BKPF-BRNCH.
      ENDIF.

      SELECT SINGLE * FROM *BSED INTO LS_BSED
                                   WHERE BUKRS = P_BUKRS
                                     AND BELNR = LS_WTAB-BELNR
                                     AND GJAHR = LS_WTAB-GJAHR
                                     AND BUZEI = LS_WTAB-BUZEI.
      IF LS_BKPF IS INITIAL OR LS_BSEG IS INITIAL OR
         LS_BSED IS INITIAL.
* create error message
* --------------------
        MESSAGE E184 WITH LS_WTAB-BELNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* determine document and Boe-to-bank account of BoE usage posting
* -----------------------------------------------------------------
      SELECT SINGLE * FROM *BSEG WHERE BELNR = LS_WTAB-BELNR
                                   AND BUKRS = P_BUKRS
                                   AND BUZEI = LS_WTAB-BUZEI
                                   AND GJAHR = LS_WTAB-GJAHR.
      LF_BELNR = *BSEG-DISBN.
      LF_GJAHR = *BSEG-DISBJ.

* determine allocation field of BoE usage posting
* -----------------------------------------------------------------
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                INTO LF_SELZU.
      SELECT SINGLE * FROM *BSEG WHERE BELNR = LF_BELNR
                                   AND BUKRS = P_BUKRS
                                   AND SHKZG = 'S'
                                   AND ZUONR = LF_SELZU
                                   AND GJAHR = LF_GJAHR.
      LF_KONTO = *BSEG-HKONT.
      IF LF_BELNR IS INITIAL OR LF_GJAHR IS INITIAL OR
         LF_SELZU IS INITIAL.
* create error message
* --------------------
        MESSAGE E181 INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* determine rec. account for bounced BoE postings
* -----------------------------------------------
      LF_KONTO2 = GF_HKONT5.
      IF LF_KONTO2 IS INITIAL.
* create error message
* --------------------
        MESSAGE E171 WITH GF_PORT5 INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check whether field bseg-xref1 is visible on dynpro
* ----------------------------------------------------
      CLEAR LF_AKONT.
      PERFORM GET_CUSTOMER_RECONC_ACCOUNT USING LS_WTAB-BUKRS
                                                LS_WTAB-KUNNR
                                                LF_AKONT.
      IF LF_AKONT IS INITIAL.
* create error message
* --------------------
        MESSAGE E169 INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.
      ENDIF.

      CLEAR LF_FLAG.
      PERFORM CHECK_FIELD_ON_SCREEN USING LS_WTAB-BUKRS
                                          LF_AKONT
                                          BSCHL1
                                          'BSEG-XREF1'
                                          LF_FLAG.
      IF LF_FLAG NE 'X'.
* create error message
* --------------------
        IF LF_FLAG EQ '1'.
          MESSAGE E163 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '2'.
          MESSAGE E164 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '3'.
          MESSAGE E165 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '4'.
          MESSAGE E166 WITH BSCHL1 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '5'.
          MESSAGE E167 WITH 'BSEG-XREF1' INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '6'.
          MESSAGE E168 WITH 'BSEG-XREF1' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check whether field bseg-xref3 is visible on dynpro
* ----------------------------------------------------
      CLEAR LF_FLAG.
      PERFORM CHECK_FIELD_ON_SCREEN USING LS_WTAB-BUKRS
                                          LF_AKONT
                                          BSCHL1
                                          'BSEG-XREF3'
                                          LF_FLAG.
      IF LF_FLAG NE 'X'.
* create error message
* --------------------
        IF LF_FLAG EQ '1'.
          MESSAGE E163 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '2'.
          MESSAGE E164 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '3'.
          MESSAGE E165 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '4'.
          MESSAGE E166 WITH BSCHL1 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '5'.
          MESSAGE E167 WITH 'BSEG-XREF3' INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '6'.
          MESSAGE E168 WITH 'BSEG-XREF3' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check lock of bank sub account
* ------------------------------
      PERFORM CHECK_LOCKED_ACCOUNT  USING P_BUKRS
                                    LF_KONTO
                                    LF_DUMMY.
      IF NOT LF_DUMMY IS INITIAL.
* create error message
* --------------------
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* set date for selection of BoE: sum of due date and protest period
* -----------------------------------------------------------------
      IF LS_WTAB-ZFBDT LE NULL.
        LF_DAT = P_BUDAT.
      ELSE.
        LF_DAT = LS_WTAB-ZFBDT + LS_WTAB-WELGF.
      ENDIF.
* read open line items in table bsix due till lf_dat
* -------------------------------------------------
      PERFORM READ_BSIX USING GT_BELDATA[] LS_WTAB P_BUKRS LF_DAT
                              LF_WAERS 'I'.
* get line number of current BoE in table with open line items
* ------------------------------------------------------------
      PERFORM GET_LINE USING GT_BELDATA[] LS_WTAB ABPOS1 ABPOS2.
* call transaction: reverse contingent liability of current BoE
* -------------------------------------------------------------
      REFRESH BDCTAB.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '102'.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' LF_WAERS.
      IF GF_DATFM = '2' OR GF_DATFM = '3'.
        CONCATENATE LF_DAT+4(2) LF_DAT+6(2) LF_DAT+0(4)
                                        INTO CHAR_DAT.
      ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
        CONCATENATE LF_DAT+0(4) LF_DAT+4(2) LF_DAT+6(2)
                                        INTO CHAR_DAT.
      ELSE.
        CONCATENATE LF_DAT+6(2) LF_DAT+4(2) LF_DAT+0(4)
                                            INTO CHAR_DAT.
      ENDIF.
      PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHAR_DAT.
      PERFORM BATCHFIELD USING 'RF05A-AGKON' LS_WTAB-SAKNR.
      PERFORM BATCHFIELD USING 'RF05A-WVERW' 'I'.
      PERFORM BATCHFIELD USING 'RF05A-SEL01' LS_WTAB-BELNR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PB'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSCOM(01)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '602'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-XPOS1(04)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO+'.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(01)' ' '.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(04)' 'X'.
      ABCOUNT = 1.
      WHILE ABCOUNT LE ABPOS1.
        IF NOT ( ABCOUNT = ABPOS2 ).
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'RF05A-ABPOS' ABCOUNT.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSBET(01)'.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=PI4'.
        ENDIF.
        ADD 1 TO ABCOUNT.
      ENDWHILE.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-20' USING BDCTAB MODE MODE UPDATE UPDATE
                                    MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ELSE.
* call transaction: new BoE with data of current BoE and SHKZG lf_umskz
* ---------------------------------------------------------------------
        REFRESH BDCTAB.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '122'.
        PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
        PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
        PERFORM BATCHFIELD USING 'BKPF-BLART' BLART2.
        PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
        PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
        PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
        PERFORM BATCHFIELD USING 'BKPF-WAERS' P_WAERS.
        PERFORM BATCHFIELD USING 'RF05A-PORTF' LF_PORTF.
        PERFORM BATCHFIELD USING 'RF05A-NEWBS' BSCHL1.
* Start of note 1324719
        IF LS_WTAB-FILKD NE SPACE.
          PERFORM BATCHFIELD USING 'RF05A-NEWKO' LS_WTAB-FILKD.
        ELSE.
          PERFORM BATCHFIELD USING 'RF05A-NEWKO' LS_WTAB-KUNNR.
        ENDIF.
* End Of Note 1324719
        PERFORM BATCHFIELD USING 'BKPF-BVORG' LS_BKPF-BVORG.
        PERFORM BATCHFIELD USING 'RF05A-NEWUM' LF_UMSKZ.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ENTR'.
*==> Start of Note 2784912
*        PERFORM batchdynpro USING 'SAPMF05A' '320'.

        IF T001-BUVAR = '2'.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '2320'.
        ELSE.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '320'.
        ENDIF.
*==> End of Note 2784912
*        WRITE ls_wtab-wrbtr TO charwrbtr CURRENCY ls_wtab-waers.  "AFLE Change
        PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-WRBTR LS_WTAB-WAERS
                                       CHANGING CHARWRBTR.
        PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
        IF LS_WTAB-WAERS NE LS_WTAB-HWAER.
*          WRITE ls_wtab-dmbtr TO chardmbtr CURRENCY ls_wtab-hwaer. "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-DMBTR LS_WTAB-HWAER
                                       CHANGING CHARDMBTR.
          PERFORM BATCHFIELD USING 'BSEG-DMBTR' CHARDMBTR.
        ENDIF.
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                INTO ZZUONR.
        PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
        IF LS_WTAB-BANK IS INITIAL.
          CHARSGTXT = LS_WTAB-BOENO.
        ELSE.
          CONCATENATE LS_WTAB-BOENO '/' LS_WTAB-BANK INTO CHARSGTXT.
        ENDIF.

        IF GV_KURZT IS NOT INITIAL.
          SELECT SINGLE LANGT
            FROM T053
            INTO CHARSGTXT
            WHERE KURZT EQ GV_KURZT
              AND SPRAS EQ SY-LANGU.
        ENDIF.

        PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
        WRITE LS_WTAB-ZFBDT TO CHARZFBDT.
        PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHARZFBDT.
        IF NOT ( LS_BSED-WDATE IS INITIAL ).
          WRITE LS_BSED-WDATE TO CHARWDATE.
          PERFORM BATCHFIELD USING 'BSED-WDATE' CHARWDATE.
        ENDIF.
        IF NOT LS_WTAB-GSBER IS INITIAL.
          PERFORM BATCHFIELD USING 'BSEG-GSBER' LS_WTAB-GSBER.
        ENDIF.
        PERFORM BATCHFIELD USING 'BSED-WNAME' LS_BSED-WNAME.
        PERFORM BATCHFIELD USING 'BSED-WORT1' LS_BSED-WORT1.
        PERFORM BATCHFIELD USING 'BSED-XSIWE' LS_BSED-XSIWE.
        PERFORM BATCHFIELD USING 'BSED-WNAME' LS_BSED-WNAME.
        PERFORM BATCHFIELD USING 'BSED-XAKTZ' LS_BSED-XAKTZ.
        PERFORM BATCHFIELD USING 'BSED-WSTAT' LS_BSED-WSTAT.
        PERFORM BATCHFIELD USING 'BSED-WGBKZ' ' '.
* check whether fields for charges are displayed
* ----------------------------------------------
        SELECT SINGLE * FROM T031 INTO LS_T031
                                       WHERE BUKRS = P_BUKRS.
        IF SY-SUBRC EQ 0.
          PERFORM BATCHFIELD USING 'BSED-WINFW' ' '.
          PERFORM BATCHFIELD USING 'BSED-DISKP' ' '.
          PERFORM BATCHFIELD USING 'BSED-DISKT' ' '.
          PERFORM BATCHFIELD USING 'BSED-WMWKZ' ' '.
          PERFORM BATCHFIELD USING 'BSED-WSTKZ' ' '.
        ENDIF.
        PERFORM BATCHFIELD USING 'BSED-WEVWV' LS_BSED-WEVWV.
        PERFORM BATCHFIELD USING 'BSED-REGIO' LS_BSED-REGIO.
        PERFORM BATCHFIELD USING 'BSED-WBZOG' LS_BSED-WBZOG.
        PERFORM BATCHFIELD USING 'BSED-WORT2' LS_BSED-WORT2.
        IF T001-BUVAR <> '2'.    " Add in Note 2784912
          PERFORM BATCHFIELD USING 'BSED-WBANK' LS_BSED-WBANK.
          PERFORM BATCHFIELD USING 'BSED-WLZBP' LS_BSED-WLZBP.
        ENDIF.                   " Add in Note 2784912
        PERFORM BATCHFIELD USING 'BSED-BANK' LS_WTAB-BANK.
        PERFORM BATCHFIELD USING 'BSED-ACCOU' LS_WTAB-ACCOU.
        PERFORM BATCHFIELD USING 'BSED-BOENO' LS_WTAB-BOENO.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ZK'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '331'.
        PERFORM BATCHFIELD USING 'BSEG-XREF1' LS_BSEG-XREF1.
        PERFORM BATCHFIELD USING 'BSEG-XREF2' LS_BSEG-XREF2.
        PERFORM BATCHFIELD USING 'BSEG-XREF3' LS_BSEG-XREF3.
* check whether credit control area in the document is not empty
        IF LS_WTAB-KKBER NE SPACE.
* check whether credit control area in the document is different
* from default credit control area
          SELECT SINGLE KKBER XKKBI FROM T001
            INTO (LF_KKBER,LF_XKKBI)
           WHERE BUKRS = P_BUKRS.
          IF LS_WTAB-KKBER NE LF_KKBER.
* check whether credit control area in the document is
* modifiable
            IF LF_XKKBI = 'X'.
              PERFORM BATCHFIELD USING 'BSEG-KKBER' LS_WTAB-KKBER.
            ENDIF.
          ENDIF.
        ENDIF.
        IF NOT ( LS_WTAB-DMBE2 IS INITIAL ) AND
                 LS_WTAB-WAERS NE LS_WTAB-HWAE2 AND
                 LS_WTAB-HWAER NE LS_WTAB-HWAE2.
*          WRITE ls_wtab-dmbe2 TO chardmbe2 CURRENCY ls_wtab-hwae2. "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT     USING LS_WTAB-DMBE2 LS_WTAB-HWAE2
                                           CHANGING CHARDMBE2.
          PERFORM BATCHFIELD USING 'BSEG-DMBE2' CHARDMBE2.
        ENDIF.
        IF NOT ( LS_WTAB-DMBE3 IS INITIAL ) AND
                 LS_WTAB-WAERS NE LS_WTAB-HWAE3 AND
                 LS_WTAB-HWAER NE LS_WTAB-HWAE3.
*          WRITE ls_wtab-dmbe3 TO chardmbe3 CURRENCY ls_wtab-hwae3. "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT     USING LS_WTAB-DMBE3 LS_WTAB-HWAE3
                                           CHANGING CHARDMBE3.
          PERFORM BATCHFIELD USING 'BSEG-DMBE3' CHARDMBE3.
        ENDIF.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'SL'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '710'.
        PERFORM BATCHFIELD USING 'RF05A-AGBUK' P_BUKRS.
        PERFORM BATCHFIELD USING 'RF05A-AGKON' LF_KONTO.
        PERFORM BATCHFIELD USING 'RF05A-AGKOA' 'S'.
        CONCATENATE 'RF05A-XPOS1(' LF_LINE ')' INTO CHARXPOS.
        PERFORM BATCHFIELD USING CHARXPOS 'X'.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PA'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '731'.
        PERFORM BATCHFIELD USING 'RF05A-SEL01(01)' LF_SELZU.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
        CALL TRANSACTION 'F-30' USING BDCTAB MODE MODE UPDATE UPDATE
                                      MESSAGES INTO GT_SYMESS.
        IF SY-SUBRC NE 0.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ELSE.
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
          ADD 1 TO COUNT2.
          LS_WTAB-BOX    = '1'.
          LS_WTAB-CHEC   = '1'.
          LS_WTAB-XBLNR  = BORDRO.
          LS_WTAB-PORTF1 = P_PORT1.
          LS_WTAB-PORTF2 = LF_PORTF.
          LS_WTAB-BANKN  = LF_KONTO.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                      PORTF1 PORTF2 BANKN.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ----------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

* at least one BoE successfully posted?
* clear status of SAPScript print for next postings
* -------------------------------------
  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1'.
      TEST = '1'.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

* SAPScript print
* ---------------
  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-007.
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    IF PSCRIPT = 'X'  .
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].

***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    ELSE.
      S_SINGLE = 'X'.
      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
**move ls_data-user into output_header_table-tran.
      OUTPUT_HEADER_TABLE-STATUS = '@0V@'.
      OUTPUT_HEADER_TABLE-EXPAND = S_SINGLE.
      BOE_GLOBAL-USER = SY-UNAME.

      PERFORM OPEN_FORM_PDF.

      PERFORM ITEM_DATA_PDF.

    ENDIF.

*ENDFORM.                    " form reprint

***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660


  ENDIF.

ENDFORM.                                                    " BATCH5

*&---------------------------------------------------------------------*
*&      Form  BATCH6
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'Bounced checks/BoE at the vendor'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH6 USING LT_WTAB LIKE GT_WTAB[].

  DATA: LT_PRINTAB   TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_WTAB      TYPE BOE_TR_LIST,
        LS_DATA      TYPE BOE_GLOBAL_TURKEY,
        LS_BKPF      LIKE BKPF, LS_BSEG LIKE BSEG, LS_BSED LIKE BSED,
        LS_T031      LIKE T031,                             "#EC NEEDED
        LF_DAT       LIKE SY-DATUM,
        LF_KONTO     LIKE BSEG-HKONT,
        LF_KONTO2    LIKE BSEG-HKONT,
        LF_FLAGCB    LIKE T045P-FLAGCB,
        LF_PORTF     LIKE T045P-PORTFO,
        LF_BELNR     LIKE BSEG-BELNR,
        LF_BELNR2    LIKE BSEG-BELNR,
        LF_GJAHR     LIKE BSEG-GJAHR,
        LF_GJAHR2    LIKE BSEG-GJAHR,
        LF_AKONT     LIKE KNB1-AKONT,
        LF_FLAG(1)   TYPE C,
        LF_RESULT(1) TYPE C,
        LF_MODE(1)   TYPE C,
        LF_UMSKZ     LIKE BSEG-UMSKZ,
        LF_SELZU     LIKE BSEG-ZUONR,
        LF_WAERS     LIKE BKPF-WAERS,
        LF_KKBER     LIKE T001-KKBER,
        LF_XKKBI     LIKE T001-XKKBI,
        LF_DUMMY     TYPE C.

* clear required global fields
* ----------------------------
  CLEAR: CHARWRBTR, CHARSGTXT, CHARZFBDT, CHARBUDAT, CHARBLDAT,
         CHAR_DAT, CHARWDATE, CHARREF, ABPOS1, ABPOS2,
         ABCOUNT, BORDRO, COUNT1, COUNT2, BLART1, BLART2, BLART3,
         BSCHL1, BSCHL2, BSCHL3, BSCHL4, ZZUONR,
         CHARDMBTR, CHARDMBE2, CHARDMBE3.

* prepare posting, document and system date for batch input
* --------------------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* get next free bordronumber and document type of postings
* --------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '07'.
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-20'.
  IF CL_COS_UTILITIES=>IS_S4H_CLOUD( ) = ABAP_TRUE. "S/4 HANA Cloud requires BLART = DZ for BoE. For OnPremise the original default of F-36 has to be kept.
    PERFORM GET_DOCUMENT_TYPE USING BLART2 'F36C'.  "F-36 for S/4 HANA Cloud using BLART = DZ
  ELSE.
    PERFORM GET_DOCUMENT_TYPE USING BLART2 'F-36'.
  ENDIF.
  PERFORM GET_DOCUMENT_TYPE USING BLART3 'F-22'.

* get posting key for special G/L debit posting
* ---------------------------------------------
  SELECT SINGLE BSDSS FROM T041A INTO BSCHL1
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL1 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* get posting key for vendor credit posting
* ---------------------------------------------
  SELECT SINGLE BSKHA FROM T041A INTO BSCHL2
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL2 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* get posting key for G/L credit posting
* ---------------------------------------------
  SELECT SINGLE BSSHA FROM T041A INTO BSCHL3
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL3 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* get posting key for customer debit posting
* ---------------------------------------------
  SELECT SINGLE RPDSO FROM T041A INTO BSCHL4
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL4 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* determine portfolio for bounced BoE
* -----------------------------------
  LF_PORTF = GF_PORT5.
  LF_FLAGCB = GF_FLAGCB5.
  IF LF_FLAGCB IS INITIAL.
    MESSAGE E172 WITH GF_PORT5.
  ENDIF.

* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.

* set correct special G/L indicator
* ---------------------------------
      IF P_UMSKZ IS INITIAL.
        LF_UMSKZ = LS_WTAB-UMSKZ.
      ELSE.
        LF_UMSKZ = P_UMSKZ.
      ENDIF.

* check table TBSLT for necessary entry of special G/L indicator
* --------------------------------------------------------------
      LF_MODE = '-'.
      PERFORM CHECK_TBSLT USING SY-LANGU BSCHL1 LF_UMSKZ LF_MODE.
      IF LF_MODE = 'X'.
* create error message
* --------------------
        MESSAGE E156 WITH LF_UMSKZ INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check tables T074* for necessary entry of special G/L indicator
* ---------------------------------------------------------------
      PERFORM CHECK_T074* USING SY-LANGU LF_UMSKZ LF_RESULT.
      IF LF_RESULT = '1' OR LF_RESULT = '2' OR LF_RESULT = '3'.
* create error message
* --------------------
        IF LF_RESULT EQ '1'.
          MESSAGE E183 WITH LF_UMSKZ 'T074A' INTO LF_DUMMY.
        ELSEIF LF_RESULT EQ '2'.
          MESSAGE E183 WITH LF_UMSKZ 'T074T' INTO LF_DUMMY.
        ELSEIF LF_RESULT EQ '3'.
          MESSAGE E183 WITH LF_UMSKZ 'T074U' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* check table T074 for necessary entry of special G/L indicator
* ---------------------------------------------------------------
      LF_MODE = '+'.
      PERFORM CHECK_T074 USING GS_T001-KTOPL LF_UMSKZ LS_WTAB-KUNNR
                               LS_WTAB-BUKRS LF_MODE.
      IF LF_MODE = '1' OR LF_MODE = '2' OR LF_MODE = '3'.
* create error message
* --------------------
        IF LF_MODE EQ '1'.
          MESSAGE E162 WITH LS_WTAB-KUNNR INTO LF_DUMMY.
        ELSEIF LF_MODE EQ '2'.
          MESSAGE E160 WITH LF_UMSKZ INTO LF_DUMMY.
        ELSEIF LF_MODE EQ '3'.
          MESSAGE E161 WITH LF_UMSKZ INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* Begin of Note 1324719
* test: is Customer branch account blocked for posting
*--------------------------------
      IF LS_WTAB-FILKD NE SPACE.
        SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-FILKD.
        SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-FILKD AND
                                        BUKRS = P_BUKRS.
        IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
          MESSAGE E351(F5) WITH LS_WTAB-FILKD INTO LF_DUMMY.
          PERFORM ADD_LINE USING SY GT_SYMESS.

          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.

* test: is Customer account blocked for posting
*--------------------------------
      SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-KUNNR.
      SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-KUNNR AND
                                      BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
        MESSAGE E351(F5) WITH LS_WTAB-KUNNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* End Of Note 1324719
* copy data of current BoE of tables bkpf, bseg and bsed
* determine document of BoE usage posting
* ------------------------------------------------------
      SELECT SINGLE * FROM *BKPF INTO LS_BKPF
                                   WHERE BUKRS = P_BUKRS
                                     AND BELNR = LS_WTAB-BELNR
                                     AND GJAHR = LS_WTAB-GJAHR.

      SELECT SINGLE * FROM *BSEG INTO LS_BSEG
                                   WHERE BUKRS = P_BUKRS
                                     AND BELNR = LS_WTAB-BELNR
                                     AND GJAHR = LS_WTAB-GJAHR
                                     AND BUZEI = LS_WTAB-BUZEI.
      LF_BELNR = LS_BSEG-DISBN.
      LF_GJAHR = LS_BSEG-DISBJ.
* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        SET PARAMETER ID 'BUPLA' FIELD LS_BSEG-BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD LS_BSEG-BUPLA.
        SET PARAMETER ID 'JEA' FIELD LS_BKPF-BRNCH.
      ENDIF.

      SELECT SINGLE * FROM *BSED INTO LS_BSED
                                   WHERE BUKRS = P_BUKRS
                                     AND BELNR = LS_WTAB-BELNR
                                     AND GJAHR = LS_WTAB-GJAHR
                                     AND BUZEI = LS_WTAB-BUZEI.

      IF LS_BKPF IS INITIAL OR LS_BSEG IS INITIAL OR
         LS_BSED IS INITIAL.

* create error message
* --------------------
        MESSAGE E184 WITH LS_WTAB-BELNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* determine document of posting: acc. pay. to BoE-to-vendor account
* -----------------------------------------------------------------
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO LF_SELZU.
      SELECT SINGLE * FROM *BSEG WHERE BELNR = LF_BELNR
                                   AND BUKRS = P_BUKRS
                                   AND GJAHR = LF_GJAHR
                                   AND ZUONR = LF_SELZU
                                   AND SHKZG = 'S'.
      LF_BELNR2 = *BSEG-AUGBL.

      CLEAR LF_GJAHR2.
      CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
        EXPORTING
          I_DATE  = *BSEG-AUGDT
          I_PERIV = GS_T001-PERIV
        IMPORTING
          E_GJAHR = LF_GJAHR2
        EXCEPTIONS
          OTHERS  = 1.

* determine acc. payable
* ----------------------
      SELECT SINGLE * FROM *BSEG WHERE BUKRS = P_BUKRS
                                   AND BELNR = LF_BELNR2
                                   AND GJAHR = LF_GJAHR2
                                   AND ZUONR = LF_SELZU
                                   AND SHKZG = 'S'.
      IF LF_BELNR2 IS INITIAL OR LF_GJAHR2 IS INITIAL OR
         *BSEG-LIFNR IS INITIAL.

* create error message
* --------------------
        MESSAGE E186 WITH LS_WTAB-BELNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
      LF_KONTO = *BSEG-LIFNR.
* determine account for bouncd BoE postings
* -----------------------------------------
      LF_KONTO2 = GF_HKONT5.
      IF LF_KONTO2 IS INITIAL.
* create error message
* --------------------
        MESSAGE E171 WITH GF_PORT5 INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check whether field bseg-xref1 is visible on dynpro
* ----------------------------------------------------
      CLEAR LF_AKONT.
      PERFORM GET_CUSTOMER_RECONC_ACCOUNT USING LS_WTAB-BUKRS
                                                LS_WTAB-KUNNR
                                                LF_AKONT.
      IF LF_AKONT IS INITIAL.
* create error message
* --------------------
        MESSAGE E169 INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.
      ENDIF.

      CLEAR LF_FLAG.
      PERFORM CHECK_FIELD_ON_SCREEN USING LS_WTAB-BUKRS
                                          LF_AKONT
                                          BSCHL1
                                          'BSEG-XREF1'
                                          LF_FLAG.
      IF LF_FLAG NE 'X'.
* create error message
* --------------------
        IF LF_FLAG EQ '1'.
          MESSAGE E163 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '2'.
          MESSAGE E164 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '3'.
          MESSAGE E165 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '4'.
          MESSAGE E166 WITH BSCHL1 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '5'.
          MESSAGE E167 WITH 'BSEG-XREF1' INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '6'.
          MESSAGE E168 WITH 'BSEG-XREF1' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check whether field bseg-xref3 is visible on dynpro
* ----------------------------------------------------
      CLEAR LF_FLAG.
      PERFORM CHECK_FIELD_ON_SCREEN USING LS_WTAB-BUKRS
                                          LF_AKONT
                                          BSCHL1
                                          'BSEG-XREF3'
                                          LF_FLAG.
      IF LF_FLAG NE 'X'.
* create error message
* --------------------
        IF LF_FLAG EQ '1'.
          MESSAGE E163 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '2'.
          MESSAGE E164 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '3'.
          MESSAGE E165 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '4'.
          MESSAGE E166 WITH BSCHL1 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '5'.
          MESSAGE E167 WITH 'BSEG-XREF3' INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '6'.
          MESSAGE E168 WITH 'BSEG-XREF3' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* set date for selection of BoE: sum of due date and protest period
* -----------------------------------------------------------------
      IF LS_WTAB-ZFBDT LE NULL.
        LF_DAT = P_BUDAT.
      ELSE.
        LF_DAT = LS_WTAB-ZFBDT + LS_WTAB-WELGF.
      ENDIF.
* read open line items in table bsix due till lf_dat
* -------------------------------------------------
      PERFORM READ_BSIX USING GT_BELDATA[] LS_WTAB P_BUKRS LF_DAT
                              LF_WAERS 'F'.
* get line number of current BoE in table with open line items
* ------------------------------------------------------------
      PERFORM GET_LINE USING GT_BELDATA[] LS_WTAB ABPOS1 ABPOS2.

* Note 971590 START
* test: is Vendor account blocked for posting
*--------------------------------
      SELECT SINGLE * FROM LFA1 WHERE LIFNR = *BSEG-LIFNR.
      SELECT SINGLE * FROM LFB1 WHERE LIFNR = *BSEG-LIFNR AND
                                      BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0 OR LFA1-SPERR EQ 'X' OR lfB1-SPERR EQ 'X'.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
        MESSAGE E351(F5) WITH LS_WTAB-VENDR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* Note 971590 END

* call transaction: reverse contingent liability of current BoE
* -------------------------------------------------------------
      REFRESH BDCTAB.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '102'.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' LF_WAERS.
      IF GF_DATFM = '2' OR GF_DATFM = '3'.
        CONCATENATE LF_DAT+4(2) LF_DAT+6(2) LF_DAT+0(4)
                                        INTO CHAR_DAT.
      ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
        CONCATENATE LF_DAT+0(4) LF_DAT+4(2) LF_DAT+6(2)
                                        INTO CHAR_DAT.
      ELSE.
        CONCATENATE LF_DAT+6(2) LF_DAT+4(2) LF_DAT+0(4)
                                            INTO CHAR_DAT.
      ENDIF.
      PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHAR_DAT.
      PERFORM BATCHFIELD USING 'RF05A-AGKON' LS_WTAB-SAKNR.
      PERFORM BATCHFIELD USING 'RF05A-WVERW' 'F'.
      PERFORM BATCHFIELD USING 'RF05A-SEL01' LS_WTAB-BELNR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PB'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSCOM(01)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '602'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-XPOS1(04)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO+'.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(01)' ' '.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(04)' 'X'.
      ABCOUNT = 1.
      WHILE ABCOUNT LE ABPOS1.
        IF NOT ( ABCOUNT = ABPOS2 ).
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'RF05A-ABPOS' ABCOUNT.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSBET(01)'.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=PI4'.
        ENDIF.
        ADD 1 TO ABCOUNT.
      ENDWHILE.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-20' USING BDCTAB MODE MODE UPDATE UPDATE
                                    MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ELSE.
* call transaction: new BoE with data of current BoE and SHKZG lf_umskz
* ---------------------------------------------------------------------
        REFRESH BDCTAB.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '122'.
        PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
        PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
        PERFORM BATCHFIELD USING 'BKPF-BLART' BLART2.
        PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
        PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
        PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
        PERFORM BATCHFIELD USING 'BKPF-WAERS' P_WAERS.
        PERFORM BATCHFIELD USING 'RF05A-PORTF' LF_PORTF.
        PERFORM BATCHFIELD USING 'RF05A-NEWBS' BSCHL1.
* Begin of Note 1324719
        IF LS_WTAB-FILKD NE SPACE.
          PERFORM BATCHFIELD USING 'RF05A-NEWKO' LS_WTAB-FILKD.
        ELSE.
          PERFORM BATCHFIELD USING 'RF05A-NEWKO' LS_WTAB-KUNNR.
        ENDIF.
* End Of Note 1324719
        PERFORM BATCHFIELD USING 'BKPF-BVORG' LS_BKPF-BVORG.
        PERFORM BATCHFIELD USING 'RF05A-NEWUM' LF_UMSKZ.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ENTR'.
*==> Start of Note 2784912
*        PERFORM batchdynpro USING 'SAPMF05A' '320'.

        IF T001-BUVAR = '2'.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '2320'.
        ELSE.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '320'.
        ENDIF.
*==> End of Note 2784912
*        WRITE ls_wtab-wrbtr TO charwrbtr CURRENCY ls_wtab-waers. "AFLE Change
        PERFORM WRITE_AMT_TO_CHARAMT   USING LS_WTAB-WRBTR LS_WTAB-WAERS
                                       CHANGING CHARWRBTR.
        PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
        IF LS_WTAB-WAERS NE LS_WTAB-HWAER.
*          WRITE ls_wtab-dmbtr TO chardmbtr CURRENCY ls_wtab-hwaer. "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-DMBTR LS_WTAB-HWAER
                                       CHANGING CHARDMBTR.
          PERFORM BATCHFIELD USING 'BSEG-DMBTR' CHARDMBTR.
        ENDIF.
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                INTO ZZUONR.
        PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
        IF LS_WTAB-BANK IS INITIAL.
          CHARSGTXT = LS_WTAB-BOENO.
        ELSE.
          CONCATENATE LS_WTAB-BOENO '/' LS_WTAB-BANK INTO CHARSGTXT.
        ENDIF.
        PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
        WRITE LS_WTAB-ZFBDT TO CHARZFBDT.
        PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHARZFBDT.
        IF NOT ( LS_BSED-WDATE IS INITIAL ).
          WRITE LS_BSED-WDATE TO CHARWDATE.
          PERFORM BATCHFIELD USING 'BSED-WDATE' CHARWDATE.
        ENDIF.
        IF NOT LS_WTAB-GSBER IS INITIAL.
          PERFORM BATCHFIELD USING 'BSEG-GSBER' LS_WTAB-GSBER.
        ENDIF.
        PERFORM BATCHFIELD USING 'BSED-WNAME' LS_BSED-WNAME.
        PERFORM BATCHFIELD USING 'BSED-WORT1' LS_BSED-WORT1.
        PERFORM BATCHFIELD USING 'BSED-XSIWE' LS_BSED-XSIWE.
        PERFORM BATCHFIELD USING 'BSED-WNAME' LS_BSED-WNAME.
        PERFORM BATCHFIELD USING 'BSED-XAKTZ' LS_BSED-XAKTZ.
        PERFORM BATCHFIELD USING 'BSED-WSTAT' LS_BSED-WSTAT.
        PERFORM BATCHFIELD USING 'BSED-WGBKZ' ' '.
* check whether fields for charges are displayed
* ----------------------------------------------
        SELECT SINGLE * FROM T031 INTO LS_T031
                                       WHERE BUKRS = P_BUKRS.
        IF SY-SUBRC EQ 0.
          PERFORM BATCHFIELD USING 'BSED-WINFW' ' '.
          PERFORM BATCHFIELD USING 'BSED-DISKP' ' '.
          PERFORM BATCHFIELD USING 'BSED-DISKT' ' '.
          PERFORM BATCHFIELD USING 'BSED-WMWKZ' ' '.
          PERFORM BATCHFIELD USING 'BSED-WSTKZ' ' '.
        ENDIF.
        PERFORM BATCHFIELD USING 'BSED-WEVWV' LS_BSED-WEVWV.
        PERFORM BATCHFIELD USING 'BSED-REGIO' LS_BSED-REGIO.
        PERFORM BATCHFIELD USING 'BSED-WBZOG' LS_BSED-WBZOG.
        PERFORM BATCHFIELD USING 'BSED-WORT2' LS_BSED-WORT2.
        IF T001-BUVAR <> '2'.    " Add in Note 2784912
          PERFORM BATCHFIELD USING 'BSED-WBANK' LS_BSED-WBANK.
          PERFORM BATCHFIELD USING 'BSED-WLZBP' LS_BSED-WLZBP.
        ENDIF.                   " Add in Note 2784912
        PERFORM BATCHFIELD USING 'BSED-BANK' LS_WTAB-BANK.
        PERFORM BATCHFIELD USING 'BSED-ACCOU' LS_WTAB-ACCOU.
        PERFORM BATCHFIELD USING 'BSED-BOENO' LS_WTAB-BOENO.
        PERFORM BATCHFIELD USING 'RF05A-NEWBS' BSCHL2.
        PERFORM BATCHFIELD USING 'RF05A-NEWKO' LF_KONTO.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ZK'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '331'.
        PERFORM BATCHFIELD USING 'BSEG-XREF1' LS_BSEG-XREF1.
        PERFORM BATCHFIELD USING 'BSEG-XREF2' LS_BSEG-XREF2.
        PERFORM BATCHFIELD USING 'BSEG-XREF3' LS_BSEG-XREF3.
* check whether credit control area in the document is not empty
        IF LS_WTAB-KKBER NE SPACE.
* check whether credit control area in the document is different
* from default credit control area
          SELECT SINGLE KKBER XKKBI FROM T001
            INTO (LF_KKBER, LF_XKKBI)
           WHERE BUKRS = P_BUKRS.
          IF LS_WTAB-KKBER NE LF_KKBER.
* check whether credit control area in the document is
* modifiable
            IF LF_XKKBI = 'X'.
              PERFORM BATCHFIELD USING 'BSEG-KKBER' LS_WTAB-KKBER.
            ENDIF.
          ENDIF.
        ENDIF.
        IF NOT ( LS_WTAB-DMBE2 IS INITIAL ) AND
                 LS_WTAB-WAERS NE LS_WTAB-HWAE2 AND
                 LS_WTAB-HWAER NE LS_WTAB-HWAE2.
*          WRITE ls_wtab-dmbe2 TO chardmbe2 CURRENCY ls_wtab-hwae2. "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-DMBE2 LS_WTAB-HWAE2
                                       CHANGING CHARDMBE2.
          PERFORM BATCHFIELD USING 'BSEG-DMBE2' CHARDMBE2.
        ENDIF.
        IF NOT ( LS_WTAB-DMBE3 IS INITIAL ) AND
                 LS_WTAB-WAERS NE LS_WTAB-HWAE3 AND
                 LS_WTAB-HWAER NE LS_WTAB-HWAE3.
*          WRITE ls_wtab-dmbe3 TO chardmbe3 CURRENCY ls_wtab-hwae3.  "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-DMBE3 LS_WTAB-HWAE3
                                       CHANGING CHARDMBE3.
          PERFORM BATCHFIELD USING 'BSEG-DMBE3' CHARDMBE3.
        ENDIF.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ENTR'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '302'.
        PERFORM BATCHFIELD USING 'BSEG-WRBTR' '*'.
        PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHARZFBDT.
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                INTO ZZUONR.
        PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
        PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
        IF NOT LS_WTAB-GSBER IS INITIAL.
          PERFORM BATCHFIELD USING 'BSEG-GSBER' LS_WTAB-GSBER.
        ENDIF.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
        CALL TRANSACTION 'F-36' USING BDCTAB MODE MODE UPDATE UPDATE
                                      MESSAGES INTO GT_SYMESS.
        IF SY-SUBRC NE 0.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
          CONTINUE.
        ELSE.
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
          ADD 1 TO COUNT2.
          LS_WTAB-BOX    = '1'.
          LS_WTAB-CHEC   = '1'.
          LS_WTAB-XBLNR  = BORDRO.
          LS_WTAB-PORTF1 = P_PORT1.
          LS_WTAB-PORTF2 = LF_PORTF.
          LS_WTAB-LIFNA  = LF_KONTO.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                      PORTF1 PORTF2 LIFNA.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ----------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

** at least one BoE successfully posted?
* clear status of SAPScript print for next postings
* -------------------------------------
  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1'.
      TEST = '1'.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

* SAPScript print
* ---------------
  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-008.
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    IF PSCRIPT = 'X'  .
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    ELSE.
      S_SINGLE = 'X'.
      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
**move ls_data-user into output_header_table-tran.
      OUTPUT_HEADER_TABLE-STATUS = '@0V@'.
      OUTPUT_HEADER_TABLE-EXPAND = S_SINGLE.
      BOE_GLOBAL-USER = SY-UNAME.

      PERFORM OPEN_FORM_PDF.

      PERFORM ITEM_DATA_PDF.

    ENDIF.

*ENDFORM.                    " form reprint

***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660

  ENDIF.

ENDFORM.                                                    " BATCH6

*&---------------------------------------------------------------------*
*&      Form  BATCH7
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'Clearing of bounced checks/BoE'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH7 USING LT_WTAB LIKE GT_WTAB[].

  DATA: LT_PRINTAB TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_DATA    TYPE BOE_GLOBAL_TURKEY,
        LS_WTAB    TYPE BOE_TR_LIST,
        LF_LINE(2) TYPE C,
        LF_BUPLA   LIKE BSEG-BUPLA,
        LF_BRNCH   LIKE BKPF-BRNCH,
        LF_GSBER   LIKE BSEG-GSBER,
        LF_LIFNA   TYPE LIFNR,
        LF_BANKN   TYPE SAKNR,
        LD_BELNR   LIKE BSEG-BELNR,
        LD_BUZEI   LIKE BSEG-BUZEI,
        LD_GJAHR   LIKE BSEG-GJAHR,
        LC_BELNR   LIKE BSEG-BELNR,
        LC_GJAHR   LIKE BSEG-GJAHR,
        LF_DUMMY   TYPE C.

* clear required global fields
* ----------------------------
  CLEAR: CHARWRBTR, CHARZFBDT, CHARBLDAT, CHARBUDAT, CHARXPOS, CHARREF,
         COUNT1, COUNT2, BLART1, BSCHL1,CHARKURSF, BORDRO.

* prepare posting date and document date for batch input
* ------------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* get line of radiobutton 'Allocation' on screen SAPMF05A 710
* ----------------------------------------------------------------
  PERFORM GET_LINE_SCREEN710 USING 'ZUONR' LF_LINE.

* get document type of posting
* ----------------------------
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-30'.

* get posting key for customer debit posting
* ---------------------------------------------
  SELECT SINGLE RPDSO FROM T041A INTO BSCHL1
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL1 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* get next free bordronumber and document type of posting
* --------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '08'.

* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.

* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        CLEAR: LF_BUPLA, LF_BRNCH.
        SELECT SINGLE BUPLA FROM BSEG INTO LF_BUPLA
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUZEI = LS_WTAB-BUZEI
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'BUPLA' FIELD LF_BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD LF_BUPLA.
        SELECT SINGLE BRNCH FROM BKPF INTO LF_BRNCH
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'JEA' FIELD LF_BRNCH.
      ENDIF.

* read vendor account or interim account
* --------------------------------------
      LD_BELNR = LS_WTAB-BKTXT(10).
      LD_GJAHR = LS_WTAB-BKTXT+13(4).
      LD_BUZEI = LS_WTAB-BKTXT+10(3).

      SELECT SINGLE VENDR FROM BSED INTO LF_LIFNA
                              WHERE BELNR = LD_BELNR
                                AND BUZEI = LD_BUZEI
                                AND BUKRS = P_BUKRS
                                AND GJAHR = LD_GJAHR.

* no vendor account -> read interim account
* -----------------------------------------
      IF LF_LIFNA IS INITIAL.
        SELECT SINGLE * FROM *BSEG WHERE BELNR = LD_BELNR
                                     AND BUZEI = LD_BUZEI
                                     AND BUKRS = P_BUKRS
                                     AND GJAHR = LD_GJAHR.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE * FROM *BSEG WHERE BELNR = *BSEG-DISBN
                                       AND SHKZG = 'S'
                                       AND BUKRS = P_BUKRS
                                       AND GJAHR = *BSEG-DISBJ.
          IF SY-SUBRC EQ 0.
            LF_BANKN = *BSEG-HKONT.
          ENDIF.
        ENDIF.
      ENDIF.

* read business area of customer line item on payment document
* ------------------------------------------------------------

      CLEAR: LC_BELNR, LC_GJAHR.

      DO.
        SELECT SINGLE * FROM BKPF WHERE BUKRS = LS_WTAB-BUKRS
                                    AND BELNR = LD_BELNR
                                    AND GJAHR = LD_GJAHR.

        IF SY-SUBRC NE 0.
          EXIT.
        ELSE.
          LC_BELNR = LD_BELNR.
          LC_GJAHR = LD_GJAHR.
          LD_BELNR = BKPF-BKTXT(10).
          LD_GJAHR = BKPF-BKTXT+13(4).
        ENDIF.
      ENDDO.

      CLEAR LF_GSBER.

      SELECT SINGLE GSBER FROM *BSEG INTO LF_GSBER
                                    WHERE BELNR = LC_BELNR
                                      AND KUNNR = LS_WTAB-KUNNR
                                      AND SHKZG = 'H'
                                      AND WRBTR = LS_WTAB-WRBTR
                                      AND BUKRS = P_BUKRS
                                      AND GJAHR = LC_GJAHR.
      IF SY-SUBRC NE 0.
        SELECT SINGLE GSBER FROM *BSEG INTO LF_GSBER
                                      WHERE BELNR = LC_BELNR
                                        AND KUNNR = LS_WTAB-KUNNR
                                        AND SHKZG = 'H'
                                        AND BUKRS = P_BUKRS
                                        AND GJAHR = LC_GJAHR.
        IF SY-SUBRC NE 0.
          LF_GSBER = LS_WTAB-GSBER.
        ENDIF.
      ENDIF.

* call transaction: reverse contingent liability of old BoE, new open
* item on acc. receivable
* -------------------------------------------------------------------
      REFRESH BDCTAB.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '122'.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' P_WAERS.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
* Changes begin : Note 1293111
      IF LS_WTAB-WAERS NE LS_WTAB-HWAER AND
         NOT ( GF_KURSF IS INITIAL ).
        WRITE GF_KURSF TO CHARKURSF.
        PERFORM BATCHFIELD USING 'BKPF-KURSF' CHARKURSF.
      ENDIF.
* Changes end : Note 1293111
      PERFORM BATCHFIELD USING 'RF05A-NEWBS' BSCHL1.
* Begin of Note 1324719
      IF LS_WTAB-FILKD NE SPACE.
        PERFORM BATCHFIELD USING 'RF05A-NEWKO' LS_WTAB-FILKD.
      ELSE.
        PERFORM BATCHFIELD USING 'RF05A-NEWKO' LS_WTAB-KUNNR.
      ENDIF.
* End Of Note 1324719
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ENTR'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '301'.
*      WRITE ls_wtab-wrbtr TO charwrbtr CURRENCY ls_wtab-waers. "AFLE Change
      PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-WRBTR LS_WTAB-WAERS
                                   CHANGING CHARWRBTR.
      PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
      PERFORM BATCHFIELD USING 'BSEG-SGTXT' TEXT-012.
      WRITE LS_WTAB-ZFBDT TO CHARZFBDT.
      PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHARZFBDT.
      PERFORM BATCHFIELD USING 'BSEG-ZTERM' '    '.
      PERFORM BATCHFIELD USING 'BSEG-ZBD1T' '   '.
      PERFORM BATCHFIELD USING 'BSEG-ZBD1P' '      '.
      PERFORM BATCHFIELD USING 'BSEG-ZBD2T' '   '.
      PERFORM BATCHFIELD USING 'BSEG-ZBD2P' '      '.
      PERFORM BATCHFIELD USING 'BSEG-ZBD3T' '   '.
      IF NOT LF_GSBER IS INITIAL.
        PERFORM BATCHFIELD USING 'BSEG-GSBER' LF_GSBER.
      ENDIF.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'SL'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '710'.
      PERFORM BATCHFIELD USING 'RF05A-AGBUK' P_BUKRS.
      PERFORM BATCHFIELD USING 'RF05A-AGKON' LS_WTAB-KUNNR.
      PERFORM BATCHFIELD USING 'RF05A-AGKOA' 'D'.
      PERFORM BATCHFIELD USING 'RF05A-AGUMS' LS_WTAB-UMSKZ.
      PERFORM BATCHFIELD USING 'RF05A-XNOPS' ' '.
      CONCATENATE 'RF05A-XPOS1(' LF_LINE ')' INTO CHARXPOS.
      PERFORM BATCHFIELD USING CHARXPOS 'X'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PA'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '731'.
      PERFORM BATCHFIELD USING 'RF05A-SEL01(01)' LS_WTAB-ZUONR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-30' USING BDCTAB MODE MODE UPDATE UPDATE
                                    MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ELSE.
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
        ADD 1 TO COUNT2.
        LS_WTAB-BOX    = '1'.
        LS_WTAB-CHEC   = '1'.
        LS_WTAB-XBLNR  = BORDRO.
        LS_WTAB-PORTF1 = P_PORT3.
        LS_WTAB-LIFNA = LF_LIFNA.
        LS_WTAB-BANKN = LF_BANKN.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                    PORTF1 LIFNA BANKN.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ----------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

* at least one BoE successfully posted?
* clear status of SAPScript print for next postings
* -------------------------------------
  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1'.
      TEST = '1'.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660


* SAPScript print
* ---------------
  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-012.
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    IF PSCRIPT = 'X'  .
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    ELSE.
      S_SINGLE = 'X'.
      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
**move ls_data-user into output_header_table-tran.
      OUTPUT_HEADER_TABLE-STATUS = '@0V@'.
      OUTPUT_HEADER_TABLE-EXPAND = S_SINGLE.
      BOE_GLOBAL-USER = SY-UNAME.

      PERFORM OPEN_FORM_PDF.

      PERFORM ITEM_DATA_PDF.

    ENDIF.

*ENDFORM.                    " form reprint

***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660

  ENDIF.

ENDFORM.                                                    " BATCH7

*&---------------------------------------------------------------------*
*&      Form  BATCH8
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'Portfolio Movement: Exits'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH8 USING LT_WTAB LIKE GT_WTAB[].

  DATA: LT_PRINTAB   TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_WTAB      TYPE BOE_TR_LIST,
        LS_DATA      TYPE BOE_GLOBAL_TURKEY,
        LF_LINECOUNT TYPE I,
        LF_HKONT     LIKE T045P-HKONT,
        LF_BUPLA     LIKE BSEG-BUPLA,
        LF_BRNCH     LIKE BKPF-BRNCH,
        LF_DUMMY     TYPE C.

* clear required global fields
* ----------------------------
  CLEAR: CHARWRBTR, CHARSGTXT, CHARZFBDT, CHARBUDAT, CHARBLDAT,
         CHAR_DAT, CHARREF, BORDRO, COUNT1, COUNT2, BLART1, ZZUONR.

* prepare posting, document, system and due date for batch input
* --------------------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* field bsed-wgbkz must be changeable by FB02
* -------------------------------------------
  PERFORM CHECK_DOC_CHANGE_RULES USING 'BSED-WGBKZ'.

* field bsed-vendr must be changeable by FB02
* -------------------------------------------
  PERFORM CHECK_DOC_CHANGE_RULES USING 'BSED-VENDR'.

* get next free bordronumber and document type of posting
* -------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '09'.
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-33'.

* determine accounts for portfolio issues
* ---------------------------------------
  SELECT SINGLE HKONT FROM T045P INTO LF_HKONT
                                 WHERE BUKRS = P_BUKRS
                                  AND PORTFO = P_PORT4.
  IF ( LF_HKONT IS INITIAL ).
    MESSAGE E134.
  ENDIF.

* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.
* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        CLEAR: LF_BUPLA, LF_BRNCH.
        SELECT SINGLE BUPLA FROM BSEG INTO LF_BUPLA
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUZEI = LS_WTAB-BUZEI
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'BUPLA' FIELD LF_BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD LF_BUPLA.
        SELECT SINGLE BRNCH FROM BKPF INTO LF_BRNCH
                                       WHERE BELNR = LS_WTAB-BELNR
                                         AND BUKRS = P_BUKRS
                                         AND GJAHR = LS_WTAB-GJAHR.
        SET PARAMETER ID 'JEA' FIELD LF_BRNCH.
      ENDIF.
* Begin of Note 1324719
* test: is Customer branch account blocked for posting
*--------------------------------
      IF LS_WTAB-FILKD NE SPACE.
        SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-FILKD.
        SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-FILKD AND
                                        BUKRS = P_BUKRS.
        IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
          MESSAGE E351(F5) WITH LS_WTAB-FILKD INTO LF_DUMMY.
          PERFORM ADD_LINE USING SY GT_SYMESS.

          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.

* test: is Customer account blocked for posting
*--------------------------------
      SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-KUNNR.
      SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-KUNNR AND
                                      BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
        MESSAGE E351(F5) WITH LS_WTAB-KUNNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* End Of Note 1324719
* call transaction: posting issue account to issue reconsi. account
* -----------------------------------------------------------------
      REFRESH BDCTAB.
*      WRITE ls_wtab-wrbtr TO charwrbtr CURRENCY ls_wtab-waers.   "AFLE Change
      PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-WRBTR LS_WTAB-WAERS
                                   CHANGING CHARWRBTR.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '101'.
      PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO ZZUONR.
      PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
      IF LS_WTAB-BANK IS INITIAL.
        CHARSGTXT = LS_WTAB-BOENO.
      ELSE.
        CONCATENATE LS_WTAB-BOENO '/' LS_WTAB-BANK INTO CHARSGTXT.
      ENDIF.
      PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' LS_WTAB-WAERS.
      IF NOT LS_WTAB-GSBER IS INITIAL.
        PERFORM BATCHFIELD USING 'BSEG-GSBER' LS_WTAB-GSBER.
      ENDIF.
      WRITE LS_WTAB-ZFBDT TO CHARZFBDT.
      PERFORM BATCHFIELD USING 'BSEG-VALUT' CHARZFBDT.
      PERFORM BATCHFIELD USING 'RF05A-KONTO' LF_HKONT.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'SW'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '705'.
      PERFORM BATCHFIELD USING '*BKPF-BELNR(1)' LS_WTAB-BELNR.
      PERFORM BATCHFIELD USING '*BSEG-BUZEI(1)' LS_WTAB-BUZEI.
      PERFORM BATCHFIELD USING '*BKPF-GJAHR(1)' LS_WTAB-GJAHR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-33' USING BDCTAB MODE MODE UPDATE UPDATE
                                    MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ELSE.
* call transaction: change receipt document with status 'on the way'
* ------------------------------------------------------------------
        REFRESH BDCTAB.
        PERFORM BATCHDYNPRO USING 'SAPMF05L' '100'.
        PERFORM BATCHFIELD USING 'RF05L-BELNR' LS_WTAB-BELNR.
        PERFORM BATCHFIELD USING 'RF05L-BUKRS' P_BUKRS.
        PERFORM BATCHFIELD USING 'RF05L-GJAHR' LS_WTAB-GJAHR.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
        PERFORM BATCHDYNPRO USING 'SAPMF05L' '700'.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' '=ABZL'.
        PERFORM BATCHDYNPRO USING 'SAPMF05L' '1130'.
        PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05L-BUZEI'.
        PERFORM BATCHFIELD USING 'RF05L-BUZEI' LS_WTAB-BUZEI.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' '=ENTR'.
        PERFORM BATCHDYNPRO USING 'SAPMF05L' '700'.
        PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05L-ANZDT(01)'.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' '=PK'.
*==> Start of Note 2784912
*        PERFORM batchdynpro USING 'SAPMF05L' '320'.

        IF T001-BUVAR = '2'.
          PERFORM BATCHDYNPRO USING 'SAPMF05L' '2320'.
        ELSE.
          PERFORM BATCHDYNPRO USING 'SAPMF05L' '320'.
        ENDIF.
*==> End of Note 2784912
        PERFORM BATCHFIELD USING 'BDC_CURSOR' 'BSED-WGBKZ'.
        PERFORM BATCHFIELD USING 'BSED-WGBKZ' '1'.
        PERFORM BATCHFIELD USING 'BDC_CURSOR' 'BSED-VENDR'.
        PERFORM BATCHFIELD USING 'BSED-VENDR' P_PORT2.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' '=AE'.
        CALL TRANSACTION 'FB02' USING BDCTAB MODE MODE UPDATE UPDATE
                                      MESSAGES INTO GT_SYMESS.
        IF SY-SUBRC NE 0.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ELSE.
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
          ADD 1 TO COUNT2.
          LS_WTAB-BOX = '1'.
          LS_WTAB-CHEC = '1'.
          LS_WTAB-XBLNR = BORDRO.
          LS_WTAB-PORTF1 = P_PORT4.
          LS_WTAB-PORTF2 = P_PORT2.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                      PORTF1 PORTF2.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ----------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

** at least one BoE successfully posted?
* clear status of SAPScript print for next postings
* -------------------------------------
  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1'.
      TEST = '1'.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

* SAPScript print
* ---------------
  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-009.
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    IF PSCRIPT = 'X'  .
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    ELSE.
      S_SINGLE = 'X'.
      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
**move ls_data-user into output_header_table-tran.
      OUTPUT_HEADER_TABLE-STATUS = '@0V@'.
      OUTPUT_HEADER_TABLE-EXPAND = S_SINGLE.
      BOE_GLOBAL-USER = SY-UNAME.

      PERFORM OPEN_FORM_PDF.

      PERFORM ITEM_DATA_PDF.

    ENDIF.

*ENDFORM.                    " form reprint

***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660

  ENDIF.

ENDFORM.                                                    " BATCH8

*&---------------------------------------------------------------------*
*&      Form  BATCH9
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'Portfolio Movement: Entries'
*----------------------------------------------------------------------*
*       --> lt_wtab    internal table with check/BoE data
*----------------------------------------------------------------------*
FORM BATCH9 USING LT_WTAB LIKE GT_WTAB[].

  DATA: LT_PRINTAB   TYPE BOE_TR_LIST OCCURS 0 WITH HEADER LINE,
        LS_WTAB      TYPE BOE_TR_LIST,
        LS_BKPF      LIKE BKPF,
        LS_BSEG      LIKE BSEG, LS_BSED LIKE BSED,
        LS_DATA      TYPE BOE_GLOBAL_TURKEY,
        LS_T031      LIKE T031,                             "#EC NEEDED
        LF_UMSKZ     LIKE BSEG-UMSKZ,
        LF_DAT       LIKE SY-DATUM,
        LF_BELNR     LIKE BSEG-BELNR,
        LF_GJAHR     LIKE BSED-GJAHR,
        LF_HKONT     LIKE T045P-VKONT,
        LF_LINE(2)   TYPE C,
        LF_AKONT     LIKE KNB1-AKONT,
        LF_FLAG(1)   TYPE C,
        LF_MODE(1)   TYPE C,
        LF_MODE2(1)  TYPE C,
        LF_RESULT(1) TYPE C,
        LF_DUMMY     TYPE C,
        LF_WAERS     LIKE BKPF-WAERS,
        LF_SELZU     LIKE BSEG-ZUONR.

*==> Start of Note 2678366
  DATA: LF_KKBER LIKE T001-KKBER,
        LF_XKKBI LIKE T001-XKKBI.
*==> End of Note 2678366
* clear required global fields
* ----------------------------
  CLEAR: CHARWRBTR, CHARSGTXT, CHARZFBDT, CHARBUDAT, CHARBLDAT,
         CHARXPOS, CHAR_DAT, CHARWDATE, CHARREF,
         ABPOS1, ABPOS2, ABCOUNT, BORDRO, COUNT1, COUNT2, BLART1,
         BLART2, BSCHL1, ZZUONR,
         CHARDMBTR, CHARDMBE2, CHARDMBE3.

* prepare posting, document, system and due date for batch input
* --------------------------------------------------------------
  IF GF_DATFM = '2' OR GF_DATFM = '3'.
    CONCATENATE P_BLDAT+4(2) P_BLDAT+6(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+4(2) P_BUDAT+6(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
    CONCATENATE P_BLDAT+0(4) P_BLDAT+4(2) P_BLDAT+6(2) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+0(4) P_BUDAT+4(2) P_BUDAT+6(2) INTO CHARBUDAT.
  ELSE.
    CONCATENATE P_BLDAT+6(2) P_BLDAT+4(2) P_BLDAT+0(4) INTO CHARBLDAT.
    CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO CHARBUDAT.
  ENDIF.

* get line of radiobutton 'Allocation' on screen SAPMF05A 710
* ----------------------------------------------------------------
  PERFORM GET_LINE_SCREEN710 USING 'ZUONR' LF_LINE.

* get next free bordronumber and document type of posting
* -------------------------------------------------------
  PERFORM GET_BORDRONUMBER USING P_BUKRS BORDRO '10'.
  PERFORM GET_DOCUMENT_TYPE USING BLART1 'F-20'.
  PERFORM GET_DOCUMENT_TYPE USING BLART2 'F-30'.

* get posting key for special G/L debit posting
* ---------------------------------------------
  SELECT SINGLE BSDSS FROM T041A INTO BSCHL1
                                 WHERE AUGLV = 'UMBUCHNG'.
  IF ( SY-SUBRC NE 0 ) OR ( BSCHL1 EQ '  ' ).
    MESSAGE E150.
  ENDIF.

* refresh table that contains system messages
* -------------------------------------------
  REFRESH GT_SYMESS.

* loop at selected checks/BoE
* ---------------------------
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-BOX = 'X'.
      MESSAGE S185 WITH LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                        INTO LF_DUMMY.
      PERFORM ADD_LINE USING SY GT_SYMESS.

      PERFORM DEQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR LS_WTAB-BELNR.
* Begin of Note 1324719
* test: is Customer branch account blocked for posting
*--------------------------------
      IF LS_WTAB-FILKD NE SPACE.
        SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-FILKD.
        SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-FILKD AND
                                        BUKRS = P_BUKRS.
        IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
          MESSAGE E351(F5) WITH LS_WTAB-FILKD INTO LF_DUMMY.
          PERFORM ADD_LINE USING SY GT_SYMESS.

          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.

* test: is Customer account blocked for posting
*--------------------------------
      SELECT SINGLE * FROM KNA1 WHERE KUNNR = LS_WTAB-KUNNR.
      SELECT SINGLE * FROM KNB1 WHERE KUNNR = LS_WTAB-KUNNR AND
                                      BUKRS = P_BUKRS.
      IF SY-SUBRC NE 0 OR KNA1-SPERR EQ 'X' OR KNB1-SPERR EQ 'X'.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
* create error message
*---------------------
        MESSAGE E351(F5) WITH LS_WTAB-KUNNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* End Of Note 1324719
* determine SHKZG and document of portfolio: issues posting
* -----------------------------------------------
      SELECT SINGLE UMSKZ FROM BSEG INTO LF_UMSKZ
                                    WHERE BUKRS = P_BUKRS
                                      AND BELNR = LS_WTAB-BELNR
                                      AND BUZEI = LS_WTAB-BUZEI
                                      AND GJAHR = LS_WTAB-GJAHR.

      SELECT SINGLE * FROM *BSEG WHERE BUKRS = P_BUKRS
                                   AND BELNR = LS_WTAB-BELNR
                                   AND BUZEI = LS_WTAB-BUZEI
                                   AND GJAHR = LS_WTAB-GJAHR.
      LF_BELNR = *BSEG-DISBN.
      LF_GJAHR = *BSEG-DISBJ.

* determine allocation of "portfolio: issues" posting
* ----------------------------------------------------
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO LF_SELZU.
      SELECT SINGLE * FROM *BSEG WHERE BELNR = LF_BELNR
                                     AND BUKRS = P_BUKRS
                                     AND GJAHR = LF_GJAHR
                                     AND ZUONR = LF_SELZU
                                     AND SHKZG = 'S'.
      LF_HKONT = *BSEG-HKONT.

      IF LF_BELNR IS INITIAL OR LF_SELZU IS INITIAL.
* create error message
* --------------------
        MESSAGE E181 INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* copy data of current BoE of tables bkpf, bseg and bsed
* ------------------------------------------------------
      SELECT SINGLE * FROM *BKPF INTO LS_BKPF
                                 WHERE BUKRS = P_BUKRS
                                   AND BELNR = LS_WTAB-BELNR
                                   AND GJAHR = LS_WTAB-GJAHR.

      SELECT SINGLE * FROM *BSEG INTO LS_BSEG
                                 WHERE BUKRS = P_BUKRS
                                   AND BELNR = LS_WTAB-BELNR
                                   AND GJAHR = LS_WTAB-GJAHR
                                   AND BUZEI = LS_WTAB-BUZEI.
* determine business place for Thailand
* -------------------------------------
      IF GF_COUNTRY = 'TH'.
        SET PARAMETER ID 'BUPLA' FIELD LS_BSEG-BUPLA.
        SET PARAMETER ID 'ID_BUPLA' FIELD LS_BSEG-BUPLA.
        SET PARAMETER ID 'JEA' FIELD LS_BKPF-BRNCH.
      ENDIF.

      SELECT SINGLE * FROM *BSED INTO LS_BSED
                                 WHERE BUKRS = P_BUKRS
                                   AND BELNR = LS_WTAB-BELNR
                                   AND GJAHR = LS_WTAB-GJAHR
                                   AND BUZEI = LS_WTAB-BUZEI.
      IF LS_BKPF IS INITIAL OR LS_BSEG IS INITIAL OR
         LS_BSED IS INITIAL.
* create error message
* --------------------
        MESSAGE E184 WITH LS_WTAB-BELNR INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* get reconciliation account of customer
* --------------------------------------
      CLEAR LF_AKONT.
      PERFORM GET_CUSTOMER_RECONC_ACCOUNT USING LS_WTAB-BUKRS
                                                LS_WTAB-KUNNR
                                                LF_AKONT.
      IF LF_AKONT IS INITIAL.
* create error message
* --------------------
        MESSAGE E169 INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.
      ENDIF.

* check whether field bseg-xref1 is visible on dynpro
* ----------------------------------------------------
      CLEAR LF_FLAG.
      PERFORM CHECK_FIELD_ON_SCREEN USING LS_WTAB-BUKRS
                                          LF_AKONT
                                          BSCHL1
                                          'BSEG-XREF1'
                                          LF_FLAG.
      IF LF_FLAG NE 'X'.
* create error message
* --------------------
        IF LF_FLAG EQ '1'.
          MESSAGE E163 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '2'.
          MESSAGE E164 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '3'.
          MESSAGE E165 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '4'.
          MESSAGE E166 WITH BSCHL1 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '5'.
          MESSAGE E167 WITH 'BSEG-XREF1' INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '6'.
          MESSAGE E168 WITH 'BSEG-XREF1' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check whether field bseg-xref3 is visible on dynpro
* ----------------------------------------------------
      CLEAR LF_FLAG.
      PERFORM CHECK_FIELD_ON_SCREEN USING LS_WTAB-BUKRS
                                          LF_AKONT
                                          BSCHL1
                                          'BSEG-XREF3'
                                          LF_FLAG.
      IF LF_FLAG NE 'X'.
* create error message
* --------------------
        IF LF_FLAG EQ '1'.
          MESSAGE E163 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '2'.
          MESSAGE E164 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '3'.
          MESSAGE E165 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '4'.
          MESSAGE E166 WITH BSCHL1 INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '5'.
          MESSAGE E167 WITH 'BSEG-XREF3' INTO LF_DUMMY.
        ELSEIF LF_FLAG EQ '6'.
          MESSAGE E168 WITH 'BSEG-XREF3' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check table TBSLT for necessary entry of special G/L indicator
* --------------------------------------------------------------
      LF_MODE = '-'.
      PERFORM CHECK_TBSLT USING SY-LANGU BSCHL1 LF_UMSKZ LF_MODE.
      IF LF_MODE = 'X'.
* create error message
* --------------------
        MESSAGE E156 WITH LF_UMSKZ INTO LF_DUMMY.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check tables T074* for necessary entry of special G/L indicator
* ---------------------------------------------------------------
      PERFORM CHECK_T074* USING SY-LANGU LF_UMSKZ LF_RESULT.
      IF LF_RESULT = '1' OR LF_RESULT = '2' OR LF_RESULT = '3'.
* create error message
* --------------------
        IF LF_RESULT EQ '1'.
          MESSAGE E183 WITH LF_UMSKZ 'T074A' INTO LF_DUMMY.
        ELSEIF LF_RESULT EQ '2'.
          MESSAGE E183 WITH LF_UMSKZ 'T074T' INTO LF_DUMMY.
        ELSEIF LF_RESULT EQ '3'.
          MESSAGE E183 WITH LF_UMSKZ 'T074U' INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.
* check table T074 for necessary entry of special G/L indicator
* ---------------------------------------------------------------
      LF_MODE2 = '+'.
      PERFORM CHECK_T074 USING GS_T001-KTOPL LF_UMSKZ LS_WTAB-KUNNR
                               LS_WTAB-BUKRS LF_MODE2.
      IF LF_MODE2 = '1' OR LF_MODE2 = '2' OR LF_MODE2 = '3'.
* create error message
* --------------------
        IF LF_MODE2 EQ '1'.
          MESSAGE E162 WITH LS_WTAB-KUNNR INTO LF_DUMMY.
        ELSEIF LF_MODE2 EQ '2'.
          MESSAGE E160 WITH LF_UMSKZ INTO LF_DUMMY.
        ELSEIF LF_MODE2 EQ '3'.
          MESSAGE E161 WITH LF_UMSKZ INTO LF_DUMMY.
        ENDIF.
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* check lock of portfolio sub account
* -----------------------------------
      PERFORM CHECK_LOCKED_ACCOUNT  USING P_BUKRS
                                    LF_HKONT
                                    LF_DUMMY.
      IF NOT LF_DUMMY IS INITIAL.
* create error message
* --------------------
        PERFORM ADD_LINE USING SY GT_SYMESS.

        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ENDIF.

* set date for selection of BoE: sum of due date and protest period
* -----------------------------------------------------------------
      IF LS_WTAB-ZFBDT LE NULL.
        LF_DAT = P_BUDAT.
      ELSE.
        LF_DAT = LS_WTAB-ZFBDT + LS_WTAB-WELGF.
      ENDIF.
* read open line items in table bsix due till lf_dat
* -------------------------------------------------
      PERFORM READ_BSIX USING GT_BELDATA[] LS_WTAB P_BUKRS LF_DAT
                              LF_WAERS 'D'.
* get line number of current BoE in table with open line items
* ------------------------------------------------------------
      PERFORM GET_LINE USING GT_BELDATA[] LS_WTAB ABPOS1 ABPOS2.
* call transaction: reverse contingent liability of current BoE
* -------------------------------------------------------------
      REFRESH BDCTAB.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '102'.
      PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
      PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
      PERFORM BATCHFIELD USING 'BKPF-BLART' BLART1.
      PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
      PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
      CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                              INTO CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
      PERFORM BATCHFIELD USING 'BKPF-WAERS' LF_WAERS.
      IF GF_DATFM = '2' OR GF_DATFM = '3'.
        CONCATENATE LF_DAT+4(2) LF_DAT+6(2) LF_DAT+0(4)
                                        INTO CHAR_DAT.
      ELSEIF GF_DATFM = '4' OR GF_DATFM = '5' OR GF_DATFM = '6'.
        CONCATENATE LF_DAT+0(4) LF_DAT+4(2) LF_DAT+6(2)
                                        INTO CHAR_DAT.
      ELSE.
        CONCATENATE LF_DAT+6(2) LF_DAT+4(2) LF_DAT+0(4)
                                            INTO CHAR_DAT.
      ENDIF.
      PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHAR_DAT.
      PERFORM BATCHFIELD USING 'RF05A-AGKON' LS_WTAB-SAKNR.
      PERFORM BATCHFIELD USING 'RF05A-WVERW' 'D'.
      PERFORM BATCHFIELD USING 'RF05A-SEL01' LS_WTAB-BELNR.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PB'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSCOM(01)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '602'.
      PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-XPOS1(04)'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '=SO+'.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(01)' ' '.
      PERFORM BATCHFIELD USING 'RF05A-XPOS1(04)' 'X'.
      ABCOUNT = 1.
      WHILE ABCOUNT LE ABPOS1.
        IF NOT ( ABCOUNT = ABPOS2 ).
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'RF05A-ABPOS' ABCOUNT.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
          PERFORM BATCHFIELD USING 'BDC_CURSOR' 'RF05A-PSBET(01)'.
          PERFORM BATCHFIELD USING 'BDC_OKCODE' '=PI4'.
        ENDIF.
        ADD 1 TO ABCOUNT.
      ENDWHILE.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'RF05A-ABPOS' '1'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' '/00'.
      PERFORM BATCHDYNPRO USING 'SAPMF05A' '716'.
      PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
      CALL TRANSACTION 'F-20' USING BDCTAB MODE MODE UPDATE UPDATE
                                    MESSAGES INTO GT_SYMESS.
      IF SY-SUBRC NE 0.
        ADD 1 TO COUNT1.
        LS_WTAB-BOX = '2'.
        MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
        PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                              LS_WTAB-BELNR
                        CHANGING LF_DUMMY.
        IF NOT ( LF_DUMMY IS INITIAL ).
          PERFORM ADD_LINE USING SY GT_SYMESS.
        ENDIF.
        CONTINUE.
      ELSE.
* call transaction: new BoE in target portfolio with data of old BoE
* ------------------------------------------------------------------
        REFRESH BDCTAB.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '122'.
        PERFORM BATCHFIELD USING 'BKPF-BLDAT' CHARBLDAT.
        PERFORM BATCHFIELD USING 'BKPF-BUDAT' CHARBUDAT.
        PERFORM BATCHFIELD USING 'BKPF-BLART' BLART2.
        PERFORM BATCHFIELD USING 'BKPF-XBLNR' BORDRO.
        PERFORM BATCHFIELD USING 'BKPF-BKTXT' CHARREF.
        PERFORM BATCHFIELD USING 'BKPF-BUKRS' P_BUKRS.
        PERFORM BATCHFIELD USING 'BKPF-WAERS' P_WAERS.
        PERFORM BATCHFIELD USING 'RF05A-NEWBS' BSCHL1.
* Begin of Note 1324719
        IF LS_WTAB-FILKD NE SPACE.
          PERFORM BATCHFIELD USING 'RF05A-NEWKO' LS_WTAB-FILKD.
        ELSE.
          PERFORM BATCHFIELD USING 'RF05A-NEWKO' LS_WTAB-KUNNR.
        ENDIF.

* End Of Note 1324719
        PERFORM BATCHFIELD USING 'BKPF-BVORG' LS_BKPF-BVORG.
        PERFORM BATCHFIELD USING 'RF05A-PORTF' P_PORT2.
        PERFORM BATCHFIELD USING 'RF05A-NEWUM' LF_UMSKZ.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ENTR'.
*==> Start of Note 2784912
*        PERFORM batchdynpro USING 'SAPMF05A' '320'.

        IF T001-BUVAR = '2'.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '2320'.
        ELSE.
          PERFORM BATCHDYNPRO USING 'SAPMF05A' '320'.
        ENDIF.
*==> End of Note 2784912
*        WRITE ls_wtab-wrbtr TO charwrbtr CURRENCY ls_wtab-waers. "AFLE Change
        PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-WRBTR LS_WTAB-WAERS
                                     CHANGING CHARWRBTR.
        PERFORM BATCHFIELD USING 'BSEG-WRBTR' CHARWRBTR.
        IF LS_WTAB-WAERS NE LS_WTAB-HWAER.
*          WRITE ls_wtab-dmbtr TO chardmbtr CURRENCY ls_wtab-hwaer. "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-DMBTR LS_WTAB-HWAER
                                       CHANGING CHARDMBTR.
          PERFORM BATCHFIELD USING 'BSEG-DMBTR' CHARDMBTR.
        ENDIF.
        CONCATENATE LS_WTAB-BELNR LS_WTAB-BUZEI LS_WTAB-GJAHR
                                                INTO ZZUONR.
        PERFORM BATCHFIELD USING 'BSEG-ZUONR' ZZUONR.
        IF LS_WTAB-BANK IS INITIAL.
          CHARSGTXT = LS_WTAB-BOENO.
        ELSE.
          CONCATENATE LS_WTAB-BOENO '/' LS_WTAB-BANK INTO CHARSGTXT.
        ENDIF.
        PERFORM BATCHFIELD USING 'BSEG-SGTXT' CHARSGTXT.
        WRITE LS_WTAB-ZFBDT TO CHARZFBDT.
        PERFORM BATCHFIELD USING 'BSEG-ZFBDT' CHARZFBDT.
        IF NOT ( LS_BSED-WDATE IS INITIAL ).
          WRITE LS_BSED-WDATE TO CHARWDATE.
          PERFORM BATCHFIELD USING 'BSED-WDATE' CHARWDATE.
        ENDIF.
        IF NOT LS_WTAB-GSBER IS INITIAL.
          PERFORM BATCHFIELD USING 'BSEG-GSBER' LS_WTAB-GSBER.
        ENDIF.
        PERFORM BATCHFIELD USING 'BSED-XSIWE' LS_BSED-XSIWE.
        PERFORM BATCHFIELD USING 'BSED-WNAME' LS_BSED-WNAME.
        PERFORM BATCHFIELD USING 'BSED-WORT1' LS_BSED-WORT1.
        PERFORM BATCHFIELD USING 'BSED-XAKTZ' LS_BSED-XAKTZ.
        PERFORM BATCHFIELD USING 'BSED-WSTAT' LS_BSED-WSTAT.
        PERFORM BATCHFIELD USING 'BSED-WGBKZ' ' '.
* check whether fields for charges are displayed
* ----------------------------------------------
        SELECT SINGLE * FROM T031 INTO LS_T031
                                       WHERE BUKRS = P_BUKRS.
        IF SY-SUBRC EQ 0.
          PERFORM BATCHFIELD USING 'BSED-WINFW' ' '.
          PERFORM BATCHFIELD USING 'BSED-DISKP' ' '.
          PERFORM BATCHFIELD USING 'BSED-DISKT' ' '.
          PERFORM BATCHFIELD USING 'BSED-WMWKZ' ' '.
          PERFORM BATCHFIELD USING 'BSED-WSTKZ' ' '.
        ENDIF.
        PERFORM BATCHFIELD USING 'BSED-WEVWV' LS_BSED-WEVWV.
        PERFORM BATCHFIELD USING 'BSED-REGIO' LS_BSED-REGIO.
        PERFORM BATCHFIELD USING 'BSED-WBZOG' LS_BSED-WBZOG.
        PERFORM BATCHFIELD USING 'BSED-WORT2' LS_BSED-WORT2.
        IF T001-BUVAR <> '2'.    " Add in Note 2784912
          PERFORM BATCHFIELD USING 'BSED-WBANK' LS_BSED-WBANK.
          PERFORM BATCHFIELD USING 'BSED-WLZBP' LS_BSED-WLZBP.
        ENDIF.                   " Add in Note 2784912
        PERFORM BATCHFIELD USING 'BSED-BANK' LS_WTAB-BANK.
        PERFORM BATCHFIELD USING 'BSED-ACCOU' LS_WTAB-ACCOU.
        PERFORM BATCHFIELD USING 'BSED-BOENO' LS_WTAB-BOENO.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'ZK'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '331'.
        PERFORM BATCHFIELD USING 'BSEG-XREF1' LS_BSEG-XREF1.
        PERFORM BATCHFIELD USING 'BSEG-XREF2' LS_BSEG-XREF2.
        PERFORM BATCHFIELD USING 'BSEG-XREF3' LS_BSEG-XREF3.
*==> Start of Note 2678366
* check whether credit control area in the document is not empty
        IF LS_WTAB-KKBER NE SPACE.
* check whether credit control area in the document is different
* from default credit control area
          SELECT SINGLE KKBER XKKBI FROM T001
            INTO (LF_KKBER,LF_XKKBI)
           WHERE BUKRS = P_BUKRS.
          IF LS_WTAB-KKBER NE LF_KKBER.
* check whether credit control area in the document is
            IF LF_XKKBI = 'X'.
              PERFORM BATCHFIELD USING 'BSEG-KKBER' LS_WTAB-KKBER.
            ENDIF.
          ENDIF.
        ENDIF.
*==> End of Note 2678366
        IF NOT ( LS_WTAB-DMBE2 IS INITIAL ) AND
                 LS_WTAB-WAERS NE LS_WTAB-HWAE2 AND
                 LS_WTAB-HWAER NE LS_WTAB-HWAE2.
*          WRITE ls_wtab-dmbe2 TO chardmbe2 CURRENCY ls_wtab-hwae2.  "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-DMBE2 LS_WTAB-HWAE2
                                       CHANGING CHARDMBE2.
          PERFORM BATCHFIELD USING 'BSEG-DMBE2' CHARDMBE2.
        ENDIF.
        IF NOT ( LS_WTAB-DMBE3 IS INITIAL ) AND
                 LS_WTAB-WAERS NE LS_WTAB-HWAE3 AND
                 LS_WTAB-HWAER NE LS_WTAB-HWAE3.
*          WRITE ls_wtab-dmbe3 TO chardmbe3 CURRENCY ls_wtab-hwae3.  "AFLE Change
          PERFORM WRITE_AMT_TO_CHARAMT USING LS_WTAB-DMBE3 LS_WTAB-HWAE3
                                       CHANGING CHARDMBE3.
          PERFORM BATCHFIELD USING 'BSEG-DMBE3' CHARDMBE3.
        ENDIF.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'SL'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '710'.
        PERFORM BATCHFIELD USING 'RF05A-AGBUK' P_BUKRS.
        PERFORM BATCHFIELD USING 'RF05A-AGKON' LF_HKONT.
        PERFORM BATCHFIELD USING 'RF05A-AGKOA' 'S'.
        CONCATENATE 'RF05A-XPOS1(' LF_LINE ')' INTO CHARXPOS.
        PERFORM BATCHFIELD USING CHARXPOS 'X'.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'PA'.
        PERFORM BATCHDYNPRO USING 'SAPMF05A' '731'.
        PERFORM BATCHFIELD USING 'RF05A-SEL01(01)' LF_SELZU.
        PERFORM BATCHFIELD USING 'BDC_OKCODE' 'BU'.
        CALL TRANSACTION 'F-30' USING BDCTAB MODE MODE UPDATE
                                UPDATE MESSAGES INTO GT_SYMESS.
        IF SY-SUBRC NE 0.
          ADD 1 TO COUNT1.
          LS_WTAB-BOX = '2'.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ELSE.
* mark processed checks/BoE: necessary for SAPScript print
* --------------------------------------------------------
          ADD 1 TO COUNT2.
          LS_WTAB-BOX = '1'.
          LS_WTAB-CHEC = '1'.
          LS_WTAB-XBLNR = BORDRO.
          LS_WTAB-PORTF1 = P_PORT4.
          LS_WTAB-PORTF2 = P_PORT2.
          MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING BOX CHEC XBLNR
                                      PORTF1 PORTF2.
          PERFORM ENQUEUE USING LS_WTAB-BUKRS LS_WTAB-GJAHR
                                LS_WTAB-BELNR
                          CHANGING LF_DUMMY.
          IF NOT ( LF_DUMMY IS INITIAL ).
            PERFORM ADD_LINE USING SY GT_SYMESS.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* set old business place for Thailand
* -----------------------------------
  IF GF_COUNTRY = 'TH'.
    SET PARAMETER ID 'BUPLA' FIELD GF_BUPLA.
    SET PARAMETER ID 'ID_BUPLA' FIELD GF_BUPLA_B.
    SET PARAMETER ID 'JEA' FIELD GF_BRNCH.
  ENDIF.

* get system messages
* -------------------
  PERFORM MSG_ADD USING GT_SYMESS G_PROBCLASS_HIGH.

* determine successful and failed postings
* ----------------------------------------
  IF COUNT1 NE 0.
    IF COUNT2 NE 0.
      MESSAGE S118 WITH COUNT2 COUNT1.                      "#EC *
    ELSE.
      MESSAGE S119.
    ENDIF.
  ENDIF.
  IF COUNT1 EQ 0.
    IF COUNT2 NE 0.
      MESSAGE S120 WITH COUNT2.
    ENDIF.
  ENDIF.

* at least one BoE successfully posted?
* clear status of SAPScript print for next postings
* -------------------------------------
  CLEAR TEST.
  REFRESH LT_PRINTAB.
  LOOP AT LT_WTAB INTO LS_WTAB.
    IF LS_WTAB-CHEC = '1'.
      TEST = '1'.
      APPEND LS_WTAB TO LT_PRINTAB.
      CLEAR LS_WTAB-CHEC.
      MODIFY LT_WTAB FROM LS_WTAB TRANSPORTING CHEC.
    ENDIF.
  ENDLOOP.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660

  LOOP AT LT_PRINTAB.
    MOVE-CORRESPONDING LT_PRINTAB TO OUTPUT_ITEM_TABLE.
    APPEND OUTPUT_ITEM_TABLE.
  ENDLOOP.
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

* SAPScript print
* ---------------
  IF TEST = '1'.
    LS_DATA-TRANSACTION = TEXT-010.
    LS_DATA-COMPANYCODE = P_BUKRS.
    LS_DATA-USER = SY-UNAME.
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    IF PSCRIPT = 'X'  .
***********end of PDF conversion ,Date: 26/06/2008 ,C5112660

      CALL FUNCTION 'PRINT_BOE_LIST_TR'
        EXPORTING
          BOE_GLOBAL = LS_DATA
          REPRINT    = ' '
        TABLES
          BOE_INTERN = LT_PRINTAB[].
***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
    ELSE.
      S_SINGLE = 'X'.
      BOE_GLOBAL-COMPANYCODE = GS_T001-BUKRS.
      BOE_GLOBAL-COMPANY_TEXT = GS_T001-BUTXT.
      MOVE LS_DATA-TRANSACTION TO OUTPUT_HEADER_TABLE-TRANS.
      MOVE LS_DATA-COMPANYCODE TO OUTPUT_HEADER_TABLE-BUKRS.
      APPEND OUTPUT_HEADER_TABLE.
**move ls_data-user into output_header_table-tran.
      OUTPUT_HEADER_TABLE-STATUS = '@0V@'.
      OUTPUT_HEADER_TABLE-EXPAND = S_SINGLE.
      BOE_GLOBAL-USER = SY-UNAME.

      PERFORM OPEN_FORM_PDF.

      PERFORM ITEM_DATA_PDF.

    ENDIF.

*ENDFORM.                    " form reprint

***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660

  ENDIF.

ENDFORM.                                                    " BATCH9

*&---------------------------------------------------------------------*
*&      Form  BATCH_CUSTOMER
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'FD03': Show master data of customer
*----------------------------------------------------------------------*
*       --> lf_kunnr    customer name
*       --> lf_bukrs    company code
*----------------------------------------------------------------------*
FORM BATCH_CUSTOMER USING LF_KUNNR LIKE BSEG-KUNNR
                          LF_BUKRS LIKE BSEG-BUKRS.

  REFRESH BDCTAB.
  PERFORM BATCHDYNPRO USING 'SAPMF02D' '106'.
  PERFORM BATCHFIELD USING 'RF02D-KUNNR' LF_KUNNR.
  PERFORM BATCHFIELD USING 'RF02D-BUKRS' LF_BUKRS.
  CALL TRANSACTION 'FD03' USING BDCTAB MODE 'E'.

ENDFORM.                    " BATCH_CUSTOMER

*&---------------------------------------------------------------------*
*&      Form  BATCH_VENDOR
*&---------------------------------------------------------------------*
*       Batch Input table for transaction
*       'FK03': Show master data of vendor
*----------------------------------------------------------------------*
*       --> lf_vendr    vendor name
*       --> lf_bukrs    company code
*----------------------------------------------------------------------*
FORM BATCH_VENDOR USING LF_VENDR LIKE BSED-VENDR
                        LF_BUKRS LIKE BSEG-BUKRS.

  REFRESH BDCTAB.
  PERFORM BATCHDYNPRO USING 'SAPMF02K' '106'.
  PERFORM BATCHFIELD USING 'RF02K-LIFNR' LF_VENDR.
  PERFORM BATCHFIELD USING 'RF02K-BUKRS' LF_BUKRS.
  CALL TRANSACTION 'FK03' USING BDCTAB MODE 'E'.

ENDFORM.                    " BATCH_VENDOR

***********Start of PDF conversion ,Date: 26/06/2008 ,C5112660
**    ENDLOOP.



*&---------------------------------------------------------------------*
*&      Form  OPEN_FORM_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_FORM_PDF .
*Start of disable printout Cloud CE1711,,
*SELECT SINGLE FORNR_PDF FROM T001F INTO gv_fpname WHERE BUKRS = p_bukrs
*                                                  AND PROGRAMM = sy-repid. "2226143 "#EC CI_NOFIRST
**  MOVE 'F110_TR_BOE_BORD' TO gv_fpname.        "2226143
*
**To retreive the function module name
*  TRY.
*      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
*        EXPORTING
*          i_name     = gv_fpname
*        IMPORTING
*          e_funcname = gv_fm_name.
*
*    CATCH cx_root INTO gv_w_cx_root.
*      gv_mesg = gv_w_cx_root->get_text( ).
*      WRITE:/  gv_mesg.
*
*  ENDTRY.
*
**To open the spool for printing
*  CALL FUNCTION 'FP_JOB_OPEN'
*    CHANGING
*      ie_outputparams = gs_fp_outputparams
*    EXCEPTIONS
*      cancel          = 1
*      usage_error     = 2
*      system_error    = 3
*      internal_error  = 4
*      OTHERS          = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*
*  ENDIF.
*End of disable printout Cloud CE1711,
****
  GV_FOUND = 'X' .

ENDFORM.                    " open_form_pdf
*&---------------------------------------------------------------------*
*&      Form  ITEM_DATA_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ITEM_DATA_PDF .
  DATA: LS_PDF_DATA_ITEM TYPE BOE_S_FORM_ITEM, "add in New PDF 247924(2018)
        LV_PORTF_NAME    TYPE T045K-NAME,      "add in New PDF 247924(2018)
        LV_LIFNR         TYPE LIFNR.           "add in New PDF 247924(2018)

*==> Start of New Output Management for OP & Cloud 247924(2018)
* unlock the PDF PRINT for country specific
*  IF  gf_country <> 'PH'
*  AND gf_country <> 'TR'.
*     EXIT.
*   ENDIF.

  REFRESH GT_PDF_DATA_ITEM.
  CLEAR GS_PDF_DATA_HEADER.
*==> End of 247924(2018)

  LOOP AT OUTPUT_HEADER_TABLE INTO WA_HEADER_TABLE.

    HELP_TABIX = SY-TABIX.

    REFRESH BOE_INTERN.

    LOOP AT OUTPUT_ITEM_TABLE INTO WA_ITEM_TABLE.

*     create entry
      MOVE-CORRESPONDING WA_ITEM_TABLE TO WA_BOE_TR_LIST.
      APPEND WA_BOE_TR_LIST TO BOE_INTERN.
    ENDLOOP.

    LOOP AT BOE_INTERN INTO BOE_INTERN.
      MOVE : BOE_INTERN-XBLNR        TO  GS_DATA-XBLNR,
             BOE_INTERN-BELNR        TO  GS_DATA-BELNR,
             BOE_INTERN-BUZEI        TO  GS_DATA-BUZEI,
             BOE_INTERN-ZFBDT        TO  GS_DATA-ZFBDT,
             BOE_INTERN-ACCOU        TO  GS_DATA-ACCOU,
             BOE_INTERN-BANK         TO  GS_DATA-BANK,
             BOE_INTERN-WRBTR        TO  GS_DATA-WRBTR,
             BOE_INTERN-WAERS        TO  GS_DATA-WAERS,
             BOE_GLOBAL-COMPANYCODE  TO  GS_DATA-COMPANYCODE,
             BOE_GLOBAL-COMPANY_TEXT TO  GS_DATA-COMPANY_TEXT,
             WA_HEADER_TABLE-TRANS   TO  GS_DATA-TRANSACTION,
             BOE_GLOBAL-USER         TO  GS_DATA-ZUSER.

      APPEND GS_DATA TO GT_DATA.
      CLEAR GS_DATA.
*==> Start of New Output Management for OP & Cloud 247924(2018)
      CLEAR LS_PDF_DATA_ITEM.
      MOVE : BOE_INTERN-XBLNR        TO  LS_PDF_DATA_ITEM-XBLNR,
             BOE_INTERN-BELNR        TO  LS_PDF_DATA_ITEM-BELNR,
             BOE_INTERN-BUZEI        TO  LS_PDF_DATA_ITEM-BUZEI,
             BOE_INTERN-ZFBDT        TO  LS_PDF_DATA_ITEM-ZFBDT,
             BOE_INTERN-ACCOU        TO  LS_PDF_DATA_ITEM-ACCOU,
             BOE_INTERN-BANK         TO  LS_PDF_DATA_ITEM-BANK,
             BOE_INTERN-WRBTR        TO  LS_PDF_DATA_ITEM-WRBTR,
             BOE_INTERN-WAERS        TO  LS_PDF_DATA_ITEM-WAERS,
             BOE_INTERN-BOENO        TO  LS_PDF_DATA_ITEM-BOENO,
             BOE_INTERN-XBLNR        TO  GS_PDF_DATA_HEADER-XBLNR,
             WA_HEADER_TABLE-TRANS   TO  LS_PDF_DATA_ITEM-TRANSACTION.
*             boe_global-user         TO  ls_pdf_item_data-zuser.
      IF BOE_INTERN-KUNNR IS NOT INITIAL.

        SELECT SINGLE NAME1
          INTO LS_PDF_DATA_ITEM-CUST_NAME
          FROM KNA1
         WHERE KUNNR = BOE_INTERN-KUNNR.
      ENDIF.

      IF GF_LIFNA IS NOT INITIAL
     AND BOE_INTERN-VENDR IS INITIAL.
        BOE_INTERN-VENDR = GF_LIFNA.
      ENDIF.

      IF BOE_INTERN-VENDR IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = BOE_INTERN-VENDR
          IMPORTING
            OUTPUT = LV_LIFNR.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LV_LIFNR
          IMPORTING
            OUTPUT = LV_LIFNR.
        SELECT SINGLE NAME1
          INTO LS_PDF_DATA_ITEM-VEND_NAME
          FROM LFA1
         WHERE LIFNR = LV_LIFNR.
      ENDIF.
      APPEND LS_PDF_DATA_ITEM TO GT_PDF_DATA_ITEM.
      CLEAR LS_PDF_DATA_ITEM.
*==> End of 247924(2018)
    ENDLOOP.


* calculate average due date
    LOOP AT BOE_INTERN INTO BOE_TR_LIST.
      IF WAERS IS INITIAL OR BUDAT IS INITIAL.
        IF WAERS IS INITIAL.
          WAERS = BOE_TR_LIST-WAERS.
        ENDIF.
        IF BUDAT IS INITIAL.
          BUDAT = BOE_TR_LIST-BUDAT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

* currency: get number of digits after comma
* ------------------------------------------
    SELECT SINGLE * FROM TCURX WHERE CURRKEY = WAERS.

    IF SY-SUBRC NE 0.
      CURR = 2.
    ELSE.
      CURR = TCURX-CURRDEC.
    ENDIF.

    LOOP AT BOE_INTERN INTO BOE_TR_LIST WHERE CHEC EQ '1'.
*      MOVE boe_tr_list-wrbtr TO amount.                         "AFLE Change Takt 2 by C5201065
      PERFORM ASSIGN_WRBTR_TO_AMT USING BOE_TR_LIST-WRBTR
                                  CHANGING AMOUNT.
      LF_WRBTR = LF_WRBTR + BOE_TR_LIST-WRBTR.
      DAYS = BOE_TR_LIST-ZFBDT - BOE_TR_LIST-BUDAT.
      PRODUCT = AMOUNT * ( 10 ** ( 2 - CURR ) ) * DAYS.
      SUM = SUM + PRODUCT.
      TOTAL = TOTAL + ( AMOUNT * ( 10 ** ( 2 - CURR ) ) ).
    ENDLOOP.

    CLEAR: AVERAGE.
    IF NOT ( TOTAL IS INITIAL ).

* compute average due date of BoE
* -------------------------------
*==> Start of Note 2871853
*     fix the issue for average due date calculation
*      prelim = sum / total.
*      resdays =  prelim.
*      average = budat + resdays.
      AVERAGE = BUDAT + TRUNC( SUM DIV TOTAL ).
*==> End of Note 2871853
    ENDIF.

    BOE_TR_SUMS-SUMME = 0.      " added in Note 2915790
    BOE_TR_SUMS-ANZAHL_BOE = 0. " added in Note 2915790
    LOOP AT BOE_INTERN INTO BOE_TR_LIST.

**   only valid table entries will be printed
      CHECK BOE_TR_LIST-CHEC = '1'.

*   if the BORDRO number hasn´t been updated within the current
*   document, it will be set automatically.
      IF BOE_TR_LIST-XBLNR IS INITIAL.
        IF BOE_GLOBAL_TURKEY-BORDRO IS INITIAL.
          MESSAGE E489(ICC_TR) RAISING ERROR.
        ELSE.
          BOE_TR_LIST-XBLNR = BOE_GLOBAL_TURKEY-BORDRO.
        ENDIF.
      ENDIF.

*      boe_tr_sums-summe = 0.      " comment in Note 2915790
*      boe_tr_sums-anzahl_boe = 0. " comment in Note 2915790

      BOE_TR_SUMS-SUMME = BOE_TR_SUMS-SUMME + BOE_TR_LIST-WRBTR.
      BOE_TR_SUMS-ANZAHL_BOE = BOE_TR_SUMS-ANZAHL_BOE + 1.
    ENDLOOP.

    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        AMOUNT    = BOE_TR_SUMS-SUMME
        CURRENCY  = BOE_TR_LIST-WAERS
*       FILLER    = ' '
        LANGUAGE  = SY-LANGU
      IMPORTING
        IN_WORDS  = IN_WORDS
      EXCEPTIONS
        NOT_FOUND = 1
        TOO_LARGE = 2
        OTHERS    = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    GS_FOOTER-IN_WORTEN    = IN_WORDS-WORD.
    BOE_TR_SUMS-IN_DECWORD = IN_WORDS-DECWORD.

    MOVE : GT_DATA                TO  GS_FOOTER-T_TABLE,
           BOE_TR_LIST-WAERS      TO  GS_FOOTER-ZWAERS,
           AVERAGE                TO  GS_FOOTER-AVERAGE,
           BOE_TR_SUMS-SUMME      TO  GS_FOOTER-SUMME,
           BOE_TR_SUMS-ANZAHL_BOE TO  GS_FOOTER-ANZAHL_BOE,
           IN_WORDS-WORD          TO  GS_FOOTER-IN_WORTEN,
           WA_HEADER_TABLE-XBLNR  TO  GS_FOOTER-XBLNR.
*==> Start of New Output Management for OP & Cloud 247924(2018)
    GV_FOUND = ABAP_TRUE.
    CLEAR GS_PDF_DATA_HEADER.
    GS_PDF_DATA_HEADER-ANZAHL_BOE   = GS_FOOTER-ANZAHL_BOE.
    GS_PDF_DATA_HEADER-AVERAGE      = GS_FOOTER-AVERAGE.
    GS_PDF_DATA_HEADER-COMPANY_TEXT = BOE_GLOBAL-COMPANY_TEXT.
    GS_PDF_DATA_HEADER-COMPANYCODE  = BOE_GLOBAL-COMPANYCODE.
    GS_PDF_DATA_HEADER-IN_WORTEN    = GS_FOOTER-IN_WORTEN.
    GS_PDF_DATA_HEADER-PORTFO       = BOE_INTERN-PORTF1.
    GS_PDF_DATA_HEADER-SUMME        = GS_FOOTER-SUMME.
*    gs_pdf_data_header-xblnr        = gs_footer-xblnr.
    GS_PDF_DATA_HEADER-ZUSER        = BOE_GLOBAL-USER.
    GS_PDF_DATA_HEADER-ZWAERS       = GS_FOOTER-ZWAERS.

    IF BOE_INTERN-PORTF1 IS NOT INITIAL.
      CLEAR LV_PORTF_NAME.
      SELECT SINGLE NAME
        INTO LV_PORTF_NAME
        FROM T045K
       WHERE LANGU = SY-LANGU
         AND BUKRS = BOE_GLOBAL-COMPANYCODE
         AND PORTFO = BOE_INTERN-PORTF1.

*    CONCATENATE boe_intern-portf1
*                ' - '
*                lv_portf_name
*            INTO gs_pdf_data_header-portfo.
      CONCATENATE BOE_INTERN-PORTF1
                  '-'
                  LV_PORTF_NAME
              INTO GS_PDF_DATA_HEADER-PORTFO
              SEPARATED BY SPACE.
    ENDIF.
*==> End of 247924(2018)
    APPEND GS_FOOTER TO GT_FOOTER.
    CLEAR GS_FOOTER.

    REFRESH GT_DATA.
****
    IF NOT GV_FOUND IS INITIAL.
      PERFORM PRINT_PDF.
      PERFORM CLOSE_FORM_PDF.
      EXIT.
    ENDIF.
  ENDLOOP.
****
ENDFORM.                    " ITEM_DATA_PDF
*&---------------------------------------------------------------------*
*&      Form  PRINT_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_PDF.
*Start of Comment Commented for Cloud CE1711,
*CALL FUNCTION    gv_fm_name     "'/1BCDWB/SM00002121'
*  EXPORTING
*   /1BCDWB/DOCPARAMS               = gs_fp_docparams
*    rftr_boe_reprint_st2_pdf       = gt_footer
*
** IMPORTING
**   /1BCDWB/FORMOUTPUT             =
*  EXCEPTIONS
*    USAGE_ERROR                    = 1
*    SYSTEM_ERROR                   = 2
*    INTERNAL_ERROR                 = 3
*    OTHERS                         = 4
*          .
*IF sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*End of Comment Commented for Cloud CE1711,

*  CALL FUNCTION gv_fm_name
*
*    EXPORTING
*      /1bcdwb/docparams         = gs_fp_docparams
*      f110_tr_boe_bord_st       = gt_footer
*
** IMPORTING
**   /1BCDWB/FORMOUTPUT        =
*    EXCEPTIONS
*      usage_error               = 1
*      system_error              = 2
*      internal_error            = 3
*      OTHERS                    = 4
*            .
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

*==> Start of New Output Management for OP & Cloud 247924(2018)
  DATA:
    LS_BOE_OUTPUT_ID TYPE BOE_S_OUTPUT_ID.

  CL_FI_BOE_OUTPUT_INST=>GET_INSTANCE( )->SET_HEADER( GS_PDF_DATA_HEADER ).

  CL_FI_BOE_OUTPUT_INST=>GET_INSTANCE( )->SET_ITEM( GT_PDF_DATA_ITEM ).

  LS_BOE_OUTPUT_ID-BUKRS = BOE_GLOBAL-COMPANYCODE.
  LS_BOE_OUTPUT_ID-XBLNR = GS_PDF_DATA_HEADER-XBLNR.

  CALL METHOD CL_FI_BOE_OUTPUT_MGR=>CREATE_OUTPUT_REQUEST
    EXPORTING
      IS_BOE      = LS_BOE_OUTPUT_ID
    IMPORTING
      EV_OBJECTID = DATA(LV_OBJECTID)
    RECEIVING
      RV_MESSAGE  = DATA(LV_MESSAGE).

  CALL METHOD CL_FI_BOE_OUTPUT_MGR=>CALL_OUTPUT_CONTROL_UI
    EXPORTING
      IV_OBJECT_ID = LV_OBJECTID.
*==> End of 247924(2018)
*==> Start of Note 3217901
  IF NOT CL_COS_UTILITIES=>IS_CLOUD( ).
    CALL METHOD CL_FI_BOE_OUTPUT_MGR=>REMOVE_OUTPUT_REQUEST
      EXPORTING
        IV_OBJECTID = LV_OBJECTID.
  ENDIF.
*==> End of Note 3217901
ENDFORM.                    " PRINT_PDF
*&---------------------------------------------------------------------*
*&      Form  CLOSE_FORM_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_FORM_PDF .
*Start of Comment Commented for Cloud CE1711
*  CALL FUNCTION 'FP_JOB_CLOSE'
** IMPORTING
**   E_RESULT             =
*   EXCEPTIONS
*     usage_error          = 1
*     system_error         = 2
*     internal_error       = 3
*     OTHERS               = 4
*            .
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*End of Comment Commented for Cloud CE1711,
ENDFORM.                    " CLOSE_FORM_PDF


***********End  of PDF conversion ,Date: 26/06/2008 ,C5112660

INCLUDE RFIDTRBOE1_AFLE IF FOUND.
