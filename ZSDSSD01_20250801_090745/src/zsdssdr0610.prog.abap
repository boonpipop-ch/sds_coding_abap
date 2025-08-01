*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0610
*  Creation Date      : 02.06.2025
*  Author             : Jakarin S.
*  Add-on ID          :
*  Description        : Program Print Credit Note
*  Purpose            :
*  Copied from        :
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
REPORT ZSDSSDR0610.
*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE RVADTABL  ##INCL_OK.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_ADRC,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,
         DATE_FROM  TYPE ADRC-DATE_FROM,
         NATION     TYPE ADRC-NATION,
         NAME1      TYPE ADRC-NAME1,
         NAME2      TYPE ADRC-NAME2,
         NAME3      TYPE ADRC-NAME3,
         NAME4      TYPE ADRC-NAME4,
         CITY1      TYPE ADRC-CITY1,
         CITY2      TYPE ADRC-CITY2,
         POST_CODE1 TYPE ADRC-POST_CODE1,
         STREET     TYPE ADRC-STREET,
         STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
         STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
         LOCATION   TYPE ADRC-LOCATION,
         COUNTRY    TYPE ADRC-COUNTRY,
         LANGU      TYPE ADRC-LANGU,
         TEL_NUMBER TYPE ADRC-TEL_NUMBER,
         FAX_NUMBER TYPE ADRC-FAX_NUMBER,
         TAXJURCODE TYPE ADRC-TAXJURCODE,
         LANDX      TYPE T005T-LANDX,
       END OF TS_ADRC.

TYPES: BEGIN OF TS_BRANCH,
         BUKRS      TYPE J_1BBRANCH-BUKRS,
         BRANCH     TYPE J_1BBRANCH-BRANCH,
         BUPLA_TYPE TYPE J_1BBRANCH-BUPLA_TYPE,
         ADRNR      TYPE J_1BBRANCH-ADRNR,
       END OF TS_BRANCH.

TYPES: BEGIN OF TS_KNA1,
         KUNNR TYPE KNA1-KUNNR,
         STCD3 TYPE KNA1-STCD3,
       END OF TS_KNA1.

TYPES: BEGIN OF TS_VBPA3,
         VBELN TYPE VBPA3-VBELN,
         STCD3 TYPE VBPA3-STCD3,
       END OF TS_VBPA3.

TYPES: BEGIN OF TS_VBRP,
         VBELN TYPE VBRP-VBELN,
         POSNR TYPE VBRP-POSNR,
         UEPOS TYPE VBRP-UEPOS,
         FKIMG TYPE VBRP-FKIMG,
         VRKME TYPE VBRP-VRKME,
         NETWR TYPE VBRP-NETWR,
         MATNR TYPE VBRP-MATNR,
         ARKTX TYPE VBRP-ARKTX,
         KZWI1 TYPE VBRP-KZWI1,
         KZWI3 TYPE VBRP-KZWI3,
         KZWI4 TYPE VBRP-KZWI4,
         KZWI5 TYPE VBRP-KZWI5,
         MWSBP TYPE VBRP-MWSBP,
       END OF TS_VBRP.

TYPES: TT_ADRC    TYPE STANDARD TABLE OF TS_ADRC.
TYPES: TT_BRANCHT TYPE STANDARD TABLE OF J_1BBRANCHT.
TYPES: TT_VBRP    TYPE STANDARD TABLE OF TS_VBRP.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_REPID               TYPE SY-REPID           VALUE 'ZSDSSDR0100',
  GC_INTFNO              TYPE ZSDSCAC004-INTFNO  VALUE 'SDF005',
  GC_OUTPUT_DEVICE_OTF   TYPE SSFCOMPOP-TDDEST   VALUE 'LPDF',
  GC_GTOTAL_TEXT_MAX_LEN TYPE I                  VALUE 65.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA: GF_XSCREEN(1)     TYPE C  ##NEEDED. "Output on printer or screen
DATA: GF_SUECHARG_DESC  TYPE ZSDSCAC001-VDESC ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA: GRT_CN_DOCTYPE                TYPE RANGE OF VBRK-FKART  ##NEEDED.
DATA: GRT_CR_DOCTYPE                TYPE RANGE OF VBRK-FKART  ##NEEDED. "<<F36K910460++
DATA: GRT_CANCEL_SERVICE_DOCTYPE    TYPE RANGE OF VBRK-FKART  ##NEEDED.
DATA: GRT_CUST_NOT_SHOW_BRANCH      TYPE RANGE OF KNA1-KUNNR  ##NEEDED.
DATA: GRT_ONETIME_GET_TAXID         TYPE RANGE OF KNA1-KUNNR  ##NEEDED.
DATA: GRT_SUECHARG                  TYPE RANGE OF PRCD_ELEMENTS-KSCHL ##NEEDED.
DATA: GRT_MAT_NOT_SHOW_DESC         TYPE RANGE OF VBRP-MATNR  ##NEEDED.
DATA: GRT_DIST_CHAN_DEFAULT_BRANCH  TYPE RANGE OF VBRK-BUPLA  ##NEEDED.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA: GT_CONST  TYPE STANDARD TABLE OF ZSDSCAC001 ##NEEDED.

*---------------------------------------------------------------------*
*       FORM ENTRY
*---------------------------------------------------------------------*
FORM ENTRY USING UF_RETCODE     TYPE SY-SUBRC ##CALLED
                 UF_PROC_SCREEN TYPE C  ##NEEDED.

* for call by other program.
  PERFORM F_PRINT USING UF_RETCODE
                        NAST  ##PERF_GLOBAL_PAR_OK
                        TNAPR-SFORM
                        UF_PROC_SCREEN.

ENDFORM.                    "ENTRY
*&---------------------------------------------------------------------*
*& Form F_PRINT
*&---------------------------------------------------------------------*
*& Print
*&---------------------------------------------------------------------*
FORM F_PRINT USING UF_RETCODE     TYPE SY-SUBRC
                   US_NAST        TYPE NAST
                   UF_FORM_NAME   TYPE TNAPR-SFORM
                   UF_PROC_SCREEN TYPE C.

  DATA: LS_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ,
        LS_CREDIT_NOTE        TYPE LBBIL_INVOICE,
        LS_JOB_INFO           TYPE SSFCRESCL,
        LS_CONTROL_PARAM      TYPE SSFCTRLOP,
        LS_COMPOSER_PARAM     TYPE SSFCOMPOP,
        LS_COMPOSER_PARAM_OTF TYPE SSFCOMPOP,
        LS_RECIPIENT          TYPE SWOTOBJID,
        LS_SENDER             TYPE SWOTOBJID,
        LS_ADDR_KEY           TYPE ADDR_KEY,
        LS_DLV_LAND           TYPE VBRK-LAND1,
        LF_FORMNAME           TYPE TDSFNAME,
        LF_SUBRC              TYPE SY-SUBRC,
        LF_FM_NAME            TYPE RS38L_FNAM.

  NAST = US_NAST.

* SmartForm from customizing table TNAPR
  LF_FORMNAME = UF_FORM_NAME.

* determine print data
  PERFORM F_SET_PRINT_DATA_TO_READ  USING LF_FORMNAME
                                          LS_PRINT_DATA_TO_READ
                                          LF_SUBRC.

  IF LF_SUBRC = 0.
*   select print data
    PERFORM F_GET_INVOICE_DATA  USING LS_PRINT_DATA_TO_READ
                                      LS_CREDIT_NOTE
                                      LF_SUBRC.
  ENDIF.

  IF LF_SUBRC = 0.
    PERFORM F_SET_PRINT_PARAM USING     LS_ADDR_KEY
                                        LS_DLV_LAND
                                        UF_RETCODE
                                        UF_PROC_SCREEN
                              CHANGING  LS_CONTROL_PARAM
                                        LS_COMPOSER_PARAM
                                        LS_RECIPIENT
                                        LS_SENDER
                                        LF_SUBRC.
  ENDIF.

  IF LF_SUBRC = 0.
*   determine smartform function module for invoice
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = LF_FORMNAME
      IMPORTING
        FM_NAME            = LF_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.
*     error handling
      LF_SUBRC = SY-SUBRC.
      PERFORM F_PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

  IF LF_SUBRC = 0.
*   call smartform credit note
    CALL FUNCTION LF_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = TOA_DARA
        ARCHIVE_PARAMETERS = ARC_PARAMS
        CONTROL_PARAMETERS = LS_CONTROL_PARAM
        MAIL_RECIPIENT     = LS_RECIPIENT
        MAIL_SENDER        = LS_SENDER
        OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
        USER_SETTINGS      = SPACE
        IS_CREDIT_NOTE     = LS_CREDIT_NOTE
      IMPORTING
        JOB_OUTPUT_INFO    = LS_JOB_INFO
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.
    IF SY-SUBRC <> 0.
*       error handling
      LF_SUBRC = SY-SUBRC.
      PERFORM F_PROTOCOL_UPDATE.
    ELSE.
      IF LS_JOB_INFO-OUTPUTDONE = ABAP_TRUE.
        LS_CONTROL_PARAM-GETOTF       = ABAP_TRUE.
        LS_CONTROL_PARAM-NO_DIALOG    = ABAP_TRUE.
        LS_COMPOSER_PARAM_OTF-TDDEST  = GC_OUTPUT_DEVICE_OTF.

*       call smartform credit note for generating PDF
        CALL FUNCTION LF_FM_NAME
          EXPORTING
            ARCHIVE_INDEX      = TOA_DARA
            ARCHIVE_PARAMETERS = ARC_PARAMS
            CONTROL_PARAMETERS = LS_CONTROL_PARAM
            MAIL_RECIPIENT     = LS_RECIPIENT
            MAIL_SENDER        = LS_SENDER
            OUTPUT_OPTIONS     = LS_COMPOSER_PARAM_OTF
            USER_SETTINGS      = SPACE
            IS_CREDIT_NOTE     = LS_CREDIT_NOTE
          IMPORTING
            JOB_OUTPUT_INFO    = LS_JOB_INFO
          EXCEPTIONS
            FORMATTING_ERROR   = 1
            INTERNAL_ERROR     = 2
            SEND_ERROR         = 3
            USER_CANCELED      = 4
            OTHERS             = 5.
        IF SY-SUBRC <> 0.
*         error handling
          LF_SUBRC = SY-SUBRC.
          PERFORM F_PROTOCOL_UPDATE.
        ELSE.
          PERFORM F_GENERATE_PDF USING LS_CREDIT_NOTE
                                       LS_JOB_INFO
                                       LF_SUBRC.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  UF_RETCODE  = LF_SUBRC.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PRINT_DATA_TO_READ
*&---------------------------------------------------------------------*
*& Set Print Data to Read
*&---------------------------------------------------------------------*
FORM F_SET_PRINT_DATA_TO_READ
  USING UF_FORMNAME           TYPE TNAPR-SFORM
        US_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ
        UF_RETCODE            TYPE SY-SUBRC.

  FIELD-SYMBOLS: <L_PRINT_DATA_TO_READ> TYPE XFELD.
  DATA: LT_FIELDLIST TYPE TSFFIELDS ##NEEDED.

* set print data requirements
  DO.
    ASSIGN COMPONENT SY-INDEX OF STRUCTURE
                     US_PRINT_DATA_TO_READ TO <L_PRINT_DATA_TO_READ>.
    IF SY-SUBRC <> 0. EXIT. ENDIF.
    <L_PRINT_DATA_TO_READ> = 'X'.
  ENDDO.

  CALL FUNCTION 'SSF_FIELD_LIST'
    EXPORTING
      FORMNAME           = UF_FORMNAME
    IMPORTING
      FIELDLIST          = LT_FIELDLIST
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
*  error handling
    UF_RETCODE = SY-SUBRC.
    PERFORM F_PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM F_PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.       *
*---------------------------------------------------------------------*
FORM F_PROTOCOL_UPDATE.
  CHECK GF_XSCREEN = SPACE.
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
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    "PROTOCOL_UPDATE
*&---------------------------------------------------------------------*
*& Form F_GET_INVOICE_DATA
*&---------------------------------------------------------------------*
*& Get Invoice Data
*&---------------------------------------------------------------------*
FORM F_GET_INVOICE_DATA
              USING US_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ
                    US_BIL_INVOICE        TYPE LBBIL_INVOICE
                    UF_RETCODE            TYPE SY-SUBRC.
* read print data
  CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
    EXPORTING
      IF_BIL_NUMBER         = NAST-OBJKY
      IF_PARVW              = NAST-PARVW
      IF_PARNR              = NAST-PARNR
      IF_LANGUAGE           = NAST-SPRAS
      IS_PRINT_DATA_TO_READ = US_PRINT_DATA_TO_READ
    IMPORTING
      ES_BIL_INVOICE        = US_BIL_INVOICE
    EXCEPTIONS
      RECORDS_NOT_FOUND     = 1
      RECORDS_NOT_REQUESTED = 2
      OTHERS                = 3.
  IF SY-SUBRC <> 0.
    UF_RETCODE  = SY-SUBRC.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CONSTANTS
*&---------------------------------------------------------------------*
*& Get Constants
*&---------------------------------------------------------------------*
FORM F_GET_CONSTANTS.
  SELECT FROM ZSDSCAC001
  FIELDS *
  WHERE REPID     EQ @GC_REPID
    AND ZDEL_FLG  EQ @SPACE
  INTO TABLE @GT_CONST.

  IF SY-SUBRC EQ 0.
    SORT GT_CONST BY REPID PARAM PARAM_EXT SEQUENCE ASCENDING.
  ENDIF.

  LOOP AT GT_CONST ASSIGNING FIELD-SYMBOL(<L_CONST>).
    CASE <L_CONST>-PARAM.
      WHEN 'SUECHARGE_PERCN'.
        APPEND INITIAL LINE TO GRT_SUECHARG
        ASSIGNING FIELD-SYMBOL(<L_SUECHARG>).
        <L_SUECHARG>-SIGN   = <L_CONST>-PARAM_SIGN.
        <L_SUECHARG>-OPTION = <L_CONST>-PARAM_OPTION.
        <L_SUECHARG>-LOW    = <L_CONST>-VALUE_LOW.
        <L_SUECHARG>-HIGH   = <L_CONST>-VALUE_HIGH.
        GF_SUECHARG_DESC    = <L_CONST>-VDESC.
      WHEN 'CANCEL_SERVICE_INV_DOCTYP'.
        APPEND INITIAL LINE TO GRT_CANCEL_SERVICE_DOCTYPE
        ASSIGNING FIELD-SYMBOL(<L_CANCEL_SERVICE_DOCTYPE>).
        <L_CANCEL_SERVICE_DOCTYPE>-SIGN    = <L_CONST>-PARAM_SIGN.
        <L_CANCEL_SERVICE_DOCTYPE>-OPTION  = <L_CONST>-PARAM_OPTION.
        <L_CANCEL_SERVICE_DOCTYPE>-LOW     = <L_CONST>-VALUE_LOW.
        <L_CANCEL_SERVICE_DOCTYPE>-HIGH    = <L_CONST>-VALUE_HIGH.
      WHEN 'CN_DOCTYPE'.
        APPEND INITIAL LINE TO GRT_CN_DOCTYPE
        ASSIGNING FIELD-SYMBOL(<L_DOCTYPE>).
        <L_DOCTYPE>-SIGN    = <L_CONST>-PARAM_SIGN.
        <L_DOCTYPE>-OPTION  = <L_CONST>-PARAM_OPTION.
        <L_DOCTYPE>-LOW     = <L_CONST>-VALUE_LOW.
        <L_DOCTYPE>-HIGH    = <L_CONST>-VALUE_HIGH.
*     <<F36K910460 start ins
      WHEN 'CR_DOCTYPE'.
        APPEND INITIAL LINE TO GRT_CR_DOCTYPE
        ASSIGNING <L_DOCTYPE>.
        <L_DOCTYPE>-SIGN    = <L_CONST>-PARAM_SIGN.
        <L_DOCTYPE>-OPTION  = <L_CONST>-PARAM_OPTION.
        <L_DOCTYPE>-LOW     = <L_CONST>-VALUE_LOW.
        <L_DOCTYPE>-HIGH    = <L_CONST>-VALUE_HIGH.
*     <<F36K910460 end ins
      WHEN 'CUSTOMER_NO'.
        IF <L_CONST>-PARAM_EXT EQ 'NOT_SHOW_BRANCH'.
          APPEND INITIAL LINE TO GRT_CUST_NOT_SHOW_BRANCH
          ASSIGNING FIELD-SYMBOL(<L_CUST_NOT_SHOW_BRANCH>).
          <L_CUST_NOT_SHOW_BRANCH>-SIGN   = <L_CONST>-PARAM_SIGN.
          <L_CUST_NOT_SHOW_BRANCH>-OPTION = <L_CONST>-PARAM_OPTION.
          <L_CUST_NOT_SHOW_BRANCH>-LOW    = <L_CONST>-VALUE_LOW.
          <L_CUST_NOT_SHOW_BRANCH>-HIGH   = <L_CONST>-VALUE_HIGH.
        ENDIF.
      WHEN 'CUSTOMER_ONE_TIME'.
        IF <L_CONST>-PARAM_EXT EQ 'GET_TAX_ID'.
          APPEND INITIAL LINE TO GRT_ONETIME_GET_TAXID
          ASSIGNING FIELD-SYMBOL(<L_ONETIME_GET_TAXID>).
          <L_ONETIME_GET_TAXID>-SIGN   = <L_CONST>-PARAM_SIGN.
          <L_ONETIME_GET_TAXID>-OPTION = <L_CONST>-PARAM_OPTION.
          <L_ONETIME_GET_TAXID>-LOW    = <L_CONST>-VALUE_LOW.
          <L_ONETIME_GET_TAXID>-HIGH   = <L_CONST>-VALUE_HIGH.
        ENDIF.
      WHEN 'MATERIAL'.
        IF <L_CONST>-PARAM_EXT EQ 'NOT_SHOW_MAT_DESC'.
          APPEND INITIAL LINE TO GRT_MAT_NOT_SHOW_DESC
          ASSIGNING FIELD-SYMBOL(<L_MAT_NOT_SHOW>).
          <L_MAT_NOT_SHOW>-SIGN   = <L_CONST>-PARAM_SIGN.
          <L_MAT_NOT_SHOW>-OPTION = <L_CONST>-PARAM_OPTION.
          <L_MAT_NOT_SHOW>-LOW    = <L_CONST>-VALUE_LOW.
          <L_MAT_NOT_SHOW>-HIGH   = <L_CONST>-VALUE_HIGH.
        ENDIF.
      WHEN 'COMP_BRANCH_CHANNEL'.
        APPEND INITIAL LINE TO GRT_DIST_CHAN_DEFAULT_BRANCH
        ASSIGNING FIELD-SYMBOL(<L_DIST_CHAN_DEFAULT_BRANCH>).
        <L_DIST_CHAN_DEFAULT_BRANCH>-SIGN   = <L_CONST>-PARAM_SIGN.
        <L_DIST_CHAN_DEFAULT_BRANCH>-OPTION = <L_CONST>-PARAM_OPTION.
        <L_DIST_CHAN_DEFAULT_BRANCH>-LOW    = <L_CONST>-VALUE_LOW.
        <L_DIST_CHAN_DEFAULT_BRANCH>-HIGH   = <L_CONST>-VALUE_HIGH.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_FORM_DATA
*&---------------------------------------------------------------------*
*& Prepare Form Data
*&---------------------------------------------------------------------*
FORM F_PREPARE_FORM_DATA USING US_CREDIT_NOTE TYPE LBBIL_INVOICE  ##CALLED
                               US_HEADER      TYPE ZSDSSDS028
                               US_FOOTER      TYPE ZSDSSDS029
                               UT_ITEM        TYPE ZSDSSDS030_TT.
  TYPES: BEGIN OF LTS_VBRP,
           VBELN      TYPE VBRP-VBELN,
           POSNR      TYPE VBRP-POSNR,
           VKGRP      TYPE VBRP-VKGRP,
           AUFNR      TYPE VBRP-AUFNR,
           AUGRU_AUFT TYPE VBRP-AUGRU_AUFT,
         END OF LTS_VBRP.

  DATA: LT_VBRP           TYPE STANDARD TABLE OF LTS_VBRP.
  DATA: LS_VBRP           LIKE LINE OF LT_VBRP.
  DATA: LS_CONST          LIKE LINE OF GT_CONST.
  DATA: LS_WORDS          TYPE SPELL.
  DATA: LF_BEZEI          TYPE TVGRT-BEZEI.
  DATA: LF_BNAME          TYPE VBAK-BNAME.
  DATA: LF_ZUONR          TYPE VBRK-ZUONR.
  DATA: LF_GRAND_TOTAL_C  TYPE IN_WORDS.
  DATA: LF_LEN            TYPE I.

  CHECK NOT US_CREDIT_NOTE IS INITIAL.

  PERFORM F_GET_CONSTANTS.

  IF NOT US_CREDIT_NOTE-IT_GEN IS INITIAL.
    SELECT FROM VBRP
    FIELDS VBELN,
           POSNR,
           VKGRP,
           AUFNR,
           AUGRU_AUFT
    FOR ALL ENTRIES IN @US_CREDIT_NOTE-IT_GEN
    WHERE VBELN EQ @US_CREDIT_NOTE-IT_GEN-BIL_NUMBER
      AND POSNR EQ @US_CREDIT_NOTE-IT_GEN-ITM_NUMBER
    INTO TABLE @LT_VBRP.
    IF SY-SUBRC EQ 0.
      SORT LT_VBRP BY VBELN POSNR ASCENDING.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*  Prepare Header
*----------------------------------------------------------------------*
  IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CN_DOCTYPE
    OR US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CR_DOCTYPE. "<<F36K910460++
    SELECT SINGLE FROM VBRK
    FIELDS XBLNR
    WHERE VBELN EQ @US_CREDIT_NOTE-HD_GEN-BIL_NUMBER
    INTO @US_HEADER-CREDIT_NOTE_NO.
  ELSEIF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CANCEL_SERVICE_DOCTYPE.
    US_HEADER-CREDIT_NOTE_NO  = US_CREDIT_NOTE-HD_GEN-BIL_NUMBER.
  ENDIF.

  US_HEADER-DOC_TYPE          = US_CREDIT_NOTE-HD_GEN-BIL_TYPE.
  US_HEADER-CUSTOMER_NO       = US_CREDIT_NOTE-HD_GEN-PAYER.
  IF NAST-SPRAS EQ '2'.
    US_HEADER-COMPANY_TAXID  = |เลขประจำตัวผู้เสียภาษี { US_CREDIT_NOTE-HD_GEN-T001_VAT+2 }|.
  ELSE.
    US_HEADER-COMPANY_TAXID  = |TAX ID NO. { US_CREDIT_NOTE-HD_GEN-T001_VAT+2 }|.
  ENDIF.

  PERFORM F_WRITE_DATE  USING US_CREDIT_NOTE-HD_GEN-BIL_DATE
                              US_HEADER-CREDIT_NOTE_DATE.

  PERFORM F_READ_INVOICE_NO_DATE USING US_CREDIT_NOTE
                                       US_HEADER-INVOICE_NO
                                       US_HEADER-INVOICE_DATE.

  READ TABLE GT_CONST INTO LS_CONST
  WITH KEY REPID  = GC_REPID
           PARAM  = 'SECRET'
  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    US_HEADER-SECRET = LS_CONST-VALUE_LOW.
  ENDIF.

  READ TABLE GT_CONST INTO LS_CONST
  WITH KEY REPID  = GC_REPID
           PARAM  = 'COMMERCIAL_ID'
  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    IF NAST-SPRAS EQ '2'.
      US_HEADER-COMMERCIAL_ID = |{ TEXT-T08 } { LS_CONST-VALUE_LOW }|.
    ELSE.
      US_HEADER-COMMERCIAL_ID = |{ TEXT-T09 } { LS_CONST-VALUE_LOW }|.
    ENDIF.
  ENDIF.

  "get tax rate
  READ TABLE US_CREDIT_NOTE-HD_KOND INTO DATA(LS_KOND)
  WITH KEY KAPPL  = 'V'
           KSCHL  = 'MWST'.
  IF SY-SUBRC EQ 0.
    US_HEADER-TAX_RATE  = LS_KOND-KBETR / 10.
    CONDENSE US_HEADER-TAX_RATE.
  ENDIF.

  PERFORM F_GET_COMPANY_INFO  USING US_CREDIT_NOTE
                                    US_HEADER.

  PERFORM F_GET_CUSTOMER_INFO USING US_CREDIT_NOTE
                                    US_HEADER.

*----------------------------------------------------------------------*
*  Prepare Items
*----------------------------------------------------------------------*
  PERFORM F_PREPARE_FORM_ITEMS USING US_CREDIT_NOTE
                                     US_FOOTER
                                     UT_ITEM.

*----------------------------------------------------------------------*
*  Prepare Footer
*----------------------------------------------------------------------*
  IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CN_DOCTYPE.
    READ TABLE LT_VBRP INTO LS_VBRP INDEX 1.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE FROM TVAUT  ##WARN_OK                   "#EC WARNOK
      FIELDS BEZEI
      WHERE AUGRU EQ @LS_VBRP-AUGRU_AUFT
      INTO @LF_BEZEI.
      IF SY-SUBRC EQ 0.
        US_FOOTER-REMARK  = LF_BEZEI.
      ENDIF.
    ENDIF.

  ELSEIF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CANCEL_SERVICE_DOCTYPE.
    PERFORM F_READ_REMARK_SERVICE USING US_CREDIT_NOTE
                                        US_FOOTER-REMARK.
  ENDIF.

  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      AMOUNT    = US_FOOTER-NET_AMOUNT
      CURRENCY  = 'THB'
      LANGUAGE  = NAST-SPRAS
    IMPORTING
      IN_WORDS  = LS_WORDS
    EXCEPTIONS
      NOT_FOUND = 1
      TOO_LARGE = 2
      OTHERS    = 3.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ELSE.
    LF_GRAND_TOTAL_C  = LS_WORDS-WORD.
    IF LS_WORDS-DECIMAL IS NOT INITIAL.
      IF NAST-SPRAS = 'E'.
        CONCATENATE LF_GRAND_TOTAL_C TEXT-T01 LS_WORDS-DECWORD TEXT-T02 INTO LF_GRAND_TOTAL_C SEPARATED BY ''.
      ELSE.
        CONCATENATE LF_GRAND_TOTAL_C TEXT-T03 LS_WORDS-DECWORD TEXT-T04 INTO LF_GRAND_TOTAL_C.
      ENDIF.
    ELSE.
      IF NAST-SPRAS = 'E'.
        CONCATENATE LF_GRAND_TOTAL_C TEXT-T05 INTO LF_GRAND_TOTAL_C SEPARATED BY ''.
      ELSE.
        CONCATENATE LF_GRAND_TOTAL_C TEXT-T06 INTO LF_GRAND_TOTAL_C.
      ENDIF.
    ENDIF.
  ENDIF.
  US_FOOTER-GRAND_TOTAL_TEXT  = LF_GRAND_TOTAL_C.

  LF_LEN  = STRLEN( US_FOOTER-GRAND_TOTAL_TEXT ).
  IF LF_LEN > GC_GTOTAL_TEXT_MAX_LEN AND NAST-SPRAS = 'E'.
    US_FOOTER-GRAND_TOTAL_TEXT    = LF_GRAND_TOTAL_C+0(GC_GTOTAL_TEXT_MAX_LEN).
    US_FOOTER-GRAND_TOTAL_TEXT_2  = LF_GRAND_TOTAL_C+GC_GTOTAL_TEXT_MAX_LEN.
    CONDENSE US_FOOTER-GRAND_TOTAL_TEXT_2.
  ENDIF.

  IF NOT US_CREDIT_NOTE-HD_REF-ORDER_NUMB IS INITIAL.
    SELECT SINGLE FROM VBAK
    FIELDS BNAME
    WHERE VBELN EQ @US_CREDIT_NOTE-HD_REF-ORDER_NUMB
    INTO @LF_BNAME.
    IF SY-SUBRC EQ 0.
      US_FOOTER-REF_NO  = LF_BNAME.
    ENDIF.
  ENDIF.

  IF US_FOOTER-REF_NO IS INITIAL.
    IF NOT US_CREDIT_NOTE-HD_GEN-SFAKN IS INITIAL.
      SELECT SINGLE FROM VBRK
      FIELDS ZUONR
      WHERE VBELN EQ @US_CREDIT_NOTE-HD_GEN-SFAKN
      INTO @LF_ZUONR.
      IF SY-SUBRC EQ 0.
        US_FOOTER-REF_NO  = LF_ZUONR.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_COMPANY_INFO
*&---------------------------------------------------------------------*
*& Get Company Info
*&---------------------------------------------------------------------*
FORM F_GET_COMPANY_INFO  USING  US_CREDIT_NOTE TYPE LBBIL_INVOICE
                                US_HEADER  TYPE ZSDSSDS028.
  TYPES: BEGIN OF LTS_VBRK,
           VBELN TYPE VBRK-VBELN,
           VTWEG TYPE VBRK-VTWEG,
           BUPLA TYPE VBRK-BUPLA,
         END OF LTS_VBRK.
  DATA: LT_ADRC         TYPE TT_ADRC.
  DATA: LT_ADRC_BRANCH  TYPE TT_ADRC.
  DATA: LT_BRANCHT      TYPE TT_BRANCHT.
  DATA: LS_ADRC         LIKE LINE OF LT_ADRC.
  DATA: LS_ADRC_BRANCH  LIKE LINE OF LT_ADRC_BRANCH.
  DATA: LS_BRANCH       TYPE TS_BRANCH.
  DATA: LS_BRANCHT      LIKE LINE OF LT_BRANCHT.
  DATA: LS_CONST        LIKE LINE OF GT_CONST.
  DATA: LS_VBRK         TYPE LTS_VBRK.
  DATA: LF_BUPLA        TYPE VBRK-BUPLA.
  DATA: LF_ADRNR        TYPE T001-ADRNR.

  SELECT SINGLE FROM T001
  FIELDS ADRNR
  WHERE BUKRS EQ @US_CREDIT_NOTE-HD_ORG-COMP_CODE
  INTO @LF_ADRNR.

  IF SY-SUBRC EQ 0.
    SELECT FROM ADRC
    LEFT OUTER JOIN T005T ON  T005T~SPRAS = ADRC~LANGU "#EC CI_BUFFJOIN
                          AND T005T~LAND1 = ADRC~COUNTRY
    FIELDS  ADRC~ADDRNUMBER,
            ADRC~DATE_FROM,
            ADRC~NATION,
            ADRC~NAME1,
            ADRC~NAME2,
            ADRC~NAME3,
            ADRC~NAME4,
            ADRC~CITY1,
            ADRC~CITY2,
            ADRC~POST_CODE1,
            ADRC~STREET,
            ADRC~STR_SUPPL1,
            ADRC~STR_SUPPL2,
            ADRC~STR_SUPPL3,
            ADRC~LOCATION,
            ADRC~COUNTRY,
            ADRC~LANGU,
            ADRC~TEL_NUMBER,
            ADRC~FAX_NUMBER,
            ADRC~TAXJURCODE,
            T005T~LANDX
    WHERE ADDRNUMBER  EQ @LF_ADRNR
      AND DATE_FROM   LE @SY-DATUM
      AND DATE_TO     GE @SY-DATUM
    INTO TABLE @LT_ADRC.
    IF SY-SUBRC EQ 0.
      SORT LT_ADRC BY ADDRNUMBER DATE_FROM NATION ASCENDING.
    ENDIF.
  ENDIF.

  "Company TH
  READ TABLE LT_ADRC INTO LS_ADRC WITH KEY NATION = SPACE.
  IF SY-SUBRC EQ 0.
    "Company name TH
    US_HEADER-COMPANY_NAME_TH =
    |{ LS_ADRC-NAME1 }{ LS_ADRC-NAME2 }{ LS_ADRC-NAME3 }{ LS_ADRC-NAME4 }|.
  ENDIF.

  "Company EN
  READ TABLE LT_ADRC INTO LS_ADRC WITH KEY NATION = 'I'.
  IF SY-SUBRC EQ 0.
    "Company name EN
    US_HEADER-COMPANY_NAME_EN =
    |{ LS_ADRC-NAME1 }{ LS_ADRC-NAME2 }{ LS_ADRC-NAME3 }{ LS_ADRC-NAME4 }|.
  ENDIF.

  "get branch
  SELECT SINGLE FROM VBRK
  FIELDS VBELN,
         VTWEG,
         BUPLA
  WHERE VBELN EQ @US_CREDIT_NOTE-HD_GEN-BIL_NUMBER
  INTO @LS_VBRK.
  IF SY-SUBRC EQ 0.
    IF LS_VBRK-VTWEG IN GRT_DIST_CHAN_DEFAULT_BRANCH.
      READ TABLE GT_CONST ASSIGNING FIELD-SYMBOL(<L_CONST>)
      WITH KEY PARAM = 'COMP_BRANCH_CHANNEL'
      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LF_BUPLA  = <L_CONST>-PARAM_EXT.
      ENDIF.
    ELSE.
      LF_BUPLA  = LS_VBRK-BUPLA.
    ENDIF.
  ENDIF.

  "Get Branch Address
  IF NOT LF_BUPLA IS INITIAL.
    SELECT SINGLE FROM J_1BBRANCH
    FIELDS BUKRS,
           BRANCH,
           BUPLA_TYPE,
           ADRNR
    WHERE BUKRS       EQ @US_CREDIT_NOTE-HD_ORG-COMP_CODE
      AND BRANCH      EQ @LF_BUPLA
      AND BUPLA_TYPE  EQ @SPACE
    INTO @LS_BRANCH.
    IF SY-SUBRC EQ 0.
      SELECT FROM ADRC
      LEFT OUTER JOIN T005T ON  T005T~SPRAS = ADRC~LANGU "#EC CI_BUFFJOIN
                            AND T005T~LAND1 = ADRC~COUNTRY
      FIELDS  ADRC~ADDRNUMBER,
              ADRC~DATE_FROM,
              ADRC~NATION,
              ADRC~NAME1,
              ADRC~NAME2,
              ADRC~NAME3,
              ADRC~NAME4,
              ADRC~CITY1,
              ADRC~CITY2,
              ADRC~POST_CODE1,
              ADRC~STREET,
              ADRC~STR_SUPPL1,
              ADRC~STR_SUPPL2,
              ADRC~STR_SUPPL3,
              ADRC~LOCATION,
              ADRC~COUNTRY,
              ADRC~LANGU,
              ADRC~TEL_NUMBER,
              ADRC~FAX_NUMBER,
              ADRC~TAXJURCODE,
              T005T~LANDX
      WHERE ADDRNUMBER  EQ @LS_BRANCH-ADRNR
        AND DATE_FROM   LE @SY-DATUM
        AND DATE_TO     GE @SY-DATUM
      INTO TABLE @LT_ADRC_BRANCH.
      IF SY-SUBRC EQ 0.
        SORT LT_ADRC_BRANCH BY ADDRNUMBER DATE_FROM NATION ASCENDING.

        SELECT FROM J_1BBRANCHT
        FIELDS *
        FOR ALL ENTRIES IN @LT_ADRC_BRANCH
        WHERE BUKRS       EQ @US_CREDIT_NOTE-HD_ORG-COMP_CODE
          AND BRANCH      EQ @LF_BUPLA
          AND BUPLA_TYPE  EQ @SPACE
          AND LANGUAGE    EQ @LT_ADRC_BRANCH-LANGU
        INTO TABLE @LT_BRANCHT.
        IF SY-SUBRC EQ 0.
          SORT LT_BRANCHT BY BUKRS BRANCH BUPLA_TYPE LANGUAGE ASCENDING.
        ENDIF.
      ENDIF.

      IF LF_BUPLA EQ '0000'.  "Head Office
        "Get Head Office TH
        READ TABLE LT_ADRC_BRANCH INTO LS_ADRC_BRANCH
        WITH KEY NATION = SPACE.
        IF SY-SUBRC EQ 0.
          READ TABLE LT_BRANCHT INTO LS_BRANCHT
          WITH KEY BUKRS      = US_CREDIT_NOTE-HD_ORG-COMP_CODE
                   BRANCH     = LF_BUPLA
                   BUPLA_TYPE = SPACE
                   LANGUAGE   = LS_ADRC_BRANCH-LANGU
          BINARY SEARCH.

          US_HEADER-COMPANY_ADDRESS_TH  =
          |{ LS_BRANCHT-NAME } : { LS_ADRC_BRANCH-STREET } { LS_ADRC_BRANCH-STR_SUPPL3 } { LS_ADRC_BRANCH-LOCATION } { LS_ADRC_BRANCH-CITY2 } { LS_ADRC_BRANCH-CITY1 } { LS_ADRC_BRANCH-POST_CODE1 }|.
          US_HEADER-COMPANY_ADDRESS_TH  =
          |{ US_HEADER-COMPANY_ADDRESS_TH } TEL. { LS_ADRC_BRANCH-TEL_NUMBER }|.
        ENDIF.

        "Get Head Office EN
        READ TABLE LT_ADRC_BRANCH INTO LS_ADRC_BRANCH
        WITH KEY NATION = 'I'.
        IF SY-SUBRC EQ 0.
          READ TABLE LT_BRANCHT INTO LS_BRANCHT
          WITH KEY BUKRS      = US_CREDIT_NOTE-HD_ORG-COMP_CODE
                   BRANCH     = LF_BUPLA
                   BUPLA_TYPE = SPACE
                   LANGUAGE   = LS_ADRC_BRANCH-LANGU
          BINARY SEARCH.

          US_HEADER-COMPANY_ADDRESS_EN  =
          |{ TEXT-T07 } : { LS_ADRC_BRANCH-STREET } { LS_ADRC_BRANCH-STR_SUPPL3 } { LS_ADRC_BRANCH-LOCATION } { LS_ADRC_BRANCH-CITY2 } { LS_ADRC_BRANCH-CITY1 } { LS_ADRC_BRANCH-POST_CODE1 } { LS_ADRC_BRANCH-LANDX }|.
        ENDIF.

      ELSE. "Branch
        "Get Branch TH
        READ TABLE LT_ADRC_BRANCH INTO LS_ADRC_BRANCH
        WITH KEY NATION = SPACE.
        IF SY-SUBRC EQ 0.
          READ TABLE LT_BRANCHT INTO LS_BRANCHT
          WITH KEY BUKRS      = US_CREDIT_NOTE-HD_ORG-COMP_CODE
                   BRANCH     = LF_BUPLA
                   BUPLA_TYPE = SPACE
                   LANGUAGE   = LS_ADRC_BRANCH-LANGU
          BINARY SEARCH.

          US_HEADER-COMPANY_ADDRESS_TH  =
          |สาขา { LS_BRANCHT-NAME } ({ LS_BRANCH-BRANCH }) : { LS_ADRC_BRANCH-STREET } { LS_ADRC_BRANCH-STR_SUPPL3 } { LS_ADRC_BRANCH-LOCATION } { LS_ADRC_BRANCH-CITY2 } { LS_ADRC_BRANCH-CITY1 } { LS_ADRC_BRANCH-POST_CODE1 }|.
          US_HEADER-COMPANY_ADDRESS_TH  =
          |{ US_HEADER-COMPANY_ADDRESS_TH } TEL. { LS_ADRC_BRANCH-TEL_NUMBER }|.
        ENDIF.
        "Get Branch EN
        READ TABLE LT_ADRC_BRANCH INTO LS_ADRC_BRANCH
        WITH KEY NATION = 'I'.
        IF SY-SUBRC EQ 0.
          READ TABLE LT_BRANCHT INTO LS_BRANCHT
          WITH KEY BUKRS      = US_CREDIT_NOTE-HD_ORG-COMP_CODE
                   BRANCH     = LF_BUPLA
                   BUPLA_TYPE = SPACE
                   LANGUAGE   = LS_ADRC_BRANCH-LANGU
          BINARY SEARCH.

          US_HEADER-COMPANY_ADDRESS_EN  =
          |'Branch' { LS_BRANCHT-NAME } ({ LS_BRANCH-BRANCH }) : { LS_ADRC_BRANCH-STREET } { LS_ADRC_BRANCH-STR_SUPPL3 } { LS_ADRC_BRANCH-LOCATION } { LS_ADRC_BRANCH-CITY2 } { LS_ADRC_BRANCH-CITY1 } { LS_ADRC_BRANCH-POST_CODE1 } { LS_ADRC_BRANCH-LANDX }|.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE GT_CONST INTO LS_CONST
  WITH KEY REPID  = GC_REPID
           PARAM  = 'COLL_LINE'
  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    US_HEADER-COMPANY_ADDRESS_TH  = |{ US_HEADER-COMPANY_ADDRESS_TH } { LS_CONST-VALUE_LOW }|.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_CUSTOMER_INFO
*&---------------------------------------------------------------------*
*& Get Customer Info
*&---------------------------------------------------------------------*
FORM F_GET_CUSTOMER_INFO  USING  US_CREDIT_NOTE TYPE LBBIL_INVOICE
                                 US_HEADER  TYPE ZSDSSDS028.
  DATA: LT_ADRC             TYPE TT_ADRC.
  DATA: LS_ADRC             LIKE LINE OF LT_ADRC.
  DATA: LS_HD_ADR           TYPE LBBIL_HD_ADR.
  DATA: LS_KNA1             TYPE TS_KNA1.
  DATA: LS_VBPA3            TYPE TS_VBPA3.
  DATA: LF_ADRNR            TYPE T001-ADRNR.
  DATA: LF_BRANCH_TH        TYPE FITHA_PBUPL_D_T-DESCRIPTION.
  DATA: LF_BRANCH_EN        TYPE FITHA_PBUPL_D_T-DESCRIPTION.
  DATA: LF_NOT_SHOW_BRANCH  TYPE FLAG.

  READ TABLE US_CREDIT_NOTE-HD_ADR INTO LS_HD_ADR       "#EC CI_SORTSEQ
  WITH KEY PARTN_ROLE = 'RG'. "Payer
  IF SY-SUBRC EQ 0.
    LF_ADRNR  = LS_HD_ADR-ADDR_NO.
  ELSE.
    RETURN.
  ENDIF.

  IF NOT US_CREDIT_NOTE-HD_GEN-PAYER IN GRT_ONETIME_GET_TAXID.
    SELECT SINGLE FROM KNA1
    FIELDS KUNNR,
           STCD3
    WHERE KUNNR EQ @US_CREDIT_NOTE-HD_GEN-PAYER
    INTO @LS_KNA1.
  ELSE.
    SELECT SINGLE FROM VBPA3  ##WARN_OK                     "#EC WARNOK
    FIELDS VBELN,
           STCD3
    WHERE VBELN EQ @US_CREDIT_NOTE-HD_GEN-BIL_NUMBER
      AND PARVW EQ 'RG' "Payer
    INTO @LS_VBPA3.
  ENDIF.

  "Get customer branch
  IF NOT LS_KNA1 IS INITIAL.
    SELECT FROM FITHA_PBUPL_D
    LEFT OUTER JOIN FITHA_PBUPL_D_T ON  FITHA_PBUPL_D~KUNNR  = FITHA_PBUPL_D_T~KUNNR
                                    AND FITHA_PBUPL_D~J_1TPBUPL = FITHA_PBUPL_D_T~J_1TPBUPL
    FIELDS  FITHA_PBUPL_D~KUNNR,
            FITHA_PBUPL_D~J_1TPBUPL,
            FITHA_PBUPL_D~DEFAULT_BRANCH,
            FITHA_PBUPL_D_T~DESCRIPTION
    WHERE FITHA_PBUPL_D~KUNNR EQ @LS_KNA1-KUNNR
    INTO TABLE @DATA(LT_CUST_BRANCH).
  ELSE.
    SELECT FROM FITHA_PBUPL_D
    LEFT OUTER JOIN FITHA_PBUPL_D_T ON  FITHA_PBUPL_D~KUNNR  = FITHA_PBUPL_D_T~KUNNR
                                    AND FITHA_PBUPL_D~J_1TPBUPL = FITHA_PBUPL_D_T~J_1TPBUPL
    FIELDS  FITHA_PBUPL_D~KUNNR,
            FITHA_PBUPL_D~J_1TPBUPL,
            FITHA_PBUPL_D~DEFAULT_BRANCH,
            FITHA_PBUPL_D_T~DESCRIPTION
    WHERE FITHA_PBUPL_D~KUNNR EQ @US_CREDIT_NOTE-HD_GEN-PAYER
    INTO TABLE @LT_CUST_BRANCH.
  ENDIF.

  SELECT FROM ADRC
  LEFT OUTER JOIN T005T ON  T005T~SPRAS = ADRC~LANGU   "#EC CI_BUFFJOIN
                        AND T005T~LAND1 = ADRC~COUNTRY
  FIELDS  ADRC~ADDRNUMBER,
          ADRC~DATE_FROM,
          ADRC~NATION,
          ADRC~NAME1,
          ADRC~NAME2,
          ADRC~NAME3,
          ADRC~NAME4,
          ADRC~CITY1,
          ADRC~CITY2,
          ADRC~POST_CODE1,
          ADRC~STREET,
          ADRC~STR_SUPPL1,
          ADRC~STR_SUPPL2,
          ADRC~STR_SUPPL3,
          ADRC~LOCATION,
          ADRC~COUNTRY,
          ADRC~LANGU,
          ADRC~TEL_NUMBER,
          ADRC~FAX_NUMBER,
          ADRC~TAXJURCODE,
          T005T~LANDX
  WHERE ADDRNUMBER  EQ @LF_ADRNR
    AND DATE_FROM   LE @SY-DATUM
    AND DATE_TO     GE @SY-DATUM
  INTO TABLE @LT_ADRC.
  IF SY-SUBRC EQ 0.
    SORT LT_ADRC BY ADDRNUMBER DATE_FROM NATION ASCENDING.
  ENDIF.

  IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CN_DOCTYPE OR
     US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CR_DOCTYPE.  "CH01 ADD
    IF NOT LS_KNA1 IS INITIAL.
      IF NOT LS_KNA1-STCD3+0(13) IS INITIAL AND
         NOT LS_KNA1-STCD3+0(13) CO '0'.
        IF NAST-SPRAS EQ '2'.
          US_HEADER-CUSTOMER_TAXID  = |เลขประจำตัวผู้เสียภาษี { LS_KNA1-STCD3+0(13) }|.
        ELSE.
          US_HEADER-CUSTOMER_TAXID  = |TAX ID NO. { LS_KNA1-STCD3+0(13) }|.
        ENDIF.
      ELSE.
        LF_NOT_SHOW_BRANCH  = ABAP_TRUE.
      ENDIF.

    ELSEIF NOT LS_VBPA3 IS INITIAL.
      IF NOT LS_VBPA3-STCD3+0(13) IS INITIAL AND
         NOT LS_VBPA3-STCD3+0(13) CO '0'.
        IF NAST-SPRAS EQ '2'.
          US_HEADER-CUSTOMER_TAXID  = |เลขประจำตัวผู้เสียภาษี { LS_VBPA3-STCD3 }|.
        ELSE.
          US_HEADER-CUSTOMER_TAXID  = |TAX ID NO. { LS_VBPA3-STCD3 }|.
        ENDIF.
      ELSE.
        LF_NOT_SHOW_BRANCH  = ABAP_TRUE.
      ENDIF.
    ENDIF.
  ENDIF.

  IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CANCEL_SERVICE_DOCTYPE. "doctype cancel service
    IF US_CREDIT_NOTE-HD_GEN-PAYER IN GRT_ONETIME_GET_TAXID.  "one time
      IF NAST-SPRAS EQ '2'.
        READ TABLE LT_ADRC INTO LS_ADRC WITH KEY NATION = SPACE.
        IF SY-SUBRC EQ 0 AND
          NOT LS_ADRC-TAXJURCODE IS INITIAL.
          US_HEADER-CUSTOMER_TAXID  = |เลขประจำตัวผู้เสียภาษี { LS_ADRC-TAXJURCODE }|.
        ENDIF.
      ELSE.
        READ TABLE LT_ADRC INTO LS_ADRC WITH KEY NATION = 'I'.
        IF SY-SUBRC NE 0.
          READ TABLE LT_ADRC INTO LS_ADRC WITH KEY NATION = SPACE.
        ENDIF.
        IF SY-SUBRC EQ 0 AND
          NOT LS_ADRC-TAXJURCODE IS INITIAL.
          US_HEADER-CUSTOMER_TAXID  = |TAX ID NO. { LS_ADRC-TAXJURCODE }|.
        ENDIF.
      ENDIF.
    ELSE.
      IF NOT LS_KNA1 IS INITIAL.
        IF NOT LS_KNA1-STCD3+0(13) IS INITIAL AND
           NOT LS_KNA1-STCD3+0(13) CO '0'.
          IF NAST-SPRAS EQ '2'.
            US_HEADER-CUSTOMER_TAXID  = |เลขประจำตัวผู้เสียภาษี { LS_KNA1-STCD3+0(13) }|.
          ELSE.
            US_HEADER-CUSTOMER_TAXID  = |TAX ID NO. { LS_KNA1-STCD3+0(13) }|.
          ENDIF.
        ELSE.
          LF_NOT_SHOW_BRANCH  = ABAP_TRUE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  "Customer
  IF NAST-SPRAS EQ '2'.
    READ TABLE LT_ADRC INTO LS_ADRC WITH KEY NATION = SPACE.
    IF SY-SUBRC EQ 0.
      "Customer name TH
      US_HEADER-CUSTOMER_NAME =
      |{ LS_ADRC-NAME1 }{ LS_ADRC-NAME2 }{ LS_ADRC-NAME3 }{ LS_ADRC-NAME4 }|.

      US_HEADER-CUSTOMER_ADDR_LINE1 = LS_ADRC-STREET.
      US_HEADER-CUSTOMER_ADDR_LINE2 = |{ LS_ADRC-STR_SUPPL3 } { LS_ADRC-LOCATION }|.
      US_HEADER-CUSTOMER_ADDR_LINE3 = |{ LS_ADRC-STR_SUPPL1 } { LS_ADRC-STR_SUPPL2 } { LS_ADRC-CITY2 }|.
      US_HEADER-CUSTOMER_ADDR_LINE4 = |{ LS_ADRC-CITY1 } { LS_ADRC-POST_CODE1 }|.
    ENDIF.
  ELSE.
    READ TABLE LT_ADRC INTO LS_ADRC WITH KEY NATION = 'I'.
    IF SY-SUBRC NE 0.
      READ TABLE LT_ADRC INTO LS_ADRC WITH KEY NATION = SPACE.
    ENDIF.
    IF SY-SUBRC EQ 0.
      "Customer name EN
      US_HEADER-CUSTOMER_NAME =
      |{ LS_ADRC-NAME1 }{ LS_ADRC-NAME2 }{ LS_ADRC-NAME3 }{ LS_ADRC-NAME4 }|.

      US_HEADER-CUSTOMER_ADDR_LINE1 = LS_ADRC-STREET.
      US_HEADER-CUSTOMER_ADDR_LINE2 = |{ LS_ADRC-STR_SUPPL3 } { LS_ADRC-LOCATION }|.
      US_HEADER-CUSTOMER_ADDR_LINE3 = |{ LS_ADRC-STR_SUPPL1 } { LS_ADRC-STR_SUPPL2 } { LS_ADRC-CITY2 }|.
      US_HEADER-CUSTOMER_ADDR_LINE4 = |{ LS_ADRC-CITY1 } { LS_ADRC-POST_CODE1 }|.
    ENDIF.
  ENDIF.

  "Get customer branch
  IF NOT US_CREDIT_NOTE-HD_GEN-PAYER IN GRT_CUST_NOT_SHOW_BRANCH AND
     LF_NOT_SHOW_BRANCH IS INITIAL.

    READ TABLE LT_CUST_BRANCH INTO DATA(LS_CUST_BRANCH)
    WITH KEY DEFAULT_BRANCH = 'X'.
    IF SY-SUBRC EQ 0.
      SPLIT LS_CUST_BRANCH-DESCRIPTION AT '/' INTO LF_BRANCH_EN LF_BRANCH_TH.
      TRANSLATE LF_BRANCH_EN TO UPPER CASE.
      IF NAST-SPRAS EQ '2'.
        IF LS_CUST_BRANCH-J_1TPBUPL EQ '00000'.
          US_HEADER-CUSTOMER_NAME = |{ US_HEADER-CUSTOMER_NAME } { LF_BRANCH_TH }|.
        ELSE.
          IF LS_CUST_BRANCH-J_1TPBUPL NE 'VAT' AND
             LS_CUST_BRANCH-J_1TPBUPL NE 'NVAT'.
            US_HEADER-CUSTOMER_NAME = |{ US_HEADER-CUSTOMER_NAME } สาขาที่ { LS_CUST_BRANCH-J_1TPBUPL }|.
          ENDIF.
        ENDIF.
      ELSE.
        IF LS_CUST_BRANCH-J_1TPBUPL EQ '00000'.
          US_HEADER-CUSTOMER_NAME = |{ US_HEADER-CUSTOMER_NAME } { LF_BRANCH_EN }|.
        ELSE.
          IF LS_CUST_BRANCH-J_1TPBUPL NE 'VAT' AND
             LS_CUST_BRANCH-J_1TPBUPL NE 'NVAT'.
            US_HEADER-CUSTOMER_NAME = |{ US_HEADER-CUSTOMER_NAME } { TEXT-T11 } { LS_CUST_BRANCH-J_1TPBUPL }|.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CONDENSE: US_HEADER-CUSTOMER_ADDR_LINE1,
            US_HEADER-CUSTOMER_ADDR_LINE2,
            US_HEADER-CUSTOMER_ADDR_LINE3,
            US_HEADER-CUSTOMER_ADDR_LINE4.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_WRITE_DATE
*&---------------------------------------------------------------------*
*& Write Date format DD-MMM-YYYY
*&---------------------------------------------------------------------*
FORM F_WRITE_DATE  USING  UF_DATE     TYPE DATUM
                          UF_NEW_DATE TYPE CHAR12.
  CHECK NOT UF_DATE IS INITIAL.

  CASE UF_DATE+4(2).
    WHEN '01'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-JAN-{ UF_DATE+0(4) }|.
    WHEN '02'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-FEB-{ UF_DATE+0(4) }|.
    WHEN '03'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-MAR-{ UF_DATE+0(4) }|.
    WHEN '04'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-APR-{ UF_DATE+0(4) }|.
    WHEN '05'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-MAY-{ UF_DATE+0(4) }|.
    WHEN '06'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-JUN-{ UF_DATE+0(4) }|.
    WHEN '07'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-JUL-{ UF_DATE+0(4) }|.
    WHEN '08'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-AUG-{ UF_DATE+0(4) }|.
    WHEN '09'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-SEP-{ UF_DATE+0(4) }|.
    WHEN '10'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-OCT-{ UF_DATE+0(4) }|.
    WHEN '11'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-NOV-{ UF_DATE+0(4) }|.
    WHEN '12'.
      UF_NEW_DATE = |{ UF_DATE+6(2) }-DEC-{ UF_DATE+0(4) }|.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREPARE_FORM_ITEMS
*&---------------------------------------------------------------------*
*& Prepare Form Items
*&---------------------------------------------------------------------*
FORM F_PREPARE_FORM_ITEMS  USING  US_CREDIT_NOTE  TYPE LBBIL_INVOICE
                                  US_FOOTER       TYPE ZSDSSDS029
                                  UT_ITEM         TYPE ZSDSSDS030_TT.
  TYPES: BEGIN OF LTS_PRCD_ELEM,
           KNUMV TYPE PRCD_ELEMENTS-KNUMV,
           KPOSN TYPE PRCD_ELEMENTS-KPOSN,
           STUNR TYPE PRCD_ELEMENTS-STUNR,
           ZAEHK TYPE PRCD_ELEMENTS-ZAEHK,
           KAPPL TYPE PRCD_ELEMENTS-KAPPL,
           KSCHL TYPE PRCD_ELEMENTS-KSCHL,
           KRECH TYPE PRCD_ELEMENTS-KRECH,
           KWERT TYPE PRCD_ELEMENTS-KWERT,
           WAERK TYPE PRCD_ELEMENTS-WAERK,
         END OF LTS_PRCD_ELEM.
  TYPES: BEGIN OF LTS_VBFA,
           VBELV TYPE VBFA-VBELV,
           POSNV TYPE VBFA-POSNV,
           VBELN TYPE VBFA-VBELN,
           POSNN TYPE VBFA-POSNN,
         END OF LTS_VBFA.

  DATA: LT_VBRP_INV       TYPE TT_VBRP.
  DATA: LT_VBRP_CN        TYPE TT_VBRP.
  DATA: LT_VBRP           TYPE TT_VBRP.
  DATA: LT_VBFA           TYPE STANDARD TABLE OF LTS_VBFA.
  DATA: LT_PRCD_ELEM_INV  TYPE STANDARD TABLE OF LTS_PRCD_ELEM.
  DATA: LS_ITEM           LIKE LINE OF UT_ITEM.
  DATA: LS_BOM            LIKE LINE OF UT_ITEM.
  DATA: LS_VBFA           LIKE LINE OF LT_VBFA.
  DATA: LF_FOUND_ZH16     TYPE FLAG.
  DATA: LF_INDEX          TYPE SY-TABIX.
  DATA: LF_QTY            TYPE I.
  DATA: LF_TOTAL_UP       TYPE ZSDSSDS010-UNIT_PRICE.
  DATA: LF_TOTAL_DC       TYPE ZSDSSDS010-DISCOUNT.
  DATA: LF_TOTAL_NUP      TYPE ZSDSSDS010-NET_UNIT_PRICE.
  DATA: LF_TOTAL_AMT      TYPE ZSDSSDS010-AMOUNT.
  DATA: LF_TOTAL_TAX      TYPE ZSDSSDS010-TAX_AMOUNT.
  DATA: LF_TOTAL_GAMT     TYPE VBRP-KZWI1.
  DATA: LF_TOTAL_QTY      TYPE VBRP-FKIMG.
  DATA: LF_TOTAL_SUECHARG TYPE ZSDSSDS010-TAX_AMOUNT.
  DATA: LF_KNUMV_INV      TYPE VBRK-KNUMV.
  DATA: LF_VBELN_INV      TYPE VBRK-VBELN.
  DATA: LF_WITHOUT_REF    TYPE FLAG.
*  DATA: LF_PRICE_REDUCE   TYPE ZSDSSDS029-PRICE_REDUCE. "CH01 DEL

  FIELD-SYMBOLS:
    <L_VBRP>      LIKE LINE OF LT_VBRP,
    <L_VBRP_2>    LIKE LINE OF LT_VBRP,
    <L_BOM>       LIKE LINE OF LT_VBRP,
    <L_ITEM>      LIKE LINE OF UT_ITEM,
    <L_PRCD_ELEM> LIKE LINE OF LT_PRCD_ELEM_INV.

  "get CN items
  SELECT FROM VBRP
  FIELDS VBELN,
         POSNR,
         UEPOS,
         FKIMG,
         VRKME,
         NETWR,
         MATNR,
         ARKTX,
         KZWI1,
         KZWI3,
         KZWI4,
         KZWI5,
         MWSBP
  WHERE VBELN EQ @US_CREDIT_NOTE-HD_GEN-BIL_NUMBER
  INTO TABLE @LT_VBRP_CN.
  IF SY-SUBRC EQ 0.
    SORT LT_VBRP_CN BY VBELN POSNR ASCENDING.
  ENDIF.

  IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CN_DOCTYPE
    OR US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CR_DOCTYPE. "<<F36K910460++
    IF NOT US_CREDIT_NOTE-HD_REF-VBELN_VG2 IS INITIAL.
      LF_VBELN_INV  = US_CREDIT_NOTE-HD_REF-VBELN_VG2.
    ELSEIF NOT US_CREDIT_NOTE-HD_REF-ORDER_NUMB IS INITIAL.
      "get invoice no.
      SELECT FROM VBFA
      FIELDS VBELV,
             POSNV,
             VBELN,
             POSNN
      WHERE VBELN   EQ @US_CREDIT_NOTE-HD_REF-ORDER_NUMB
        AND VBTYP_N EQ 'K'
        AND VBTYP_V EQ 'M'
      INTO TABLE @LT_VBFA.
      IF SY-SUBRC EQ 0.
        READ TABLE LT_VBFA INTO LS_VBFA INDEX 1.        "#EC CI_NOORDER
        IF SY-SUBRC EQ 0.
          LF_VBELN_INV  = LS_VBFA-VBELV.
        ENDIF.
      ELSE.
        LF_WITHOUT_REF  = ABAP_TRUE.
      ENDIF.
    ENDIF.

  ELSEIF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CANCEL_SERVICE_DOCTYPE.
    LF_VBELN_INV  = US_CREDIT_NOTE-HD_GEN-SFAKN.
  ENDIF.

  IF NOT LF_VBELN_INV IS INITIAL.
    "get invoice items
    SELECT FROM VBRP
    FIELDS VBELN,
           POSNR,
           UEPOS,
           FKIMG,
           VRKME,
           NETWR,
           MATNR,
           ARKTX,
           KZWI1,
           KZWI3,
           KZWI4,
           KZWI5,
           MWSBP
    WHERE VBELN EQ @LF_VBELN_INV
    INTO TABLE @LT_VBRP_INV.
    IF SY-SUBRC EQ 0.
      SORT LT_VBRP_INV BY VBELN POSNR ASCENDING.
    ENDIF.

    "get prices from invoice
    SELECT SINGLE FROM VBRK
    FIELDS KNUMV
    WHERE VBELN EQ @LF_VBELN_INV
    INTO @LF_KNUMV_INV.
    IF SY-SUBRC EQ 0.
      SELECT FROM PRCD_ELEMENTS
      FIELDS KNUMV,
             KPOSN,
             STUNR,
             ZAEHK,
             KAPPL,
             KSCHL,
             KRECH,
             KWERT,
             WAERK
      WHERE KNUMV EQ @LF_KNUMV_INV
      INTO TABLE @LT_PRCD_ELEM_INV.
      IF SY-SUBRC EQ 0.
        SORT LT_PRCD_ELEM_INV BY KNUMV KPOSN STUNR ZAEHK ASCENDING.
      ENDIF.
    ENDIF.
  ENDIF.

  "doctype in genc use items from invoice
  IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CN_DOCTYPE.
    IF LF_WITHOUT_REF IS INITIAL.
      LT_VBRP = LT_VBRP_INV.
    ELSE.
      LT_VBRP = LT_VBRP_CN.
    ENDIF.
  ELSEIF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CANCEL_SERVICE_DOCTYPE.
    LT_VBRP = LT_VBRP_INV.
  ELSE.
    "doctype not in genc use items from CN
    LT_VBRP = LT_VBRP_CN.
  ENDIF.

  PERFORM F_READ_MERGE_ITEMS  USING US_CREDIT_NOTE
                                    LF_FOUND_ZH16.

  IF LF_FOUND_ZH16  EQ ABAP_TRUE. "text ZH16 found
    "Case duplicate materials within 1 invoice
    "Merge items with same material into 1 line
    LOOP AT LT_VBRP ASSIGNING <L_VBRP>.
      CLEAR: LS_ITEM,
             LF_TOTAL_GAMT,
             LF_TOTAL_QTY,
             LF_TOTAL_AMT,
             LF_TOTAL_TAX.

      IF <L_VBRP>-MATNR IN GRT_MAT_NOT_SHOW_DESC.
        CONTINUE.
      ENDIF.

      READ TABLE UT_ITEM WITH KEY MODEL  = <L_VBRP>-MATNR
      TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
        "New material
        LF_INDEX                = LF_INDEX + 1.
        LS_ITEM-CREDIT_NOTE_NO  = <L_VBRP>-VBELN.
        LS_ITEM-ITEM_NO         = <L_VBRP>-POSNR.
        LS_ITEM-ITEM_NO_TEXT    = LF_INDEX.
        LS_ITEM-MODEL           = <L_VBRP>-MATNR.
        LS_ITEM-MODEL_DESC      = <L_VBRP>-ARKTX.
        LS_ITEM-SALES_UNIT      = <L_VBRP>-VRKME.

        PERFORM F_READ_MAT_DESC USING US_CREDIT_NOTE
                                    <L_VBRP>
                                    LS_ITEM-MODEL_DESC.

        PERFORM F_READ_QUANTITY USING US_CREDIT_NOTE
                                      <L_VBRP>
                                      LS_ITEM-QUANTITY.

        PERFORM F_READ_UNIT USING US_CREDIT_NOTE
                                  <L_VBRP>
                                  LS_ITEM-SALES_UNIT
                                  LS_ITEM-SALES_UNIT_TEXT.

        "Loop other items with same material
        LOOP AT LT_VBRP ASSIGNING <L_VBRP_2> WHERE MATNR EQ <L_VBRP>-MATNR. "#EC CI_NESTED
          IF LF_WITHOUT_REF IS INITIAL.
            LF_TOTAL_AMT  = LF_TOTAL_AMT + <L_VBRP_2>-KZWI4.
            LF_TOTAL_GAMT = LF_TOTAL_GAMT + <L_VBRP_2>-KZWI1.
          ELSE.
            LF_TOTAL_AMT  = LF_TOTAL_AMT + <L_VBRP_2>-KZWI3.
            LF_TOTAL_GAMT = LF_TOTAL_GAMT + <L_VBRP_2>-KZWI3.
          ENDIF.
          LF_TOTAL_QTY  = LF_TOTAL_QTY + <L_VBRP_2>-FKIMG.
          LF_TOTAL_TAX  = LF_TOTAL_TAX + <L_VBRP_2>-MWSBP.
        ENDLOOP.

        LS_ITEM-UNIT_PRICE      = LF_TOTAL_GAMT.
        LS_ITEM-QUANTITY        = 1.
        LS_ITEM-DISCOUNT        = LF_TOTAL_GAMT - LF_TOTAL_AMT.
        LS_ITEM-NET_UNIT_PRICE  = LF_TOTAL_GAMT - LS_ITEM-DISCOUNT.
        LS_ITEM-AMOUNT          = LF_TOTAL_AMT.
        LS_ITEM-TAX_AMOUNT      = LF_TOTAL_TAX.
        APPEND LS_ITEM TO UT_ITEM.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.

  ELSE. "Text ZH16 not found
    "Loop at main item
    LOOP AT LT_VBRP ASSIGNING <L_VBRP> WHERE UEPOS IS INITIAL.
      CLEAR: LS_ITEM,
             LS_BOM.

      IF <L_VBRP>-MATNR IN GRT_MAT_NOT_SHOW_DESC.
        CONTINUE.
      ENDIF.

      LF_INDEX                = LF_INDEX + 1.
      LS_ITEM-CREDIT_NOTE_NO  = <L_VBRP>-VBELN.
      LS_ITEM-ITEM_NO         = <L_VBRP>-POSNR.
      LS_ITEM-ITEM_NO_TEXT    = LF_INDEX.
      LS_ITEM-MODEL           = <L_VBRP>-MATNR.

      PERFORM F_READ_MAT_DESC USING US_CREDIT_NOTE
                                    <L_VBRP>
                                    LS_ITEM-MODEL_DESC.

      PERFORM F_READ_QUANTITY USING US_CREDIT_NOTE
                                    <L_VBRP>
                                    LS_ITEM-QUANTITY.

      PERFORM F_READ_UNIT USING US_CREDIT_NOTE
                                <L_VBRP>
                                LS_ITEM-SALES_UNIT
                                LS_ITEM-SALES_UNIT_TEXT.

      APPEND LS_ITEM TO UT_ITEM.

      "Sum Qty, Price from BOM level
      CLEAR:  LS_BOM,
              LF_TOTAL_UP,
              LF_TOTAL_DC,
              LF_TOTAL_NUP,
              LF_TOTAL_AMT,
              LF_TOTAL_TAX.
      LOOP AT LT_VBRP ASSIGNING <L_BOM> WHERE UEPOS EQ <L_VBRP>-POSNR. "#EC CI_NESTED

        IF <L_BOM>-MATNR IN GRT_MAT_NOT_SHOW_DESC.
          CONTINUE.
        ENDIF.

        LS_BOM-CREDIT_NOTE_NO = <L_BOM>-VBELN.
        LS_BOM-ITEM_NO        = <L_BOM>-POSNR.
        LS_BOM-HI_LEVEL_ITEM  = <L_BOM>-UEPOS.
        LF_QTY                = <L_BOM>-FKIMG.
        IF LS_BOM-MODEL_DESC IS INITIAL.
          LS_BOM-MODEL_DESC  = |{ <L_BOM>-MATNR }({ LF_QTY })|.
        ELSE.
          LS_BOM-MODEL_DESC  = |{ LS_BOM-MODEL_DESC }+{ <L_BOM>-MATNR }({ LF_QTY })|.
        ENDIF.

        IF LF_WITHOUT_REF IS INITIAL.
          LF_TOTAL_AMT  = LF_TOTAL_AMT + <L_BOM>-KZWI4 .
          IF NOT <L_BOM>-FKIMG IS INITIAL.
            LF_TOTAL_UP = LF_TOTAL_UP  + ( <L_BOM>-KZWI1 / <L_VBRP>-FKIMG ).
            LF_TOTAL_DC = LF_TOTAL_DC  + ( ( <L_BOM>-KZWI1 - <L_BOM>-KZWI4 ) / <L_VBRP>-FKIMG ).
          ENDIF.
        ELSE.
          LF_TOTAL_AMT  = LF_TOTAL_AMT + <L_BOM>-KZWI3 .
          IF NOT <L_BOM>-FKIMG IS INITIAL.
            LF_TOTAL_UP = LF_TOTAL_UP  + ( <L_BOM>-KZWI3 / <L_VBRP>-FKIMG ).
          ENDIF.
        ENDIF.
        LF_TOTAL_TAX  = LF_TOTAL_TAX + <L_BOM>-MWSBP.
      ENDLOOP.
      LF_TOTAL_NUP  = LF_TOTAL_UP - LF_TOTAL_DC.

      IF NOT LS_BOM IS INITIAL. "has BOM
        APPEND LS_BOM TO UT_ITEM.

        "Update prices at main item using total from BOM
        READ TABLE UT_ITEM ASSIGNING <L_ITEM>
        WITH KEY  CREDIT_NOTE_NO  = LS_BOM-CREDIT_NOTE_NO
                  ITEM_NO         = LS_BOM-HI_LEVEL_ITEM.
        IF SY-SUBRC EQ 0.
          <L_ITEM>-UNIT_PRICE     = LF_TOTAL_UP.
          <L_ITEM>-DISCOUNT       = LF_TOTAL_DC.
          <L_ITEM>-NET_UNIT_PRICE = LF_TOTAL_NUP.
          <L_ITEM>-AMOUNT         = LF_TOTAL_AMT.
          <L_ITEM>-TAX_AMOUNT     = LF_TOTAL_TAX.
        ENDIF.
      ELSE.
        "no BOM
        "Update prices at main item
        READ TABLE UT_ITEM ASSIGNING <L_ITEM>
        WITH KEY  CREDIT_NOTE_NO  = <L_VBRP>-VBELN
                  ITEM_NO         = <L_VBRP>-POSNR.
        IF SY-SUBRC EQ 0.
          IF LF_WITHOUT_REF IS INITIAL.
            <L_ITEM>-AMOUNT = <L_VBRP>-KZWI4.
            IF NOT <L_VBRP>-FKIMG IS INITIAL.
              <L_ITEM>-UNIT_PRICE = <L_VBRP>-KZWI1 / <L_VBRP>-FKIMG.
              <L_ITEM>-DISCOUNT   = ( <L_VBRP>-KZWI1 - <L_VBRP>-KZWI4 ) / <L_VBRP>-FKIMG.
            ENDIF.
          ELSE.
            <L_ITEM>-AMOUNT = <L_VBRP>-KZWI3.
            IF NOT <L_VBRP>-FKIMG IS INITIAL.
              <L_ITEM>-UNIT_PRICE = <L_VBRP>-KZWI3 / <L_VBRP>-FKIMG.
            ENDIF.
          ENDIF.
          <L_ITEM>-TAX_AMOUNT     = <L_VBRP>-MWSBP.
          <L_ITEM>-NET_UNIT_PRICE = <L_ITEM>-UNIT_PRICE - <L_ITEM>-DISCOUNT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Append line Sue Charge
  IF NOT GRT_SUECHARG IS INITIAL.
    LOOP AT LT_PRCD_ELEM_INV ASSIGNING <L_PRCD_ELEM>.
      IF <L_PRCD_ELEM>-KSCHL IN GRT_SUECHARG.
        LF_TOTAL_SUECHARG = LF_TOTAL_SUECHARG + <L_PRCD_ELEM>-KWERT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF NOT LF_TOTAL_SUECHARG IS INITIAL.
    CLEAR LS_ITEM.
    LF_INDEX              = LF_INDEX + 1.
    LS_ITEM-ITEM_NO_TEXT  = LF_INDEX.
    LS_ITEM-MODEL_DESC    = GF_SUECHARG_DESC.
    LS_ITEM-AMOUNT        = LF_TOTAL_SUECHARG.
    APPEND LS_ITEM TO UT_ITEM.
  ENDIF.

  "calculate totals in footer
  PERFORM F_READ_INVOICE_AMOUNT USING US_CREDIT_NOTE
                                      LT_VBRP_INV
                                      US_FOOTER-INVOICE_AMOUNT.

  "calculate CN amount
  LOOP AT LT_VBRP_CN ASSIGNING <L_VBRP>.
*BOD CH01
*    IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CN_DOCTYPE
*      OR US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CR_DOCTYPE. "<<F36K910460++
*      LF_PRICE_REDUCE  = LF_PRICE_REDUCE + <L_VBRP>-KZWI5.
*    ELSE.
*      LF_PRICE_REDUCE  = LF_PRICE_REDUCE + <L_VBRP>-KZWI4.
*    ENDIF.
*EOD CH01
    US_FOOTER-PRICE_REDUCE  = US_FOOTER-PRICE_REDUCE + <L_VBRP>-NETWR. "CH01 ADD
    US_FOOTER-VALUE_ADD_TAX = US_FOOTER-VALUE_ADD_TAX + <L_VBRP>-MWSBP.
  ENDLOOP.

*BOD CH01
*  IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CN_DOCTYPE
*    OR US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CR_DOCTYPE. "<<F36K910460++
*    US_FOOTER-PRICE_REDUCE  = LF_PRICE_REDUCE.
*    US_FOOTER-TOTAL_AMOUNT  = US_FOOTER-INVOICE_AMOUNT - LF_PRICE_REDUCE.
*  ENDIF.
*  US_FOOTER-NET_AMOUNT  = LF_PRICE_REDUCE + US_FOOTER-VALUE_ADD_TAX.
*EOD CH01
*BOI CH01
  US_FOOTER-TOTAL_AMOUNT  = US_FOOTER-INVOICE_AMOUNT - US_FOOTER-PRICE_REDUCE.
  US_FOOTER-NET_AMOUNT    = US_FOOTER-PRICE_REDUCE + US_FOOTER-VALUE_ADD_TAX.
*EOI CH01
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GENERATE_PDF
*&---------------------------------------------------------------------*
*& Generate PDF file
*&---------------------------------------------------------------------*
FORM F_GENERATE_PDF  USING  US_CREDIT_NOTE  TYPE LBBIL_INVOICE
                            US_JOB_INFO     TYPE SSFCRESCL
                            UF_SUBRC        TYPE SY-SUBRC.
  TYPES:
    BEGIN OF LTS_VBRK,
      GJAHR TYPE VBRK-GJAHR,
      XBLNR TYPE VBRK-XBLNR,
    END OF LTS_VBRK.

  DATA: LT_LINE         TYPE STANDARD TABLE OF TLINE.
  DATA: LT_DATABIN      TYPE ZCL_SDSCA_FILE_INTERFACE=>TT_DATABIN.
  DATA: LS_RETURN       TYPE BAPIRET2.
  DATA: LS_VBRK         TYPE LTS_VBRK.
  DATA: LF_BIN_FILESIZE TYPE I  ##NEEDED.
  DATA: LF_BIN_FILE     TYPE XSTRING.
  DATA: LF_FILENAME     TYPE STRING.
  DATA: LF_LANG         TYPE CHAR2.
  DATA: LF_DOCTY        TYPE ZSDSSDC017-DOCTY.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      FORMAT                = 'PDF'
    IMPORTING
      BIN_FILESIZE          = LF_BIN_FILESIZE
      BIN_FILE              = LF_BIN_FILE
    TABLES
      OTF                   = US_JOB_INFO-OTFDATA
      LINES                 = LT_LINE
    EXCEPTIONS
      ERR_MAX_LINEWIDTH     = 1
      ERR_FORMAT            = 2
      ERR_CONV_NOT_POSSIBLE = 3
      ERR_BAD_OTF           = 4
      OTHERS                = 5.
  IF SY-SUBRC <> 0.
    UF_SUBRC  = SY-SUBRC.
    PERFORM F_PROTOCOL_UPDATE.
    RETURN.
  ENDIF.

  IF NOT LF_BIN_FILE IS INITIAL.
    SELECT SINGLE FROM VBRK
    FIELDS GJAHR,
           XBLNR
    WHERE VBELN EQ @US_CREDIT_NOTE-HD_GEN-BIL_NUMBER
    INTO @LS_VBRK.

    IF NAST-SPRAS EQ 'E'.
      LF_LANG = 'EN'.
    ELSEIF NAST-SPRAS EQ '2'.
      LF_LANG = 'TH'.
    ENDIF.

    SELECT SINGLE FROM ZSDSSDC017
    FIELDS DOCTY
    WHERE FKART EQ @US_CREDIT_NOTE-HD_GEN-BIL_TYPE
    INTO @LF_DOCTY.

    LF_FILENAME = |B{ LF_DOCTY }_{ LF_LANG }_{ SY-DATUM }_{ SY-UZEIT }|.
    LF_FILENAME = |{ LF_FILENAME }_{ US_CREDIT_NOTE-HD_ORG-COMP_CODE }|.
    LF_FILENAME = |{ LF_FILENAME }{ LS_VBRK-XBLNR }{ LS_VBRK-GJAHR }{ TEXT-T10 }|.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        BUFFER     = LF_BIN_FILE
      TABLES
        BINARY_TAB = LT_DATABIN.

    CALL METHOD ZCL_SDSCA_FILE_INTERFACE=>CREATE_INTERFACE_FILE
      EXPORTING
        IF_INTFNO   = GC_INTFNO
        IF_FILENAME = LF_FILENAME
        IT_DATABIN  = LT_DATABIN
      IMPORTING
        ES_RETURN   = LS_RETURN.

    IF LS_RETURN-TYPE NE 'S'.
*     Error
      MESSAGE ID LS_RETURN-ID TYPE 'I'
              NUMBER LS_RETURN-NUMBER
              DISPLAY LIKE LS_RETURN-TYPE
              WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                   LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_MAT_DESC
*&---------------------------------------------------------------------*
*& Read Material Description
*&---------------------------------------------------------------------*
FORM F_READ_MAT_DESC  USING US_CREDIT_NOTE  TYPE LBBIL_INVOICE
                            US_VBRP         TYPE TS_VBRP
                            UF_MODEL_DESC   TYPE ZSDSSDS030-MODEL_DESC.
  DATA: LT_LINE     TYPE STANDARD TABLE OF TLINE.
  DATA: LS_LINE     LIKE LINE OF LT_LINE.
  DATA: LF_TDID     TYPE THEAD-TDID.
  DATA: LF_TDNAME   TYPE THEAD-TDNAME.
  DATA: LF_TDOBJECT TYPE THEAD-TDOBJECT.

  LF_TDID     = 'ZI06'.
  LF_TDNAME   = |{ US_CREDIT_NOTE-HD_GEN-BIL_NUMBER }{ US_VBRP-POSNR }|.
  LF_TDOBJECT = 'VBBP'.

  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      ID                      = LF_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LF_TDNAME
      OBJECT                  = LF_TDOBJECT
    TABLES
      LINES                   = LT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        ID                      = LF_TDID
        LANGUAGE                = '2'
        NAME                    = LF_TDNAME
        OBJECT                  = LF_TDOBJECT
      TABLES
        LINES                   = LT_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.

  READ TABLE LT_LINE INTO LS_LINE INDEX 1.
  IF SY-SUBRC EQ 0.
    UF_MODEL_DESC  = LS_LINE-TDLINE.
  ELSE.
    UF_MODEL_DESC  = US_VBRP-ARKTX.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_QUANTITY
*&---------------------------------------------------------------------*
*& Read Quantity
*&---------------------------------------------------------------------*
FORM F_READ_QUANTITY  USING  US_CREDIT_NOTE TYPE LBBIL_INVOICE
                             US_VBRP        TYPE TS_VBRP
                             UF_QUANTITY    TYPE ZSDSSDS030-QUANTITY.
  DATA: LT_LINE     TYPE STANDARD TABLE OF TLINE.
  DATA: LS_LINE     LIKE LINE OF LT_LINE.
  DATA: LF_TDID     TYPE THEAD-TDID.
  DATA: LF_TDNAME   TYPE THEAD-TDNAME.
  DATA: LF_TDOBJECT TYPE THEAD-TDOBJECT.

  LF_TDID     = 'ZI05'.
  LF_TDNAME   = |{ US_CREDIT_NOTE-HD_GEN-BIL_NUMBER }{ US_VBRP-POSNR }|.
  LF_TDOBJECT = 'VBBP'.

  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      ID                      = LF_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LF_TDNAME
      OBJECT                  = LF_TDOBJECT
    TABLES
      LINES                   = LT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        ID                      = LF_TDID
        LANGUAGE                = '2'
        NAME                    = LF_TDNAME
        OBJECT                  = LF_TDOBJECT
      TABLES
        LINES                   = LT_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.

  READ TABLE LT_LINE INTO LS_LINE INDEX 1.
  IF SY-SUBRC EQ 0.
    UF_QUANTITY  = LS_LINE-TDLINE.
  ELSE.
    UF_QUANTITY  = US_VBRP-FKIMG.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_UNIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_READ_UNIT  USING US_CREDIT_NOTE      TYPE LBBIL_INVOICE
                        US_VBRP             TYPE TS_VBRP
                        UF_SALES_UNIT       TYPE ZSDSSDS030-SALES_UNIT
                        UF_SALES_UNIT_TEXT  TYPE ZSDSSDS030-SALES_UNIT_TEXT.
  DATA: LT_LINE     TYPE STANDARD TABLE OF TLINE.
  DATA: LS_LINE     LIKE LINE OF LT_LINE.
  DATA: LF_TDID     TYPE THEAD-TDID.
  DATA: LF_TDNAME   TYPE THEAD-TDNAME.
  DATA: LF_TDOBJECT TYPE THEAD-TDOBJECT.

  LF_TDID     = 'ZI04'.
  LF_TDNAME   = |{ US_CREDIT_NOTE-HD_GEN-BIL_NUMBER }{ US_VBRP-POSNR }|.
  LF_TDOBJECT = 'VBBP'.

  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      ID                      = LF_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LF_TDNAME
      OBJECT                  = LF_TDOBJECT
    TABLES
      LINES                   = LT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        ID                      = LF_TDID
        LANGUAGE                = '2'
        NAME                    = LF_TDNAME
        OBJECT                  = LF_TDOBJECT
      TABLES
        LINES                   = LT_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.

  READ TABLE LT_LINE INTO LS_LINE INDEX 1.
  IF SY-SUBRC EQ 0.
    UF_SALES_UNIT_TEXT  = LS_LINE-TDLINE.
  ELSE.
    UF_SALES_UNIT = US_VBRP-VRKME.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_MERGE_ITEMS
*&---------------------------------------------------------------------*
*& Read Merge Items
*&---------------------------------------------------------------------*
FORM F_READ_MERGE_ITEMS  USING US_CREDIT_NOTE TYPE LBBIL_INVOICE
                               UF_FOUND_ZH16  TYPE FLAG.
  DATA: LT_LINE     TYPE STANDARD TABLE OF TLINE.
  DATA: LS_LINE     LIKE LINE OF LT_LINE  ##NEEDED.
  DATA: LF_TDID     TYPE THEAD-TDID.
  DATA: LF_TDNAME   TYPE THEAD-TDNAME.
  DATA: LF_TDOBJECT TYPE THEAD-TDOBJECT.

  LF_TDID     = 'ZH16'.
  LF_TDNAME   = US_CREDIT_NOTE-HD_GEN-BIL_NUMBER.
  LF_TDOBJECT = 'VBBK'.

  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      ID                      = LF_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LF_TDNAME
      OBJECT                  = LF_TDOBJECT
    TABLES
      LINES                   = LT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        ID                      = LF_TDID
        LANGUAGE                = '2'
        NAME                    = LF_TDNAME
        OBJECT                  = LF_TDOBJECT
      TABLES
        LINES                   = LT_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.

  READ TABLE LT_LINE INTO LS_LINE INDEX 1.
  IF SY-SUBRC EQ 0.
    UF_FOUND_ZH16  = ABAP_TRUE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_INVOICE_AMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_READ_INVOICE_AMOUNT  USING  US_CREDIT_NOTE     TYPE LBBIL_INVOICE
                                   UT_VBRP            TYPE TT_VBRP
                                   UF_INVOICE_AMOUNT  TYPE ZSDSSDS029-INVOICE_AMOUNT.
  DATA: LT_LINE     TYPE STANDARD TABLE OF TLINE.
  DATA: LS_LINE     LIKE LINE OF LT_LINE.
  DATA: LF_TDID     TYPE THEAD-TDID.
  DATA: LF_TDNAME   TYPE THEAD-TDNAME.
  DATA: LF_TDOBJECT TYPE THEAD-TDOBJECT.

  LF_TDID     = 'ZH27'.
  LF_TDNAME   = |{ US_CREDIT_NOTE-HD_GEN-BIL_NUMBER }|.
  LF_TDOBJECT = 'VBBK'.

  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      ID                      = LF_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LF_TDNAME
      OBJECT                  = LF_TDOBJECT
    TABLES
      LINES                   = LT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        ID                      = LF_TDID
        LANGUAGE                = '2'
        NAME                    = LF_TDNAME
        OBJECT                  = LF_TDOBJECT
      TABLES
        LINES                   = LT_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.

  READ TABLE LT_LINE INTO LS_LINE INDEX 1.
  IF SY-SUBRC EQ 0.
    UF_INVOICE_AMOUNT  = LS_LINE-TDLINE.
  ELSE.
    "calculate invoice amount
    LOOP AT UT_VBRP ASSIGNING FIELD-SYMBOL(<L_VBRP>).
      UF_INVOICE_AMOUNT  = UF_INVOICE_AMOUNT + <L_VBRP>-NETWR.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_INVOICE_NO_DATE
*&---------------------------------------------------------------------*
*& Read Invoice no. and Date
*&---------------------------------------------------------------------*
FORM F_READ_INVOICE_NO_DATE  USING  US_CREDIT_NOTE  TYPE LBBIL_INVOICE
                                    UF_INVOICE_NO   TYPE ZSDSSDS028-INVOICE_NO
                                    UF_INVOICE_DATE TYPE ZSDSSDS028-INVOICE_DATE.
  TYPES: BEGIN OF LTS_VBFA,
           VBELV TYPE VBFA-VBELV,
           POSNV TYPE VBFA-POSNV,
           VBELN TYPE VBFA-VBELN,
           POSNN TYPE VBFA-POSNN,
         END OF LTS_VBFA.

  TYPES: BEGIN OF LTS_VBRK,
           FKDAT TYPE VBRK-FKDAT,
           XBLNR TYPE VBRK-XBLNR,
         END OF LTS_VBRK.

  DATA: LT_VBFA       TYPE STANDARD TABLE OF LTS_VBFA.
  DATA: LT_LINE       TYPE STANDARD TABLE OF TLINE.
  DATA: LS_VBFA       LIKE LINE OF LT_VBFA.
  DATA: LS_LINE       LIKE LINE OF LT_LINE.
  DATA: LS_VBRK       TYPE LTS_VBRK.
  DATA: LF_TDID       TYPE THEAD-TDID.
  DATA: LF_TDNAME     TYPE THEAD-TDNAME.
  DATA: LF_TDOBJECT   TYPE THEAD-TDOBJECT.
  DATA: LF_VBELN_INV  TYPE VBRK-VBELN.
  DATA: LF_XBLNR_INV  TYPE VBRK-XBLNR.
  DATA: LF_FKDAT_INV  TYPE CHAR12.

  "Read invoice no.
  LF_TDID     = 'ZH26'.
  LF_TDNAME   = |{ US_CREDIT_NOTE-HD_GEN-BIL_NUMBER }|.
  LF_TDOBJECT = 'VBBK'.

  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      ID                      = LF_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LF_TDNAME
      OBJECT                  = LF_TDOBJECT
    TABLES
      LINES                   = LT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        ID                      = LF_TDID
        LANGUAGE                = '2'
        NAME                    = LF_TDNAME
        OBJECT                  = LF_TDOBJECT
      TABLES
        LINES                   = LT_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.
  READ TABLE LT_LINE INTO LS_LINE INDEX 1.
  IF SY-SUBRC EQ 0.
    UF_INVOICE_NO  = LS_LINE-TDLINE.
  ENDIF.

  "Read invoice date
  CLEAR LT_LINE.
  LF_TDID     = 'ZH28'.
  LF_TDNAME   = |{ US_CREDIT_NOTE-HD_GEN-BIL_NUMBER }|.
  LF_TDOBJECT = 'VBBK'.

  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      ID                      = LF_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LF_TDNAME
      OBJECT                  = LF_TDOBJECT
    TABLES
      LINES                   = LT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        ID                      = LF_TDID
        LANGUAGE                = '2'
        NAME                    = LF_TDNAME
        OBJECT                  = LF_TDOBJECT
      TABLES
        LINES                   = LT_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.
  READ TABLE LT_LINE INTO LS_LINE INDEX 1.
  IF SY-SUBRC EQ 0.
    UF_INVOICE_DATE  = LS_LINE-TDLINE.
  ENDIF.

  IF UF_INVOICE_NO IS INITIAL OR
     UF_INVOICE_DATE IS INITIAL.

    "Read invoice no. from ref doc
    IF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CN_DOCTYPE
      OR US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CR_DOCTYPE. "<<F36K910460++
      IF NOT US_CREDIT_NOTE-HD_REF-VBELN_VG2 IS INITIAL.
        SELECT SINGLE FROM VBRK
        FIELDS FKDAT,
               XBLNR
        WHERE VBELN EQ @US_CREDIT_NOTE-HD_REF-VBELN_VG2
        INTO @LS_VBRK.
        IF SY-SUBRC EQ 0.
          LF_XBLNR_INV = LS_VBRK-XBLNR.
          PERFORM F_WRITE_DATE  USING LS_VBRK-FKDAT
                                      LF_FKDAT_INV.
        ENDIF.
      ELSE.
        "Read invoice no. from doc flow
        IF NOT US_CREDIT_NOTE-HD_REF-ORDER_NUMB IS INITIAL.
          "get invoice no.
          SELECT FROM VBFA
          FIELDS VBELV,
                 POSNV,
                 VBELN,
                 POSNN
          WHERE VBELN   EQ @US_CREDIT_NOTE-HD_REF-ORDER_NUMB
            AND VBTYP_N EQ 'L'
            AND VBTYP_V EQ 'M'
          INTO TABLE @LT_VBFA.
          IF SY-SUBRC EQ 0.
            READ TABLE LT_VBFA INTO LS_VBFA INDEX 1.    "#EC CI_NOORDER
            IF SY-SUBRC EQ 0.
              LF_VBELN_INV  = LS_VBFA-VBELV.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT LF_VBELN_INV IS INITIAL.
          SELECT SINGLE FROM VBRK
          FIELDS FKDAT,
                 XBLNR
          WHERE VBELN EQ @LF_VBELN_INV
          INTO @LS_VBRK.
          IF SY-SUBRC EQ 0.
            LF_XBLNR_INV = LS_VBRK-XBLNR.
            PERFORM F_WRITE_DATE  USING LS_VBRK-FKDAT
                                        LF_FKDAT_INV.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF US_CREDIT_NOTE-HD_GEN-BIL_TYPE IN GRT_CANCEL_SERVICE_DOCTYPE. "Cancel Service invoice
      IF NOT US_CREDIT_NOTE-HD_GEN-SFAKN IS INITIAL.
        SELECT SINGLE FROM VBRK
        FIELDS FKDAT,
               XBLNR
        WHERE VBELN EQ @US_CREDIT_NOTE-HD_GEN-SFAKN
        INTO @LS_VBRK.
        IF SY-SUBRC EQ 0.
          LF_XBLNR_INV = LS_VBRK-XBLNR.
          PERFORM F_WRITE_DATE  USING LS_VBRK-FKDAT
                                      LF_FKDAT_INV.
        ENDIF.
      ENDIF.
    ENDIF.

    IF UF_INVOICE_NO IS INITIAL.
      UF_INVOICE_NO = LF_XBLNR_INV.
    ENDIF.
    IF UF_INVOICE_DATE IS INITIAL.
      UF_INVOICE_DATE = LF_FKDAT_INV.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PRINT_PARAM
*&---------------------------------------------------------------------*
*& Set Print Parameters
*&---------------------------------------------------------------------*
FORM F_SET_PRINT_PARAM  USING     US_ADDR_KEY       TYPE ADDR_KEY
                                  US_DLV_LAND       TYPE VBRK-LAND1
                                  UF_RETCODE        TYPE SY-SUBRC ##NEEDED
                                  US_PROC_SCREEN    TYPE C
                        CHANGING  CS_CONTROL_PARAM  TYPE SSFCTRLOP
                                  CS_COMPOSER_PARAM TYPE SSFCOMPOP  ##NEEDED
                                  CS_RECIPIENT      TYPE SWOTOBJID
                                  CS_SENDER         TYPE SWOTOBJID
                                  CF_RETCODE        TYPE SY-SUBRC.

  DATA: LS_ITCPO     TYPE ITCPO.
  DATA: LF_REPID     TYPE SY-REPID.
  DATA: LF_DEVICE    TYPE TDDEVICE  ##NEEDED.

  CONSTANTS : BEGIN OF LC_CON,
                ZVF31 TYPE C LENGTH 9 VALUE 'ZSDSSD001',
                VF31  TYPE C LENGTH 4 VALUE 'VF31',
              END OF LC_CON.

  LF_REPID = GC_REPID.

  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
    EXPORTING
      PI_NAST       = NAST
      PI_COUNTRY    = US_DLV_LAND
      PI_ADDR_KEY   = US_ADDR_KEY
      PI_REPID      = LF_REPID
      PI_SCREEN     = GF_XSCREEN
    IMPORTING
      PE_RETURNCODE = CF_RETCODE
      PE_ITCPO      = LS_ITCPO
      PE_DEVICE     = LF_DEVICE
      PE_RECIPIENT  = CS_RECIPIENT
      PE_SENDER     = CS_SENDER.

  IF CF_RETCODE = 0.
    IF US_PROC_SCREEN  EQ SPACE.
      MOVE-CORRESPONDING LS_ITCPO TO CS_COMPOSER_PARAM.

      SELECT COUNT( * )
        FROM TSP03D
        WHERE PADEST   EQ CS_COMPOSER_PARAM-TDDEST
          AND PAMETHOD EQ 'S'.
      IF SY-SUBRC EQ 0.
        CS_CONTROL_PARAM-NO_DIALOG   = ABAP_TRUE.
      ENDIF.

      IF SY-TCODE EQ LC_CON-ZVF31 OR
         SY-TCODE EQ LC_CON-VF31.
        CS_CONTROL_PARAM-NO_DIALOG   = ABAP_TRUE.
      ENDIF.
    ELSE.
      CS_CONTROL_PARAM-PREVIEW     = ABAP_TRUE.
    ENDIF.
    MOVE-CORRESPONDING LS_ITCPO TO CS_COMPOSER_PARAM.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_READ_REMARK_SERVICE
*&---------------------------------------------------------------------*
*& Read Remark for Service Invoice
*&---------------------------------------------------------------------*
FORM F_READ_REMARK_SERVICE USING  US_CREDIT_NOTE TYPE LBBIL_INVOICE
                                  UF_REMARK      TYPE ZSDSSDS029-REMARK.
  DATA: LT_LINE     TYPE STANDARD TABLE OF TLINE.
  DATA: LS_LINE     LIKE LINE OF LT_LINE.
  DATA: LF_TDID     TYPE THEAD-TDID.
  DATA: LF_TDNAME   TYPE THEAD-TDNAME.
  DATA: LF_TDOBJECT TYPE THEAD-TDOBJECT.

  LF_TDID     = 'ZH13'.
  LF_TDNAME   = |{ US_CREDIT_NOTE-HD_GEN-SFAKN }|.
  LF_TDOBJECT = 'VBBK'.

  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      ID                      = LF_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LF_TDNAME
      OBJECT                  = LF_TDOBJECT
    TABLES
      LINES                   = LT_LINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        ID                      = LF_TDID
        LANGUAGE                = '2'
        NAME                    = LF_TDNAME
        OBJECT                  = LF_TDOBJECT
      TABLES
        LINES                   = LT_LINE
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.

  READ TABLE LT_LINE INTO LS_LINE INDEX 1.
  IF SY-SUBRC EQ 0.
    UF_REMARK  = LS_LINE-TDLINE.
  ENDIF.
ENDFORM.
