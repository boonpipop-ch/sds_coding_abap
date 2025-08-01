*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0040
*  Creation Date      : 25.03.2024
*  Author             : Jakarin Sirilertlak
*  Add-on ID          : ZFIARF008
*  Description        : Print AR Receipt
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
REPORT ZSDSSDR0040 MESSAGE-ID ZSDSSD01.
INCLUDE ZSDSSDR0040_TOP.
*---------------------------------------------------------------------*
*       FORM ENTRY
*---------------------------------------------------------------------*
FORM ENTRY USING UV_PROC_SCREEN
                 UV_RETCODE.

  DATA : LV_NO_DIALOG TYPE C.

  LV_NO_DIALOG = SPACE.

* for call by other program.
  PERFORM F_PRINT USING UV_PROC_SCREEN
                        UV_RETCODE
                        NAST
                        SPACE
                        TNAPR-SFORM
                        LV_NO_DIALOG.

ENDFORM.                    "ENTRY
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA USING IS_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ
             CHANGING CS_ADDR_KEY           LIKE ADDR_KEY
                      LS_DLV_LAND           LIKE VBRK-LAND1
                      CS_BIL_INVOICE        TYPE LBBIL_INVOICE
                      CF_RETCODE.

  IF NAST-OBJKY+10 NE SPACE.
    NAST-OBJKY = NAST-OBJKY+16(10).
  ELSE.
    NAST-OBJKY = NAST-OBJKY.
  ENDIF.
* read print data
  CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
    EXPORTING
      IF_BIL_NUMBER         = NAST-OBJKY
      IF_PARVW              = NAST-PARVW
      IF_PARNR              = NAST-PARNR
      IF_LANGUAGE           = NAST-SPRAS
      IS_PRINT_DATA_TO_READ = IS_PRINT_DATA_TO_READ
    IMPORTING
      ES_BIL_INVOICE        = CS_BIL_INVOICE
    EXCEPTIONS
      RECORDS_NOT_FOUND     = 1
      RECORDS_NOT_REQUESTED = 2
      OTHERS                = 3.
  IF SY-SUBRC <> 0.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PRINT_DATA_TO_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LF_FORMNAME
*&      <-- LS_PRINT_DATA_TO_READ
*&      <-- CF_RETCODE
*&---------------------------------------------------------------------*
FORM SET_PRINT_DATA_TO_READ USING IF_FORMNAME LIKE TNAPR-SFORM
                         CHANGING CS_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ
                                  CF_RETCODE.

  FIELD-SYMBOLS: <FS_PRINT_DATA_TO_READ> TYPE XFELD.
  DATA: LT_FIELDLIST TYPE TSFFIELDS.

* set print data requirements
  DO.
    ASSIGN COMPONENT SY-INDEX OF STRUCTURE
                     CS_PRINT_DATA_TO_READ TO <FS_PRINT_DATA_TO_READ>.
    IF SY-SUBRC <> 0. EXIT. ENDIF.
    <FS_PRINT_DATA_TO_READ> = 'X'.
  ENDDO.

  CALL FUNCTION 'SSF_FIELD_LIST'
    EXPORTING
      FORMNAME           = IF_FORMNAME
*     VARIANT            = ' '
    IMPORTING
      FIELDLIST          = LT_FIELDLIST
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
*  error handling
    CF_RETCODE = SY-SUBRC.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.       *
*---------------------------------------------------------------------*
FORM PROTOCOL_UPDATE.
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

ENDFORM.                    "PROTOCOL_UPDATE
*&---------------------------------------------------------------------*
*& Form SET_PRINT_PARAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_ADDR_KEY
*&      --> LS_DLV_LAND
*&      <-- LS_CONTROL_PARAM
*&      <-- LS_COMPOSER_PARAM
*&      <-- LS_RECIPIENT
*&      <-- LS_SENDER
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM SET_PRINT_PARAM  USING   IS_ADDR_KEY LIKE ADDR_KEY
                              IS_DLV_LAND LIKE VBRK-LAND1
                              UV_RETCODE
                              UF_NODIALOG TYPE C
                     CHANGING CS_CONTROL_PARAM TYPE SSFCTRLOP
                              CS_COMPOSER_PARAM TYPE SSFCOMPOP
                              CS_RECIPIENT TYPE  SWOTOBJID
                              CS_SENDER TYPE  SWOTOBJID
                              CF_RETCODE TYPE SY-SUBRC.

  DATA: LS_ITCPO     TYPE ITCPO.
  DATA: LF_REPID     TYPE SY-REPID.
  DATA: LF_DEVICE    TYPE TDDEVICE.
  DATA: LS_RECIPIENT TYPE SWOTOBJID.
  DATA: LS_SENDER    TYPE SWOTOBJID.

  CONSTANTS : BEGIN OF LC_CON,
                ZVF31 TYPE C LENGTH 9 VALUE 'ZSDSSD001',
                VF31  TYPE C LENGTH 4 VALUE 'VF31',
              END OF LC_CON.

  LF_REPID = SY-REPID.

  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
    EXPORTING
      PI_NAST       = NAST
      PI_COUNTRY    = IS_DLV_LAND
      PI_ADDR_KEY   = IS_ADDR_KEY
      PI_REPID      = LF_REPID
      PI_SCREEN     = XSCREEN
    IMPORTING
      PE_RETURNCODE = CF_RETCODE
      PE_ITCPO      = LS_ITCPO
      PE_DEVICE     = LF_DEVICE
      PE_RECIPIENT  = CS_RECIPIENT
      PE_SENDER     = CS_SENDER.

  IF CF_RETCODE = 0.
    IF UV_RETCODE EQ SPACE.
      MOVE-CORRESPONDING LS_ITCPO TO CS_COMPOSER_PARAM.

      SELECT COUNT( * )
        FROM TSP03D
        WHERE PADEST   EQ CS_COMPOSER_PARAM-TDDEST
          AND PAMETHOD EQ GC_CON-S.
      IF SY-SUBRC EQ 0.
        CS_CONTROL_PARAM-NO_DIALOG   = ABAP_TRUE.
      ENDIF.

      IF SY-TCODE EQ LC_CON-ZVF31 OR
         SY-TCODE EQ LC_CON-VF31.
        CS_CONTROL_PARAM-NO_DIALOG   = ABAP_TRUE.
      ENDIF.

      IF UF_NODIALOG EQ ABAP_TRUE.
        CS_CONTROL_PARAM-NO_DIALOG   = ABAP_TRUE.
      ENDIF.
    ELSE.
      CS_CONTROL_PARAM-PREVIEW     = ABAP_TRUE.
    ENDIF.

*   CS_CONTROL_PARAM-NO_OPEN
*   CS_CONTROL_PARAM-NO_CLOSE
*    CS_CONTROL_PARAM-DEVICE      = LF_DEVICE.
*    CS_CONTROL_PARAM-NO_DIALOG   = 'X'.
*    CS_CONTROL_PARAM-PREVIEW     = XSCREEN.
*    CS_CONTROL_PARAM-GETOTF      = LS_ITCPO-TDGETOTF.
*    CS_CONTROL_PARAM-LANGU       = NAST-SPRAS.
*   CS_CONTROL_PARAM-REPLANGU1
*   CS_CONTROL_PARAM-REPLANGU2
*   CS_CONTROL_PARAM-REPLANGU3
*   CS_CONTROL_PARAM-STARTPAGE
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MAP_DATA_INV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_BIL_INVOICE
*&---------------------------------------------------------------------*
FORM F_MAP_DATA_INV USING LS_BIL_INVOICE TYPE LBBIL_INVOICE.

  DATA : LS_DATA     LIKE GS_DATA,
         LS_DATA_TAB LIKE LINE OF GT_DATA_TAB.

  DATA : LT_SERIAL_TAB LIKE GT_DATA_TAB,
         LS_SERIAL_TAB LIKE LINE OF GT_DATA_TAB.

  DATA : LS_ADR LIKE LINE OF LS_BIL_INVOICE-HD_ADR.

  DATA : LS_GEN LIKE LINE OF LS_BIL_INVOICE-IT_GEN.

  DATA : LS_PRICE LIKE LINE OF LS_BIL_INVOICE-IT_PRICE.

  DATA : LS_HD_REF LIKE TABLE OF LS_BIL_INVOICE-HD_REF.
  DATA: US_BIL_INVOICE TYPE LBBIL_INVOICE.

  SELECT SINGLE BELNR
    FROM BKPF
    INTO @DATA(LV_FI_DOC)
    WHERE XBLNR EQ @LS_BIL_INVOICE-HD_GEN-BIL_NUMBER.
  IF SY-SUBRC EQ 0.
    GS_DATA-VBELN = LV_FI_DOC.
  ELSE.
    GS_DATA-VBELN = LS_BIL_INVOICE-HD_GEN-BIL_NUMBER.
  ENDIF.
  GS_DATA-FKDAT = LS_BIL_INVOICE-HD_GEN-BIL_DATE.
  GS_DATA-CREDT = LS_BIL_INVOICE-HD_GEN-BIL_EDATE.
  GS_DATA-KUNNR = LS_BIL_INVOICE-HD_GEN-PAYER.

  GCL_UTIL->GET_CUST_NAME( EXPORTING I_CUST_NO  = GS_DATA-KUNNR
                                     I_NATION   = GC_CON-I
                           IMPORTING E_NAME1    = GS_DATA-NAME1
                                     E_NAME2    = GS_DATA-NAME2
                                     E_NAME3    = GS_DATA-NAME3
                                     E_NAME4    = GS_DATA-NAME4 ).

  READ TABLE LS_BIL_INVOICE-HD_ADR INTO LS_ADR
  WITH KEY PARTN_ROLE = GC_CON-PARTN_ROLE.
  IF SY-SUBRC EQ 0.
    GS_DATA-PERNR = LS_ADR-ADDR_NO.
    GCL_UTIL->GET_SALE_NAME( EXPORTING I_SALE_NO   = GS_DATA-PERNR
                             IMPORTING E_SALE_NAEM = GS_DATA-SALEN ).

  ENDIF.
  GS_DATA-DUEDT = ''.
  GS_DATA-DUETT = ''.
  READ TABLE LS_BIL_INVOICE-HD_ADR INTO LS_ADR
  WITH KEY PARTN_ROLE = GC_CON-AG.
  IF SY-SUBRC EQ 0.
    GCL_UTIL->GET_ADDRESS( EXPORTING I_ADDRESS_NO           = LS_ADR-ADDR_NO
                                     I_NATION               = GC_CON-I
                           IMPORTING E_ADDRESS_CONCAT_LINE1 = GS_DATA-ADDR1
                                     E_ADDRESS_CONCAT_LINE2 = GS_DATA-ADDR2 ).
  ENDIF.

  GS_DATA-REMAK = GCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-REMARK_ID
                                                I_NAME     = GS_DATA-VBELN
                                                I_OBJECT   = GC_CON-REMARK_OBJECT
                                                I_LANGUAGE = SY-LANGU ).

  GS_DATA-AMONT = ''.
  GS_DATA-ADVAN = ''.
  GS_DATA-TOTAL = ''.
  GS_DATA-TAXAM = ''.
  GS_DATA-NETWR = ''.
*  GS_DATA-SIGNC = ''.
*  GS_DATA-SIGNR = ''.

  PERFORM F_CONVERT_AMOUNT_TO_TEXT USING GS_DATA-NETWR
                                CHANGING GS_DATA-TXTAM.

  WRITE GS_DATA-AMONT TO GS_DATA-AMOTT.
  WRITE GS_DATA-ADVAN TO GS_DATA-ADVTT.
  WRITE GS_DATA-TOTAL TO GS_DATA-TOTTT.
  WRITE GS_DATA-TAXAM TO GS_DATA-TAXTT.
  WRITE GS_DATA-NETWR TO GS_DATA-NETTT.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-AMOTT WITH ''.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-ADVTT WITH ''.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-TOTTT WITH ''.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-TAXTT WITH ''.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_DATA-NETTT WITH ''.



  PERFORM F_GET_SERAIL_TAB USING LS_BIL_INVOICE-HD_REF-DELIV_NUMB
                                 LS_BIL_INVOICE-IT_REF
                        CHANGING LT_SERIAL_TAB.
  US_BIL_INVOICE = LS_BIL_INVOICE.



  LOOP AT LS_BIL_INVOICE-IT_GEN INTO LS_GEN.

    LS_DATA_TAB-VBELN = LS_GEN-BIL_NUMBER.
    LS_DATA_TAB-POSNR = LS_GEN-ITM_NUMBER.
*          LS_DATA_TAB-POSTT = LS_DATA_TAB-POSNR.

    PERFORM F_ALPHA_OUT USING LS_DATA_TAB-POSNR
                     CHANGING LS_DATA_TAB-POSTT.

    LS_DATA_TAB-MATNR = LS_GEN-MATERIAL.
    LS_DATA_TAB-MAKTX = LS_GEN-SHORT_TEXT.
    LS_DATA_TAB-FKIMG = LS_GEN-FKIMG.
    LS_DATA_TAB-VRKME = LS_GEN-SALES_UNIT.
    WRITE LS_DATA_TAB-FKIMG TO LS_DATA_TAB-QTYTT.
    READ TABLE LS_BIL_INVOICE-IT_PRICE INTO LS_PRICE
    WITH KEY BIL_NUMBER = LS_GEN-BIL_NUMBER
             ITM_NUMBER = LS_GEN-ITM_NUMBER.
    IF SY-SUBRC EQ 0.
      LS_DATA_TAB-PRICE = LS_PRICE-NETWR.
      LS_DATA_TAB-NETWR = LS_PRICE-NETWR.
    ENDIF.

    LS_DATA_TAB-DISCT = ''.
    LS_DATA_TAB-TOTAL = ''.

    WRITE LS_DATA_TAB-PRICE TO LS_DATA_TAB-PRITT.
    WRITE LS_DATA_TAB-DISCT TO LS_DATA_TAB-DISTT.
    WRITE LS_DATA_TAB-NETWR TO LS_DATA_TAB-NETTT.
    WRITE LS_DATA_TAB-NETWR TO LS_DATA_TAB-TOTTT.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-PRITT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-DISTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-NETTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-TOTTT WITH ''.

    APPEND LS_DATA_TAB TO GT_DATA_TAB.

    CLEAR : LS_DATA_TAB.
    LOOP AT LT_SERIAL_TAB INTO LS_SERIAL_TAB WHERE VBELN EQ LS_GEN-BIL_NUMBER AND
                                                   POSNR EQ LS_GEN-ITM_NUMBER.

      LS_DATA_TAB-MATNR = LS_SERIAL_TAB-MATNR.
      LS_DATA_TAB-MAKTX = LS_SERIAL_TAB-MAKTX.
      APPEND LS_DATA_TAB TO GT_DATA_TAB.
      CLEAR : LS_DATA_TAB.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALPHA_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_TAB_POSTT
*&      <-- LS_DATA_TAB_POSTT
*&---------------------------------------------------------------------*
FORM F_ALPHA_OUT  USING UV_IN
               CHANGING CV_OUT.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = UV_IN
    IMPORTING
      OUTPUT = CV_OUT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CONVERT_AMOUNT_TO_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DATA_NETWR
*&      <-- GS_DATA_TXTAM
*&---------------------------------------------------------------------*
FORM F_CONVERT_AMOUNT_TO_TEXT  USING    UV_NETWR
                               CHANGING CV_TXTAM.

  DATA: LV_AMOUNT      TYPE BSEG-WRBTR, " Assuming bseg-wrbtr as the type for amount
        LV_TEXT        TYPE STRING,
        LV_CURRENCY    TYPE WAERS, " Currency key
        LV_LANGUAGE    LIKE SY-LANGU VALUE '2',
        LT_SPELL_LINES TYPE TABLE OF TLINE.

  DATA: LS_IN_WORDS LIKE  SPELL.

  LV_AMOUNT   = UV_NETWR. " Example amount
  LV_CURRENCY = GC_CON-THB.    " Example currency

  IF LV_AMOUNT IS NOT INITIAL.
    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        AMOUNT   = LV_AMOUNT
        CURRENCY = LV_CURRENCY
        LANGUAGE = LV_LANGUAGE
      IMPORTING
        IN_WORDS = LS_IN_WORDS.
    IF SY-SUBRC = 0.
      CV_TXTAM = |{ LS_IN_WORDS-WORD }| & |{ SPACE }| & |{ TEXT-101 }|.
      IF LS_IN_WORDS-DECWORD IS NOT INITIAL.
        CONCATENATE CV_TXTAM LS_IN_WORDS-DECWORD TEXT-102 INTO CV_TXTAM SEPARATED BY SPACE.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_print
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SPACE
*&---------------------------------------------------------------------*
FORM F_PRINT USING UV_PROC_SCREEN
                   UV_RETCODE
                   UV_NAST
                   UV_DOCNO
                   UV_FORM_NAME
                   UF_NODIALOG  TYPE C.

  DATA: lV_SUBRC TYPE SY-SUBRC.

  DATA: I_NATION TYPE ADRC-NATION.

  DATA: LS_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ,
        LS_BIL_INVOICE        TYPE LBBIL_INVOICE,
        LF_FM_NAME            TYPE RS38L_FNAM,
        LS_CONTROL_PARAM      TYPE SSFCTRLOP,
        LS_COMPOSER_PARAM     TYPE SSFCOMPOP,
        LS_RECIPIENT          TYPE SWOTOBJID,
        LS_SENDER             TYPE SWOTOBJID,
        LF_FORMNAME           TYPE TDSFNAME,
        LS_ADDR_KEY           LIKE ADDR_KEY,
        LS_DLV_LAND           LIKE VBRK-LAND1,
        LS_JOB_INFO           TYPE SSFCRESCL.

  CONSTANTS : BEGIN OF LC_CON,
                E TYPE C LENGTH 1 VALUE 'E',
                I TYPE C LENGTH 1 VALUE 'I',
              END OF LC_CON.

  IF GCL_UTIL IS NOT BOUND.
    CREATE OBJECT GCL_UTIL.
  ENDIF.

  NAST = UV_NAST.

  IF NAST-SPRAS EQ LC_CON-E.
    I_NATION = LC_CON-I.
  ELSE.
    I_NATION = SPACE.
  ENDIF.

* SmartForm from customizing table TNAPR
  LF_FORMNAME = UV_FORM_NAME.

* determine print data
  PERFORM SET_PRINT_DATA_TO_READ USING    LF_FORMNAME
                                 CHANGING LS_PRINT_DATA_TO_READ
                                          lV_SUBRC.

  IF lV_SUBRC = 0.
* select print data
    PERFORM F_GET_DATA  USING LS_PRINT_DATA_TO_READ
                     CHANGING LS_ADDR_KEY
                              LS_DLV_LAND
                              LS_BIL_INVOICE
                              lV_SUBRC.

*    PERFORM F_MAP_DATA_INV USING LS_BIL_INVOICE. " USE CLASS ZCL_SDSSD_GEN_DATA_SD_FORM INSTANT

  ENDIF.

  IF lV_SUBRC = 0.
*    PERFORM F_SET_DIALOG CHANGING LS_CONTROL_PARAM.
    PERFORM SET_PRINT_PARAM USING    LS_ADDR_KEY
                                     LS_DLV_LAND
                                     UV_RETCODE
                                     UF_NODIALOG
                            CHANGING LS_CONTROL_PARAM
                                     LS_COMPOSER_PARAM
                                     LS_RECIPIENT
                                     LS_SENDER
                                     lV_SUBRC.
  ENDIF.

  IF lV_SUBRC = 0.
*   determine smartform function module for invoice
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = LF_FORMNAME
*       variant            = ' '
*       direct_call        = ' '
      IMPORTING
        FM_NAME            = LF_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.
*     error handling
      lV_SUBRC = SY-SUBRC.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.
  IF lV_SUBRC = 0.
*     call smartform invoice
    CALL FUNCTION LF_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = TOA_DARA
        ARCHIVE_PARAMETERS = ARC_PARAMS
        CONTROL_PARAMETERS = LS_CONTROL_PARAM
*       mail_appl_obj      =
        MAIL_RECIPIENT     = LS_RECIPIENT
        MAIL_SENDER        = LS_SENDER
        OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
        USER_SETTINGS      = SPACE
        IS_BIL_INVOICE     = LS_BIL_INVOICE
        IS_NAST            = NAST
        IS_REPEAT          = REPEAT
        I_NATION           = I_NATION
      IMPORTING
        JOB_OUTPUT_INFO    = LS_JOB_INFO
*       document_output_info =
*       job_output_options =
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.
    IF SY-SUBRC <> 0.
*       error handling
      lV_SUBRC = SY-SUBRC.
      PERFORM PROTOCOL_UPDATE.
    ELSE.
      UV_PROC_SCREEN = 0.
*      PERFORM F_SEND_PDF USING LS_BIL_INVOICE
*                               LF_FM_NAME.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_send_pdf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SEND_PDF USING LS_BIL_INVOICE TYPE LBBIL_INVOICE
                      LF_FM_NAME     TYPE RS38L_FNAM.

  DATA: LS_CONTROL_PARAM  TYPE SSFCTRLOP,
        LS_COMPOSER_PARAM TYPE SSFCOMPOP,
        LS_RECIPIENT      TYPE SWOTOBJID,
        LS_SENDER         TYPE SWOTOBJID,
        LF_FORMNAME       TYPE TDSFNAME,
        LS_ADDR_KEY       LIKE ADDR_KEY,
        LS_DLV_LAND       LIKE VBRK-LAND1,
        LS_JOB_INFO       TYPE SSFCRESCL.

  DATA: LCL_DATA TYPE REF TO ZCL_SDSCA_UTIL_SDS.

  DATA: LV_PATH    TYPE STRING,
        LV_MESSAGE TYPE CHAR50,
        LV_STATUS  TYPE CHAR1.

  CONSTANTS : BEGIN OF LC_CON,
                SWINCF TYPE C LENGTH 6 VALUE 'SWINCF',
              END OF LC_CON.

  IF LCL_DATA IS BOUND.
    CREATE OBJECT LCL_DATA.
  ENDIF.

  LS_CONTROL_PARAM-GETOTF     = ABAP_TRUE.
  LS_CONTROL_PARAM-NO_DIALOG  = ABAP_TRUE.
  LS_COMPOSER_PARAM-TDPRINTER = LC_CON-SWINCF.

  CALL FUNCTION LF_FM_NAME
    EXPORTING
      ARCHIVE_INDEX      = TOA_DARA
      ARCHIVE_PARAMETERS = ARC_PARAMS
      CONTROL_PARAMETERS = LS_CONTROL_PARAM
*     mail_appl_obj      =
      MAIL_RECIPIENT     = LS_RECIPIENT
      MAIL_SENDER        = LS_SENDER
      OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
      USER_SETTINGS      = SPACE
      IS_BIL_INVOICE     = LS_BIL_INVOICE
      IS_NAST            = NAST
      IS_REPEAT          = REPEAT
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

  IF LS_JOB_INFO-OTFDATA IS NOT INITIAL.
*get path
    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = GC_CON-REPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = GC_CON-PARAM
                                         CHANGING C_RETURN            = LV_PATH ).

    CONCATENATE LV_PATH 'File_name.pdf' INTO LV_PATH.

    LCL_DATA->SEND_OTF_FILE_TO_AL11( EXPORTING IT_OTF      = LS_JOB_INFO-OTFDATA
                                               I_PATH      = LV_PATH
                                               I_DATA_AL11 = ABAP_TRUE
                                     IMPORTING E_MESSAGE   = LV_MESSAGE
                                               E_STATUS    = LV_STATUS ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SERAIL_TAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_BIL_INVOICE_HD_REF_DELIV_NU
*&      <-- LT_SERAIL_TAB
*&---------------------------------------------------------------------*
FORM F_GET_SERAIL_TAB  USING    LV_DO
                                LT_DO_TAB     TYPE LB_T_BIL_IT_REF
                       CHANGING LT_SERAIL_TAB TYPE ZSDSFIS006_TT.

  DATA LS_DATA LIKE LINE OF LT_SERAIL_TAB.

  DATA : LS_DO_TAB LIKE LINE OF LT_DO_TAB.

  DATA : BEGIN OF LS_SER01,
           OBKNR   TYPE SER01-OBKNR,
           LIEF_NR TYPE SER01-LIEF_NR,
           POSNR   TYPE SER01-POSNR,
           SERNR   TYPE OBJK-SERNR,
           MATNR   TYPE OBJK-MATNR,
         END OF LS_SER01.
  DATA LT_SER01 LIKE TABLE OF LS_SER01.

  CONSTANTS : LC_601   TYPE C LENGTH 3 VALUE '601',
              LC_SER01 TYPE C LENGTH 5 VALUE 'SER01',
              LC_J     TYPE C LENGTH 1 VALUE 'J'.

  SELECT SER01~OBKNR,
         SER01~LIEF_NR,
         SER01~POSNR,
         OBJK~SERNR,
         OBJK~MATNR
    FROM SER01
    INNER JOIN OBJK ON SER01~OBKNR EQ OBJK~OBKNR AND
                       OBJK~TASER  EQ @LC_SER01
    INTO TABLE @LT_SER01
    WHERE LIEF_NR EQ @LV_DO
      AND BWART   EQ @LC_601.


  LOOP AT LT_SER01 INTO LS_SER01.
    READ TABLE LT_DO_TAB  INTO LS_DO_TAB
    WITH KEY REF_DOC      = LS_SER01-LIEF_NR
             REF_DOC_IT   = LS_SER01-POSNR
             REF_DOC_TYPE = LC_J.
    IF SY-SUBRC EQ 0.
      LS_DATA-VBELN = LS_DO_TAB-BIL_NUMBER.
      LS_DATA-POSNR = LS_DO_TAB-ITM_NUMBER.
    ENDIF.
    LS_DATA-MATNR = LS_SER01-MATNR.
    LS_DATA-MAKTX = LS_SER01-SERNR.
    APPEND LS_DATA TO LT_SERAIL_TAB.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_DIALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_CONTROL_PARAM
*&---------------------------------------------------------------------*
FORM F_SET_DIALOG  CHANGING CS_CONTROL_PARAM TYPE SSFCTRLOP.

  IF NAST-TDSPRAS EQ GC_CON-S.
    CS_CONTROL_PARAM-NO_DIALOG = ABAP_TRUE.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_READ_MERGE_ITEMS
*&---------------------------------------------------------------------*
*& Read Merge Items
*&---------------------------------------------------------------------*
FORM F_READ_MERGE_ITEMS  USING US_BIL_INVOICE TYPE LBBIL_INVOICE
                               UF_FOUND_ZH16  TYPE FLAG.
  DATA: LT_LINE     TYPE STANDARD TABLE OF TLINE.
  DATA: LS_LINE     LIKE LINE OF LT_LINE  ##NEEDED.
  DATA: LF_TDID     TYPE THEAD-TDID.
  DATA: LF_TDNAME   TYPE THEAD-TDNAME.
  DATA: LF_TDOBJECT TYPE THEAD-TDOBJECT.

  LF_TDID     = 'ZH16'.
  LF_TDNAME   = US_BIL_INVOICE-HD_GEN-BIL_NUMBER.
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
