*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0390
*  Creation Date      : 15.11.2024
*  Author             : B CHIEWSARIKIJ (SDS)
*  Add-on ID          : N/A
*  Description        : Update DO document by email
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0390.
*&---------------------------------------------------------------------*
*& Report ZSDSSDR0390
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES NAST.
*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES : BEGIN OF GY_ATTACH,
          FILE_SEQ           TYPE STRING,
          PR_REQUEST_DOCID   TYPE STRING,
          FILE_TYPE_ID       TYPE STRING,
          FILE_NAME_ORIGINAL TYPE STRING,
          CONTENT_TYPE       TYPE STRING,
          FILE_NAME          TYPE STRING,
          FILE_PATH          TYPE STRING,
          IS_ACTIVE          TYPE STRING,
          CREATE_DATE        TYPE STRING,
          CREATE_BY          TYPE STRING,
          CREATE_BY_NAME     TYPE STRING,
          UPDATE_DATE        TYPE STRING,
          UPDATE_BY          TYPE STRING,
          UPDATE_BY_NAME     TYPE STRING,
        END OF GY_ATTACH.



*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA: GC_FORM_NAME TYPE TNAPR-SFORM VALUE 'ZSDSSD009',
      GS_LIKP      TYPE LIKP,
      GV_VBELN     TYPE LIKP-VBELN,
      GT_ADSMTP    TYPE STANDARD TABLE OF BAPIADSMTP.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : BEGIN OF GC_CON,
              REPID        TYPE ZSDSCAC001-REPID VALUE 'ZCL_IM_SDS_BADI_DO_PROC',
              URL_ATT      TYPE ZSDSCAC001-PARAM VALUE 'URL_ATTACH',
              SUBJECT      TYPE ZSDSCAC001-PARAM VALUE 'SUBJECT_EMAIL',
              SENDER_NAME  TYPE ZSDSCAC001-PARAM VALUE 'SENDER_NAME',
              SENDER_EMAIL TYPE ZSDSCAC001-PARAM VALUE 'SENDER_EMAIL',
            END OF GC_CON.

CONSTANTS: GC_S_EMP TYPE PARVW VALUE 'VE'.

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK S01 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_VBELN TYPE LIKP-VBELN.
* SELECT-OPTIONS:
SELECTION-SCREEN END OF BLOCK S01.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*  GV_VBELN    = '4010000018'.P_VBELN
  GV_VBELN    = P_VBELN.

  SELECT SINGLE * FROM LIKP INTO @GS_LIKP WHERE VBELN = @GV_VBELN.
  SELECT SINGLE KUNNR,
                NAME1,
                NAME2
  FROM KNA1
  INTO @DATA(GS_KNA1)
  WHERE KUNNR = @GS_LIKP-KUNAG.
  SELECT SINGLE * FROM KNVP INTO @DATA(LS_KNVP) WHERE  KUNNR = @GS_LIKP-KUNAG AND PARVW = @GC_S_EMP.
  IF SY-SUBRC = 0.
*       CS_CUSTOMER-SALES_EMP = LS_KNVP-PERNR.
    DATA(GV_BPARTNER_VE) = CVI_PARTNER_FUNCTIONS=>GET_BP_FROM_KNVP( EXPORTING IS_KNVP = LS_KNVP ).

    CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
      EXPORTING
        BUSINESSPARTNER      = GV_BPARTNER_VE
        VALID_DATE           = SY-DATLO
        IV_REQ_MASK          = 'X'
      TABLES
        E_MAILDATANONADDRESS = GT_ADSMTP.
  ENDIF.

*  EXIT.
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM F_SEND_MAIL.
*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
FORM F_SEND_MAIL .

  DATA: IT_OTF   TYPE STANDARD TABLE OF ITCOO,
        IT_DOCS  TYPE STANDARD TABLE OF DOCS,
        IT_LINES TYPE STANDARD TABLE OF TLINE.

  DATA: LS_LINES LIKE LINE OF IT_LINES.
* Declaration of local variables.
  DATA: LV_BIN_FILESIZE TYPE I,
        LV_NAME         TYPE STRING,
        LV_FILE         TYPE STRING.

  DATA: LV_BIN_FILE  TYPE  XSTRING.

  DATA : LT_HTML_CON           TYPE SOLI_TAB,
         LV_FM_NAME            TYPE RS38L_FNAM,
         LS_DLV_DELNOTE        TYPE LEDLV_DELNOTE,
         LS_ADDR_KEY           LIKE ADDR_KEY,
         LS_COMPOSER_PARAM     TYPE SSFCOMPOP,
         LS_CONTROL_PARAM      TYPE SSFCTRLOP,
         LS_JOB_INFO           TYPE SSFCRESCL,
         LS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ.

  DATA : LCL_UTIL       TYPE REF TO ZCL_SDSCA_UTIL_SDS,
         LS_ATTACH_FILE TYPE ZSDSCAS019,
         LT_ATTACH_FILE TYPE ZSDSCAS019_TT.

  DATA:  LV_SUBRC TYPE SY-SUBRC.
  DATA : LS_EMAIL TYPE STRING,
         LT_REC   LIKE TABLE OF LS_EMAIL,
         LT_CC    LIKE TABLE OF LS_EMAIL.

  DATA: LV_SUBJECT      TYPE CHAR50,
        LV_SENDER_NAME  TYPE SO_RECNAME,
        LV_SENDER_EMAIL TYPE SO_RECNAME.



  PERFORM SET_PRINT_DATA_TO_READ USING    GC_FORM_NAME
                                 CHANGING LS_PRINT_DATA_TO_READ
                                          LV_SUBRC.

  PERFORM F_GET_DATA  USING LS_PRINT_DATA_TO_READ
                            GV_VBELN
                   CHANGING LS_ADDR_KEY
                            LS_DLV_DELNOTE
                            LV_SUBRC.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = GC_FORM_NAME
    IMPORTING
      FM_NAME            = LV_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    "error hadning
  ENDIF.

  LS_CONTROL_PARAM-GETOTF     = ABAP_TRUE.
  LS_CONTROL_PARAM-NO_DIALOG  = ABAP_TRUE.
  LS_COMPOSER_PARAM-TDPRINTER = 'SWINCF'.

  CALL FUNCTION LV_FM_NAME
    EXPORTING
      CONTROL_PARAMETERS = LS_CONTROL_PARAM
      OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
      USER_SETTINGS      = SPACE
      GS_DLV_DELNOTE     = LS_DLV_DELNOTE
      GS_NAST            = NAST
    IMPORTING
      JOB_OUTPUT_INFO    = LS_JOB_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
    "error handling
  ELSE.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = 'PDF'
      IMPORTING
        BIN_FILESIZE          = LV_BIN_FILESIZE
        BIN_FILE              = LV_BIN_FILE
      TABLES
        OTF                   = LS_JOB_INFO-OTFDATA
        LINES                 = IT_LINES
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        ERR_BAD_OTF           = 4
        OTHERS                = 5.
    IF LV_BIN_FILE IS NOT INITIAL.
      IF LCL_UTIL IS NOT BOUND.
        CREATE OBJECT LCL_UTIL.
      ENDIF.

      LS_ATTACH_FILE-GROUP = 1.
      LS_ATTACH_FILE-FILENAME = GV_VBELN.
      LS_ATTACH_FILE-LEN     = LV_BIN_FILESIZE.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          BUFFER        = LV_BIN_FILE
        IMPORTING
          OUTPUT_LENGTH = LV_BIN_FILESIZE
        TABLES
          BINARY_TAB    = LS_ATTACH_FILE-DATAF.

      APPEND LS_ATTACH_FILE TO LT_ATTACH_FILE.

      "SUBJECT
      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = GC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = GC_CON-SUBJECT
                                            CHANGING  C_RETURN            = LV_SUBJECT ).
      REPLACE '&1' IN LV_SUBJECT WITH SPACE.
      REPLACE '&2' IN LV_SUBJECT WITH |{ GS_KNA1-NAME1 }{ GS_KNA1-NAME2 }|.
      CONDENSE LV_SUBJECT.

      "SENDER NAME & EMAIL
      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = GC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = GC_CON-SENDER_EMAIL
                                            CHANGING  C_RETURN            = LV_SENDER_EMAIL ).
      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = GC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = GC_CON-SENDER_NAME
                                            CHANGING  C_RETURN            = LV_SENDER_NAME ).

      PERFORM GET_CONTENT USING GV_VBELN
                          CHANGING LT_HTML_CON.

      "Sales Emp (Can be only 1 record)
      READ TABLE GT_ADSMTP INTO DATA(LS_ADSMTP)
                           INDEX 1.
*      LS_EMAIL = 'boonpipop@daikin.co.th'.
      LS_EMAIL = LS_ADSMTP-E_MAIL.
      APPEND LS_EMAIL TO LT_REC.

*      LOOP AT S_CC INTO LS_CC.
*        LS_EMAIL = LS_CC-LOW.
*        APPEND LS_EMAIL TO LT_CC.
*      ENDLOOP.

      LCL_UTIL->SEND_MAIL( I_SUBJECT      = LV_SUBJECT
                           IT_HTML_CON    = LT_HTML_CON
                           IT_ATTACH_FILE = LT_ATTACH_FILE
                           I_SENDER_EMAIL = LV_SENDER_EMAIL
                           I_SENDER_NAME  = LV_SENDER_NAME
                           IT_RECEIVER    = LT_REC
                           IT_CC          = LT_CC  ).

      COMMIT WORK AND WAIT.
*      MESSAGE S000 WITH 'Please Check Status at t-code SOST'.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CONTENT
*&---------------------------------------------------------------------*
*& get html content function
*&---------------------------------------------------------------------*
FORM GET_CONTENT  USING    UV_VBELN TYPE LIKP-VBELN
                  CHANGING CT_HTML_CON TYPE SOLI_TAB.


  DATA : LS_CONTENT TYPE SOLI.
  DATA : LV_WEBNO TYPE ZSDSMMT006-WEBNO.
  DATA : LT_ATTACH TYPE TABLE OF GY_ATTACH.
  DATA : LV_ENDPOINT TYPE STRING.
  DATA : LV_DOWNLOAD_PATH TYPE STRING.
  DATA : LV_DELIVERY_DATE TYPE CHAR15.

  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = GC_CON-REPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = GC_CON-URL_ATT
                                        CHANGING  C_RETURN            = LV_ENDPOINT ).

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = 'DEAR SALES PERSON'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = '<BR><BR>'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  SELECT SINGLE VBELV FROM VBFA INTO @DATA(LV_SO) WHERE VBELN = @UV_VBELN AND VBTYP_V = 'C'.
  CONCATENATE 'SALES ORDER NO.'
              LV_SO
        INTO LS_CONTENT-LINE SEPARATED BY SPACE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  CONCATENATE '<BR>DELIVERY NO.'
              GS_LIKP-VBELN
        INTO LS_CONTENT-LINE SEPARATED BY SPACE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  PERFORM CONVERT_DATE USING GS_LIKP-BLDAT
                       CHANGING LV_DELIVERY_DATE.
  CONCATENATE '<BR>DELIVERY DATE'
              LV_DELIVERY_DATE
        INTO LS_CONTENT-LINE SEPARATED BY SPACE.
  APPEND LS_CONTENT TO CT_HTML_CON.


*KUNAG
  CLEAR : LS_CONTENT.
  CONCATENATE '<BR>CUSTOMER NO.'
              GS_KNA1-KUNNR
        INTO LS_CONTENT-LINE SEPARATED BY SPACE.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = |<BR>CUSTOMER NAME. { GS_KNA1-NAME1 }{ GS_KNA1-NAME2 }|.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = '<BR><BR>'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = 'แจ้งเพื่อทราบ'.
  APPEND LS_CONTENT TO CT_HTML_CON.

  CLEAR : LS_CONTENT.
  LS_CONTENT-LINE = '<BR>SAP SYSTEM'.
  APPEND LS_CONTENT TO CT_HTML_CON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_GET_DATA  USING IS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ
                       UV_VBELN             TYPE LIKP-VBELN
             CHANGING CS_ADDR_KEY           LIKE ADDR_KEY
                      CS_DLV_DELNOTE        TYPE LEDLV_DELNOTE
                      CF_RETCODE.

  DATA: LS_DELIVERY_KEY TYPE  LESHP_DELIVERY_KEY.
  LS_DELIVERY_KEY-VBELN = UV_VBELN.
* read print data
  CALL FUNCTION 'LE_SHP_DLV_OUTP_READ_PRTDATA'
    EXPORTING
      IS_DELIVERY_KEY       = LS_DELIVERY_KEY
      IS_PRINT_DATA_TO_READ = IS_PRINT_DATA_TO_READ
      IF_PARVW              = 'WE'
      IF_PARNR              = GS_LIKP-KUNAG
      IF_LANGUAGE           = SY-LANGU
    IMPORTING
      ES_DLV_DELNOTE        = CS_DLV_DELNOTE
    EXCEPTIONS
      RECORDS_NOT_FOUND     = 1
      RECORDS_NOT_REQUESTED = 2
      OTHERS                = 3.
  IF SY-SUBRC <> 0.
    CF_RETCODE = SY-SUBRC.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form convert_date
*&---------------------------------------------------------------------*
*& Convert Date function
*&---------------------------------------------------------------------*
FORM CONVERT_DATE  USING  UV_DATE TYPE DATUM
                   CHANGING CV_TEXT TYPE ANY.


  DATA: LV_MONTH              TYPE MONTH,
        LV_DATE_CONVERTED(11) TYPE C,
        LV_DATE(2)            TYPE C,
        LV_YEAR(4)            TYPE C.
  DATA: LWA_T247 TYPE T247.

  CLEAR: LV_MONTH, LV_DATE, LV_YEAR, LV_DATE_CONVERTED.
  MOVE UV_DATE+6(2) TO LV_DATE.
  MOVE UV_DATE+0(4) TO LV_YEAR.
  MOVE UV_DATE+4(2) TO LV_MONTH.

  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      LANGU = SY-LANGU
      MONTH = LV_MONTH
    IMPORTING
      T247  = LWA_T247.

  IF LWA_T247-KTX IS NOT INITIAL.
    CONCATENATE LV_DATE LWA_T247-KTX LV_YEAR INTO LV_DATE_CONVERTED SEPARATED BY '-'.
  ENDIF.

  MOVE LV_DATE_CONVERTED TO CV_TEXT.

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
                         CHANGING CS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ
                                  CF_RETCODE.

  FIELD-SYMBOLS: <FS_PRINT_DATA_TO_READ> TYPE XFELD.
  DATA: LT_FIELDLIST TYPE TSFFIELDS.

* set print data requirements
  DO.
    ASSIGN COMPONENT SY-INDEX OF STRUCTURE
                     CS_PRINT_DATA_TO_READ TO <FS_PRINT_DATA_TO_READ>.
    IF SY-SUBRC <> 0. EXIT. ENDIF.
    <FS_PRINT_DATA_TO_READ> = ABAP_TRUE.
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
  ENDIF.

ENDFORM.
