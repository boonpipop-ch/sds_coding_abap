*&---------------------------------------------------------------------*
*& Report ZSDSSDR0140
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0140.
INCLUDE RVADTABL.
INCLUDE ZSDSSDR0140_TOP.

*---------------------------------------------------------------------*
*       FORM ENTRY
*---------------------------------------------------------------------*
FORM ENTRY USING UV_PROC_SCREEN
        CHANGING CV_RETCODE.

* for call by other program.
  PERFORM F_PRINT USING UV_PROC_SCREEN
                        NAST
                        SPACE
                        TNAPR-SFORM
               CHANGING CV_RETCODE.

ENDFORM.                    "ENTRY
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_GET_DATA USING IS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ
             CHANGING CS_ADDR_KEY           LIKE ADDR_KEY
                      CS_DLV_DELNOTE        TYPE LEDLV_DELNOTE
                      CF_RETCODE
                      lV_SUBRC.

  DATA: LS_DELIVERY_KEY TYPE  LESHP_DELIVERY_KEY.
  LS_DELIVERY_KEY-VBELN = NAST-OBJKY.
* read print data
  CALL FUNCTION 'LE_SHP_DLV_OUTP_READ_PRTDATA'
    EXPORTING
      IS_DELIVERY_KEY       = LS_DELIVERY_KEY
      IS_PRINT_DATA_TO_READ = IS_PRINT_DATA_TO_READ
      IF_PARVW              = NAST-PARVW
      IF_PARNR              = NAST-PARNR
      IF_LANGUAGE           = NAST-SPRAS
    IMPORTING
      ES_DLV_DELNOTE        = CS_DLV_DELNOTE
    EXCEPTIONS
      RECORDS_NOT_FOUND     = 1
      RECORDS_NOT_REQUESTED = 2
      OTHERS                = 3.
  IF SY-SUBRC <> 0.
*  error handling
    CF_RETCODE = SY-SUBRC.
    lV_SUBRC   = SY-SUBRC.
    PERFORM PROTOCOL_UPDATE.
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
    MOVE-CORRESPONDING LS_ITCPO TO CS_COMPOSER_PARAM.
*   CS_CONTROL_PARAM-NO_OPEN
*   CS_CONTROL_PARAM-NO_CLOSE
    CS_CONTROL_PARAM-DEVICE      = LF_DEVICE.
    CS_CONTROL_PARAM-NO_DIALOG   = 'X'.
    CS_CONTROL_PARAM-PREVIEW     = XSCREEN.
    CS_CONTROL_PARAM-GETOTF      = LS_ITCPO-TDGETOTF.
    CS_CONTROL_PARAM-LANGU       = NAST-SPRAS.
*   CS_CONTROL_PARAM-REPLANGU1
*   CS_CONTROL_PARAM-REPLANGU2
*   CS_CONTROL_PARAM-REPLANGU3
*   CS_CONTROL_PARAM-STARTPAGE
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
                   UV_NAST
                   UV_DOCNO
                   UV_FORM_NAME
          CHANGING CV_RETCODE.

  DATA: lV_SUBRC TYPE SY-SUBRC.

  DATA: LS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ,
        LS_DLV_DELNOTE        TYPE LEDLV_DELNOTE,
        LF_FM_NAME            TYPE RS38L_FNAM,
        LS_CONTROL_PARAM      TYPE SSFCTRLOP,
        LS_COMPOSER_PARAM     TYPE SSFCOMPOP,
        LS_RECIPIENT          TYPE SWOTOBJID,
        LS_SENDER             TYPE SWOTOBJID,
        LF_FORMNAME           TYPE TDSFNAME,
        LS_ADDR_KEY           LIKE ADDR_KEY,
        LS_DLV_LAND           LIKE VBRK-LAND1,
        LS_JOB_INFO           TYPE SSFCRESCL.

  IF GCL_UTIL IS NOT BOUND.
    CREATE OBJECT GCL_UTIL.
  ENDIF.

  NAST = UV_NAST.

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
                              LS_DLV_DELNOTE
                              CV_RETCODE
                              lV_SUBRC.
  ENDIF.

*  IF lV_SUBRC = 0.
*    PERFORM SET_PRINT_PARAM USING    LS_ADDR_KEY
*                                     LS_DLV_LAND
*                            CHANGING LS_CONTROL_PARAM
*                                     LS_COMPOSER_PARAM
*                                     LS_RECIPIENT
*                                     LS_SENDER
*                                     lV_SUBRC.
*  ENDIF.

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
        GS_DLV_DELNOTE     = LS_DLV_DELNOTE
        GS_NAST            = NAST
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
      PERFORM F_WARRANTY_CARD USING NAST-OBJKY
                                    NAST-KSCHL.
      UV_PROC_SCREEN = 0.
*      PERFORM F_SEND_PDF USING LS_BIL_INVOICE
*                               LF_FM_NAME.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_warranty_card
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> NAST_OBJKY
*&---------------------------------------------------------------------*
FORM F_WARRANTY_CARD  USING  LV_DOCNO
                             LV_OUTPUT_TYPE.

  DATA : S_VBELN TYPE RANGE OF LIKP-VBELN.

  CONSTANTS:
    BEGIN OF LC_CON,
      I     TYPE C LENGTH 1 VALUE 'I',
      EQ    TYPE C LENGTH 2 VALUE 'EQ',
      REPID TYPE ZSDSCAC001-REPID VALUE 'ZSDSSDR0140',
      PARAM TYPE ZSDSCAC001-PARAM VALUE 'OUTPUT_TYPE',
    END OF LC_CON .

  DATA : LV_LOW TYPE ZSDSCAC001-VALUE_LOW.

  S_VBELN = VALUE #( ( SIGN = LC_CON-I OPTION = LC_CON-EQ LOW = LV_DOCNO ) ).

  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                I_PARAM             = LC_CON-PARAM
                                      CHANGING  C_RETURN            = LV_LOW ).

  IF LV_LOW EQ LV_OUTPUT_TYPE.
    SUBMIT ZSDSSDR0280 USING SELECTION-SCREEN 1000
                       WITH S_VBELN IN S_VBELN[]
                       AND RETURN.
  ENDIF.

ENDFORM.
