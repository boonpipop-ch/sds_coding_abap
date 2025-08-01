*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0390_CLASS
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
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
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
      CALL_SMART_FORM,
      CALL_FORM_PR IMPORTING I_DOCNO    TYPE EBAN-BANFN
                             I_ALL_LINE TYPE I
                             I_INDEX    TYPE I,
      CHECK_SCREEN,
      SEND_MAIL_PR IMPORTING I_DOCNO    TYPE EBAN-BANFN,
      GET_CONTENT IMPORTING I_DOCNO  TYPE EBAN-BANFN
                  RETURNING VALUE(R) TYPE SOLI_TAB,
      GET_ATTACHMENT IMPORTING I_WEBNO  TYPE ZSDSMMT006-WEBNO
                     RETURNING VALUE(R) LIKE GT_ATTACH,
      GET_URL_ATTACH RETURNING VALUE(R) TYPE STRING,
      GET_HEADER_API_ATTACH CHANGING C_HEADER TYPE ZSDSCAS001_TT,
      GET_BODY_API_ATTACH CHANGING C_BODY TYPE ANY
                                   C_LEN  TYPE ANY,
      GET_USER_PASS CHANGING I_USER TYPE STRING
                             I_PASS TYPE STRING,
      GET_DATA_FROM_WEB IMPORTING I_WEB TYPE ANY
                                  I_PR  TYPE ANY,
      GET_HEADER_API_K2 IMPORTING I_WEB_NO TYPE CHAR20
                        CHANGING  C_HEADER TYPE ANY,
      GET_DETAIL_API_K2 IMPORTING I_WEB_NO TYPE CHAR20
                        CHANGING  C_DETAIL TYPE ANY,
      GET_URL_HEADER        IMPORTING I_WEB_NO TYPE ANY
                            CHANGING  C_URL    TYPE ANY,
      GET_HEADER_API_HEADER CHANGING C_HEADER TYPE ZSDSCAS001_TT,
      GET_DETAIL_API_HEADER CHANGING C_BODY TYPE ANY
                                     C_LEN  TYPE ANY,
      GET_URL_DETAIL        IMPORTING I_WEB_NO TYPE ANY
                            CHANGING  C_URL    TYPE ANY,
      GET_HEADER_API_DETAIL CHANGING C_HEADER TYPE ZSDSCAS001_TT,
      GET_DETAIL_API_DETAIL CHANGING C_BODY TYPE ANY
                                     C_LEN  TYPE ANY.
    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD GET_DATA.
    IF LO IS INITIAL.
      CREATE OBJECT LO.
    ENDIF.

    LO->START_PROCESS( ).
  ENDMETHOD.
  METHOD START_PROCESS.

    DATA : BEGIN OF LS_PR_WEB,
             BANFN TYPE EBAN-BANFN,
             WEBNO TYPE ZSDSMMT006-WEBNO,
           END OF LS_PR_WEB.
    DATA : LT_PR_WEB LIKE TABLE OF LS_PR_WEB WITH EMPTY KEY.

    IF S_WEBNO IS NOT INITIAL.
      SELECT BANFN
             WEBNO
        FROM ZSDSMMT006
        INTO TABLE LT_PR_WEB
        WHERE WEBNO IN S_WEBNO
          AND BANFN NE SPACE
          AND FLAGD EQ SPACE.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_PR_WEB INTO LS_PR_WEB.
          LCL_DATA=>GET_DATA_FROM_WEB( I_WEB = LS_PR_WEB-WEBNO
                                       I_PR  = LS_PR_WEB-BANFN ).
        ENDLOOP.
*        SELECT EBAN~BANFN,
*               EBAN~BNFPO,
*               EBAN~ERDAT,
*               EBAN~ERNAM,
*               EBAN~MATNR,
*               EBAN~TXZ01,
*               EBAN~MENGE,
*               EBAN~MEINS,
*               EBAN~PREIS,
*               EBAN~WAERS,
*               EBAN~LFDAT,
*               EBKN~SAKTO,
*               EBKN~KOSTL,
*               EBKN~AUFNR,
*               EBKN~PS_PSP_PNR,
*               T006A~MSEHL,
*               @ABAP_TRUE AS CHECK
*          FROM @LT_PR_WEB AS A
*          INNER JOIN EBAN ON  A~BANFN     EQ EBAN~BANFN
*          LEFT JOIN  EBKN ON  EBAN~BANFN  EQ EBKN~BANFN AND
*                              EBAN~BNFPO  EQ EBKN~BNFPO AND
*                              EBKN~ZEBKN  EQ 1
*          LEFT JOIN T006A ON  EBAN~MEINS  EQ T006A~MSEHI AND
*                              T006A~SPRAS EQ @SY-LANGU
*          WHERE EBAN~BANFN IN @S_BANFN
*          INTO TABLE @GT_RESULT.
      ENDIF.
    ELSE.
      SELECT BANFN,
             WEBNO
        FROM ZSDSMMT006
        INTO TABLE @LT_PR_WEB
        WHERE WEBNO IN @S_WEBNO
          AND BANFN IN @S_BANFN
          AND BANFN NE @SPACE
          AND FLAGD EQ @SPACE.
      IF SY-SUBRC EQ 0.
        LOOP AT LT_PR_WEB INTO LS_PR_WEB.
          LCL_DATA=>GET_DATA_FROM_WEB( I_WEB = LS_PR_WEB-WEBNO
                                       I_PR  = LS_PR_WEB-BANFN ).
        ENDLOOP.
      ENDIF.
      SELECT EBAN~BANFN,
             EBAN~BNFPO,
             EBAN~ERDAT,
             EBAN~ERNAM,
             EBAN~MATNR,
             EBAN~TXZ01,
             EBAN~MENGE,
             EBAN~MEINS,
             EBAN~PREIS,
             EBAN~WAERS,
             EBAN~LFDAT,
             EBKN~SAKTO,
             EBKN~KOSTL,
             EBKN~AUFNR,
             EBKN~PS_PSP_PNR,
             T006A~MSEHL,
             @ABAP_TRUE AS CHECK
        FROM EBAN
        LEFT JOIN EBKN ON  EBAN~BANFN  EQ EBKN~BANFN AND
                           EBAN~BNFPO  EQ EBKN~BNFPO AND
                           EBKN~ZEBKN  EQ 1
        LEFT JOIN T006A ON EBAN~MEINS  EQ T006A~MSEHI AND
                           T006A~SPRAS EQ @SY-LANGU
        APPENDING TABLE @GT_RESULT
        WHERE EBAN~BANFN IN @S_BANFN AND
        NOT EXISTS ( SELECT BANFN
                       FROM ZSDSMMT006
                       WHERE BANFN EQ EBAN~BANFN
                         AND FLAGD EQ @SPACE ).
    ENDIF.
  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.

    ENDLOOP.
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
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       i_html_height_top  = 12
*       I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
*       I_GRID_TITLE       =
*       I_GRID_SETTINGS    =
        IS_LAYOUT          = GS_LAYOUT
        IT_FIELDCAT        = GT_FCAT
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
        IT_SORT            = GT_SORT
*       IT_FILTER          =
*       IS_SEL_HIDE        =
        I_DEFAULT          = GC_X
        I_SAVE             = GC_A
*       IS_VARIANT         =
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  = 0
*       I_HTML_HEIGHT_END  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
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
  METHOD CALL_SMART_FORM.

    DATA: LT_TMP LIKE GT_RESULT,
          LS_TMP LIKE LINE OF LT_TMP.

    DATA: LV_ALL_LINE TYPE I,
          LV_INDEX    TYPE I.

    LT_TMP[]    = GT_RESULT[].

    SORT LT_TMP BY CHECK ASCENDING.
    DELETE LT_TMP WHERE CHECK EQ SPACE.
    SORT LT_TMP BY BANFN.
    DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING BANFN.

    DESCRIBE TABLE LT_TMP LINES LV_ALL_LINE.

    LOOP AT LT_TMP INTO LS_TMP.
      LV_INDEX = SY-TABIX.
      IF P_EMAIL EQ ABAP_TRUE.
        SEND_MAIL_PR( I_DOCNO    = LS_TMP-BANFN ).
      ELSE.
        CALL_FORM_PR( I_DOCNO    = LS_TMP-BANFN
                      I_ALL_LINE = LV_ALL_LINE
                      I_INDEX    = LV_INDEX ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD CALL_FORM_PR.
    DATA : LV_FM_NAME TYPE RS38L_FNAM,
           LV_REPEAT  TYPE C LENGTH 1.

    DATA : LS_NAST       TYPE NAST,
           LS_ARC_PARAMS TYPE ARC_PARAMS,
           LS_TOA_DARA   TYPE TOA_DARA.

    DATA : LS_CONTROL_PARAM  TYPE SSFCTRLOP,
           LS_COMPOSER_PARAM TYPE SSFCOMPOP,
           LS_RECIPIENT      TYPE SWOTOBJID,
           LS_SENDER         TYPE SWOTOBJID,
           LS_ADDR_KEY       TYPE ADDR_KEY,
           LS_DLV_LAND       TYPE VBRK-LAND1,
           LS_JOB_INFO       TYPE SSFCRESCL.

    IF I_INDEX = 1.               "fisrt call
      LS_CONTROL_PARAM-NO_OPEN   = SPACE .
      LS_CONTROL_PARAM-NO_CLOSE  = ABAP_TRUE .
    ELSEIF I_INDEX = I_ALL_LINE.    "last call
      LS_CONTROL_PARAM-NO_OPEN   = ABAP_TRUE .
      LS_CONTROL_PARAM-NO_CLOSE  = SPACE .
    ELSE.                          "other calls
      LS_CONTROL_PARAM-NO_OPEN   = ABAP_TRUE .
      LS_CONTROL_PARAM-NO_CLOSE  = ABAP_TRUE .
    ENDIF.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = GC_FORM_NAME
*       variant            = ' '
*       direct_call        = ' '
      IMPORTING
        FM_NAME            = LV_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.

    ENDIF.
*     call smartform invoice
    CALL FUNCTION LV_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = LS_TOA_DARA
        ARCHIVE_PARAMETERS = LS_ARC_PARAMS
        CONTROL_PARAMETERS = LS_CONTROL_PARAM
*       mail_appl_obj      =
        MAIL_RECIPIENT     = LS_RECIPIENT
        MAIL_SENDER        = LS_SENDER
        OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
        USER_SETTINGS      = SPACE
        GV_DOCNO           = I_DOCNO
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
    ELSE.
*      SEND_PDF( I_BIL_INVOICE = I_DATA
*                I_FM_NAME     = LV_FM_NAME ).
    ENDIF.

    IF I_ALL_LINE = 1.
      CALL FUNCTION 'SSF_CLOSE'.
    ENDIF.

  ENDMETHOD.
  METHOD CHECK_SCREEN.
    CONSTANTS : BEGIN OF LC_CON,
                  MAL TYPE C LENGTH 3 VALUE 'MIL',
                END OF LC_CON.
    LOOP AT SCREEN.
      IF P_EMAIL EQ ABAP_TRUE.
        IF SCREEN-GROUP1 = LC_CON-MAL.
          SCREEN-ACTIVE = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF SCREEN-GROUP1 = LC_CON-MAL.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD SEND_MAIL_PR.
    DATA : LV_FM_NAME TYPE RS38L_FNAM,
           LV_REPEAT  TYPE C LENGTH 1.

    DATA : LS_NAST       TYPE NAST,
           LS_ARC_PARAMS TYPE ARC_PARAMS,
           LS_TOA_DARA   TYPE TOA_DARA.

    DATA : LS_CONTROL_PARAM  TYPE SSFCTRLOP,
           LS_COMPOSER_PARAM TYPE SSFCOMPOP,
           LS_RECIPIENT      TYPE SWOTOBJID,
           LS_SENDER         TYPE SWOTOBJID,
           LS_ADDR_KEY       TYPE ADDR_KEY,
           LS_DLV_LAND       TYPE VBRK-LAND1,
           LS_JOB_INFO       TYPE SSFCRESCL.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA : LS_EMAIL TYPE STRING.

    DATA : LT_CC  LIKE TABLE OF LS_EMAIL,
           LT_REC LIKE TABLE OF LS_EMAIL.

    DATA : LT_HTML_CON    TYPE SOLI_TAB,
           LT_ATTACH_FILE TYPE ZSDSCAS019_TT.

    DATA : LS_ATTACH_FILE TYPE ZSDSCAS019.

    DATA : LS_DATAF TYPE SOLIX.

    DATA : LS_RECEI LIKE LINE OF S_RECEI[],
           LS_CC    LIKE LINE OF S_CC[].

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = GC_FORM_NAME
*       variant            = ' '
*       direct_call        = ' '
      IMPORTING
        FM_NAME            = LV_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.

    ENDIF.

    LS_CONTROL_PARAM-GETOTF     = ABAP_TRUE.
    LS_CONTROL_PARAM-NO_DIALOG  = ABAP_TRUE.
    LS_COMPOSER_PARAM-TDPRINTER = 'SWINCF'.

    CALL FUNCTION LV_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = LS_TOA_DARA
        ARCHIVE_PARAMETERS = LS_ARC_PARAMS
        CONTROL_PARAMETERS = LS_CONTROL_PARAM
*       mail_appl_obj      =
        MAIL_RECIPIENT     = LS_RECIPIENT
        MAIL_SENDER        = LS_SENDER
        OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
        USER_SETTINGS      = SPACE
        GV_DOCNO           = I_DOCNO
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
    ELSE.
      DATA: IT_OTF   TYPE STANDARD TABLE OF ITCOO,
            IT_DOCS  TYPE STANDARD TABLE OF DOCS,
            IT_LINES TYPE STANDARD TABLE OF TLINE.

      DATA: LS_LINES LIKE LINE OF IT_LINES.
* Declaration of local variables.
      DATA: LV_BIN_FILESIZE TYPE I,
            LV_NAME         TYPE STRING,
            LV_FILE         TYPE STRING.

      DATA: LV_BIN_FILE  TYPE  XSTRING.

      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          FORMAT                = 'PDF'
*         max_linewidth         = 132
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
        LS_ATTACH_FILE-FILENAME = I_DOCNO.
        LS_ATTACH_FILE-LEN     = LV_BIN_FILESIZE.


        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            BUFFER        = LV_BIN_FILE
          IMPORTING
            OUTPUT_LENGTH = LV_BIN_FILESIZE
          TABLES
            BINARY_TAB    = LS_ATTACH_FILE-DATAF.

*        LOOP AT IT_LINES INTO LS_LINES.
*          LS_DATAF-LINE = LS_LINES-TDLINE.
*          APPEND LS_DATAF TO LS_ATTACH_FILE-DATAF.
*          CLEAR : LS_DATAF.
*        ENDLOOP.

        APPEND LS_ATTACH_FILE TO LT_ATTACH_FILE.

        LT_HTML_CON = GET_CONTENT( I_DOCNO ).

        LOOP AT S_RECEI INTO LS_RECEI.
          LS_EMAIL = LS_RECEI-LOW.
          APPEND LS_EMAIL TO LT_REC.
        ENDLOOP.

        LOOP AT S_CC INTO LS_CC.
          LS_EMAIL = LS_CC-LOW.
          APPEND LS_EMAIL TO LT_CC.
        ENDLOOP.

        LCL_UTIL->SEND_MAIL( I_SUBJECT      = P_SUBJE
                             IT_HTML_CON    = LT_HTML_CON
                             IT_ATTACH_FILE = LT_ATTACH_FILE
                             I_SENDER_EMAIL = P_SENDD
                             I_SENDER_NAME  = P_SENDN
                             IT_RECEIVER    = LT_REC
                             IT_CC          = LT_CC  ).

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD GET_CONTENT.

    DATA : LS_CONTENT TYPE SOLI.

    DATA : LV_WEBNO TYPE ZSDSMMT006-WEBNO.

    DATA : LT_ATTACH TYPE TABLE OF GY_ATTACH.

    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 11 VALUE 'ZSDSMMR0390',
                  PARAM TYPE C LENGTH 10 VALUE 'URL_ATTACH',
                END OF LC_CON.

    DATA : LV_ENDPOINT TYPE STRING.

    DATA : LV_DOWNLOAD_PATH TYPE STRING.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = LC_CON-REPID
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                    I_PARAM             = LC_CON-PARAM
                                          CHANGING  C_RETURN            = LV_ENDPOINT ).

    SELECT SINGLE WEBNO
      FROM ZSDSMMT006
      INTO LV_WEBNO
      WHERE BANFN EQ I_DOCNO.
    IF SY-SUBRC EQ 0.
      LT_ATTACH = GET_ATTACHMENT( LV_WEBNO ).
    ENDIF.

    CLEAR : LS_CONTENT.
    LS_CONTENT-LINE = 'Dear GA Team'.
    APPEND LS_CONTENT TO R.

    CLEAR : LS_CONTENT.
    LS_CONTENT-LINE = '<BR><BR>'.
    APPEND LS_CONTENT TO R.

    CONCATENATE 'PR No.'
                I_DOCNO
                'has been approved.'
          INTO LS_CONTENT-LINE SEPARATED BY SPACE.

    APPEND LS_CONTENT TO R.

    IF LT_ATTACH IS NOT INITIAL.
      CLEAR : LS_CONTENT.
      LS_CONTENT-LINE = '<BR><BR>'.
      APPEND LS_CONTENT TO R.
      CLEAR : LS_CONTENT.
      LS_CONTENT-LINE = 'Please check below attachment.'.
      APPEND LS_CONTENT TO R.
      LOOP AT LT_ATTACH INTO GS_ATTACH.
        CLEAR : LS_CONTENT.
        LS_CONTENT-LINE = '<BR>'.
        APPEND LS_CONTENT TO R.

        CLEAR : LS_CONTENT.
        CONCATENATE LV_ENDPOINT
                    GS_ATTACH-FILE_PATH
               INTO LV_DOWNLOAD_PATH.

        CONCATENATE '<A HREF="'
                    LV_DOWNLOAD_PATH
                    '">'
                    GS_ATTACH-FILE_NAME_ORIGINAL
                    '</a>'
               INTO LS_CONTENT-LINE.
        APPEND LS_CONTENT TO R.
      ENDLOOP.
    ENDIF.

    CLEAR : LS_CONTENT.
    LS_CONTENT-LINE = '<BR><BR>'.
    APPEND LS_CONTENT TO R.

    CLEAR : LS_CONTENT.
    LS_CONTENT-LINE = 'Best regards,'.
    APPEND LS_CONTENT TO R.
  ENDMETHOD.
  METHOD GET_ATTACHMENT.
    DATA : I_URL              TYPE STRING,
           I_METHOD           TYPE STRING,
           I_HEADER           TYPE ZSDSCAS001_TT,
           I_BODY_TEXT        TYPE STRING,
           I_BODY_BIN         TYPE XSTRING,
           I_LEN              TYPE I,
           I_LEN_BIN          TYPE I,
           I_FROM             TYPE ZSDSCAS001_TT,
           I_USER             TYPE STRING,
           I_PASS             TYPE STRING,
           E_RETURN_BODY_TEXT TYPE STRING,
           E_RETURN_BODY_BIN  TYPE XSTRING,
           E_MESSAGE          TYPE STRING,
           E_STATUS           TYPE C.

    DATA : BEGIN OF LS_DATA,
             VALUE LIKE GT_ATTACH,
           END OF LS_DATA.

    I_METHOD = GC_CON-GET.

    I_URL = GET_URL_ATTACH( ).
    GET_HEADER_API_ATTACH( CHANGING C_HEADER = I_HEADER ).
    GET_BODY_API_ATTACH( CHANGING C_BODY = I_BODY_TEXT
                                  C_LEN  = I_LEN ).

    GET_USER_PASS( CHANGING I_USER = I_USER
                            I_PASS = I_PASS ).


    CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
      EXPORTING
        I_URL              = I_URL
        I_METHOD           = I_METHOD
        I_HEADER           = I_HEADER
        I_BODY_TEXT        = I_BODY_TEXT
        I_BODY_BIN         = I_BODY_BIN
        I_LEN              = I_LEN
        I_LEN_BIN          = I_LEN_BIN
        I_FROM             = I_FROM
        I_USER             = I_USER
        I_PASS             = I_PASS
      IMPORTING
        E_RETURN           = LS_DATA
        E_RETURN_BODY_TEXT = E_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = E_RETURN_BODY_BIN
        E_MESSAGE          = E_MESSAGE
        E_STATUS           = E_STATUS.
    IF SY-SUBRC EQ 0.
      R = LS_DATA-VALUE.
    ENDIF.

  ENDMETHOD.
  METHOD GET_URL_ATTACH.

    CONSTANTS : BEGIN OF LC_CON,
                  SQ    TYPE C LENGTH 1  VALUE '''',
                  REPID TYPE C LENGTH 11 VALUE 'ZSDSCAR0010',
                  PARAM TYPE C LENGTH 6  VALUE 'URL_K2',
                END OF LC_CON.

    DATA : LV_DOC  TYPE STRING,
           LV_CODE TYPE STRING.

    DATA : LV_END_POINT TYPE STRING.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM_EXT         = LC_CON-PARAM
                                        CHANGING  C_RETURN            = LV_END_POINT ).

    CONCATENATE LV_END_POINT
                'Attachements?$filter=pr_request_docid eq'
           INTO LV_END_POINT.

    SELECT WEBNO
      FROM ZSDSMMT006
      INTO TABLE @DATA(LT_WEB)
      WHERE WEBNO IN @S_WEBNO.

    DATA : LS_WEB LIKE LINE OF LT_WEB.

    LOOP AT LT_WEB INTO LS_WEB.
      IF LV_DOC IS INITIAL.
        CONCATENATE LC_CON-SQ LS_WEB LC_CON-SQ INTO LV_DOC.
      ELSE.
        CONCATENATE LC_CON-SQ LS_WEB LC_CON-SQ INTO LV_CODE.
        CONCATENATE LV_DOC 'or' 'pr_request_docid' LV_CODE INTO LV_DOC.
      ENDIF.
    ENDLOOP.
    CONCATENATE LV_END_POINT
                LV_DOC
           INTO R SEPARATED BY SPACE.

  ENDMETHOD.
  METHOD GET_HEADER_API_ATTACH.
    DATA LS_HEADER LIKE LINE OF C_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO C_HEADER.

  ENDMETHOD.
  METHOD GET_BODY_API_ATTACH.


  ENDMETHOD.
  METHOD GET_USER_PASS.

    CONSTANTS : BEGIN OF LC_CON,
                  RAPID TYPE STRING VALUE 'ZSDSCAR0010',
                  PARAM TYPE STRING VALUE '1',
                  USER  TYPE STRING VALUE 'USER',
                  PASS  TYPE STRING VALUE 'PASS',
                END OF LC_CON.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-RAPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = LC_CON-PARAM
                                                  I_PARAM_EXT         = LC_CON-USER
                                         CHANGING C_RETURN            = I_USER ).

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-RAPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = LC_CON-PARAM
                                                  I_PARAM_EXT         = LC_CON-PASS
                                         CHANGING C_RETURN            = I_PASS ).

  ENDMETHOD.
  METHOD GET_DATA_FROM_WEB.
    DATA : BEGIN OF LS_DETAIL_API_K2,
             PR_REQUEST_DETAIL_SEQ TYPE STRING,
             PR_REQUEST_DOCID      TYPE STRING,
             PURCHASE_TYPE         TYPE STRING,
             MATERIAL_GROUP        TYPE STRING,
             BUDGET_ID             TYPE STRING,
             BUDGET_TYPE           TYPE STRING,
             MATERIAL_CODE         TYPE STRING,
             DESCRIPTION           TYPE STRING,
             QTY                   TYPE STRING,
             BASE_UNIT             TYPE STRING,
             AMOUNT                TYPE STRING,
             CURRENCY              TYPE STRING,
             DELIVERY_DATE         TYPE STRING,
             SAP_NO                TYPE STRING,
             SAP_ITEM              TYPE STRING,
             PO_QTY                TYPE STRING,
             STORAGE_LOCATION      TYPE STRING,
             REQUEST_FOR           TYPE STRING,
             ITEM_REMARK           TYPE STRING,
             MEMO_ID               TYPE STRING,
             GL                    TYPE STRING,
             IS_ACTIVE             TYPE STRING,
             COST_CENTER           TYPE STRING,
             CREATE_DATE           TYPE STRING,
             CREATE_BY             TYPE STRING,
             CREATE_BY_NAME        TYPE STRING,
             UPDATE_DATE           TYPE STRING,
             UPDATE_BY             TYPE STRING,
             UPDATE_BY_NAME        TYPE STRING,
           END OF LS_DETAIL_API_K2.

    DATA : BEGIN OF LS_DATA_DETAIL,
             VALUE LIKE TABLE OF LS_DETAIL_API_K2,
           END OF LS_DATA_DETAIL.

    DATA : LV_LINE TYPE C LENGTH 5.

    GET_DETAIL_API_K2( EXPORTING I_WEB_NO = I_WEB
                        CHANGING C_DETAIL = LS_DATA_DETAIL ).

    IF LS_DATA_DETAIL-VALUE IS NOT INITIAL.

      SELECT MSEHI,
             MSEHL
        FROM T006A
        INTO TABLE @DATA(LT_T006A)
        WHERE SPRAS EQ @SY-LANGU.

      LOOP AT LS_DATA_DETAIL-VALUE INTO LS_DETAIL_API_K2.
        GS_RESULT-CHECK      = ABAP_TRUE.
        GS_RESULT-BANFN      = I_PR.
        ADD 1 TO GS_RESULT-BNFPO.
*        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_LINE WITH ''.
*        GS_RESULT-BNFPO = |{ LV_LINE ALPHA = OUT }|.

        REPLACE ALL OCCURRENCES OF PCRE '-' IN LS_DETAIL_API_K2-CREATE_DATE WITH SPACE.
        GS_RESULT-ERDAT      = LS_DETAIL_API_K2-CREATE_DATE.
        GS_RESULT-ERNAM      = LS_DETAIL_API_K2-CREATE_BY.
        GS_RESULT-MATNR      = LS_DETAIL_API_K2-MATERIAL_CODE .
        GS_RESULT-TXZ01      = LS_DETAIL_API_K2-DESCRIPTION.
        GS_RESULT-MENGE      = LS_DETAIL_API_K2-QTY.
        GS_RESULT-MEINS      = LS_DETAIL_API_K2-BASE_UNIT.
        IF LS_DETAIL_API_K2-QTY NE 0.
          GS_RESULT-PREIS      = LS_DETAIL_API_K2-AMOUNT / LS_DETAIL_API_K2-QTY.
        ENDIF.
        GS_RESULT-WAERS      = LS_DETAIL_API_K2-CURRENCY.
        REPLACE ALL OCCURRENCES OF PCRE '-' IN LS_DETAIL_API_K2-DELIVERY_DATE WITH SPACE.
        GS_RESULT-LFDAT      = LS_DETAIL_API_K2-DELIVERY_DATE.
        GS_RESULT-SAKTO      = LS_DETAIL_API_K2-GL.
        GS_RESULT-KOSTL      = LS_DETAIL_API_K2-COST_CENTER.
        IF LS_DETAIL_API_K2-BUDGET_TYPE  EQ 'IO'.
          GS_RESULT-AUFNR      =  LS_DETAIL_API_K2-BUDGET_ID.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
            EXPORTING
              INPUT     = LS_DETAIL_API_K2-BUDGET_ID
            IMPORTING
              OUTPUT    = GS_RESULT-PS_PSP_PNR
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.
        ENDIF.
        READ TABLE LT_T006A INTO DATA(LS_T006A)
        WITH KEY MSEHI = GS_RESULT-MEINS.
        IF SY-SUBRC EQ 0.
          GS_RESULT-MSEHL = LS_T006A-MSEHL.
        ENDIF.

        APPEND GS_RESULT TO GT_RESULT.
        CLEAR : GS_RESULT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD GET_HEADER_API_K2.

    DATA : I_URL              TYPE STRING,
           I_METHOD           TYPE STRING,
           I_HEADER           TYPE ZSDSCAS001_TT,
           I_BODY_TEXT        TYPE STRING,
           I_BODY_BIN         TYPE XSTRING,
           I_LEN              TYPE I,
           I_LEN_BIN          TYPE I,
           I_FROM             TYPE ZSDSCAS001_TT,
           I_USER             TYPE STRING,
           I_PASS             TYPE STRING,
           E_RETURN_BODY_TEXT TYPE STRING,
           E_RETURN_BODY_BIN  TYPE XSTRING,
           E_MESSAGE          TYPE STRING,
           E_STATUS           TYPE C.

    I_METHOD = GC_CON-GET.

    LCL_DATA=>GET_URL_HEADER( EXPORTING I_WEB_NO = I_WEB_NO
                               CHANGING C_URL    = I_URL ).
    LCL_DATA=>GET_HEADER_API_HEADER( CHANGING C_HEADER = I_HEADER ).
    LCL_DATA=>GET_DETAIL_API_HEADER( CHANGING C_BODY = I_BODY_TEXT
                                              C_LEN  = I_LEN ).

    LCL_DATA=>GET_USER_PASS( CHANGING I_USER = I_USER
                                      I_PASS = I_PASS ).


    CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
      EXPORTING
        I_URL              = I_URL
        I_METHOD           = I_METHOD
        I_HEADER           = I_HEADER
        I_BODY_TEXT        = I_BODY_TEXT
        I_BODY_BIN         = I_BODY_BIN
        I_LEN              = I_LEN
        I_LEN_BIN          = I_LEN_BIN
        I_FROM             = I_FROM
        I_USER             = I_USER
        I_PASS             = I_PASS
      IMPORTING
        E_RETURN           = C_HEADER
        E_RETURN_BODY_TEXT = E_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = E_RETURN_BODY_BIN
        E_MESSAGE          = E_MESSAGE
        E_STATUS           = E_STATUS.
    IF SY-SUBRC EQ 0.
      MESSAGE S000(ZSDSDMM01) WITH E_RETURN_BODY_TEXT.
    ENDIF.

  ENDMETHOD.
  METHOD GET_DETAIL_API_K2.

    DATA : I_URL              TYPE STRING,
           I_METHOD           TYPE STRING,
           I_HEADER           TYPE ZSDSCAS001_TT,
           I_BODY_TEXT        TYPE STRING,
           I_BODY_BIN         TYPE XSTRING,
           I_LEN              TYPE I,
           I_LEN_BIN          TYPE I,
           I_FROM             TYPE ZSDSCAS001_TT,
           I_USER             TYPE STRING,
           I_PASS             TYPE STRING,
           E_RETURN_BODY_TEXT TYPE STRING,
           E_RETURN_BODY_BIN  TYPE XSTRING,
           E_MESSAGE          TYPE STRING,
           E_STATUS           TYPE C.

    I_METHOD = GC_CON-GET.

    LCL_DATA=>GET_URL_DETAIL( EXPORTING I_WEB_NO =  I_WEB_NO
                               CHANGING C_URL    = I_URL ).
    LCL_DATA=>GET_HEADER_API_DETAIL( CHANGING C_HEADER = I_HEADER ).
    LCL_DATA=>GET_DETAIL_API_DETAIL( CHANGING C_BODY = I_BODY_TEXT
                                              C_LEN  = I_LEN ).

    LCL_DATA=>GET_USER_PASS( CHANGING I_USER = I_USER
                                      I_PASS = I_PASS ).

    CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
      EXPORTING
        I_URL              = I_URL
        I_METHOD           = I_METHOD
        I_HEADER           = I_HEADER
        I_BODY_TEXT        = I_BODY_TEXT
        I_BODY_BIN         = I_BODY_BIN
        I_LEN              = I_LEN
        I_LEN_BIN          = I_LEN_BIN
        I_FROM             = I_FROM
        I_USER             = I_USER
        I_PASS             = I_PASS
      IMPORTING
        E_RETURN           = C_DETAIL
        E_RETURN_BODY_TEXT = E_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = E_RETURN_BODY_BIN
        E_MESSAGE          = E_MESSAGE
        E_STATUS           = E_STATUS.
    IF SY-SUBRC EQ 0.
*      MESSAGE S000(ZSDSDMM01) WITH E_RETURN_BODY_TEXT.
    ENDIF.
  ENDMETHOD.
  METHOD GET_URL_HEADER.

    CONSTANTS : BEGIN OF LC_CON,
                  SQ TYPE C LENGTH 1 VALUE '''',
                END OF LC_CON.

    DATA : LV_DOC TYPE STRING.

    DATA : LV_END_POINT TYPE STRING.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSCAR0010'
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM_EXT         = 'URL_K2'
                                        CHANGING  C_RETURN            = LV_END_POINT ).

    CONCATENATE LV_END_POINT
                'prHeaders?$filter=pr_request_docid eq'
           INTO LV_END_POINT.

    CONCATENATE LC_CON-SQ I_WEB_NO LC_CON-SQ INTO LV_DOC.

    CONCATENATE LV_END_POINT
                LV_DOC
           INTO C_URL SEPARATED BY SPACE.

  ENDMETHOD.
  METHOD GET_HEADER_API_HEADER.
    DATA LS_HEADER LIKE LINE OF C_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO C_HEADER.

  ENDMETHOD.
  METHOD GET_DETAIL_API_HEADER.


  ENDMETHOD.
  METHOD GET_URL_DETAIL.
    CONSTANTS : BEGIN OF LC_CON,
                  SQ TYPE C LENGTH 1 VALUE '''',
                END OF LC_CON.

    DATA : LV_DOC TYPE STRING.

    DATA : LV_END_POINT TYPE STRING.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSCAR0010'
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM_EXT         = 'URL_K2'
                                        CHANGING  C_RETURN            = LV_END_POINT ).

    CONCATENATE LV_END_POINT
                'prDetails?$filter=pr_request_docid eq'
           INTO LV_END_POINT.

    CONCATENATE LC_CON-SQ I_WEB_NO LC_CON-SQ INTO LV_DOC.

    CONCATENATE LV_END_POINT
                LV_DOC
           INTO C_URL SEPARATED BY SPACE.

  ENDMETHOD.
  METHOD GET_HEADER_API_DETAIL.
    DATA LS_HEADER LIKE LINE OF C_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO C_HEADER.

  ENDMETHOD.
  METHOD GET_DETAIL_API_DETAIL.

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
