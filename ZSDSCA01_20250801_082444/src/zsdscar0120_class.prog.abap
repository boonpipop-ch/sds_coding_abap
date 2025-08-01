*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0120_CLASS
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
      GET_CONTENT  IMPORTING P_REQNM  TYPE RP50G-PERNR
                   RETURNING VALUE(R) TYPE SOLI_TAB,
      ATTACH_FILE  IMPORTING IV_FILENAME TYPE ZSDSCAS019-FILENAME
                   EXPORTING ES_ATTACH   TYPE ZSDSCAS019.
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

    DATA LV_FM_NAME TYPE RS38L_FNAM.
    DATA: LS_RESULT  TYPE ZSDSCAS0121,
          LS_CONTROL TYPE SSFCTRLOP,
          LS_OUTPUT  TYPE SSFCOMPOP.

    DATA : LS_CONTROL_PARAM  TYPE SSFCTRLOP,
           LS_COMPOSER_PARAM TYPE SSFCOMPOP,
           LS_RECIPIENT      TYPE SWOTOBJID,
           LS_SENDER         TYPE SWOTOBJID,
           LS_ARC_PARAMS     TYPE ARC_PARAMS,
           LS_TOA_DARA       TYPE TOA_DARA,
           LS_JOB_INFO       TYPE SSFCRESCL.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.


    DATA : LS_ATTACH_FILE TYPE ZSDSCAS019,
           LT_ATTACH_FILE TYPE ZSDSCAS019_TT.

    DATA: LT_CC TYPE STANDARD TABLE OF STRING,
          LS_CC TYPE STRING.

    DATA : LT_HTML_CON    TYPE SOLI_TAB.

    DATA: LT_RECEIVER TYPE STANDARD TABLE OF STRING,
          LV_RECEIVER TYPE STRING.

    DATA: LS_ATTACH_FROM_FILE TYPE ZSDSCAS019.

*    IF LCL_UTIL IS NOT BOUND.
*      CREATE OBJECT LCL_UTIL.
*    ENDIF.

    CALL METHOD LCL_DATA=>ATTACH_FILE
      EXPORTING
        IV_FILENAME = P_FILES
      IMPORTING
        ES_ATTACH   = LS_ATTACH_FROM_FILE.

    APPEND LS_ATTACH_FROM_FILE TO LT_ATTACH_FILE.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = 'ZSDSCA001'
      IMPORTING
        FM_NAME            = LV_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.

    IF SY-SUBRC <> 0.
      MESSAGE 'Smart Form not found' TYPE 'E'.
    ENDIF.

    LS_RESULT-ITREQ = P_ITREQ.
    LS_RESULT-SUBJT = P_SUBJT.
    LS_RESULT-INFMT = P_INFMT.
    LS_RESULT-TCODE = P_TCODE.
    LS_RESULT-REQNM = P_REQNM.
    LS_RESULT-EMAIL = P_EMAIL.
    LS_RESULT-SENDD = P_SENDD.
    LS_RESULT-CCEML = S_CCEML.
    LS_RESULT-FILES = P_FILES.

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
        GS_RESULT          = LS_RESULT
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

    ELSE.

      LT_HTML_CON = LCL_DATA=>GET_CONTENT( P_REQNM = P_REQNM ).

      LV_RECEIVER = P_EMAIL.
      APPEND LV_RECEIVER TO LT_RECEIVER.

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
        LS_ATTACH_FILE-FILENAME = 'UAT CONFIRMATION SIGNED PROJECT.pdf'.
        LS_ATTACH_FILE-LEN     = LV_BIN_FILESIZE.


        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            BUFFER        = LV_BIN_FILE
          IMPORTING
            OUTPUT_LENGTH = LV_BIN_FILESIZE
          TABLES
            BINARY_TAB    = LS_ATTACH_FILE-DATAF.

        APPEND LS_ATTACH_FILE TO LT_ATTACH_FILE.

        LV_RECEIVER = |{ P_EMAIL }|.
        APPEND LV_RECEIVER TO LT_RECEIVER.

        LOOP AT S_CCEML INTO DATA(LS_RANGE).
          LS_CC = LS_RANGE-LOW.
          APPEND LS_CC TO LT_CC.
        ENDLOOP.

        LCL_UTIL->SEND_MAIL( I_SUBJECT      = P_SUBJT
                             IT_HTML_CON    = LT_HTML_CON
                             IT_ATTACH_FILE = LT_ATTACH_FILE
                             I_SENDER_EMAIL = P_SENDD
                             I_SENDER_NAME  = ' '
                             IT_RECEIVER    = LT_RECEIVER
                             IT_CC          = LT_CC ).
      ENDIF.
    ENDIF.

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
  METHOD GET_CONTENT.


    DATA : LS_CONTENT TYPE SOLI.
    CLEAR R.

    LS_CONTENT-LINE = |<html><body>|.
    APPEND LS_CONTENT TO R.

    CLEAR LS_CONTENT.
    CONCATENATE '<p>Dear Khun ' P_REQNM ',</p>' INTO LS_CONTENT-LINE.
    APPEND LS_CONTENT TO R.

    CLEAR LS_CONTENT.
    LS_CONTENT-LINE = '<br>'.
    APPEND LS_CONTENT TO R.
    CLEAR LS_CONTENT.
    LS_CONTENT-LINE = '<p>Please find the attached file for your reference.</p>'.
    APPEND LS_CONTENT TO R.

    CLEAR LS_CONTENT.
    LS_CONTENT-LINE = '<p>If you have any questions, please feel free to contact us.</p>'.
    APPEND LS_CONTENT TO R.

    CLEAR LS_CONTENT.
    LS_CONTENT-LINE = '<br><p>Best regards,</p>'.
    APPEND LS_CONTENT TO R.

    CLEAR LS_CONTENT.
    LS_CONTENT-LINE = '<p>Your SAP System</p>'.
    APPEND LS_CONTENT TO R.

    CLEAR LS_CONTENT.
    LS_CONTENT-LINE = |</body></html>|.
    APPEND LS_CONTENT TO R.

* LS_CONTENT-LINE = |<html><body style="margin:0;padding:0;font-family:Arial,sans-serif;font-size:14px;">|.
*  APPEND LS_CONTENT TO R.
*
** Header bar
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = |<div style="background-color:#00BFFF;padding:15px;color:white;text-align:center;">|.
*  APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '<h2> User Accept Test </h2>'.
*  APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '</div>'.
*  APPEND LS_CONTENT TO R.
*
** Body with border
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = |<div style="border:1px solid #ccc; padding:20px; background-color:#f9f9f9;">|.
*  APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  CONCATENATE '<p>Dear Khun ' P_REQNM ',</p>' INTO LS_CONTENT-LINE.
*  APPEND LS_CONTENT TO R.
*
*CLEAR LS_CONTENT.
*    LS_CONTENT-LINE = '<br><br>'.
*    APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '<p>Please find the attached file for your reference.</p>'.
*  APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '<p>If you have any questions, please feel free to contact us.</p>'.
*  APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '<br><p>Best regards,</p>'.
*  APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '<p>Your SAP System</p>'.
*  APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '</div>'.
*  APPEND LS_CONTENT TO R.
*
*  CLEAR LS_CONTENT.
*  LS_CONTENT-LINE = '</body></html>'.
*  APPEND LS_CONTENT TO R.





  ENDMETHOD.
  METHOD ATTACH_FILE.
    DATA: LT_BIN_DATA TYPE SOLIX_TAB,
          LV_FILESIZE TYPE I.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        FILENAME        = IV_FILENAME
        FILETYPE        = 'BIN'
      IMPORTING
        FILELENGTH      = LV_FILESIZE
      TABLES
        DATA_TAB        = LT_BIN_DATA
      EXCEPTIONS
        FILE_OPEN_ERROR = 1
        FILE_READ_ERROR = 2
        OTHERS          = 3.

    IF SY-SUBRC = 0.
      ES_ATTACH-FILENAME = IV_FILENAME.
      ES_ATTACH-GROUP = 2.
      ES_ATTACH-DATAF = LT_BIN_DATA.
      ES_ATTACH-LEN = LV_FILESIZE.
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
