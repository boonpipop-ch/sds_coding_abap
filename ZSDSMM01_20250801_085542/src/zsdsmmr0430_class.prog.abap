*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0430_CLASS
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
      GET_URL RETURNING VALUE(R) TYPE STRING,
      GET_HEADER_API CHANGING C_HEADER TYPE ZSDSCAS001_TT,
      GET_BODY_API CHANGING C_BODY TYPE ANY
                            C_LEN  TYPE ANY,
      GET_USER_PASS CHANGING I_USER TYPE STRING
                             I_PASS TYPE STRING,
      DOWNLOAD.

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
             VALUE LIKE TABLE OF GS_RESULT,
           END OF LS_DATA.

    I_METHOD = GC_CON-GET.

    I_URL = GET_URL( ).
    GET_HEADER_API( CHANGING C_HEADER = I_HEADER ).
    GET_BODY_API( CHANGING C_BODY = I_BODY_TEXT
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
      GT_RESULT = LS_DATA-VALUE.
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

    CONSTANTS : BEGIN OF LC_CON,
                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
                  CHK_NAME  TYPE C LENGTH 3 VALUE 'CHK',
                END OF LC_CON.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = LC_CON-CHK_FILED.
    LS_FCAT-SELTEXT_S   = LC_CON-CHK_NAME.
    LS_FCAT-SELTEXT_M   = LC_CON-CHK_NAME.
    LS_FCAT-SELTEXT_L   = LC_CON-CHK_FILED.
    LS_FCAT-CHECKBOX    = ABAP_TRUE.
    LS_FCAT-INPUT       = ABAP_TRUE.
    LS_FCAT-EDIT        = ABAP_TRUE.
    APPEND LS_FCAT TO GT_FCAT.

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
        I_CALLBACK_PROGRAM       = SY-REPID
        I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
        I_callback_user_command  = 'USER_COMMAND'
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       i_html_height_top        = 12
*       I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME         =
*       I_BACKGROUND_ID          = ' '
*       I_GRID_TITLE             =
*       I_GRID_SETTINGS          =
        IS_LAYOUT                = GS_LAYOUT
        IT_FIELDCAT              = GT_FCAT
*       IT_EXCLUDING             =
*       IT_SPECIAL_GROUPS        =
        IT_SORT                  = GT_SORT
*       IT_FILTER                =
*       IS_SEL_HIDE              =
        I_DEFAULT                = GC_X
        I_SAVE                   = GC_A
*       IS_VARIANT               =
*       IT_EVENTS                =
*       IT_EVENT_EXIT            =
*       IS_PRINT                 =
*       IS_REPREP_ID             =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE        = 0
*       I_HTML_HEIGHT_TOP        = 0
*       I_HTML_HEIGHT_END        = 0
*       IT_ALV_GRAPHICS          =
*       IT_HYPERLINK             =
*       IT_ADD_FIELDCAT          =
*       IT_EXCEPT_QINFO          =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        T_OUTTAB                 = GT_RESULT
      EXCEPTIONS
        PROGRAM_ERROR            = 1
        OTHERS                   = 2.
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
  METHOD GET_URL.

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
  METHOD GET_HEADER_API.
    DATA LS_HEADER LIKE LINE OF C_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO C_HEADER.

  ENDMETHOD.
  METHOD GET_BODY_API.


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
  METHOD DOWNLOAD.

    DATA : LV_URL TYPE C LENGTH 2500.

    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 11 VALUE 'ZSDSMMR0390',
                  PARAM TYPE C LENGTH 10 VALUE 'URL_ATTACH',
                END OF LC_CON.

    DATA : LV_ENDPOINT TYPE STRING.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = LC_CON-REPID
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                    I_PARAM             = LC_CON-PARAM
                                          CHANGING  C_RETURN            = LV_ENDPOINT ).

    LOOP AT GT_RESULT INTO GS_RESULT WHERE CHECK EQ ABAP_TRUE.
      CONCATENATE LV_ENDPOINT
                  GS_RESULT-FILE_PATH
             INTO LV_URL.
      CALL FUNCTION 'CALL_BROWSER'
        EXPORTING
          URL        = LV_URL
*         window_name = 'X'
          NEW_WINDOW = 'X'.
    ENDLOOP.
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
