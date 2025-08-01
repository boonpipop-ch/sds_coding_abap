*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0520_CLASS
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
      SEND_DATA,
      ENDPOINT CHANGING  C_PATH   TYPE STRING
               RETURNING VALUE(R) TYPE STRING,
      HEADER_API RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY_API IMPORTING I_NAME   TYPE STRING
                         I_PATH   TYPE STRING
                         I_FILE   TYPE XSTRING
               RETURNING VALUE(R) TYPE STRING,
      BODY_API_LEN IMPORTING I_DATA   TYPE STRING
                   RETURNING VALUE(R) TYPE I.
    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.

    CONSTANTS : BEGIN OF LC_CON,
                  S    TYPE C LENGTH 1 VALUE 'S',
                  E    TYPE C LENGTH 1 VALUE 'E',
                  DEV  TYPE C LENGTH 3 VALUE 'F36',
                  QAS  TYPE C LENGTH 3 VALUE 'F46',
                  PRD  TYPE C LENGTH 3 VALUE 'F56',
                  POST TYPE C LENGTH 4 VALUE 'POST',
                END OF LC_CON.
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
    SELECT LIEF_NR,
           POSNR,
           MATNR,
           SERNR,
           @GC_WARN,
           CASE WHEN @CB_TEST EQ @ABAP_TRUE THEN 'Test run mode'
           END AS MESSAGE
      FROM SER01
      INNER JOIN OBJK ON SER01~OBKNR EQ OBJK~OBKNR AND
                         OBJK~TASER  EQ 'SER01'
      WHERE LIEF_NR IN @S_VBELN[]
        AND SUBSTRING( LIEF_NR,1,1 ) = '4'
      INTO TABLE @GT_RESULT.

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
  METHOD SEND_DATA.

    DATA : LT_DATA TYPE TABLE OF CHAR255,
           LS_DATA LIKE LINE OF LT_DATA.

    DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

    DATA : LV_STATUS TYPE C.

    DATA : LV_FILE TYPE EVE_TT_STRING.

    DATA : LV_FILE_NAME TYPE STRING.

    DATA : LV_FILE_PATH TYPE STRING.

    DATA : BEGIN OF LS_BINARY,
             LINE(512) TYPE C,
           END OF LS_BINARY.

    DATA : LT_BINARY LIKE TABLE OF LS_BINARY,
           LV_LEN    TYPE I.

    DATA : LV_FILE_SEND TYPE XSTRING.

    DATA: LV_PATH(100) TYPE C.

    DATA: LV_URL              TYPE STRING,
          LV_METHOD           TYPE STRING,
          LV_BODY_TEXT        TYPE STRING,
          LV_BODY_BIN         TYPE XSTRING,
          LV_LEN_api          TYPE I,
          LV_LEN_BIN          TYPE I,
          LV_RETURN_BODY_TEXT TYPE STRING,
          LV_RETURN_BODY_BIN  TYPE XSTRING,
          LV_MESSAGE          TYPE STRING.

    DATA: LV_POSNR TYPE C LENGTH 6.

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.


    IF LCL_FTP IS NOT BOUND.
      CREATE OBJECT LCL_FTP.
    ENDIF.

    LV_METHOD    = LC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT( CHANGING C_path = LV_FILE_PATH ).
    LT_HEADER    = LCL_DATA=>HEADER_API( ).

    SORT GT_RESULT BY LIEF_NR.
    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
      MOVE-CORRESPONDING <LFS_DATA> TO GS_RESULT.
      LV_POSNR = GS_RESULT-POSNR.
      LV_POSNR = |{ LV_POSNR ALPHA = OUT }|.
      CONCATENATE GS_RESULT-LIEF_NR LV_POSNR GS_RESULT-SERNR INTO LS_DATA RESPECTING BLANKS.
      APPEND LS_DATA TO LT_DATA.
      AT END OF LIEF_NR.

        CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
          IMPORTING
            OUTPUT_LENGTH = LV_LEN
          TABLES
            TEXT_TAB      = LT_DATA
            BINARY_TAB    = LT_BINARY
          EXCEPTIONS
            FAILED        = 1
            OTHERS        = 2.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
        ENDIF.

        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            INPUT_LENGTH = LV_LEN
          IMPORTING
            BUFFER       = LV_FILE_SEND
          TABLES
            BINARY_TAB   = LT_BINARY
          EXCEPTIONS
            FAILED       = 1
            OTHERS       = 2.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
        ENDIF.

        CONCATENATE 'SER_' GS_RESULT-LIEF_NR '.txt' INTO LV_FILE_NAME.
        LV_BODY_TEXT = LCL_DATA=>BODY_API( I_NAME = LV_FILE_NAME
                                           I_PATH = LV_FILE_PATH
                                           I_FILE = LV_FILE_SEND ).
        LV_LEN_api   = LCL_DATA=>BODY_API_LEN( LV_BODY_TEXT ).

        CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
          EXPORTING
            I_URL              = LV_URL
            I_METHOD           = LV_METHOD
            I_HEADER           = LT_HEADER
            I_BODY_TEXT        = LV_BODY_TEXT
            I_BODY_BIN         = LV_BODY_BIN
            I_LEN              = LV_LEN_api
            I_LEN_BIN          = LV_LEN_BIN
          IMPORTING
*           E_RETURN           = LT_DATA_API
            E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
            E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
            E_MESSAGE          = LV_MESSAGE
            E_STATUS           = LV_STATUS.
        IF LV_STATUS EQ 'S'.
          <LFS_DATA>-STATUS  = GC_SUCS.
          <LFS_DATA>-MESSAGE = TEXT-S01.
        ELSE.
          <LFS_DATA>-STATUS = GC_ERRO.
          <LFS_DATA>-MESSAGE = TEXT-E01.
        ENDIF.

        MODIFY GT_RESULT FROM <LFS_DATA> TRANSPORTING STATUS MESSAGE
                                                WHERE LIEF_NR EQ GS_RESULT-LIEF_NR.

        CLEAR LT_DATA.
      ENDAT.
      CLEAR : LS_DATA.
    ENDLOOP.
  ENDMETHOD.
  METHOD ENDPOINT.
    CONSTANTS : BEGIN OF LC_API,
*                  REPID TYPE C LENGTH 22 VALUE 'SONY_STATUS_DO',
                  REPID TYPE C LENGTH 22 VALUE 'SONY_SERIAL',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  API   TYPE C LENGTH 3  VALUE 'API',
                END OF LC_API.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-DEV
                                                      I_PARAM_EXT         = LC_API-API
                                            CHANGING  C_RETURN            = R
                                                      C_RETURN_HIGH       = C_PATH ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-QAS
                                                      I_PARAM_EXT         = LC_API-API
                                            CHANGING  C_RETURN            = R
                                                      C_RETURN_HIGH       = C_PATH ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-PRD
                                                      I_PARAM_EXT         = LC_API-API
                                            CHANGING  C_RETURN            = R
                                                      C_RETURN_HIGH       = C_PATH ).
    ENDCASE.

  ENDMETHOD.
  METHOD HEADER_API.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    DATA : LV_TOKEN TYPE STRING.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.
  ENDMETHOD.
  METHOD BODY_API.
    DATA: BEGIN OF LS_DATA_API,
            _File_Name TYPE STRING,
            _File_Path TYPE STRING,
            _File      TYPE STRING,
          END OF LS_DATA_API.

    LS_DATA_API-_File_Name = I_NAME.
    LS_DATA_API-_File_Path = I_PATH.
    LS_DATA_API-_File      = I_FILE.

    CALL METHOD /UI2/CL_JSON=>SERIALIZE
      EXPORTING
        DATA        = LS_DATA_API
        PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
      RECEIVING
        R_JSON      = R
      EXCEPTIONS
        OTHERS      = 1.
  ENDMETHOD.
  METHOD BODY_API_LEN.
    R = STRLEN( I_DATA ).
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
