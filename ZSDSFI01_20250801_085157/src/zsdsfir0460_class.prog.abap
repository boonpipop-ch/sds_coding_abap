*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0460_CLASS
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
      SELECT_INPUT_FILE_NAME,
      SAVE,
      UPDATE_ZSDSFIT002,
      UPDATE_ZSDSFIT003,
      UPDATE_ZSDSFIT022,
      COMMIT.
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
    DATA: IT_RAW   TYPE TRUXS_T_TEXT_DATA,
          LV_TABIX TYPE SY-TABIX.

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        I_FIELD_SEPERATOR    = 'X'
        I_LINE_HEADER        = 'X'
        I_TAB_RAW_DATA       = IT_RAW
        I_FILENAME           = P_FILE
      TABLES
        I_TAB_CONVERTED_DATA = GT_RESULT
      EXCEPTIONS
        CONVERSION_FAILED    = 1
        OTHERS               = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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
  METHOD SELECT_INPUT_FILE_NAME.
    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        PROGRAM_NAME  = SY-REPID
        DYNPRO_NUMBER = SY-DYNNR
        FIELD_NAME    = 'PATH'
      IMPORTING
        FILE_NAME     = GV_TMP_FILE_PATH.
  ENDMETHOD.
  METHOD SAVE.
    LOOP AT GT_RESULT INTO GS_RESULT.
      UPDATE_ZSDSFIT002( ).
      UPDATE_ZSDSFIT003( ).
      UPDATE_ZSDSFIT022( ).
    ENDLOOP.
    COMMIT( ).
  ENDMETHOD.
  METHOD UPDATE_ZSDSFIT002.
    DATA : LS_ZSDSFIT002 TYPE ZSDSFIT002.

    LS_ZSDSFIT002-CONTN = GS_RESULT-CONTRACT_NUMBER .
    LS_ZSDSFIT002-LIFNR = GS_RESULT-VENDOR.
    LS_ZSDSFIT002-RUNNG = 1.
    LS_ZSDSFIT002-BUKRS = '1000'.
    LS_ZSDSFIT002-ANLN1 = GS_RESULT-ASSET_NUMBER    .
    LS_ZSDSFIT002-ANLN2 = GS_RESULT-SUB_ASSET_NUMBER.
    LS_ZSDSFIT002-ASSNA = ''.
    LS_ZSDSFIT002-WAERS = GS_RESULT-CURRENCY .
    LS_ZSDSFIT002-INTER = GS_RESULT-INTEREST_MLR.
    LS_ZSDSFIT002-INTEE = GS_RESULT-INTEREST_EIR.
    LS_ZSDSFIT002-LEASA = GS_RESULT-LEASE_ASSET_NAME.
    LS_ZSDSFIT002-STADT = GS_RESULT-START_DATE.
    LS_ZSDSFIT002-ENDDT = GS_RESULT-END_DATE.
    LS_ZSDSFIT002-PAYDT = ''.
    LS_ZSDSFIT002-COSEV = GS_RESULT-RENT_EXCLUDE_VAT.
    LS_ZSDSFIT002-VATMT = GS_RESULT-VAT.
    LS_ZSDSFIT002-RESID = GS_RESULT-RESIDUAL_VALUE.
    LS_ZSDSFIT002-LEAST = ''.
    LS_ZSDSFIT002-PAYMP = ''.
    LS_ZSDSFIT002-TIMES = ''.
    LS_ZSDSFIT002-RENEV = ''.
    LS_ZSDSFIT002-DEPOS = ''.
    LS_ZSDSFIT002-TODAY = ''.
    LS_ZSDSFIT002-FLAGE = ''.
    LS_ZSDSFIT002-EXTED = ''.
    LS_ZSDSFIT002-REMAK = ''.
    LS_ZSDSFIT002-STATU = ''.
    LS_ZSDSFIT002-NPVFG = GS_RESULT-NPV .
    LS_ZSDSFIT002-PVFAG = GS_RESULT-PV.
    LS_ZSDSFIT002-GLREN = GS_RESULT-GL_RENT.
    LS_ZSDSFIT002-FLAGP = GS_RESULT-PREPAID.
    LS_ZSDSFIT002-PRCTR = GS_RESULT-PROFIT_CENTER.
    LS_ZSDSFIT002-ACPVL = GS_RESULT-ACCEPT_VALUE.
    LS_ZSDSFIT002-ERNAM = SY-UNAME.
    LS_ZSDSFIT002-ERDAT = SY-DATUM.
    LS_ZSDSFIT002-ERZET = SY-UZEIT.
    LS_ZSDSFIT002-AENAM = SY-UNAME.
    LS_ZSDSFIT002-AEDAT = SY-DATUM.
    LS_ZSDSFIT002-AEZET = SY-UZEIT.

    MODIFY ZSDSFIT002 FROM LS_ZSDSFIT002.
  ENDMETHOD.
  METHOD UPDATE_ZSDSFIT003.
    DATA : LS_ZSDSFIT003 TYPE ZSDSFIT003.

    LS_ZSDSFIT003-CONTN = ''.
    LS_ZSDSFIT003-LIFNR = ''.
    LS_ZSDSFIT003-RUNNG = ''.
    LS_ZSDSFIT003-MONIT = ''.
    LS_ZSDSFIT003-STDDT = ''.
    LS_ZSDSFIT003-ENDDT = ''.
    LS_ZSDSFIT003-INTER = ''.
    LS_ZSDSFIT003-AMOZT = ''.
    LS_ZSDSFIT003-VATMT = ''.
    LS_ZSDSFIT003-OUTSC = ''.
    LS_ZSDSFIT003-DEPRC = ''.
    LS_ZSDSFIT003-ERNAM = ''.
    LS_ZSDSFIT003-ERDAT = ''.
    LS_ZSDSFIT003-ERZET = ''.
    LS_ZSDSFIT003-AENAM = ''.
    LS_ZSDSFIT003-AEDAT = ''.
    LS_ZSDSFIT003-AEZET = ''.

    MODIFY ZSDSFIT003 FROM LS_ZSDSFIT003.

  ENDMETHOD.
  METHOD UPDATE_ZSDSFIT022.
    DATA : LS_ZSDSFIT022 TYPE ZSDSFIT022.

    LS_ZSDSFIT022-CONTN = ''.
    LS_ZSDSFIT022-LIFNR = ''.
    LS_ZSDSFIT022-RUNNG = ''.
    LS_ZSDSFIT022-MONIT = ''.
    LS_ZSDSFIT022-TYPES = ''.
    LS_ZSDSFIT022-RUNMP = ''.
    LS_ZSDSFIT022-BUKRS = ''.
    LS_ZSDSFIT022-BELNR = ''.
    LS_ZSDSFIT022-GJAHR = ''.
    LS_ZSDSFIT022-DFLAG = ''.
    LS_ZSDSFIT022-ERNAM = ''.
    LS_ZSDSFIT022-ERDAT = ''.
    LS_ZSDSFIT022-ERZET = ''.
    LS_ZSDSFIT022-AENAM = ''.
    LS_ZSDSFIT022-AEDAT = ''.
    LS_ZSDSFIT022-AEZET = ''.

    MODIFY ZSDSFIT022 FROM LS_ZSDSFIT022.

  ENDMETHOD.
  METHOD COMMIT.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.
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
