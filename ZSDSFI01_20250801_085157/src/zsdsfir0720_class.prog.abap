*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0720_CLASS
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
      UPDATE_TEXT IMPORTING I_NAME TYPE STRING
                            I_ID   TYPE CHAR4
                            I_TEXT TYPE GY_LEN_5000,
      INSERT_TEXT IMPORTING I_NAME TYPE STRING
                            I_ID   TYPE CHAR4
                            I_TEXT TYPE GY_LEN_5000,
      SAVE,
      MODIFY_TEXT IMPORTING I_BUKRS TYPE BSEG-BUKRS
                            I_BELNR TYPE BSEG-BELNR
                            I_GJAHR TYPE BSEG-GJAHR
                            I_BUZEI TYPE BSEG-BUZEI
                            I_ID    TYPE CHAR4
                            I_TEXT  TYPE GY_LEN_5000.
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
*    DATA: IT_RAW   TYPE TRUXS_T_TEXT_DATA,
*          LV_TABIX TYPE SY-TABIX.
*
*    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
*      EXPORTING
*        I_FIELD_SEPERATOR    = 'X'
*        I_LINE_HEADER        = 'X'
*        I_TAB_RAW_DATA       = IT_RAW
*        I_FILENAME           = P_FILE
*      TABLES
*        I_TAB_CONVERTED_DATA = GT_RESULT
*      EXCEPTIONS
*        CONVERSION_FAILED    = 1
*        OTHERS               = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
*      <LFS_DATA>-STATUS = GC_WARN.
*    ENDLOOP.

    DATA: IT_RAW   TYPE TRUXS_T_TEXT_DATA,
          LV_TABIX TYPE SY-TABIX.

    DATA: LV_SCOL TYPE I VALUE 1,
          LV_SROW TYPE I VALUE 2,
          LV_ECOL TYPE I VALUE 100,
          LV_EROW TYPE I VALUE 99999.

    DATA: LT_TABLE TYPE ZSDSFIS190_TT,
          LS_TABLE TYPE ZSDSFIS190.

    DATA: LV_COUNT TYPE I.

    CALL FUNCTION 'Z_SDSFI_UPLOAD_EXCEL'
      EXPORTING
        FILENAME                = P_FILE
        I_BEGIN_COL             = LV_SCOL
        I_BEGIN_ROW             = LV_SROW
        I_END_COL               = LV_ECOL
        I_END_ROW               = LV_EROW
      TABLES
        INTERN                  = LT_TABLE
      EXCEPTIONS
        INCONSISTENT_PARAMETERS = 1
        UPLOAD_OLE              = 2
        OTHERS                  = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    LOOP AT LT_TABLE INTO DATA(LS_TMP).
      MOVE-CORRESPONDING LS_TMP TO LS_TABLE.
      IF LS_TABLE-COL EQ 1.
        GS_RESULT-VBELN          = LS_TABLE-VALUE.
      ELSEIF LS_TABLE-COL EQ 2.
        GS_RESULT-ISSUE_LIST     = LS_TABLE-VALUE.
      ELSEIF LS_TABLE-COL EQ 3.
        GS_RESULT-FOLLOWDT       = LS_TABLE-VALUE.
      ELSEIF LS_TABLE-COL EQ 4.
        GS_RESULT-TEXT1          = LS_TABLE-VALUE.
      ELSEIF LS_TABLE-COL EQ 5.
        GS_RESULT-TEXT2          = LS_TABLE-VALUE.
      ELSEIF LS_TABLE-COL EQ 6.
        GS_RESULT-TEXT3          = LS_TABLE-VALUE.
      ENDIF.

      AT END OF ROW.
        GS_RESULT-STATUS = GC_WARN.
        APPEND GS_RESULT TO GT_RESULT.
        CLEAR : GS_RESULT.
      ENDAT.
    ENDLOOP.




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
  METHOD SELECT_INPUT_FILE_NAME.
    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        PROGRAM_NAME  = SY-REPID
        DYNPRO_NUMBER = SY-DYNNR
        FIELD_NAME    = 'PATH'
      IMPORTING
        FILE_NAME     = GV_TMP_FILE_PATH.
  ENDMETHOD.
  METHOD UPDATE_TEXT.

    DATA : LS_HEADER TYPE THEAD.

    DATA : LT_LINES	TYPE TABLE OF TLINE,
           LS_LINES	TYPE TLINE.

    DATA : IV_TEXT TYPE GY_LEN_5000.

    DATA : LV_LEN     TYPE I,
           LV_LEN_CHK TYPE I.

    LS_HEADER-TDOBJECT = 'ZSDSFI001'.
    LS_HEADER-TDNAME   = I_NAME.
    LS_HEADER-TDID     = I_ID.
    LS_HEADER-TDSPRAS  = SY-LANGU.

    LS_LINES-TDFORMAT = '*'.

    IV_TEXT = I_TEXT.
    DO.
      LV_LEN = STRLEN( IV_TEXT ).

      IF LV_LEN LE 132.
        LS_LINES-TDLINE = IV_TEXT.
        APPEND LS_LINES TO LT_LINES.
        EXIT.
      ELSE.
        LS_LINES-TDLINE = IV_TEXT+0(132).
        LV_LEN_CHK = LV_LEN - 132.
        IV_TEXT = IV_TEXT+132(LV_LEN_CHK).
        APPEND LS_LINES TO LT_LINES.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*       CLIENT   = SY-MANDT
        HEADER   = LS_HEADER
        INSERT   = SPACE
*       SAVEMODE_DIRECT         = ' '
*       OWNER_SPECIFIED         = ' '
*       LOCAL_CAT               = ' '
*       KEEP_LAST_CHANGED       = ' '
*     IMPORTING
*       FUNCTION =
*       NEWHEADER               =
      TABLES
        LINES    = LT_LINES
      EXCEPTIONS
        ID       = 1
        LANGUAGE = 2
        NAME     = 3
        OBJECT   = 4
        OTHERS   = 5.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
  METHOD INSERT_TEXT.
    DATA : LS_HEADER TYPE THEAD.

    DATA : LT_LINES	TYPE TABLE OF TLINE,
           LS_LINES	TYPE TLINE.

    DATA : IV_TEXT TYPE GY_LEN_5000.

    DATA : LV_LEN     TYPE I,
           LV_LEN_CHK TYPE I.

    LS_HEADER-TDOBJECT = 'ZSDSFI001'.
    LS_HEADER-TDNAME   = I_NAME.
    LS_HEADER-TDID     = I_ID.
    LS_HEADER-TDSPRAS  = SY-LANGU.

    LS_LINES-TDFORMAT = '*'.

    IV_TEXT = I_TEXT.
    DO.
      LV_LEN = STRLEN( IV_TEXT ).

      IF LV_LEN LE 132.
        LS_LINES-TDLINE = IV_TEXT.
        APPEND LS_LINES TO LT_LINES.
        EXIT.
      ELSE.
        LS_LINES-TDLINE = IV_TEXT+0(132).
        LV_LEN_CHK = LV_LEN - 132.
        IV_TEXT = IV_TEXT+132(LV_LEN_CHK).
        APPEND LS_LINES TO LT_LINES.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*       CLIENT   = SY-MANDT
        HEADER   = LS_HEADER
        INSERT   = ABAP_TRUE
*       SAVEMODE_DIRECT         = ' '
*       OWNER_SPECIFIED         = ' '
*       LOCAL_CAT               = ' '
*       KEEP_LAST_CHANGED       = ' '
*     IMPORTING
*       FUNCTION =
*       NEWHEADER               =
      TABLES
        LINES    = LT_LINES
      EXCEPTIONS
        ID       = 1
        LANGUAGE = 2
        NAME     = 3
        OBJECT   = 4
        OTHERS   = 5.
    IF SY-SUBRC <> 0.
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
*         CLIENT   = SY-MANDT
          HEADER   = LS_HEADER
          INSERT   = SPACE
*         SAVEMODE_DIRECT         = ' '
*         OWNER_SPECIFIED         = ' '
*         LOCAL_CAT               = ' '
*         KEEP_LAST_CHANGED       = ' '
*     IMPORTING
*         FUNCTION =
*         NEWHEADER               =
        TABLES
          LINES    = LT_LINES
        EXCEPTIONS
          ID       = 1
          LANGUAGE = 2
          NAME     = 3
          OBJECT   = 4
          OTHERS   = 5.
      IF SY-SUBRC EQ 0.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD SAVE.
    DATA : BEGIN OF LS_DOC,
             VBELN TYPE VBRK-VBELN,
           END OF LS_DOC.
    DATA : LT_DOC LIKE HASHED TABLE OF LS_DOC WITH UNIQUE KEY VBELN.

    DATA : LV_BUZEI TYPE BSEG-BUZEI.

    DATA : LV_TABIX TYPE SY-TABIX VALUE 1.

    DATA : LV_CHECK TYPE I.

    LT_DOC =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).

    SELECT CASE WHEN BKPF~XBLNR IS INITIAL THEN BKPF~BELNR
           ELSE BKPF~XBLNR
           END AS VBELN,
           ACDOCA~RBUKRS,
           ACDOCA~GJAHR,
           ACDOCA~BELNR,
           ACDOCA~DOCLN
      FROM @LT_DOC AS A
*      INNER JOIN VBRP   ON A~VBELN      EQ VBRP~VBELN
      INNER JOIN BKPF   ON A~VBELN   EQ BKPF~XBLNR
      INNER JOIN ACDOCA ON BKPF~BELNR   EQ ACDOCA~BELNR AND
                           ACDOCA~RLDNR EQ '0L'
      INTO TABLE @DATA(LT_DATA).

    SORT LT_DOC BY VBELN.
    SORT LT_DATA BY VBELN.

    LOOP AT LT_DOC INTO LS_DOC.
      CLEAR : LV_CHECK.
      LOOP AT LT_DATA INTO DATA(LS_DATA) FROM LV_TABIX.
        ADD 1 TO LV_CHECK.
        IF LS_DOC-VBELN NE LS_DATA-VBELN.
          LV_TABIX = SY-TABIX.
          EXIT.
        ENDIF.

        READ TABLE GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_RESULT>)
        WITH KEY VBELN = LS_DATA-VBELN.
        IF SY-SUBRC EQ 0.
          IF <LFS_RESULT>-ISSUE_LIST IS NOT INITIAL.
            UPDATE ZSDSFIT040 SET ISSUE_LIST = <LFS_RESULT>-ISSUE_LIST
                                  UPD_DATE   = SY-DATUM
                                  UPD_TIME   = SY-UZEIT
                                  UPD_USER   = SY-UNAME
                            WHERE BUKRS = LS_DATA-RBUKRS
                              AND BELNR = LS_DATA-BELNR
                              AND GJAHR = LS_DATA-GJAHR
                              AND BUZEI = LS_DATA-DOCLN.
            COMMIT WORK AND WAIT.
          ENDIF.

          IF <LFS_RESULT>-FOLLOWDT IS NOT INITIAL.
            UPDATE ZSDSFIT040 SET FOLLOWDT = <LFS_RESULT>-FOLLOWDT
                                  UPD_DATE   = SY-DATUM
                                  UPD_TIME   = SY-UZEIT
                                  UPD_USER   = SY-UNAME
                            WHERE BUKRS = LS_DATA-RBUKRS
                              AND BELNR = LS_DATA-BELNR
                              AND GJAHR = LS_DATA-GJAHR
                              AND BUZEI = LS_DATA-DOCLN.
            COMMIT WORK AND WAIT.
          ENDIF.

          LV_BUZEI = LS_DATA-DOCLN+2(4).
          IF <LFS_RESULT>-TEXT1 IS NOT INITIAL.
            MODIFY_TEXT( I_BUKRS = LS_DATA-RBUKRS
                         I_BELNR = LS_DATA-BELNR
                         I_GJAHR = LS_DATA-GJAHR
                         I_BUZEI = LV_BUZEI
                         I_ID    = 'FI01'
                         I_TEXT  = <LFS_RESULT>-TEXT1 ).
          ENDIF.

          IF <LFS_RESULT>-TEXT2 IS NOT INITIAL.
            MODIFY_TEXT( I_BUKRS = LS_DATA-RBUKRS
                         I_BELNR = LS_DATA-BELNR
                         I_GJAHR = LS_DATA-GJAHR
                         I_BUZEI = LV_BUZEI
                         I_ID    = 'FI02'
                         I_TEXT  = <LFS_RESULT>-TEXT2 ).
          ENDIF.

          IF <LFS_RESULT>-TEXT3 IS NOT INITIAL.
            MODIFY_TEXT( I_BUKRS = LS_DATA-RBUKRS
                         I_BELNR = LS_DATA-BELNR
                         I_GJAHR = LS_DATA-GJAHR
                         I_BUZEI = LV_BUZEI
                         I_ID    = 'FI03'
                         I_TEXT  = <LFS_RESULT>-TEXT3 ).
          ENDIF.
          <LFS_RESULT>-STATUS  = GC_SUCS.
          <LFS_RESULT>-MESSAGE = TEXT-S01.
        ELSE.
          <LFS_RESULT>-STATUS  = GC_ERRO.
          <LFS_RESULT>-MESSAGE = TEXT-E01.
        ENDIF.
      ENDLOOP.
      IF LV_CHECK EQ 1.
        IF <LFS_RESULT> IS ASSIGNED.
          <LFS_RESULT>-STATUS  = GC_ERRO.
          <LFS_RESULT>-MESSAGE = TEXT-E02.
        ENDIF.
      ELSEIF LV_CHECK EQ 0.
        GS_RESULT-STATUS  = GC_ERRO.
        GS_RESULT-MESSAGE = TEXT-E03.
        MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING STATUS MESSAGE
                                               WHERE VBELN EQ LS_DOC-VBELN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD MODIFY_TEXT.
    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA : LV_NAME TYPE STRING.

    IF LCL_UTIL IS NOT BOUND.
      CREATE OBJECT LCL_UTIL.
    ENDIF.

    CONCATENATE I_BUKRS I_BELNR I_GJAHR I_BUZEI
    INTO LV_NAME.

    DATA(LV_TEXT) = LCL_UTIL->GET_TEXT( I_ID       = I_ID
                                        I_NAME     = LV_NAME
                                        I_OBJECT   = 'ZSDSFI001'
                                        I_LANGUAGE = SY-LANGU ).
    IF LV_TEXT IS INITIAL.
      INSERT_TEXT( I_NAME = LV_NAME
                   I_ID   = I_ID
                   I_TEXT = I_TEXT ).
    ELSE.
      UPDATE_TEXT( I_NAME = LV_NAME
                   I_ID   = I_ID
                   I_TEXT = I_TEXT ).
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
