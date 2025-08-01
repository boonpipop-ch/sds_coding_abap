*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0660_CLASS
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
      SAVE.
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

    DATA : LV_ANLN1 TYPE ZSDSFIT050-ANLN1,
           LV_ANLN2 TYPE ZSDSFIT050-ANLN2.

    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
      <LFS_RESULT>-STATUS = GC_WARN.
      <LFS_RESULT>-CHECK  = ABAP_TRUE.
      LV_ANLN1            = <LFS_RESULT>-ANLN1.
      LV_ANLN2            = <LFS_RESULT>-ANLN2.
      <LFS_RESULT>-ANLN1  = |{ LV_ANLN1 ALPHA = IN }|.
      <LFS_RESULT>-ANLN2  = |{ LV_ANLN2 ALPHA = IN }|.
    ENDLOOP.
  ENDMETHOD.
  METHOD SHOW_REPORT.
    SET_LAYOUT_OUTPUT( ).
    BUILD_FCAT( ).
    SET_SORT( ).
    SET_ALV_GRID( ).
  ENDMETHOD.
  METHOD SET_LAYOUT_OUTPUT.
    CONSTANTS : BEGIN OF LC_CON,
                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
                END OF LC_CON.
    GS_LAYOUT-ZEBRA             = GC_X.
    GS_LAYOUT-COLWIDTH_OPTIMIZE = GC_X.
    GS_LAYOUT-BOX_FIELDNAME     = LC_CON-CHK_FILED.
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
  METHOD SAVE.
    DATA :  LS_ZSDSFIT050 TYPE ZSDSFIT050.
    DATA :  LS_ZSDSFIT055 TYPE ZSDSFIT055.

    DATA : BEGIN OF LS_ASSET,
             ANLN1 TYPE ZSDSFIT050-ANLN1,
             ANLN2 TYPE ZSDSFIT050-ANLN2,
           END OF LS_ASSET.
    DATA : LT_ASSET LIKE HASHED TABLE OF LS_ASSET WITH UNIQUE KEY ANLN1
                                                                  ANLN2.

    DATA : LV_RUNNG TYPE ZSDSDE_RUNNING6.

    DATA : LV_CHECK TYPE C.

    DATA : LT_INPUT   TYPE ZSDSFIS087_TT,
           LT_MESSAGE TYPE ZSDSFIS088_TT.

    DATA : LS_INPUT   TYPE ZSDSFIS087,
           LS_MESSAGE TYPE ZSDSFIS088.

    LT_ASSET =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).

    SELECT ANLZ~ANLN1,
           ANLZ~ANLN2,
           ANLZ~STORT,
           ZSDSFIT050~ERNAM,
           ZSDSFIT050~ERDAT,
           ZSDSFIT050~ERZET
      FROM @LT_ASSET AS A
      INNER JOIN ANLZ ON A~ANLN1    EQ ANLZ~ANLN1 AND
                         A~ANLN2    EQ ANLZ~ANLN2 AND
                         ANLZ~BUKRS EQ '1000'
      LEFT JOIN ZSDSFIT050 ON ANLZ~ANLN1 EQ ZSDSFIT050~ANLN1 AND
                              ANLZ~ANLN2 EQ ZSDSFIT050~ANLN2
      WHERE ANLZ~BDATU GE @SY-DATUM
        AND ANLZ~ADATU LE @SY-DATUM
      INTO TABLE @DATA(LT_DATA).

    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>) WHERE CHECK EQ ABAP_TRUE.
      READ TABLE LT_DATA INTO DATA(LS_DATA)
      WITH KEY ANLN1 = <LFS_DATA>-ANLN1
               ANLN2 = <LFS_DATA>-ANLN2.
      IF SY-SUBRC EQ 0.

        CLEAR : LV_CHECK.
        SELECT COUNT( * )
          FROM ZSDSCAC009
          WHERE PROCESS EQ 'TRANFASSET'
            AND STATU   EQ ABAP_TRUE.
        IF SY-SUBRC EQ 0.
          CLEAR : LT_INPUT,LT_MESSAGE.
          LS_INPUT-COMCODE              = '1000'.
          LS_INPUT-FIX_ASSET_NO         = <LFS_DATA>-ANLN1.
          LS_INPUT-FIX_ASSET_SUB_NO     = <LFS_DATA>-ANLN2.
          LS_INPUT-OWNER_NEW            = <LFS_DATA>-PERNR.
          LS_INPUT-OWNER_NEW_COSTCENTER = <LFS_DATA>-KOSTL.
          APPEND LS_INPUT TO LT_INPUT.

          CALL FUNCTION 'Z_SDSFI_TRANSFER_FIX_ASSET'
            EXPORTING
              IT_INPUT   = LT_INPUT
            IMPORTING
              ET_MESSAGE = LT_MESSAGE.
          READ TABLE LT_MESSAGE
          WITH KEY MESSAGE_TYPE = 'E' TRANSPORTING NO FIELDS.
          IF SY-SUBRC EQ 0.
            LV_CHECK = ABAP_TRUE.
          ELSE.
            CLEAR : LV_CHECK.
          ENDIF.
        ENDIF.
        IF LV_CHECK IS INITIAL.
          <LFS_DATA>-STATUS    = GC_SUCS.
          <LFS_DATA>-MESSAGE   = TEXT-S01.
          LS_ZSDSFIT050-ANLN1  = <LFS_DATA>-ANLN1.
          LS_ZSDSFIT050-ANLN1  = |{ LS_ZSDSFIT050-ANLN1 ALPHA = IN }|.
          LS_ZSDSFIT050-ANLN2  = <LFS_DATA>-ANLN2.
          LS_ZSDSFIT050-ANLN2  = |{ LS_ZSDSFIT050-ANLN2 ALPHA = IN }|.
          LS_ZSDSFIT050-KOSTL  = <LFS_DATA>-KOSTL.
          LS_ZSDSFIT050-KOSTL  = |{ LS_ZSDSFIT050-KOSTL ALPHA = IN }| .
          LS_ZSDSFIT050-PERNR  = <LFS_DATA>-PERNR.
          LS_ZSDSFIT050-PERNR  = |{ LS_ZSDSFIT050-PERNR ALPHA = IN }|.
          LS_ZSDSFIT050-REMARK = <LFS_DATA>-REMARK.
          LS_ZSDSFIT050-STORT  = LS_DATA-STORT.
          LS_ZSDSFIT050-STATU  = 'CHEKED'.
          IF LS_DATA-ERNAM IS NOT INITIAL.
            LS_ZSDSFIT050-ERNAM  = LS_DATA-ERNAM.
            LS_ZSDSFIT050-ERDAT  = LS_DATA-ERDAT.
            LS_ZSDSFIT050-ERZET  = LS_DATA-ERZET.
          ELSE.
            LS_ZSDSFIT050-ERNAM  = SY-UNAME.
            LS_ZSDSFIT050-ERDAT  = SY-DATUM.
            LS_ZSDSFIT050-ERZET  = SY-UZEIT.
          ENDIF.
          LS_ZSDSFIT050-AENAM  = SY-UNAME.
          LS_ZSDSFIT050-AEDAT  = SY-DATUM.
          LS_ZSDSFIT050-AEZET  = SY-UZEIT.
          MODIFY ZSDSFIT050 FROM LS_ZSDSFIT050.

          SELECT MAX( RUNNG )
            FROM ZSDSFIT055
            WHERE ANLN1 EQ @<LFS_DATA>-ANLN1
              AND ANLN2 EQ @<LFS_DATA>-ANLN2
            INTO @LV_RUNNG.

          LV_RUNNG = LV_RUNNG + 1.

          MOVE-CORRESPONDING LS_ZSDSFIT050 TO LS_ZSDSFIT055.
          LS_ZSDSFIT055-RUNNG = LV_RUNNG.
          MODIFY ZSDSFIT055 FROM LS_ZSDSFIT055.
          COMMIT WORK AND WAIT.
        ELSE.
          <LFS_DATA>-STATUS  = GC_ERRO.
          <LFS_DATA>-MESSAGE = TEXT-E02.
        ENDIF.
      ELSE.
        <LFS_DATA>-STATUS  = GC_ERRO.
        <LFS_DATA>-MESSAGE = TEXT-E01.
      ENDIF.
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
