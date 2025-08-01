*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0600_CLASS
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
      HTML_TOP_OF_PAGE.
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
    SELECT DISTINCT ANLA~ANLN1,
           ANLA~ANLN2,
           ANLA~MCOA1,
           ANLA~TXT50,
           ANLA~INVZU,
           ANLZ~KOSTL,
           ANLZ~STORT,
           ANLZ~RAUMN,
           ANLZ~PERNR,
           ZSDSFIT050~PERNR,
           ZSDSFIT050~KOSTL,
           ZSDSFIT050~REMARK,
           ZSDSFIT050~STATU,
           ZSDSFIT050~ERNAM,
           ZSDSFIT050~ERDAT,
           ZSDSFIT050~ERZET,
           ZSDSFIT050~AENAM,
           ZSDSFIT050~AEDAT,
           ZSDSFIT050~AEZET,
           ( A~VORNA && ' ' && A~NACHN ) AS OWNNA,
           ( B~VORNA && ' ' && B~NACHN ) AS MGNNA,
           ( C~VORNA && ' ' && C~NACHN ) AS INPNA,
           T499S~KTEXT,
           CSKT~LTEXT
    FROM ANLA
    INNER JOIN ANLZ ON ANLA~BUKRS EQ ANLZ~BUKRS AND
                       ANLA~ANLN1 EQ ANLZ~ANLN1 AND
                       ANLA~ANLN2 EQ ANLZ~ANLN2 AND
                       ANLZ~BDATU GE @SY-DATUM AND
                       ANLZ~ADATU LE @SY-DATUM
    LEFT JOIN ZSDSFIT050 ON ANLA~ANLN1 EQ ZSDSFIT050~ANLN1 AND
                            ANLA~ANLN2 EQ ZSDSFIT050~ANLN2
    LEFT JOIN PA0002 AS A ON ANLZ~RAUMN       EQ A~PERNR   AND
                             A~ENDDA          GE @SY-DATUM AND
                             A~BEGDA          LE @SY-DATUM
    LEFT JOIN PA0002 AS B ON ANLZ~PERNR       EQ B~PERNR   AND
                             B~ENDDA          GE @SY-DATUM AND
                             B~BEGDA          LE @SY-DATUM
    LEFT JOIN PA0002 AS C ON ZSDSFIT050~PERNR EQ C~PERNR AND
                             C~ENDDA          GE @SY-DATUM AND
                             C~BEGDA          LE @SY-DATUM
    LEFT JOIN T499S ON ANLZ~WERKS EQ T499S~WERKS AND
                       ANLZ~STORT EQ T499S~STAND
    LEFT JOIN CSKT  ON ANLZ~KOSTL EQ CSKT~KOSTL
    WHERE ANLA~BUKRS IN @S_BUKRS[]
      AND ANLA~ANLN1 IN @S_ANLN1[]
      AND ANLA~ANLN2 IN @S_ANLN2[]
      AND ANLA~MCOA1 IN @S_MCOA1[]
      AND ANLZ~KOSTL IN @S_KOSTL[]
      AND ANLA~INVZU IN @S_INVZU[]
      AND ANLZ~STORT IN @S_STORT[]
      AND ANLA~ERDAT LE @P_ERDAT
      AND ( ( ANLA~DEAKT EQ '00000000' OR
              ANLA~DEAKT IS NULL )  OR
            ANLA~DEAKT GE @SY-DATUM )
      AND EXISTS ( SELECT BUKRS,
                          ANLN1,
                          ANLN2
                     FROM ANLC
                    WHERE BUKRS EQ ANLA~BUKRS
                      AND ANLN1 EQ ANLA~ANLN1
                      AND ANLN2 EQ ANLA~ANLN2 )
    INTO TABLE @GT_RESULT.

  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
    DATA: BEGIN OF LS_PERNR,
            PERNR TYPE PA0002-PERNR,
          END OF LS_PERNR.
    DATA: LT_PERNR LIKE HASHED TABLE OF LS_PERNR
                        WITH UNIQUE KEY PERNR.

    DATA: LV_PERNR TYPE PA0002-PERNR.

    LT_PERNR =  CORRESPONDING #( BASE (  LT_PERNR )
                                      GT_RESULT  DISCARDING DUPLICATES
                                                 MAPPING PERNR = RAUMN ).

    SELECT A~PERNR,
           ( VORNA && ' ' && NACHN ) AS OWNNA
      FROM @LT_PERNR AS A
      INNER JOIN PA0002 ON A~PERNR EQ PA0002~PERNR
      INTO TABLE @DATA(LT_EMP).

    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
      IF <LFS_RESULT>-OWNNA IS INITIAL.
        LV_PERNR = <LFS_RESULT>-RAUMN.
        LV_PERNR = |{ LV_PERNR ALPHA = IN }|.
        <LFS_RESULT>-RAUMN = LV_PERNR.
        READ TABLE LT_EMP INTO DATA(LS_EMP)
        WITH KEY PERNR = LV_PERNR.
        IF SY-SUBRC EQ 0.
          <LFS_RESULT>-OWNNA = LS_EMP-OWNNA.
        ENDIF.
      ENDIF.

      <LFS_RESULT>-PERNR_IN = |{ <LFS_RESULT>-PERNR_IN ALPHA = IN }|.

      IF <LFS_RESULT>-RAUMN NE <LFS_RESULT>-PERNR_IN.
        <LFS_RESULT>-DIF = 'Dif Employee'.
      ENDIF.

      IF <LFS_RESULT>-KOSTL NE <LFS_RESULT>-KOSTL_IN.
        <LFS_RESULT>-DIF = 'Dif Cost Center'.
      ENDIF.

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
