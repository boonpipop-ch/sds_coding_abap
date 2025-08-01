*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0680_CLASS
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
      UPDATE_DATA,
      SAVE,
      PRINT,
      REFRESH_DATA,
      PRINT_ZIV5,
      PRINT_ZIR1.
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

    SELECT ZSDSSDT025~DO_NO,
           ZSDSSDT025~SMART_TRACKING_DATE AS SMTDT,
           ZSDSSDT025~SMART_TRACKING_TIME AS SMTTM,
           ZSDSSDT025~INVNO,
           ZSDSSDT025~PETAX,
           KNA1~KUNNR,
           KNA1~NAME1,
           KNA1~ADRNR
      FROM  ZSDSSDT025
      INNER JOIN LIKP ON ZSDSSDT025~DO_NO EQ LIKP~VBELN
      INNER JOIN KNA1 ON LIKP~KUNAG       EQ KNA1~KUNNR
      LEFT  JOIN VBRK ON ZSDSSDT025~INVNO EQ VBRK~VBELN
      WHERE ZSDSSDT025~DO_NO               IN @S_DO_NO
        AND ZSDSSDT025~SMART_TRACKING_DATE IN @S_SMTDT
        AND ( ZSDSSDT025~INVNO             IN @S_INVNO OR
              VBRK~VBELN                   IN @S_INVNO )
        AND ZSDSSDT025~FLAG                IN @S_FLAG
        AND ZSDSSDT025~PETAX               IN @S_PETAX
  INTO TABLE @GT_RESULT.

  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.

    SELECT VBRP~VBELN,
           VBRP~VGBEL,
*           VBRP~ERDAT,
*           VBRP~ERZET,
           VBAK~ZZPOB
      FROM @GT_RESULT AS A
      INNER JOIN VBRP ON A~DO_NO EQ VBRP~VGBEL
      LEFT  JOIN VBAK ON VBRP~AUBEL EQ VBAK~VBELN
      ORDER BY VBRP~ERDAT DESCENDING, VBRP~ERZET DESCENDING
      INTO TABLE @DATA(LT_TMP).

    SELECT LIPS~VBELN,
           VBAK~ZZPOB
      FROM @GT_RESULT AS A
      INNER JOIN LIPS ON A~DO_NO EQ LIPS~VBELN
      LEFT  JOIN VBAK ON LIPS~VGBEL EQ VBAK~VBELN
      INTO TABLE @DATA(LT_POB).

    SELECT A~DO_NO,
           VBRP~VBELN
*           VBRP~ERDAT,
*           VBRP~ERZET
      FROM @GT_RESULT AS A
      LEFT  JOIN LIPS ON A~DO_NO    EQ LIPS~VBELN
      LEFT  JOIN VBAP ON LIPS~VGBEL EQ VBAP~VBELN AND
                         LIPS~VGPOS EQ VBAP~POSNR
      INNER JOIN VBRP ON VBAP~VBELN EQ VBRP~VGBEL AND
                         VBAP~POSNR EQ VBRP~VGPOS
      ORDER BY VBRP~ERDAT DESCENDING, VBRP~ERZET DESCENDING
      INTO TABLE @DATA(LT_BILLING_PLAN).

    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
      IF <LFS_RESULT>-INVNO IS INITIAL.
        READ TABLE LT_TMP INTO DATA(LS_TMP)
        WITH KEY VGBEL  = <LFS_RESULT>-DO_NO.
        IF SY-SUBRC EQ 0.
          <LFS_RESULT>-INVNO = LS_TMP-VBELN.
          <LFS_RESULT>-ZZPOB = LS_TMP-ZZPOB.
        ENDIF.
      ENDIF.

      IF <LFS_RESULT>-INVNO IS INITIAL.
        READ TABLE LT_BILLING_PLAN INTO DATA(LS_TMP_BILL)
        WITH KEY DO_NO  = <LFS_RESULT>-DO_NO.
        IF SY-SUBRC EQ 0.
          <LFS_RESULT>-INVNO = LS_TMP_BILL-VBELN.
        ENDIF.
      ENDIF.

      IF <LFS_RESULT>-ZZPOB IS INITIAL.
        READ TABLE LT_POB INTO DATA(LS_POB)
        WITH KEY VBELN = <LFS_RESULT>-DO_NO.
        IF SY-SUBRC EQ 0.
          <LFS_RESULT>-ZZPOB = LS_POB-ZZPOB.
        ENDIF.
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
    CLEAR : LS_FCAT,GT_FCAT.
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
  METHOD UPDATE_DATA.

    DATA : S_VBELN TYPE RANGE OF GY_RESULT-DO_NO.

    IF GS_RESULT-INVNO IS NOT INITIAL.
      S_VBELN =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = GS_RESULT-DO_NO ) ).

      SUBMIT ZSDSSDR0630 USING SELECTION-SCREEN  1000
                              WITH P_KSCHL    EQ 'ZIV5'
                              WITH S_VBELN    IN S_VBELN
                              WITH P_AUTO     EQ ABAP_TRUE
                              AND RETURN."VIA SELECTION-SCREEN.
      REFRESH_DATA( ).
    ENDIF.

  ENDMETHOD.
  METHOD SAVE.
    DATA : S_VBELN TYPE RANGE OF GY_RESULT-DO_NO.

    LOOP AT GT_RESULT INTO GS_RESULT WHERE CHECK EQ ABAP_TRUE AND
                                           INVNO IS INITIAL.
      S_VBELN =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = GS_RESULT-DO_NO ) ).

      SUBMIT ZSDSSDR0630 USING SELECTION-SCREEN  1000
                              WITH P_KSCHL    EQ 'ZIV5'
                              WITH S_VBELN    IN S_VBELN
                              WITH P_AUTO     EQ ABAP_TRUE
                              AND RETURN.
    ENDLOOP.
    REFRESH_DATA( ).
  ENDMETHOD.
  METHOD REFRESH_DATA.
    LCL_DATA=>GET_DATA( ).
    IF GT_RESULT[] IS NOT INITIAL.
      LCL_DATA=>GET_ADDTIONAL_DATA( ).
      SET_LAYOUT_OUTPUT( ).
      BUILD_FCAT( ).
      SET_SORT( ).
*      LCL_DATA=>SHOW_REPORT( ).
    ELSE.
      MESSAGE S004 DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD PRINT.
    LOOP AT GT_RESULT INTO GS_RESULT WHERE CHECK EQ ABAP_TRUE.
      PRINT_ZIV5( ).
      PRINT_ZIR1( ).
    ENDLOOP.
  ENDMETHOD.
  METHOD PRINT_ZIV5.
    PERFORM F_PRINT_ZIV5.
  ENDMETHOD.
  METHOD PRINT_ZIR1.
    PERFORM F_PRINT_ZIR1.
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
