*&---------------------------------------------------------------------*
*& Include          ZSDSMMI0020_CLASS
*&---------------------------------------------------------------------*
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
    DATA : LV_PURCHASEORDER TYPE  BAPIMMPARA-PO_NUMBER,
           LV_PO_REL_CODE   TYPE  BAPIMMPARA-PO_REL_COD.

    DATA : BEGIN OF LS_EKKO,
             EBELN TYPE EKKO-EBELN,
           END OF LS_EKKO.
    DATA LT_EKKO LIKE TABLE OF LS_EKKO WITH EMPTY KEY.

    DATA : BEGIN OF LS_T16FS,
             EBELN TYPE EKKO-EBELN,
             FRGC1 TYPE T16FS-FRGC1,
           END OF LS_T16FS.
    DATA LT_T16FS LIKE TABLE OF LS_T16FS.

    CONSTANTS : BEGIN OF LC_CON,
                  COM TYPE C LENGTH 3 VALUE 'COM',
                END OF LC_CON.

    SELECT EBELN
    FROM EKKO
    INTO TABLE LT_EKKO
    WHERE EBELN IN S_EBELN
      AND ( FRGKE EQ 'G' OR
            FRGKE EQ 'R' ).

    SELECT ZSDSMMT002~EBELN
      FROM @LT_EKKO AS A
      INNER JOIN ZSDSMMT002 ON A~EBELN EQ ZSDSMMT002~EBELN
      WHERE STATU EQ @LC_CON-COM
        AND FLAGD EQ @SPACE
      INTO TABLE @DATA(LT_STATUS).

    SELECT EBELN
           FRGC1
      FROM EKKO
      INNER JOIN T16FS ON EKKO~FRGGR EQ T16FS~FRGGR AND
                          EKKO~FRGSX EQ T16FS~FRGSX
      INTO TABLE LT_T16FS
      WHERE EBELN IN S_EBELN.

    LOOP AT LT_EKKO INTO LS_EKKO.
      READ TABLE LT_STATUS INTO DATA(LS_STATUS)
      WITH KEY EBELN = LS_EKKO-EBELN.
      IF SY-SUBRC EQ 0.
        LV_PURCHASEORDER  = LS_EKKO-EBELN.

        READ TABLE LT_T16FS INTO LS_T16FS
        WITH KEY EBELN = LS_EKKO-EBELN.
        IF SY-SUBRC = 0.
          LV_PO_REL_CODE    = LS_T16FS-FRGC1.
        ENDIF.

*        CALL FUNCTION 'BAPI_PO_RESET_RELEASE'
*          EXPORTING
*            PURCHASEORDER            = LV_PURCHASEORDER
*            PO_REL_CODE              = LV_PO_REL_CODE
**           USE_EXCEPTIONS           = 'X'
**   IMPORTING
**           REL_STATUS_NEW           =
**           REL_INDICATOR_NEW        =
**   TABLES
**           RETURN                   =
*          EXCEPTIONS
*            AUTHORITY_CHECK_FAIL     = 1
*            DOCUMENT_NOT_FOUND       = 2
*            ENQUEUE_FAIL             = 3
*            PREREQUISITE_FAIL        = 4
*            RELEASE_ALREADY_POSTED   = 5
*            RESPONSIBILITY_FAIL      = 6
*            NO_RELEASE_ALREADY       = 7
*            NO_NEW_RELEASE_INDICATOR = 8
*            OTHERS                   = 9.
*        IF SY-SUBRC <> 0.
**          GS_RESULT-STATU = GC_ERROR.
**          GS_RESULT-EBELN = LS_EKKO-EBELN.
**          GS_RESULT-MESSG = TEXT-E01.
**        ELSE.
**          GS_RESULT-STATU = GC_SUCCESS.
**          GS_RESULT-EBELN = LS_EKKO-EBELN.
**          GS_RESULT-MESSG = TEXT-S01.
*        ENDIF.

        GS_RESULT-STATU = GC_SUCCESS.
        GS_RESULT-EBELN = LS_EKKO-EBELN.
        GS_RESULT-MESSG = TEXT-S01.

        APPEND GS_RESULT TO GT_RESULT.

        UPDATE ZSDSMMT002 SET FLAGD = ABAP_TRUE
                        WHERE EBELN EQ LS_EKKO-EBELN.
        COMMIT WORK AND WAIT.
      ELSE.
        GS_RESULT-STATU = GC_ERROR.
        GS_RESULT-EBELN = LS_EKKO-EBELN.
        GS_RESULT-MESSG = TEXT-E03.
        APPEND GS_RESULT TO GT_RESULT.
      ENDIF.
      CLEAR :  LS_EKKO,LS_T16FS,LV_PURCHASEORDER,LV_PO_REL_CODE.
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
    GS_LAYOUT-ZEBRA             = GC_X.
    GS_LAYOUT-COLWIDTH_OPTIMIZE = GC_X.
  ENDMETHOD.
  METHOD BUILD_FCAT.
    DATA:
       LS_FCAT TYPE SLIS_FIELDCAT_ALV.

**  CLEAR ls_fcat.
**  ls_fcat-ref_tabname = 'LFA1'.
**  ls_fcat-fieldname   = 'LIFNR'.
**  APPEND ls_fcat TO gt_fcat.

*    CLEAR LS_FCAT.
**  ls_fcat-ref_tabname = 'GT_RESULT'.
*    LS_FCAT-FIELDNAME   = 'CHECK'.
*    LS_FCAT-SELTEXT_S   = 'Check'.
*    LS_FCAT-SELTEXT_M   = 'Check'.
*    LS_FCAT-SELTEXT_L   = 'Check'.
*    LS_FCAT-CHECKBOX    = 'X'.
*    LS_FCAT-INPUT       = 'X'.
*    LS_FCAT-EDIT        = 'X'.
*    APPEND LS_FCAT TO GT_FCAT.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = TEXT-801.
    LS_FCAT-SELTEXT_S   = TEXT-701.
    LS_FCAT-SELTEXT_M   = TEXT-701.
    LS_FCAT-SELTEXT_L   = TEXT-701.
    APPEND LS_FCAT TO GT_FCAT.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = TEXT-802.
    LS_FCAT-SELTEXT_S   = TEXT-702.
    LS_FCAT-SELTEXT_M   = TEXT-702.
    LS_FCAT-SELTEXT_L   = TEXT-702.
    APPEND LS_FCAT TO GT_FCAT.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = TEXT-803.
    LS_FCAT-SELTEXT_S   = TEXT-703.
    LS_FCAT-SELTEXT_M   = TEXT-703.
    LS_FCAT-SELTEXT_L   = TEXT-703.
    APPEND LS_FCAT TO GT_FCAT.

*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-804.
*    LS_FCAT-SELTEXT_S   = TEXT-704.
*    LS_FCAT-SELTEXT_M   = TEXT-704.
*    LS_FCAT-SELTEXT_L   = TEXT-704.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-805.
*    LS_FCAT-SELTEXT_S   = TEXT-705.
*    LS_FCAT-SELTEXT_M   = TEXT-705.
*    LS_FCAT-SELTEXT_L   = TEXT-705.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-806.
*    LS_FCAT-SELTEXT_S   = TEXT-706.
*    LS_FCAT-SELTEXT_M   = TEXT-706.
*    LS_FCAT-SELTEXT_L   = TEXT-706.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-807.
*    LS_FCAT-SELTEXT_S   = TEXT-707.
*    LS_FCAT-SELTEXT_M   = TEXT-707.
*    LS_FCAT-SELTEXT_L   = TEXT-707.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-808.
*    LS_FCAT-SELTEXT_S   = TEXT-708.
*    LS_FCAT-SELTEXT_M   = TEXT-708.
*    LS_FCAT-SELTEXT_L   = TEXT-708.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-809.
*    LS_FCAT-SELTEXT_S   = TEXT-709.
*    LS_FCAT-SELTEXT_M   = TEXT-709.
*    LS_FCAT-SELTEXT_L   = TEXT-709.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-810.
*    LS_FCAT-SELTEXT_S   = TEXT-710.
*    LS_FCAT-SELTEXT_M   = TEXT-710.
*    LS_FCAT-SELTEXT_L   = TEXT-710.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-811.
*    LS_FCAT-SELTEXT_S   = TEXT-711.
*    LS_FCAT-SELTEXT_M   = TEXT-711.
*    LS_FCAT-SELTEXT_L   = TEXT-711.
*    LS_FCAT-NO_ZERO     = GC_X.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-812.
*    LS_FCAT-SELTEXT_S   = TEXT-712.
*    LS_FCAT-SELTEXT_M   = TEXT-712.
*    LS_FCAT-SELTEXT_L   = TEXT-712.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-813.
*    LS_FCAT-SELTEXT_S   = TEXT-713.
*    LS_FCAT-SELTEXT_M   = TEXT-713.
*    LS_FCAT-SELTEXT_L   = TEXT-713.
*    LS_FCAT-NO_ZERO     = GC_X.
*    APPEND LS_FCAT TO GT_FCAT.

*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-814.
*    LS_FCAT-SELTEXT_S   = TEXT-714.
*    LS_FCAT-SELTEXT_M   = TEXT-714.
*    LS_FCAT-SELTEXT_L   = TEXT-714.
*    APPEND LS_FCAT TO GT_FCAT.
*
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-815.
*    LS_FCAT-SELTEXT_S   = TEXT-715.
*    LS_FCAT-SELTEXT_M   = TEXT-715.
*    LS_FCAT-SELTEXT_L   = TEXT-715.
*    LS_FCAT-NO_ZERO     = GC_X.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-816.
*    LS_FCAT-SELTEXT_S   = TEXT-716.
*    LS_FCAT-SELTEXT_M   = TEXT-716.
*    LS_FCAT-SELTEXT_L   = TEXT-716.
*    APPEND LS_FCAT TO GT_FCAT.
*
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-817.
*    LS_FCAT-SELTEXT_S   = TEXT-717.
*    LS_FCAT-SELTEXT_M   = TEXT-717.
*    LS_FCAT-SELTEXT_L   = TEXT-717.
*    LS_FCAT-NO_ZERO     = GC_X.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-818.
*    LS_FCAT-SELTEXT_S   = TEXT-718.
*    LS_FCAT-SELTEXT_M   = TEXT-718.
*    LS_FCAT-SELTEXT_L   = TEXT-718.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-819.
*    LS_FCAT-SELTEXT_S   = TEXT-719.
*    LS_FCAT-SELTEXT_M   = TEXT-719.
*    LS_FCAT-SELTEXT_L   = TEXT-719.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-820.
*    LS_FCAT-SELTEXT_S   = TEXT-720.
*    LS_FCAT-SELTEXT_M   = TEXT-720.
*    LS_FCAT-SELTEXT_L   = TEXT-720.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-821.
*    LS_FCAT-SELTEXT_S   = TEXT-721.
*    LS_FCAT-SELTEXT_M   = TEXT-721.
*    LS_FCAT-SELTEXT_L   = TEXT-721.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-822.
*    LS_FCAT-SELTEXT_S   = TEXT-722.
*    LS_FCAT-SELTEXT_M   = TEXT-722.
*    LS_FCAT-SELTEXT_L   = TEXT-722.
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = TEXT-823.
*    LS_FCAT-SELTEXT_S   = TEXT-723.
*    LS_FCAT-SELTEXT_M   = TEXT-723.
*    LS_FCAT-SELTEXT_L   = TEXT-723.
*    APPEND LS_FCAT TO GT_FCAT.
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
