*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0550_CLASS
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
      CHECK_SCREEN,
      CHECK_SELECTION RETURNING VALUE(R) TYPE CHAR255,
      CHECK_START,
      CHECK_END,
      SHOW_MESSAGE_ERROR IMPORTING I_DATA TYPE ANY,
      GET_DATA_FOR_INSERT,
      SAVE_R1,
      SAVE_R2,
      GET_DATA_UPDATE,
      DELETE_LINE.


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

    IF R1 EQ ABAP_TRUE.
      GV_ERROR = CHECK_SELECTION( ).
      IF GV_ERROR IS INITIAL.
        GET_DATA_FOR_INSERT( ).
      ENDIF.
    ELSEIF R2 EQ ABAP_TRUE.
      GET_DATA_UPDATE( ).
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

    CONSTANTS : BEGIN OF LC_CON,
                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
                  CHK_NAME  TYPE C LENGTH 3 VALUE 'CHK',
                  CONTF     TYPE C LENGTH 5 VALUE 'CONTF',
                  FLAGD     TYPE C LENGTH 5 VALUE 'FLAGD',
                  CONTF_D   TYPE C LENGTH 20 VALUE 'Control Flag',
                  FLAGD_D   TYPE C LENGTH 20 VALUE 'Delete Flag',
                END OF LC_CON.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = LC_CON-CONTF.
    LS_FCAT-SELTEXT_S   = LC_CON-CONTF_D.
    LS_FCAT-SELTEXT_M   = LC_CON-CONTF_D.
    LS_FCAT-SELTEXT_L   = LC_CON-CONTF_D.
    LS_FCAT-CHECKBOX    = ABAP_TRUE.
    LS_FCAT-INPUT       = ABAP_TRUE.
    LS_FCAT-EDIT        = ABAP_TRUE.
    APPEND LS_FCAT TO GT_FCAT.

*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = 'DAY_FIRST'.
*    LS_FCAT-SELTEXT_S   = 'Start Date'.
*    LS_FCAT-SELTEXT_M   = 'Start Date'.
*    LS_FCAT-SELTEXT_L   = 'Start Date'.
*    LS_FCAT-EDIT        = ABAP_TRUE. " เปิดให้แก้ไข
*    LS_FCAT-INPUT       = ABAP_TRUE. " รองรับการกรอกข้อมูล
*    APPEND LS_FCAT TO GT_FCAT.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = 'DAY_LAST'.
*    LS_FCAT-SELTEXT_S   = 'End Date'.
*    LS_FCAT-SELTEXT_M   = 'End Date'.
*    LS_FCAT-SELTEXT_L   = 'End Date'.
*    LS_FCAT-EDIT        = ABAP_TRUE. " เปิดให้แก้ไข
*    LS_FCAT-INPUT       = ABAP_TRUE. " รองรับการกรอกข้อมูล
*    APPEND LS_FCAT TO GT_FCAT.

    IF R2 EQ ABAP_TRUE.
      CLEAR LS_FCAT.
      LS_FCAT-FIELDNAME   = LC_CON-FLAGD.
      LS_FCAT-SELTEXT_S   = LC_CON-FLAGD_D.
      LS_FCAT-SELTEXT_M   = LC_CON-FLAGD_D.
      LS_FCAT-SELTEXT_L   = LC_CON-FLAGD_D.
      LS_FCAT-CHECKBOX    = ABAP_TRUE.
      LS_FCAT-INPUT       = ABAP_TRUE.
      LS_FCAT-EDIT        = ABAP_TRUE.
      LS_FCAT-COL_POS = 5.
      APPEND LS_FCAT TO GT_FCAT.


*      CLEAR LS_FCAT.
*      LS_FCAT-FIELDNAME   = DAY_FIRST_NEW.
*      LS_FCAT-SELTEXT_S   = 'StartDate NEW'.
*      LS_FCAT-SELTEXT_M   = 'StartDate NEW'.
*      LS_FCAT-SELTEXT_L   = 'StartDate NEW'.
*
*      LS_FCAT-COL_POS = 6.
*      APPEND LS_FCAT TO GT_FCAT.
*
*       CLEAR LS_FCAT.
*      LS_FCAT-FIELDNAME   = DAY_LAST_NEW.
*      LS_FCAT-SELTEXT_S   = 'End Date NEW'.
*      LS_FCAT-SELTEXT_M   = 'End Date NEW'.
*      LS_FCAT-SELTEXT_L   = 'End Date NEW'.
*
*      LS_FCAT-COL_POS = 7.
*      APPEND LS_FCAT TO GT_FCAT.

    ENDIF.

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

      IF LV_RUNNING EQ 2 OR
         LV_RUNNING EQ 3.
        IF R1 EQ ABAP_TRUE.
          LS_FCAT-EDIT        = SPACE.
          LS_FCAT-INPUT       = SPACE.
        ELSEIF R2 EQ ABAP_TRUE.
          LS_FCAT-EDIT        = ABAP_TRUE. " เปิดให้แก้ไข
          LS_FCAT-INPUT       = ABAP_TRUE. " รองรับการกรอกข้อมูล
        ENDIF.
      ENDIF.
      APPEND LS_FCAT TO GT_FCAT.
      CLEAR LS_FCAT.
    ENDDO.
*
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME   = LC_CON-FLAGD.
*    LS_FCAT-SELTEXT_S   = LC_CON-FLAGD_D.
*    LS_FCAT-SELTEXT_M   = LC_CON-FLAGD_D.
*    LS_FCAT-SELTEXT_L   = LC_CON-FLAGD_D.
*    LS_FCAT-CHECKBOX    = ABAP_TRUE.
*    LS_FCAT-INPUT       = ABAP_TRUE.
*    LS_FCAT-EDIT        = ABAP_TRUE.
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
  METHOD CHECK_SCREEN.
    IF R1 EQ ABAP_TRUE.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'DAT'.
          SCREEN-REQUEST  = 1.
          SCREEN-REQUIRED = 2.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'DAT'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD CHECK_SELECTION.
    IF P_START IS INITIAL.
      R = TEXT-E01.
    ENDIF.

    IF P_END IS INITIAL.
      R = TEXT-E02.
    ENDIF.

    IF P_END LT P_START.
      R = TEXT-E03.
    ENDIF.

  ENDMETHOD.
  METHOD CHECK_START.
    IF R1 EQ ABAP_TRUE.
      IF P_START IS INITIAL.
        SET CURSOR FIELD 'P_START'.
        GV_ERROR = TEXT-E01.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD CHECK_END.
    IF R1 EQ ABAP_TRUE.
      IF P_END IS INITIAL.
        SET CURSOR FIELD 'P_END'.
        GV_ERROR = TEXT-E02.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD SHOW_MESSAGE_ERROR.
    MESSAGE S000 WITH I_DATA DISPLAY LIKE 'E'.
    CLEAR : GV_ERROR.
  ENDMETHOD.
  METHOD GET_DATA_FOR_INSERT.
    SELECT MARA~MATNR,
           @P_START,
           @P_END
      FROM MARA
      WHERE MARA~MATNR IN @S_MATNR
      INTO TABLE @GT_RESULT.
  ENDMETHOD.
  METHOD SAVE_R1.

    DATA: LS_ZSDSSDC030 TYPE ZSDSSDC030.

    DATA: LT_DOC TYPE ZSDSSDS127_TT.

    DATA: LCL_SO TYPE REF TO ZCL_SDSSD_SALES_DOCUMENT.

    IF LCL_SO IS NOT BOUND.
      CREATE OBJECT LCL_SO.
    ENDIF.

    LOOP AT GT_RESULT INTO DATA(LS_RESULT) WHERE CONTF = ABAP_TRUE.
      CLEAR LS_ZSDSSDC030.
      LS_ZSDSSDC030-MATNR     = LS_RESULT-MATNR.
      LS_ZSDSSDC030-DAY_FIRST = LS_RESULT-DAY_FIRST.
      LS_ZSDSSDC030-DAY_LAST  = LS_RESULT-DAY_LAST.
      LS_ZSDSSDC030-CONTF     = LS_RESULT-CONTF.
      LS_ZSDSSDC030-FLAGD     = LS_RESULT-FLAGD.
      LS_ZSDSSDC030-AENAM     = SY-UNAME.
      LS_ZSDSSDC030-AEDAT     = SY-DATUM.
      LS_ZSDSSDC030-AEZET     = SY-UZEIT.
      LS_ZSDSSDC030-ERNAM     = SY-UNAME.
      LS_ZSDSSDC030-ERDAT     = SY-DATUM.
      LS_ZSDSSDC030-ERZET     = SY-UZEIT.

      IF LS_ZSDSSDC030-CONTF EQ ABAP_TRUE.
        DATA(LT_TMP) = LCL_SO->GET_OPEN_SCHDULE_LINE_SO( LS_ZSDSSDC030-MATNR ).
        IF LT_TMP IS NOT INITIAL.
          APPEND LINES OF LT_TMP TO LT_DOC.
        ENDIF.
      ENDIF.

      MODIFY ZSDSSDC030 FROM LS_ZSDSSDC030.
    ENDLOOP.

    IF LT_DOC IS NOT INITIAL.
      SORT LT_DOC.
      DELETE ADJACENT DUPLICATES FROM LT_DOC.
      DATA(LT_RETURN) = LCL_SO->CANCEL_CONFIRM_QTY( LT_DOC ).
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.

*--------------------------------------------------------------------*.
  METHOD SAVE_R2.
    DATA: LS_ZSDSSDC030 TYPE ZSDSSDC030,
          LS_EXISTING   TYPE ZSDSSDC030.
    DATA: LT_EXISTING TYPE TABLE OF ZSDSSDC030.

    DATA: LCL_SO TYPE REF TO ZCL_SDSSD_SALES_DOCUMENT.

    DATA: LT_MATNR TYPE FIP_T_MATNR_RANGE,
          LS_MATNR LIKE LINE OF LT_MATNR.

    IF LCL_SO IS BOUND.
      CREATE OBJECT LCL_SO.
    ENDIF.

    LOOP AT GT_RESULT INTO DATA(LS_RESULT) .
      DELETE FROM ZSDSSDC030 WHERE MATNR     EQ LS_RESULT-MATNR
                               AND DAY_FIRST EQ LS_RESULT-DAY_FIRST_OLD
                               AND DAY_LAST  EQ LS_RESULT-DAY_LAST_OLD.

      COMMIT WORK AND WAIT.

      CLEAR LS_ZSDSSDC030.
      LS_ZSDSSDC030-MATNR     = LS_RESULT-MATNR.
      LS_ZSDSSDC030-DAY_FIRST = LS_RESULT-DAY_FIRST.
      LS_ZSDSSDC030-DAY_LAST  = LS_RESULT-DAY_LAST.
      LS_ZSDSSDC030-CONTF     = LS_RESULT-CONTF.
      LS_ZSDSSDC030-FLAGD     = LS_RESULT-FLAGD.

      IF SY-SUBRC = 0.
        LS_ZSDSSDC030-ERNAM = LS_RESULT-ERNAM.
        LS_ZSDSSDC030-ERDAT = LS_RESULT-ERDAT.
        LS_ZSDSSDC030-ERZET = LS_RESULT-ERZET.
      ELSE.
        LS_ZSDSSDC030-ERNAM = SY-UNAME.
        LS_ZSDSSDC030-ERDAT = SY-DATUM.
        LS_ZSDSSDC030-ERZET = SY-UZEIT.
      ENDIF.

      LS_ZSDSSDC030-AENAM = SY-UNAME.
      LS_ZSDSSDC030-AEDAT = SY-DATUM.
      LS_ZSDSSDC030-AEZET = SY-UZEIT.

      IF LS_ZSDSSDC030-CONTF IS INITIAL.
        LS_MATNR-SIGN   = 'I'.
        LS_MATNR-OPTION = 'EQ'.
        LS_MATNR-LOW    = LS_ZSDSSDC030-MATNR.
        APPEND LS_MATNR TO LT_MATNR.
      ELSE.
        IF LS_ZSDSSDC030-FLAGD IS INITIAL.
          LS_MATNR-SIGN   = 'I'.
          LS_MATNR-OPTION = 'EQ'.
          LS_MATNR-LOW    = LS_ZSDSSDC030-MATNR.
          APPEND LS_MATNR TO LT_MATNR.
        ENDIF.
      ENDIF.

      MODIFY ZSDSSDC030 FROM LS_ZSDSSDC030.
      COMMIT WORK AND WAIT.
    ENDLOOP.

    IF LT_MATNR IS NOT INITIAL.
      LCL_SO->CONFIRM_QTY_V_V2( LT_MATNR ).
    ENDIF.

  ENDMETHOD.
  METHOD GET_DATA_UPDATE.

    DATA : LS_ZSDSSDC030 TYPE ZSDSSDC030,
           LT_ZSDSSDC030 TYPE TABLE OF ZSDSSDC030.

    SELECT ZSDSSDC030~MATNR,
           ZSDSSDC030~DAY_FIRST,
           ZSDSSDC030~DAY_LAST,
           ZSDSSDC030~CONTF,
           ZSDSSDC030~FLAGD,
           @SPACE AS STATUS,
           ZSDSSDC030~ERNAM,
           ZSDSSDC030~ERDAT,
           ZSDSSDC030~ERZET,
           ZSDSSDC030~AENAM,
           ZSDSSDC030~AEDAT,
           ZSDSSDC030~AEZET,
           ZSDSSDC030~DAY_FIRST AS DAY_FIRST_OLD,
           ZSDSSDC030~DAY_LAST AS DAY_LAST_OLD
      FROM ZSDSSDC030
     WHERE ZSDSSDC030~MATNR IN @S_MATNR
       AND ZSDSSDC030~FLAGD EQ @SPACE
      INTO TABLE @GT_RESULT.
  ENDMETHOD.
  METHOD DELETE_LINE.



    DELETE GT_RESULT WHERE FLAGD = ABAP_TRUE.

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
