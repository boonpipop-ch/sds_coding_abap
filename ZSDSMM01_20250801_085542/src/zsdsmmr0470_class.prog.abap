*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0470_CLASS
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
      RELEASE_PO IMPORTING I_DATA   TYPE EKKO-EBELN
                 RETURNING VALUE(R) TYPE FLAG,
      UPDATE_STATUS IMPORTING I_DATA   TYPE EKKO-EBELN
                    RETURNING VALUE(R) TYPE FLAG,
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
    DATA : BEGIN OF LS_PO,
             EBELN TYPE EKKO-EBELN,
             FRGKE TYPE EKKO-FRGKE,
           END OF LS_PO.
    DATA LT_PO LIKE TABLE OF LS_PO.

    DATA : LV_STATUS TYPE FLAG.

    CONSTANTS : BEGIN OF LC_CON,
                  COM TYPE C LENGTH 3 VALUE 'COM',
                END OF LC_CON.

    SELECT EKKO~EBELN,
           EKKO~FRGKE
      FROM EKKO
*      INNER JOIN ZSDSMMT002 AS A ON EKKO~EBELN EQ A~EBELN
      WHERE EKKO~EBELN IN @S_EBELN
*        AND A~RUNNG EQ ( SELECT MAX( RUNNG )
*                           FROM ZSDSMMT002
*                           WHERE EBELN EQ A~EBELN )
*        AND A~STATU EQ @LC_CON-COM
*        AND A~FLAGD EQ @SPACE
       INTO TABLE @LT_PO.
    IF SY-SUBRC EQ 0.
      SELECT *
        FROM ZSDSMMT002
        INTO TABLE @GT_ZSDSMMT002
        WHERE EBELN IN @S_EBELN.

      SORT GT_ZSDSMMT002 BY EBELN RUNNG DESCENDING.

      LOOP AT LT_PO INTO LS_PO.
        IF LS_PO-FRGKE EQ 'G' OR
           LS_PO-FRGKE EQ 'R'.
          LV_STATUS = UPDATE_STATUS( LS_PO-EBELN ).
        ELSE.
          READ TABLE GT_ZSDSMMT002
          WITH KEY EBELN = LS_PO-EBELN
                   STATU = 'COM'
                   RELES = ABAP_TRUE TRANSPORTING NO FIELDS.
          IF SY-SUBRC EQ 0.
            LV_STATUS = RELEASE_PO( LS_PO-EBELN ).
*            UPDATE_STATUS( LS_PO-EBELN ).
          ENDIF.
        ENDIF.
        CLEAR : LS_PO.
      ENDLOOP.
      IF LV_STATUS EQ SPACE.
        MESSAGE S001 WITH 'Data has been saved'.
      ENDIF.
    ELSE.
      MESSAGE S001 WITH 'Status has not been compleated yet.' DISPLAY LIKE 'E'.
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
  METHOD RELEASE_PO.
    DATA : I_EBELN TYPE  EKKO-EBELN,
           I_EBELP TYPE  EKPO-EBELP,
           I_BSTYP TYPE  EKKO-BSTYP,
           I_FRGCO TYPE  FRGCO.

    DATA : PURCHASEORDER  TYPE  BAPIMMPARA-PO_NUMBER,
           PO_REL_CODE    TYPE  BAPIMMPARA-PO_REL_COD,
           USE_EXCEPTIONS TYPE  BAPIMMPARA-SELECTION,
           NO_COMMIT      TYPE  BAPIMMPARA-SELECTION.

    DATA : BEGIN OF LS_T16FS,
             FRGC1 TYPE T16FS-FRGC1,
           END OF LS_T16FS.

    DATA : LV_MESSAGE TYPE C LENGTH 255.

    I_EBELN = I_DATA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_EBELN
      IMPORTING
        OUTPUT = I_EBELN.

    SELECT SINGLE FRGC1
      FROM EKKO
      INNER JOIN T16FS ON EKKO~FRGGR EQ T16FS~FRGGR AND
                          EKKO~FRGSX EQ T16FS~FRGSX
      INTO LS_T16FS-FRGC1
      WHERE EBELN EQ I_EBELN.

    PURCHASEORDER  = I_EBELN.
    PO_REL_CODE    = LS_T16FS-FRGC1.

    CALL FUNCTION 'BAPI_PO_RELEASE'
      EXPORTING
        PURCHASEORDER          = PURCHASEORDER
        PO_REL_CODE            = PO_REL_CODE
      EXCEPTIONS
        AUTHORITY_CHECK_FAIL   = 1
        DOCUMENT_NOT_FOUND     = 2
        ENQUEUE_FAIL           = 3
        PREREQUISITE_FAIL      = 4
        RELEASE_ALREADY_POSTED = 5
        RESPONSIBILITY_FAIL    = 6
        OTHERS                 = 7.
    IF SY-SUBRC <> 0.
      CONCATENATE I_EBELN ':' SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO LV_MESSAGE SEPARATED BY SPACE.
      MESSAGE S001 WITH LV_MESSAGE.

      CLEAR : LV_MESSAGE.
      CONCATENATE I_EBELN ':' 'Cannot release' INTO LV_MESSAGE SEPARATED BY SPACE.
      MESSAGE S001 WITH LV_MESSAGE.
      R = ABAP_TRUE.
    ELSE.
      UPDATE ZSDSMMT002 SET RELES = SPACE
                      WHERE EBELN EQ I_EBELN
                        AND STATU EQ 'COM'
                        AND FLAGD EQ SPACE.
      CLEAR : R.
    ENDIF.

    COMMIT( ).
  ENDMETHOD.
  METHOD UPDATE_STATUS.
    CONSTANTS : LC_SAVE TYPE C LENGTH 3 VALUE 'SUB'.

    DATA : LT_TMP LIKE GT_ZSDSMMT002.

    LOOP AT GT_ZSDSMMT002 ASSIGNING FIELD-SYMBOL(<LS_DATA>) WHERE EBELN EQ I_DATA.
      <LS_DATA>-FLAGD = SPACE.
      APPEND <LS_DATA> TO LT_TMP.
      IF <LS_DATA>-STATU EQ LC_SAVE.
        EXIT.
      ENDIF.
    ENDLOOP.

    MODIFY ZSDSMMT002 FROM TABLE LT_TMP.
  ENDMETHOD.
  METHOD COMMIT.
    COMMIT WORK AND WAIT.
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
