*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0080_CLASS
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
      CONVERT_DATE IMPORTING I_DATA   TYPE ANY
                   RETURNING VALUE(R) TYPE SY-DATUM,
      CREATE_EQUI IMPORTING I_DATA   TYPE GY_RESULT
                  RETURNING VALUE(R) TYPE EQUI-EQUNR.
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
      <LFS_RESULT>-STATUS = GC_warn.
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
  METHOD SAVE.

    DATA : LV_COMDT       TYPE SY-DATUM,
           LV_CUS_REG_BEG TYPE SY-DATUM,
           LV_CUS_REG_END TYPE SY-DATUM.

    DATA : BEGIN OF LS_MATERIAL,
             MATNR TYPE ZSDSCMT003-MATNR,
             SERNR TYPE ZSDSCMT003-SERNR,
           END OF LS_MATERIAL.
    DATA : LT_MATERIAL LIKE HASHED TABLE OF LS_MATERIAL WITH UNIQUE KEY MATNR SERNR.

    DATA : LT_METERIAL_NEW LIKE HASHED TABLE OF LS_MATERIAL WITH UNIQUE KEY MATNR SERNR.

    DATA : LS_ZSDSCMT003 TYPE ZSDSCMT003.

    LT_MATERIAL =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES
                                        MAPPING MATNR = MATERIAL
                                                SERNR = SERIAL ).

    LT_METERIAL_NEW =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES
                                        MAPPING MATNR = MATERIAL
                                                SERNR = SERIAL_NEW ).

    SELECT ZSDSCMT003~MATNR,
           ZSDSCMT003~SERNR,
           ZSDSCMT003~STD_WRT_BEG,
           ZSDSCMT003~STD_WRT_END
      FROM @LT_MATERIAL AS A
      INNER JOIN ZSDSCMT003 ON A~MATNR EQ ZSDSCMT003~MATNR AND
                               A~SERNR EQ ZSDSCMT003~SERNR
      INTO TABLE @DATA(LT_DATA_OLD).

    SELECT ZSDSCMT003~MATNR,
           ZSDSCMT003~SERNR,
           ZSDSCMT003~STD_WRT_BEG,
           ZSDSCMT003~STD_WRT_END
      FROM @LT_METERIAL_NEW AS A
      INNER JOIN ZSDSCMT003 ON A~MATNR EQ ZSDSCMT003~MATNR AND
                               A~SERNR EQ ZSDSCMT003~SERNR
      INTO TABLE @DATA(LT_DATA_NEW).

    SELECT EQUI~EQUNR,
           EQUI~MATNR,
           EQUI~SERNR
      FROM @LT_METERIAL_NEW AS A
      INNER JOIN EQUI ON A~MATNR EQ EQUI~MATNR AND
                         A~SERNR EQ EQUI~SERNR
      INTO TABLE @DATA(LT_DATA_EQUI).

    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
      MOVE-CORRESPONDING <LFS_DATA> TO GS_RESULT.

      LV_CUS_REG_BEG = CONVERT_DATE( GS_RESULT-START_DATE ).
      LV_CUS_REG_END = CONVERT_DATE( GS_RESULT-END_DATE ).
      LV_COMDT       = CONVERT_DATE( GS_RESULT-COMM_DATE ).

      READ TABLE LT_DATA_OLD INTO DATA(LS_DATA_OLD)
      WITH KEY MATNR = GS_RESULT-MATERIAL
               SERNR = GS_RESULT-SERIAL.
      IF SY-SUBRC EQ 0.

        IF LS_DATA_OLD-STD_WRT_BEG IS INITIAL.
          LS_DATA_OLD-STD_WRT_BEG = LV_CUS_REG_BEG.
        ENDIF.

        IF LS_DATA_OLD-STD_WRT_END IS INITIAL.
          LS_DATA_OLD-STD_WRT_END = LV_CUS_REG_END.
        ENDIF.

        UPDATE ZSDSCMT003 SET SERNR       = GS_RESULT-SERIAL_NEW
                              COMDT       = LV_COMDT
                              STD_WRT_BEG = LS_DATA_OLD-STD_WRT_BEG
                              STD_WRT_END = LS_DATA_OLD-STD_WRT_END
                              CUS_REG_BEG = LV_CUS_REG_BEG
                              CUS_REG_END = LV_CUS_REG_END
                              UPDFLG      = ABAP_TRUE
                              AENAM       = SY-UNAME
                              AEDAT       = SY-DATUM
                              AEZET       = SY-UZEIT
                        WHERE MATNR EQ GS_RESULT-MATERIAL
                          AND SERNR EQ GS_RESULT-SERIAL.
        <LFS_DATA>-STATUS  = GC_SUCS.
        <LFS_DATA>-MESSAGE = TEXT-S01.
      ELSE.
        READ TABLE LT_DATA_NEW INTO DATA(LS_DATA_NEW)
        WITH KEY MATNR = GS_RESULT-MATERIAL
                 SERNR = GS_RESULT-SERIAL_NEW.
        IF SY-SUBRC EQ 0.

          IF LS_DATA_NEW-STD_WRT_BEG IS INITIAL.
            LS_DATA_NEW-STD_WRT_BEG = LV_CUS_REG_BEG.
          ENDIF.

          IF LS_DATA_NEW-STD_WRT_END IS INITIAL.
            LS_DATA_NEW-STD_WRT_END = LV_CUS_REG_END.
          ENDIF.

          UPDATE ZSDSCMT003 SET COMDT       = LV_COMDT
                                STD_WRT_BEG = LS_DATA_NEW-STD_WRT_BEG
                                STD_WRT_END = LS_DATA_NEW-STD_WRT_END
                                CUS_REG_BEG = LV_CUS_REG_BEG
                                CUS_REG_END = LV_CUS_REG_END
                                UPDFLG      = ABAP_TRUE
                                AENAM       = SY-UNAME
                                AEDAT       = SY-DATUM
                                AEZET       = SY-UZEIT
                          WHERE MATNR EQ GS_RESULT-MATERIAL
                            AND SERNR EQ GS_RESULT-SERIAL_NEW.
          <LFS_DATA>-STATUS  = GC_SUCS.
          <LFS_DATA>-MESSAGE = TEXT-S01.
        ELSE.
          READ TABLE LT_DATA_EQUI INTO DATA(LS_DATA_EQUI)
          WITH KEY MATNR = GS_RESULT-MATERIAL
                   SERNR = GS_RESULT-SERIAL_NEW .
          IF SY-SUBRC EQ 0.
            LS_ZSDSCMT003-EQUNR       = LS_DATA_EQUI-EQUNR.
          ELSE.
            LS_ZSDSCMT003-EQUNR = CREATE_EQUI( <LFS_DATA> ).
          ENDIF.

          IF LS_ZSDSCMT003-EQUNR IS NOT INITIAL.
            LS_ZSDSCMT003-MATNR       = GS_RESULT-MATERIAL.
            LS_ZSDSCMT003-SERNR       = GS_RESULT-SERIAL_NEW.
            LS_ZSDSCMT003-COMDT       = LV_COMDT.
            LS_ZSDSCMT003-STD_WRT_BEG = LV_CUS_REG_BEG.
            LS_ZSDSCMT003-STD_WRT_END = LV_CUS_REG_END.
            LS_ZSDSCMT003-CUS_REG_BEG = LV_CUS_REG_BEG.
            LS_ZSDSCMT003-CUS_REG_END = LV_CUS_REG_END.
            LS_ZSDSCMT003-UPDFLG      = ABAP_TRUE.
            LS_ZSDSCMT003-ERNAM       = SY-UNAME.
            LS_ZSDSCMT003-ERDAT       = SY-DATUM.
            LS_ZSDSCMT003-ERZET       = SY-UZEIT.
            LS_ZSDSCMT003-AENAM       = SY-UNAME.
            LS_ZSDSCMT003-AEDAT       = SY-DATUM.
            LS_ZSDSCMT003-AEZET       = SY-UZEIT.
            INSERT ZSDSCMT003 FROM LS_ZSDSCMT003.
            <LFS_DATA>-STATUS  = GC_SUCS.
            <LFS_DATA>-MESSAGE = TEXT-S01.
          ELSE.
            <LFS_DATA>-STATUS  = GC_ERRO.
            <LFS_DATA>-MESSAGE = TEXT-E02.
          ENDIF.
*          LS_MATERIAL-MATNR = GS_RESULT-MATERIAL.
*          LS_MATERIAL-SERNR = GS_RESULT-SERIAL_NEW.
*          APPEND LS_MATERIAL TO LT_MATERIAL_UPDATE.
*          <LFS_DATA>-STATUS  = GC_ERRO.
*          <LFS_DATA>-MESSAGE = TEXT-E01.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.
  METHOD CONVERT_DATE.

    DATA : LV_DATE  TYPE C LENGTH 2,
           LV_MONTH TYPE C LENGTH 2,
           LV_YEAR  TYPE C LENGTH 4.

    SPLIT I_DATA AT '.' INTO LV_DATE
                             LV_MONTH
                             LV_YEAR.

    CONCATENATE LV_YEAR LV_MONTH LV_DATE INTO R.
  ENDMETHOD.
  METHOD CREATE_EQUI.
    PERFORM F_BDC_DATA USING I_DATA.

    DATA : LV_MATNR TYPE EQUI-MATNR,
           lv_Sernr TYPE EQUI-SERNR.

    LV_MATNR = I_DATA-MATERIAL.
    LV_SERNR = I_DATA-SERIAL_NEW.

    SELECT SINGLE EQUNR
      FROM EQUI
      WHERE MATNR EQ @LV_MATNR
        AND SERNR EQ @lv_Sernr
      INTO @R.
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
