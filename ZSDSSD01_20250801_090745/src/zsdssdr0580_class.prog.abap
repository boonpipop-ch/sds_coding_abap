*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0140_CLASS
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
      SAVE,
      CLEAR_CONFIRM.
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

    DATA : BEGIN OF LS_SALESMAN,
             PERNR TYPE VBPA-PERNR,
           END OF LS_SALESMAN.
    DATA : LT_SALESMAN LIKE HASHED TABLE OF LS_SALESMAN WITH UNIQUE KEY PERNR.

    SELECT VBEP~VBELN,
           VBEP~POSNR,
           VBEP~ETENR,
           VBEP~EDATU,
           VBEP~WMENG,
           VBEP~BMENG,
           VBAP~MATNR,
           VBAP~GBSTA,
           VBAK~KUNNR,
           VBPA~PARVW,
           VBPA~PERNR,
           KNA1~NAME1,
           KNA1~NAME2,
           KNA1~NAME3,
           KNA1~NAME4
      FROM VBAP
      INNER JOIN VBEP  ON VBAP~VBELN EQ VBEP~VBELN
                      AND VBAP~POSNR EQ VBEP~POSNR
      INNER JOIN VBAK  ON VBAP~VBELN EQ VBAK~VBELN
       LEFT JOIN VBPA  ON VBAP~VBELN EQ VBPA~VBELN
                      AND VBPA~POSNR EQ '000000'
                      AND VBPA~PARVW EQ 'VE'
      INNER JOIN KNA1  ON VBAK~KUNNR EQ  KNA1~KUNNR
      WHERE VBAP~VBELN IN @S_VBELN AND
            VBAP~POSNR IN @S_POSNR AND
            VBAP~MATNR IN @S_MATNR AND
            VBAP~GBSTA NE 'C'      AND
            VBAK~VBTYP EQ 'C'
            INTO TABLE @DATA(LT_TMP).

    LT_SALESMAN =  CORRESPONDING #( LT_TMP  DISCARDING DUPLICATES ).

    SELECT PA0002~PERNR,
           PA0002~NACHN,
           PA0002~VORNA
    FROM @LT_SALESMAN AS A
    INNER JOIN PA0002 ON PA0002~PERNR EQ A~PERNR
    INTO TABLE @DATA(LT_TMP2).

    LOOP AT LT_TMP INTO DATA(LS_TMP).
      GS_RESULT-VBELN = LS_TMP-VBELN.
      GS_RESULT-POSNR = LS_TMP-POSNR.
      GS_RESULT-ETENR = LS_TMP-ETENR.
      GS_RESULT-EDATU = LS_TMP-EDATU.
      GS_RESULT-WMENG = LS_TMP-WMENG.
      GS_RESULT-BMENG = LS_TMP-BMENG.
      GS_RESULT-MATNR = LS_TMP-MATNR.
      GS_RESULT-GBSTA = LS_TMP-GBSTA.
      GS_RESULT-KUNNR = LS_TMP-KUNNR.
      GS_RESULT-PARVW = LS_TMP-PARVW.
      GS_RESULT-PERNR = LS_TMP-PERNR.
      GS_RESULT-CHECK = ABAP_TRUE.

      CONCATENATE LS_TMP-NAME1 LS_TMP-NAME2 LS_TMP-NAME3 LS_TMP-NAME4 INTO GS_RESULT-NAME SEPARATED BY SPACE.

      READ TABLE LT_TMP2 INTO DATA(LS_TMP2)
      WITH KEY PERNR = LS_TMP-PERNR.
      IF SY-SUBRC = 0.
        GS_RESULT-NACHN           = LS_TMP2-NACHN.
        GS_RESULT-VORNA           = LS_TMP2-VORNA.
      ENDIF.

      APPEND GS_RESULT TO GT_RESULT.
      CLEAR GS_RESULT.
    ENDLOOP.

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

      IF LS_FCAT-FIELDNAME = 'BMENG'.
        LS_FCAT-INPUT       = ABAP_TRUE.
        LS_FCAT-EDIT        = ABAP_TRUE.
        LS_FCAT-DO_SUM      = ABAP_TRUE.
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
  METHOD SAVE.
    DATA LCL_SO TYPE REF TO ZCL_SDSSD_SALES_DOCUMENT.

    DATA : LT_VBELN	 TYPE ZSDSSDS128_TT,
           LT_RETURN TYPE ZSDSSDS124_TT.

    DATA : LS_VBELN TYPE ZSDSSDS128.

    IF LCL_SO IS NOT BOUND.
      CREATE OBJECT LCL_SO.
    ENDIF.

    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>) WHERE CHECK EQ ABAP_TRUE.
      LS_VBELN-VBELN = <LFS_DATA>-VBELN.
      LS_VBELN-POSNR = <LFS_DATA>-POSNR.
      LS_VBELN-ETENR = <LFS_DATA>-ETENR.
      IF P_ZERO EQ ABAP_TRUE.
        LS_VBELN-BMENG = 0.
      ELSE.
        LS_VBELN-BMENG = <LFS_DATA>-BMENG.
      ENDIF.
      APPEND LS_VBELN TO LT_VBELN.
    ENDLOOP.

    IF LT_VBELN IS NOT INITIAL.
      SORT LT_VBELN.
      DELETE ADJACENT DUPLICATES FROM LT_VBELN.
      LT_RETURN = LCL_SO->CHANGE_CONFIRM_QTY( LT_VBELN ).
    ENDIF.
  ENDMETHOD.
  METHOD CLEAR_CONFIRM.
    LOOP AT gt_Result ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
      <LFS_DATA>-BMENG = 0.
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
