*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0570_CLASS
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
      SAVE_DATA,
      UPDATE_WHT CHANGING C_DATA TYPE GY_RESULT,
      GET_CONFIG,
      REFRESH_ALV,
      GET_NUMBER IMPORTING I_DATA   TYPE BKPF-BUDAT
                 RETURNING VALUE(R) TYPE WITH_ITEM-CTNUMBER.
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

    SELECT DISTINCT VBRK~VBELN,
           VBRK~KUNRG,
           ( KNA1~NAME1 && ' ' && KNA1~NAME2 && ' ' && KNA1~NAME3 && ' ' && KNA1~NAME4 ) AS NAMEA,
           VBRK~NETWR,
           VBRK~WAERK,
           KNA1~ADRNR,
           KNA1~STCD3,
           VBRK~BUPLA,
           VBRK~ERDAT,
           @GC_CON-WARNING AS STATU,
           @ABAP_TRUE AS CHK,
      CASE WHEN SUM( PRCD_ELEMENTS~KWERT ) >  0 THEN SUM( PRCD_ELEMENTS~KWERT )
                ELSE 0
      END AS SUM,
           BKPF~BUKRS,
           BKPF~BELNR,
           BKPF~GJAHR,
           PRCD_ELEMENTS~KBETR,
           KNBW~QSREC,
           BKPF~BUDAT
      FROM VBRK
      INNER JOIN KNA1 ON VBRK~KUNRG EQ KNA1~KUNNR
      INNER JOIN PRCD_ELEMENTS ON VBRK~KNUMV          EQ PRCD_ELEMENTS~KNUMV AND
                                  PRCD_ELEMENTS~KSCHL EQ 'ZWHT'
      INNER JOIN BKPF ON VBRK~VBELN EQ BKPF~AWKEY
      LEFT JOIN KNBW ON VBRK~KUNRG EQ KNBW~KUNNR AND
                        KNBW~BUKRS EQ '1000' AND
                        KNBW~WITHT EQ 'R2'
      WHERE VBRK~VBELN IN @S_VBELN[]
        AND VBRK~FKDAT IN @S_FKDAT[]
        AND VBRK~FKART IN @S_FKART[] AND
      NOT EXISTS ( SELECT *
                     FROM WITH_ITEM
                    WHERE BUKRS EQ BKPF~BUKRS
                      AND BELNR EQ VBRK~VBELN
                      AND GJAHR EQ BKPF~GJAHR
                      AND WITHT EQ 'R2' )
      GROUP BY VBRK~VBELN,VBRK~KUNRG,VBRK~NETWR,KNA1~NAME1,KNA1~NAME2,KNA1~NAME3,KNA1~NAME4,
      VBRK~WAERK,KNA1~ADRNR,KNA1~STCD3,VBRK~BUPLA,VBRK~ERDAT,BKPF~BUKRS,BKPF~BELNR,BKPF~GJAHR,
      PRCD_ELEMENTS~KBETR,KNBW~QSREC,BKPF~BUDAT
      INTO TABLE @GT_RESULT.

    SORT GT_RESULT BY WHTAM DESCENDING.
    DELETE GT_RESULT WHERE WHTAM EQ 0.

    SORT GT_RESULT BY DOCNO.

  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    LCL_DATA=>GET_CONFIG( ).
    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
      <LFS_RESULT>-AMOUT = <LFS_RESULT>-AMOUT + <LFS_RESULT>-WHTAM.
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

    CONSTANTS : BEGIN OF LC_CON,
                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
                  CHK_NAME  TYPE C LENGTH 3 VALUE 'CHK',
                END OF LC_CON.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = LC_CON-CHK_FILED.
    LS_FCAT-SELTEXT_S   = LC_CON-CHK_NAME.
    LS_FCAT-SELTEXT_M   = LC_CON-CHK_NAME.
    LS_FCAT-SELTEXT_L   = LC_CON-CHK_FILED.
    LS_FCAT-CHECKBOX    = ABAP_TRUE.
    LS_FCAT-INPUT       = ABAP_TRUE.
    LS_FCAT-EDIT        = ABAP_TRUE.
    APPEND LS_FCAT TO GT_FCAT.

    DATA : LV_RUNNING  TYPE I,
           LV_DATA     TYPE C LENGTH 6 VALUE 'TEXT-',
           LV_RUN_TEXT TYPE C LENGTH 2.

    CONSTANTS : LC_F TYPE C VALUE 'F',
                LC_T TYPE C VALUE 'T',
                LC_d TYPE C VALUE 'D',
                LC_z TYPE C VALUE 'Z'.

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

      IF <LFS> IS ASSIGNED.
        UNASSIGN <LFS>.
      ENDIF.
      CONCATENATE LV_DATA LC_Z LV_RUN_TEXT INTO LV_TEXT.
      ASSIGN (LV_TEXT) TO <LFS>.
      IF <LFS> IS ASSIGNED.
        LS_FCAT-DO_SUM = ABAP_TRUE.
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
  METHOD SAVE_DATA.
    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>) WHERE CHECK EQ ABAP_TRUE.
      LCL_DATA=>UPDATE_WHT( CHANGING C_DATA = <LFS_DATA> ).
    ENDLOOP.
    REFRESH_ALV( ).
  ENDMETHOD.
  METHOD UPDATE_WHT.

    DATA : LS_WITH_ITEM TYPE WITH_ITEM,
           LS_MESSTAB   TYPE BDCMSGCOLL.

    DATA : LS_ZSDSFIT054 TYPE ZSDSFIT054.

    DATA : LV_CHECK TYPE C,
           LV_num   TYPE I.

    LS_WITH_ITEM-CTNUMBER    = GET_NUMBER( C_DATA-BUDAT ).

    IF LS_WITH_ITEM-CTNUMBER IS NOT INITIAL.

      LS_WITH_ITEM-MANDT       = SY-MANDT.
      LS_WITH_ITEM-BUKRS       = C_DATA-BUKRS.
      LS_WITH_ITEM-BELNR       = C_DATA-DOCNO.
      LS_WITH_ITEM-AUGBL       = C_DATA-BELNR.
      LS_WITH_ITEM-GJAHR       = C_DATA-GJAHR.
      LS_WITH_ITEM-WT_QSSHH    = C_DATA-AMOUT."* -1.
      LS_WITH_ITEM-WT_QSSHB    = C_DATA-AMOUT."* -1.
      LS_WITH_ITEM-WT_QBSHH    = C_DATA-WHTAM."* -1.
      LS_WITH_ITEM-WT_QBSHB    = C_DATA-WHTAM."* -1.
      LS_WITH_ITEM-WT_QBSHHA   = C_DATA-WHTAM."* -1.
      LS_WITH_ITEM-WT_QBSHHB   = C_DATA-WHTAM."* -1.
      LS_WITH_ITEM-WT_basman   = ABAP_TRUE.
      LS_WITH_ITEM-BUZEI       = '001'.
      LS_WITH_ITEM-WITHT       = 'R2'.
      LS_WITH_ITEM-WT_ACCO     = C_DATA-KUNNR.
      LS_WITH_ITEM-KOART       = 'D'.
      LS_WITH_ITEM-QSATZ       = C_DATA-KBETR.
      LS_WITH_ITEM-QSREC       = C_DATA-QSREC.
      LS_WITH_ITEM-CTISSUEDATE = C_DATA-CREDT.

      LV_CHECK = LV_num = LS_WITH_ITEM-QSATZ.

      LS_MESSTAB-MSGV1  = LS_WITH_ITEM-BELNR .

      READ TABLE GT_WITH_TYPE INTO DATA(LS_DATA)
      WITH KEY PARAM_EXT = LV_CHECK.
      IF SY-SUBRC EQ 0.
        LS_WITH_ITEM-WT_WITHCD = LS_DATA-VALUE_LOW.
      ENDIF.

      CALL FUNCTION 'J1IEWT_UPDATE_WITH_ITEM'
        EXPORTING
          IS_WITH_ITEM = LS_WITH_ITEM
          IS_MESSTAB   = LS_MESSTAB.
      IF SY-SUBRC EQ 0.
        C_DATA-STATU = GC_CON-SUCCESS.
        C_DATA-MESSG = TEXT-S01.
        COMMIT WORK AND WAIT.

        LS_ZSDSFIT054-DOCNO    = LS_WITH_ITEM-BELNR.
        LS_ZSDSFIT054-CTNUMBER = LS_WITH_ITEM-CTNUMBER .
        LS_ZSDSFIT054-ERNAM    = SY-UNAME.
        LS_ZSDSFIT054-ERDAT    = SY-DATUM.
        LS_ZSDSFIT054-ERZET    = SY-UZEIT.
        LS_ZSDSFIT054-AENAM    = SY-UNAME.
        LS_ZSDSFIT054-AEDAT    = SY-DATUM.
        LS_ZSDSFIT054-AEZET    = SY-UZEIT.
        MODIFY ZSDSFIT054 FROM LS_ZSDSFIT054.
        COMMIT WORK AND WAIT.
      ELSE.
        C_DATA-STATU = GC_CON-ERROR.
        C_DATA-MESSG = TEXT-E01.
      ENDIF.


    ELSE.
      C_DATA-STATU = GC_CON-ERROR.
      C_DATA-MESSG = TEXT-E02.
    ENDIF.

  ENDMETHOD.
  METHOD GET_CONFIG.
    SELECT PARAM_EXT,
           VALUE_LOW
      FROM ZSDSCAC001
      WHERE REPID EQ 'ZSDSFIR0570'
        AND PARAM EQ 'WHT_TYPE'
      INTO TABLE @GT_WITH_TYPE.
  ENDMETHOD.
  METHOD REFRESH_ALV.
    DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

    IF REF_GRID IS INITIAL.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          E_GRID = REF_GRID.
      CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
    ENDIF.

    CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
    CLEAR : REF_GRID.
  ENDMETHOD.
  METHOD GET_NUMBER.

    DATA : LV_NR_RANGE_NR TYPE INRI-NRRANGENR,
           LV_TOYEAR      TYPE INRI-TOYEAR.

    LV_NR_RANGE_NR = I_DATA+4(2).
    LV_TOYEAR      = I_DATA+0(4).

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = LV_NR_RANGE_NR
        OBJECT                  = 'WITH_CTNO1'
        SUBOBJECT               = '200053'
        TOYEAR                  = LV_TOYEAR
      IMPORTING
        NUMBER                  = R
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
*       OTHERS                  = 8
      .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
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
