*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0720_CLASS
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
    SELECT A~RSNUM,
           A~RSPOS,
           A~MATNR,
           F~MAKTX,
           A~WERKS,
           A~LGORT,
           A~UMWRK,
           A~UMLGO,
           A~BDMNG,
           A~MEINS,
           @GC_WARN AS STATUS,
           @SPACE AS MESSAGE,
           A~BDTER
          FROM ZSDSMMT028
          INNER JOIN RESB AS A ON ZSDSMMT028~RSNUM EQ A~RSNUM
          LEFT JOIN LFA1  AS E ON A~LIFNR EQ E~LIFNR  ##SELECT_NULL_VALUES[LIFNR]
          LEFT JOIN MAKT  AS F ON A~MATNR EQ F~MATNR AND
                                  F~SPRAS EQ @SY-LANGU
          LEFT JOIN T001L AS G ON A~UMLGO EQ G~LGORT AND
                                  A~WERKS EQ G~WERKS
          WHERE
*                EXISTS ( SELECT RSNUM
*                           FROM ZSDSMMT028 AS B
*                          WHERE B~RSNUM EQ A~RSNUM )
             ZSDSMMT028~RSNUM IN @S_RSNUM[]
         AND A~RSNUM          NE @SPACE
         AND A~SHKZG          EQ 'H'
         AND ZSDSMMT028~STATU EQ 'SAV'
         AND ZSDSMMT028~MIGOF EQ @SPACE
         INTO TABLE @GT_RESULT.

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
                END OF LC_CON.
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

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = 'STATUS'.
    LS_FCAT-SELTEXT_S   = 'Status'.
    LS_FCAT-SELTEXT_M   = 'Status'.
    LS_FCAT-SELTEXT_L   = 'Status'.
    APPEND LS_FCAT TO GT_FCAT.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = 'MESSAGE'.
    LS_FCAT-SELTEXT_S   = 'Message'.
    LS_FCAT-SELTEXT_M   = 'Message'.
    LS_FCAT-SELTEXT_L   = 'Message'.
    APPEND LS_FCAT TO GT_FCAT.

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
  METHOD SAVE.

    DATA : LS_GOODSMVT_HEADER TYPE BAPI2017_GM_HEAD_01.

    DATA : LT_GOODSMVT_ITEM	TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
           LT_RETURN        TYPE TABLE OF BAPIRET2.

    DATA : LS_GOODSMVT_ITEM TYPE BAPI2017_GM_ITEM_CREATE.

    DATA : LV_GOODSMVT_HEADRET TYPE BAPI2017_GM_HEAD_RET,
           LV_MATERIALDOCUMENT TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC,
           LV_MATDOCUMENTYEAR  TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR.

    DATA : LV_TESTRUN TYPE BAPI2017_GM_GEN-TESTRUN.

    DATA : LT_TMP LIKE GT_RESULT.

    LT_TMP = GT_RESULT.

    SORT LT_TMP BY RSNUM RSPOS.

    LOOP AT LT_TMP INTO DATA(LS_RESULT).
      MOVE-CORRESPONDING LS_RESULT TO GS_RESULT.

      LV_TESTRUN = C_TEST.

      LS_GOODSMVT_ITEM-MATERIAL_LONG = GS_RESULT-MATNR.
      LS_GOODSMVT_ITEM-PLANT         = GS_RESULT-WERKS.
      LS_GOODSMVT_ITEM-STGE_LOC      = GS_RESULT-LGORT.
      LS_GOODSMVT_ITEM-MOVE_TYPE     = '311'.
      LS_GOODSMVT_ITEM-ENTRY_QNT     = GS_RESULT-BDMNG.
      LS_GOODSMVT_ITEM-ENTRY_UOM     = GS_RESULT-MEINS.
      LS_GOODSMVT_ITEM-ENTRY_UOM_ISO = ''.
      LS_GOODSMVT_ITEM-RESERV_NO     = GS_RESULT-RSNUM.
      LS_GOODSMVT_ITEM-RES_ITEM      = GS_RESULT-RSPOS.
      LS_GOODSMVT_ITEM-MOVE_MAT_LONG = GS_RESULT-MATNR.
      LS_GOODSMVT_ITEM-MOVE_PLANT    = GS_RESULT-UMWRK.
      LS_GOODSMVT_ITEM-MOVE_STLOC    = GS_RESULT-UMLGO.

      APPEND LS_GOODSMVT_ITEM TO LT_GOODSMVT_ITEM.
      CLEAR LS_GOODSMVT_ITEM.
      AT END OF RSNUM.
        LS_GOODSMVT_HEADER-DOC_DATE        = P_BLDAT.
        LS_GOODSMVT_HEADER-PSTNG_DATE      = P_BUDAT.
        LS_GOODSMVT_HEADER-VER_GR_GI_SLIP  = '3'.
        LS_GOODSMVT_HEADER-REF_DOC_NO      = SPACE.
        LS_GOODSMVT_HEADER-HEADER_TXT      = SPACE.
        LS_GOODSMVT_HEADER-REF_DOC_NO_LONG = SPACE.

        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            GOODSMVT_HEADER  = LS_GOODSMVT_HEADER
            GOODSMVT_CODE    = '04'
            TESTRUN          = LV_TESTRUN
*           GOODSMVT_REF_EWM =
*           GOODSMVT_PRINT_CTRL           =
          IMPORTING
            GOODSMVT_HEADRET = LV_GOODSMVT_HEADRET
            MATERIALDOCUMENT = LV_MATERIALDOCUMENT
            MATDOCUMENTYEAR  = LV_MATDOCUMENTYEAR
          TABLES
            GOODSMVT_ITEM    = LT_GOODSMVT_ITEM
*           GOODSMVT_SERIALNUMBER         =
            RETURN           = LT_RETURN
*           GOODSMVT_SERV_PART_DATA       =
*           EXTENSIONIN      =
*           GOODSMVT_ITEM_CWM             =
          EXCEPTIONS
            ERROR_MESSAGE    = 1
            OTHERS           = 2.

        IF LV_MATERIALDOCUMENT IS NOT INITIAL.
          COMMIT WORK AND WAIT.
          UPDATE ZSDSMMT028 SET MIGOF = ABAP_TRUE
                          WHERE RSNUM EQ GS_RESULT-RSNUM.

          GS_RESULT-STATUS  = GC_SUCS.
          GS_RESULT-MESSAGE = TEXT-S01.
          GS_RESULT-MBLNR   = LV_MATERIALDOCUMENT.
          GS_RESULT-MJAHR   = LV_MATDOCUMENTYEAR.

          MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING STATUS MESSAGE MBLNR MJAHR
                                          WHERE RSNUM = GS_RESULT-RSNUM.
        ELSE.
          IF C_TEST EQ ABAP_TRUE.
            IF LT_RETURN IS INITIAL.
              GS_RESULT-STATUS  = GC_SUCS.
              GS_RESULT-MESSAGE = TEXT-S02.

              MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING STATUS MESSAGE MBLNR MJAHR
                                                     WHERE RSNUM = GS_RESULT-RSNUM.
            ENDIF.
          ENDIF.

          LOOP AT LT_RETURN INTO DATA(LS_DATA) WHERE TYPE EQ 'E' OR
                                                     TYPE EQ 'A'.
            GS_RESULT-STATUS  = GC_ERRO.
            GS_RESULT-MESSAGE = LS_DATA-MESSAGE.

            MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING STATUS MESSAGE
                                            WHERE RSNUM = GS_RESULT-RSNUM.
            EXIT.
          ENDLOOP.
        ENDIF.

        CLEAR : LT_GOODSMVT_ITEM,LT_RETURN,LV_GOODSMVT_HEADRET,
                LV_MATERIALDOCUMENT,LS_GOODSMVT_HEADER.
      ENDAT.
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
