*&---------------------------------------------------------------------*
*& Include          ZSDSCAR0060_CLASS
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
      CALL_Z_SDSMM_MATERIAL,
      CALL_Z_SDSSD_IF_SOINV,
      CALL_Z_SDSSD_IF_SOGI,
      CALL_Z_SDSMM_SEND_MAT_MASTER,
      CALL_Z_SDSMM_SEND_BOM_MASTER,
      CALL_Z_SDSFI_CUSTOMER_JSON.
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
    IF     R1 EQ ABAP_TRUE.
      CALL_Z_SDSMM_SEND_MAT_MASTER( ).
    ELSEIF R2 EQ ABAP_TRUE.
      CALL_Z_SDSMM_SEND_BOM_MASTER( ).
    ELSEIF R3 EQ ABAP_TRUE.
      CALL_Z_SDSMM_MATERIAL( ).
    ELSEIF R4 EQ ABAP_TRUE.
      CALL_Z_SDSSD_IF_SOINV( ).
    ELSEIF R5 EQ ABAP_TRUE.
      CALL_Z_SDSSD_IF_SOGI( ).
    ELSEIF R6 EQ ABAP_TRUE.
      CALL_Z_SDSFI_CUSTOMER_JSON( ).
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
  METHOD CHECK_SCREEN.

    DATA : LV_MAT TYPE I,
           LV_DAT TYPE I,
           LV_MDC TYPE I,
           LV_NAM TYPE I,
           LV_AL1 TYPE I,
           LV_CUS TYPE I,
           LV_LOC TYPE I.

    CONSTANTS : BEGIN OF LC_CON,
                  MAT TYPE C LENGTH 3 VALUE 'MAT',
                  DAT TYPE C LENGTH 3 VALUE 'DAT',
                  MDC TYPE C LENGTH 3 VALUE 'MDC',
                  NAM TYPE C LENGTH 3 VALUE 'NAM',
                  AL1 TYPE C LENGTH 3 VALUE 'AL1',
                  CUS TYPE C LENGTH 3 VALUE 'CUS',
                  LOC TYPE c LENGTH 3 VALUE 'LOC',
                END OF LC_CON.

    IF     R1 EQ ABAP_TRUE.
      LV_MAT = 1.
      LV_DAT = 1.
      LV_MDC = 0.
      LV_NAM = 0.
      LV_AL1 = 1.
      LV_CUS = 0.
      LV_LOC = 1.
    ELSEIF R2 EQ ABAP_TRUE.
      LV_MAT = 1.
      LV_DAT = 1.
      LV_MDC = 0.
      LV_NAM = 0.
      LV_AL1 = 1.
      LV_CUS = 0.
      LV_LOC = 0.
    ELSEIF R3 EQ ABAP_TRUE.
      LV_MAT = 1.
      LV_DAT = 0.
      LV_MDC = 1.
      LV_NAM = 0.
      LV_AL1 = 1.
      LV_CUS = 0.
      LV_LOC = 0.
    ELSEIF R4 EQ ABAP_TRUE.
      LV_MAT = 0.
      LV_DAT = 1.
      LV_MDC = 0.
      LV_NAM = 1.
      LV_AL1 = 1.
      LV_CUS = 0.
      LV_LOC = 0.
    ELSEIF R5 EQ ABAP_TRUE.
      LV_MAT = 0.
      LV_DAT = 1.
      LV_MDC = 0.
      LV_NAM = 1.
      LV_AL1 = 1.
      LV_CUS = 0.
      LV_LOC = 0.
    ELSEIF R6 EQ ABAP_TRUE.
      LV_MAT = 0.
      LV_DAT = 0.
      LV_MDC = 0.
      LV_NAM = 0.
      LV_AL1 = 1.
      LV_CUS = 1.
      LV_LOC = 0.
    ENDIF.

    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = LC_CON-MAT.
        SCREEN-ACTIVE = LV_MAT.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-GROUP1 = LC_CON-DAT.
        SCREEN-ACTIVE = LV_DAT.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-GROUP1 = LC_CON-MDC.
        SCREEN-ACTIVE = LV_MDC.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-GROUP1 = LC_CON-NAM.
        SCREEN-ACTIVE = LV_NAM.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-GROUP1 = LC_CON-AL1.
        SCREEN-ACTIVE = LV_AL1.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-GROUP1 = LC_CON-CUS.
        SCREEN-ACTIVE = LV_CUS.
        MODIFY SCREEN.
      ENDIF.

       IF SCREEN-GROUP1 = LC_CON-LOC.
        SCREEN-ACTIVE = LV_LOC.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
  METHOD CALL_Z_SDSMM_MATERIAL.
    DATA : LV_STATUS TYPE FLAG.
    CALL FUNCTION 'Z_SDSMM_MATERIAL'
      EXPORTING
        IS_MATNR = S_MATNR[]
        IS_MATKL = S_MATKL[]
        I_AL11   = P_AL11
      IMPORTING
        E_STATUS = LV_STATUS.
    IF LV_STATUS EQ GC_S.
      MESSAGE S000 WITH TEXT-S01.
    ELSE.
      MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE GC_E.
    ENDIF.
  ENDMETHOD.
  METHOD CALL_Z_SDSSD_IF_SOINV.
    DATA : LV_STATUS TYPE FLAG.
    CALL FUNCTION 'Z_SDSSD_IF_SOINV'
      EXPORTING
        IS_DATE  = S_ERSDA[]
        IS_UNAME = S_UNAME[]
        I_AL11   = P_AL11
      IMPORTING
        E_STATUS = LV_STATUS.
    IF LV_STATUS EQ GC_S.
      MESSAGE S000 WITH TEXT-S01.
    ELSE.
      MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE GC_E.
    ENDIF.
  ENDMETHOD.
  METHOD CALL_Z_SDSSD_IF_SOGI.
    DATA : LV_STATUS TYPE FLAG.
    CALL FUNCTION 'Z_SDSSD_IF_SOGI'
      EXPORTING
        IS_DATE  = S_ERSDA[]
        IS_UNAME = S_UNAME[]
        I_AL11   = P_AL11
      IMPORTING
        E_STATUS = LV_STATUS.
    IF LV_STATUS EQ GC_S.
      MESSAGE S000 WITH TEXT-S01.
    ELSE.
      MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE GC_E.
    ENDIF.
  ENDMETHOD.
  METHOD CALL_Z_SDSMM_SEND_MAT_MASTER.

    DATA : LV_STATUS TYPE FLAG.

    CALL FUNCTION 'Z_SDSMM_SEND_MAT_MASTER'
      EXPORTING
        IR_DATE  = S_ERSDA[]
        IR_MATNR = S_MATNR[]
        IR_LGORT = S_LGORT[]
        I_AL11   = P_AL11
      IMPORTING
        E_STATUS = LV_STATUS.

    IF LV_STATUS EQ GC_S.
      MESSAGE S000 WITH TEXT-S01.
    ELSE.
      MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE GC_E.
    ENDIF.
  ENDMETHOD.
  METHOD CALL_Z_SDSMM_SEND_BOM_MASTER.
    DATA : LV_STATUS TYPE FLAG.
    CALL FUNCTION 'Z_SDSMM_SEND_BOM_MASTER'
      EXPORTING
        IR_DATE  = S_ERSDA[]
        IR_MATNR = S_MATNR[]
        I_AL11   = P_AL11
      IMPORTING
        E_STATUS = LV_STATUS.
    IF LV_STATUS EQ GC_S.
      MESSAGE S000 WITH TEXT-S01.
    ELSE.
      MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE GC_E.
    ENDIF.
  ENDMETHOD.
  METHOD CALL_Z_SDSFI_CUSTOMER_JSON.
    DATA : LV_STATUS TYPE FLAG.

    CALL FUNCTION 'Z_SDSFI_CUSTOMER_JSON'
      EXPORTING
        I_BUKRS   = P_BUKRS
        IR_KUNNR  = S_KUNNR[]
        IR_ERDAT  = S_ERDAT[]
        I_ETAX    = P_ETAX
        I_EMAIL   = P_EMAIL
        I_AL11    = P_AL11
      IMPORTING
        E_MESTYPE = LV_STATUS.
    IF LV_STATUS EQ GC_S.
      MESSAGE S000 WITH TEXT-S01.
    ELSE.
      MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE GC_E.
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
