*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0610_CLASS
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
    DATA : LCL_REPORT TYPE REF TO ZCL_SDSFI_REPORT.

    IF LCL_REPORT IS NOT BOUND.
      CREATE OBJECT LCL_REPORT.
    ENDIF.

    GT_RESULT = LCL_REPORT->GP_REPORT( IR_RLDNR  = S_RLDNR[]
                                       IR_RBUKRS = S_RBUKRS[]
                                       IR_GJAHR  = S_GJAHR[]
                                       IR_BELNR  = S_BELNR[]
                                       IR_BUDAT  = S_BUDAT[]
                                       IR_POSID  = S_POSID[]
                                       IR_PRCTR  = S_PRCTR[]
                                       IR_AUFNR  = S_AUFNR[]
                                       IR_MATNR  = S_MATNR[]
                                       IR_FKART  = S_FKART[]
                                       IR_VKORG  = S_VKORG[]
                                       IR_VTWEG  = S_VTWEG[]
                                       IR_VKGRP  = S_VKGRP[]
                                       IR_VKBUR  = S_VKBUR[]
                                       IR_PRODH  = S_PRODH[]
                                       IR_RACCT  = S_RACCT[]
     ).

*    SELECT *
*      FROM ZSDSVC_GET_SALES_REPORT
*      WHERE RLDNR    IN @S_RLDNR[]
*        AND RBUKRS   IN @S_RBUKRS[]
*        AND GJAHR    IN @S_GJAHR[]
*        AND BELNR    IN @S_BELNR[]
*        AND BUDAT    IN @S_BUDAT[]
*        AND PS_POSID IN @S_POSID[]
*      INTO CORRESPONDING FIELDS OF TABLE @GT_RESULT.
*
*    SELECT COUNT( * )
*      FROM ZSDSFIT056.
*    IF SY-SUBRC EQ 0.
*      SELECT B~*
*        FROM ZSDSFIT056 AS A
*        INNER JOIN ZSDSVC_GET_SALES_REPORT_ALL AS B ON A~RLDNR  EQ B~RLDNR  AND
*                                                       A~RBUKRS EQ B~RBUKRS AND
*                                                       A~GJAHR  EQ B~GJAHR  AND
*                                                       A~BELNR  EQ B~BELNR  AND
*                                                       A~DOCLN  EQ B~DOCLN
*        INTO CORRESPONDING FIELDS OF TABLE @GT_RESULT.
*    ENDIF.

  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
    DATA : LV_PRODH TYPE GY_RESULT-PRODH.

    DATA : BEGIN OF LS_DATA,
             AUFNR TYPE GY_RESULT-AUFNR,
             PRODH TYPE GY_RESULT-PRODH,
           END OF LS_DATA.
    DATA : LT_DATA LIKE HASHED TABLE OF LS_DATA WITH UNIQUE KEY AUFNR PRODH.

    DATA : BEGIN OF LS_PERNR,
             PERNR TYPE PA0002-PERNR,
           END OF LS_PERNR.
    DATA : LT_PERNR LIKE HASHED TABLE OF LS_PERNR WITH UNIQUE KEY PERNR.

    DATA : BEGIN OF LS_VBELN,
             VBELN TYPE VBRK-VBELN,
           END OF LS_VBELN.
    DATA : LT_VBELN LIKE HASHED TABLE OF LS_VBELN WITH UNIQUE KEY VBELN.

    DATA : LCL_SD TYPE REF TO ZCL_SDSSD_SALES_ANALYSIS.

    IF LCL_SD IS NOT BOUND.
      CREATE OBJECT LCL_SD.
    ENDIF.

    SORT  GT_RESULT BY RLDNR
                       RBUKRS
                       GJAHR
                       BELNR
                       DOCLN.

    DELETE ADJACENT DUPLICATES FROM GT_RESULT COMPARING RLDNR
                                                        RBUKRS
                                                        GJAHR
                                                        BELNR
                                                        DOCLN.

    LT_DATA = CORRESPONDING #( GT_RESULT DISCARDING DUPLICATES ).
    DELETE LT_DATA WHERE PRODH IS INITIAL.

    LT_VBELN = CORRESPONDING #( GT_RESULT DISCARDING DUPLICATES ).

    IF LT_VBELN IS NOT INITIAL.
      SELECT FROM VBPA
          FIELDS VBELN,
                 PARVW,
                 PERNR
          FOR ALL ENTRIES IN @LT_VBELN
           WHERE VBELN = @LT_VBELN-VBELN
             AND PARVW IN ( 'VE', 'ZM' )
            INTO TABLE @DATA(LT_PERS_C).

      IF SY-SUBRC IS INITIAL.
        SORT LT_PERS_C BY VBELN PARVW.
      ENDIF.
    ENDIF.

    LT_PERNR = CORRESPONDING #( GT_RESULT DISCARDING DUPLICATES ).
    LCL_SD->GET_PERSON_NAME(
      EXPORTING
        IT_PERNR  = LT_PERNR
      IMPORTING
        ET_PA0002 = DATA(LT_PERS) ).

    SORT GT_RESULT BY PS_POSID DESCENDING PRODH DESCENDING RACCT DESCENDING.
    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
      AT NEW PS_POSID.
        IF <LFS_RESULT>-PRODH IS NOT INITIAL.
          LV_PRODH = <LFS_RESULT>-PRODH.
        ELSE.
          LOOP AT GT_RESULT INTO DATA(LS_TMP) WHERE PS_POSID EQ <LFS_RESULT>-PS_POSID AND
                                                    PRODH    IS NOT INITIAL.
            LV_PRODH = LS_TMP-PRODH.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDAT.
      IF <LFS_RESULT>-RACCT+0(2) NE '41'.
        <LFS_RESULT>-SOQTY = <LFS_RESULT>-MSL.
      ENDIF.

      IF <LFS_RESULT>-PS_POSID IS NOT INITIAL AND
         <LFS_RESULT>-PRODH IS INITIAL.
        <LFS_RESULT>-PRODH = LV_PRODH.
      ENDIF.

      IF <LFS_RESULT>-BLART+0(1) NE 'R'.
        <LFS_RESULT>-FKART = <LFS_RESULT>-FKART_V.
        <LFS_RESULT>-VKORG = <LFS_RESULT>-VKORG_V.
        <LFS_RESULT>-VTWEG = <LFS_RESULT>-VTWEG_V.
        <LFS_RESULT>-VBELN = <LFS_RESULT>-VBELN_V.
        <LFS_RESULT>-POSNR = <LFS_RESULT>-POSNR_V.
        <LFS_RESULT>-VKGRP = <LFS_RESULT>-VKGRP_V.
        <LFS_RESULT>-VKBUR = <LFS_RESULT>-VKBUR_V.
        <LFS_RESULT>-SONO  = <LFS_RESULT>-SONO_V.
        <LFS_RESULT>-DONO  = <LFS_RESULT>-DONO_V.
        <LFS_RESULT>-GRPDS = <LFS_RESULT>-GRPDS_M.
        <LFS_RESULT>-OFFDS = <LFS_RESULT>-OFFDS_M.
        <LFS_RESULT>-CHADS = <LFS_RESULT>-CHADS_M.
        <LFS_RESULT>-ORGDS = <LFS_RESULT>-ORGDS_M.
        <LFS_RESULT>-DIVIS = <LFS_RESULT>-DIVIS_M.
        <LFS_RESULT>-APPLI = <LFS_RESULT>-APPLI_M.
        <LFS_RESULT>-PSTYV     = <LFS_RESULT>-PSTYV_V.
        <LFS_RESULT>-MATNR_BOM = <LFS_RESULT>-MATNR_BOM_V.
      ELSE.
        IF <LFS_RESULT>-VBELN_V IS NOT INITIAL.
          <LFS_RESULT>-FKART = <LFS_RESULT>-FKART_V.
          <LFS_RESULT>-VKORG = <LFS_RESULT>-VKORG_V.
          <LFS_RESULT>-VTWEG = <LFS_RESULT>-VTWEG_V.
          <LFS_RESULT>-VBELN = <LFS_RESULT>-VBELN_V.
          <LFS_RESULT>-POSNR = <LFS_RESULT>-POSNR_V.
          <LFS_RESULT>-VKGRP = <LFS_RESULT>-VKGRP_V.
          <LFS_RESULT>-VKBUR = <LFS_RESULT>-VKBUR_V.
          <LFS_RESULT>-SONO  = <LFS_RESULT>-SONO_V.
          <LFS_RESULT>-DONO  = <LFS_RESULT>-DONO_V.
          <LFS_RESULT>-GRPDS = <LFS_RESULT>-GRPDS_M.
          <LFS_RESULT>-OFFDS = <LFS_RESULT>-OFFDS_M.
          <LFS_RESULT>-CHADS = <LFS_RESULT>-CHADS_M.
          <LFS_RESULT>-ORGDS = <LFS_RESULT>-ORGDS_M.
          <LFS_RESULT>-DIVIS = <LFS_RESULT>-DIVIS_M.
          <LFS_RESULT>-APPLI = <LFS_RESULT>-APPLI_M.
          <LFS_RESULT>-PSTYV     = <LFS_RESULT>-PSTYV_V.
          <LFS_RESULT>-MATNR_BOM = <LFS_RESULT>-MATNR_BOM_V.
        ENDIF.
      ENDIF.

      IF <LFS_RESULT>-PRODH IS INITIAL AND
         <LFS_RESULT>-AUFNR IS NOT INITIAL.
        READ TABLE lt_Data INTO LS_DATA
        WITH KEY AUFNR = <LFS_RESULT>-AUFNR.
        IF SY-SUBRC EQ 0.
          <LFS_RESULT>-PRODH = LS_DATA-PRODH.
        ENDIF.
      ENDIF.

      IF <LFS_RESULT>-PRODH IS NOT INITIAL.
        <LFS_RESULT>-PH1 =  <LFS_RESULT>-PRODH+0(5).
        <LFS_RESULT>-PH2 =  <LFS_RESULT>-PRODH+5(5).
        <LFS_RESULT>-PH3 =  <LFS_RESULT>-PRODH+10(8).
      ENDIF.

      IF <LFS_RESULT>-PERNR IS INITIAL.
        READ TABLE LT_PERS_C ASSIGNING FIELD-SYMBOL(<L_PERS_C>)
          WITH KEY VBELN = <LFS_RESULT>-VBELN
             PARVW = 'VE'
             BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          <LFS_RESULT>-PERNR  = <L_PERS_C>-PERNR.
        ENDIF.
      ENDIF.

      READ TABLE LT_PERS ASSIGNING FIELD-SYMBOL(<L_PERS>)
      WITH KEY PERNR = <LFS_RESULT>-PERNR.
      IF SY-SUBRC IS INITIAL.
        <LFS_RESULT>-SALEN = <L_PERS>-NAME.
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
