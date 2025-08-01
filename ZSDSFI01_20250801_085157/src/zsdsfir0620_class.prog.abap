*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0620_CLASS
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
      INIT_DATA,
      GET_LAST_DAY_MONTH IMPORTING I_DATA   TYPE SY-DATUM
                         RETURNING VALUE(R) TYPE SY-DATUM,
      DEFAULT_STORAGE RETURNING VALUE(R) LIKE S_LAGER[],
      DEFAULT_MOVEMENT_TYPE RETURNING VALUE(R) LIKE S_BWART[],
      GET_YEAR CHANGING C_DATA TYPE GY_RESULT,
      GET_DATA_SERIAL,
      GET_DATA_NON_SERIAL.
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

    GET_DATA_SERIAL( ).
    GET_DATA_NON_SERIAL( ).

  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.

    DATA : BEGIN OF LS_MAT,
             MATNR TYPE EQUI-MATNR,
             SERNR TYPE EQUI-SERNR,
           END OF LS_MAT.
    DATA : LT_MAT LIKE HASHED TABLE OF LS_MAT WITH UNIQUE KEY MATNR
                                                              SERNR.

    DATA : BEGIN OF LS_MAT_NO_SERIAL,
             MATNR TYPE EQUI-MATNR,
           END OF LS_MAT_NO_SERIAL.
    DATA : LT_MAT_NO_SERIAL LIKE HASHED TABLE OF LS_MAT_NO_SERIAL WITH UNIQUE KEY MATNR.

    LT_MAT           =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).
    LT_MAT_NO_SERIAL =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).

    SELECT ZDSMMT005~MATNR,
           ZDSMMT005~ZSERNR,
           ZDSMMT005~DATUM
      FROM @LT_MAT AS A
      INNER JOIN ZDSMMT005 ON A~MATNR         EQ ZDSMMT005~MATNR AND
                              A~SERNR         EQ ZDSMMT005~ZSERNR AND
                              ZDSMMT005~WERKS EQ '1000'
      INTO TABLE @DATA(LT_DATA).

    SELECT MSEG~MATNR,
           MKPF~BUDAT,
           MSEG~MBLNR,
           MSEG~MJAHR,
           MSEG~LGORT,
           MSEG~MENGE,
           MSEG~BWART
     FROM @LT_MAT_NO_SERIAL AS A
     INNER JOIN MARC ON A~MATNR EQ MARC~MATNR AND
                        MARC~SERNP EQ @SPACE
     INNER JOIN MSEG ON MARC~MATNR EQ MSEG~MATNR
     INNER JOIN MKPF ON MSEG~MBLNR EQ MKPF~MBLNR AND
                        MSEG~MJAHR EQ MKPF~MJAHR
     WHERE MSEG~WERKS EQ '1000'
       AND MSEG~LGORT IN @S_LAGER
       AND MSEG~BWART IN @S_BWART
       AND MSEG~SOBKZ EQ @SPACE
    ORDER BY MSEG~MATNR,MKPF~BUDAT DESCENDING
    INTO TABLE @DATA(LT_MSEG).

    SELECT ZDSMMT004~MATNR,
           ZDSMMT004~BUDAT
     FROM @LT_MAT_NO_SERIAL AS A
     INNER JOIN MARC ON A~MATNR EQ MARC~MATNR AND
                        MARC~SERNP EQ @SPACE
     INNER JOIN ZDSMMT004 ON MARC~MATNR EQ ZDSMMT004~MATNR
     WHERE ZDSMMT004~WERKS EQ '1000'
       AND ZDSMMT004~LGORT IN @S_LAGER
       AND ZDSMMT004~BWART IN @S_BWART
    ORDER BY ZDSMMT004~MATNR,ZDSMMT004~BUDAT DESCENDING
    INTO TABLE @DATA(LT_ZDSMMT005).

    DATA : LS_DATA      LIKE LINE OF LT_DATA,
           LS_MSEG      LIKE LINE OF LT_MSEG,
           Ls_ZDSMMT005 LIKE LINE OF LT_ZDSMMT005.

    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
      READ TABLE LT_DATA INTO LS_DATA
      WITH KEY MATNR  = <LFS_RESULT>-MATNR
               ZSERNR = <LFS_RESULT>-SERNR.
      IF SY-SUBRC EQ 0.
        <LFS_RESULT>-ERDAT = LS_DATA-DATUM.
      ELSE.
        READ TABLE LT_MSEG INTO LS_MSEG
        WITH KEY MATNR = <LFS_RESULT>-MATNR.
        IF SY-SUBRC EQ 0.
          <LFS_RESULT>-ERDAT = LS_MSEG-BUDAT.
          IF LS_MSEG-BWART EQ '561'.
            READ TABLE LT_ZDSMMT005 INTO LS_ZDSMMT005
            WITH KEY MATNR = <LFS_RESULT>-MATNR.
            IF SY-SUBRC EQ 0.
              <LFS_RESULT>-ERDAT = LS_ZDSMMT005-BUDAT.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE LT_ZDSMMT005 INTO LS_ZDSMMT005
          WITH KEY MATNR = <LFS_RESULT>-MATNR.
          IF SY-SUBRC EQ 0.
            <LFS_RESULT>-ERDAT = LS_ZDSMMT005-BUDAT.
          ENDIF.
        ENDIF.
      ENDIF.
      IF <LFS_RESULT>-ERDAT IS NOT INITIAL.
        GET_YEAR( CHANGING C_DATA = <LFS_RESULT> ).
      ENDIF.

      <LFS_RESULT>-COST_ALL = <LFS_RESULT>-QTY * <LFS_RESULT>-VERPR.
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
      IF LV_RUNNING EQ 1.
        CONTINUE.
      ENDIF.
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
  METHOD INIT_DATA.
    P_ASOF = GET_LAST_DAY_MONTH( SY-DATUM ).
    S_LAGER[] = DEFAULT_STORAGE( ).
    S_BWART[] = DEFAULT_MOVEMENT_TYPE( ).
  ENDMETHOD.
  METHOD GET_LAST_DAY_MONTH.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = I_DATA
      IMPORTING
        LAST_DAY_OF_MONTH = R
      EXCEPTIONS
        DAY_IN_NO_DATE    = 1
        OTHERS            = 2.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.
  METHOD DEFAULT_STORAGE.
    CONSTANTS : BEGIN OF LC_RANGES,
                  OPTION TYPE C LENGTH 2 VALUE 'BT',
                  SIGN   TYPE C LENGTH 1 VALUE 'I',
                END OF LC_RANGES.

    DATA LS_LAGER LIKE LINE OF S_LAGER.
    LS_LAGER-OPTION  = LC_RANGES-OPTION.
    LS_LAGER-SIGN    = LC_RANGES-SIGN.
    LS_LAGER-LOW     = '1000'.
    LS_LAGER-HIGH    = '1999'.
    APPEND LS_LAGER TO R.
  ENDMETHOD.
  METHOD DEFAULT_MOVEMENT_TYPE.
    CONSTANTS : BEGIN OF LC_RANGES,
                  OPTION TYPE C LENGTH 2 VALUE 'EQ',
                  SIGN   TYPE C LENGTH 1 VALUE 'I',
                END OF LC_RANGES.

    CONSTANTS : BEGIN OF LC_MOVE_TYPE,
                  R1 TYPE C LENGTH 3 VALUE '101',
                  R2 TYPE C LENGTH 3 VALUE '105',
                  R3 TYPE C LENGTH 3 VALUE '561',
                END OF LC_MOVE_TYPE.

    DATA LS_BWART LIKE LINE OF S_BWART.

    LS_BWART-OPTION  = LC_RANGES-OPTION.
    LS_BWART-SIGN    = LC_RANGES-SIGN.
    LS_BWART-LOW     = LC_MOVE_TYPE-R1.
    APPEND LS_BWART TO R.

    LS_BWART-OPTION  = LC_RANGES-OPTION.
    LS_BWART-SIGN    = LC_RANGES-SIGN.
    LS_BWART-LOW     = LC_MOVE_TYPE-R2.
    APPEND LS_BWART TO R.

    LS_BWART-OPTION  = LC_RANGES-OPTION.
    LS_BWART-SIGN    = LC_RANGES-SIGN.
    LS_BWART-LOW     = LC_MOVE_TYPE-R3.
    APPEND LS_BWART TO R.

  ENDMETHOD.
  METHOD GET_YEAR.
    DATA : LV_YEAR        TYPE I,
           LV_YEAR_RESULT TYPE P DECIMALS 2.
    DATA : LV_FDPOS TYPE SY-FDPOS.

    DATA : LV_YEAR_1   TYPE C LENGTH 12,
           LV_AGING    TYPE C LENGTH 12,
           LV_AGINGDAY TYPE C LENGTH 12.

    DATA : LV_CHECK TYPE I.

    CONSTANTS : LC_1_YEAR TYPE I VALUE '365'.
    CLEAR : LV_YEAR,LV_YEAR_RESULT.

    LV_YEAR         = P_ASOF - C_DATA-ERDAT.
    LV_YEAR_RESULT  = LV_YEAR / LC_1_YEAR.
    LV_YEAR_1       = LV_YEAR_RESULT.
    LV_AGINGDAY     = LV_YEAR.

    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_YEAR_1   WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_AGINGDAY WITH ''.
    SEARCH LV_YEAR_1 FOR '. .'.
    IF SY-SUBRC = 0.
      LV_FDPOS  = SY-FDPOS - 3.
      LV_AGING = LV_YEAR_1+0(LV_FDPOS).

      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_AGINGDAY WITH ''.
      LV_CHECK = LV_AGINGDAY.
      IF     LV_CHECK LE 365.
        LV_AGING = '0'.
      ELSEIF LV_CHECK GT 365 AND
             LV_CHECK LE 730.
        LV_AGING = '1'.
      ENDIF.

      C_DATA-YEAR     = LV_YEAR_1.
      C_DATA-AGEING   = LV_AGING.
      C_DATA-AGINGDAY = LV_AGINGDAY.

    ENDIF.
  ENDMETHOD.
  METHOD GET_DATA_SERIAL.
    SELECT ZSDSVC_GET_REMAIN_SERIAL~*,
           @ABAP_TRUE AS CHECK,
           1 AS QTY,
           MARA~MFRNR,
           ( LFA1~NAME1 && ' ' && LFA1~NAME2 && ' ' && LFA1~NAME3 && ' ' && LFA1~NAME4 ) AS NAME_ALL
      FROM ZSDSVC_GET_REMAIN_SERIAL
      INNER JOIN MARA ON ZSDSVC_GET_REMAIN_SERIAL~MATNR EQ MARA~MATNR
      LEFT JOIN  LFA1 ON MARA~MFRNR EQ LFA1~LIFNR
      WHERE ZSDSVC_GET_REMAIN_SERIAL~B_LAGER IN @S_LAGER[]
        AND ZSDSVC_GET_REMAIN_SERIAL~MATNR   IN @S_MATNR[]
      INTO TABLE @GT_RESULT.
  ENDMETHOD.
  METHOD GET_DATA_NON_SERIAL.
    SELECT MARD~MATNR,
           MARD~LGORT AS B_LAGER,
           MARD~LABST AS QTY,
           @ABAP_TRUE AS CHECK,
           SUBSTRING( MARA~PRDHA,1,5 )  AS PH1,
           SUBSTRING( MARA~PRDHA,6,5 )  AS PH2,
           SUBSTRING( MARA~PRDHA,11,8 ) AS PH3,
           MARA~MFRNR,
           ( LFA1~NAME1 && ' ' && LFA1~NAME2 && ' ' && LFA1~NAME3 && ' ' && LFA1~NAME4 ) AS NAME_ALL,
           MBEW~VERPR
      FROM MARC
      INNER JOIN MARA ON MARC~MATNR EQ MARA~MATNR
      INNER JOIN MARD ON MARC~MATNR EQ MARD~MATNR
      LEFT JOIN LFA1 ON MARA~MFRNR EQ LFA1~LIFNR
      LEFT JOIN MBEW ON MARA~MATNR EQ MBEW~MATNR AND
                        MBEW~BWKEY EQ '1000'     AND
                        MBEW~BWTAR EQ ''
      WHERE MARC~MATNR IN @S_MATNR[]
      AND MARC~SERNP EQ @SPACE
      AND MARD~LABST NE 0
      AND MARD~LVORM EQ @SPACE
      AND MARD~LGORT IN @S_LAGER[]
      AND MARD~WERKS EQ '1000'
      APPENDING CORRESPONDING FIELDS OF TABLE @GT_RESULT.
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
