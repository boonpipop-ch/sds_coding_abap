*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0640_CLASS
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
      CANCEL_BILLING IMPORTING I_DATA TYPE VBAK-VBELN,
      CREATE_INV IMPORTING I_DATA TYPE VBAK-VBELN,
      UPDATE_DATA IMPORTING I_REF TYPE CHAR10
                            I_DOC TYPE CHAR10
                            I_STA TYPE FLAG.
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

    DELETE S_VBELN[] WHERE LOW EQ '*'.

    IF S_VBELN[] IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT VBRK~VBELN,
                    VBRK~KUNRG,
                    VBRK~NETWR,
                    ( KNA1~NAME1 && ' ' && KNA1~NAME2 && ' ' && KNA1~NAME3 && ' ' && KNA1~NAME4 ) AS NAME1,
                    VBRP~VGBEL,
                    VBRP~VGTYP,
                    @GC_WARN AS STATUS,
                    ZSDSSDT026~VBELN
      FROM VBRK
      INNER JOIN VBRP      ON VBRK~VBELN EQ VBRP~VBELN
      LEFT JOIN KNA1       ON VBRK~KUNRG EQ KNA1~KUNNR
      LEFT JOIN ZSDSSDT026 ON VBRK~VBELN       EQ ZSDSSDT026~REFDC AND
                              ZSDSSDT026~STATU EQ 'A'
      INTO TABLE @GT_RESULT
      WHERE VBRK~VBELN IN @S_VBELN[]
        AND NOT EXISTS ( SELECT ZSDSSDT026~REFDC
                           FROM ZSDSSDT026
                          WHERE ZSDSSDT026~REFDC EQ VBRK~VBELN
                            AND ZSDSSDT026~STATU EQ 'B' ).
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
  METHOD SAVE.

    DATA : BEGIN OF LS_DATA,
             VBELN  TYPE VBAK-VBELN,
             STATUS TYPE GY_RESULT-STATUS,
             CNDOC  TYPE GY_RESULT-CNDOC,
           END OF LS_DATA.
    DATA : LT_DATA LIKE HASHED TABLE OF LS_DATA WITH UNIQUE KEY VBELN.

    LT_DATA =  CORRESPONDING #( GT_RESULT DISCARDING DUPLICATES ).

    LOOP AT LT_DATA INTO LS_DATA WHERE STATUS NE GC_SUCS.
      IF LS_DATA-CNDOC IS INITIAL.
        CANCEL_BILLING( LS_DATA-VBELN ).
      ENDIF.
      CREATE_INV( LS_DATA-VBELN ).
    ENDLOOP.

  ENDMETHOD.
  METHOD CANCEL_BILLING.
    DATA : LV_BILLINGDOCUMENT TYPE BAPIVBRKSUCCESS-BILL_DOC,
           LV_TESTRUN         TYPE BAPIVBRKTESTRUN-TESTRUN,
           LV_NO_COMMIT       TYPE BAPI_NCOMT,
           LV_BILLINGDATE     TYPE BF_DATM1EB.

    DATA : LT_RETURN  TYPE TABLE OF BAPIRETURN1 WITH EMPTY KEY,
           LT_SUCCESS TYPE TABLE OF BAPIVBRKSUCCESS.

    DATA : LS_RESULT TYPE GY_RESULT.

    LV_BILLINGDOCUMENT = I_DATA.
    LV_BILLINGDATE     = P_CNDAT.

    CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
      EXPORTING
        BILLINGDOCUMENT = LV_BILLINGDOCUMENT
        TESTRUN         = LV_TESTRUN
        NO_COMMIT       = LV_NO_COMMIT
        BILLINGDATE     = LV_BILLINGDATE
      TABLES
        RETURN          = LT_RETURN
        SUCCESS         = LT_SUCCESS.

    READ TABLE LT_SUCCESS INTO DATA(LS_SUCC) INDEX 1.
    IF SY-SUBRC EQ 0.
      LS_RESULT-STATUS  = GC_SUCS.
      LS_RESULT-MESSAGE = LS_SUCC-BILL_DOC.
      LS_RESULT-CNDOC   = LS_SUCC-BILL_DOC.
      UPDATE_DATA( I_REF = I_DATA
                   I_DOC = LS_SUCC-BILL_DOC
                   I_STA = 'A' ).
      COMMIT WORK AND WAIT.
    ELSE.
      SELECT SINGLE MESSAGE
        FROM @LT_RETURN AS A
        WHERE TYPE EQ @GC_E
           OR TYPE EQ @GC_A
        INTO @LS_RESULT-MESSAGE.
      IF SY-SUBRC EQ 0.
        LS_RESULT-STATUS  = GC_ERRO.
      ELSE.
        LS_RESULT-STATUS  = GC_ERRO.
        LS_RESULT-MESSAGE = TEXT-E01.
      ENDIF.
    ENDIF.

    MODIFY GT_RESULT FROM LS_RESULT TRANSPORTING STATUS MESSAGE CNDOC
                                    WHERE VBELN EQ I_DATA.

  ENDMETHOD.
  METHOD CREATE_INV.

    DATA : LT_BILLINGDATAIN   TYPE TABLE OF BAPIVBRK,
           LT_CONDITIONDATAIN TYPE TABLE OF	BAPIKOMV,
           LT_CCARDDATAIN     TYPE TABLE OF	BAPICCARD_VF,
           LT_TEXTDATAIN      TYPE TABLE OF BAPIKOMFKTX,
           LT_ERRORS          TYPE TABLE OF BAPIVBRKERRORS WITH EMPTY KEY,
           LT_RETURN          TYPE TABLE OF BAPIRET1,
           LT_SUCCESS         TYPE TABLE OF	BAPIVBRKSUCCESS.

    DATA : LT_SUCCESS_TMP     TYPE TABLE OF	BAPIVBRKSUCCESS.

    DATA : LS_BILLINGDATAIN   TYPE BAPIVBRK.

    DATA : LS_CREATORDATAIN	TYPE BAPICREATORDATA,
           LS_TESTRUN	      TYPE BAPIVBRKTESTRUN-TESTRUN,
           LS_POSTING	      TYPE POSTING_TYPE_CT.

    LOOP AT GT_RESULT INTO DATA(LS_RESULT) WHERE VBELN  EQ I_DATA       AND
                                                 CNDOC  IS NOT INITIAL  AND
                                                 INVNO  IS INITIAL.
      LS_BILLINGDATAIN-REF_DOC    = LS_RESULT-REFDC.

      DATA(LV_LEN) = STRLEN( LS_RESULT-VGTYP ).

      IF LV_LEN GT 1.
        LS_BILLINGDATAIN-REF_DOC_CA_LONG = LS_RESULT-VGTYP.
      ELSE.
        LS_BILLINGDATAIN-REF_DOC_CA = LS_RESULT-VGTYP.
      ENDIF.

      LS_BILLINGDATAIN-BILL_DATE  = P_DATUM.
      APPEND LS_BILLINGDATAIN TO LT_BILLINGDATAIN.
    ENDLOOP.

    IF LT_BILLINGDATAIN IS NOT INITIAL.
      COMMIT WORK AND WAIT.
      WAIT UP TO 1 SECONDS.
      CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
        EXPORTING
          CREATORDATAIN   = LS_CREATORDATAIN
          TESTRUN         = LS_TESTRUN
          POSTING         = LS_POSTING
        TABLES
          BILLINGDATAIN   = LT_BILLINGDATAIN
          CONDITIONDATAIN = LT_CONDITIONDATAIN
          CCARDDATAIN     = LT_CCARDDATAIN
          TEXTDATAIN      = LT_TEXTDATAIN
          ERRORS          = LT_ERRORS
          RETURN          = LT_RETURN
          SUCCESS         = LT_SUCCESS_TMP.

      SELECT SINGLE MESSAGE
        FROM @LT_ERRORS AS A
        WHERE TYPE EQ @GC_E
           OR TYPE EQ @GC_A
        INTO @LS_RESULT-MESSAGE.
      IF SY-SUBRC EQ 0.
        LS_RESULT-STATUS  = GC_ERRO.
      ELSE.
        READ TABLE LT_SUCCESS_TMP INTO DATA(LS_SUCC) INDEX 1.
        IF SY-SUBRC EQ 0.
          LS_RESULT-STATUS  = GC_SUCS.
          LS_RESULT-MESSAGE = LS_SUCC-BILL_DOC.
          LS_RESULT-INVNO   = LS_SUCC-BILL_DOC.
          UPDATE_DATA( I_REF = I_DATA
                       I_DOC = LS_SUCC-BILL_DOC
                       I_STA = 'B' ).
          COMMIT WORK AND WAIT.
        ELSE.
          LS_RESULT-STATUS  = GC_ERRO.
          LS_RESULT-MESSAGE = TEXT-E02.
        ENDIF.
      ENDIF.

      MODIFY GT_RESULT FROM LS_RESULT TRANSPORTING STATUS MESSAGE INVNO
                                      WHERE VBELN EQ I_DATA.
    ENDIF.

  ENDMETHOD.
  METHOD UPDATE_DATA.
    DATA : LS_ZSDSSDT026 TYPE ZSDSSDT026.

    LS_ZSDSSDT026-VBELN = I_DOC.
    LS_ZSDSSDT026-REFDC = I_REF.
    LS_ZSDSSDT026-STATU = I_STA.
    LS_ZSDSSDT026-ERNAM = SY-UNAME.
    LS_ZSDSSDT026-ERDAT = SY-DATUM.
    LS_ZSDSSDT026-ERZET = SY-UZEIT.
    LS_ZSDSSDT026-AENAM = SY-UNAME.
    LS_ZSDSSDT026-AEDAT = SY-DATUM.
    LS_ZSDSSDT026-AEZET = SY-UZEIT.

    MODIFY ZSDSSDT026 FROM LS_ZSDSSDT026.
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
