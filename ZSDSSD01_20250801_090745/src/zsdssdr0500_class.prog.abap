*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0500_CLASS
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
      SELECT_BY_SO,
      SELECT_BY_DO,
      GET_DATA_INV CHANGING CT_DATA TYPE ANY TABLE,
      VALIDATION_DATA CHANGING CT_DATA TYPE ANY TABLE.
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
*    IF R_SO EQ ABAP_TRUE OR
*       R_BR EQ ABAP_TRUE.
    SELECT_BY_SO( ).
*    ELSEIF R_DO EQ ABAP_TRUE.
*      SELECT_BY_DO( ).
*    ENDIF.
  ENDMETHOD.
  METHOD SELECT_BY_SO.
    SELECT VBRK~VBELN,
           VBRK~KUNRG,
           VBRK~NETWR,
           ( ADRC~NAME1 && ' ' && ADRC~NAME2 && ' ' && ADRC~NAME3 && ' ' && ADRC~NAME4 ) AS NAME1,
           @GC_WARN AS STATUS,
           VBRK~BDR_REF,
           ADRC~STREET,
           ADRC~STR_SUPPL1,
           ADRC~STR_SUPPL2,
           ADRC~STR_SUPPL3,
           ADRC~LOCATION,
           ADRC~CITY2,
           ADRC~CITY1,
           ADRC~POST_CODE1,
           VBRK~ZTERM,
           CRMS4D_SERV_H~ZZ1_CUS_PO,
           ADR6~SMTP_ADDR,
           FITHA_PBUPL_D~J_1TPBUPL,
           FITHA_PBUPL_D_T~DESCRIPTION,
           DFKKBPTAXNUM~TAXNUM
      FROM VBRK
      LEFT  JOIN KNA1 ON VBRK~KUNRG  EQ KNA1~KUNNR
      INNER JOIN VBPA ON VBRK~VBELN  EQ VBPA~VBELN
                     AND VBPA~PARVW  EQ 'RE'
      INNER JOIN ADRC ON VBPA~ADRNR  EQ ADRC~ADDRNUMBER
                     AND ADRC~NATION EQ @SPACE
      INNER JOIN CRMS4D_SERV_H ON VBRK~BDR_REF EQ CRMS4D_SERV_H~OBJECT_ID
      LEFT JOIN  ADR6 ON VBPA~ADRNR      EQ ADR6~ADDRNUMBER AND
                         ADR6~PERSNUMBER EQ @SPACE          AND
                         ADR6~FLGDEFAULT EQ 'X'
      LEFT JOIN FITHA_PBUPL_D ON VBPA~KUNNR                   EQ FITHA_PBUPL_D~KUNNR AND
                                 FITHA_PBUPL_D~DEFAULT_BRANCH EQ 'X'
      LEFT JOIN  FITHA_PBUPL_D_T  ON  FITHA_PBUPL_D~J_1TPBUPL EQ FITHA_PBUPL_D_T~J_1TPBUPL AND
                                      FITHA_PBUPL_D~KUNNR     EQ FITHA_PBUPL_D_T~KUNNR
     LEFT JOIN DFKKBPTAXNUM ON VBPA~KUNNR           EQ DFKKBPTAXNUM~PARTNER AND
                               DFKKBPTAXNUM~TAXTYPE EQ 'TH3'
      WHERE VBRK~VBELN IN @S_VBELN[]
        AND VBRK~FKART EQ 'ZV1B'
        AND NOT EXISTS (
              SELECT VGBEL
                FROM VBRP
                WHERE VBRP~VGBEL EQ VBRK~VBELN )
      INTO TABLE @GT_RESULT.
  ENDMETHOD.
  METHOD SELECT_BY_DO.
    SELECT LIKP~VBELN,
           LIKP~KUNNR,
           ( KNA1~NAME1 && ' ' && KNA1~NAME2 && ' ' && KNA1~NAME3 && ' ' && KNA1~NAME4 ) AS NAME1,
           @GC_WARN AS STATUS
      FROM LIKP
      LEFT JOIN KNA1 ON LIKP~KUNNR EQ KNA1~KUNNR
      INTO CORRESPONDING FIELDS OF TABLE @GT_RESULT
      WHERE LIKP~VBELN IN @S_VBELN[].
  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.

    DATA : BEGIN OF LS_INV,
             VBELN TYPE VBAK-VBELN,
           END OF LS_INV.
    DATA : LT_INV  LIKE TABLE OF LS_INV,
           LT_MIGO LIKE TABLE OF LS_INV.

*    GET_DATA_INV( CHANGING CT_DATA = LT_INV ).
    VALIDATION_DATA( CHANGING CT_DATA = LT_MIGO ).

    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
*      READ TABLE LT_INV
*      WITH KEY VBELN = <LFS_RESULT>-VBELN TRANSPORTING NO FIELDS.
*      IF SY-SUBRC EQ 0.
*        <LFS_RESULT>-STATUS  = GC_ERRO.
*        <LFS_RESULT>-MESSAGE = TEXT-E01.
*        CONTINUE.
*      ENDIF.

      READ TABLE LT_MIGO
      WITH KEY VBELN = <LFS_RESULT>-VBELN TRANSPORTING NO FIELDS.
      IF SY-SUBRC EQ 0.
        <LFS_RESULT>-STATUS  = GC_ERRO.
        <LFS_RESULT>-MESSAGE = TEXT-E02.
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

*      IF R_DO EQ ABAP_TRUE AND
*         LV_RUNNING EQ 6.
*        CONTINUE.
*      ENDIF.

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

    DATA : LT_BILLINGDATAIN   TYPE TABLE OF BAPIVBRK,
           LT_CONDITIONDATAIN TYPE TABLE OF	BAPIKOMV,
           LT_CCARDDATAIN     TYPE TABLE OF	BAPICCARD_VF,
           LT_TEXTDATAIN      TYPE TABLE OF BAPIKOMFKTX,
           LT_ERRORS          TYPE TABLE OF BAPIVBRKERRORS,
           LT_RETURN          TYPE TABLE OF BAPIRET1,
           LT_SUCCESS         TYPE TABLE OF	BAPIVBRKSUCCESS.

    DATA : LT_SUCCESS_TMP     TYPE TABLE OF	BAPIVBRKSUCCESS.

    DATA : LS_BILLINGDATAIN   TYPE BAPIVBRK.

    DATA : LS_CREATORDATAIN	TYPE BAPICREATORDATA,
           LS_TESTRUN	      TYPE BAPIVBRKTESTRUN-TESTRUN,
           LS_POSTING	      TYPE POSTING_TYPE_CT.

    SORT GT_RESULT BY VBELN.
    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>) WHERE CHECK  EQ ABAP_TRUE
                                                           AND STATUS EQ GC_WARN.
      LS_BILLINGDATAIN-REF_DOC         = <LFS_DATA>-VBELN.
      LS_BILLINGDATAIN-REF_DOC_CA_LONG = 'EBDR'.
      LS_BILLINGDATAIN-BILL_DATE       = P_DATUM.

      APPEND LS_BILLINGDATAIN TO LT_BILLINGDATAIN.
    ENDLOOP.

    CLEAR : GS_RESULT.
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
    READ TABLE LT_ERRORS INTO DATA(LS_ERROR)
    WITH KEY TYPE = 'E'.
    IF SY-SUBRC EQ 0.
      GS_RESULT-STATUS  = GC_ERRO.
      GS_RESULT-MESSAGE = LS_ERROR-MESSAGE.
    ENDIF.

    READ TABLE LT_ERRORS INTO LS_ERROR
    WITH KEY TYPE = 'A'.
    IF SY-SUBRC EQ 0.
      GS_RESULT-STATUS  = GC_ERRO.
      GS_RESULT-MESSAGE = LS_ERROR-MESSAGE.
    ENDIF.

    IF GS_RESULT-STATUS IS INITIAL.
      READ TABLE LT_SUCCESS_TMP INTO DATA(LS_SUCC) INDEX 1.
      IF SY-SUBRC EQ 0.
        GS_RESULT-STATUS  = GC_SUCS.
        GS_RESULT-MESSAGE = LS_SUCC-BILL_DOC.
      ELSE.
        GS_RESULT-STATUS  = GC_ERRO.
        GS_RESULT-MESSAGE = TEXT-E03.
      ENDIF.
    ENDIF.

    MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING STATUS MESSAGE
                                    WHERE CHECK EQ ABAP_TRUE.

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD GET_DATA_INV.
    DATA : BEGIN OF LS_INV_CHK,
             VBELN TYPE VBAK-VBELN,
           END OF LS_INV_CHK.
    DATA : LT_INV_CHK LIKE HASHED TABLE OF LS_INV_CHK WITH UNIQUE KEY VBELN.
    LT_INV_CHK =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).

    SELECT VBRP~VGBEL
      FROM @LT_INV_CHK AS A
      INNER JOIN VBRP ON A~VBELN EQ VBRP~VGBEL
      INTO TABLE @CT_DATA.

  ENDMETHOD.
  METHOD VALIDATION_DATA.
    DATA : BEGIN OF LS_INV_CHK,
             VBELN TYPE VBAK-VBELN,
           END OF LS_INV_CHK.
    DATA : LT_INV_CHK LIKE HASHED TABLE OF LS_INV_CHK WITH UNIQUE KEY VBELN.

    DATA : LT_DATA_INV LIKE TABLE OF LS_INV_CHK.

    LT_INV_CHK =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).

*    DATA : BEGIN OF LS_DATA,
*             OBJKEY_B_SEL TYPE CRMD_BRELVONAI-OBJKEY_B,
*           END OF LS_DATA.
*    DATA LT_DATA LIKE TABLE OF LS_DATA.

    SELECT VBRP~VBELN,
           VBRP~POSNR
      FROM @LT_INV_CHK AS A
      INNER JOIN VBRP ON A~VBELN EQ VBRP~VBELN AND
                         SUBSTRING( VBRP~MATKL,1,2 ) EQ 'SP'
      INTO TABLE @DATA(LT_DATA).

    SELECT VBRP~VBELN,
           VBRP~POSNR
      FROM @LT_INV_CHK AS A
      INNER JOIN VBRP ON A~VBELN EQ VBRP~VBELN AND
                         SUBSTRING( VBRP~MATKL,1,2 ) EQ 'SP'
      INNER JOIN CRMS4D_SERV_I ON VBRP~VGBEL EQ CRMS4D_SERV_I~OBJECT_ID AND
                                  VBRP~VGPOS EQ CRMS4D_SERV_I~NUMBER_INT
      INNER JOIN CRMD_BRELVONAI AS B ON CRMS4D_SERV_I~ITEM_GUID_CHAR EQ B~OBJKEY_A AND
                                        B~OBJTYPE_B                  EQ 'BUS2000142'
      INNER JOIN CRMD_BRELVONAI AS C ON B~OBJKEY_B  EQ C~OBJKEY_A AND
                                        C~OBJTYPE_B EQ 'BUS2017'  AND
                                        C~OBJTYPE_A EQ 'BUS2000117'
      INNER JOIN MSEG ON SUBSTRING( C~OBJKEY_B,1,10 ) EQ MSEG~MBLNR AND
                         SUBSTRING( C~OBJKEY_B,11,4 ) EQ MSEG~MJAHR AND
                         C~POSNO                      EQ MSEG~ZEILE
*      WHERE EXISTS
*       ( SELECT VBRP~VBELN,
*                VBRP~POSNR
*           FROM VBRP
*           INNER JOIN CRMS4D_SERV_I ON VBRP~VGBEL EQ CRMS4D_SERV_I~OBJECT_ID AND
*                                       VBRP~VGPOS EQ CRMS4D_SERV_I~NUMBER_INT
*           INNER JOIN CRMD_BRELVONAI AS B ON CRMS4D_SERV_I~ITEM_GUID_CHAR EQ B~OBJKEY_A AND
*                                             B~OBJTYPE_B                  EQ 'BUS2000142'
*           INNER JOIN CRMD_BRELVONAI AS C ON B~OBJKEY_B  EQ C~OBJKEY_A AND
*                                             C~OBJTYPE_B EQ 'BUS2017'  AND
*                                             C~OBJTYPE_A EQ 'BUS2000117'
*           INNER JOIN MSEG ON SUBSTRING( C~OBJKEY_B,1,10 ) EQ MSEG~MBLNR AND
*                              SUBSTRING( C~OBJKEY_B,11,4 ) EQ MSEG~MJAHR AND
*                              C~POSNO                      EQ MSEG~ZEILE
*           WHERE VBRP~VBELN EQ A~VBELN
*             AND SUBSTRING( VBRP~MATKL,1,2 ) EQ 'SP'
*       )
      INTO TABLE @DATA(LT_DATA_CHECK).

    LOOP AT LT_DATA INTO DATA(LS_DATA).
      READ TABLE LT_DATA_CHECK INTO DATA(LS_DATA_CHECK)
      WITH KEY VBELN = LS_DATA-VBELN
               POSNR = LS_DATA-POSNR.
      IF SY-SUBRC NE 0.
        APPEND LS_DATA-VBELN TO LT_DATA_INV.
      ENDIF.
    ENDLOOP.
    SORT LT_DATA_INV.
    DELETE ADJACENT DUPLICATES FROM LT_DATA_INV.
    CT_DATA = LT_DATA_INV.
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
