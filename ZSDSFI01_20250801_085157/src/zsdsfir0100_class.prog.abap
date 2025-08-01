*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0100_CLASS
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
      PRINT,
      GET_DETAIL IMPORTING I_DATA   LIKE GT_RESULT
                 RETURNING VALUE(R) TYPE GTY_DETAIL,
      SET_PRINT_DATA_TO_READ IMPORTING I_FORM_NAME TYPE RS38L_FNAM
                             RETURNING VALUE(R)    TYPE LBBIL_PRINT_DATA_TO_READ,
      GET_BILLING_DETAIL IMPORTING I_DATA   TYPE LBBIL_PRINT_DATA_TO_READ
                         RETURNING VALUE(R) TYPE LBBIL_INVOICE,
      GET_BILLING_DETAIL_BY_FI IMPORTING IT_DETAIL TYPE GTY_DETAIL
                               RETURNING VALUE(R)  TYPE LBBIL_INVOICE,
      CALL_SMART_FORM IMPORTING I_DATA     TYPE LBBIL_INVOICE
                                I_ALL_LINE TYPE I
                                I_INDEX    TYPE I,
      SEND_PDF IMPORTING I_BIL_INVOICE TYPE LBBIL_INVOICE
                         I_FM_NAME     TYPE RS38L_FNAM.

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
*    SELECT VBRK~VBELN,
*           VBRK~KUNRG,
*           ( KNA1~NAME1 && ' ' && KNA1~NAME2 ) AS NAME1,
*           VBRK~NETWR
*      FROM VBRK
*      INNER JOIN KNA1 ON VBRK~KUNRG EQ KNA1~KUNNR
*      WHERE VBRK~VBELN IN @S_VBELN
*        AND VBRK~KUNRG IN @S_KUNRG
*        AND VBRK~FKDAT IN @S_FKDAT
*        AND VBRK~ERDAT IN @S_ERDAT
*       INTO TABLE @GT_RESULT.

    SELECT ACDOCA~RLDNR,
           ACDOCA~RBUKRS,
           ACDOCA~DOCLN,
           ACDOCA~BELNR,
           ACDOCA~GJAHR,
           ACDOCA~BUDAT,
           ACDOCA~BLDAT,
           ACDOCA~MWSKZ,
           ACDOCA~AUGBL,
           ACDOCA~AUGGJ,
           ACDOCA~TSL,
           ACDOCA~RTCUR,
           ACDOCA~KUNNR,
           ACDOCA~NETDT,
           ( KNA1~NAME1 && ' ' && KNA1~NAME2 ) AS NAME1,
           KNA1~ADRNR,
           BSEG~BUPLA
      FROM ACDOCA
      INNER JOIN BKPF ON ACDOCA~RBUKRS EQ BKPF~BUKRS AND
                         ACDOCA~BELNR  EQ BKPF~BELNR AND
                         ACDOCA~GJAHR  EQ BKPF~GJAHR
      INNER JOIN BSEG ON ACDOCA~RBUKRS EQ BSEG~BUKRS AND
                         ACDOCA~BELNR  EQ BSEG~BELNR AND
                         ACDOCA~GJAHR  EQ BSEG~GJAHR AND
                         ACDOCA~BUZEI  EQ BSEG~BUZEI
      INNER JOIN KNA1 ON ACDOCA~KUNNR  EQ KNA1~KUNNR
      WHERE ACDOCA~RLDNR  EQ @GC_CON-0L
        AND ACDOCA~RBUKRS IN @S_BUKRS
        AND ACDOCA~GJAHR  IN @S_GJAHR
        AND ACDOCA~BLART  IN @S_BLART
        AND ACDOCA~BELNR  IN @S_BELNR
        AND ACDOCA~KUNNR  IN @S_KUNNR
        AND ACDOCA~BUDAT  IN @S_BUDAT
        AND ACDOCA~KOART  EQ @GC_CON-D
*        AND ACDOCA~DRCRK  EQ @GC_CON-S
        AND BKPF~XBLNR    IN @S_XBLNR
        AND ( ACDOCA~DRCRK EQ @GC_CON-S OR
              ACDOCA~DRCRK EQ @GC_CON-H AND ACDOCA~UMSKZ EQ @GC_A )
      INTO TABLE @GT_RESULT.

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

    CONSTANTS : BEGIN OF LC_CON,
                  CHK_FILED TYPE C LENGTH 5 VALUE 'CHECK',
                  CHK_NAME  TYPE C LENGTH 3 VALUE 'CHK',
                END OF LC_CON.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = LC_CON-CHK_FILED.
    LS_FCAT-SELTEXT_S   = LC_CON-CHK_NAME.
    LS_FCAT-SELTEXT_M   = LC_CON-CHK_NAME.
    LS_FCAT-SELTEXT_L   = LC_CON-CHK_NAME.
    LS_FCAT-CHECKBOX    = ABAP_TRUE.
    LS_FCAT-INPUT       = ABAP_TRUE.
    LS_FCAT-EDIT        = ABAP_TRUE.
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
  METHOD PRINT.
    DATA: LS_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ,
          LS_BIL_INVOICE        TYPE LBBIL_INVOICE.

    DATA: LT_TMP LIKE GT_RESULT.

    DATA: LV_ALL_LINE TYPE I,
          LV_INDEX    TYPE I.


    DATA : LS_DETAIL TYPE GY_DETAIL,
           LT_DETAIL TYPE GTY_DETAIL.

    LT_TMP[]    = GT_RESULT[].

    SORT LT_TMP BY CHECK ASCENDING.
    DELETE LT_TMP WHERE CHECK EQ SPACE.

    DESCRIBE TABLE LT_TMP LINES LV_ALL_LINE.

    LT_DETAIL = GET_DETAIL( LT_TMP ).

    LS_PRINT_DATA_TO_READ = SET_PRINT_DATA_TO_READ( GC_FORM_NAME ).

    LOOP AT LT_TMP INTO GS_RESULT.
      ADD 1 TO LV_INDEX.
      CLEAR : LS_BIL_INVOICE.
      LS_BIL_INVOICE = GET_BILLING_DETAIL_BY_FI( LT_DETAIL ).
      CALL_SMART_FORM( I_DATA     = LS_BIL_INVOICE
                       I_ALL_LINE = LV_ALL_LINE
                       I_INDEX    = LV_INDEX ).
    ENDLOOP.
  ENDMETHOD.
  METHOD GET_DETAIL.
    CONSTANTS : BEGIN OF LC_CON,
                  OL TYPE ACDOCA-RLDNR VALUE '0L',
                END OF LC_CON.

    SELECT ACDOCA~BELNR,
           ACDOCA~GJAHR,
           ACDOCA~MWSKZ,
           ACDOCA~AUGBL,
           ACDOCA~AUGGJ,
           ACDOCA~TSL,
           ACDOCA~RTCUR,
           ACDOCA~SGTXT,
           ACDOCA~LINETYPE
      FROM @I_DATA AS A
      INNER JOIN ACDOCA ON A~RLDNR  EQ ACDOCA~RLDNR  AND
                           A~RBUKRS EQ ACDOCA~RBUKRS AND
                           A~GJAHR  EQ ACDOCA~GJAHR  AND
                           A~BELNR  EQ ACDOCA~BELNR
      WHERE ACDOCA~KOART NE @GC_CON-D
        AND ACDOCA~RLDNR EQ @LC_CON-OL
      INTO TABLE @R.

    SELECT B~BELNR,
           B~GJAHR,
           B~MWSKZ,
           B~AUGBL,
           B~AUGGJ,
           B~TSL,
           B~RTCUR,
           B~SGTXT,
           B~LINETYPE
  FROM @I_DATA AS C
  INNER JOIN ACDOCA AS B ON C~RLDNR  EQ B~RLDNR  AND
                            C~RBUKRS EQ B~RBUKRS AND
                            C~GJAHR  EQ B~GJAHR  AND
                            C~BELNR  EQ B~BELNR
  WHERE B~KOART  EQ @GC_CON-D
    AND B~UMSKZ  EQ @GC_CON-S
    AND B~RLDNR  EQ @LC_CON-OL
  APPENDING TABLE @R.
    IF SY-SUBRC NE 0.
      SELECT B~BELNR,
             B~GJAHR,
             B~MWSKZ,
             B~AUGBL,
             B~AUGGJ,
             B~TSL,
             B~RTCUR,
             B~SGTXT,
             B~LINETYPE
    FROM @I_DATA AS C
    INNER JOIN ACDOCA AS B ON C~RLDNR  EQ B~RLDNR  AND
                              C~RBUKRS EQ B~RBUKRS AND
                              C~GJAHR  EQ B~GJAHR  AND
                              C~BELNR  EQ B~BELNR
    WHERE B~KOART  EQ @GC_CON-D
      AND B~DRCRK  EQ @GC_CON-H
      AND B~RLDNR  EQ @LC_CON-OL
  APPENDING TABLE @R.
    ENDIF.

    IF R IS NOT INITIAL.
      DELETE R WHERE SGTXT    IS INITIAL
                 AND LINETYPE NE GC_CON-05100.
    ENDIF.
  ENDMETHOD.
  METHOD SET_PRINT_DATA_TO_READ.
    FIELD-SYMBOLS: <FS_PRINT_DATA_TO_READ> TYPE XFELD.
    DATA: LT_FIELDLIST TYPE TSFFIELDS.

* set print data requirements
    DO.
      ASSIGN COMPONENT SY-INDEX OF STRUCTURE
                       R TO <FS_PRINT_DATA_TO_READ>.
      IF SY-SUBRC <> 0. EXIT. ENDIF.
      <FS_PRINT_DATA_TO_READ> = 'X'.
    ENDDO.

    CALL FUNCTION 'SSF_FIELD_LIST'
      EXPORTING
        FORMNAME           = I_FORM_NAME
*       VARIANT            = ' '
      IMPORTING
        FIELDLIST          = LT_FIELDLIST
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.

    ENDIF.
  ENDMETHOD.
  METHOD GET_BILLING_DETAIL.
*    DATA : LV_OBJKY TYPE NAST-OBJKY.
*
*    CONSTANTS LC_AG TYPE NAST-PARVW VALUE 'AG'.
*
*    LV_OBJKY = GS_RESULT-VBELN.
*
*    CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
*      EXPORTING
*        IF_BIL_NUMBER         = LV_OBJKY
*        IF_PARVW              = LC_AG
*        IF_PARNR              = GS_RESULT-KUNRR
*        IF_LANGUAGE           = SY-LANGU
*        IS_PRINT_DATA_TO_READ = I_DATA
*      IMPORTING
*        ES_BIL_INVOICE        = R
*      EXCEPTIONS
*        RECORDS_NOT_FOUND     = 1
*        RECORDS_NOT_REQUESTED = 2
*        OTHERS                = 3.
*    IF SY-SUBRC <> 0.
*
*    ENDIF.
  ENDMETHOD.
  METHOD GET_BILLING_DETAIL_BY_FI.

    DATA : LS_PART_ADD LIKE LINE OF R-HD_PART_ADD,
           LS_ADR      LIKE LINE OF R-HD_ADR,
           LS_PRICE    LIKE LINE OF R-IT_PRICE,
           LS_GEN      LIKE LINE OF R-IT_GEN,
           LS_KOND     LIKE LINE OF R-HD_KOND.

    DATA : LS_DETAIL LIKE LINE OF IT_DETAIL.

    DATA : LS_TMP LIKE LINE OF IT_DETAIL.

    DATA : LV_LINE TYPE I.

    DATA : LV_LEN TYPE I.

    R-HD_GEN-BIL_NUMBER = GS_RESULT-BELNR.
    R-HD_GEN-BIL_DATE   = GS_RESULT-BUDAT.
    R-HD_GEN-BIL_EDATE  = GS_RESULT-BUDAT.
    R-HD_GEN-PAYER      = GS_RESULT-KUNNR.
    R-HD_GEN-TERMS_PAYM = GC_CON-ZZZZ.
    R-HD_GEN-VAL_DATE   = GS_RESULT-NETDT.
    R-HD_GEN-BUPLA      = GS_RESULT-BUPLA.

    LS_PART_ADD-PARTN_NUMB = SPACE.
    APPEND LS_PART_ADD TO R-HD_PART_ADD.

    LS_ADR-PARTN_ROLE = GC_CON-RE.
    LS_ADR-ADDR_NO    = GS_RESULT-ADRNR.
    APPEND LS_ADR TO R-HD_ADR.

    LOOP AT IT_DETAIL INTO LS_DETAIL WHERE BELNR EQ GS_RESULT-BELNR AND
                                           GJAHR EQ GS_RESULT-GJAHR AND
                                           LINETYPE NE GC_CON-05100.

      ADD 1 TO LV_LINE.
      LS_GEN-BIL_NUMBER = GS_RESULT-BELNR.
      LS_GEN-ITM_NUMBER = LV_LINE.
      LS_GEN-MATERIAL   = SPACE.
      LV_LEN = STRLEN( LS_DETAIL-SGTXT ).
      IF LV_LEN GT 40.
        LV_LEN = LV_LEN - 40.
        LS_GEN-MATERIAL   = 'FIDOC'.
        LS_GEN-MAT_ENTRD  = LS_DETAIL-SGTXT+40(LV_LEN).
        LS_GEN-SHORT_TEXT = LS_DETAIL-SGTXT.
      ELSE.
        LS_GEN-SHORT_TEXT = LS_DETAIL-SGTXT.
      ENDIF.
*      FIND '/' IN LS_DETAIL-SGTXT.
*      IF SY-SUBRC = 0.
*        SPLIT LS_DETAIL-SGTXT AT '/'
*          INTO LS_GEN-SHORT_TEXT
*               LS_GEN-MAT_ENTRD.
*      ELSE.
*        LS_GEN-SHORT_TEXT = LS_DETAIL-SGTXT+0(40).
*      ENDIF.

*      LS_GEN-SHORT_TEXT = LS_DETAIL-SGTXT.
      LS_GEN-FKIMG      = 1.
      LS_GEN-SALES_UNIT = SPACE.
      APPEND LS_GEN TO R-IT_GEN.


      LS_PRICE-BIL_NUMBER = GS_RESULT-BELNR.
      LS_PRICE-ITM_NUMBER = LV_LINE.
      LS_PRICE-KZWI1 = ABS( LS_DETAIL-TSL ).
      READ TABLE IT_DETAIL INTO LS_TMP
      WITH KEY BELNR    = GS_RESULT-BELNR
               GJAHR    = GS_RESULT-GJAHR
               LINETYPE = GC_CON-05100.
      IF SY-SUBRC EQ 0.
*        LS_PRICE-KZWI4 = abs( LS_TMP-TSL ).
        LS_KOND-KWERT  = ABS( LS_TMP-TSL ).
      ENDIF.
      LS_PRICE-NETWR = ABS( LS_DETAIL-TSL ).
      APPEND LS_PRICE TO R-IT_PRICE.
      CLEAR : LS_PRICE,LS_GEN.
    ENDLOOP.

    LS_KOND-BIL_NUMBER = GS_RESULT-BELNR.
    APPEND LS_KOND TO R-HD_KOND.

  ENDMETHOD.
  METHOD CALL_SMART_FORM.

    DATA : LV_FM_NAME TYPE RS38L_FNAM,
           LV_REPEAT  TYPE C LENGTH 1.

    DATA : LS_NAST       TYPE NAST,
           LS_ARC_PARAMS TYPE ARC_PARAMS,
           LS_TOA_DARA   TYPE TOA_DARA.

    DATA : LS_CONTROL_PARAM  TYPE SSFCTRLOP,
           LS_COMPOSER_PARAM TYPE SSFCOMPOP,
           LS_RECIPIENT      TYPE SWOTOBJID,
           LS_SENDER         TYPE SWOTOBJID,
           LS_ADDR_KEY       TYPE ADDR_KEY,
           LS_DLV_LAND       TYPE VBRK-LAND1,
           LS_JOB_INFO       TYPE SSFCRESCL.

    DATA : LV_NATION TYPE ADRC-NATION.

    IF I_INDEX = 1.               "fisrt call
      LS_CONTROL_PARAM-NO_OPEN   = SPACE .
      LS_CONTROL_PARAM-NO_CLOSE  = ABAP_TRUE .
    ELSEIF I_INDEX = I_ALL_LINE.    "last call
      LS_CONTROL_PARAM-NO_OPEN   = ABAP_TRUE .
      LS_CONTROL_PARAM-NO_CLOSE  = SPACE .
    ELSE.                          "other calls
      LS_CONTROL_PARAM-NO_OPEN   = ABAP_TRUE .
      LS_CONTROL_PARAM-NO_CLOSE  = ABAP_TRUE .
    ENDIF.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = GC_FORM_NAME
*       variant            = ' '
*       direct_call        = ' '
      IMPORTING
        FM_NAME            = LV_FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.

    ENDIF.

    IF R_EN EQ ABAP_TRUE.
      LV_NATION = GC_I.
    ELSE.
      CLEAR : LV_NATION.
    ENDIF.
*     call smartform invoice
    CALL FUNCTION LV_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = LS_TOA_DARA
        ARCHIVE_PARAMETERS = LS_ARC_PARAMS
        CONTROL_PARAMETERS = LS_CONTROL_PARAM
*       mail_appl_obj      =
        MAIL_RECIPIENT     = LS_RECIPIENT
        MAIL_SENDER        = LS_SENDER
        OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
        USER_SETTINGS      = SPACE
        IS_BIL_INVOICE     = I_DATA
        IS_NAST            = LS_NAST
        IS_REPEAT          = LV_REPEAT
        I_NATION           = LV_NATION
      IMPORTING
        JOB_OUTPUT_INFO    = LS_JOB_INFO
*       document_output_info =
*       job_output_options =
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.
    IF SY-SUBRC <> 0.
*       error handling
    ELSE.
*      SEND_PDF( I_BIL_INVOICE = I_DATA
*                I_FM_NAME     = LV_FM_NAME ).
    ENDIF.

    IF I_ALL_LINE = 1.
      CALL FUNCTION 'SSF_CLOSE'.
    ENDIF.

  ENDMETHOD.
  METHOD SEND_PDF.
    DATA: LS_CONTROL_PARAM  TYPE SSFCTRLOP,
          LS_COMPOSER_PARAM TYPE SSFCOMPOP,
          LS_RECIPIENT      TYPE SWOTOBJID,
          LS_SENDER         TYPE SWOTOBJID,
          LF_FORMNAME       TYPE TDSFNAME,
          LS_DLV_LAND       TYPE VBRK-LAND1,
          LS_JOB_INFO       TYPE SSFCRESCL,
          LV_REPEAT         TYPE C LENGTH 1,
          LS_NAST           TYPE NAST.

    DATA: LS_ARC_PARAMS TYPE ARC_PARAMS,
          LS_TOA_DARA   TYPE TOA_DARA.

    DATA: LCL_DATA TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA: LV_PATH    TYPE STRING,
          LV_MESSAGE TYPE CHAR50,
          LV_STATUS  TYPE CHAR1.

    CONSTANTS : BEGIN OF LC_CON,
                  SWINCF TYPE C LENGTH 6 VALUE 'SWINCF',
                END OF LC_CON.

    IF LCL_DATA IS BOUND.
      CREATE OBJECT LCL_DATA.
    ENDIF.

    LS_CONTROL_PARAM-GETOTF     = ABAP_TRUE.
    LS_CONTROL_PARAM-NO_DIALOG  = ABAP_TRUE.
    LS_COMPOSER_PARAM-TDPRINTER = LC_CON-SWINCF.

    CALL FUNCTION I_FM_NAME
      EXPORTING
        ARCHIVE_INDEX      = LS_TOA_DARA
        ARCHIVE_PARAMETERS = LS_ARC_PARAMS
        CONTROL_PARAMETERS = LS_CONTROL_PARAM
*       mail_appl_obj      =
        MAIL_RECIPIENT     = LS_RECIPIENT
        MAIL_SENDER        = LS_SENDER
        OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
        USER_SETTINGS      = SPACE
        IS_BIL_INVOICE     = I_BIL_INVOICE
        IS_NAST            = LS_NAST
        IS_REPEAT          = LV_REPEAT
      IMPORTING
        JOB_OUTPUT_INFO    = LS_JOB_INFO
*       document_output_info =
*       job_output_options =
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

    IF LS_JOB_INFO-OTFDATA IS NOT INITIAL.
*get path
      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = GC_CON-REPID
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                    I_PARAM             = GC_CON-PARAM
                                           CHANGING C_RETURN            = LV_PATH ).

      CONCATENATE LV_PATH 'File_name.pdf' INTO LV_PATH.

      LCL_DATA->SEND_OTF_FILE_TO_AL11( EXPORTING IT_OTF      = LS_JOB_INFO-OTFDATA
                                                 I_PATH      = LV_PATH
                                                 I_DATA_AL11 = ABAP_TRUE
                                       IMPORTING E_MESSAGE   = LV_MESSAGE
                                                 E_STATUS    = LV_STATUS ).
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
