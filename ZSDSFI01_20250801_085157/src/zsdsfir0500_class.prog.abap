*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0500_CLASS
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
      MODIFY_SCREEN,
      GET_SD_DATA,
      GET_FI_DATA,
      GET_STATUS,
      GET_DATA_OT01,
      MODIFY_REUSLT,
      SEND_TO_ETAX,
      CANCEL_DOCUMENT,
      ENDPOINT_SEND RETURNING VALUE(R) TYPE STRING,
      HEADER_SEND RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY_SEND IMPORTING I_PDF    TYPE STRING
                          I_DATA   TYPE LBBIL_INVOICE
                RETURNING VALUE(R) TYPE STRING,
      BODY_SEND_LEN IMPORTING I_DATA   TYPE STRING
                    RETURNING VALUE(R) TYPE I,
      ENDPOINT_CANCEL RETURNING VALUE(R) TYPE STRING,
      HEADER_CANCEL RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY_CANCEL RETURNING VALUE(R)   TYPE STRING,
      BODY_CANCEL_LEN IMPORTING I_DATA   TYPE STRING
                      RETURNING VALUE(R) TYPE I,
      API_SEND IMPORTING I_PDF  TYPE STRING
                         I_DATA TYPE LBBIL_INVOICE,
      API_CANC,
      SET_PRINT_DATA_TO_READ IMPORTING I_FORM_NAME TYPE RS38L_FNAM
                             RETURNING VALUE(R)    TYPE LBBIL_PRINT_DATA_TO_READ,
      GET_BILLING_DETAIL IMPORTING I_DATA   TYPE LBBIL_PRINT_DATA_TO_READ
                         RETURNING VALUE(R) TYPE LBBIL_INVOICE,
      GET_BILLING_DETAIL_BY_FI IMPORTING IT_DETAIL TYPE GTY_DETAIL
                               RETURNING VALUE(R)  TYPE LBBIL_INVOICE,
      GET_DETAIL IMPORTING I_DATA   LIKE GT_RESULT
                 RETURNING VALUE(R) TYPE GTY_DETAIL,
      CALL_SMART_FORM IMPORTING I_DATA     TYPE LBBIL_INVOICE
                                I_ALL_LINE TYPE I
                                I_INDEX    TYPE I
                      RETURNING VALUE(R)   TYPE STRING,
      CONVERT_BASE64 IMPORTING I_DATA   TYPE XSTRING
                     RETURNING VALUE(R) TYPE STRING,
      COVERT_OTF IMPORTING IT_DATA  TYPE TSFOTF
                 RETURNING VALUE(R) TYPE XSTRING,
      CONCAT_DBQ CHANGING C_RESULT TYPE STRING,
      GET_DATA_API IMPORTING I_DATA   TYPE LBBIL_INVOICE
                   RETURNING VALUE(R) TYPE ZSDSFIS145,
      CONVERT_DATE IMPORTING I_DATE   TYPE SY-DATUM
                             I_TIME   TYPE SY-UZEIT
                   RETURNING VALUE(R) TYPE STRING,
      GET_ADDRESS IMPORTING I_DATA   TYPE STRING
                  RETURNING VALUE(R) TYPE GY_ADRC,
      GET_DISTRINT_SUBDISTRICT_CODE IMPORTING I_DATA TYPE ANY
                                    CHANGING  C_DIS  TYPE ANY
                                              C_SUB  TYPE ANY,
      GET_BRANCH IMPORTING IT_DATA TYPE GTY_RESULT,
      GET_DIST_SUB_DICT,
      GET_ADDR_SDS,
      GET_ADDR IMPORTING IT_DATA TYPE GTY_RESULT,
      READ_CUST_ADDR       RETURNING VALUE(R) TYPE GY_ADRC_CUST,
      READ_CUST_PHONE_ADDR RETURNING VALUE(R) TYPE GY_CUST_PHONE,
      READ_CUST_ONE_TIME  RETURNING VALUE(R) TYPE GY_ONE_TIME_CUST,
      READ_CUST IMPORTING I_DATA   TYPE KUNNR
                RETURNING VALUE(R) TYPE STRING,
      READ_CUST_PHONE IMPORTING I_DATA   TYPE KUNNR
                      RETURNING VALUE(R) TYPE STRING,
      GET_WHT_CODE IMPORTING IT_DATA TYPE GTY_RESULT,
      READ_KNBW IMPORTING I_DATA   TYPE KUNNR
                RETURNING VALUE(R) TYPE CHAR4,
      GET_DISTRICT,
      GET_SUBDISTRICT,
      GET_PROVINCE,
      GET_CUST_DISTRICT CHANGING  C_DATA   TYPE GY_ADRC_CUST
                        RETURNING VALUE(R) TYPE CHAR10,
      GET_CUST_SUB_DISTRICT IMPORTING I_DATA   TYPE GY_ADRC_CUST
                                      I_DIST   TYPE CHAR10
                            RETURNING VALUE(R) TYPE CHAR10,
      GET_CUST_PROVINCE IMPORTING I_DATA   TYPE GY_ADRC_CUST
                        RETURNING VALUE(R) TYPE CHAR3,
      CANCEL_DOC.
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
    IF R1 EQ ABAP_TRUE.
      LCL_DATA=>GET_SD_DATA( ).
    ELSE.
      LCL_DATA=>GET_FI_DATA( ).
    ENDIF.
  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
    LCL_DATA=>GET_STATUS( ).
    LCL_DATA=>GET_DATA_OT01( ).
    LCL_DATA=>MODIFY_REUSLT( ).

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
  METHOD MODIFY_SCREEN.
    CONSTANTS : BEGIN OF LC_CON,
                  SD TYPE C LENGTH 2 VALUE 'SD',
                  FI TYPE C LENGTH 2 VALUE 'FI',
                END OF LC_CON.

    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ LC_CON-SD.
        IF R1 EQ ABAP_TRUE.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-GROUP1 EQ LC_CON-FI.
        IF R1 EQ ABAP_TRUE.
          SCREEN-ACTIVE = 0.
        ELSE.
          SCREEN-ACTIVE = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD GET_SD_DATA.

    SELECT VBRK~VBELN,
           VBRK~KUNRG,
           ( KNA1~NAME1 && ' ' && KNA1~NAME2 && ' ' && KNA1~NAME3 && ' ' && KNA1~NAME4 ) AS NAMEA,
           VBRK~NETWR,
           VBRK~WAERK,
           KNA1~ADRNR,
           KNA1~STCD3,
           VBRK~BUPLA,
           VBRK~ERDAT
      FROM VBRK
      INNER JOIN KNA1 ON VBRK~KUNRG EQ KNA1~KUNNR
      INTO TABLE @GT_RESULT
      WHERE VBRK~VBELN IN @S_VBELN[]
        AND VBRK~ERDAT IN @S_ERDAT[]
        AND VBRK~ERNAM IN @S_ERNAM[]
        AND VBRK~VKORG IN @S_VKORG[]
        AND VBRK~VTWEG IN @S_VTWEG[]
        AND VBRK~FKART IN @S_FKART[].

  ENDMETHOD.
  METHOD GET_FI_DATA.
    SELECT ACDOCA~BELNR,
           ACDOCA~KUNNR,
           ( KNA1~NAME1 && ' ' && KNA1~NAME2 && ' ' && KNA1~NAME3 && ' ' && KNA1~NAME4 ) AS NAMEA,
           ACDOCA~TSL,
           ACDOCA~RTCUR,
           KNA1~ADRNR,
           KNA1~STCD3,
           BSEG~BUPLA,
           BKPF~CPUDT,
           ACDOCA~RLDNR,
           ACDOCA~RBUKRS,
           ACDOCA~GJAHR,
           ACDOCA~BUDAT,
           ACDOCA~BLDAT,
           ACDOCA~MWSKZ,
           ACDOCA~AUGBL,
           ACDOCA~AUGGJ,
           ACDOCA~NETDT
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
        AND ACDOCA~BELNR  IN @S_BELNR
        AND ACDOCA~BUDAT  IN @S_BUDAT
        AND ACDOCA~USNAM  IN @S_USNAM
        AND ACDOCA~BLART  IN @S_BLART
      INTO TABLE @GT_RESULT.
  ENDMETHOD.
  METHOD GET_STATUS.
    SELECT ZSDSFIT051~DOCNO,
           ZSDSFIT051~DYEAR,
           ZSDSFIT051~STATU,
           ZSDSFIT051~MESSG,
           ZSDSFIT051~SENDF,
           ZSDSFIT051~CANRE,
           ZSDSFIT051~ERNAM,
           ZSDSFIT051~ERDAT,
           ZSDSFIT051~ERZET
      FROM ZSDSFIT051
      INNER JOIN @GT_RESULT AS A ON ZSDSFIT051~DOCNO EQ A~DOCNO
      INTO TABLE @GT_STATUS.
  ENDMETHOD.
  METHOD MODIFY_REUSLT.

    DATA : LS_ONE_TIME LIKE LINE OF GT_ONE_TIME_CUST.

    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
      <LFS_DATA>-CHECK = ABAP_TRUE.

      IF R1 EQ ABAP_TRUE.
        <LFS_DATA>-DOCFI = ''.
      ELSE.
        <LFS_DATA>-DOCFI = <LFS_DATA>-DOCNO.
      ENDIF.

      READ TABLE GT_ONE_TIME_CUST INTO LS_ONE_TIME
      WITH KEY VBELN = <LFS_DATA>-DOCNO.
      IF SY-SUBRC EQ 0.
        <LFS_DATA>-ADRNR = LS_ONE_TIME-ADRNR.
      ENDIF.

      READ TABLE GT_STATUS INTO DATA(LS_DATA)
      WITH KEY DOCNO = <LFS_DATA>-DOCNO.
      IF SY-SUBRC EQ 0.
        <LFS_DATA>-STATU = LS_DATA-STATU.
        <LFS_DATA>-MESSG = LS_DATA-MESSG.
        <LFS_DATA>-ERNAM = LS_DATA-ERNAM.
        <LFS_DATA>-ERDAT = LS_DATA-ERDAT.
        <LFS_DATA>-ERZET = LS_DATA-ERZET.
        <LFS_DATA>-SENDF = LS_DATA-SENDF.
        <LFS_DATA>-CANRE = LS_DATA-CANRE.
      ELSE.
        <LFS_DATA>-STATU = GC_CON-WARNING.
        <LFS_DATA>-MESSG = TEXT-101.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD SEND_TO_ETAX.
    DATA: LS_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ,
          LS_BIL_INVOICE        TYPE LBBIL_INVOICE.

    DATA: LT_TMP LIKE GT_RESULT.

    DATA : LS_DETAIL TYPE GY_DETAIL,
           LT_DETAIL TYPE GTY_DETAIL.

    DATA: LV_ALL_LINE TYPE I,
          LV_INDEX    TYPE I.

    LT_TMP[]    = GT_RESULT[].

    SORT LT_TMP BY CHECK ASCENDING.
    DELETE LT_TMP WHERE CHECK EQ SPACE.
    DELETE LT_TMP WHERE STATU EQ GC_CON-SUCCESS.

    DESCRIBE TABLE LT_TMP LINES LV_ALL_LINE.

    LCL_DATA=>GET_BRANCH( LT_TMP ).
    LCL_DATA=>GET_DIST_SUB_DICT( ).
    LCL_DATA=>GET_ADDR_SDS( ).
    LCL_DATA=>GET_ADDR( LT_TMP ).
    LCL_DATA=>GET_WHT_CODE( LT_TMP ).
    LCL_DATA=>GET_SUBDISTRICT( ).
    LCL_DATA=>GET_DISTRICT( ).
    LCL_DATA=>GET_PROVINCE( ).

    LS_PRINT_DATA_TO_READ = SET_PRINT_DATA_TO_READ( GC_FORM_NAME ).
    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>) WHERE CHECK EQ ABAP_TRUE.
      MOVE-CORRESPONDING <LFS_DATA> TO GS_RESULT.

      READ TABLE LT_TMP
      WITH KEY DOCNO = GS_RESULT-DOCNO TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      IF R1 EQ ABAP_TRUE.
        LS_BIL_INVOICE = GET_BILLING_DETAIL( LS_PRINT_DATA_TO_READ ).
      ELSE.
        LT_DETAIL = GET_DETAIL( LT_TMP ).
        LS_BIL_INVOICE = GET_BILLING_DETAIL_BY_FI( LT_DETAIL ).
      ENDIF.

      DATA(LV_PDF) = CALL_SMART_FORM( I_DATA     = LS_BIL_INVOICE
                                      I_ALL_LINE = LV_ALL_LINE
                                      I_INDEX    = LV_INDEX ).

      API_SEND( I_PDF  = LV_PDF
                I_DATA = LS_BIL_INVOICE ).
      <LFS_DATA> = GS_RESULT.
    ENDLOOP.

  ENDMETHOD.
  METHOD CANCEL_DOCUMENT.
    DATA: LT_TMP LIKE GT_RESULT.

    DATA: LV_ALL_LINE TYPE I,
          LV_INDEX    TYPE I.

    LT_TMP[]    = GT_RESULT[].

    SORT LT_TMP BY CHECK ASCENDING.
    DELETE LT_TMP WHERE CHECK EQ SPACE.
    DELETE LT_TMP WHERE STATU NE GC_CON-SUCCESS.

    DESCRIBE TABLE LT_TMP LINES LV_ALL_LINE.

    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>) WHERE CHECK EQ ABAP_TRUE.
      MOVE-CORRESPONDING <LFS_DATA> TO GS_RESULT.

      READ TABLE LT_TMP
      WITH KEY DOCNO = GS_RESULT-DOCNO TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

*      API_CANC( ).
      CANCEL_DOC( ).
      <LFS_DATA> = GS_RESULT.
    ENDLOOP.
  ENDMETHOD.
  METHOD ENDPOINT_SEND.
    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 21 VALUE 'ZSDSFIR0500',
                  PAEXT TYPE C LENGTH 21 VALUE 'ENDPOINT_ETAX_SEND',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                END OF LC_CON.

    CASE SY-SYSID.
      WHEN GC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-DEV
                                                      I_PARAM_EXT         = LC_CON-PAEXT
                                            CHANGING  C_RETURN            = R ).
      WHEN GC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-QAS
                                                      I_PARAM_EXT         = LC_CON-PAEXT
                                            CHANGING  C_RETURN            = R ).
      WHEN GC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-PRD
                                                      I_PARAM_EXT         = LC_CON-PAEXT
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD HEADER_SEND.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
  ENDMETHOD.
  METHOD BODY_SEND.

    DATA : LS_DATA TYPE ZSDSFIS145.

    LS_DATA = GET_DATA_API( I_DATA ).
    LS_DATA-_DOCUMENT = I_PDF.

    CALL METHOD /UI2/CL_JSON=>SERIALIZE
      EXPORTING
        DATA        = LS_DATA
        PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
      RECEIVING
        R_JSON      = R
      EXCEPTIONS
        OTHERS      = 1.

    REPLACE ALL OCCURRENCES OF PCRE 'replacesupplychaintradetransac' IN R WITH 'SupplyChainTradeTransaction'.
    REPLACE ALL OCCURRENCES OF PCRE 'replaceguidelinespecifieddocum' IN R WITH 'GuidelineSpecifiedDocumentContextParameter'.
    REPLACE ALL OCCURRENCES OF PCRE 'replaceapplicableheadertradeag' IN R WITH 'ApplicableHeaderTradeAgreement'.
    REPLACE ALL OCCURRENCES OF PCRE 'replaceemailuriuniversalcommun' IN R WITH 'EmailURIUniversalCommunication'.
    REPLACE ALL OCCURRENCES OF PCRE 'replacetelephoneuniversalcommu' IN R WITH 'TelephoneUniversalCommunication'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleaceapplicabletradedeliver' IN R WITH 'ApplicableTradeDeliveryTerms'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacebuyerorderreferenceddo' IN R WITH 'BuyerOrderReferencedDocument'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleaceadditionalreferenceddo' IN R WITH 'AdditionalReferencedDocument'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleaceapplicableheadertraded' IN R WITH 'ApplicableHeaderTradeDelivery'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleaceapplicableheadertrades' IN R WITH 'ApplicableHeaderTradeSettlement'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacespecifiedtradeallowanc' IN R WITH 'SpecifiedTradeAllowanceCharge'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacespecifiedtradesettleme' IN R WITH 'specifiedTradeSettlementHeaderMonetarySummation'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleaceincludedsupplychaintra' IN R WITH 'IncludedSupplyChainTradeLineItem'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleaceassociateddocumentline' IN R WITH 'AssociatedDocumentLineDocument'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleaceindividualtradeproduct' IN R WITH 'IndividualTradeProductInstance'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacedesignatedproductclass' IN R WITH 'DesignatedProductClassification'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacegrosspriceproducttrade' IN R WITH 'GrossPriceProductTradePrice'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleaceappliedtradeallowancec' IN R WITH 'AppliedTradeAllowanceCharge'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacenetincludingtaxeslinet' IN R WITH 'NetIncludingTaxesLineTotalAmount'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacespecifiedlinetradeagre' IN R WITH 'specifiedLineTradeAgreement'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacespecifiedlinetradesett' IN R WITH 'SpecifiedLineTradeSettlement'.
    REPLACE ALL OCCURRENCES OF PCRE 'repleacespecifiedtradesettlemz' IN R WITH 'specifiedTradeSettlementLineMonetarySummation'.
  ENDMETHOD.
  METHOD CONCAT_DBQ.
    CONCATENATE '"' C_RESULT '"' INTO C_RESULT.
  ENDMETHOD.
  METHOD BODY_SEND_LEN.
    R = STRLEN( I_DATA ).
  ENDMETHOD.
  METHOD ENDPOINT_CANCEL.
    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 21 VALUE 'ZSDSFIR0500',
                  PAEXT TYPE C LENGTH 21 VALUE 'ENDPOINT_ETAX_CANC',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                END OF LC_CON.

    CASE SY-SYSID.
      WHEN GC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-DEV
                                                      I_PARAM_EXT         = LC_CON-PAEXT
                                            CHANGING  C_RETURN            = R ).
      WHEN GC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-QAS
                                                      I_PARAM_EXT         = LC_CON-PAEXT
                                            CHANGING  C_RETURN            = R ).
      WHEN GC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-PRD
                                                      I_PARAM_EXT         = LC_CON-PAEXT
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD HEADER_CANCEL.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
  ENDMETHOD.
  METHOD BODY_CANCEL.
    DATA : BEGIN OF LS_DATA,
             _DOCUMENT_NO   TYPE STRING,
             _FISICAL_YEAR  TYPE STRING,
             _COMPANY_CODE  TYPE STRING,
             _DOCUMENT_TYPE TYPE STRING,
             _BRANCH        TYPE STRING,
             _PURPOSE_CODE  TYPE STRING,
           END OF LS_DATA.

    DATA LV_BRANCH TYPE C LENGTH 5.

    IF GS_RESULT-BUPLA IS INITIAL.
      LV_BRANCH = '00000'.
    ELSE.
      LV_BRANCH = GS_RESULT-BUPLA.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_BRANCH WITH ''.
      CONCATENATE '0' LV_BRANCH INTO LV_BRANCH.
    ENDIF.


    LS_DATA-_DOCUMENT_NO   = GS_RESULT-DOCNO.
    LS_DATA-_FISICAL_YEAR   = GS_RESULT-CREDT+0(4).
    LS_DATA-_COMPANY_CODE  = '0105525008237'.
    LS_DATA-_DOCUMENT_TYPE = 'T01'.
    LS_DATA-_BRANCH        = LV_BRANCH.
    LS_DATA-_PURPOSE_CODE  = 'CLN999'.

    CALL METHOD /UI2/CL_JSON=>SERIALIZE
      EXPORTING
        DATA        = LS_DATA
        PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
      RECEIVING
        R_JSON      = R
      EXCEPTIONS
        OTHERS      = 1.

  ENDMETHOD.
  METHOD BODY_CANCEL_LEN.
    R = STRLEN( I_DATA ).
  ENDMETHOD.
  METHOD API_SEND.
    DATA: LV_URL              TYPE STRING,
          LV_METHOD           TYPE STRING,
          LV_BODY_TEXT        TYPE STRING,
          LV_BODY_BIN         TYPE XSTRING,
          LV_LEN              TYPE I,
          LV_LEN_BIN          TYPE I,
          LV_RETURN_BODY_TEXT TYPE STRING,
          LV_RETURN_BODY_BIN  TYPE XSTRING,
          LV_MESSAGE          TYPE STRING,
          LV_STATUS           TYPE C.

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    DATA: LS_ZSDSFIT051 TYPE ZSDSFIT051.

    DATA : BEGIN OF LS_VALIDATION,
             INVOICE_HDR_ID TYPE STRING,
             LOG_EVENT_ID   TYPE STRING,
             HEADER         TYPE STRING,
             MESSAGE        TYPE STRING,
             CREATED_BY     TYPE STRING,
             CREATED_DATE   TYPE STRING,
           END OF LS_VALIDATION.
    DATA LT_CALIDATION LIKE TABLE OF LS_VALIDATION.


    DATA: BEGIN OF LS_DATA,
            trans_Code         TYPE STRING,
            response_Code      TYPE STRING,
            response_Msg       TYPE STRING,
            error_Validate_Msg LIKE LT_CALIDATION,
            DOCUMENT           TYPE STRING,
          END OF LS_DATA.
    "transCode":null,#    "responseCode":"E",#    "responseMsg":"Cannot find signing config.",#    "errorValidateMsg":null,#    "document":null#}
    LV_METHOD = 'POST'.

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT_SEND( ).
    LT_HEADER    = LCL_DATA=>HEADER_SEND( ).
    LV_BODY_TEXT = LCL_DATA=>BODY_SEND( I_PDF  = I_PDF
                                        I_DATA = I_DATA ).
    LV_LEN       = LCL_DATA=>BODY_SEND_LEN( LV_BODY_TEXT ).

    CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
      EXPORTING
        I_URL              = LV_URL
        I_METHOD           = LV_METHOD
        I_HEADER           = LT_HEADER
        I_BODY_TEXT        = LV_BODY_TEXT
        I_BODY_BIN         = LV_BODY_BIN
        I_LEN              = LV_LEN
        I_LEN_BIN          = LV_LEN_BIN
      IMPORTING
        E_RETURN           = LS_DATA
        E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
        E_MESSAGE          = LV_MESSAGE
        E_STATUS           = LV_STATUS.
    IF LS_DATA-RESPONSE_CODE = 'S'.
      LS_ZSDSFIT051-MESSG = TEXT-S03.
      LS_ZSDSFIT051-STATU = GC_CON-SUCCESS.
      LS_ZSDSFIT051-SENDF = ABAP_TRUE.
      LS_ZSDSFIT051-CANRE = ABAP_FALSE.
      GS_RESULT-STATU = GC_CON-SUCCESS.
      GS_RESULT-MESSG = TEXT-S03.
    ELSE.
      READ TABLE LS_DATA-ERROR_VALIDATE_MSG INTO DATA(LS_MSG) INDEX 1.
      IF SY-SUBRC EQ 0.
        LS_ZSDSFIT051-MESSG = LS_MSG-MESSAGE.
        GS_RESULT-MESSG     = LS_MSG-MESSAGE.
      ELSE.
        LS_ZSDSFIT051-MESSG = LS_DATA-RESPONSE_MSG.
        GS_RESULT-MESSG     = LS_DATA-RESPONSE_MSG.
      ENDIF.

      LS_ZSDSFIT051-SENDF = GS_RESULT-SENDF.
      IF LS_DATA-RESPONSE_MSG EQ 'Data is duplicate'.
        LS_ZSDSFIT051-SENDF = ABAP_TRUE.
        GS_RESULT-SENDF     = ABAP_TRUE.
      ENDIF.

      LS_ZSDSFIT051-STATU = GC_CON-ERROR.
      GS_RESULT-STATU = GC_CON-ERROR.
    ENDIF.

    IF GS_RESULT-ERDAT IS INITIAL.
      LS_ZSDSFIT051-ERNAM = SY-UNAME.
      LS_ZSDSFIT051-ERDAT = SY-DATUM.
      LS_ZSDSFIT051-ERZET = SY-UZEIT.
    ELSE.
      LS_ZSDSFIT051-ERNAM = GS_RESULT-ERNAM.
      LS_ZSDSFIT051-ERDAT = GS_RESULT-ERDAT.
      LS_ZSDSFIT051-ERZET = GS_RESULT-ERZET.
    ENDIF.
    LS_ZSDSFIT051-DOCNO = GS_RESULT-DOCNO.
    LS_ZSDSFIT051-DYEAR = GS_RESULT-GJAHR.

    LS_ZSDSFIT051-AENAM = SY-UNAME.
    LS_ZSDSFIT051-AEDAT = SY-DATUM.
    LS_ZSDSFIT051-AEZET = SY-UZEIT.
    MODIFY ZSDSFIT051 FROM LS_ZSDSFIT051.
    COMMIT WORK AND WAIT.
  ENDMETHOD.
  METHOD API_CANC.
    DATA: LV_URL              TYPE STRING,
          LV_METHOD           TYPE STRING,
          LV_BODY_TEXT        TYPE STRING,
          LV_BODY_BIN         TYPE XSTRING,
          LV_LEN              TYPE I,
          LV_LEN_BIN          TYPE I,
          LV_RETURN_BODY_TEXT TYPE STRING,
          LV_RETURN_BODY_BIN  TYPE XSTRING,
          LV_MESSAGE          TYPE STRING,
          LV_STATUS           TYPE C.

    DATA: LT_HEADER TYPE ZSDSCAS001_TT.

    DATA: BEGIN OF LS_DATA,
            TRANS_CODE    TYPE STRING,
            RESPONSE_CODE TYPE STRING,
            RESPONSE_MSG  TYPE STRING,
          END OF LS_DATA.

    DATA : LS_ZSDSFIT051 TYPE ZSDSFIT051.

    LV_METHOD = 'POST'.

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT_CANCEL( ).
    LT_HEADER    = LCL_DATA=>HEADER_CANCEL( ).
    LV_BODY_TEXT = LCL_DATA=>BODY_CANCEL( ).
    LV_LEN       = LCL_DATA=>BODY_CANCEL_LEN( LV_BODY_TEXT ).

    CALL METHOD ZCL_SDSCA_CAL_API=>CALL_API
      EXPORTING
        I_URL              = LV_URL
        I_METHOD           = LV_METHOD
        I_HEADER           = LT_HEADER
        I_BODY_TEXT        = LV_BODY_TEXT
        I_BODY_BIN         = LV_BODY_BIN
        I_LEN              = LV_LEN
        I_LEN_BIN          = LV_LEN_BIN
      IMPORTING
        E_RETURN           = LS_DATA
        E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
        E_MESSAGE          = LV_MESSAGE
        E_STATUS           = LV_STATUS.
    IF LV_STATUS = 'S'.
      LS_ZSDSFIT051-MESSG = TEXT-S02.
      LS_ZSDSFIT051-STATU = GC_CON-WARNING.
      LS_ZSDSFIT051-CANRE = ABAP_TRUE.
      GS_RESULT-STATU = GC_CON-SUCCESS.
      GS_RESULT-MESSG = TEXT-S01.
    ELSE.
      LS_ZSDSFIT051-MESSG = LS_DATA-RESPONSE_MSG.
      LS_ZSDSFIT051-STATU = GC_CON-SUCCESS.
      GS_RESULT-STATU     = GC_CON-SUCCESS.
      GS_RESULT-MESSG     = LS_DATA-RESPONSE_MSG.
    ENDIF.

    IF GS_RESULT-ERDAT IS INITIAL.
      LS_ZSDSFIT051-ERNAM = SY-UNAME.
      LS_ZSDSFIT051-ERDAT = SY-DATUM.
      LS_ZSDSFIT051-ERZET = SY-UZEIT.
    ELSE.
      LS_ZSDSFIT051-ERNAM = GS_RESULT-ERNAM.
      LS_ZSDSFIT051-ERDAT = GS_RESULT-ERDAT.
      LS_ZSDSFIT051-ERZET = GS_RESULT-ERZET.
    ENDIF.
    LS_ZSDSFIT051-DOCNO = GS_RESULT-DOCNO.
    LS_ZSDSFIT051-DYEAR = GS_RESULT-GJAHR.

    LS_ZSDSFIT051-AENAM = SY-UNAME.
    LS_ZSDSFIT051-AEDAT = SY-DATUM.
    LS_ZSDSFIT051-AEZET = SY-UZEIT.
    MODIFY ZSDSFIT051 FROM LS_ZSDSFIT051.
    COMMIT WORK AND WAIT.
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
    DATA : LV_OBJKY TYPE NAST-OBJKY.

    CONSTANTS LC_AG TYPE NAST-PARVW VALUE 'AG'.

    LV_OBJKY = GS_RESULT-DOCNO.

    CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
      EXPORTING
        IF_BIL_NUMBER         = LV_OBJKY
        IF_PARVW              = LC_AG
        IF_PARNR              = GS_RESULT-KUNNR
        IF_LANGUAGE           = SY-LANGU
        IS_PRINT_DATA_TO_READ = I_DATA
      IMPORTING
        ES_BIL_INVOICE        = R
      EXCEPTIONS
        RECORDS_NOT_FOUND     = 1
        RECORDS_NOT_REQUESTED = 2
        OTHERS                = 3.
    IF SY-SUBRC <> 0.

    ENDIF.
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

    R-HD_GEN-BIL_NUMBER = GS_RESULT-DOCNO.
    R-HD_GEN-BIL_DATE   = GS_RESULT-BUDAT.
    R-HD_GEN-BIL_EDATE  = GS_RESULT-BUDAT.
    R-HD_GEN-PAYER      = GS_RESULT-KUNNR.
    R-HD_GEN-TERMS_PAYM = GC_CON-ZZZZ.
    R-HD_GEN-VAL_DATE   = GS_RESULT-NETDT.
    R-HD_GEN-BUPLA      = GS_RESULT-BUPLA.

    LS_PART_ADD-PARTN_NUMB = SPACE.
    APPEND LS_PART_ADD TO R-HD_PART_ADD.

    LS_ADR-PARTN_ROLE = GC_CON-AG.
    LS_ADR-ADDR_NO    = GS_RESULT-ADRNR.
    APPEND LS_ADR TO R-HD_ADR.

    LOOP AT IT_DETAIL INTO LS_DETAIL WHERE BELNR EQ GS_RESULT-DOCNO AND
                                           GJAHR EQ GS_RESULT-GJAHR AND
                                           LINETYPE NE GC_CON-05100.

      ADD 1 TO LV_LINE.
      LS_GEN-BIL_NUMBER = GS_RESULT-DOCNO.
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
      LS_GEN-FKIMG      = 1.
      LS_GEN-SALES_UNIT = SPACE.
      APPEND LS_GEN TO R-IT_GEN.


      LS_PRICE-BIL_NUMBER = GS_RESULT-DOCNO.
      LS_PRICE-ITM_NUMBER = LV_LINE.
      LS_PRICE-KZWI1 = ABS( LS_DETAIL-TSL ).
      READ TABLE IT_DETAIL INTO LS_TMP
      WITH KEY BELNR    = GS_RESULT-DOCNO
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

    LS_KOND-BIL_NUMBER = GS_RESULT-DOCNO.
    APPEND LS_KOND TO R-HD_KOND.

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
                           A~DOCNO  EQ ACDOCA~BELNR
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
                            C~DOCNO  EQ B~BELNR
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
                              C~DOCNO  EQ B~BELNR
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
  METHOD CALL_SMART_FORM.
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

    DATA: LV_FM_NAME TYPE RS38L_FNAM.

    DATA: LV_PATH    TYPE STRING,
          LV_MESSAGE TYPE CHAR50,
          LV_STATUS  TYPE CHAR1.

    DATA : LV_BIN TYPE XSTRING.

    DATA : LV_BASE64 TYPE STRING.

    CONSTANTS : BEGIN OF LC_CON,
                  SWINCF TYPE C LENGTH 6 VALUE 'SWINCF',
                END OF LC_CON.

    IF LCL_DATA IS NOT BOUND.
      CREATE OBJECT LCL_DATA.
    ENDIF.

    LS_CONTROL_PARAM-GETOTF     = ABAP_TRUE.
    LS_CONTROL_PARAM-NO_DIALOG  = ABAP_TRUE.
    LS_COMPOSER_PARAM-TDPRINTER = LC_CON-SWINCF.

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
        I_NATION           = SPACE
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


    LV_BIN = COVERT_OTF( LS_JOB_INFO-OTFDATA ).
    R      = CONVERT_BASE64( LV_BIN ).
  ENDMETHOD.
  METHOD CONVERT_BASE64.
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        INPUT  = I_DATA
      IMPORTING
        OUTPUT = R.
  ENDMETHOD.
  METHOD COVERT_OTF.
    DATA : LT_LINES TYPE STANDARD TABLE OF TLINE.

    DATA : LS_LINE LIKE LINE OF LT_LINES.

    DATA : LV_BIN_FILESIZE TYPE I.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = GC_CON-FORMAT
*       max_linewidth         = 132
      IMPORTING
        BIN_FILESIZE          = LV_BIN_FILESIZE
        BIN_FILE              = R
      TABLES
        OTF                   = IT_DATA
        LINES                 = LT_LINES
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        ERR_BAD_OTF           = 4
        OTHERS                = 5.

  ENDMETHOD.
  METHOD GET_DATA_API.

    DATA : LS_ZSDSFIS147 TYPE ZSDSFIS147.
    DATA : LS_ZSDSFIS168 TYPE ZSDSFIS168,
           LS_ZSDSFIS173 TYPE ZSDSFIS173,
           LS_ZSDSFIS174 TYPE ZSDSFIS174,
           LS_ZSDSFIS176 TYPE ZSDSFIS176,
           LS_ZSDSFIS177 TYPE ZSDSFIS177,
           LS_ZSDSFIS178 TYPE ZSDSFIS178.

    DATA : LV_STRING TYPE STRING,
           LV_TAX    TYPE STRING.

    DATA : LS_ADRC     TYPE GY_ADRC,
           LS_ADRC_BUY TYPE GY_ADRC_CUST,
           LS_PHON_BUY TYPE GY_CUST_PHONE.

    DATA : LV_DIS TYPE CHAR10,
           LV_SUB TYPE CHAR10.

    DATA : LS_GRAND_TOTAL TYPE ZSDSFIS167.

    DATA : LV_LINE   TYPE I,
           LV_LINE_C TYPE CHAR4.

    DATA : LS_PRICE LIKE LINE OF I_DATA-IT_PRICE.

    DATA : LS_ONE_TIME_CUST LIKE LINE OF GT_ONE_TIME_CUST.

*--------------------------------------------------------------------*
    READ TABLE I_DATA-HD_KOND INTO DATA(LS_VAT)
    WITH KEY BIL_NUMBER = GS_RESULT-DOCNO
             KSCHL      = 'MWST'.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
    LS_ADRC_BUY      = READ_CUST_ADDR( ).
    LS_PHON_BUY      = READ_CUST_PHONE_ADDR( ).
*    LS_ONE_TIME_CUST = READ_CUST_ONE_TIME( ).
*    IF LS_ONE_TIME_CUST IS NOT INITIAL.
*      IF LS_ONE_TIME_CUST-SMTP_ADDR IS NOT INITIAL.
*        R-_EMAIL             = LS_ONE_TIME_CUST-SMTP_ADDR.
*        R-_REQUEST_SEND_MAIL = ABAP_TRUE.
*      ELSEIF LS_ONE_TIME_CUST-TEL_NUMBER IS NOT INITIAL.
*        R-_MOBILE_NUMBER      = LS_ONE_TIME_CUST-TEL_NUMBER.
*        R-_REQUEST_SEND_S_M_S = ABAP_TRUE.
*      ENDIF.
*    ELSE.
    R-_EMAIL    = READ_CUST( GS_RESULT-KUNNR ).
    IF R-_EMAIL IS INITIAL.
      R-_EMAIL = LS_ADRC_BUY-SMTP_ADDR.
    ENDIF.

    IF R-_EMAIL IS NOT INITIAL.
      R-_REQUEST_SEND_MAIL = ABAP_TRUE.
    ELSE.
      R-_MOBILE_NUMBER = READ_CUST_PHONE( GS_RESULT-KUNNR ).

      IF R-_MOBILE_NUMBER IS INITIAL.
        R-_MOBILE_NUMBER = LS_PHON_BUY-TEL_NUMBER.
      ENDIF.

      IF R-_MOBILE_NUMBER IS NOT INITIAL.
        R-_REQUEST_SEND_S_M_S = ABAP_TRUE.
      ENDIF.
    ENDIF.
*    ENDIF.
*--------------------------------------------------------------------*
    IF GS_RESULT-SENDF EQ SPACE.
      R-_REPLACE_FLAG = 'N'.
    ELSE.
      IF GS_RESULT-CANRE EQ ABAP_TRUE.
        R-_REPLACE_FLAG = 'Y'.
      ELSEIF GS_RESULT-STATU EQ GC_CON-ERROR.
        R-_REPLACE_FLAG = 'Y'.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.
*    R-_REPLACE_FLAG = 'N'.
*--------------------------------------------------------------------*
    R-_R_D_FLAG = 'N'.
*--------------------------------------------------------------------*
    IF GS_RESULT-BUPLA IS INITIAL.
      R-_BRANCH       = '00000'.
    ELSE.
      R-_BRANCH = GS_RESULT-BUPLA.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN R-_BRANCH WITH ''.
      CONCATENATE '0' R-_BRANCH INTO R-_BRANCH.
    ENDIF.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN R-_BRANCH WITH ''.
*--------------------------------------------------------------------*
    LS_ZSDSFIS147-_I_D-SCHEME_AGENCY_I_D  = 'ETDA'.
    LS_ZSDSFIS147-_I_D-SCHEME_VERSION_I_D = 'v2.0'.
    LS_ZSDSFIS147-_I_D-_VALUE             = 'ER3-2560'.
    APPEND LS_ZSDSFIS147 TO
    R-_EXCHANGED_DOCUMENT_CONTEXT-REPLACEGUIDELINESPECIFIEDDOCUM.
*--------------------------------------------------------------------*
*    SELECT SINGLE BELNR
*    FROM BKPF
*    INTO @R-_EXCHANGED_DOCUMENT-_I_D
*    WHERE AWKEY EQ @GS_RESULT-DOCNO.
*    IF SY-SUBRC NE 0.
    R-_EXCHANGED_DOCUMENT-_I_D = GS_RESULT-DOCNO.
*    ENDIF.
*--------------------------------------------------------------------*
    LV_STRING = 'ใบรับ (Receipt)'.
    APPEND LV_STRING TO R-_EXCHANGED_DOCUMENT-_NAME.
*--------------------------------------------------------------------*
    R-_EXCHANGED_DOCUMENT-_TYPE_CODE = 'T01'.
*--------------------------------------------------------------------*
    R-_EXCHANGED_DOCUMENT-_ISSUE_DATE_TIME = CONVERT_DATE( I_DATE = GS_RESULT-CREDT
                                                           I_TIME = SY-UZEIT ).
*--------------------------------------------------------------------*
    CLEAR : R-_EXCHANGED_DOCUMENT-_PURPOSE.
*--------------------------------------------------------------------*
    CLEAR : R-_EXCHANGED_DOCUMENT-_PURPOSE_CODE.
*--------------------------------------------------------------------*
    CLEAR : R-_EXCHANGED_DOCUMENT-_GLOBAL_ID.
*--------------------------------------------------------------------*
    LV_STRING = CONVERT_DATE( I_DATE = SY-DATUM
                              I_TIME = SY-UZEIT ).
    APPEND LV_STRING TO
    R-_EXCHANGED_DOCUMENT-_CREATION_DATE_TIME.
*--------------------------------------------------------------------*
    CLEAR R-_EXCHANGED_DOCUMENT-_INCLUDED_NOTE.
*--------------------------------------------------------------------*
*    LV_TAX = '0105525008237'.
    LV_TAX = '1000'.
    APPEND LV_TAX TO
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-ID.
*--------------------------------------------------------------------*
    LS_ADRC = GET_ADDRESS( R-_BRANCH ).
    CONCATENATE : LS_ADRC-NAME1
                  LS_ADRC-NAME2
                  LS_ADRC-NAME3
                  LS_ADRC-NAME4
            INTO
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-NAME
    SEPARATED BY SPACE.
*--------------------------------------------------------------------*
    LV_TAX = '0105525008237'.
    CONCATENATE LV_TAX
                R-_BRANCH
           INTO
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_SPECIFIED_TAX_REGISTRATION-_I_D-VALUE.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_SPECIFIED_TAX_REGISTRATION-_I_D-SCHEME_I_D =
    'TXID'.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_POSTCODE_CODE =
    LS_ADRC-POST_CODE1.
*--------------------------------------------------------------------*
    GET_DISTRINT_SUBDISTRICT_CODE( EXPORTING I_DATA = R-_BRANCH
                                   CHANGING  C_DIS  = LV_DIS
                                             C_SUB  = LV_SUB ).
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_CITY_NAME =
    LV_DIS.

    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_CITY_SUB_DIVISION_NAME =
    LV_SUB.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_COUNTRY_I_D-SCHEME_I_D
    = '3166-1 alpha-2'.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_COUNTRY_I_D-_VALUE
    = 'TH'.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_COUNTRY_SUB_DIVISION_I_D
    = LV_DIS+0(2).
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_SELLER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_BUILDING_NUMBER
    = LS_ADRC-STREET+0(16).
*--------------------------------------------------------------------*
    APPEND GS_RESULT-KUNNR TO R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_I_D.
*    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_I_D = GS_RESULT-KUNNR.
    CONCATENATE : LS_ADRC_BUY-NAME1
                  LS_ADRC_BUY-NAME2
                  LS_ADRC_BUY-NAME3
                  LS_ADRC_BUY-NAME4
            INTO
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_NAME
    SEPARATED BY SPACE.
*--------------------------------------------------------------------*
    READ TABLE GT_BRANCH INTO DATA(LS_BRANCH)
    WITH KEY KUNNR = GS_RESULT-KUNNR.
    READ TABLE GT_ONE_TIME_CUST_ID INTO GS_ONE_TIME_CUST_ID
    WITH TABLE KEY VBELN = GS_RESULT-DOCNO.
    IF SY-SUBRC EQ 0.
      CONCATENATE GS_ONE_TIME_CUST_ID-STCD3 LS_BRANCH-J_1TPBUPL INTO LV_STRING.
    ELSE.
      CONCATENATE GS_RESULT-STCD3 LS_BRANCH-J_1TPBUPL INTO LV_STRING.
    ENDIF.

    IF LV_STRING EQ '000000000000000000' OR
       LV_STRING IS INITIAL OR
       LV_STRING EQ '0000'.
      LV_STRING = 'N/A'.
    ENDIF.

    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_SPECIFIED_TAX_REGISTRATION-_I_D-VALUE
    = LV_STRING.
*--------------------------------------------------------------------*
    DATA(LV_TAX_CUST) = READ_KNBW( GS_RESULT-KUNNR ).
    IF LV_STRING EQ 'N/A'.
      LV_TAX_CUST = 'OTHR'.
    ENDIF.
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_SPECIFIED_TAX_REGISTRATION-_I_D-SCHEME_I_D =
    LV_TAX_CUST.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_POSTCODE_CODE =
    LS_ADRC_BUY-POST_CODE1.
*--------------------------------------------------------------------*
    LV_DIS = GET_CUST_DISTRICT( CHANGING C_DATA = LS_ADRC_BUY ).
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_CITY_NAME =
    LV_DIS.

    LV_SUB = GET_CUST_SUB_DISTRICT( I_DATA = LS_ADRC_BUY
                                    I_DIST = LV_DIS ).
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_CITY_SUB_DIVISION_NAME =
    LV_SUB.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_COUNTRY_I_D-SCHEME_I_D
    = '3166-1 alpha-2'.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_COUNTRY_I_D-_VALUE
    = 'TH'.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_COUNTRY_SUB_DIVISION_I_D
    = LS_ADRC_BUY-REGION.
*--------------------------------------------------------------------*
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_BUILDING_NUMBER
    = LS_ADRC_BUY-STREET+0(16).

*    CONCATENATE LS_ADRC_BUY-HOUSE_NUM1
*                LS_ADRC_BUY-STREET
*                LS_ADRC_BUY-STR_SUPPL2
*                LS_ADRC_BUY-STR_SUPPL3
*                LS_ADRC_BUY-LOCATION
*                LS_ADRC_BUY-CITY2
*           INTO
*    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLACEAPPLICABLEHEADERTRADEAG-_BUYER_TRADE_PARTY-_POSTAL_TRADE_ADDRESS-_LINE_ONE
*    SEPARATED BY SPACE.
*--------------------------------------------------------------------*
    LS_GRAND_TOTAL-VALUE = GS_RESULT-AMOUT + LS_VAT-KWERT.
    "( GS_RESULT-AMOUT * 107 ) / 100.
    APPEND LS_GRAND_TOTAL TO
    R-REPLACESUPPLYCHAINTRADETRANSAC-REPLEACEAPPLICABLEHEADERTRADES-REPLEACESPECIFIEDTRADESETTLEME-_GRAND_TOTAL_AMOUNT.
*--------------------------------------------------------------------*
    CLEAR : LV_LINE.
    LOOP AT I_DATA-IT_GEN INTO DATA(LS_GEN)." WHERE NET_WEIGHT NE 0.

      READ TABLE I_DATA-IT_PRICE INTO LS_PRICE
      WITH KEY BIL_NUMBER = LS_GEN-BIL_NUMBER
               ITM_NUMBER = LS_GEN-ITM_NUMBER.
      IF LS_PRICE-NETWR EQ 0.
        CONTINUE.
      ENDIF.

      ADD 1 TO LV_LINE.
      LV_LINE_C = LV_LINE.
      LV_LINE_C = |{ LV_LINE_C  ALPHA = IN }|.
      ls_ZSDSFIS168-REPLEACEASSOCIATEDDOCUMENTLINE-_LINE_I_D = LV_LINE_C.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN
      ls_ZSDSFIS168-REPLEACEASSOCIATEDDOCUMENTLINE-_LINE_I_D WITH ''.
*--------------------------------------------------------------------*
      LV_STRING = LS_GEN-SHORT_TEXT.
      APPEND LV_STRING TO
      ls_ZSDSFIS168-_SPECIFIED_TRADE_PRODUCT-_NAME.
*--------------------------------------------------------------------*
      LS_ZSDSFIS173-currency_I_D = 'THB'.
      LS_ZSDSFIS173-VALUE        = LS_PRICE-NETWR.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_ZSDSFIS173-VALUE WITH ''.

      APPEND LS_ZSDSFIS173 TO
      ls_ZSDSFIS168-REPLEACESPECIFIEDLINETRADEAGRE-REPLEACEGROSSPRICEPRODUCTTRADE-_CHARGE_AMOUNT.
*--------------------------------------------------------------------*
*      APPEND LS_ZSDSFIS174 TO
      ls_ZSDSFIS168-_SPECIFIED_LINE_TRADE_DELIVERY-_BILLED_QUANTITY-_VALUE = LS_GEN-FKIMG.
*--------------------------------------------------------------------*
      ls_ZSDSFIS177-_TYPE_CODE = 'VAT'.
*--------------------------------------------------------------------*
      LV_STRING = LS_PRICE-NETWR.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_STRING WITH ''.
      APPEND LV_STRING TO ls_ZSDSFIS177-_BASIS_AMOUNT.
*--------------------------------------------------------------------*
      LV_STRING = LS_PRICE-MWSBP.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_STRING WITH ''.
      APPEND LV_STRING TO ls_ZSDSFIS177-_CALCULATED_AMOUNT.
*--------------------------------------------------------------------*
      APPEND ls_ZSDSFIS177 TO
      ls_ZSDSFIS168-REPLEACESPECIFIEDLINETRADESETT-_APPLICABLE_TRADE_TAX.
*--------------------------------------------------------------------*
      LS_ZSDSFIS173-CURRENCY_I_D = 'THB'.
      LV_STRING = LS_PRICE-MWSBP.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_STRING WITH ''.
      LS_ZSDSFIS173-VALUE        = LV_STRING.
      APPEND LS_ZSDSFIS173 TO LS_ZSDSFIS178-_TAX_TOTAL_AMOUNT.
*--------------------------------------------------------------------*
      LS_ZSDSFIS173-CURRENCY_I_D = 'THB'.
      LV_STRING = LS_PRICE-NETWR.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_STRING WITH ''.
      LS_ZSDSFIS173-VALUE        = LV_STRING.
      APPEND LS_ZSDSFIS173 TO LS_ZSDSFIS178-_NET_LINE_TOTAL_AMOUNT.
*--------------------------------------------------------------------*
      LS_ZSDSFIS173-CURRENCY_I_D = 'THB'.
      LV_STRING = LS_PRICE-NETWR + LS_PRICE-MWSBP.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_STRING WITH ''.
      LS_ZSDSFIS173-VALUE        = LV_STRING.
      APPEND LS_ZSDSFIS173 TO LS_ZSDSFIS178-REPLEACENETINCLUDINGTAXESLINET.
*--------------------------------------------------------------------*
      ls_ZSDSFIS168-REPLEACESPECIFIEDLINETRADESETT-REPLEACESPECIFIEDTRADESETTLEMZ =
      LS_ZSDSFIS178.
*      ls_ZSDSFIS168-REPLEACESPECIFIEDLINETRADESETT.
*--------------------------------------------------------------------*
      APPEND ls_ZSDSFIS168 TO
      R-REPLACESUPPLYCHAINTRADETRANSAC-REPLEACEINCLUDEDSUPPLYCHAINTRA.
    ENDLOOP.

  ENDMETHOD.
  METHOD CONVERT_DATE.

    R = |{ I_DATE(4) }-{ I_DATE+4(2) }-{ I_DATE+6(2) }T| &&
        |{ I_TIME(2) }:{ I_TIME+2(2) }:{ I_TIME+4(2) }.000Z|.

  ENDMETHOD.
  METHOD GET_ADDRESS.
    DATA : LV_BTANCH TYPE J_1BBRANCH-BRANCH.

    LV_BTANCH = I_DATA.

    READ TABLE GT_ADRC INTO R
    WITH KEY BRANCH = LV_BTANCH.
  ENDMETHOD.
  METHOD GET_DISTRINT_SUBDISTRICT_CODE.

    READ TABLE GT_DIS_SUB INTO DATA(LS_DIS_SUB)
    WITH KEY BUPLA = I_DATA.
    IF SY-SUBRC EQ 0.
      C_DIS = LS_DIS_SUB-DISCD.
      C_SUB = LS_DIS_SUB-SUBCD.
    ENDIF.
*    SELECT SINGLE DISCD
*                  SUBCD
*      FROM ZSDSFIT052
*      INTO (C_DIS,C_SUB)
*      WHERE BUPLA EQ I_DATA.
  ENDMETHOD.
  METHOD GET_BRANCH.
    SELECT KUNNR,
           J_1TPBUPL
      FROM FITHA_PBUPL_D
      FOR ALL ENTRIES IN @IT_DATA
      WHERE KUNNR EQ @IT_DATA-KUNNR
      INTO TABLE @GT_BRANCH.
  ENDMETHOD.
  METHOD GET_DIST_SUB_DICT.
    SELECT BUPLA,
           DISCD,
           SUBCD
      FROM ZSDSFIT052
      INTO TABLE @GT_DIS_SUB.
  ENDMETHOD.
  METHOD GET_ADDR_SDS.
    CONSTANTS : BEGIN OF LC_CON,
                  COM_CODE TYPE J_1BBRANCH-BUKRS VALUE '1000',
                END OF LC_CON.

    DATA : BEGIN OF LS_ADDR,
             ADRNR  TYPE J_1BBRANCH-ADRNR,
             BRANCH TYPE J_1BBRANCH-BRANCH,
           END OF LS_ADDR.
    DATA LT_ADDR LIKE TABLE OF LS_ADDR WITH EMPTY KEY.

    DATA : LV_BTANCH TYPE J_1BBRANCH-BRANCH.

    SELECT J_1BBRANCH~ADRNR,
           J_1BBRANCH~BRANCH
      FROM J_1BBRANCH
      WHERE J_1BBRANCH~BUKRS  EQ @LC_CON-COM_CODE
      INTO TABLE @LT_ADDR.
    IF LT_ADDR IS NOT INITIAL.
      SELECT A~BRANCH,
             ADRC~*
        FROM @LT_ADDR AS A
        INNER JOIN ADRC ON A~ADRNR EQ ADRC~ADDRNUMBER
        INTO TABLE @GT_ADRC.
    ENDIF.

  ENDMETHOD.
  METHOD GET_ADDR.
    SELECT ADRC~*
      FROM ADRC
      LEFT JOIN ADR6 ON ADRC~ADDRNUMBER EQ ADR6~ADDRNUMBER
      FOR ALL ENTRIES IN @IT_DATA
      WHERE ADRC~ADDRNUMBER EQ @IT_DATA-ADRNR
        AND ADRC~NATION     EQ @SPACE
      INTO CORRESPONDING FIELDS OF TABLE @GT_ADRC_CUST.

    SELECT ADDRNUMBER,
           TEL_NUMBER
      FROM ADR2
      FOR ALL ENTRIES IN @IT_DATA
      WHERE ADR2~ADDRNUMBER EQ @IT_DATA-ADRNR
        AND ADR2~R3_USER    EQ '3'
      INTO TABLE @GT_CUST_PHONE.

    SELECT KUNNR,
           E_MAIL,
           MOBILE_PHONE
      FROM ZSDSFIC001
      FOR ALL ENTRIES IN @IT_DATA
      WHERE KUNNR EQ @IT_DATA-KUNNR
      INTO TABLE @GT_ETAX_CUST.
  ENDMETHOD.
  METHOD READ_CUST_ADDR.
    READ TABLE GT_ADRC_CUST INTO R
    WITH KEY ADDRNUMBER = GS_RESULT-ADRNR.
  ENDMETHOD.
  METHOD READ_CUST_PHONE_ADDR.
    READ TABLE GT_CUST_PHONE INTO R
    WITH KEY ADDRNUMBER = GS_RESULT-ADRNR.
  ENDMETHOD.
  METHOD READ_CUST_ONE_TIME.
    READ TABLE GT_ONE_TIME_CUST INTO R
    WITH KEY VBELN = GS_RESULT-DOCNO.
  ENDMETHOD.
  METHOD READ_CUST.
    READ TABLE GT_ETAX_CUST INTO DATA(LS_DATA)
    WITH KEY KUNNR = I_DATA.
    IF SY-SUBRC EQ 0.
      R = LS_DATA-E_MAIL.
    ENDIF.
  ENDMETHOD.
  METHOD READ_CUST_PHONE.
    READ TABLE GT_ETAX_CUST INTO DATA(LS_DATA)
    WITH KEY KUNNR = I_DATA.
    IF SY-SUBRC EQ 0.
      R = LS_DATA-MOBILE_PHONE.
    ENDIF.
  ENDMETHOD.
  METHOD GET_WHT_CODE.
    SELECT KUNNR,
           QSREC
      FROM KNBW
      FOR ALL ENTRIES IN @IT_DATA
      WHERE KUNNR EQ @IT_DATA-KUNNR
      INTO TABLE @GT_KNBW.
  ENDMETHOD.
  METHOD READ_KNBW.
    READ TABLE GT_KNBW INTO DATA(LS_DATA)
    WITH KEY KUNNR = I_DATA.
    CASE LS_DATA.
      WHEN '03'.
        R = 'NIDN'.
      WHEN '53'.
        R = 'TXID'.
      WHEN OTHERS.
        R = 'TXID'.
    ENDCASE.
  ENDMETHOD.
  METHOD GET_DISTRICT.
    SELECT DIST_CODE
           DIST_NAME
           PROVINCE_CODE
      FROM  ZSDSFIC017
      INTO  TABLE GT_ZSDSFIC017.
  ENDMETHOD.
  METHOD GET_SUBDISTRICT.
    SELECT SUB_DIST_CODE
           SUB_DIST_NAME
           DIST_CODE
           PROVINCE_CODE
      FROM  ZSDSFIC020
      INTO  TABLE GT_ZSDSFIC020.
  ENDMETHOD.
  METHOD GET_PROVINCE.
    SELECT BLAND
           BEZEI
      FROM T005U
      INTO TABLE GT_T005U
      WHERE SPRAS = 2
        AND LAND1 = 'TH'.
  ENDMETHOD.
  METHOD GET_CUST_DISTRICT.
    DATA : LV_DATA LIKE C_DATA-CITY2.
    DATA : LV_PROVINCE LIKE C_DATA-REGION.

    IF C_DATA-REGION IS INITIAL.
      C_DATA-REGION = GET_CUST_PROVINCE( C_DATA ).
    ENDIF.

    LV_DATA = C_DATA-CITY2.

    REPLACE ALL OCCURRENCES OF 'เขต'    IN LV_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF 'อำเภอ' IN LV_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF 'อ.'      IN LV_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_DATA WITH ''.

    READ TABLE GT_ZSDSFIC017 INTO DATA(LS_DATA)
    WITH KEY DIST_NAME     = LV_DATA
             PROVINCE_CODE = C_DATA-REGION.
    IF SY-SUBRC EQ 0.
      R = LS_DATA-DIST_CODE.
    ELSE.
      R = 'N/A'.
    ENDIF.

    IF C_DATA-REGION IS INITIAL.
      C_DATA-REGION = 'N/A'.
    ENDIF.

  ENDMETHOD.
  METHOD GET_CUST_SUB_DISTRICT.
    DATA : LV_DATA LIKE I_DATA-LOCATION.

    LV_DATA = I_DATA-LOCATION.

    IF LV_DATA IS INITIAL.
*      IF I_DATA-STR_SUPPL1 IS NOT INITIAL.
*        LV_DATA = I_DATA-STR_SUPPL1.
*      ELSEIF I_DATA-STR_SUPPL2 IS NOT INITIAL.
*        LV_DATA = I_DATA-STR_SUPPL2.
*      ELSEIF I_DATA-STR_SUPPL3 IS NOT INITIAL.
*        LV_DATA = I_DATA-STR_SUPPL3.
*      ENDIF.

      IF I_DATA-STR_SUPPL2 IS NOT INITIAL.
        LV_DATA = I_DATA-STR_SUPPL2.
      ENDIF.

    ENDIF.

    REPLACE ALL OCCURRENCES OF 'แขวง'   IN LV_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF 'ตำบล'  IN LV_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF 'ต.'       IN LV_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_DATA WITH ''.

    READ TABLE GT_ZSDSFIC020 INTO DATA(LS_DATA)
    WITH KEY SUB_DIST_NAME = LV_DATA
             DIST_CODE     = I_DIST
             PROVINCE_CODE = I_DATA-REGION.
    IF SY-SUBRC EQ 0.
      R = LS_DATA-SUB_DIST_CODE.
    ELSE.
      R = 'N/A'.
    ENDIF.
  ENDMETHOD.
  METHOD GET_CUST_PROVINCE.

    DATA : LV_DATA LIKE I_DATA-CITY1.
    LV_DATA = I_DATA-CITY1.

    REPLACE ALL OCCURRENCES OF 'จังหวัด'   IN LV_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF 'จ.'         IN LV_DATA WITH ''.

    IF     LV_DATA EQ 'กรุงเทพฯ'.
      LV_DATA = 'กรุงเทพ'.
    ELSEIF LV_DATA EQ 'กรุงเทพมหานคร'.
      LV_DATA = 'กรุงเทพ'.
    ENDIF.

    IF     LV_DATA EQ 'อยุธยา'.
      LV_DATA = 'พระนครศรีอยุธยา'.
    ENDIF.

    READ TABLE GT_T005U INTO DATA(LS_DATA)
    WITH KEY BEZEI = LV_DATA.
    IF SY-SUBRC EQ 0.
      R = LS_DATA-BLAND.
    ELSE.
      R = 'N/A'.
    ENDIF.

  ENDMETHOD.
  METHOD CANCEL_DOC.
    DATA : LS_ZSDSFIT051 TYPE ZSDSFIT051.

    LS_ZSDSFIT051-MESSG = TEXT-S02.
    LS_ZSDSFIT051-STATU = GC_CON-WARNING.
    LS_ZSDSFIT051-CANRE = ABAP_TRUE.
    GS_RESULT-STATU = GC_CON-WARNING.
    GS_RESULT-MESSG = TEXT-S01.
    GS_RESULT-CANRE = ABAP_TRUE.

    IF GS_RESULT-ERDAT IS INITIAL.
      LS_ZSDSFIT051-ERNAM = SY-UNAME.
      LS_ZSDSFIT051-ERDAT = SY-DATUM.
      LS_ZSDSFIT051-ERZET = SY-UZEIT.
    ELSE.
      LS_ZSDSFIT051-ERNAM = GS_RESULT-ERNAM.
      LS_ZSDSFIT051-ERDAT = GS_RESULT-ERDAT.
      LS_ZSDSFIT051-ERZET = GS_RESULT-ERZET.
    ENDIF.
    LS_ZSDSFIT051-DOCNO = GS_RESULT-DOCNO.
    LS_ZSDSFIT051-DYEAR = GS_RESULT-GJAHR.

    LS_ZSDSFIT051-AENAM = SY-UNAME.
    LS_ZSDSFIT051-AEDAT = SY-DATUM.
    LS_ZSDSFIT051-AEZET = SY-UZEIT.
    MODIFY ZSDSFIT051 FROM LS_ZSDSFIT051.
    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD GET_DATA_OT01.
    CLEAR : GT_ONE_TIME_CUST.
    DATA : LR_KUNNR TYPE RANGE OF KNA1-KUNNR.

    LR_KUNNR =  VALUE #( ( SIGN  = 'I' OPTION = 'CP' LOW = 'OT*' ) ).

    SELECT VBRP~VBELN,
           VBPA~ADRNR
*           ADR2~TEL_NUMBER,
*           ADR6~SMTP_ADDR
      FROM VBRK
      INNER JOIN VBRP ON VBRK~VBELN   EQ VBRP~VBELN
      INNER JOIN VBPA ON VBRP~VBELN   EQ VBPA~VBELN    AND
                         VBPA~POSNR   EQ '000000' AND
                         VBPA~PARVW   EQ 'RE'
*      LEFT JOIN ADR2 ON VBPA~ADRNR   EQ ADR2~ADDRNUMBER AND
*                         ADR2~R3_USER EQ '3'
*      LEFT JOIN ADR6 ON VBPA~ADRNR   EQ ADR2~ADDRNUMBER
      FOR ALL ENTRIES IN @GT_RESULT
      WHERE VBRK~VBELN EQ @GT_RESULT-DOCNO
        AND VBRK~KUNRG IN @LR_KUNNR[]
      INTO TABLE @GT_ONE_TIME_CUST.

    SELECT VBELN,
           STCD3
      FROM VBPA3
      FOR ALL ENTRIES IN @GT_RESULT
      WHERE VBELN EQ @GT_RESULT-DOCNO
        AND POSNR EQ '000000'
        AND PARVW EQ 'AG'
      INTO TABLE @GT_ONE_TIME_CUST_ID.

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
