*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0800_CLASS
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
      DEL_LINE.

    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    GS_HEADER-LANGUAGE = 'TH'.
    GS_HEADER-DOC_NAME_TH = 'ใบลดหนี้'.
    GS_HEADER-DOC_NAME_EN = 'CREDIT NOTE'.
    GS_HEADER-HOLDING_TAX = 'หักภาษีเงินได้ ณ ที่จ่าย 0.0000%'.
    GS_HEADER-FI_DOC = '4220008802'.

  ENDMETHOD.
  METHOD GET_DATA.
    IF LO IS INITIAL.
      CREATE OBJECT LO.
    ENDIF.

    LO->START_PROCESS( ).
  ENDMETHOD.
  METHOD START_PROCESS.
    CALL SCREEN 101.
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
  METHOD PRINT.

    DATA : LV_FM_NAME TYPE RS38L_FNAM.

    DATA : LS_HEADER TYPE ZSDSFIS191.

    DATA : LT_DETAIL TYPE ZSDSFIS192_TT,
           LS_DETAIL TYPE ZSDSFIS192.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = 'ZSDSFI016'
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

    LS_HEADER-DOC_NUMBER    = GS_HEADER-DOC_NUMBER.
    LS_HEADER-LANGUAGE      = GS_HEADER-LANGUAGE.
    LS_HEADER-CUSTOMER      = GS_HEADER-CUSTOMER.
    LS_HEADER-ADRESS1       = GS_HEADER-ADRESS1.
    LS_HEADER-ADRESS2       = GS_HEADER-ADRESS2.
    LS_HEADER-ADRESS3       = GS_HEADER-ADRESS3.
    LS_HEADER-ADRESS4       = GS_HEADER-ADRESS4.
    LS_HEADER-INVOICE_TAX   = GS_HEADER-INVOICE_TAX.

    IF GS_HEADER-INVOICE_DATE IS NOT INITIAL.
      LS_HEADER-INVOICE_DATE  = GS_HEADER-INVOICE_DATE.
    ELSE.
      CLEAR : LS_HEADER-INVOICE_DATE.
    ENDIF.
    LS_HEADER-CUST_CODE     = GS_HEADER-CUST_CODE.
    LS_HEADER-REMARK1       = GS_HEADER-REMARK1.
    LS_HEADER-REMARK2       = GS_HEADER-REMARK2.
    LS_HEADER-REMARK3       = GS_HEADER-REMARK3.
    LS_HEADER-REMARK4       = GS_HEADER-REMARK4.
    LS_HEADER-TOTAL_TEXT    = GS_HEADER-TOTAL_TEXT.
    LS_HEADER-DOC_NAME_TH   = GS_HEADER-DOC_NAME_TH.
    LS_HEADER-DOC_NAME_EN   = GS_HEADER-DOC_NAME_EN.
    LS_HEADER-HOLDING_TAX   = GS_HEADER-HOLDING_TAX.
    LS_HEADER-FI_DOC        = GS_HEADER-FI_DOC.
    LS_HEADER-CUSTOMER2     = GS_HEADER-CUSTOMER2.
    IF GS_HEADER-DATE  IS NOT INITIAL.
      WRITE GS_HEADER-DATE          TO LS_HEADER-DATE.
    ENDIF.
    WRITE GS_HEADER-PRICE_ORG_INV TO LS_HEADER-PRICE_ORG_INV.
    WRITE GS_HEADER-ADV_REC       TO LS_HEADER-ADV_REC.
    WRITE GS_HEADER-TOTAL_AMT     TO LS_HEADER-TOTAL_AMT.
    WRITE GS_HEADER-PRICE_DUC     TO LS_HEADER-PRICE_DUC.
    WRITE GS_HEADER-VAL_ADDTAX    TO LS_HEADER-VAL_ADDTAX.
    WRITE GS_HEADER-NET_TOTAL     TO LS_HEADER-NET_TOTAL.

    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_HEADER-DATE          WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_HEADER-PRICE_ORG_INV WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_HEADER-ADV_REC       WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_HEADER-TOTAL_AMT     WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_HEADER-PRICE_DUC     WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_HEADER-VAL_ADDTAX    WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_HEADER-NET_TOTAL     WITH ''.

    LOOP AT GT_DETAIL INTO GS_DETAIL.
      LS_DETAIL-ITEM        = GS_DETAIL-ITEM.
      LS_DETAIL-MODEL       = GS_DETAIL-MODEL.
      LS_DETAIL-DES         = GS_DETAIL-DES.
      WRITE GS_DETAIL-QTY         TO LS_DETAIL-QTY.
      WRITE GS_DETAIL-UNIT_PRICE  TO LS_DETAIL-UNIT_PRICE.
      WRITE GS_DETAIL-DISCOUNT    TO LS_DETAIL-DISCOUNT.
      WRITE GS_DETAIL-NET_UNIT    TO LS_DETAIL-NET_UNIT.
      WRITE GS_DETAIL-NET_AMOUNT  TO LS_DETAIL-NET_AMOUNT.

      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-QTY        WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-UNIT_PRICE WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-DISCOUNT   WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-NET_UNIT   WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-NET_AMOUNT WITH ''.

      APPEND LS_DETAIL TO LT_DETAIL.
    ENDLOOP.


    CALL FUNCTION LV_FM_NAME
      EXPORTING
        GS_HEADER = LS_HEADER
        GT_DETAIL = LT_DETAIL.






  ENDMETHOD.
  METHOD DEL_LINE.

    DATA LV_LANGU TYPE SY-LANGU.

    DELETE GT_DETAIL WHERE CHECK EQ ABAP_TRUE.

    CLEAR : GS_HEADER-NET_TOTAL,GS_HEADER-VAL_ADDTAX.

    LOOP AT GT_DETAIL INTO GS_DETAIL.

      ADD GS_DETAIL-NET_AMOUNT TO GS_HEADER-NET_TOTAL.
      "ADD GS_DETAIL-VAT_TMP TO GS_HEADER-VAL_ADDTAX.

    ENDLOOP.

    GS_HEADER-VAL_ADDTAX = GS_HEADER-NET_TOTAL * 7 / 100.
    GS_HEADER-NET_TOTAL  = GS_HEADER-NET_TOTAL + GS_HEADER-VAL_ADDTAX.

    IF GS_HEADER-LANGUAGE EQ 'TH'.
      LV_LANGU = '2'.
    ELSE.
      LV_LANGU = 'E'.
    ENDIF.

    CALL FUNCTION 'Z_SDSCA_SPELL_AMOUNT'
      EXPORTING
        AMOUNT     = GS_HEADER-NET_TOTAL
        CURRENCY   = 'THB'
        LANGUAGE   = LV_LANGU
      IMPORTING
        SPELL_WORD = GS_HEADER-TOTAL_TEXT
      EXCEPTIONS
        NOT_FOUND  = 1
        TOO_LARGE  = 2
        OTHERS     = 3.
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
