*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0600_CLASS
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
      RELEASE_PO IMPORTING I_DATA   TYPE EKKO-EBELN
                 RETURNING VALUE(R) TYPE FLAG,
      UPDATE_STATUS IMPORTING I_DATA   TYPE EKKO-EBELN
                    RETURNING VALUE(R) TYPE FLAG,
      COMMIT,
      SEND_DATA,
      GET_VENDOR_HOLD RETURNING VALUE(R) TYPE LFA1-LIFNR,
      UPDATE_STATUS_DO IMPORTING IT_DATA TYPE ZSDSMMS042_TT.
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

    SELECT MMIM_PREDOC_ORG~GUID ,
           MMIM_PREDOC_ORG~REFID,
           MMIM_PRED~FIELD,
           MMIM_PRED~VALUE
      FROM MMIM_PREDOC_ORG
      INNER JOIN MMIM_PRED ON MMIM_PREDOC_ORG~GUID EQ MMIM_PRED~GUID
      WHERE MMIM_PREDOC_ORG~REFID IN @S_REFID
        AND MMIM_PREDOC_ORG~UNAME IN @S_UNAME
        AND MMIM_PREDOC_ORG~DATLO IN @S_DATLO
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
  METHOD RELEASE_PO.
    DATA : I_EBELN TYPE  EKKO-EBELN,
           I_EBELP TYPE  EKPO-EBELP,
           I_BSTYP TYPE  EKKO-BSTYP,
           I_FRGCO TYPE  FRGCO.

    DATA : PURCHASEORDER  TYPE  BAPIMMPARA-PO_NUMBER,
           PO_REL_CODE    TYPE  BAPIMMPARA-PO_REL_COD,
           USE_EXCEPTIONS TYPE  BAPIMMPARA-SELECTION,
           NO_COMMIT      TYPE  BAPIMMPARA-SELECTION.

    DATA : BEGIN OF LS_T16FS,
             FRGC1 TYPE T16FS-FRGC1,
           END OF LS_T16FS.

    DATA : LV_MESSAGE TYPE C LENGTH 255.

    I_EBELN = I_DATA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_EBELN
      IMPORTING
        OUTPUT = I_EBELN.

    SELECT SINGLE FRGC1
      FROM EKKO
      INNER JOIN T16FS ON EKKO~FRGGR EQ T16FS~FRGGR AND
                          EKKO~FRGSX EQ T16FS~FRGSX
      INTO LS_T16FS-FRGC1
      WHERE EBELN EQ I_EBELN.

    PURCHASEORDER  = I_EBELN.
    PO_REL_CODE    = LS_T16FS-FRGC1.

    CALL FUNCTION 'BAPI_PO_RELEASE'
      EXPORTING
        PURCHASEORDER          = PURCHASEORDER
        PO_REL_CODE            = PO_REL_CODE
      EXCEPTIONS
        AUTHORITY_CHECK_FAIL   = 1
        DOCUMENT_NOT_FOUND     = 2
        ENQUEUE_FAIL           = 3
        PREREQUISITE_FAIL      = 4
        RELEASE_ALREADY_POSTED = 5
        RESPONSIBILITY_FAIL    = 6
        OTHERS                 = 7.
    IF SY-SUBRC <> 0.
      CONCATENATE I_EBELN ':' SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO LV_MESSAGE SEPARATED BY SPACE.
      MESSAGE S001 WITH LV_MESSAGE.

      CLEAR : LV_MESSAGE.
      CONCATENATE I_EBELN ':' 'Cannot release' INTO LV_MESSAGE SEPARATED BY SPACE.
      MESSAGE S001 WITH LV_MESSAGE.
      R = ABAP_TRUE.
    ELSE.
      UPDATE ZSDSMMT002 SET RELES = SPACE
                      WHERE EBELN EQ I_EBELN
                        AND STATU EQ 'COM'
                        AND FLAGD EQ SPACE.
      CLEAR : R.
    ENDIF.

    COMMIT( ).
  ENDMETHOD.
  METHOD UPDATE_STATUS.
    CONSTANTS : LC_SAVE TYPE C LENGTH 3 VALUE 'SUB'.

    DATA : LT_TMP LIKE GT_ZSDSMMT002.

    LOOP AT GT_ZSDSMMT002 ASSIGNING FIELD-SYMBOL(<LS_DATA>) WHERE EBELN EQ I_DATA.
      <LS_DATA>-FLAGD = SPACE.
      APPEND <LS_DATA> TO LT_TMP.
      IF <LS_DATA>-STATU EQ LC_SAVE.
        EXIT.
      ENDIF.
    ENDLOOP.

    MODIFY ZSDSMMT002 FROM TABLE LT_TMP.
  ENDMETHOD.
  METHOD COMMIT.
    COMMIT WORK AND WAIT.
  ENDMETHOD.
  METHOD SEND_DATA.
    DATA : LV_VENDOR TYPE LFA1-LIFNR.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA : LCL_API TYPE REF TO ZCL_SDSSD_SEND_API.

    DATA : LV_STATUS TYPE FLAG.

    DATA : LV_MESSAGE TYPE C LENGTH 255.

    DATA : LS_DATA TYPE ZSDSMMS042,
           LT_DATA TYPE TABLE OF ZSDSMMS042.

    DATA : LV_UMMAT_KDAUF TYPE STRING,
           LV_UMMAT_KDPOS TYPE STRING.

    DATA: BEGIN OF LS_RETURN,
            IS_SUCCESS TYPE STRING,
            STATUS     TYPE P DECIMALS 0,
            MESSAGE    TYPE STRING,
          END OF LS_RETURN.

    DATA : LV_ADDRESS TYPE STRING.

    IF LCL_UTIL IS NOT BOUND.
      CREATE OBJECT LCL_UTIL.
    ENDIF.

    LV_VENDOR = LCL_DATA=>GET_VENDOR_HOLD( ).
    LV_VENDOR = |{ LV_VENDOR ALPHA = IN }|.
    LCL_UTIL->GET_VEND_NAME( EXPORTING I_VEND_NO  = LV_VENDOR
                             IMPORTING E_NAME_ALL = DATA(LV_NAME) ).

    SELECT SINGLE ADRNR
      FROM LFA1
      INTO @DATA(LV_ADRNR)
      WHERE LIFNR EQ @LV_VENDOR.

    LCL_UTIL->GET_ADDRESS( EXPORTING I_ADDRESS_NO = LV_ADRNR
                           IMPORTING E_ADRC                 = DATA(LS_ADRC)
                                     E_ADDRESS_CONCAT_LINE1 = DATA(LV_ADD1)
                                     E_ADDRESS_CONCAT_LINE2 = DATA(LV_ADD2)
                                     E_ADDRESS_CONCAT_LINE3 = DATA(LV_ADD3)
                                     E_ADDRESS_CONCAT_LINE4 = DATA(LV_ADD4) ).

    DATA : LV_DATE TYPE SY-DATUM.

    CONCATENATE LV_ADD1
                LV_ADD2
                LV_ADD3
                LV_ADD4
           INTO LV_ADDRESS SEPARATED BY SPACE.


*    SORT GT_RESULT BY GUID.
    LOOP AT GT_RESULT INTO DATA(LS_TMP).
      MOVE-CORRESPONDING LS_TMP TO GS_RESULT.

      AT NEW GUID.
        CLEAR : LV_DATE.
      ENDAT.

      LS_DATA-LFART           = '1'.
      LS_DATA-CUST_CODE       = 'SDS'.
      LS_DATA-VBELN           = GS_RESULT-REFID.
      IF GS_RESULT-FIELD = 'BLDAT'.
        LV_DATE                 = GS_RESULT-VALUE.
      ENDIF.
      LS_DATA-ERDAT           = SY-DATUM.
      IF GS_RESULT-FIELD = 'ZEILE'.
        LS_DATA-POSNR           = GS_RESULT-VALUE.
      ENDIF.
      IF GS_RESULT-FIELD = 'MATNR'.
        LS_DATA-MATNR           = GS_RESULT-VALUE.
      ENDIF.
*      LS_DATA-ARKTX           = ''.
      IF GS_RESULT-FIELD = 'ERFMG'.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_RESULT-VALUE WITH ''.
        LS_DATA-LGMNG           = GS_RESULT-VALUE.
      ENDIF.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA-LGMNG WITH ''.
      LS_DATA-KUNNR           = LV_VENDOR.
      LS_DATA-CUST_NAME       = LV_NAME.
      LS_DATA-DELI_TO         = LV_ADD1.
*      LS_DATA-REMARK          = ''.
*      LS_DATA-PICHON          = ''.
*      LS_DATA-REMARK_PICHON   = ''.
      IF GS_RESULT-FIELD = 'LGORT'.
        LS_DATA-LGORT           = GS_RESULT-VALUE.
      ENDIF.
*      LS_DATA-WARRANTY        = ''.
      IF GS_RESULT-FIELD = 'UMMAT_KDAUF'.
        LV_UMMAT_KDAUF = GS_RESULT-VALUE.
      ENDIF.
      IF GS_RESULT-FIELD = 'UMMAT_KDPOS'.
        LV_UMMAT_KDPOS = GS_RESULT-VALUE.
      ENDIF.

      IF GS_RESULT-FIELD = 'SGTXT'.
        LS_DATA-REMARK = GS_RESULT-VALUE.
      ENDIF.

*      CONCATENATE LS_GOITEM-UMMAT_KDAUF
*                  LS_GOITEM-UMMAT_KDPOS
*             INTO LS_DATA-SO SEPARATED BY '/'.
*      LS_DATA-REQ_MAP         = ''.
*      LS_DATA-REQ_INV         = ''.
      LS_DATA-SHIP_ADDR       = LV_ADDRESS.
      LS_DATA-SHIP_PROVINCE   = LS_ADRC-CITY1.
      LS_DATA-POSTCODE        = LS_ADRC-POST_CODE1.
*      LS_DATA-AM_PM           = ''.
      LS_DATA-LOADING_POINT   = 'SONY'.
*      LS_DATA-SALESMAN        = ''.
*      LS_DATA-DUE_DATE        = ''.
*      LS_DATA-PO_NO           = ''.
*      LS_DATA-PROJECT         = ''.
*      LS_DATA-TERM_OF_PAYMENT = ''.
*      LS_DATA-CONTACT_NAME    = ''.
*      LS_DATA-REF_INV         = ''.
*      LS_DATA-SALES_DIV       = ''.
*      LS_DATA-SALES_OFFICE    = ''.
*      LS_DATA-SALES_GROUP     = ''.
*      LS_DATA-UOM             = ''.
*      LS_DATA-DOC_FLAG        = ''.
*      LS_DATA-MHA             = ''.
*      LS_DATA-FLAG_BOM        = ''.
*      LS_DATA-REFER_BOM       = ''.
*      LS_DATA-SPSOLC          = ''.
      IF GS_RESULT-FIELD EQ 'XTRANSFER_MVT'.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_UMMAT_KDAUF WITH ''.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_UMMAT_KDPOS WITH ''.
        CONCATENATE LV_UMMAT_KDAUF
                    LV_UMMAT_KDPOS
               INTO LS_DATA-SO SEPARATED BY '/'.

        LS_DATA-LFDAT = LV_DATE.
        APPEND LS_DATA TO LT_DATA.
        CLEAR : LV_UMMAT_KDAUF,LV_UMMAT_KDPOS,LS_DATA.
      ENDIF.
    ENDLOOP.

    IF LT_DATA IS NOT INITIAL.
      IF LCL_API IS NOT BOUND.
        CREATE OBJECT LCL_API.
      ENDIF.
      LCL_API->SEND_DO_FG_TO_SONY( EXPORTING IT_DATA   = LT_DATA
                                   IMPORTING E_MESTYPE = DATA(E_TYPE)
                                   CHANGING  C_RETURN  = LS_RETURN ).
      IF LS_RETURN-IS_SUCCESS EQ ABAP_TRUE.
        LCL_DATA=>UPDATE_STATUS_DO( LT_DATA ).
        MESSAGE S001 WITH TEXT-S01.
      ELSE.
        MESSAGE S001 WITH LS_RETURN-MESSAGE DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 1000.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD GET_VENDOR_HOLD.
    CONSTANTS : BEGIN OF LC_CON,
                  REPID TYPE C LENGTH 22 VALUE 'SONY_HOLD_DATA',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  F36   TYPE C LENGTH 3  VALUE 'F36',
                  F46   TYPE C LENGTH 3  VALUE 'F46',
                  F56   TYPE C LENGTH 3  VALUE 'F56',
                  PARAE TYPE C LENGTH 10 VALUE 'VENDOR',
                END OF LC_CON.

    CASE SY-SYSID.
      WHEN LC_CON-F36.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-DEV
                                                      I_PARAM_EXT         = LC_CON-PARAE
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-F46.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-QAS
                                                      I_PARAM_EXT         = LC_CON-PARAE
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-F56.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_CON-PRD
                                                      I_PARAM_EXT         = LC_CON-PARAE
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD UPDATE_STATUS_DO.
    DATA : ls_ZSDSSDT001 TYPE ZSDSSDT001,
           ls_ZSDSSDT002 TYPE ZSDSSDT002.

    DATA : LT_TMP LIKE IT_DATA[].

    LT_TMP = IT_DATA[].

    SORT LT_TMP BY VBELN.
    DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING VBELN.

    LOOP AT LT_TMP INTO DATA(LS_TMP).
      LS_ZSDSSDT001-DONUM = LS_TMP-VBELN.
      LS_ZSDSSDT001-WMSDT = ''.
      LS_ZSDSSDT001-WMSTM = ''.
      LS_ZSDSSDT001-STAPC = ''.
      LS_ZSDSSDT001-STAPT = ''.
      LS_ZSDSSDT001-LOADD = ''.
      LS_ZSDSSDT001-LOADT = ''.
      LS_ZSDSSDT001-CONTD = ''.
      LS_ZSDSSDT001-CONTS = ''.
      LS_ZSDSSDT001-CONTP = ''.
      LS_ZSDSSDT001-CONTT = ''.
      LS_ZSDSSDT001-DOSIG = ''.
      LS_ZSDSSDT001-PODDA = ''.
      LS_ZSDSSDT001-PODRE = ''.
      LS_ZSDSSDT001-DOSDS = ''.
      LS_ZSDSSDT001-DOSDT = ''.
      LS_ZSDSSDT001-ORDDA = ''.
      LS_ZSDSSDT001-DEALC = ''.
      LS_ZSDSSDT001-DEALN = ''.
      LS_ZSDSSDT001-REMAK = LS_TMP-REMARK.
      LS_ZSDSSDT001-SHPAD = LS_TMP-SHIP_ADDR.
      LS_ZSDSSDT001-SHPPV = LS_TMP-SHIP_PROVINCE.
      LS_ZSDSSDT001-AMPMF = LS_TMP-AM_PM.
      LS_ZSDSSDT001-LOADP = LS_TMP-LOADING_POINT.
      LS_ZSDSSDT001-REMAP = LS_TMP-REQ_MAP.
      LS_ZSDSSDT001-REINV = LS_TMP-REQ_INV.
      LS_ZSDSSDT001-SERNF = ''.
      LS_ZSDSSDT001-MAPST = ''.
      LS_ZSDSSDT001-EDOTM = ''.
      LS_ZSDSSDT001-DIVNM = ''.
      LS_ZSDSSDT001-PLTNO = ''.
      LS_ZSDSSDT001-CARNA = ''.
      LS_ZSDSSDT001-TUKTY = ''.
      LS_ZSDSSDT001-SHPNO = ''.
      LS_ZSDSSDT001-ERNAM = SY-UNAME.
      LS_ZSDSSDT001-ERDAT = SY-DATUM.
      LS_ZSDSSDT001-ERZET = SY-UZEIT.
      LS_ZSDSSDT001-AENAM = SY-UNAME.
      LS_ZSDSSDT001-AEDAT = SY-DATUM.
      LS_ZSDSSDT001-AEZET = SY-UZEIT.
      MODIFY ZSDSSDT001 FROM LS_ZSDSSDT001.

      LS_ZSDSSDT002-VBELN   = LS_TMP-VBELN.
      LS_ZSDSSDT002-RUN_ID  = 1.
      LS_ZSDSSDT002-REQ_INV = LS_TMP-REQ_INV.
      LS_ZSDSSDT002-REQ_MAP = LS_TMP-REQ_MAP.
      LS_ZSDSSDT002-AM_PM   = LS_TMP-AM_PM  .
      LS_ZSDSSDT002-ERDAT   = SY-DATUM.
      LS_ZSDSSDT002-ERZET   = SY-UZEIT.
      MODIFY ZSDSSDT002 FROM LS_ZSDSSDT002.
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
