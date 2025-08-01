*&---------------------------------------------------------------------*
*& Include          ZSDSSDR0630_CLASS
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
      POST_DO CHANGING C_DATA TYPE GY_RESULT,
      CREATE_INV CHANGING C_DATA TYPE GY_RESULT,
      SEND_DOCUMETN CHANGING C_DATA TYPE GY_RESULT,
      PRINT_INV CHANGING C_DATA TYPE GY_RESULT,
      UPDATE_ZSDSSDT025 CHANGING C_DATA TYPE GY_RESULT,
      GET_UPDATE_FILE_SONY,
      ENDPOINT RETURNING VALUE(R) TYPE STRING,
      HEADER_API IMPORTING I_DATA   TYPE STRING
                 RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY_API RETURNING VALUE(R)   TYPE STRING,
      BODY_API_LEN IMPORTING I_DATA   TYPE STRING
                   RETURNING VALUE(R) TYPE I,
      NORMAL_CASE,
      MHA_CASE,
      SET_BATCH,
      RUN_MHA_AGAIN.

    CLASS-DATA :
      LO TYPE REF TO LCL_DATA.

    CONSTANTS : BEGIN OF LC_CON,
                  S   TYPE C LENGTH 1 VALUE 'S',
                  E   TYPE C LENGTH 1 VALUE 'E',
                  DEV TYPE C LENGTH 3 VALUE 'F36',
                  QAS TYPE C LENGTH 3 VALUE 'F46',
                  PRD TYPE C LENGTH 3 VALUE 'F56',
                END OF LC_CON.
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
    IF p_MHA EQ ABAP_TRUE.
      MHA_CASE( ).
    ELSE.
      NORMAL_CASE( ).
    ENDIF.
  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.
*    FIELD-SYMBOLS <LFS_RESULT> LIKE LINE OF GT_RESULT.
*    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
*
*    ENDLOOP.

    SELECT SINGLE SFORM
    FROM TNAPR
    WHERE KSCHL EQ @P_KSCHL
      AND NACHA EQ '1'
      AND KAPPL EQ 'V3'
    INTO @GV_FORM.

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
    LOOP AT gt_Result ASSIGNING FIELD-SYMBOL(<LFS_DATA>)." WHERE WBSTK EQ GC_C.
      POST_DO( CHANGING C_DATA = <LFS_DATA> ).
      IF P_GIS EQ ABAP_FALSE.
        CREATE_INV( CHANGING C_DATA = <LFS_DATA> ).
        SEND_DOCUMETN( CHANGING C_DATA = <LFS_DATA>  ).
        UPDATE_ZSDSSDT025( CHANGING C_DATA = <LFS_DATA>  ).
        PRINT_INV( CHANGING C_DATA = <LFS_DATA>  ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD POST_DO.

    DATA : LS_HEADER_DATA	   TYPE	BAPIOBDLVHDRCON,
           LS_HEADER_CONTROL TYPE	BAPIOBDLVHDRCTRLCON,
           LV_DELIVERY       TYPE BAPIOBDLVHDRCON-DELIV_NUMB.

    DATA : LT_RETURN TYPE TABLE OF BAPIRET2 WITH EMPTY KEY.

    DATA : PE_RETURN1 TYPE  BAPIRETURN1.

    IF C_DATA-WBSTK NE GC_C.
      IF P_MHA EQ ABAP_TRUE.
        CALL FUNCTION 'CSO_P_DLVRY_GOODS_ISSUE_CREATE'
          EXPORTING
            PI_DELIV_NUMB = C_DATA-VBELN
          IMPORTING
            PE_RETURN1    = PE_RETURN1.

        IF PE_RETURN1-TYPE EQ 'E' OR
           PE_RETURN1-TYPE EQ 'A'.
          C_DATA-STATUS  = GC_ERRO.
          C_DATA-MESSAGE = TEXT-E01.
          RUN_MHA_AGAIN( ).
        ELSE.
          COMMIT WORK AND WAIT.
          DO 10 TIMES.
            SELECT SINGLE LIKP~WBSTK
              FROM LIKP
              WHERE VBELN EQ @C_DATA-VBELN
              INTO @C_DATA-WBSTK.
            IF C_DATA-WBSTK EQ GC_C.
              EXIT.
            ENDIF.
          ENDDO.

          IF C_DATA-WBSTK NE GC_C.
            C_DATA-STATUS  = GC_ERRO.
            C_DATA-MESSAGE = TEXT-E01.
            RUN_MHA_AGAIN( ).
          ENDIF.
        ENDIF.
      ELSE.
        SELECT COUNT( * )
          FROM ZSDSCAC009
          WHERE PROCESS EQ 'BDC_DO'
            AND STATU   EQ ABAP_TRUE.
        IF SY-SUBRC EQ 0.
          CALL FUNCTION 'CSO_P_DLVRY_GOODS_ISSUE_CREATE'
            EXPORTING
              PI_DELIV_NUMB = C_DATA-VBELN
            IMPORTING
              PE_RETURN1    = PE_RETURN1.

          IF PE_RETURN1-TYPE EQ 'E' OR
             PE_RETURN1-TYPE EQ 'A'.
            C_DATA-STATUS  = GC_ERRO.
            C_DATA-MESSAGE = TEXT-E01.
          ELSE.
            COMMIT WORK AND WAIT.
            DO 10 TIMES.
              SELECT SINGLE LIKP~WBSTK
                FROM LIKP
                WHERE VBELN EQ @C_DATA-VBELN
                INTO @C_DATA-WBSTK.
              IF C_DATA-WBSTK EQ GC_C.
                EXIT.
              ENDIF.
            ENDDO.

            IF C_DATA-WBSTK NE GC_C.
              C_DATA-STATUS  = GC_ERRO.
              C_DATA-MESSAGE = TEXT-E01.
            ENDIF.
          ENDIF.
        ELSE.
          PERFORM F_BDC_DATA USING C_DATA.
        ENDIF.
      ENDIF.
*      LV_DELIVERY                   = C_DATA-VBELN.
*      LS_HEADER_DATA-DELIV_NUMB     = LV_DELIVERY.
*      LS_HEADER_CONTROL-DELIV_NUMB  = LV_DELIVERY.
*      LS_HEADER_CONTROL-POST_GI_FLG = ABAP_TRUE.

*      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CONFIRM_DEC'
*        EXPORTING
*          HEADER_DATA    = LS_HEADER_DATA
*          HEADER_CONTROL = LS_HEADER_CONTROL
*          DELIVERY       = LV_DELIVERY
*        TABLES
*          RETURN         = LT_RETURN
*        EXCEPTIONS
*          ERROR_MESSAGE  = 1
*          OTHERS         = 2.
*
*      SELECT COUNT(*)
*        FROM @LT_RETURN AS A
*        WHERE TYPE EQ 'A'
*           OR TYPE EQ 'E'.
*      IF SY-SUBRC EQ 0.
*        C_DATA-STATUS  = GC_ERRO.
*        C_DATA-MESSAGE = TEXT-E01.
*      ELSE.
*        C_DATA-WBSTK = GC_C.
*        COMMIT WORK AND WAIT.
*      ENDIF.

    ENDIF.
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

    IF C_DATA-WBSTK EQ GC_C.
      LS_BILLINGDATAIN-REF_DOC    = C_DATA-VBELN.
      LS_BILLINGDATAIN-REF_DOC_CA = 'J'.
      LS_BILLINGDATAIN-BILL_DATE  = C_DATA-SMARD.

      APPEND LS_BILLINGDATAIN TO LT_BILLINGDATAIN.

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
        INTO @C_DATA-MESSAGE.
      IF SY-SUBRC EQ 0.
        C_DATA-STATUS  = GC_ERRO.
        IF P_MHA EQ ABAP_TRUE.
          RUN_MHA_AGAIN( ).
        ENDIF.
      ELSE.
        READ TABLE LT_SUCCESS_TMP INTO DATA(LS_SUCC) INDEX 1.
        IF SY-SUBRC EQ 0.
          C_DATA-STATUS  = GC_SUCS.
          C_DATA-MESSAGE = LS_SUCC-BILL_DOC.
          C_DATA-INVNO   = LS_SUCC-BILL_DOC.
          COMMIT WORK AND WAIT.
        ELSE.
          C_DATA-STATUS  = GC_ERRO.
          C_DATA-MESSAGE = TEXT-E02.
          IF P_MHA EQ ABAP_TRUE.
            RUN_MHA_AGAIN( ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD SEND_DOCUMETN.
    DATA : LS_ZSDSFIT027 TYPE ZSDSFIT027.

    IF C_DATA-STATUS EQ GC_SUCS.
      LS_ZSDSFIT027-VBELN  = C_DATA-INVNO.
      LS_ZSDSFIT027-ZCHECK = '1'.
      MODIFY ZSDSFIT027 FROM LS_ZSDSFIT027.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.
  METHOD PRINT_INV.
    IF C_DATA-STATUS EQ GC_SUCS.
      IF P_MHA EQ abap_True.
        CALL FUNCTION 'Z_SDSSD_CALL_PRINT_OUTPUT'
          EXPORTING
            I_OUTPUT   = P_KSCHL
            I_FORM     = GV_FORM
            I_DOC      = C_DATA-INVNO
            I_CUS_CODE = C_DATA-KUNNR
            I_ADDRESS  = C_DATA-ADRNR
            I_MHA      = C_DATA-KVGR5.
      ENDIF.
*      PERFORM F_PRINT USING C_DATA.
    ENDIF.
*    DATA RG_VBELN TYPE RANGE OF VBRK-VBELN.
*
*    RG_VBELN =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = C_DATA-VBELN ) ).
*
*    SUBMIT ZSDSSDR0010 USING SELECTION-SCREEN  1000
*                            WITH RG_KSCHL EQ P_KSCHL
*                            WITH RG_VBELN IN RG_VBELN[]
**                            WITH PM_ALLEL EQ ABAP_TRUE
**                            WITH PM_ALLEA EQ ABAP_TRUE
**                            WITH PM_ALLEB EQ ABAP_TRUE
**                            WITH PM_ALLEI EQ ABAP_TRUE
**                            WITH PM_ALLEF EQ ABAP_TRUE
**                            WITH PM_ALLED EQ ABAP_TRUE
**                            WITH PM_ETAXM EQ ABAP_TRUE
*                            AND RETURN."VIA SELECTION-SCREEN.
  ENDMETHOD.
  METHOD  GET_UPDATE_FILE_SONY.
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
            fileN  TYPE STRING,
            STATUS TYPE STRING,
          END OF LS_DATA.
    DATA : LT_DATA LIKE TABLE OF LS_DATA.

    DATA : LV_TOKEN TYPE STRING.

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT( ).
    LT_HEADER    = LCL_DATA=>HEADER_API( LV_TOKEN ).
    LV_BODY_TEXT = LCL_DATA=>BODY_API( ).
    LV_LEN       = LCL_DATA=>BODY_API_LEN( LV_BODY_TEXT ).

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
        E_RETURN           = LT_DATA
        E_RETURN_BODY_TEXT = LV_RETURN_BODY_TEXT
        E_RETURN_BODY_BIN  = LV_RETURN_BODY_BIN
        E_MESSAGE          = LV_MESSAGE
        E_STATUS           = LV_STATUS.
    IF LV_STATUS EQ GC_CON-S.

    ELSE.

    ENDIF.

    MESSAGE S000 WITH TEXT-S01.
  ENDMETHOD.
  METHOD ENDPOINT.
    CONSTANTS : BEGIN OF LC_API,
                  REPID TYPE C LENGTH 22 VALUE 'SONY_SMART_TRACK',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                  API   TYPE C LENGTH 3  VALUE 'API',
                END OF LC_API.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-DEV
                                                      I_PARAM_EXT         = LC_API-API
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-QAS
                                                      I_PARAM_EXT         = LC_API-API
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-PRD
                                                      I_PARAM_EXT         = LC_API-API
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.

  ENDMETHOD.
  METHOD HEADER_API.
    DATA : LS_HEADER TYPE ZSDSCAS001.

    DATA : LV_TOKEN TYPE STRING.

    LS_HEADER-NAME  = 'Content-Type'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.

    LS_HEADER-NAME  = 'Accept'.
    LS_HEADER-VALUE = 'application/json'.
    APPEND LS_HEADER TO R.
    CLEAR LS_HEADER.
  ENDMETHOD.
  METHOD BODY_API.

  ENDMETHOD.
  METHOD BODY_API_LEN.

  ENDMETHOD.
  METHOD UPDATE_ZSDSSDT025.
    DATA : LS_ZSDSSDT025 TYPE ZSDSSDT025.

    IF C_DATA-INVNO IS NOT INITIAL.
      IF P_MHA EQ ABAP_TRUE.
        LS_ZSDSSDT025-DO_NO = C_DATA-VBELN.
        LS_ZSDSSDT025-FLAG  = ABAP_TRUE.
        LS_ZSDSSDT025-INVNO = C_DATA-INVNO.
        LS_ZSDSSDT025-MHA   = ABAP_TRUE.
        LS_ZSDSSDT025-ERNAM = SY-UNAME.
        LS_ZSDSSDT025-ERDAT = SY-DATUM.
        LS_ZSDSSDT025-ERZET = SY-UZEIT.
        LS_ZSDSSDT025-AENAM = SY-UNAME.
        LS_ZSDSSDT025-AEDAT = SY-DATUM.
        LS_ZSDSSDT025-AEZET = SY-UZEIT.
        MODIFY ZSDSSDT025 FROM LS_ZSDSSDT025.
      ELSE.
        UPDATE ZSDSSDT025 SET FLAG  = ABAP_TRUE
                              INVNO = C_DATA-INVNO
                              AENAM = SY-UNAME
                              AEDAT = SY-DATUM
                              AEZET = SY-UZEIT
                        WHERE DO_NO EQ C_DATA-VBELN.
      ENDIF.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.
  METHOD NORMAL_CASE.
    GET_UPDATE_FILE_SONY( ).

    SELECT DISTINCT LIKP~VBELN,
                    LIKP~KUNNR,
                    LIKP~WBSTK,
                    KNA1~ADRNR,
                    A~SMART_TRACKING_DATE
      FROM LIKP
      INNER JOIN ZSDSSDT025 AS A ON LIKP~VBELN EQ A~DO_NO
      INNER JOIN LIPS            ON A~DO_NO    EQ LIPS~VBELN
      INNER JOIN VBAK            ON LIPS~VGBEL EQ VBAK~VBELN
      INNER JOIN KNA1            ON LIKP~KUNNR EQ KNA1~KUNNR
      WHERE LIKP~VBELN            IN @S_VBELN[]
        AND LIKP~GBSTK            NE @GC_C "Not yet created Invoice
        AND LIKP~ERDAT            IN @S_ERDAT[]
        AND LIKP~LSTEL            IN @S_LSTEL[]
        AND A~SMART_TRACKING_DATE IN @S_SMTDT[]
        AND VBAK~ZZPOB            IN @S_ZZPOB[]
     INTO TABLE @GT_RESULT.

    SORT GT_RESULT BY VBELN.
    DELETE ADJACENT DUPLICATES FROM GT_RESULT COMPARING VBELN.
  ENDMETHOD.
  METHOD MHA_CASE.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    SELECT COUNT( * )
      FROM ZSDSCAC009
      WHERE PROCESS EQ 'MHA'
        AND STATU   EQ ABAP_TRUE.
    IF SY-SUBRC EQ 0.
      IF S_VBELN[] IS NOT INITIAL.
        SELECT DISTINCT LIKP~VBELN,
                        LIKP~KUNNR,
                        LIKP~WBSTK,
                        KNA1~ADRNR,
                        @SY-DATUM,
                        VBAK~KVGR5
*                      LIPS~VGBEL
*                      KNA1~KDKG5
        FROM LIKP
        INNER JOIN LIPS            ON LIKP~VBELN EQ LIPS~VBELN
        INNER JOIN VBAK            ON LIPS~VGBEL EQ VBAK~VBELN
        INNER JOIN KNA1            ON LIKP~KUNNR EQ KNA1~KUNNR
        WHERE LIKP~VBELN IN @S_VBELN[]
          AND LIKP~GBSTK NE @GC_C "Not yet created Invoice
          AND LIKP~ERDAT IN @S_ERDAT[]
          AND LIKP~LSTEL IN @S_LSTEL[]
          AND VBAK~ZZPOB IN @S_ZZPOB[]
          AND ( VBAK~KVGR5 EQ 'MHA' OR
                VBAK~KVGR5 EQ 'MTD' OR
                VBAK~KVGR5 EQ 'TCR' )
       INTO TABLE @GT_RESULT.

*      IF LCL_UTIL IS NOT BOUND.
*        CREATE OBJECT LCL_UTIL.
*      ENDIF.
*
*      LOOP AT LT_TMP INTO DATA(LS_TMP).
*        GS_RESULT-MHA   = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = 'ZH18'
*                                                        I_NAME     = LS_TMP-VGBEL
*                                                        I_OBJECT   = 'VBBK'
*                                                        I_LANGUAGE = SY-LANGU ).
*        IF GS_RESULT-MHA NE ABAP_TRUE.
*          CLEAR GS_RESULT.
*          CONTINUE.
*        ENDIF.
*
*        GS_RESULT-VBELN = LS_TMP-VBELN.
*        GS_RESULT-KUNNR = LS_TMP-KUNNR.
*        GS_RESULT-WBSTK = LS_TMP-WBSTK.
*        GS_RESULT-ADRNR = LS_TMP-ADRNR.
*        APPEND GS_RESULT TO GT_RESULT.
*        CLEAR : GS_RESULT.
*      ENDLOOP.

        SORT GT_RESULT BY VBELN.
        DELETE ADJACENT DUPLICATES FROM GT_RESULT COMPARING VBELN.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD SET_BATCH.
    DATA : LV_JOBNAME  TYPE TBTCO-JOBNAME,
           LV_JOBCOUNT TYPE TBTCO-JOBCOUNT.

    DATA : LS_PRINT_PARAMETERS TYPE PRI_PARAMS.
    DATA : LS_SELTAB           TYPE TABLE OF RSPARAMS.

    DATA : C_RETURN      TYPE C LENGTH 255.

    DATA : LV_USER LIKE SY-UNAME.

    CONSTANTS : BEGIN OF LC_CON,
                  I_REPID             TYPE CHAR20 VALUE 'USER_JOB_POST_DO',
                  I_SINGLE_VALUE_FLAG TYPE FLAG   VALUE 'X',
                  I_PARAM             TYPE CHAR20 VALUE 'USER',
                END OF LC_CON.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID              = LC_CON-I_REPID
                                                  I_SINGLE_VALUE_FLAG  = LC_CON-I_SINGLE_VALUE_FLAG
                                                  I_PARAM              = LC_CON-I_PARAM
                                         CHANGING C_RETURN             = LV_USER ).

    CONCATENATE 'AUTO_INV' '_' SY-DATUM '_' SY-UZEIT INTO LV_JOBNAME.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LV_JOBNAME
      IMPORTING
        JOBCOUNT         = LV_JOBCOUNT
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.
    CASE SY-SUBRC.
      WHEN 0.
      WHEN OTHERS.
        MESSAGE E208(00) WITH 'Error'.
    ENDCASE.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
*       immediately            = 'X'
        NO_DIALOG              = 'X'
        USER                   = LV_USER
      IMPORTING
        OUT_PARAMETERS         = LS_PRINT_PARAMETERS
      EXCEPTIONS
        ARCHIVE_INFO_NOT_FOUND = 1
        INVALID_PRINT_PARAMS   = 2
        INVALID_ARCHIVE_PARAMS = 3
        OTHERS                 = 4.

    SUBMIT ZSDSSDR0630 TO SAP-SPOOL AND RETURN
                                  WITH SELECTION-TABLE LS_SELTAB
                                                  USER LV_USER
                                      SPOOL PARAMETERS LS_PRINT_PARAMETERS
                                  WITHOUT SPOOL DYNPRO
                                               VIA JOB LV_JOBNAME
                                                NUMBER LV_JOBCOUNT
                       USING SELECTION-SCREEN  1000
                             WITH P_KSCHL EQ P_KSCHL
                             WITH S_VBELN IN S_VBELN[]
                             WITH S_ERDAT IN S_ERDAT[]
                             WITH S_LSTEL IN S_LSTEL[]
                             WITH S_ZZPOB IN S_ZZPOB[]
                             WITH P_AUTO  EQ P_AUTO
                             WITH P_MHA   EQ P_MHA
                             WITH P_GIS   EQ P_GIS
                             WITH P_BAT   EQ SPACE.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LV_JOBCOUNT
        JOBNAME              = LV_JOBNAME
        STRTIMMED            = ABAP_TRUE
      EXCEPTIONS
        CANT_START_IMMEDIATE = 1
        INVALID_STARTDATE    = 2
        JOBNAME_MISSING      = 3
        JOB_CLOSE_FAILED     = 4
        JOB_NOSTEPS          = 5
        JOB_NOTEX            = 6
        LOCK_FAILED          = 7
        OTHERS               = 8.

  ENDMETHOD.
  METHOD RUN_MHA_AGAIN.
    DATA: LV_JOBNAME  TYPE TBTCO-JOBNAME,
          LV_JOBCOUNT TYPE TBTCO-JOBCOUNT.

    DATA : LS_PRINT_PARAMETERS TYPE PRI_PARAMS.
    DATA : LS_SELTAB           TYPE TABLE OF RSPARAMS.

    DATA : LV_USER LIKE SY-UNAME.

    CONSTANTS : BEGIN OF LC_CON,
                  I_REPID             TYPE CHAR20 VALUE 'USER_JOB_POST_DO',
                  I_SINGLE_VALUE_FLAG TYPE FLAG   VALUE 'X',
                  I_PARAM             TYPE CHAR20 VALUE 'USER',
                END OF LC_CON.

    DATA : CR_RETURN     TYPE RANGE OF VBAK-VBELN,
           C_RETURN      TYPE C LENGTH 255,
           C_RETURN_HIGH TYPE C LENGTH 255.

    IF P_MHA     EQ ABAP_TRUE AND
       S_VBELN[] IS NOT INITIAL.

      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID              = LC_CON-I_REPID
                                                    I_SINGLE_VALUE_FLAG  = LC_CON-I_SINGLE_VALUE_FLAG
                                                    I_PARAM              = LC_CON-I_PARAM
                                           CHANGING C_RETURN             = LV_USER
                               ).

      READ TABLE S_VBELN INTO DATA(LV_VBELN) INDEX 1.
      IF LV_VBELN IS INITIAL.
        RETURN.
      ENDIF.

      CONCATENATE 'RE_MHA' '_' LV_VBELN '_' SY-DATUM '_' SY-UZEIT INTO LV_JOBNAME.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          JOBNAME          = LV_JOBNAME
        IMPORTING
          JOBCOUNT         = LV_JOBCOUNT
        EXCEPTIONS
          CANT_CREATE_JOB  = 1
          INVALID_JOB_DATA = 2
          JOBNAME_MISSING  = 3
          OTHERS           = 4.
      CASE SY-SUBRC.
        WHEN 0.
        WHEN OTHERS.
          MESSAGE E208(00) WITH 'Error'.
      ENDCASE.

      CALL FUNCTION 'GET_PRINT_PARAMETERS'
        EXPORTING
*         immediately            = 'X'
          NO_DIALOG              = 'X'
          USER                   = LV_USER
        IMPORTING
          OUT_PARAMETERS         = LS_PRINT_PARAMETERS
        EXCEPTIONS
          ARCHIVE_INFO_NOT_FOUND = 1
          INVALID_PRINT_PARAMS   = 2
          INVALID_ARCHIVE_PARAMS = 3
          OTHERS                 = 4.

      SUBMIT ZSDSSDR0630 TO SAP-SPOOL AND RETURN
                                   WITH SELECTION-TABLE LS_SELTAB
                                                   USER LV_USER
                                       SPOOL PARAMETERS LS_PRINT_PARAMETERS
                                   WITHOUT SPOOL DYNPRO
                                                VIA JOB LV_JOBNAME
                                                 NUMBER LV_JOBCOUNT
                        USING SELECTION-SCREEN  1000
                              WITH P_KSCHL EQ P_KSCHL
                              WITH S_VBELN IN S_VBELN[]
                              WITH S_ZZPOB IN S_ZZPOB[]
                              WITH P_AUTO  EQ ABAP_TRUE
                              WITH P_MHA   EQ ABAP_TRUE.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          JOBCOUNT             = LV_JOBCOUNT
          JOBNAME              = LV_JOBNAME
          STRTIMMED            = ABAP_TRUE
        EXCEPTIONS
          CANT_START_IMMEDIATE = 1
          INVALID_STARTDATE    = 2
          JOBNAME_MISSING      = 3
          JOB_CLOSE_FAILED     = 4
          JOB_NOSTEPS          = 5
          JOB_NOTEX            = 6
          LOCK_FAILED          = 7
          OTHERS               = 8.
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
