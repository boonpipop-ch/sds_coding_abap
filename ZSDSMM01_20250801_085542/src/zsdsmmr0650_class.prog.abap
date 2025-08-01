*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0650_CLASS
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
      CALL_API IMPORTING I_DATA TYPE GY_REQUEST_JSON,
      ENDPOINT RETURNING VALUE(R) TYPE STRING,
      HEADER RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY IMPORTING I_DATA   TYPE GY_REQUEST_JSON
           RETURNING VALUE(R) TYPE STRING,
      BODY_LEN IMPORTING I_DATA   TYPE STRING
               RETURNING VALUE(R) TYPE I,
      GEN_DATA_FORMAT IMPORTING I_DATA   TYPE SY-DATUM
                      RETURNING VALUE(R) TYPE STRING,
      SEND_DATA_AGAIN IMPORTING I_YEAR TYPE ZSDSMMT027-MJAHR
                                I_DOC  TYPE ZSDSMMT027-MBLNR.
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
    SELECT MSEG~MBLNR,
           MSEG~MJAHR,
           MSEG~ZEILE,
           MSEG~MATNR,
           MSEG~LGORT,
           MSEG~UMLGO,
           MSEG~SHKZG,
           MSEG~MENGE,
           MSEG~RSNUM,
           MSEG~RSPOS,
           MKPF~CPUDT,
           MSEG~BWART,
           MSEG~XAUTO,
           ZSDSMMT027~MESSG,
           @GC_ERRO   AS STATU,
           @ABAP_TRUE AS CHECK
      FROM ZSDSMMT027
      INNER JOIN MKPF ON ZSDSMMT027~MBLNR EQ MKPF~MBLNR AND
                         ZSDSMMT027~MJAHR EQ MKPF~MJAHR AND
                         ZSDSMMT027~METYP EQ 'E'
      INNER JOIN MSEG ON MKPF~MBLNR EQ MSEG~MBLNR AND
                         MKPF~MJAHR EQ MSEG~MJAHR AND
                         MSEG~SHKZG EQ 'H'
      WHERE MKPF~MBLNR IN @S_MBLNR
        AND MKPF~MJAHR IN @S_MJAHR
        AND ( MSEG~RSNUM NE @SPACE AND
              MSEG~RSNUM IS NOT NULL )
        AND MSEG~BWART IN @S_BWART
*        AND NOT EXISTS
*      ( SELECT *
*          FROM ZSDSMMT027
*         WHERE MBLNR IN @S_MBLNR
*           AND MJAHR IN @S_MJAHR
*           AND METYP EQ 'S' )
      INTO TABLE @GT_RESULT.
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

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = 'STATU'.
    LS_FCAT-SELTEXT_S   = 'Status'.
    LS_FCAT-SELTEXT_M   = 'Status'.
    LS_FCAT-SELTEXT_L   = 'Status'.
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
  METHOD SAVE.
    DATA : LS_BODY TYPE GY_REQUEST_JSON.

    DATA : LS_TRANSFER LIKE LINE OF LS_BODY-_TRANS_FER_ITEM.

    DATA : LT_DATA LIKE GT_RESULT.

    LT_DATA = GT_RESULT.

    SORT LT_DATA BY CHECK.
    DELETE LT_DATA WHERE CHECK EQ SPACE.

    SORT LT_DATA BY MBLNR MJAHR.
    SORT GT_RESULT BY MBLNR MJAHR.

    DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING MBLNR MJAHR.

    LOOP AT LT_DATA INTO DATA(LS_DATA).
      LOOP AT GT_RESULT INTO DATA(LS_TMP) WHERE MBLNR EQ LS_DATA-MBLNR AND
                                                MJAHR EQ LS_DATA-MJAHR.
        MOVE-CORRESPONDING LS_TMP TO GS_RESULT.

        LS_BODY-_REF_SAP_RESER_NO = GS_RESULT-RSNUM.
        LS_BODY-_MAT_DOC_NO       = GS_RESULT-MBLNR.

        CONCATENATE GS_RESULT-MBLNR '/' GS_RESULT-ZEILE INTO LS_TRANSFER-_TRANSFER_KEY.
        CONCATENATE GS_RESULT-RSNUM '/' GS_RESULT-RSPOS INTO LS_TRANSFER-_RESER_KEY.
        LS_TRANSFER-_MAT_DOC_NO         = GS_RESULT-MBLNR.
        LS_TRANSFER-_MAT_DOC_ITEM       = GS_RESULT-ZEILE.
        LS_TRANSFER-_SAP_CREATED_DATE = GEN_DATA_FORMAT( GS_RESULT-CPUDT ).
*      LS_TRANSFER-_SAP_CREATED_DATE   = GS_RESULT-CPUDT.
        LS_TRANSFER-_QUANTITY           = GS_RESULT-MENGE.
        IF GS_RESULT-XAUTO EQ ABAP_TRUE.
          LS_TRANSFER-_TYPE             = 'Cancelled'.
        ELSE.
          LS_TRANSFER-_TYPE             = 'Transferred'.
        ENDIF.
        LS_TRANSFER-_PRODUCT_CODE       = GS_RESULT-MATNR.
        LS_TRANSFER-_FROM_LOCATION      = GS_RESULT-LGORT.
        LS_TRANSFER-_TO_LOCATION        = GS_RESULT-UMLGO.
        LS_TRANSFER-_REF_SAP_RESER_NO   = GS_RESULT-RSNUM.
        LS_TRANSFER-_REF_SAP_RESER_ITEM = GS_RESULT-RSPOS.
        APPEND LS_TRANSFER TO LS_BODY-_TRANS_FER_ITEM.
        CLEAR : LS_TRANSFER.
*        AT END OF MBLNR.
*        CALL_API( LS_BODY ).
*        CLEAR : LS_BODY.
*        MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING MESSG
*                                        WHERE MBLNR EQ LS_DATA-MBLNR AND
*                                              MJAHR EQ LS_DATA-MJAHR.
*        ENDAT.
      ENDLOOP.
      CALL_API( LS_BODY ).
      CLEAR : LS_BODY.
      MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING MESSG
                                      WHERE MBLNR EQ LS_DATA-MBLNR AND
                                            MJAHR EQ LS_DATA-MJAHR.
    ENDLOOP.

    MESSAGE S001 WITH TEXT-S01.

  ENDMETHOD.
  METHOD ENDPOINT.
    CONSTANTS : BEGIN OF LC_API,
                  REPID TYPE C LENGTH 22 VALUE 'ZSDSMMR0650',
                  PARAM TYPE C LENGTH 22 VALUE 'ENDPOINT_SFDC_RES',
                  DEV   TYPE C LENGTH 3  VALUE 'DEV',
                  QAS   TYPE C LENGTH 3  VALUE 'QAS',
                  PRD   TYPE C LENGTH 3  VALUE 'PRD',
                END OF LC_API.

    CASE SY-SYSID.
      WHEN LC_CON-DEV.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-PARAM
                                                      I_PARAM_EXT         = LC_API-DEV
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-QAS.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-PARAM
                                                      I_PARAM_EXT         = LC_API-QAS
                                            CHANGING  C_RETURN            = R ).
      WHEN LC_CON-PRD.
        ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_API-REPID
                                                      I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                      I_PARAM             = LC_API-PARAM
                                                      I_PARAM_EXT         = LC_API-PRD
                                            CHANGING  C_RETURN            = R ).
    ENDCASE.
  ENDMETHOD.
  METHOD HEADER.
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
  METHOD BODY.
    CALL METHOD /UI2/CL_JSON=>SERIALIZE
      EXPORTING
        DATA        = I_DATA
        PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
      RECEIVING
        R_JSON      = R
      EXCEPTIONS
        OTHERS      = 1.
  ENDMETHOD.
  METHOD BODY_LEN.
    R = STRLEN( I_DATA ).
  ENDMETHOD.
  METHOD CALL_API.
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

    DATA: BEGIN OF LS_TRANSFER_ITEM,
            _Transfer_Key TYPE STRING,
            _Status       TYPE STRING,
          END OF LS_TRANSFER_ITEM.

    DATA: BEGIN OF LS_DATA,
            _Ref_Sap_Reser_No TYPE STRING,
            _Mat_Doc_No       TYPE STRING,
            _Response_Status  TYPE STRING,
            _Response_Message TYPE STRING,
            _Response         LIKE TABLE OF LS_TRANSFER_ITEM,
          END OF LS_DATA.

    DATA : LV_TOKEN TYPE STRING.

    DATA : LS_ZSDSMMT027 TYPE ZSDSMMT027.

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT( ).
    IF LV_URL IS NOT INITIAL.
      LT_HEADER    = LCL_DATA=>HEADER( ).
      LV_BODY_TEXT = LCL_DATA=>BODY( I_DATA ).
      LV_LEN       = LCL_DATA=>BODY_LEN( LV_BODY_TEXT ).

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
      LS_ZSDSMMT027-MBLNR = GS_RESULT-MBLNR.
      LS_ZSDSMMT027-MJAHR = GS_RESULT-MJAHR.
      IF LV_STATUS EQ GC_CON-S.
        IF LS_DATA-_Response_Status EQ 'Success' OR
           LS_DATA-_Response_Status EQ 'SUCCESS' OR
           LS_DATA-_Response_Status EQ 'S'.
          LS_ZSDSMMT027-METYP = GC_S.
          LS_ZSDSMMT027-MESSG = LS_DATA-_Response_MESSAGE.
          GS_RESULT-MESSG     = LS_DATA-_Response_MESSAGE.
          GS_RESULT-STATU     = GC_SUCS.
        ELSE.
          LS_ZSDSMMT027-METYP = GC_E.
          LS_ZSDSMMT027-MESSG = LS_DATA-_Response_MESSAGE.
          GS_RESULT-MESSG     = LS_DATA-_Response_MESSAGE.
          GS_RESULT-STATU     = GC_ERRO.
        ENDIF.
      ELSE.
        LS_ZSDSMMT027-METYP = GC_E.
        LS_ZSDSMMT027-MESSG = TEXT-E01.
        GS_RESULT-MESSG     = TEXT-E01.
        GS_RESULT-STATU     = GC_ERRO.
        LCL_DATA=>SEND_DATA_AGAIN( I_YEAR = LS_ZSDSMMT027-MJAHR
                                   I_DOC  = LS_ZSDSMMT027-MBLNR ).
      ENDIF.

      SELECT SINGLE ERNAM,
                    ERDAT,
                    ERZET
        FROM ZSDSMMT027
        WHERE MBLNR EQ @LS_ZSDSMMT027-MBLNR
          AND MJAHR EQ @LS_ZSDSMMT027-MJAHR
        INTO @DATA(LS_CHECK_INSERT).
      IF SY-SUBRC EQ 0.
        LS_ZSDSMMT027-ERNAM = LS_CHECK_INSERT-ERNAM.
        LS_ZSDSMMT027-ERDAT = LS_CHECK_INSERT-ERDAT.
        LS_ZSDSMMT027-ERZET = LS_CHECK_INSERT-ERZET.
      ELSE.
        LS_ZSDSMMT027-ERNAM = SY-UNAME.
        LS_ZSDSMMT027-ERDAT = SY-DATUM.
        LS_ZSDSMMT027-ERZET = SY-UZEIT.
      ENDIF.

      LS_ZSDSMMT027-AENAM = SY-UNAME.
      LS_ZSDSMMT027-AEDAT = SY-DATUM.
      LS_ZSDSMMT027-AEZET = SY-UZEIT.

      MODIFY ZSDSMMT027 FROM LS_ZSDSMMT027.
    ELSE.
      LS_ZSDSMMT027-METYP = GC_E.
      LS_ZSDSMMT027-MESSG = TEXT-E02.
      GS_RESULT-MESSG     = TEXT-E02.
      GS_RESULT-STATU     = GC_ERRO.
    ENDIF.

  ENDMETHOD.
  METHOD GEN_DATA_FORMAT.
    CONCATENATE I_DATA+0(4) '-'
                I_DATA+4(2) '-'
                I_DATA+6(2)
           INTO R.
  ENDMETHOD.
  METHOD SEND_DATA_AGAIN.
    DATA : S_MJAHR TYPE RANGE OF MKPF-MJAHR,
           S_MBLNR TYPE RANGE OF MKPF-MBLNR.

    DATA : LV_JOBNAME  TYPE TBTCO-JOBNAME,
           LV_JOBCOUNT TYPE TBTCO-JOBCOUNT.

    DATA: LV_DATA TYPE SY-DATUM,
          LV_TIME TYPE SY-UZEIT.

    DATA : LS_PRINT_PARAMETERS TYPE PRI_PARAMS.
    DATA : LS_SELTAB           TYPE TABLE OF RSPARAMS.

    S_MJAHR =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = I_YEAR ) ).
    S_MBLNR =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = I_DOC ) ).

    CONCATENATE 'RESER' '_' SY-UNAME '_' SY-UZEIT INTO LV_JOBNAME.

    LV_DATA = SY-DATUM.
    LV_TIME = SY-UZEIT + 60.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = LV_JOBNAME
        SDLSTRTDT        = LV_DATA
        SDLSTRTTM        = LV_TIME
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
    ENDCASE.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
*       immediately            = 'X'
        NO_DIALOG              = 'X'
        USER                   = SY-UNAME
      IMPORTING
        OUT_PARAMETERS         = LS_PRINT_PARAMETERS
      EXCEPTIONS
        ARCHIVE_INFO_NOT_FOUND = 1
        INVALID_PRINT_PARAMS   = 2
        INVALID_ARCHIVE_PARAMS = 3
        OTHERS                 = 4.

    SUBMIT ZSDSMMR0650 TO SAP-SPOOL AND RETURN
                                   WITH SELECTION-TABLE LS_SELTAB
                                                   USER SY-UNAME
                                       SPOOL PARAMETERS LS_PRINT_PARAMETERS
                                   WITHOUT SPOOL DYNPRO
                                                VIA JOB LV_JOBNAME
                                                 NUMBER LV_JOBCOUNT
                            USING SELECTION-SCREEN  1000
                            WITH S_MJAHR    IN S_MJAHR
                            WITH S_MBLNR    IN S_MBLNR
                            WITH P_AUTO EQ ABAP_TRUE.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        JOBCOUNT             = LV_JOBCOUNT
        JOBNAME              = LV_JOBNAME
        SDLSTRTDT            = LV_DATA
        SDLSTRTTM            = LV_TIME
*       STRTIMMED            = ABAP_TRUE
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
