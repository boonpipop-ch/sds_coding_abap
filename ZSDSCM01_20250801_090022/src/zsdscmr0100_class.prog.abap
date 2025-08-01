*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0100_CLASS
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
      GET_INSERT_DATA,
      GET_REPORT,
      GET_DATA_API CHANGING C_DATA TYPE GY_RESULT,
      ENDPOINT RETURNING VALUE(R) TYPE STRING,
      HEADER RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY IMPORTING I_DATA   TYPE GY_REQUEST_JSON OPTIONAL
           RETURNING VALUE(R) TYPE STRING,
      BODY_LEN IMPORTING I_DATA   TYPE STRING
               RETURNING VALUE(R) TYPE I,
      SAVE,
      UPDATE_ZSDSCMT009,
      UPDATE_ZSDSCMT003.
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
    IF R1 EQ ABAP_TRUE.
      GET_INSERT_DATA( ).
    ELSE.
      GET_REPORT( ).
    ENDIF.
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
  METHOD GET_INSERT_DATA.
    DATA : BEGIN OF LS_TMP,
             EQUNR TYPE EQUI-EQUNR,
           END OF LS_TMP.
    DATA : LT_TMP   LIKE TABLE OF LS_TMP,
           LT_CHECK LIKE TABLE OF LS_TMP WITH EMPTY KEY.

    DATA : LS_ZSDSCMT009 TYPE ZSDSCMT009,
           LT_ZSDSCMT009 TYPE TABLE OF ZSDSCMT009.

    OPEN CURSOR WITH HOLD @DATA(LV_CURSOR) FOR
    SELECT A~EQUNR
      FROM EQUI AS A
      INNER JOIN MARA ON A~MATNR    EQ MARA~MATNR AND
                         MARA~MTART NE 'ZAC' AND
                         MARA~MTART NE 'ZSP'
      WHERE A~EQUNR IN @S_EQUNR
        AND A~MATNR IN @S_MATNR
        AND A~SERNR IN @S_SERNR AND
      NOT EXISTS ( SELECT EQUNR
                     FROM ZSDSCMT003 AS B
                    WHERE B~EQUNR EQ A~EQUNR ) AND
      NOT EXISTS ( SELECT EQUNR
                     FROM ZSDSCMT009 AS C
                    WHERE C~EQUNR EQ A~EQUNR )            .
    DO.
      FETCH NEXT CURSOR @LV_CURSOR INTO TABLE @LT_TMP
      PACKAGE SIZE @GC_MAX_FETCH.
      IF SY-SUBRC <> 0.
*        CLOSE CURSOR LV_CURSOR.
        EXIT.
      ENDIF.
      APPEND LINES OF LT_TMP TO LT_CHECK.
    ENDDO.

    SORT LT_CHECK BY EQUNR.
    DELETE ADJACENT DUPLICATES FROM LT_CHECK COMPARING EQUNR.

    SELECT EQUI~EQUNR,
           EQUI~SERNR,
           EQUI~MATNR,
           BGMKOBJ~GWLDT,
           BGMKOBJ~GWLEN,
           @SY-UNAME,
           @SY-DATUM,
           @SY-UZEIT,
           @SY-UNAME,
           @SY-DATUM,
           @SY-UZEIT
   FROM @LT_CHECK AS A
   INNER JOIN EQUI ON A~EQUNR EQ EQUI~EQUNR
   LEFT JOIN BGMKOBJ ON EQUI~OBJNR    EQ BGMKOBJ~J_OBJNR AND
                        BGMKOBJ~GAART EQ '1'
   INTO TABLE @GT_RESULT.

    IF GT_RESULT IS NOT INITIAL.
      LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
        IF <LFS_DATA>-STD_WRT_BEG IS INITIAL.
          GET_DATA_API( CHANGING C_DATA = <LFS_DATA> ).
        ENDIF.

        LS_ZSDSCMT009-EQUNR       = <LFS_DATA>-EQUNR.
        LS_ZSDSCMT009-SERNR       = <LFS_DATA>-SERNR.
        LS_ZSDSCMT009-MATNR       = <LFS_DATA>-MATNR.
        LS_ZSDSCMT009-STD_WRT_BEG = <LFS_DATA>-STD_WRT_BEG.
        LS_ZSDSCMT009-STD_WRT_END = <LFS_DATA>-STD_WRT_END.
        LS_ZSDSCMT009-ERNAM       = <LFS_DATA>-ERNAM.
        LS_ZSDSCMT009-ERDAT       = <LFS_DATA>-ERDAT.
        LS_ZSDSCMT009-ERZET       = <LFS_DATA>-ERZET.
        LS_ZSDSCMT009-AENAM       = <LFS_DATA>-AENAM.
        LS_ZSDSCMT009-AEDAT       = <LFS_DATA>-AEDAT.
        LS_ZSDSCMT009-AEZET       = <LFS_DATA>-AEZET.

        MODIFY ZSDSCMT009 FROM LS_ZSDSCMT009.
        COMMIT WORK AND WAIT.

*        APPEND LS_ZSDSCMT009 TO LT_ZSDSCMT009.
      ENDLOOP.

*      MODIFY ZSDSCMT009 FROM TABLE LT_ZSDSCMT009.
*      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.
  METHOD GET_REPORT.
    SELECT ZSDSCMT009~EQUNR,
           ZSDSCMT009~SERNR,
           ZSDSCMT009~MATNR,
           ZSDSCMT009~STD_WRT_BEG,
           ZSDSCMT009~STD_WRT_END,
           ZSDSCMT009~ERNAM,
           ZSDSCMT009~ERDAT,
           ZSDSCMT009~ERZET,
           ZSDSCMT009~AENAM,
           ZSDSCMT009~AEDAT,
           ZSDSCMT009~AEZET,
           TJ02T~TXT04
      FROM ZSDSCMT009
      INNER JOIN EQUI ON ZSDSCMT009~EQUNR EQ EQUI~EQUNR
      LEFT JOIN JEST ON EQUI~OBJNR EQ JEST~OBJNR AND
                        JEST~INACT EQ @SPACE
      INNER JOIN TJ02T ON JEST~STAT   EQ TJ02T~ISTAT AND
                          TJ02T~SPRAS EQ @SY-LANGU
      WHERE ZSDSCMT009~EQUNR IN @S_EQUNR
        AND ZSDSCMT009~MATNR IN @S_MATNR
        AND ZSDSCMT009~SERNR IN @S_SERNR
        AND TJ02T~TXT04      IN @S_TXT04
      INTO TABLE @GT_RESULT.
  ENDMETHOD.
  METHOD GET_DATA_API.
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
            MATNR TYPE STRING,
            SERNR TYPE STRING,
            GWLDT TYPE STRING,
            GWLEN TYPE STRING,
          END OF LS_TRANSFER_ITEM.

    DATA: BEGIN OF LS_DATA,
            DATA LIKE TABLE OF LS_TRANSFER_ITEM,
          END OF LS_DATA.

    DATA: LS_REQUEST_JSON TYPE GY_REQUEST_JSON.

    LV_METHOD    = GC_CON-POST.
    LV_URL       = LCL_DATA=>ENDPOINT( ).
    IF LV_URL IS NOT INITIAL.
      LT_HEADER    = LCL_DATA=>HEADER( ).
      LS_REQUEST_JSON-I_SERNR = C_DATA-SERNR.
      LS_REQUEST_JSON-I_MATNR = C_DATA-MATNR.
      LV_BODY_TEXT = LCL_DATA=>BODY( LS_REQUEST_JSON ).
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

      READ TABLE LS_DATA-DATA INTO LS_TRANSFER_ITEM INDEX 1.
      IF SY-SUBRC EQ 0.
        REPLACE ALL OCCURRENCES OF PCRE '-' IN LS_TRANSFER_ITEM-GWLDT WITH ''.
        REPLACE ALL OCCURRENCES OF PCRE '-' IN LS_TRANSFER_ITEM-GWLEN WITH ''.
        C_DATA-STD_WRT_BEG = LS_TRANSFER_ITEM-GWLDT.
        C_DATA-STD_WRT_END = LS_TRANSFER_ITEM-GWLEN.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD ENDPOINT.
    CONSTANTS : BEGIN OF LC_API,
                  REPID TYPE C LENGTH 22 VALUE 'ZSDSCMR0100',
                  PARAM TYPE C LENGTH 22 VALUE 'ENDPOINT_EQUI',
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
        PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-NONE
      RECEIVING
        R_JSON      = R
      EXCEPTIONS
        OTHERS      = 1.
  ENDMETHOD.
  METHOD BODY_LEN.
    R = STRLEN( I_DATA ).
  ENDMETHOD.
  METHOD SAVE.
    IF R1 EQ ABAP_TRUE.
      UPDATE_ZSDSCMT009( ).
    ELSE.
      UPDATE_ZSDSCMT003( ).
    ENDIF.
  ENDMETHOD.
  METHOD UPDATE_ZSDSCMT009.
    DATA : LS_ZSDSCMT009 TYPE ZSDSCMT009,
           LT_ZSDSCMT009 TYPE TABLE OF ZSDSCMT009.

    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
      LS_ZSDSCMT009-EQUNR       = <LFS_DATA>-EQUNR.
      LS_ZSDSCMT009-SERNR       = <LFS_DATA>-SERNR.
      LS_ZSDSCMT009-MATNR       = <LFS_DATA>-MATNR.
      LS_ZSDSCMT009-STD_WRT_BEG = <LFS_DATA>-STD_WRT_BEG.
      LS_ZSDSCMT009-STD_WRT_END = <LFS_DATA>-STD_WRT_END.
      LS_ZSDSCMT009-ERNAM       = <LFS_DATA>-ERNAM.
      LS_ZSDSCMT009-ERDAT       = <LFS_DATA>-ERDAT.
      LS_ZSDSCMT009-ERZET       = <LFS_DATA>-ERZET.
      LS_ZSDSCMT009-AENAM       = <LFS_DATA>-AENAM.
      LS_ZSDSCMT009-AEDAT       = <LFS_DATA>-AEDAT.
      LS_ZSDSCMT009-AEZET       = <LFS_DATA>-AEZET.
      APPEND LS_ZSDSCMT009 TO LT_ZSDSCMT009.
    ENDLOOP.

    MODIFY ZSDSCMT009 FROM TABLE LT_ZSDSCMT009.
    COMMIT WORK AND WAIT.
  ENDMETHOD.
  METHOD UPDATE_ZSDSCMT003.
    DATA : LS_ZSDSCMT003 TYPE ZSDSCMT003,
           LT_ZSDSCMT003 TYPE TABLE OF ZSDSCMT003.

    DATA : BEGIN OF LS_EQUIP,
             EQUNR TYPE EQUI-EQUNR,
           END OF LS_EQUIP.
    DATA : LT_EQUIP LIKE HASHED TABLE OF LS_EQUIP WITH UNIQUE KEY EQUNR.

    LT_EQUIP =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).

    SELECT ZSDSCMT003~*
      FROM @LT_EQUIP AS A
      INNER JOIN ZSDSCMT003 ON A~EQUNR EQ ZSDSCMT003~EQUNR
      INTO TABLE @DATA(LT_CHECK).

    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
      READ TABLE LT_CHECK INTO DATA(LS_CHECK)
      WITH KEY EQUNR = <LFS_DATA>-EQUNR.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING LS_CHECK TO LS_ZSDSCMT003.
        LS_ZSDSCMT003-STD_WRT_BEG = <LFS_DATA>-STD_WRT_BEG.
        LS_ZSDSCMT003-STD_WRT_END = <LFS_DATA>-STD_WRT_END.
        LS_ZSDSCMT003-AENAM       = <LFS_DATA>-AENAM.
        LS_ZSDSCMT003-AEDAT       = <LFS_DATA>-AEDAT.
        LS_ZSDSCMT003-AEZET       = <LFS_DATA>-AEZET.
      ELSE.
        LS_ZSDSCMT003-EQUNR       = <LFS_DATA>-EQUNR.
        LS_ZSDSCMT003-SERNR       = <LFS_DATA>-SERNR.
        LS_ZSDSCMT003-MATNR       = <LFS_DATA>-MATNR.
        LS_ZSDSCMT003-STD_WRT_BEG = <LFS_DATA>-STD_WRT_BEG.
        LS_ZSDSCMT003-STD_WRT_END = <LFS_DATA>-STD_WRT_END.
        LS_ZSDSCMT003-ERNAM       = <LFS_DATA>-ERNAM.
        LS_ZSDSCMT003-ERDAT       = <LFS_DATA>-ERDAT.
        LS_ZSDSCMT003-ERZET       = <LFS_DATA>-ERZET.
        LS_ZSDSCMT003-AENAM       = <LFS_DATA>-AENAM.
        LS_ZSDSCMT003-AEDAT       = <LFS_DATA>-AEDAT.
        LS_ZSDSCMT003-AEZET       = <LFS_DATA>-AEZET.
      ENDIF.
      APPEND LS_ZSDSCMT003 TO LT_ZSDSCMT003.

      DELETE FROM ZSDSCMT009 WHERE EQUNR EQ LS_ZSDSCMT003-EQUNR.
      COMMIT WORK AND WAIT.
      CLEAR LS_ZSDSCMT003.
    ENDLOOP.

    MODIFY ZSDSCMT003 FROM TABLE LT_ZSDSCMT003.
    COMMIT WORK AND WAIT.

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
