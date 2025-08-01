*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0350_CLASS
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
      CHECK_SERAIL IMPORTING IT_DATA TYPE ZSDSCAS008_TT,
      UPDATE_DATA,
      UPDATE_DO IMPORTING IT_DATA TYPE GTY_SERIAL
                          I_DDD   TYPE FLAG,
      UPDATE_STORAGE_FOR_PROJ_DDD IMPORTING IT_DATA TYPE GTY_SERIAL
                                            I_DDD   TYPE FLAG,
      VALIDATE_SERIAL IMPORTING PLV_DELIVERY TYPE ANY
                                PLV_ITEM     TYPE ANY
                                PLV_MATNR    TYPE ANY
                                PLV_SERIAL   TYPE ANY
                      CHANGING  CLV_INVALID  TYPE ANY
                                CLV_INPICK   TYPE ANY
                                CLV_INBLOCK  TYPE ANY
                                IN_PICK      TYPE GTY_SERIAL,
      CHECK_DUP_UPLOAD IMPORTING PLV_CHK_DELIVERY TYPE ANY
                                 PLV_CHK_ITEM     TYPE ANY
                       RETURNING VALUE(R)         TYPE FLAG,
      UPDATE_ERROR,
      SEND_MAIL IMPORTING LV_TEXT  TYPE ANY
                          LV_CHECK TYPE ANY
                          IN_CHK   TYPE GTY_SERIAL,
      GET_SUBJECT RETURNING VALUE(R) TYPE STRING,
      GET_CONTANT_TEXT IMPORTING LV_SNAME TYPE ANY
                                 IT_CHK   TYPE GTY_SERIAL
                                 LV_CHECK TYPE ANY
                                 LV_TEXT  TYPE ANY
                       RETURNING VALUE(R) TYPE SOLI_TAB,
      GET_SENDER_EMAIL IMPORTING I_SNAME  TYPE ANY
                       RETURNING VALUE(R) TYPE ADR6-SMTP_ADDR,
      GET_SENDER_NAME IMPORTING I_SNAME  TYPE ANY
                      RETURNING VALUE(R) TYPE ADR6-SMTP_ADDR,
      PROCESS_DATA IMPORTING IT_CHK TYPE GTY_SERIAL,
      BDC_DYNPRO IMPORTING I_PROGRAM TYPE ANY
                           I_DYNPRO  TYPE ANY
                 RETURNING VALUE(R)  TYPE TAB_BDCDATA,
      BDC_FIELD IMPORTING I_FNAM   TYPE ANY
                          I_FVAL   TYPE ANY
                RETURNING VALUE(R) TYPE TAB_BDCDATA,
      COMMIT.
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

    DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

    DATA : LV_STATUS TYPE C.

    DATA : LV_MODE TYPE C.

    DATA : LT_FILE TYPE ZSDSCAS008_TT,
           LS_FILE LIKE LINE OF LT_FILE.

    IF LCL_FTP IS NOT BOUND.
      CREATE OBJECT LCL_FTP.
    ENDIF.

    IF R1 EQ ABAP_TRUE.
      LV_MODE = 'U'.
    ELSE.
      LV_MODE = 'B'.
    ENDIF.
    LT_FILE = LCL_FTP->FTP_FILE_MGET( I_WINDOW_PATH = 'SONY_LOGISTIC_FG/DEV/IN'
                                     I_AL11_PATH   = '/tmp'
                                     I_FILE_NAME   = P_NAME
                                     I_USER        = 'ds'
                                     I_PASS        = 'ds=20240521'
                                     I_IP          = '172.31.136.249'
                                     I_MODE        = LV_MODE
                                     I_PORT        = '21' ).
    IF LT_FILE IS NOT INITIAL.
      LCL_DATA=>CHECK_SERAIL( LT_FILE ).
      IF GT_SERIAL IS NOT INITIAL.
        LCL_DATA=>UPDATE_DATA( ).
      ENDIF.
      " SUCCESS FTP FILE
    ELSE.
      " CANNOT FTP FILE
    ENDIF.

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
  METHOD CHECK_SERAIL.
    DATA : LS_DATA LIKE LINE OF IT_DATA.
*           LS_TMP  LIKE LINE OF IT_DATA.

    LOOP AT IT_DATA INTO LS_DATA.
*      MOVE-CORRESPONDING LS_TMP TO LS_DATA.
*
*      AT NEW FILE.
*
*      ENDAT.
      GS_SERIAL-DELIVERY = LS_DATA-DATA+0(15).
      GS_SERIAL-ITEM     = LS_DATA-DATA+15(4).
      GS_SERIAL-SERIAL   = LS_DATA-DATA+19(18).

      APPEND GS_SERIAL TO GT_SERIAL.
      CLEAR GS_SERIAL.
    ENDLOOP.

  ENDMETHOD.
  METHOD UPDATE_DATA.

    DATA : BEGIN OF LS_LIKP,
             VBELN TYPE LIKP-VBELN,
             POSNR TYPE LIPS-POSNR,
             MATNR TYPE LIPS-MATNR,
             LFART TYPE LIKP-LFART,
           END OF LS_LIKP.
    DATA : LT_LIKP LIKE TABLE OF LS_LIKP.

    DATA : LT_SERIAL LIKE GT_SERIAL.

    DATA : LS_TMP LIKE GS_SERIAL.

    CONSTANTS : BEGIN OF LC_CASE,
                  DDD TYPE C LENGTH 7 VALUE 'DDDuser',
                END OF LC_CASE.

    IF GT_SERIAL[] IS NOT INITIAL.
      SELECT LIKP~VBELN,
             LIPS~POSNR,
             LIPS~MATNR,
             LIKP~LFART
        FROM @GT_SERIAL AS A
        INNER JOIN LIKP ON A~DELIVERY EQ LIKP~VBELN
        INNER JOIN LIPS ON LIKP~VBELN EQ LIPS~VBELN
        INTO TABLE @LT_LIKP.
    ENDIF.

    SORT GT_SERIAL BY DELIVERY ITEM SERIAL.

    LOOP AT GT_SERIAL INTO LS_TMP.
      LS_TMP-ITEM = |{ LS_TMP-ITEM ALPHA = IN }|.
      READ TABLE LT_LIKP INTO LS_LIKP
      WITH KEY VBELN = GS_SERIAL-DELIVERY+0(10)
               POSNR = GS_SERIAL-ITEM.
      IF SY-SUBRC NE 0.
        CLEAR : LS_TMP,GS_SERIAL.
        CONTINUE.
      ELSE.
        LS_TMP-MATNR = LS_LIKP-MATNR.
        LS_TMP-LFART = LS_LIKP-LFART.
      ENDIF.

      MOVE-CORRESPONDING LS_TMP TO GS_SERIAL.
      APPEND GS_SERIAL TO LT_SERIAL.

      IF SY-UNAME EQ LC_CASE-DDD.
        UPDATE_DO( IT_DATA = LT_SERIAL
                   I_DDD   = ABAP_TRUE ).
      ELSE.
        UPDATE_DO( IT_DATA = LT_SERIAL
                   I_DDD   = ABAP_FALSE ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD UPDATE_DO.
    DATA : LS_DATA LIKE LINE OF IT_DATA.

    DATA : LV_DELIV    TYPE VBELN_VL,
           LV_MATERIAL TYPE BAPIEKPO-MATERIAL,
           LV_SERIAL   TYPE EQUI-SERNR,
           LV_QUEST    TYPE STRING,
           LV_ANSWER   TYPE C,
           LV_FLAG     TYPE CHAR1,
           LV_INVALID  TYPE CHAR1,
           LV_INPICK   TYPE CHAR1,
           LV_INBLOCK  TYPE CHAR1.

    DATA : LT_PICK LIKE IT_DATA.

    DATA : LT_CHK TYPE TABLE OF GY_SERIAL WITH EMPTY KEY,
           LS_CHK TYPE GY_SERIAL.
*
*    LOOP AT IT_DATA INTO LS_DATA.
*      LS_CHK-DELIVERY = LS_DATA-DELIVERY.
*      LS_CHK-ITEM     = LS_DATA-ITEM.
*      LS_CHK-MATNR    = LS_DATA-MATNR.
*      LS_CHK-SERIAL   = LS_DATA-SERIAL.
*      LS_CHK-LFART    = LS_DATA-LFART.
*    ENDLOOP.

    UPDATE_STORAGE_FOR_PROJ_DDD( IT_DATA = IT_DATA
                                 I_DDD   = I_DDD ).
    LT_CHK[]  = IT_DATA[].
    LT_PICK[] = IT_DATA[].
    LOOP AT LT_CHK INTO LS_DATA.
      CLEAR: LV_DELIV, LV_MATERIAL, LV_SERIAL, LV_FLAG.
      LV_DELIV    = LS_DATA-DELIVERY.
      LV_MATERIAL = LS_DATA-MATNR.
      LV_SERIAL   = LS_DATA-SERIAL.

      CLEAR: LV_INVALID, LV_INPICK, LV_INBLOCK.
      VALIDATE_SERIAL( EXPORTING PLV_DELIVERY = LS_DATA-DELIVERY
                                 PLV_ITEM     = LS_DATA-ITEM
                                 PLV_MATNR    = LS_DATA-MATNR
                                 PLV_SERIAL   = LS_DATA-SERIAL
                        CHANGING CLV_INVALID  = LV_INVALID
                                 CLV_INPICK   = LV_INPICK
                                 CLV_INBLOCK  = LV_INBLOCK
                                 IN_PICK      = LT_PICK ).

      IF LV_INVALID EQ ABAP_TRUE.
        LS_DATA-FLAG = ABAP_TRUE.
        MODIFY LT_CHK FROM LS_DATA.
      ELSEIF LV_INPICK IS NOT INITIAL.
        LS_DATA-PICK = ABAP_TRUE.
        MODIFY LT_CHK FROM LS_DATA.
      ELSEIF LV_INBLOCK IS NOT INITIAL.
        LS_DATA-BLOCK = ABAP_TRUE.
        MODIFY LT_CHK FROM LS_DATA.
      ENDIF.
    ENDLOOP.

    DATA : LT_DO   LIKE LT_CHK,
           LS_DO   LIKE LINE OF LT_DO,
           LV_ITEM TYPE CHAR50.

    READ TABLE LT_CHK
    WITH KEY FLAG = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      LT_DO[] = LT_CHK[].

      SORT LT_DO BY DELIVERY ITEM.
      DELETE ADJACENT DUPLICATES FROM LT_DO COMPARING DELIVERY ITEM.

      CLEAR LV_INVALID.
      LOOP AT LT_DO INTO LS_DO.
        LV_INVALID = CHECK_DUP_UPLOAD( PLV_CHK_DELIVERY = LS_DO-DELIVERY
                                       PLV_CHK_ITEM     = LS_DO-ITEM ).
        IF  LV_INVALID = ABAP_TRUE.
          IF LV_ITEM IS INITIAL.
            LV_ITEM = LS_DO-ITEM.
          ELSE.
            CONCATENATE LV_ITEM LS_DO-ITEM
                   INTO LV_ITEM
            SEPARATED BY ','.
          ENDIF.
        ENDIF.

      ENDLOOP.
      IF LV_INVALID = ABAP_TRUE.
        CONCATENATE TEXT-101
                    LS_DO-DELIVERY
                    TEXT-102
                    TEXT-103
               INTO LV_QUEST
        SEPARATED BY SPACE.
        CLEAR LV_ANSWER.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = TEXT-104
            TEXT_QUESTION         = LV_QUEST
            DISPLAY_CANCEL_BUTTON = SPACE
          IMPORTING
            ANSWER                = LV_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.
        IF SY-SUBRC = 0.
          IF LV_ANSWER = '2'.
            LEAVE TO CURRENT TRANSACTION.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE LT_CHK
    WITH KEY FLAG = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      UPDATE_ERROR( ).
      GV_TAXT = TEXT-105.
      SEND_MAIL( LV_TEXT  = GV_TAXT
                 LV_CHECK = GC_F
                 IN_CHK   = LT_CHK ).
      RETURN.
    ENDIF.

    READ TABLE LT_CHK
    WITH KEY PICK = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      UPDATE_ERROR( ).
      GV_TAXT = TEXT-107.
      SEND_MAIL( LV_TEXT  = GV_TAXT
                 LV_CHECK = GC_P
                 IN_CHK   = LT_CHK ).
    ENDIF.

    READ TABLE LT_CHK
    WITH KEY BLOCK = ABAP_TRUE TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      UPDATE_ERROR( ).
      GV_TAXT = TEXT-108.
      SEND_MAIL( LV_TEXT  = GV_TAXT
                 LV_CHECK = GC_P
                 IN_CHK   = LT_CHK ).
    ENDIF.

    IF LT_CHK IS INITIAL.
      UPDATE_ERROR( ).
      GV_TAXT = TEXT-109.
      SEND_MAIL( LV_TEXT  = GV_TAXT
                 LV_CHECK = GC_P
                 IN_CHK   = LT_CHK ).
    ELSE.
      PROCESS_DATA( IT_CHK   = LT_CHK ).
    ENDIF.

  ENDMETHOD.
  METHOD UPDATE_STORAGE_FOR_PROJ_DDD.
    DATA : LT_LIPS TYPE TABLE OF LIPS,
           LS_LIPS TYPE LIPS.

    DATA : LLTAK   TYPE LTAK,
           L_TRTYP TYPE	C.

    DATA : LLTAP TYPE TABLE OF LTAP,
           XLIPS TYPE TABLE OF LIPS,
           XVBFA TYPE TABLE OF VBFAVB,
           XVBUK TYPE TABLE OF VBUKVB,
           XVBUP TYPE TABLE OF VBUPVB,
           YVBFA TYPE TABLE OF VBFAVB,
           YVBUK TYPE TABLE OF VBUKVB,
           YVBUP TYPE TABLE OF VBUPVB.

    DATA : LS_XLIPS TYPE LIPS,
           LS_LLTAP TYPE LTAP.

    DATA : LS_VBKOK TYPE  VBKOK.
    DATA : LT_VBPOK TYPE TABLE OF	VBPOK.

    FIELD-SYMBOLS <FS_VBPOK> TYPE VBPOK.
    DATA : LV_VBELN TYPE LIKP-VBELN.

    DATA : BEGIN OF LS_LIKP,
             VBELN TYPE LIKP-VBELN,
           END OF LS_LIKP.
    DATA : LT_LIKP LIKE TABLE OF LS_LIKP.

    CONSTANTS : BEGIN OF LC_CON,
                  ZD  TYPE C LENGTH 2 VALUE 'ZD',
                  SET TYPE C LENGTH 3 VALUE 'SET',
                  TSS TYPE C LENGTH 4 VALUE '1700',
                  TSE TYPE C LENGTH 4 VALUE '1800',
                END OF LC_CON.


    SELECT VBELN
      FROM @IT_DATA AS A
      INNER JOIN LIKP ON A~DELIVERY EQ LIKP~VBELN AND
                         LIKP~LSTEL EQ @LC_CON-ZD
      INTO TABLE @LT_LIKP.
    IF SY-SUBRC = 0.
      SELECT *
        FROM LIPS
        INTO TABLE LT_LIPS
        FOR ALL ENTRIES IN LT_LIKP
        WHERE VBELN EQ LT_LIKP-VBELN.

      LOOP AT LT_LIPS INTO LS_LIPS WHERE VRKME NE LC_CON-SET AND
                                         LGORT EQ SPACE.

        APPEND INITIAL LINE TO LT_VBPOK ASSIGNING <FS_VBPOK>.
        <FS_VBPOK>-VBELN_VL = LS_LIPS-VBELN.
        <FS_VBPOK>-POSNR_VL = LS_LIPS-POSNR.
        <FS_VBPOK>-KZLGO    = ABAP_TRUE.
        <FS_VBPOK>-WERKS    = LS_LIPS-WERKS.
        "Add by Wantanee 20200115
        IF I_DDD IS NOT INITIAL.
          <FS_VBPOK>-LGORT    = LC_CON-TSS. " Here put your stor. loc
        ELSE.
          <FS_VBPOK>-LGORT    = LC_CON-TSE. " Here put your stor. loc
        ENDIF.
        "End Add by Wantanee 20200115

        <FS_VBPOK>-XWMPP    = ABAP_TRUE.
        <FS_VBPOK>-LGPLA    = LS_LIPS-LGPLA.
        <FS_VBPOK>-LGTYP    = LS_LIPS-LGTYP.
        <FS_VBPOK>-BWLVS    = LS_LIPS-BWLVS.

        LV_VBELN = LS_LIPS-VBELN.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        LS_VBKOK-VBELN_VL   = LV_VBELN.
        CALL FUNCTION 'WS_DELIVERY_UPDATE'
          EXPORTING
            VBKOK_WA      = LS_VBKOK
            DELIVERY      = LV_VBELN
            COMMIT        = ABAP_TRUE
          TABLES
            VBPOK_TAB     = LT_VBPOK
*           prot          = lt_prot
          EXCEPTIONS
            ERROR_MESSAGE = 1
            OTHERS        = 2.
      ENDIF.
      COMMIT WORK AND WAIT.
      WAIT UP TO 10 SECONDS.
    ENDIF.

  ENDMETHOD.
  METHOD VALIDATE_SERIAL.
    DATA : LV_STATUS  TYPE BSVX-STTXT.

    DATA : LV_SERNR   TYPE EQUI-SERNR.

    DATA : LS_PICK LIKE LINE OF IN_PICK.

    DATA : LWA_LIPS        TYPE LIPS,
           LWA_MARC        TYPE MARC,
           LWA_EQUI        TYPE EQUI,
           LWA_SER01       TYPE SER01,
           LWA_OBJK        TYPE OBJK,
           LWA_V_EQUI_EQBS TYPE V_EQUI_EQBS_SML,
           LWA_COUNT       TYPE I.

    CONSTANTS : BEGIN OF LC_CON,
                  ESTO TYPE C LENGTH 4 VALUE 'ESTO',
                  OS   TYPE C LENGTH 2 VALUE '07',
                END OF LC_CON.

    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        INPUT  = PLV_SERIAL
      IMPORTING
        OUTPUT = LV_SERNR.

    SELECT SINGLE *
      FROM LIPS
      INTO LWA_LIPS
     WHERE VBELN = PLV_DELIVERY
       AND POSNR = PLV_ITEM.

    CLEAR LWA_COUNT.
    LOOP AT IN_PICK INTO LS_PICK WHERE DELIVERY EQ LWA_LIPS-VBELN AND
                                       ITEM     EQ LWA_LIPS-POSNR.
      ADD 1 TO LWA_COUNT.
    ENDLOOP.

    IF LWA_LIPS-LFIMG NE LWA_COUNT.
      CLV_INPICK = ABAP_TRUE.
    ENDIF.

* Check Mat master has Serial Profile ?
    SELECT SINGLE *
      FROM MARC
      INTO LWA_MARC
     WHERE WERKS EQ LWA_LIPS-WERKS
       AND MATNR EQ LWA_LIPS-MATNR.
* Serial Profile is not initail
    CHECK LWA_MARC-SERNP IS NOT INITIAL.
* Check Serial exist in system ?
    SELECT SINGLE *
      FROM EQUI
      INTO LWA_EQUI
     WHERE MATNR = LWA_LIPS-MATNR
       AND SERNR = LV_SERNR.
    IF SY-SUBRC = 0.
*   Check new serial is the same existing serial in DO item ?
      SELECT SINGLE *
        FROM SER01
        INTO LWA_SER01
       WHERE LIEF_NR = LWA_LIPS-VBELN
         AND POSNR   = LWA_LIPS-POSNR.
      IF SY-SUBRC = 0.
        SELECT SINGLE *
          FROM OBJK
          INTO LWA_OBJK
         WHERE OBKNR = LWA_SER01-OBKNR
           AND MATNR = LWA_LIPS-MATNR
           AND SERNR = LV_SERNR.
*     New Serial
        IF SY-SUBRC <> 0.
*       Check Serial Status
          CLEAR LV_STATUS.
          CALL FUNCTION 'STATUS_TEXT_EDIT'
            EXPORTING
              CLIENT           = SY-MANDT
              FLG_USER_STAT    = ABAP_TRUE
              OBJNR            = LWA_EQUI-OBJNR
              SPRAS            = GC_E
            IMPORTING
              LINE             = LV_STATUS
            EXCEPTIONS
              OBJECT_NOT_FOUND = 1
              OTHERS           = 2.
          IF SY-SUBRC = 0.
            CONDENSE LV_STATUS.
            IF LV_STATUS+0(4) <> LC_CON-ESTO.
              CLV_INVALID = ABAP_TRUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*   Check Serial Number with SLOC
      CLEAR LWA_V_EQUI_EQBS.
      SELECT SINGLE * INTO LWA_V_EQUI_EQBS FROM V_EQUI_EQBS_SML
        WHERE MATNR   = LWA_LIPS-MATNR
          AND SERNR   = LV_SERNR
          AND B_WERK  = LWA_LIPS-WERKS
          AND B_LAGER = LWA_LIPS-LGORT.
      IF SY-SUBRC <> 0.
        CLV_INVALID = ABAP_TRUE.
      ENDIF.
      SELECT SINGLE * INTO LWA_V_EQUI_EQBS FROM V_EQUI_EQBS_SML
        WHERE MATNR   = LWA_LIPS-MATNR
          AND SERNR   = LV_SERNR
          AND B_WERK  = LWA_LIPS-WERKS
          AND B_LAGER = LWA_LIPS-LGORT
          AND LBBSA   = LC_CON-OS.
      IF SY-SUBRC EQ 0.
        CLV_INBLOCK = ABAP_TRUE.
      ENDIF.
    ELSE.
      CLV_INVALID = ABAP_TRUE.
    ENDIF.
  ENDMETHOD.
  METHOD CHECK_DUP_UPLOAD.
*    DATA : LWA_SER01 TYPE SER01.
    SELECT COUNT(*)
      FROM SER01
     WHERE LIEF_NR EQ PLV_CHK_DELIVERY
       AND POSNR   EQ PLV_CHK_ITEM.
    IF SY-SUBRC EQ 0.
      R = ABAP_TRUE.
    ENDIF.
  ENDMETHOD.
  METHOD UPDATE_ERROR.
    DATA : LS_SERIAL LIKE LINE OF GT_SERIAL.
    READ TABLE GT_SERIAL INTO LS_SERIAL INDEX 1.
    IF SY-SUBRC = 0.
      UPDATE ZSDSSDT001 SET SERNF = GC_E
                     WHERE DONUM EQ LS_SERIAL-DELIVERY.
    ENDIF.
  ENDMETHOD.
  METHOD SEND_MAIL.
*    DATA: BEGIN OF ASC_FILE OCCURS 100,
*            STRING TYPE STRING,
*          END OF ASC_FILE.
*    DATA: BEGIN OF ASC_PDF OCCURS 100,
*            STRING TYPE XSTRING,
*          END OF ASC_PDF.

    DATA : LV_MONTH_TEXT TYPE C LENGTH 10.

    DATA : LS_DOCUMENT_DATA       TYPE  SODOCCHGI1,
           LV_PUT_IN_OUTBOX       TYPE  SONV-FLAG,
           LV_SENDER_ADDRESS      TYPE  SOEXTRECI1-RECEIVER,
           LV_SENDER_ADDRESS_TYPE TYPE  SOEXTRECI1-ADR_TYP,
           LV_COMMIT_WORK         TYPE  SONV-FLAG.

    DATA : LV_SENT_TO_ALL   TYPE  SONV-FLAG,
           LV_NEW_OBJECT_ID TYPE  SOFOLENTI1-OBJECT_ID,
           LV_SENDER_ID     TYPE  SOUDK.

    DATA : I_TEXT    TYPE TABLE OF SOLI,
           LV_NUM    TYPE C LENGTH 15,
           TAB_LINE1 TYPE SO_BD_NUM,
           TAB_LINE2 TYPE	SO_BD_NUM,
           LV_FIELEC TYPE SOOD-OBJDES.

    DATA : LO_DOC_BCS TYPE REF TO CL_DOCUMENT_BCS,
           LO_BCS     TYPE REF TO CL_BCS.

    DATA : LO_SENDER TYPE REF TO CL_CAM_ADDRESS_BCS.

    DATA : LO_RECIPIENT TYPE REF TO CL_CAM_ADDRESS_BCS.

    DATA : LT_SOLIX TYPE SOLIX_TAB,
           LS_SOLIX LIKE LINE OF LT_SOLIX.

    DATA : LT_SOLIX_ATTH TYPE SOLIX_TAB,
           LS_SOLIX_ATTH LIKE LINE OF LT_SOLIX.

    DATA : LT_SOLI TYPE SOLI_TAB,
           LS_SOLI LIKE LINE OF LT_SOLI.

    DATA : LT_SOLI_ATTH TYPE SOLI_TAB,
           LS_SOLI_ATTH LIKE LINE OF LT_SOLI.

    DATA : I_SUBJECT TYPE SO_OBJ_DES.

    DATA : LV_SUB TYPE STRING.

    DATA : LV_LINE TYPE SO_TEXT255.

    DATA : LS_RECEI LIKE LINE OF S_RECEI.

*    DATA : LV_CC TYPE ADR6-SMTP_ADDR.

    DATA : LV_CC    TYPE ADR6-SMTP_ADDR, " VALUE 'jakarin_s@tripetch-it.co.th',
           LV_EMAIL TYPE ADR6-SMTP_ADDR. " VALUE 'jakarin_s@tripetch-it.co.th'.

    DATA : LV_SENDER TYPE ADR6-SMTP_ADDR, " VALUE 'jakarin_s@tripetch-it.co.th',
           LV_SNAME  TYPE ADR6-SMTP_ADDR. " VALUE 'jakarin_s@tripetch-it.co.th'.

    CONSTANTS : C_CRET(2) TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>CR_LF.

    CONSTANTS : BEGIN OF LC_CON,
                  HTM TYPE C LENGTH 3 VALUE 'HTM',
                END OF LC_CON.

    CLEAR : LO_BCS,LO_DOC_BCS .

    LV_SNAME  = SY-UNAME.
    LV_SENDER = GET_SENDER_EMAIL( LV_SNAME ).
    LV_SNAME  = GET_SENDER_NAME( LV_SNAME ).

    LV_SUB  = GET_SUBJECT( ).
    LT_SOLI = GET_CONTANT_TEXT( LV_SNAME = LV_SNAME
                                LV_CHECK = LV_CHECK
                                IT_CHK   = IN_CHK
                                LV_TEXT  = LV_TEXT ).

    I_SUBJECT = LV_SUB.
    TRY.
        LO_DOC_BCS = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                            I_TYPE    = LC_CON-HTM
                            I_TEXT    = LT_SOLI
                            I_SUBJECT = I_SUBJECT ).
      CATCH CX_DOCUMENT_BCS.
    ENDTRY.
    TRY.
        LO_BCS = CL_BCS=>CREATE_PERSISTENT( ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_DOCUMENT( I_DOCUMENT = LO_DOC_BCS ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_MESSAGE_SUBJECT( IP_SUBJECT = LV_SUB ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING = LV_SENDER
                                                                 I_ADDRESS_NAME   = LV_SNAME ).
      CATCH CX_ADDRESS_BCS.
    ENDTRY.
    TRY.
        LO_BCS->SET_SENDER( EXPORTING I_SENDER = LO_SENDER ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.
    TRY.
        LOOP AT S_RECEI INTO LS_RECEI.
          LV_EMAIL = LS_RECEI-LOW.
          LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING =  LV_EMAIL ).
        ENDLOOP.
      CATCH CX_ADDRESS_BCS.
    ENDTRY.
    TRY.
        LO_BCS->ADD_RECIPIENT( I_RECIPIENT = LO_RECIPIENT ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.

*--------------------------------------------------------------------*
* Set UTF-8 For Excel
*--------------------------------------------------------------------*
*      DATA: BINARY_CONTENT TYPE SOLIX_TAB,
*            TEXT_STRING    TYPE STRING.
*
*      DATA: XS_CONTENT   TYPE XSTRING,
*            APP_TYPE(50) .
*
*      DATA LENGTH TYPE I.
*
*      APP_TYPE = 'text/plain; charset=utf-8'.
*
*      PERFORM F_GEN_DATA_EXCEL TABLES LT_SOLIX_ATTH
*                                USING LV_LINE
*                                      C_CRET.
*
*      LV_FIELEC = TEXT-101.
*      LO_DOC_BCS->ADD_ATTACHMENT( I_ATTACHMENT_TYPE    = 'XLS'
*                                  I_ATTACHMENT_SUBJECT = LV_FIELEC
*                                  I_ATT_CONTENT_HEX    = LT_SOLIX_ATTH ).
*--------------------------------------------------------------------*
* Set Send Excel
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* CC
*--------------------------------------------------------------------*
*      IF S_CC[] IS NOT INITIAL.
*        LOOP AT S_CC.
*          LV_CC = S_CC-LOW.
*          LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING = LV_CC  ).
*          LO_BCS->ADD_RECIPIENT( I_RECIPIENT = LO_RECIPIENT
*                                 I_COPY      = GC_X ).
*        ENDLOOP.
*      ENDIF.
*--------------------------------------------------------------------*
*Call Function send mail
*--------------------------------------------------------------------*
    TRY.
        LO_BCS->SEND( ).
      CATCH CX_SEND_REQ_BCS.
    ENDTRY.

*--------------------------------------------------------------------*
* Commit Work And Wait
*--------------------------------------------------------------------*
    COMMIT( ).

  ENDMETHOD.
  METHOD COMMIT.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.
  ENDMETHOD.
  METHOD GET_SUBJECT.
    R = TEXT-101.
  ENDMETHOD.
  METHOD GET_CONTANT_TEXT.
    DATA : LS_SOLI TYPE SOLI,
           IN_CHK  LIKE LINE OF IT_CHK.

    CONCATENATE '<body bgcolor = "#E6E6FA">'
                '<FONT COLOR = "#191970" face="Garamond">' '<b>'
                '<p>' 'Dear Sir/Madam,' '</p>'
                '<p>'
         INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE 'Program cannot upload data form text file. Kindly check below detail.'
                '</p>'
                '<center>'
                '<TABLE  width= "100%" border="1">'
                '<TR >'
           INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE'<td align = "LEFT" BGCOLOR = "#708090">'
               '<FONT COLOR = "BLUE"><B>DO No.</B> </FONT>'
               '</td>'
          INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE '<td align = "LEFT" BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"><B>Item</B> </FONT>'
                '</td>'
           INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE'<td align = "LEFT" BGCOLOR = "#708090">'
               '<FONT COLOR = "BLUE"><B>Model</B> </FONT>'
               '</td>'
          INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE '<td align = "LEFT" BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"><B>Serial</B> </FONT>'
                '</td>'
          INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE '<td align = "LEFT"  BGCOLOR = "#708090">'
                '<FONT COLOR = "BLUE"> <B>Message</B> </FONT>'
                '</td></tr>'
           INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.

    LOOP AT IT_CHK INTO IN_CHK.
      IF     LV_CHECK EQ 'f'.
        IF IN_CHK-FLAG NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
      ELSEIF LV_CHECK EQ 'p'.
        IF IN_CHK-PICK NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
      ELSEIF LV_CHECK EQ 'b'.
        IF IN_CHK-BLOCK NE ABAP_TRUE.
          CONTINUE.
        ENDIF.
      ENDIF.

      CONCATENATE '<TR><td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' IN_CHK-DELIVERY '</FONT>'
                  '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.

      CONCATENATE '<td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' IN_CHK-ITEM '</FONT>'
                  '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.

      CONCATENATE '<td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' IN_CHK-MATNR '</FONT>'
                  '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.

      CONCATENATE '<td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' IN_CHK-SERIAL '</FONT>'
                  '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.

      CONCATENATE '<td align = "LEFT">'
                  '<FONT COLOR = "BLUE">' LV_TEXT '</FONT>'
                   '</td>'  INTO LS_SOLI-LINE.
      APPEND LS_SOLI TO R.
      CLEAR LS_SOLI.
    ENDLOOP.

    LS_SOLI-LINE = '</TABLE>'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    LS_SOLI-LINE = '</center>'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    LS_SOLI-LINE =  '<br>Regards,<br />'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    CONCATENATE LV_SNAME '<br />' INTO LS_SOLI-LINE.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    LS_SOLI-LINE = '<br><br><b><center><i><font color = "RED">This is an auto generated Email.'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.
    LS_SOLI-LINE = '</FONT></body>'.
    APPEND LS_SOLI TO R.
    CLEAR LS_SOLI.

  ENDMETHOD.
  METHOD GET_SENDER_EMAIL.
    SELECT SINGLE SMTP_ADDR
      FROM USR21
      INNER JOIN ADR6 ON USR21~PERSNUMBER EQ ADR6~PERSNUMBER AND
                         USR21~ADDRNUMBER EQ ADR6~ADDRNUMBER
      INTO R
      WHERE BNAME EQ I_SNAME.
  ENDMETHOD.
  METHOD GET_SENDER_NAME.
    SELECT SINGLE NAME_TEXT
      FROM USR21
      INNER JOIN ADRP ON USR21~PERSNUMBER EQ ADRP~PERSNUMBER
      INTO R
      WHERE BNAME EQ I_SNAME.
  ENDMETHOD.
  METHOD PROCESS_DATA.
    DATA: LT_SERIAL TYPE TABLE OF BAPIDLVITMSERNO WITH EMPTY KEY,
          LT_RET    TYPE TABLE OF BAPIRET2.

    DATA: LS_SERIAL TYPE  BAPIDLVITMSERNO,
          LS_RET    TYPE  BAPIRET2.

    DATA: L_ODOH      TYPE BAPIOBDLVHDRCHG,
          L_ODOHC     TYPE BAPIOBDLVHDRCTRLCHG,
          L_ODELIVERY TYPE BAPIOBDLVHDRCHG-DELIV_NUMB.

    DATA: LT_BDCDATA  TYPE TABLE OF BDCDATA.

    DATA: IN_CHK LIKE LINE OF IT_CHK.

    DATA: BEGIN OF LS_SER01,
            LIEF_NR TYPE SER01-LIEF_NR,
            POSNR   TYPE SER01-POSNR,
          END OF LS_SER01.
    DATA LT_SER01 LIKE TABLE OF LS_SER01.

    DATA : LV_MODE TYPE C LENGTH 1 VALUE 'N'.

    DATA : LV_MSG  TYPE C LENGTH 100.

    LOOP AT IT_CHK INTO IN_CHK.
      IF SY-TABIX = '1'.
        L_ODOH-DELIV_NUMB  = IN_CHK-DELIVERY.
        L_ODOHC-DELIV_NUMB = IN_CHK-DELIVERY.
        L_ODELIVERY        = IN_CHK-DELIVERY.
      ENDIF.
*
      LS_SERIAL-DELIV_NUMB = IN_CHK-DELIVERY.
      LS_SERIAL-ITM_NUMBER = IN_CHK-ITEM.
      LS_SERIAL-SERIALNO   = IN_CHK-SERIAL.
      APPEND LS_SERIAL TO LT_SERIAL.
      CLEAR LS_SERIAL.
    ENDLOOP.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        HEADER_DATA    = L_ODOH
        HEADER_CONTROL = L_ODOHC
        DELIVERY       = L_ODELIVERY
      TABLES
        ITEM_SERIAL_NO = LT_SERIAL
        RETURN         = LT_RET.

    READ TABLE LT_RET
    WITH KEY TYPE = GC_E TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      COMMIT( ).

      SELECT LIEF_NR POSNR
        FROM SER01
        INTO TABLE LT_SER01
        FOR ALL ENTRIES IN LT_SERIAL
        WHERE LIEF_NR EQ LT_SERIAL-DELIV_NUMB
          AND POSNR   EQ LT_SERIAL-ITM_NUMBER.
      IF LT_SER01[] IS NOT INITIAL.
        UPDATE ZSDSSDT001 SET SERNF = ABAP_TRUE
                        WHERE DONUM EQ L_ODOH-DELIV_NUMB.
      ELSE.
        UPDATE ZSDSSDT001 SET SERNF = GC_E
                        WHERE DONUM EQ L_ODOH-DELIV_NUMB.
      ENDIF.
    ELSE.
      UPDATE ZSDSSDT001 SET SERNF = GC_E
                      WHERE DONUM EQ L_ODOH-DELIV_NUMB.
    ENDIF.

*   post GI
    IF P_GI IS NOT INITIAL.
      SELECT COUNT(*)
        FROM VBUK
       WHERE VBELN EQ L_ODELIVERY
         AND WBSTK EQ GC_C.
      IF SY-SUBRC = 0.
        MESSAGE I001 WITH TEXT-113.
      ELSE.
        LT_BDCDATA = BDC_DYNPRO( I_PROGRAM = 'sapmv50a'
                                 I_DYNPRO  = '4004' ).

        LT_BDCDATA = BDC_FIELD( I_FNAM = 'bdc_cursor'
                                I_FVAL = 'likp-vbeln' ).

        LT_BDCDATA = BDC_FIELD( I_FNAM = 'likp-vbeln'
                                I_FVAL = L_ODELIVERY ).

        LT_BDCDATA = BDC_FIELD( I_FNAM = 'bdc_okcode'
                                I_FVAL = '/00' ).

        LT_BDCDATA = BDC_DYNPRO( I_PROGRAM = 'sapmv50a'
                                 I_DYNPRO  = '1000' ).

        LT_BDCDATA = BDC_FIELD( I_FNAM = 'bdc_okcode'
                                I_FVAL = '=wabu_t' ).

*       Change Outbound Delivery > Post Goods Issue
*       MODE
*       'A' Display screen
*       'E' Display screen only if an error occurs
*       'N' No display

        CALL TRANSACTION GC_VL02N USING BDCDATA MODE LV_MODE.
        IF SY-SUBRC = 0.
          CONCATENATE TEXT-110 L_ODELIVERY TEXT-111
                 INTO LV_MSG SEPARATED BY SPACE.
          MESSAGE S001 WITH LV_MSG.
        ELSE.
          MESSAGE I001 WITH TEXT-112.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD BDC_DYNPRO.
    DATA : LS_R LIKE LINE OF R.
    LS_R-PROGRAM  = I_PROGRAM.
    LS_R-DYNPRO   = I_DYNPRO.
    LS_R-DYNBEGIN = GC_X.
    APPEND LS_R to R.
  ENDMETHOD.
  METHOD BDC_FIELD.
    DATA : LS_R LIKE LINE OF R.
    LS_R-FNAM = I_FNAM.
    LS_R-FVAL = I_FVAL.
    APPEND LS_R to R.
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
