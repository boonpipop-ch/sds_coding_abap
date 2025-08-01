*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0780_CLASS
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
      CLEAR_DATA,
      REJECT,
      CHECK_DOC.
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
      SELECT
             VBAK~VBELN,
             VBAK~KUNNR,
             VBAK~VKGRP,
             VBAK~VKBUR,
             A~ERDAT,
             A~ERNAM,
             VBAK~PS_PSP_PNR,
             A~AEDAT,
             A~AENAM,
*             VBAP~MATNR,
*             VBAP~ARKTX,
*             VBAP~POSNR,
*             VBAP~VRKME,
*             VBAP~WAERK,
*             VBAP~NETWR,
             A~STATU,
             A~REMARK_FI,
             A~REMARK_SD,
             FPLA~FPLNR,
             VBAK~NETWR,
             FPLT~FPLTR,
             FPLT~FAKWR,
             FPLT~FAKSP

     FROM ZSDSSDT022 AS A
             INNER JOIN VBAK ON A~VBELN EQ VBAK~VBELN

             LEFT JOIN FPLA  ON VBAK~VBELN EQ FPLA~VBELN
                            AND FPLA~RFPLN EQ @SPACE
                            AND FPLA~FPART EQ 'Z2'
            LEFT  JOIN FPLT ON FPLA~FPLNR EQ FPLT~FPLNR AND
                               ( FPLT~FPLTR EQ A~FPLTR OR
                                 FPLT~FPLTR EQ '000000' )
            WHERE A~STATU EQ 'SAV'
             AND   A~VBELN IN @S_VBELN
             AND    A~ERDAT IN @S_ERDAT
             AND    A~ERNAM IN @S_ERNAM
             AND    VBAK~AUART IN @S_AUART
             INTO TABLE @GT_RESULT.

    ELSEIF R2 EQ ABAP_TRUE.
      SELECT     VBAK~VBELN,
                 VBAK~KUNNR,
                 VBAK~VKGRP,
                 VBAK~VKBUR,
                 VBAK~ERDAT,
                 VBAK~ERNAM,
                 VBAK~PS_PSP_PNR,
                 VBAK~AEDAT,
                 @SPACE AS AENAM,
*                 VBAP~MATNR,
*                 VBAP~ARKTX,
*                 VBAP~POSNR,
*                 VBAP~VRKME,
*                 VBAP~WAERK,
*                 VBAP~NETWR,
                 A~STATU,
                 A~REMARK_FI,
                 A~REMARK_SD,
                FPLA~FPLNR,
                VBAK~NETWR,
                FPLT~FPLTR,
                FPLT~FAKWR,
                FPLT~FAKSP

       FROM ZSDSSDT022 AS A
                  INNER JOIN VBAK ON A~VBELN   EQ VBAK~VBELN
                  LEFT  JOIN FPLA ON VBAK~VBELN EQ FPLA~VBELN
                                 AND FPLA~RFPLN EQ @SPACE
                                 AND FPLA~FPART EQ 'Z2'
                  LEFT JOIN FPLT  ON FPLT~FPLNR EQ FPLA~FPLNR AND
                                   ( FPLT~FPLTR EQ A~FPLTR OR
                                     FPLT~FPLTR EQ '000000' )
                  WHERE  A~STATU  EQ 'APP'
                  AND    A~VBELN  IN @S_VBELN
                  AND    A~ERDAT  IN @S_ERDAT
                  AND    A~ERNAM  IN @S_ERNAM
                  AND    VBAK~AUART IN @S_AUART
                  INTO TABLE @GT_RESULT.

    ELSEIF R3 EQ ABAP_TRUE.

      SELECT   VBAK~VBELN,
               VBAK~KUNNR,
               VBAK~VKGRP,
               VBAK~VKBUR,
               VBAK~ERDAT,
               VBAK~ERNAM,
               VBAK~PS_PSP_PNR,
               VBAK~AEDAT,
               @SPACE AS AENAM,
*               VBAP~MATNR,
*               VBAP~ARKTX,
*               VBAP~POSNR,
*               VBAP~VRKME,
*               VBAP~WAERK,
*               VBAP~NETWR,
               A~STATU,
               A~REMARK_FI,
               A~REMARK_SD,
               FPLA~FPLNR,
               VBAK~NETWR,
               FPLT~FPLTR,
               fPLT~FAKWR,
               FPLT~FAKSP


        FROM ZSDSSDT022 AS A
                INNER JOIN VBAK ON A~VBELN EQ VBAK~VBELN
                LEFT  JOIN FPLA ON VBAK~VBELN EQ FPLA~VBELN
                               AND FPLA~RFPLN EQ @SPACE
                               AND FPLA~FPART EQ 'Z2'
                LEFT  JOIN FPLT ON FPLT~FPLNR EQ FPLA~FPLNR AND
                                 ( FPLT~FPLTR EQ A~FPLTR OR
                                   FPLT~FPLTR EQ '000000' )
                WHERE  A~VBELN IN @S_VBELN
                AND    A~ERDAT IN @S_ERDAT
                AND    A~ERNAM IN @S_ERNAM
                AND    VBAK~AUART IN @S_AUART
                INTO TABLE @GT_RESULT.


    ENDIF.
    DATA : BEGIN OF LS_CUSTNAME,

             KUNNR TYPE KNA1-KUNNR,

           END OF LS_CUSTNAME.
    DATA : LT_CUSTNAME LIKE HASHED TABLE OF LS_CUSTNAME WITH UNIQUE KEY KUNNR.

    LT_CUSTNAME =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).

    SELECT A~KUNNR,
           KNA1~NAME1

     FROM @LT_CUSTNAME AS A
      INNER JOIN KNA1 ON A~KUNNR EQ KNA1~KUNNR
      INTO TABLE  @DATA(LT_CUSTNAME1).


    DATA : BEGIN OF LS_SALESGP_SALESOF,

             VKGRP TYPE VBAK-VKGRP,
             VKBUR TYPE VBAK-VKBUR,

           END OF LS_SALESGP_SALESOF.
    DATA : LT_SALESGP_SALESOF LIKE HASHED TABLE OF LS_SALESGP_SALESOF WITH UNIQUE KEY VKGRP,
           VKBUR.

    LT_SALESGP_SALESOF =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).



    SELECT
            TVKBT~BEZEI AS BEZEI_GP ,
            TVGRT~BEZEI AS BEZEI_OF ,
            A~VKGRP,
            A~VKBUR

     FROM @LT_SALESGP_SALESOF AS A
      INNER JOIN TVGRT ON A~VKGRP EQ TVGRT~VKGRP
      INNER JOIN TVKBT ON A~VKBUR EQ TVKBT~VKBUR

      INTO TABLE  @DATA(LT_SALESGP_SALESOF1).
*
*
*

    LOOP AT GT_RESULT ASSIGNING FIELD-SYMBOL(<LFS_DATA>).

      READ TABLE LT_CUSTNAME1 INTO DATA(LS_CUSTNAME1)
      WITH KEY KUNNR = <LFS_DATA>-KUNNR.
      IF SY-SUBRC = 0.
        <LFS_DATA>-NAME1 = LS_CUSTNAME1-NAME1.

      ENDIF.

      READ TABLE LT_SALESGP_SALESOF1 INTO DATA(LS_SALESGP_SALESOF1)
       WITH KEY VKGRP = <LFS_DATA>-VKGRP.
      IF SY-SUBRC = 0.
        <LFS_DATA>-BEZEI_GP = LS_SALESGP_SALESOF1-BEZEI_GP.
        <LFS_DATA>-BEZEI_OF = LS_SALESGP_SALESOF1-BEZEI_OF.

      ENDIF.

    ENDLOOP.

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

      IF LV_RUNNING EQ 13 OR
         LV_RUNNING EQ 14.
        CONTINUE.
      ENDIF.

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

      IF LV_RUNNING = 21.
        LS_FCAT-EDIT        = ABAP_TRUE.
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

    DATA : LS_ZSDSSDT022 TYPE ZSDSSDT022,
           LT_ZSDSSDT022 TYPE TABLE OF ZSDSSDT022.

    DATA : BEGIN OF LS_Doc,
             VBELN TYPE ZSDSSDT022-VBELN,
             FPLTR TYPE ZSDSSDT022-FPLTR,

           END OF LS_Doc.
    DATA : LT_Doc LIKE HASHED TABLE OF LS_Doc WITH UNIQUE KEY VBELN,
           POSNR.


    LT_Doc =  CORRESPONDING #( GT_RESULT DISCARDING DUPLICATES ).

    SELECT ZSDSSDT022~VBELN,
           ZSDSSDT022~FPLTR,
           ZSDSSDT022~ERNAM_FI,
           ZSDSSDT022~ERDAT_FI,
           ZSDSSDT022~ERZET_FI,
           ZSDSSDT022~ERNAM,
           ZSDSSDT022~ERDAT,
           ZSDSSDT022~ERZET,
           ZSDSSDT022~AENAM,
           ZSDSSDT022~AEDAT,
           ZSDSSDT022~AEZET

      FROM @LT_Doc AS A
      INNER JOIN ZSDSSDT022 ON A~VBELN EQ ZSDSSDT022~VBELN
                           AND A~FPLTR EQ ZSDSSDT022~FPLTR
      INTO TABLE @DATA(LT_TMP).

    LOOP AT GT_RESULT INTO DATA(LS_RESULT) WHERE CHECK EQ ABAP_TRUE.
      LS_ZSDSSDT022-VBELN = LS_RESULT-VBELN.
      LS_ZSDSSDT022-FPLTR = LS_RESULT-FPLTR.
      LS_ZSDSSDT022-STATU = 'APP'.

      READ TABLE LT_TMP INTO DATA(LS_TMP)
      WITH KEY VBELN = LS_RESULT-VBELN
               FPLTR = LS_RESULT-FPLTR.
      IF SY-SUBRC = 0.
        IF LS_TMP-ERNAM_FI IS NOT INITIAL .
          LS_ZSDSSDT022-ERNAM_FI = LS_TMP-ERNAM_FI.
          LS_ZSDSSDT022-ERDAT_FI = LS_TMP-ERDAT_FI.
          LS_ZSDSSDT022-ERZET_FI = LS_TMP-ERZET_FI.
        ELSE .
          LS_ZSDSSDT022-ERNAM_FI = SY-UNAME.
          LS_ZSDSSDT022-ERDAT_FI = SY-DATUM.
          LS_ZSDSSDT022-ERZET_FI = SY-UZEIT.
        ENDIF.
        LS_ZSDSSDT022-ERNAM    = LS_TMP-ERNAM.
        LS_ZSDSSDT022-ERDAT    = LS_TMP-ERDAT.
        LS_ZSDSSDT022-ERZET    = LS_TMP-ERZET.
        LS_ZSDSSDT022-AENAM    = LS_TMP-AENAM.
        LS_ZSDSSDT022-AEDAT    = LS_TMP-AEDAT.
        LS_ZSDSSDT022-AEZET    = LS_TMP-AEZET.

*      ELSE.
*        LS_ZSDSSDT022-ERNAM_FI = SY-UNAME.
*        LS_ZSDSSDT022-ERDAT_FI = SY-DATUM.
*        LS_ZSDSSDT022-ERZET_FI = SY-UZEIT.
      ENDIF.

      LS_ZSDSSDT022-AENAM_FI = SY-UNAME.
      LS_ZSDSSDT022-AEDAT_FI = SY-DATUM.
      LS_ZSDSSDT022-AEZET_FI = SY-UZEIT.
      LS_ZSDSSDT022-REMARK_FI = LS_RESULT-REMARK_FI.
      LS_ZSDSSDT022-REMARK_SD = LS_RESULT-REMARK_SD.

      APPEND LS_ZSDSSDT022 TO LT_ZSDSSDT022.

    ENDLOOP.

    MODIFY ZSDSSDT022 FROM TABLE LT_ZSDSSDT022.
    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD CLEAR_DATA..

    CLEAR : GT_RESULT ,GS_RESULT.

  ENDMETHOD.
  METHOD REJECT.
    DATA : LS_ZSDSSDT022 TYPE ZSDSSDT022,
           LT_ZSDSSDT022 TYPE TABLE OF ZSDSSDT022.

    DATA : BEGIN OF LS_Doc,
             VBELN TYPE ZSDSSDT022-VBELN,
             FPLTR TYPE ZSDSSDT022-FPLTR,

           END OF LS_Doc.
    DATA : LT_Doc LIKE HASHED TABLE OF LS_Doc WITH UNIQUE KEY VBELN,
           FPLTR.


    LT_Doc =  CORRESPONDING #( GT_RESULT DISCARDING DUPLICATES ).

    SELECT ZSDSSDT022~VBELN,
           ZSDSSDT022~FPLTR,
           ZSDSSDT022~ERNAM_FI,
           ZSDSSDT022~ERDAT_FI,
           ZSDSSDT022~ERZET_FI,
           ZSDSSDT022~ERNAM,
           ZSDSSDT022~ERDAT,
           ZSDSSDT022~ERZET,
           ZSDSSDT022~AENAM,
           ZSDSSDT022~AEDAT,
           ZSDSSDT022~AEZET

      FROM @LT_Doc AS A
      INNER JOIN ZSDSSDT022 ON A~VBELN EQ ZSDSSDT022~VBELN
                           AND A~FPLTR EQ ZSDSSDT022~FPLTR
      INTO TABLE @DATA(LT_TMP).

    LOOP AT GT_RESULT INTO DATA(LS_RESULT) WHERE CHECK EQ ABAP_TRUE.
      LS_ZSDSSDT022-VBELN = LS_RESULT-VBELN.
      LS_ZSDSSDT022-FPLTR = LS_RESULT-FPLTR.
      LS_ZSDSSDT022-STATU = 'REJ'.

      READ TABLE LT_TMP INTO DATA(LS_TMP)
      WITH KEY VBELN = LS_RESULT-VBELN
               FPLTR = LS_RESULT-FPLTR.
      IF SY-SUBRC = 0.
        IF LS_TMP-ERNAM IS NOT INITIAL.

          LS_ZSDSSDT022-ERNAM_FI = LS_TMP-ERNAM_FI.
          LS_ZSDSSDT022-ERDAT_FI = LS_TMP-ERDAT_FI.
          LS_ZSDSSDT022-ERZET_FI = LS_TMP-ERZET_FI.
        ELSE.
          LS_ZSDSSDT022-ERNAM_FI = SY-UNAME.
          LS_ZSDSSDT022-ERDAT_FI = SY-DATUM.
          LS_ZSDSSDT022-ERZET_FI = SY-UZEIT.
        ENDIF.
        LS_ZSDSSDT022-ERNAM    = LS_TMP-ERNAM.
        LS_ZSDSSDT022-ERDAT    = LS_TMP-ERDAT.
        LS_ZSDSSDT022-ERZET    = LS_TMP-ERZET.
        LS_ZSDSSDT022-AENAM    = LS_TMP-AENAM.
        LS_ZSDSSDT022-AEDAT    = LS_TMP-AEDAT.
        LS_ZSDSSDT022-AEZET    = LS_TMP-AEZET.
*      ELSE.
*        LS_ZSDSSDT022-ERNAM_FI = SY-UNAME.
*        LS_ZSDSSDT022-ERDAT_FI = SY-DATUM.
*        LS_ZSDSSDT022-ERZET_FI = SY-UZEIT.

      ENDIF.
      LS_ZSDSSDT022-AENAM_FI = SY-UNAME.
      LS_ZSDSSDT022-AEDAT_FI = SY-DATUM.
      LS_ZSDSSDT022-AEZET_FI = SY-UZEIT.
      LS_ZSDSSDT022-REMARK_FI = LS_RESULT-REMARK_FI.
      LS_ZSDSSDT022-REMARK_SD = LS_RESULT-REMARK_SD.

      APPEND LS_ZSDSSDT022 TO LT_ZSDSSDT022.

    ENDLOOP.

    MODIFY ZSDSSDT022 FROM TABLE LT_ZSDSSDT022.
    COMMIT WORK AND WAIT.
  ENDMETHOD.
  METHOD CHECK_DOC.


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
