*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0180_CLASS
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
      SAVE.
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

    DATA : BEGIN OF LS_ZSDSCMT010,
             SERNR TYPE ZSDSCMT010-SERNR,
             MATNR TYPE ZSDSCMT010-MATNR,
             GWLDT TYPE ZSDSCMT010-GWLDT,
             GWLEN TYPE ZSDSCMT010-GWLEN,
             TXT04 TYPE ZSDSCMT010-TXT04,
             GIDAT TYPE ZSDSCMT010-GIDAT,
           END OF LS_ZSDSCMT010.
    DATA : LT_ZSDSCMT010 LIKE HASHED TABLE OF LS_ZSDSCMT010 WITH UNIQUE KEY SERNR MATNR.


    DATA : BEGIN OF LS_MAT,
             MATKL TYPE ZSDSCMC002-MATKL,
           END OF LS_MAT.
    DATA : LT_MAT LIKE HASHED TABLE OF LS_MAT WITH UNIQUE KEY MATKL.

    DATA : BEGIN OF LS_MAT_DO,
             MATNR TYPE OBJK-MATNR,
             SERNR TYPE OBJK-SERNR,
           END OF LS_MAT_DO.
    DATA : LT_MAT_DO LIKE HASHED TABLE OF LS_MAT_DO WITH UNIQUE KEY MATNR
                                                                    SERNR.

    DATA : BEGIN OF LS_CHECK_DATA,
             SERNR TYPE ZSDSCMT010-SERNR,
             MATNR TYPE ZSDSCMT010-MATNR,
             TXT04 TYPE ZSDSCMT010-TXT04,
             GIDAT TYPE ZSDSCMT010-GIDAT,
             GWLDT TYPE ZSDSCMT010-GWLDT,
             GWLEN TYPE ZSDSCMT010-GWLEN,
           END OF LS_CHECK_DATA.
    DATA : LT_CHECK_DATA LIKE HASHED TABLE OF LS_CHECK_DATA WITH UNIQUE KEY SERNR
                                                                            MATNR
                                                                            TXT04.

    SELECT ZSDSCMT003~EQUNR,
           ZSDSCMT003~MATNR,
           ZSDSCMT003~SERNR,
           ZSDSCMT003~WRTPK,
           ZSDSCMT003~WRTLT_FLAG,
           ZSDSCMT003~WRTLT,
           ZSDSCMT003~VBELN_VL,
           ZSDSCMT003~FKDAT,
           ZSDSCMT003~WADAT_IST,
           ZSDSCMT003~VBELN_VA,
           ZSDSCMT003~KUNNR,
           ZSDSCMT003~VKBUR,
           ZSDSCMT003~VKGRP,
           ZSDSCMT003~INSDT,
           ZSDSCMT003~COMDT,
           ZSDSCMT003~INSSVO,
           ZSDSCMT003~COMSVO,
           ZSDSCMT003~GI_WRT_BEG,
           ZSDSCMT003~GI_WRT_END,
           ZSDSCMT003~STD_WRT_BEG,
           ZSDSCMT003~STD_WRT_END,
           ZSDSCMT003~CUS_REG_BEG,
           ZSDSCMT003~CUS_REG_END,
           ZSDSCMT003~EXT_WRT_BEG,
           ZSDSCMT003~EXT_WRT_END,
           ZSDSCMT003~MAT_POSID,
           ZSDSCMT003~STD_POSID,
           ZSDSCMT003~EXT_POSID,
           ZSDSCMT003~MIGFLG,
           ZSDSCMT003~UPDFLG,
           ZSDSCMT003~ERNAM,
           ZSDSCMT003~ERDAT,
           ZSDSCMT003~ERZET,
           ZSDSCMT003~AENAM,
           ZSDSCMT003~AEDAT,
           ZSDSCMT003~AEZET,
           MARA~MATKL,
           BGMKOBJ~GWLDT,
           BGMKOBJ~GWLEN
      FROM ZSDSCMT003
      INNER JOIN MARA    ON ZSDSCMT003~MATNR   EQ MARA~MATNR
      LEFT JOIN  EQUI    ON ZSDSCMT003~EQUNR   EQ EQUI~EQUNR
      LEFT JOIN  BGMKOBJ ON EQUI~OBJNR    EQ BGMKOBJ~J_OBJNR AND
                            BGMKOBJ~GAART EQ '1'
      WHERE ZSDSCMT003~EQUNR     IN @S_EQUNR
        AND ZSDSCMT003~MATNR     IN @S_MATNR
        AND ZSDSCMT003~SERNR     IN @S_SERNR
        AND ZSDSCMT003~WADAT_IST IN @S_WADAT
        AND ZSDSCMT003~ERNAM     IN @S_ERNAM
        AND ZSDSCMT003~ERDAT     IN @S_ERDAT
        AND ZSDSCMT003~ERZET     IN @S_ERZET
        AND ZSDSCMT003~AENAM     IN @S_AENAM
        AND ZSDSCMT003~AEDAT     IN @S_AEDAT
        AND ZSDSCMT003~AEZET     IN @S_AEZET
     INTO TABLE @DATA(LT_TMP).

    IF LT_TMP IS NOT INITIAL.
      LT_MAT    =  CORRESPONDING #( LT_TMP DISCARDING DUPLICATES ).
      LT_MAT_DO =  CORRESPONDING #( LT_TMP DISCARDING DUPLICATES ).

      SELECT ZSDSCMC002~WRTPK,
             ZSDSCMC002~MATKL
        FROM @LT_MAT AS A
        INNER JOIN ZSDSCMC002 ON A~MATKL EQ ZSDSCMC002~MATKL
        INTO TABLE @DATA(LT_ZSDSCMC002).

      SELECT SER01~OBKNR,
             SER01~LIEF_NR,
             SER01~POSNR,
             SER01~DATUM, " gi date
             SER01~VBTYP, " J Send T return
             SER01~BWART, " 601 good issue
             OBJK~MATNR,
             OBJK~SERNR,
             LIKP~LFDAT
        FROM @LT_MAT_DO AS A
        INNER JOIN OBJK ON A~MATNR    EQ OBJK~MATNR AND
                           A~SERNR    EQ OBJK~SERNR AND
                           OBJK~TASER EQ 'SER01'
        INNER JOIN SER01 ON OBJK~OBKNR    EQ SER01~OBKNR
        INNER JOIN LIKP  ON SER01~LIEF_NR EQ LIKP~VBELN
        INTO TABLE @DATA(LT_DO).
      SORT LT_DO BY OBKNR DESCENDING.

      SELECT ZSDSCMT010~SERNR,
             ZSDSCMT010~MATNR,
             ZSDSCMT010~GWLDT,
             ZSDSCMT010~GWLEN,
             ZSDSCMT010~TXT04,
             ZSDSCMT010~GIDAT
        FROM @LT_MAT_DO AS A
        INNER JOIN ZSDSCMT010 ON A~MATNR EQ ZSDSCMT010~MATNR AND
                                 A~SERNR EQ ZSDSCMT010~SERNR
        INTO TABLE @LT_ZSDSCMT010.

      LT_CHECK_DATA =  CORRESPONDING #( LT_TMP  DISCARDING DUPLICATES ).

      LOOP AT LT_TMP INTO DATA(LS_TMP).
        READ TABLE LT_ZSDSCMC002 INTO DATA(LS_ZSDSCMC002)
        WITH KEY MATKL = LS_TMP-MATKL.
        IF SY-SUBRC EQ 0.
          GS_RESULT-WRTPK        = LS_ZSDSCMC002-WRTPK.
        ENDIF.

        READ TABLE LT_CHECK_DATA INTO DATA(LS_CHECK_ECC)
        WITH TABLE KEY MATNR = LS_TMP-MATNR
                       SERNR = LS_TMP-SERNR
                       TXT04 = 'ECUS'.
        IF SY-SUBRC EQ 0.
          MOVE-CORRESPONDING LS_TMP TO GS_RESULT.
          GS_RESULT-WADAT_IST    = LS_CHECK_ECC-GIDAT.
          GS_RESULT-STD_WRT_BEG  = LS_CHECK_ECC-GWLDT.
          GS_RESULT-STD_WRT_END  = LS_CHECK_ECC-GWLEN.
          GS_RESULT-AENAM = SY-UNAME.
          GS_RESULT-AEDAT = SY-DATUM.
          GS_RESULT-AEZET = SY-UZEIT.
        ELSE.
          READ TABLE LT_DO INTO DATA(LS_DO)
          WITH KEY MATNR = LS_TMP-MATNR
                   SERNR = LS_TMP-SERNR.
          IF SY-SUBRC EQ 0.
            CASE LS_DO-LIEF_NR.
              WHEN LS_TMP-VBELN_VL.
                MOVE-CORRESPONDING LS_TMP TO GS_RESULT.
                GS_RESULT-AENAM        = SY-UNAME.
                GS_RESULT-AEDAT        = SY-DATUM.
                GS_RESULT-AEZET        = SY-UZEIT.
              WHEN OTHERS.
                IF LS_DO-VBTYP NE 'J'.
                  MOVE-CORRESPONDING LS_TMP TO GS_RESULT.
                  GS_RESULT-AENAM        = SY-UNAME.
                  GS_RESULT-AEDAT        = SY-DATUM.
                  GS_RESULT-AEZET        = SY-UZEIT.
*                CLEAR : GS_RESULT-GI_WRT_BEG,
*                        GS_RESULT-GI_WRT_END,
*                        GS_RESULT-STD_WRT_BEG,
*                        GS_RESULT-STD_WRT_END,
*                        GS_RESULT-CUS_REG_BEG,
*                        GS_RESULT-CUS_REG_END,
*                        GS_RESULT-EXT_WRT_BEG,
*                        GS_RESULT-EXT_WRT_END.
                ELSE.
                  GS_RESULT-VBELN_VL   = LS_DO-LIEF_NR.
                  GS_RESULT-FKDAT      = LS_DO-LFDAT.
                  GS_RESULT-WADAT_IST    = LS_DO-DATUM.
                  GS_RESULT-VBELN_VL     = LS_DO-LIEF_NR.
                  GS_RESULT-FKDAT        = LS_DO-LFDAT.
                  GS_RESULT-VBELN_VA     = SPACE.
                  GS_RESULT-KUNNR        = SPACE.
                  GS_RESULT-VKBUR        = SPACE.
                  GS_RESULT-VKGRP        = SPACE.
                  GS_RESULT-INSDT        = SPACE.
                  GS_RESULT-COMDT        = SPACE.
                  GS_RESULT-INSSVO       = SPACE.
                  GS_RESULT-COMSVO       = SPACE.
                  GS_RESULT-GI_WRT_BEG   = SPACE.
                  GS_RESULT-GI_WRT_END   = SPACE.

                  IF LS_TMP-GWLDT IS NOT INITIAL.
                    GS_RESULT-STD_WRT_BEG  = LS_TMP-GWLDT.
                  ELSE.
                    READ TABLE LT_ZSDSCMT010 INTO LS_ZSDSCMT010
                    WITH TABLE KEY SERNR = LS_TMP-SERNR
                                   MATNR = LS_TMP-MATNR.
                    IF SY-SUBRC EQ 0.
                      GS_RESULT-STD_WRT_BEG  = LS_ZSDSCMT010-GWLDT.
                    ENDIF.
                  ENDIF.

                  IF LS_TMP-GWLEN IS NOT INITIAL.
                    GS_RESULT-STD_WRT_END  = LS_TMP-GWLEN.
                  ELSE.
                    READ TABLE LT_ZSDSCMT010 INTO LS_ZSDSCMT010
                    WITH TABLE KEY SERNR = LS_TMP-SERNR
                                   MATNR = LS_TMP-MATNR.
                    IF SY-SUBRC EQ 0.
                      GS_RESULT-STD_WRT_END  = LS_ZSDSCMT010-GWLEN.
                    ENDIF.
                  ENDIF.

                  GS_RESULT-CUS_REG_BEG  = SPACE.
                  GS_RESULT-CUS_REG_END  = SPACE.
                  GS_RESULT-EXT_WRT_BEG  = SPACE.
                  GS_RESULT-EXT_WRT_END  = SPACE.
                  GS_RESULT-MAT_POSID    = SPACE.
                  GS_RESULT-STD_POSID    = SPACE.
                  GS_RESULT-EXT_POSID    = SPACE.
                  GS_RESULT-MIGFLG       = SPACE.
                  GS_RESULT-UPDFLG       = SPACE.
                  GS_RESULT-ERNAM        = LS_TMP-ERNAM.
                  GS_RESULT-ERDAT        = LS_TMP-ERDAT.
                  GS_RESULT-ERZET        = LS_TMP-ERZET.
                  GS_RESULT-AENAM        = SY-UNAME.
                  GS_RESULT-AEDAT        = SY-DATUM.
                  GS_RESULT-AEZET        = SY-UZEIT.
                ENDIF.
            ENDCASE.
          ELSE.
            CLEAR : GS_RESULT.
            READ TABLE LT_ZSDSCMT010 INTO LS_ZSDSCMT010
            WITH TABLE KEY SERNR = LS_TMP-SERNR
                           MATNR = LS_TMP-MATNR.
            IF SY-SUBRC EQ 0.
              GS_RESULT-STD_WRT_BEG  = LS_ZSDSCMT010-GWLDT.
              GS_RESULT-WADAT_IST    = LS_ZSDSCMT010-GWLDT.
            ENDIF.

            READ TABLE LT_ZSDSCMT010 INTO LS_ZSDSCMT010
            WITH TABLE KEY SERNR = LS_TMP-SERNR
                           MATNR = LS_TMP-MATNR.
            IF SY-SUBRC EQ 0.
              GS_RESULT-STD_WRT_END  = LS_ZSDSCMT010-GWLEN.
            ENDIF.

            READ TABLE LT_ZSDSCMC002 INTO LS_ZSDSCMC002
            WITH KEY MATKL = LS_TMP-MATKL.
            IF SY-SUBRC EQ 0.
              GS_RESULT-WRTPK        = LS_ZSDSCMC002-WRTPK.
            ENDIF.

            GS_RESULT-ERNAM        = LS_TMP-ERNAM.
            GS_RESULT-ERDAT        = LS_TMP-ERDAT.
            GS_RESULT-ERZET        = LS_TMP-ERZET.
            GS_RESULT-AENAM        = SY-UNAME.
            GS_RESULT-AEDAT        = SY-DATUM.
            GS_RESULT-AEZET        = SY-UZEIT.
          ENDIF.
        ENDIF.

        IF GS_RESULT-WADAT_IST   IS NOT INITIAL AND
           GS_RESULT-STD_WRT_END IS INITIAL.
          GS_RESULT-STD_WRT_BEG = GS_RESULT-WADAT_IST.
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              DATE      = GS_RESULT-WADAT_IST
              DAYS      = '00'
              MONTHS    = '18'
              SIGNUM    = '+'
              YEARS     = '00'
            IMPORTING
              CALC_DATE = GS_RESULT-STD_WRT_END.
        ENDIF.

        GS_RESULT-EQUNR        = LS_TMP-EQUNR.
        GS_RESULT-MATNR        = LS_TMP-MATNR.
        GS_RESULT-SERNR        = LS_TMP-SERNR.

        APPEND GS_RESULT TO GT_RESULT.
        CLEAR : GS_RESULT.
      ENDLOOP.
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
  METHOD SAVE.
    MODIFY ZSDSCMT003 FROM TABLE GT_RESULT.
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
