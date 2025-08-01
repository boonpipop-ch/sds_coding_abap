*&---------------------------------------------------------------------*
*& Include          ZSDSSDI0030_CLASS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZSDSFII0020_CLASS
*&---------------------------------------------------------------------*
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
      CONVERT_ALPHA_IN  IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY,
      CONVERT_ALPHA_OUT IMPORTING I_DATA TYPE ANY
                        EXPORTING E_DATA TYPE ANY,
      GET_DATA_FROM_LIKP,
      GET_DATA_FROM_Z,
      GET_SHIP_TO_ADDR,
      GET_STATUS_SEND_DATA,
      GET_SHIPING_POINT,
      GET_VBUK CHANGING C_TABLE TYPE ANY TABLE,
      READ_TEXT,
      READ_TEXT_INV,
      INIT_DATA,
      UPDATE_STATUS_DO,
      ENDPOINT RETURNING VALUE(R) TYPE STRING,
      HEADER_API IMPORTING I_DATA   TYPE STRING
                 RETURNING VALUE(R) TYPE ZSDSCAS001_TT,
      BODY_API RETURNING VALUE(R)   TYPE STRING,
      BODY_API_LEN IMPORTING I_DATA   TYPE STRING
                   RETURNING VALUE(R) TYPE I,
      GET_SMART_TRACKING.
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
    IF S_WMSDT[] IS INITIAL AND
       S_STAPC[] IS INITIAL AND
       S_LOADD[] IS INITIAL AND
       S_CONTD[] IS INITIAL AND
       S_DEALC[] IS INITIAL AND
       S_SERNF[] IS INITIAL.
      GET_DATA_FROM_LIKP( ).
    ELSE.
      GET_DATA_FROM_Z( ).
    ENDIF.

    IF GT_RESULT IS NOT INITIAL.
      SORT GT_RESULT BY LFDAT.
      DELETE ADJACENT DUPLICATES FROM GT_RESULT COMPARING ALL FIELDS.
    ENDIF.
  ENDMETHOD.
  METHOD GET_ADDTIONAL_DATA.

    DATA : LV_TABIX TYPE SY-TABIX.

    DATA : BEGIN OF LS_VBUK,
             VBELN TYPE VBUK-VBELN,
           END OF LS_VBUK.
    DATA LT_VBUK LIKE TABLE OF LS_VBUK.

    GET_SHIP_TO_ADDR( ).
    GET_STATUS_SEND_DATA( ).
    GET_SHIPING_POINT( ).
    GET_SMART_TRACKING( ).

    GET_VBUK( CHANGING C_TABLE = LT_VBUK ).

    LOOP AT GT_LIKP INTO GS_LIKP.
      READ TABLE GT_RESULT INTO GS_RESULT
      WITH KEY DONUM = GS_LIKP-VBELN.
      IF SY-SUBRC = 0.
        LV_TABIX = SY-TABIX.

        READ TABLE GT_VBPA INTO GS_VBPA
        WITH KEY VBELN = GS_LIKP-VBELN_S.

        IF SY-SUBRC = 0.
          GS_RESULT-SDS_CUSTCODE = GS_VBPA-KUNNR.
          CONCATENATE GS_VBPA-NAME1 GS_VBPA-NAME2 GS_VBPA-NAME3 GS_VBPA-NAME4 INTO GS_RESULT-SDS_CUSTNAME SEPARATED BY SPACE.
          CONCATENATE GS_VBPA-STREET GS_VBPA-STR_SUPPL1 GS_VBPA-STR_SUPPL2
                      GS_VBPA-STR_SUPPL3 GS_VBPA-LOCATION GS_VBPA-POST_CODE1 INTO GS_RESULT-SDS_SHIPADDR SEPARATED BY SPACE.

          GS_RESULT-SDS_SHPPV = GS_VBPA-CITY1.
        ENDIF.

        READ_TEXT( ).
        READ_TEXT_INV( ).
        READ TABLE GT_ZTMM_DO_EXPORT INTO GS_ZTMM_DO_EXPORT
        WITH KEY VBELN = GS_LIKP-VBELN.
        IF SY-SUBRC = 0.
          GS_RESULT-ERZETE = GS_ZTMM_DO_EXPORT-ERZET.
          GS_RESULT-ERDATE = GS_ZTMM_DO_EXPORT-ERDAT.
          GS_RESULT-AMPMF  = GS_ZTMM_DO_EXPORT-AM_PM.
          GS_RESULT-REMAP  = GS_ZTMM_DO_EXPORT-REQ_MAP.
          GS_RESULT-REINV  = GS_ZTMM_DO_EXPORT-REQ_INV.
        ENDIF.
        GS_RESULT-AMPMF_S = GS_RESULT-AMPMF.
        GS_RESULT-REMAP_S = GS_RESULT-REMAP.
        GS_RESULT-REINV_S = GS_RESULT-REINV.
        GS_RESULT-DOSIG_D = GS_RESULT-DOSIG.
        GS_RESULT-PODDA_D = GS_RESULT-PODDA+0(8).
        GS_RESULT-LFDAT   = GS_LIKP-LFDAT.
        GS_RESULT-BEZEI_G = GS_LIKP-BEZEI_G.
        GS_RESULT-BEZEI_O = GS_LIKP-BEZEI_O.
        GS_RESULT-VTEXT   = GS_LIKP-VTEXT.
        GS_RESULT-VTWEG   = GS_LIKP-VTWEG.
        GS_RESULT-VKGRP   = GS_LIKP-VKGRP.
        GS_RESULT-VKBUR   = GS_LIKP-VKBUR.
        GS_RESULT-ERZETD  = GS_LIKP-ERZET.
        GS_RESULT-ERDATD  = GS_LIKP-ERDAT.

        READ TABLE GT_TVLAT INTO GS_TVLAT
        WITH KEY VSTEL = GS_LIKP-VSTEL
                 LSTEL = GS_LIKP-LSTEL.
        IF SY-SUBRC = 0.
          GS_RESULT-LOADP = GS_TVLAT-VTEXT.
        ENDIF.

        READ TABLE LT_VBUK INTO LS_VBUK
        WITH KEY VBELN = GS_LIKP-VBELN.
        IF SY-SUBRC = 0.
          CONCATENATE GS_RESULT-SERNF 'S' INTO GS_RESULT-STATU_U.
        ELSE.
          GS_RESULT-STATU_U = GS_RESULT-SERNF.
        ENDIF.

        MODIFY GT_RESULT FROM GS_RESULT INDEX LV_TABIX.
      ELSE.
        READ TABLE GT_VBPA INTO GS_VBPA
        WITH KEY VBELN = GS_LIKP-VBELN_S.

        IF SY-SUBRC = 0.
          GS_RESULT-SDS_CUSTCODE = GS_VBPA-KUNNR.
          CONCATENATE GS_VBPA-NAME1 GS_VBPA-NAME2 GS_VBPA-NAME3 GS_VBPA-NAME4 INTO GS_RESULT-SDS_CUSTNAME SEPARATED BY SPACE.
          CONCATENATE GS_VBPA-STREET GS_VBPA-STR_SUPPL1 GS_VBPA-STR_SUPPL2
                      GS_VBPA-STR_SUPPL3 GS_VBPA-LOCATION GS_VBPA-POST_CODE1 INTO GS_RESULT-SDS_SHIPADDR SEPARATED BY SPACE.

          GS_RESULT-SDS_SHPPV = GS_VBPA-CITY1.
        ENDIF.

        READ_TEXT( ).
        READ_TEXT_INV( ).

        READ TABLE GT_TVLAT INTO GS_TVLAT
        WITH KEY VSTEL = GS_LIKP-VSTEL
                 LSTEL = GS_LIKP-LSTEL.
        IF SY-SUBRC = 0.
          GS_RESULT-LOADP = GS_TVLAT-VTEXT.
        ENDIF.

        READ TABLE GT_ZTMM_DO_EXPORT INTO GS_ZTMM_DO_EXPORT
        WITH KEY VBELN = GS_LIKP-VBELN.
        IF SY-SUBRC = 0.
          GS_RESULT-ERZETE = GS_ZTMM_DO_EXPORT-ERZET.
          GS_RESULT-ERDATE = GS_ZTMM_DO_EXPORT-ERDAT.
          GS_RESULT-AMPMF  = GS_ZTMM_DO_EXPORT-AM_PM.
          GS_RESULT-REMAP  = GS_ZTMM_DO_EXPORT-REQ_MAP.
          GS_RESULT-REINV  = GS_ZTMM_DO_EXPORT-REQ_INV.
        ENDIF.

        GS_RESULT-DONUM   = GS_LIKP-VBELN.
        GS_RESULT-LFDAT   = GS_LIKP-LFDAT.
        GS_RESULT-BEZEI_G = GS_LIKP-BEZEI_G.
        GS_RESULT-BEZEI_O = GS_LIKP-BEZEI_O.
        GS_RESULT-VTEXT   = GS_LIKP-VTEXT.
        GS_RESULT-VTWEG   = GS_LIKP-VTWEG.
        GS_RESULT-VKGRP   = GS_LIKP-VKGRP.
        GS_RESULT-VKBUR   = GS_LIKP-VKBUR.
        GS_RESULT-ERZETD  = GS_LIKP-ERZET.
        GS_RESULT-ERDATD  = GS_LIKP-ERDAT.

        READ TABLE LT_VBUK INTO LS_VBUK
        WITH KEY VBELN = GS_LIKP-VBELN.
        IF SY-SUBRC = 0.
          CONCATENATE GS_RESULT-SERNF 'S' INTO GS_RESULT-STATU_U.
        ELSE.
          GS_RESULT-STATU_U = GS_RESULT-SERNF.
        ENDIF.

        READ TABLE GT_ZSDSSDT025 INTO GS_zsdssdt025
        WITH TABLE KEY DO_NO = GS_RESULT-DONUM.
        IF SY-SUBRC EQ 0.
          GS_RESULT-SMART_TRACKING_DATE = GS_ZSDSSDT025-SMART_TRACKING_DATE.
          GS_RESULT-SMART_TRACKING_TIME = GS_ZSDSSDT025-SMART_TRACKING_TIME.
          GS_RESULT-FLAG                = GS_ZSDSSDT025-FLAG.
          GS_RESULT-INVNO               = GS_ZSDSSDT025-INVNO.
          GS_RESULT-PETAX               = GS_ZSDSSDT025-PETAX.
        ENDIF.

        APPEND GS_RESULT TO GT_RESULT.
      ENDIF.


      CLEAR : GS_LIKP,GS_RESULT,GS_ZTMM_DO_EXPORT,GS_TVLAT.
    ENDLOOP.
  ENDMETHOD.
  METHOD SHOW_REPORT.
    SET_LAYOUT_OUTPUT( ).
    BUILD_FCAT( ).
    SET_SORT( ).
    SET_ALV_GRID( ).
  ENDMETHOD.
  METHOD SET_LAYOUT_OUTPUT.
    GS_LAYOUT-ZEBRA             = GC_X.
    GS_LAYOUT-COLWIDTH_OPTIMIZE = GC_X.
    GS_LAYOUT-INFO_FIELDNAME    = 'LINE_COLOR'.
  ENDMETHOD.
  METHOD BUILD_FCAT.
    DATA:
       LS_FCAT TYPE SLIS_FIELDCAT_ALV.

    CLEAR LS_FCAT.
*  ls_fcat-ref_tabname = 'GT_RESULT'.
*    LS_FCAT-FIELDNAME   = 'CHECK'.
*    LS_FCAT-SELTEXT_S   = 'Check'.
*    LS_FCAT-SELTEXT_M   = 'Check'.
*    LS_FCAT-SELTEXT_L   = 'Check'.
*    LS_FCAT-CHECKBOX    = 'X'.
*    LS_FCAT-INPUT       = 'X'.
*    LS_FCAT-EDIT        = 'X'.
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
    DO 99 TIMES.
      ADD 1 TO LV_RUNNING.
      LV_RUN_TEXT = LV_RUNNING.

      CONVERT_ALPHA_IN( EXPORTING I_DATA = LV_RUN_TEXT
                        IMPORTING e_Data = LV_RUN_TEXT ).

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
      CLEAR : LS_FCAT.
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
  METHOD GET_DATA_FROM_LIKP.
    DATA : LV_TABIX TYPE SY-TABIX.

    SELECT LIKP~VBELN,
           LIKP~LFDAT,
           TVGRT~BEZEI,
           TVKBT~BEZEI,
           TVTWT~VTEXT,
           VBAK~VTWEG,
           VBAK~VKGRP,
           VBAK~VKBUR,
           LIKP~ERZET,
           LIKP~ERDAT,
           VBAP~VBELN,
           VBAP~POSNR,
           LIKP~LSTEL,
           LIKP~VSTEL
    FROM LIKP
    INNER JOIN LIPS   ON LIKP~VBELN EQ LIPS~VBELN
    INNER JOIN VBAP   ON LIPS~VGBEL EQ VBAP~VBELN AND
                         LIPS~VGPOS EQ VBAP~POSNR
    INNER JOIN VBAK   ON VBAP~VBELN EQ VBAK~VBELN
    INNER JOIN TVGRT  ON ( VBAK~VKGRP  EQ TVGRT~VKGRP AND "get sale group description
                           TVGRT~SPRAS EQ @SY-LANGU )
    INNER JOIN TVKBT  ON ( VBAK~VKBUR  EQ TVKBT~VKBUR AND "get sale office description
                           TVKBT~SPRAS EQ @SY-LANGU )
    INNER JOIN TVTWT  ON ( VBAK~VTWEG  EQ TVTWT~VTWEG  AND "Distibution channel
                           TVTWT~SPRAS EQ @SY-LANGU )
    INTO TABLE @GT_LIKP
    WHERE LIKP~VBELN IN @S_DONUM
      AND LIKP~LFDAT IN @S_LFDAT
      AND LIKP~ERDAT IN @S_ERDAT
      AND LIKP~ERZET IN @S_ERZET
      AND VBAK~VTWEG IN @S_VTWEG
      AND VBAK~VKGRP IN @S_VKGRP
      AND VBAK~VKBUR IN @S_VKBUR
      AND LIKP~LFART IN @S_LFART.
    IF GT_LIKP[] IS NOT INITIAL.
      SELECT *
        FROM @GT_LIKP AS A
        INNER JOIN ZSDSSDT001 ON A~VBELN EQ ZSDSSDT001~DONUM
        INTO CORRESPONDING FIELDS OF TABLE @GT_RESULT.

      SELECT
        ZSDSCAC001~VALUE_LOW
        FROM  ZSDSCAC001
        WHERE ZSDSCAC001~REPID ='ZSDSSDR0030'
        AND   ZSDSCAC001~PARAM = 'CHECK_USER_LOGIS'
        INTO TABLE @DATA(LT_TMP).

      LOOP AT GT_RESULT INTO GS_RESULT.
        LV_TABIX = SY-TABIX.
        READ TABLE LT_TMP INTO DATA(LS_TMP)
        WITH KEY  VALUE_LOW = GS_RESULT-AENAM.
        IF SY-SUBRC NE 0.
          GS_RESULT-LINE_COLOR = 'C600'.
        ENDIF.
        MODIFY GT_RESULT FROM GS_RESULT INDEX LV_TABIX.
      ENDLOOP.


*    PERFORM f_get_ship_to_addr.
*      PERFORM F_INSERT_RESULT.
    ENDIF.

  ENDMETHOD.
  METHOD GET_DATA_FROM_Z.
    DATA : LV_TABIX TYPE SY-TABIX.

    SELECT *
      FROM ZSDSSDT001
      INTO CORRESPONDING FIELDS OF TABLE @GT_RESULT
      WHERE DONUM IN @S_DONUM
        AND WMSDT IN @S_WMSDT
        AND STAPC IN @S_STAPC
        AND LOADD IN @S_LOADD
        AND CONTD IN @S_CONTD
        AND DEALC IN @S_DEALC
        AND SERNF IN @S_SERNF.
    IF GT_RESULT[] IS NOT INITIAL.
      SELECT LIKP~VBELN,
             LIKP~LFDAT,
             TVGRT~BEZEI,
             TVKBT~BEZEI,
             TVTWT~VTEXT,
             VBAK~VTWEG,
             VBAK~VKGRP,
             VBAK~VKBUR,
             LIKP~ERZET,
             LIKP~ERDAT,
             VBAP~VBELN,
             VBAP~POSNR,
             LIKP~LSTEL,
             LIKP~VSTEL
      FROM @GT_RESULT AS A
      INNER JOIN LIKP   ON A~DONUM    EQ LIKP~VBELN
      INNER JOIN LIPS   ON LIKP~VBELN EQ LIPS~VBELN
      INNER JOIN VBAP   ON LIPS~VGBEL EQ VBAP~VBELN AND
                           LIPS~VGPOS EQ VBAP~POSNR
      INNER JOIN VBAK   ON VBAP~VBELN EQ VBAK~VBELN
      INNER JOIN TVGRT  ON ( VBAK~VKGRP  EQ TVGRT~VKGRP AND "get sale group description
                             TVGRT~SPRAS EQ @SY-LANGU )
      INNER JOIN TVKBT  ON ( VBAK~VKBUR  EQ TVKBT~VKBUR AND "get sale office description
                             TVKBT~SPRAS EQ @SY-LANGU )
      INNER JOIN TVTWT  ON ( VBAK~VTWEG  EQ TVTWT~VTWEG  AND "Distibution channel
                             TVTWT~SPRAS EQ @SY-LANGU )
      WHERE LIKP~LFDAT IN @S_LFDAT
        AND LIKP~ERDAT IN @S_ERDAT                             "CH3+
        AND LIKP~ERZET IN @S_ERZET                             "CH3+
        AND VBAK~VTWEG IN @S_VTWEG
        AND VBAK~VKGRP IN @S_VKGRP
        AND VBAK~VKBUR IN @S_VKBUR
        AND LIKP~LFART IN @S_LFART
      INTO TABLE @GT_LIKP.

      SELECT
        ZSDSCAC001~VALUE_LOW
        FROM  ZSDSCAC001
        WHERE ZSDSCAC001~REPID EQ 'ZSDSSDR0030'
        AND   ZSDSCAC001~PARAM EQ 'CHECK_USER_LOGIS'
        INTO TABLE @DATA(LT_TMP).

      LOOP AT GT_RESULT INTO GS_RESULT.
        LV_TABIX = SY-TABIX.

        READ TABLE LT_TMP INTO DATA(LS_TMP)
        WITH KEY  VALUE_LOW = GS_RESULT-AENAM.
        IF SY-SUBRC NE 0.
          GS_RESULT-LINE_COLOR = 'C600'.
        ENDIF.
        MODIFY GT_RESULT FROM GS_RESULT INDEX LV_TABIX.
      ENDLOOP.

*      PERFORM F_INSERT_RESULT.
    ENDIF.
  ENDMETHOD.
  METHOD GET_SHIP_TO_ADDR.

    CONSTANTS LC_WE TYPE C LENGTH 2 VALUE 'WE'.

    SELECT VBPA~VBELN,
           VBPA~KUNNR,
           ADRC~NAME1,
           ADRC~NAME2,
           ADRC~NAME3,
           ADRC~NAME4,
           ADRC~STREET,
           ADRC~STR_SUPPL1,
           ADRC~STR_SUPPL2,
           ADRC~STR_SUPPL3,
           ADRC~LOCATION,
           ADRC~POST_CODE1,
           ADRC~CITY1
    FROM @GT_LIKP AS A
    INNER JOIN VBPA ON A~VBELN_S  EQ VBPA~VBELN  AND
                       VBPA~PARVW EQ @LC_WE      AND
                       VBPA~POSNR EQ @SPACE
    INNER JOIN ADRC ON VBPA~ADRNR EQ ADRC~ADDRNUMBER
    INTO TABLE @GT_VBPA.
  ENDMETHOD.
  METHOD GET_STATUS_SEND_DATA.

    SELECT *
      FROM ZSDSSDT002
      INTO TABLE GT_ZTMM_DO_EXPORT
      FOR ALL ENTRIES IN GT_LIKP
      WHERE VBELN EQ GT_LIKP-VBELN.

    SORT GT_ZTMM_DO_EXPORT BY RUN_ID DESCENDING.

  ENDMETHOD.
  METHOD GET_SHIPING_POINT.
    SELECT VSTEL
           LSTEL
           VTEXT
      FROM TVLAT
      INTO TABLE GT_TVLAT
      FOR ALL ENTRIES IN GT_LIKP
      WHERE SPRAS EQ SY-LANGU
        AND VSTEL EQ GT_LIKP-VSTEL
        AND LSTEL EQ GT_LIKP-LSTEL.
  ENDMETHOD.
  METHOD GET_VBUK.
    CONSTANTS LC_8 TYPE C LENGTH 1 VALUE '8'.
    SELECT VBELV
      FROM VBFA
      INTO TABLE C_TABLE
      FOR ALL ENTRIES IN GT_LIKP
      WHERE VBELV   EQ GT_LIKP-VBELN
        AND POSNV   EQ SPACE
        AND VBTYP_N EQ LC_8.
  ENDMETHOD.
  METHOD READ_TEXT.
*    DATA : LV_ID       TYPE THEAD-TDID,
*           LV_LANGUAGE TYPE THEAD-TDSPRAS,
*           LV_NAME     TYPE THEAD-TDNAME,
*           LV_OBJECT   TYPE THEAD-TDOBJECT,
*           LV_TEXT     TYPE STRING.
*
*    DATA : LT_LINES TYPE TABLE OF TLINE,
*           LS_LINES TYPE TLINE.
*
*    CONSTANTS : BEGIN OF LC_READ_TEXT,
*                  ID TYPE C LENGTH 4 VALUE 'Z012',
*                  OB TYPE C LENGTH 4 VALUE 'VBBK',
*                END OF LC_READ_TEXT.
**--------------------------------------------------------------------*
*    LV_ID       = LC_READ_TEXT-ID.
*    LV_LANGUAGE = SY-LANGU.
*    LV_NAME     = GS_LIKP-VBELN_S.
*    LV_OBJECT   = LC_READ_TEXT-OB.
**--------------------------------------------------------------------*
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
*        CLIENT                  = SY-MANDT
*        ID                      = LV_ID
*        LANGUAGE                = LV_LANGUAGE
*        NAME                    = LV_NAME
*        OBJECT                  = LV_OBJECT
*      TABLES
*        LINES                   = LT_LINES
*      EXCEPTIONS
*        ID                      = 1
*        LANGUAGE                = 2
*        NAME                    = 3
*        NOT_FOUND               = 4
*        OBJECT                  = 5
*        REFERENCE_CHECK         = 6
*        WRONG_ACCESS_TO_ARCHIVE = 7
*        OTHERS                  = 8.
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    CLEAR LV_TEXT.
*    IF LT_LINES[] IS NOT INITIAL.
*      LOOP AT LT_LINES INTO LS_LINES.
*        IF LV_TEXT IS NOT INITIAL.
*          CONCATENATE LV_TEXT LS_LINES-TDLINE INTO LV_TEXT.
*        ELSE.
*          LV_TEXT = LS_LINES-TDLINE.
*        ENDIF.
*      ENDLOOP.
*      GS_RESULT-REMAR_R = LV_TEXT.
*    ENDIF.
  ENDMETHOD.
  METHOD READ_TEXT_INV.
*    DATA : LV_ID       TYPE THEAD-TDID,
*           LV_LANGUAGE TYPE THEAD-TDSPRAS,
*           LV_NAME     TYPE THEAD-TDNAME,
*           LV_OBJECT   TYPE THEAD-TDOBJECT,
*           LV_TEXT     TYPE STRING.
*
*    DATA : LT_LINES TYPE TABLE OF TLINE,
*           LS_LINES TYPE TLINE.
*
*    CONSTANTS : BEGIN OF LC_READ_TEXT,
*                  ID TYPE C LENGTH 4 VALUE 'Z011',
*                  OB TYPE C LENGTH 4 VALUE 'VBBK',
*                END OF LC_READ_TEXT.
**--------------------------------------------------------------------*
*    LV_ID       = LC_READ_TEXT-ID.
*    LV_LANGUAGE = SY-LANGU.
*    LV_NAME     = GS_LIKP-VBELN_S.
*    LV_OBJECT   = LC_READ_TEXT-OB.
**--------------------------------------------------------------------*
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
*        CLIENT                  = SY-MANDT
*        ID                      = LV_ID
*        LANGUAGE                = LV_LANGUAGE
*        NAME                    = LV_NAME
*        OBJECT                  = LV_OBJECT
*      TABLES
*        LINES                   = LT_LINES
*      EXCEPTIONS
*        ID                      = 1
*        LANGUAGE                = 2
*        NAME                    = 3
*        NOT_FOUND               = 4
*        OBJECT                  = 5
*        REFERENCE_CHECK         = 6
*        WRONG_ACCESS_TO_ARCHIVE = 7
*        OTHERS                  = 8.
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    CLEAR LV_TEXT.
*    IF LT_LINES[] IS NOT INITIAL.
*      LOOP AT LT_LINES INTO LS_LINES.
*        IF LV_TEXT IS NOT INITIAL.
*          CONCATENATE LV_TEXT LS_LINES-TDLINE INTO LV_TEXT.
*        ELSE.
*          LV_TEXT = LS_LINES-TDLINE.
*        ENDIF.
*      ENDLOOP.
*      GS_RESULT-REMAR_I = LV_TEXT.
*    ENDIF.
  ENDMETHOD.
  METHOD INIT_DATA.
    BUT_UPS = 'Update Status'.
  ENDMETHOD.
  METHOD UPDATE_STATUS_DO.

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
                  REPID TYPE C LENGTH 22 VALUE 'SONY_STATUS_DO',
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
  METHOD GET_SMART_TRACKING.
    DATA : BEGIN OF LS_DO,
             DONUM TYPE GY_RESULT-DONUM,
           END OF LS_DO.
    DATA : LT_DO LIKE HASHED TABLE OF LS_DO WITH UNIQUE KEY DONUM.

    LT_DO =  CORRESPONDING #( GT_RESULT  DISCARDING DUPLICATES ).

    SELECT DO_NO,
           SMART_TRACKING_DATE,
           SMART_TRACKING_TIME,
           FLAG,
           INVNO,
           PETAX
      FROM @LT_DO AS A
      INNER JOIN ZSDSSDT025 ON A~DONUM EQ ZSDSSDT025~DO_NO
      INTO TABLE @GT_ZSDSSDT025.
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
