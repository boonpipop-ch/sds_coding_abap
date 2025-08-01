*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0850
*  Creation Date      : 22.02.2025
*  Author             : Jakarin Sirilertlak
*  Add-on ID          :
*  Description        : Post Sub-Service & Install
*  Purpose            :
*  Copied from        :
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0850 MESSAGE-ID ZSDSFI01.
TYPE-POOLS : TRUXS,SLIS,ICON.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : EKPO.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF GY_RESULT,
          EBELN TYPE EKET-EBELN,
          EBELP TYPE EKET-EBELP,
          MENGE TYPE EKET-MENGE,
          WEMNG TYPE EKET-WEMNG,
          NETPR TYPE EKPO-NETPR,
          MEINS TYPE EKPO-MEINS,
          LIFNR TYPE EKKO-LIFNR,
          WAERS TYPE EKKO-WAERS,
          NAME1 TYPE LFA1-NAME1,
          NAME2 TYPE LFA1-NAME2,
          NAME3 TYPE LFA1-NAME3,
          NAME4 TYPE LFA1-NAME4,
          KVERM TYPE LFB1-KVERM,
          REMAT TYPE EKPO-NETPR,
          DMBTR TYPE MSEG-DMBTR,
          MWSBP TYPE VBAP-MWSBP,
          NAME  TYPE C LENGTH 255,
          QTY   TYPE P DECIMALS 0,
          XBLNR TYPE BKPF-XBLNR,
          BLDAT TYPE BKPF-BLDAT,
          BKTXT TYPE BKPF-BKTXT,
          TAXTT TYPE VBAP-MWSBP,
          CHECK TYPE C,
        END OF GY_RESULT.

TYPES : BEGIN OF GY_SAVE,
          STATU TYPE ICON-ID,
          EBELN TYPE EKET-EBELN,
          BELNR TYPE RBKP-BELNR,
          FIDOC TYPE BKPF-BELNR,
          XBLNR TYPE BKPF-XBLNR,
          BLDAT TYPE BKPF-BLDAT,
          BKTXT TYPE BKPF-BKTXT,
          MESSG TYPE C LENGTH 400,
        END OF GY_SAVE.

TYPES : BEGIN OF GY_SUM,
          EBELN TYPE EKBE-EBELN,
          EBELP TYPE EKBE-EBELP,
          MENGE TYPE EKBE-MENGE,
        END OF GY_SUM.

*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_ZSFI_POST_MIRO TYPE TABLE OF ZSDSFIS193,
       GS_ZSFI_POST_MIRO TYPE ZSDSFIS193.

DATA : GT_SAVE TYPE TABLE OF GY_SAVE,
       GS_SAVE TYPE GY_SAVE.

DATA : GT_SUM TYPE TABLE OF GY_SUM,
       GS_SUM TYPE GY_SUM.

DATA : P_LIFNR TYPE LFA1-LIFNR.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : GR_STAT FOR JEST-STAT.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : GC_TRUE      TYPE C VALUE 'X'.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : P_BUKRS TYPE BUKRS.
  SELECT-OPTIONS : S_EBELN FOR EKPO-EBELN,
                   S_EBELP FOR EKPO-EBELP.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : P_BUDAT TYPE BKPF-BUDAT OBLIGATORY,
               P_BLDAT TYPE BKPF-BLDAT OBLIGATORY,
               P_XBLNR TYPE BKPF-XBLNR,
               P_BKTXT TYPE BKPF-BKTXT,
               P_MWSBP TYPE VBAP-MWSBP,
               P_KVERM TYPE LFB1-KVERM.
SELECTION-SCREEN END OF BLOCK BLOCK2.
*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_GET_INIT.
*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
*  START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_GET_DATA.
*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_RESULT[] IS NOT INITIAL.
    PERFORM F_ADDTIONAL_DATA.
    PERFORM F_SHOW_REPORT.
  ELSE.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ENDIF.
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
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_DATA.
  FIELD-SYMBOLS <LFS_RESULT> LIKE GS_RESULT.

  DATA LV_CHK TYPE C.

  DATA : LV_TABIX  TYPE SY-TABIX,
         LV_TABIX1 TYPE SY-TABIX.

  DATA : LV_AMT TYPE EKET-MENGE.

  DATA : BEGIN OF LS_EKBE,
           EBELN TYPE EKBE-EBELN,
           EBELP TYPE EKBE-EBELP,
           ZEKKN TYPE EKBE-ZEKKN,
           VGABE TYPE EKBE-VGABE,
           MENGE TYPE EKBE-MENGE,
         END OF LS_EKBE.
  DATA LT_EKBE LIKE TABLE OF LS_EKBE.

  DATA : BEGIN OF LS_MKPF,
           EBELN TYPE MSEG-EBELN,
           BKTXT TYPE MKPF-BKTXT,
         END OF LS_MKPF.
  DATA LT_MKPF LIKE TABLE OF LS_MKPF.

  DATA : BEGIN OF LS_MSEG_SUM,
           EBELN TYPE MSEG-EBELN,
           EBELP TYPE MSEG-EBELP,
           DMBTR TYPE MSEG-DMBTR,
         END OF LS_MSEG_SUM.
  DATA LT_MSEG_SUM LIKE TABLE OF LS_MSEG_SUM.

  DATA : BEGIN OF LS_MSEG,
           MBLNR TYPE MSEG-MBLNR,
           MJAHR TYPE MSEG-MJAHR,
           SHKZG TYPE MSEG-SHKZG,
           ZEILE TYPE MSEG-ZEILE,
           EBELN TYPE MSEG-EBELN,
           EBELP TYPE MSEG-EBELP,
           DMBTR TYPE MSEG-DMBTR,
           MENGE TYPE MSEG-MENGE,
           SMBLN TYPE MSEG-SMBLN,
         END OF LS_MSEG.
  DATA LT_MSEG LIKE TABLE OF LS_MSEG.

  DATA : LT_MSEG_TMP LIKE TABLE OF LS_MSEG,
         LS_MSEG_TMP LIKE LS_MSEG.

  SELECT EKET~EBELN
         EKET~EBELP
         EKET~MENGE
         EKET~WEMNG
         EKPO~NETPR
         EKPO~MEINS
         EKKO~LIFNR
         EKKO~WAERS
         LFA1~NAME1
         LFA1~NAME2
         LFA1~NAME3
         LFA1~NAME4
         LFB1~KVERM
    FROM EKPO
    INNER JOIN EKET ON EKPO~EBELN EQ EKET~EBELN AND
                       EKPO~EBELP EQ EKET~EBELP AND
                       EKET~ETENR EQ '0001'     AND
                       EKET~WEMNG NE 0
    INNER JOIN EKKO ON EKPO~EBELN EQ EKKO~EBELN
    INNER JOIN LFA1 ON EKKO~LIFNR EQ LFA1~LIFNR
    INNER JOIN LFB1 ON LFA1~LIFNR EQ LFB1~LIFNR AND
                       LFB1~BUKRS EQ P_BUKRS
    INTO TABLE GT_RESULT
    WHERE EKPO~EBELN IN S_EBELN
      AND EKPO~EBELP IN S_EBELP
      AND EKPO~LOEKZ EQ SPACE.

  IF GT_RESULT IS NOT INITIAL.

    SELECT MBLNR
           MJAHR
           SHKZG
           ZEILE
           EBELN
           EBELP
           DMBTR
      FROM MSEG
      INTO TABLE LT_MSEG
      FOR ALL ENTRIES IN GT_RESULT
      WHERE EBELN EQ GT_RESULT-EBELN
        AND EBELP EQ GT_RESULT-EBELP.

    IF LT_MSEG[] IS NOT INITIAL.
      LT_MSEG_TMP[] = LT_MSEG[].
      DELETE LT_MSEG_TMP WHERE SHKZG EQ 'H'.
      DELETE LT_MSEG_TMP WHERE SMBLN IS NOT INITIAL.
      SORT  LT_MSEG_TMP BY MBLNR DESCENDING
                           EBELN.
    ENDIF.

    LOOP AT LT_MSEG INTO LS_MSEG.
      MOVE-CORRESPONDING LS_MSEG TO LS_MSEG_SUM.

      IF LS_MSEG-SHKZG EQ 'H'.
        LS_MSEG_SUM-DMBTR = LS_MSEG_SUM-DMBTR * -1.
      ENDIF.

      COLLECT LS_MSEG_SUM INTO LT_MSEG_SUM.
      CLEAR : LS_MSEG,LS_MSEG_SUM.
    ENDLOOP.

    SELECT EKBE~EBELN
           EKBE~EBELP
           EKBE~ZEKKN
           EKBE~VGABE
           EKBE~MENGE
      FROM EKBE
      INNER JOIN RBKP ON EKBE~BELNR EQ RBKP~BELNR AND
                         EKBE~GJAHR EQ RBKP~GJAHR AND
                         RBKP~STBLG EQ SPACE
      INTO TABLE LT_EKBE
      FOR ALL ENTRIES IN GT_RESULT
      WHERE EKBE~EBELN EQ GT_RESULT-EBELN
        AND EKBE~EBELP EQ GT_RESULT-EBELP
        AND EKBE~BEWTP EQ 'Q'
        AND EKBE~SHKZG EQ 'S'.

    SELECT DISTINCT MSEG~EBELN
                    MKPF~BKTXT
      FROM MSEG
      INNER JOIN MKPF ON MSEG~MBLNR EQ MKPF~MBLNR AND
                         MSEG~MJAHR EQ MKPF~MJAHR
      INTO TABLE LT_MKPF
      FOR ALL ENTRIES IN GT_RESULT
      WHERE MSEG~EBELN EQ GT_RESULT-EBELN.

    LOOP AT LT_EKBE INTO LS_EKBE.
      MOVE-CORRESPONDING LS_EKBE TO GS_SUM.
      COLLECT GS_SUM INTO GT_SUM.
      CLEAR : LS_EKBE,GS_SUM.
    ENDLOOP.
  ENDIF.

  IF GT_RESULT IS NOT INITIAL.
    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.

      <LFS_RESULT>-BLDAT = P_BLDAT.
      <LFS_RESULT>-XBLNR = P_XBLNR.
      <LFS_RESULT>-BKTXT = P_BKTXT.
      <LFS_RESULT>-TAXTT = P_MWSBP.

      READ TABLE LT_MKPF INTO LS_MKPF
      WITH KEY EBELN = <LFS_RESULT>-EBELN.
      IF SY-SUBRC EQ 0.
        TRANSLATE LS_MKPF-BKTXT TO UPPER CASE.
        IF LS_MKPF-BKTXT IS NOT INITIAL.
          <LFS_RESULT>-KVERM = LS_MKPF-BKTXT.
        ENDIF.
      ENDIF.

*      IF p_kverm IS NOT INITIAL.
      <LFS_RESULT>-KVERM = P_KVERM.
*      ENDIF.

      LV_TABIX = SY-TABIX.
      CLEAR : LV_CHK.
      <LFS_RESULT>-CHECK = 'X'.
      READ TABLE GT_SUM INTO GS_SUM
        WITH KEY EBELN = <LFS_RESULT>-EBELN
                 EBELP = <LFS_RESULT>-EBELP.
      IF SY-SUBRC = 0.
        LV_AMT = <LFS_RESULT>-WEMNG - GS_SUM-MENGE.
      ELSE.
        LV_AMT = <LFS_RESULT>-WEMNG.
      ENDIF.

      IF LV_AMT GT 0.
        READ TABLE LT_MSEG_TMP INTO LS_MSEG_TMP
          WITH KEY EBELN = <LFS_RESULT>-EBELN
                   EBELP = <LFS_RESULT>-EBELP.
        IF SY-SUBRC EQ 0.
          GS_ZSFI_POST_MIRO-DMBTR = LS_MSEG_TMP-DMBTR.
        ELSE.
          GS_ZSFI_POST_MIRO-DMBTR = LV_AMT * <LFS_RESULT>-NETPR.
        ENDIF.

        ADD GS_ZSFI_POST_MIRO-DMBTR TO <LFS_RESULT>-DMBTR.
        <LFS_RESULT>-QTY = LV_AMT.
        CONCATENATE <LFS_RESULT>-NAME1 <LFS_RESULT>-NAME2 <LFS_RESULT>-NAME3 <LFS_RESULT>-NAME4
        INTO <LFS_RESULT>-NAME.
        CLEAR : GS_ZSFI_POST_MIRO.

        IF <LFS_RESULT>-KVERM EQ 'VAT' OR
           <LFS_RESULT>-KVERM EQ 'DVAT'.
          <LFS_RESULT>-MWSBP = ( <LFS_RESULT>-DMBTR * 7 ) / 100.
        ELSE.
          CLEAR : <LFS_RESULT>-MWSBP.
        ENDIF.
      ELSE.
        DELETE GT_RESULT INDEX LV_TABIX.
        CLEAR : GS_ZSFI_POST_MIRO.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "f_get_data
*&---------------------------------------------------------------------*
*&      Form  pf_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_PF_ALV_GRID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FCAT
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
      IT_SORT                  = GT_SORT
*     IT_FILTER                =
*     IS_SEL_HIDE              =
      I_DEFAULT                = 'X'
      I_SAVE                   = 'X'
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "pf_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_layout_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_LAYOUT  text
*----------------------------------------------------------------------*
FORM F_SET_LAYOUT_OUTPUT." CHANGING ps_layout TYPE slis_layout_alv.
  "gs_layout-box_fieldname     = 'SEL'.
*  gs_layout-zebra             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

ENDFORM.                    " set_layout_output
*&---------------------------------------------------------------------*
*&      Form  build_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_BUILD_FCAT.

  DATA:
   LS_FCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR LS_FCAT.
*  ls_fcat-ref_tabname = 'GT_RESULT'.
  LS_FCAT-FIELDNAME   = 'CHECK'.
  LS_FCAT-SELTEXT_S   = 'Check'.
  LS_FCAT-SELTEXT_M   = 'Check'.
  LS_FCAT-SELTEXT_L   = 'Check'.
  LS_FCAT-CHECKBOX    = 'X'.
  LS_FCAT-INPUT       = 'X'.
  LS_FCAT-EDIT        = 'X'.
  LS_FCAT-OUTPUTLEN   = 5.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKPO'.
  LS_FCAT-FIELDNAME   = 'EBELN'.
  LS_FCAT-OUTPUTLEN   = 10.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKPO'.
  LS_FCAT-FIELDNAME   = 'EBELP'.
  LS_FCAT-OUTPUTLEN   = 5.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKKO'.
  LS_FCAT-FIELDNAME   = 'LIFNR'.
  LS_FCAT-OUTPUTLEN   = 10.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'NAME'.
  LS_FCAT-SELTEXT_S   = 'Vendor Name'.
  LS_FCAT-SELTEXT_M   = 'Vendor Name'.
  LS_FCAT-SELTEXT_L   = 'Vendor Name'.
  LS_FCAT-OUTPUTLEN   = 20.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'BKPF'.
  LS_FCAT-FIELDNAME   = 'BLDAT'.
  LS_FCAT-INPUT       = 'X'.
  LS_FCAT-EDIT        = 'X'.
  LS_FCAT-OUTPUTLEN   = 10.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'BKPF'.
  LS_FCAT-FIELDNAME   = 'XBLNR'.
  LS_FCAT-INPUT       = 'X'.
  LS_FCAT-EDIT        = 'X'.
  LS_FCAT-LOWERCASE   = 'X'.
  LS_FCAT-OUTPUTLEN   = 16.
  APPEND LS_FCAT TO GT_FCAT.

*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'BKPF'.
*  ls_fcat-fieldname   = 'BKTXT'.
*  ls_fcat-input       = 'X'.
*  ls_fcat-edit        = 'X'.
*  APPEND ls_fcat TO gt_fcat.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'KVERM'.
  LS_FCAT-SELTEXT_S   = 'Vat Type'.
  LS_FCAT-SELTEXT_M   = 'Vat Type'.
  LS_FCAT-SELTEXT_L   = 'Vat Type'.
  LS_FCAT-INPUT       = 'X'.
  LS_FCAT-EDIT        = 'X'.
  LS_FCAT-OUTPUTLEN   = 8.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'QTY'.
  LS_FCAT-SELTEXT_S   = 'Qty'.
  LS_FCAT-SELTEXT_M   = 'Qty'.
  LS_FCAT-SELTEXT_L   = 'Qty'.
  LS_FCAT-OUTPUTLEN   = 10.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKPO'.
  LS_FCAT-FIELDNAME   = 'MEINS'.
  LS_FCAT-OUTPUTLEN   = 10.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'DMBTR'.
  LS_FCAT-SELTEXT_S   = 'Amount'.
  LS_FCAT-SELTEXT_M   = 'Amount'.
  LS_FCAT-SELTEXT_L   = 'Amount'.
  LS_FCAT-OUTPUTLEN   = 12.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'MWSBP'.
  LS_FCAT-DATATYPE    = 'CURR'.
  LS_FCAT-SELTEXT_S   = 'VAT'.
  LS_FCAT-SELTEXT_M   = 'VAT'.
  LS_FCAT-SELTEXT_L   = 'VAT'.
*  ls_fcat-input       = 'X'.
*  ls_fcat-edit        = 'X'.
  LS_FCAT-OUTPUTLEN   = 10.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'TAXTT'.
  LS_FCAT-DATATYPE    = 'Vat Total'.
  LS_FCAT-SELTEXT_S   = 'Vat Total'.
  LS_FCAT-SELTEXT_M   = 'Vat Total'.
  LS_FCAT-SELTEXT_L   = 'Vat Total'.
  LS_FCAT-INPUT       = 'X'.
  LS_FCAT-EDIT        = 'X'.
  LS_FCAT-OUTPUTLEN   = 10.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKKO'.
  LS_FCAT-FIELDNAME   = 'WAERS'.
  LS_FCAT-OUTPUTLEN   = 10.
  APPEND LS_FCAT TO GT_FCAT.


ENDFORM.                    "build_fcat_1
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SHOW_REPORT .
  PERFORM F_SET_LAYOUT_OUTPUT.
  PERFORM F_BUILD_FCAT.
  PERFORM F_SORT.
  PERFORM F_PF_ALV_GRID.
ENDFORM.                    " F_SHOW_REPORT
*&---------------------------------------------------------------------*
*&      Form  F_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SORT .
*  CLEAR gs_sort.
*  gs_sort-fieldname = 'LIFNR'.
*  gs_sort-spos = '1'.
*  gs_sort-up = 'X'.
**  gs_sort-subtot = 'X'.
*  APPEND gs_sort TO gt_sort.
ENDFORM.                    " F_SORT
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_EXTAB   text
*----------------------------------------------------------------------*
FORM PF_STATUS_1 USING US_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'ZSTANDARD' EXCLUDING US_EXTAB.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                        P_SELFLD TYPE SLIS_SELFIELD.
*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
  ENDIF.
*&---------------------------------------------------------------------*

  CASE P_UCOMM.
    WHEN '&IC1'.
      PERFORM F_CALL_TRANSECTION.
    WHEN 'SAVE_D'.
      CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
      PERFORM F_SAVE_DETAIL.
    WHEN 'ALL'.
      PERFORM F_SET_ALL USING 'X'.
    WHEN 'SAL'.
      PERFORM F_SET_ALL USING ''.
  ENDCASE.

  CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
  CLEAR : REF_GRID.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_TRANSECTION.
  DATA: LE_ROW     TYPE I,
        LE_VALUE   TYPE C,
        LE_COL     TYPE I,
        LES_ROW_ID TYPE LVC_S_ROW,
        LES_COL_ID TYPE LVC_S_COL,
        LES_ROW_NO TYPE LVC_S_ROID.

  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.
  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
  ENDIF.

  CALL METHOD REF_GRID->GET_CURRENT_CELL
    IMPORTING
      E_ROW     = LE_ROW
      E_VALUE   = LE_VALUE
      E_COL     = LE_COL
      ES_ROW_ID = LES_ROW_ID
      ES_COL_ID = LES_COL_ID.

  CLEAR : BDCDATA[],MESSTAB[].

  READ TABLE GT_RESULT INTO GS_RESULT INDEX LES_ROW_ID-INDEX.
  IF SY-SUBRC = 0.


  ENDIF.

ENDFORM.                    " F_CALL_TRANSECTION
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION  USING    TCODE.
  DATA: L_MSTRING(480).
  DATA: L_SUBRC LIKE SY-SUBRC.
  DATA: LV_OPT  TYPE CTU_PARAMS.

  LV_OPT-DISMODE  = 'E'."'A'
  LV_OPT-UPDMODE  = 'L'.
  LV_OPT-NOBINPT  = 'X'.
  "lv_opt-cattmode = 'A'.
  "lv_opt-defsize  = 'X'.

  CALL TRANSACTION TCODE USING BDCDATA
*                   MODE   'E'"N
*                   UPDATE 'L'
                   OPTIONS FROM LV_OPT
                   MESSAGES INTO MESSTAB.

ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  F_GET_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_INIT.

ENDFORM.                    " F_GET_INIT
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAVE_DETAIL .
  DATA : GV_POST_DT	   TYPE	BKPF-BUDAT,
         GV_DOC_DT     TYPE BKPF-BLDAT,
         GV_REF_DOC_NO TYPE	XBLNR,
         GV_VENDOR     TYPE LIFNR,
         GV_ITEM_TEXT	 TYPE	BKTXT.

  DATA : GV_OUTPUT  TYPE  BELNR_D,
         GV_MESSAGE TYPE  CHAR255.

  DATA : GT_DETAIL_MAP TYPE TABLE OF ZSDSFIS193,
         GT_RETURN     TYPE TABLE OF BAPIRETURN.

  DATA : GS_DETAIL_MAP  TYPE ZSDSFIS193.

  DATA : LS_TMP LIKE GS_ZSFI_POST_MIRO.

  DATA : LV_MESSAGE TYPE C LENGTH 255.

  DATA : LV_AMT LIKE GS_ZSFI_POST_MIRO-MENGE.

  DATA : I_BUKRS TYPE  BUKRS,
         I_DATUM TYPE  DATUM.

  DATA : E_PERIOD TYPE POPER,
         E_GJAHR  TYPE GJAHR.

  DATA : BEGIN OF LS_DOC,
           BELNR TYPE RBKP-BELNR,
           AWKEY TYPE BKPF-AWKEY,
           GJAHR TYPE BKPF-GJAHR,
         END OF LS_DOC.
  DATA LT_DOC LIKE TABLE OF LS_DOC.


  GV_POST_DT    = P_BUDAT.
*  gv_doc_dt     = p_bldat.
*  gv_ref_doc_no = p_xblnr.
*
*  gv_item_text  = p_bktxt.

  I_BUKRS = '1000'.
  I_DATUM = P_BUDAT.

  CALL FUNCTION 'Z_SDSFI_GET_PERIOD_FISCAL_YEAR'
    EXPORTING
      I_BUKRS  = I_BUKRS
      I_DATUM  = I_DATUM
    IMPORTING
      E_PERIOD = E_PERIOD
      E_GJAHR  = E_GJAHR.


  CLEAR : GT_ZSFI_POST_MIRO[].
  LOOP AT GT_RESULT INTO GS_RESULT WHERE CHECK EQ 'X'.

    GV_DOC_DT     = GS_RESULT-BLDAT.
    GV_REF_DOC_NO = GS_RESULT-XBLNR.
    GV_ITEM_TEXT  = GS_RESULT-BKTXT.
    READ TABLE GT_SUM INTO GS_SUM
      WITH KEY EBELN = GS_RESULT-EBELN
               EBELP = GS_RESULT-EBELP.
    IF SY-SUBRC = 0.
      LV_AMT = GS_RESULT-WEMNG - GS_SUM-MENGE.
    ELSE.
      LV_AMT = GS_RESULT-WEMNG.
    ENDIF.
*    lv_amt = gs_result-menge - gs_result-wemng.
    GS_ZSFI_POST_MIRO-EBELN = GS_RESULT-EBELN.
    GS_ZSFI_POST_MIRO-EBELP = GS_RESULT-EBELP.
    GS_ZSFI_POST_MIRO-MENGE = LV_AMT.
    IF GS_ZSFI_POST_MIRO-MENGE LE 0.
      GS_ZSFI_POST_MIRO-MENGE = 1.
    ENDIF.
    GS_ZSFI_POST_MIRO-DMBTR = GS_RESULT-DMBTR.
    IF GS_RESULT-KVERM EQ 'VAT'.
      GS_ZSFI_POST_MIRO-FLAGV =  'X'.
      GS_ZSFI_POST_MIRO-MWSBP = GS_RESULT-TAXTT.
    ELSEIF GS_RESULT-KVERM EQ 'DVAT'.
      GS_ZSFI_POST_MIRO-FLAGV =  'Y'.
      GS_ZSFI_POST_MIRO-MWSBP = GS_RESULT-TAXTT.
    ELSE.
      CLEAR : GS_ZSFI_POST_MIRO-FLAGV.
    ENDIF.

    APPEND GS_ZSFI_POST_MIRO TO GT_ZSFI_POST_MIRO.
    CLEAR : GS_ZSFI_POST_MIRO,GS_RESULT,GS_SUM.
  ENDLOOP.


  SORT GT_ZSFI_POST_MIRO BY EBELN EBELP.
  CLEAR : GT_SAVE[].
  LOOP AT GT_ZSFI_POST_MIRO INTO LS_TMP.
    MOVE-CORRESPONDING LS_TMP TO GS_ZSFI_POST_MIRO.

    MOVE-CORRESPONDING GS_ZSFI_POST_MIRO TO GS_DETAIL_MAP.
    APPEND GS_DETAIL_MAP TO GT_DETAIL_MAP.

    AT END OF EBELN.
      READ TABLE GT_RESULT INTO GS_RESULT
      WITH KEY EBELN = GS_ZSFI_POST_MIRO-EBELN
               EBELP = GS_ZSFI_POST_MIRO-EBELP.
      IF SY-SUBRC = 0.
        GV_VENDOR     = GS_RESULT-LIFNR.
      ENDIF.

      CALL FUNCTION 'Z_SDSFI_POST_MIRO'
        EXPORTING
          GV_POST_DT    = GV_POST_DT
          GV_DOC_DT     = GV_DOC_DT
          GV_REF_DOC_NO = GV_REF_DOC_NO
          GV_VENDOR     = GV_VENDOR
          GV_ITEM_TEXT  = GV_ITEM_TEXT
        IMPORTING
          GV_OUTPUT     = GV_OUTPUT
          GV_MESSAGE    = GV_MESSAGE
        TABLES
          GT_DETAIL     = GT_DETAIL_MAP
        EXCEPTIONS
          ERROR_MESSAGE = 1
          OTHERS        = 2.

      IF GV_OUTPUT IS NOT INITIAL.
        PERFORM F_COMMIT.
        GS_SAVE-STATU = '@08@'.
        GS_SAVE-EBELN = GS_ZSFI_POST_MIRO-EBELN.
        GS_SAVE-BELNR = GV_OUTPUT.
        GS_SAVE-XBLNR = GS_RESULT-XBLNR.
        GS_SAVE-BLDAT = GS_RESULT-BLDAT.
        GS_SAVE-MESSG = GV_MESSAGE.
        APPEND GS_SAVE TO GT_SAVE.

        CONCATENATE GS_SAVE-BELNR E_GJAHR INTO LS_DOC-AWKEY.
        LS_DOC-BELNR = GS_SAVE-BELNR.
        LS_DOC-GJAHR = E_GJAHR.
        APPEND LS_DOC TO LT_DOC.
*        MESSAGE s998 WITH gv_message.
      ELSE.
        GS_SAVE-STATU = '@0A@'.
        GS_SAVE-EBELN = GS_ZSFI_POST_MIRO-EBELN.
        GS_SAVE-MESSG = GV_MESSAGE.
        APPEND GS_SAVE TO GT_SAVE.
*        MESSAGE s998 WITH gv_message DISPLAY LIKE 'E'.
      ENDIF.
      CLEAR : GT_DETAIL_MAP,GV_OUTPUT,GV_MESSAGE.
      PERFORM F_ROLLBACK.
    ENDAT.
  ENDLOOP.

  IF GT_SAVE[] IS NOT INITIAL.
    PERFORM F_GET_ADDTIONAL_DATA_SAVE TABLES LT_DOC.
    PERFORM F_GET_DATA(ZSDSFIR0870) TABLES GT_SAVE.
    LEAVE TO SCREEN 0.
  ELSE.
    MESSAGE S998 WITH TEXT-101 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " F_SAVE_DETAIL
*&---------------------------------------------------------------------*
*&      Form  F_COMMIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_COMMIT .
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.
ENDFORM.                    " F_COMMIT
*&---------------------------------------------------------------------*
*&      Form  F_ROLLBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ROLLBACK .
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ENDFORM.                    " F_ROLLBACK
*&---------------------------------------------------------------------*
*&      Form  F_SET_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0798   text
*----------------------------------------------------------------------*
FORM F_SET_ALL  USING P_CHK.

  DATA LS_TMP LIKE GS_RESULT.

  LS_TMP-CHECK = P_CHK.

  MODIFY GT_RESULT FROM LS_TMP TRANSPORTING CHECK
                               WHERE CHECK NE P_CHK.

ENDFORM.                    " F_SET_ALL
*&---------------------------------------------------------------------*
*&      Form  F_GET_ADDTIONAL_DATA_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DOC  text
*----------------------------------------------------------------------*
FORM F_GET_ADDTIONAL_DATA_SAVE TABLES FT_DOC.

  DATA : BEGIN OF LS_DOC,
           BELNR TYPE RBKP-BELNR,
           AWKEY TYPE BKPF-AWKEY,
           GJAHR TYPE BKPF-GJAHR,
         END OF LS_DOC.
  DATA LT_DOC LIKE TABLE OF LS_DOC.

  DATA : BEGIN OF LS_BKPF,
           AWKEY TYPE BKPF-AWKEY,
           BELNR TYPE BKPF-BELNR,
           BKTXT TYPE BKPF-BKTXT,
         END OF LS_BKPF.
  DATA LT_BKPF LIKE TABLE OF LS_BKPF.

  FIELD-SYMBOLS <LFS_SAVE> LIKE GS_SAVE.

  LT_DOC[] = FT_DOC[].

  IF LT_DOC[] IS NOT INITIAL.
    SELECT AWKEY
           BELNR
           BKTXT
      FROM BKPF
      INTO TABLE LT_BKPF
      FOR ALL ENTRIES IN LT_DOC
      WHERE AWKEY EQ LT_DOC-AWKEY.

    LOOP AT GT_SAVE ASSIGNING <LFS_SAVE>.
      READ TABLE LT_DOC INTO LS_DOC
      WITH KEY BELNR = <LFS_SAVE>-BELNR.
      IF SY-SUBRC EQ 0.
        READ TABLE LT_BKPF INTO LS_BKPF
        WITH KEY AWKEY = LS_DOC-AWKEY.
        IF SY-SUBRC EQ 0.
          <LFS_SAVE>-FIDOC = LS_BKPF-BELNR.
          <LFS_SAVE>-BKTXT = LS_BKPF-BKTXT.
          PERFORM F_INSERT_ITEM USING <LFS_SAVE>-FIDOC
                                      LS_DOC-GJAHR
                                      <LFS_SAVE>-EBELN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_GET_ADDTIONAL_DATA_SAVE
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LFS_SAVE>_FIDOC  text
*      -->P_<LFS_SAVE>_BKTXT  text
*----------------------------------------------------------------------*
FORM F_INSERT_ITEM  USING  LV_FIDOC
                           LV_GJAHR
                           LV_EBELN.

  DATA : LV_TEXT TYPE C LENGTH 255.

  DATA : LV_TMP TYPE EKKO-EBELN.

  LV_TMP = LV_EBELN.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = LV_TMP
    IMPORTING
      OUTPUT = LV_TMP.


  IF     LV_TMP+0(1) EQ '4'.
    LV_TEXT    = 'SUB-INSTALLATION'.
  ELSEIF LV_TMP+0(1) EQ '5'.
    LV_TEXT    = 'SUB-SERVICE'.
  ENDIF.

  CLEAR : BDCDATA[],MESSTAB[],BDCDATA.

  PERFORM BDC_DYNPRO   USING  'SAPMF05L'   '0100'.
  PERFORM BDC_FIELD    USING  'BDC_CURSOR' 'RF05L-BELNR'.
  PERFORM BDC_FIELD    USING  'BDC_OKCODE' '=AZ'.
  PERFORM BDC_FIELD    USING  'RF05L-BELNR' LV_FIDOC.
  PERFORM BDC_FIELD    USING  'RF05L-BUKRS' '1000'.
  PERFORM BDC_FIELD    USING  'RF05L-GJAHR' LV_GJAHR.

  PERFORM BDC_DYNPRO   USING  'SAPMF05L'   '0302'.
  PERFORM BDC_FIELD    USING  'BDC_CURSOR' 'BSEG-SGTXT'.
  PERFORM BDC_FIELD    USING  'BDC_OKCODE' '/00'.
  PERFORM BDC_FIELD    USING  'BSEG-SGTXT' LV_TEXT.

  PERFORM BDC_DYNPRO   USING  'SAPMF05L'   '0302'.
  PERFORM BDC_FIELD    USING  'BDC_CURSOR' 'BSEG-SGTXT'.
  PERFORM BDC_FIELD    USING  'BDC_OKCODE' '=AE'.
  PERFORM BDC_FIELD    USING  'BSEG-SGTXT' LV_TEXT.

  PERFORM BDC_TRANSACTION USING 'FB02'.

ENDFORM.                    " F_INSERT_ITEM
*&---------------------------------------------------------------------*
*&      Form  F_ADDTIONAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ADDTIONAL_DATA .
*  FIELD-SYMBOLS <LFS_RESULT> LIKE GS_RESULT.
*
*  DATA : BEGIN OF LS_ZTCS_INV_SUBINST,
*           EBELN TYPE ZTCS_INV_SUBINST-EBELN,
*           XBLNR TYPE ZTCS_INV_SUBINST-XBLNR,
*           BLDAT TYPE ZTCS_INV_SUBINST-BLDAT,
*           KVERM TYPE ZTCS_INV_SUBINST-KVERM,
*           MWSBP TYPE ZTCS_INV_SUBINST-MWSBP,
*         END OF LS_ZTCS_INV_SUBINST.
*  DATA LT_ZTCS_INV_SUBINST LIKE TABLE OF LS_ZTCS_INV_SUBINST.
*
*  SELECT EBELN
*         XBLNR
*         BLDAT
*         KVERM
*         MWSBP
*    FROM ZTCS_INV_SUBINST
*    INTO TABLE LT_ZTCS_INV_SUBINST
*    FOR ALL ENTRIES IN GT_RESULT
*    WHERE EBELN EQ GT_RESULT-EBELN.
*
*  LOOP AT LT_ZTCS_INV_SUBINST INTO LS_ZTCS_INV_SUBINST.
*    READ TABLE GT_RESULT ASSIGNING <LFS_RESULT>
*    WITH KEY EBELN = LS_ZTCS_INV_SUBINST-EBELN.
*    IF SY-SUBRC EQ 0.
*      <LFS_RESULT>-EBELN = LS_ZTCS_INV_SUBINST-EBELN.
*      <LFS_RESULT>-XBLNR = LS_ZTCS_INV_SUBINST-XBLNR.
*      <LFS_RESULT>-BLDAT = LS_ZTCS_INV_SUBINST-BLDAT.
*      <LFS_RESULT>-KVERM = LS_ZTCS_INV_SUBINST-KVERM.
*      <LFS_RESULT>-MWSBP = LS_ZTCS_INV_SUBINST-MWSBP.
*    ENDIF.
*
**    IF p_kverm IS NOT INITIAL.
*    <LFS_RESULT>-KVERM = P_KVERM.
**    ENDIF.
*    CLEAR : LS_ZTCS_INV_SUBINST.
*  ENDLOOP.

ENDFORM.                    " F_ADDTIONAL_DATA
