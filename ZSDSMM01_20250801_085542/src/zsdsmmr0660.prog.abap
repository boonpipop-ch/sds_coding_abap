*&---------------------------------------------------------------------*
*& Report ZSDSMMR0660
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSMMR0660 MESSAGE-ID ZSDSMM01.
TYPE-POOLS : TRUXS,SLIS,ICON.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : ZSDSMMT025,ISEG.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF GY_RESULT,
  GJAHR            TYPE ZSDSMMT025-GJAHR,
  IBLNR            TYPE ZSDSMMT025-IBLNR,
  ITEM             TYPE ZSDSMMT025-ITEM,
  MATERIAL         TYPE ZSDSMMT025-MATERIAL,
  BATCH            TYPE ZSDSMMT025-BATCH,
  ENTRY_QNT        TYPE ZSDSMMT025-ENTRY_QNT,
  ENTRY_UOM        TYPE ZSDSMMT025-ENTRY_UOM,
  ENTRY_UOM_ISO    TYPE ZSDSMMT025-ENTRY_UOM_ISO,
  SALES_VAL        TYPE ZSDSMMT025-SALES_VAL,
  ZERO_COUNT       TYPE ZSDSMMT025-ZERO_COUNT,
  MATERIAL_EXTERNA TYPE ZSDSMMT025-MATERIAL_EXTERNA,
  MATERIAL_GUID    TYPE ZSDSMMT025-MATERIAL_GUID,
  MATERIAL_VERSION TYPE ZSDSMMT025-MATERIAL_VERSION,
  LGPBE            TYPE ZSDSMMT025-LGPBE,
  BUCHM            TYPE ZSDSMMT025-BUCHM,
  ERNAM            TYPE ZSDSMMT025-ERNAM,
  ERDAT            TYPE ZSDSMMT025-ERDAT,
  ERZET            TYPE ZSDSMMT025-ERZET,
  AEDAT            TYPE ZSDSMMT025-AEDAT,
  AEZET            TYPE ZSDSMMT025-AEZET,
  AENAM            TYPE ZSDSMMT025-AENAM,
  REMARK           TYPE ZSDSMMT025-REMARK,
  DIF              TYPE ERFMG,
  FLAG             TYPE C,
  ALL              TYPE ZSDSMMT025-BUCHM,
  CHECK            TYPE ZSDSMMT025-BUCHM,
  REMAIN           TYPE ZSDSMMT025-BUCHM,
  PROGRESS         TYPE C LENGTH 255,
  DIFS             TYPE ZSDSMMT025-BUCHM,
  DIF_PER          TYPE C LENGTH 255,
  NONDIF           TYPE ZSDSMMT025-BUCHM,
  NONDIF_PER       TYPE C LENGTH 255,
  MAKTX            TYPE MAKT-MAKTX,
  LGORT            TYPE ISEG-LGORT,
*  message          TYPE c LENGTH 255,
END OF GY_RESULT.

TYPES : BEGIN OF GY_RESULT_POST,
  STATUS  TYPE ICON-ID,
  GJAHR   TYPE ZSDSMMT025-GJAHR,
  IBLNR   TYPE ZSDSMMT025-IBLNR,
  MESSAGE TYPE C LENGTH 255,
END OF GY_RESULT_POST.

TYPES : BEGIN OF GY_ISEG,
  IBLNR TYPE ISEG-IBLNR,
  GJAHR TYPE ISEG-GJAHR,
  ZEILI TYPE ISEG-ZEILI,
  XZAEL TYPE ISEG-XZAEL,
*  matnr LIKE iseg-matnr,
*  werks LIKE iseg-werks,
*  lgort LIKE iseg-lgort,
*  meins LIKE mara-meins,
*  mtart LIKE mara-mtart,
*  matkl LIKE mara-matkl,
END OF GY_ISEG.

TYPES : BEGIN OF GY_MAKT,
  MATNR TYPE MAKT-MATNR,
  MAKTX TYPE MAKT-MAKTX,
END OF GY_MAKT.
*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : GT_RESULT_POST TYPE TABLE OF GY_RESULT_POST,
       GS_RESULT_POST TYPE GY_RESULT_POST.

DATA : GT_MAKT TYPE TABLE OF GY_MAKT,
       GS_MAKT TYPE GY_MAKT.

DATA : GT_ISEG TYPE TABLE OF GY_ISEG,
       GS_ISEG TYPE GY_ISEG.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB     LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB   LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT   TYPE SLIS_SORTINFO_ALV.

DATA : GT_FCAT1   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT1 TYPE SLIS_LAYOUT_ALV,
       GT_SORT1   TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT1   TYPE SLIS_SORTINFO_ALV.

*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : GR_STAT FOR JEST-STAT.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : GC_TRUE      TYPE C VALUE 'X'.

CONSTANTS : BEGIN OF GC_STATUS,
    ERROR   TYPE C LENGTH 4 VALUE '@0A@',
    SUCCESS TYPE C LENGTH 4 VALUE '@08@',
END OF GC_STATUS.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_GJAHR FOR  ZSDSMMT025-GJAHR,
                 S_IBLNR FOR  ZSDSMMT025-IBLNR,
                 S_ITEM  FOR  ZSDSMMT025-ITEM,
                 S_LGORT FOR  ISEG-LGORT,
                 S_MATNR FOR  ISEG-MATNR,
                 S_LGPBE FOR  ZSDSMMT025-LGPBE.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
PARAMETERS : P_DATE TYPE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK BLOCK2.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK3 WITH FRAME TITLE TEXT-003.
PARAMETERS : R1 RADIOBUTTON GROUP LOG,
             R2 RADIOBUTTON GROUP LOG,
             R3 RADIOBUTTON GROUP LOG.
SELECTION-SCREEN END OF BLOCK BLOCK3.
*&---------------------------------------------------------------------*
*  INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_GET_INITIAL.
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
  IF R3 NE 'X'.
    PERFORM F_GET_DATA.
  ELSE.
    PERFORM F_GET_DATA_SUM.
  ENDIF.
*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_RESULT[] IS NOT INITIAL.
    PERFORM F_SHOW_REPORT.
  ELSE.
    MESSAGE S003 DISPLAY LIKE 'E'.
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
  DATA: BEGIN OF LS_ISEG,
    GJAHR LIKE ISEG-GJAHR,
    IBLNR LIKE ISEG-IBLNR,
    ZEILI LIKE ISEG-ZEILI,
    MATNR LIKE ISEG-MATNR,
    WERKS LIKE ISEG-WERKS,
    LGORT LIKE ISEG-LGORT,
    BUCHM LIKE ISEG-BUCHM,
    MEINS LIKE MARA-MEINS,
    MTART LIKE MARA-MTART,
    MATKL LIKE MARA-MATKL,
    LGPBE LIKE MARD-LGPBE,
  END OF LS_ISEG.
  DATA LT_ISEG LIKE TABLE OF LS_ISEG.

  DATA : LT_TMP LIKE GT_RESULT,
         LS_TMP LIKE LINE OF GT_RESULT.

  SELECT ISEG~GJAHR
         ISEG~IBLNR
         ISEG~ZEILI
         ISEG~MATNR
         ISEG~WERKS
         ISEG~LGORT
         ISEG~BUCHM
         MARA~MEINS
         MARA~MTART
         MARA~MATKL
         MARD~LGPBE
   INTO TABLE LT_ISEG
   FROM ISEG
   INNER JOIN MARD ON ISEG~MATNR EQ MARD~MATNR AND
                      ISEG~LGORT EQ MARD~LGORT AND
                      MARD~WERKS EQ '1000'
   INNER JOIN MARA ON ISEG~MATNR EQ MARA~MATNR
   WHERE ISEG~IBLNR IN S_IBLNR
     AND ISEG~LGORT IN S_LGORT
     AND ISEG~MATNR IN S_MATNR
     AND ISEG~GJAHR IN S_GJAHR
     AND ISEG~ZEILI IN S_ITEM
     AND MARD~LGPBE IN S_LGPBE.

  IF LT_ISEG[] IS NOT INITIAL.
    SELECT GJAHR
           IBLNR
           ITEM
           MATERIAL
           BATCH
           ENTRY_QNT
           ENTRY_UOM
           ENTRY_UOM_ISO
           SALES_VAL
           ZERO_COUNT
           MATERIAL_EXTERNA
           MATERIAL_GUID
           MATERIAL_VERSION
           LGPBE
           BUCHM
           ERNAM
           ERDAT
           ERZET
           AEDAT
           AEZET
           AENAM
           REMARK
      FROM ZSDSMMT025
      INTO CORRESPONDING FIELDS OF TABLE LT_TMP
      FOR ALL ENTRIES IN LT_ISEG
      WHERE IBLNR EQ LT_ISEG-IBLNR"IN s_iblnr
        AND GJAHR EQ LT_ISEG-GJAHR"IN s_gjahr
        AND ITEM  EQ LT_ISEG-ZEILI"IN s_item
        AND LGPBE EQ LT_ISEG-LGPBE."IN s_lgpbe.

    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO TABLE GT_MAKT
      FOR ALL ENTRIES IN LT_ISEG
      WHERE MATNR EQ LT_ISEG-MATNR.
  ENDIF.

  LOOP AT LT_ISEG INTO LS_ISEG.

    READ TABLE LT_TMP INTO LS_TMP
    WITH KEY IBLNR = LS_ISEG-IBLNR
             GJAHR = LS_ISEG-GJAHR
             ITEM  = LS_ISEG-ZEILI
             LGPBE = LS_ISEG-LGPBE.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING LS_TMP TO GS_RESULT.
      GS_RESULT-LGORT    = LS_ISEG-LGORT.
    ELSE.
      GS_RESULT-GJAHR    = LS_ISEG-GJAHR.
      GS_RESULT-IBLNR    = LS_ISEG-IBLNR.
      GS_RESULT-ITEM     = LS_ISEG-ZEILI.
      GS_RESULT-MATERIAL = LS_ISEG-MATNR.
      GS_RESULT-LGPBE    = LS_ISEG-LGPBE.
      GS_RESULT-BUCHM    = LS_ISEG-BUCHM.
      GS_RESULT-LGORT    = LS_ISEG-LGORT.
      GS_RESULT-FLAG     = 'X'.
    ENDIF.

    READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = LS_ISEG-MATNR.
    IF SY-SUBRC = 0.
      GS_RESULT-MAKTX = GS_MAKT-MAKTX.
    ENDIF.

    APPEND GS_RESULT TO GT_RESULT.
    CLEAR : GS_RESULT,LS_TMP,LS_ISEG.
  ENDLOOP.

  IF GT_RESULT[] IS NOT INITIAL.
    PERFORM F_MODIF_DATA.
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
      I_CALLBACK_PROGRAM = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      IS_LAYOUT          = GS_LAYOUT
      IT_FIELDCAT        = GT_FCAT
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
      IT_SORT            = GT_SORT
*     IT_FILTER          =
*     IS_SEL_HIDE        =
      I_DEFAULT          = 'X'
      I_SAVE             = 'X'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB           = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
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
  GS_LAYOUT-ZEBRA             = 'X'.
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
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'IBLNR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'GJAHR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'ITEM'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'MATERIAL'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'MAKT'.
  LS_FCAT-FIELDNAME   = 'MAKTX'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'LGPBE'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'MARD'.
  LS_FCAT-FIELDNAME   = 'LGORT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
*  ls_fcat-ref_tabname  = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME    = 'BUCHM'.
  LS_FCAT-SELTEXT_S    = 'System Qty'.
  LS_FCAT-SELTEXT_M    = 'System Qty'.
  LS_FCAT-SELTEXT_L    = 'System Qty'.
  LS_FCAT-DECIMALS_OUT = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
*  ls_fcat-ref_tabname  = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME    = 'ENTRY_QNT'.
  LS_FCAT-SELTEXT_S    = 'Count Qty'.
  LS_FCAT-SELTEXT_M    = 'Count Qty'.
  LS_FCAT-SELTEXT_L    = 'Count Qty'.
  LS_FCAT-DECIMALS_OUT = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'DIF'.
  LS_FCAT-SELTEXT_S    = 'Dif'.
  LS_FCAT-SELTEXT_M    = 'Dif'.
  LS_FCAT-SELTEXT_L    = 'Dif'.
  LS_FCAT-EMPHASIZE    = 'C31'.
  LS_FCAT-DECIMALS_OUT = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'ENTRY_UOM'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'REMARK'.
  LS_FCAT-SELTEXT_S   = 'Remark'.
  LS_FCAT-SELTEXT_M   = 'Remark'.
  LS_FCAT-SELTEXT_L   = 'Remark'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'ERNAM'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'ERDAT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'ERZET'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'AEDAT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'AEZET'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'AENAM'.
  APPEND LS_FCAT TO GT_FCAT.

*   CLEAR ls_fcat.
*   ls_fcat-fieldname  = 'MESSAGE'.
*  ls_fcat-seltext_s   = 'Message'.
*  ls_fcat-seltext_m   = 'Message'.
*  ls_fcat-seltext_l   = 'Message'.
*   APPEND ls_fcat TO gt_fcat.



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
  IF R3 NE 'X'.
    PERFORM F_BUILD_FCAT.
  ELSE.
    PERFORM F_BUILD_FCAT_SUM.
  ENDIF.
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
    WHEN 'POST'.
      PERFORM F_CHECK_CHANGE.
      PERFORM F_INPUT_COUNT.
    WHEN 'REFRESH'.
      PERFORM F_CLEAR_DATA.
      IF R3 NE 'X'.
        PERFORM F_GET_DATA.
      ELSE.
        PERFORM F_GET_DATA_SUM.
      ENDIF.
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

  READ TABLE GT_RESULT INTO GS_RESULT INDEX LE_ROW.
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
*&      Form  F_MODIF_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MODIF_DATA .
  DATA LV_TABIX TYPE SY-TABIX.

  LOOP AT GT_RESULT INTO GS_RESULT.
    LV_TABIX = SY-TABIX.

    GS_RESULT-DIF = GS_RESULT-ENTRY_QNT - GS_RESULT-BUCHM.

    MODIFY GT_RESULT FROM GS_RESULT INDEX LV_TABIX
                             TRANSPORTING DIF.
    CLEAR GS_RESULT.
  ENDLOOP.

  IF R2 EQ 'X'.
    SORT GT_RESULT BY DIF.
    DELETE GT_RESULT WHERE DIF EQ 0.
  ENDIF.

  SORT GT_RESULT BY GJAHR IBLNR ITEM.

ENDFORM.                    " F_MODIF_DATA
*&---------------------------------------------------------------------*
*&      Form  F_POST_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_POST_COUNT .

  DATA : LV_PHYSINVENTORY TYPE IKPF-IBLNR,
         LV_FISCALYEAR    TYPE IKPF-GJAHR,
         LV_COUNT_DATE    TYPE IIKPF-ZLDAT.

  DATA : LT_ITEMS_C   TYPE TABLE OF BAPI_PHYSINV_COUNT_ITEMS,
         LT_RETURN_C TYPE TABLE OF BAPIRET2.

  DATA : LS_ITEMS_C   TYPE BAPI_PHYSINV_COUNT_ITEMS,
         LS_RETURN_C TYPE BAPIRET2.

  DATA LS_TMP LIKE GS_RESULT.

  SORT GT_RESULT BY GJAHR IBLNR ITEM.
  SORT GT_ISEG BY XZAEL GJAHR IBLNR ZEILI.

  LOOP AT GT_ISEG INTO GS_ISEG WHERE XZAEL EQ 'X'.
    READ TABLE GT_RESULT INTO GS_RESULT
    WITH KEY IBLNR = GS_ISEG-IBLNR
             ITEM  = GS_ISEG-ZEILI.
    IF SY-SUBRC = 0.
      DELETE GT_RESULT INDEX SY-TABIX.
    ENDIF.

    CLEAR : GS_RESULT,GS_ISEG.
  ENDLOOP.


  LOOP AT GT_RESULT INTO LS_TMP.

    MOVE-CORRESPONDING LS_TMP    TO GS_RESULT.
    MOVE-CORRESPONDING GS_RESULT TO LS_ITEMS_C.
    APPEND LS_ITEMS_C TO LT_ITEMS_C.

    AT END OF IBLNR.
      LV_PHYSINVENTORY = GS_RESULT-IBLNR.
      LV_FISCALYEAR    = GS_RESULT-GJAHR.
      LV_COUNT_DATE    = P_DATE.

      CALL FUNCTION 'BAPI_MATPHYSINV_COUNT'
        EXPORTING
          PHYSINVENTORY = LV_PHYSINVENTORY
          FISCALYEAR    = LV_FISCALYEAR
          COUNT_DATE    = LV_COUNT_DATE
        TABLES
          ITEMS         = LT_ITEMS_C
          RETURN        = LT_RETURN_C.

      READ TABLE LT_RETURN_C INTO LS_RETURN_C
      WITH KEY TYPE = 'E'.
      IF SY-SUBRC = 0.
        GS_RESULT_POST-STATUS  = GC_STATUS-ERROR.
        GS_RESULT_POST-GJAHR   = LV_FISCALYEAR.
        GS_RESULT_POST-IBLNR   = LV_PHYSINVENTORY.
        GS_RESULT_POST-MESSAGE = LS_RETURN_C-MESSAGE.
      ELSE.
        PERFORM F_COMMIT.
        GS_RESULT_POST-STATUS  = GC_STATUS-SUCCESS.
        GS_RESULT_POST-GJAHR   = LV_FISCALYEAR.
        GS_RESULT_POST-IBLNR   = LV_PHYSINVENTORY.
        GS_RESULT_POST-MESSAGE = TEXT-101.
      ENDIF.

      APPEND GS_RESULT_POST TO GT_RESULT_POST.

      CLEAR : LV_PHYSINVENTORY,LV_FISCALYEAR,LV_COUNT_DATE,LT_ITEMS_C[],LT_RETURN_C[],GS_RESULT_POST,
              LS_RETURN_C.

    ENDAT.

    CLEAR : GS_RESULT,LS_ITEMS_C.
  ENDLOOP.

ENDFORM.                    " F_POST_COUNT
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_REPORT_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SHOW_REPORT_POST .
  PERFORM F_SET_LAYOUT_OUTPUT_POST.
  PERFORM F_BUILD_FCAT_POST.
  PERFORM F_SORT_POST.
  PERFORM F_PF_ALV_GRID_POST.

ENDFORM.                    " F_SHOW_REPORT_POST
*&---------------------------------------------------------------------*
*&      Form  F_SET_LAYOUT_OUTPUT_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SET_LAYOUT_OUTPUT_POST .
  "gs_layout-box_fieldname     = 'SEL'.
  GS_LAYOUT1-ZEBRA             = 'X'.
  GS_LAYOUT1-COLWIDTH_OPTIMIZE = 'X'.

ENDFORM.                    " F_SET_LAYOUT_OUTPUT_POST
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FCAT_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUILD_FCAT_POST .
  DATA:
  LS_FCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR : GT_FCAT1[],LS_FCAT.
*  ls_fcat-ref_tabname  = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME    = 'STATUS '.
  LS_FCAT-SELTEXT_S    = 'System Qty'.
  LS_FCAT-SELTEXT_M    = 'System Qty'.
  LS_FCAT-SELTEXT_L    = 'System Qty'.
  APPEND LS_FCAT TO GT_FCAT1.


  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'IBLNR'.
  APPEND LS_FCAT TO GT_FCAT1.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME   = 'GJAHR'.
  APPEND LS_FCAT TO GT_FCAT1.

  CLEAR LS_FCAT.
*  ls_fcat-ref_tabname  = 'ZSDSMMT025'.
  LS_FCAT-FIELDNAME    = 'MESSAGE'.
  LS_FCAT-SELTEXT_S    = 'Message'.
  LS_FCAT-SELTEXT_M    = 'Message'.
  LS_FCAT-SELTEXT_L    = 'Message'.
  APPEND LS_FCAT TO GT_FCAT1.




ENDFORM.                    " F_BUILD_FCAT_POST
*&---------------------------------------------------------------------*
*&      Form  F_PF_ALV_GRID_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PF_ALV_GRID_POST .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_pf_status_set = 'PF_STATUS_1'
*     i_callback_user_command  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
     IS_LAYOUT          = GS_LAYOUT1
     IT_FIELDCAT        = GT_FCAT1
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
     IT_SORT            = GT_SORT1
*     IT_FILTER          =
*     IS_SEL_HIDE        =
     I_DEFAULT          = 'X'
     I_SAVE             = 'X'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
   TABLES
     T_OUTTAB           = GT_RESULT_POST
   EXCEPTIONS
     PROGRAM_ERROR      = 1
     OTHERS             = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_PF_ALV_GRID_POST
*&---------------------------------------------------------------------*
*&      Form  F_SORT_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SORT_POST .
*  CLEAR gs_sort.
*  gs_sort1-fieldname = 'LIFNR'.
*  gs_sort1-spos = '1'.
*  gs_sort1-up = 'X'.
*  gs_sort1-subtot = 'X'.
*  APPEND gs_sort1 TO gt_sort1.

ENDFORM.                    " F_SORT_POST
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
     WAIT          = 'X'
* IMPORTING
*   RETURN        =
            .

ENDFORM.                    " F_COMMIT
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_CHANGE .

  SELECT IBLNR
         GJAHR
         ZEILI
         XZAEL
    FROM ISEG
    INTO TABLE GT_ISEG
    FOR ALL ENTRIES IN GT_RESULT
    WHERE IBLNR EQ GT_RESULT-IBLNR
      AND GJAHR EQ GT_RESULT-GJAHR
      AND ZEILI EQ GT_RESULT-ITEM.

ENDFORM.                    " F_CHECK_CHANGE
*&---------------------------------------------------------------------*
*&      Form  F_INPUT_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INPUT_COUNT .

  DATA : LT_TMP LIKE GT_RESULT.

  LT_TMP = GT_RESULT.

  SORT GT_RESULT BY FLAG DESCENDING.
  DELETE GT_RESULT WHERE FLAG EQ 'X'.

  PERFORM F_POST_COUNT.

  READ TABLE GT_ISEG INTO GS_ISEG
  WITH KEY XZAEL = 'X'.
  IF SY-SUBRC = 0.
    GT_RESULT = LT_TMP.
    SORT GT_RESULT BY FLAG DESCENDING.
    DELETE GT_RESULT WHERE FLAG EQ 'X'.
    PERFORM F_CHANGE_COUNT.
  ENDIF.

  IF GT_RESULT_POST[] IS NOT INITIAL.
    PERFORM F_SHOW_REPORT_POST.
    CLEAR : GT_RESULT_POST.
  ENDIF.

  GT_RESULT = LT_TMP.

ENDFORM.                    " F_INPUT_COUNT
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHANGE_COUNT .
  DATA : LV_PHYSINVENTORY TYPE IKPF-IBLNR,
         LV_FISCALYEAR    TYPE IKPF-GJAHR,
         LV_COUNT_DATE    TYPE IIKPF-ZLDAT.

  DATA : LT_ITEMS_C   TYPE TABLE OF BAPI_PHYSINV_COUNT_ITEMS,
         LT_RETURN_C TYPE TABLE OF BAPIRET2.

  DATA : LS_ITEMS_C   TYPE BAPI_PHYSINV_COUNT_ITEMS,
         LS_RETURN_C TYPE BAPIRET2.

  DATA LS_TMP LIKE GS_RESULT.

  SORT GT_RESULT BY GJAHR IBLNR ITEM.

  SORT GT_ISEG BY XZAEL GJAHR IBLNR ZEILI.

  LOOP AT GT_ISEG INTO GS_ISEG WHERE XZAEL EQ SPACE.
    READ TABLE GT_RESULT INTO GS_RESULT
    WITH KEY IBLNR = GS_ISEG-IBLNR
             ITEM  = GS_ISEG-ZEILI.
    IF SY-SUBRC = 0.
      DELETE GT_RESULT INDEX SY-TABIX.
    ENDIF.

    CLEAR : GS_RESULT,GS_ISEG.
  ENDLOOP.

  LOOP AT GT_RESULT INTO LS_TMP.

    MOVE-CORRESPONDING LS_TMP    TO GS_RESULT.
    MOVE-CORRESPONDING GS_RESULT TO LS_ITEMS_C.
    APPEND LS_ITEMS_C TO LT_ITEMS_C.

    AT END OF IBLNR.
      LV_PHYSINVENTORY = GS_RESULT-IBLNR.
      LV_FISCALYEAR    = GS_RESULT-GJAHR.
      LV_COUNT_DATE    = P_DATE.

      CALL FUNCTION 'BAPI_MATPHYSINV_CHANGECOUNT'
        EXPORTING
          PHYSINVENTORY = LV_PHYSINVENTORY
          FISCALYEAR    = LV_FISCALYEAR
        TABLES
          ITEMS         = LT_ITEMS_C
          RETURN        = LT_RETURN_C.

      READ TABLE LT_RETURN_C INTO LS_RETURN_C
      WITH KEY TYPE = 'E'.
      IF SY-SUBRC = 0.
        GS_RESULT_POST-STATUS  = GC_STATUS-ERROR.
        GS_RESULT_POST-GJAHR   = LV_FISCALYEAR.
        GS_RESULT_POST-IBLNR   = LV_PHYSINVENTORY.
        GS_RESULT_POST-MESSAGE = LS_RETURN_C-MESSAGE.
      ELSE.
        PERFORM F_COMMIT.
        GS_RESULT_POST-STATUS  = GC_STATUS-SUCCESS.
        GS_RESULT_POST-GJAHR   = LV_FISCALYEAR.
        GS_RESULT_POST-IBLNR   = LV_PHYSINVENTORY.
        GS_RESULT_POST-MESSAGE = TEXT-102.
      ENDIF.

      APPEND GS_RESULT_POST TO GT_RESULT_POST.

      CLEAR : LV_PHYSINVENTORY,LV_FISCALYEAR,LV_COUNT_DATE,LT_ITEMS_C[],LT_RETURN_C[],GS_RESULT_POST,
              LS_RETURN_C.

    ENDAT.

    CLEAR : GS_RESULT,LS_ITEMS_C.
  ENDLOOP.

ENDFORM.                    " F_CHANGE_COUNT
*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CLEAR_DATA .
  CLEAR : GT_RESULT,GT_RESULT_POST,GT_ISEG,GT_MAKT.
ENDFORM.                    " F_CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_INITIAL .
*  CLEAR S_LGORT.
*  S_LGORT-SIGN   = 'I'.
*  S_LGORT-OPTION = 'BT'.
*  S_LGORT-LOW    = '2100'.
*  S_LGORT-HIGH   = '2199'.
*  APPEND S_LGORT.

ENDFORM.                    " F_GET_INITIAL
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FCAT_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUILD_FCAT_SUM .
  DATA:
   LS_FCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'ALL'.
  LS_FCAT-SELTEXT_S    = 'Total Part'.
  LS_FCAT-SELTEXT_M    = 'Total Part'.
  LS_FCAT-SELTEXT_L    = 'Total Part'.
  LS_FCAT-DECIMALS_OUT = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'CHECK'.
  LS_FCAT-SELTEXT_S    = 'Checked'.
  LS_FCAT-SELTEXT_M    = 'Checked'.
  LS_FCAT-SELTEXT_L    = 'Checked'.
  LS_FCAT-DECIMALS_OUT = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'REMAIN'.
  LS_FCAT-SELTEXT_S    = 'Remain'.
  LS_FCAT-SELTEXT_M    = 'Remain'.
  LS_FCAT-SELTEXT_L    = 'Remain'.
  LS_FCAT-DECIMALS_OUT = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'PROGRESS'.
  LS_FCAT-SELTEXT_S    = 'Progress'.
  LS_FCAT-SELTEXT_M    = 'Progress'.
  LS_FCAT-SELTEXT_L    = 'Progress'.
*  ls_fcat-decimals_out = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'DIFS'.
  LS_FCAT-SELTEXT_S    = 'Dif'.
  LS_FCAT-SELTEXT_M    = 'Dif'.
  LS_FCAT-SELTEXT_L    = 'Dif'.
  LS_FCAT-DECIMALS_OUT = 0.
*  ls_fcat-decimals_out = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'DIF_PER'.
  LS_FCAT-SELTEXT_S    = 'Dif %'.
  LS_FCAT-SELTEXT_M    = 'Dif %'.
  LS_FCAT-SELTEXT_L    = 'Dif %'.
*  ls_fcat-decimals_out = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'NONDIF'.
  LS_FCAT-SELTEXT_S    = 'Non Dif'.
  LS_FCAT-SELTEXT_M    = 'Non Dif'.
  LS_FCAT-SELTEXT_L    = 'Non Dif'.
  LS_FCAT-DECIMALS_OUT = 0.
*  ls_fcat-decimals_out = 0.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME    = 'NONDIF_PER'.
  LS_FCAT-SELTEXT_S    = 'Non Dif %'.
  LS_FCAT-SELTEXT_M    = 'Non Dif %'.
  LS_FCAT-SELTEXT_L    = 'Non Dif %'.
*  ls_fcat-decimals_out = 0.
  APPEND LS_FCAT TO GT_FCAT.

ENDFORM.                    " F_BUILD_FCAT_SUM
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DATA_SUM .
  DATA: BEGIN OF LS_ISEG,
    GJAHR LIKE ISEG-GJAHR,
    IBLNR LIKE ISEG-IBLNR,
    ZEILI LIKE ISEG-ZEILI,
    MATNR LIKE ISEG-MATNR,
    WERKS LIKE ISEG-WERKS,
    LGORT LIKE ISEG-LGORT,
    BUCHM LIKE ISEG-BUCHM,
    MEINS LIKE MARA-MEINS,
    MTART LIKE MARA-MTART,
    MATKL LIKE MARA-MATKL,
    LGPBE LIKE MARD-LGPBE,
  END OF LS_ISEG.
  DATA LT_ISEG LIKE TABLE OF LS_ISEG.

  DATA : LT_TMP LIKE GT_RESULT,
         LS_TMP LIKE LINE OF GT_RESULT.

  SELECT ISEG~GJAHR
         ISEG~IBLNR
         ISEG~ZEILI
         ISEG~MATNR
         ISEG~WERKS
         ISEG~LGORT
         ISEG~BUCHM
         MARA~MEINS
         MARA~MTART
         MARA~MATKL
         MARD~LGPBE
   INTO TABLE LT_ISEG
   FROM ISEG
   INNER JOIN MARD ON ISEG~MATNR EQ MARD~MATNR AND
                      ISEG~LGORT EQ MARD~LGORT AND
                      MARD~WERKS EQ '1000'
   INNER JOIN MARA ON ISEG~MATNR EQ MARA~MATNR
   WHERE ISEG~IBLNR IN S_IBLNR
     AND ISEG~LGORT IN S_LGORT
     AND ISEG~MATNR IN S_MATNR
     AND ISEG~GJAHR IN S_GJAHR
     AND ISEG~ZEILI IN S_ITEM
     AND MARD~LGPBE IN S_LGPBE.

  IF LT_ISEG[] IS NOT INITIAL.
    SELECT GJAHR
           IBLNR
           ITEM
           MATERIAL
           BATCH
           ENTRY_QNT
           ENTRY_UOM
           ENTRY_UOM_ISO
           SALES_VAL
           ZERO_COUNT
           MATERIAL_EXTERNA
           MATERIAL_GUID
           MATERIAL_VERSION
           LGPBE
           BUCHM
           ERNAM
           ERDAT
           ERZET
           AEDAT
           AEZET
           AENAM
           REMARK
      FROM ZSDSMMT025
      INTO CORRESPONDING FIELDS OF TABLE LT_TMP
      FOR ALL ENTRIES IN LT_ISEG
      WHERE IBLNR EQ LT_ISEG-IBLNR"IN s_iblnr
        AND GJAHR EQ LT_ISEG-GJAHR"IN s_gjahr
        AND ITEM  EQ LT_ISEG-ZEILI"IN s_item
        AND LGPBE EQ LT_ISEG-LGPBE."IN s_lgpbe.
  ENDIF.

  DATA LV_CHECK TYPE I.

  DATA LV_PER TYPE I.

  LOOP AT LT_ISEG INTO LS_ISEG.

    ADD 1 TO GS_RESULT-ALL.

    READ TABLE LT_TMP INTO LS_TMP
    WITH KEY IBLNR = LS_ISEG-IBLNR
             GJAHR = LS_ISEG-GJAHR
             ITEM  = LS_ISEG-ZEILI
             LGPBE = LS_ISEG-LGPBE.
    IF SY-SUBRC = 0.
      ADD 1 TO GS_RESULT-CHECK.

      LV_CHECK = LS_ISEG-BUCHM - LS_TMP-ENTRY_QNT.
      IF LV_CHECK NE 0.
        ADD 1 TO GS_RESULT-DIFS.
      ELSE.
        ADD 1 TO GS_RESULT-NONDIF .
      ENDIF.
    ELSE.
      ADD 1 TO GS_RESULT-REMAIN.
      ADD 1 TO GS_RESULT-DIFS.
    ENDIF.

    CLEAR : LS_TMP,LS_ISEG.
  ENDLOOP.

  LV_PER   = ( GS_RESULT-CHECK * 100 ) / GS_RESULT-ALL.
  GS_RESULT-PROGRESS = LV_PER.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_RESULT-PROGRESS WITH ''.
  CONCATENATE GS_RESULT-PROGRESS '%' INTO GS_RESULT-PROGRESS.

  LV_PER        = ( GS_RESULT-DIFS * 100 ) / GS_RESULT-ALL.
  GS_RESULT-DIF_PER = LV_PER.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_RESULT-DIF_PER WITH ''.
  CONCATENATE GS_RESULT-DIF_PER '%' INTO GS_RESULT-DIF_PER.

  LV_PER    = ( GS_RESULT-NONDIF * 100 ) / GS_RESULT-ALL.
  GS_RESULT-NONDIF_PER = LV_PER.
  REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN GS_RESULT-NONDIF_PER WITH ''.
  CONCATENATE GS_RESULT-NONDIF_PER '%' INTO GS_RESULT-NONDIF_PER.

  APPEND GS_RESULT TO GT_RESULT.
  CLEAR GS_RESULT.
ENDFORM.                    " F_GET_DATA_SUM
