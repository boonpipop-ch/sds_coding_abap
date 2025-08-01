*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0870
*  Creation Date      : 22.07.2025
*  Author             : Jakarin Sirilertlak
*  Add-on ID          :
*  Description        : Post Sub-Service & InStall
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
REPORT ZSDSFIR0870 MESSAGE-ID ZSDSFI01.
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
          STATU TYPE ICON-ID,
          EBELN TYPE EKET-EBELN,
          BELNR TYPE RBKP-BELNR,
          FIDOC TYPE BKPF-BELNR,
          XBLNR TYPE BKPF-XBLNR,
          BLDAT TYPE BKPF-BLDAT,
          BKTXT TYPE BKPF-BKTXT,
          MESSG TYPE C LENGTH 400,
        END OF GY_RESULT.
*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

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

*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*  PERFORM f_show_report.
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
FORM F_GET_DATA TABLES LT_RESULT STRUCTURE GS_RESULT.
  GT_RESULT[] = LT_RESULT[].
  PERFORM F_SHOW_REPORT.
ENDFORM.                    "f_get_data
*&---------------------------------------------------------------------*
*&      Form  pf_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_PF_ALV_GRID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = SY-REPID
      "_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE  = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME        =
*     I_BACKGROUND_ID         = ' '
*     I_GRID_TITLE            =
*     I_GRID_SETTINGS         =
      IS_LAYOUT               = GS_LAYOUT
      IT_FIELDCAT             = GT_FCAT
*     IT_EXCLUDING            =
*     IT_SPECIAL_GROUPS       =
      IT_SORT                 = GT_SORT
*     IT_FILTER               =
*     IS_SEL_HIDE             =
      I_DEFAULT               = 'X'
      I_SAVE                  = 'X'
*     IS_VARIANT              =
*     IT_EVENTS               =
*     IT_EVENT_EXIT           =
*     IS_PRINT                =
*     IS_REPREP_ID            =
*     I_SCREEN_START_COLUMN   = 0
*     I_SCREEN_START_LINE     = 0
*     I_SCREEN_END_COLUMN     = 0
*     I_SCREEN_END_LINE       = 0
*     I_HTML_HEIGHT_TOP       = 0
*     I_HTML_HEIGHT_END       = 0
*     IT_ALV_GRAPHICS         =
*     IT_HYPERLINK            =
*     IT_ADD_FIELDCAT         =
*     IT_EXCEPT_QINFO         =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER =
*     ES_EXIT_CAUSED_BY_USER  =
    TABLES
      T_OUTTAB                = GT_RESULT
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.
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

  CLEAR : GT_FCAT[],LS_FCAT.
*  ls_fcat-ref_tabname = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'STATU'.
  LS_FCAT-SELTEXT_S   = 'Status'.
  LS_FCAT-SELTEXT_M   = 'Status'.
  LS_FCAT-SELTEXT_L   = 'Status'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKKO'.
  LS_FCAT-FIELDNAME   = 'EBELN'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'RBKP'.
  LS_FCAT-FIELDNAME   = 'BELNR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'FIDOC'.
  LS_FCAT-SELTEXT_S   = 'FI Document'.
  LS_FCAT-SELTEXT_M   = 'FI Document'.
  LS_FCAT-SELTEXT_L   = 'FI Document'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'BKPF'.
  LS_FCAT-FIELDNAME   = 'BLDAT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'BKPF'.
  LS_FCAT-FIELDNAME   = 'XBLNR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'BKPF'.
  LS_FCAT-FIELDNAME   = 'BKTXT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
*  ls_fcat-ref_tabname = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'MESSG'.
  LS_FCAT-SELTEXT_S   = 'Message'.
  LS_FCAT-SELTEXT_M   = 'Message'.
  LS_FCAT-SELTEXT_L   = 'Message'.
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
