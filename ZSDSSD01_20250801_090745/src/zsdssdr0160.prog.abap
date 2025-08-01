*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0160
*  Creation Date      : 23.06.2024
*  Author             : Jakarin S.
*  Add-on ID          : ZSDF001
*  Description        : Print DO no Serial by Customer
*  Purpose            :
*  Copied from        : ZP_SD_DO_CUST_SERIAL
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0160 MESSAGE-ID ZSDSSD01.

TYPE-POOLS : TRUXS,SLIS,ICON.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : VBAK,VBEP.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF GY_RESULT,
*  vbeln  TYPE vbap-vbeln,
*  posnr  TYPE vbap-posnr,
          KUNNR TYPE VBAK-KUNNR,
          NAME1 TYPE KNA1-NAME1,
          NAME2 TYPE KNA1-NAME2,
*  kwmeng TYPE vbap-kwmeng,
*  meins  TYPE vbap-meins,
*  netwr  TYPE vbap-netwr,
*  vtweg  TYPE vbak-vtweg,
*  vkbur  TYPE vbak-vkbur,
*  vkgrp  TYPE vbak-vkgrp,
*  vtext  TYPE tvtwt-vtext,
*  saleo  TYPE tvkbt-bezei,
*  saleg  TYPE tvgrt-bezei,
*  edatu  TYPE vbep-edatu,
          CHECK TYPE C,
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
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_KUNNR FOR VBAK-KUNNR." OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK1.

*SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
*PARAMETER : r1 RADIOBUTTON GROUP log DEFAULT 'X',
*            r2 RADIOBUTTON GROUP log.
*SELECTION-SCREEN END OF BLOCK block2.
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
    PERFORM F_GET_ADDITIONAL_DATA.
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

  SELECT KNA1~KUNNR
         KNA1~NAME1
         KNA1~NAME2
    FROM KNA1 "vbak
    INTO TABLE GT_RESULT
    WHERE KNA1~KUNNR IN S_KUNNR.

  SORT GT_RESULT BY KUNNR.

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
*  ls_fcat-ref_tabname = 'GT_RESULT'.
  LS_FCAT-FIELDNAME   = 'CHECK'.
  LS_FCAT-SELTEXT_S   = 'No Serial'.
  LS_FCAT-SELTEXT_M   = 'No Serial'.
  LS_FCAT-SELTEXT_L   = 'No Serial'.
  LS_FCAT-CHECKBOX    = 'X'.
  LS_FCAT-INPUT       = 'X'.
  LS_FCAT-EDIT        = 'X'.
  APPEND LS_FCAT TO GT_FCAT.

*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'VBAP'.
*  ls_fcat-fieldname   = 'VBELN'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'VBAP'.
*  ls_fcat-fieldname   = 'POSNR'.
*  APPEND ls_fcat TO gt_fcat.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'VBAK'.
  LS_FCAT-FIELDNAME   = 'KUNNR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'NAME1'.
  LS_FCAT-SELTEXT_S   = 'Customer Name1'.
  LS_FCAT-SELTEXT_M   = 'Customer Name1'.
  LS_FCAT-SELTEXT_L   = 'Customer Name1'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'NAME2'.
  LS_FCAT-SELTEXT_S   = 'Customer Name1'.
  LS_FCAT-SELTEXT_M   = 'Customer Name1'.
  LS_FCAT-SELTEXT_L   = 'Customer Name1'.
  LS_FCAT-NO_OUT      = 'X'.
  APPEND LS_FCAT TO GT_FCAT.

*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'VBEP'.
*  ls_fcat-fieldname   = 'EDATU'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'VBAP'.
*  ls_fcat-fieldname   = 'KWMENG'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'VBAP'.
*  ls_fcat-fieldname   = 'MEINS'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'VBAP'.
*  ls_fcat-fieldname   = 'NETWR'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'VBAK'.
*  ls_fcat-fieldname   = 'VKBUR'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'SALEO'.
*  ls_fcat-seltext_s   = 'Sales Office Desc.'.
*  ls_fcat-seltext_m   = 'Sales Office Desc.'.
*  ls_fcat-seltext_l   = 'Sales Office Desc.'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-ref_tabname = 'VBAK'.
*  ls_fcat-fieldname   = 'VKGRP'.
*  APPEND ls_fcat TO gt_fcat.
*
*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'SALEG'.
*  ls_fcat-seltext_s   = 'Sales Group Desc.'.
*  ls_fcat-seltext_m   = 'Sales Group Desc.'.
*  ls_fcat-seltext_l   = 'Sales Group Desc.'.
*  APPEND ls_fcat TO gt_fcat.

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
**  CLEAR gs_sort.
**  gs_sort-fieldname = 'LIFNR'.
**  gs_sort-spos = '1'.
**  gs_sort-up = 'X'.
***  gs_sort-subtot = 'X'.
**  APPEND gs_sort TO gt_sort.
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
    WHEN 'SAVE'.
      PERFORM F_SAVE_DATA.
    WHEN 'ALL'.
      PERFORM F_SELECT USING 'X'.
    WHEN 'SAL'.
      PERFORM F_SELECT USING SPACE.
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
*&      Form  F_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAVE_DATA.

  DATA : LT_RESULT LIKE GT_RESULT.

  DATA : LS_ZSDSSDT007 TYPE ZSDSSDT007,
         LT_ZSDSSDT007 TYPE TABLE OF ZSDSSDT007.

  LT_RESULT[] = GT_RESULT[].

*  SORT lt_result BY check DESCENDING.
*  DELETE lt_result WHERE check EQ space.
*
*  IF lt_result[] IS NOT INITIAL.
  SELECT *
    FROM ZSDSSDT007
    INTO TABLE LT_ZSDSSDT007
    FOR ALL ENTRIES IN LT_RESULT
    WHERE KUNNR EQ LT_RESULT-KUNNR.

  LOOP AT LT_RESULT INTO GS_RESULT.
    READ TABLE LT_ZSDSSDT007 INTO LS_ZSDSSDT007
    WITH KEY KUNNR = GS_RESULT-KUNNR.
    IF SY-SUBRC NE 0.
      LS_ZSDSSDT007-ERNAM = SY-UNAME.
      LS_ZSDSSDT007-ERDAT = SY-DATUM.
    ENDIF.

    LS_ZSDSSDT007-KUNNR = GS_RESULT-KUNNR.
    LS_ZSDSSDT007-NOSER = GS_RESULT-CHECK.
    LS_ZSDSSDT007-AENAM = SY-UNAME.
    LS_ZSDSSDT007-AEDAT = SY-DATUM.

    MODIFY ZSDSSDT007 FROM LS_ZSDSSDT007.
    COMMIT WORK AND WAIT.
    CLEAR : GS_RESULT.
  ENDLOOP.
  MESSAGE S000 WITH TEXT-998.
  LEAVE TO SCREEN 0.
*  ELSE.
*    MESSAGE s000 WITH text-999 DISPLAY LIKE 'E'.
*  ENDIF.

ENDFORM.                    " F_SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM F_SELECT USING LV_CHECK.

  GS_RESULT-CHECK = LV_CHECK.
  MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING CHECK
                                  WHERE CHECK NE GS_RESULT-CHECK.

ENDFORM.                    " F_SELECT
*&---------------------------------------------------------------------*
*&      Form  F_GET_ADDITIONAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_ADDITIONAL_DATA .
  DATA : BEGIN OF LS_ZSDSSDT007,
           KUNNR TYPE ZSDSSDT007-KUNNR,
           NOSER TYPE ZSDSSDT007-NOSER,
         END OF LS_ZSDSSDT007.
  DATA LT_ZSDSSDT007 LIKE SORTED TABLE OF LS_ZSDSSDT007 WITH UNIQUE KEY KUNNR.

  DATA LV_TABIX TYPE SY-TABIX.

  SELECT KUNNR
         NOSER
    FROM ZSDSSDT007
    INTO TABLE LT_ZSDSSDT007
    FOR ALL ENTRIES IN GT_RESULT
    WHERE KUNNR EQ GT_RESULT-KUNNR.

  LOOP AT GT_RESULT INTO GS_RESULT.
    LV_TABIX = SY-TABIX.

    READ TABLE LT_ZSDSSDT007 INTO LS_ZSDSSDT007
    WITH TABLE KEY KUNNR = GS_RESULT-KUNNR.
    IF SY-SUBRC EQ 0.
      GS_RESULT-CHECK = LS_ZSDSSDT007-NOSER.
      MODIFY GT_RESULT FROM GS_RESULT INDEX LV_TABIX
                               TRANSPORTING CHECK.
    ENDIF.

    CLEAR : GS_RESULT,LS_ZSDSSDT007.
  ENDLOOP.

ENDFORM.                    " F_GET_ADDITIONAL_DATA
