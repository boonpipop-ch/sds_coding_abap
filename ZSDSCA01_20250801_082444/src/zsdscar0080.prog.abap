*-----------------------------------------------------------------------
*  Program ID         : ZSDSCAR0080
*  Creation Date      : 19.10.2024
*  Author             : Jakarin S.
*  Add-on ID          : <<Refer WRICEF List)
*  Description        : <<Description of program>>
*  Purpose            :
*  Copied from        : <<Reference Program>>
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSCAR0080.
TYPE-POOLS : TRUXS,SLIS,ICON.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : AGR_USERS,AGR_TCODES,USER_ADDR.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF GY_RESULT,
  AGR_NAME   TYPE AGR_USERS-AGR_NAME,
  UNAME      TYPE AGR_USERS-UNAME,
  TCODE      TYPE AGR_TCODES-TCODE,
  FROM_DAT   TYPE AGR_USERS-FROM_DAT,
  TO_DAT     TYPE AGR_USERS-TO_DAT,
  CHANGE_DAT TYPE AGR_USERS-CHANGE_DAT,
  CHANGE_TIM TYPE AGR_USERS-CHANGE_TIM,
  TEXT       TYPE AGR_TEXTS-TEXT,
  DEPARTMENT TYPE USER_ADDR-DEPARTMENT,
  TTEXT      TYPE TSTCT-TTEXT,
  COUNT      TYPE I,
END OF GY_RESULT.
*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA : MESSTAB     LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       T_MESSTAB   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       S_MESSTAB   LIKE BDCMSGCOLL.

DATA : DYNPFIELDS  LIKE DYNPREAD OCCURS 5 WITH HEADER LINE.

DATA : GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
       GS_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_SORT TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT TYPE SLIS_SORTINFO_ALV.

*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : GR_STAT FOR JEST-STAT,
         GR_USER FOR AGR_USERS-UNAME.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : GC_TRUE      TYPE C VALUE 'X'.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_AGRNA FOR AGR_USERS-AGR_NAME,
                 S_UNAME FOR AGR_USERS-UNAME,
                 S_TCODE FOR AGR_TCODES-TCODE,
                 S_DEPAR FOR USER_ADDR-DEPARTMENT LOWER CASE.
SELECTION-SCREEN END OF BLOCK BLOCK1.

PARAMETERS P_GROUP AS CHECKBOX.
*&---------------------------------------------------------------------*
*  INITIALIZATION.
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_EXCLUDE_USER.
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
    PERFORM F_SHOW_REPORT.
  ELSE.
    MESSAGE 'Data Not Found' TYPE 'S' DISPLAY LIKE 'E'.
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

  SELECT AGR_USERS~AGR_NAME,
         AGR_USERS~UNAME,
         AGR_TCODES~TCODE,
         AGR_USERS~FROM_DAT,
         AGR_USERS~TO_DAT,
         AGR_USERS~CHANGE_DAT,
         AGR_USERS~CHANGE_TIM,
         AGR_TEXTS~TEXT,
         USER_ADDR~DEPARTMENT,
         TSTCT~TTEXT
    FROM AGR_USERS
    INNER JOIN AGR_TEXTS  ON AGR_USERS~AGR_NAME EQ AGR_TEXTS~AGR_NAME  AND
                             AGR_TEXTS~SPRAS    EQ @SY-LANGU            AND
                             AGR_TEXTS~LINE     EQ @SPACE
    INNER JOIN AGR_TCODES ON AGR_USERS~AGR_NAME EQ AGR_TCODES~AGR_NAME AND
                             AGR_TCODES~TYPE    EQ 'TR'
    INNER JOIN TSTCT      ON AGR_TCODES~TCODE   EQ TSTCT~TCODE         AND
                             TSTCT~SPRSL        EQ @SY-LANGU
    INNER JOIN USR02      ON AGR_USERS~UNAME    EQ USR02~BNAME
    LEFT OUTER JOIN USER_ADDR  ON AGR_USERS~UNAME    EQ USER_ADDR~BNAME
    INTO TABLE @GT_RESULT
    WHERE AGR_USERS~AGR_NAME   IN @S_AGRNA
      AND AGR_USERS~UNAME      IN @S_UNAME
      AND AGR_USERS~UNAME      IN @GR_USER
      AND AGR_USERS~FROM_DAT   LE @SY-DATUM
      AND AGR_USERS~TO_DAT     GE @SY-DATUM
      AND AGR_TCODES~TCODE     IN @S_TCODE
      AND USER_ADDR~DEPARTMENT IN @S_DEPAR
      AND ( USR02~GLTGB        GE @SY-DATUM OR
            USR02~GLTGB        EQ '00000000' )
      AND ( USR02~USTYP        EQ 'A' OR
          ( USR02~USTYP        EQ 'S' AND
            USR02~CLASS        EQ 'OPERATIONAL' ) ).

  SORT GT_RESULT BY AGR_NAME
                    UNAME
                    TCODE
                    FROM_DAT DESCENDING.

  DELETE ADJACENT DUPLICATES FROM GT_RESULT COMPARING AGR_NAME UNAME TCODE.

  IF P_GROUP EQ 'X'.
    SORT GT_RESULT BY UNAME TO_DAT DESCENDING.
    DELETE ADJACENT DUPLICATES FROM GT_RESULT COMPARING UNAME.
  ENDIF.

  GS_RESULT-COUNT = 1.

  MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING COUNT
                                  WHERE AGR_NAME IS  NOT INITIAL.


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
      "_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
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
  IF P_GROUP NE 'X'.
    CLEAR LS_FCAT.
    LS_FCAT-REF_TABNAME = 'AGR_USERS'.
    LS_FCAT-FIELDNAME   = 'AGR_NAME'.
    APPEND LS_FCAT TO GT_FCAT.

    CLEAR LS_FCAT.
    LS_FCAT-REF_TABNAME = 'AGR_TEXTS'.
    LS_FCAT-FIELDNAME   = 'TEXT'.
    APPEND LS_FCAT TO GT_FCAT.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = 'TCODE'.
    LS_FCAT-SELTEXT_S   = 'T-Code'.
    LS_FCAT-SELTEXT_M   = 'T-Code'.
    LS_FCAT-SELTEXT_L   = 'T-Code'.
    APPEND LS_FCAT TO GT_FCAT.

    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME   = 'TTEXT'.
    LS_FCAT-SELTEXT_S   = 'T-Code Desc.'.
    LS_FCAT-SELTEXT_M   = 'T-Code Desc.'.
    LS_FCAT-SELTEXT_L   = 'T-Code Description'.
    APPEND LS_FCAT TO GT_FCAT.

  ENDIF.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'AGR_USERS'.
  LS_FCAT-FIELDNAME   = 'UNAME'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'USER_ADDR'.
  LS_FCAT-FIELDNAME   = 'DEPARTMENT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'AGR_USERS'.
  LS_FCAT-FIELDNAME   = 'FROM_DAT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'AGR_USERS'.
  LS_FCAT-FIELDNAME   = 'TO_DAT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'AGR_USERS'.
  LS_FCAT-FIELDNAME   = 'CHANGE_DAT'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'AGR_USERS'.
  LS_FCAT-FIELDNAME   = 'CHANGE_TIM'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'COUNT'.
  LS_FCAT-SELTEXT_S   = 'Count'.
  LS_FCAT-SELTEXT_M   = 'Count'.
  LS_FCAT-SELTEXT_L   = 'Count'.
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
*&      Form  F_EXCLUDE_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXCLUDE_USER .
*  CLEAR GR_USER.
*  GR_USER-SIGN   = 'E'.
*  GR_USER-OPTION = 'CP'.
*  GR_USER-LOW    = 'ATOS*'.
*  APPEND GR_USER.
*
*  CLEAR GR_USER.
*  GR_USER-SIGN   = 'E'.
*  GR_USER-OPTION = 'CP'.
*  GR_USER-LOW    = 'ISS*'.
*  APPEND GR_USER.
*
*  CLEAR GR_USER.
*  GR_USER-SIGN   = 'E'.
*  GR_USER-OPTION = 'CP'.
*  GR_USER-LOW    = 'WEB*'.
*  APPEND GR_USER.
*
*  CLEAR GR_USER.
*  GR_USER-SIGN   = 'E'.
*  GR_USER-OPTION = 'EQ'.
*  GR_USER-LOW    = 'NITAYA'.
*  APPEND GR_USER.
ENDFORM.                    " F_EXCLUDE_USER
