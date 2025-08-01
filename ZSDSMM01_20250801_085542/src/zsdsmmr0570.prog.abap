*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0570
*  Creation Date      : 25.10.2024
*  Author             : Jakarin S.
*  Add-on ID          :
*  Description        : Update PO Delivery Date
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
REPORT ZSDSMMR0570 MESSAGE-ID ZSDSMM01.
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
          EBELN  TYPE EKET-EBELN,
          EBELP  TYPE EKET-EBELP,
          MATNR  TYPE EKPO-MATNR,
          MENGE  TYPE EKPO-MENGE,
          MEINS  TYPE EKPO-MEINS,
          EINDT  TYPE EKET-EINDT,
          ETENR  TYPE EKET-ETENR,
          WEMNG  TYPE EKET-WEMNG,    "CH01++
          DELIB  TYPE EKET-EINDT,
          REMAIN TYPE EKPO-MENGE,   "CH01++
          CHECK  TYPE C,
        END OF GY_RESULT.

*TYPES : BEGIN OF GY_ZTMM_STATUS_PO,
*  EBELN  TYPE ZTMM_STATUS_PO-EBELN,
*  STATUS TYPE ZTMM_STATUS_PO-STATUS,
*END OF GY_ZTMM_STATUS_PO.
*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : GT_RESULT TYPE TABLE OF GY_RESULT,
       GS_RESULT TYPE GY_RESULT.

*DATA : GT_ZTMM_STATUS_PO TYPE TABLE OF GY_ZTMM_STATUS_PO,
*       GS_ZTMM_STATUS_PO TYPE GY_ZTMM_STATUS_PO.

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
  SELECT-OPTIONS : S_EBELN FOR EKPO-EBELN NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK BLOCK1.
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
    PERFORM F_ADDITIONAL_DATA.
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

  FIELD-SYMBOLS: <LFS_RESULT> TYPE GY_RESULT. "CH01++

  SELECT EKPO~EBELN
         EKPO~EBELP
         EKPO~MATNR
         EKPO~MENGE
         EKPO~MEINS
         EKET~EINDT
         EKET~ETENR
         EKET~WEMNG "CH01++
    FROM EKPO
    INNER JOIN EKET ON EKPO~EBELN EQ EKET~EBELN AND
                       EKPO~EBELP EQ EKET~EBELP AND
                       EKET~ETENR EQ '0001'
    INTO TABLE GT_RESULT
    WHERE EKPO~EBELN IN S_EBELN
      AND EKPO~ELIKZ EQ SPACE
      AND EKPO~LOEKZ EQ SPACE.
*BOI CH01
  IF SY-SUBRC = 0.
    LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
      <LFS_RESULT>-REMAIN = <LFS_RESULT>-MENGE - <LFS_RESULT>-WEMNG.
    ENDLOOP.
  ENDIF.
*EOI CH01


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
  LS_FCAT-REF_TABNAME = 'EKET'.
  LS_FCAT-FIELDNAME   = 'EBELN'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKET'.
  LS_FCAT-FIELDNAME   = 'EBELP'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKPO'.
  LS_FCAT-FIELDNAME   = 'MATNR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
*  ls_fcat-ref_tabname = 'EKPO'.    "CH01--
*  ls_fcat-fieldname   = 'MENGE'.   "CH01--
  LS_FCAT-SELTEXT_S   = 'Quantity'. "CH01++
  LS_FCAT-FIELDNAME   = 'REMAIN'.   "CH01++
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKPO'.
  LS_FCAT-FIELDNAME   = 'MEINS'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'EKET'.
  LS_FCAT-FIELDNAME   = 'EINDT'.
  LS_FCAT-EDIT        = 'X'.
  APPEND LS_FCAT TO GT_FCAT.

*  CLEAR ls_fcat.
*  ls_fcat-fieldname   = 'NAME1'.
*  ls_fcat-seltext_s   = 'Vendor Name'.
*  ls_fcat-seltext_m   = 'Vendor Name'.
*  ls_fcat-seltext_l   = 'Vendor Name'.
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
    WHEN 'ALL'.
      PERFORM F_CHECK_BOX USING 'X'.
    WHEN 'SAL'.
      PERFORM F_CHECK_BOX USING SPACE.
    WHEN 'SAVE'.
      PERFORM F_SAVE_DATA.
    WHEN 'SPLITPO'.
      PERFORM F_CALL_SPLIT_PO.
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

  DATA: I_EBELN    TYPE EKKO-EBELN.

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
    I_EBELN = GS_RESULT-EBELN.
    CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
      EXPORTING
        I_EBELN              = I_EBELN
      EXCEPTIONS
        NOT_FOUND            = 1
        NO_AUTHORITY         = 2
        INVALID_CALL         = 3
        PREVIEW_NOT_POSSIBLE = 4
        OTHERS               = 5.
    IF SY-SUBRC <> 0.

    ENDIF.
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
*&      Form  F_CHECK_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0645   text
*----------------------------------------------------------------------*
FORM F_CHECK_BOX  USING LV_CHECK.
  GS_RESULT-CHECK = LV_CHECK.
  MODIFY GT_RESULT FROM GS_RESULT TRANSPORTING CHECK
                                         WHERE CHECK NE GS_RESULT-CHECK.

ENDFORM.                    " F_CHECK_BOX
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAVE_DATA.

  DATA : POSCHEDULE  TYPE TABLE OF BAPIMEPOSCHEDULE,
         POSCHEDULEX TYPE TABLE OF BAPIMEPOSCHEDULX.

  DATA : LS_POSCHEDULE  LIKE LINE OF POSCHEDULE,
         LS_POSCHEDULEX LIKE LINE OF POSCHEDULEX.

  DATA : EXPHEADER         TYPE BAPIMEPOHEADER,
         EXPPOEXPIMPHEADER TYPE BAPIEIKP.

  DATA : PURCHASEORDER TYPE BAPIMEPOHEADER-PO_NUMBER.

  DATA : RETURN    TYPE TABLE OF BAPIRET2,
         LS_RETURN TYPE BAPIRET2.

  DATA : LS_RESULT LIKE GS_RESULT.

  DATA : LT_RESULT LIKE GT_RESULT.

  DATA : LV_TABIX TYPE SY-TABIX.

  CONSTANTS : BEGIN OF LC_CON,
                COM TYPE C LENGTH 3 VALUE 'COM',
                I   TYPE C LENGTH 1 VALUE 'I',
                EQ  TYPE C LENGTH 2 VALUE 'EQ',
              END OF LC_CON.

  RANGES : LR_EBELN FOR EKKO-EBELN.

  SORT GT_RESULT BY EBELN EBELP.

  LT_RESULT[] = GT_RESULT[].

  LOOP AT LT_RESULT INTO LS_RESULT." WHERE eindt NE delib.
    LV_TABIX = SY-TABIX.
    IF LS_RESULT-EINDT EQ LS_RESULT-DELIB.
      DELETE LT_RESULT INDEX LV_TABIX.
    ENDIF.
    CLEAR : LS_RESULT.
  ENDLOOP.

  IF LT_RESULT[] IS NOT INITIAL.
    LOOP AT LT_RESULT INTO LS_RESULT.
      MOVE-CORRESPONDING LS_RESULT TO GS_RESULT.
*    PERFORM f_unreleased_po.
      PURCHASEORDER = GS_RESULT-EBELN.

      LS_POSCHEDULE-PO_ITEM        = GS_RESULT-EBELP.
      LS_POSCHEDULE-SCHED_LINE     = GS_RESULT-ETENR.
      LS_POSCHEDULE-DELIVERY_DATE  = GS_RESULT-EINDT.
      APPEND LS_POSCHEDULE TO POSCHEDULE.

      LS_POSCHEDULEX-PO_ITEM        = LS_POSCHEDULE-PO_ITEM.
      LS_POSCHEDULEX-SCHED_LINE     = LS_POSCHEDULE-SCHED_LINE.
      LS_POSCHEDULEX-DELIVERY_DATE  = 'X'.
      APPEND LS_POSCHEDULEX TO POSCHEDULEX.

      AT END OF EBELN.
        LR_EBELN[] =  VALUE #( ( SIGN  = LC_CON-I OPTION = LC_CON-EQ LOW = PURCHASEORDER ) ).

        SUBMIT ZSDSMMR0020 USING SELECTION-SCREEN 1000
                            WITH S_EBELN IN LR_EBELN[]
                            WITH P_NOALV EQ ABAP_TRUE
                            AND RETURN.
        PERFORM F_COMMIT.

        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
            PURCHASEORDER     = PURCHASEORDER
*           POHEADER          = POHEADER
*           POHEADERX         = POHEADERX
          IMPORTING
            EXPHEADER         = EXPHEADER
            EXPPOEXPIMPHEADER = EXPPOEXPIMPHEADER
          TABLES
            RETURN            = RETURN
            POSCHEDULE        = POSCHEDULE
            POSCHEDULEX       = POSCHEDULEX.
      ENDAT.

      READ TABLE RETURN INTO LS_RETURN
      WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0.
        MESSAGE S001 WITH LS_RETURN-MESSAGE.
      ELSE.
        PERFORM F_COMMIT.
        PERFORM F_UPDATE_STATUS.
        MESSAGE S001 WITH TEXT-999.
      ENDIF.

      CLEAR : GS_RESULT,LS_RESULT.
    ENDLOOP.
  ELSE.
    MESSAGE S001 WITH TEXT-998 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " F_SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_UNRELEASED_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UNRELEASED_PO.

  RANGES : S_EBELN FOR EKKO-EBELN.

  CLEAR : S_EBELN,S_EBELN[].
  S_EBELN-SIGN   = 'I'.
  S_EBELN-OPTION = 'EQ'.
  S_EBELN-LOW    = GS_RESULT-EBELN.
  APPEND S_EBELN.

  SUBMIT ZP_UNLEASE_PO USING SELECTION-SCREEN 1000
                       WITH S_EBELN IN S_EBELN
                       AND RETURN.

ENDFORM.                    " F_UNRELEASED_PO
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
*   IMPORTING
*     RETURN        =
    EXCEPTIONS
      ERROR_MESSAGE = 1
      OTHERS        = 2.

ENDFORM.                    " F_COMMIT
*&---------------------------------------------------------------------*
*&      Form  F_ADDITIONAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ADDITIONAL_DATA .

  FIELD-SYMBOLS <LFS_RESULT> LIKE GS_RESULT.

*  SELECT EBELN
*         STATUS
*    FROM ZTMM_STATUS_PO
*    INTO TABLE GT_ZTMM_STATUS_PO
*    FOR ALL ENTRIES IN GT_RESULT
*    WHERE EBELN EQ GT_RESULT-EBELN.

  LOOP AT GT_RESULT ASSIGNING <LFS_RESULT>.
    <LFS_RESULT>-DELIB = <LFS_RESULT>-EINDT.
  ENDLOOP.

ENDFORM.                    " F_ADDITIONAL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UPDATE_STATUS .

*  READ TABLE GT_ZTMM_STATUS_PO INTO GS_ZTMM_STATUS_PO
*  WITH KEY EBELN = GS_RESULT-EBELN.
*  IF SY-SUBRC EQ 0.
*    UPDATE ZTMM_STATUS_PO SET STATUS = GS_ZTMM_STATUS_PO-STATUS
*                        WHERE EBELN EQ GS_ZTMM_STATUS_PO-EBELN.
*  ENDIF.
*  PERFORM F_COMMIT.
ENDFORM.                    " F_UPDATE_STATUS
*&---------------------------------------------------------------------*
*&      Form  F_CALL_SPLIT_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_SPLIT_PO.

  CALL TRANSACTION 'ZSDSMM038'.

ENDFORM.                    " F_CALL_SPLIT_PO
