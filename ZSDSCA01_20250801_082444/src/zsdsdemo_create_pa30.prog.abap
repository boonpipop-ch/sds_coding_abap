*&---------------------------------------------------------------------*
*& Report ZSDSDEMO_CREATE_PA30
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSDEMO_CREATE_PA30 MESSAGE-ID ZSDSCA01.
TYPE-POOLS : TRUXS,SLIS,ICON.
*&---------------------------------------------------------------------*
*  DECLARATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  TABLE
*&---------------------------------------------------------------------*
TABLES : LFA1,ADRC,LFB1,PA0001.
*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF GY_RESULT,
          LIFNR      TYPE LFA1-LIFNR,
          NAME1      TYPE ADRC-NAME1,
          CITY1      TYPE ADRC-CITY1,
          CITY2      TYPE ADRC-CITY2,
          PSTLZ      TYPE LFA1-PSTLZ,
          STREET     TYPE ADRC-STREET,
          STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
          STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
          STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
          LOCATION   TYPE ADRC-LOCATION,
          SPERR      TYPE LFA1-SPERR,
          STCD1      TYPE LFA1-STCD1,
          STCD2      TYPE LFA1-STCD2,
          STCD3      TYPE LFA1-STCD3,
          KVERM      TYPE LFB1-KVERM,
          KTOKK      TYPE LFA1-KTOKK,
          TXT30      TYPE T077Y-TXT30,
          CHECK      TYPE C,
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
  SELECT-OPTIONS : S_PERNR FOR PA0001-PERNR NO INTERVALS OBLIGATORY.
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
*  IF GT_RESULT[] IS NOT INITIAL.
*    PERFORM F_SHOW_REPORT.
*  ELSE.
  MESSAGE S000 WITH 'Data has been saved'.
*  ENDIF.
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

  LOOP AT S_PERNR.

    PERFORM BDC_DYNPRO   USING  'SAPMP50A'   '1000'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR' 'RP50G-PERNR'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE' '=INS'.
    PERFORM BDC_FIELD    USING  'RP50G-PERNR' S_PERNR-LOW.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR' '/1PAPAXX/HDR_60060A                     0100SUBSCR_HEADER'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR' 'SAPMP50A                                0320SUBSCR_ITMENU'.
    PERFORM BDC_FIELD    USING  'RP50G-SELEC(01)' 'X'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR' 'SAPMP50A                                0330SUBSCR_TIME'.
    PERFORM BDC_FIELD    USING  'RP50G-TIMR6' 'X'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR' 'SAPMP50A                                0350SUBSCR_ITKEYS'.

    PERFORM BDC_DYNPRO   USING  'MP000000'   '2000'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR' 'PSPAR-PLANS'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE' '/00'.
    PERFORM BDC_FIELD    USING  'PSPAR-PERNR' S_PERNR-LOW.
    PERFORM BDC_FIELD    USING  'P0000-BEGDA' '10.07.2024'.
    PERFORM BDC_FIELD    USING  'P0000-ENDDA' '31.12.9999'.
    PERFORM BDC_FIELD    USING  'P0000-MASSN' '01'.
    PERFORM BDC_FIELD    USING  'PSPAR-PLANS' '99999999'.
    PERFORM BDC_FIELD    USING  'PSPAR-WERKS' 'TH01'.
    PERFORM BDC_FIELD    USING  'PSPAR-PERSG' '1'.
    PERFORM BDC_FIELD    USING  'PSPAR-PERSK' '01'.

    PERFORM BDC_DYNPRO   USING  'MP000000'   '2000'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'   'PSPAR-PERNR'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'   '=UPD'.
    PERFORM BDC_FIELD    USING  'PSPAR-PERNR'  S_PERNR-LOW.
    PERFORM BDC_FIELD    USING  'P0000-BEGDA'  '10.07.2024'.
    PERFORM BDC_FIELD    USING  'P0000-ENDDA'  '31.12.9999'.
    PERFORM BDC_FIELD    USING  'P0000-MASSN'  '01'.
    PERFORM BDC_FIELD    USING  'PSPAR-PLANS'  '99999999'.
    PERFORM BDC_FIELD    USING  'PSPAR-WERKS'  'TH01'.
    PERFORM BDC_FIELD    USING  'PSPAR-PERSG'  '1'.
    PERFORM BDC_FIELD    USING  'PSPAR-PERSK'  '01'.

    PERFORM BDC_DYNPRO   USING  'MP000200'    '2026'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'  'P0002-GBDAT'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'  '=UPD'.
    PERFORM BDC_FIELD    USING  'P0002-BEGDA' '10.07.2024'.
    PERFORM BDC_FIELD    USING  'P0002-ENDDA' '31.12.9999'.
    PERFORM BDC_FIELD    USING  'Q0002-ANREX' 'Mr'.
    PERFORM BDC_FIELD    USING  'P0002-VORNA' 'First'.
    PERFORM BDC_FIELD    USING  'P0002-NACHN' 'Last'.
    PERFORM BDC_FIELD    USING  'P0002-GBDAT' '01011999'.
    PERFORM BDC_FIELD    USING  'Q0002-GESC2' 'X'.
    PERFORM BDC_FIELD    USING  'P0002-NATIO' 'TH'.
    PERFORM BDC_FIELD    USING  'P0002-SPRSL' 'EN'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR ' 'SAPMP50A                                0090SUBSCREEN_T582C'.

    PERFORM BDC_DYNPRO   USING  'MP000100'    '2000'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'  'P0001-ORGEH'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'  '=UPD'.
    PERFORM BDC_FIELD    USING  'P0001-BEGDA' '10.07.2024'.
    PERFORM BDC_FIELD    USING  'P0001-ENDDA' '31.12.9999'.
    PERFORM BDC_FIELD    USING  'P0001-BTRTL' '01'.
    PERFORM BDC_FIELD    USING  'P0001-KOSTL' '10999907'.
    PERFORM BDC_FIELD    USING  'P0001-GSBER' '0001'.
    PERFORM BDC_FIELD    USING  'P0001-ABKRS' '01'.
    PERFORM BDC_FIELD    USING  'P0001-PLANS' '11000000'.
    PERFORM BDC_FIELD    USING  'P0001-STELL' '00000001'.
    PERFORM BDC_FIELD    USING  'P0001-ORGEH' '00000001'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'  'MP000100                                0100SUB0001'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'  'SAPMP50A                                0090SUBSCREEN_T582C'.

    PERFORM BDC_DYNPRO   USING  'MP000600'    '2026'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'  'P0006-BEGDA'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'  '=UPD'.
    PERFORM BDC_FIELD    USING  'P0006-BEGDA' '10.07.2024'.
    PERFORM BDC_FIELD    USING  'P0006-ENDDA' '31.12.9999'.
    PERFORM BDC_FIELD    USING  'P0006-LAND1' 'TH'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'  'MP336500                                0100SUB0006'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'  'SAPMP50A                                0090SUBSCREEN_T582C'.

    PERFORM BDC_DYNPRO   USING  'MP000700'    '2000'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'  '/ENXT'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'  'P0007-BEGDA'.

    PERFORM BDC_DYNPRO   USING  'MP000800'    '2000'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'  'P0008-TRFST'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'  '=UPD'.
    PERFORM BDC_FIELD    USING  'P0008-BEGDA' '10.07.2024'.
    PERFORM BDC_FIELD    USING  'P0008-ENDDA' '31.12.9999'.
    PERFORM BDC_FIELD    USING  'P0008-TRFAR' '01'.
    PERFORM BDC_FIELD    USING  'P0008-TRFGB' '01'.
    PERFORM BDC_FIELD    USING  'P0008-TRFGR' 'GR01'.
    PERFORM BDC_FIELD    USING  'P0008-TRFST' '01'.
    PERFORM BDC_FIELD    USING  'P0008-ANCUR' 'THB'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'  'MP000800                                0300SUBSCREEN_TC0008'.
    PERFORM BDC_FIELD    USING  'Q0008-IBBEG' '12.07.2024'.
    PERFORM BDC_FIELD    USING  'P0008-WAERS' 'THB'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'  'MP000800                                0100SUB0008'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'  'SAPMP50A                                0090SUBSCREEN_T582C'.

    PERFORM BDC_DYNPRO   USING  'MP000900'         '2000'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'       'P0009-BANKN'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'       '=UPD'.
    PERFORM BDC_FIELD    USING  'P0009-BEGDA'      '10.07.2024'.
    PERFORM BDC_FIELD    USING  'P0009-ENDDA'      '31.12.9999'.
    PERFORM BDC_FIELD    USING  'P0009-BNKSA'      '0'.
    PERFORM BDC_FIELD    USING  'Q0009-EMFTX'      'Mr First Last'.
    PERFORM BDC_FIELD    USING  'Q0009-ADRS_BANKS' 'TH'.
    PERFORM BDC_FIELD    USING  'P0009-BANKS'      'TH'.
    PERFORM BDC_FIELD    USING  'P0009-BANKL'      '0020130'.
    PERFORM BDC_FIELD    USING  'P0009-BANKN'      '342424234234'.
    PERFORM BDC_FIELD    USING  'P0009-ZLSCH'      'T'.
    PERFORM BDC_FIELD    USING  'P0009-WAERS'      'THB'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'       'MP000900                                0100SUB0009'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'       'SAPMP50A                                0090SUBSCREEN_T582C'.

    PERFORM BDC_DYNPRO   USING  'SAPLRPBS'         '1000'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'       'TABLE_CTRL_ITAB_WP-MSG_TYPE(01)'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'       '=ON'.

    PERFORM BDC_DYNPRO   USING  'MP200000'    '2251'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'  'P2006-KTART(01)'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'  '=UPD'.
    PERFORM BDC_FIELD    USING  'RP50M-BEGDA' '10.07.2024'.
    PERFORM BDC_FIELD    USING  'RP50M-ENDDA' '10.07.2024'.

    PERFORM BDC_DYNPRO   USING  'MP018500'    '2426'.
    PERFORM BDC_FIELD    USING  'BDC_CURSOR'  'P0185-BEGDA'.
    PERFORM BDC_FIELD    USING  'BDC_OKCODE'  '=UPD'.
    PERFORM BDC_FIELD    USING  'P0185-BEGDA' '10.07.2024'.
    PERFORM BDC_FIELD    USING  'P0185-ENDDA' '31.12.9999'.
    PERFORM BDC_FIELD    USING  'BDC_SUBSCR'  'SAPMP50A                                0090SUBSCREEN_T582C'.

    PERFORM BDC_TRANSACTION USING 'PA30'.
  ENDLOOP.

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

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'LIFNR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'NAME1'.
  LS_FCAT-SELTEXT_S   = 'Vendor Name'.
  LS_FCAT-SELTEXT_M   = 'Vendor Name'.
  LS_FCAT-SELTEXT_L   = 'Vendor Name'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ADRC'.
  LS_FCAT-FIELDNAME   = 'CITY1'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ADRC'.
  LS_FCAT-FIELDNAME   = 'CITY2'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'PSTLZ'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ADRC'.
  LS_FCAT-FIELDNAME   = 'STREET'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ADRC'.
  LS_FCAT-FIELDNAME   = 'STR_SUPPL1'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ADRC'.
  LS_FCAT-FIELDNAME   = 'STR_SUPPL2'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ADRC'.
  LS_FCAT-FIELDNAME   = 'STR_SUPPL3'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'ADRC'.
  LS_FCAT-FIELDNAME   = 'LOCATION'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'SPERR'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'STCD1'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'STCD2'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'STCD3'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'KVERM'.
  LS_FCAT-SELTEXT_S   = 'Account Memo'.
  LS_FCAT-SELTEXT_M   = 'Account Memo'.
  LS_FCAT-SELTEXT_L   = 'Account Memo'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-REF_TABNAME = 'LFA1'.
  LS_FCAT-FIELDNAME   = 'KTOKK'.
  APPEND LS_FCAT TO GT_FCAT.

  CLEAR LS_FCAT.
  LS_FCAT-FIELDNAME   = 'TXT30'.
  LS_FCAT-SELTEXT_S   = 'Acc.G. Des.'.
  LS_FCAT-SELTEXT_M   = 'Acc.Group Des.'.
  LS_FCAT-SELTEXT_L   = 'Account Group Description'.
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
  CLEAR GS_SORT.
  GS_SORT-FIELDNAME = 'LIFNR'.
  GS_SORT-SPOS = '1'.
  GS_SORT-UP = 'X'.
*  gs_sort-subtot = 'X'.
  APPEND GS_SORT TO GT_SORT.
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

  LV_OPT-DISMODE  = 'E'."'E'."'A'
  LV_OPT-UPDMODE  = 'A'.
*  LV_OPT-NOBINPT  = 'X'.
  "lv_opt-cattmode = 'A'.
  LV_OPT-DEFSIZE  = 'X'.

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
