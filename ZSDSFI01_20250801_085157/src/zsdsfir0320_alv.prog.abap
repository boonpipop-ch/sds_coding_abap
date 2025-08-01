*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0320_ALV
*  Creation Date      : 05.06.2024
*  Author             : Thanapong C. (Eviden)
*  Add-on ID          : FSCMR013
*  Description        : This is include program which contains logic for
*                       displaying report as ALV Grid.
*  Purpose            : Include program for ALV Processing
*  Purpose            :
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*----------------------------------------------------------------------*
*-- Class
*----------------------------------------------------------------------*
CLASS:
  LCL_EVENT_HANDLER DEFINITION DEFERRED.  "for event handling

CLASS:
  CL_GUI_COLUMN_TREE DEFINITION LOAD,
  CL_GUI_CFW DEFINITION LOAD.

DATA:
  GREF_TREE    TYPE REF TO CL_GUI_ALV_TREE ##NEEDED,
  GREF_TOOLBAR TYPE REF TO CL_GUI_TOOLBAR  ##NEEDED.


*----------------------------------------------------------------------*
*-- Tables
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Type definitions
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Constants
*----------------------------------------------------------------------*
CONSTANTS:
  GC_SAVE_ALL TYPE CHAR1         VALUE 'A',                 "#EC NEEDED
  GC_SAVE     TYPE SY-UCOMM      VALUE 'SAVE_1'.            "#EC NEEDED

CONSTANTS:
  BEGIN OF GC_FNAME,
    PARTNER             TYPE LVC_FNAME VALUE 'PARTNER',
    PARTNER_NAME        TYPE LVC_FNAME VALUE 'PARTNER_NAME',
    CREDIT_SGMNT        TYPE LVC_FNAME VALUE 'CREDIT_SGMNT',
    XBLOCKED            TYPE LVC_FNAME VALUE 'XBLOCKED',
    PSPID               TYPE LVC_FNAME VALUE 'PSPID',
    CREDIT_LIMIT        TYPE LVC_FNAME VALUE 'CREDIT_LIMIT',
    CREDIT_EXPOSURE     TYPE LVC_FNAME VALUE 'CREDIT_EXPOSURE',
    UTILIZATION_PERCENT TYPE LVC_FNAME VALUE 'UTILIZATION_PERCENT',
    RISK_CLASS_TXT      TYPE LVC_FNAME VALUE 'RISK_CLASS_TXT',
    DOC_NO              TYPE LVC_FNAME VALUE 'DOC_NO',
    DOC_POSID           TYPE LVC_FNAME VALUE 'DOC_POSID',
    DOC_DATE            TYPE LVC_FNAME VALUE 'DOC_DATE',
    REF_DOC             TYPE LVC_FNAME VALUE 'REF_DOC',
    PAYMENT_DATE        TYPE LVC_FNAME VALUE 'PAYMENT_DATE',
    ZTERM               TYPE LVC_FNAME VALUE 'ZTERM DZTERM',
    DAY1                TYPE LVC_FNAME VALUE 'DAY1',
    NET_DUEDATE         TYPE LVC_FNAME VALUE 'NET_DUEDATE',
    AMOUNT              TYPE LVC_FNAME VALUE 'AMOUNT',
    GJAHR               TYPE LVC_FNAME VALUE 'GJAHR',
    BUKRS               TYPE LVC_FNAME VALUE 'BUKRS',
    DOC_TYPE            TYPE LVC_FNAME VALUE 'DOC_TYPE',
    PSPNR               TYPE LVC_FNAME VALUE 'PSPNR',
    TREE_DESC           TYPE LVC_FNAME VALUE 'TREE_DESC',
    REMAIN_CREDIT_LIMIT TYPE LVC_FNAME VALUE 'REMAIN_CREDIT_LIMIT',
    VKORG_TX            TYPE LVC_FNAME VALUE 'VKORG_TX',
    VTWEG_TX            TYPE LVC_FNAME VALUE 'VTWEG_TX',
    SPART_TX            TYPE LVC_FNAME VALUE 'SPART_TX',
    VKGRP_TX            TYPE LVC_FNAME VALUE 'VKGRP_TX',
    VKBUR_TX            TYPE LVC_FNAME VALUE 'VKBUR_TX',
    SALES_NAME          TYPE LVC_FNAME VALUE 'SALES_NAME  ',
    ACCT_CLERK_TX       TYPE LVC_FNAME VALUE 'ACCT_CLERK_TX  ',
  END OF GC_FNAME.

CONSTANTS:
  BEGIN OF GC_FNAME_XLS,
    PARTNER             TYPE LVC_FNAME VALUE 'PARTNER',
    PARTNER_NAME        TYPE LVC_FNAME VALUE 'PARTNER_NAME',
    ACCT_CLERK_TX       TYPE LVC_FNAME VALUE 'ACCT_CLERK_TX',
    PROJECT             TYPE LVC_FNAME VALUE 'PROJECT',
    PROJECT_TX          TYPE LVC_FNAME VALUE 'PROJECT_TX',
    CREDIT_LIMIT        TYPE LVC_FNAME VALUE 'CREDIT_LIMIT',
    CREDIT_EXPOSURE     TYPE LVC_FNAME VALUE 'CREDIT_EXPOSURE',
    UTILIZATION_PERCENT TYPE LVC_FNAME VALUE 'UTILIZATION_PERCENT',
    REMAIN_CREDIT_LIMIT TYPE LVC_FNAME VALUE 'REMAIN_CREDIT_LIMIT',
    DOC_TYPE_TX         TYPE LVC_FNAME VALUE 'DOC_TYPE_TX',
    DOC_NO              TYPE LVC_FNAME VALUE 'DOC_NO',
    VKORG_TX            TYPE LVC_FNAME VALUE 'VKORG_TX',
    VTWEG_TX            TYPE LVC_FNAME VALUE 'VTWEG_TX',
    SPART_TX            TYPE LVC_FNAME VALUE 'SPART_TX',
    VKGRP_TX            TYPE LVC_FNAME VALUE 'VKGRP_TX',
    VKBUR_TX            TYPE LVC_FNAME VALUE 'VKBUR_TX',
    SALES_NAME          TYPE LVC_FNAME VALUE 'SALES_NAME',
    DOC_DATE            TYPE LVC_FNAME VALUE 'DOC_DATE',
    REF_DOC             TYPE LVC_FNAME VALUE 'REF_DOC',
    ZTERM               TYPE LVC_FNAME VALUE 'ZTERM',
    DAY1                TYPE LVC_FNAME VALUE 'DAY1',
    PAYMENT_DATE        TYPE LVC_FNAME VALUE 'PAYMENT_DATE',
    NET_DUEDATE         TYPE LVC_FNAME VALUE 'NET_DUEDATE',
    AMOUNT              TYPE LVC_FNAME VALUE 'AMOUNT',
    RISK_CLASS_TXT      TYPE LVC_FNAME VALUE 'RISK_CLASS_TXT',
    CREDIT_SGMNT        TYPE LVC_FNAME VALUE 'CREDIT_SGMNT',
    XBLOCKED            TYPE LVC_FNAME VALUE 'XBLOCKED',
  END OF GC_FNAME_XLS.

*----------------------------------------------------------------------*
*-- Internal Tables
*----------------------------------------------------------------------*
DATA:
  GT_FIELDCAT     TYPE LVC_T_FCAT,                          "#EC NEEDED
  GT_FIELDCAT_XLS TYPE LVC_T_FCAT,                          "#EC NEEDED
  GT_SORT         TYPE LVC_T_SORT,                          "#EC NEEDED
  GT_TOOL_EXC     TYPE UI_FUNCTIONS,                        "#EC NEEDED
  GT_EXCL         TYPE STANDARD TABLE OF SY-UCOMM,          "#EC NEEDED
  GT_GROUP_KEY    TYPE LVC_T_NKEY,                          "#EC NEEDED

  GT_ALL_NODES    TYPE LVC_T_NKEY.                          "#EC NEEDED

*----------------------------------------------------------------------*
*-- Work Areas
*----------------------------------------------------------------------*
DATA:
  GS_VARIANT TYPE DISVARIANT,                               "#EC NEEDED
  GS_LAYOUT  TYPE LVC_S_LAYO.                               "#EC NEEDED

*----------------------------------------------------------------------*
*-- Ranges
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Variable declarations
*----------------------------------------------------------------------*
DATA:
  GF_OK_CODE          TYPE SY-UCOMM,                        "#EC NEEDED
  GF_SAVE_OK          TYPE SY-UCOMM,                        "#EC NEEDED
  GREF_EVENT_RECEIVER TYPE REF TO LCL_EVENT_HANDLER.        "#EC NEEDED

*----------------------------------------------------------------------*
*-- Field-Symbols
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- GenC Constants
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*-- Macros
*----------------------------------------------------------------------*

************************************************************************
*-----------------------------------------------------------------------
* CLASS DEFINITION
*-----------------------------------------------------------------------
*Event Handler
CLASS LCL_EVENT_HANDLER DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:

      ON_LINK_CLICK     FOR EVENT LINK_CLICK OF CL_GUI_ALV_TREE
        IMPORTING FIELDNAME NODE_KEY,

      ON_FUNCTION_SELECTED FOR EVENT FUNCTION_SELECTED OF CL_GUI_TOOLBAR
        IMPORTING FCODE.


ENDCLASS.                    "lcl_event_handler DEFINITION

*-----------------------------------------------------------------------
* CLASS IMPLEMENTATION
*-----------------------------------------------------------------------
* Event Handler
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD ON_LINK_CLICK.
    PERFORM F_LINK_CLICK IN PROGRAM (SY-CPROG)
      USING FIELDNAME NODE_KEY
      IF FOUND.
  ENDMETHOD.                    "on_hotspot_click

  METHOD ON_FUNCTION_SELECTED.
    PERFORM F_ALV_FUNCTION IN PROGRAM (SY-CPROG)
      USING FCODE
      IF FOUND.
  ENDMETHOD.
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*----------------------------------------------------------------------*
*-- Subroutines
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       Form  f_prepare_fieldcat_o
*----------------------------------------------------------------------*
*       Prepare fieldcat method ALV used for online
*----------------------------------------------------------------------*
*  -->  PFD_STRUCTURE  Structure name of the list table
*  <--  PIT_FIELDCAT   Field catalog
*----------------------------------------------------------------------*
FORM F_PREPARE_FIELDCAT_O  USING  UF_STRUCTURE TYPE DD02L-TABNAME
                         CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT ##CALLED.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = UF_STRUCTURE
    CHANGING
      CT_FIELDCAT            = CT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " f_prepare_fieldcat_o

*----------------------------------------------------------------------*
*       Module  STATUS_ALV_1  OUTPUT
*----------------------------------------------------------------------*
*       Set GUI Status & GUI Title
*----------------------------------------------------------------------*
MODULE STATUS_ALV OUTPUT.

  SET PF-STATUS '9000' EXCLUDING GT_EXCL ##STAT_UNDEF.
  SET TITLEBAR  '9000' ##TITL_UNDEF.

ENDMODULE.                 " STATUS_ALV  OUTPUT
*
*----------------------------------------------------------------------*
*       Module  DISPLAY_ALV  OUTPUT
*----------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.

* Display ALV Tree
  IF GREF_TREE IS INITIAL.
    PERFORM F_INIT_TREE.
  ENDIF.
  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT

*----------------------------------------------------------------------*
*       Module  EXIT_COMMANDS_1 INPUT
*----------------------------------------------------------------------*
*       Cancel transaction and leave the program
*----------------------------------------------------------------------*
MODULE EXIT_COMMANDS INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " EXIT_COMMANDS_1  INPUT
*
*
*&---------------------------------------------------------------------*
*& Form f_init_tree
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_INIT_TREE .

  DATA:
    LF_HIERARCHY_HEADER    TYPE TREEV_HHDR,
    LF_TREE_CONTAINER_NAME TYPE CHAR30,
    LREF_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
    LT_TOOLBAR_EXCLUDING   TYPE UI_FUNCTIONS.


* Create field catalog
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT.

* Create container for alv-tree
  LF_TREE_CONTAINER_NAME = 'CTL_ALV'.

  IF SY-BATCH IS INITIAL.
    CREATE OBJECT LREF_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = LF_TREE_CONTAINER_NAME
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.
    IF SY-SUBRC <> 0.
      MESSAGE X208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.
  ENDIF.

* Create tree control
  CREATE OBJECT GREF_TREE
    EXPORTING
      PARENT                      = LREF_CUSTOM_CONTAINER
      NODE_SELECTION_MODE         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_MULTIPLE
      ITEM_SELECTION              = GC_TRUE
      NO_HTML_HEADER              = GC_TRUE
      NO_TOOLBAR                  = ''
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      ILLEGAL_NODE_SELECTION_MODE = 5
      FAILED                      = 6
      ILLEGAL_COLUMN_NAME         = 7.
  IF SY-SUBRC <> 0.
    MESSAGE X208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

* Repid for saving variants
  DATA: LS_VARIANT TYPE DISVARIANT.
  LS_VARIANT-REPORT = SY-REPID.

* create Hierarchy-header
  PERFORM F_CREATE_HIERARCHY_HEADER CHANGING LF_HIERARCHY_HEADER.

* Create toolbar excluding
  PERFORM F_CREATE_TOOLBAR_EXCLUDING CHANGING LT_TOOLBAR_EXCLUDING.

* Create empty tree-control
  CALL METHOD GREF_TREE->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_HIERARCHY_HEADER  = LF_HIERARCHY_HEADER
*     it_list_commentary   = lt_list_commentary
*     i_logo               = l_logo
      IT_TOOLBAR_EXCLUDING = LT_TOOLBAR_EXCLUDING
      I_BACKGROUND_ID      = 'ALV_BACKGROUND'
      I_SAVE               = 'A'
      IS_VARIANT           = LS_VARIANT
    CHANGING
      IT_OUTTAB            = GT_OUTPUT "table must be emty !!
      IT_FIELDCATALOG      = GT_FIELDCAT.

* Create hierarchy
  PERFORM F_CREATE_HIERARCHY CHANGING GT_RESULT.

* Add own function codes to the toolbar
  PERFORM F_CHANGE_TOOLBAR.

* Register events
  PERFORM F_REGISTER_EVENTS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*       Create hierarchy-header-information
*----------------------------------------------------------------------*
FORM F_CREATE_HIERARCHY_HEADER CHANGING CF_HIERARCHY_HEADER TYPE TREEV_HHDR.

  CONSTANTS:
   LC_WIDTH TYPE INT4 VALUE 50.

*Text-C00:  Partner/Customer&Project/Document Type/Document No
  CF_HIERARCHY_HEADER-HEADING   = TEXT-C00.
  CF_HIERARCHY_HEADER-TOOLTIP   = TEXT-C00.
  CF_HIERARCHY_HEADER-WIDTH     = LC_WIDTH.
  CF_HIERARCHY_HEADER-WIDTH_PIX = ''.

ENDFORM.                               " build_hierarchy_header


*&---------------------------------------------------------------------*
*& Form F_REGISTER_EVENTS
*&---------------------------------------------------------------------*
*& Register Events
*&---------------------------------------------------------------------*
FORM F_REGISTER_EVENTS .

  DATA: LT_EVENTS TYPE CNTL_SIMPLE_EVENTS.

  CREATE OBJECT GREF_EVENT_RECEIVER.
  SET HANDLER:
    GREF_EVENT_RECEIVER->ON_LINK_CLICK        FOR GREF_TREE,
    GREF_EVENT_RECEIVER->ON_FUNCTION_SELECTED FOR GREF_TOOLBAR.

  APPEND VALUE CNTL_SIMPLE_EVENT( EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK )
            TO LT_EVENTS.

  GREF_TREE->SET_REGISTERED_EVENTS(
    EXPORTING
      EVENTS                    = LT_EVENTS                 " Event Table
    EXCEPTIONS
      CNTL_ERROR                = 1                " cntl_error
      CNTL_SYSTEM_ERROR         = 2                " cntl_system_error
      ILLEGAL_EVENT_COMBINATION = 3                " ILLEGAL_EVENT_COMBINATION
      OTHERS                    = 4
  ).
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_ALV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_ALV INPUT.

* to react on oi_custom_events:
  CALL METHOD CL_GUI_CFW=>DISPATCH.

  GF_SAVE_OK = GF_OK_CODE.
  CLEAR GF_OK_CODE.

  CASE GF_SAVE_OK.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'END'.
      PERFORM F_EXIT_PROGRAM.
    WHEN OTHERS.
*     Call specific user command handler if it exists
      PERFORM F_USER_COMMAND IN PROGRAM (SY-CPROG) USING GF_SAVE_OK
        IF FOUND.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*& Change toolbar
*&---------------------------------------------------------------------*
FORM F_CHANGE_TOOLBAR .

  CALL METHOD GREF_TREE->GET_TOOLBAR_OBJECT
    IMPORTING
      ER_TOOLBAR = GREF_TOOLBAR.

  IF GREF_TOOLBAR IS INITIAL. "could happen if you do not use the standard toolbar
    RETURN.
  ENDIF.

* add seperator to toolbar
  CALL METHOD GREF_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = ''
      ICON      = ''
      BUTN_TYPE = CNTB_BTYPE_SEP.

* add Select none Button to toolbar
  CALL METHOD GREF_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = GC_FCODE-DOWNLOAD "SEL_NONE
      ICON      = '@49@'
      BUTN_TYPE = CNTB_BTYPE_BUTTON
      TEXT      = ''
      QUICKINFO = TEXT-F01.   "Text-F01:  Download

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CREATE_TOOLBAR_EXCLUDING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_CREATE_TOOLBAR_EXCLUDING  CHANGING CT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS.

  APPEND CL_GUI_ALV_TREE=>MC_FC_CALCULATE TO CT_TOOLBAR_EXCLUDING.

ENDFORM.
