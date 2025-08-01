*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0220_TOP
*  Creation Date      : 03.05.2024
*  Author             : Apichat Ch. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program for Global data
*                       and selection screen
*  Purpose            : Include program for global data type and data object
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
* TABLES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_group.
    INCLUDE TYPE zsdsfis077.
TYPES:
    nodekey TYPE lvc_nkey,
  END OF gty_group,

  BEGIN OF gty_profit_center,
    prctr TYPE cepct-prctr,
    ktext TYPE cepct-ktext,
  END OF gty_profit_center,

  BEGIN OF gty_cust_branch,
    kunnr     TYPE fitha_pbupl_d-kunnr,
    j_1tpbupl TYPE fitha_pbupl_d-j_1tpbupl,
  END OF gty_cust_branch,

  tt_profit_center TYPE SORTED TABLE OF gty_profit_center WITH UNIQUE KEY prctr,
  tt_group         TYPE STANDARD TABLE OF gty_group  WITH EMPTY KEY,
  tt_cust_branch   TYPE STANDARD TABLE OF gty_cust_branch WITH EMPTY KEY,
  tt_output        TYPE STANDARD TABLE OF zsdsfis077 WITH EMPTY KEY.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  BEGIN OF gc_mode,
    test_run TYPE char1    VALUE 'X',
    prod_run TYPE char1    VALUE '',
  END OF gc_mode,

  BEGIN OF gc_status,
    success TYPE icon_d VALUE '@08@',
    warning TYPE icon_d VALUE '@09@',
    error   TYPE icon_d VALUE '@0A@',
  END OF gc_status,

  BEGIN OF gc_stype,
    header TYPE stype_pi VALUE 'K',
    item   TYPE stype_pi VALUE 'P',
  END OF gc_stype,

  BEGIN OF gc_fcode,
    sel_a    TYPE sy-ucomm VALUE 'SEL_ALL',
    sel_n    TYPE sy-ucomm VALUE 'SEL_NONE',
    download TYPE sy-ucomm VALUE 'DOWNLOAD',
  END OF gc_fcode,

  BEGIN OF gc_genc_param,
    posting_doc_type TYPE zsdsde_param_name VALUE 'POSTING_DOCUMENT_TYPE',
    doc_type         TYPE zsdsde_param_name VALUE 'DOCUMENT_TYPE',
    sp_gl            TYPE zsdsde_param_name VALUE 'SPECIAL_GL',
    posting_sp_gl    TYPE zsdsde_param_name VALUE 'POSTING_SPECIAL_GL',
    item_text        TYPE zsdsde_param_name VALUE 'ITEM_TEXT',
    hide_amount      TYPE zsdsde_param_name VALUE 'HIDE_AMOUNT',
  END OF gc_genc_param,

  BEGIN OF gc_default,
    mwskz    TYPE bseg-mwskz VALUE 'O7',
    mwskz_o0 TYPE bseg-mwskz VALUE 'O0',
  END OF gc_default,

  gc_blart_dz TYPE bkpf-blart VALUE 'DZ',
  gc_true     TYPE  char1     VALUE 'X',
  gc_tcode    TYPE  sy-tcode  VALUE 'ZSDSFI011'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  gt_profit_center TYPE tt_profit_center                    ##NEEDED,
  gt_cust_branch   TYPE tt_cust_branch                      ##NEEDED,
  gt_group         TYPE tt_group                            ##NEEDED,
  gt_open_itms     TYPE tt_output                           ##NEEDED,
  gt_output        TYPE tt_output                           ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  gv_amt  TYPE aflex09d2o13s,
  gs_bsid TYPE bsid_view.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
*DATA:
*  GF_PARAM           TYPE  CHAR10                              ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  gc_structure     TYPE  tabname  VALUE 'ZSDSFIS077'.

*CONSTANTS:
*  gc_header_height TYPE  i                VALUE 10,
*  gc_alv_height    TYPE  i                VALUE 90.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE mc_show_progress.
  IF sy-batch IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = &1 ##NUMBER_OK
        text       = &2.
  ELSE.
*   Show message step in background
    MESSAGE i000(38) WITH &2 space space space.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Customer Selection
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_bukrs   TYPE knb1-bukrs OBLIGATORY.
  SELECT-OPTIONS:
    s_kunnr   FOR  gs_bsid-kunnr,
    s_umskz   FOR  gs_bsid-umskz OBLIGATORY.
  SELECT-OPTIONS:
    s_blart   FOR  gs_bsid-blart OBLIGATORY.
*  PARAMETERS:
*    p_opdate  TYPE rfpdo-allgstid DEFAULT sy-datum OBLIGATORY.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 20.
    PARAMETERS: cb_hide    TYPE char01 AS CHECKBOX USER-COMMAND hide.
    SELECTION-SCREEN COMMENT (28) TEXT-s04 FOR FIELD cb_hide.
  SELECTION-SCREEN END OF LINE.
  SELECT-OPTIONS: s_amt FOR gv_amt NO-EXTENSION MODIF ID amt.
SELECTION-SCREEN END OF BLOCK b1.
* Text-s02: Document Selection
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  SELECT-OPTIONS:
    s_belnr   FOR  gs_bsid-belnr.
  PARAMETERS:
    p_gjahr   TYPE bkpf-gjahr OBLIGATORY.
  SELECT-OPTIONS:
    s_prctr   FOR  gs_bsid-prctr.
SELECTION-SCREEN END OF BLOCK b2.
* Text-s03: Posting Information
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s03.
  PARAMETERS:
    p_blart  TYPE bsid_view-blart OBLIGATORY,
    p_bldat  TYPE bsid_view-bldat OBLIGATORY,
    p_budat  TYPE bsid_view-budat OBLIGATORY,
    p_bupla  TYPE bsid_view-bupla OBLIGATORY,
    p_umskzc TYPE bsid_view-umskz OBLIGATORY,
    p_sgtxt  TYPE bsid_view-sgtxt OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b3.
