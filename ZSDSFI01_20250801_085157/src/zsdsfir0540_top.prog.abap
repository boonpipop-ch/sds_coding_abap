*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0540_TOP
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0540_TOP
*  Creation Date      : 11.11.2024
*  Author             : Apichat Ch(Eviden)
*  Add-on ID          : N/A
*  Description        : Program Advance Received Report
*                       RICEP ZFIAR036
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
* TABLES
*----------------------------------------------------------------------*
TABLES: zsdsfit045.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*


TYPES:
  tt_output    TYPE STANDARD TABLE OF zsdsfis144.
*  tt_action_status TYPE STANDARD TABLE OF gty_action_status.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:

  BEGIN OF gc_func,
    del         TYPE ui_func  VALUE 'DEL',
    sel_a       TYPE sy-ucomm VALUE 'SEL_ALL',
    sel_n       TYPE sy-ucomm VALUE 'SEL_NONE',
    save        TYPE ui_func  VALUE 'SAVE',

    sel_a_new   TYPE sy-ucomm VALUE 'SEL_ALL_N',
    sel_n_new   TYPE sy-ucomm VALUE 'SEL_NONE_N',
    approve     TYPE sy-ucomm VALUE 'APPROVE',
    disp_attach TYPE sy-ucomm VALUE 'DISP_ATTACH',
  END OF gc_func,

  BEGIN OF  gc_block_status,
    block   TYPE zsdsfit045-block_status VALUE 'B',
    approve TYPE zsdsfit045-block_status VALUE 'A',
  END OF  gc_block_status,

  gc_structure     TYPE  tabname  VALUE 'ZSDSFIS144',
  gc_header_height TYPE  i                VALUE 16,
  gc_alv_height    TYPE  i                VALUE 90,

  gc_true          TYPE  char1     VALUE 'X'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
  data:            gt_output    TYPE tt_output                         ##NEEDED.
*
*DATA: gt_genc   TYPE zcl_sdsca_utilities=>tt_gen_c                ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
*DATA:
*  BEGIN OF gs_default_new_entry   ##NEEDED,
*    blart            TYPE zsdsfit045-blart,
*    bupla            TYPE zsdsfit045-bupla,
*    mwsk1            TYPE zsdsfit045-mwsk1,
*    umskz            TYPE zsdsfit045-umskz,
*
*    blart_ar_invoice TYPE zsdsfit045-blart,
*    mwsk1_ar_invoice TYPE zsdsfit045-mwsk1,
*  END OF gs_default_new_entry.
*
*DATA: gs_bsid       TYPE bsid_view    ##NEEDED,
*      gs_zsdsfit029 TYPE zsdsfit029   ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
      gf_path           TYPE char255                 ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <g_list> TYPE STANDARD TABLE ##FS_ASSIGN_OK.              "#EC NEEDED

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bn1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_bukrsn TYPE zsdsfit045-bukrs OBLIGATORY DEFAULT '1000'.

  SELECT-OPTIONS:
    s_wrkdtn FOR zsdsfit045-work_date ,
    s_pernrn FOR zsdsfit045-pernr     MATCHCODE OBJECT zsdsh_pernr,
    s_trnfn  FOR zsdsfit045-tranf_no  MODIF ID trn,
    s_kunnrn FOR zsdsfit045-kunnr     MODIF ID csn,
    s_xblnrn FOR zsdsfit045-xblnr     MODIF ID csn,
    s_bldat  FOR zsdsfit045-bldat.

SELECTION-SCREEN END OF BLOCK bn1.

*----------------------------------------------------------------------*
