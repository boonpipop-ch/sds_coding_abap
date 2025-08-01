*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0430_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0430_TOP
*  Creation Date      : 22.08.2024
*  Author             : Atitep B. (Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program routine
*  Purpose            : Include program data type and global data object
*  Copied from        : ZSDSFIR0250_TOP
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
TABLES: sscrfields.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF gty_action_status,
    seq         TYPE zsdsde_param_seq,
    action_type TYPE zsdsfit029-action_type,
    status      TYPE zsdsfit029-status,
  END OF gty_action_status.

TYPES:
  "New entry
  tt_output_new    TYPE STANDARD TABLE OF zsdsfis144,
*  tt_output        TYPE STANDARD TABLE OF zsdsfis085,
  tt_action_status TYPE STANDARD TABLE OF gty_action_status.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:

  BEGIN OF gc_func,
    del         TYPE ui_func  VALUE 'DEL',
    sel_a       TYPE sy-ucomm VALUE 'SEL_ALL',
    sel_n       TYPE sy-ucomm VALUE 'SEL_NONE',
    save        TYPE ui_func  VALUE 'SAVE',

    del_new     TYPE ui_func  VALUE 'DEL_N',
    sel_a_new   TYPE sy-ucomm VALUE 'SEL_ALL_N',
    sel_n_new   TYPE sy-ucomm VALUE 'SEL_NONE_N',
    attach_file TYPE sy-ucomm VALUE 'ATTACH_FILE',
    disp_attach TYPE sy-ucomm VALUE 'DISP_ATTACH',
  END OF gc_func,

  BEGIN OF gc_no,
    nr  TYPE inri-nrrangenr VALUE '01',
    obj TYPE inri-object    VALUE 'ZFI002',
  END OF gc_no,

  BEGIN OF gc_msg_ty,
    err_no_item_select       TYPE char20 VALUE 'E_NO_ITEM_SELECT',
    err_item_select_gt_1     TYPE char20 VALUE 'E_ITEM_SELECT_GT_1',
    err_required             TYPE char20 VALUE 'E_REQUIRED',
    err_amt_over             TYPE char20 VALUE 'E_AMT_OVER_LIMIT',
    err_amt_balance          TYPE char20 VALUE 'E_AMT_BALANCE',
    err_amt_receive          TYPE char20 VALUE 'E_AMT_RECEIVE',
    err_action_status        TYPE char20 VALUE 'E_ACTION_STATUS',
    err_action_status_before TYPE char20 VALUE 'E_ACTION_STATUS_BF',
    err_invalid_value        TYPE char20 VALUE 'E_INVALID_VALUE',
    err_duplicate            TYPE char20 VALUE 'E_DUPLICATE',
    err_invalid_input        TYPE char20 VALUE 'E_INVALID_INPUT',
    err_change_not_permit    TYPE char20 VALUE 'E_CHANGE_NOT_PERMIT',
    err_invalid_acc_id       TYPE char20 VALUE 'E_INVALID_ACC_ID',
  END OF gc_msg_ty,

  BEGIN OF gc_genc_param,
    bank_transfer TYPE zsdsde_param_name VALUE 'PYMT_METHOD_BANK',
    pdc           TYPE zsdsde_param_name VALUE 'PYMT_METHOD_PDC',
  END OF gc_genc_param.

CONSTANTS:
  gc_structure_new TYPE  tabname  VALUE 'ZSDSFIS144',
  gc_header_height TYPE  i                VALUE 16,
  gc_alv_height    TYPE  i                VALUE 90.

CONSTANTS:
  gc_true         TYPE char1     VALUE 'X',
  gc_warning      TYPE char1     VALUE 'W',
  gc_block_status TYPE zsdsfit045-block_status VALUE 'B'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: gt_output_new    TYPE tt_output_new                         ##NEEDED,
      gt_output_temp   TYPE tt_output_new                         ##NEEDED,
      gt_action_status TYPE tt_action_status                      ##NEEDED.

DATA: gt_genc   TYPE zcl_sdsca_utilities=>tt_gen_c                ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  BEGIN OF gs_default_new_entry   ##NEEDED,
    blart            TYPE zsdsfit045-blart,
    bupla            TYPE zsdsfit045-bupla,
    mwsk1            TYPE zsdsfit045-mwsk1,
    umskz            TYPE zsdsfit045-umskz,

    blart_ar_invoice TYPE zsdsfit045-blart,
    mwsk1_ar_invoice TYPE zsdsfit045-mwsk1,
  END OF gs_default_new_entry.

DATA: gs_bsid       TYPE bsid_view    ##NEEDED,
      gs_zsdsfit045 TYPE zsdsfit045   ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  gf_data_type  TYPE sy-ucomm                ##NEEDED,
  gf_edit       TYPE abap_bool               ##NEEDED,
  gf_fullname   TYPE p19_ui_cname            ##NEEDED,
  gf_waers      TYPE t001-waers              ##NEEDED,
  gf_dfpymn     TYPE zsdsfit045-pymt_method  ##NEEDED,
  gf_path       TYPE char255                 ##NEEDED,

  gf_umskz_tran TYPE umskz                   ##NEEDED,
  gf_umskz_pdc  TYPE umskz                   ##NEEDED,
  gf_umskz_bank TYPE umskz                   ##NEEDED,
  gf_umskz_ar   TYPE umskz                   ##NEEDED.
*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA: grt_pernr  TYPE RANGE OF p0002-pernr              ##NEEDED,
      grt_rcvamn TYPE RANGE OF zsdsfit045-wrbtr         ##NEEDED,
      grt_dfpymn TYPE RANGE OF zsdsfit045-pymt_method   ##NEEDED,

      grt_bank   TYPE RANGE OF zsdsfit029-pymt_method   ##NEEDED,
      grt_pdc    TYPE RANGE OF zsdsfit029-pymt_method   ##NEEDED.


*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
