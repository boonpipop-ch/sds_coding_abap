*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0250_TOP
*  Creation Date      : 21.05.2024
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
##NEEDED
TABLES sscrfields.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_default_value,
    param     TYPE zsdsv_gen_c-param,
    param_ext TYPE zsdsv_gen_c-param_ext,
    r_chk     TYPE RANGE OF bseg-belnr,
  END OF gty_default_value,

  BEGIN OF gty_action_status,
    seq         TYPE zsdsde_param_seq,
    action_type TYPE zsdsfit029-action_type,
    status      TYPE zsdsfit029-status,
  END OF gty_action_status,

  BEGIN OF gty_onetime,
    bukrs TYPE bsec-bukrs,
    belnr TYPE bsec-belnr,
    gjahr TYPE bsec-gjahr,
    buzei TYPE bsec-buzei,
    name1 TYPE bsec-name1,
    name2 TYPE bsec-name2,
  END OF gty_onetime,

  BEGIN OF gty_tax_base,
    bukrs TYPE bseg-bukrs,
    belnr TYPE bseg-belnr,
    gjahr TYPE bseg-gjahr,
    buzei TYPE bseg-buzei,
    wrbtr TYPE bseg-wrbtr,
    fwbas TYPE bseg-fwbas,
  END OF gty_tax_base,

  BEGIN OF gty_whtax_type,
    kunnr TYPE knbw-kunnr,
    qsrec TYPE knbw-kunnr,
  END OF gty_whtax_type,

  BEGIN OF gty_whtax,
    bukrs     TYPE with_item-bukrs,
    belnr     TYPE with_item-belnr,
    gjahr     TYPE with_item-gjahr,
    buzei     TYPE with_item-buzei,
    witht     TYPE with_item-witht,
    wt_withcd TYPE with_item-wt_withcd,
    wt_qsshb  TYPE with_item-wt_qsshb,
    qsatz     TYPE t059z-qsatz,
    qproz     TYPE t059z-qproz,
    wt_amount TYPE with_item-wt_qbshb,
  END OF gty_whtax,

  BEGIN OF gty_partial_pay,
    bukrs    TYPE bsid_view-bukrs,
    belnr    TYPE bsid_view-belnr,
    gjahr    TYPE bsid_view-gjahr,
    buzei    TYPE bsid_view-buzei,
    waers    TYPE bsid_view-waers,
    dmbtr    TYPE bsid_view-dmbtr,
    wrbtr    TYPE bsid_view-wrbtr,
    rebzg    TYPE bsid_view-rebzg,
    rebzj    TYPE bsid_view-rebzj,
    rebzz    TYPE bsid_view-rebzz,
    xref2_hd TYPE bkpf-xref2_hd,
  END OF gty_partial_pay,

  BEGIN OF gty_personal,
    pernr     TYPE zsdsfit029-pernr,
    full_name TYPE zsdsfis085-full_name,
  END OF gty_personal,

  BEGIN OF gty_grp_doc.
    INCLUDE TYPE zsdsfit029.
TYPES: trn_type      TYPE char03,
    sort_field    TYPE char50,
    tranf_no_flag TYPE abap_bool,
  END OF gty_grp_doc,

  BEGIN OF gty_doc_update,
    bukrs TYPE bsid_view-bukrs,
    belnr TYPE bsid_view-belnr,
    gjahr TYPE bsid_view-gjahr,
    buzei TYPE bsid_view-buzei,
    samnr TYPE bseg-samnr,
  END OF gty_doc_update,

  BEGIN OF gty_do,
    vbeln TYPE vbrp-vbeln,
    fkdat TYPE vbrk-fkdat,    "<<F36K911749 ins
    aubel TYPE vbrp-aubel,    "<<F36K911780 ins
    vgbel TYPE vbrp-vgbel,
    lfdat TYPE likp-lfdat,
    adrnr TYPE vbpa-adrnr,    "<<F36K912524 - 07 ins
  END OF gty_do,

*<<F36K914812 - 04 Begin of ins
  BEGIN OF gty_user_data,
    crusr     TYPE zsdsfit033-crusr,
    pernr     TYPE zsdsfit029-pernr,
    full_name TYPE zsdsfis085-full_name,
  END OF gty_user_data,
*<<F36K914812 - 04 End of ins

*<<F36K914812 - 07 Begin of ins
  BEGIN OF gty_cust_adrc,
    addrnumber TYPE adrc-addrnumber,
    nation     TYPE adrc-nation,
    name1      TYPE adrc-name1,
  END OF gty_cust_adrc,
*<<F36K914812 - 07 End of ins

*<<F36K914812 - 02 Begin of ins
  BEGIN OF gty_so,
    vbeln    TYPE vbfa-vbeln,
    vbeln_so TYPE vbfa-vbelv,
    bstkd    TYPE zsdsfit029-po_ref,
  END OF gty_so,
*<<F36K914812 - 02 End of ins

*<<F36K917931 - 03 Begin of ins
  BEGIN OF gty_clr_link_all.
    INCLUDE TYPE zsdsfit037.
TYPES:
    xreversed TYPE bkpf-xreversed,
    stblg     TYPE bkpf-stblg,
    stjah     TYPE bkpf-stjah,
  END OF gty_clr_link_all,
*<<F36K917931 - 03 End of ins

"<<F36K919968 - 01 Begin of del
**<<F36K917931 - 07 Begin of ins
*  BEGIN OF gty_inv_chk_status,
*    vbeln       TYPE zsdsfit027-vbeln,
*    zrcvd_pernr TYPE zsdsfit027-zrcvd_pernr, "<<F36K919968 - 01
*    zrcvd_d     TYPE zsdsfit027-zrcvd_d,     "<<F36K919968 - 01
*    action_type TYPE zsdsfic028-bl_actty,
*    status      TYPE zsdsfic028-bl_stat,
*  END OF gty_inv_chk_status,
**<<F36K917931 - 07 End of ins
"<<F36K919968 - 01 end of del

  gty_log_del        TYPE zsdsfit038 WITH INDICATORS col_ind TYPE abap_bool,
  gty_bank_statement TYPE zsdsfit042 WITH INDICATORS col_ind TYPE abap_bool,



  tt_default_value   TYPE STANDARD TABLE OF gty_default_value WITH EMPTY KEY,
  tt_personal        TYPE SORTED TABLE OF gty_personal WITH UNIQUE KEY pernr,
  tt_action_status   TYPE STANDARD TABLE OF gty_action_status,
  tt_onetime         TYPE STANDARD TABLE OF gty_onetime,
  tt_tax_base        TYPE SORTED TABLE OF gty_tax_base
                      WITH NON-UNIQUE KEY bukrs belnr gjahr buzei,
  tt_whtax_type      TYPE SORTED TABLE OF gty_whtax_type
                      WITH NON-UNIQUE KEY kunnr,
  tt_whtax           TYPE SORTED TABLE OF gty_whtax
                      WITH NON-UNIQUE KEY bukrs belnr gjahr buzei,
  tt_partial         TYPE STANDARD TABLE OF gty_partial_pay,
  tt_bplog           TYPE STANDARD TABLE OF zsdsfit029,
  tt_output          TYPE STANDARD TABLE OF zsdsfis085 WITH EMPTY KEY,
  tt_status_hist     TYPE STANDARD TABLE OF zsdsfit038,
  tt_grp_doc         TYPE STANDARD TABLE OF gty_grp_doc,
  tt_doc_update      TYPE STANDARD TABLE OF gty_doc_update,
  tt_do              TYPE STANDARD TABLE OF gty_do WITH EMPTY KEY,
  tt_so              TYPE STANDARD TABLE OF gty_so WITH EMPTY KEY,  "<<F36K914812 - 02 ins
*  tt_inv_chk_status  TYPE STANDARD TABLE OF gty_inv_chk_status WITH EMPTY KEY, "<<F36K917931 - 07 ins "<<F36K919968 - 01 del
  tt_bank_statement  TYPE STANDARD TABLE OF gty_bank_statement,

  "New entry
  tt_output_new      TYPE STANDARD TABLE OF zsdsfis123 WITH EMPTY KEY,

  "History log
  tt_history_log     TYPE STANDARD TABLE OF zsdsfis138 WITH EMPTY KEY,

*<<F36K917931 - 03 Begin of ins
  tt_clr_link_all    TYPE STANDARD TABLE OF gty_clr_link_all WITH EMPTY KEY,
*<<F36K917931 - 03 End of ins

*<<F36K914812 - 04 Begin of ins
  tt_user_data       TYPE SORTED TABLE OF gty_user_data
                      WITH NON-UNIQUE KEY crusr pernr,
*<<F36K914812 - 04 End of ins

*<<F36K914812 - 07 Begin of ins
  tt_cust_adrc       TYPE SORTED TABLE OF gty_cust_adrc
                      WITH UNIQUE KEY addrnumber nation.
*<<F36K914812 - 07 End of ins


*-Beg of INS by Jutamas Y.
TYPES: BEGIN OF ts_billpl ,
         bukrs       TYPE bkpf-bukrs,
         belnr       TYPE bkpf-belnr,
         gjahr       TYPE bkpf-gjahr,
         blart       TYPE bkpf-blart,
         billpl_no   TYPE zsdsfit033-billpl_no,
         billpl_date TYPE zsdsfit033-billpl_date,
         buzei       TYPE zsdsfit035-buzei,
       END   OF ts_billpl .

TYPES tt_billpl  TYPE STANDARD TABLE OF ts_billpl .
*-End of INS by Jutamas Y.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
##NEEDED
CONSTANTS:
  gc_true      TYPE  char1     VALUE 'X',
  gc_tcode     TYPE sy-tcode VALUE 'ZSDSFI014',
  gc_tcode_his TYPE sy-tcode VALUE 'ZSDSFI048',

  BEGIN OF gc_data_type,
    invoice TYPE zsdsfit029-data_type VALUE '',
    memo    TYPE zsdsfit029-data_type VALUE 'M',
  END OF gc_data_type,

  BEGIN OF gc_no,
    nr  TYPE inri-nrrangenr VALUE '01',
    obj TYPE inri-object    VALUE 'ZFI002',
  END OF gc_no,

  BEGIN OF gc_tab_sel,
    collection_log TYPE sy-ucomm VALUE 'COL_LOG',
    new_entry      TYPE sy-ucomm VALUE 'NEW_ENT',
    update_bank    TYPE sy-ucomm VALUE 'UPD_BNK',
    memo           TYPE sy-ucomm VALUE 'MEMO',
    history_log    TYPE sy-ucomm VALUE 'HIST_LOG',
  END OF gc_tab_sel,

  BEGIN OF gc_func,
*    duplicate TYPE ui_func  VALUE 'DUPLICATE_ITEM',
    del       TYPE ui_func  VALUE 'DEL',
    sel_a     TYPE sy-ucomm VALUE 'SEL_ALL',
    sel_n     TYPE sy-ucomm VALUE 'SEL_NONE',
    save      TYPE ui_func  VALUE 'SAVE',
    post_inc  TYPE ui_func  VALUE 'ZRECEIVED',
    mass_upd  TYPE ui_func  VALUE 'ZMASS_UPD',
    his_sel_a TYPE sy-ucomm VALUE 'SEL_ALL_H',
    his_sel_n TYPE sy-ucomm VALUE 'SEL_NONE_H',
    his_dsp   TYPE ui_func  VALUE 'ZHIST_DISP',

    del_new   TYPE ui_func  VALUE 'DEL_N',
    sel_a_new TYPE sy-ucomm VALUE 'SEL_ALL_N',
    sel_n_new TYPE sy-ucomm VALUE 'SEL_NONE_N',

  END OF gc_func,

  BEGIN OF gc_genc_param,
    df_house_bank     TYPE zsdsde_param_name VALUE 'DEFAULT_HOUSE_BANK',
    df_bank_acct      TYPE zsdsde_param_name VALUE 'DEFAULT_ACCOUNT_ID',
    df_pdc_bank_key   TYPE zsdsde_param_name VALUE 'DEFAULT_PDC_BANK_KEY',
    df_pdc_bank_acct  TYPE zsdsde_param_name VALUE 'DEFAULT_PDC_BANK_ACCT',
    df_order_exp      TYPE zsdsde_param_name VALUE 'DEFAULT_ORDER_EXPENSE',
    df_order_fee      TYPE zsdsde_param_name VALUE 'DEFAULT_ORDER_BANK_FEE',

    srv_inv_doc_type  TYPE zsdsde_param_name VALUE 'SRV_INV_DOC_TYPE',
    srv_whtax_rate    TYPE zsdsde_param_name VALUE 'SRV_INV_WHTAX_RATE_%',
    bank_method       TYPE zsdsde_param_name VALUE 'BANK_TRANSFER_METHOD',
    doc_type          TYPE zsdsde_param_name VALUE 'DOCUMENT_TYPE',
    sp_gl             TYPE zsdsde_param_name VALUE 'SPECIAL_GL',
    kostl             TYPE zsdsde_param_name VALUE 'COST_CENTER',
    prctr             TYPE zsdsde_param_name VALUE 'PROFIT_CENTER',
    bank_transfer     TYPE zsdsde_param_name VALUE 'PYMT_METHOD_BANK',
    pdc               TYPE zsdsde_param_name VALUE 'PYMT_METHOD_PDC',
    action_status     TYPE zsdsde_param_name VALUE 'DOCUMENT_ACTION_STATUS',
    man_whtax_doc_typ TYPE zsdsde_param_name VALUE 'MANUAL_WHTAX_DOCUMENT_TYP',
  END OF gc_genc_param,

  BEGIN OF gc_genc_param_ext,
    action_type TYPE zsdsde_param_ext VALUE 'ACTION_TYPE',
    status      TYPE zsdsde_param_ext VALUE 'STATUS',
  END OF gc_genc_param_ext,

  BEGIN OF gc_genc_param_new_entry,
    blart TYPE zsdsde_param_name VALUE 'DOCUMENT_TYPE_NEW_ENTRY',
    bupla TYPE zsdsde_param_name VALUE 'BUSINESS_PLACE_NEW_ENTRY',
    mwsk1 TYPE zsdsde_param_name VALUE 'TAX_CODE_NEW_ENTRY',
    umskz TYPE zsdsde_param_name VALUE 'SPECIAL_GL_NEW_ENTRY',
    prctr TYPE zsdsde_param_name VALUE 'PROFIT_CENTER_NEW_ENTRY',
  END OF gc_genc_param_new_entry,

  BEGIN OF gc_genc_param_memo,
    bupla TYPE zsdsde_param_name VALUE 'BUSINESS_PLACE_MEMO',
    prctr TYPE zsdsde_param_name VALUE 'PROFIT_CENTER_MEMO',
  END OF gc_genc_param_memo,

  BEGIN OF gc_msg_ty,
    err_required             TYPE char20 VALUE 'E_REQUIRED',
    err_amt_over             TYPE char20 VALUE 'E_AMT_OVER_LIMIT',
    err_amt_lower            TYPE char20 VALUE 'E_AMT_LOWER_LIMIT',
    err_amt_balance          TYPE char20 VALUE 'E_AMT_BALANCE',
    err_amt_receive          TYPE char20 VALUE 'E_AMT_RECEIVE',
    err_action_status        TYPE char20 VALUE 'E_ACTION_STATUS',
    err_action_status_before TYPE char20 VALUE 'E_ACTION_STATUS_BF',
    err_input_not_allow      TYPE char20 VALUE 'E_INPUT_NOT_ALLOW',
    err_invalid_input        TYPE char20 VALUE 'E_INVALID_INPUT',
    err_invalid_acc_id       TYPE char20 VALUE 'E_INVALID_ACC_ID',
*-Beg of INS by Jutamas Y.
    err_invalid_billpl_no    TYPE char20 VALUE 'E_INVALID_BILLPL_NO',
    err_invalid_billpl_date  TYPE char20 VALUE 'E_INVALID_BILLPLDATE',
*-End of INS by Jutamas Y.
  END OF gc_msg_ty,

  BEGIN OF gc_dom ##NEEDED,
    action TYPE dd04d-domname VALUE 'ZSDSDM_ACTION_TYPE',
    status TYPE dd04d-domname VALUE 'ZSDSDM_COL_STATUS',
  END OF gc_dom,

  gc_qsrec_03 TYPE knbw-qsrec VALUE '03'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
*  gt_bank        TYPE tt_bank,
  gt_fullname       TYPE tt_personal                           ##NEEDED,
  gt_action_status  TYPE tt_action_status                      ##NEEDED,
  gt_onetime        TYPE tt_onetime                            ##NEEDED,
  gt_partial        TYPE tt_partial                            ##NEEDED,
  gt_tax_base       TYPE tt_tax_base                           ##NEEDED,
  gt_whtax_type     TYPE tt_whtax_type                         ##NEEDED,
  gt_whtax          TYPE tt_whtax                              ##NEEDED,
  gt_do             TYPE tt_do                                 ##NEEDED,
  gt_so             TYPE tt_so                                 ##NEEDED,  "<<F36K914812 - 02 ins
  gt_output         TYPE tt_output                             ##NEEDED,
  gt_bplog          TYPE tt_bplog                              ##NEEDED,
*  gt_clr_link      TYPE STANDARD TABLE OF zsdsfit037         ##NEEDED,  "<<F36K917931 - 03 del
  gt_clr_link       TYPE tt_clr_link_all                       ##NEEDED,  "<<F36K917931 - 03 ins
*  gt_inv_chk_status TYPE tt_inv_chk_status                    ##NEEDED,  "<<F36K917931 - 07 ins "<<F36K919968 - 01 del
  gt_status_hist    TYPE STANDARD TABLE OF zsdsfit038          ##NEEDED,

  gt_output_new     TYPE tt_output_new                         ##NEEDED,
  gt_history_log    TYPE tt_history_log                        ##NEEDED,
  gt_user_data      TYPE tt_user_data                          ##NEEDED,  "<<F36K914812 - 04 ins
  gt_cust_adrc      TYPE tt_cust_adrc                          ##NEEDED.  "<<F36K914812 - 07 ins

*-Beg of INS by Jutamas Y.
DATA gt_billpl TYPE tt_billpl ##NEEDED.
*-End of INS by Jutamas Y.
*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  BEGIN OF gs_default_coll_log    ##NEEDED,
    prctr TYPE zsdsfit029-prctr,
    kostl TYPE zsdsfit029-kostl,
  END OF gs_default_coll_log,

  BEGIN OF gs_default_new_entry   ##NEEDED,
    blart TYPE zsdsfit043-blart,
    bupla TYPE zsdsfit043-bupla,
    mwsk1 TYPE zsdsfit043-mwsk1,
    umskz TYPE zsdsfit043-umskz,
    prctr TYPE zsdsfit043-prctr,
  END OF gs_default_new_entry,

  BEGIN OF gs_default_memo   ##NEEDED,
    brnch TYPE zsdsfit029-brnch,
    prctr TYPE zsdsfit029-prctr,
  END OF gs_default_memo,

  gs_bsid       TYPE bsid_view    ##NEEDED,
  gs_zsdsfit029 TYPE zsdsfit029   ##NEEDED.


*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  gv_data_type        TYPE sy-ucomm               ##NEEDED,
  gv_edit             TYPE abap_bool              ##NEEDED,
  gv_fullname         TYPE p19_ui_cname           ##NEEDED,
  gv_waers            TYPE t001-waers             ##NEEDED,
  gv_df_pdc_bank_key  TYPE zsdsfit029-bankk       ##NEEDED,
  gv_df_pdc_bank_acct TYPE zsdsfit029-bankn       ##NEEDED,
  gv_df_order_exp     TYPE zsdsfit029-aufnr_exps  ##NEEDED,
  gv_df_order_fee     TYPE zsdsfit029-aufnr_fee   ##NEEDED,
  gv_srv_whtax_rate   TYPE with_item-qsatz        ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA:
  gr_bank        TYPE RANGE OF zsdsfit029-pymt_method   ##NEEDED,
  gr_pdc         TYPE RANGE OF zsdsfit029-pymt_method   ##NEEDED,
  gr_blart       TYPE RANGE OF bsid_view-blart          ##NEEDED,
  gr_spgl        TYPE RANGE OF bsid_view-umskz          ##NEEDED,
  gr_bank_trn    TYPE RANGE OF zsdsfit029-pymt_method   ##NEEDED,
  gr_srv_blart   TYPE RANGE OF zsdsfis085-blart         ##NEEDED,
  gr_whtax_blart TYPE RANGE OF zsdsfis085-blart         ##NEEDED.

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
##NEEDED
CONSTANTS:
  gc_structure     TYPE  tabname  VALUE 'ZSDSFIS085',
  gc_structure_new TYPE  tabname  VALUE 'ZSDSFIS123',
  gc_structure_log TYPE  tabname  VALUE 'ZSDSFIS138'.

##NEEDED
CONSTANTS:
  gc_header_height TYPE  i                VALUE 20,
  gc_alv_height    TYPE  i                VALUE 90.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
