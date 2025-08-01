*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0240_TOP
*  Creation Date      : 13.05.2024
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
*<<F36K917933 - 04 begin of del
*TABLES:
*  sscrfields,
*  zsdsfit043.
*<<F36K917933 - 04 end of del

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_default_value,
    param     TYPE zsdsv_gen_c-param,
    param_ext TYPE zsdsv_gen_c-param_ext,
    r_chk     TYPE RANGE OF bseg-belnr,
  END OF gty_default_value,

  BEGIN OF gty_open_item,
    bukrs       TYPE bsid_view-bukrs,
    belnr       TYPE bsid_view-belnr,
    gjahr       TYPE bsid_view-gjahr,
    buzei       TYPE bsid_view-buzei,
    budat       TYPE bsid_view-budat,
    bldat       TYPE bsid_view-bldat,
    waers       TYPE bsid_view-waers,
    xblnr       TYPE bsid_view-xblnr,
    blart       TYPE bsid_view-blart,
    kunnr       TYPE bsid_view-kunnr,
    mwskz       TYPE bsid_view-mwskz,
    dmbtr       TYPE bsid_view-dmbtr,
    wrbtr       TYPE bsid_view-wrbtr,
    xref1       TYPE bsid_view-xref1,
    xref2       TYPE bsid_view-xref2,
    zuonr       TYPE bsid_view-zuonr,
    rebzg       TYPE bsid_view-rebzg,
    rebzj       TYPE bsid_view-rebzj,
    rebzz       TYPE bsid_view-rebzz,
    sgtxt       TYPE bsid_view-sgtxt,
    prctr       TYPE bsid_view-prctr,
    waers_doc   TYPE bsid_view-waers,
    waers_loc   TYPE t001-waers,
    vbeln       TYPE bsid_view-vbeln,
    name_org1   TYPE but000-name_org1,
    name_org2   TYPE but000-name_org2,
    xcpdk       TYPE kna1-xcpdk,
    kursf       TYPE bkpf-kursf,
    brnch       TYPE bkpf-brnch,
    bktxt       TYPE bkpf-bktxt,
    shkzg       TYPE bsid_view-shkzg,
    umskz       TYPE bsid_view-umskz,
    billpl_no   TYPE zsdsfit033-billpl_no,
    billpl_date TYPE zsdsfit033-billpl_date,
    seq         TYPE zsdsfit029-seq,
  END OF gty_open_item,

  BEGIN OF gty_with_item,
    bukrs     TYPE with_item-bukrs,
    belnr     TYPE with_item-belnr,
    gjahr     TYPE with_item-gjahr,
    buzei     TYPE with_item-buzei,
    witht     TYPE with_item-witht,
    wt_withcd TYPE with_item-wt_withcd,
    wt_qbshh  TYPE with_item-wt_qbshh,
    wt_qsshb  TYPE with_item-wt_qsshb,
    wt_qbshb  TYPE with_item-wt_qbshb,
  END OF gty_with_item,

  BEGIN OF gty_account,
    expense_account        TYPE hkont,
    fee_account            TYPE hkont,
    online_tran_account    TYPE hkont,
    online_fee_account     TYPE hkont,
    online_payment_account TYPE hkont,
    retention_account      TYPE hkont,
    refund_account         TYPE hkont,
    income_account         TYPE hkont,
    cash_contra_account    TYPE hkont,
    manual_whtax_account   TYPE hkont,
  END OF gty_account,

  BEGIN OF gty_total,
    net        TYPE zsdsfis092-net,
    exp_amt    TYPE zsdsfis092-exps_amt,
    fee        TYPE zsdsfis092-fee,
    retention  TYPE zsdsfis092-retention,
    income_amt TYPE zsdsfis092-income_amt,
    cash_con   TYPE zsdsfis092-cash_con,
    whtax_amt  TYPE zsdsfis092-wt_qbshb,
  END OF gty_total,

  BEGIN OF gty_action_status,
    seq         TYPE zsdsde_param_seq,
    action_type TYPE zsdsfit029-action_type,
    status      TYPE zsdsfit029-status,
  END OF gty_action_status,

  gty_bank_statement TYPE zsdsfit042 WITH INDICATORS col_ind TYPE abap_bool,

  BEGIN OF gty_output.
    INCLUDE TYPE zsdsfis092.
TYPES:
    wt_qsshb_i TYPE zsdsfis115-wt_qsshh,  "<<F36K917933 - 04 ins
    wt_qsshh   TYPE zsdsfis115-wt_qsshh,
    brnch_doc  TYPE zsdsfis115-brnch_doc,
    blart_clr  TYPE zsdsfis115-blart_clr,
    cheque_no  TYPE zsdsfis115-cheque_no,
    bankn      TYPE zsdsfis115-bankn,
    bankl      TYPE zsdsfis115-bankl,
    vbeln_vf   TYPE zsdsfis115-vbeln_vf,
    payin_amt  TYPE zsdsfis115-payin_amt,
    bankk      TYPE zsdsfit029-bankk,
    seq        TYPE zsdsfit029-seq,
    pernr      TYPE zsdsfit029-pernr,
    work_date  TYPE zsdsfit029-work_date,
    aufnr_fee  TYPE zsdsfit029-aufnr_fee,
    aufnr_exps TYPE zsdsfit029-aufnr_exps,
    zbank_item TYPE zsdsfit029-zbank_item,
    data_type  TYPE zsdsfit029-data_type,
    bank_date  TYPE zsdsfit029-bank_date,
    hkont      TYPE t012k-hkont,
  END OF gty_output,

  BEGIN OF gty_output_new.
    INCLUDE TYPE zsdsfis124.
TYPES:
    celltab    TYPE  lvc_t_styl,
    uuid       TYPE zsdsfit043-uuid,
    hkont      TYPE t012k-hkont,
    sort_field TYPE char30,
  END OF gty_output_new,

  tt_default_value  TYPE STANDARD TABLE OF gty_default_value WITH EMPTY KEY,
  tt_open_item      TYPE STANDARD TABLE OF gty_open_item WITH EMPTY KEY,
  tt_action_status  TYPE STANDARD TABLE OF gty_action_status WITH EMPTY KEY,
  tt_with_item      TYPE STANDARD TABLE OF gty_with_item WITH EMPTY KEY,
  tt_partial        TYPE STANDARD TABLE OF zsdsfis093 WITH EMPTY KEY,
*  tt_coll_log       TYPE STANDARD TABLE OF zsdsfit029 WITH EMPTY KEY,
  tt_coll_log       TYPE STANDARD TABLE OF zsdsfis085 WITH EMPTY KEY,
  tt_bank_statement TYPE STANDARD TABLE OF gty_bank_statement,
  tt_clr_link       TYPE STANDARD TABLE OF zsdsfit037 WITH EMPTY KEY,
  tt_status_hist    TYPE STANDARD TABLE OF zsdsfit038 WITH EMPTY KEY,
  tt_output         TYPE STANDARD TABLE OF gty_output WITH EMPTY KEY,

  tt_output_new     TYPE STANDARD TABLE OF gty_output_new WITH EMPTY KEY.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  gc_true  TYPE  char1     VALUE 'X',
  gc_tcode TYPE sy-tcode VALUE 'ZSDSFI020',

  BEGIN OF gc_genc_param,
    "GenC in ZSDSFIR0240
    kunnr_one_time    TYPE zsdsde_param_name VALUE 'KUNNR_ONE_TIME',
    cost_center       TYPE zsdsde_param_name VALUE 'COST_CENTER',
    profit_center     TYPE zsdsde_param_name VALUE 'PROFIT_CENTER',
    order             TYPE zsdsde_param_name VALUE 'ORDER',
    post_dated_check  TYPE zsdsde_param_name VALUE 'POST_DATED_CHECK',
    memo_account      TYPE zsdsde_param_name VALUE 'MEMO_ACCOUNT',
    memo_tax_code     TYPE zsdsde_param_name VALUE 'MEMO_TAX_CODE',
    blart_pdc         TYPE zsdsde_param_name VALUE 'BLART_PDC',
    blart_bank_tran   TYPE zsdsde_param_name VALUE 'BLART_BANK_TRANSFER',
    blart_memo        TYPE zsdsde_param_name VALUE 'BLART_MEMO',

    "GenC in ZSDSFIR0250
    doc_type          TYPE zsdsde_param_name VALUE 'DOCUMENT_TYPE',
    sp_gl             TYPE zsdsde_param_name VALUE 'SPECIAL_GL',
    pymt_meth_bank    TYPE zsdsde_param_name VALUE 'PYMT_METHOD_BANK',
    pymt_meth_pdc     TYPE zsdsde_param_name VALUE 'PYMT_METHOD_PDC',
    action_status     TYPE zsdsde_param_name VALUE 'DOCUMENT_ACTION_STATUS',
    man_whtax_doc_typ TYPE zsdsde_param_name VALUE 'MANUAL_WHTAX_DOCUMENT_TYP',
  END OF gc_genc_param,

  BEGIN OF gc_genc_param_ext,
    action_type TYPE zsdsde_param_ext VALUE 'ACTION_TYPE',
    status      TYPE zsdsde_param_ext VALUE 'STATUS',
  END OF gc_genc_param_ext,

  BEGIN OF gc_data_type,
    invoice TYPE zsdsfit029-data_type VALUE '',
    memo    TYPE zsdsfit029-data_type VALUE 'M',
  END OF gc_data_type,

  BEGIN OF gc_mode,
    test_run TYPE char1    VALUE 'X',
    prod_run TYPE char1    VALUE '',
  END OF gc_mode,

  BEGIN OF gc_tab_sel,
    collection_log TYPE sy-ucomm VALUE 'COL_LOG',
    new_entry      TYPE sy-ucomm VALUE 'NEW_ENT',
  END OF gc_tab_sel,

  BEGIN OF gc_func,
    sel_a    TYPE sy-ucomm VALUE 'SEL_ALL',
    sel_n    TYPE sy-ucomm VALUE 'SEL_NONE',
    auto_exe TYPE sy-ucomm VALUE 'AUTO_EXE',
    man_exe  TYPE sy-ucomm VALUE 'MANUAL_EXE',
    reject   TYPE sy-ucomm VALUE 'REJECT',
  END OF gc_func,

  BEGIN OF gc_reject,
    action TYPE zsdsfit029-action_type VALUE '18',
    status TYPE zsdsfit029-status      VALUE '10',
  END OF gc_reject,

  BEGIN OF gc_status,
    new     TYPE icon_d VALUE '@EB@',
    success TYPE icon_d VALUE '@08@',
    warning TYPE icon_d VALUE '@09@',
    error   TYPE icon_d VALUE '@0A@',
  END OF gc_status,

  BEGIN OF gc_stype,
    header TYPE stype_pi VALUE 'K',
    item   TYPE stype_pi VALUE 'P',
  END OF gc_stype,
*BOI 420000700
  BEGIN OF GC_WITHT,
    AR_AT_PAYMENT TYPE WITH_ITEM-WITHT VALUE 'R1',
  END OF GC_WITHT.
*EOI 420000700
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  gt_coll_log      TYPE tt_coll_log                          ##NEEDED,
  gt_status_hist   TYPE STANDARD TABLE OF zsdsfit038          ##NEEDED,
  gt_default_value TYPE tt_default_value                      ##NEEDED,
  gt_action_status TYPE tt_action_status                      ##NEEDED,
  gt_output        TYPE tt_output                             ##NEEDED,

  gt_output_new    TYPE tt_output_new                         ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  gs_bsid     TYPE bsid_view                                  ##NEEDED,
  gs_coll_log TYPE zsdsfit029                                 ##NEEDED,
  gs_account  TYPE gty_account                                ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  gv_post_dated_chk TYPE bsed-portf                            ##NEEDED,
  gv_memo_account   TYPE bseg-hkont                            ##NEEDED,
  gv_memo_tax_code  TYPE bseg-mwskz                            ##NEEDED,
  gv_waers          TYPE t001-waers                            ##NEEDED,
  gv_edit           TYPE abap_bool                             ##NEEDED,
  gv_data_type      TYPE sy-ucomm                              ##NEEDED,

  gv_blart_pdc      TYPE blart                                 ##NEEDED,
  gv_blart_bank     TYPE blart                                 ##NEEDED,
  gv_blart_memo     TYPE blart                                 ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA:
  gr_one_time    TYPE RANGE OF bsid_view-kunnr                 ##NEEDED,
  gr_blart       TYPE RANGE OF bsid_view-blart                 ##NEEDED,
  gr_whtax_blart TYPE RANGE OF bsid_view-blart                 ##NEEDED,
  gr_spgl        TYPE RANGE OF bsid_view-umskz                 ##NEEDED,
  gr_pymt        TYPE RANGE OF zsdsfit029-pymt_method          ##NEEDED.

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
  gc_structure_tran TYPE  tabname  VALUE 'ZSDSFIS092',
  gc_structure_pdc  TYPE  tabname  VALUE 'ZSDSFIS115'.
*  gc_structure_new  TYPE  tabname  VALUE 'ZSDSFIS124'. *<<F36K917933 - 04 del

CONSTANTS:
  gc_header_height TYPE  i                VALUE 10,
  gc_alv_height    TYPE  i                VALUE 90.

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
*----------------------------------------------------------------------*
* SELECTION-SCREEN 100
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN. *<<F36K917933 - 03 del
** Text-s01: Customer Selection
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_bukrs TYPE knb1-bukrs OBLIGATORY.

  SELECT-OPTIONS:
    s_trnf    FOR  gs_coll_log-tranf_no MEMORY ID zsds_var_tranf_no.

  PARAMETERS:
    "By Customer No.
    rb_cust RADIOBUTTON GROUP sel1 DEFAULT 'X' USER-COMMAND cus.
  SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME.
    SELECT-OPTIONS:
      s_kunnr   FOR  gs_bsid-kunnr MODIF ID cus.
  SELECTION-SCREEN END OF BLOCK b11.

  "By Bill Placement No.
  PARAMETERS:
    rb_bill RADIOBUTTON GROUP sel1.
  SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME.
    SELECT-OPTIONS:
      s_plno    FOR gs_coll_log-billpl_no MODIF ID bil,
      s_pldate  FOR gs_coll_log-billpl_date MODIF ID bil.
  SELECTION-SCREEN END OF BLOCK b12.

  "By Invoice FI Document
  PARAMETERS:
    rb_inv RADIOBUTTON GROUP sel1.
  SELECTION-SCREEN BEGIN OF BLOCK b13 WITH FRAME.
    SELECT-OPTIONS:
      s_belnr   FOR gs_coll_log-belnr MODIF ID inv,
      s_gjahr   FOR gs_coll_log-gjahr NO-EXTENSION NO INTERVALS MODIF ID inv.
  SELECTION-SCREEN END OF BLOCK b13.

SELECTION-SCREEN END OF BLOCK b1.

* Text-s02: Posting Information
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  PARAMETERS:
    p_blartc TYPE bsid_view-blart OBLIGATORY,
    p_bldatc TYPE bsid_view-bldat, "OBLIGATORY,
    p_budatc TYPE bsid_view-budat, "OBLIGATORY,
*    p_waersc TYPE bkpf-waers         OBLIGATORY,
*    p_kursfc TYPE bkpf-kursf         OBLIGATORY,
    p_brnchc TYPE bkpf-brnch,      "OBLIGATORY,
    p_bktxtc TYPE bkpf-bktxt,
    p_sgtxt  TYPE bsid_view-sgtxt.

* Text-s21: Incoming payment Bank
  SELECTION-SCREEN BEGIN OF BLOCK b21 WITH FRAME TITLE TEXT-s21.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 21(9) TEXT-s93 FOR FIELD rb_memo.
      PARAMETERS:
        rb_memo RADIOBUTTON GROUP pay USER-COMMAND in_bank.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 27(3) TEXT-s91 FOR FIELD rb_pdc.
      PARAMETERS:
        rb_pdc  RADIOBUTTON  GROUP pay.
      SELECTION-SCREEN COMMENT 33(10) TEXT-s92 FOR FIELD p_pdspgl.
      PARAMETERS:
        p_pdspgl TYPE bsid_view-umskz.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 17(13) TEXT-s94 FOR FIELD rb_tran.
      PARAMETERS:
        rb_tran RADIOBUTTON GROUP pay DEFAULT 'X'.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 8(22) TEXT-s95 FOR FIELD rb_oth.
      PARAMETERS:
      rb_oth  RADIOBUTTON GROUP pay.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF BLOCK b22 WITH FRAME.
      PARAMETERS:
        p_hbkid TYPE zsdsfit029-hbkid MODIF ID opy,
        p_hktid TYPE zsdsfit029-hktid MODIF ID opy,
*      p_konto TYPE rf05a-konto MODIF ID opy,
*      p_prctr TYPE bseg-prctr  MODIF ID opy,
        p_zfbdt TYPE bseg-zfbdt  MODIF ID opy ##NEEDED.
    SELECTION-SCREEN END OF BLOCK b22.

  SELECTION-SCREEN END OF BLOCK b21.

SELECTION-SCREEN END OF BLOCK b2.

*<<F36K917933 - 03 Begin of del
*SELECTION-SCREEN END OF SCREEN 100.
*
**----------------------------------------------------------------------*
** SELECTION-SCREEN 200
**----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
**----------------------------------------------------------------------*
*  SELECTION-SCREEN BEGIN OF BLOCK bn1 WITH FRAME TITLE TEXT-s03. "Selection By
*    PARAMETERS:
*      p_bukrsn TYPE knb1-bukrs OBLIGATORY.
*
*    SELECT-OPTIONS:
*      s_pernrn   FOR  zsdsfit043-pernr MODIF ID csn,
*      s_trnfn    FOR  gs_coll_log-tranf_no,
*      s_erdatn   FOR  zsdsfit043-erdat MODIF ID csn,
*      s_kunnrn   FOR  gs_bsid-kunnr MODIF ID csn,
*      s_xblnrn   FOR  gs_bsid-xblnr MODIF ID csn.
*
*  SELECTION-SCREEN END OF BLOCK bn1.
*SELECTION-SCREEN END OF SCREEN 200.
*
**----------------------------------------------------------------------*
** SELECTION-SCREEN TAB
**----------------------------------------------------------------------*
*SELECTION-SCREEN: BEGIN OF TABBED BLOCK tab_blk FOR 30 LINES,
*TAB (20) tab_bt1 USER-COMMAND col_log,
**TAB (20) tab_bt2 USER-COMMAND new_ent,
*END OF BLOCK tab_blk.
*<<F36K917933 - 03 End of del
