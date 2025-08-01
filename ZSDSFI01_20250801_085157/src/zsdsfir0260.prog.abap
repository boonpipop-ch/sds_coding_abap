*&---------------------------------------------------------------------*
*& Report ZSDSFIR0260
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZFIAPE004
*  Description        : Report and Export data employee advance into text file
*  Purpose            :
*  Copied from        :  ZR_FIAP_ADVANCE
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0260.
TABLES : payr,    "Payment Transaction View
           bkpf,
           bsak,
           bsad,
           lfa1,
           lfbk,
           BNKA,
           bsec,
           BSAS,
           bsis.

*---------DATA TO BE DISPLAY IN ALV GRID----------*

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gy_itab,
       run_no               TYPE i,                 "Run No.
       payee_no(32)         TYPE c,        "Payee no
       payee_name(200)      TYPE c,                 "Payee Name
       payee_addr(260)      TYPE c,                 "Payee Address
       payee_phone          TYPE adrc-tel_number,   "Payee Phone no
       bank_name            TYPE bnka-banka,        "Bank Name
       zbank_code(3)         TYPE c,                 "Bank Code
       branch_code(4)       TYPE c,                 "Branch Code
       branch_name(80)      TYPE c,                 "Branch Name
       bank_account(20)     TYPE c,                 "Bank Account
       pay_amt              TYPE bsak-dmbtr,        "Payment Amount
       pay_doc              TYPE bsak-augbl,        "Payment Doc.
       pay_date             TYPE bkpf-budat,        "Payment date
       email                TYPE adr6-smtp_addr,    "E-mail Address
       zipcode              TYPE adrc-post_code1,   "Zipcode
       fax_no               TYPE adrc-fax_number,   "Fax Number
       cheque_no            TYPE payr-chect,        "Cheque No.
       ap_inv               TYPE bsak-belnr,        "AP Invoice
       ap_inv_date          TYPE bsak-budat,        "AP Invoice Date
       ap_inv_amt           TYPE bsak-dmbtr,        "AP Invoice Amt
       ap_wht_amt           TYPE bsak-qbshb,        "WHT  amt
       ap_inv_text          TYPE bsak-sgtxt,        "Text
       ap_wht_typ(50)        TYPE c,                 "WHT Type
       ap_customer          TYPE kna1-kunnr,        "Customer Code
       status_regis(50)     TYPE c,                 "Status Vender Regis
       bankl                TYPE bnka-bankl,   "Bank Key


END OF gy_itab.

TYPES: BEGIN OF gy_itab_regis,

       payee_no(32)         TYPE c,        "Payee no
       payee_name(200)      TYPE c,                 "Payee Name
       payee_addr(260)      TYPE c,                 "Payee Address
       payee_phone          TYPE adrc-tel_number,   "Payee Phone no
       bank_name            TYPE bnka-banka,        "Bank Name
       zbank_code(3)         TYPE c,                 "Bank Code
       branch_code(4)       TYPE c,                 "Branch Code
       branch_name(80)      TYPE c,                 "Branch Name
       bank_account(20)     TYPE c,                 "Bank Account
       email                TYPE adr6-smtp_addr,    "E-mail Address
       zipcode              TYPE adrc-post_code1,   "Zipcode
       fax_no               TYPE adrc-fax_number,   "Fax Number
       status_regis(50)     TYPE c,                 "Status Vender Regis
       bankl                TYPE bnka-bankl,   "Bank Key
       status_check(250)    TYPE c,                 " Status Check
       line_color(4)      TYPE c,              "Line Color

END OF gy_itab_regis.

TYPES: BEGIN OF gy_bsak_pay,
       lifnr               TYPE bsak-lifnr,         "Vender
       gjahr               TYPE bsak-gjahr,         "Fiscal Year
       augbl               TYPE bsak-augbl,         "Doc.Payment
*       buzei               TYPE bsak-buzei,         "Item
       auggj               TYPE bsak-auggj,         "Fiscal Year
       budat               TYPE bkpf-budat,         "Doc.Post date
       shkzg               TYPE bsak-shkzg,         "S Debit H credit
       dmbtr               TYPE bsak-dmbtr,         "Amount
       qbshb               TYPE bsak-qbshb,         "WHT
       sgtxt               TYPE bsak-sgtxt,         "Text
       hkont               TYPE bsak-hkont,         "GL number
       adrnr               TYPE lfa1-adrnr,         "Address number

END OF gy_bsak_pay.

TYPES: BEGIN OF gy_bsad_clear,
       lifnr               TYPE bsak-lifnr,         "Vender
       augbl               TYPE bsad-augbl,         "Doc.Payment
       auggj               TYPE bsad-auggj,         "Fiscal Year
       belnr               TYPE bsad-belnr,         "Doc. Invoice
       gjahr               TYPE bsad-gjahr,         "Fiscal Year Invoice
       buzei               TYPE bsad-buzei,         "Item
       budat               TYPE bsad-budat,         "Doc.Post date
       shkzg               TYPE bsad-shkzg,         "S Debit H credit
       dmbtr               TYPE bsad-dmbtr,         "Amount
*       qbshb               TYPE bsad-qbshb,         "WHT
       sgtxt               TYPE bsad-sgtxt,         "Text
       xblnr               TYPE bsad-xblnr,         "Referance
       rebzg               TYPE bsad-rebzg,         "Referance doc.
*       hkont               TYPE bsad-hkont,         "GL number
END OF gy_bsad_clear.
TYPES: BEGIN OF gy_bsid_clear,
       lifnr               TYPE bsak-lifnr,         "Vender
       augbl               TYPE bsid-augbl,         "Doc.Payment
       auggj               TYPE bsid-auggj,         "Fiscal Year
       belnr               TYPE bsid-belnr,         "Doc. Invoice
       gjahr               TYPE bsid-gjahr,         "Fiscal Year Invoice
       buzei               TYPE bsid-buzei,         "Item
       budat               TYPE bsid-budat,         "Doc.Post date
       shkzg               TYPE bsid-shkzg,         "S Debit H credit
       dmbtr               TYPE bsid-dmbtr,         "Amount
*       qbshb               TYPE bsad-qbshb,         "WHT
       sgtxt               TYPE bsid-sgtxt,         "Text
       xblnr               TYPE bsid-xblnr,         "Referance
       rebzg               TYPE bsid-rebzg,         "Referance doc.
*       hkont               TYPE bsid-hkont,         "GL number
END OF gy_bsid_clear.

TYPES: BEGIN OF gy_bsak_inv,
       lifnr               TYPE bsak-lifnr,         "Vender
       augbl               TYPE bsak-augbl,         "Doc.Payment
       auggj               TYPE bsak-auggj,         "Fiscal Year
       belnr               TYPE bsak-belnr,         "Doc. Invoice
       gjahr               TYPE bsak-gjahr,         "Fiscal Year Invoice
       buzei               TYPE bsak-buzei,         "Item
       budat               TYPE bsak-budat,         "Doc.Post date
       shkzg               TYPE bsak-shkzg,         "S Debit H credit
       dmbtr               TYPE bsak-dmbtr,         "Amount
       qbshb               TYPE bsak-qbshb,         "WHT
       sgtxt               TYPE bsak-sgtxt,         "Text
       xblnr               TYPE bsak-xblnr,         "Referance
*       hkont               TYPE bsak-hkont,         "GL number
END OF gy_bsak_inv.

"Add by Wantanee 20140707
TYPES: BEGIN OF gy_bsis,
       hkont               TYPE bsis-hkont,         "GL Number
       belnr               TYPE bsis-belnr,         "Doc. Invoice
       gjahr               TYPE bsis-gjahr,         "Fiscal Year Invoice
       buzei               TYPE bsis-buzei,         "Item
       budat               TYPE bsis-budat,         "Doc.Post date
       shkzg               TYPE bsis-shkzg,         "S Debit H credit
       dmbtr               TYPE bsis-dmbtr,         "Amount
       sgtxt               TYPE bsis-sgtxt,         "Text
       xref3               TYPE bsis-xref3,         "Referance
END OF gy_bsis.
"End add by Wantanee 20140707


TYPES: BEGIN OF gy_adrc,

       addrnumber     TYPE adrc-addrnumber, "Address Number
       name1          TYPE adrc-name1,      "Name
       name2          TYPE adrc-name2,      "Name2
       street         TYPE adrc-street,     "Street
       str_suppl3     TYPE adrc-str_suppl3, "Street4
       location       TYPE adrc-location,    " Street 5
       city2          TYPE adrc-city2,      "city2
       city1          TYPE adrc-city1,      "city1
       post_code1     TYPE adrc-post_code1, "Post_code
       tel_number     TYPE adrc-tel_number, "tel
       fax_number     TYPE adrc-fax_number, "Fax
       nation         TYPE adrc-nation, "Fax
END OF gy_adrc.

TYPES: BEGIN OF gy_with_item,
       belnr               TYPE with_item-belnr,         "Doc. Pay
       gjahr               TYPE with_item-gjahr,         "Fiscal Year Invoice
       buzei               TYPE with_item-buzei,         "Item
       witht               TYPE with_item-witht,         "Wht
       wt_withcd           TYPE with_item-wt_withcd,     "Type wht

END OF gy_with_item.


TYPES: BEGIN OF gy_t059zt,
       witht               TYPE t059zt-witht,         "Wht
       wt_withcd           TYPE t059zt-wt_withcd,     "Type wht
       text40              TYPE t059zt-text40,        "Text
END OF gy_t059zt.

TYPES: BEGIN OF gy_rep_excel,
       lifnr           TYPE lfa1-lifnr,
       augbl           TYPE bsak-augbl,
       name(100)       TYPE c,
       acc_bank(18)    TYPE c,
       bank_amt        TYPE bsis-dmbtr,
       bank_name(50)   TYPE c,
       e_mail(50)      TYPE c,
       Beneficiary(2)  TYPE c,
       bank_fee(15)    TYPE c,
       net_amt         TYPE bsis-dmbtr,
       hotkey_code(10) TYPE c,
       remark(50)      TYPE c,

END OF gy_rep_excel.
TYPES: BEGIN OF gy_exp_excel,
       run_no(3)       TYPE c,
       name(100)       TYPE c,
       acc_bank(18)    TYPE c,
       bank_amt(15)    TYPE c,
       bank_name(50)   TYPE c,
       e_mail(50)      TYPE c,
       Beneficiary(15) TYPE c,
       bank_fee(15)    TYPE c,
       net_amt(15)     TYPE c,
       hotkey_code(15) TYPE c,
       remark(20)      TYPE c,
END OF gy_exp_excel.
TYPES: BEGIN OF gy_exp_excel_header,
       header_name(50)       TYPE c,

END OF gy_exp_excel_header.
TYPES: BEGIN OF gy_payee_code,
       lifnr          TYPE lfa1-lifnr,
       pay_name(100)  TYPE c,
       email          TYPE adr6-smtp_addr,    "E-mail Address
       product_code(3) TYPE c,    "
       bank_name            TYPE bnka-banka,        "Bank Name
       zbank_code(3)         TYPE c,                 "Bank Code
       branch_code(4)       TYPE c,                 "Branch Code
       branch_name(80)      TYPE c,                 "Branch Name
END OF gy_payee_code.
"CH04 Add by Wantanee 20210111
TYPES: BEGIN OF gy_product_code,
       product_code(3) TYPE c,    "
       z_payamt        TYPE bsis-dmbtr,
       no_of_credit    TYPE i,
END OF gy_product_code.
"CH04 End Add by Wantanee 20210111
TYPES: BEGIN OF gy_header,
        env(3)              TYPE c,
        defaultid(20)       TYPE c,
        sm                  TYPE c,
        transfertype(2)     TYPE c,
        debittype(2)        TYPE c,
        custref(32)         TYPE c,
        valudate(8)         TYPE c,
        ccy(3)              TYPE c,
        debitbranchcd(4)    TYPE c,
        debitglcd(4)        TYPE c,
        debitglsubcd(3)     TYPE c,
        debitssno(35)       TYPE c,
        debitaccountno(35)  TYPE c,
        debitsubno(2)       TYPE c,
        debitccy(3)         TYPE c,
        debitmktcd(2)       TYPE c,
  "Add by Wantanee 20140211
        chargebranchcd(4)   TYPE c,
        chargeglcd(4)       TYPE c,
        chargeglsubcd(4)    TYPE c,
        chargessno(35)      TYPE c,
        chargeaccountno(35) TYPE c,
        chargesubno(2)      TYPE c,
        chargeccy(3)        TYPE c,
        chargemktcd(2)      TYPE c,
  "End Add by Wantanee 20140211



END OF gy_header.

TYPES: BEGIN OF gy_bnka,
       banks        TYPE bnka-banks,   "Bank th
       bankl        TYPE bnka-bankl,   "Bank Key
       banka        TYPE bnka-banka,   "Bank Name
END OF gy_bnka.

TYPES: BEGIN OF gy_lfbk,
       lifnr        TYPE lfbk-lifnr,
       banks        TYPE bnka-banks,   "Bank th
       bankl        TYPE bnka-bankl,   "Bank Key
       bankn        TYPE lfbk-bankn,   "Bank Account
       bkref        TYPE lfbk-bkref,   "Branch Name
END OF gy_lfbk.

TYPES: BEGIN OF gy_payr,
      lifnr         TYPE payr-lifnr,  "Vender
      vblnr         TYPE payr-vblnr,  "Pay date
      gjahr         TYPE payr-gjahr,  "Fiscal Year
      chect         TYPE payr-chect,  "Cheque No

END OF gy_payr.

TYPES: BEGIN OF ty_dfile,
         text  TYPE text1000,
END OF ty_dfile.

TYPES: BEGIN OF gy_001,
      rec_typ(3)      TYPE c,
      com_id(12)      TYPE c,
      cust_ref(32)    TYPE c,
      mes_date(8)     TYPE c,
      mes_time(6)     TYPE c,
      channel_id(6)   TYPE c,
      batch_ref(32)   TYPE c,
END OF gy_001.
TYPES: BEGIN OF gy_002,
      rec_typ(3)              TYPE c,
      product_code(3)         TYPE c,
      value_date(8)           TYPE c,
      debit_acc_no(25)        TYPE c,
      acc_gy_debit(2)        TYPE c,
      debit_brach_code(4)     TYPE c,
      debit_curr(3)           TYPE c,
      debit_amt(16)           TYPE n,
      internal_ref(8)         TYPE c,
      no_of_credit(6)         TYPE n,
      fee_debit_acc(15)       TYPE c,
      filler(9)               TYPE c,
      media_clear_cycel       TYPE c,
      acc_typ_fee(2)          TYPE c,
      debit_branch_code_fee(4) TYPE c,
END OF gy_002.


TYPES: BEGIN OF gy_pay_003,
        rec_typ(3)            TYPE c,
        credit_sequence_no(6) TYPE n,
        credit_acc(25)        TYPE c,
        credit_amt(16)        TYPE n,
        credit_curr(3)        TYPE c,
        internal_ref(8)       TYPE c,
        wht_present(1)        TYPE c,
        inv_details_present(1) TYPE c,
        credit_advice(1)      TYPE c,
        delivery_mode(1)      TYPE c,
        pickup_location(4)    TYPE c,
        wht_form_type(2)      TYPE c,
        wht_tax_running(14)   TYPE c,
        wht_attach_no(6)      TYPE c,
        no_of_wht(2)          TYPE c,
        total_wht_amt(16)     TYPE c,
        no_of_inv(6)          TYPE c,
        total_inv_amt(16)     TYPE c,
        wht_pay_type(1)       TYPE c,
        wht_remark(40)        TYPE c,
        wht_deduct_date(8)    TYPE c,
        receive_zbank_code(3)  TYPE c,
        receive_bank_name(35) TYPE c,
        receive_branch_code(4) TYPE c,
        receive_branch_name(35) TYPE c,
        wht_signatory(1)      TYPE c,
        beneficiary(1)        TYPE c,
        cust_ref(20)          TYPE c,
        cheque_refer(1)       TYPE c,
        payment_type_code(3)  TYPE c,
        service_type(2)       TYPE c,
        remark(50)            TYPE c,
        scb_remark(18)        TYPE c,
        beneficiary_charge(2) TYPE c,

END OF gy_pay_003.


TYPES: BEGIN OF gy_pay_004,
        record_typ(3)         TYPE c,
        internal_ref(8)       TYPE c,
        credit_sequence_no(6) TYPE n,
        payee_idcard(15)      TYPE c,
        payee_name(100)       TYPE c,
        payee_addr1(70)       TYPE c,
        payee_addr2(70)       TYPE c,
        payee_addr3(70)       TYPE c,
        payee_taxid(10)       TYPE c,
        payee_name_eng(70)    TYPE c,
        payee_fax_no(10)      TYPE c,
        payee_mobile(10)      TYPE c,
        payee_e_mail(64)      TYPE c,
        payee2_name(100)      TYPE c,
        payee2_addr1(70)      TYPE c,
        payee2_addr2(70)      TYPE c,
        payee2_addr3(70)      TYPE c,
END OF gy_pay_004.

TYPES: BEGIN OF gy_999,
        record_typ(3)         TYPE c,
        total_no_of_debit(6)  TYPE n,
        total_no_of_credit(6) TYPE n,
        total_amt(16)         TYPE n,

END OF gy_999.



TYPES: BEGIN OF gy_inv,
      lifnr             TYPE bsak-lifnr,         "Vender
      augbl             TYPE bsak-augbl,         "Doc.Payment
      auggj             TYPE bsak-auggj,         "Fiscal Year
      inv(3)            TYPE c,
      defaultid(20)     TYPE c,
      reference(20)     TYPE c,
      calcfor(2)        TYPE c,
      invoicedate(8)    TYPE c,
      invoiceamount(21) TYPE c,
      vatamount(21)     TYPE c,
      vatrate(6)        TYPE c,
      whtamount(21)     TYPE c,
      whttype(8)        TYPE c,
      description(248)  TYPE c,
      gross_amount(20)  TYPE c,
END OF gy_inv.

TYPES: BEGIN OF gy_regis,
      debitbranchcd(6)        TYPE c,
      transfertype(2)         TYPE c,
      payeeno(32)             TYPE c,
      payeename(60)           TYPE c,
      address(60)             TYPE c,
      phoneno(32)             TYPE c,
      message(60)             TYPE c,
      bankcode(20)            TYPE c,
      bankname(60)            TYPE c,
      brancheregioncd(10)     TYPE c,
      branchregionname(60)    TYPE c,
      branchname(60)          TYPE c,
      accountno(32)           TYPE c,
      emailaddress(70)        TYPE c,
      zipcd(10)               TYPE c,
      faxno(15)               TYPE c,
      attention(160)          TYPE c,
      whtid(13)               TYPE c,
      whtformid(2)            TYPE c,
      cardid(13)              TYPE c,
      pickupid(3)             TYPE c,
      transactioncd(8)        TYPE c,
      inputkana(1)            TYPE c,
      ankcountryname(50)      TYPE c,
      countryname(50)         TYPE c,
      useregpayeematrix(1)    TYPE c,
      emailnotification(1)    TYPE c,
      faxnotification(1)      TYPE c,

END OF gy_regis.

TYPES: BEGIN OF gy_ZSDSFIC025,
       bankl        TYPE ZSDSFIC025-bankl,   "Bank Key
       zbank_code    TYPE ZSDSFIC025-zbank_code,  "Bank Code
END OF gy_ZSDSFIC025.

TYPES: BEGIN OF gy_lfa1,
     lifnr      TYPE lfa1-lifnr,
     adrnr      TYPE lfa1-adrnr,

END OF gy_lfa1.

TYPES: BEGIN OF gy_ZSDSFIC026,
     mandt      TYPE ZSDSFIC026-mandt,
     lifnr      TYPE ZSDSFIC026-lifnr,
     flag_email TYPE ZSDSFIC026-flag_email,
     flag_fax   TYPE ZSDSFIC026-flag_email,
     kunnr      TYPE ZSDSFIC026-kunnr,
     bankl      TYPE ZSDSFIC026-bankl,
     bankn      TYPE ZSDSFIC026-bankn,
     bkref      TYPE ZSDSFIC026-bkref,
     zbank_code  TYPE ZSDSFIC026-zbank_code,
     banka      TYPE ZSDSFIC026-banka,


END OF gy_ZSDSFIC026.

TYPES: BEGIN OF gy_bsec,
     belnr    TYPE  bsec-belnr,
     gjahr    TYPE  bsec-gjahr,
     name1    TYPE  bsec-name1,
     name2    TYPE  bsec-name2,
     stras    TYPE  bsec-stras, "street
     ort01    TYPE  bsec-ort01,  "city
     pstlz    TYPE  bsec-pstlz, "post Code
     bankn    TYPE  bsec-bankn, "Bank Account
     bankl    TYPE  bsec-bankl, "Bank Code

END OF gy_bsec.



TYPE-POOLS : vrm.
TYPE-POOLS: slis.
*&-----------------------------------------------------------------------------------*
*& D A T A
*&-----------------------------------------------------------------------------------*
DATA:     gv_formname  type TDSFNAME,
          gv_fm_name   type RS38L_FNAM,
          gv_tabix     type syst-tabix,
          gv_last_item type syst-tabix.

DATA: v_pos TYPE i.
DATA:
      gt_fieldcat     TYPE slis_t_fieldcat_alv,
      gt_events       TYPE slis_t_event,
      gt_heading      TYPE slis_t_listheader,
      gt_sort         TYPE slis_t_sortinfo_alv,
      gt_layout       TYPE slis_layout_alv,
      gt_exit(1)      TYPE c,
      gt_variant      TYPE disvariant,
      gx_variant      TYPE disvariant,
      REF_GRID        TYPE REF TO CL_GUI_ALV_GRID. "Refresh

DEFINE def_list_head.
  CLEAR: lw_listline.
  lw_listline-typ  = &1.
  lw_listline-key  = &2.
  lw_listline-info = &3.
  APPEND lw_listline TO lt_listhead.
END-OF-DEFINITION.
*&-----------------------------------------------------------------------------------*
*& D A T A Variables
*&-----------------------------------------------------------------------------------*

************************************************************************
*      W O R K - A R E A                                               *
************************************************************************

*&-----------------------------------------------------------------------------------*
*& I N T E R N A L   T A B L E S
*&-----------------------------------------------------------------------------------*
DATA: gt_itab         TYPE STANDARD TABLE OF gy_itab,
      gt_itab_regis   TYPE STANDARD TABLE OF gy_itab_regis,
      gt_bsak_pay     TYPE STANDARD TABLE OF gy_bsak_pay,
      gt_bsak_pay_tmp TYPE STANDARD TABLE OF gy_bsak_pay,
      gt_bsad_clear   TYPE STANDARD TABLE OF gy_bsad_clear,  "Add by Wantanee 20140306
      gt_bsid_clear   TYPE STANDARD TABLE OF gy_bsid_clear,"Add by Wantanee 20140306
      gt_bsak_inv     TYPE STANDARD TABLE OF gy_bsak_inv,
      gt_adrc         TYPE STANDARD TABLE OF gy_adrc,
      gt_with_item    TYPE STANDARD TABLE OF gy_with_item,
      gt_t059zt       TYPE STANDARD TABLE OF gy_t059zt,
      gt_header       TYPE STANDARD TABLE OF gy_header,
      gt_bnka         TYPE STANDARD TABLE OF gy_bnka,
      gt_lfbk         TYPE STANDARD TABLE OF gy_lfbk,
      gt_payr         TYPE STANDARD TABLE OF gy_payr,
      gt_dfile        TYPE STANDARD TABLE OF ty_dfile,
      gt_inv          TYPE STANDARD TABLE OF gy_inv,
      gt_bank         TYPE STANDARD TABLE OF gy_ZSDSFIC025,
      gt_regis        TYPE STANDARD TABLE OF gy_regis,
      gt_lfa1         TYPE STANDARD TABLE OF gy_lfa1,
      gt_ZSDSFIC026 TYPE STANDARD TABLE OF gy_ZSDSFIC026,
      gt_bsis         TYPE STANDARD TABLE OF gy_bsis,
      gt_bsec         TYPE STANDARD TABLE OF gy_bsec,
      gt_ex_excel     TYPE STANDARD TABLE OF gy_exp_excel,
      gt_rep_excel    TYPE STANDARD TABLE OF gy_rep_excel,
      gt_send_mail    TYPE STANDARD TABLE OF gy_rep_excel,
      gt_payee_code   TYPE STANDARD TABLE OF gy_payee_code,
      gt_ex_excel_h   TYPE STANDARD TABLE OF gy_exp_excel_header,
      gt_001          TYPE STANDARD TABLE OF gy_001,
      gt_002          TYPE STANDARD TABLE OF gy_002,
      gt_pay          TYPE STANDARD TABLE OF gy_pay_003,
      gt_pay_004      TYPE STANDARD TABLE OF gy_pay_004,
      gt_999          TYPE STANDARD TABLE OF gy_999,
      gt_product_code    TYPE STANDARD TABLE OF gy_product_code. "CH04 Add by WAntanee 20210111


DATA: gs_itab         TYPE gy_itab,
      gs_itab_regis   TYPE gy_itab_regis,
      gs_bsak_pay     TYPE gy_bsak_pay,
      gs_bsak_pay_tmp TYPE gy_bsak_pay,
      gs_bsad_clear   TYPE gy_bsad_clear, "Add by Wantanee 20140306
      gs_bsid_clear   TYPE gy_bsid_clear,
      gs_bsak_inv     TYPE gy_bsak_inv,
      gs_adrc         TYPE gy_adrc,
      gs_with_item    TYPE gy_with_item,
      gs_t059zt       TYPE gy_t059zt,
      gs_header       TYPE gy_header,
      gs_bnka         TYPE gy_bnka,
      gs_lfbk         TYPE gy_lfbk,
      gs_payr         TYPE gy_payr,
      gs_dfile        TYPE ty_dfile,
      gs_inv          TYPE gy_inv,
      gs_bank         TYPE gy_ZSDSFIC025,
      gs_regis        TYPE gy_regis,
      gs_lfa1         TYPE gy_lfa1,
      gs_ZSDSFIC026 TYPE gy_ZSDSFIC026,
      gs_bsis         TYPE gy_bsis,
      gs_bsec         TYPE gy_bsec,
      gs_ex_excel     TYPE gy_exp_excel,
      gs_rep_excel    TYPE gy_rep_excel,
      gs_send_mail    TYPE gy_rep_excel,
      gs_payee_code   TYPE gy_payee_code,
      gs_ex_excel_h   TYPE gy_exp_excel_header,
      gs_001          TYPE gy_001,
      gs_002          TYPE gy_002,
      gs_pay          TYPE gy_pay_003,
      gs_pay_004      TYPE gy_pay_004,
      gs_999          TYPE gy_999,
      gs_product_code    TYPE  gy_product_code. "CH04 Add by WAntanee 20210111

RANGES: r_hkont          FOR bsis-hkont.
RANGES: r_text           FOR bsak-sgtxt.

DATA: lv_post_date   TYPE bsak-augdt .
*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
*Variable
constants: gc_charx    type c value 'X',
           gc_repid    TYPE ZSDSV_GEN_C-repid VALUE 'ZR_FIAP_ADVANCE'.

CONSTANTS: gc_lang_en     TYPE c      VALUE 'E',
           gc_lang_th     TYPE c      VALUE 'T',
           gc_mark_x      TYPE c      VALUE 'X'.
CONSTANTS: gc_mask_time   TYPE char8  VALUE '__:__',
           gc_mask_date   TYPE char10 VALUE '__/__/____'.
CONSTANTS: gc_tmty_header TYPE char1  VALUE 'S'.


*&---------------------------------------------------------------------*
*& Program Variables
*&---------------------------------------------------------------------*
DATA: xscreen(1)   TYPE c.                "Output on printer or screen
DATA: lv_fm        TYPE rs38l_fnam.
*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N  ( %_ )            *
*&---------------------------------------------------------------------*
DEFINE %show. " show input field
  if screen-group1 = &1.
    screen-invisible = 0.
    screen-active = 1.
  endif.
END-OF-DEFINITION.
DEFINE %hide. " hide input field
  if screen-group1 = &1.
    screen-invisible = 1.
    screen-active = 0.
  endif.
END-OF-DEFINITION.
*&-----------------------------------------------------------------------------------*
*& S E L E C T I O N - S C R E E N
*&-----------------------------------------------------------------------------------*

*Selection Screen.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-h02 .
PARAMETERS: p_bukrs TYPE bsak-bukrs DEFAULT '1000'   .
SELECT-OPTIONS:
                s_gjahr for payr-gjahr    DEFAULT sy-datum MODIF ID SC4,
                s_blart for bkpf-blart MODIF ID SC1,  "CH5 Add by Wantanee 20210702
                s_augbl for payr-vblnr MODIF ID SC1,
                s_augdt for bsak-augdt MODIF ID SC1,
                s_ktokk for lfa1-ktokk MODIF ID SC2 DEFAULT '6000',  "CH03  Add by WAntanee
                s_lifnr for bsak-lifnr MODIF ID SC2.

PARAMETERS: p_test AS CHECKBOX  MODIF ID sc5 DEFAULT 'X',
            p_exp  AS CHECKBOX  MODIF ID sc5 USER-COMMAND ucom,                   " Download to WEB Server
            p_expt AS CHECKBOX  MODIF ID sc5 DEFAULT 'X' USER-COMMAND ucom,                   " Download to WEB Server
            p_path LIKE rlgrap-filename DEFAULT 'C:\' MODIF ID SC5,
            p_post LIKE bsak-augdt MODIF ID sc5.


SELECTION-SCREEN END OF BLOCK b2.
************************************************************************
*      A T     S E L E C T I O N     S C R E E N                       *
************************************************************************


* Event:Initialization
INITIALIZATION.
 PERFORM f_get_default_doc_type.

AT SELECTION-SCREEN OUTPUT.
* Set Screen
  PERFORM f_modify_screen.
  lv_post_date = sy-datlo + 2.
  p_post = lv_post_date.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.


       PERFORM get_path_name CHANGING p_path.


TOP-OF-PAGE.
*  perform print_header.


START-OF-SELECTION.

        PERFORM get_data_autopayment.
        PERFORM map_data_autopayment.
        PERFORM map_send_data_mail.
        IF NOT p_exp IS INITIAL.
          PERFORM map_exp_data.
          IF  p_test IS INITIAL.
            PERFORM send_mail_to_employee.
          ENDIF.
        ELSEIF NOT p_expt IS INITIAL..
          perform map_exp_data_text using p_path.
          IF  p_test IS INITIAL.
              PERFORM send_mail_to_employee.
          ENDIF.
        ELSEIF NOT p_test IS INITIAL.
          PERFORM map_exp_data.
        ENDIF.




END-OF-SELECTION.


        IF NOT gt_itab IS INITIAL.

              IF ( NOT p_exp IS INITIAL ) OR ( NOT p_expt IS INITIAL ).
                 "IF NOT p_test IS INITIAL.
                    PERFORM f_download_file USING p_path.
                 "ENDIF.
*                DELETE ADJACENT DUPLICATES FROM gt_itab.
                 PERFORM sub_main.
              ELSE.
                PERFORM sub_main.
              ENDIF.

        ELSE.
*           MESSAGE s004.
        ENDIF.



*&---------------------------------------------------------------------*
*&      Form  F_GET_DEFAULT_MOVEMENT_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_default_doc_type .
  DATA: ls_blart LIKE LINE OF s_blart.

  CLEAR s_blart[].

    CLEAR ls_blart.
    ls_blart-sign    = 'I'.
    ls_blart-option  = 'EQ'.
    ls_blart-low     = 'KZ'.
    APPEND ls_blart TO s_blart.

    CLEAR ls_blart.
    ls_blart-sign    = 'I'.
    ls_blart-option  = 'EQ'.
    ls_blart-low     = 'K4'.
    APPEND ls_blart TO s_blart.

    CLEAR ls_blart.
    ls_blart-sign    = 'I'.
    ls_blart-option  = 'EQ'.
    ls_blart-low     = 'KU'.
    APPEND ls_blart TO s_blart.


ENDFORM.                    " F_GET_DEFAULT_MOVEMENT_TYPE



**&---------------------------------------------------------------------*
**&      Form  f_modify_screen
**&---------------------------------------------------------------------*


FORM f_modify_screen.


    LOOP AT SCREEN.
      %show 'SC1'.
      %show 'SC2'.
      %show 'SC4'.
      %show 'SC5'.
      %hide 'SC3'.
         IF NOT p_exp IS INITIAL.
            p_path = 'C:\test.xls'.
         ELSEIF NOT p_expt IS INITIAL.
            p_path = 'C:\test.txt'.
         ENDIF.

    MODIFY SCREEN.

    ENDLOOP.


ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  SUB_MAIN
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM sub_main .

       GET TIME.
       PERFORM: build_catalog CHANGING gt_fieldcat[],
                build_event   USING gt_events,
                build_layout  USING gt_layout,
                build_sort,
                display_output.





ENDFORM.

**&---------------------------------------------------------------------*
**&      Form  GET_DATA
**&---------------------------------------------------------------------*
FORM get_data_autopayment .
  CLEAR: gs_header.



 IF NOT s_augbl IS INITIAL.



            SELECT a~lifnr a~gjahr
               a~augbl
*              a~buzei
               a~auggj
               d~budat a~shkzg
               sum( a~dmbtr )
               sum( a~qbshb )
               a~sgtxt
               a~hkont
               b~adrnr
        INTO TABLE gt_bsak_pay
        FROM bsak AS a INNER JOIN lfa1 AS b
                       ON ( a~lifnr EQ b~lifnr )
                       INNER JOIN bkpf AS d
                       ON ( a~augbl EQ d~belnr
                       AND  a~auggj EQ d~gjahr )
*                       INNER JOIN bsis AS f
*                       ON ( a~belnr EQ f~belnr
*                       AND  a~gjahr EQ f~gjahr )
        WHERE a~bukrs  EQ p_bukrs
          AND a~augbl  IN s_augbl
          AND a~auggj  IN s_gjahr
*          AND b~ktokk EQ '6000'  "CH03 delete Wantanee
          AND b~ktokk IN s_ktokk  "CH03 Add Wantanee
          "CH5 Remove by wantanee 20210702
*          AND ( d~blart EQ 'KZ'
*           OR   d~blart EQ 'K4' )
          "CH5 End remove by Wantanee 20210702
          AND  d~blart IN s_blart  ""CH5 add by Wantanee 20210702
          AND d~budat  IN s_augdt
*          AND a~lifnr  IN s_lifnr
          AND d~stblg  NOT LIKE '29%'
          AND a~augbl  EQ a~belnr
          AND a~shkzg  EQ 'S'
        GROUP BY a~lifnr a~gjahr
              a~augbl
*              a~buzei
              a~auggj
               d~budat a~shkzg

               a~sgtxt
               a~hkont
               b~adrnr.

*          AND f~hkont IN r_hkont.

 ELSE.


        SELECT a~lifnr a~gjahr
               a~augbl
*              a~buzei
               a~auggj
               d~budat a~shkzg
               sum( a~dmbtr )
               sum( a~qbshb )
               a~sgtxt
               a~hkont
               b~adrnr
        INTO TABLE gt_bsak_pay
        FROM bsak AS a INNER JOIN lfa1 AS b
                       ON ( a~lifnr EQ b~lifnr )
                       INNER JOIN bkpf AS d
                       ON ( a~augbl EQ d~belnr
                       AND  a~auggj EQ d~gjahr )
*                       INNER JOIN bsis AS f
*                       ON ( a~belnr EQ f~belnr
*                       AND  a~gjahr EQ f~gjahr )
        WHERE a~bukrs  EQ p_bukrs
          AND a~augbl  IN s_augbl
          AND a~auggj  IN s_gjahr
*          AND b~ktokk EQ '6000'  "CH03 delete Wantanee
          AND b~ktokk IN s_ktokk  "CH03 Add Wantanee
          "CH5 Remove by wantanee 20210702
*          AND ( d~blart EQ 'KZ'
*           OR   d~blart EQ 'K4' )
          "CH5 End remove by Wantanee 20210702
          AND  d~blart IN s_blart  ""CH5 add by Wantanee 20210702
          AND d~budat  IN s_augdt
          AND a~lifnr  IN s_lifnr
          AND d~stblg  NOT LIKE '29%'
          AND a~augbl  EQ a~belnr
          AND a~shkzg  EQ 'S'
        GROUP BY a~lifnr a~gjahr
              a~augbl
*              a~buzei
              a~auggj
               d~budat a~shkzg

               a~sgtxt
               a~hkont
               b~adrnr.
*          AND f~hkont IN r_hkont.


 ENDIF.


    DELETE ADJACENT DUPLICATES FROM gt_bsak_pay.





  IF NOT gt_bsak_pay IS INITIAL.
*          SORT gt_bsak_pay BY lifnr gjahr augbl .
*
*          LOOP AT gt_bsak_pay INTO gs_bsak_pay.
*             CLEAR: gs_bsak_pay_tmp.
*              gs_bsak_pay_tmp-lifnr = gs_bsak_pay-lifnr.
*              gs_bsak_pay_tmp-gjahr = gs_bsak_pay-gjahr.
*              gs_bsak_pay_tmp-augbl = gs_bsak_pay-augbl.
*              gs_bsak_pay_tmp-auggj = gs_bsak_pay-auggj.
*              gs_bsak_pay_tmp-shkzg = gs_bsak_pay-shkzg.
*              gs_bsak_pay_tmp-dmbtr = gs_bsak_pay-dmbtr.
*              gs_bsak_pay_tmp-qbshb = gs_bsak_pay-qbshb.
*              gs_bsak_pay_tmp-sgtxt = gs_bsak_pay-sgtxt.
*              gs_bsak_pay_tmp-adrnr = gs_bsak_pay-adrnr.
*
*              COLLECT  gs_bsak_pay_tmp INTO   gt_bsak_pay_tmp.
*
*          ENDLOOP.
*
*          CLEAR: gt_bsak_pay,gs_bsak_pay.
*          MOVE gt_bsak_pay_tmp TO gt_bsak_pay.





      SELECT a~lifnr a~augbl a~auggj a~belnr
             a~gjahr a~buzei a~budat a~shkzg
             a~dmbtr a~qbshb a~sgtxt
*             d~xblnr
      INTO TABLE gt_bsak_inv
      FROM bsak AS a
*        INNER JOIN bkpf AS d
*                     ON ( a~belnr EQ d~belnr
*                     AND  a~gjahr EQ d~gjahr )
      FOR ALL ENTRIES IN gt_bsak_pay
      WHERE a~lifnr EQ gt_bsak_pay-lifnr
        AND a~auggj EQ gt_bsak_pay-auggj
        AND  a~augbl EQ gt_bsak_pay-augbl
        AND a~augbl NE a~belnr.




      IF NOT gt_bsak_inv IS INITIAL.

          SELECT lifnr vblnr gjahr chect
          INTO TABLE gt_payr
          FROM payr
          FOR ALL ENTRIES IN gt_bsak_inv
          WHERE lifnr EQ gt_bsak_inv-lifnr
            AND vblnr EQ gt_bsak_inv-augbl
            AND gjahr EQ gt_bsak_inv-auggj.

          SELECT   belnr gjahr buzei witht wt_withcd
          INTO  TABLE gt_with_item
          FROM with_item
          FOR ALL ENTRIES IN gt_bsak_inv
          WHERE belnr EQ gt_bsak_inv-belnr
            AND buzei EQ gt_bsak_inv-buzei
            AND gjahr EQ gt_bsak_inv-gjahr.



      ENDIF.



       SELECT b~lifnr  a~augbl
              a~auggj a~belnr a~gjahr
              a~buzei a~budat a~shkzg a~dmbtr
              a~sgtxt a~xblnr a~rebzg
       INTO TABLE gt_bsad_clear
       FROM bsad AS a INNER JOIN bsak AS b
                      ON ( a~augbl EQ b~augbl
                      AND  a~gjahr EQ b~gjahr )
       FOR ALL ENTRIES IN gt_bsak_pay
       WHERE a~augbl EQ gt_bsak_pay-augbl
       AND  a~belnr EQ gt_bsak_pay-augbl
       AND  a~gjahr EQ gt_bsak_pay-gjahr.
      " AND  a~shkzg EQ 'H'.


       DELETE ADJACENT DUPLICATES FROM gt_bsad_clear.

       LOOP AT gt_bsad_clear INTO gs_bsad_clear.
            gs_bsak_inv-lifnr = gs_bsad_clear-lifnr.
            gs_bsak_inv-augbl = gs_bsad_clear-augbl.
            gs_bsak_inv-auggj = gs_bsad_clear-auggj.
            gs_bsak_inv-belnr = gs_bsad_clear-belnr.
            gs_bsak_inv-gjahr = gs_bsad_clear-gjahr.
            gs_bsak_inv-buzei = gs_bsad_clear-buzei.
            gs_bsak_inv-budat = gs_bsad_clear-budat.
            IF gs_bsad_clear-shkzg = 'H'.
               gs_bsak_inv-shkzg = 'S'.
            ELSE.
                gs_bsak_inv-shkzg = 'H'.
            ENDIF.

            gs_bsak_inv-dmbtr = gs_bsad_clear-dmbtr.
            gs_bsak_inv-xblnr = gs_bsad_clear-xblnr.

            APPEND gs_bsak_inv TO gt_bsak_inv.



       ENDLOOP.




       SELECT b~lifnr  a~augbl
              a~auggj a~belnr a~gjahr
              a~buzei a~budat a~shkzg a~dmbtr
              a~sgtxt a~xblnr a~rebzg
       INTO TABLE gt_bsid_clear
       FROM bsid AS a INNER JOIN bsak AS b
                      ON ( a~belnr EQ b~augbl
                      AND  a~gjahr EQ b~gjahr
                       )
       FOR ALL ENTRIES IN gt_bsak_pay
       WHERE a~belnr EQ gt_bsak_pay-augbl
       AND   a~gjahr EQ gt_bsak_pay-gjahr
       AND  a~shkzg EQ 'H'.


       DELETE ADJACENT DUPLICATES FROM gt_bsid_clear.

       LOOP AT gt_bsid_clear INTO gs_bsid_clear.
            gs_bsak_inv-lifnr = gs_bsid_clear-lifnr.
            gs_bsak_inv-augbl = gs_bsid_clear-belnr.
            gs_bsak_inv-auggj = gs_bsid_clear-gjahr.
            gs_bsak_inv-belnr = gs_bsid_clear-belnr.
            gs_bsak_inv-gjahr = gs_bsid_clear-gjahr.
            gs_bsak_inv-buzei = gs_bsid_clear-buzei.
            gs_bsak_inv-budat = gs_bsid_clear-budat.
            IF gs_bsid_clear-shkzg = 'H'.
               gs_bsak_inv-shkzg = 'S'.
            ENDIF.

            gs_bsak_inv-dmbtr = gs_bsid_clear-dmbtr.
            gs_bsak_inv-xblnr = gs_bsid_clear-rebzg.

            APPEND gs_bsak_inv TO gt_bsak_inv.



       ENDLOOP.

      "Add by Wantanee 20140707


        SELECT hkont belnr gjahr buzei budat
               shkzg dmbtr sgtxt xref3
        INTO TABLE gt_bsis
        FROM bsis
        FOR ALL ENTRIES IN gt_bsak_pay
        WHERE belnr EQ gt_bsak_pay-augbl
          AND gjahr EQ gt_bsak_pay-auggj
          AND qsskz EQ ''
          AND hkont NE gt_bsak_pay-hkont.
*          AND xref3 NE gt_bsak_pay-augbl.

         DELETE ADJACENT DUPLICATES FROM gt_bsis.

             LOOP AT gt_bsis INTO gs_bsis .

                      READ TABLE gt_bsak_pay INTO gs_bsak_pay WITH KEY augbl = gs_bsis-belnr
                                                                       auggj = gs_bsis-gjahr.
                            IF sy-subrc EQ 0.

                               IF gs_bsis-xref3 NE gs_bsak_pay-augbl.
                                   gs_bsak_inv-lifnr = gs_bsak_pay-lifnr.
                                   gs_bsak_inv-augbl = gs_bsis-belnr.
                                   gs_bsak_inv-auggj = gs_bsis-gjahr.
                                   gs_bsak_inv-belnr = gs_bsis-belnr.
                                   gs_bsak_inv-gjahr = gs_bsis-gjahr.
                                   gs_bsak_inv-buzei = gs_bsis-buzei.
                                   gs_bsak_inv-budat = gs_bsis-budat.
                                   IF gs_bsis-shkzg = 'H'.
                                      gs_bsak_inv-shkzg = 'S'.
                                   ELSE.
                                       gs_bsak_inv-shkzg = 'H'.
                                   ENDIF.
                                   gs_bsak_inv-dmbtr = gs_bsis-dmbtr.

                                   APPEND gs_bsak_inv TO gt_bsak_inv.
                               ENDIF.

                            ENDIF.



             ENDLOOP.

        "Add by Wantanee 20140707

  ENDIF.




    SELECT banks bankl banka
    INTO TABLE gt_bnka
    FROM bnka
    WHERE banks EQ 'TH'.

*             witht               TYPE t059zt-witht,         "Wht
*       wt_withcd           TYPE t059zt-wt_withcd,     "Type wht
*       text40              TYPE t059zt-text40,        "Text

    SELECT witht wt_withcd text40
    INTO TABLE gt_t059zt
    FROM t059zt
    WHERE spras = 'E'
      AND land1 = 'TH'
      AND witht = '11'.


    SELECT bankl zbank_code
    INTO TABLE gt_bank
    FROM ZSDSFIC025.





ENDFORM.                    " GET_DATA
**&---------------------------------------------------------------------*
**&      Form  Map Data
**&---------------------------------------------------------------------*
FORM map_data_autopayment .

  DATA: lv_replace(100) TYPE c.
  DATA: lv_runno TYPE i.
  DATA: lv_runno_payee TYPE i.
  DATA: lv_invamt TYPE bsak-dmbtr,        "AP Invoice Amt
        lv_whtamt TYPE bsak-qbshb.        "WHT  amt
  DATA: lv_payamt TYPE  bsak-dmbtr.  "Add by Wantanee 20140306
  DATA: lv_remark_temp(55) TYPE c.
  DATA: lv_sumpay_amt TYPE bsak-dmbtr.
  DATA: lv_count_product_code TYPE i.  "CH04 Add by WAntanee 20210111



  CLEAR:gt_itab,gs_itab,gs_bsak_pay,lv_runno,lv_sumpay_amt.
  SORT gt_bsak_pay BY augbl.
  SORT gt_bsak_inv BY augbl belnr.

     LOOP AT gt_bsak_pay INTO gs_bsak_pay.

         CLEAR: lv_invamt,lv_whtamt,lv_payamt,gs_ex_excel,lv_remark_temp.

         lv_runno = lv_runno + 1.

         gs_itab-run_no = lv_runno.
         gs_itab-payee_no = gs_bsak_pay-lifnr.
         READ TABLE gt_ZSDSFIC026 INTO gs_ZSDSFIC026 WITH KEY lifnr = gs_bsak_pay-lifnr.
              IF sy-subrc EQ 0.
                 gs_itab-status_regis = ''.
              ELSE.
                 gs_itab-status_regis = 'NO Regis.'.
              ENDIF.
         PERFORM get_vender USING gs_bsak_pay-lifnr gs_bsak_pay-adrnr gs_bsak_pay-augbl gs_bsak_pay-gjahr
                            CHANGING gs_itab-payee_name gs_itab-payee_addr
                                     gs_itab-zipcode
                                     gs_itab-payee_phone gs_itab-fax_no
                                     gs_itab-email.

         PERFORM get_vender_bank USING   gs_bsak_pay-lifnr gs_bsak_pay-augbl gs_bsak_pay-gjahr
                                 CHANGING gs_itab-bank_name
                                          gs_itab-bank_account
                                          gs_itab-branch_name
                                          gs_itab-zbank_code
                                          gs_itab-branch_code
                                          gs_itab-bankl.


         gs_itab-pay_doc = gs_bsak_pay-augbl.
         gs_itab-pay_date = gs_bsak_pay-budat.
*         gs_itab-pay_amt = gs_bsak_pay-dmbtr - gs_bsak_pay-qbshb .
         READ TABLE gt_payr INTO gs_payr WITH KEY lifnr = gs_bsak_pay-lifnr
                                                  vblnr = gs_bsak_pay-augbl
                                                  gjahr = gs_bsak_pay-auggj.
              IF sy-subrc EQ 0.
                 gs_itab-cheque_no = gs_payr-chect.


              ENDIF.

         CLEAR: gs_bsak_inv.


         LOOP AT gt_bsak_inv INTO gs_bsak_inv WHERE augbl EQ gs_bsak_pay-augbl
                                                AND auggj EQ gs_bsak_pay-auggj
                                                AND lifnr EQ gs_bsak_pay-lifnr.

              IF gs_bsak_inv-shkzg EQ 'S'.
                 gs_bsak_inv-dmbtr = gs_bsak_inv-dmbtr * -1.
                 gs_bsak_inv-qbshb = gs_bsak_inv-qbshb * -1.

              ENDIF.
              gs_itab-ap_inv = gs_bsak_inv-belnr.
              gs_itab-ap_inv_date = gs_bsak_inv-budat.
              gs_itab-ap_inv_amt = gs_bsak_inv-dmbtr.
              lv_invamt = lv_invamt + gs_bsak_inv-dmbtr.
              gs_itab-ap_wht_amt = gs_bsak_inv-qbshb.
              lv_whtamt = lv_whtamt + gs_bsak_inv-qbshb.
              gs_itab-ap_inv_text = gs_bsak_inv-sgtxt.

              gs_itab-pay_amt = gs_bsak_inv-dmbtr - gs_bsak_inv-qbshb.  "Remove by Wantanee 20140306
              lv_payamt = lv_invamt - lv_whtamt.

              READ TABLE gt_with_item INTO gs_with_item WITH KEY belnr = gs_bsak_inv-belnr
                                                                 buzei = gs_bsak_inv-buzei
                                                                 gjahr = gs_bsak_inv-gjahr.
                   IF sy-subrc EQ 0.

                     READ TABLE gt_t059zt INTO gs_t059zt WITH KEY wt_withcd = gs_with_item-wt_withcd.

                          IF sy-subrc EQ 0.
                             CONCATENATE gs_t059zt-wt_withcd gs_t059zt-text40 INTO gs_itab-ap_wht_typ SEPARATED BY '-'.

                          ELSE.
                            gs_itab-ap_wht_typ = ''.
                          ENDIF.
                   ELSE.
                     gs_itab-ap_wht_typ = ''.


                   ENDIF.

              APPEND gs_itab TO gt_itab.

                gs_inv-augbl = gs_bsak_inv-augbl.
                gs_inv-auggj = gs_bsak_inv-auggj.
                gs_inv-lifnr = gs_bsak_inv-lifnr.



                gs_inv-inv = 'INV'.
                gs_inv-defaultid = ''.
                gs_inv-reference = gs_itab-ap_inv.
                gs_inv-calcfor = 'NC'.
                gs_inv-invoicedate = gs_itab-ap_inv_date.

                CLEAR:lv_replace.
                lv_replace = gs_itab-ap_inv_amt.
                SHIFT lv_replace LEFT DELETING LEADING space.
                CONDENSE lv_replace NO-GAPS.
                CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                  CHANGING
                    VALUE         = lv_replace.
                gs_inv-invoiceamount = lv_replace.


                gs_inv-vatamount = ''.
                gs_inv-vatrate = ''.

                CLEAR:lv_replace.

                lv_replace = gs_itab-ap_wht_amt.
                SHIFT lv_replace LEFT DELETING LEADING space.
                CONDENSE lv_replace NO-GAPS.
                CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                  CHANGING
                    VALUE         = lv_replace.
                gs_inv-whtamount = lv_replace.

*                CONCATENATE '"' gs_itab-ap_wht_typ '"' INTO gs_inv-whttype.
                gs_inv-whttype = '99'.

                CONCATENATE '"' gs_itab-ap_inv_text '"' INTO gs_inv-description.
                gs_inv-gross_amount = ''.  "Add by Wantanee 20140203

                APPEND gs_inv TO gt_inv.

                IF lv_remark_temp IS INITIAL.
                   lv_remark_temp =  gs_bsak_inv-belnr.
                ELSE.
                  CONCATENATE lv_remark_temp gs_bsak_inv-belnr INTO lv_remark_temp SEPARATED BY ','.
                ENDIF.


         ENDLOOP.


                 gs_rep_excel-lifnr = gs_bsak_pay-lifnr.
                 gs_rep_excel-augbl = gs_bsak_pay-augbl.

                 gs_rep_excel-name = gs_itab-payee_name.
                 lv_replace = gs_itab-bank_account.
                 REPLACE ALL OCCURRENCES OF REGEX '-' IN lv_replace WITH '' .
                 CONDENSE lv_replace NO-GAPS.
                 gs_rep_excel-acc_bank = lv_replace.

                 gs_rep_excel-bank_amt = lv_payamt.

                 lv_sumpay_amt = lv_sumpay_amt + lv_payamt.


                 CONCATENATE gs_itab-zbank_code gs_itab-bank_name INTO gs_rep_excel-bank_name SEPARATED BY ':'.
                 gs_rep_excel-e_mail = gs_itab-email.
                 gs_rep_excel-Beneficiary = 'N'.
                 gs_rep_excel-bank_fee = ''.
                 gs_rep_excel-net_amt = gs_rep_excel-bank_amt.
                 gs_rep_excel-hotkey_code = ''.
                 gs_rep_excel-remark = lv_remark_temp.




             APPEND gs_rep_excel TO gt_rep_excel.



                  gs_payee_code-lifnr = gs_bsak_pay-lifnr.
                  gs_payee_code-pay_name = gs_itab-payee_name.
                  gs_payee_code-email = gs_itab-email.
                  " CH04 Add by Wantanee 20210111
                  IF gs_itab-zbank_code EQ '014'.
                      gs_payee_code-product_code = 'PAY'.
                      gs_product_code-product_code =   'PAY'. "
                  ELSE.
                      gs_payee_code-product_code = 'MCL'.
                      gs_product_code-product_code =   'MCL'.
                  ENDIF.
                  gs_payee_code-bank_name = gs_itab-bank_name.
                  gs_payee_code-zbank_code = gs_itab-zbank_code.
                  gs_payee_code-branch_code = gs_itab-branch_code.
                  gs_payee_code-branch_name = gs_itab-branch_name.
                  " CH04 End Add by Wantanee 20210111
                  APPEND gs_payee_code TO gt_payee_code.

                  gs_product_code-z_payamt = lv_payamt.

                  COLLECT gs_product_code INTO gt_product_code.  "CH04 Add by Wantanee 20210111



     ENDLOOP.

     SORT gt_payee_code.
     DELETE ADJACENT DUPLICATES FROM gt_payee_code.
     CLEAR : lv_runno_payee.
     LOOP AT gt_payee_code INTO gs_payee_code.
        CLEAR: gs_product_code.
        lv_runno_payee = lv_runno_payee + 1.

        IF gs_payee_code-zbank_code EQ '014'.
            gs_product_code-product_code =   'PAY'. "
        ELSE.
            gs_product_code-product_code =   'MCL'.
        ENDIF.
        gs_product_code-no_of_credit = 1.
        COLLECT gs_product_code INTO gt_product_code.

     ENDLOOP.


     "Type header 001
          gs_001-rec_typ = '001'.
          gs_001-com_id = 'sdks932'.
          gs_001-cust_ref = 'employee'.
          gs_001-mes_date = sy-datum.
          gs_001-mes_time = '000000'.
          gs_001-channel_id = 'BCM'.
          APPEND gs_001 TO gt_001.

     "Type header 002
     break wantanee.

     "CH04 Edit by Wantanee 20210111

        SORT gt_product_code.
        DELETE ADJACENT DUPLICATES FROM gt_product_code.

        CLEAR: lv_count_product_code.
        LOOP AT gt_product_code INTO gs_product_code.

           lv_count_product_code = lv_count_product_code + 1.

          gs_002-rec_typ = '002'.
*          gs_002-product_code = 'PAY'.  "CH04 Edit by WAntanee 20210111
          gs_002-product_code = gs_product_code-product_code. "CH04 Add by WAntanee 20210111
          gs_002-value_date = p_post.
          gs_002-debit_acc_no = '0893008932'.
          gs_002-acc_gy_debit = '03'.
          gs_002-debit_brach_code = '0089'.
          gs_002-debit_curr = 'THB'.

*          gs_002-debit_amt = ( lv_sumpay_amt * 1000 ).  "CH04 Remove by Wantanee 20210127
          gs_002-debit_amt = ( gs_product_code-z_payamt * 1000 ).  "CH04 Add by Wantanee 20210127
*          gs_002-internal_ref = '00000001'.  "CH04 Edit by WAntanee 20210111
          "CH04 Add by WAntanee 20210111
          IF lv_count_product_code = 1.
             gs_002-internal_ref = '00000001'.
          ELSE.
             gs_002-internal_ref = '00000002'.
          ENDIF.
          "CH04 End Add by WAntanee 20210111
*          gs_002-no_of_credit = lv_runno_payee.
          gs_002-no_of_credit = gs_product_code-no_of_credit.
          gs_002-fee_debit_acc = '0893008932'.
          gs_002-acc_typ_fee = '03'.
          gs_002-debit_branch_code_fee = '0089'.

          APPEND gs_002 TO gt_002.

       ENDLOOP.






          gs_999-record_typ = '999'.
          "CH04 Add by WAntanee 20210111
          IF lv_count_product_code = 1.
             gs_999-total_no_of_debit = '000001'.
          ELSE.
             gs_999-total_no_of_debit = '000002'.
          ENDIF.
          "CH04 End Add by WAntanee 20210111
*          gs_999-total_no_of_credit = lv_runno.
          gs_999-total_no_of_credit = lv_runno_payee.
          gs_999-total_amt = ( lv_sumpay_amt * 1000 ).

          APPEND gs_999 TO gt_999.


ENDFORM.                    " MAP_DATA
**&---------------------------------------------------------------------*
**&      Form  Map Data
**&---------------------------------------------------------------------*
FORM map_exp_data .

  DATA: lv_replace(100) TYPE c.
  DATA: lv_runno TYPE i.
  DATA: lv_invamt TYPE bsak-dmbtr,        "AP Invoice Amt
        lv_whtamt TYPE bsak-qbshb.        "WHT  amt
  DATA: lv_payamt TYPE  bsak-dmbtr.
  DATA: lv_remark(50) TYPE c.




  CLEAR:lv_runno.
  SORT gt_rep_excel BY lifnr.

     LOOP AT gt_payee_code INTO gs_payee_code.
          CLEAR: lv_remark,lv_payamt.
          lv_runno = lv_runno + 1.
          gs_rep_excel-lifnr = gs_payee_code-lifnr.

          LOOP AT gt_rep_excel INTO gs_rep_excel WHERE lifnr EQ gs_payee_code-lifnr.


                  gs_ex_excel-name = gs_rep_excel-name .
                  gs_ex_excel-acc_bank = gs_rep_excel-acc_bank .
                  lv_payamt = lv_payamt + gs_rep_excel-bank_amt.
                  gs_ex_excel-bank_name = gs_rep_excel-bank_name .
                  gs_ex_excel-e_mail = gs_rep_excel-e_mail .

                  CONCATENATE lv_remark gs_rep_excel-augbl INTO lv_remark SEPARATED BY SPACE.

          ENDLOOP.
         gs_ex_excel-run_no = lv_runno.
         gs_ex_excel-bank_amt = lv_payamt.
         gs_ex_excel-Beneficiary = 'N'.
         gs_ex_excel-bank_fee = ''.
         gs_ex_excel-net_amt = lv_payamt.
         gs_ex_excel-hotkey_code = ''.
         gs_ex_excel-remark = lv_remark.

         APPEND gs_ex_excel TO gt_ex_excel.

     ENDLOOP.




      gs_ex_excel_h-header_name = 'No.'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'Account Name'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'Account No.'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'Amount'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'Bank Name'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'EMail'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'Beneficiary charge'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'Bank Fee'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'Net Amount'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'HOTKEY (CODE)'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.
      gs_ex_excel_h-header_name = 'REMARK'.
      APPEND gs_ex_excel_h TO gt_ex_excel_h.









ENDFORM.                    " MAP_DATA

**&---------------------------------------------------------------------*
**&      Form  Map Data
**&---------------------------------------------------------------------*
FORM map_send_data_mail .

  DATA: lv_replace(100) TYPE c.
  DATA: lv_runno TYPE i.
  DATA: lv_invamt TYPE bsak-dmbtr,        "AP Invoice Amt
        lv_whtamt TYPE bsak-qbshb.        "WHT  amt
  DATA: lv_payamt TYPE  bsak-dmbtr.
  DATA: lv_remark(50) TYPE c.




  CLEAR:lv_runno.
  SORT gt_rep_excel BY lifnr.

     LOOP AT gt_payee_code INTO gs_payee_code.
          CLEAR: lv_remark,lv_payamt.
          lv_runno = lv_runno + 1.
          gs_send_mail-lifnr = gs_payee_code-lifnr.

          LOOP AT gt_rep_excel INTO gs_rep_excel WHERE lifnr EQ gs_payee_code-lifnr.


                  gs_send_mail-name = gs_rep_excel-name .
                  gs_send_mail-acc_bank = gs_rep_excel-acc_bank .
                  lv_payamt = lv_payamt + gs_rep_excel-bank_amt.
                  gs_send_mail-bank_name = gs_rep_excel-bank_name .
                  gs_send_mail-e_mail = gs_rep_excel-e_mail .

                  CONCATENATE lv_remark gs_rep_excel-augbl INTO lv_remark SEPARATED BY SPACE.

          ENDLOOP.
         gs_send_mail-bank_amt = lv_payamt.
         gs_send_mail-Beneficiary = 'N'.
         gs_send_mail-bank_fee = ''.
         gs_send_mail-net_amt = lv_payamt.
         gs_send_mail-hotkey_code = ''.
         gs_send_mail-remark = lv_remark.

         APPEND gs_send_mail TO gt_send_mail.

     ENDLOOP.








ENDFORM.                    " MAP_DATA
**&---------------------------------------------------------------------*
**&      Form  Send mail to employee
**&---------------------------------------------------------------------*
FORM send_mail_to_employee .

  DATA: lv_post_date(10) TYPE c,
        lv_e_mail TYPE SO_RECNAME,
        lv_name   TYPE ZREPNAM,
        lv_doc(10)    TYPE c.


  SORT gt_rep_excel BY lifnr.
  LOOP AT gt_payee_code INTO gs_payee_code.

*        lv_post_date = p_post.
        CONCATENATE p_post+6(2) p_post+4(2) p_post+0(4) INTO  lv_post_date SEPARATED BY '.'.

*                LOOP AT gt_rep_excel INTO gs_rep_excel WHERE lifnr EQ gs_payee_code-lifnr.
*                    lv_e_mail = gs_payee_code-email.
*                    lv_name = gs_payee_code-pay_name.
*                    lv_doc = gs_rep_excel-augbl.
*                           CALL FUNCTION 'Z_SEND_EMAIL_ADV'
*                               EXPORTING
*                                 P_RECIEVER       = lv_e_mail
*                                 P_REPNAM         = lv_name
*                                 P_DOCNR          = lv_doc
*                                 P_PAYDATE        = lv_post_date
*                                 P_LIFNR          = gs_payee_code-lifnr
*                               TABLES
*                                 I_DETAIL         = gt_rep_excel
*                                 I_INV            = gt_inv.                          .
*
*                ENDLOOP.
        lv_e_mail = gs_payee_code-email.
        lv_name = gs_payee_code-pay_name.
*        lv_doc = gs_rep_excel-augbl.

                             CALL FUNCTION 'Z_SEND_EMAIL_ADV'
                               EXPORTING
                                 P_RECIEVER       = lv_e_mail
                                 P_REPNAM         = lv_name
                                 P_DOCNR          = ''
                                 P_PAYDATE        = lv_post_date
                                 P_LIFNR          = gs_payee_code-lifnr
                               TABLES
                                 I_DETAIL         = gt_rep_excel
                                 I_INV            = gt_inv.



   ENDLOOP.















ENDFORM.                    " MAP_DATA
**&---------------------------------------------------------------------*
**&      Form  VENDER
**&---------------------------------------------------------------------*
FORM get_vender USING    p_lifnr TYPE lfa1-lifnr
                         p_adrnr TYPE lfa1-adrnr
                         p_belnr TYPE bsec-belnr
                         p_gjahr TYPE bsec-gjahr
                  CHANGING value(p_name) TYPE c
                           value(p_addr) TYPE c
                           value(p_post_code) TYPE adrc-post_code1
                           value(p_tel)  TYPE c
                           value(p_fax)  TYPE c
                           value(p_email) TYPE c.

DATA: p_name_tha(200) TYPE c.
     CLEAR:p_name_tha,p_name,p_addr,p_post_code,p_email.
     DATA: lv_name1 TYPE bsec-name1,
           lv_name2 TYPE bsec-name2,
           lv_street TYPE bsec-STRAS,
           lv_city2 TYPE bsec-ORT01,
           lv_post_code TYPE bsec-PSTLZ.
     CLEAR: lv_name1,lv_name2,lv_street,lv_city2,lv_post_code.


          SELECT addrnumber name1 name2
                 street str_suppl3 location
                 city2  city1 post_code1 tel_number
                 fax_number nation
          INTO TABLE gt_adrc
          FROM adrc
          WHERE addrnumber = p_adrnr.
*           AND nation = 'I'.

         IF ( p_lifnr EQ 'OT01' OR p_lifnr EQ 'OT02' ).

               SELECT SINGLE name1 name2 STRAS ORT01 PSTLZ
                 INTO (lv_name1,lv_name2,lv_street,lv_city2,lv_post_code)
                 FROM BSEC
                 WHERE belnr EQ p_belnr
                 AND gjahr EQ p_gjahr.

                 CONCATENATE lv_name1  lv_name2 INTO p_name.
                 CONCATENATE lv_street lv_city2 INTO p_addr SEPARATED BY SPACE.
                 p_post_code = lv_post_code.
         ELSEIF p_lifnr NE 'OT01'.
              READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = p_adrnr
                                                       nation = ''.
                IF sy-subrc EQ 0.
                    CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name.
                    CONCATENATE  gs_adrc-street  gs_adrc-str_suppl3
                                 gs_adrc-location  gs_adrc-city2
                                  gs_adrc-city1 INTO p_addr SEPARATED BY SPACE.
                    p_tel = gs_adrc-tel_number.
                    p_fax = gs_adrc-fax_number.
                    p_post_code = gs_adrc-post_code1.
                ENDIF.


          ENDIF.

          SELECT SINGLE smtp_addr
          INTO (p_email)
          FROM adr6
          WHERE addrnumber = p_adrnr.

ENDFORM. "


**&---------------------------------------------------------------------*
**&      Form  Bank for vender
**&---------------------------------------------------------------------*
FORM get_vender_bank USING    p_lifnr TYPE lfa1-lifnr
                              p_belnr TYPE bsec-belnr
                             p_gjahr TYPE bsec-gjahr
                     CHANGING value(p_banka) TYPE c
                              value(p_bankn) TYPE c
                              value(p_bkref) TYPE c
                              value(p_zbank_code) TYPE c
                              value(p_branch_code) TYPE c
                              value(p_bankl) TYPE bnka-bankl.



     CLEAR:p_banka,p_bankn,p_bkref,p_zbank_code,p_branch_code,p_bankl.
*       lifnr        TYPE lfbk-lifnr,
*       banks        TYPE bnka-banks,   "Bank th
*       bankl        TYPE bnka-bankl,   "Bank Key
*       bankn        TYPE lfbk-bankn,   "Bank Account
*       bkref        TYPE lfbk-bkref,   "Branch Name
     DATA: lv_bankl TYPE bsec-bankl,
           lv_bankn TYPE bsec-bankn.

     DATA: lv_lenbankcode type i. "CH04 Add by WAntanee 20210111
     CLEAR: lv_bankl,lv_bankn.

          SELECT lifnr banks bankl bankn bkref
          INTO TABLE gt_lfbk
          FROM lfbk
          WHERE lifnr = p_lifnr.

          READ TABLE gt_lfbk INTO gs_lfbk WITH KEY lifnr = p_lifnr.
               IF sy-subrc EQ 0.
                  p_bankn = gs_lfbk-bankn.
                  p_bkref = gs_lfbk-bkref.
                  p_bankl = gs_lfbk-bankl.
                  CONCATENATE '0' gs_lfbk-bankn(3) INTO p_branch_code.

                  READ TABLE gt_bnka INTO gs_bnka WITH KEY bankl = gs_lfbk-bankl.
                       IF sy-subrc EQ 0.
                           p_banka = gs_bnka-banka.

                       ENDIF.
                  READ TABLE gt_bank INTO gs_bank WITH KEY bankl = gs_lfbk-bankl.
                       IF sy-subrc EQ 0.
                           p_zbank_code = gs_bank-zbank_code.

                       ENDIF.

                       lv_lenbankcode =  strlen( p_bankn ). "CH04 Add by WAntanee 20210111
               ELSE.
                 SELECT SINGLE  bankn bankl
                   INTO (lv_bankn,lv_bankl)
                   FROM bsec
                  WHERE belnr EQ p_belnr
                 AND gjahr EQ p_gjahr.

                  p_bankn = lv_bankn.
                  p_bankl = lv_bankl.
                  CONCATENATE '0' lv_bankn(3) INTO p_branch_code.
                  READ TABLE gt_bnka INTO gs_bnka WITH KEY bankl = lv_bankl.
                       IF sy-subrc EQ 0.
                           p_banka = gs_bnka-banka.

                       ENDIF.
                  READ TABLE gt_bank INTO gs_bank WITH KEY bankl = lv_bankl.
                       IF sy-subrc EQ 0.
                           p_zbank_code = gs_bank-zbank_code.

                       ENDIF.
                 lv_lenbankcode =  strlen( p_bankn ). "CH04 Add by WAntanee 20210111

               ENDIF.


"CH04 Add by WAntanee 20210111
                                IF p_bankl EQ 'TISCO'.
                                    p_branch_code = p_bankn+0(4).
                                 ELSEIF p_bankl EQ 'MIZUHO'.
                                    p_branch_code = '0001'.
                                 ELSEIF p_bankl EQ 'HSBC'.
                                    p_branch_code = '0001'.
                                 ELSEIF p_bankl EQ 'CITI'.
                                    p_branch_code = '0001'.
                                 ELSEIF p_bankl EQ 'SMBC001'.
                                    p_branch_code = '0001'.
                                ELSEIF p_bankl EQ 'DB'.
                                    p_branch_code = '0001'.
                                ELSEIF p_bankl EQ 'GSB'.

                                    IF lv_lenbankcode = 15.
                                       p_branch_code = p_bankn+0(4).
                                       p_bankn = p_bankn+5(11).
                                    ELSEIF lv_lenbankcode = 12.
                                       p_bankn = p_bankn+1(11).
                                       CONCATENATE '999' p_bankn+0(1) INTO p_branch_code.

                                    ELSE.
                                       p_bankn = p_bankn.
                                       CONCATENATE '999' '0' INTO p_branch_code.

                                    ENDIF.
                                ELSEIF p_bankl EQ 'KIATNAKIN'.
                                   p_branch_code = p_bankn+0(4).

                                ELSEIF p_bankl EQ 'GHB'.
                                    IF lv_lenbankcode = 12.
                                      p_branch_code = p_bankn+0(3).
*
*                                    ELSE.
*                                       CONCATENATE '00' gs_result-bank_acc+0(2) INTO p_branch_code.
                                    ENDIF.
                                ELSEIF p_bankl EQ 'BAAC'.
                                  p_branch_code = '0000'.
*                                  IF lv_lenbankcode = 12.
*                                      lv_bank_acc = gs_result-bank_acc+1(11).
*                                  ENDIF.
                                ELSE.
                                  CONCATENATE '0' p_bankn(3) INTO p_branch_code.

                                ENDIF.
"CH04 End Add by Wantanee 20210111


ENDFORM. "
*
**&---------------------------------------------------------------------*
**&      Form  BUILD_CATALOG
**&---------------------------------------------------------------------*
FORM build_catalog   USING rt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: gs_fieldcat TYPE slis_fieldcat_alv,
        pos         TYPE I VALUE 1.
  CLEAR gs_fieldcat.



*     Running No
      PERFORM append_fieldcat USING 'RUN_NO' '' ''  'Running No'
                                     space  space  space '' gt_fieldcat[].
*     Payee Code
      PERFORM append_fieldcat USING 'PAYEE_NO' '' ''  'Payee Code'
                                     space  space  space '' gt_fieldcat[].

*     Payee Name
      PERFORM append_fieldcat USING 'PAYEE_NAME' '' ''  'Payee Name'
                                     space  space  space '' gt_fieldcat[].
*     Payee Address
      PERFORM append_fieldcat USING 'PAYEE_ADDR' '' ''  'Payee Address'
                                     space  space  space '' gt_fieldcat[].
*     Payee Phone.
      PERFORM append_fieldcat USING 'PAYEE_PHONE' 'ADRC' 'TEL_NUMBER'  'Payee Phone.'
                                     space  space  space '' gt_fieldcat[].
*     Bank Name
      PERFORM append_fieldcat USING 'BANK_NAME' 'BNKA' 'BANKA'  'Bank Name'
                                     space  space  space '' gt_fieldcat[].
*     Bank CODE
      PERFORM append_fieldcat USING 'BANK_CODE' '' ''  'Bank Code'
                                     space  space  space '' gt_fieldcat[].
*     Branch CODE
      PERFORM append_fieldcat USING 'BRANCH_CODE' '' ''  'Branch Code'
                                     space  space  space '' gt_fieldcat[].
*     Branch Name
      PERFORM append_fieldcat USING 'BRANCH_NAME' '' ''  'Branch Name'
                                     space  space  space '' gt_fieldcat[].
*     Bank Account
      PERFORM append_fieldcat USING 'BANK_ACCOUNT' '' ''  'Bank Account'
                                     space  space  space '' gt_fieldcat[].
*     Payment Amount
      PERFORM append_fieldcat USING 'PAY_AMT' 'BSAK' 'DMBTR'  'Payment Amount'
                                     space  space  space '' gt_fieldcat[].
*     Payment Doc.
      PERFORM append_fieldcat USING 'PAY_DOC' 'BSAK' 'AUGBL'  'Payment Doc.'
                                     space  space  space '' gt_fieldcat[].
*     Payment Date.
      PERFORM append_fieldcat USING 'PAY_DATE' 'BKPF' 'BUDAT'  'Payment Date.'
                                     space  space  space '' gt_fieldcat[].
*     E-mail
      PERFORM append_fieldcat USING 'EMAIL' '' ''  'E-mail'
                                     space  space  space '' gt_fieldcat[].
*     Zipcode
      PERFORM append_fieldcat USING 'ZIPCODE' '' ''  'Zipcode'
                                     space  space  space '' gt_fieldcat[].
*     Fax Number
      PERFORM append_fieldcat USING 'FAX_NO' '' ''  'Fax Number'
                                     space  space  space '' gt_fieldcat[].
*     Cheque No.
      PERFORM append_fieldcat USING 'CHEQUE_NO' '' ''  'Cheque No.'
                                     space  space  space '' gt_fieldcat[].
*     AP Invoice
      PERFORM append_fieldcat USING 'AP_INV' '' ''  'AP Invoice'
                                     space  space  space '' gt_fieldcat[].
*     AP Invoice Date
      PERFORM append_fieldcat USING 'AP_INV_DATE' '' ''  'AP Invoice Date'
                                     space  space  space '' gt_fieldcat[].
*     AP Invoice AMT
      PERFORM append_fieldcat USING 'AP_INV_AMT' '' ''  'AP Invoice Amt.'
                                     space  space  space '' gt_fieldcat[].
*     WHT  amt
      PERFORM append_fieldcat USING 'AP_WHT_AMT' '' ''  'WHT Amt'
                                     space  space  space '' gt_fieldcat[].
*     WHT  TYPE
      PERFORM append_fieldcat USING 'AP_WHT_TYP' '' ''  'WHT Type'
                                     space  space  space '' gt_fieldcat[].
*     TEXT
      PERFORM append_fieldcat USING 'AP_INV_TEXT' '' ''  'Text'
                                     space  space  space '' gt_fieldcat[].

*     Customer Code
      PERFORM append_fieldcat USING 'AP_CUSTOMER' 'KNA1' 'KUNNR'  'Customer Code'
                                     space  space  space '' gt_fieldcat[].
**     Status Vender Regis
*      PERFORM append_fieldcat USING 'STATUS_REGIS' '' ''  'Vender Regis'
*                                     space  space  space '' gt_fieldcat[].







ENDFORM.                    " BUILD_CATALOG

**&---------------------------------------------------------------------*
**&      Form  BUILD_EVENT
**&---------------------------------------------------------------------*
FORM build_event  USING  p_ggy_event TYPE slis_t_event.
  FIELD-SYMBOLS <fs_events> LIKE LINE OF p_ggy_event.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      et_events       = p_ggy_event
    EXCEPTIONS
      list_type_wrong = 0
      OTHERS          = 0.

  LOOP AT p_ggy_event ASSIGNING <fs_events>
                    WHERE name = 'TOP_OF_PAGE'.
    <fs_events>-form = 'REPORT_HEADER'.
  ENDLOOP.

ENDFORM.                    " BUILD_EVENT
**&---------------------------------------------------------------------*
**&      Form  BUILD_LAYOUT
**&---------------------------------------------------------------------*
FORM build_layout  USING p_ggy_layout TYPE slis_layout_alv.
  p_ggy_layout-window_titlebar = sy-title.
  p_ggy_layout-zebra = 'X'.
  p_ggy_layout-colwidth_optimize = 'X'.
  p_ggy_layout-info_fieldname =      'LINE_COLOR'. "Add by Wantanee 20110701



ENDFORM.                    " BUILD_LAYOUT
**&---------------------------------------------------------------------*
**&      Form  BUILD_SORT
**&---------------------------------------------------------------------*
FORM BUILD_SORT .
  DATA: gs_sort TYPE slis_sortinfo_alv.
  CLEAR gt_sort[].

ENDFORM.                    " BUILD_SORT
**&---------------------------------------------------------------------*
**&      Form  DISPLAY_OUTPUT
**&---------------------------------------------------------------------*
FORM DISPLAY_OUTPUT .
  DATA variant LIKE disvariant.
  variant-report = gc_repid.


     CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
         i_callback_program      = gc_repid
         i_callback_user_command = 'USER_COMMAND'
*         i_callback_pf_status_set = 'SUB_DISP_ALV_PFSTATUS'  "Refresh
         is_layout               = gt_layout
         it_fieldcat             = gt_fieldcat
         i_save                  = 'A'
         is_variant              = gt_variant
         it_sort                 = gt_sort
         it_events               = gt_events
       TABLES
         t_outtab                = gt_itab
       EXCEPTIONS
         program_error           = 1
         OTHERS                  = 2.


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_OUTPUT
**&---------------------------------------------------------------------*
**&      Form  APPEND_FIELDCAT
**&---------------------------------------------------------------------*
FORM append_fieldcat  USING  p_field      "Field name
*                            p_table      "Table name
                             p_reftable   "Reference Table name
                             p_reffield   "Reference Field name
*                            p_colpos     "Col position
                             p_coltxt     "Col Text(for specify)
                             p_dosum      "Sum total
                             p_cfieldname "currency
                             p_no_zero    "no zero
                             p_color      "color
*                             p_checkbox
                             p_it_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: gs_infieldcat TYPE slis_fieldcat_alv,
        v_coltxt_length TYPE i.

  ADD 1 TO v_pos.

  gs_infieldcat-fieldname      = p_field.
* gs_infieldcat-tabname        = 'I_TAB'.
  gs_infieldcat-ref_tabname    = p_reftable.
  gs_infieldcat-ref_fieldname  = p_reffield.
  gs_infieldcat-col_pos        = v_pos.
  gs_infieldcat-do_sum         = p_dosum.
*  gs_infieldcat-checkbox       = '1' .
" Add by wantanee 20110701
  IF NOT p_no_zero IS INITIAL.
    gs_infieldcat-no_zero = p_no_zero.
  ENDIF.
  IF NOT p_cfieldname IS INITIAL.
    gs_infieldcat-cfieldname = p_cfieldname.
  ENDIF.
*
  if p_color = '1'.
    gs_infieldcat-EMPHASIZE = 'C110'.
  elseif p_color = '2' .
    gs_infieldcat-EMPHASIZE = 'C310'.
  elseif p_color = '3' .
    gs_infieldcat-EMPHASIZE = 'C510'.
  elseif p_color = '4' .
    gs_infieldcat-EMPHASIZE = 'C710'.
  elseif p_color = '5' .
    gs_infieldcat-EMPHASIZE = 'C200'.
  elseif p_color = '6' .
    gs_infieldcat-EMPHASIZE = 'C400'.
  elseif p_color = '7' .
    gs_infieldcat-EMPHASIZE = 'C500'.
  endif.
" End Add by wantanee 20110701
* If we need to specify text, don't need to derive from data dictionary
* program will check length and define width of the colum
  IF NOT p_coltxt IS INITIAL.
    v_coltxt_length = STRLEN( p_coltxt ).

    IF v_coltxt_length > 20.
      gs_infieldcat-ddictxt   = 'L'. "Long text
      gs_infieldcat-seltext_l = p_coltxt.
    ELSEIF v_coltxt_length > 10.
      gs_infieldcat-ddictxt   = 'M'. "Medium Text
      gs_infieldcat-seltext_m = p_coltxt.
    ELSE.
      gs_infieldcat-ddictxt   = 'S'. "Short Text
      gs_infieldcat-seltext_s = p_coltxt.
    ENDIF.
    gs_infieldcat-reptext_ddic = p_coltxt.
  ENDIF.
  APPEND gs_infieldcat TO p_it_fieldcat.

ENDFORM.                    " APPEND_FIELDCAT
**&---------------------------------------------------------------------*
**&      Form  REPORT_HEADER
**&---------------------------------------------------------------------*
FORM report_header.
  DATA: lt_listhead  TYPE slis_t_listheader,
        lw_listline  TYPE slis_listheader.
  DATA: lv_text(256) TYPE c.
  DATA: lv_date(10)  TYPE c,
        lv_time(5)   TYPE c.

* execution date & time
  CLEAR: lv_date, lv_time.
  WRITE sy-datum TO lv_date USING EDIT MASK gc_mask_date.
  WRITE sy-uzeit TO lv_time USING EDIT MASK gc_mask_time.

  CLEAR lv_text.
  lv_text = 'Report export Data Autopayment to bank'.
  def_list_head 'H' '' lv_text.
*  CLEAR lv_text.
*  concatenate 'DATE:' lv_date 'TIME:' lv_time INTO lv_text
*  separated by space.
*  def_list_head 'S' '' lv_text.

  CLEAR lv_text.
  LOOP AT  gt_header INTO gs_header.
       CONCATENATE gs_header-env gs_header-defaultid gs_header-sm
                   gs_header-transfertype gs_header-debittype
                   gs_header-custref gs_header-valudate
                   gs_header-ccy
                   gs_header-debitbranchcd
                   gs_header-debitglcd gs_header-debitglsubcd
                   gs_header-debitssno gs_header-debitaccountno
                   gs_header-debitsubno gs_header-debitccy
                   gs_header-debitmktcd
                   "Add by Wantanee 20140211
                    gs_header-chargebranchcd
                    gs_header-chargeglcd
                    gs_header-chargeglsubcd
                    gs_header-chargessno
                    gs_header-chargeaccountno
                    gs_header-chargesubno
                    gs_header-chargeccy
                    gs_header-chargemktcd
                  "Add by Wantanee 20140211




                   INTO lv_text SEPARATED BY ','.
  ENDLOOP.

  def_list_head 'S' '' lv_text.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_listhead
    EXCEPTIONS
      OTHERS             = 0.

ENDFORM.                    " REPORT_HEADER
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM read_text  USING    p_id
                         p_object
                         p_vbeln
                CHANGING p_value.

  DATA: it_lines TYPE STANDARD TABLE OF tline.
  DATA: gs_lines LIKE LINE OF it_lines.
  DATA: v_name TYPE thead-tdname.

  CLEAR: p_value, it_lines[].

  v_name = p_vbeln.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = p_id
      language                = sy-langu
      name                    = v_name
      object                  = p_object
    TABLES
      lines                   = it_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc = 0.
    LOOP AT it_lines INTO gs_lines.
      CONCATENATE p_value  gs_lines-tdline INTO p_value SEPARATED BY SPACE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT
**---------------------------------------------------------------------*
**       FORM User_command
**---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                                        rs_selfield TYPE slis_selfield.

*  IF r_ucomm = '&IC1'.  "Double click event
**   goto VA03 display sales order
*    IF rs_selfield-fieldname EQ 'VBELN'.
*        READ TABLE gt_itab INTO gs_itab INDEX rs_selfield-tabindex.
*        SET PARAMETER ID 'AUN'  FIELD gs_itab-vbeln.
*        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*    ELSE.
*        READ TABLE gt_itab INTO gs_itab INDEX rs_selfield-tabindex.
*        SET PARAMETER ID 'VL'  FIELD gs_itab-lief_nr.
*        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
*    ENDIF.
*  ENDIF.
**  IF r_ucomm = '&NTE'. "Refresh
***      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
***      PERFORM get_data.
***      PERFORM map_data.
***      PERFORM sub_main.
****      rs_selfield-refresh = 'X'.
****      SET USER-COMMAND '&OPT'.
**       PERFORM REFRESH      .
**" Optimize columns width
**       CLEAR r_ucomm.
** ENDIF.
* CLEAR r_ucomm.
ENDFORM.                    "user_comman

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_download_file USING p_p_path TYPE localfile.
  DATA: lv_file TYPE string.

  lv_file = p_p_path.

  SORT gt_regis BY payeeno.



IF NOT p_exp IS INITIAL.


          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
*           BIN_FILESIZE                    =
              filename                      =  lv_file
              filetype                      =  'DBF'
*           APPEND                          = ' '
           WRITE_FIELD_SEPARATOR            =  'X'
*           HEADER                          = '00'
*           TRUNC_TRAILING_BLANKS           = ' '
*           WRITE_LF                        = 'X'
*           COL_SELECT                      = ' '
*           COL_SELECT_MASK                 = ' '
*           DAT_MODE                        = ' '
*           CONFIRM_OVERWRITE               = ' '
*           NO_AUTH_CHECK                   = ' '
           codepage                        = '8600'
*           IGNORE_CERR                     = ABAP_TRUE
*           REPLACEMENT                     = '#'
*           WRITE_BOM                       = ' '
*           TRUNC_TRAILING_BLANKS_EOL       = 'X'
*           WK1_N_FORMAT                    = ' '
*           WK1_N_SIZE                      = ' '
*           WK1_T_FORMAT                    = ' '
*           WK1_T_SIZE                      = ' '
*           WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*           SHOW_TRANSFER_STATUS            = ABAP_TRUE
*         IMPORTING
*           FILELENGTH                      =
            TABLES
              data_tab                      =  gt_ex_excel
              FIELDNAMES                    =  gt_ex_excel_h
             EXCEPTIONS
              file_write_error                = 1
              no_batch                        = 2
              gui_refuse_filetransfer         = 3
              invalid_type                    = 4
              no_authority                    = 5
              unknown_error                   = 6
              header_not_allowed              = 7
              separator_not_allowed           = 8
              filesize_not_allowed            = 9
              header_too_long                 = 10
              dp_error_create                 = 11
              dp_error_send                   = 12
              dp_error_write                  = 13
              unknown_dp_error                = 14
              access_denied                   = 15
              dp_out_of_memory                = 16
              disk_full                       = 17
              dp_timeout                      = 18
              file_not_found                  = 19
              dataprovider_exception          = 20
              control_flush_error             = 21
              OTHERS                          = 22
                         .
               IF sy-subrc <> 0.
                 MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
               ENDIF.
  ELSE.


            CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING
*             BIN_FILESIZE                    =
                filename                      =  lv_file
                filetype                      =  'DAT'
*             APPEND                          = ' '
*             WRITE_FIELD_SEPARATOR           = ' '
*             HEADER                          = '00'
*             TRUNC_TRAILING_BLANKS           = ' '
*             WRITE_LF                        = 'X'
*             COL_SELECT                      = ' '
*             COL_SELECT_MASK                 = ' '
*             DAT_MODE                        = ' '
*             CONFIRM_OVERWRITE               = ' '
*             NO_AUTH_CHECK                   = ' '
             codepage                        = '8600'
*             IGNORE_CERR                     = ABAP_TRUE
*             REPLACEMENT                     = '#'
*             WRITE_BOM                       = ' '
*             TRUNC_TRAILING_BLANKS_EOL       = 'X'
*             WK1_N_FORMAT                    = ' '
*             WK1_N_SIZE                      = ' '
*             WK1_T_FORMAT                    = ' '
*             WK1_T_SIZE                      = ' '
*             WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*             SHOW_TRANSFER_STATUS            = ABAP_TRUE
*           IMPORTING
*             FILELENGTH                      =
              TABLES
                data_tab                      =  gt_dfile
*             FIELDNAMES                      =
              EXCEPTIONS
               file_write_error                = 1
               no_batch                        = 2
               gui_refuse_filetransfer         = 3
               invalid_type                    = 4
               no_authority                    = 5
               unknown_error                   = 6
               header_not_allowed              = 7
               separator_not_allowed           = 8
               filesize_not_allowed            = 9
               header_too_long                 = 10
               dp_error_create                 = 11
               dp_error_send                   = 12
               dp_error_write                  = 13
               unknown_dp_error                = 14
               access_denied                   = 15
               dp_out_of_memory                = 16
               disk_full                       = 17
               dp_timeout                      = 18
               file_not_found                  = 19
               dataprovider_exception          = 20
               control_flush_error             = 21
               OTHERS                          = 22
                          .
               IF sy-subrc <> 0.
                 MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
               ENDIF.

  ENDIF.

ENDFORM.                    " F_DOWNLOAD_file
*&---------------------------------------------------------------------*
*&      Form  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM get_path_name  CHANGING path.
  DATA: l_length TYPE i.
  DATA: l_mask(20) TYPE c.
         IF NOT p_exp IS INITIAL.
            p_path = 'C:\test.xls'.
         ELSEIF NOT p_expt IS INITIAL.
            p_path = 'C:\test.txt'.
         ENDIF.

  IF NOT p_exp IS INITIAL.
*     S = Save, O = Open
      CALL FUNCTION 'WS_FILENAME_GET'
        EXPORTING
          mask             = ',*.xls,*.xls.'
          mode             = 'O'
        IMPORTING
          filename         = path
        EXCEPTIONS
          inv_winsys       = 01
          no_batch         = 02
          selection_cancel = 03
          selection_error  = 04.
   ELSE.
*     S = Save, O = Open
      CALL FUNCTION 'WS_FILENAME_GET'
        EXPORTING
          mask             = ',*.txt,*.txt.'
          mode             = 'O'
        IMPORTING
          filename         = path
        EXCEPTIONS
          inv_winsys       = 01
          no_batch         = 02
          selection_cancel = 03
          selection_error  = 04.

   ENDIF.

ENDFORM.                    " GET_PATH_NAME


*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form map_exp_data_text using p_p_path type localfile.
  DATA: lv_file TYPE string.
  DATA: g_space TYPE string,
        l_pos TYPE i.
        g_space = cl_abap_conv_in_ce=>uccp( '00a0' ).
  DATA: lv_remark(50) TYPE c.
  DATA: lv_payamt TYPE  bsak-dmbtr.
  DATA: lv_runno TYPE i.
  lv_file = p_p_path.




          CLEAR: gs_dfile.
          LOOP AT  gt_001 INTO gs_001.
               CONCATENATE gs_001-rec_typ
                           gs_001-com_id
                           gs_001-cust_ref
                           gs_001-mes_date
                           gs_001-mes_time
                           gs_001-channel_id
                           gs_001-batch_ref

                           INTO gs_dfile-text  RESPECTING BLANKS.

                           l_pos = strlen( gs_dfile-text ).
                           WHILE l_pos < 96.

                               gs_dfile-text+l_pos(1) = g_space.
                               l_pos = l_pos + 1.
                           ENDWHILE.
                           APPEND gs_dfile TO gt_dfile.

          ENDLOOP.

          CLEAR: gs_dfile.
          LOOP AT   gt_002 INTO gs_002.
               CLEAR: lv_runno.
               CONCATENATE gs_002-rec_typ
                           gs_002-product_code
                           gs_002-value_date
                           gs_002-debit_acc_no
                           gs_002-acc_gy_debit
                           gs_002-debit_brach_code
                           gs_002-debit_curr
                           gs_002-debit_amt
                           gs_002-internal_ref
                           gs_002-no_of_credit
                           gs_002-fee_debit_acc
                           gs_002-filler
                           gs_002-media_clear_cycel
                           gs_002-acc_typ_fee
                           gs_002-debit_branch_code_fee

                           INTO gs_dfile-text RESPECTING BLANKS.
                           l_pos = strlen( gs_dfile-text ).
                           WHILE l_pos < 109.

                               gs_dfile-text+l_pos(1) = g_space.
                               l_pos = l_pos + 1.
                           ENDWHILE.
                            APPEND gs_dfile TO gt_dfile.


                   "CH04  Add by WAntanee 20210111

                    LOOP AT gt_payee_code INTO gs_payee_code WHERE product_code = gs_002-product_code.

                        CLEAR: lv_remark,lv_payamt,gs_pay,gs_pay_004.
                        lv_runno = lv_runno + 1.
                        gs_rep_excel-lifnr = gs_payee_code-lifnr.

                        LOOP AT gt_rep_excel INTO gs_rep_excel WHERE lifnr EQ gs_payee_code-lifnr.


                                gs_ex_excel-name = gs_rep_excel-name .
                                gs_ex_excel-acc_bank = gs_rep_excel-acc_bank .
                                lv_payamt = lv_payamt + gs_rep_excel-bank_amt.
                                gs_ex_excel-bank_name = gs_rep_excel-bank_name .
                                gs_ex_excel-e_mail = gs_rep_excel-e_mail .

                                CONCATENATE lv_remark gs_rep_excel-augbl INTO lv_remark SEPARATED BY SPACE.

                        ENDLOOP.
                       gs_ex_excel-run_no = lv_runno.
                       gs_ex_excel-bank_amt = lv_payamt.
                       gs_ex_excel-Beneficiary = 'N'.
                       gs_ex_excel-bank_fee = ''.
                       gs_ex_excel-net_amt = lv_payamt.
                       gs_ex_excel-hotkey_code = ''.
                       gs_ex_excel-remark = lv_remark.


                       gs_pay-rec_typ = '003'.
                       gs_pay-credit_sequence_no = lv_runno.
                       gs_pay-credit_acc = gs_ex_excel-acc_bank.
                       gs_pay-credit_amt = ( lv_payamt * 1000 ).
                       gs_pay-credit_curr = 'THB'.
*                       gs_pay-internal_ref = '00000001'.
                       gs_pay-internal_ref = gs_002-internal_ref.
                       gs_pay-wht_present = 'N'.
                       gs_pay-inv_details_present = 'N'.
                       gs_pay-credit_advice = 'Y'.
                       gs_pay-delivery_mode = 'S'.
*                       gs_pay-pickup_location
                       gs_pay-wht_form_type = '00'.
*                       gs_pay-wht_tax_running
                       gs_pay-wht_attach_no = '000000'.
                       gs_pay-no_of_wht = '00'.
                       gs_pay-total_wht_amt = '0000000000000000'.
                       gs_pay-no_of_inv = '000000'.
                       gs_pay-total_inv_amt = '0000000000000000'.
                       gs_pay-wht_pay_type = '0'.
                       gs_pay-receive_zbank_code = gs_payee_code-zbank_code.
                       gs_pay-receive_bank_name = gs_payee_code-bank_name.
                       gs_pay-receive_branch_code =    gs_payee_code-branch_code.

                       gs_pay-beneficiary = 'E'.
                       gs_pay-cust_ref = lv_remark.

                       IF gs_payee_code-product_code = 'PAY'.
                          gs_pay-service_type   = '01'.
                       ELSE.
                          gs_pay-service_type   = '04'.
                       ENDIF.
                       gs_pay-remark = lv_remark.


                          CONCATENATE gs_pay-rec_typ  gs_pay-credit_sequence_no gs_pay-credit_acc
                                      gs_pay-credit_amt  gs_pay-credit_curr gs_pay-internal_ref
                                      gs_pay-wht_present  gs_pay-inv_details_present gs_pay-credit_advice
                                      gs_pay-delivery_mode  gs_pay-pickup_location gs_pay-wht_form_type
                                      gs_pay-wht_tax_running  gs_pay-wht_attach_no gs_pay-no_of_wht
                                      gs_pay-total_wht_amt  gs_pay-no_of_inv gs_pay-total_inv_amt
                                      gs_pay-wht_pay_type  gs_pay-wht_remark gs_pay-wht_deduct_date
                                      gs_pay-receive_zbank_code  gs_pay-receive_bank_name gs_pay-receive_branch_code
                                      gs_pay-receive_branch_name  gs_pay-wht_signatory gs_pay-beneficiary
                                      gs_pay-cust_ref  gs_pay-cheque_refer gs_pay-payment_type_code
                                      gs_pay-service_type  gs_pay-remark gs_pay-scb_remark
                                      gs_pay-beneficiary_charge
                         INTO gs_dfile-text RESPECTING BLANKS.
                               l_pos = strlen( gs_dfile-text ).
                               WHILE l_pos < 355.

                                   gs_dfile-text+l_pos(1) = g_space.
                                   l_pos = l_pos + 1.
                               ENDWHILE.
                        APPEND gs_dfile TO gt_dfile.



                       gs_pay_004-record_typ = '004'.
*                       gs_pay_004-internal_ref = '00000001'.
                       gs_pay_004-internal_ref = gs_002-internal_ref.
                       gs_pay_004-credit_sequence_no = lv_runno.
                       gs_pay_004-payee_idcard = '000000000000000'.
                       gs_pay_004-payee_name = gs_ex_excel-name.
                       gs_pay_004-payee_e_mail = gs_ex_excel-e_mail.


                       CONCATENATE gs_pay_004-record_typ  gs_pay_004-internal_ref gs_pay_004-credit_sequence_no
                                   gs_pay_004-payee_idcard gs_pay_004-payee_name gs_pay_004-payee_addr1
                                   gs_pay_004-payee_addr2 gs_pay_004-payee_addr3 gs_pay_004-payee_taxid
                                   gs_pay_004-payee_name_eng gs_pay_004-payee_fax_no gs_pay_004-payee_mobile
                                   gs_pay_004-payee_e_mail gs_pay_004-payee2_name gs_pay_004-payee2_addr1
                                   gs_pay_004-payee2_addr2 gs_pay_004-payee2_addr3


                       INTO gs_dfile-text RESPECTING BLANKS.
                               l_pos = strlen( gs_dfile-text ).
                               WHILE l_pos < 816.

                                   gs_dfile-text+l_pos(1) = g_space.
                                   l_pos = l_pos + 1.
                               ENDWHILE.
                        APPEND gs_dfile TO gt_dfile.

                   ENDLOOP.

                   "CH04 End Add by WAntanee 20210111

          ENDLOOP.


               LOOP AT gt_999 INTO gs_999.
*                 gs_999-total_no_of_credit = lv_runno.
                     CONCATENATE gs_999-record_typ
                                  gs_999-total_no_of_debit
                                  gs_999-total_no_of_credit
                                  gs_999-total_amt

                           INTO gs_dfile-text  RESPECTING BLANKS.

                           l_pos = strlen( gs_dfile-text ).
                           WHILE l_pos < 31.

                               gs_dfile-text+l_pos(1) = g_space.
                               l_pos = l_pos + 1.
                           ENDWHILE.
                           APPEND gs_dfile TO gt_dfile.
               ENDLOOP.




*




endform.
