*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0190_TOPZ
*&---------------------------------------------------------------------*
TYPE-POOLS: abap,
            slis.

*&--------------------------------------------------------------------*
*& TABLES
*&--------------------------------------------------------------------*
TABLES : t001,
         bkpf,
         bseg,
         vbrk,
         ZSDSFIT014.

*&--------------------------------------------------------------------*
*& TYPES
*&--------------------------------------------------------------------*
TYPES : BEGIN OF gty_addfields,
          document_no      TYPE ZSDSFIT014-document_no,
          rd_doc_type_desc TYPE ZSDSFIC005-rd_doc_typ_ds_th,
        END OF gty_addfields.

* เก็บข้อมูลเอกสาร-ระดับ Item
TYPES : BEGIN OF gty_doc_item.
        INCLUDE STRUCTURE ZSDSFIT015.
INCLUDE TYPE gty_addfields.
TYPES: kunnr            TYPE ZSDSFIT014-kunnr,
       kunnr_name       TYPE ZSDSFIT014-kunnr_name,
       link_disc_charge TYPE c LENGTH 10.
TYPES : END OF gty_doc_item.

* เก็บข้อมูลเอกสาร-ระดับ disc Charge
TYPES : BEGIN OF gty_doc_dischg.
        INCLUDE STRUCTURE ZSDSFIT013.
INCLUDE TYPE gty_addfields.
TYPES: kunnr          TYPE ZSDSFIT014-kunnr,
       kunnr_name     TYPE ZSDSFIT014-kunnr_name,
       sap_curr       TYPE ZSDSFIT014-sap_curr,
       rd_dischg      TYPE ZSDSFIC008-rd_dischg,
       rd_dischg_desc TYPE ZSDSFIC016-rd_dischg_desc_t.
TYPES : END OF gty_doc_dischg.

* เก็บข้อมูลเอกสาร-ระดับ Partner
TYPES : BEGIN OF gty_doc_partner.
        INCLUDE STRUCTURE ZSDSFIT018.
INCLUDE TYPE gty_addfields.
TYPES: rd_partner_desc  TYPE ZSDSFIC018-rd_partner_desc.
TYPES : END OF gty_doc_partner.

* เก็บข้อมูลเอกสาร – VAT ระดับ Header
TYPES : BEGIN OF gty_doc_h_vat.
        INCLUDE STRUCTURE ZSDSFIT017.
INCLUDE TYPE gty_addfields.
TYPES: vat_rate_txt TYPE c LENGTH 21,
       kunnr        TYPE ZSDSFIT014-kunnr,
       kunnr_name   TYPE ZSDSFIT014-kunnr_name.
TYPES : END OF gty_doc_h_vat.

* เก็บข้อมูลเอกสาร – อ้างอิง ระดับ Header
TYPES : BEGIN OF gty_doc_h_ref.
        INCLUDE STRUCTURE ZSDSFIT016.
INCLUDE TYPE gty_addfields.
TYPES: kunnr                TYPE ZSDSFIT014-kunnr,
       kunnr_name           TYPE ZSDSFIT014-kunnr_name,
       ref_rd_doc_type_desc TYPE ZSDSFIC005-rd_doc_typ_ds_th.
TYPES : END OF gty_doc_h_ref.

* เก็บข้อมูลเอกสาร-ระดับ Header
TYPES: BEGIN OF gty_doc_header,
         bukrs               TYPE ZSDSFIT014-bukrs,
         sap_doc_no          TYPE ZSDSFIT014-sap_doc_no,
         gjahr               TYPE ZSDSFIT014-gjahr,
         rd_doc_type         TYPE ZSDSFIT014-rd_doc_type,
         module_etx          TYPE ZSDSFIT014-module_etx,
         document_no         TYPE ZSDSFIT014-document_no,
         sap_posting_date    TYPE ZSDSFIT014-sap_posting_date,
         rd_doc_type_grp     TYPE ZSDSFIT014-rd_doc_type_grp,
         rd_doc_type_desc_th TYPE ZSDSFIC005-rd_doc_typ_ds_th,
         sap_doc_type        TYPE ZSDSFIT014-sap_doc_type,
         butxt               TYPE t001-butxt,
         reverse_flag        TYPE ZSDSFIT014-reverse_flag,
         rd_flag             TYPE ZSDSFIT014-rd_flag,
         bupla               TYPE ZSDSFIT014-bupla,
         kunnr               TYPE ZSDSFIT014-kunnr,
         kunnr_name          TYPE ZSDSFIT014-kunnr_name,
         kunnr_branch        TYPE ZSDSFIT014-kunnr_branch,
         kunnr_tax_id        TYPE ZSDSFIT014-kunnr_tax_id,
         replace_flag        TYPE ZSDSFIT014-replace_flag,
         sap_doc_resn        TYPE ZSDSFIT014-sap_doc_resn,
         sap_doc_resn_des    TYPE ZSDSFIT014-sap_doc_resn_des,
         rd_doc_resn         TYPE ZSDSFIT014-rd_doc_resn,
         rd_doc_resn_desc    TYPE ZSDSFIC004-rd_doc_resn_desc,
         req_dev_date        TYPE ZSDSFIT014-req_dev_date,
         pay_term            TYPE ZSDSFIT014-pay_term,
         pay_term_desc       TYPE ZSDSFIT014-pay_term_desc,
         vtext               TYPE tvzbt-vtext,
         pay_due_date        TYPE ZSDSFIT014-pay_due_date,
         inco_term           TYPE ZSDSFIT014-inco_term,
         sap_po_no           TYPE ZSDSFIT014-sap_po_no,
         sap_po_date         TYPE ZSDSFIT014-sap_po_date,
         global_doc_no       TYPE ZSDSFIT014-global_doc_no,
         sap_curr            TYPE ZSDSFIT014-sap_curr,
         rd_curr_code        TYPE ZSDSFIT014-rd_curr_code,
         vat_base_amt        TYPE ZSDSFIT014-vat_base_amt,
         net_amt_bf_vat      TYPE ZSDSFIT014-net_amt_bf_vat,
         vat_amt             TYPE ZSDSFIT014-vat_amt,
         net_amt_aft_vat     TYPE ZSDSFIT014-net_amt_aft_vat,
         ref_doc_amt         TYPE ZSDSFIT014-ref_doc_amt,
         correct_amt         TYPE ZSDSFIT014-correct_amt,
         diff_amt            TYPE ZSDSFIT014-diff_amt,
         total_disc_amt      TYPE ZSDSFIT014-total_disc_amt,
         total_charge_amt    TYPE ZSDSFIT014-total_charge_amt,
         status              TYPE ZSDSFIT014-status,
*>>> BEGIN OF MODIFICATION: <ETAX003> on 16.09.2020 <<<
         bupla_info          TYPE ZSDSFIT014-bupla_info,
         subject             TYPE ZSDSFIT014-subject,
         content             TYPE ZSDSFIT014-content,
         deposit_flag        TYPE ZSDSFIT014-deposit_flag,
*>>> END OF MODIFICATION: <ETAX003> on 16.09.2020 <<<
         etax_by             TYPE ZSDSFIT014-etax_by,     "INS CH01
         etax_date           TYPE ZSDSFIT014-etax_date,   "INS CH01
         print_user          TYPE ZSDSFIT014-print_user,  "INS CH01
         print_date          TYPE ZSDSFIT014-print_date,  "INS CH01
       END OF gty_doc_header,

       BEGIN OF gty_tvfkt,
         fkart TYPE tvfkt-fkart,
         vtext TYPE tvfkt-vtext,
       END OF gty_tvfkt,

       BEGIN OF gty_t003t,
         blart TYPE t003t-blart,
         ltext TYPE t003t-ltext,
       END OF gty_t003t,

       BEGIN OF gty_rd_dischg,
         rd_dischg         TYPE ZSDSFIC016-rd_dischg,
         rd_dischg_desc_th TYPE ZSDSFIC016-rd_dischg_desc_t,
       END OF gty_rd_dischg,

       BEGIN OF gty_rd_doc_type,
         rd_doc_type         TYPE ZSDSFIC005-rd_doc_type,
         rd_doc_type_desc_th TYPE ZSDSFIC005-rd_doc_typ_ds_th,
       END OF gty_rd_doc_type,

       gtty_rd_doc_type   TYPE HASHED TABLE OF gty_rd_doc_type WITH UNIQUE KEY rd_doc_type,
       gtty_doc_item      TYPE TABLE OF gty_doc_item WITH NON-UNIQUE DEFAULT KEY,
       gtty_doc_partner   TYPE TABLE OF gty_doc_partner WITH NON-UNIQUE DEFAULT KEY,
       gtty_doc_h_vat     TYPE TABLE OF gty_doc_h_vat WITH NON-UNIQUE DEFAULT KEY,
       gtty_doc_h_ref     TYPE TABLE OF gty_doc_h_ref WITH NON-UNIQUE DEFAULT KEY,
       gtty_doc_dischg    TYPE TABLE OF gty_doc_dischg WITH NON-UNIQUE DEFAULT KEY,
       gtty_doc_item_s    TYPE SORTED TABLE OF gty_doc_item WITH NON-UNIQUE KEY
                                                  bukrs sap_doc_no gjahr rd_doc_type module_etx,
       gtty_doc_partner_s TYPE SORTED TABLE OF gty_doc_partner WITH NON-UNIQUE KEY
                                                  bukrs sap_doc_no gjahr rd_doc_type module_etx,
       gtty_doc_h_vat_s   TYPE SORTED TABLE OF gty_doc_h_vat WITH NON-UNIQUE KEY
                                                  bukrs sap_doc_no gjahr rd_doc_type module_etx,
       gtty_doc_h_ref_s   TYPE SORTED TABLE OF gty_doc_h_ref WITH NON-UNIQUE KEY
                                                  bukrs sap_doc_no gjahr rd_doc_type module_etx,
       gtty_doc_dischg_s  TYPE SORTED TABLE OF gty_doc_dischg WITH NON-UNIQUE KEY
                                                  bukrs sap_doc_no gjahr rd_doc_type module_etx.

* Screen#1
TYPES : BEGIN OF gty_head,
          sel_cb  TYPE char1,
          mail_cb TYPE char1,
          icon    TYPE icon-id,
          messg   TYPE string,
          e_mail  TYPE ZSDSFIC001-e_mail.
*        INCLUDE STRUCTURE ZSDSFIT014. "DEL CH01
        INCLUDE STRUCTURE ZSDSFIS073.  "INS CH01
TYPES :

  cancel            TYPE c LENGTH 30,
  rd_doc_type_desc  TYPE ZSDSFIC005-rd_doc_typ_ds_th,
  sap_doc_type_desc TYPE text40,
  link_partner      TYPE c LENGTH 10,
  replace_doc       TYPE c LENGTH 3,
*  rd_doc_resn_desc  TYPE ZSDSFIC004-rd_doc_resn_desc,
  link_ref_doc      TYPE c LENGTH 10,
  tax_code          TYPE ZSDSFIT017-tax_code,
  vat_rate          TYPE ZSDSFIT017-vat_rate,
  gross_amt         TYPE ZSDSFIT017-net_amt_bf_vat,
  rd_vat_type       TYPE ZSDSFIC014-rd_vat_type,
  comp_name         TYPE t001-butxt,
  link_disc_charge  TYPE c LENGTH 10,
  link_vat          TYPE c LENGTH 10,
  it_item           TYPE gtty_doc_item, "TABLE OF gty_doc_item WITH NON-UNIQUE DEFAULT KEY,
  it_charge         TYPE gtty_doc_dischg,
  it_partner        TYPE gtty_doc_partner, "TABLE OF gty_doc_partner WITH NON-UNIQUE DEFAULT KEY,
  it_h_vat          TYPE gtty_doc_h_vat,
  it_h_ref          TYPE gtty_doc_h_ref, "TABLE OF gty_doc_h_ref WITH NON-UNIQUE DEFAULT KEY,
  sapobjectid       TYPE saeobjid,
  done              TYPE flag,
  status_desc       TYPE c LENGTH 50.
TYPES : END OF gty_head.

TYPES: gtty_r_module_etx      TYPE RANGE OF ZSDSFIT014-module_etx,
       gtty_r_replace_flag    TYPE RANGE OF ZSDSFIT014-replace_flag,
       gtty_r_reverse_flag    TYPE RANGE OF ZSDSFIT014-reverse_flag,
*       gtty_r_rd_flag         TYPE RANGE OF ZSDSFIT014-rd_flag,
       gtty_r_rd_doc_type_grp TYPE RANGE OF ZSDSFIT014-rd_doc_type_grp,
       gtty_r_status          TYPE RANGE OF ZSDSFIT014-status,
       gtty_where_cond        TYPE STANDARD TABLE OF char100,
       gtty_r_rd_flag         TYPE RANGE OF ZSDSFIT014-rd_flag.   "INS: <ETAX003>

*&--------------------------------------------------------------------*
*& CONSTANTS
*&--------------------------------------------------------------------*
CONSTANTS : gc_alpha TYPE char5  VALUE 'ALPHA',
            gc_gjahr TYPE char5  VALUE 'GJAHR'.

CONSTANTS: gc_yes    TYPE c LENGTH 3 VALUE 'Yes',
           gc_no     TYPE c LENGTH 3 VALUE 'No',
           gc_detail TYPE c LENGTH 10 VALUE 'Detail'.

*STRUCTURE
CONSTANTS: gc_doc_header  TYPE tabname VALUE 'ZSDSFIT014',
           gc_doc_item    TYPE tabname VALUE 'ZSDSFIT015',
           gc_doc_dischg  TYPE tabname VALUE 'ZSDSFIT013',
           gc_doc_partner TYPE tabname VALUE 'ZSDSFIT018',
           gc_doc_h_vat   TYPE tabname VALUE 'ZSDSFIT017',
           gc_doc_h_ref   TYPE tabname VALUE 'ZSDSFIT016'.

*&--------------------------------------------------------------------*
*& INTERNAL TABLE
*&--------------------------------------------------------------------*
DATA : gt_head        TYPE STANDARD TABLE OF gty_head,
       gt_doc_header  TYPE STANDARD TABLE OF gty_doc_header,
       gt_doc_item    TYPE gtty_doc_item_s,
       gt_doc_partner TYPE gtty_doc_partner_s,
       gt_doc_h_vat   TYPE gtty_doc_h_vat_s,
       gt_doc_h_ref   TYPE gtty_doc_h_ref_s,
       gt_doc_dischg  TYPE gtty_doc_dischg_s,
       gt_tvfkt       TYPE HASHED TABLE OF gty_tvfkt WITH UNIQUE KEY fkart,
       gt_t003t       TYPE HASHED TABLE OF gty_t003t WITH UNIQUE KEY blart,
       gt_rd_dischg   TYPE HASHED TABLE OF gty_rd_dischg WITH UNIQUE KEY rd_dischg,
       gt_rd_doc_type TYPE gtty_rd_doc_type,
       gt_charge      TYPE gtty_doc_dischg,
       gt_doma_add    TYPE TABLE OF dd07v.

*&--------------------------------------------------------------------*
*& VARIABLES
*&--------------------------------------------------------------------*
DATA : gv_sap_doc_no  TYPE char255,
       gv_rd_doc_type TYPE char255,
       gv_kunnr       TYPE char255,
       gv_dis_detail  TYPE flag.

*&--------------------------------------------------------------------*
*& MACRO
*&--------------------------------------------------------------------*
DEFINE m_message.
  perform throw_message using &1  "p_msgty
                              &2  "p_msgno
                              &3  "p_msgid
                              &4  "p_msgtx
                              &5. "p_displ
END-OF-DEFINITION.

DEFINE append_range.
  clear &2.
  &2-sign = 'I'.
  &2-option = 'EQ'.
  &2-low = &3.
  append &2 to &1.
END-OF-DEFINITION.

DEFINE m_add_fieldcat.
  clear &1.
  &1-fieldname = &3.
  &1-tabname   = &4.
  append &1 to &2.
END-OF-DEFINITION.

DEFINE %fcat.

  if &2 is not initial.
    &1-scrtext_s = &1-scrtext_m =
    &1-scrtext_l = &1-coltext =
    &1-seltext   = &1-tooltip =
    &1-reptext   = &2.
  endif.

  &1-checkbox  = &3.
  &1-edit      = &4.
  &1-hotspot   = &5.
  &1-tech      = &6.
  &1-just      = &7.
  &1-convexit  = &8.
  &1-col_pos   = &9.

END-OF-DEFINITION.
