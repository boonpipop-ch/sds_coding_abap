*&---------------------------------------------------------------------*
*& Report ZSDSCMR0150
*  Creation Date      : 12.05.2025
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          :
*  Description        : Report Service
*  Purpose            :
*  Copied from        :
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
REPORT zsdscmr0150.
*---------------------------------------------------------*
*                       CONSTANTS                         *
*---------------------------------------------------------*
CONSTANTS: gc_save(1) TYPE c      VALUE 'A',
           gc_eqtyp   TYPE eqtyp  VALUE 'M'.

************************************************************************
*      D E C L A R E  T A B L E & S T R U C T  U R E & V A R I A B L E *
************************************************************************
TYPE-POOLS: slis,vrm,truxs.
TABLES: crms4d_serv_h,crms4d_serv_i,TVKBT,TVGRT.
TYPES: BEGIN OF gy_out,
         objtype_h          TYPE  crms4d_serv_h-objtype_h,
         object_id          TYPE  crms4d_serv_h-object_id,
         process_type       TYPE  crms4d_serv_h-process_type,
         process_type_desc  TYPE  CRMC_PROC_TYPE_T-P_DESCRIPTION_20,
         po_number_sold     TYPE  crms4d_serv_h-po_number_sold,
         posting_date       TYPE  crms4d_serv_h-posting_date,
         description_h      TYPE  crms4d_serv_h-description_h,
         sales_office_sd    TYPE  crms4d_serv_h-sales_office_sd,
         sales_group_sd     TYPE  crms4d_serv_h-sales_group_sd,
         net_value_h        TYPE  crms4d_serv_h-net_value_h,
         net_value_man_h    TYPE  crms4d_serv_h-net_value_man_h,
         tax_amount_h       TYPE  crms4d_serv_h-tax_amount_h,
         freight_h          TYPE  crms4d_serv_h-freight_h,
         net_wo_freight_h   TYPE  crms4d_serv_h-net_wo_freight_h,
         contstart          TYPE  crms4d_serv_h-contstart,
         contend            TYPE  crms4d_serv_h-contend,
         header_guid        TYPE  crms4d_serv_h-header_guid,
         descr_language     TYPE  crms4d_serv_h-descr_language,
         created_at_h       TYPE  crms4d_serv_h-created_at_h,
         created_by_h       TYPE  crms4d_serv_h-created_by_h,
         changed_at_h       TYPE  crms4d_serv_h-changed_at_h,
         changed_by_h       TYPE  crms4d_serv_h-changed_by_h,
         head_changed_at    TYPE  crms4d_serv_h-head_changed_at,
         header_guid_char   TYPE  crms4d_serv_h-header_guid_char,
         sold_to_party      TYPE  crms4d_serv_h-sold_to_party,
         ship_to_party      TYPE  crms4d_serv_h-ship_to_party,
         bill_to_party      TYPE  crms4d_serv_h-bill_to_party,
         payer              TYPE  crms4d_serv_h-payer,
         person_resp        TYPE  crms4d_serv_h-person_resp,
         category           TYPE  crms4d_serv_h-category,
         priority           TYPE  crms4d_serv_h-priority,
         ac_object_type     TYPE  crms4d_serv_h-ac_object_type,
         ac_assignment_h    TYPE  crms4d_serv_h-ac_assignment,
         pmnttrms           TYPE  crms4d_serv_h-pmnttrms,
         stat_lifecycle_h   TYPE  crms4d_serv_h-stat_lifecycle,
         zz1_cus_po         TYPE  crms4d_serv_h-zz1_cus_po,
         ordered_prod       TYPE  crms4d_serv_i-ordered_prod,
         description_i      TYPE  crms4d_serv_i-description_i,
         order_qty          TYPE  crms4d_serv_i-order_qty,
         net_price_i        TYPE  crms4d_serv_i-net_price_i,
         itm_type           TYPE  crms4d_serv_i-itm_type,
         itm_type_desc      TYPE  CRMC_ITEM_TYPE_T-I_DESCRIPTION_20,
         prod_hierarchy     TYPE  crms4d_serv_i-prod_hierarchy,
         zz1_lgort          TYPE  crms4d_serv_i-zz1_lgort,
         process_type_i     TYPE  crms4d_serv_i-process_type,

         ac_assignment_i    TYPE  crms4d_serv_i-ac_assignment,
         product_id         TYPE  crms4d_serv_i-product_id,
         stat_lifecycle_i   TYPE  crms4d_serv_i-stat_lifecycle,
         aufnr              TYPE  iaom_crm_aufk-aufnr,
         process_type_iaom  TYPE  iaom_crm_aufk-process_type,
         itm_type_iaom      TYPE  iaom_crm_aufk-itm_type,
         sold_to_code       TYPE crms4d_partner-partner_id,
         sold_to_name_th(100)  TYPE c,
         sold_to_name_en(100)  TYPE c,
         sold_to_addr1(100) TYPE c,
         sold_to_addr2(100) TYPE c,
         sold_to_district   TYPE adrc-city2,
         sold_to_city       TYPE adrc-city1,
         sold_to_postcodey  TYPE adrc-post_code1,
         payler_code        TYPE crms4d_partner-partner_id,
         payler_name_th(100)   TYPE c,
         payler_name_en(100)   TYPE c,
         payler_addr1(100)  TYPE c,
         payler_addr2(100)  TYPE c,
         payler_district    TYPE adrc-city2,
         payler_city        TYPE adrc-city1,
         payler_postcode    TYPE adrc-post_code1,
         ship_to_code       TYPE crms4d_partner-partner_id,
         ship_to_name_th(100)  TYPE c,
         ship_to_name_en(100)  TYPE c,
         ship_to_addr1(100) TYPE c,
         ship_to_addr2(100) TYPE c,
         ship_to_district   TYPE adrc-city2,
         ship_to_city       TYPE adrc-city1,
         ship_to_postcode   TYPE adrc-post_code1,
         tel                TYPE adrc-tel_number,
         bdr_vbeln          TYPE  vbrk-vbeln,
         bdr_fkart          TYPE  vbrk-fkart,
         bdr_ernam          TYPE  vbrk-ernam,
         bdr_erzet          TYPE  vbrk-erzet,
         bdr_erdat          TYPE  vbrk-erdat,
         vbeln              TYPE  vbrk-vbeln,
         fkart              TYPE  vbrk-fkart,
         ernam              TYPE  vbrk-ernam,
         erzet              TYPE  vbrk-erzet,
         erdat              TYPE  vbrk-erdat,
         belnr              TYPE  vbrk-belnr,
         netwr              TYPE vbrp-netwr,
         mblnr              TYPE  mkpf-mblnr,
         mjahr              TYPE  mkpf-mjahr,
         budat              TYPE  mkpf-budat,
         zeile              TYPE  mseg-zeile,
         lifnr              TYPE  mseg-lifnr,
         dmbtr              TYPE  mseg-dmbtr,
         menge              TYPE  mseg-menge,
         aufnr_mseg         TYPE  mseg-aufnr,
         matnr              TYPE  mseg-matnr,
         ebeln              TYPE  mseg-ebeln,
         ebelp              TYPE  mseg-ebelp,
         banfn TYPE  eban-banfn,
         bnfpo TYPE  eban-bnfpo,
         sales_office      TYPE TVKBT-BEZEI,
         sales_group       TYPE TVGRT-BEZEI,
         MA_NO             TYPE crms4d_serv_h-object_id,
       END OF gy_out.

TYPES: BEGIN OF gy_crms4d_serv_h_i,
         objtype_h           TYPE  crms4d_serv_h-objtype_h,
         object_id           TYPE  crms4d_serv_h-object_id,
         process_type        TYPE  crms4d_serv_h-process_type,
         po_number_sold      TYPE  crms4d_serv_h-po_number_sold,
         posting_date        TYPE  crms4d_serv_h-posting_date,
         description_h       TYPE  crms4d_serv_h-description_h,
         sales_office_sd     TYPE  crms4d_serv_h-sales_office_sd,
         sales_group_sd      TYPE  crms4d_serv_h-sales_group_sd,
         net_value_h         TYPE  crms4d_serv_h-net_value_h,
         net_value_man_h     TYPE  crms4d_serv_h-net_value_man_h,
         tax_amount_h        TYPE  crms4d_serv_h-tax_amount_h,
         freight_h           TYPE  crms4d_serv_h-freight_h,
         net_wo_freight_h    TYPE  crms4d_serv_h-net_wo_freight_h,
         contstart           TYPE  crms4d_serv_h-contstart,
         contend             TYPE  crms4d_serv_h-contend,
         header_guid         TYPE  crms4d_serv_h-header_guid,
         descr_language      TYPE  crms4d_serv_h-descr_language,
         created_at_h        TYPE  crms4d_serv_h-created_at_h,
         created_by_h        TYPE  crms4d_serv_h-created_by_h,
         changed_at_h        TYPE  crms4d_serv_h-changed_at_h,
         changed_by_h        TYPE  crms4d_serv_h-changed_by_h,
         head_changed_at     TYPE  crms4d_serv_h-head_changed_at,
         header_guid_char    TYPE  crms4d_serv_h-header_guid_char,
         sold_to_party       TYPE  crms4d_serv_h-sold_to_party,
         ship_to_party       TYPE  crms4d_serv_h-ship_to_party,
         bill_to_party       TYPE  crms4d_serv_h-bill_to_party,
         payer               TYPE  crms4d_serv_h-payer,
         person_resp         TYPE  crms4d_serv_h-person_resp,
         category            TYPE  crms4d_serv_h-category,
         priority            TYPE  crms4d_serv_h-priority,
         ac_object_type      TYPE  crms4d_serv_h-ac_object_type,
         ac_assignment       TYPE  crms4d_serv_h-ac_assignment,
         pmnttrms            TYPE  crms4d_serv_h-pmnttrms,
         stat_lifecycle      TYPE  crms4d_serv_h-stat_lifecycle,
         stat_released       TYPE  crms4d_serv_h-stat_released,
         stat_quotation      TYPE  crms4d_serv_h-stat_quotation,
         stat_error          TYPE  crms4d_serv_h-stat_error,
         stat_delivery       TYPE  crms4d_serv_h-stat_delivery,
         stat_goods_issue    TYPE  crms4d_serv_h-stat_goods_issue,
         stat_billing        TYPE  crms4d_serv_h-stat_billing,
         stat_cancelled      TYPE  crms4d_serv_h-stat_cancelled,
         stat_open           TYPE  crms4d_serv_h-stat_open,
         stat_maintenance    TYPE  crms4d_serv_h-stat_maintenance,
         stat_transfer       TYPE  crms4d_serv_h-stat_transfer,
         stat_archivable     TYPE  crms4d_serv_h-stat_archivable,
         stat_archived       TYPE  crms4d_serv_h-stat_archived,
         incoterms1          TYPE  crms4d_serv_h-incoterms1,
         incoterms2          TYPE  crms4d_serv_h-incoterms2,
         ship_cond           TYPE  crms4d_serv_h-ship_cond,
         zz1_lob_srh         TYPE  crms4d_serv_h-zz1_lob_srh,
         zz1_proactivity     TYPE  crms4d_serv_h-zz1_proactivity,
         zz1_bill_method     TYPE  crms4d_serv_h-zz1_bill_method,
         zz1_ext_refno       TYPE  crms4d_serv_h-zz1_ext_refno,
         zz1_cus_po          TYPE  crms4d_serv_h-zz1_cus_po,
         zz1_inv_remark      TYPE  crms4d_serv_h-zz1_inv_remark,
         zz1_partner_segment TYPE  crms4d_serv_h-zz1_partner_segment,
         zz1_delivery_ord    TYPE  crms4d_serv_h-zz1_delivery_ord,
         zz1_break_reason    TYPE  crms4d_serv_h-zz1_break_reason,
         zz1_break_position  TYPE  crms4d_serv_h-zz1_break_position,
         zz1_result_progress TYPE  crms4d_serv_h-zz1_result_progress,
         zz1_error_code      TYPE  crms4d_serv_h-zz1_error_code,
         zz1_main_cause      TYPE  crms4d_serv_h-zz1_main_cause,
*         objtype_h            TYPE  crms4d_serv_i-objtype_h,
*         object_id            TYPE  crms4d_serv_i-object_id,
         number_int           TYPE  crms4d_serv_i-number_int,
         po_number_sold_i       TYPE  crms4d_serv_i-po_number_sold,
         ordered_prod         TYPE  crms4d_serv_i-ordered_prod,
         description_i        TYPE  crms4d_serv_i-description_i,
         orig_order_qty       TYPE  crms4d_serv_i-orig_order_qty,
         order_qty            TYPE  crms4d_serv_i-order_qty,
         confirmed_qty        TYPE  crms4d_serv_i-confirmed_qty,
         net_price_i          TYPE  crms4d_serv_i-net_price_i,
         net_value_i          TYPE  crms4d_serv_i-net_value_i,
         net_value_man_i      TYPE  crms4d_serv_i-net_value_man_i,
         netpr_pric_unit      TYPE  crms4d_serv_i-netpr_pric_unit,
         itm_type             TYPE  crms4d_serv_i-itm_type,
         sales_org_sd         TYPE  crms4d_serv_i-sales_org_sd,
         sales_office_sd_i      TYPE  crms4d_serv_i-sales_office_sd,
         sales_group_sd_i       TYPE  crms4d_serv_i-sales_group_sd,
         sold_to_party_i        TYPE  crms4d_serv_i-sold_to_party,
         sold_to_region       TYPE  crms4d_serv_i-sold_to_region,
         sold_to_country      TYPE  crms4d_serv_i-sold_to_country,
         ship_to_party_i        TYPE  crms4d_serv_i-ship_to_party,
         bill_to_party_i        TYPE  crms4d_serv_i-bill_to_party,
         payer_i                TYPE  crms4d_serv_i-payer,
         person_resp_i          TYPE  crms4d_serv_i-person_resp,
         pmnttrms_i             TYPE  crms4d_serv_i-pmnttrms,
         prod_hierarchy       TYPE  crms4d_serv_i-prod_hierarchy,
         process_type_i         TYPE  crms4d_serv_i-process_type,
         description_h_i        TYPE  crms4d_serv_i-description_h,
         zz1_lgort            TYPE  crms4d_serv_i-zz1_lgort,
         zz1_werks            TYPE  crms4d_serv_i-zz1_werks,
         zz1_vendor_team      TYPE  crms4d_serv_i-zz1_vendor_team,
         ac_object_type_i       TYPE  crms4d_serv_i-ac_object_type,
         ac_assignment_i        TYPE  crms4d_serv_i-ac_assignment,
         product_id           TYPE  crms4d_serv_i-product_id,
         item_guid            TYPE  crms4d_serv_i-item_guid,
         stat_released_i        TYPE  crms4d_serv_i-stat_released,
         stat_activation      TYPE  crms4d_serv_i-stat_activation,
         stat_accepted        TYPE  crms4d_serv_i-stat_accepted,
         stat_billing_i         TYPE  crms4d_serv_i-stat_billing,
         stat_bsln_cost_postg TYPE  crms4d_serv_i-stat_bsln_cost_postg,
         stat_cancelled_i       TYPE  crms4d_serv_i-stat_cancelled,
         stat_chklst_exec     TYPE  crms4d_serv_i-stat_chklst_exec,
         stat_cont_ts         TYPE  crms4d_serv_i-stat_cont_ts,
         stat_credit          TYPE  crms4d_serv_i-stat_credit,
         stat_delivery_i        TYPE  crms4d_serv_i-stat_delivery,
         stat_error_i           TYPE  crms4d_serv_i-stat_error,
         stat_for_billing     TYPE  crms4d_serv_i-stat_for_billing,
         stat_goods_issue_i     TYPE  crms4d_serv_i-stat_goods_issue,
         stat_lifecycle_i       TYPE  crms4d_serv_i-stat_lifecycle,
         stat_maintenance_i     TYPE  crms4d_serv_i-stat_maintenance,
         stat_open_i            TYPE  crms4d_serv_i-stat_open,
         stat_quotation_i       TYPE  crms4d_serv_i-stat_quotation,
         stat_transfer_i        TYPE  crms4d_serv_i-stat_transfer,
         stat_withdrawn_i       TYPE  crms4d_serv_i-stat_withdrawn,
         created_by_i         TYPE  crms4d_serv_i-created_by_i,
         changed_by_i         TYPE  crms4d_serv_i-changed_by_i,
         objtype_i            TYPE  crms4d_serv_i-objtype_i,
       END OF gy_crms4d_serv_h_i.
TYPES: BEGIN OF gy_crms4d_partner,
         objtype_h    TYPE  crms4d_partner-objtype_h,
         object_id    TYPE  crms4d_partner-object_id,
         number_int   TYPE  crms4d_partner-number_int,
         partner_fct  TYPE  crms4d_partner-partner_fct,
         partner_no   TYPE  crms4d_partner-partner_no,
         no_type      TYPE  crms4d_partner-no_type,
         addr_nr      TYPE  crms4d_partner-addr_nr,
         addr_np      TYPE  crms4d_partner-addr_np,
         addr_type    TYPE  crms4d_partner-addr_type,
         display_type TYPE  crms4d_partner-display_type,
         partner_pft  TYPE  crms4d_partner-partner_pft,
         partner_id   TYPE  crms4d_partner-partner_id,

       END OF gy_crms4d_partner.
TYPES: BEGIN OF gy_iaom_crm_aufk,
         ext_object_id    TYPE  iaom_crm_aufk-ext_object_id,
         aufnr            TYPE  iaom_crm_aufk-aufnr,
         auart            TYPE  iaom_crm_aufk-auart,
         object_id        TYPE  iaom_crm_aufk-object_id,
         process_descript TYPE  iaom_crm_aufk-process_descript,
         process_type     TYPE  iaom_crm_aufk-process_type,
         number_int       TYPE  iaom_crm_aufk-number_int,
         item_description TYPE  iaom_crm_aufk-item_description,
         itm_type         TYPE  iaom_crm_aufk-itm_type,
         category_id      TYPE  iaom_crm_aufk-category_id,
         kunnr            TYPE  iaom_crm_aufk-kunnr,
       END OF gy_iaom_crm_aufk.
TYPES: BEGIN OF gy_crmd_brelvonae,
         relationid    TYPE  crmd_brelvonae-relationid,
         posno         TYPE  crmd_brelvonae-posno,
         breltyp       TYPE  crmd_brelvonae-breltyp,
         attribut      TYPE  crmd_brelvonae-attribut,
         vona_kind     TYPE  crmd_brelvonae-vona_kind,
         invalid       TYPE  crmd_brelvonae-invalid,
         secondary     TYPE  crmd_brelvonae-secondary,
         seqno         TYPE  crmd_brelvonae-seqno,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         objkey_a_sel  TYPE  crmd_brelvonae-objkey_a_sel,
         objtype_a_sel TYPE  crmd_brelvonae-objtype_a_sel,
         logsys_a_sel  TYPE  crmd_brelvonae-logsys_a_sel,
         objguid_b_sel TYPE  crmd_brelvonae-objguid_b_sel,
         objkey_b_sel  TYPE  crmd_brelvonae-objkey_b_sel,
         objtype_b_sel TYPE  crmd_brelvonae-objtype_b_sel,
       END OF gy_crmd_brelvonae.
TYPES: BEGIN OF gy_DD07L,
         domname    TYPE  dd07l-domname,
         as4local   TYPE  dd07l-as4local,
         valpos     TYPE  dd07l-valpos,
         as4vers    TYPE  dd07l-as4vers,
         domvalue_l TYPE  dd07l-domvalue_l,
         ddlanguage TYPE  dd07t-ddlanguage,
         ddtext     TYPE  dd07t-ddtext,

       END OF gy_DD07L.
TYPES: BEGIN OF gy_VBRK,

         vbeln    TYPE  vbrk-vbeln,
         fkart    TYPE  vbrk-fkart,
         ernam    TYPE  vbrk-ernam,
         erzet    TYPE  vbrk-erzet,
         erdat    TYPE  vbrk-erdat,
         belnr    TYPE  vbrk-belnr,
         bstnk_vf TYPE vbrk-bstnk_vf,
         posnr    TYPE vbrp-posnr,
         matnr    TYPE vbrp-matnr,
         netwr    TYPE vbrp-netwr,
         aubel    TYPE vbrp-aubel,
         aufnr    TYPE vbrp-aufnr,
         PS_PSP_PNR TYPE vbrp-PS_PSP_PNR,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         ac_assignment TYPE crms4d_serv_i-ac_assignment,
       END OF gy_VBRK.
TYPES: BEGIN OF gy_CRMS4D_BILLREQ_I,  "Billing plan
         objtype_h  TYPE  crms4d_billreq_i-objtype_h,
         object_id  TYPE  crms4d_billreq_i-object_id,
         bill_date  TYPE  crms4d_billreq_i-bill_date,
         price_date TYPE  crms4d_billreq_i-price_date,
         net_value  TYPE  crms4d_billreq_i-net_value,
       END OF gy_CRMS4D_BILLREQ_I.
TYPES: BEGIN OF gy_MKPF_MSEG,

         mblnr TYPE  mkpf-mblnr,
         mjahr TYPE  mkpf-mjahr,
         budat TYPE  mkpf-budat,
         zeile TYPE  mseg-zeile,
         lifnr TYPE  mseg-lifnr,
         dmbtr TYPE  mseg-dmbtr,
         erfmg TYPE  mseg-erfmg,
         aufnr TYPE  mseg-aufnr,
         matnr TYPE  mseg-matnr,
         ebeln TYPE  mseg-ebeln,
         ebelp TYPE  mseg-ebelp,
         PS_PSP_PNR TYPE mseg-PS_PSP_PNR,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         ac_assignment TYPE crms4d_serv_i-ac_assignment,
       END OF gy_MKPF_MSEG.
TYPES: BEGIN OF typ_adrc,
         addrnumber TYPE adrc-addrnumber,      "Address Number
         name1      TYPE adrc-name1,      "Name
         name2      TYPE adrc-name2,      "Name2
         street     TYPE adrc-street,     "Street
         str_suppl3 TYPE adrc-str_suppl3, " street4
         location   TYPE adrc-location, "Street 5
         city2      TYPE adrc-city2,      "District
         city1      TYPE adrc-city1,      "City
         post_code1 TYPE adrc-post_code1, "post code
         tel_number TYPE adrc-tel_number, "telephone
         nation     TYPE adrc-nation,                     "Name2
       END OF typ_adrc .



TYPES: BEGIN OF gy_EKKO_EKKN,

         ebeln TYPE  ekko-ebeln,
         bsart TYPE  ekko-bsart,
         lifnr TYPE  ekko-lifnr,
         ebelp TYPE  ekpo-ebelp,
         matnr TYPE  ekpo-matnr,
         menge TYPE  ekpo-menge,
         netpr TYPE  ekpo-netpr,
         aufnr TYPE  ekkn-aufnr,
         prctr TYPE  ekkn-prctr,
         PS_PSP_PNR TYPE  ekkn-PS_PSP_PNR,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         ac_assignment TYPE crms4d_serv_i-ac_assignment,
       END OF gy_EKKO_EKKN.
TYPES: BEGIN OF gy_EBAN_EBKN,
         banfn TYPE  eban-banfn,
         bnfpo TYPE  eban-bnfpo,
         matnr TYPE  eban-matnr,
         menge TYPE  eban-menge,
         PREIS TYPE  eban-PREIS,
         aufnr TYPE  ebkn-aufnr,
         prctr TYPE  ebkn-prctr,
         PS_PSP_PNR TYPE  ebkn-PS_PSP_PNR,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
         ac_assignment TYPE crms4d_serv_i-ac_assignment,
       END OF gy_EBAN_EBKN.
TYPES: BEGIN OF gy_CONFIRMATION,
         GUID TYPE  CRMS4D_BTX_H-GUID,
         OBJTYPE_H TYPE  CRMS4D_BTX_H-OBJTYPE_H,
         OBJECT_ID TYPE  CRMS4D_BTX_H-OBJECT_ID,
         PROCESS_TYPE TYPE  CRMS4D_BTX_H-PROCESS_TYPE,
         objguid_a_sel TYPE  crmd_brelvonae-objguid_a_sel,
       END OF gy_CONFIRMATION.

TYPES: BEGIN OF GY_TVKBT,
   VKBUR      TYPE VBAK-VKBUR,     "SALE OFFICE
   BEZEI      TYPE TVKBT-BEZEI,    "SALE OFFICE DETAIL
END OF GY_TVKBT.

TYPES: BEGIN OF GY_TVGRT,
   VKGRP          TYPE VBAK-VKGRP,          "SALE GROUP
   BEZEI          TYPE TVGRT-BEZEI,         "SALE GROUP - TEXT
END OF GY_TVGRT.

TYPES: BEGIN OF GY_CRMC_PROC_TYPE_T,
   PROCESS_TYPE   TYPE CRMC_PROC_TYPE_T-PROCESS_TYPE,
   P_DESCRIPTION_20 TYPE CRMC_PROC_TYPE_T-P_DESCRIPTION_20,
END OF GY_CRMC_PROC_TYPE_T.

TYPES: BEGIN OF GY_CRMC_ITEM_TYPE_T,
   ITM_TYPE   TYPE CRMC_ITEM_TYPE_T-ITM_TYPE,
   I_DESCRIPTION_20 TYPE CRMC_ITEM_TYPE_T-I_DESCRIPTION_20,
END OF GY_CRMC_ITEM_TYPE_T.

TYPES: BEGIN OF GY_MA_OBJECT,
  OBJECT_ID  TYPE crms4d_serv_h-object_id,
  header_guid         TYPE  crms4d_serv_h-header_guid,
END OF GY_MA_OBJECT.

 TYPES: BEGIN OF GY_MA_REFER,
    OBJECT_ID     TYPE CRMS4D_SERV_H-OBJECT_ID,
    OBJGUID_A_SEL TYPE CRMD_BRELVONAE-OBJGUID_A_SEL,
    OBJGUID_B_SEL TYPE CRMD_BRELVONAE-OBJGUID_B_SEL,
    OBJTYPE_B_SEL TYPE CRMD_BRELVONAE-OBJTYPE_B_SEL,
 END OF GY_MA_REFER.


DATA: gt_output           TYPE TABLE OF gy_out,
      gs_output           TYPE gy_out,
      wa_output           TYPE gy_out,
      gw_output           TYPE gy_out,
      gt_crms4d_serv_h_i    TYPE STANDARD TABLE OF gy_crms4d_serv_h_i,
      gs_crms4d_serv_h_i    TYPE  gy_crms4d_serv_h_i,
*      gt_crms4d_serv_i    TYPE STANDARD TABLE OF gy_crms4d_serv_i,
*      gs_crms4d_serv_i    TYPE  gy_crms4d_serv_i,
      gt_crms4d_partner   TYPE STANDARD TABLE OF gy_crms4d_partner,
      gs_crms4d_partner   TYPE  gy_crms4d_partner,
      gt_iaom_crm_aufk    TYPE STANDARD TABLE OF gy_iaom_crm_aufk,
      gs_iaom_crm_aufk    TYPE  gy_iaom_crm_aufk,
      gt_crmd_brelvonae   TYPE STANDARD TABLE OF gy_crmd_brelvonae,
      gs_crmd_brelvonae   TYPE  gy_crmd_brelvonae,
      gt_DD07L            TYPE STANDARD TABLE OF gy_DD07L,
      gs_DD07L            TYPE  gy_DD07L,
      gt_VBRK             TYPE STANDARD TABLE OF gy_VBRK,
      gt_VBRK_temp             TYPE STANDARD TABLE OF gy_VBRK,
      gs_VBRK             TYPE  gy_VBRK,
      gt_CRMS4D_BILLREQ_I TYPE STANDARD TABLE OF gy_CRMS4D_BILLREQ_I,
      gs_CRMS4D_BILLREQ_I TYPE  gy_CRMS4D_BILLREQ_I,
      gt_MKPF_MSEG        TYPE STANDARD TABLE OF gy_MKPF_MSEG,
      gt_MKPF_MSEG_temp        TYPE STANDARD TABLE OF gy_MKPF_MSEG,
      gs_MKPF_MSEG        TYPE  gy_MKPF_MSEG,
      gt_ADRC             TYPE STANDARD TABLE OF typ_adrc,
      gs_ADRC             TYPE  typ_adrc,
      gt_EKKO_EKKN   TYPE STANDARD TABLE OF gy_EKKO_EKKN,
      gt_EKKO_EKKN_temp   TYPE STANDARD TABLE OF gy_EKKO_EKKN,
      gs_EKKO_EKKN   TYPE  gy_EKKO_EKKN,
      gt_EBAN_EBKN   TYPE STANDARD TABLE OF gy_EBAN_EBKN,
      gt_EBAN_EBKN_temp   TYPE STANDARD TABLE OF gy_EBAN_EBKN,
      gs_EBAN_EBKN   TYPE  gy_EBAN_EBKN,
      gt_CONFIRMATION   TYPE STANDARD TABLE OF gy_CONFIRMATION,
      gt_CONFIRMATION_temp   TYPE STANDARD TABLE OF gy_CONFIRMATION,
      gs_CONFIRMATION   TYPE  gy_CONFIRMATION,
      gt_tvkbt              TYPE STANDARD TABLE OF gy_tvkbt,
      gs_tvkbt              TYPE gy_tvkbt,
      gt_tvgrt              TYPE STANDARD TABLE OF gy_tvgrt,
      gs_tvgrt              TYPE gy_tvgrt,
      gt_CRMC_PROC_TYPE_T   TYPE STANDARD TABLE OF GY_CRMC_PROC_TYPE_T,
      gs_CRMC_PROC_TYPE_T   TYPE GY_CRMC_PROC_TYPE_T,
      Gt_CRMC_ITEM_TYPE_T   TYPE STANDARD TABLE OF GY_CRMC_ITEM_TYPE_T,
      Gs_CRMC_ITEM_TYPE_T   TYPE GY_CRMC_ITEM_TYPE_T,
      GT_MA_OBJECT          TYPE STANDARD TABLE OF GY_MA_OBJECT,
      GS_MA_OBJECT          TYPE GY_MA_OBJECT.

 DATA: GT_MA_REFER TYPE STANDARD TABLE OF   GY_MA_REFER,
       GS_MA_REFER TYPE GY_MA_REFER.

DATA: msg_show(200) TYPE c.



DATA: GRID_MAIN      TYPE REF TO CL_GUI_ALV_GRID,
      CONTAINER_MAIN TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_VARIANT     LIKE DISVARIANT.

DATA : V_POS TYPE I .
DATA : GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
       GT_EVENTS   TYPE SLIS_T_EVENT.
RANGES : GR_TCODE FOR CDHDR-TCODE.

DATA: bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE.
DATA:     g_tc_100_lines  LIKE sy-loopc.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

DATA: lv_linecount TYPE i.
DATA: fill TYPE i .
CONTROLS: tc_100 TYPE TABLEVIEW USING SCREEN 0100.

RANGES : GAC_ASSIGNMENT FOR CRMS4D_SERV_I-AC_ASSIGNMENT.
RANGES : GAC_OBJECT_ID FOR CRMS4D_SERV_H-OBJECT_ID.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS : GC_REPID       TYPE REPID         VALUE 'ZSDSCMR0150',
            GC_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
CONSTANTS: gc_mask_time   TYPE char8  VALUE '__:__',
           gc_mask_date   TYPE char10 VALUE '__/__/____'.
*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N                    *
*&---------------------------------------------------------------------*
DEFINE m_fill_cat.
  gs_fieldcat-tabname    = &1.
  gs_fieldcat-fieldname  = &2.
  gs_fieldcat-col_pos    = &3.
  gs_fieldcat-seltext_l  = &4.
  gs_fieldcat-no_out     = &5.
  gs_fieldcat-outputlen  = &6.
*  gs_fieldcat-input       = &7.
  gs_fieldcat-checkbox    = &7.
  gs_fieldcat-edit        = &8.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

END-OF-DEFINITION.

DEFINE def_list_head.
  clear: lw_listline.
  lw_listline-typ  = &1.
  lw_listline-key  = &2.
  lw_listline-info = &3.
  append lw_listline to lt_listhead.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N  ( %_ )            *
*&---------------------------------------------------------------------*
DEFINE %show. " show input field
  IF screen-group1 = &1.
    screen-invisible = 0.
    screen-active = 1.
  ENDIF.
END-OF-DEFINITION.
DEFINE %hide. " hide input field
  IF screen-group1 = &1.
    screen-invisible = 1.
    screen-active = 0.
  ENDIF.
END-OF-DEFINITION.
************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.

  SELECT-OPTIONS : "s_OBJT_H FOR crms4d_serv_h-objtype_h,
                   s_OBJ_ID FOR crms4d_serv_h-object_id,
                   s_PRO_TY FOR crms4d_serv_h-process_type,
                   s_SF_NO FOR crms4d_serv_h-po_number_sold,
                   s_IO FOR crms4d_serv_i-ac_assignment,
                   s_WBS FOR crms4d_serv_i-ac_assignment,
                   s_VKBUR FOR TVKBT-VKBUR,
                   s_VKGRP FOR TVGRT-VKGRP,
                   s_date  FOR crms4d_serv_h-POSTING_DATE,
                   S_MA_NO FOR crms4d_serv_h-object_id.



SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
*      A T     S E L E C T I O N     S C R E E N                       *
************************************************************************
INITIALIZATION.




AT SELECTION-SCREEN OUTPUT.

************************************************************************
*      B E G I N      S E L E C T I O N                                *
************************************************************************
START-OF-SELECTION .
  PERFORM F_CONVERT_DATA_IO.
  IF S_MA_NO IS NOT INITIAL.
     PERFORM F_CONVERT_DATA_JOB_MA.
  ENDIF.

  PERFORM f_get_data.
  PERFORM f_map_data.



************************************************************************
*      E N D      S E L E C T I O N                                    *
************************************************************************
END-OF-SELECTION .

  IF gt_output[] IS NOT INITIAL.
    PERFORM f_display_report.
  ELSE.
    MESSAGE i000(38) WITH TEXT-e04 DISPLAY LIKE 'E'.
  ENDIF.






*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_UNMAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .
  DATA: lv_banfn TYPE eban-banfn,
        lv_ebeln TYPE ekko-ebeln,
        lv_vbeln TYPE vbrk-vbeln,
        lv_mblnr TYPE mkpf-mblnr,
        lv_mjahr TYPE mkpf-mjahr,
        lv_objguid_a_sel TYPE crmd_brelvonae-objguid_a_sel,
        lv_objkey_b_sel TYPE crmd_brelvonae-objkey_b_sel.

  IF S_MA_NO IS NOT INITIAL.
     SELECT  a~objtype_h,a~object_id,a~process_type,a~po_number_sold,a~posting_date,a~description_h,
            a~sales_office_sd,a~sales_group_sd,a~net_value_h,a~net_value_man_h,a~tax_amount_h,
            a~freight_h,a~net_wo_freight_h,a~contstart,a~contend,a~header_guid,a~descr_language,a~created_at_h,
            a~created_by_h,a~changed_at_h,a~changed_by_h,a~head_changed_at,a~header_guid_char,a~sold_to_party,
            a~ship_to_party,a~bill_to_party,a~payer,a~person_resp,a~category,a~priority,a~ac_object_type,
            a~ac_assignment,a~pmnttrms,a~stat_lifecycle,a~stat_released,a~stat_quotation,a~stat_error,
            a~stat_delivery,a~stat_goods_issue,a~stat_billing,a~stat_cancelled,a~stat_open,a~stat_maintenance,
            a~stat_transfer,a~stat_archivable,a~stat_archived,a~incoterms1,a~incoterms2,a~ship_cond,
            a~zz1_lob_srh,a~zz1_proactivity,a~zz1_bill_method,a~zz1_ext_refno,a~zz1_cus_po,a~zz1_inv_remark,
            a~zz1_partner_segment,a~zz1_delivery_ord,a~zz1_break_reason,a~zz1_break_position,a~zz1_result_progress,
            a~zz1_error_code,a~zz1_main_cause,b~number_int,
            b~po_number_sold,b~ordered_prod,b~description_i,b~orig_order_qty,b~order_qty,b~confirmed_qty,
            b~net_price_i,b~net_value_i,b~net_value_man_i,b~netpr_pric_unit,b~itm_type,
            b~sales_org_sd,b~sales_office_sd,b~sales_group_sd,b~sold_to_party,b~sold_to_region,
            b~sold_to_country,b~ship_to_party,b~bill_to_party,b~payer,b~person_resp,b~pmnttrms,
            b~prod_hierarchy,b~process_type,b~description_h,b~zz1_lgort,b~zz1_werks,b~zz1_vendor_team,
            b~ac_object_type,b~ac_assignment,b~product_id,b~item_guid,b~stat_released,b~stat_activation,
            b~stat_accepted,b~stat_billing,b~stat_bsln_cost_postg,b~stat_cancelled,b~stat_chklst_exec,
            b~stat_cont_ts,b~stat_credit,b~stat_delivery,b~stat_error,b~stat_for_billing,b~stat_goods_issue,
            b~stat_lifecycle,b~stat_maintenance,b~stat_open,b~stat_quotation,b~stat_transfer,
            b~stat_withdrawn,b~created_by_i,b~changed_by_i,b~objtype_i
    INTO TABLE @gt_crms4d_serv_h_i
      FROM crms4d_serv_h AS a INNER JOIN crms4d_serv_i AS b
                                ON ( a~objtype_h EQ b~objtype_h
                                AND  a~object_id EQ b~object_id )
*                                 ON ( a~object_id EQ b~object_id )
      FOR ALL ENTRIES IN @GT_MA_REFER
      WHERE a~object_id EQ @GT_MA_REFER-OBJECT_ID.

  ELSE.
     SELECT  a~objtype_h,a~object_id,a~process_type,a~po_number_sold,a~posting_date,a~description_h,
            a~sales_office_sd,a~sales_group_sd,a~net_value_h,a~net_value_man_h,a~tax_amount_h,
            a~freight_h,a~net_wo_freight_h,a~contstart,a~contend,a~header_guid,a~descr_language,a~created_at_h,
            a~created_by_h,a~changed_at_h,a~changed_by_h,a~head_changed_at,a~header_guid_char,a~sold_to_party,
            a~ship_to_party,a~bill_to_party,a~payer,a~person_resp,a~category,a~priority,a~ac_object_type,
            a~ac_assignment,a~pmnttrms,a~stat_lifecycle,a~stat_released,a~stat_quotation,a~stat_error,
            a~stat_delivery,a~stat_goods_issue,a~stat_billing,a~stat_cancelled,a~stat_open,a~stat_maintenance,
            a~stat_transfer,a~stat_archivable,a~stat_archived,a~incoterms1,a~incoterms2,a~ship_cond,
            a~zz1_lob_srh,a~zz1_proactivity,a~zz1_bill_method,a~zz1_ext_refno,a~zz1_cus_po,a~zz1_inv_remark,
            a~zz1_partner_segment,a~zz1_delivery_ord,a~zz1_break_reason,a~zz1_break_position,a~zz1_result_progress,
            a~zz1_error_code,a~zz1_main_cause,b~number_int,
            b~po_number_sold,b~ordered_prod,b~description_i,b~orig_order_qty,b~order_qty,b~confirmed_qty,
            b~net_price_i,b~net_value_i,b~net_value_man_i,b~netpr_pric_unit,b~itm_type,
            b~sales_org_sd,b~sales_office_sd,b~sales_group_sd,b~sold_to_party,b~sold_to_region,
            b~sold_to_country,b~ship_to_party,b~bill_to_party,b~payer,b~person_resp,b~pmnttrms,
            b~prod_hierarchy,b~process_type,b~description_h,b~zz1_lgort,b~zz1_werks,b~zz1_vendor_team,
            b~ac_object_type,b~ac_assignment,b~product_id,b~item_guid,b~stat_released,b~stat_activation,
            b~stat_accepted,b~stat_billing,b~stat_bsln_cost_postg,b~stat_cancelled,b~stat_chklst_exec,
            b~stat_cont_ts,b~stat_credit,b~stat_delivery,b~stat_error,b~stat_for_billing,b~stat_goods_issue,
            b~stat_lifecycle,b~stat_maintenance,b~stat_open,b~stat_quotation,b~stat_transfer,
            b~stat_withdrawn,b~created_by_i,b~changed_by_i,b~objtype_i
    INTO TABLE @gt_crms4d_serv_h_i
      FROM crms4d_serv_h AS a INNER JOIN crms4d_serv_i AS b
                                ON ( a~objtype_h EQ b~objtype_h
                                AND  a~object_id EQ b~object_id )
*                                 ON ( a~object_id EQ b~object_id )
      WHERE a~object_id IN @s_OBJ_ID
      AND   a~process_type IN @s_PRO_TY
      AND   a~po_number_sold IN @s_SF_NO
      AND   b~ac_assignment IN @GAC_ASSIGNMENT
      AND   a~sales_office_sd IN @s_VKBUR
      AND   a~sales_group_sd IN @s_VKGRP.

  ENDIF.





      IF gt_crms4d_serv_h_i IS NOT INITIAL.

         SELECT objtype_h,object_id,number_int,partner_fct,partner_no,
                no_type,addr_nr,addr_np,addr_type,display_type,partner_pft,
                partner_id
         INTO TABLE @gt_crms4d_partner
         FROM crms4d_partner
         FOR ALL ENTRIES IN @gt_crms4d_serv_h_i
         WHERE objtype_h = @gt_crms4d_serv_h_i-objtype_h
           AND object_id = @gt_crms4d_serv_h_i-object_id.

        SELECT ext_object_id,aufnr,auart,object_id,process_descript,process_type,
               number_int,item_description,itm_type,category_id,kunnr
         INTO TABLE @gt_iaom_crm_aufk
         FROM iaom_crm_aufk
         FOR ALL ENTRIES IN @gt_crms4d_serv_h_i
         WHERE object_id = @gt_crms4d_serv_h_i-object_id
          AND process_type = @gt_crms4d_serv_h_i-process_type.

        SELECT relationid,posno,breltyp,attribut,vona_kind,invalid,secondary,
               seqno,objguid_a_sel,objkey_a_sel,objtype_a_sel,logsys_a_sel,
               objguid_b_sel,objkey_b_sel,objtype_b_sel
         INTO TABLE @gt_crmd_brelvonae
         FROM crmd_brelvonae
         FOR ALL ENTRIES IN @gt_crms4d_serv_h_i
         WHERE OBJGUID_A_SEL = @gt_crms4d_serv_h_i-header_guid.

*         SELECT a~vbeln,a~fkart,a~ernam,a~erzet,a~erdat,a~belnr,
*                a~bstnk_vf,b~posnr,b~matnr,b~aufnr,b~PS_PSP_PNR
*         INTO TABLE @gt_VBRK
*         FROM VBRK AS a INNER JOIN VBRP AS b
*                              ON (a~vbeln EQ b~vbeln)
**         FOR ALL ENTRIES IN @gt_crmd_brelvonae
*         WHERE OBJGUID_A_SEL = @gt_crms4d_serv_h_i-header_guid.

          SELECT objtype_h,object_id,bill_date,price_date,net_value
            INTO TABLE @gt_CRMS4D_BILLREQ_I
            FROM CRMS4D_BILLREQ_I
            FOR ALL ENTRIES IN @gt_crms4d_serv_h_i
            WHERE objtype_h = @gt_crms4d_serv_h_i-objtype_h
            AND object_id = @gt_crms4d_serv_h_i-object_id.

            LOOP AT gt_crmd_brelvonae INTO gs_crmd_brelvonae.
                 CLEAR: gt_EKKO_EKKN_temp,gt_EBAN_EBKN_temp ,gt_MKPF_MSEG_temp,gt_VBRK_temp,
                 lv_banfn, lv_ebeln,lv_vbeln,lv_mblnr,lv_mjahr,lv_objguid_a_sel,
                 gs_EBAN_EBKN,gs_EKKO_EKKN,gs_VBRK.
                 IF gs_crmd_brelvonae-objtype_b_sel EQ 'BUS2105'. "PR
                    lv_banfn = gs_crmd_brelvonae-objkey_b_sel.

                    SELECT a~banfn, a~bnfpo, a~matnr, a~menge, a~PREIS, b~aufnr, b~prctr, b~PS_PSP_PNR
                      INTO TABLE @gt_EBAN_EBKN_temp
                      FROM EBAN AS a INNER JOIN EBKN AS b
                                           ON ( a~banfn EQ b~banfn )
                      WHERE a~banfn EQ @lv_banfn.

                      LOOP AT gt_EBAN_EBKN_temp INTO gs_EBAN_EBKN.
                          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                          EXPORTING
                            INPUT  = gs_EBAN_EBKN-PS_PSP_PNR
                          IMPORTING
                            OUTPUT = gs_EBAN_EBKN-ac_assignment.

                          gs_EBAN_EBKN-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
                          APPEND gs_EBAN_EBKN TO gt_EBAN_EBKN.

                      ENDLOOP.

                 ELSEIF gs_crmd_brelvonae-objtype_b_sel EQ 'BUS2012'. "PO
                       lv_ebeln = gs_crmd_brelvonae-objkey_b_sel.
                      SELECT b~ebeln, b~bsart, b~lifnr,
                             a~ebelp, a~matnr, a~menge, a~netpr,
                             d~aufnr, d~prctr,a~PS_PSP_PNR
                      INTO TABLE @gt_EKKO_EKKN_temp
                      FROM  ekpo AS a INNER JOIN EKKO AS b
                                           ON ( a~ebeln EQ b~ebeln )
                                     INNER JOIN ekkn AS d
                                           ON ( a~ebeln EQ d~ebeln
                                           AND  a~ebelp EQ d~ebelp )
                      WHERE a~ebeln EQ @lv_ebeln
                        AND b~ebeln EQ @lv_ebeln
                        AND d~ebeln EQ @lv_ebeln.


                      LOOP AT gt_EKKO_EKKN_temp INTO gs_EKKO_EKKN.
                          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                          EXPORTING
                            INPUT  = gs_EKKO_EKKN-PS_PSP_PNR
                          IMPORTING
                            OUTPUT = gs_EKKO_EKKN-ac_assignment.

                          gs_EKKO_EKKN-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
                          APPEND gs_EKKO_EKKN TO gt_EKKO_EKKN.

                      ENDLOOP.

                      SELECT a~mblnr, a~mjahr, a~budat,
                             b~zeile, b~lifnr, b~dmbtr, b~erfmg,
                             b~aufnr, b~matnr, b~ebeln, b~ebelp,
                             b~PS_PSP_PNR
                      INTO TABLE @gt_MKPF_MSEG_temp
                      FROM mkpf AS a INNER JOIN mseg AS b
                                           ON ( a~mblnr EQ b~mblnr
                                           AND a~mjahr EQ b~mjahr )
                      WHERE b~EBELN = @lv_ebeln.
                        LOOP AT gt_MKPF_MSEG_temp INTO gs_MKPF_MSEG.
                              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                                 EXPORTING
                                   INPUT  = gs_MKPF_MSEG-PS_PSP_PNR
                                 IMPORTING
                                   OUTPUT = gs_MKPF_MSEG-ac_assignment.

*                               gs_MKPF_MSEG-objguid_a_sel = lv_objkey_b_sel.
                                 gs_MKPF_MSEG-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
                                 APPEND gs_MKPF_MSEG TO gt_MKPF_MSEG.

                        ENDLOOP.

                 ELSEIF gs_crmd_brelvonae-objtype_b_sel EQ 'VBRK'. "INV
                      lv_vbeln = gs_crmd_brelvonae-objkey_b_sel.
                      SELECT a~vbeln, a~fkart, a~ernam,
                             a~erzet, a~erdat, a~belnr, a~bstnk_vf,
                             b~posnr, b~matnr, b~netwr,b~aubel,b~aufnr, b~PS_PSP_PNR
                      INTO TABLE @gt_VBRK_temp
                      FROM vbrk AS a INNER JOIN vbrp AS b
                                           ON ( a~vbeln EQ b~vbeln )
                      WHERE a~vbeln EQ @lv_vbeln.

                      LOOP AT gt_VBRK_temp INTO gs_VBRK.
                         CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                          EXPORTING
                            INPUT  = gs_VBRK-PS_PSP_PNR
                          IMPORTING
                            OUTPUT = gs_VBRK-ac_assignment.

                          gs_VBRK-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
                          APPEND gs_VBRK TO gt_VBRK.

                      ENDLOOP.

                 ELSEIF gs_crmd_brelvonae-objtype_b_sel EQ 'BUS2017'. "MSEG
                     lv_vbeln = gs_crmd_brelvonae-objkey_b_sel+0(10).
                     lv_mjahr = gs_crmd_brelvonae-objkey_b_sel+10(4).

                     SELECT a~mblnr, a~mjahr, a~budat,
                             b~zeile, b~lifnr, b~dmbtr, b~erfmg,
                             b~aufnr, b~matnr, b~ebeln, b~ebelp,
                             b~PS_PSP_PNR
                      INTO TABLE @gt_MKPF_MSEG_temp
                      FROM mkpf AS a INNER JOIN mseg AS b
                                           ON ( a~mblnr EQ b~mblnr
                                           AND a~mjahr EQ b~mjahr )
                      WHERE a~mblnr EQ @lv_vbeln
                       AND  a~mjahr EQ @lv_mjahr.

                      LOOP AT gt_MKPF_MSEG_temp INTO gs_MKPF_MSEG.
                         CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                          EXPORTING
                            INPUT  = gs_MKPF_MSEG-PS_PSP_PNR
                          IMPORTING
                            OUTPUT = gs_MKPF_MSEG-ac_assignment.


                          gs_MKPF_MSEG-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
                          APPEND gs_MKPF_MSEG TO gt_MKPF_MSEG.

                      ENDLOOP.

                 ELSEIF gs_crmd_brelvonae-objtype_b_sel EQ 'BUS2000117'. "Confirmation
                      CLEAR : lv_objkey_b_sel.
                      lv_objkey_b_sel = gs_crmd_brelvonae-objkey_b_sel.
                      lv_objguid_a_sel = gs_crmd_brelvonae-objkey_b_sel.

                      SELECT GUID,OBJTYPE_H,OBJECT_ID,PROCESS_TYPE
                      INTO TABLE @gt_CONFIRMATION_temp
                      FROM CRMS4D_BTX_H
                      WHERE GUID EQ @lv_objguid_a_sel.
                      LOOP AT gt_CONFIRMATION_temp INTO gs_CONFIRMATION.

                          gs_CONFIRMATION-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
                          APPEND gs_CONFIRMATION TO gt_CONFIRMATION.

                      ENDLOOP.

                      SELECT SINGLE objkey_b_sel
                        INTO lv_objkey_b_sel
                        FROM crmd_brelvonae
                        WHERE objguid_a_sel = lv_objkey_b_sel
                        AND objtype_b_sel EQ 'BUS2017'.
                     IF lv_objkey_b_sel IS NOT INITIAL.
                          lv_vbeln = lv_objkey_b_sel+0(10).
                          lv_mjahr = lv_objkey_b_sel+10(4).

                          SELECT a~mblnr, a~mjahr, a~budat,
                                  b~zeile, b~lifnr, b~dmbtr, b~menge,
                                  b~aufnr, b~matnr, b~ebeln, b~ebelp,
                                  b~PS_PSP_PNR
                           INTO TABLE @gt_MKPF_MSEG_temp
                           FROM mkpf AS a INNER JOIN mseg AS b
                                                ON ( a~mblnr EQ b~mblnr
                                                AND a~mjahr EQ b~mjahr )
                           WHERE a~mblnr EQ @lv_vbeln
                            AND  a~mjahr EQ @lv_mjahr.

                           LOOP AT gt_MKPF_MSEG_temp INTO gs_MKPF_MSEG.
                              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                                 EXPORTING
                                   INPUT  = gs_MKPF_MSEG-PS_PSP_PNR
                                 IMPORTING
                                   OUTPUT = gs_MKPF_MSEG-ac_assignment.

*                               gs_MKPF_MSEG-objguid_a_sel = lv_objkey_b_sel.
                                 gs_MKPF_MSEG-objguid_a_sel = gs_crmd_brelvonae-objguid_a_sel.
                                 APPEND gs_MKPF_MSEG TO gt_MKPF_MSEG.

                           ENDLOOP.
                      ENDIF.

                 ENDIF.

            ENDLOOP.


        SELECT a~domname,a~as4local,a~valpos,a~as4vers,a~domvalue_l,b~ddlanguage,b~ddtext
         INTO TABLE @gt_DD07L
         FROM DD07L AS a INNER JOIN dd07t AS b
                                ON ( a~domname EQ b~domname )
         WHERE a~domname = 'CRMS4_STAT_LIFECYCLE'
          AND  b~ddlanguage EQ 'E'.


      ENDIF.

  SELECT vkbur bezei
  INTO TABLE gt_tvkbt
  FROM tvkbt
  WHERE spras EQ 'E'.

  SELECT vkgrp bezei
  INTO TABLE gt_tvgrt
  FROM tvgrt
  WHERE spras EQ 'E'.

   SELECT PROCESS_TYPE P_DESCRIPTION_20
  INTO TABLE gt_CRMC_PROC_TYPE_T
  FROM CRMC_PROC_TYPE_T
  WHERE LANGU EQ 'E'.

   SELECT ITM_TYPE I_DESCRIPTION_20
  INTO TABLE GT_CRMC_ITEM_TYPE_T
  FROM CRMC_ITEM_TYPE_T
  WHERE LANGU EQ 'E'.



ENDFORM.                    " F_PROCESS_DATA


*&---------------------------------------------------------------------*
*&      Form  F_MAP_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_map_data .

  LOOP AT gt_crms4d_serv_h_i INTO gs_crms4d_serv_h_i.
          CLEAR: gs_output.
          gs_output-OBJTYPE_H = gs_crms4d_serv_h_i-OBJTYPE_H.
          gs_output-OBJECT_ID = gs_crms4d_serv_h_i-OBJECT_ID.
          gs_output-PROCESS_TYPE = gs_crms4d_serv_h_i-PROCESS_TYPE.
          gs_output-PO_NUMBER_SOLD = gs_crms4d_serv_h_i-PO_NUMBER_SOLD.
          gs_output-POSTING_DATE = gs_crms4d_serv_h_i-POSTING_DATE.
          gs_output-DESCRIPTION_H = gs_crms4d_serv_h_i-DESCRIPTION_H.
          gs_output-SALES_OFFICE_SD = gs_crms4d_serv_h_i-SALES_OFFICE_SD.
          gs_output-SALES_GROUP_SD = gs_crms4d_serv_h_i-SALES_GROUP_SD.
          gs_output-NET_VALUE_H = gs_crms4d_serv_h_i-NET_VALUE_H.
          gs_output-NET_VALUE_MAN_H = gs_crms4d_serv_h_i-NET_VALUE_MAN_H.
          gs_output-TAX_AMOUNT_H = gs_crms4d_serv_h_i-TAX_AMOUNT_H.
          gs_output-FREIGHT_H = gs_crms4d_serv_h_i-FREIGHT_H.
          gs_output-NET_WO_FREIGHT_H = gs_crms4d_serv_h_i-NET_WO_FREIGHT_H.
          gs_output-CONTSTART = gs_crms4d_serv_h_i-CONTSTART.
          gs_output-CONTEND = gs_crms4d_serv_h_i-CONTEND.
          gs_output-HEADER_GUID = gs_crms4d_serv_h_i-HEADER_GUID.
          gs_output-DESCR_LANGUAGE = gs_crms4d_serv_h_i-DESCR_LANGUAGE.
          gs_output-CREATED_AT_H = gs_crms4d_serv_h_i-CREATED_AT_H.
          gs_output-CREATED_BY_H = gs_crms4d_serv_h_i-CREATED_BY_H.
          gs_output-CHANGED_AT_H = gs_crms4d_serv_h_i-CHANGED_AT_H.
          gs_output-CHANGED_BY_H = gs_crms4d_serv_h_i-CHANGED_BY_H.
          gs_output-HEAD_CHANGED_AT = gs_crms4d_serv_h_i-HEAD_CHANGED_AT.
          gs_output-HEADER_GUID_CHAR = gs_crms4d_serv_h_i-HEADER_GUID_CHAR.
          gs_output-SOLD_TO_PARTY = gs_crms4d_serv_h_i-SOLD_TO_PARTY.
          gs_output-SHIP_TO_PARTY = gs_crms4d_serv_h_i-SHIP_TO_PARTY.
          gs_output-BILL_TO_PARTY = gs_crms4d_serv_h_i-BILL_TO_PARTY.
          gs_output-PAYER = gs_crms4d_serv_h_i-PAYER.
          gs_output-PERSON_RESP = gs_crms4d_serv_h_i-PERSON_RESP.
          gs_output-CATEGORY = gs_crms4d_serv_h_i-CATEGORY.
          gs_output-PRIORITY = gs_crms4d_serv_h_i-PRIORITY.
          gs_output-AC_OBJECT_TYPE = gs_crms4d_serv_h_i-AC_OBJECT_TYPE.
          gs_output-AC_ASSIGNMENT_H = gs_crms4d_serv_h_i-AC_ASSIGNMENT.
          gs_output-PMNTTRMS = gs_crms4d_serv_h_i-PMNTTRMS.
          gs_output-STAT_LIFECYCLE_H = gs_crms4d_serv_h_i-STAT_LIFECYCLE.
          gs_output-ZZ1_CUS_PO = gs_crms4d_serv_h_i-ZZ1_CUS_PO.
          gs_output-ORDERED_PROD = gs_crms4d_serv_h_i-ORDERED_PROD.
          gs_output-DESCRIPTION_I = gs_crms4d_serv_h_i-DESCRIPTION_I.
          gs_output-ORDER_QTY = gs_crms4d_serv_h_i-ORDER_QTY.
          gs_output-NET_PRICE_I = gs_crms4d_serv_h_i-NET_PRICE_I.
          gs_output-ITM_TYPE = gs_crms4d_serv_h_i-itm_type.
          gs_output-PROD_HIERARCHY = gs_crms4d_serv_h_i-PROD_HIERARCHY.
          gs_output-ZZ1_LGORT = gs_crms4d_serv_h_i-ZZ1_LGORT.
          gs_output-PROCESS_TYPE_I = gs_crms4d_serv_h_i-PROCESS_TYPE_I.
          gs_output-AC_ASSIGNMENT_I = gs_crms4d_serv_h_i-AC_ASSIGNMENT_I.
          gs_output-PRODUCT_ID = gs_crms4d_serv_h_i-PRODUCT_ID.
          gs_output-STAT_LIFECYCLE_I = gs_crms4d_serv_h_i-STAT_LIFECYCLE_I.

          READ TABLE gt_iaom_crm_aufk INTO gs_iaom_crm_aufk WITH KEY OBJECT_ID = gs_crms4d_serv_h_i-OBJECT_ID
                                                                      number_int =  gs_crms4d_serv_h_i-number_int.
              IF sy-subrc EQ 0.
                gs_output-AUFNR   =  gs_iaom_crm_aufk-aufnr.
                gs_output-PROCESS_TYPE_IAOM  =  gs_iaom_crm_aufk-process_type.
                gs_output-ITM_TYPE_IAOM   =  gs_iaom_crm_aufk-itm_type.

              ELSE.
                 READ TABLE gt_iaom_crm_aufk INTO gs_iaom_crm_aufk WITH KEY OBJECT_ID = gs_crms4d_serv_h_i-OBJECT_ID.
                    IF sy-subrc EQ 0.
                         gs_output-AUFNR   =  gs_iaom_crm_aufk-aufnr.
                         gs_output-PROCESS_TYPE_IAOM  =  gs_iaom_crm_aufk-process_type.
                         gs_output-ITM_TYPE_IAOM   =  gs_iaom_crm_aufk-itm_type.
                    ENDIF.
              ENDIF.



           READ TABLE gt_MKPF_MSEG INTO gs_MKPF_MSEG WITH KEY objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID
                                                              AC_ASSIGNMENT = gs_output-AC_ASSIGNMENT_I
                                                              MATNR         = gs_output-ORDERED_PROD.
                IF sy-subrc EQ 0.
                   gs_output-MBLNR = gs_MKPF_MSEG-MBLNR.
                   gs_output-MJAHR = gs_MKPF_MSEG-MJAHR.
                   gs_output-BUDAT = gs_MKPF_MSEG-BUDAT.
                   gs_output-ZEILE = gs_MKPF_MSEG-ZEILE.
                   gs_output-LIFNR = gs_MKPF_MSEG-LIFNR.
                   gs_output-DMBTR = gs_MKPF_MSEG-DMBTR.
                   gs_output-MENGE = gs_MKPF_MSEG-erfmg.
                   gs_output-AUFNR_MSEG = gs_MKPF_MSEG-aufnr.
                   gs_output-MATNR = gs_MKPF_MSEG-MATNR.
                   gs_output-EBELN = gs_MKPF_MSEG-EBELN.
                   gs_output-EBELP = gs_MKPF_MSEG-EBELP.
                ELSE.
                  IF gs_crms4d_serv_h_i-ac_object_type NE '03'.
                        READ TABLE gt_MKPF_MSEG INTO gs_MKPF_MSEG WITH KEY objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID
                                                                            AUFNR = gs_output-AUFNR.


                                IF sy-subrc EQ 0.
                                   gs_output-MBLNR = gs_MKPF_MSEG-MBLNR.
                                   gs_output-MJAHR = gs_MKPF_MSEG-MJAHR.
                                   gs_output-BUDAT = gs_MKPF_MSEG-BUDAT.
                                   gs_output-ZEILE = gs_MKPF_MSEG-ZEILE.
                                   gs_output-LIFNR = gs_MKPF_MSEG-LIFNR.
                                   gs_output-DMBTR = gs_MKPF_MSEG-DMBTR.
                                   gs_output-MENGE = gs_MKPF_MSEG-erfmg.
                                   gs_output-AUFNR_MSEG = gs_MKPF_MSEG-aufnr.
                                   gs_output-MATNR = gs_MKPF_MSEG-MATNR.
                                   gs_output-EBELN = gs_MKPF_MSEG-EBELN.
                                   gs_output-EBELP = gs_MKPF_MSEG-EBELP.
                                 ENDIF.
                    ENDIF.

                ENDIF.
           READ TABLE gt_EBAN_EBKN INTO gs_EBAN_EBKN WITH KEY objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID
                                                              AC_ASSIGNMENT = gs_output-AC_ASSIGNMENT_I
                                                              MATNR         = gs_output-ORDERED_PROD.

                IF sy-subrc EQ 0.
                   gs_output-BANFN = gs_EBAN_EBKN-BANFN.
                   gs_output-BNFPO = gs_EBAN_EBKN-BNFPO.

                ELSE.
                    IF gs_crms4d_serv_h_i-ac_object_type NE '03'.
                        READ TABLE gt_EBAN_EBKN INTO gs_EBAN_EBKN WITH KEY objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID
                                                                           AUFNR = gs_output-AUFNR.
                           IF sy-subrc EQ 0.
                              gs_output-BANFN = gs_EBAN_EBKN-BANFN.
                              gs_output-BNFPO = gs_EBAN_EBKN-BNFPO.
                           ENDIF.
                     ENDIF.
                ENDIF.
            LOOP AT gt_CONFIRMATION INTO gs_CONFIRMATION WHERE objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID.
                    gs_output-BANFN = gs_EBAN_EBKN-BANFN.
                    gs_output-BNFPO = gs_EBAN_EBKN-BNFPO.

            ENDLOOP.

            READ TABLE GT_TVKBT INTO GS_TVKBT WITH KEY vkbur = gs_crms4d_serv_h_i-SALES_OFFICE_SD.
                 IF sy-subrc EQ 0.
                      gs_output-sales_office = GS_TVKBT-bezei.
                 ENDIF.

            READ TABLE GT_TVGRT INTO GS_TVGRT WITH KEY vkgrp = gs_crms4d_serv_h_i-SALES_GROUP_SD.
                 IF sy-subrc EQ 0.
                      gs_output-sales_group = GS_TVGRT-bezei.
                 ENDIF.

            READ TABLE gt_CRMC_PROC_TYPE_T INTO gs_CRMC_PROC_TYPE_T WITH KEY PROCESS_TYPE = gs_crms4d_serv_h_i-PROCESS_TYPE.
                 IF sy-subrc EQ 0.
                      gs_output-PROCESS_TYPE_DESC = gs_CRMC_PROC_TYPE_T-P_DESCRIPTION_20.
                 ENDIF.

            READ TABLE GT_CRMC_ITEM_TYPE_T INTO GS_CRMC_ITEM_TYPE_T WITH KEY ITM_TYPE = gs_crms4d_serv_h_i-itm_type.
                 IF sy-subrc EQ 0.
                      gs_output-itm_type_desc = GS_CRMC_ITEM_TYPE_T-I_DESCRIPTION_20.
                 ENDIF.




*            READ TABLE gt_CONFIRMATION INTO gs_CONFIRMATION WITH KEY objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID.
*                IF sy-subrc EQ 0.
*                   gs_output-BANFN = gs_EBAN_EBKN-BANFN.
*                   gs_output-BNFPO = gs_EBAN_EBKN-BNFPO.
*                ENDIF.




         READ TABLE gt_crms4d_partner INTO gs_crms4d_partner WITH KEY partner_pft = '0001'
                                                                      object_id = gs_crms4d_serv_h_i-OBJECT_ID.
               IF sy-subrc EQ 0.
                  PERFORM get_customer USING  gs_crms4d_partner-partner_id gs_crms4d_partner-addr_nr
                         CHANGING gs_output-sold_to_name_th gs_output-sold_to_name_en
                                  gs_output-sold_to_addr1 gs_output-sold_to_addr2
                                  gs_output-sold_to_district  gs_output-sold_to_city
                                  gs_output-sold_to_postcodey  gs_output-tel .
                  gs_output-sold_to_code = gs_crms4d_partner-partner_id.
               ENDIF.
         READ TABLE gt_crms4d_partner INTO gs_crms4d_partner WITH KEY partner_pft = '0004'
                                                                      object_id = gs_crms4d_serv_h_i-OBJECT_ID.
               IF sy-subrc EQ 0.
                  PERFORM get_customer USING  gs_crms4d_partner-partner_id gs_crms4d_partner-addr_nr
                         CHANGING gs_output-payler_name_th gs_output-payler_name_en
                                  gs_output-payler_addr1 gs_output-payler_addr2
                                  gs_output-payler_district  gs_output-payler_city
                                  gs_output-payler_postcode  gs_output-tel .
                  gs_output-payler_code = gs_crms4d_partner-partner_id.
               ENDIF.
         READ TABLE gt_crms4d_partner INTO gs_crms4d_partner WITH KEY partner_pft = '0002'
                                                                      object_id = gs_crms4d_serv_h_i-OBJECT_ID.
               IF sy-subrc EQ 0.
                  PERFORM get_customer USING  gs_crms4d_partner-partner_id gs_crms4d_partner-addr_nr
                         CHANGING gs_output-ship_to_name_th gs_output-ship_to_name_en
                                  gs_output-ship_to_addr1 gs_output-ship_to_addr2
                                  gs_output-ship_to_district  gs_output-ship_to_city
                                  gs_output-ship_to_postcode  gs_output-tel .
                  gs_output-ship_to_code = gs_crms4d_partner-partner_id.
               ENDIF.
           IF gs_output-PROCESS_TYPE EQ 'ZSC1'.
                IF gt_VBRK IS NOT INITIAL.
                      LOOP AT gt_VBRK INTO gs_VBRK WHERE objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID
                                                     AND AUFNR = gs_output-AUFNR
                                                     AND fkart = 'ZVB3'.

                              gs_output-VBELN = gs_VBRK-vbeln.
                              gs_output-FKART = gs_VBRK-FKART.
                              gs_output-ERNAM = gs_VBRK-ERNAM.
                              gs_output-ERZET = gs_VBRK-ERZET.
                              gs_output-ERDAT = gs_VBRK-ERDAT.
                              gs_output-BELNR = gs_VBRK-BELNR.
                              gs_output-NETWR = gs_VBRK-NETWR.
                              gs_output-bdr_vbeln = gs_VBRK-AUBEL.
                              APPEND gs_output TO gt_output.
                       ENDLOOP.
                  ELSE.
                      APPEND gs_output TO gt_output.
                  ENDIF.
            ELSE.
              IF gs_output-PROCESS_TYPE EQ 'ZRP7'.
                 READ TABLE gt_VBRK INTO gs_VBRK WITH KEY objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID
                                                                    AC_ASSIGNMENT = gs_output-AC_ASSIGNMENT_I
                                                                    MATNR         = gs_output-ORDERED_PROD.
                             IF sy-subrc EQ 0.
                                gs_output-VBELN = gs_VBRK-vbeln.
                                gs_output-FKART = gs_VBRK-FKART.
                                gs_output-ERNAM = gs_VBRK-ERNAM.
                                gs_output-ERZET = gs_VBRK-ERZET.
                                gs_output-ERDAT = gs_VBRK-ERDAT.
                                gs_output-BELNR = gs_VBRK-BELNR.
                                gs_output-NETWR = gs_VBRK-NETWR.
                             ENDIF.
              ELSE.
                 READ TABLE gt_VBRK INTO gs_VBRK WITH KEY objguid_a_sel = gs_crms4d_serv_h_i-HEADER_GUID
                                                          AUFNR = gs_output-AUFNR.
                       IF sy-subrc EQ 0.
                          gs_output-VBELN = gs_VBRK-vbeln.
                          gs_output-FKART = gs_VBRK-FKART.
                          gs_output-ERNAM = gs_VBRK-ERNAM.
                          gs_output-ERZET = gs_VBRK-ERZET.
                          gs_output-ERDAT = gs_VBRK-ERDAT.
                          gs_output-BELNR = gs_VBRK-BELNR.
                          gs_output-NETWR = gs_VBRK-NETWR.



                       ENDIF.

                 ENDIF.
                   APPEND gs_output TO gt_output.
            ENDIF.









  ENDLOOP.


IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.




ENDFORM.                    " F_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_display_report .


  PERFORM BUILD_LAYOUT.
  PERFORM BUILD_CATALOG.
  PERFORM BUILD_EVENT USING GT_EVENTS[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = GC_REPID
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      I_SAVE                  = 'A'
      is_layout               = gt_layout
      IT_EVENTS               = GT_EVENTS[]
      IT_FIELDCAT             = GT_FIELDCAT
    TABLES
      T_OUTTAB                = gt_output
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_DISPLAY_REPORT
* ----------------------------------------------------
* Status
* ----------------------------------------------------
FORM status_set USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZSTANDARD' EXCLUDING rt_extab.

ENDFORM.                    "status_set
*&---------------------------------------------------------------------*
*&      Form  usercommand
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_UCOMM    text
*      -->I_SELFIELD text
*----------------------------------------------------------------------*
FORM usercommand USING i_ucomm i_selfield TYPE slis_selfield.

  DATA: lt_mess_tab TYPE tab_bdcmsgcoll,
        lw_mess_tab TYPE bdcmsgcoll.

  DATA: lv_mode   TYPE c VALUE 'N',
        lv_upd    TYPE c VALUE 'S',
        lv_msgtyp TYPE c.

*
**&---------------------------------------------------------------------*
**&for Check = 'X' when tick Check Box
**&---------------------------------------------------------------------*
*  DATA : ref_grid TYPE REF TO cl_gui_alv_grid.
*
*  IF ref_grid IS INITIAL.
*    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*      IMPORTING
*        e_grid = ref_grid.
*    CALL METHOD ref_grid->check_changed_data.
*  ENDIF.
**&---------------------------------------------------------------------*

*  BREAK wantanee.
*
*
*
*  CASE i_ucomm.
*    WHEN 'UPLOAD'.
*
*      LOOP AT gt_data INTO gs_data.
*        INSERT INTO zsdsfit042
*         VALUES gs_data.
*        COMMIT WORK.
*      ENDLOOP.
*      MESSAGE s000(38) WITH 'Upload Table ZTAP_STATE_BANK Complete'.
*      LEAVE TO SCREEN 0.
*
*    WHEN OTHERS.
*
*  ENDCASE.
*
*  i_selfield-refresh = 'X'.
*  i_selfield-col_stable = 'X'.
*  i_selfield-row_stable = 'X'.

ENDFORM.                    "USERCOMMAND


*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
  GT_LAYOUT-WINDOW_TITLEBAR = SY-TITLE.
  GT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GT_LAYOUT-ZEBRA = 'X'.

ENDFORM.                    "build_layout

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CATALOG .
  CLEAR : V_POS.


  PERFORM APPEND_FIELDCAT USING  'OBJTYPE_H' 'CRMS4D_SERV_H' 'OBJTYPE_H' 'OBJECT TYPE HEADER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].


  PERFORM APPEND_FIELDCAT USING 'OBJECT_ID' 'CRMS4D_SERV_H' 'OBJECT_ID' 'WORK ORDER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'PROCESS_TYPE' 'CRMS4D_SERV_H' 'PROCESS_TYPE' 'PROCESS TYPE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

   PERFORM APPEND_FIELDCAT USING 'PROCESS_TYPE_DESC' 'CRMC_PROC_TYPE_T' 'P_DESCRIPTION_20' 'PROCESS TYPE DESC.'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'PO_NUMBER_SOLD' 'CRMS4D_SERV_H' 'PO_NUMBER_SOLD' 'SF NUMBER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'POSTING_DATE' 'CRMS4D_SERV_H' 'POSTING_DATE' 'DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'DESCRIPTION_H' 'CRMS4D_SERV_H' 'DESCRIPTION_H' 'DESCRIPTION'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SALES_OFFICE_SD' 'CRMS4D_SERV_H' 'SALES_OFFICE_SD' 'SALES OFFICE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SALES_OFFICE' 'TVKBT' 'BEZEI' 'SALES OFFICE DESC.'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'SALES_GROUP_SD' 'CRMS4D_SERV_H' 'SALES_GROUP_SD' 'SALES GROUP'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SALES_GROUP' 'TVGRT' 'BEZEI' 'SALES GROUP DESC'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

  PERFORM APPEND_FIELDCAT USING 'NET_VALUE_H' 'CRMS4D_SERV_H' 'NET_VALUE_H' 'NET VALUE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'NET_VALUE_MAN_H' 'CRMS4D_SERV_H' 'NET_VALUE_MAN_H' 'NET VALUE MAIN'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'TAX_AMOUNT_H' 'CRMS4D_SERV_H' 'TAX_AMOUNT_H' 'TAX AMT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'FREIGHT_H' 'CRMS4D_SERV_H' 'FREIGHT_H' 'SHIPMENT COSTS'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'NET_WO_FREIGHT_H' 'CRMS4D_SERV_H' 'NET_WO_FREIGHT_H' 'NET VALUE WITHOUT FREIGHT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'CONTSTART' 'CRMS4D_SERV_H' 'CONTSTART' 'START DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'CONTEND' 'CRMS4D_SERV_H' 'CONTEND' 'END DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'HEADER_GUID' 'CRMS4D_SERV_H' 'HEADER_GUID' 'SV HEADER ID'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'DESCR_LANGUAGE' 'CRMS4D_SERV_H' 'DESCR_LANGUAGE' 'LANGUAGE KEY OF DESCRIPTION'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'CREATED_AT_H' 'CRMS4D_SERV_H' 'CREATED_AT_H' 'CREATE AT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'CREATED_BY_H' 'CRMS4D_SERV_H' 'CREATED_BY_H' 'CREATE BY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'CHANGED_AT_H' 'CRMS4D_SERV_H' 'CHANGED_AT_H' 'CHANGE AT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'CHANGED_BY_H' 'CRMS4D_SERV_H' 'CHANGED_BY_H' 'CHANGE BY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'HEAD_CHANGED_AT' 'CRMS4D_SERV_H' 'HEAD_CHANGED_AT' 'H CHANGE AT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'HEADER_GUID_CHAR' 'CRMS4D_SERV_H' 'HEADER_GUID_CHAR' 'H SV ID'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SOLD_TO_PARTY' 'CRMS4D_SERV_H' 'SOLD_TO_PARTY' 'SOLD TO CODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_PARTY' 'CRMS4D_SERV_H' 'SHIP_TO_PARTY' 'SHIP TO CODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BILL_TO_PARTY' 'CRMS4D_SERV_H' 'BILL_TO_PARTY' 'BILL TO CODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYER' 'CRMS4D_SERV_H' 'PAYER' 'PAYER CODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PERSON_RESP' 'CRMS4D_SERV_H' 'PERSON_RESP' 'PERSON RESPOSE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'CATEGORY' 'CRMS4D_SERV_H' 'CATEGORY' 'CATEGORY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PRIORITY' 'CRMS4D_SERV_H' 'PRIORITY' 'PRIORITY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'AC_OBJECT_TYPE' 'CRMS4D_SERV_H' 'AC_OBJECT_TYPE' 'AC OBJECT TYPE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'AC_ASSIGNMENT_H' 'CRMS4D_SERV_H' 'AC_ASSIGNMENT_H' 'AC ASSIGNMENT HEADER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PMNTTRMS' 'CRMS4D_SERV_H' 'PMNTTRMS' 'PAYMENT TEAM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'STAT_LIFECYCLE_H' 'CRMS4D_SERV_H' 'STAT_LIFECYCLE_H' 'STATUS HEADER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ZZ1_CUS_PO' 'CRMS4D_SERV_H' 'ZZ1_CUS_PO' 'CUSTOMER PO'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ORDERED_PROD' 'CRMS4D_SERV_I' 'ORDERED_PROD' 'PRODUCT NAME ENTERED'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'DESCRIPTION_I' 'CRMS4D_SERV_I' 'DESCRIPTION_I' 'DESCRIPTION ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ORDER_QTY' 'CRMS4D_SERV_I' 'ORDER_QTY' 'ITEM QTY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'NET_PRICE_I' 'CRMS4D_SERV_I' 'NET_PRICE_I' 'ITEM PRICE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ITM_TYPE' 'CRMS4D_SERV_I' 'ITM_TYPE' 'ITEM TYPE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ITM_TYPE_DESC' 'CRMC_ITEM_TYPE_T' 'I_DESCRIPTION_20' 'TYPE ITEM DESC'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PROD_HIERARCHY' 'CRMS4D_SERV_I' 'PROD_HIERARCHY' 'PH'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ZZ1_LGORT' 'CRMS4D_SERV_I' 'ZZ1_LGORT' 'STORAGE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PROCESS_TYPE_I' 'CRMS4D_SERV_I' 'PROCESS_TYPE_I' 'PROCESS TYPE ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].

   PERFORM APPEND_FIELDCAT USING 'AC_ASSIGNMENT_I' 'CRMS4D_SERV_I' 'AC_ASSIGNMENT_I' 'AC ASSIGNMENT ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PRODUCT_ID' 'CRMS4D_SERV_I' 'PRODUCT_ID' 'PRODUCT ID'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'STAT_LIFECYCLE_I' 'CRMS4D_SERV_I' 'STAT_LIFECYCLE_I' 'STATUS ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'AUFNR' 'IAOM_CRM_AUFK' 'AUFNR' 'IO'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PROCESS_TYPE_IAOM' 'IAOM_CRM_AUFK' 'PROCESS_TYPE_IAOM' 'PROCESS TYPE IAOM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ITM_TYPE_IAOM' 'IAOM_CRM_AUFK' 'ITM_TYPE_IAOM' 'ITEM TYPE IAOM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SOLD_TO_CODE' 'CRMS4D_PARTNER' 'SOLD_TO_CODE' 'SOLD TO CODE H'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SOLD_TO_NAME_TH' '' '' 'Sold to Name th'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
    PERFORM APPEND_FIELDCAT USING 'SOLD_TO_NAME_EN' '' '' 'Sold to Name en'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SOLD_TO_ADDR1' '' '' 'Sold to Addr1'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SOLD_TO_ADDR2' '' '' 'Sold to Addr2'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SOLD_TO_DISTRICT' 'ADRC' 'SOLD_TO_DISTRICT' 'SOLD TO DISTRICT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SOLD_TO_CITY' 'ADRC' 'SOLD_TO_CITY' 'SOLD TO CITY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SOLD_TO_POSTCODEY' 'ADRC' 'SOLD_TO_POSTCODEY' 'SOLD TO POSTCODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYLER_CODE' 'CRMS4D_PARTNER' 'PAYLER_CODE' 'PAYLER CODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYLER_NAME_TH' '' '' 'Payer Name th'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYLER_NAME_EN' '' '' 'Payer Name en'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYLER_ADDR1' '' '' 'Payer addr1'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYLER_ADDR2' '' '' 'Payer addr2'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYLER_DISTRICT' 'ADRC' 'PAYLER_DISTRICT' 'PAYLER DISTRICT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYLER_CITY' 'ADRC' 'PAYLER_CITY' 'PAYLER CITY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'PAYLER_POSTCODE' 'ADRC' 'PAYLER_POSTCODE' 'PAYLER POSTCODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_CODE' 'CRMS4D_PARTNER' 'SHIP_TO_CODE' 'SHIP TO CODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_NAME_TH' '' '' 'Ship to name th'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_NAME_EN' '' '' 'Ship to name en'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_ADDR1' '' '' 'Ship to addr1'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_ADDR2' '' '' 'Ship to addr2'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_DISTRICT' 'ADRC' 'SHIP_TO_DISTRICT' 'SHIP TO DISTRICT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_CITY' 'ADRC' 'SHIP_TO_CITY' 'SHIP TO CITY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'SHIP_TO_POSTCODE' 'ADRC' 'SHIP_TO_POSTCODE' 'SHIP TO POSTCODE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'TEL' 'ADRC' 'TEL' 'TEL'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BDR_VBELN' 'VBRK' 'BDR_VBELN' 'BDR NUMBER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BDR_FKART' 'VBRK' 'BDR_FKART' 'BDR TYPE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BDR_ERNAM' 'VBRK' 'BDR_ERNAM' 'BDR CREATE NAME'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BDR_ERZET' 'VBRK' 'BDR_ERZET' 'BDR CREATE TIME'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BDR_ERDAT' 'VBRK' 'BDR_ERDAT' 'BDR CREATE DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'VBELN' 'VBRK' 'VBELN' 'INV NUMBER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'FKART' 'VBRK' 'FKART' 'INV TYPE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ERNAM' 'VBRK' 'ERNAM' 'INV CREATE NAME'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ERZET' 'VBRK' 'ERZET' 'INV CREATE TIME'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ERDAT' 'VBRK' 'ERDAT' 'INV CREATE DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BELNR' 'VBRK' 'BELNR' 'FI NUMBER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
    PERFORM APPEND_FIELDCAT USING 'NETWR' 'VBRK' 'NETWR' 'INV Amount'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'MBLNR' 'MKPF' 'MBLNR' 'GR/GI NUMBER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'MJAHR' 'MKPF' 'MJAHR' 'GR/GI FY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BUDAT' 'MKPF' 'BUDAT' 'GR/GI POST DATE'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'ZEILE' 'MSEG' 'ZEILE' 'GR/GI TIME'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'LIFNR' 'MSEG' 'LIFNR' 'VENDOR'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'DMBTR' 'MSEG' 'DMBTR' 'GR/GI AMT'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'MENGE' 'MSEG' 'MENGE' 'GR/GI QTY'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'AUFNR_MSEG' 'MSEG' 'AUFNR_MSEG' 'GR/GI IO'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'MATNR' 'MSEG' 'MATNR' 'GR/GI MATERIAL'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'EBELN' 'MSEG' 'EBELN' 'PO NUMBER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'EBELP' 'MSEG' 'EBELP' 'PO ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BANFN' 'EBAN' 'BANFN' 'PR NUMBER'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].
  PERFORM APPEND_FIELDCAT USING 'BNFPO' 'EBAN' 'BNFPO' 'PR ITEM'
                                 SPACE  SPACE  SPACE
                                 GT_FIELDCAT[].









ENDFORM.




*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM BUILD_EVENT  USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LW_EVENT TYPE SLIS_ALV_EVENT.
  FIELD-SYMBOLS <fs_events> LIKE LINE OF E03_LT_EVENTS.
* Read Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 1
    IMPORTING
      ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                           INTO LW_EVENT.
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_list
*                             INTO lw_event.
*  IF SY-SUBRC = 0.
** register top of page event
*    MOVE GC_TOP_OF_PAGE TO LW_EVENT-FORM.
*    APPEND LW_EVENT TO E03_LT_EVENTS.
*  ENDIF.

  LOOP AT E03_LT_EVENTS ASSIGNING <fs_events>
                    WHERE name = 'TOP_OF_PAGE'.
    <fs_events>-form = 'REPORT_HEADER'.
  ENDLOOP.
ENDFORM.                    " BUILD_EVENT
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1453   text
*      -->P_1454   text
*      -->P_1455   text
*      -->P_TEXT_T01  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*

FORM APPEND_FIELDCAT USING   P_FIELD   "Field name
                             P_REFTABLE"Reference Table name
                             P_REFFIELD"Reference Field name
                             P_COLTXT  "Col Text(for specify)
                             P_DOSUM   "Sum total
                             P_CFIELDNAME  "  currency
                             P_NO_ZERO     " no zero
                             P_IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: WA_INFIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        V_COLTXT_LENGTH TYPE I.

  ADD 1 TO V_POS.

  WA_INFIELDCAT-FIELDNAME     = P_FIELD.
  WA_INFIELDCAT-REF_TABNAME   = P_REFTABLE.
  WA_INFIELDCAT-REF_FIELDNAME = P_REFFIELD.
  WA_INFIELDCAT-COL_POS       = V_POS .
  WA_INFIELDCAT-DO_SUM        = P_DOSUM.

  IF NOT P_NO_ZERO IS INITIAL .
    WA_INFIELDCAT-NO_ZERO = P_NO_ZERO.
  ENDIF.
  IF NOT P_CFIELDNAME IS INITIAL .
    WA_INFIELDCAT-CFIELDNAME = P_CFIELDNAME .
  ENDIF.


*If we need to specify text ,don't need to derive from data dictionary
*program will check length and define width of the colum
  IF NOT P_COLTXT IS INITIAL.
    V_COLTXT_LENGTH = STRLEN( P_COLTXT ).

    IF V_COLTXT_LENGTH > 20.
      WA_INFIELDCAT-DDICTXT = 'L'."Long text
      WA_INFIELDCAT-SELTEXT_L = P_COLTXT.
    ELSEIF V_COLTXT_LENGTH > 10.
      WA_INFIELDCAT-DDICTXT = 'M'."Medium Text
      WA_INFIELDCAT-SELTEXT_M = P_COLTXT.
    ELSE.
      WA_INFIELDCAT-DDICTXT = 'S'."Short Text
      WA_INFIELDCAT-SELTEXT_S = P_COLTXT.
    ENDIF.
    WA_INFIELDCAT-REPTEXT_DDIC = P_COLTXT  .
  ENDIF.
  APPEND WA_INFIELDCAT TO P_IT_FIELDCAT.
ENDFORM.                    " APPEND_FIELDCAT

**&---------------------------------------------------------------------*
**&      Form  CUSTOMER
**&---------------------------------------------------------------------*
FORM get_customer USING    p_parnr TYPE  CRMS4D_PARTNER-PARTNER_ID
                           p_adrnr TYPE kna1-adrnr
                  CHANGING value(p_name_tha) TYPE c
                           value(p_name_eng) TYPE c
                           value(p_addr1) TYPE c
                           value(p_addr2) TYPE c
                           value(p_city2) TYPE adrc-city2
                           value(p_city1) TYPE adrc-city1
                           value(p_post_code1) TYPE adrc-post_code1
                           value(p_tel_number) TYPE adrc-tel_number.
*                           value(p_brsch) TYPE kna1-brsch
*                           value(p_brtxt)  TYPE t016t-brtxt
*                           value(p_location) TYPE c.

  DATA: lv_adrnr TYPE kna1-adrnr.
*  DATA: lv_brsch TYPE kna1-brsch,
*        lv_brtxt TYPE t016t-brtxt.


  CLEAR:p_name_tha,p_name_eng,p_addr1,p_addr2,p_city2,
        p_city1,p_post_code1,p_tel_number.


  lv_adrnr =  p_adrnr.

  IF p_adrnr EQ ''.
    SELECT SINGLE adrnr
      INTO (lv_adrnr)
      FROM kna1
    WHERE kunnr EQ p_parnr.

  ENDIF.

  SELECT addrnumber name1 name2 street str_suppl3 location
         city2 city1 post_code1 tel_number nation
  INTO TABLE gt_adrc
  FROM adrc
  WHERE addrnumber = lv_adrnr.
*            AND nation = 'I'.

  IF p_parnr NE 'OT01'.
    READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
                                             nation = 'I'.
    IF sy-subrc EQ 0.
      CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_eng.
    ENDIF.
    READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
                                             nation = ''.
    IF sy-subrc EQ 0.
      CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_tha.
      p_addr1 = gs_adrc-street.
      CONCATENATE gs_adrc-str_suppl3 gs_adrc-location INTO p_addr2.
*      p_addr2 = gs_adrc-str_suppl3.
      p_city2 = gs_adrc-city2.
      p_city1 = gs_adrc-city1.
      p_post_code1 = gs_adrc-post_code1.
      p_tel_number = gs_adrc-tel_number.
*      p_location = gs_adrc-location.
    ENDIF.
  ELSE.

    READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
                                             nation = ''.
    IF sy-subrc EQ 0.
      CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_tha.
      CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_eng.
      p_addr1 = gs_adrc-street.
      CONCATENATE gs_adrc-str_suppl3 gs_adrc-location INTO p_addr2.
*      p_addr2 = gs_adrc-str_suppl3.
      p_city2 = gs_adrc-city2.
      p_city1 = gs_adrc-city1.
      p_post_code1 = gs_adrc-post_code1.
      p_tel_number = gs_adrc-tel_number.
*      p_location = gs_adrc-location.
    ENDIF.
  ENDIF.

*  CLEAR:lv_brsch ,lv_brtxt,p_brsch,p_brtxt.
*
*      SELECT SINGLE brsch
*      INTO lv_brsch
*      FROM kna1
*      WHERE kunnr EQ p_parnr.
*
*      IF NOT lv_brsch IS INITIAL.
*          p_brsch = lv_brsch.
*          SELECT SINGLE brtxt
*          INTO lv_brtxt
*          FROM t016t
*          WHERE brsch EQ lv_brsch
*            AND spras EQ 'E'.
*
*            p_brtxt = lv_brtxt.
*      ENDIF.




ENDFORM. "

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
  lv_text = 'Report Service Order Detail '.
  def_list_head 'H' '' lv_text.
  CLEAR lv_text.
  CONCATENATE 'DATE:' lv_date 'TIME:' lv_time INTO lv_text
    SEPARATED BY space.
  def_list_head 'S' '' lv_text.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_listhead
    EXCEPTIONS
      OTHERS             = 0.

ENDFORM.                    " REPORT_HEADER


*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_DATA_IO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_CONVERT_DATA_IO.
 DATA: LVAC_ASSIGNMENT TYPE CRMS4D_SERV_I-AC_ASSIGNMENT.
 DATA : r_AC_ASSIGNMENT_line LIKE LINE OF GAC_ASSIGNMENT.
 DATA : LV_AUFNR TYPE AUFK-AUFNR.

  break 3sds006.
*  RANGES : GAC_ASSIGNMENT FOR CRMS4D_SERV_I-AC_ASSIGNMENT.
* s_IO FOR crms4d_serv_i-ac_assignment,
  LOOP AT s_IO .
    CLEAR: LV_AUFNR.
    IF s_IO-option EQ 'BT'.

       LV_AUFNR = s_IO-LOW.

       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
           input         = LV_AUFNR
        IMPORTING
          OUTPUT        = LV_AUFNR
                 .
       r_AC_ASSIGNMENT_line-sign = s_IO-SIGN.
       r_AC_ASSIGNMENT_line-option = s_IO-option.
       r_AC_ASSIGNMENT_line-LOW = LV_AUFNR.

       LV_AUFNR = S_IO-HIGH.

       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
           input         = LV_AUFNR
        IMPORTING
          OUTPUT        = LV_AUFNR
                 .
       r_AC_ASSIGNMENT_line-HIGH = LV_AUFNR.


       APPEND r_AC_ASSIGNMENT_line TO GAC_ASSIGNMENT.

    ELSE.

       LV_AUFNR = s_IO-LOW.
       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
           input         = LV_AUFNR
        IMPORTING
          OUTPUT        = LV_AUFNR
                 .

       r_AC_ASSIGNMENT_line-sign = s_IO-SIGN.
       r_AC_ASSIGNMENT_line-option = s_IO-option.
       r_AC_ASSIGNMENT_line-LOW = LV_AUFNR.
       APPEND r_AC_ASSIGNMENT_line TO GAC_ASSIGNMENT.

    ENDIF.



  ENDLOOP.

  LOOP AT S_WBS.
    r_AC_ASSIGNMENT_line = S_WBS.
*    r_AC_ASSIGNMENT_line-sign = 'I'.
*    r_AC_ASSIGNMENT_line-option = 'BT'.
*    r_AC_ASSIGNMENT_line-low = 'AA'.
*    r_AC_ASSIGNMENT_line-high = 'LH'.

    APPEND r_AC_ASSIGNMENT_line TO GAC_ASSIGNMENT.
  ENDLOOP.


ENDFORM.                    " F_CHECK_ERROR


*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_DATA_IO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_CONVERT_DATA_JOB_MA.
 DATA: LVHEADER_GUID TYPE CRMS4D_SERV_H-HEADER_GUID.
 DATA : r_OBJECT_ID_line LIKE LINE OF GAC_OBJECT_ID.


* DATA
* DATA:


       SELECT OBJECT_ID, HEADER_GUID
       INTO TABLE @GT_MA_OBJECT
       FROM CRMS4D_SERV_H
       WHERE OBJECT_ID IN @S_MA_NO.

       IF GT_MA_OBJECT IS NOT INITIAL.
          SELECT B~OBJECT_ID , A~OBJGUID_A_SEL, A~OBJGUID_B_SEL, A~OBJTYPE_B_SEL
            INTO TABLE @GT_MA_REFER
            FROM CRMD_BRELVONAE AS A INNER JOIN CRMS4D_SERV_H AS B
                                     ON ( A~OBJGUID_B_SEL = B~HEADER_GUID )
            FOR ALL ENTRIES IN @GT_MA_OBJECT
            WHERE A~OBJGUID_A_SEL = @GT_MA_OBJECT-HEADER_GUID
            AND A~OBJTYPE_B_SEL EQ 'BUS2000116'.

*            LOOP AT GT_MA_REFER INTO GS_MA_REFER.
*                 r_OBJECT_ID_line-sign = s_OBJ_ID-SIGN.
*                 r_OBJECT_ID_line-option = s_OBJ_ID-option.
*                 r_OBJECT_ID_line-LOW = GS_MA_REFER-OBJECT_ID.
*                 APPEND r_OBJECT_ID_line TO s_OBJ_ID.
*            ENDLOOP.
       ENDIF.



ENDFORM.                    " F_CHECK_ERROR
