*&---------------------------------------------------------------------*
*& Report ZSDSFIR0200
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZFIARI020
*  Description        : Report and Export AR Invoice information (EPP)
*  Purpose            :
*  Copied from        :  ZR_SD_EXPORT_AUTO_EPP
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0200.

*---------DATA DEFINATION-------------------------*
*&-----------------------------------------------------------------------------------*
*& T A B L E S
*&-----------------------------------------------------------------------------------*
TABLES: kna1,bsid.


*&-----------------------------------------------------------------------------------*
*& D A T A
*&-----------------------------------------------------------------------------------*
TYPES: BEGIN OF ty_header_bbl,
     RECORDHEADER       TYPE  C, "Record Type ส่วนHeader ระบุค่าเริ่มต้นเป็น H
     DATESEND(12)       TYPE   C,
     CORPORATENAME(30)  TYPE   C,
     CORPORATECODE(6)   TYPE   C,
     TOBANK(3)          TYPE   C,
     FILLER(397)        TYPE   C,
     ENDF               TYPE   C,
END OF ty_header_bbl.
TYPES: BEGIN OF ty_detail_bbl,
          RECORDTYPE              TYPE   C,
          BILLER_CODE(6)          TYPE   C,
          PAYEE_CODE(20)          TYPE   C,
          PAYER_CODE(20)          TYPE   C,
          DOCUMENTTYPE(5)         TYPE   C,
          DOCUMENTNO(25)          TYPE   C,
          DOCUMENTDATE(8)         TYPE   C,
          DOCUMENTDUEDATE(8)      TYPE   C,
          DOCUMENTAMOUNT(15)      TYPE   C,
          OUTSTANDINGAMOUNT(15)   TYPE   C,
          DOCUMENTBASEAMOUNT(15)  TYPE   C,
          BASESTATUSFLAG          TYPE   C,
          WHTRATE(5)              TYPE   C,
          WHT1(10)                TYPE   C,
          WHT2(10)                TYPE   C,
          VATSTATUSFLAG           TYPE   C,
          VATRATE(5)              TYPE   C,
          DISCOUNTTYPE            TYPE   C,
          DISCOUNTVALUE(15)       TYPE   C,
          DISCOUNTEXPIREDDATE(8)  TYPE   C,
          ALLOCATIONNUMBER(25)    TYPE   C,
          PAYMENTTERMS(10)        TYPE   C,
          RECODEMODE              TYPE   C,
          USERDATA1(20)           TYPE   C,
          USERDATA2(20)           TYPE   C,
          USERDATA3(20)           TYPE   C,
          USERDATA4(50)           TYPE   C,
          USERDATA5(50)           TYPE   C,
          USERDATA6(50)           TYPE   C,
          FILLER(10)              TYPE   C,

END OF ty_detail_bbl.
TYPES: BEGIN OF ty_footer_bbl,
          RECORDTYPE                    TYPE   C,
          TOTALDOCUMENTS(6)             TYPE   C,
          TOTALOUTSTANDINGAMOUNT(15)    TYPE   C,
          OUTSTANDINGAMOUNTFLAG         TYPE   C,
          FILLER(427)                   TYPE   C,

END OF ty_footer_bbl.

TYPES: BEGIN OF ty_header_scb,
        RECORDHEADER(1)       TYPE   C,
        DATESEND(12)          TYPE   C,
        CORPORATENAME(30)     TYPE   C,
        CORPORATECODE(5)      TYPE   C,
        TOBANK(10)            TYPE   C,
        DOWNLOADFROMDATE(8)   TYPE   C,
        DOWNLOADTODATE(8)     TYPE   C,
        FILLER(185)           TYPE   C,
        ENDF                  TYPE   C,

END OF ty_header_scb.
TYPES: BEGIN OF ty_detail_scb,
          RECORDTYPE(1)             TYPE   C,
          CORPORATECODE(5)          TYPE   C,
          CUSTOMERCODE(10)          TYPE   C,
          DOCUMENTTYPE(2)           TYPE   C,
          INVOICENUMBER(12)         TYPE   C,
          INVOICEDATE(8)            TYPE   C,
          DUEDATE(8)                TYPE   C,
          INVOICEAMOUNT(15)         TYPE   C,
          OUTSTANDINGAMOUNT(15)     TYPE   C,
          ALLOCATIONNUMBER(20)      TYPE   C,
          REFERENCE(40)             TYPE   C,
          INVOICESTATUSFLAG(1)      TYPE   C,
          BASEAMOUNT(15)            TYPE   C,
          VATRATE(5)                TYPE   C,
          BANKCODE(3)               TYPE   C,
          PAYMENTTERMS(10)          TYPE   C,
          SUBCOMPANYCODE(4)         TYPE   C,
          RECORDMODE(1)             TYPE   C,
          PAYMENTAMOUNT(15)         TYPE   C,
          REASONCODE(5)             TYPE   C,
          BANKPAYMENTREFERENCE(20)  TYPE   C,
          PAYMENTTRANSACTIONDATE(8) TYPE   C,
          PAYMENTTRANSACTIONTIME(6) TYPE   C,
          DEBITAC(10)               TYPE   C,
          DISCOUNTAMOUNT(15)        TYPE   C,
          FILLER(6)                 TYPE   C,


END OF ty_detail_scb.
TYPES: BEGIN OF ty_footer_scb,
          RECORDTYPE(1)               TYPE   C,
          DATESEND(12)                TYPE   C,
          CORPORATENAME(30)           TYPE   C,
          CORPORATECODE(5)            TYPE   C,
          TOBANK(10)                  TYPE   C,
          TOTALINVOICENUMBER(6)       TYPE   C,
          TOTALOUTSTANDINGINVOICEAMOUNT(15)    TYPE   C,
          FILLER(181)                 TYPE   C,


END OF ty_footer_scb.
TYPES: BEGIN OF gy_itab,
       kunnr          TYPE kna1-kunnr,
       name(100)      TYPE c,
       blart          TYPE bsid-blart,
       belnr          TYPE bsid-belnr,
       budat          TYPE bsid-budat,
       prctr          TYPE vbap-prctr,
       dmbtr          TYPE bsid-dmbtr,
       dmbtr_s        TYPE bsid-dmbtr,
       shkzg          TYPE bsid-shkzg,
       mwsbp          TYPE vbrp-mwsbp,
       mwsbp_s        TYPE vbrp-mwsbp,
       base_amt       TYPE bsid-dmbtr,
       base_amt_s     TYPE bsid-dmbtr,
       vat_rate       TYPE i,
       vat_rate_s     TYPE i,
       vat_type(3)    TYPE c,
       bank_code      TYPE ZSDSFIC022-bank_code,
       b_name_s       TYPE ZSDSFIC022-b_name_s,
       zterm          TYPE vbkd-zterm,
       due_date       TYPE bsid-budat,  "due date add by Wantanee 20150827
       doc_type(5)    TYPE c,
       doc_type_c(20) TyPE c,


 END OF gy_itab.

TYPES: BEGIN OF gy_itab_cust,
       kunnr              TYPE kna1-kunnr,
       cust_name_th(100)  TYPE c,
       cust_name_en(100)  TYPE c,
       bank_code          TYPE ZSDSFIC022-bank_code,
       b_name_s           TYPE ZSDSFIC022-b_name_s,

END OF gy_itab_cust.
TYPES: BEGIN OF gy_itab_scb,
       cust_no(10)        TYPE c,
       doc_type(2)        TYPE c,
       doc_no(12)         TYPE c,
       doc_date(8)        TYPE c,
       due_date(8)        TYPE c,
       doc_amt(15)        TYPE c,
       doc_base_amt(15)   TYPE c,
       vat_rate(5)        TYPE c,
       allocation_num(20) TYPE c,
       reference2(40)     TYPE c,
       inv_flag(1)        TYPE c,  "Invoice flag 0 no discount 1 cal discount  "Add by Wantanee 20160215
       payment_term(10)   TYPE c,
       sub_com(4)         TYPE c,
       dmbtr              TYPE bsid-dmbtr,
       shkzg              TYPE bsid-shkzg,
       record_type        TYPE c,

 END OF gy_itab_scb.

 TYPES: BEGIN OF gy_itab_bbl,
       cust_no(20)        TYPE c,
       doc_type(5)        TYPE c,
       doc_no(25)         TYPE c,
       doc_date(8)        TYPE c,
       due_date(8)        TYPE c,
       doc_amt(15)        TYPE c,
       doc_base_amt(15)   TYPE c,  "Add by Wantanee 20160224
       payee_code(20)     TYPE c,
       allocation_no(25)  TYPE c,
       dis_expired(8)     TYPE c,
       payment_term(10)   TYPE c,
       user_data1(20)     TYPE c,
        user_data2(20)     TYPE c,
*       user_data3(20)     TYPE c,
*       user_data4(50)     TYPE c,
*       user_data5(50)     TYPE c,
*       user_data6(50)     TYPE c,
       dmbtr              TYPE bsid-dmbtr,
       shkzg              TYPE bsid-shkzg,
       record_type        TYPE c,
       zterm       TYPE bsid-zterm,

 END OF gy_itab_bbl.

 TYPES: BEGIN OF gy_itab_kbank,
       cust_no(20)    TYPE c,
       doc_type(3)    TYPE c,
       doc_no(20)     TYPE c,
       doc_date(8)    TYPE c,
       due_date(8)        TYPE c,
       doc_amt(15)    TYPE c,
       base_amt(15)   TYPE c,
       doc_flag    TYPE c,
       matching_refer(20)  TYPE c,
       profit(7)   TYPE c,
       dmbtr              TYPE bsid-dmbtr,
       shkzg              TYPE bsid-shkzg,
       record_type        TYPE c,
       inv_flag(1)        TYPE c,  "Invoice flag 0 no discount 1 cal discount  "Add by Wantanee 20170711
       payment_term(5)    TYPE c,  " Payment term flag

 END OF gy_itab_kbank.

TYPES: BEGIN OF gy_bsid,
       belnr    TYPE bsid-belnr,
       kunnr    TYPE bsid-kunnr,
       budat    TYPE bsid-budat,
       zterm    TYPE bsid-zterm,
       gjahr    TYPE bsid-gjahr,
       zfbdt   type bsid-zfbdt,    "Baseline Payment Dte
       zbd1t   type bsid-zbd1t,    "Terms of Payment

       blart    TYPE bkpf-blart,
       dmbtr    TYPE bsid-dmbtr,
       adrnr    TYPE kna1-adrnr,



END OF gy_bsid.

TYPES: BEGIN OF gy_bsad,
       belnr    TYPE bsad-belnr,
       kunnr    TYPE bsad-kunnr,
       budat    TYPE bsad-budat,
       zterm    TYPE bsad-zterm,
       gjahr    TYPE bsad-gjahr,
       zfbdt   type bsad-zfbdt,    "Baseline Payment Dte
       zbd1t   type bsad-zbd1t,    "Terms of Payment

       blart    TYPE bkpf-blart,
       dmbtr    TYPE bsad-dmbtr,
       adrnr    TYPE kna1-adrnr,
END OF gy_bsad.


TYPES: BEGIN OF gy_vbak,
       vbeln    TYPE vbak-vbeln,
       auart    TYPE vbak-auart,
       erdat    TYPE vbak-erdat,
       kunnr    TYPE vbak-kunnr,
       posnr    TYPE vbap-posnr,
       netwr    TYPE vbap-netwr,
       mwsbp    TYPE vbap-mwsbp,
       prctr    TYPE vbap-prctr,
       zterm    TYPE vbkd-zterm,
       adrnr    TYPE vbpa-adrnr,

END OF gy_vbak.
TYPES: BEGIN OF gy_vbak_vbeln,
       vbeln    TYPE vbak-vbeln,
       kunnr    TYPE vbak-kunnr,
       auart    TYPE vbak-auart,
       erdat    TYPE vbak-erdat,
       knumv    TYPE vbak-knumv,
       adrnr    TYPE vbpa-adrnr,
       zterm    TYPE vbkd-zterm,


END OF gy_vbak_vbeln.


TYPES: BEGIN OF gy_ZSDSFIC022,
       kunnr      TYPE ZSDSFIC022-kunnr,
       bank_code  TYPE ZSDSFIC022-bank_code,
       b_name_s   TYPE ZSDSFIC022-b_name_s,
       b_name_l   TYPE ZSDSFIC022-b_name_l ,

END OF gy_ZSDSFIC022.


TYPES: BEGIN OF gy_ZSDSFIC023,
       blart          TYPE ZSDSFIC023-blart,
       shkzg          TYPE ZSDSFIC023-shkzg,
       EPP_BANK_DOCTYPE  TYPE ZSDSFIC023-EPP_BANK_DOCTYPE,
END OF gy_ZSDSFIC023.



TYPES: BEGIN OF gy_adrc,
       addrnumber     TYPE adrc-addrnumber,      "Address Number
       name1          TYPE adrc-name1,      "Name
       name2          TYPE adrc-name2,      "Name2
       street         TYPE adrc-street,     "Street
       str_suppl3     TYPE adrc-str_suppl3, "
       location       TYPE adrc-location,
       city2          TYPE adrc-city2,      "District
       city1          TYPE adrc-city1,      "City
       post_code1     TYPE adrc-post_code1, "post code
       tel_number     TYPE adrc-tel_number, "telephone
       fax_number     TYPE adrc-fax_number, "fax_number
       nation         TYPE adrc-nation,      "Name2
END OF gy_adrc.

TYPES: BEGIN OF gy_bsis,
       belnr      TYPE bsis-belnr,
       gjahr      TYPE bsis-gjahr,
       mwskz      TYPE bsis-mwskz,
       shkzg      TYPE bsis-shkzg,
       dmbtr      TYPE bsis-dmbtr,
       prctr      TYPE bsis-prctr,
END OF gy_bsis.
TYPES: BEGIN OF gy_prctr,
       belnr      TYPE bsis-belnr,
       gjahr      TYPE bsis-gjahr,
       prctr      TYPE bsis-prctr,
END OF gy_prctr.


TYPES: BEGIN OF gy_tax_rate,
       mwskz      TYPE a003-mwskz,
       kbetr      TYPE konp-kbetr,

END OF gy_tax_rate.


TYPES: BEGIN OF gy_hrp1001,
       objid  TYPE hrp1001-objid,
       sobid  TYPE hrp1001-sobid,
END OF gy_hrp1001.

TYPES: BEGIN OF gy_so,
       vbeln TYPE vbak-vbeln,
       kvgr2 TYPE vbak-kvgr2,
       augru TYPE vbak-augru,
       mahdt TYPE vbak-mahdt,
       vkbur TYPE vbak-vkbur,  "Sale office
END OF gy_so.

TYPES: BEGIN OF gy_ins_update_cust_bank,
       kunnr          TYPE ZSDSFIC022-kunnr,
       bank_key(20)   TYPE c,
       bank_code      TYPE ZSDSFIC022-bank_code,
       b_name_s       TYPE ZSDSFIC022-b_name_s,
       cust_name(200) TYPE c,
       mark           TYPE c,
       up_flag        TYPE c,
END OF gy_ins_update_cust_bank.
TYPES: BEGIN OF gy_ins_update_cust_bank1,
       mandt          TYPE ZSDSFIC022-mandt,
       kunnr          TYPE ZSDSFIC022-kunnr,
       bank_code      TYPE ZSDSFIC022-bank_code,
       b_name_s       TYPE ZSDSFIC022-b_name_s,
       b_name_l       TYPE ZSDSFIC022-b_name_l,
END OF gy_ins_update_cust_bank1.
TYPES: BEGIN OF gy_vrm_value,
         key(40) TYPE c,
         text(80) TYPE c,
END OF gy_vrm_value.


types: gy_vrm_values type gy_vrm_value occurs 0,
       vrm_id type gy_vrm_value-text,
       vrm_ids type vrm_id occurs 0.
*&-----------------------------------------------------------------------------------*
*& I N T E R N A L   T A B L E S
*&-----------------------------------------------------------------------------------*
* Data for ALV display

TYPE-POOLS: slis.
TYPE-POOLS: truxs.
* The inputs that need to be passed to the REUSE_ALV function module
DATA: gt_gs_list_fieldcat  TYPE lvc_t_fcat,      "Field Catalog for List Viewer Control
      gt_exit(1)        TYPE c,
      gt_variant        TYPE disvariant,
      gx_variant        TYPE disvariant,
      gs_print           TYPE lvc_s_prnt.

DATA : BEGIN OF tab_err occurs 0.
       INCLUDE STRUCTURE  bapiret2.
DATA : END   OF tab_err.

DATA: gt_itab             TYPE STANDARD TABLE OF gy_itab,
      gt_itab_scb         TYPE STANDARD TABLE OF gy_itab_scb,
      gt_itab_bbl         TYPE STANDARD TABLE OF gy_itab_bbl,
      gt_itab_kbank       TYPE STANDARD TABLE OF gy_itab_kbank,
      gt_itab_cust        TYPE STANDARD TABLE OF gy_itab_cust,  "Add by Wantanee 20150929
      gt_bsid             TYPE STANDARD TABLE OF gy_bsid,
      gt_bsid_temp        TYPE STANDARD TABLE OF gy_bsid,
      gt_bsid_cn          TYPE STANDARD TABLE OF gy_bsid,
      gt_bsis             TYPE STANDARD TABLE OF gy_bsis,
      gt_bsis_del         TYPE STANDARD TABLE OF gy_bsis,
      gt_prctr            TYPE STANDARD TABLE OF  gy_prctr,
      gt_prctr_del        TYPE STANDARD TABLE OF  gy_prctr,
      gt_tax_rate         TYPE STANDARD TABLE OF gy_tax_rate,
      gt_ZSDSFIC022  TYPE STANDARD TABLE OF gy_ZSDSFIC022,
      gt_ZSDSFIC023    TYPE STANDARD TABLE OF gy_ZSDSFIC023,
      gt_vbak             TYPE STANDARD TABLE OF gy_vbak,
      gt_vbak_temp        TYPE STANDARD TABLE OF gy_vbak,
      gt_vbak_vbeln       TYPE STANDARD TABLE OF gy_vbak_vbeln,
      gt_adrc             TYPE STANDARD TABLE OF gy_adrc,
      gt_so               TYPE STANDARD TABLE OF gy_so,
      gt_so_del           TYPE STANDARD TABLE OF gy_so,
      gt_inst_up_cust_bank TYPE STANDARD TABLE OF gy_ins_update_cust_bank,
      gt_instup_cust_bank TYPE STANDARD TABLE OF gy_ins_update_cust_bank1,
      gt_upsert           TYPE STANDARD TABLE OF  gy_ins_update_cust_bank1,
      gt_del              TYPE STANDARD TABLE OF  gy_ins_update_cust_bank1,
      gt_bsad             TYPE STANDARD TABLE OF gy_bsad.

DATA: gs_itab             TYPE gy_itab,
      gs_itab_scb         TYPE gy_itab_scb,
      gs_itab_bbl         TYPE gy_itab_bbl,
      gs_itab_kbank       TYPE gy_itab_kbank,
      gs_itab_cust        TYPE gy_itab_cust,  "Add by Wantanee 20150929
      gs_bsid             TYPE gy_bsid,
      gs_bsid_cn          TYPE gy_bsid,
      gs_so               TYPE gy_so,
      gs_inst_up_cust_bank TYPE gy_ins_update_cust_bank.

DATA: wa_itab             TYPE gy_itab,
      wa_itab_scb         TYPE gy_itab_scb,
      wa_itab_bbl         TYPE gy_itab_bbl,
      wa_itab_kbank       TYPE gy_itab_kbank,
      wa_itab_cust        TYPE gy_itab_cust,  "Add by Wantanee 20150929
      wa_bsid             TYPE gy_bsid,
      wa_bsid_cn          TYPE gy_bsid,
      gs_bsis             TYPE gy_bsis,
      gs_bsis_del         TYPE gy_bsis,
      gs_prctr            TYPE gy_prctr,
      gs_prctr_del        TYPE gy_prctr,
      gs_tax_rate         TYPE gy_tax_rate,
      gs_ZSDSFIC022  TYPE gy_ZSDSFIC022,
      gs_ZSDSFIC023    TYPE gy_ZSDSFIC023,
      gs_vbak             TYPE gy_vbak,
      gs_vbak_temp        TYPE gy_vbak,
      gs_vbak_vbeln       TYPE gy_vbak_vbeln,
      gs_adrc             TYPE gy_adrc,
      gs_so1               TYPE gy_so,
      gs_so1_del           TYPE gy_so,
      gs_so1_temp          TYPE gy_so,
      gs_inst_up_cust_bank1 TYPE gy_ins_update_cust_bank,
      gs_upsert            TYPE  gy_ins_update_cust_bank1,
      gs_del               TYPE  gy_ins_update_cust_bank1,
      gs_bsad              TYPE gy_bsad.

DATA: gs_so_temp               TYPE gy_so.

DATA: gs_grid_main        TYPE REF TO cl_gui_alv_grid,
     gs_container_main   TYPE REF TO cl_gui_custom_container,
     gs_variant       LIKE disvariant.

DATA: gt_txt_scb   TYPE truxs_t_text_data,
      gt_txt_bbl   TYPE truxs_t_text_data,
      gt_txt_kbank TYPE truxs_t_text_data.


DATA: gt_header_bbl TYPE STANDARD TABLE OF ty_header_bbl,
      gs_header_bbl TYPE ty_header_bbl,
      gt_detail_bbl TYPE STANDARD TABLE OF ty_detail_bbl,
      gs_detail_bbl TYPE ty_detail_bbl,
      gt_footer_bbl TYPE STANDARD TABLE OF ty_footer_bbl,
      gs_footer_bbl TYPE ty_footer_bbl.


DATA: gt_header_scb TYPE STANDARD TABLE OF ty_header_scb,
      gs_header_scb TYPE ty_header_scb,
      gt_detail_scb TYPE STANDARD TABLE OF ty_detail_scb,
      gs_detail_scb TYPE ty_detail_scb,
      gt_footer_scb TYPE STANDARD TABLE OF ty_footer_scb,
      gs_footer_scb TYPE ty_footer_scb.

  DATA: gs_txt_scb(450)    TYPE c,
        gs_text_scb(260)    TYPE c,
        gs_text_kbank(300)  TYPE c.

  DATA: gs_amt(15) TYPE n,
        gs_vat_rate(5) TYPE n,
        gs_amt_temp TYPE p.

  DATA: gs_amt_c(15),
        gs_vat_rate1(5),
        gs_count_doc(6).

  DATA: gs_total_amt TYPE bsid-dmbtr,
        gs_total_count TYPE i.
  DATA: gs_date(8) TYPE c.
DATA: gs_path_name TYPE string.
DATA: gs_path_name_bbl1 TYPE string.

DATA: CT_RESULT TYPE TABLE OF STRING.
DATA: CS_STRING TYPE STRING.


 DATA: it_lines TYPE STANDARD TABLE OF tline.
  DATA: gs_lines LIKE LINE OF it_lines.
  DATA: v_name TYPE thead-tdname.

DATA: gs_list TYPE gy_vrm_values,
      value LIKE LINE OF  gs_list.
DATA: gs_lstbox_name TYPE vrm_id." value 'ZSDCE-CBO_OH1'.
DATA: fill type i .
DATA: gs_linecount type i.
data: gs_tc_100_lines  like sy-loopc.
DATA: gs_add TYPE c.

DATA: GV_PATH(100) TYPE C.
DATA: GV_LEN TYPE i.

data  linno type i.                    "line number at cursor position
data  fld(20).                         "field name at cursor position
data  off type i.
ranges: r_blart         for bsid-blart.  "order type
controls: tc_100 type tableview using screen 0100.

DATA : V_LINES TYPE i.
*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
  DATA : v_pos TYPE i .

  DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
         gt_layout   TYPE slis_layout_alv,
         gt_events   TYPE slis_t_event.
*        gt_heading  TYPE slis_t_gs_listheader.

  DATA: mestyp1   like  edidc-mestyp.
*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N  ( %_ )            *
*&---------------------------------------------------------------------*
define %show. " show input field
  if screen-group1 = &1.
    screen-invisible = 0.
    screen-active = 1.
  endif.
end-of-definition.
define %hide. " hide input field
  if screen-group1 = &1.
    screen-invisible = 1.
    screen-active = 0.
  endif.
end-of-definition.
*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
  CONSTANTS :  gc_repid       TYPE repid         VALUE 'ZSDSFIR0200',
               gc_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
*  CONSTANTS : mestyp1   like  edidc-mestyp value 'ZEPPM001'.
*&-----------------------------------------------------------------------------------*
*& S E L E C T I O N   S C R E E N
*&-----------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-h01.

PARAMETERS:       p_bukrs       TYPE bukrs   OBLIGATORY DEFAULT '1000'.

  SELECT-OPTIONS: s_kunnr     FOR kna1-kunnr,     "Customer Code
                  s_belnr     FOR bsid-belnr.

SELECTION-SCREEN END OF BLOCK b1.

*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-h03.
*PARAMETERS : r_scb      RADIOBUTTON GROUP typb DEFAULT 'X' ,
*             r_bbl      RADIOBUTTON GROUP typb  ,
*             r_kbank    RADIOBUTTON GROUP typb.
*
*SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-h02.
PARAMETERS : r_cash     RADIOBUTTON GROUP typs DEFAULT 'X' USER-COMMAND exec,
             r_credit   RADIOBUTTON GROUP typs  ,
             r_add      RADIOBUTTON GROUP typs.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-h04 .
   PARAMETERS:  p_disp radiobutton group gr1 MODIF ID SC1,                   " Download to WEB Server
                p_cust RADIOBUTTON GROUP gr1 MODIF ID SC1,
                p_local radiobutton group gr1 MODIF ID SC1,
                p_idoc radiobutton group gr1 default 'X' MODIF ID SC1,
                p_path TYPE string   LOWER CASE MODIF ID SC1.
*                p_path like rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b4.

*END SELECTION SCREEN
*&-----------------------------------------------------------------------------------*
* Event:Initialization
INITIALIZATION.
  p_path = '/usr/sap/tmp/'.
AT SELECTION-SCREEN OUTPUT.
* Set Screen
  PERFORM f_modify_screen.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*  PERFORM get_path_name CHANGING p_path.
  PERFORM pf_directory_browse USING p_path.
*&-----------------------------------------------------------------------------------*
*& * START-OF-SELECTION
*&-----------------------------------------------------------------------------------*
START-OF-SELECTION .


IF NOT r_add IS INITIAL.
    PERFORM f_get_data_customer.
    PERFORM f_map_data_customer.
    IF V_LINES NE 0.
      fill = V_LINES.
    ELSE.
      fill = 22.  "Set line for screen
    ENDIF.


    call screen 0100.
ELSE.
*     Get data
    IF NOT p_cust IS INITIAL.
       PERFORM f_get_data_customer.
       PERFORM f_map_data_customer.

    ELSE.
      PERFORM f_get_data.
      PERFORM f_get_data_del.
      PERFORM f_map_data.
      PERFORM f_map_data_del.
    ENDIF.
ENDIF.
*  IF NOT gt_itab_bbl IS INITIAL.
*
*  ENDIF.
*  IF NOT gt_itab_scb IS INITIAL.
*
*      PERFORM f_text_scb.
*  ENDIF.
*  IF NOT gt_itab_kbank IS INITIAL.
*
*  ENDIF.

*&-----------------------------------------------------------------------------------*
*& * END-OF-SELECTION
*&-----------------------------------------------------------------------------------*
END-OF-SELECTION.
  IF ( NOT gt_itab[] IS INITIAL ) OR ( NOT gt_itab_cust[] IS INITIAL ).
    IF NOT p_disp IS INITIAL.
       PERFORM display_reprot.
    ELSEIF NOT p_cust IS INITIAL.
       PERFORM display_reprot_cust.
    ELSEIF NOT p_local IS INITIAL.

            IF NOT gt_itab_bbl IS INITIAL.
             CLEAR: gs_path_name,gs_path_name_bbl1.

             CONCATENATE 'BBL_' sy-datum '_' sy-timlo '.txt' INTO gs_path_name.

             PERFORM f_text_bbl.
             CONCATENATE p_path gs_path_name INTO gs_path_name SEPARATED BY '\'.
             PERFORM export USING gs_path_name
                                  gt_txt_bbl.
             READ TABLE gt_itab_bbl INTO gs_itab_bbl WITH KEY zterm = 'A030'.
               IF sy-subrc EQ 0.
                 CONCATENATE 'BBL1_' sy-datum '_' sy-timlo '.txt' INTO gs_path_name_bbl1.
                 PERFORM f_text_bbl_a30.
                 CONCATENATE p_path gs_path_name_bbl1 INTO gs_path_name_bbl1 SEPARATED BY '\'.
                 PERFORM export USING gs_path_name_bbl1
                                      gt_txt_bbl.
               ENDIF.



            ENDIF.
            IF NOT gt_itab_scb IS INITIAL.
                 CLEAR: gs_path_name.
                " CONCATENATE 'SCB_' sy-datum '_' sy-timlo '.txt' INTO gs_path_name.
*                  CONCATENATE 'SCB.txt' INTO gs_path_name.
                 gs_path_name = 'SCB.txt'. "T41K920436

                 PERFORM f_text_scb.
                 CONCATENATE p_path gs_path_name INTO gs_path_name SEPARATED BY '\'.
                 PERFORM export USING gs_path_name
                                      gt_txt_scb.
            ENDIF.
            IF NOT gt_itab_kbank IS INITIAL.
                 CLEAR: gs_path_name.
                 CONCATENATE 'KBANK_' sy-datum '_' sy-timlo '.txt' INTO gs_path_name.

                 PERFORM f_text_kbank.
                 CONCATENATE p_path gs_path_name INTO gs_path_name SEPARATED BY '\'.
                 PERFORM export USING gs_path_name
                                      gt_txt_kbank.
            ENDIF.
    ELSE.

*            IF NOT gt_itab_bbl IS INITIAL.
                 PERFORM f_text_bbl.
                 PERFORM f_text_bbl_a30.
*                 PERFORM F_TEXT_EXPORT_SERVER_BBL.
*                 PERFORM F_TEXT_EXPORT_SERVER_a30.


*            ENDIF.
*            IF NOT gt_itab_scb IS INITIAL.
                PERFORM f_text_scb.
*            ENDIF.
            IF NOT gt_itab_kbank IS INITIAL.
                PERFORM f_text_kbank.
            ENDIF.
    ENDIF.
  ELSEIF NOT r_credit IS INITIAL.
           PERFORM f_text_bbl.
           PERFORM f_text_bbl_a30.
           PERFORM f_text_scb.
  ELSE.
*    MESSAGE i004.
    EXIT.
  ENDIF.
*--------------------------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&      Form  f_modify_screen
**&---------------------------------------------------------------------*


FORM f_modify_screen.
  IF NOT r_add IS INITIAL.
    LOOP AT SCREEN.
      %hide 'SC1'.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      %show 'SC1'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "f_modify_screen
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*

FORM f_get_data .

  DATA: lv_vbelv TYPE vbfa-vbelv.
  DATA: lv_inv_remark(200) TYPE c.
  DATA: gs_date TYPE sy-datum.
  DATA: lv_check_doc_dup TYPE bsid-belnr.

  gs_date = sy-datum.

     SELECT kunnr bank_code b_name_s b_name_l
          INTO TABLE gt_ZSDSFIC022
          FROM ZSDSFIC022
          WHERE kunnr IN s_kunnr.


     IF  s_kunnr IS INITIAL.



          LOOP AT gt_ZSDSFIC022 INTO gs_ZSDSFIC022.
               s_kunnr  = 'IEQ'.
               s_kunnr-low  = gs_ZSDSFIC022-kunnr.
               APPEND s_kunnr.
          ENDLOOP.
     ENDIF.

     SELECT blart shkzg EPP_BANK_DOCTYPE
     INTO TABLE gt_ZSDSFIC023
     FROM ZSDSFIC023.


         LOOP AT gt_ZSDSFIC023 INTO gs_ZSDSFIC023 .
                 r_blart = 'IEQ'.
                 r_blart-low = gs_ZSDSFIC023-blart.

                 append r_blart.
         ENDLOOP.


     SELECT a~mwskz b~kbetr
     INTO TABLE gt_tax_rate
     FROM a003 AS a INNER JOIN konp AS b
                    ON ( a~knumh EQ b~knumh )
     WHERE a~aland EQ 'TH'.



     IF NOT gt_ZSDSFIC022 IS INITIAL.

        IF NOT r_cash IS INITIAL.
                 SELECT a~vbeln a~kunnr a~auart a~erdat a~knumv
                        e~adrnr
                        d~zterm
                 INTO TABLE gt_vbak_vbeln
                 FROM vbak AS a    INNER JOIN vbkd AS d
                                   ON ( a~vbeln EQ d~vbeln
                                   AND  d~posnr EQ '' )
                                   INNER JOIN vbpa as e
                                   ON ( a~vbeln EQ e~vbeln
                                   AND  e~parvw EQ 'AG'
                                   AND  e~posnr EQ '' )
                                   INNER JOIN ZSDSFIC022 AS z
                                   ON ( a~kunnr EQ z~kunnr )
*                 FOR ALL ENTRIES IN gt_ZSDSFIC022
*                 WHERE a~kunnr EQ gt_ZSDSFIC022-kunnr
                  WHERE a~kunnr IN s_kunnr
*                    AND  a~auart IN r_blart
                    AND  ( a~auart EQ 'ZQ01'
                     OR  a~auart EQ 'ZO01'
                     OR  a~auart EQ 'ZO04'
                     OR  a~auart EQ 'ZO07'
                   )
                    AND  a~vbeln IN s_belnr
                    AND  d~zterm EQ 'C000'.

                 DELETE ADJACENT DUPLICATES FROM   gt_vbak_vbeln.


                 LOOP AT gt_vbak_vbeln INTO gs_vbak_vbeln.

                               CLEAR: lv_vbelv.
                               SELECT SINGLE vbelv
                               INTO lv_vbelv
                               FROM vbfa
                               WHERE vbelv EQ gs_vbak_vbeln-vbeln
                               AND vbtyp_n EQ 'M'.

                               IF NOT lv_vbelv IS INITIAL.
                                  DELETE gt_vbak_vbeln INDEX sy-tabix.

                               ENDIF.




                 ENDLOOP.

                 IF NOT gt_vbak_vbeln IS INITIAL.
                   SELECT a~vbeln a~auart a~erdat a~kunnr
                           b~posnr b~netwr b~mwsbp b~prctr
                           d~zterm
                           e~adrnr
                    INTO TABLE gt_vbak_temp
                    FROM vbak AS a INNER JOIN vbap AS b
                                   ON ( a~vbeln EQ b~vbeln )
                                   INNER JOIN vbkd AS d
                                   ON ( a~vbeln EQ d~vbeln
                                   AND  d~posnr EQ '' )
                                   INNER JOIN vbpa as e
                                   ON ( a~vbeln EQ e~vbeln
                                   AND  e~parvw EQ 'AG'
                                   AND  e~posnr EQ '' )
                                   INNER JOIN ZSDSFIC022 AS z
                                   ON ( a~kunnr EQ z~kunnr )
                    FOR ALL ENTRIES IN gt_vbak_vbeln
                    WHERE a~vbeln EQ gt_vbak_vbeln-vbeln
                    AND   a~kunnr IN s_kunnr
                    AND  ( a~auart EQ 'ZQ01'
                     OR  a~auart EQ 'ZO01'
                     OR  a~auart EQ 'ZO04'
                     OR  a~auart EQ 'ZO07'
                      )
                    AND  a~vbeln IN s_belnr
                    AND   d~zterm EQ 'C000'.

                 ENDIF.



        ELSE.
                      SELECT a~belnr a~kunnr a~budat
                              a~zterm a~gjahr
                              a~zfbdt  "Add by Wantanee 20150827
                              a~zbd1t  "Add by Wantanee 20150827
                              b~blart
                              a~dmbtr
                              d~adrnr
                       INTO TABLE gt_bsid
                       FROM bsid AS a INNER JOIN bkpf AS b
                                      ON ( a~belnr EQ b~belnr )
                                      INNER JOIN kna1 AS d
                                      ON ( a~kunnr EQ d~kunnr )
                                      INNER JOIN ZSDSFIC022 AS z
                                      ON ( a~kunnr EQ z~kunnr )
*                       FOR ALL ENTRIES IN gt_ZSDSFIC022
*                       WHERE a~kunnr EQ gt_ZSDSFIC022-kunnr
                        FOR ALL ENTRIES IN gt_ZSDSFIC023
                        WHERE b~blart EQ gt_ZSDSFIC023-blart
                        AND a~bukrs EQ p_bukrs
                        AND   b~bukrs EQ p_bukrs
                        AND a~kunnr IN s_kunnr
                        AND a~bukrs EQ p_bukrs
                        AND  a~belnr IN s_belnr
                        AND   a~zterm NE 'C000'
                        AND   a~zterm NE 'C001' . "CH10  Add by Wantanee 20210506




                       SELECT a~belnr a~kunnr a~budat
                              a~zterm a~gjahr
                              a~zfbdt  "Add by Wantanee 20150827
                              a~zbd1t  "Add by Wantanee 20150827
                              b~blart
                              a~dmbtr
                              d~adrnr
                       INTO TABLE gt_bsid_cn
                       FROM bsid AS a INNER JOIN bkpf AS b
                                      ON ( a~belnr EQ b~belnr )
                                      INNER JOIN kna1 AS d
                                      ON ( a~kunnr EQ d~kunnr )
                                      INNER JOIN ZSDSFIC023 AS z
                                      ON ( b~blart EQ z~blart
                                      AND  z~EPP_BANK_DOCTYPE EQ 'RC' )
                                      INNER JOIN ZSDSFIC022 AS ze
                                      ON ( a~kunnr EQ ze~kunnr )
*
                        FOR ALL ENTRIES IN gt_ZSDSFIC023
                        WHERE b~blart EQ gt_ZSDSFIC023-blart
                        AND a~bukrs EQ p_bukrs
                        AND   b~bukrs EQ p_bukrs
                        AND a~kunnr IN s_kunnr
                        AND a~bukrs EQ p_bukrs
                        AND  a~belnr IN s_belnr.
                       " AND  a~zterm NE 'A000'
*                        AND  b~blart EQ 'RA'.

                      IF NOT gt_bsid_cn IS INITIAL.
                         LOOP AT gt_bsid_cn INTO gs_bsid_cn.
                              CLEAR: lv_inv_remark.
                              PERFORM read_text USING   'Z011' 'VBBK' gs_bsid_cn-belnr
                                                      CHANGING lv_inv_remark.

                              TRANSLATE lv_inv_remark TO UPPER CASE.
                              SEARCH lv_inv_remark FOR '#EPP#' .
                              IF sy-subrc EQ 0.
                                 READ TABLE gt_bsid INTO wa_bsid WITH KEY belnr = gs_bsid_cn-belnr.
                                      IF sy-subrc NE 0.
                                             APPEND gs_bsid_cn TO gt_bsid.
                                      ENDIF.
                              ELSE.
                                  READ TABLE gt_bsid INTO wa_bsid WITH KEY belnr = gs_bsid_cn-belnr.
                                      IF sy-subrc EQ 0.
                                             DELETE gt_bsid index sy-tabix.
                                      ENDIF.

                              ENDIF.


                         ENDLOOP.

                      ENDIF.

                      IF NOT gt_bsid IS  INITIAL.
                            SELECT belnr gjahr mwskz shkzg dmbtr
                            INTO TABLE gt_bsis
                            FROM bsis
                            FOR ALL ENTRIES IN gt_bsid
                            WHERE bukrs EQ p_bukrs
                             AND  belnr EQ gt_bsid-belnr
                             AND  gjahr EQ gt_bsid-gjahr
                             AND  hkont IN ('2492000110','2492000120','2493000020').

                            SELECT belnr gjahr prctr
                            INTO TABLE gt_prctr
                            FROM bsis
                            FOR ALL ENTRIES IN gt_bsid
                            WHERE bukrs EQ p_bukrs
                             AND  belnr EQ gt_bsid-belnr
                             AND  gjahr EQ gt_bsid-gjahr
                             AND  hkont NOT IN ('2492000110','2492000120','2493000020').

                              DELETE ADJACENT DUPLICATES FROM gt_prctr.


                           SELECT a~vbeln b~kvgr2 b~augru b~mahdt b~vkbur
                           INTO TABLE gt_so
                           FROM vbfa AS a INNER JOIN vbak AS b
                                          ON ( a~vbelv EQ b~vbeln )
                           FOR ALL ENTRIES IN gt_bsid
                           WHERE a~vbeln EQ gt_bsid-belnr
                              AND  a~vbtyp_v EQ 'C'.

                           SORT gt_so.
                           DELETE ADJACENT DUPLICATES FROM gt_so.



                        gt_bsid_temp = gt_bsid.
                        CLEAR: gt_bsid.

*                               belnr    TYPE bsid-belnr,
*       kunnr    TYPE bsid-kunnr,
*       budat    TYPE bsid-budat,
*       zterm    TYPE bsid-zterm,
*       gjahr    TYPE bsid-gjahr,
*       zfbdt   type bsid-zfbdt,    "Baseline Payment Dte
*       zbd1t   type bsid-zbd1t,    "Terms of Payment
*
*       blart    TYPE bkpf-blart,
*       dmbtr    TYPE bsid-dmbtr,
*       adrnr    TYPE kna1-adrnr,

                        SORT gt_bsid_temp by belnr.
                        CLEAR: lv_check_doc_dup.
                        LOOP AT gt_bsid_temp INTO gs_bsid.
                              IF lv_check_doc_dup EQ gs_bsid-belnr.
                                 COLLECT gs_bsid INTO gt_bsid.
                              ELSE.
                                 APPEND gs_bsid TO gt_bsid.
                              ENDIF.
                              lv_check_doc_dup = gs_bsid-belnr.


                        ENDLOOP.



                      ENDIF.



        ENDIF.
      ENDIF.











ENDFORM.   "Get data.

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_DEL
*&---------------------------------------------------------------------*

FORM f_get_data_del .

  DATA: gs_date TYPE sy-datum.
  break wantanee.
  gs_date = sy-datum.

     IF NOT gt_ZSDSFIC022 IS INITIAL.




                 "Add by Wantanee 20160610
                        SELECT a~belnr a~kunnr a~budat
                              a~zterm a~gjahr
                              a~zfbdt  "Add by Wantanee 20150827
                              a~zbd1t  "Add by Wantanee 20150827
                              a~blart
                              a~dmbtr
                              d~adrnr
                       INTO TABLE gt_bsad
                       FROM bsad AS a
*                                      INNER JOIN bkpf AS b
*                                      ON ( a~belnr EQ b~belnr )
                                      INNER JOIN kna1 AS d
                                      ON ( a~kunnr EQ d~kunnr )
                                      INNER JOIN ZSDSFIC022 AS z
                                      ON ( a~kunnr EQ z~kunnr )
                                      INNER JOIN bkpf as f
                                      ON ( a~augbl EQ f~belnr
                                      AND  a~augdt EQ f~budat )
*                       FOR ALL ENTRIES IN gt_ZSDSFIC022
*                       WHERE a~kunnr EQ gt_ZSDSFIC022-kunnr
                        FOR ALL ENTRIES IN gt_ZSDSFIC023
                        WHERE a~blart EQ gt_ZSDSFIC023-blart
                        AND a~bukrs EQ p_bukrs
*                        AND   b~bukrs EQ p_bukrs
                        AND a~kunnr IN s_kunnr
                        AND a~bukrs EQ p_bukrs
                        AND  a~belnr IN s_belnr
                        AND  f~cpudt EQ gs_date
                        AND  a~zterm NE 'C000'.
                     IF  gt_bsad IS INITIAL.
                       gs_date = gs_date - 1.
                        SELECT a~belnr a~kunnr a~budat
                              a~zterm a~gjahr
                              a~zfbdt  "Add by Wantanee 20150827
                              a~zbd1t  "Add by Wantanee 20150827
                              a~blart
                              a~dmbtr
                              d~adrnr
                       INTO TABLE gt_bsad
                       FROM bsad AS a
*                                      INNER JOIN bkpf AS b
*                                      ON ( a~belnr EQ b~belnr )
                                      INNER JOIN kna1 AS d
                                      ON ( a~kunnr EQ d~kunnr )
                                      INNER JOIN ZSDSFIC022 AS z
                                      ON ( a~kunnr EQ z~kunnr )
                                      INNER JOIN bkpf as f
                                      ON ( a~augbl EQ f~belnr
                                      AND  a~augdt EQ f~budat )
*                       FOR ALL ENTRIES IN gt_ZSDSFIC022
*                       WHERE a~kunnr EQ gt_ZSDSFIC022-kunnr
                        FOR ALL ENTRIES IN gt_ZSDSFIC023
                        WHERE a~blart EQ gt_ZSDSFIC023-blart
                        AND a~bukrs EQ p_bukrs
*                        AND   b~bukrs EQ p_bukrs
                        AND a~kunnr IN s_kunnr
                        AND a~bukrs EQ p_bukrs
                        AND  a~belnr IN s_belnr
*                        AND  a~augdt EQ gs_date
                        AND  f~cpudt EQ gs_date
                        AND  a~zterm NE 'C000'.
                     ENDIF.

                 "End Add by Wantanee 20160610



                      IF NOT gt_bsad IS  INITIAL.
                            SELECT belnr gjahr mwskz shkzg dmbtr
                            INTO TABLE gt_bsis_del
                            FROM bsis
                            FOR ALL ENTRIES IN gt_bsad
                            WHERE bukrs EQ p_bukrs
                             AND  belnr EQ gt_bsad-belnr
                             AND  gjahr EQ gt_bsad-gjahr
                             AND  hkont IN ('2492000110','2492000120','2493000020').

                            SELECT belnr gjahr prctr
                            INTO TABLE gt_prctr_del
                            FROM bsis
                            FOR ALL ENTRIES IN gt_bsad
                            WHERE bukrs EQ p_bukrs
                             AND  belnr EQ gt_bsad-belnr
                             AND  gjahr EQ gt_bsad-gjahr
                             AND  hkont NOT IN ('2492000110','2492000120','2493000020').

                              DELETE ADJACENT DUPLICATES FROM gt_prctr_del.


                           SELECT a~vbeln b~kvgr2 b~augru b~mahdt b~vkbur
                           INTO TABLE gt_so_del
                           FROM vbfa AS a INNER JOIN vbak AS b
                                          ON ( a~vbelv EQ b~vbeln )
                           FOR ALL ENTRIES IN gt_bsad
                           WHERE a~vbeln EQ gt_bsad-belnr
                              AND  a~vbtyp_v EQ 'C'.

                           SORT gt_so_del.
                           DELETE ADJACENT DUPLICATES FROM gt_so_del.





                      ENDIF.


      ENDIF.











ENDFORM.   "Get data.
*&---------------------------------------------------------------------*
*&      Form  MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM f_map_data.
  DATA: gs_vat_rate TYPE konv-kbetr.
  DATA: name_eng(100)    TYPE c.
  DATA: gs_date_replace TYPE vbak-mahdt.
  DATA: lv_check_sale_office TYPE c.  "Check Sale Office NE 1015,1016







  SORT gt_ZSDSFIC022 BY bank_code kunnr.



  LOOP AT gt_ZSDSFIC022 INTO gs_ZSDSFIC022.

       LOOP AT gt_vbak_vbeln INTO gs_vbak_vbeln WHERE kunnr EQ gs_ZSDSFIC022-kunnr.

               CLEAR: wa_itab.
               wa_itab-kunnr = gs_ZSDSFIC022-kunnr.
               wa_itab-bank_code = gs_ZSDSFIC022-bank_code.
               wa_itab-b_name_s = gs_ZSDSFIC022-b_name_s.

                wa_itab-belnr = gs_vbak_vbeln-vbeln.
                wa_itab-zterm = gs_vbak_vbeln-zterm.

                SELECT SINGLE kbetr
                INTO gs_vat_rate
                FROM konv
                WHERE knumv EQ gs_vbak_vbeln-knumv
                AND kschl EQ 'MWST'.

                wa_itab-vat_rate_s = gs_vat_rate * ( 10 / 100 ) .
                gs_vat_rate = gs_vat_rate * 10.
                wa_itab-vat_rate = gs_vat_rate.

                PERFORM get_customer USING wa_itab-kunnr  gs_vbak_vbeln-adrnr CHANGING wa_itab-name name_eng.
                wa_itab-doc_type = gs_vbak_vbeln-auart.
                wa_itab-doc_type_c = 'Quotation'.
                wa_itab-budat = gs_vbak_vbeln-erdat.
                wa_itab-due_date = gs_vbak_vbeln-erdat.
                wa_itab-shkzg = 'S'.

                LOOP AT gt_vbak_temp INTO gs_vbak_temp WHERE kunnr EQ gs_ZSDSFIC022-kunnr
                                                       AND vbeln EQ gs_vbak_vbeln-vbeln.
                        wa_itab-dmbtr = wa_itab-dmbtr + ( gs_vbak_temp-netwr + gs_vbak_temp-mwsbp ).
                        wa_itab-mwsbp = wa_itab-mwsbp + gs_vbak_temp-mwsbp.
                        wa_itab-base_amt = wa_itab-base_amt + ( gs_vbak_temp-netwr ).
                        wa_itab-prctr  = gs_vbak_temp-prctr.
                ENDLOOP.
                IF wa_itab-dmbtr NE 0.


                      wa_itab-dmbtr_s = wa_itab-dmbtr.

                      wa_itab-mwsbp_s = wa_itab-mwsbp.
                      wa_itab-base_amt_s = wa_itab-base_amt.
                      APPEND wa_itab TO gt_itab.

                      CLEAR: wa_itab_scb,wa_itab_bbl,wa_itab_kbank.
                      IF wa_itab-bank_code EQ '002'. "BBL
                         wa_itab_bbl-cust_no = wa_itab-kunnr.
                         IF ( wa_itab-doc_type EQ 'ZO07' OR wa_itab-doc_type EQ 'ZO04' ).
                              wa_itab-doc_type = 'ZO01'.
                         ENDIF.
                         wa_itab_bbl-doc_type = wa_itab-doc_type.
                         wa_itab_bbl-doc_no = wa_itab-belnr.
                         wa_itab_bbl-doc_date = wa_itab-budat.
                         wa_itab_bbl-due_date = wa_itab-budat.  "add by Wantanee 20150827

                         wa_itab_bbl-dmbtr =   wa_itab-dmbtr.

                         wa_itab_bbl-shkzg = wa_itab-shkzg.
                         wa_itab_bbl-user_data2 = wa_itab-prctr.
                         wa_itab_bbl-record_type = 'A'.



                         CLEAR: gs_amt_temp.
                         gs_amt_temp = wa_itab-dmbtr * 100.
                         UNPACK gs_amt_temp TO gs_amt_c.
*                         MOVE gs_amt_temp TO gs_amt.
                         WRITE gs_amt_c TO wa_itab_bbl-doc_amt.

                         "Add by wantanee 20160224

                         gs_amt_temp = wa_itab-base_amt * 100.
                         UNPACK gs_amt_temp TO gs_amt_c.
                         WRITE gs_amt_c TO wa_itab_bbl-doc_base_amt .
                         "End Add by wantanee 20160224

                         APPEND wa_itab_bbl TO gt_itab_bbl.

                      ELSEIF wa_itab-bank_code EQ '004'.  "KBANK
                         wa_itab_kbank-cust_no = wa_itab-kunnr.
                         wa_itab_kbank-doc_type = 'QT'.
                         wa_itab_kbank-doc_no = wa_itab-belnr.
                         wa_itab_kbank-doc_date = wa_itab-budat.
                         wa_itab_kbank-due_date = wa_itab-budat. "add by Wantanee 20150827
                         wa_itab_kbank-dmbtr = wa_itab-dmbtr.
                         wa_itab_kbank-shkzg = wa_itab-shkzg.
                         wa_itab_kbank-profit = wa_itab-prctr. "Add by Wantanee 20150929
                         wa_itab_kbank-doc_flag = '1'.
                         wa_itab_kbank-record_type = 'A'.  "Recode Mode


                         CLEAR: gs_amt_temp.
                         gs_amt_temp = wa_itab-dmbtr * 100.
                         UNPACK gs_amt_temp TO gs_amt_c.
*                         MOVE gs_amt_temp TO gs_amt.
                         WRITE gs_amt_c TO wa_itab_kbank-doc_amt .

                         IF wa_itab-doc_type EQ 'R1'.
                               IF wa_itab_kbank-doc_date GE '20160216'.
                                  wa_itab_kbank-inv_flag = '1'.
                               ELSE.
                                  wa_itab_kbank-inv_flag = '0'.
                               ENDIF.
                         ELSE.
                               wa_itab_kbank-inv_flag = '0'.
                         ENDIF.
                         wa_itab_kbank-payment_term = '00000'.

                         APPEND wa_itab_kbank TO gt_itab_kbank.

                      ELSEIF wa_itab-bank_code EQ '014'.  "SCB
                         wa_itab_scb-cust_no = wa_itab-kunnr.
                         wa_itab_scb-doc_type = 'RI'.
                         wa_itab_scb-doc_no = wa_itab-belnr.
                         wa_itab_scb-doc_date = wa_itab-budat.
                         wa_itab_scb-due_date = wa_itab-budat. "add by Wantanee 20150827
                         wa_itab_scb-dmbtr = wa_itab-dmbtr.
                         wa_itab_scb-shkzg = wa_itab-shkzg.
                         wa_itab_scb-record_type = 'A'.

                         gs_amt_temp = wa_itab-dmbtr * 100.
                         UNPACK gs_amt_temp TO gs_amt_c.
*                         MOVE gs_amt_temp TO gs_amt.
                         WRITE gs_amt_c TO wa_itab_scb-doc_amt .

                         CLEAR: gs_amt_temp.
                         gs_amt_temp = wa_itab-base_amt * 100.
                         UNPACK gs_amt_temp TO gs_amt_c.
                         WRITE gs_amt_c TO wa_itab_scb-doc_base_amt .

                         CLEAR: gs_amt_temp.
                         gs_amt_temp = wa_itab-vat_rate.
                         UNPACK gs_amt_temp TO gs_vat_rate1.
                         WRITE gs_vat_rate1 TO wa_itab_scb-vat_rate .


                         wa_itab_scb-allocation_num = wa_itab-prctr.
                         IF wa_itab-doc_type EQ 'R1'.
                               IF wa_itab_scb-doc_date GE '20160216'.
                                  wa_itab_scb-inv_flag = '1'.
                               ELSE.
                                  wa_itab_scb-inv_flag = '0'.
                               ENDIF.
                         ELSE.
                               wa_itab_scb-inv_flag = '0'.
                         ENDIF.

                         APPEND wa_itab_scb TO gt_itab_scb.

                      ENDIF.
                 ENDIF.
       ENDLOOP.

*       a~belnr a~kunnr a~budat
*                a~zterm a~gjahr
*                b~blart
*                a~dmbtr
*                d~adrnr

       LOOP AT gt_bsid INTO wa_bsid WHERE kunnr EQ gs_ZSDSFIC022-kunnr.

               CLEAR: wa_itab,lv_check_sale_office,gs_so_temp.

               READ TABLE gt_so INTO gs_so1 WITH KEY vbeln = wa_bsid-belnr
                                                    kvgr2 = 'ZR5'.
                    IF sy-subrc NE 0.
                       READ TABLE gt_so INTO gs_so1 WITH KEY vbeln = wa_bsid-belnr
                                                            augru = 'Z32'.
                            IF sy-subrc NE 0.
                                 READ TABLE gt_so INTO gs_so1 WITH KEY vbeln = wa_bsid-belnr
                                                            augru = 'Z33'.
                                       IF sy-subrc NE 0.
                                         IF wa_bsid-blart EQ 'R1'.

                                            LOOP AT gt_so INTO gs_so_temp WHERE vbeln = wa_bsid-belnr.
                                                 IF gs_so_temp-vkbur EQ '1000' OR gs_so_temp-vkbur EQ '1002'
                                                    OR gs_so_temp-vkbur EQ '1004' OR gs_so_temp-vkbur EQ '1005'
                                                    OR gs_so_temp-vkbur EQ '1015' OR gs_so_temp-vkbur EQ '1020'
                                                    OR gs_so_temp-vkbur EQ '1021' OR gs_so_temp-vkbur EQ '1022'
                                                    OR gs_so_temp-vkbur EQ '1023' OR gs_so_temp-vkbur EQ '1024'
                                                   .
                                                      lv_check_sale_office = 'X'.
                                                 ELSE.
                                                      lv_check_sale_office = ''.
                                                 ENDIF.

                                            ENDLOOP.
                                          ELSE.
                                              lv_check_sale_office = ''.
                                          ENDIF.



                                                     wa_itab-kunnr = gs_ZSDSFIC022-kunnr.
                                                     wa_itab-bank_code = gs_ZSDSFIC022-bank_code.
                                                     wa_itab-b_name_s = gs_ZSDSFIC022-b_name_s.
                                                     wa_itab-belnr = wa_bsid-belnr.
                                                     wa_itab-zterm = wa_bsid-zterm.



                                                     PERFORM get_customer USING wa_itab-kunnr  wa_bsid-adrnr CHANGING wa_itab-name name_eng.

                                                     READ TABLE gt_ZSDSFIC023 INTO gs_ZSDSFIC023 WITH KEY blart = wa_bsid-blart.
                                                          IF sy-subrc EQ 0.
                                                                IF gs_ZSDSFIC022-bank_code = '014'.
                                                                     wa_itab-doc_type = gs_ZSDSFIC023-EPP_BANK_DOCTYPE.
                                                                ELSE.
                                                                     wa_itab-doc_type = wa_bsid-blart.
                                                                ENDIF.


                                                                IF gs_ZSDSFIC023-EPP_BANK_DOCTYPE EQ 'RI'.
                                                                   wa_itab-doc_type_c = 'Invoice'.
                                                                ELSEIF gs_ZSDSFIC023-EPP_BANK_DOCTYPE EQ 'RD'.
                                                                   wa_itab-doc_type_c = 'Debit Note'.
                                                                ELSEIF gs_ZSDSFIC023-EPP_BANK_DOCTYPE EQ 'RC'.
                                                                   wa_itab-doc_type_c = 'Credit Note'.
                                                                ENDIF.
                                                                wa_itab-shkzg = gs_ZSDSFIC023-shkzg.
                                                           ENDIF.
                                                       READ TABLE gt_so INTO gs_so1_temp WITH KEY vbeln = wa_bsid-belnr.
                                                            IF sy-subrc EQ 0.
                                                                IF NOT gs_so1_temp-mahdt IS INITIAL.
                                                                      wa_itab-budat = gs_so1_temp-mahdt.
                                                                      wa_itab-due_date = wa_itab-budat + wa_bsid-zbd1t.
                                                                ELSE.
                                                                      wa_itab-budat = wa_bsid-budat.
                                                                      wa_itab-due_date = wa_bsid-zfbdt + wa_bsid-zbd1t.
                                                                ENDIF.
                                                             ELSE.
                                                                     wa_itab-budat = wa_bsid-budat.
                                                                      wa_itab-due_date = wa_bsid-zfbdt + wa_bsid-zbd1t.

                                                            ENDIF.


                                                       wa_itab-dmbtr = wa_bsid-dmbtr.


                                                       READ TABLE gt_bsis INTO gs_bsis WITH KEY belnr = wa_bsid-belnr
                                                                                                gjahr = wa_bsid-gjahr.
                                                            IF sy-subrc EQ 0.

                                                                wa_itab-mwsbp = wa_itab-mwsbp + gs_bsis-dmbtr.
                                                                wa_itab-base_amt = wa_itab-base_amt + ( wa_itab-dmbtr - gs_bsis-dmbtr ).
                                                                READ TABLE gt_tax_rate INTO gs_tax_rate WITH KEY mwskz = gs_bsis-mwskz.
                                                                     IF sy-subrc EQ 0.

                                                                        gs_vat_rate = gs_tax_rate-kbetr * 10.
                                                                        wa_itab-vat_rate_s = gs_vat_rate * ( 10 / 1000 ) .
                                                                        wa_itab-vat_rate = gs_vat_rate.
                                                                     ENDIF.
                                                            ENDIF.
                                                       IF gs_ZSDSFIC023-shkzg EQ 'S'.

                                                          wa_itab-dmbtr_s = wa_bsid-dmbtr.
                                                          wa_itab-mwsbp_s = wa_itab-mwsbp.
                                                          wa_itab-base_amt_s = wa_itab-base_amt.
                                                       ELSE.
                                                          wa_itab-dmbtr_s = wa_bsid-dmbtr * -1 .
                                                          wa_itab-mwsbp_s = wa_itab-mwsbp * -1.
                                                          wa_itab-base_amt_s = wa_itab-base_amt * -1.
                                                       ENDIF.
                                                       READ TABLE gt_prctr INTO gs_prctr WITH KEY belnr = wa_bsid-belnr
                                                                                                  gjahr = wa_bsid-gjahr.
                                                            IF sy-subrc EQ 0.
                                                               wa_itab-prctr = gs_prctr-prctr.
                                                            ENDIF.



                                                     APPEND wa_itab TO gt_itab.

                                                     CLEAR: wa_itab_scb,wa_itab_bbl,wa_itab_kbank.
                                                     IF wa_itab-bank_code EQ '002'. "BBL
                                                        wa_itab_bbl-cust_no = wa_itab-kunnr.
                                                        wa_itab_bbl-doc_type =  wa_bsid-blart.
                                                        wa_itab_bbl-doc_no = wa_itab-belnr.
                                                        wa_itab_bbl-doc_date = wa_itab-budat.
                                                        wa_itab_bbl-due_date = wa_itab-due_date.  "Add by Wantanee 20150827
                                                        wa_itab_bbl-dmbtr = wa_itab-dmbtr.
                                                        wa_itab_bbl-shkzg = wa_itab-shkzg.
                                                        wa_itab_bbl-user_data2 = wa_itab-prctr.
                                                        wa_itab_bbl-record_type = 'A'.

                                                        CLEAR: gs_amt_temp.
                                                        gs_amt_temp = wa_itab-dmbtr * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
                                                        WRITE gs_amt_c TO wa_itab_bbl-doc_amt.

                                                        CLEAR: gs_amt_temp.
                                                        gs_amt_temp = wa_itab-base_amt * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
                                                        WRITE gs_amt_c TO wa_itab_bbl-doc_base_amt .


                                                        IF wa_bsid-blart EQ 'R1'.

                                                              IF wa_itab_bbl-doc_date GE '20160301'.
                                                                  IF lv_check_sale_office EQ 'X'.
                                                                     IF wa_bsid-zterm EQ 'A060' OR wa_bsid-zterm EQ 'A030'.
                                                                        wa_itab_bbl-doc_type = 'R1'.
                                                                     ELSE.
                                                                        wa_itab_bbl-doc_type = 'ZR1'.
                                                                     ENDIF.

                                                                  ELSE.
                                                                    wa_itab_bbl-doc_type = 'ZR1'.
                                                                  ENDIF.
                                                              ELSE.
                                                                    wa_itab_bbl-doc_type = 'ZR1'.
                                                              ENDIF.
                                                        ELSE.
                                                                    wa_itab_bbl-doc_type =  wa_bsid-blart. "'ZR1'.
                                                        ENDIF.
                                                        wa_itab_bbl-zterm = wa_bsid-zterm.

                                                        APPEND wa_itab_bbl TO gt_itab_bbl.

                                                     ELSEIF wa_itab-bank_code EQ '004'.  "KBANK
                                                        wa_itab_kbank-cust_no = wa_itab-kunnr.
                                                        wa_itab_kbank-doc_type =  wa_bsid-blart.
                                                        wa_itab_kbank-doc_no = wa_itab-belnr.
                                                        wa_itab_kbank-doc_date = wa_itab-budat.
                                                        wa_itab_kbank-due_date = wa_itab-due_date.  "Add by Wantanee 20150827
                                                        wa_itab_kbank-dmbtr = wa_itab-dmbtr.
                                                        wa_itab_kbank-shkzg = wa_itab-shkzg. "Add by Wantanee 20150929
                                                        wa_itab_kbank-profit = wa_itab-prctr.
                                                        IF wa_itab-shkzg EQ 'H'.
                                                           wa_itab_kbank-doc_flag = '0'.
                                                        ELSE.
                                                           wa_itab_kbank-doc_flag = '1'.
                                                        ENDIF.
                                                        wa_itab_kbank-record_type = 'A'.

                                                        gs_amt_temp = wa_itab-dmbtr * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
*                                                        MOVE gs_amt_temp TO gs_amt.
                                                        WRITE gs_amt_c TO wa_itab_kbank-doc_amt .
                                                            IF wa_bsid-blart EQ 'R1'.
                                                                  IF wa_itab_kbank-doc_date GE '20160216'.
                                                                     IF lv_check_sale_office EQ 'X'.
                                                                         IF wa_bsid-zterm EQ 'A060'.
                                                                            wa_itab_kbank-inv_flag = '1'.
                                                                            wa_itab_kbank-payment_term = '00060'.
                                                                         ELSEIF wa_bsid-zterm EQ 'A030'.
                                                                            wa_itab_kbank-inv_flag = '1'.
                                                                            wa_itab_kbank-payment_term = '00030'.
                                                                         ELSE.
                                                                            wa_itab_kbank-inv_flag = '0'.
                                                                            wa_itab_kbank-payment_term = '00000'.
                                                                         ENDIF.
                                                                     ELSE.
                                                                        wa_itab_kbank-inv_flag = '0'.
                                                                        wa_itab_kbank-payment_term = '00000'.
                                                                     ENDIF.
                                                                  ELSE.
                                                                        wa_itab_kbank-inv_flag = '0'.
                                                                        wa_itab_kbank-payment_term = '00000'.
                                                                  ENDIF.
                                                            ELSE.
                                                                    wa_itab_kbank-inv_flag = '0'.
                                                                    wa_itab_kbank-payment_term = '00000'.
                                                        ENDIF.



                                                        APPEND wa_itab_kbank TO gt_itab_kbank.

                                                     ELSEIF wa_itab-bank_code EQ '014'.  "SCB
                                                        wa_itab_scb-cust_no = wa_itab-kunnr.
                                                        wa_itab_scb-doc_type = wa_itab-doc_type.
                                                        wa_itab_scb-doc_no = wa_itab-belnr.
                                                        wa_itab_scb-doc_date = wa_itab-budat.
                                                        wa_itab_scb-due_date = wa_itab-due_date.  "Add by Wantanee 20150827
                                                        wa_itab_scb-dmbtr = wa_itab-dmbtr.
                                                        wa_itab_scb-shkzg = wa_itab-shkzg.
                                                        wa_itab_scb-record_type = 'A'.

                                                        gs_amt_temp = wa_itab-dmbtr * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
*                                                        MOVE gs_amt_temp TO gs_amt.
                                                        WRITE gs_amt_c TO wa_itab_scb-doc_amt .

                                                         CLEAR: gs_amt_temp.
                                                        gs_amt_temp = wa_itab-base_amt * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
                                                        WRITE gs_amt_c TO wa_itab_scb-doc_base_amt .

                                                        CLEAR: gs_amt_temp.
                                                        gs_amt_temp = wa_itab-vat_rate.
                                                        UNPACK gs_amt_temp TO gs_vat_rate1.
                                                        WRITE gs_vat_rate1 TO wa_itab_scb-vat_rate .

                                                        wa_itab_scb-allocation_num = wa_itab-prctr.
                                                        IF wa_bsid-blart EQ 'R1'.
                                                              IF wa_itab_scb-doc_date GE '20160216'.
                                                                 IF lv_check_sale_office EQ 'X'.
                                                                     IF wa_bsid-zterm EQ 'A060' OR wa_bsid-zterm EQ 'A030'.
*                                                                    IF wa_bsid-zterm EQ 'A030'. "Edit case SCB check A030 only
                                                                        wa_itab_scb-inv_flag = '1'.
                                                                     ELSE.
                                                                        wa_itab_scb-inv_flag = '0'.
                                                                     ENDIF.


                                                                 ELSE.
                                                                    wa_itab_scb-inv_flag = '0'.
                                                                 ENDIF.
                                                              ELSE.
                                                                    wa_itab_scb-inv_flag = '0'.
                                                              ENDIF.
                                                        ELSE.
                                                                    wa_itab_scb-inv_flag = '0'.
                                                        ENDIF.
                                                        APPEND wa_itab_scb TO gt_itab_scb.

                                                     ENDIF.

                                   ENDIF.
                            ENDIF.




                    ENDIF.



       ENDLOOP.



  ENDLOOP.










ENDFORM.  "map data
*&---------------------------------------------------------------------*
*&      Form  MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM f_map_data_del.
  DATA: gs_vat_rate TYPE konv-kbetr.
  DATA: name_eng(100)    TYPE c.
  DATA: gs_date_replace TYPE vbak-mahdt.
  DATA: lv_check_sale_office TYPE c.  "Check Sale Office NE 1015,1016



  SORT gt_ZSDSFIC022 BY bank_code kunnr.
    LOOP AT gt_ZSDSFIC022 INTO gs_ZSDSFIC022.

       LOOP AT gt_bsad INTO gs_bsad WHERE kunnr EQ gs_ZSDSFIC022-kunnr.

               CLEAR: wa_itab,lv_check_sale_office,gs_so_temp.

               READ TABLE gt_so_del INTO gs_so1_del WITH KEY vbeln = gs_bsad-belnr
                                                    kvgr2 = 'ZR5'.
                    IF sy-subrc NE 0.
                       READ TABLE gt_so_del INTO gs_so1_del WITH KEY vbeln = gs_bsad-belnr
                                                            augru = 'Z32'.
                            IF sy-subrc NE 0.
                                 READ TABLE gt_so_del INTO gs_so1_del WITH KEY vbeln = gs_bsad-belnr
                                                            augru = 'Z33'.
                                       IF sy-subrc NE 0.
                                         IF gs_bsad-blart EQ 'R1'.
                                            LOOP AT gt_so_del INTO gs_so_temp WHERE vbeln = gs_bsad-belnr.
                                                 IF gs_so_temp-vkbur EQ '1015' OR gs_so_temp-vkbur EQ '1016'.
                                                      lv_check_sale_office = 'X'.
                                                 ELSE.
                                                      lv_check_sale_office = ''.
                                                 ENDIF.

                                            ENDLOOP.
                                          ELSE.
                                              lv_check_sale_office = ''.
                                          ENDIF.



                                                     wa_itab-kunnr = gs_ZSDSFIC022-kunnr.
                                                     wa_itab-bank_code = gs_ZSDSFIC022-bank_code.
                                                     wa_itab-b_name_s = gs_ZSDSFIC022-b_name_s.
                                                     wa_itab-belnr = gs_bsad-belnr.
                                                     wa_itab-zterm = gs_bsad-zterm.



                                                     PERFORM get_customer USING wa_itab-kunnr  gs_bsad-adrnr CHANGING wa_itab-name name_eng.

                                                     READ TABLE gt_ZSDSFIC023 INTO gs_ZSDSFIC023 WITH KEY blart = gs_bsad-blart.
                                                          IF sy-subrc EQ 0.
                                                                IF gs_ZSDSFIC022-bank_code = '014'.
                                                                     wa_itab-doc_type = gs_ZSDSFIC023-EPP_BANK_DOCTYPE.
                                                                ELSE.
                                                                     wa_itab-doc_type = gs_bsad-blart.
                                                                ENDIF.


                                                                IF gs_ZSDSFIC023-EPP_BANK_DOCTYPE EQ 'RI'.
                                                                   wa_itab-doc_type_c = 'Invoice'.
                                                                ELSEIF gs_ZSDSFIC023-EPP_BANK_DOCTYPE EQ 'RD'.
                                                                   wa_itab-doc_type_c = 'Debit Note'.
                                                                ELSEIF gs_ZSDSFIC023-EPP_BANK_DOCTYPE EQ 'RC'.
                                                                   wa_itab-doc_type_c = 'Credit Note'.
                                                                ENDIF.
                                                                wa_itab-shkzg = gs_ZSDSFIC023-shkzg.
                                                           ENDIF.
                                                       READ TABLE gt_so INTO gs_so1_temp WITH KEY vbeln = gs_bsad-belnr.
                                                            IF sy-subrc EQ 0.
                                                                IF NOT gs_so1_temp-mahdt IS INITIAL.
                                                                      wa_itab-budat = gs_so1_temp-mahdt.
                                                                      wa_itab-due_date = wa_itab-budat + gs_bsad-zbd1t.
                                                                ELSE.
                                                                      wa_itab-budat = gs_bsad-budat.
                                                                      wa_itab-due_date = gs_bsad-zfbdt + gs_bsad-zbd1t.
                                                                ENDIF.
                                                             ELSE.
*                                                                      wa_itab-budat = wa_bsid-budat. "CH11 Remove by Wantanee 20210907
                                                                      wa_itab-budat = gs_bsad-budat.   "CH11 Add by Wantanee 20210907
                                                                      wa_itab-due_date = gs_bsad-zfbdt + gs_bsad-zbd1t.

                                                            ENDIF.


                                                       wa_itab-dmbtr = gs_bsad-dmbtr.


                                                       READ TABLE gt_bsis_del INTO gs_bsis_del WITH KEY belnr = gs_bsad-belnr
                                                                                                gjahr = gs_bsad-gjahr.
                                                            IF sy-subrc EQ 0.

                                                                wa_itab-mwsbp = wa_itab-mwsbp + gs_bsis_del-dmbtr.
                                                                wa_itab-base_amt = wa_itab-base_amt + ( wa_itab-dmbtr - gs_bsis_del-dmbtr ).
                                                                READ TABLE gt_tax_rate INTO gs_tax_rate WITH KEY mwskz = gs_bsis_del-mwskz.
                                                                     IF sy-subrc EQ 0.

                                                                        gs_vat_rate = gs_tax_rate-kbetr * 10.
                                                                        wa_itab-vat_rate_s = gs_vat_rate * ( 10 / 1000 ) .
                                                                        wa_itab-vat_rate = gs_vat_rate.
                                                                     ENDIF.
                                                            ENDIF.
                                                       IF gs_ZSDSFIC023-shkzg EQ 'S'.

                                                          wa_itab-dmbtr_s = gs_bsad-dmbtr.
                                                          wa_itab-mwsbp_s = wa_itab-mwsbp.
                                                          wa_itab-base_amt_s = wa_itab-base_amt.
                                                       ELSE.
                                                          wa_itab-dmbtr_s = gs_bsad-dmbtr * -1 .
                                                          wa_itab-mwsbp_s = wa_itab-mwsbp * -1.
                                                          wa_itab-base_amt_s = wa_itab-base_amt * -1.
                                                       ENDIF.
                                                       READ TABLE gt_prctr_del INTO gs_prctr_del WITH KEY belnr = gs_bsad-belnr
                                                                                                  gjahr = gs_bsad-gjahr.
                                                            IF sy-subrc EQ 0.
                                                               wa_itab-prctr = gs_prctr_del-prctr.
                                                            ENDIF.



                                                     APPEND wa_itab TO gt_itab.

                                                     CLEAR: wa_itab_scb,wa_itab_bbl,wa_itab_kbank.
                                                     IF wa_itab-bank_code EQ '002'. "BBL
                                                        wa_itab_bbl-cust_no = wa_itab-kunnr.
                                                        wa_itab_bbl-doc_type = wa_itab-doc_type.
                                                        wa_itab_bbl-doc_no = wa_itab-belnr.
                                                        wa_itab_bbl-doc_date = wa_itab-budat.
                                                        wa_itab_bbl-due_date = wa_itab-due_date.  "Add by Wantanee 20150827
                                                        wa_itab_bbl-dmbtr = wa_itab-dmbtr.
                                                        wa_itab_bbl-shkzg = wa_itab-shkzg.
                                                        wa_itab_bbl-user_data2 = wa_itab-prctr.
                                                        wa_itab_bbl-record_type = 'D'.

                                                        CLEAR: gs_amt_temp.
                                                        gs_amt_temp = wa_itab-dmbtr * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
                                                        WRITE gs_amt_c TO wa_itab_bbl-doc_amt.

                                                        CLEAR: gs_amt_temp.
                                                        gs_amt_temp = wa_itab-base_amt * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
                                                        WRITE gs_amt_c TO wa_itab_bbl-doc_base_amt .


                                                        IF gs_bsad-blart EQ 'R1'.

                                                              IF wa_itab_bbl-doc_date GE '20160301'.
                                                                  IF lv_check_sale_office EQ 'X'.
                                                                     IF wa_bsid-zterm EQ 'A060'.
                                                                        wa_itab_bbl-doc_type = 'R1'.
                                                                     ELSE.
                                                                        wa_itab_bbl-doc_type = 'ZR1'.
                                                                     ENDIF.

                                                                  ELSE.
                                                                    wa_itab_bbl-doc_type = 'ZR1'.
                                                                  ENDIF.
                                                              ELSE.
                                                                    wa_itab_bbl-doc_type = 'ZR1'.
                                                              ENDIF.
                                                        ELSE.
                                                                    wa_itab_bbl-doc_type = 'ZR1'.
                                                        ENDIF.

                                                        APPEND wa_itab_bbl TO gt_itab_bbl.

                                                     ELSEIF wa_itab-bank_code EQ '004'.  "KBANK
                                                        wa_itab_kbank-cust_no = wa_itab-kunnr.
                                                        wa_itab_kbank-doc_type = wa_itab-doc_type.
                                                        wa_itab_kbank-doc_no = wa_itab-belnr.
                                                        wa_itab_kbank-doc_date = wa_itab-budat.
                                                        wa_itab_kbank-due_date = wa_itab-due_date.  "Add by Wantanee 20150827
                                                        wa_itab_kbank-dmbtr = wa_itab-dmbtr.
                                                        wa_itab_kbank-shkzg = wa_itab-shkzg. "Add by Wantanee 20150929
                                                        wa_itab_kbank-profit = wa_itab-prctr.
                                                        IF wa_itab-shkzg EQ 'H'.
                                                           wa_itab_kbank-doc_flag = '0'.
                                                        ELSE.
                                                           wa_itab_kbank-doc_flag = '1'.
                                                        ENDIF.
                                                        wa_itab_kbank-record_type = 'D'.

                                                        gs_amt_temp = wa_itab-dmbtr * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
*                                                        MOVE gs_amt_temp TO gs_amt.
                                                        WRITE gs_amt_c TO wa_itab_kbank-doc_amt .
                                                            IF wa_bsid-blart EQ 'R1'.
                                                                  IF wa_itab_kbank-doc_date GE '20160216'.
                                                                     IF lv_check_sale_office EQ 'X'.
                                                                         IF wa_bsid-zterm EQ 'A060'.
                                                                            wa_itab_kbank-inv_flag = '1'.
                                                                         ELSE.
                                                                            wa_itab_kbank-inv_flag = '0'.
                                                                         ENDIF.


                                                                     ELSE.
                                                                        wa_itab_kbank-inv_flag = '0'.
                                                                     ENDIF.
                                                                  ELSE.
                                                                        wa_itab_kbank-inv_flag = '0'.
                                                                  ENDIF.
                                                              ELSE.
                                                                    wa_itab_kbank-inv_flag = '0'.
                                                        ENDIF.



                                                        APPEND wa_itab_kbank TO gt_itab_kbank.

                                                     ELSEIF wa_itab-bank_code EQ '014'.  "SCB
                                                        wa_itab_scb-cust_no = wa_itab-kunnr.
                                                        wa_itab_scb-doc_type = wa_itab-doc_type.
                                                        wa_itab_scb-doc_no = wa_itab-belnr.
                                                        wa_itab_scb-doc_date = wa_itab-budat.
                                                        wa_itab_scb-due_date = wa_itab-due_date.  "Add by Wantanee 20150827
                                                        wa_itab_scb-dmbtr = wa_itab-dmbtr.
                                                        wa_itab_scb-shkzg = wa_itab-shkzg.
                                                        wa_itab_scb-record_type = 'D'.

                                                        gs_amt_temp = wa_itab-dmbtr * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
*                                                        MOVE gs_amt_temp TO gs_amt.
                                                        WRITE gs_amt_c TO wa_itab_scb-doc_amt .

                                                         CLEAR: gs_amt_temp.
                                                        gs_amt_temp = wa_itab-base_amt * 100.
                                                        UNPACK gs_amt_temp TO gs_amt_c.
                                                        WRITE gs_amt_c TO wa_itab_scb-doc_base_amt .

                                                        CLEAR: gs_amt_temp.
                                                        gs_amt_temp = wa_itab-vat_rate.
                                                        UNPACK gs_amt_temp TO gs_vat_rate1.
                                                        WRITE gs_vat_rate1 TO wa_itab_scb-vat_rate .

                                                        wa_itab_scb-allocation_num = wa_itab-prctr.
                                                        IF wa_bsid-blart EQ 'R1'.
                                                              IF wa_itab_scb-doc_date GE '20160216'.
                                                                 IF lv_check_sale_office EQ 'X'.
                                                                     IF wa_bsid-zterm EQ 'A060'.
                                                                        wa_itab_scb-inv_flag = '1'.
                                                                     ELSE.
                                                                        wa_itab_scb-inv_flag = '0'.
                                                                     ENDIF.


                                                                 ELSE.
                                                                    wa_itab_scb-inv_flag = '0'.
                                                                 ENDIF.
                                                              ELSE.
                                                                    wa_itab_scb-inv_flag = '0'.
                                                              ENDIF.
                                                        ELSE.
                                                                    wa_itab_scb-inv_flag = '0'.
                                                        ENDIF.
                                                        APPEND wa_itab_scb TO gt_itab_scb.

                                                     ENDIF.

                                   ENDIF.
                            ENDIF.




                    ENDIF.



           ENDLOOP.
        ENDLOOP.














ENDFORM.  "map data
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_Customer
*&---------------------------------------------------------------------*

FORM f_get_data_customer .
  CLEAR: V_LINES.

    IF NOT s_kunnr IS INITIAL.
       SELECT kunnr bank_code b_name_s b_name_l
          INTO TABLE gt_ZSDSFIC022
          FROM ZSDSFIC022
          WHERE kunnr IN s_kunnr.
    ELSE.
       SELECT kunnr bank_code b_name_s b_name_l
          INTO TABLE gt_ZSDSFIC022
          FROM ZSDSFIC022.
    ENDIF.

    DESCRIBE TABLE gt_ZSDSFIC022 LINES V_LINES.
    SORT gt_ZSDSFIC022.



ENDFORM.   "Get data.
*&---------------------------------------------------------------------*
*&      Form  F_MAP_DATA_CUSTOMER
*&---------------------------------------------------------------------*

FORM f_map_data_customer .
  DATA: lv_temp_eng(200) TYPE c.


    IF NOT r_add IS  INITIAL.
        LOOP AT gt_ZSDSFIC022 INTO gs_ZSDSFIC022.
                 gs_inst_up_cust_bank-kunnr = gs_ZSDSFIC022-kunnr .
                 PERFORM get_customer USING gs_ZSDSFIC022-kunnr  '' CHANGING gs_inst_up_cust_bank-cust_name lv_temp_eng.
                 gs_inst_up_cust_bank-bank_key = gs_ZSDSFIC022-bank_code .

                 APPEND gs_inst_up_cust_bank TO gt_inst_up_cust_bank.

        ENDLOOP.
    ELSE.
         LOOP AT gt_ZSDSFIC022 INTO gs_ZSDSFIC022.
                 wa_itab_cust-kunnr = gs_ZSDSFIC022-kunnr .
                 PERFORM get_customer USING gs_ZSDSFIC022-kunnr  '' CHANGING wa_itab_cust-cust_name_th wa_itab_cust-cust_name_en .
                 wa_itab_cust-bank_code = gs_ZSDSFIC022-bank_code .
                 wa_itab_cust-b_name_s = gs_ZSDSFIC022-b_name_s .

                 APPEND wa_itab_cust TO gt_itab_cust.


         ENDLOOP.
    ENDIF.


ENDFORM.   "Get data.
**&---------------------------------------------------------------------*
**&      F_TEXT_BBL
**&---------------------------------------------------------------------*
FORM f_text_bbl.




DATA: g_space TYPE string,
      l_pos type i.
      g_space = cl_abap_conv_in_ce=>uccp( '00a0' ).
DATA: lv_bbl_corporate_name(30) TYPE c,
      lv_bbl_corporate_code(6)  TYPe c,
      lv_to_bank(3) TYPE c,
      gs_date_send(12) TYPE c.

     CLEAR: wa_itab,wa_itab_bbl.
     CLEAR: gs_header_bbl,gt_header_bbl,gs_detail_bbl,gt_detail_bbl,gs_footer_bbl,gt_footer_bbl.






      CLEAR: gs_total_count,gs_total_amt.
                      UNPACK gs_total_count TO gs_count_doc.
                      gs_amt_temp = gs_total_amt * 100.
                      UNPACK gs_amt_temp TO gs_amt_c.

                      CONCATENATE sy-datum sy-timlo+0(4) INTO gs_date_send.
                      gs_header_bbl-RECORDHEADER = 'H' .
                      gs_header_bbl-DATESEND = gs_date_send.
                      gs_header_bbl-CORPORATENAME = 'Siam Daikin Sales Co.,Ltd.'.
                      gs_header_bbl-CORPORATECODE = 'DAIKIN'.
                      gs_header_bbl-TOBANK = '002'.
                      gs_header_bbl-FILLER = ' '.
                      gs_header_bbl-ENDF = ' '.
                      APPEND gs_header_bbl TO gt_header_bbl.


      LOOP AT gt_itab_bbl INTO wa_itab_bbl WHERE zterm NE 'C030'.
                     gs_detail_bbl-RECORDTYPE = 'D'. "Record Type
                     gs_detail_bbl-BILLER_CODE = 'DAIKIN'. "Corporate Code (Biller)
                     gs_detail_bbl-PAYEE_CODE = wa_itab_bbl-payee_code. "Payee_code
                     gs_detail_bbl-PAYER_CODE = wa_itab_bbl-cust_no. "Customer Code
                     gs_detail_bbl-DOCUMENTTYPE = wa_itab_bbl-doc_type. "Document Type
                     gs_detail_bbl-DOCUMENTNO = wa_itab_bbl-doc_no. "Document No
                     gs_detail_bbl-DOCUMENTDATE = wa_itab_bbl-doc_date.  "Document Date
                     gs_detail_bbl-DOCUMENTDUEDATE = wa_itab_bbl-due_date.  "Document due date
                     gs_detail_bbl-DOCUMENTAMOUNT = wa_itab_bbl-doc_amt.    "Document Amount
                     gs_detail_bbl-OUTSTANDINGAMOUNT = wa_itab_bbl-doc_amt.    "Outstanding Amount
                     gs_detail_bbl-DOCUMENTBASEAMOUNT = wa_itab_bbl-doc_base_amt.    "'000000000000000'      "Document Base Amount
                     gs_detail_bbl-BASESTATUSFLAG = '1'.                    "0' "Base Status Flag
                     gs_detail_bbl-WHTRATE = '00000'.                "WHT RATE
                     gs_detail_bbl-WHT1 = '0000000000'.               "WHT Amount1
                     gs_detail_bbl-WHT2 = '0000000000'.               "WHT Amount2
                     gs_detail_bbl-VATSTATUSFLAG = '1'.                    "VAT Status Flag
                     gs_detail_bbl-VATRATE = '00700' .                "VAT RATE
                     gs_detail_bbl-DISCOUNTTYPE = 'B'.                    "Discount Type
                     gs_detail_bbl-DISCOUNTVALUE = '000000000000000'.      "Discount Value
                     gs_detail_bbl-DISCOUNTEXPIREDDATE = wa_itab_bbl-dis_expired. "Discount Expired date
                     gs_detail_bbl-ALLOCATIONNUMBER = wa_itab_bbl-allocation_no.  "Allocation Number
                     gs_detail_bbl-PAYMENTTERMS = wa_itab_bbl-payment_term.  "Payment Term
                     gs_detail_bbl-RECODEMODE = wa_itab_bbl-record_type.  "Record mode
                     gs_detail_bbl-USERDATA1 = wa_itab_bbl-user_data1.  "user_data1
                     gs_detail_bbl-USERDATA2 = wa_itab_bbl-user_data2.  "user_data2
                     gs_detail_bbl-USERDATA3 = wa_itab_bbl-user_data2.  "user_data2
                     gs_detail_bbl-USERDATA4 = ' '.  "user_data3
                     gs_detail_bbl-USERDATA5 = ' '.  "user_data4
                     gs_detail_bbl-USERDATA6 = ' '.  "user_data5
                     gs_detail_bbl-FILLER = ' '.


                       APPEND gs_detail_bbl TO gt_detail_bbl.


             gs_total_count = gs_total_count + 1.
             IF wa_itab_bbl-shkzg EQ 'S'.
                 gs_total_amt = gs_total_amt + wa_itab_bbl-dmbtr.
             ELSE.
                 gs_total_amt = gs_total_amt + ( wa_itab_bbl-dmbtr * -1 ).
             ENDIF.

      ENDLOOP.

                      UNPACK gs_total_count TO gs_count_doc.
                      gs_amt_temp = gs_total_amt * 100.
                      UNPACK gs_amt_temp TO gs_amt_c.
                      gs_footer_bbl-RECORDTYPE = 'T'.
                      gs_footer_bbl-TOTALDOCUMENTS = gs_count_doc.
                      gs_footer_bbl-TOTALOUTSTANDINGAMOUNT = gs_amt_c .
                      gs_footer_bbl-OUTSTANDINGAMOUNTFLAG = '1'.
                      gs_footer_bbl-FILLER = ' '.


                      APPEND gs_footer_bbl TO gt_footer_bbl.
                       CONCATENATE 'BBL_' SY-DATUM '_' SY-TIMLO '.txt' INTO GV_PATH.
                       GV_LEN = 450.

  PERFORM F_TEXT_EXPORT_SERVER_BBL.

ENDFORM.

**&---------------------------------------------------------------------*
**&      F_TEXT_BBL
**&---------------------------------------------------------------------*
FORM f_text_bbl_a30.
DATA: g_space TYPE string,
      l_pos type i.
      g_space = cl_abap_conv_in_ce=>uccp( '00a0' ).
DATA: lv_bbl_corporate_name(30) TYPE c,
      lv_bbl_corporate_code(6)  TYPe c,
      lv_to_bank(3) TYPE c,
      gs_date_send(12) TYPE c.

     CLEAR: wa_itab,wa_itab_bbl,gt_txt_bbl.
     CLEAR: gs_header_bbl,gt_header_bbl,gs_detail_bbl,gt_detail_bbl,gs_footer_bbl,gt_footer_bbl.

      CLEAR: gs_total_count,gs_total_amt.
              CLEAR: gs_txt_scb.
                      UNPACK gs_total_count TO gs_count_doc.
                      gs_amt_temp = gs_total_amt * 100.
                      UNPACK gs_amt_temp TO gs_amt_c.

                      CONCATENATE sy-datum sy-timlo+0(4) INTO gs_date_send.
                      gs_header_bbl-RECORDHEADER = 'H' .
                      gs_header_bbl-DATESEND = gs_date_send.
                      gs_header_bbl-CORPORATENAME = 'Siam Daikin Sales Co.,Ltd.'.
                      gs_header_bbl-CORPORATECODE = 'DAIKI1'.
                      gs_header_bbl-TOBANK = '002'.
                      gs_header_bbl-FILLER = ' '.
                      gs_header_bbl-ENDF = ' '.
                      APPEND gs_header_bbl TO gt_header_bbl.


      LOOP AT gt_itab_bbl INTO wa_itab_bbl WHERE zterm EQ 'C030'.

                     gs_detail_bbl-RECORDTYPE = 'D'. "Record Type
                     gs_detail_bbl-BILLER_CODE = 'DAIKI1'. "Corporate Code (Biller)
                     gs_detail_bbl-PAYEE_CODE = wa_itab_bbl-payee_code. "Payee_code
                     gs_detail_bbl-PAYER_CODE = wa_itab_bbl-cust_no. "Customer Code
                     gs_detail_bbl-DOCUMENTTYPE = wa_itab_bbl-doc_type. "Document Type
                     gs_detail_bbl-DOCUMENTNO = wa_itab_bbl-doc_no. "Document No
                     gs_detail_bbl-DOCUMENTDATE = wa_itab_bbl-doc_date.  "Document Date
                     gs_detail_bbl-DOCUMENTDUEDATE = wa_itab_bbl-due_date.  "Document due date
                     gs_detail_bbl-DOCUMENTAMOUNT = wa_itab_bbl-doc_amt.    "Document Amount
                     gs_detail_bbl-OUTSTANDINGAMOUNT = wa_itab_bbl-doc_amt.    "Outstanding Amount
                     gs_detail_bbl-DOCUMENTBASEAMOUNT = wa_itab_bbl-doc_base_amt.    "'000000000000000'      "Document Base Amount
                     gs_detail_bbl-BASESTATUSFLAG = '1'.                    "0' "Base Status Flag
                     gs_detail_bbl-WHTRATE = '00000'.                "WHT RATE
                     gs_detail_bbl-WHT1 = '0000000000'.               "WHT Amount1
                     gs_detail_bbl-WHT2 = '0000000000'.               "WHT Amount2
                     gs_detail_bbl-VATSTATUSFLAG = '1'.                    "VAT Status Flag
                     gs_detail_bbl-VATRATE = '00700' .                "VAT RATE
                     gs_detail_bbl-DISCOUNTTYPE = 'B'.                    "Discount Type
                     gs_detail_bbl-DISCOUNTVALUE = '000000000000000'.      "Discount Value
                     gs_detail_bbl-DISCOUNTEXPIREDDATE = wa_itab_bbl-dis_expired. "Discount Expired date
                     gs_detail_bbl-ALLOCATIONNUMBER = wa_itab_bbl-allocation_no.  "Allocation Number
                     gs_detail_bbl-PAYMENTTERMS = wa_itab_bbl-payment_term.  "Payment Term
                     gs_detail_bbl-RECODEMODE = wa_itab_bbl-record_type.  "Record mode
                     gs_detail_bbl-USERDATA1 = wa_itab_bbl-user_data1.  "user_data1
                     gs_detail_bbl-USERDATA2 = wa_itab_bbl-user_data2.  "user_data2
                     gs_detail_bbl-USERDATA3 = wa_itab_bbl-user_data2.  "user_data2
                     gs_detail_bbl-USERDATA4 = ' '.  "user_data3
                     gs_detail_bbl-USERDATA5 = ' '.  "user_data4
                     gs_detail_bbl-USERDATA6 = ' '.  "user_data5
                     gs_detail_bbl-FILLER = ' '.


                       APPEND gs_detail_bbl TO gt_detail_bbl.



             gs_total_count = gs_total_count + 1.
             IF wa_itab_bbl-shkzg EQ 'S'.
                 gs_total_amt = gs_total_amt + wa_itab_bbl-dmbtr.
             ELSE.
                 gs_total_amt = gs_total_amt + ( wa_itab_bbl-dmbtr * -1 ).
             ENDIF.

      ENDLOOP.


              CLEAR: gs_txt_scb.
                      UNPACK gs_total_count TO gs_count_doc.
                      gs_amt_temp = gs_total_amt * 100.
                      UNPACK gs_amt_temp TO gs_amt_c.

                      gs_footer_bbl-RECORDTYPE = 'T'.
                      gs_footer_bbl-TOTALDOCUMENTS = gs_count_doc.
                      gs_footer_bbl-TOTALOUTSTANDINGAMOUNT = gs_amt_c .
                      gs_footer_bbl-OUTSTANDINGAMOUNTFLAG = '1'.
                      gs_footer_bbl-FILLER = ' '.


                      APPEND gs_footer_bbl TO gt_footer_bbl.
              CONCATENATE 'BBL1_' SY-DATUM '_' SY-TIMLO '.txt' INTO GV_PATH.
              GV_LEN = 450.

   PERFORM F_TEXT_EXPORT_SERVER_BBL.

ENDFORM.
**&---------------------------------------------------------------------*
**&      F_TEXT_BBL
**&---------------------------------------------------------------------*
FORM f_text_scb.
DATA: g_space TYPE string,
      l_pos type i.
      g_space = cl_abap_conv_in_ce=>uccp( '00a0' ).
DATA: lv_scb_corporate_name(30) TYPE c,
      lv_scb_corporate_code(5)  TYPe c,
      lv_to_bank(10) TYPE c,
      gs_date_send(12) TYPE c.

     CLEAR: wa_itab,wa_itab_scb,gs_total_amt,gs_total_count.
     CLEAR: gs_header_scb,gt_header_scb,gs_detail_scb,gt_detail_scb,gs_footer_scb,gt_footer_scb.

      CLEAR: gs_total_count,gs_total_amt.
              CLEAR: gs_text_scb.
                      UNPACK gs_total_count TO gs_count_doc.
                      gs_amt_temp = gs_total_amt * 100.
                      UNPACK gs_amt_temp TO gs_amt_c.

                      CONCATENATE sy-datum sy-timlo+0(4) INTO gs_date_send.
                      gs_header_scb-RECORDHEADER = 'H'.
                      gs_header_scb-DATESEND = gs_date_send.
                      gs_header_scb-CORPORATENAME = 'Siam Daikin Sales Co.,Ltd.'.
                      gs_header_scb-CORPORATECODE = 'BS105'.
                      gs_header_scb-TOBANK = 'SCB'.
                      gs_header_scb-DOWNLOADFROMDATE = ' '.
                      gs_header_scb-DOWNLOADTODATE = ' '.
                      gs_header_scb-FILLER = ' '.
                      gs_header_scb-ENDF = ' '.

                      APPEND gs_header_scb TO gt_header_scb.


      LOOP AT gt_itab_scb INTO wa_itab_scb.
           gs_detail_scb-RECORDTYPE = 'D'.
                       gs_detail_scb-CORPORATECODE = 'BS105'.  "Corporate Code (Biller)
                       gs_detail_scb-CUSTOMERCODE = wa_itab_scb-cust_no.   "Customer Code
                       gs_detail_scb-DOCUMENTTYPE = wa_itab_scb-doc_type.  "Document Type
                       gs_detail_scb-INVOICENUMBER = wa_itab_scb-doc_no .   "Invoice No
                       gs_detail_scb-INVOICEDATE = wa_itab_scb-doc_date.  "Invoice Date
                       gs_detail_scb-DUEDATE = wa_itab_scb-due_date.  "due date
                       gs_detail_scb-INVOICEAMOUNT = wa_itab_scb-doc_amt .   "Invoice Amount
                       gs_detail_scb-OUTSTANDINGAMOUNT = wa_itab_scb-doc_amt .   "Outstanding Amount
                       gs_detail_scb-ALLOCATIONNUMBER = wa_itab_scb-allocation_num .    "Allocation Number
                       gs_detail_scb-REFERENCE = wa_itab_scb-reference2 .    "Reference
                       gs_detail_scb-INVOICESTATUSFLAG = wa_itab_scb-inv_flag .   "'0'                    "Invoice Status Flag  "Edit by Wantanee 20160215
                       gs_detail_scb-BASEAMOUNT = wa_itab_scb-doc_base_amt.     "Base Amount
                       gs_detail_scb-VATRATE = wa_itab_scb-vat_rate.    "VAT RATE
                       gs_detail_scb-BANKCODE = '014'   .            "Bank Code
                       gs_detail_scb-PAYMENTTERMS = wa_itab_scb-payment_term .   "Payment Term
                       gs_detail_scb-SUBCOMPANYCODE = wa_itab_scb-sub_com  .  "Sub Company Code
                       gs_detail_scb-RECORDMODE = wa_itab_scb-record_type.  "Record mode
                       gs_detail_scb-PAYMENTAMOUNT = ' '.
                       gs_detail_scb-REASONCODE =  ' '.
                       gs_detail_scb-BANKPAYMENTREFERENCE = ' '.
                       gs_detail_scb-PAYMENTTRANSACTIONDATE = ' '.
                       gs_detail_scb-PAYMENTTRANSACTIONTIME = ' '.
                       gs_detail_scb-DEBITAC = ' '.
                       gs_detail_scb-DISCOUNTAMOUNT = ' '.
                       gs_detail_scb-FILLER =' '.

             gs_total_count = gs_total_count + 1.
             IF wa_itab_scb-shkzg EQ 'S'.
                 gs_total_amt = gs_total_amt + wa_itab_scb-dmbtr.
             ELSE.
                 gs_total_amt = gs_total_amt + ( wa_itab_scb-dmbtr * -1 ).
             ENDIF.

              APPEND gs_detail_scb TO gt_detail_scb.

      ENDLOOP.


                      UNPACK gs_total_count TO gs_count_doc.
                      gs_amt_temp = gs_total_amt * 100.
                      UNPACK gs_amt_temp TO gs_amt_c.


                      gs_footer_scb-RECORDTYPE =  'T'.  "Footer Section
                      gs_footer_scb-DATESEND = gs_date_send.  "Date Send
                      gs_footer_scb-CORPORATENAME = 'Siam Daikin Sales Co.,Ltd.'.     "Corporate Name
                      gs_footer_scb-CORPORATECODE = 'BS105'.          "Corporate Code
                      gs_footer_scb-TOBANK = 'SCB'.       "To bank
                      gs_footer_scb-TOTALINVOICENUMBER = gs_count_doc.  "Total Document
                      gs_footer_scb-TOTALOUTSTANDINGINVOICEAMOUNT = gs_amt_c.     "Total Outstanding Amount
                      gs_footer_scb-FILLER = ' '.

                      APPEND gs_footer_scb TO gt_footer_scb.

              GV_PATH = 'SCB.txt'.
              GV_LEN = 260.


   PERFORM F_TEXT_EXPORT_SERVER_SCB.


ENDFORM.

**&---------------------------------------------------------------------*
**&      F_TEXT_KBANK
**&---------------------------------------------------------------------*
FORM f_text_kbank.
DATA: g_space TYPE string,
      l_pos type i.
      g_space = cl_abap_conv_in_ce=>uccp( '00a0' ).
DATA: lv_kbank_corporate_name(30) TYPE c,
      lv_kbank_corporate_code(5)  TYPe c,
      lv_to_bank(3) TYPE c,
      gs_date_send(8) TYPE c,
      lv_time_send(6) TYPE c,
      gs_total_amt_flag TYPE c.


     CLEAR: wa_itab,wa_itab_scb,gs_total_amt,gs_total_count,gs_total_amt_flag.


      CLEAR: gs_total_count,gs_total_amt.
              CLEAR: gs_text_kbank.
                      UNPACK gs_total_count TO gs_count_doc.
                      gs_amt_temp = gs_total_amt * 100.
                      UNPACK gs_amt_temp TO gs_amt_c.
                      gs_date_send = sy-datum.
                      lv_time_send = sy-timlo.
                      lv_kbank_corporate_name = 'Siam Daikin Sales Co.,Ltd.'.
                      lv_kbank_corporate_code = 'SIA'.
                      lv_to_bank = '004'.
                      CONCATENATE 'H'           "Record TYPE
                                  gs_date_send       "Date send
                                  lv_time_send    "Send Time
                                  lv_kbank_corporate_name     "Corporate Name
                                  lv_kbank_corporate_code          "Corporate Code
                                  lv_to_bank          "To bank

                      INTO gs_text_kbank RESPECTING BLANKS.
                      L_pos = strlen( gs_text_kbank ).
                      WHILE L_pos < 300.

                          gs_text_kbank+l_pos(1) = g_space.
                          l_pos = l_pos + 1.
                      ENDWHILE.
                      INSERT gs_text_kbank INTO gt_txt_kbank INDEX 1.

      LOOP AT gt_itab_kbank INTO wa_itab_kbank.
           CLEAR: gs_text_kbank.
           CONCATENATE 'D'  "Record Type
                       lv_kbank_corporate_code  "Corporate Code (Biller)
                       wa_itab_kbank-cust_no   "Customer Code
                       wa_itab_kbank-doc_type  "Document Type
                       wa_itab_kbank-doc_no    "Document No
                       wa_itab_kbank-matching_refer    "Matching Reference
                       wa_itab_kbank-doc_date  "Document Date
                       wa_itab_kbank-due_date  "Document due date
                       wa_itab_kbank-doc_amt    "Document Amount
                       wa_itab_kbank-doc_flag    "Document Amount flag
                       wa_itab_kbank-doc_amt    "Outstanding Amount
                       wa_itab_kbank-doc_flag    "Outstanding Amount flag
                       wa_itab_kbank-profit    "Profit center
                       wa_itab_kbank-record_type  "Record mode
                       wa_itab_kbank-inv_flag    "'0'                    "Invoice Status Flag  "Edit by Wantanee 20160215
                       wa_itab_kbank-payment_term
                       INTO gs_text_kbank RESPECTING BLANKS.
                        l_pos = strlen( gs_text_kbank ).
                        WHILE l_pos < 300.

                            gs_text_kbank+l_pos(1) = g_space.
                            l_pos = l_pos + 1.
                        ENDWHILE.
                       APPEND gs_text_kbank TO gt_txt_kbank.

             gs_total_count = gs_total_count + 1.
             IF wa_itab_kbank-shkzg EQ 'S'.
                 gs_total_amt = gs_total_amt + wa_itab_kbank-dmbtr.
             ELSE.
                 gs_total_amt = gs_total_amt + ( wa_itab_kbank-dmbtr * -1 ).
             ENDIF.
             IF gs_total_amt LT 0 .

             ENDIF.

      ENDLOOP.


              CLEAR: gs_text_kbank.
                      UNPACK gs_total_count TO gs_count_doc.
                      gs_amt_temp = gs_total_amt * 100.
                      UNPACK gs_amt_temp TO gs_amt_c.

                      IF gs_total_amt LT 0 .
                         gs_total_amt_flag = '0'.
                      ELSE.
                         gs_total_amt_flag = '1'.
                      ENDIF.


                      CONCATENATE 'F'  "Footer Section
                                  gs_date_send       "Date send
                                  lv_time_send    "Send Time
                                  lv_kbank_corporate_name      "Corporate Name
                                  lv_kbank_corporate_code           "Corporate Code
                                  lv_to_bank         "To bank
                                  gs_count_doc  "Total Document Number
                                  gs_amt_c      "Total Document Amount
                                  gs_total_amt_flag  "Total Document Amount flag
                                  gs_amt_c      "Total Outstanding Amount
                                  gs_total_amt_flag  "Total Outstanding Amount flag

                      INTO gs_text_kbank RESPECTING BLANKS.
                        l_pos = strlen( gs_text_kbank ).
                        WHILE l_pos < 300.

                            gs_text_kbank+l_pos(1) = g_space.
                            l_pos = l_pos + 1.
                        ENDWHILE.
                      APPEND gs_text_kbank TO gt_txt_kbank.




ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  CUSTOMER
**&---------------------------------------------------------------------*
FORM get_customer USING    p_kunnr TYPE kna1-kunnr
                           p_adrnr TYPE kna1-adrnr
                  CHANGING VALUE(p_name_tha) TYPE c
                           VALUE(p_name_eng) TYPE c.
*                           VALUE(p_street) TYPE c
*                           VALUE(p_str_suppl3) TYPE c
*                           VALUE(p_location) TYPE c
*                           VALUE(p_city2) TYPE adrc-city2
*                           VALUE(p_city1) TYPE adrc-city1
*                           VALUE(p_street_en) TYPE c
*                           VALUE(p_str_suppl3_en) TYPE c
*                           VALUE(p_location_en) TYPE c
*                           VALUE(p_city2_en) TYPE adrc-city2
*                           VALUE(p_city1_en) TYPE adrc-city1
*                           VALUE(p_post_code1) TYPE adrc-post_code1
*                           VALUE(p_tel_number) TYPE adrc-tel_number
*                           VALUE(p_fax_number) TYPE adrc-fax_number.


   DATA: lv_adrnr TYPE kna1-adrnr.


     CLEAR:p_name_tha,p_name_eng.
*           ,p_name_eng,p_street,p_str_suppl3,p_location,p_city2,
*           p_city1,
*           p_street_en,p_str_suppl3_en,p_location_en,p_city2_en,
*           p_city1_en,
*           p_post_code1,p_tel_number.

     lv_adrnr =  p_adrnr.

     IF p_adrnr EQ ''.
        SELECT SINGLE adrnr
          INTO (lv_adrnr)
          FROM kna1
        WHERE kunnr EQ p_kunnr.

     ENDIF.


          SELECT addrnumber name1 name2 street str_suppl3
                 location city2 city1 post_code1 tel_number
                 fax_number nation
          INTO TABLE gt_adrc
          FROM adrc
          WHERE addrnumber = lv_adrnr.
*            AND nation = 'I'.

          IF p_kunnr NE 'OT01'.
              READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
                                                       nation = 'I'.
                IF sy-subrc EQ 0.
                    CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_eng.
*                    p_street_en = gs_adrc-street.
*                    p_str_suppl3_en = gs_adrc-str_suppl3.
*                    p_location_en = gs_adrc-location.
*                    p_city2_en = gs_adrc-city2.
*                    p_city1_en = gs_adrc-city1.



                ENDIF.
              READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
                                                       nation = ''.
                IF sy-subrc EQ 0.
                    CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_tha.
*                    p_street = gs_adrc-street.
*                    p_str_suppl3 = gs_adrc-str_suppl3.
*                    p_location = gs_adrc-location.
*                    p_city2 = gs_adrc-city2.
*                    p_city1 = gs_adrc-city1.
*                    p_post_code1 = gs_adrc-post_code1.
*                    p_tel_number = gs_adrc-tel_number.
*                    p_fax_number = gs_adrc-fax_number.

                ENDIF.
          ELSE.


           READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = lv_adrnr
                                                    nation = ''.
                 IF sy-subrc EQ 0.
                    CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_tha.
*                    CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_eng.
*                    p_street = gs_adrc-street.
*                    p_str_suppl3 = gs_adrc-str_suppl3.
*                    p_location = gs_adrc-location.
*                    p_city2 = gs_adrc-city2.
*                    p_city1 = gs_adrc-city1.
*                    p_post_code1 = gs_adrc-post_code1.
*                    p_tel_number = gs_adrc-tel_number.
*                    p_fax_number = gs_adrc-fax_number.
                 ENDIF.
          ENDIF.



ENDFORM. "


*&-----------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPROT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_reprot .
  PERFORM build_layout.
  PERFORM build_catalog.
  PERFORM build_event USING gt_events[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gc_repid
*      i_callback_user_command = 'USER_COMMAND'
      i_save             = 'A'
*      is_layout          = gt_layout
      it_events          = gt_events[]
      it_fieldcat        = gt_fieldcat

    TABLES
      t_outtab           = gt_itab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DISPLAY_REPROT



*&-----------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPROT_Cust
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_reprot_cust .
  PERFORM build_layout.
  PERFORM build_catalog.
  PERFORM build_event USING gt_events[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gc_repid
*      i_callback_user_command = 'USER_COMMAND'
      i_save             = 'A'
*      is_layout          = gt_layout
      it_events          = gt_events[]
      it_fieldcat        = gt_fieldcat

    TABLES
      t_outtab           = gt_itab_cust
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DISPLAY_REPROT

* Add by K 20110215
*---------------------------------------------------------------------*
*       FORM User_command
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  IF r_ucomm = '&IC1'.  "Double click event

*   goto XD03 display Customer Master

*     READ TABLE gt_itab INTO wa_itab INDEX rs_selfield-tabindex.
*
*              SET: PARAMETER ID 'KUN'  FIELD wa_itab-kunnr,
*                      PARAMETER ID 'BUK'  FIELD p_bukrs  .
*              CALL TRANSACTION 'XD03'.

 ENDIF.

 CLEAR r_ucomm.

ENDFORM.                    "user_comman

* End 20110215

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .
  gt_layout-window_titlebar = sy-title.
  gt_layout-colwidth_optimize = 'X'.
  gt_layout-zebra = 'X'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_catalog .

CLEAR : v_pos.

IF NOT p_disp IS INITIAL.

*       Customer Code
        PERFORM append_fieldcat USING 'KUNNR'
                                      ''
                                      ''
                                      'Customer Code'
                                       space  space  space
                                       gt_fieldcat[].

*       Customer Name
        PERFORM append_fieldcat USING 'NAME'
                                      ''
                                      ''
                                      'Customer Name'
                                       space  space  space
                                       gt_fieldcat[].
*       Document Type
         PERFORM append_fieldcat USING 'DOC_TYPE'
                                      ''
                                      ''
                                      'Document Type'
                                       space  space  space
                                       gt_fieldcat[].

*       Document Type detail
        PERFORM append_fieldcat USING 'DOC_TYPE_C'
                                      ''
                                      ''
                                      'Document Type detail'
                                       space  space  space
                                       gt_fieldcat[].

*      Document No
        PERFORM append_fieldcat USING 'BELNR'
                                      ''
                                      ''
                                      'Document No.'
                                       space  space  space
                                       gt_fieldcat[].
*       Document Date
        PERFORM append_fieldcat USING 'BUDAT'
                                      ''
                                      ''
                                      'Document Date'
                                       space  space  space
                                       gt_fieldcat[].
*       Document Date
        PERFORM append_fieldcat USING 'DUE_DATE'
                                      ''
                                      ''
                                      'Due Date'
                                       space  space  space
                                       gt_fieldcat[].
*       Amount
        PERFORM append_fieldcat USING 'DMBTR_S'
                                      ''
                                      ''
                                      'Amount'
                                       space  space  space
                                       gt_fieldcat[].
*       Profit Center
        PERFORM append_fieldcat USING 'PRCTR'
                                      ''
                                      ''
                                      'Profit Center'
                                       space  space  space
                                       gt_fieldcat[].
*       Base Amount
        PERFORM append_fieldcat USING 'BASE_AMT_S'
                                      ''
                                      ''
                                      'Base Amount'
                                       space  space  space
                                       gt_fieldcat[].
*       VAT Amount
        PERFORM append_fieldcat USING 'MWSBP_S'
                                      ''
                                      ''
                                      'VAT'
                                       space  space  space
                                       gt_fieldcat[].

*       VAT Rate
        PERFORM append_fieldcat USING 'VAT_RATE_S'
                                      ''
                                      ''
                                      'VAT RATE'
                                       space  space  space
                                       gt_fieldcat[].
*      * VAT TYPE
*        PERFORM append_fieldcat USING 'VAT_TYPE'
*                                      ''
*                                      ''
*                                      'VAT TYPE'
*                                       space  space  space
*                                       gt_fieldcat[].
*       Bank Code
        PERFORM append_fieldcat USING 'BANK_CODE'
                                      ''
                                      ''
                                      'Bank Code'
                                       space  space  space
                                       gt_fieldcat[].
*       Bank Name
        PERFORM append_fieldcat USING 'B_NAME_S'
                                      ''
                                      ''
                                      'Bank Name'
                                       space  space  space
                                       gt_fieldcat[].
*       Payment Term
        PERFORM append_fieldcat USING 'ZTERM'
                                      ''
                                      ''
                                      'Payment Term'
                                       space  space  space
                                       gt_fieldcat[].
  ELSEIF NOT p_cust IS INITIAL.


*       Customer Code
        PERFORM append_fieldcat USING 'KUNNR'
                                      ''
                                      ''
                                      'Customer Code'
                                       space  space  space
                                       gt_fieldcat[].

*       Customer Name thai
        PERFORM append_fieldcat USING 'CUST_NAME_TH'
                                      ''
                                      ''
                                      'Customer Name thai'
                                       space  space  space
                                       gt_fieldcat[].
*       Customer Name Eng
        PERFORM append_fieldcat USING 'CUST_NAME_EN'
                                      ''
                                      ''
                                      'Customer Name eng'
                                       space  space  space
                                       gt_fieldcat[].

*       Bank Code
        PERFORM append_fieldcat USING 'BANK_CODE'
                                      ''
                                      ''
                                      'Bank Code'
                                       space  space  space
                                       gt_fieldcat[].
*       Bank Name
        PERFORM append_fieldcat USING 'B_NAME_S'
                                      ''
                                      ''
                                      'Bank Name'
                                       space  space  space
                                       gt_fieldcat[].

  ENDIF.


 ENDFORM.             "BUILD_ATALOG

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

FORM append_fieldcat USING   p_field   "Field name
                             p_reftable"Reference Table name
                             p_reffield"Reference Field name
                             p_coltxt  "Col Text(for specify)
                             p_dosum   "Sum total
                             p_cfieldname  "  currency
                             p_no_zero     " no zero
                             p_it_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: wa_infieldcat TYPE slis_fieldcat_alv,
        v_coltxt_length TYPE i.

  ADD 1 TO v_pos.

  wa_infieldcat-fieldname     = p_field.
  wa_infieldcat-ref_tabname   = p_reftable.
  wa_infieldcat-ref_fieldname = p_reffield.
  wa_infieldcat-col_pos       = v_pos .
  wa_infieldcat-do_sum        = p_dosum.

  IF NOT p_no_zero IS INITIAL .
    wa_infieldcat-no_zero = p_no_zero.
  ENDIF.
  IF NOT p_cfieldname IS INITIAL .
    wa_infieldcat-cfieldname = p_cfieldname .
  ENDIF.


*If we need to specify text ,don't need to derive from data dictionary
*program will check length and define width of the colum
  IF NOT p_coltxt IS INITIAL.
    v_coltxt_length = STRLEN( p_coltxt ).

    IF v_coltxt_length > 20.
      wa_infieldcat-ddictxt = 'L'."Long text
      wa_infieldcat-seltext_l = p_coltxt.
    ELSEIF v_coltxt_length > 10.
      wa_infieldcat-ddictxt = 'M'."Medium Text
      wa_infieldcat-seltext_m = p_coltxt.
    ELSE.
      wa_infieldcat-ddictxt = 'S'."Short Text
      wa_infieldcat-seltext_s = p_coltxt.
    ENDIF.
    wa_infieldcat-reptext_ddic = p_coltxt  .
  ENDIF.
  APPEND wa_infieldcat TO p_it_fieldcat.
ENDFORM.                    " APPEND_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM build_event  USING e03_lt_events TYPE slis_t_event.
  DATA: lw_event TYPE slis_alv_event.
* Read Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 1
    IMPORTING
      et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_page
                           INTO lw_event.
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_gs_list
*                             INTO lw_event.
  IF sy-subrc = 0.
* register top of page event
    MOVE gc_top_of_page TO lw_event-form.
    APPEND lw_event TO e03_lt_events.
  ENDIF.
ENDFORM.                    " BUILD_EVENT

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.
  PERFORM write_heading.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_heading .
  DATA: t_header      TYPE   slis_t_listheader,
        wa_header     TYPE   slis_listheader.

  wa_header-typ  = 'S'.
  wa_header-key = 'Report Name : '.
  wa_header-info = 'EPP Account'.
  APPEND wa_header TO t_header.
  CLEAR wa_header.


  wa_header-typ  = 'S'.
  wa_header-key = 'Report Date : '.
  CONCATENATE  sy-datum+6(2) '.'
               sy-datum+4(2) '.'
               sy-datum(4) INTO wa_header-info.   "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.

  wa_header-typ  = 'S'.
  wa_header-key = 'Report Time : '.
  WRITE: sy-uzeit TO wa_header-info.    "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.

ENDFORM.                    " WRITE_HEADING

FORM export USING pa_path
                  lt_data_tab TYPE truxs_t_text_data.

*
*CALL FUNCTION 'WS_DOWNLOAD'
* EXPORTING
**   BIN_FILESIZE                  = ' '
*   codepage                      = '8600'
*   filename                      = p_path
*   filetype                      = 'DAT'
*   mode                          = 'O'
**   WK1_N_FORMAT                  = ' '
**   WK1_N_SIZE                    = ' '
**   WK1_T_FORMAT                  = ' '
**   WK1_T_SIZE                    = ' '
**   COL_SELECT                    = ' '
**   COL_SELECTMASK                = ' '
**   NO_AUTH_CHECK                 = ' '
** IMPORTING
**   FILELENGTH                    = FILELENGTH
*  tables
*    data_tab                      = lt_data_tab
**   FIELDNAMES                    = FIELDNAMES
** EXCEPTIONS
**   FILE_OPEN_ERROR               = 1
**   FILE_WRITE_ERROR              = 2
**   INVALID_FILESIZE              = 3
**   INVALID_TYPE                  = 4
**   NO_BATCH                      = 5
**   UNKNOWN_ERROR                 = 6
**   INVALID_TABLE_WIDTH           = 7
**   GUI_REFUSE_FILETRANSFER       = 8
**   CUSTOMER_ERROR                = 9
**   NO_AUTHORITY                  = 10
*          .
*  if sy-subrc <> 0.
*    message e001(z_bc) with 'WS_DOWNLOAD' sy-subrc p_path.
*  else.
*    message s430(ds).
*  endif.
*

call function 'GUI_DOWNLOAD'
    exporting
      filename                = pa_path
      filetype                = 'ASC'
      write_field_separator   = 'X'
      trunc_trailing_blanks   = 'X'
    tables
*      data_tab                = <fs_table>
      data_tab                = lt_data_tab
*      fieldnames              = lt_data_tab
    exceptions
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      others                  = 22.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM get_path_name  CHANGING path.
  DATA: l_length TYPE i.
  DATA: l_mask(20) TYPE c.

* S = Save, O = Open
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

                .


ENDFORM.                    " GET_PATH_NAME



*&---------------------------------------------------------------------*
*&      Form  PF_DIRECTORY_BROWSE
*&---------------------------------------------------------------------*
FORM pf_directory_browse  USING  lv_path.
*                                 lv_filename.

  DATA: lv_temp TYPE string.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
*    window_title         =
      initial_folder       = 'C:'
    CHANGING
      selected_folder      = lv_temp
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*    CONCATENATE  lv_temp
**                 lv_filename
*                INTO lv_path SEPARATED BY '\'.
    lv_path = lv_temp.

  ENDIF.

ENDFORM.                    " PF_DIRECTORY_BROWSE
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
      CONCATENATE p_value  gs_lines-tdline INTO p_value.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Module  SET_INIT_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_INIT_LISTBOX OUTPUT.

  REFRESH  gs_list.
  value-key = '002'.
  value-text = 'BBL'.
  APPEND value TO gs_list.
  value-key = '014'.
  value-text = 'SCB'.
  APPEND value TO gs_list.
  value-key = '004'.
  value-text = 'KBANK'.
  APPEND value TO gs_list.
  gs_lstbox_name =  'GS_INST_UP_CUST_BANK-BANK_KEY'.
  CALL FUNCTION  'VRM_SET_VALUES'
    EXPORTING
      id     = gs_lstbox_name
      values = gs_list.

ENDMODULE.                 " SET_INIT_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
 case sy-ucomm.
    when 'EXIT'.
      leave to screen 0.
    when 'BACK'.
      leave to screen 0.
    when 'CANCEL'.
      leave to screen 0.
    when 'DEL'.
        break wantanee.
* remove marked lines
       loop at gt_inst_up_cust_bank into gs_inst_up_cust_bank1 where mark = 'X'.
         CLEAR : gs_del.
           gs_del-mandt = sy-mandt.
           gs_del-kunnr = gs_inst_up_cust_bank1-kunnr.
           gs_del-bank_code = gs_inst_up_cust_bank1-bank_key.
           APPEND gs_del TO gt_del.
*        delete gt_inst_up_cust_bank where mark = 'X'.
           delete gt_inst_up_cust_bank where kunnr = gs_inst_up_cust_bank1-kunnr AND mark = 'X'.
      endloop.
      if sy-subrc <> 0.
        get cursor field fld line linno offset off.
        set cursor field fld line linno offset off.
        if fld cp 'GW_INST_UP_CUST_BANK*' and sy-subrc = 0.
          linno = linno + tc_100-top_line - 1.
*          READ TABLE gt_inst_up_cust_bank INTO gs_inst_up_cust_bank1 INDEX linno.


          delete gt_inst_up_cust_bank index linno.


          tc_100-lines = tc_100-lines - 1.
        endif.
      endif.
    when 'ADD'.
         gs_add = 'X'.
         REFRESH gt_inst_up_cust_bank.

    when 'SAVE'.
*      break 3sds006.
      sort gt_inst_up_cust_bank by kunnr .
      IF gt_del IS INITIAL.
          if  not gt_inst_up_cust_bank is initial.

              loop at gt_inst_up_cust_bank into gs_inst_up_cust_bank1.
                     move-corresponding gs_inst_up_cust_bank1 to gs_upsert.
                     gs_upsert-bank_code = gs_inst_up_cust_bank1-bank_key.
                     LOOP AT gs_list INTO value WHERE key EQ gs_inst_up_cust_bank1-bank_key.
                          gs_upsert-b_name_s = value-text.
                     ENDLOOP.
                     call function 'CONVERSION_EXIT_ALPHA_INPUT'
                       exporting
                         input  = gs_upsert-kunnr
                       importing
                         output = gs_upsert-kunnr.
                         gs_upsert-mandt = sy-mandt.
                IF gs_add EQ 'X'.
                          INSERT INTO ZSDSFIC022
                          VALUES gs_upsert.
                          COMMIT WORK.
                ELSE.
                    IF gs_inst_up_cust_bank1-up_flag EQ 'X'.
                         UPDATE ZSDSFIC022
                         SET bank_code = gs_upsert-bank_code
                             b_name_s  = gs_upsert-b_name_s
                         WHERE kunnr = gs_upsert-kunnr.
                         COMMIT WORK.
                    ENDIF.
                 ENDIF.



              endloop.
                 message s000(38) with 'Insert/Update Table ZSDSFIC022 Complete'.
            endif.
      ELSE.
          LOOP AT gt_del INTO gs_del.
              DELETE FROM ZSDSFIC022
              WHERE kunnr = gs_del-kunnr
              AND   bank_code = gs_del-bank_code.
              COMMIT WORK.
          ENDLOOP.
           message s000(38) with 'Insert/Update/Delete Table ZSDSFIC022 Complete'.
      ENDIF.



  endcase.
  clear : sy-ucomm.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_100_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_100_GET_LINES OUTPUT.
 gs_tc_100_lines = sy-loopc.
ENDMODULE.                 " TC_100_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_TC_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_TC_100 OUTPUT.
*  if r_disp is not initial.
*    loop at screen.
*      screen-input = 0.
*      modify screen.
*    endloop.
*  endif.
ENDMODULE.                 " MODIFY_TC_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_100_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_100_CHANGE_TC_ATTR OUTPUT.
  tc_100-lines = fill.
ENDMODULE.                 " TC_100_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '100'.
  tc_100-lines = fill.
  set titlebar '100' with 'Insert Data'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_100_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_100_MODIFY INPUT.

  data: lv_kunnr type kna1-kunnr.
  DATA: lv_name_eng(200) TYPE c.

  clear: lv_kunnr,gs_inst_up_cust_bank1,lv_name_eng.
  gs_linecount = lines( gt_inst_up_cust_bank ).
 if gs_inst_up_cust_bank-kunnr ne ''.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = gs_inst_up_cust_bank-kunnr
      importing
        output = lv_kunnr.

    gs_inst_up_cust_bank-kunnr = lv_kunnr.
  endif.

  IF tc_100-current_line gt gs_linecount.
        fill = fill + 1.
    IF gs_inst_up_cust_bank-kunnr NE '' .

       perform get_customer using gs_inst_up_cust_bank-kunnr '' changing gs_inst_up_cust_bank-cust_name lv_name_eng.
*       gs_inst_up_cust_bank-bank_key =  gs_inst_up_cust_bank-bank_key.
       append gs_inst_up_cust_bank to gt_inst_up_cust_bank.
    ENDIF.
  ELSE.

       gs_inst_up_cust_bank-up_flag = 'X'.
       modify gt_inst_up_cust_bank
       from gs_inst_up_cust_bank
       index tc_100-current_line.
  ENDIF.



ENDMODULE.                 " TC_100_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_HELP_FILE_CONDI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module f4_help_bank_key input.
 BREAK-POINT.
 REFRESH  gs_list.
  value-key = '002'.
  value-text = 'BBL'.
  APPEND value TO gs_list.
  value-key = '014'.
  value-text = 'SCB'.
  APPEND value TO gs_list.
  value-key = '004'.
  value-text = 'KBANK'.
  APPEND value TO gs_list.
  gs_lstbox_name =  'gs_inst_up_cust_bank-bank_key'.
  CALL FUNCTION  'VRM_SET_VALUES'
    EXPORTING
      id     = gs_lstbox_name
      values = gs_list.

endmodule.                 " F4_HELP_FILE_CONDI  INPUT
**&---------------------------------------------------------------------*
**&      F_TEXT_Export
**&---------------------------------------------------------------------*
FORM F_TEXT_EXPORT_SERVER_BBL.
  DATA: G_SPACE TYPE STRING,
        L_POS   TYPE I.
  G_SPACE = CL_ABAP_CONV_IN_CE=>UCCP( '00a0' ).
  DATA: LV_SCB_CORPORATE_NAME(30) TYPE C,
        LV_SCB_CORPORATE_CODE(5)  TYPE C,
        LV_TO_BANK(10)            TYPE C,
        LV_DATE_SEND(12)          TYPE C.

*  BREAK-POINT.

*  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_ITEM      = GT_DELI_WMS_TXT "gt_export_txt
*                                                                 I_FIX_LEN   = 'X'
*                                                                 I_LEN       = 94 ).
**                                                             I_SEPARATOR = '","'
**                                                             I_START_END_VALUE = '"').

  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING  I_HEADER = gt_header_bbl
                                                                 I_ITEM      = gt_detail_bbl "gt_export_txt
                                                                 I_FOOTER      = gt_footer_bbl "gt_export_txt
                                                                 I_FIX_LEN   = 'X'
                                                                 I_LEN       = GV_LEN ).
*                                                             I_SEPARATOR = '","'
*                                                             I_START_END_VALUE = '"').

  PERFORM F_EXPORT_TO_SERVER.


ENDFORM.
**&---------------------------------------------------------------------*
**&      F_TEXT_Export
**&---------------------------------------------------------------------*
FORM F_TEXT_EXPORT_SERVER_SCB.
  DATA: G_SPACE TYPE STRING,
        L_POS   TYPE I.
  G_SPACE = CL_ABAP_CONV_IN_CE=>UCCP( '00a0' ).
  DATA: LV_SCB_CORPORATE_NAME(30) TYPE C,
        LV_SCB_CORPORATE_CODE(5)  TYPE C,
        LV_TO_BANK(10)            TYPE C,
        LV_DATE_SEND(12)          TYPE C.

*  BREAK-POINT.

*  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_ITEM      = GT_DELI_WMS_TXT "gt_export_txt
*                                                                 I_FIX_LEN   = 'X'
*                                                                 I_LEN       = 94 ).
**                                                             I_SEPARATOR = '","'
**                                                             I_START_END_VALUE = '"').

  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING  I_HEADER = gt_header_scb
                                                                 I_ITEM      = gt_detail_scb "gt_export_txt
                                                                 I_FOOTER      = gt_footer_scb "gt_export_txt
                                                                 I_FIX_LEN   = 'X'
                                                                 I_LEN       = GV_LEN ).
*                                                             I_SEPARATOR = '","'
*                                                             I_START_END_VALUE = '"').

  PERFORM F_EXPORT_TO_SERVER.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_export_to_server
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXPORT_TO_SERVER.
  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.
  DATA: LV_PATH_FILE TYPE STRING.



  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.

*        LS_FILE = 'Hello Wold'.
*        APPEND LS_FILE TO LT_FILE.
*        LS_FILE = 'Hello Wold1'.
*        APPEND LS_FILE TO LT_FILE.
  LV_PATH_FILE = P_PATH.

  LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
                                  I_AL11_PATH   = LV_PATH_FILE
                                 I_FILE_NAME   = GV_PATH "'TEST_FTP_FILE.txt'
                                " I_USER        = 'ds'
                                 "I_PASS        = 'ds=20240521'
                                " I_IP          = '172.31.136.250'
                                " I_PORT        = '21'
                                  I_DATA_SPIDER = 'X'
                                 IT_DATA       = CT_RESULT ).

*  LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'MY_DAIKIN/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
*                                     I_USER        = 'ds'
*                                     I_PASS        = 'ds=20240521'
*                                     I_IP          = '172.31.136.250'
*                                     I_PORT        = '21'
*                                     IT_DATA       = CT_RESULT ).
  IF LV_STATUS EQ 'S'.
    " SUCCESS FTP FILE
  ELSE.
    " CANNOT FTP FILE
  ENDIF.
ENDFORM.                 " F_GET_RESULT
