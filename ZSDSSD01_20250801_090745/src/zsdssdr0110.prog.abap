*&---------------------------------------------------------------------*
*& Report ZSDSSDR0110
*  Creation Date      : 18.06.2024
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZSDI021
*  Description        : Report and Export interface Invoice and CN to SF (Shipped)
*  Purpose            :
*  Copied from        : ZR_SFDC_SHIPPED
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSSDR0110.

*&-----------------------------------------------------------------------------------*
*& T A B L E S
*&-----------------------------------------------------------------------------------*
TABLES: vbrk,vbrp,mara.
type-pools : truxs.

*&-----------------------------------------------------------------------------------*
*& D A T A
*&-----------------------------------------------------------------------------------*


TYPES: BEGIN OF gy_out,

     shipped_uid(100)    TYPE c,   "Shipped UID
     comp(10)            TYPE c,   "Company
     branch(10)          TYPE c,   "Branch
     vtweg               TYPE vbrk-vtweg,  "Distribute Channel
     vkbur               TYPE vbrp-vkbur,  "Sales Office
     vkgrp               TYPE vbrp-vkgrp,  "Sales Group
     emp_name_en(100)    TYPE c,     "Sale man
     kunnr               TYPE kna1-kunnr,  "Customer ID
     acc_name_th(255)    TYPE c,   "Account name thai
     sfdc_enq(20)        TYPE c,   "SFDC Enquiry No  -->CH12
     sfdc_quo(20)        TYPE c,   "SFDC Quote no  -- CH12
     erp_quo(10)         TYPE c,   "ERP Quotation No
     erp_so(10)          TYPE c,   "ERP SO No.
     erp_so_item(6)      TYPE c,   "ERP SO Item
     erp_inv_no(10)      TYPE c,   "ERP INV. No "Add by wantanee 20131218
     erp_inv_item(6)     TYPE c,   "ERP INV. ITEM "Add by Wantanee 20150522
     erp_matnr(18)       TYPE c,   "Material__c
     ph1(5)              TYPE c,   "Product Group LV1
     ph2(5)              TYPE c,   "Product Group LV2
     ph3(8)              TYPE c,   "Product Group LV3
     ship_date(10)       TYPE c,   "ship Date
     currency_code(5)    TYPE c,   "Currency Code
     sale_amt            TYPE p DECIMALS 2,
     actual_qty          TYPE i,
     sale_qty            TYPE i,
     ship_cost           TYPE p DECIMALS 2,   "Cost
*     ship_pro_name(100)  TYPE c,  "Project name "Add by Wantanee 20150305
*     vbeln               TYPE vbrk-vbeln,
*     posnr               TYPE vbrp-posnr,
*     matnr               TYPE vbrp-matnr,
*      serial(18)             TYPE c,  "Serial number
*      ship_cn_no(10)         TYPE c,  "CN Number
     fg_wbs(50)           TYPE c,
     std_warranty_wbs(50) TYPE c,
     ext_warranty_wbs(50) TYPE c,
     work_order(20)       TYPE c,  "SV Work order number
     Service_contract_number(20)       TYPE c, "SV contract number

 END OF gy_out.

 TYPES: BEGIN OF gy_inv,
     vbeln              TYPE vbrk-vbeln,
     fkdat              TYPE vbrk-fkdat,
     erdat              TYPE vbrk-erdat,
     vtweg              TYPE vbrk-vtweg,
     vbtyp              TYPE vbrk-vbtyp,
     posnr              TYPE vbrp-posnr,
     aufnr              TYPE vbrp-aufnr, "Add by Wantanee 20150305
     netwr              TYPE vbrp-netwr,
     prodh              TYPE vbrp-prodh,
     vbelv              TYPE vbrp-vbelv,
     posnv              TYPE vbrp-posnv,
     vkgrp              TYPE vbrp-vkgrp,
     vkbur              TYPE vbrp-vkbur,
     fkimg              TYPE vbrp-fklmg,
     kowrr              TYPE vbrp-kowrr,
     aubel              TYPE vbrp-aubel,
*     aupos              TYPE vbrp-aupos,
     matnr              TYPE vbrp-matnr,
*     submi              TYPE vbak-submi,
*     ihrez_e            TYPE vbkd-ihrez_e,
     kunnr              TYPE vbpa-kunnr,
     adrnr              TYPE vbpa-adrnr,
     pernr              TYPE vbpa-pernr,
     aupos              TYPE vbrp-aupos,
     wavwr              TYPE vbrp-wavwr,  "Add by Wantanee
     ps_psp_pnr         TYPE vbrp-ps_psp_pnr,

*     vbelv_fa           TYPE vbfa-vbelv,


 END OF gy_inv.

 TYPES: BEGIN OF gy_vbrk,
     vbeln              TYPE vbrk-vbeln,
     fkdat              TYPE vbrk-fkdat,
     erdat              TYPE vbrk-erdat,

 END OF gy_vbrk.

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
 TYPES: BEGIN OF  gy_pa0002,
    pernr     TYPE pa0002-pernr,
    vorna     TYPE pa0002-vorna,  "Name
    nachn     TYPE pa0002-nachn,  "Sername
 END OF  gy_pa0002.

TYPES: BEGIN OF gy_output,
      SHIPPED_UID(100)      TYPE  C,
      COMP(10)              TYPE  C,
      BRANCH(10)            TYPE  C,
      SHIP_VTWEG(25)        TYPE  C,
      SHIP_VKBUR(30)        TYPE  C,
      SHIP_VKGRP(30)        TYPE  C,
      SHIP_EMP_NAME_EN(100) TYPE  C,
      SHIP_KUNNR(20)        TYPE  C,
      SHIP_ACC_NAME_TH(100) TYPE  C,
      SHIP_SFDC_ENQ(20)     TYPE  C,
      SHIP_SFDC_QUO(20)     TYPE  C,
      SHIP_ERP_QUO(10)      TYPE  C,
      SHIP_ERP_SO(10)       TYPE  C,
      SHIP_ERP_SO_ITEM(6)   TYPE  C,
      SHIP_ERP_INV_NO(10)   TYPE  C,
      SHIP_ERP_INV_ITEM(6)  TYPE  C,
      SHIP_ERP_MATNR(18)    TYPE  C,
      SHIP_PH1(20)          TYPE  C,
      SHIP_PH2(20)          TYPE  C,
      SHIP_PH3(20)          TYPE  C,
      SHIP_DATE(20)         TYPE  C,
      SHIP_CURRENCY_CODE(20)  TYPE  C,
      SHIP_SALE_AMT(20)     TYPE  C,
      SHIP_ACTUAL_QTY(20)   TYPE  C,
      SHIP_SALE_QTY(20)     TYPE  C,
      SHIP_COST(20)         TYPE  C,
*      SHIP_PRO_NAME(100)    TYPE  C,
*      SHIP_fg_wbs(50)           TYPE c,
*      SHIP_std_warranty_wbs(50) TYPE c,
*      SHIP_ext_warranty_wbs(50) TYPE c,

*      SHIP_SERIAL(18)       TYPE  C,
*      SHIP_CN_NO(10)        TYPE  C,
      SHIP_SF_WORK_ORDER_NO(20)     TYPE  C,
      SHIP_SF_SV_CONTRACT_NO(20)         TYPE  C,
END OF gy_output.

*TYPES: BEGIN OF gy_check_ph,
*  repid     TYPE zsds_gen_c-repid,
*  const     TYPE zsds_gen_c-const,
*  prodh     TYPE zsds_gen_c-value,
*
*END OF gy_check_ph.

TYPES: BEGIN OF gy_so_qt,
  vbeln     TYPE vbfa-vbeln,
  vbelv     TYPE vbfa-vbelv,
END OF gy_so_qt.

TYPES: BEGIN OF gy_sfdc_quo_enq,
  vbelv     TYPE vbak-vbeln,
  ihrez     TYPE vbkd-ihrez,
  ihrez_e   TYPE vbkd-ihrez_e,

END OF gy_sfdc_quo_enq.

"Add by Wantanee 20150311
TYPES: BEGIN OF gy_aufnr,
       aufnr TYPE aufk-aufnr,

END OF gy_aufnr.
TYPES: BEGIN OF gy_aufk,
       aufnr TYPE aufk-aufnr,
       ktext TYPE aufk-ktext,

END OF gy_aufk.

"End Add by Wantanee 20150311
"-->CH11-001 Add by Wantanee 20220419
TYPES: BEGIN OF gy_check_inv,
       vbelv             TYPE vbfa-vbelv,   "do
       posnv             TYPE vbfa-posnv,     "item
       vbeln             TYPE vbfa-vbeln,   "invoice or cn
       posnn             TYPE vbfa-posnn,   " invoice item
       matnr             TYPE objk-matnr,   "material
       sernr             TYPE objk-sernr,   "Serial
END OF gy_check_inv.
TYPES: BEGIN OF gy_check_cn_so,
       vbelv             TYPE vbfa-vbelv,   "do
       posnv             TYPE vbfa-posnv,     "item
       vbeln             TYPE vbfa-vbeln,   " cn
       posnn             TYPE vbfa-posnn,   " cn item
       matnr             TYPE objk-matnr,   "material
       sernr             TYPE objk-sernr,   "Serial
END OF gy_check_cn_so.
"-->CH11-001 End Add by Wantanee 20220419

DATA : BEGIN OF LS_HEADER,
           FIELD1 TYPE STRING,
           FIELD2 TYPE STRING,
           FIELD3 TYPE STRING,
           FIELD4 TYPE STRING,
           FIELD5 TYPE STRING,
           FIELD6 TYPE STRING,
           FIELD7 TYPE STRING,
           FIELD8 TYPE STRING,
           FIELD9 TYPE STRING,
           FIELD10 TYPE STRING,
           FIELD11 TYPE STRING,
           FIELD12 TYPE STRING,
           FIELD13 TYPE STRING,
           FIELD14 TYPE STRING,
           FIELD15 TYPE STRING,
           FIELD16 TYPE STRING,
           FIELD17 TYPE STRING,
           FIELD18 TYPE STRING,
           FIELD19 TYPE STRING,
           FIELD20 TYPE STRING,
           FIELD21 TYPE STRING,
          FIELD22 TYPE STRING,
          FIELD23 TYPE STRING,
          FIELD24 TYPE STRING,
          FIELD25 TYPE STRING,
          FIELD26 TYPE STRING,
          FIELD27 TYPE STRING,
          FIELD28 TYPE STRING,

         END OF LS_HEADER.
DATA LT_HEADER LIKE TABLE OF LS_HEADER.
DATA: CT_RESULT TYPE TABLE OF STRING.
  CONSTANTS : BEGIN OF LC_CON,
                FIELD1    TYPE STRING VALUE 'SHIPPED_UID',
                FIELD2    TYPE STRING VALUE 'COMP',
                FIELD3    TYPE STRING VALUE 'BRANCH',
                FIELD4    TYPE STRING VALUE 'SHIP_VTWEG',
                FIELD5    TYPE STRING VALUE 'SHIP_VKBUR',
                FIELD6    TYPE STRING VALUE 'SHIP_VKGRP',
                FIELD7    TYPE STRING VALUE 'SHIP_EMP_NAME_EN',
                FIELD8    TYPE STRING VALUE 'SHIP_KUNNR',
                FIELD9    TYPE STRING VALUE 'SHIP_ACC_NAME_TH',
                FIELD10   TYPE STRING VALUE 'SHIP_SFDC_ENQ',
                FIELD11    TYPE STRING VALUE 'SHIP_SFDC_QUO',
                FIELD12    TYPE STRING VALUE 'SHIP_ERP_QUO',
                FIELD13    TYPE STRING VALUE 'SHIP_ERP_SO',
                FIELD14    TYPE STRING VALUE 'SHIP_ERP_SO_ITEM',
                FIELD15    TYPE STRING VALUE 'SHIP_ERP_INV_NO',
                FIELD16    TYPE STRING VALUE 'SHIP_ERP_INV_ITEM',
                FIELD17    TYPE STRING VALUE 'SHIP_ERP_MATNR',
                FIELD18    TYPE STRING VALUE 'SHIP_PH1',
                FIELD19    TYPE STRING VALUE 'SHIP_PH2',
                FIELD20   TYPE STRING VALUE 'SHIP_PH3',
                FIELD21   TYPE STRING VALUE 'SHIP_DATE',
                FIELD22   TYPE STRING VALUE 'SHIP_CURRENCY_CODE',
                FIELD23   TYPE STRING VALUE 'SHIP_SALE_AMT',
                FIELD24   TYPE STRING VALUE 'SHIP_ACTUAL_QTY',
                FIELD25   TYPE STRING VALUE 'SHIP_SALE_QTY',
                FIELD26   TYPE STRING VALUE 'SHIP_COST',
                FIELD27   TYPE STRING VALUE 'SHIP_SF_WORK_ORDER_NO',
                FIELD28   TYPE STRING VALUE 'SHIP_SF_CONTRACT_NO',
*                FIELD27   TYPE STRING VALUE 'Project_name__c',
*                FIELD1    TYPE STRING VALUE 'Shipped_UID__c',
*                FIELD2    TYPE STRING VALUE 'Company__c',
*                FIELD3    TYPE STRING VALUE 'Branch__c',
*                FIELD4    TYPE STRING VALUE 'Distribution_Channel__c',
*                FIELD5    TYPE STRING VALUE 'Sale_Office__c',
*                FIELD6    TYPE STRING VALUE 'Sale_Group__c',
*                FIELD7    TYPE STRING VALUE 'Sale_Man__c',
*                FIELD8    TYPE STRING VALUE 'ERP_Customer_ID__c',
*                FIELD9    TYPE STRING VALUE 'Account_Name_Local__c',
*                FIELD10   TYPE STRING VALUE 'SFDC_Enquiry_No__c',
*                FIELD11    TYPE STRING VALUE 'SFDC_Quote_No__c',
*                FIELD12    TYPE STRING VALUE 'ERP_Quotation_No__c',
*                FIELD13    TYPE STRING VALUE 'SO_No__c',
*                FIELD14    TYPE STRING VALUE 'Item_No__c',
*                FIELD15    TYPE STRING VALUE 'Inv_No__c',
*                FIELD16    TYPE STRING VALUE 'Inv_Item_No__c',
*                FIELD17    TYPE STRING VALUE 'Material__c',
*                FIELD18    TYPE STRING VALUE 'Product_Group_LV1__c',
*                FIELD19    TYPE STRING VALUE 'Product_Group_LV2__c',
*                FIELD20   TYPE STRING VALUE 'Product_Group_LV3__c',
*                FIELD21   TYPE STRING VALUE 'Ship_Date__c',
*                FIELD22   TYPE STRING VALUE 'CurrencyIsoCode',
*                FIELD23   TYPE STRING VALUE 'Sale_Amt__c',
*                FIELD24   TYPE STRING VALUE 'Actual_Qty__c',
*                FIELD25   TYPE STRING VALUE 'Sale_Qty__c',
*                FIELD26   TYPE STRING VALUE 'Cost__c',
*                FIELD27   TYPE STRING VALUE 'Project_name__c',
              END OF LC_CON.

*&-----------------------------------------------------------------------------------*
*& I N T E R N A L   T A B L E S
*&-----------------------------------------------------------------------------------*
* Data for ALV display

TYPE-POOLS: slis.

* The inputs that need to be passed to the REUSE_ALV function module
DATA: gt_list_fieldcat  TYPE lvc_t_fcat,      "Field Catalog for List Viewer Control
      gt_exit(1)        TYPE c,
      gt_variant        TYPE disvariant,
      gx_variant        TYPE disvariant,
      gs_print           TYPE lvc_s_prnt.

*DATA : BEGIN OF tab_cat OCCURS 0.
*        INCLUDE STRUCTURE ztgain01.
*DATA : END   OF tab_cat.
*DATA : BEGIN OF tab_err occurs 0.
*        INCLUDE STRUCTURE  bapiret2.
*DATA : END   OF tab_err.

DATA: gt_itab               TYPE STANDARD TABLE OF gy_out,
      gt_itab_temp          TYPE STANDARD TABLE OF gy_out,
      gt_inv                TYPE STANDARD TABLE OF gy_inv,
      gt_vbrk               TYPE STANDARD TABLE OF gy_vbrk,
      gt_adrc               TYPE STANDARD TABLE OF gy_adrc,
      gt_pa0002             TYPE STANDARD TABLE OF gy_pa0002,
*      gt_check_ph           TYPE STANDARD TABLE OF gy_check_ph,
      gt_so_qt              TYPE STANDARD TABLE OF gy_so_qt,
      gt_sfdc_quo_enq       TYPE STANDARD TABLE OF gy_sfdc_quo_enq,
      gt_aufnr              TYPE STANDARD TABLE OF gy_aufnr,
      gt_aufk               TYPE STANDARD TABLE OF gy_aufk.



DATA: gs_inv                TYPE gy_inv,
      gs_so_qt              TYPE gy_so_qt,
      gs_sfdc_quo_enq       TYPE gy_sfdc_quo_enq.

DATA: gs_itab               TYPE gy_out,
      gs_inv1                TYPE gy_inv,
      gs_adrc               TYPE gy_adrc,
      gs_pa0002             TYPE gy_pa0002,
*      gs_check_ph           TYPE gy_check_ph,
      gs_so_qt1              TYPE gy_so_qt,
      gs_aufnr              TYPE gy_aufnr,
      gs_aufk               TYPE gy_aufk.

"-->CH11-002 Add by Wantanee 20220419
DATA: gt_check_inv TYPE STANDARD TABLE OF  gy_check_inv,
      gs_check_inv TYPE    gy_check_inv,
      gs_check_inv1 TYPE    gy_check_inv.
DATA: gt_check_cn_so TYPE STANDARD TABLE OF  gy_check_cn_so,
      gs_check_cn_so TYPE    gy_check_cn_so,
      gs_check_cn_so1 TYPE    gy_check_cn_so.
"-->CH11-002 End Add by Wantanee 20220419


DATA: gt_out            TYPE STANDARD TABLE OF gy_output,
      gs_out           TYPE gy_output.
DATA : gt_result TYPE TABLE OF gy_output,
       gs_result TYPE gy_output.

DATA: grid_main        TYPE REF TO cl_gui_alv_grid,
     container_main   TYPE REF TO cl_gui_custom_container,
     gs_variant       LIKE disvariant.


DATA: p_date TYPE vbrk-erdat.

*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
DATA : v_pos TYPE i .

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gt_layout   TYPE slis_layout_alv,
       gt_events   TYPE slis_t_event.
*        gt_heading  TYPE slis_t_listheader.

*&-----------------------------------------------------------------------------------*
*& C O N S T A N T
*&-----------------------------------------------------------------------------------*
CONSTANTS :  gc_repid       TYPE repid         VALUE 'ZSDSSDR0110',
             gc_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
*CONSTANTS : functi1 =

CONSTANTS : mestyp1   LIKE  edidc-mestyp VALUE 'ZSFM011'.


*&-----------------------------------------------------------------------------------*
*& S E L E C T I O N   S C R E E N
*&-----------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-h01.


PARAMETERS:       p_bukrs       TYPE bukrs   OBLIGATORY DEFAULT '1000'.

SELECT-OPTIONS: s_erdat     FOR vbrk-erdat,
                s_vbeln     FOR vbrk-vbeln,
                s_vtweg     FOR vbrk-vtweg,
                s_vkbur     FOR vbrp-vkbur,
                s_vkgrp     FOR vbrp-vkgrp,
                s_mtart     FOR mara-mtart,  "Add by Wantanee 20150625
                s_prodh     FOR vbrp-prodh,   "Add by Wantanee 20161004
                s_fkart     FOR vbrk-fkart. "Invoice Type


PARAMETERS: p_disp RADIOBUTTON GROUP gr1 ,                   " Download to WEB Server
*            p_local RADIOBUTTON GROUP gr1,
            p_idoc RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_path LIKE rlgrap-filename LOWER CASE. .



SELECTION-SCREEN END OF BLOCK b1.


*END SELECTION SCREEN
*&-----------------------------------------------------------------------------------*
* Event:Initialization
INITIALIZATION.

     p_path = '/usr/sap/tmp/'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*  IF NOT p_local IS INITIAL.
*     p_path = 'D:\'.
*
*   ELSEIF NOT p_idoc IS INITIAL.
*     p_path = '/usr/sap/tmp/'.
*   ENDIF.
  PERFORM get_path_name CHANGING p_path.
*&-----------------------------------------------------------------------------------*
*& * START-OF-SELECTION
*&-----------------------------------------------------------------------------------*
START-OF-SELECTION .
* Get data
  PERFORM f_get_data.
  PERFORM f_map_data.

*&-----------------------------------------------------------------------------------*
*& * END-OF-SELECTION
*&-----------------------------------------------------------------------------------*
END-OF-SELECTION.
  IF NOT gt_itab[] IS INITIAL.
    IF NOT p_disp IS INITIAL.
      PERFORM display_reprot.
*    ELSEIF NOT p_local IS INITIAL.
*      PERFORM cat_edit_rtn .
*      PERFORM export.
    ELSE.
      PERFORM F_GET_RESULT.
    ENDIF.
  ELSE.
    PERFORM F_GET_RESULT.
*    MESSAGE i003.
    EXIT.
  ENDIF.
*--------------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*

FORM f_get_data .
     CLEAR : p_date.
     p_date = sy-datlo.



*     SELECT vbeln fkdat erdat
*     INTO TABLE gt_vbrk
*     FROM vbrk
*     WHERE erdat EQ p_date.
*
*     IF gt_vbrk IS INITIAL.
        p_date = sy-datlo - 1.
*     ENDIF.

     IF NOT s_erdat IS INITIAL.
        SELECT a~vbeln a~fkdat a~erdat a~vtweg a~vbtyp
                   b~posnr
                   b~aufnr "Add by wantanee 20150310
                   b~netwr b~prodh
                   b~vbelv b~posnv
                   b~vkgrp b~vkbur b~fkimg b~kowrr
                   b~aubel b~matnr
*                   d~submi
*                   f~ihrez_e
                   g~kunnr g~adrnr
                   h~pernr
                   b~aupos
                   b~wavwr
*                   l~vbelv
                   b~ps_psp_pnr
            INTO TABLE gt_inv
            FROM vbrk AS a INNER JOIN vbrp AS b
                           ON ( a~vbeln EQ b~vbeln )
*                           INNER JOIN vbak AS d
*                           ON ( b~aubel EQ d~vbeln )
*                           INNER JOIN vbkd AS f
*                           ON ( d~vbeln EQ f~vbeln
*                           AND  f~posnr EQ '000000' )
                           INNER JOIN vbpa AS g
                           ON ( a~vbeln EQ g~vbeln
                           AND  g~posnr EQ '000000'
                           AND  g~parvw EQ 'AG' )
                           INNER JOIN vbpa AS h
                           ON ( a~vbeln EQ h~vbeln )
*                           AND  h~posnr EQ '000000'
*                           AND  h~parvw EQ 'VE' )
                           INNER JOIN mara AS k
                           ON ( b~matnr EQ k~matnr )
*                           AND  k~mtart EQ 'ZFG' )
*                           INNER JOIN vbfa AS l
*                           ON ( b~aubel EQ l~vbeln
*                           AND  b~aupos EQ l~posnn
*                           AND  l~vbtyp_v EQ 'B' )
            WHERE a~erdat IN s_erdat
            AND   a~vbeln IN s_vbeln
            AND   a~vtweg IN s_vtweg
            AND   b~vkbur IN s_vkbur
            AND   b~vkgrp IN s_vkgrp
            AND   b~prodh IN s_prodh
            AND   a~fkart IN s_fkart
            AND   ( h~parvw EQ 'VE' OR h~parvw EQ 'ZM' )
            AND   k~mtart IN s_mtart.  "Add by Wantanee 20150625



     ELSE.

            SELECT a~vbeln a~fkdat a~erdat a~vtweg a~vbtyp
                   b~posnr
                   b~aufnr "Add by wantanee 20150310
                   b~netwr b~prodh
                   b~vbelv b~posnv
                   b~vkgrp b~vkbur b~fkimg  b~kowrr
                   b~aubel b~matnr
*                   d~submi
*                   f~ihrez_e
                   g~kunnr g~adrnr
                   h~pernr
                   b~aupos
                   b~wavwr
*                   l~vbelv
                   b~ps_psp_pnr
            INTO TABLE gt_inv
            FROM vbrk AS a INNER JOIN vbrp AS b
                           ON ( a~vbeln EQ b~vbeln )
*                           INNER JOIN vbak AS d
*                           ON ( b~aubel EQ d~vbeln )
*                           INNER JOIN vbkd AS f
*                           ON ( d~vbeln EQ f~vbeln
*                           AND  f~posnr EQ '000000' )
                           INNER JOIN vbpa AS g
                           ON ( a~vbeln EQ g~vbeln
                           AND  g~posnr EQ '000000'
                           AND  g~parvw EQ 'AG' )
                           INNER JOIN vbpa AS h
                           ON ( a~vbeln EQ h~vbeln )
*                           AND  h~posnr EQ '000000'
*                           AND  h~parvw EQ 'VE' )
                           INNER JOIN mara AS k
                           ON ( b~matnr EQ k~matnr )
*                           AND  k~mtart EQ 'ZFG' )
*                           INNER JOIN vbfa AS l
*                           ON ( b~aubel EQ l~vbeln
*                           AND  b~aupos EQ l~posnn
*                           AND  l~vbtyp_v EQ 'B' )
            WHERE a~erdat EQ p_date
              AND   a~vbeln IN s_vbeln
              AND   a~vtweg IN s_vtweg
              AND   b~vkbur IN s_vkbur
              AND   b~vkgrp IN s_vkgrp
              AND   b~prodh IN s_prodh
              AND   a~fkart IN s_fkart
              AND   ( h~parvw EQ 'VE' OR h~parvw EQ 'ZM' )
              AND k~mtart IN s_mtart.  "Add by Wantanee 20150625
    ENDIF.

    DELETE ADJACENT DUPLICATES FROM gt_inv.

    LOOP AT gt_inv INTO gs_inv.
         CLEAR: gs_so_qt,gs_aufk.
         SELECT SINGLE vbeln vbelv
         INTO (gs_so_qt-vbeln,gs_so_qt-vbelv)
         FROM vbfa
         WHERE vbeln = gs_inv-aubel
         AND   posnv = gs_inv-aupos  "Add by Wantanee 20170309
         AND   ( vbtyp_v = 'B' OR vbtyp_v = 'CSVO' OR vbtyp_v =  'CSCT').

        IF gs_so_qt-vbeln EQ '' OR gs_so_qt-vbelv EQ ''.
             CLEAR: gs_so_qt,gs_aufk.
             SELECT SINGLE vbeln vbelv
             INTO (gs_so_qt-vbeln,gs_so_qt-vbelv)
             FROM vbfa
             WHERE vbeln = gs_inv-aubel
             AND   posnn = gs_inv-aupos  "Add by Wantanee 20170309
             AND   ( vbtyp_v = 'B' OR vbtyp_v = 'CSVO' OR vbtyp_v =  'CSCT').
        ENDIF.

        IF gs_so_qt-vbeln EQ '' OR gs_so_qt-vbelv EQ ''.
             CLEAR: gs_so_qt,gs_aufk.
             SELECT SINGLE vbeln vbelv
             INTO (gs_so_qt-vbeln,gs_so_qt-vbelv)
             FROM vbfa
             WHERE vbeln = gs_inv-aubel
             AND   ( vbtyp_v = 'B' OR vbtyp_v = 'CSVO' OR vbtyp_v =  'CSCT').
        ENDIF.

         IF gs_so_qt-vbeln NE '' OR gs_so_qt-vbelv NE ''.

            APPEND gs_so_qt TO gt_so_qt.

            IF gs_so_qt-vbelv NE ''.
               gs_sfdc_quo_enq-vbelv = gs_so_qt-vbelv.

               SELECT SINGLE ihrez ihrez_e
                 INTO (gs_sfdc_quo_enq-ihrez,gs_sfdc_quo_enq-ihrez_e)
                 FROM vbkd
                 WHERE vbeln = gs_so_qt-vbelv
                 AND   posnr EQ '000000'.
             APPEND gs_sfdc_quo_enq TO gt_sfdc_quo_enq.

            ENDIF.

         ENDIF.
         "Add by Wantanee 20150311
         IF gs_inv-aufnr NE ''.
           SELECT SINGLE aufnr ktext
           INTO (gs_aufk-aufnr,gs_aufk-ktext)
           FROM aufk
           WHERE aufnr = gs_inv-aufnr.
           APPEND gs_aufk TO gt_aufk.
         ENDIF.
         "End Add by Wantanee 20150311



    ENDLOOP.
    "Add by Wantanee 20150311
    IF NOT gt_aufk IS INITIAL.
       SORT gt_aufk.
       DELETE ADJACENT DUPLICATES FROM  gt_aufk.
    ENDIF.
    "End Add by Wantanee 20150311



    IF NOT gt_inv IS INITIAL.
         SELECT a~pernr a~vorna a~nachn
         INTO TABLE gt_pa0002
         FROM pa0002 AS a
         FOR ALL ENTRIES IN gt_inv
         WHERE a~pernr EQ gt_inv-pernr.

        "-->CH11-003 Add by Wantanee 20220419
         SELECT a~vbelv a~posnv a~vbeln a~posnn d~matnr d~sernr
         INTO TABLE gt_check_inv
         FROM vbfa AS a INNER JOIN ser01 AS b
                         ON ( a~vbelv EQ b~LIEF_NR
                         AND  a~posnv EQ b~posnr )
                        INNER JOIN objk AS d
                         ON ( b~obknr EQ d~obknr )
         FOR ALL ENTRIES IN gt_inv
         WHERE vbeln EQ gt_inv-vbeln
           AND posnn EQ gt_inv-posnr
           AND  vbtyp_n EQ 'M'
           AND  vbtyp_v EQ 'J'.
*           OR  vbgy_n EQ 'N'
*           OR  vbgy_n EQ 'O'
*           OR  vbgy_n EQ 'P' .

         SELECT b~vbeln b~posnn a~vbeln a~posnn f~matnr f~sernr
         INTO TABLE gt_check_cn_so
         FROM vbfa AS a INNER JOIN vbfa AS b
                        ON ( a~vbelv EQ b~vbelv
                        AND  a~posnv = b~posnv )
                        INNER JOIN  ser01 AS d
                         ON ( b~vbeln EQ d~LIEF_NR
                         AND  b~posnn EQ d~posnr )
                        INNER JOIN objk AS f
                         ON ( d~obknr EQ f~obknr )

         FOR ALL ENTRIES IN gt_inv
         WHERE a~vbeln EQ gt_inv-vbeln
           AND a~posnn EQ gt_inv-posnr
           AND  a~vbtyp_n EQ 'O'
           AND  a~vbtyp_v EQ 'H'
           AND  b~vbtyp_v EQ 'H'
           AND  b~vbtyp_n EQ 'T'.
*           OR  vbgy_n EQ 'N'
*           OR  vbgy_n EQ 'O'
*           OR  vbgy_n EQ 'P' .
        "-->CH11-003 End Add by Wantanee 20220419

    ENDIF.

*    SELECT repid const value
*    INTO TABLE gt_check_ph
*    FROM   zsds_gen_c
*    WHERE repid EQ gc_check_ph.

   "-->CH11-004 Add by Wantanee 20220419

   IF gt_check_cn_so IS NOT INITIAL.
      LOOP AT gt_check_cn_so INTO gs_check_cn_so.
           CLEAR: gs_check_inv.
           gs_check_inv-vbelv = gs_check_cn_so-vbelv.
           gs_check_inv-posnv = gs_check_cn_so-posnv.
           gs_check_inv-vbeln = gs_check_cn_so-vbeln.
           gs_check_inv-posnn = gs_check_cn_so-posnn.
           gs_check_inv-matnr = gs_check_cn_so-matnr.
           gs_check_inv-sernr = gs_check_cn_so-sernr.
           APPEND gs_check_inv TO gt_check_inv.

      ENDLOOP.

   ENDIF.
   "-->CH11-004 End Add by Wantanee 20220419


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

  DATA: lfd_vbak_vbtyp     TYPE vbak-vbtyp.
  DATA: lv_year(4) TYPE c,
        lv_month(2) TYPE c,
        lv_day(2)  TYPE c.
  DATA: lv_original_inv TYPE vbfa-vbeln.

  DATA lv_amount_sign TYPE p DECIMALS 2.

  DATA: lv_sfdc_quo(10)        TYPE c.
  DATA: lv_object_type TYPE CRMS4D_SERV_H-OBJTYPE_H.





 DELETE ADJACENT DUPLICATES FROM gt_inv.
   LOOP AT gt_inv INTO gs_inv1.
     CLEAR: lfd_vbak_vbtyp,lv_year,lv_month,lv_day,gs_itab,gs_aufk.



      gs_itab-comp = 'SDS'.
      gs_itab-branch = 'BKK'.

*      gs_itab-vbeln = gs_inv1-vbeln.
*      gs_itab-posnr = gs_inv1-posnr.
      gs_itab-erp_matnr = gs_inv1-matnr.


      gs_itab-vtweg = gs_inv1-vtweg.
      gs_itab-vkbur = gs_inv1-vkbur.
      gs_itab-vkgrp = gs_inv1-vkgrp.
      READ TABLE gt_pa0002 INTO gs_pa0002 WITH KEY pernr = gs_inv1-pernr.
           IF sy-subrc EQ 0.
               CONCATENATE gs_pa0002-vorna gs_pa0002-nachn INTO gs_itab-emp_name_en SEPARATED BY SPACE.
           ENDIF.
      gs_itab-kunnr = gs_inv1-kunnr.
      PERFORM get_customer USING   gs_inv1-kunnr gs_inv1-adrnr
                         CHANGING gs_itab-acc_name_th.


*      gs_itab-sfdc_enq = gs_inv1-ihrez_e.
*      gs_itab-sfdc_quo = gs_inv1-submi.
      gs_itab-erp_so = gs_inv1-aubel.
      gs_itab-erp_so_item = gs_inv1-aupos.
      "CH11-008 Add by Wantanee 20220419
*      IF gs_inv1-vbeln+0(1) = '6'.
**         gs_itab-SHIP_CN_NO =  gs_inv1-vbeln.
*         CLEAR: lv_original_inv.
*         SELECT SINGLE vbelv
*           INTO lv_original_inv
*           FROM vbfa
*           WHERE vbeln = gs_inv1-vbeln
*           AND vbtyp_v EQ 'M'.
*         gs_itab-erp_inv_no = lv_original_inv.
*       "CH11-008 End Add by Wantanee 20220419
*      ELSE.
         gs_itab-erp_inv_no =  gs_inv1-vbeln. "Add by Wantanee 20131218
*      ENDIF.
      gs_itab-erp_inv_item = gs_inv1-posnr. "Add by Wantanee 20150522

      READ TABLE gt_so_qt INTO gs_so_qt1 WITH KEY vbeln = gs_inv1-aubel.
          IF sy-subrc EQ 0.
              gs_itab-erp_quo = gs_so_qt1-vbelv.
              CLEAR: lv_sfdc_quo.

              SELECT SINGLE bname
                INTO (lv_sfdc_quo)
                FROM vbak
                WHERE vbeln EQ gs_itab-erp_quo.

                gs_itab-sfdc_quo = lv_sfdc_quo.
                IF gs_itab-sfdc_quo IS INITIAL.
                   CLEAR: lv_sfdc_quo,lv_object_type.
                   SELECT SINGLE PO_NUMBER_SOLD OBJTYPE_H
                     INTO (lv_sfdc_quo,lv_object_type)
                     FROM CRMS4D_SERV_H
                     WHERE OBJECT_ID EQ gs_itab-erp_quo.
                     IF lv_object_type EQ 'BUS2000112'.
                         gs_itab-Service_contract_number = lv_sfdc_quo.
                     ELSEIF lv_object_type EQ 'BUS2000116'.
                         gs_itab-work_order = lv_sfdc_quo.
                     ENDIF.

                ENDIF.


          ELSE.
              gs_itab-erp_quo = ''.
              gs_itab-sfdc_quo = ''.
          ENDIF.

      gs_itab-ph1 = gs_inv1-prodh+0(5).
      gs_itab-ph2 = gs_inv1-prodh+5(5).
      gs_itab-ph3 = gs_inv1-prodh+10(8).
      lv_year = gs_inv1-fkdat+0(4).
      lv_month = gs_inv1-fkdat+4(2).
      lv_day = gs_inv1-fkdat+6(2).
      CONCATENATE lv_day  lv_month  lv_year INTO gs_itab-ship_date SEPARATED BY '/'.
*      gs_itab-ship_date = gs_inv1-fkdat.
      gs_itab-currency_code = 'THB'.
      IF gs_inv1-vbtyp = 'O' OR gs_inv1-vbtyp = 'N'.    "Credit Memo / Cancle Inv.
          gs_itab-sale_amt = gs_inv1-netwr * -1 .
          gs_itab-ship_cost = gs_inv1-wavwr * -1 .

*          gs_itab-actual_qty = gs_inv1-fkimg * -1 .  "Add by Wantanee 20120731
*         CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*            CHANGING
*              VALUE         = gs_itab-sale_amt
*                    .
      ELSE.

         gs_itab-sale_amt = gs_inv1-netwr  .
         gs_itab-ship_cost = gs_inv1-wavwr  .
*         CONDENSE gs_itab-sale_amt.
*          gs_itab-actual_qty = gs_inv1-fkimg  .  "Add by Wantanee 20120731
      ENDIF.


          IF gs_inv1-kowrr EQ 'X'.
                  gs_itab-actual_qty = 0.
                  gs_itab-sale_qty = 0.

          ELSE.
*             IF gt_check_ph IS INITIAL.
*                  gs_itab-sale_qty = 0.
*             ELSE.
                      gs_itab-sale_qty = gs_inv1-fkimg.
                      IF gs_inv1-vbtyp = 'P' OR gs_inv1-vbtyp = 'S'.     "Debit Memo/Cancle CN.
                          gs_itab-sale_qty = 0.
                      ELSEIF gs_inv1-vbtyp = 'O'.   "Credit Memo
*                      CHECK sale ORDER category IF it IS doc return : vbak-vbtyp = 'H'.
                        SELECT SINGLE vbtyp INTO lfd_vbak_vbtyp
                                            FROM vbak
                                           WHERE vbeln = gs_inv1-aubel.
*                        sale qty = 0 for wrong price credit note and sale qty
*                        -1 for return credit note
*                        case credit note and return
                        IF lfd_vbak_vbtyp = 'H'.
                          gs_itab-sale_qty = gs_inv1-fkimg * -1.
*                          case credit note, no return
                        ELSE.
                          gs_itab-sale_qty = 0.
                        ENDIF.

                      ELSEIF gs_inv1-vbtyp = 'N'.  " Add Inv. Cancle
                        gs_itab-sale_qty = gs_inv1-fkimg * -1.

                      ENDIF.


*                   READ TABLE gt_check_ph INTO gs_check_ph  WITH KEY prodh+0(10) = gs_inv1-prodh+0(10).
*                        IF sy-subrc NE 0.
*                           gs_itab-sale_qty = 0.
*
*                        ENDIF.


                "Actual QTY
                     gs_itab-actual_qty = gs_inv1-fkimg.
                      IF gs_inv1-vbtyp = 'O'.     "Credit Memo
*                        CHECK sale ORDER category IF it IS return document (vbak-vbtyp = 'H'.
                          SELECT SINGLE vbtyp INTO lfd_vbak_vbtyp
                                              FROM vbak
                                             WHERE vbeln = gs_inv1-aubel.
*                        actual qty = 0 for wrong price credit note and sale qty
*                        -1 for return credit note
*                        case credit note, no return

                              IF lfd_vbak_vbtyp = 'H'.
                                gs_itab-actual_qty = gs_inv1-fkimg * -1.
*                            case credit note and return
                              ELSE.
                                gs_itab-actual_qty = 0.
                              ENDIF.
                        ELSEIF gs_inv1-vbtyp = 'N'.    "Cancle Inv.
                          gs_itab-actual_qty =  gs_inv1-fkimg * -1.
                        ELSEIF gs_inv1-vbtyp = 'S'.    "Cancle Credit memo.

                                  SELECT SINGLE vbtyp INTO lfd_vbak_vbtyp
                                                      FROM vbak
                                                     WHERE vbeln = gs_inv1-aubel.
*                                actual qty = 0 for wrong price credit note and sale qty
*                                -1 for return credit note
*                                case credit note, no return

                                  IF lfd_vbak_vbtyp = 'H'.
                                    gs_itab-actual_qty = gs_inv1-fkimg.

*                                case credit note and return
                                  ELSE.
                                    gs_itab-actual_qty = 0.
                                  ENDIF.
                        ELSEIF gs_inv1-vbtyp = 'P'. "Debit Memo/

                             gs_itab-actual_qty = 0.
                        ENDIF.
*             ENDIF.

          ENDIF.



*      CONCATENATE gs_itab-comp '+' gs_itab-branch '+' gs_itab-erp_so '+' gs_itab-erp_so_item '+'
*                  lv_day  lv_month  lv_year   INTO  gs_itab-shipped_uid.




*         CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*            CHANGING
*              VALUE         = gs_itab-sale_amt
*                    .


*          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*            CHANGING
*              VALUE         = gs_itab-sale_qty.
*
*          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*            CHANGING
*              VALUE         = gs_itab-actual_qty.

    "Add by Wantanee 20150311 ship_pro_name

*         READ TABLE gt_aufk INTO gs_aufk WITH KEY aufnr = gs_inv1-aufnr.
*              IF sy-subrc EQ 0.
*                 gs_itab-ship_pro_name = gs_aufk-ktext.
*              ENDIF.
*              IF gs_itab-ship_pro_name EQ ''.
*                 PERFORM read_text_head USING 'Z002' 'VBBK' gs_itab-erp_inv_no CHANGING gs_itab-ship_pro_name.    "Project Description  WCH140312
*
*              ENDIF.

*          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*            EXPORTING
*              input         = gs_inv1-ps_psp_pnr
*           IMPORTING
*             OUTPUT        = gs_itab-ship_pro_name
*                    .





    "End Add by Wantanee 20150311
      "-->CH11-005  Add by Wantanee 20220419
      READ TABLE gt_check_inv INTO gs_check_inv WITH KEY vbeln = gs_inv1-vbeln
                                                         posnn = gs_inv1-posnr.
           IF sy-subrc EQ 0.
               LOOP AT gt_check_inv INTO gs_check_inv1 WHERE vbeln = gs_inv1-vbeln and posnn = gs_inv1-posnr.
*                    gs_itab-serial = gs_check_inv1-sernr.
                    CONCATENATE gs_itab-comp '+' gs_itab-branch '+' gs_itab-erp_so '+' gs_itab-erp_so_item '+'
                    gs_itab-erp_inv_no '+'  gs_itab-erp_inv_item    INTO  gs_itab-shipped_uid.
*                    gs_itab-erp_inv_no '+'  gs_itab-erp_inv_item '+' gs_itab-serial   INTO  gs_itab-shipped_uid.
                    APPEND gs_itab TO gt_itab_temp.
               ENDLOOP.

           ELSE.
              CONCATENATE gs_itab-comp '+' gs_itab-branch '+' gs_itab-erp_so '+' gs_itab-erp_so_item '+'
                          gs_itab-erp_inv_no '+'  gs_itab-erp_inv_item   INTO  gs_itab-shipped_uid.
              APPEND gs_itab TO gt_itab_temp.
           ENDIF.

       "-->CH11-005 End Add by Wantanee 20220419

*      APPEND gs_itab TO gt_itab_temp. "-->CH11-006 Remove by Wantanee 20220419
*      APPEND gs_itab TO gt_itab.
   ENDLOOP.



   SORT gt_itab_temp BY shipped_uid comp  branch vtweg vkbur vkgrp emp_name_en kunnr acc_name_th sfdc_enq sfdc_quo
                        erp_quo erp_so erp_so_item
                        erp_inv_no  "Add by Wantanee 20131218
                        erp_matnr ph1  ph2 ph3 ship_date currency_code.
   LOOP AT gt_itab_temp INTO gs_itab.
        COLLECT gs_itab INTO gt_itab.

   ENDLOOP.




        clear: gs_out.

   CLEAR: gs_itab.


   LOOP AT gt_itab INTO gs_itab.

        gs_out-shipped_uid = gs_itab-shipped_uid.
        gs_out-comp = gs_itab-comp.
        gs_out-branch = gs_itab-branch.
        gs_out-ship_vtweg = gs_itab-vtweg.
        gs_out-ship_vkbur = gs_itab-vkbur.
        gs_out-ship_vkgrp = gs_itab-vkgrp.
        gs_out-ship_emp_name_en = gs_itab-emp_name_en.
        gs_out-ship_kunnr = gs_itab-kunnr.
        gs_out-ship_acc_name_th = gs_itab-acc_name_th.
        gs_out-ship_sfdc_enq = gs_itab-sfdc_enq.
        gs_out-ship_sfdc_quo = gs_itab-sfdc_quo.
        gs_out-ship_erp_quo = gs_itab-erp_quo.
        gs_out-ship_erp_so = gs_itab-erp_so.
        gs_out-ship_erp_so_item = gs_itab-erp_so_item.
        gs_out-ship_erp_inv_no = gs_itab-erp_inv_no.
        gs_out-ship_erp_inv_item = gs_itab-erp_inv_item.  "Add by Wantanee 20150522
        gs_out-ship_erp_matnr = gs_itab-erp_matnr.
        gs_out-ship_ph1 = gs_itab-ph1.
        gs_out-ship_ph2 = gs_itab-ph2.
        gs_out-ship_ph3 = gs_itab-ph3.
        gs_out-ship_date = gs_itab-ship_date.
        gs_out-ship_currency_code = gs_itab-currency_code.
        gs_out-ship_sale_amt = gs_itab-sale_amt.
        gs_out-ship_actual_qty = gs_itab-actual_qty.
        gs_out-ship_sale_qty = gs_itab-sale_qty.
        gs_out-ship_cost = gs_itab-ship_cost.
*        gs_out-ship_pro_name = gs_itab-ship_pro_name. "Add by Wantanee 20150311
*        gs_out-ship_serial = gs_itab-serial. "-->CH11-007 Add by Wantanee 20220419
*        gs_out-SHIP_CN_NO = gs_itab-SHIP_CN_NO. "-->CH11-009 Add by Wantanee 20220419
        gs_out-SHIP_SF_WORK_ORDER_NO = gs_itab-work_order.
        gs_out-SHIP_SF_SV_CONTRACT_NO = gs_itab-Service_contract_number.



          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              VALUE         = gs_out-ship_sale_amt.


          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              VALUE         = gs_out-ship_actual_qty.

          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              VALUE         = gs_out-ship_sale_qty.

          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              VALUE         = gs_out-ship_cost.


*          gwa_edidd-segnam = 'ZSFS014'.
*          gwa_edidd-sdata = gs_out.
*          APPEND gwa_edidd TO gt_edidd.
*          clear gwa_edidd.
          APPEND gs_out TO gt_result.

          clear: gs_out.

    ENDLOOP.


*        CONCATENATE '"' gwa_edidd-sdata '"' INTO gwa_edidd-sdata.

*       Linefeed
*        DATA: lv_lf.
*        lv_lf = cl_abap_char_utilities=>cr_lf.
*        CONCATENATE gwa_edidd-sdata lv_lf INTO gwa_edidd-sdata.
*        gwa_edidd-segnam = 'ZSFSH01'.
*        INSERT gwa_edidd INTO gt_edidd INDEX 1.
*        CLEAR gwa_edidd.

*  PERFORM F_GET_RESULT.



ENDFORM.  "map data


**&---------------------------------------------------------------------*
**&      Form  CUSTOMER
**&---------------------------------------------------------------------*
FORM get_customer USING    p_kunnr TYPE kna1-kunnr
                           p_adrnr TYPE kna1-adrnr
                  CHANGING VALUE(p_name_tha) TYPE c.
*                           VALUE(p_name_eng) TYPE c.
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


     CLEAR:p_name_tha.
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
*                IF sy-subrc EQ 0.
*                    CONCATENATE gs_adrc-name1 gs_adrc-name2 INTO p_name_eng.
*                    p_street_en = gs_adrc-street.
*                    p_str_suppl3_en = gs_adrc-str_suppl3.
*                    p_location_en = gs_adrc-location.
*                    p_city2_en = gs_adrc-city2.
*                    p_city1_en = gs_adrc-city1.
*
*
*
*                ENDIF.
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
      i_callback_user_command = 'USER_COMMAND'
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

* Add by K 20110215
*---------------------------------------------------------------------*
*       FORM User_command
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  IF r_ucomm = '&IC1'.  "Double click event

*   goto XD03 display Customer Master

*     READ TABLE gt_itab INTO gs_itab INDEX rs_selfield-tabindex.
*
*              SET: PARAMETER ID 'KUN'  FIELD gs_itab-kunnr,
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

ENDFORM.                    "build_layout

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


* Company
  PERFORM append_fieldcat USING 'SHIPPED_UID'
                                ''
                                ''
                                'SHIPPED UID'
                                 space  space  space
                                 gt_fieldcat[].

* Company
  PERFORM append_fieldcat USING 'COMP'
                                ''
                                ''
                                'Company'
                                 space  space  space
                                 gt_fieldcat[].

* Branch
  PERFORM append_fieldcat USING 'BRANCH'
                                ''
                                ''
                                'Branch'
                                 space  space  space
                                 gt_fieldcat[].


*       vbeln               TYPE vbrk-vbeln,
*     posnr               TYPE vbrp-posnr,
*     matnr               TYPE vbrp-matnr,
** Distribute Channel
*  PERFORM append_fieldcat USING 'VBELN'
*                                ''
*                                ''
*                                'INV'
*                                 space  space  space
*                                 gt_fieldcat[].
** Distribute Channel
*  PERFORM append_fieldcat USING 'POSNR'
*                                ''
*                                ''
*                                'item'
*                                 space  space  space
*                                 gt_fieldcat[].
** Distribute Channel
*  PERFORM append_fieldcat USING 'MATNR'
*                                ''
*                                ''
*                                'MAT'
*                                 space  space  space
*                                 gt_fieldcat[].
* Distribute Channel
  PERFORM append_fieldcat USING 'VTWEG'
                                ''
                                ''
                                'Distribute Channel'
                                 space  space  space
                                 gt_fieldcat[].
* Sales Office
  PERFORM append_fieldcat USING 'VKBUR'
                                ''
                                ''
                                'Sales Office'
                                 space  space  space
                                 gt_fieldcat[].
* Sales Group
  PERFORM append_fieldcat USING 'VKGRP'
                                ''
                                ''
                                'Sales Group'
                                 space  space  space
                                 gt_fieldcat[].
* Sale man
  PERFORM append_fieldcat USING 'EMP_NAME_EN'
                                ''
                                ''
                                'Sale man'
                                 space  space  space
                                 gt_fieldcat[].
* Customer ID
  PERFORM append_fieldcat USING 'KUNNR'
                                ''
                                ''
                                'Customer ID'
                                 space  space  space
                                 gt_fieldcat[].
* Account name thai
  PERFORM append_fieldcat USING 'ACC_NAME_TH'
                                ''
                                ''
                                'Account name thai'
                                 space  space  space
                                 gt_fieldcat[].
* SFDC Enquiry No
  PERFORM append_fieldcat USING 'SFDC_ENQ'
                                ''
                                ''
                                'SFDC Enquiry No'
                                 space  space  space
                                 gt_fieldcat[].
* SFDC Quote no
  PERFORM append_fieldcat USING 'SFDC_QUO'
                                ''
                                ''
                                'SFDC Quote no'
                                 space  space  space
                                 gt_fieldcat[].
* ERP Quote no
  PERFORM append_fieldcat USING 'ERP_QUO'
                                ''
                                ''
                                'ERP Quotation no'
                                 space  space  space
                                 gt_fieldcat[].
* ERP SO
  PERFORM append_fieldcat USING 'ERP_SO'
                                ''
                                ''
                                'ERP SO'
                                 space  space  space
                                 gt_fieldcat[].
* ERP SO
  PERFORM append_fieldcat USING 'ERP_SO_ITEM'
                                ''
                                ''
                                'ERP SO Item'
                                 space  space  space
                                 gt_fieldcat[].
"Add by wantanee 20131218
* ERP INV No
  PERFORM append_fieldcat USING 'ERP_INV_NO'
                                ''
                                ''
                                'ERP Inv. No.'
                                 space  space  space
                                 gt_fieldcat[].
"End Add by wantanee 20131218

* ERP Material
  PERFORM append_fieldcat USING 'ERP_MATNR'
                                ''
                                ''
                                'ERP Material'
                                 space  space  space
                                 gt_fieldcat[].

* Product Group LV1
  PERFORM append_fieldcat USING 'PH1'
                                ''
                                ''
                                'Product Group LV1'
                                 space  space  space
                                 gt_fieldcat[].
* Product Group LV2
  PERFORM append_fieldcat USING 'PH2'
                                ''
                                ''
                                'Product Group LV2'
                                 space  space  space
                                 gt_fieldcat[].
* Product Group LV3
  PERFORM append_fieldcat USING 'PH3'
                                ''
                                ''
                                'Product Group LV3'
                                 space  space  space
                                 gt_fieldcat[].
* ship Date
  PERFORM append_fieldcat USING 'SHIP_DATE'
                                ''
                                ''
                                'Ship Date'
                                 space  space  space
                                 gt_fieldcat[].
* Currency Code
  PERFORM append_fieldcat USING 'CURRENCY_CODE'
                                ''
                                ''
                                'Currency Code'
                                 space  space  space
                                 gt_fieldcat[].
* Sale Amount
  PERFORM append_fieldcat USING 'SALE_AMT'
                                ''
                                ''
                                'Sale Amount'
                                 space  space  space
                                 gt_fieldcat[].
* Actual QTY
  PERFORM append_fieldcat USING 'ACTUAL_QTY'
                                ''
                                ''
                                'Actual QTY'
                                 space  space  space
                                 gt_fieldcat[].

* Sales QTY
  PERFORM append_fieldcat USING 'SALE_QTY'
                                ''
                                ''
                                'Sales QTY'
                                 space  space  space
                                 gt_fieldcat[].

* Cost
  PERFORM append_fieldcat USING 'SHIP_COST'
                                ''
                                ''
                                'Cost'
                                 space  space  space
                                 gt_fieldcat[].
** Project Name  "add by Wantanee 20150311
*  PERFORM append_fieldcat USING 'SHIP_PRO_NAME'
*                                ''
*                                ''
*                                'Project Name'
*                                 space  space  space
*                                 gt_fieldcat[].

**Serial number
*  PERFORM append_fieldcat USING 'SERIAL'
*                                ''
*                                ''
*                                'Serial number'
*                                 space  space  space
*                                 gt_fieldcat[].


* SF Work order number
  PERFORM append_fieldcat USING 'WORK_ORDER'
                                ''
                                ''
                                'SF Work Order number'
                                 space  space  space
                                 gt_fieldcat[].

* SF contract number
  PERFORM append_fieldcat USING 'SERVICE_CONTRACT_NUMBER'
                                ''
                                ''
                                'SF Contract no'
                                 space  space  space
                                 gt_fieldcat[].



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
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_list
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
  wa_header-info = 'SFDC SHIPPED'.
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
*
**&---------------------------------------------------------------------*
**&      Form  export
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM export.
*
*  CALL FUNCTION 'ZFM_CREATE_FILE_FOR_SFDC_LC'
*    EXPORTING
*      IMP_FILENAME = p_path
*    TABLES
*      TBL_DATA     = gt_itab
*      TBL_FIELDCAT = tab_cat
*      TBL_RETURN   = tab_err.
*
*
*ENDFORM.                    "export
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

ENDFORM.                    " GET_PATH_NAME

*
**&---------------------------------------------------------------------*
**&      Form  cat_edit_rtn
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM cat_edit_rtn .
*
*
**
*  PERFORM append_rtn USING 'SHIPPED_UID' 'X' '100' ' '. "
*  PERFORM append_rtn USING 'COMP' 'X' '10' ' '. "
*  PERFORM append_rtn USING 'BRANCH' 'X' '10' ' '. "
*  PERFORM append_rtn USING 'VTWEG'   'X' '2' ' '. "
*  PERFORM append_rtn USING 'VKBUR'   'X' '4' ' '.
*  PERFORM append_rtn USING 'VKGRP'   'X' '2' ' '.
*  PERFORM append_rtn USING 'EMP_NAME_EN'   'X' '100' ' '.
*  PERFORM append_rtn USING 'KUNNR'   'X' '10' ' '.
*  PERFORM append_rtn USING 'ACC_NAME_TH'   'X' '255' ' '.
*  PERFORM append_rtn USING 'SFDC_ENG'   'X' '10' ' '.
*  PERFORM append_rtn USING 'SFDC_QUO'   'X' '10' ' '.
*  PERFORM append_rtn USING 'ERP_QUO'   'X' '10' ' '.
*  PERFORM append_rtn USING 'ERP_SO'   'X' '10' ' '.
*  PERFORM append_rtn USING 'ERP_SO_ITEM'   'X' '6' ' '.
*  PERFORM append_rtn USING 'ERP_INV_NO'   'X' '10' ' '.
*  PERFORM append_rtn USING 'ERP_MATNR'   'X' '18' ' '.
*  PERFORM append_rtn USING 'PH1'   'X' '5' ' '.
*  PERFORM append_rtn USING 'PH2'   'X' '5' ' '.
*  PERFORM append_rtn USING 'PH3'   'X' '1' ' '.
*  PERFORM append_rtn USING 'SHIP_DATE'   'X' '10' ' '.
*  PERFORM append_rtn USING 'CURRENCY_CODE'   'X' '5' ' '.
*  PERFORM append_rtn USING 'SALE_AMT'   'X' '100' ' '.
*  PERFORM append_rtn USING 'ACTUAL_QTY'   'X' '20' ' '.
*  PERFORM append_rtn USING 'SALE_QTY'   'X' '20' ' '.
*  PERFORM append_rtn USING 'SHIP_COST'   'X' '20' ' '.
*
*
*
**
*ENDFORM.                    "cat_edit_rtn
*
**&---------------------------------------------------------------------*
**&      Form  append_rtn
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_FN       text
**      -->P_TYPE     text
**      -->P_LENG     text
**      -->P_DEC      text
**----------------------------------------------------------------------*
*FORM append_rtn USING    p_fn
*                         p_type
*                         p_leng
*                         p_dec.
*
*  clear tab_cat.
*
*  tab_cat-fieldname = p_fn.             "
*  tab_cat-type      = p_type.           "
*  tab_cat-leng      = p_leng.           "
*  tab_cat-decimals  = p_dec.            "
*
*  append tab_cat.
*
*ENDFORM.                    "append_rtn
**&---------------------------------------------------------------------*
**&      Form  export_idoc
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_FN       text
**      -->P_TYPE     text
**      -->P_LENG     text
**      -->P_DEC      text
**----------------------------------------------------------------------*
*FORM export_idoc.
*
*  DATA: lv_filename TYPE rlgrap-filename.
*  CLEAR: lv_filename.
*
*  CONCATENATE '_' sy-datum sy-uzeit '.csv'
*              INTO lv_filename.
*
*
*
*
*  gwa_path-pathname = p_path.
*  gwa_path-filename = lv_filename.
*  gwa_edidd-segnam = 'ZSFS999'.
*  gwa_edidd-sdata = gwa_path.
*
*  insert gwa_edidd into gt_edidd index 1.
*
*  CLEAR: gwa_idoc_control.
*
*  select single * into gwa_edp13
*         from edp13
*         where mestyp = mestyp1.
*
*  gwa_idoc_control-mestyp = gwa_edp13-mestyp.
*  gwa_idoc_control-idoctp = gwa_edp13-idoctyp.
*  gwa_idoc_control-rcvpor = gwa_edp13-rcvpor.
*  gwa_idoc_control-rcvprn = gwa_edp13-rcvprn.
*  gwa_idoc_control-rcvprt = gwa_edp13-rcvprt.
*
*  call function 'MASTER_IDOC_DISTRIBUTE'
*    exporting
*      master_idoc_control                  = gwa_idoc_control
**     OBJ_TYPE                             = ''
**     CHNUM                                = ''
*    tables
*      communication_idoc_control           = gt_edidc
*      master_idoc_data                     = gt_edidd
*    exceptions
*      error_in_idoc_control                = 1
*      error_writing_idoc_status            = 2
*      error_in_idoc_data                   = 3
*      sending_logical_system_unknown       = 4
*      others                               = 5 .
*
*  commit work and wait.
*   READ TABLE gt_edidc into gwa_edidc index 1.
*    CALL FUNCTION 'EDI_DOCUMENT_DEQUEUE_LATER'
*      EXPORTING
*        docnum                 = gwa_edidc-docnum
*      EXCEPTIONS
*        idoc_is_not_to_dequeue = 1.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT  "Add by Wantanee 20150311
*&---------------------------------------------------------------------*
FORM read_text_head  USING    p_id
                         p_object
                         p_vbeln
                CHANGING p_value.

  DATA: it_lines TYPE STANDARD TABLE OF tline.
  DATA: wa_lines LIKE LINE OF it_lines.
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
    LOOP AT it_lines INTO wa_lines.
      CONCATENATE p_value  wa_lines-tdline INTO p_value.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_RESULT.

  LS_HEADER-FIELD1 = LC_CON-FIELD1.
  LS_HEADER-FIELD2 = LC_CON-FIELD2.
  LS_HEADER-FIELD3 = LC_CON-FIELD3.
  LS_HEADER-FIELD4 = LC_CON-FIELD4.
  LS_HEADER-FIELD5 = LC_CON-FIELD5.
  LS_HEADER-FIELD6 = LC_CON-FIELD6.
  LS_HEADER-FIELD7 = LC_CON-FIELD7.
  LS_HEADER-FIELD8 = LC_CON-FIELD8.
  LS_HEADER-FIELD9 = LC_CON-FIELD9.
  LS_HEADER-FIELD10 = LC_CON-FIELD10.
  LS_HEADER-FIELD11 = LC_CON-FIELD11.
  LS_HEADER-FIELD12 = LC_CON-FIELD12.
  LS_HEADER-FIELD13 = LC_CON-FIELD13.
  LS_HEADER-FIELD14 = LC_CON-FIELD14.
  LS_HEADER-FIELD15 = LC_CON-FIELD15.
  LS_HEADER-FIELD16 = LC_CON-FIELD16.
  LS_HEADER-FIELD17 = LC_CON-FIELD17.
  LS_HEADER-FIELD18 = LC_CON-FIELD18.
  LS_HEADER-FIELD19 = LC_CON-FIELD19.
  LS_HEADER-FIELD20 = LC_CON-FIELD20.
  LS_HEADER-FIELD21 = LC_CON-FIELD21.
  LS_HEADER-FIELD22 = LC_CON-FIELD22.
  LS_HEADER-FIELD23 = LC_CON-FIELD23.
  LS_HEADER-FIELD24 = LC_CON-FIELD24.
  LS_HEADER-FIELD25 = LC_CON-FIELD25.
  LS_HEADER-FIELD26 = LC_CON-FIELD26.
  LS_HEADER-FIELD27 = LC_CON-FIELD27.
  LS_HEADER-FIELD28 = LC_CON-FIELD28.
  APPEND LS_HEADER TO LT_HEADER.

  "Z_DEMO_GEN_FILE
  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_HEADER    = LT_HEADER
                                                                I_ITEM      = GT_RESULT
                                                                I_SEPARATOR = '","'
                                                                I_START_END_VALUE = '"').
  PERFORM F_EXPORT_TO_SERVER.
ENDFORM.                    " F_GET_RESULT
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
  DATA: LV_PATH_FILE TYPE string.



LV_PATH_FILE = p_path.

  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.


  LV_PATH_FILE = P_PATH.
  CONCATENATE 'Shipped_' SY-DATUM  sy-timlo '.csv' INTO LV_PATH.
      LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                       I_AL11_PATH   = '/tmp'
                                        I_AL11_PATH   = LV_PATH_FILE
                                        I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
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
