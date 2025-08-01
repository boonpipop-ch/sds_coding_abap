class ZCL_SDSCM_PROC_SRVORDER_FWD definition
  public
  inheriting from CL_CRMS4_PROC_SRVORDER_FWD
  final
  create public .

public section.
protected section.

  methods PREPARE_PURCH_REQ_SDS
    importing
      !IS_SRVORDER_DATA type CRMS4S_DATAEXCH_BTX
      !IT_TASK_INFO type CRMT_SRV_LOG_TASK_INFO_T
    exporting
      !EV_NEW_PR type CHAR1
      !ET_PROCESS_INFO type CRMT_SRV_LOG_PROCESS_INFO_T
      !ET_RETURN type BAPIRET2_T
      !ET_ITEM_ERRORS type CRMS4T_BIL_ERRORS
      !ES_HEADER type BAPIMEREQHEADER
      !ES_HEADER_X type BAPIMEREQHEADERX
      !ET_ITEM type TY_BAPIMEREQITEMIMP
      !ET_ITEM_X type TY_BAPIMEREQITEMX
      !ET_SOURCE type TY_BAPIMEREQSOURCE
      !ET_ACC type TY_BAPIMEREQACCOUNT
      !ET_ACC_X type TY_BAPIMEREQACCOUNTX
      !ET_SERVICELINES type BAPI_SRV_SERVICE_LINE_TTY
      !ET_SERVICELINESX type BAPI_SRV_SERVICE_LINEX_TTY
      !ET_SERVICEACCOUNT type BAPI_SRV_ACC_DATA_TTY
      !ET_SERVICEACCOUNTX type BAPI_SRV_ACC_DATAX_TTY
      !ET_HEADER_TEXT type TY_BAPIMEREQHEADTEXT
      !ET_ITEM_TEXT type TY_BAPIMEREQITEMTEXT
      !ET_ADDR type TY_BAPIMERQADDRDELIVERY .

  methods PREPARE_PURCH_REQ
    redefinition .
private section.

  data ADVS_DOUBLE type ref to LIF_ADV_SHIPMENT .
  constants GC_SERVICE type PRODUCT_TYPE value '2' ##NO_TEXT.
  constants GC_SALES_ITEM type CRMT_SWO_OBJTYP_PROCESS_ITM_DB value 'BUS2000131' ##NO_TEXT.

  methods GET_DOCUMENT_SETTINGS
    importing
      !IV_DOC_OBJ type BSTYP
    exporting
      !EV_DOC_TYPE type BSART
      !EV_INCR type PINCR .
  methods GET_BP
    importing
      !IV_ITEM_GUID type CRMT_OBJECT_GUID
      !IT_PARTNER type CRMT_PARTNER_EXTERNAL_WRKT
      !IV_PTNR_FUNCTION type COMT_PARTNER_PFT
    returning
      value(RV_BP) type CHAR12 .
  methods SET_POS_NO
    importing
      !IV_ITEM_GUID type CRMT_OBJECT_GUID
      !IV_ITEM_NO type CRMT_ITEM_NO
      !IV_LOOP_INDEX type I
      !IV_INCR type PINCR
    changing
      value(CV_POS_NO) type BNFPO .
  methods MAP_CRM_TEXT_PR
    importing
      !IV_PR_DOC_NO type BANFN
      !IV_PR_POS_NO type BNFPO
      !IT_TEXT type CRMT_TEXT_WRKT
      !IV_HEADER_DESCRIPTION type TDLINE
    exporting
      !ET_HEADER_TEXT type TY_BAPIMEREQHEADTEXT
      !ET_ITEM_TEXT type TY_BAPIMEREQITEMTEXT .
  methods CHECK_MATERIAL_EXIST
    importing
      !LV_MATERIAL type MATNR
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods GET_PRICE_FOR_MATERIAL
    importing
      !IV_PLANT type WERKS_D
      !IV_MATERIAL type MATNR
    exporting
      !EV_PRICE type BAPICUREXT
      !EV_PRICE_UNIT type EPEIN
      !EV_CURRENCY type WAERS .
  methods GET_PURCHASING_GROUP
    importing
      !IV_MATERIAL_GROUP type MATKL
      !IV_MATERIAL type MATNR optional
      !IV_PLANT type EWERK optional
    exporting
      !ET_RETURN type BAPIRET2_T
    returning
      value(RV_PURCHASING_GROUP) type EKGRP .
  methods CHECK_SERVICE_EXIST
    importing
      !IV_SERVICE type ASNUM
    returning
      value(RV_SERVICE_EXIST) type ABAP_BOOL .
  methods GET_PRIDOC_PRICE
    importing
      !IT_PRIDOC type CRMT_PRIC_COND_T
      !IS_ORDERADM_I type CRMT_ORDERADM_I_WRK
      !IS_HEADER type CRMT_ORDERADM_H_WRK
    exporting
      !EV_PRICE type BAPICUREXT
      !EV_PRICE_UNIT type EPEIN
      !EV_CURRENCY type WAERS .
  methods IDENTIFY_QUOTATION
    importing
      !IV_PROCESS_TYPE type CRMT_PROCESS_TYPE_DB
    exporting
      !EV_STAT_QUOT type ABAP_BOOL .
  methods GET_PRICE_FROM_PREDECESSOR
    importing
      !IV_HEADER_GUID type CRMT_OBJECT_GUID
      !IS_ORDERADM_I type CRMT_ORDERADM_I_WRK
    exporting
      !EV_PRICE type BAPICUREXT
      !EV_PRICE_UNIT type EPEIN
      !EV_CURRENCY type WAERS .
ENDCLASS.



CLASS ZCL_SDSCM_PROC_SRVORDER_FWD IMPLEMENTATION.


  METHOD CHECK_MATERIAL_EXIST.

* DATA:lv_material  TYPE matnr.

*    lv_material = iv_product.
**material number conversion
*    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*      EXPORTING
*        input        = iv_product
*      IMPORTING
*        output       = lv_material
*      EXCEPTIONS
*        length_error = 1
*        OTHERS       = 2.
*    IF sy-subrc NE 0.
**   In case of error tring to take as is..
*      lv_material = iv_product.
*    ENDIF.

    DATA:lv_del_flag TYPE char1,
         ls_return   TYPE bapireturn1.

    TEST-SEAM material_exists.
      CALL FUNCTION 'BAPI_MATERIAL_EXISTENCECHECK'
        EXPORTING
          material_long = lv_material
        IMPORTING
          deletion_flag = lv_del_flag
          return        = ls_return.
    END-TEST-SEAM.

    IF ls_return-type = 'S'.
      rv_exists = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD CHECK_SERVICE_EXIST.

    DATA: lt_return   TYPE TABLE OF bapiret2,
          ls_return   TYPE bapiret2,
          ls_srv_data TYPE bapisrv.

    rv_service_exist = 'X'.
    TEST-SEAM get_service_detail.
      CALL FUNCTION 'BAPI_SERVICE_GET_DETAIL'
        EXPORTING
          servicenumber      = iv_service
          servicetexts       = space
        IMPORTING
          servicegeneraldata = ls_srv_data
        TABLES
          return             = lt_return.
    END-TEST-SEAM.
    LOOP AT lt_return INTO ls_return
      WHERE
        type EQ 'A' OR type EQ 'E'.

      IF sy-subrc EQ 0.
        rv_service_exist = ' '.
        EXIT.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_BP.

    INCLUDE com_partner_pft3.

    DATA: ls_partner TYPE crmt_partner_external_wrk,
*          ls_partner1 TYPE crmt_srv_inf_all_partner,
          ls_bp      TYPE crmt_srv_inf_all_partner.

    DATA:
      lv_r3_id        TYPE char10,
      lv_bp_id        TYPE pernr_d,
      lv_employee     TYPE char12,
      ls_vendor       TYPE crmm_but_vendno,
      lt_employee_id  TYPE crmt_employee_idt,
      ls_employee_id  TYPE crmt_employee_ids,
      lt_partner_guid TYPE bu_partner_guid_t,
      lt_customers    TYPE cvis_cust_link_t.

    DATA: lt_vendors   TYPE cvis_vend_link_t.
    FIELD-SYMBOLS: <fs_vendors> TYPE cvi_vend_link.

    FIELD-SYMBOLS: <fs_customer> TYPE cvi_cust_link.


    READ TABLE it_partner INTO ls_partner WITH KEY ref_guid = iv_item_guid
                                                partner_pft = iv_ptnr_function.

    IF sy-subrc = 0.
      READ TABLE gt_bp INTO ls_bp WITH KEY guid      = iv_item_guid
                                   bp_function_type  = iv_ptnr_function
                                        partner_guid = ls_partner-bp_partner_guid.
      IF sy-subrc <> 0.

        ls_bp-guid = iv_item_guid.
        ls_bp-bp_crm_id = ls_partner-ref_partner_no.
        ls_bp-bp_function_type = iv_ptnr_function.
        ls_bp-partner_guid = ls_partner-bp_partner_guid.

*-----------------------------------------------------------------------
* check if we could obtain a S4 number
*-----------------------------------------------------------------------

        CASE iv_ptnr_function.

          WHEN gc_partner_pft-vendor_proposal OR
               gc_partner_pft-vendor.

*       search for S4 vendor number
         CLEAR: lt_partner_guid,
                lt_vendors.

            APPEND ls_partner-bp_partner_guid TO lt_partner_guid.

            cvi_mapper=>get_instance( )->get_assigned_vendors_for_bps(
           EXPORTING
             i_partner_guids = lt_partner_guid
           RECEIVING
             r_vendors     = lt_vendors ).

            READ TABLE lt_vendors ASSIGNING <fs_vendors> INDEX 1.
            IF sy-subrc = 0.
              ls_bp-bp_r3_id = <fs_vendors>-vendor.
            ENDIF.

          WHEN gc_partner_pft-employee OR
               gc_partner_pft-responsible.

*       search for S4 HR number
            CALL FUNCTION 'CRM_CENTRALPERSON_GET'
              EXPORTING
                iv_bu_partner_guid  = ls_partner-bp_partner_guid
              IMPORTING
                et_employee_id      = lt_employee_id
              EXCEPTIONS
                no_central_person   = 1
                no_business_partner = 2
                no_id               = 3
                OTHERS              = 4.

            IF sy-subrc EQ 0.
              READ TABLE lt_employee_id INTO ls_employee_id INDEX 1.
              ls_bp-bp_r3_id = ls_employee_id-employeeid.
            ENDIF.

          WHEN OTHERS.
*       search for S4 customer ID

            CLEAR: lt_partner_guid,
                   lt_customers.

            APPEND ls_partner-bp_partner_guid TO lt_partner_guid.

            cvi_mapper=>get_instance( )->get_assigned_customers_for_bps(
           EXPORTING
             i_partner_guids = lt_partner_guid
           RECEIVING
             r_customers     = lt_customers ).

            READ TABLE lt_customers ASSIGNING <fs_customer> INDEX 1.
            IF sy-subrc = 0.
              ls_bp-bp_r3_id = <fs_customer>-customer.
            ENDIF.

        ENDCASE.

        IF ls_bp-bp_r3_id IS NOT INITIAL.
          APPEND ls_bp TO gt_bp.
        ENDIF.

      ENDIF.

      IF ls_bp-bp_r3_id IS NOT INITIAL.

        IF iv_ptnr_function = gc_partner_pft-employee OR
           iv_ptnr_function = gc_partner_pft-responsible.

          lv_bp_id = ls_bp-bp_r3_id.


          CALL FUNCTION 'WFD_HR_GET_USER_FR_EMPLOYEE_DB'
            EXPORTING
              pernr = lv_bp_id
            IMPORTING
              user  = lv_employee.

          IF NOT lv_employee IS INITIAL.
            rv_bp = lv_employee.
          ENDIF.

        ELSE.

          rv_bp = ls_bp-bp_r3_id.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GET_DOCUMENT_SETTINGS.

    DATA:
      ls_t161c TYPE t161c,
      ls_t161  TYPE t161.

*   get the corresponding process type for the document type
    SELECT SINGLE * FROM t161c INTO ls_t161c
      WHERE bstyp = iv_doc_obj.

    IF sy-subrc EQ 0.
      ev_doc_type = ls_t161c-bsart.

*   get the position increment value
      SELECT SINGLE * FROM t161 INTO ls_t161
        WHERE bsart = ev_doc_type.

      IF sy-subrc EQ 0.
        ev_incr = ls_t161-pincr.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GET_PRICE_FOR_MATERIAL.

    DATA:
      ls_t001  TYPE t001,
      ls_t001k TYPE t001k,
      lt_mbew  TYPE mbew_tab,
      ls_mbew  TYPE mbew.


* get default currency
    CLEAR ev_currency.

* search for pricing information in R/3 from MBEW
    TEST-SEAM get_curr1.
      CALL FUNCTION 'MEX_CHECK_WERKS'
        EXPORTING
          im_werks = iv_plant
          im_bupru = 'X'
        IMPORTING
          ex_t001k = ls_t001k
          ex_t001  = ls_t001.
    END-TEST-SEAM.

* get currency from company code
    IF NOT ls_t001 IS INITIAL.
      ev_currency = ls_t001-waers.
    ENDIF.

* get pricing information for this material
    CALL FUNCTION 'MBEW_READ_WITH_MATNR'
      EXPORTING
        matnr                = iv_material
      TABLES
        mbew_tab             = lt_mbew
      EXCEPTIONS
        not_found            = 1
        lock_on_mbew         = 2
        lock_system_error    = 3
        enqueue_mode_changed = 4
        OTHERS               = 5.

    IF sy-subrc EQ 0.
      DELETE lt_mbew WHERE bwkey NE ls_t001k-bwkey.
      READ TABLE lt_mbew INTO ls_mbew INDEX 1.

*   price dependend from price control
      CASE ls_mbew-vprsv.
        WHEN 'S'.

          CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERN_9'
            EXPORTING
              currency        = ev_currency
              amount_internal = ls_mbew-stprs
            IMPORTING
              amount_external = ev_price.

        WHEN 'V'.
          CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERN_9'
            EXPORTING
              currency        = ev_currency
              amount_internal = ls_mbew-verpr
            IMPORTING
              amount_external = ev_price.
      ENDCASE.

      ev_price_unit = ls_mbew-peinh.

    ENDIF.

  ENDMETHOD.


  METHOD GET_PRICE_FROM_PREDECESSOR.
    INCLUDE crms4_srv_logistic_ext.

    DATA: lt_srv_doc_flow   TYPE crmt_doc_flow_wrkt,
          lt_sq_header_guid TYPE crmt_object_guid_tab,
          lv_pri_guid_h     TYPE crmt_object_guid,
          lv_pri_guid_i     TYPE crmt_object_guid,
          ls_pridoc         TYPE crmt_pric_cond,
          lt_pridoc         TYPE crmt_pric_cond_t,
          lv_kschl          TYPE crms4_kscha,
          ls_orderadm_i_wrk TYPE crmt_orderadm_i_wrk,
          ls_header         TYPE crmt_orderadm_h_wrk.

* read docflow and identify the service order document
    APPEND iv_header_guid TO lt_sq_header_guid.


    CALL FUNCTION 'CRM_DOC_FLOW_READ_OB'
      EXPORTING
        iv_header_guid  = iv_header_guid
        iv_item_guid    = is_orderadm_i-guid
      IMPORTING
        et_doc_flow_wrk = lt_srv_doc_flow.

    CHECK lt_srv_doc_flow IS NOT INITIAL.

    SORT lt_srv_doc_flow BY utctime DESCENDING.
    READ TABLE lt_srv_doc_flow INTO DATA(ls_doc_flow_h) WITH KEY objtype_a = gc_crmt_bus_obj-crm_header
                                                                 objkey_b  = iv_header_guid
                                                                 objtype_b = gc_crmt_bus_obj-crm_header
                                                                 brel_kind = 'A'.


    IF sy-subrc EQ 0.
      READ TABLE lt_srv_doc_flow INTO DATA(ls_doc_flow_i) WITH KEY objtype_a = gc_crmt_bus_obj-crm_spare_part
                                                                   objkey_b  = is_orderadm_i-guid
                                                                   objtype_b = gc_crmt_bus_obj-crm_spare_part
                                                                   brel_kind = 'B'
                                                                   relationid = ls_doc_flow_h-relationid.
      lv_pri_guid_h = ls_doc_flow_h-objkey_a.
      lv_pri_guid_i = ls_doc_flow_i-objkey_a .
      CALL FUNCTION 'CRM_PRIDOC_READ_OW'
        EXPORTING
          iv_header_guid = lv_pri_guid_h
*         iv_pd_handle   =
*         it_item_guid   =
          iv_item_guid   = lv_pri_guid_i
*         it_pridoc      =
        IMPORTING
          es_pridoc      = ls_pridoc.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'CRM_ORDERADM_I_READ_OW'
          EXPORTING
            iv_guid           = lv_pri_guid_i
          IMPORTING
            es_orderadm_i_wrk = ls_orderadm_i_wrk
          EXCEPTIONS
            item_not_found    = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        APPEND ls_pridoc TO lt_pridoc.
*      Fetch the PR price from the Pricing data information
        ls_header-guid = lv_pri_guid_h.
        CALL METHOD me->get_pridoc_price
          EXPORTING
            it_pridoc     = lt_pridoc
            is_orderadm_i = ls_orderadm_i_wrk
            is_header     = ls_header
          IMPORTING
            ev_price      = ev_price
            ev_price_unit = ev_price_unit
            ev_currency   = ev_currency.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD GET_PRIDOC_PRICE.

    DATA : ls_sales_item TYPE crmc_sales_item,
           lt_pric_cond  TYPE prct_cond_du_tab.

    FIELD-SYMBOLS : <ls_pridoc>    TYPE crmt_pric_cond,
                    <ls_pric_cond> TYPE prct_cond_du.
*     Read Customizing for Item Type
    CALL FUNCTION 'CRM_ORDER_SALES_ITEM_SELECT_CB'
      EXPORTING
        iv_item_type    = is_orderadm_i-itm_type
      IMPORTING
        es_sales_item   = ls_sales_item
      EXCEPTIONS
        entry_not_found = 1
        OTHERS          = 2.
    IF sy-subrc IS INITIAL AND ls_sales_item-kschl IS NOT INITIAL.
*      condition type found in cutomizing
      READ TABLE it_pridoc ASSIGNING <ls_pridoc> WITH KEY guid = is_header-guid.
      IF sy-subrc IS INITIAL.
        lt_pric_cond = <ls_pridoc>-pric_cond.
        LOOP AT lt_pric_cond ASSIGNING <ls_pric_cond>.
          CHECK <ls_pric_cond>-kposn = is_orderadm_i-guid.
          CHECK <ls_pric_cond>-kschl = ls_sales_item-kschl.
          ev_price       = <ls_pric_cond>-kbetr .
          ev_price_unit  = <ls_pric_cond>-kpein .
          ev_currency    = <ls_pric_cond>-waers .
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD GET_PURCHASING_GROUP.

    DATA: ls_return_msg TYPE bapiret2,
          lv_msgv1      TYPE sy-msgv1.

    CLEAR: rv_purchasing_group, et_return.

*set purchasing group
    DATA(lr_purgrp_object) = cl_mm_pur_purchgroup_provider=>get_instance( ).

    CALL METHOD lr_purgrp_object->get_purchgroup_for_mat_data
      EXPORTING
        iv_material              = iv_material
        iv_plant                 = iv_plant
        iv_materialgroup         = iv_material_group
      IMPORTING
        ev_purchgroup_mat_master = DATA(lv_purch_grp)
        et_purchasinggroup       = DATA(lt_purch_grp).

* When the Purchasing grp is read successfully from Material Master.
    IF NOT lv_purch_grp IS INITIAL.
      rv_purchasing_group = lv_purch_grp.
*When read from material master is unsuccessful and Purchasing Grp is read from SSC UI customizing.
    ELSEIF lines( lt_purch_grp ) = 1.
*If the table returns either 0 or multiple entries, Purchasing grp will not be filled  and is passed empty.
      READ TABLE lt_purch_grp INTO lv_purch_grp INDEX '1'.
      rv_purchasing_group = lv_purch_grp.
    ENDIF.

*create error msg if no purchase group is found
    IF rv_purchasing_group IS INITIAL.
      lv_msgv1  = iv_material_group.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = 'E'
          cl     = 'CRM_SRV_LOG_EXT_OLTP'
          number = '002'
          par1   = lv_msgv1
        IMPORTING
          return = ls_return_msg.
      APPEND ls_return_msg TO et_return.
    ENDIF.

  ENDMETHOD.


  METHOD IDENTIFY_QUOTATION.

    CALL FUNCTION 'CRM_ORDER_QUOTATION_STATUS_CB'
      EXPORTING
        iv_process_type = iv_process_type
      IMPORTING
        ev_stat_quot    = ev_stat_quot.

  ENDMETHOD.


  METHOD MAP_CRM_TEXT_PR.

   DATA:
      ls_item_text   TYPE bapimereqitemtext,
      ls_header_text TYPE bapimereqheadtext,
      ls_text        TYPE LINE OF crmt_text_wrkt,
      ls_lines       TYPE LINE OF comt_text_lines_t,
      ls_text_id     TYPE crmc_text,
      lv_r3object    TYPE tdobject.

    LOOP AT it_text INTO ls_text.

*     get the crm object and text id
      ls_text_id-crmobject = ls_text-stxh-tdobject.
      ls_text_id-crmid     = ls_text-stxh-tdid.

*     determine R/3 equivalanet level (header/item) object
      CASE ls_text_id-crmobject.
        WHEN 'CRM_ORDERH'.
          lv_r3object = 'EBANH'.
        WHEN 'CRM_ORDERI'.
          lv_r3object = 'EBAN'.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

*     look for record in the customizing table
      TEST-SEAM pr_text.
        SELECT SINGLE * FROM crmc_text                      "#EC *
          INTO
            ls_text_id
          WHERE
            crmobject = ls_text_id-crmobject AND
            crmid     = ls_text_id-crmid AND
            r3object  = lv_r3object.
      END-TEST-SEAM.

*     record found
      IF sy-subrc EQ 0.

        IF ls_text_id-r3object EQ 'EBAN' AND ls_text-lines IS NOT INITIAL.
*         set item text
          LOOP AT ls_text-lines INTO ls_lines.
            MOVE-CORRESPONDING ls_text TO ls_item_text.
            ls_item_text-preq_no   = iv_pr_doc_no.
            ls_item_text-preq_item = iv_pr_pos_no.
            ls_item_text-text_id   = ls_text_id-r3id.
*           map the lines
            LOOP AT ls_text-lines ASSIGNING FIELD-SYMBOL(<ls_item_lines>).
              ls_item_text-text_form = <ls_item_lines>-tdformat.
              ls_item_text-text_line = <ls_item_lines>-tdline.
              APPEND ls_item_text TO et_item_text.
            ENDLOOP.
            CLEAR: ls_lines.
          ENDLOOP.


        ELSEIF ls_text_id-r3object EQ 'EBANH'.
*         set header text
          MOVE-CORRESPONDING ls_text TO ls_header_text.
          ls_header_text-preq_no   = iv_pr_doc_no.
          ls_header_text-preq_item = 0.
          ls_header_text-text_id   = ls_text_id-r3id.
*           map the lines
          LOOP AT ls_text-lines ASSIGNING FIELD-SYMBOL(<ls_header_lines>).
            ls_header_text-text_form = <ls_header_lines>-tdformat.
            ls_header_text-text_line = <ls_header_lines>-tdline.
            APPEND ls_header_text TO et_header_text.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CHECK et_header_text IS INITIAL.
    SELECT SINGLE * FROM crmc_text                          "#EC *
            INTO
            ls_text_id
            WHERE
            crmobject = 'CRM_ORDERH' AND r3object  = 'EBANH'.

    CHECK sy-subrc = 0.
*      set header text with the description of the service order when the customizing for
*      header text of service order to the header test of Purchase req is nto existing.
    CLEAR: ls_header_text.
    ls_header_text-preq_no     = iv_pr_doc_no.
    ls_header_text-preq_item   = 0.
    ls_header_text-text_id     = 'B01'. "gc_header_text_id_b01.
    ls_header_text-text_line   = iv_header_description.
    APPEND ls_header_text TO et_header_text.

  ENDMETHOD.


METHOD PREPARE_PURCH_REQ.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSCM_PROC_SRVORDER_FWD->PREPARE_PURCH_REQ
*  Creation Date      : 13.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : CME018 & CME023
*  Description        : Method preparing PR created from SVO
*  Purpose            : - To update equipment no to Unloading Point (CME018
*                       - To determine GL Account from ZSDSCMC005
*  Copied from        : Inherit from SAP class CL_CRMS4_PROC_SRVORDER_FWD
*                       and update setting in view CRMS4V_EXCH_FCTR (SRVO/20)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  DATA:
    LF_SDS  TYPE  FLAG.


  TRY.
      CLEAR LF_SDS.
*     Get Sales Org
      DATA(LF_SALES_ORG_SD) = IS_SRVORDER_DATA-ORGMAN[ REF_GUID = IS_SRVORDER_DATA-ORDERADM_H[ 1 ]-GUID
                                                       REF_KIND = 'A' ]-SALES_ORG_SD. "#EC CI_SORTSEQ
*     Only for SDS related
      LF_SDS = ZCL_SDSCM_ENHANCEMENT=>IS_SDS( IF_VKORG = LF_SALES_ORG_SD ).
    CATCH CX_ROOT ##CATCH_ALL.
      CLEAR LF_SDS.
  ENDTRY.

* Use Standard Method for Non-SDS
  IF LF_SDS IS INITIAL.
*   Call Existing Logic from Super class
    CALL METHOD SUPER->PREPARE_PURCH_REQ
      EXPORTING
        IS_SRVORDER_DATA   = IS_SRVORDER_DATA
        IT_TASK_INFO       = IT_TASK_INFO
      IMPORTING
        EV_NEW_PR          = EV_NEW_PR
        ET_PROCESS_INFO    = ET_PROCESS_INFO
        ET_RETURN          = ET_RETURN
        ET_ITEM_ERRORS     = ET_ITEM_ERRORS
        ES_HEADER          = ES_HEADER
        ES_HEADER_X        = ES_HEADER_X
        ET_ITEM            = ET_ITEM
        ET_ITEM_X          = ET_ITEM_X
        ET_SOURCE          = ET_SOURCE
        ET_ACC             = ET_ACC
        ET_ACC_X           = ET_ACC_X
        ET_SERVICELINES    = ET_SERVICELINES
        ET_SERVICELINESX   = ET_SERVICELINESX
        ET_SERVICEACCOUNT  = ET_SERVICEACCOUNT
        ET_SERVICEACCOUNTX = ET_SERVICEACCOUNTX
        ET_HEADER_TEXT     = ET_HEADER_TEXT
        ET_ITEM_TEXT       = ET_ITEM_TEXT
        ET_ADDR            = ET_ADDR.
  ELSE.
*<-- Start of Insertion CME018 CME023 13.08.2024
*   Call Existing Logic from Super class
    CALL METHOD PREPARE_PURCH_REQ_SDS
      EXPORTING
        IS_SRVORDER_DATA   = IS_SRVORDER_DATA
        IT_TASK_INFO       = IT_TASK_INFO
      IMPORTING
        EV_NEW_PR          = EV_NEW_PR
        ET_PROCESS_INFO    = ET_PROCESS_INFO
        ET_RETURN          = ET_RETURN
        ET_ITEM_ERRORS     = ET_ITEM_ERRORS
        ES_HEADER          = ES_HEADER
        ES_HEADER_X        = ES_HEADER_X
        ET_ITEM            = ET_ITEM
        ET_ITEM_X          = ET_ITEM_X
        ET_SOURCE          = ET_SOURCE
        ET_ACC             = ET_ACC
        ET_ACC_X           = ET_ACC_X
        ET_SERVICELINES    = ET_SERVICELINES
        ET_SERVICELINESX   = ET_SERVICELINESX
        ET_SERVICEACCOUNT  = ET_SERVICEACCOUNT
        ET_SERVICEACCOUNTX = ET_SERVICEACCOUNTX
        ET_HEADER_TEXT     = ET_HEADER_TEXT
        ET_ITEM_TEXT       = ET_ITEM_TEXT
        ET_ADDR            = ET_ADDR.
*--> End of Insertion CHE018 CME023 13.08.2024
  ENDIF.

ENDMETHOD.


METHOD PREPARE_PURCH_REQ_SDS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSCM_PROC_SRVORDER_FWD=>PREPARE_PURCH_REQ_SDS
*  Creation Date      : 22.08.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is method to prepare PR data which created
*                       when release Service order item. It was copied from
*                       SAP Standard method then add custom code under
*                       CME018 Comment
*  Purpose            : To assign PR data
*  Copied from        : CL_CRMS4_PROC_SRVORDER_FWD=>PREPARE_PURCH_REQ
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

  INCLUDE CRMS4_SRV_LOGISTIC_EXT.

*Logic moved from FM: CRM_SRV_LOG_MAP_PREQ1

  DATA:
    LT_TASK_INFO           TYPE CRMT_SRV_LOG_TASK_INFO_T,
    LT_ITEM                TYPE CRMT_ORDERADM_I_WRKT,
    LS_ITEM                TYPE CRMT_ORDERADM_I_WRK,
    LT_SCHEDLIN            TYPE CRMT_SCHEDLIN_WRKT,
    LS_SCHEDLIN            TYPE CRMT_SCHEDLIN_WRK,
    LT_TEXT                TYPE CRMT_TEXT_WRKT,
    LT_STATUS              TYPE CRMT_STATUS_WRKT,
    LT_DOCFLOW             TYPE CRMT_DOC_FLOW_WRKT,
    LS_DOCFLOW             TYPE CRMT_DOC_FLOW_WRK,
    LS_DOCFLOW_ITEM        TYPE CRMT_DOC_FLOW_WRK,
    LT_PARTNER             TYPE CRMT_PARTNER_EXTERNAL_WRKT,
    LS_PARTNER             TYPE CRMT_PARTNER_EXTERNAL_WRK,
    LT_HEADER              TYPE CRMT_ORDERADM_H_WRKT,
    LS_HEADER              TYPE CRMT_ORDERADM_H_WRK,
    LS_SRVORDER_GROUP_DATA TYPE CRMS4S_DATAEXCH_BTX,
    LT_PRODUCT_I           TYPE CRMT_PRODUCT_I_WRKT,
    LS_PRODUCT_I           TYPE CRMT_PRODUCT_I_WRK,
    LT_ORGMAN              TYPE CRMT_ORGMAN_WRKT,
    LS_SERVICE_DOC_KEY     TYPE FCOS_SERVICE_DOC_KEY,
    LS_SERVICE_DOC         TYPE FCOS_SERVICE_DOC,
    LT_PRIDOC              TYPE CRMT_PRIC_COND_T,
    LV_STAT_QUOT           TYPE ABAP_BOOL,
    LV_MSGV1               TYPE SY-MSGV1,
    LV_ITEMNO              TYPE SY-MSGV1,
    LS_ORGMAN              TYPE  CRMT_ORGMAN_WRK.

**    outgoing data
  DATA:
    LS_PR_ITEM   TYPE BAPIMEREQITEMIMP,
    LS_PR_ITEM_X TYPE BAPIMEREQITEMX,
    LS_ACC       TYPE BAPIMEREQACCOUNT,
    LS_ACC_X     TYPE BAPIMEREQACCOUNTX,
    LS_SOURCE    TYPE BAPIMEREQSOURCE.


**    LOCAL data
  DATA:
    LV_DOC_TYPE        TYPE BSART,
    LV_DOC_NO          TYPE BANFN,
    LV_POS_NO          TYPE BNFPO,
    LV_INCR            TYPE PINCR,
    LV_DELIV_DATE      TYPE SY-DATUM,
    LV_DELIV_TIME      TYPE SY-UZEIT,
    LV_MATERIAL_EXIST  TYPE CHAR1,
    LV_SERVICE_EXIST   TYPE CHAR1,
    LV_MATERIAL        TYPE MATNR,
    LV_MATERIAL_SHORT  TYPE CHAR18,
    LV_LOOP_COUNT      TYPE I VALUE 0,
    LV_LOOP_INDEX      TYPE I VALUE 0,
    LT_GUID            TYPE CRMT_OBJECT_GUID_TAB,
    LV_CURRENCY        TYPE WAERS,
    LV_PRICE           TYPE BAPICUREXT,
    LV_PRICE_UNIT      TYPE PEINH,
    LV_CURRENCY_2      TYPE WAERS,
    LV_PRICE_2         TYPE BAPICUREXT,
    LV_PRICE_UNIT_2    TYPE PEINH,
    LV_CANCEL_FLAG     TYPE CHAR1,
    LV_CRM_CANCEL_FLAG TYPE CHAR1,
    LV_ITEM_GUID       TYPE CRMT_OBJECT_GUID,
    LS_MARA            TYPE MARA,
    LV_PARTNER         TYPE KUNNR,
    LT_PRICING         TYPE CRMT_PRICING_WRKT.

**    service handling
  DATA:
    LV_SERVICE      TYPE ASNUM,
    LS_SERVICE      TYPE BAPI_SRV_SERVICE_LINE,
    LS_SERVICE_X    TYPE BAPI_SRV_SERVICE_LINEX,
    LS_SRV_ACC      TYPE BAPI_SRV_ACC_DATA,
    LS_SRV_ACC_X    TYPE BAPI_SRV_ACC_DATAX,
    LV_PACKAGE      TYPE PACKNO,
    LV_OUTLINE      TYPE OUTLINE_NO,
    LV_SERIAL_NO    TYPE I VALUE 0,
    LV_SERVICE_NO   TYPE I VALUE 0,
    LV_PROCESS_TYPE TYPE CRMT_PROCESS_TYPE,
    LV_ITM_TYPE     TYPE CRMT_ITM_TYPE,
    LV_ACC_ASS      TYPE KNTTP.

**    error handling
  DATA:
    LS_RETURN_MSG   TYPE BAPIRET2,
    LV_ERROR_PARAM1 TYPE SYMSGV,
    LV_ERROR_PARAM2 TYPE SYMSGV,
    LT_ITEM_ERRORS  TYPE CRMS4T_BIL_ERRORS,
    LS_ITEM_ERRORS  LIKE LINE OF LT_ITEM_ERRORS.

**  PROCESS status variables
  DATA:
    LS_PROC_INFO TYPE CRMT_SRV_LOG_PROCESS_INFO,
    LS_TASK_INFO TYPE CRMT_SRV_LOG_TASK_INFO.

**    crm service accounting information
  DATA:
    LS_ACCOUNT_ASSIGNMENT TYPE IAOM_ACCOUNT_ASSIGNMENT,
    LS_ACCOUNT_INFO       TYPE IAOM_ADD_ACC_INFO.
  CONSTANTS: LC_ACCTASSCAT    TYPE C VALUE 'R'.

**    variables for sos determination
  DATA:
    LS_SEARCH    TYPE BBPS_SOS_SEARCH_BE_CRITERIA,
    LS_CONTRACTS TYPE BBPS_SOS_FOUND_BE_CONTRACT,
    LV_PURCH_ORG TYPE EKORG.

**  bp
  DATA:
    LV_REQUESTOR        TYPE CHAR12,
    LV_EXEC_SERVICE_ORG TYPE CHAR12,
    LV_PROPOSED_VENDOR  TYPE CHAR10,
    LV_FIXED_VENDOR     TYPE CHAR10,
    LV_PRICE_DATE       TYPE DATUM.

**    variables to read pr
  DATA:
    LS_EBAN TYPE EBAN,
    LS_T134 TYPE T134,
    LV_FLAG TYPE ABAP_BOOLEAN.

  FIELD-SYMBOLS: <LS_PRICING>  TYPE CRMT_PRICING_WRK.

  CLEAR: LV_FLAG.


**    badi handling
  CLASS CL_EXITHANDLER DEFINITION LOAD.
*<-- Start of Insertion CME018 22.08.2024
  ADVS_DOUBLE = NEW LCL_ADV_SHIPMENT( ).
*--> End of Insertion CME018 22.08.2024

**----------------------------------------------------------------------
**  determine general information
**-----------------------------------------------------------------------

*     copy task info table to local table
  LT_TASK_INFO = IT_TASK_INFO.

  DATA(LV_CLOUD) = CL_COS_UTILITIES=>IS_CLOUD( ).

*   initialize tables
  CLEAR:
    ET_ITEM, ET_ITEM_X, ET_SOURCE, ET_ACC, ET_ACC_X,
    ET_SERVICEACCOUNT, ET_SERVICEACCOUNTX.

*    determine document type to use, get item increment
  CALL METHOD GET_DOCUMENT_SETTINGS
    EXPORTING
      IV_DOC_OBJ  = GC_BSTYP-PURCH_REQ
    IMPORTING
      EV_DOC_TYPE = LV_DOC_TYPE
      EV_INCR     = LV_INCR.

**    -----------------------------------------------------------------------
*     set header fields
**    -----------------------------------------------------------------------

*docflow
  LT_DOCFLOW =  IS_SRVORDER_DATA-DOC_FLOW.
  LT_PRIDOC  =  IS_SRVORDER_DATA-PRIDOC.
  LT_PRICING =  IS_SRVORDER_DATA-PRICING.

  ES_HEADER-PR_TYPE   = LV_DOC_TYPE.
  ES_HEADER_X-PR_TYPE = ABAP_TRUE.

  ES_HEADER-AUTO_SOURCE   = ABAP_TRUE.
  ES_HEADER_X-AUTO_SOURCE = ABAP_TRUE.

** Fill CRM origin indicator
  ES_HEADER-CREATE_IND = GC_CRM_ORIGIN-PURCH_REQ.
  ES_HEADER_X-CREATE_IND = ABAP_TRUE.

**    -----------------------------------------------------------------------
**    pre-processing
**    -----------------------------------------------------------------------

**    check item task info, to determine if the overall
*    process task is create or update
  READ TABLE LT_TASK_INFO
    WITH KEY
      TASK = GC_TASK-UPDATE
      TRANSPORTING NO FIELDS.

  IF SY-SUBRC EQ 0.
    EV_NEW_PR = SPACE.
*      es_header-preq_no = iv_docno.
*-----------------------------------------------------------------------
*read PR through service order doc flow
*-----------------------------------------------------------------------
*      READ TABLE lt_docflow INTO ls_docflow_item WITH KEY obj_a COMPONENTS
*                                  objkey_a = ls_item-guid
*                                  objtype_b = gc_crmt_bus_obj-purch_req.
*      IF sy-subrc EQ 0.
*        READ TABLE lt_docflow INTO ls_docflow WITH KEY obj_a COMPONENTS
*                                   objkey_a = ls_header-guid
*                                   relationid = ls_docflow_item-relationid
*                                   objtype_b = gc_crmt_bus_obj-purch_req.
*        IF sy-subrc EQ 0.
*          lv_doc_no = ls_docflow-objkey_b."PR no
*          lv_pos_no = ls_docflow_item-objkey_b. "PR item no.
*        ENDIF.
*
*      ENDIF.
  ELSE.
    EV_NEW_PR = ABAP_TRUE.
  ENDIF.

**    check how many records we are dealing with
  DESCRIBE TABLE IS_SRVORDER_DATA-ORDERADM_I LINES LV_LOOP_COUNT.

*    if we have more than one record,
*      sort the task info table
*so that we first handle the updates and then the inserts
  IF LV_LOOP_COUNT GT 1 AND EV_NEW_PR NE ABAP_TRUE.
    SORT LT_TASK_INFO STABLE ASCENDING BY TASK.
  ENDIF.

*    DO lv_loop_count TIMES.

  LOOP AT LT_TASK_INFO INTO LS_TASK_INFO.

*      count no of loops performed
    LV_LOOP_INDEX = LV_LOOP_INDEX + 1.

**-----------------------------------------------------------------------
**         collect the data for this specific item
**-----------------------------------------------------------------------
    CLEAR: LV_POS_NO, LV_ITEM_GUID.

*    add header guid for group data filter.
    LT_HEADER = IS_SRVORDER_DATA-ORDERADM_H.
    READ TABLE LT_HEADER INTO LS_HEADER INDEX 1.
    IF SY-SUBRC EQ 0.
      INSERT LS_HEADER-GUID INTO TABLE LT_GUID.
      LV_PROCESS_TYPE = LS_HEADER-PROCESS_TYPE  .
    ENDIF.
*     add specific group item guid to guid tab
    LV_ITEM_GUID = LS_TASK_INFO-ITEM_GUID.
    INSERT LV_ITEM_GUID INTO TABLE LT_GUID.


*      CALL FUNCTION 'CRM_SRV_LOG_GROUP_FILTER'
*        EXPORTING
*          it_guid     = lt_guid
*          it_item     = it_item[]
*          it_schedlin = it_schedlin[]
*          it_docflow  = it_docflow[]
*          it_partner  = it_partner[]
*          it_text     = it_text[]
*          it_status   = it_status[]
*        IMPORTING
*          es_item     = ls_item
*          es_schedlin = ls_schedlin
*        TABLES
*          et_docflow  = lt_docflow
*          et_partner  = lt_partner
*          et_text     = lt_text
*          et_status   = lt_status.

*-----------------------------------------------------------------------
*   collect the data for the specific item
*-----------------------------------------------------------------------
    CALL METHOD FILTER_SRV_GROUPS
      EXPORTING
        IT_GROUP_GUIDS           = LT_GUID
        IS_SRV_DATA_BTX          = IS_SRVORDER_DATA
      IMPORTING
        ES_SRV_GROUP_FILTER_DATA = LS_SRVORDER_GROUP_DATA.

    CLEAR: LT_GUID,  LT_STATUS,  LT_DOCFLOW, LT_HEADER, LT_PRODUCT_I,
           LT_PARTNER, LT_SCHEDLIN,  LT_STATUS,  LT_ITEM,  LS_ITEM,  LS_SCHEDLIN, LT_TEXT.
*item
    LT_ITEM   = LS_SRVORDER_GROUP_DATA-ORDERADM_I.
    READ TABLE LT_ITEM INTO LS_ITEM INDEX 1.

    IF SY-SUBRC EQ 0.
      LV_ITM_TYPE = LS_ITEM-ITM_TYPE .
    ENDIF.

    CHECK LS_HEADER IS NOT INITIAL AND LS_ITEM IS NOT INITIAL.
* status
    LT_STATUS = LS_SRVORDER_GROUP_DATA-STATUS.
*orgman
    LT_ORGMAN = LS_SRVORDER_GROUP_DATA-ORGMAN.
*    READ TABLE lt_orgman INTO ls_orgman WITH KEY guid = ls_item-guid.

*schedule line
    LT_SCHEDLIN = LS_SRVORDER_GROUP_DATA-SCHEDLIN.
    READ TABLE LT_SCHEDLIN INTO LS_SCHEDLIN INDEX 1.

*partner
    LT_PARTNER  = LS_SRVORDER_GROUP_DATA-PARTNER.

*docflow
    LT_DOCFLOW =  LS_SRVORDER_GROUP_DATA-DOC_FLOW.

*product_i
    LT_PRODUCT_I =  LS_SRVORDER_GROUP_DATA-PRODUCT_I.

**Text
    LT_TEXT =  LS_SRVORDER_GROUP_DATA-TEXT.

    READ TABLE LT_PRODUCT_I INTO LS_PRODUCT_I INDEX 1.
* material number conversion
    CL_CRMS4_PROD_API_FACTORY=>CREATE_INSTANCE( )->GET_SINGLE_HEADER_BY_GUID(
        EXPORTING
          IV_GUID                 = LS_ITEM-PRODUCT
          IV_CHECK_AUTHORIZATIONS = ABAP_FALSE
        IMPORTING
          ES_PRODUCT              = DATA(LS_PRODUCT)
        EXCEPTIONS
          OTHERS                  = 1
    ).
    IF SY-SUBRC IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = LS_PRODUCT-PRODUCT
        IMPORTING
          OUTPUT       = LV_MATERIAL
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC IS INITIAL.
        CALL METHOD CL_CRM_FLE_BASIC_MAPPER=>MOVE_CHAR40_TO_CHAR18_40
          EXPORTING
            IV_MATNR_LONG  = LV_MATERIAL
          IMPORTING
            EV_MATNR_SHORT = LV_MATERIAL_SHORT
            EV_MATNR_LONG  = LV_MATERIAL.
      ELSE.
        LV_MATERIAL = LS_ITEM-ORDERED_PROD.
      ENDIF.
    ELSE.
*     In case of error tring to take as is..
      LV_MATERIAL = LS_ITEM-ORDERED_PROD.
*               MESSAGE e018(crms4_product_api) RAISING not_found .
    ENDIF.
*      -----------------------------------------------------------------------
*         check for old version in case of update task
*      -----------------------------------------------------------------------

    IF LS_TASK_INFO-TASK EQ GC_TASK-UPDATE.

*     first of all, get the old logistic scenario

*-----------------------------------------------------------------------
*read PR through service order doc flow
*-----------------------------------------------------------------------
      READ TABLE LT_DOCFLOW INTO LS_DOCFLOW_ITEM WITH KEY OBJ_A COMPONENTS
                                  OBJKEY_A = LS_ITEM-GUID
                                  OBJTYPE_B = GC_CRMT_BUS_OBJ-PURCH_REQ.
      IF SY-SUBRC EQ 0.
        READ TABLE LT_DOCFLOW INTO LS_DOCFLOW WITH KEY OBJ_A COMPONENTS
                                   OBJKEY_A = LS_HEADER-GUID
                                   RELATIONID = LS_DOCFLOW_ITEM-RELATIONID
                                   OBJTYPE_B = GC_CRMT_BUS_OBJ-PURCH_REQ.
        IF SY-SUBRC EQ 0.
          LV_DOC_NO = LS_DOCFLOW-OBJKEY_B."PR no
          LV_POS_NO = LS_DOCFLOW_ITEM-OBJKEY_B. "PR item no.
        ENDIF.


*        CALL FUNCTION 'CRM_SRV_LOG_READ_SC_INFO_REC'       "#EC *
*          EXPORTING
*            iv_header_guid = is_header-process_guid
*            iv_item_guid   = ls_task_info-item_guid
*          IMPORTING
*            es_info_rec    = ls_rec_info_old
*          EXCEPTIONS
*            invalid_params = 1
*            rec_not_found  = 2
*            OTHERS         = 3.
*
**     convert params
*        MOVE ls_rec_info_old-doc_no TO lv_doc_no.
*        MOVE ls_rec_info_old-item_no TO lv_pos_no.

*      set pr no
        ES_HEADER-PREQ_NO = LV_DOC_NO.

*     read the old pr
        TEST-SEAM READ_REQ.
          CALL FUNCTION 'ME_READ_REQUISITION_EXT'
            EXPORTING
              I_BANFN                 = LV_DOC_NO
              I_BNFPO                 = LV_POS_NO
              I_NO_ACCOUNT_ASSIGNMENT = ABAP_TRUE            "1583491
            IMPORTING
              E_EBAN                  = LS_EBAN.
        END-TEST-SEAM.

*      check status

        IF ( NOT LS_EBAN-LOEKZ IS INITIAL ) OR
                 ( NOT LS_EBAN-BLCKD IS INITIAL ) OR
                 ( LS_EBAN-EBAKZ EQ ABAP_TRUE ) OR
                 ( LS_EBAN-STATU EQ 'B' )."PO created
*         CALL FUNCTION 'CRM_MESSAGE_COLLECT'
*            EXPORTING
*              iv_caller_name = gc_object_name-orderadm_i
*              iv_ref_object  = ls_item-guid
*              iv_msgno       = '017'
*              iv_msgid       = 'CRMS4_DATA_EXCH'
*              iv_msgty       = 'W'
*              iv_cumulate    = 'X'.
*
*          lv_msgv1 = ls_header-object_id.
*          lv_itemno =  ls_item-number_int.
*          SHIFT lv_itemno LEFT DELETING LEADING '0'.
*
*          CALL FUNCTION 'CRM_MESSAGE_COLLECT'
*            EXPORTING
*              iv_caller_name = gc_object_name-orderadm_i
*              iv_ref_object  = ls_item-guid
*              iv_msgno       = '039'
*              iv_msgid       = 'CRMS4_DATA_EXCH'
*              iv_msgty       = 'W'
*              iv_msgv1       = lv_msgv1
*              iv_msgv2       = lv_itemno
*              iv_cumulate    = 'X'.

          CONTINUE."   Raise message , skip item and continue.
        ENDIF.
      ENDIF."doc flow

*    check,
*    if we marked the item as cancelled already
      LOOP AT LT_STATUS
        TRANSPORTING NO FIELDS
        WHERE
         ( GUID   = LS_ITEM-GUID ) AND
         ( STATUS = GC_CRM_SYSTEM_STATUS-PREQ_COMPLETED ).
        LV_CRM_CANCEL_FLAG = ABAP_TRUE.
        EXIT.
      ENDLOOP.

      IF NOT LV_CRM_CANCEL_FLAG IS INITIAL.
*      skip item and continue
        CONTINUE.
      ENDIF.
    ENDIF.

*    -----------------------------------------------------------------------
*     set item pos no
*    -----------------------------------------------------------------------
    CALL METHOD SET_POS_NO
      EXPORTING
        IV_ITEM_GUID  = LS_ITEM-GUID
        IV_ITEM_NO    = LS_ITEM-NUMBER_INT
        IV_LOOP_INDEX = LV_LOOP_INDEX
        IV_INCR       = LV_INCR
      CHANGING
        CV_POS_NO     = LV_POS_NO.

*  -----------------------------------------------------------------------
*   get business partners
*  -----------------------------------------------------------------------

*     clear bps
    CLEAR:
      LV_REQUESTOR, LV_EXEC_SERVICE_ORG, LV_PROPOSED_VENDOR,
      LV_FIXED_VENDOR.

*    lv_item_guid = ls_task_info-item_guid.

    LV_REQUESTOR = GET_BP( IV_ITEM_GUID     = LV_ITEM_GUID
                           IT_PARTNER       = LT_PARTNER
                           IV_PTNR_FUNCTION = GC_PARTNER_PFT-RESPONSIBLE ).

    LV_EXEC_SERVICE_ORG = GET_BP( IV_ITEM_GUID     = LV_ITEM_GUID
                                  IT_PARTNER       = LT_PARTNER
                                  IV_PTNR_FUNCTION = GC_PARTNER_PFT-EXEC_SERVICE_ORG ).

    LV_PROPOSED_VENDOR = GET_BP( IV_ITEM_GUID     = LV_ITEM_GUID
                                 IT_PARTNER       = LT_PARTNER
                                 IV_PTNR_FUNCTION = GC_PARTNER_PFT-VENDOR_PROPOSAL ).

    LV_FIXED_VENDOR = GET_BP( IV_ITEM_GUID     = LV_ITEM_GUID
                              IT_PARTNER       = LT_PARTNER
                              IV_PTNR_FUNCTION = GC_PARTNER_PFT-VENDOR ).

    IF NOT LV_REQUESTOR IS INITIAL.
      LS_PR_ITEM-PREQ_NAME = LV_REQUESTOR.
      LS_PR_ITEM_X-PREQ_NAME = ABAP_TRUE.
    ENDIF.


*  -----------------------------------------------------------------------
*   delivery_date
*  -----------------------------------------------------------------------
    CALL FUNCTION 'CIF_GEN4_CONVERT_TIMESTAMP'
      EXPORTING
        IV_TIMESTAMP           = LS_SCHEDLIN-FROM_TIME
        IV_TIMEZONE            = LS_SCHEDLIN-FROM_TIMEZONE
      IMPORTING
        EV_DATE                = LV_DELIV_DATE
        EV_TIME                = LV_DELIV_TIME
      EXCEPTIONS
        TIME_CONVERSION_FAILED = 1
        OTHERS                 = 2.

    IF SY-SUBRC NE 0.
*  create error msg
      MOVE LS_ITEM-DESCRIPTION TO LV_ERROR_PARAM1.
      CALL METHOD COLLECT_ITEM_MESSAGES
        EXPORTING
          IV_MSGID       = 'CRM_SRV_LOG_EXT_OLTP'
          IV_MSGNO       = '000'
          IV_MSGTY       = 'E'
          IV_MSGV1       = LV_ERROR_PARAM1
          IV_ITEM_GUID   = LS_ITEM-GUID
        CHANGING
          CT_ITEM_ERRORS = ET_ITEM_ERRORS.

*  set delivery date to today, to avoid problems
*  an error msg has been set, so this is ok
      LV_DELIV_DATE = SY-DATUM.
    ENDIF.

    LS_PR_ITEM-PREQ_DATE = LV_DELIV_DATE.

*  -----------------------------------------------------------------------
*   accounting data:
*  -----------------------------------------------------------------------
*get account assignment

*Account assignment for cloud scenario where the purchase requisition is linked to service order.
*

    LS_SERVICE_DOC_KEY-SERVICE_DOC_ID      = LS_HEADER-OBJECT_ID.
    LS_SERVICE_DOC_KEY-SERVICE_DOC_TYPE    = LS_HEADER-PROCESS_TYPE.
    DATA(LC_BUNDLE_UTIL) = CL_CRMS4_SRV_BUNDLE_UTIL=>GET_INSTANCE( ).
    IF LS_ITEM-PARENT IS NOT INITIAL AND LC_BUNDLE_UTIL->IS_MAIN_ITEM_OF_FP_SB( LS_ITEM-PARENT ) EQ ABAP_TRUE.
*     for service part subitems in a service bundle with a fixed price, we transfer the main item number to the purchase requisition
      DATA(LO_ITEM_WRAPPER) = CL_CRMS4_ITEM_WRAPPER=>GET_INSTANCE( ).
      CALL METHOD LO_ITEM_WRAPPER->GET_ITEM_DETAILS
        EXPORTING
          IV_GUID       = LS_ITEM-PARENT
        IMPORTING
*         es_service_i  = DATA(ls_service_i)
          ES_ORDERADM_I = DATA(LS_MAIN_ORDERADM_I).
      LS_SERVICE_DOC_KEY-SERVICE_DOC_ITEM_ID = LS_MAIN_ORDERADM_I-NUMBER_INT.
      DATA(LV_IS_SB_FP_SUBITEM) = ABAP_TRUE.
    ELSE.
      LS_SERVICE_DOC_KEY-SERVICE_DOC_ITEM_ID = LS_ITEM-NUMBER_INT.
    ENDIF.

    TRY.
        CL_FCO_SRVDOC_API=>READ_SERVICE_DOCUMENT(
          EXPORTING
            IS_SERVICE_DOC_KEY = LS_SERVICE_DOC_KEY
          IMPORTING
            ES_SERVICE_DOC     = LS_SERVICE_DOC ).
      CATCH CX_FCO_SRVDOC_EXCEPTION.
    ENDTRY.

*    ELSE.
    TEST-SEAM ACCOUNT_ASSIGNMENT_PR.
      ##FM_SUBRC_OK
      CALL FUNCTION 'IAOM_GET_ACCOUNT_ASSIGNMENT'
        EXPORTING
          I_BUS_SCENARIO_ID          = GC_BUSINESS_SCENARIO_ID
          I_EXT_OBJECT_ID            = LS_TASK_INFO-ITEM_GUID
        IMPORTING
          E_ACCOUNT_ASSIGNMENT       = LS_ACCOUNT_ASSIGNMENT
          E_ADD_ACC_INFO             = LS_ACCOUNT_INFO
        EXCEPTIONS
          BUS_SCENARIO_UNKNOWN       = 1
          EXT_OBJECT_ID_UNKNOWN      = 2
          OBJECT_LOCKED              = 3
          EXT_OBJECT_NOT_CO_RELEVANT = 4
          POSTING_YEAR_MISSING       = 5
          PROF_SEGMENT_ERROR         = 6
          OTHER_ERROR                = 7
          OTHERS                     = 8.
    END-TEST-SEAM.

    IF LS_ACCOUNT_ASSIGNMENT IS INITIAL AND LS_SERVICE_DOC IS INITIAL.
      IF LV_IS_SB_FP_SUBITEM EQ ABAP_TRUE.
*  create specific error msg for service bundle fixed price subitem
        LO_ITEM_WRAPPER = CL_CRMS4_ITEM_WRAPPER=>GET_INSTANCE( ).
        CALL METHOD LO_ITEM_WRAPPER->GET_ITEM_DETAILS
          EXPORTING
            IV_GUID       = LS_ITEM-PARENT
          IMPORTING
            ES_ORDERADM_I = DATA(LS_PARENT_ORDERADM_I).
        DATA LV_NUMBER_INT TYPE INT4.
        LV_NUMBER_INT = LS_PARENT_ORDERADM_I-NUMBER_INT.
        LV_MSGV1 = LV_NUMBER_INT.

        CALL METHOD COLLECT_ITEM_MESSAGES
          EXPORTING
            IV_MSGID       = 'CRM_SRV_LOG_EXT_OLTP'
            IV_MSGNO       = '014'
            IV_MSGTY       = 'E'
            IV_MSGV1       = LV_MSGV1
            IV_ITEM_GUID   = LS_ITEM-GUID
          CHANGING
            CT_ITEM_ERRORS = ET_ITEM_ERRORS.


        CLEAR LV_IS_SB_FP_SUBITEM.
      ELSE.
*  create error msg
        CALL METHOD COLLECT_ITEM_MESSAGES
          EXPORTING
            IV_MSGID       = 'CRM_SRV_LOG_EXT_OLTP'
            IV_MSGNO       = '008'
            IV_MSGTY       = 'E'
            IV_ITEM_GUID   = LS_ITEM-GUID
          CHANGING
            CT_ITEM_ERRORS = ET_ITEM_ERRORS.
      ENDIF.
    ENDIF.
*    ENDIF.

*  If the Item belongs to Advance Shipment scenario then Account Assignment, SalesDocument and Sales Document Item are filled in, otherwise standard logic is executed
    IF ADVS_DOUBLE->IS_ADV_SHIPMENT_ACT( IV_ITEM_GUID = LS_ITEM-GUID ) = ABAP_TRUE.
      LS_PR_ITEM-ACCTASSCAT = 'H'.
      LS_ACC-SD_DOC         = LS_HEADER-OBJECT_ID. "Sales Order has same number as Service Order
      LS_ACC-ITM_NUMBER     = LS_ITEM-NUMBER_INT.  "Sales Order Item has same number as Service Order Item
      LS_ACC_X-SD_DOC       = ABAP_TRUE.
      LS_ACC_X-ITM_NUMBER   = ABAP_TRUE.
    ELSE.
*  accounting object:
*  internal order
      IF NOT LS_ACCOUNT_ASSIGNMENT-ORDER_NO IS INITIAL.
        LS_PR_ITEM-ACCTASSCAT = 'F'.
        LS_ACC-ORDERID        = LS_ACCOUNT_ASSIGNMENT-ORDER_NO.
        LS_ACC_X-ORDERID      = ABAP_TRUE.
      ENDIF.

*cost centre
      IF NOT LS_ACCOUNT_ASSIGNMENT-COST_CENTER IS INITIAL.
        LS_PR_ITEM-ACCTASSCAT = 'K'.
        LS_ACC-COSTCENTER     = LS_ACCOUNT_ASSIGNMENT-COST_CENTER.
        LS_ACC_X-COSTCENTER   = ABAP_TRUE.
      ENDIF.

*wbs (psp) element
      IF NOT LS_ACCOUNT_ASSIGNMENT-WBS_ELEMENT IS INITIAL.
        LS_PR_ITEM-ACCTASSCAT = 'P'.
        LS_ACC-WBS_ELEMENT    = LS_ACCOUNT_ASSIGNMENT-WBS_ELEMENT.
        LS_ACC_X-WBS_ELEMENT  = ABAP_TRUE.
      ENDIF.

*sales order/item
      IF NOT LS_ACCOUNT_ASSIGNMENT-SALES_DOCUMENT IS INITIAL.
        LS_PR_ITEM-ACCTASSCAT = 'C'.
        LS_ACC-SD_DOC         = LS_ACCOUNT_ASSIGNMENT-SALES_DOCUMENT.
        LS_ACC-ITM_NUMBER     = LS_ACCOUNT_ASSIGNMENT-SALES_DOCUMENT_POS.
        LS_ACC_X-SD_DOC       = ABAP_TRUE.
        LS_ACC_X-ITM_NUMBER   = ABAP_TRUE.
      ENDIF.

*network
      IF NOT LS_ACCOUNT_ASSIGNMENT-NETWORK IS INITIAL.
        LS_PR_ITEM-ACCTASSCAT = 'N'.
        LS_ACC-NETWORK        = LS_ACCOUNT_ASSIGNMENT-NETWORK.
        LS_ACC-ACTIVITY       = LS_ACCOUNT_ASSIGNMENT-ACTIVITY.
        LS_ACC_X-NETWORK      = ABAP_TRUE.
        LS_ACC_X-ACTIVITY     = ABAP_TRUE.
      ENDIF.

*  accounting object:   Service Order " Service order is my cost collector ( Cloud Scenario )
      IF LS_SERVICE_DOC IS NOT INITIAL.

        CALL METHOD CL_CRMS4_LOGINT_UTIL=>AUTO_ACC_ASSIGN_CAT_FOR_PR
          EXPORTING
            IV_PROCESS_TYPE       = LV_PROCESS_TYPE
            IV_ITM_TYPE           = LV_ITM_TYPE
*           iv_service_org        = ls_orgman-service_org
*           iv_service_org_resp   = ls_orgman-service_org_resp
*           iv_enterprise_service_org = ls_orgman-enterprise_service_org
          IMPORTING
            EV_ACC_ASSIGNMENT     = LV_ACC_ASS
            ET_RETURN             = DATA(LT_RETURN_TY)
          EXCEPTIONS
            NO_ENTRY_FOUND        = 0
            NO_UNIQUE_ENTRY_FOUND = 1
            ERROR_OCCURRED        = 2.
        MOVE-CORRESPONDING LT_RETURN_TY TO LT_ITEM_ERRORS.
        LOOP AT LT_ITEM_ERRORS INTO LS_ITEM_ERRORS.
          LS_ITEM_ERRORS-ITEM_GUID = LS_ITEM-GUID.
          APPEND LS_ITEM_ERRORS TO ET_ITEM_ERRORS.
        ENDLOOP.
*      CHECK  sy-subrc = 0.
        LS_PR_ITEM-ACCTASSCAT     = LV_ACC_ASS.
        LS_ACC-SERVICE_DOC        = LS_SERVICE_DOC-SERVICE_DOC_ID.
        LS_ACC-SERVICE_DOC_TYPE   = LS_SERVICE_DOC-SERVICE_DOC_TYPE.
        LS_ACC-SERVICE_ITEM       = LS_SERVICE_DOC-SERVICE_DOC_ITEM_ID.
        LS_ACC_X-SERVICE_DOC      = ABAP_TRUE.
        LS_ACC_X-SERVICE_DOC_TYPE = ABAP_TRUE.
        LS_ACC_X-SERVICE_ITEM     = ABAP_TRUE.
      ENDIF.
    ENDIF.

*set remaining accounting information
    LS_ACC-PREQ_ITEM    = LV_POS_NO.
    LS_ACC-PREQ_ITEM    = LV_POS_NO.
    LS_ACC_X-PREQ_ITEM  = LV_POS_NO.
    LS_ACC_X-PREQ_ITEMX = ABAP_TRUE.
    LS_PR_ITEM_X-ACCTASSCAT = ABAP_TRUE.

*  only for accounting main item number is used, now again normal item number of service part
    LS_SERVICE_DOC_KEY-SERVICE_DOC_ITEM_ID = LS_ITEM-NUMBER_INT.

*  -----------------------------------------------------------------------
*status handling
*  -----------------------------------------------------------------------

    CLEAR: LV_CANCEL_FLAG.

*check for status complete or cancelled
    LOOP AT LT_STATUS
    TRANSPORTING NO FIELDS
    WHERE
    ( GUID   = LS_TASK_INFO-ITEM_GUID ) AND
    ( STATUS = GC_CRM_SYSTEM_STATUS-COMPLETED OR
    STATUS = GC_CRM_SYSTEM_STATUS-CANCELLED ).
      LV_CANCEL_FLAG = ABAP_TRUE.
      EXIT.
    ENDLOOP.

    IF NOT LV_CANCEL_FLAG IS INITIAL.
*set closed and blocked flag
      LS_PR_ITEM-CLOSED   = ABAP_TRUE.
      LS_PR_ITEM_X-CLOSED = ABAP_TRUE.
      LS_PR_ITEM-REQ_BLOCKED   = '1'.
      LS_PR_ITEM_X-REQ_BLOCKED = ABAP_TRUE.
      LS_PR_ITEM-REASON_BLOCKING = TEXT-001.
      LS_PR_ITEM_X-REASON_BLOCKING = ABAP_TRUE.
    ENDIF.

*  -----------------------------------------------------------------------
*item data:
*  -----------------------------------------------------------------------
*    IF abap_true EQ cl_crms4_srv_bundle_util=>is_sub_item( iv_guid = ls_item-guid ). " for service bundle sub items take the real item no over to the PR, makes life easier
*      ls_pr_item-preq_item  = ls_item-number_int.
*    ELSE.
    LS_PR_ITEM-PREQ_ITEM  = LV_POS_NO.
*    ENDIF.
    LS_PR_ITEM-PREQ_DATE  = LV_DELIV_DATE.
    LS_PR_ITEM-SHORT_TEXT = LS_ITEM-DESCRIPTION.

*read plant and storage location
    DATA: LT_RETURN_PLANT TYPE BAPIRET2_T.
    TEST-SEAM READ_PLANT_PR.
      CALL METHOD GET_STORAGE_LOCATION_PLANT
        EXPORTING
          IS_ORDERADM_I       = LS_ITEM
          IT_ORGMAN           = LT_ORGMAN
          IT_PARTNERS         = LT_PARTNER
        IMPORTING
          EV_PLANT            = LS_PR_ITEM-PLANT
          EV_STORAGE_LOCATION = LS_PR_ITEM-STORE_LOC
          ET_RETURN           = LT_RETURN_PLANT.
      MOVE-CORRESPONDING LT_RETURN_PLANT TO LT_ITEM_ERRORS.
      LOOP AT LT_ITEM_ERRORS INTO LS_ITEM_ERRORS.
        LS_ITEM_ERRORS-ITEM_GUID = LS_ITEM-GUID.
        APPEND LS_ITEM_ERRORS TO ET_ITEM_ERRORS.
      ENDLOOP.
    END-TEST-SEAM.

*Delivery partner determination.
    READ TABLE LT_PARTNER
          WITH KEY
           REF_GUID    = LS_ITEM-GUID
           PARTNER_PFT = GC_PARTNER_PFT-INBOUND_DELIVERY_POINT
           MAINPARTNER = ABAP_TRUE
          INTO LS_PARTNER.
    IF SY-SUBRC NE 0.
      READ TABLE LT_PARTNER
        WITH KEY
          REF_GUID    = LS_ITEM-GUID
          PARTNER_PFT = GC_PARTNER_PFT-INBOUND_DELIVERY_POINT
            INTO LS_PARTNER.
    ENDIF.
    IF LS_ITEM-OBJECT_TYPE = GC_CRMT_BUS_OBJ-CRM_SERVICE.
      READ TABLE LT_PARTNER
           WITH KEY
            REF_GUID    = LS_ITEM-GUID
            PARTNER_FCT = GC_PARTNER_FNCT-SHIPTO_SERVICE_RECIPIENT
            MAINPARTNER = ABAP_TRUE
           INTO LS_PARTNER.
      IF SY-SUBRC NE 0.
        READ TABLE LT_PARTNER
          WITH KEY
            REF_GUID    = LS_ITEM-GUID
            PARTNER_FCT = GC_PARTNER_FNCT-SHIPTO_SERVICE_RECIPIENT
              INTO LS_PARTNER.
      ENDIF.
    ENDIF.
    IF NOT LS_PARTNER IS INITIAL.
*Ensure that the entered partner is a customer(as delivery address will be copied to PR only for customer).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_PARTNER-PARTNER_NO
        IMPORTING
          OUTPUT = LV_PARTNER.
      MOVE LV_PARTNER TO LS_PR_ITEM-CUSTOMER .
      LS_PR_ITEM_X-CUSTOMER = ABAP_TRUE.
    ENDIF.

    IF LS_TASK_INFO-TASK EQ GC_TASK-UPDATE. "Delivery address update from SO-PR: Only one address should be avaialble.Generally if we didnt pass any address by default address is calculated from plant.
      IF LS_EBAN-ADRN2 IS NOT INITIAL AND LS_PR_ITEM-CUSTOMER IS NOT INITIAL.
        " Address is already avaialble in the PR then Spare part recipent is added then clear the older address from PR and upate with customer address
        LS_PR_ITEM-ADDRESS = ''.
        LS_PR_ITEM_X-ADDRESS = 'X'.
      ELSEIF LS_EBAN-KUNNR IS NOT INITIAL AND LS_PR_ITEM-CUSTOMER IS INITIAL.
        " Customer address is already avaialble in the PR then Spare part recipent is cleared from SO then delete the older cust.addr from PR where default address will auto calculated by BAPI PR
        LS_PR_ITEM-CUSTOMER = ''.
        LS_PR_ITEM_X-CUSTOMER = 'X'.
      ENDIF.
    ENDIF.

*Is the Unit of measure equal to the base unit of measure?
    CLEAR: LS_MARA.

    CALL FUNCTION 'MARA_SINGLE_READ'
      EXPORTING
        MATNR  = LV_MATERIAL
      IMPORTING
        WMARA  = LS_MARA
      EXCEPTIONS
        OTHERS = 99.
    IF SY-SUBRC IS INITIAL.
      LS_PR_ITEM-MATL_GROUP = LS_MARA-MATKL.
* Item exists in MARA i.e. it's a material, not a service
      IF LS_PRODUCT_I-PROCESS_QTY_UNIT NE LS_MARA-MEINS.
*can the unit of measure be converted to the base uom?
        CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
          EXPORTING
            INPUT   = LS_SCHEDLIN-QUANTITY
            KZMEINH = 'X'
            MATNR   = LV_MATERIAL
            MEINH   = LS_PRODUCT_I-PROCESS_QTY_UNIT
            MEINS   = LS_MARA-MEINS
          IMPORTING
            OUTPUT  = LS_PR_ITEM-QUANTITY
          EXCEPTIONS
            OTHERS  = 99.
        IF SY-SUBRC <> 0 OR LS_PR_ITEM-QUANTITY IS INITIAL.
*Conversion to base uom is not possible
          MOVE LS_PRODUCT_I-PROCESS_QTY_UNIT TO LV_ERROR_PARAM1.
          MOVE LS_MARA-MEINS TO LV_ERROR_PARAM2.
          CALL METHOD COLLECT_ITEM_MESSAGES
            EXPORTING
              IV_MSGID       = 'KK'
              IV_MSGNO       = '407'
              IV_MSGTY       = 'E'
              IV_MSGV1       = LV_ERROR_PARAM1
              IV_MSGV2       = LV_ERROR_PARAM2
              IV_ITEM_GUID   = LS_ITEM-GUID
            CHANGING
              CT_ITEM_ERRORS = ET_ITEM_ERRORS.
*          IF 1 = 0. "just for where used
*            MESSAGE e407(kk) WITH lv_error_param1 lv_error_param2.
*          ENDIF.
        ELSE.
          LS_PR_ITEM-QUANTITY   = LS_PR_ITEM-QUANTITY.
          LS_PR_ITEM-UNIT       = LS_MARA-MEINS.
        ENDIF.
      ELSE.
        LS_PR_ITEM-QUANTITY   = LS_SCHEDLIN-QUANTITY.
        LS_PR_ITEM-UNIT       = LS_PRODUCT_I-PROCESS_QTY_UNIT.
      ENDIF.
    ELSE.
* Item is a service, not a material
      LS_PR_ITEM-QUANTITY   = LS_SCHEDLIN-QUANTITY.
      LS_PR_ITEM-UNIT       = LS_PRODUCT_I-PROCESS_QTY_UNIT.
    ENDIF.

*check if a material group was specified
    IF LS_PR_ITEM-MATL_GROUP IS INITIAL.
*create error msg
      MOVE LS_PR_ITEM-SHORT_TEXT TO LV_ERROR_PARAM1.
      CALL METHOD COLLECT_ITEM_MESSAGES
        EXPORTING
          IV_MSGID       = 'CRM_SRV_LOG_EXT_OLTP'
          IV_MSGNO       = '006'
          IV_MSGTY       = 'E'
          IV_MSGV1       = LV_ERROR_PARAM1
          IV_ITEM_GUID   = LS_ITEM-GUID
        CHANGING
          CT_ITEM_ERRORS = ET_ITEM_ERRORS.
    ENDIF.

*get the default purch. org.
*for the specified plant
    CALL FUNCTION 'ME_SELECT_EKORG_FOR_PLANT'
      EXPORTING
        I_WERKS                    = LS_PR_ITEM-PLANT
        I_STANDARD                 = 'X'
      IMPORTING
        E_EKORG                    = LS_PR_ITEM-PURCH_ORG
      EXCEPTIONS
        MORE_THAN_ONE_ORGANIZATION = 1
        NO_ENTRY_FOUND             = 2
        NO_DEFAULT_FOUND           = 3
        OTHERS                     = 4.

    IF SY-SUBRC NE 0.
*create error msg
      MOVE LS_PR_ITEM-PLANT TO LV_ERROR_PARAM1.
      CALL METHOD COLLECT_ITEM_MESSAGES
        EXPORTING
          IV_MSGID       = 'CRM_SRV_LOG_EXT_OLTP'
          IV_MSGNO       = '001'
          IV_MSGTY       = 'E'
          IV_MSGV1       = LV_ERROR_PARAM1
          IV_ITEM_GUID   = LS_ITEM-GUID
        CHANGING
          CT_ITEM_ERRORS = ET_ITEM_ERRORS.
    ENDIF.
    LV_PURCH_ORG = LS_PR_ITEM-PURCH_ORG.

*set purchasing group
    CALL METHOD ME->GET_PURCHASING_GROUP
      EXPORTING
        IV_MATERIAL         = LV_MATERIAL
        IV_PLANT            = LS_PR_ITEM-PLANT
        IV_MATERIAL_GROUP   = LS_PR_ITEM-MATL_GROUP
      IMPORTING
        ET_RETURN           = DATA(LT_RETURN)
      RECEIVING
        RV_PURCHASING_GROUP = LS_PR_ITEM-PUR_GROUP.
    MOVE-CORRESPONDING LT_RETURN TO LT_ITEM_ERRORS.
    LOOP AT LT_ITEM_ERRORS INTO LS_ITEM_ERRORS.
      LS_ITEM_ERRORS-ITEM_GUID = LS_ITEM-GUID.
      APPEND LS_ITEM_ERRORS TO ET_ITEM_ERRORS.
    ENDLOOP.

*  -----------------------------------------------------------------------
*set basic X-fields
*  -----------------------------------------------------------------------
    LS_PR_ITEM_X-PREQ_ITEM  = LV_POS_NO.
    LS_PR_ITEM_X-PREQ_ITEMX = ABAP_TRUE.
    LS_PR_ITEM_X-PUR_GROUP  = ABAP_TRUE.
    LS_PR_ITEM_X-SHORT_TEXT = ABAP_TRUE.
    LS_PR_ITEM_X-PLANT      = ABAP_TRUE.
    LS_PR_ITEM_X-MATL_GROUP = ABAP_TRUE.
    LS_PR_ITEM_X-QUANTITY   = ABAP_TRUE.
    LS_PR_ITEM_X-UNIT       = ABAP_TRUE.
    LS_PR_ITEM_X-PREQ_DATE  = ABAP_TRUE.
* To test auto source of supply determination.
    IF LV_FIXED_VENDOR IS INITIAL.
      CLEAR LS_PR_ITEM-PURCH_ORG.
*    ls_pr_item_x-purch_org  = abap_true.
    ELSE.
      LS_PR_ITEM_X-PURCH_ORG  = ABAP_TRUE.
    ENDIF.
*set update flag,
*if storage location was filled from outside
    IF LS_PR_ITEM-STORE_LOC IS NOT INITIAL.
      LS_PR_ITEM_X-STORE_LOC  = ABAP_TRUE.
    ENDIF.

*  -----------------------------------------------------------------------
*map texts
*  -----------------------------------------------------------------------
    CALL METHOD ME->MAP_CRM_TEXT_PR
      EXPORTING
        IV_PR_DOC_NO          = LV_DOC_NO
        IV_PR_POS_NO          = LV_POS_NO
        IT_TEXT               = LT_TEXT
        IV_HEADER_DESCRIPTION = CONV #( LS_HEADER-DESCRIPTION )
      IMPORTING
        ET_HEADER_TEXT        = ET_HEADER_TEXT
        ET_ITEM_TEXT          = ET_ITEM_TEXT.

*  -----------------------------------------------------------------------
*product type code
*  -----------------------------------------------------------------------
    CALL FUNCTION 'T134_SINGLE_READ'
      EXPORTING
        T134_MTART = LS_MARA-MTART
      IMPORTING
        WT134      = LS_T134
      EXCEPTIONS
        NOT_FOUND  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      LS_PR_ITEM-PRODUCTTYPE = LS_T134-PROD_TYPE_CODE .
      LS_PR_ITEM_X-PRODUCTTYPE = ABAP_TRUE.

      IF LS_T134-PROD_TYPE_CODE = GC_SERVICE. "Service Performer
        CLEAR:LS_PARTNER,LV_PARTNER.
        READ TABLE LT_PARTNER WITH KEY REF_GUID = LS_ITEM-GUID  PARTNER_PFT = GC_PARTNER_PFT-SERVICE_PERFORMER MAINPARTNER = ABAP_TRUE INTO LS_PARTNER.
        IF NOT LS_PARTNER IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = LS_PARTNER-PARTNER_NO
            IMPORTING
              OUTPUT = LV_PARTNER.
          MOVE LV_PARTNER TO LS_PR_ITEM-SERVICEPERFORMER .
          LS_PR_ITEM_X-SERVICEPERFORMER = ABAP_TRUE.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ( ( LV_CLOUD EQ ABAP_TRUE AND
         ( LS_ITEM-OBJECT_TYPE EQ GC_CRMT_BUS_OBJ-CRM_SPARE_PART
          OR  LS_ITEM-OBJECT_TYPE EQ GC_CRMT_BUS_OBJ-CRM_SERVICE ) ) OR
        ( LV_CLOUD EQ ABAP_FALSE AND
          LS_ITEM-OBJECT_TYPE EQ GC_CRMT_BUS_OBJ-CRM_SPARE_PART ) ).

      LV_FLAG = ABAP_TRUE.

    ENDIF.

*<-- Start of Insertion CME018 22.08.2024
*   SDS: Also fill material code for sales item scenario
    IF LV_FLAG IS INITIAL AND
       LS_ITEM-OBJECT_TYPE EQ GC_SALES_ITEM.
      LV_FLAG = ABAP_TRUE.
    ENDIF.
*--> End of Insertion CME018 22.08.2024

*  -----------------------------------------------------------------------
*sourcing
*  -----------------------------------------------------------------------
*spare parts
    IF LV_FLAG = ABAP_TRUE.
*check if material exists
      IF CHECK_MATERIAL_EXIST( LV_MATERIAL ) = ABAP_TRUE.
        LS_PR_ITEM-MATERIAL_LONG   = LV_MATERIAL.
        LS_PR_ITEM_X-MATERIAL_LONG = ABAP_TRUE.
        LV_MATERIAL_EXIST = ABAP_TRUE.
      ENDIF.

*  --------------begin of logic to determine price of item ---------------------------------------*
* onPremise processing
      IF LV_CLOUD NE ABAP_TRUE.

        CALL METHOD ME->GET_PRICE_FOR_MATERIAL
          EXPORTING
            IV_PLANT      = LS_PR_ITEM-PLANT
            IV_MATERIAL   = LV_MATERIAL
          IMPORTING
            EV_PRICE      = LV_PRICE
            EV_CURRENCY   = LV_CURRENCY
            EV_PRICE_UNIT = LV_PRICE_UNIT.

* if available, overwrite price from material master
        CALL METHOD GET_PRICE
          EXPORTING
            IS_ORDERADM_I = LS_ITEM
            IT_PARTNER    = LT_PARTNER
            IT_PRIDOC     = LT_PRIDOC
          IMPORTING
            EV_PRICE      = LV_PRICE_2
            EV_CURRENCY   = LV_CURRENCY_2
            EV_PRICE_UNIT = LV_PRICE_UNIT_2.

        IF LV_PRICE_2 IS NOT INITIAL
          OR LV_CURRENCY_2 IS NOT INITIAL
          OR  LV_PRICE_UNIT_2 IS NOT INITIAL.
          LV_PRICE = LV_PRICE_2.
          LV_CURRENCY = LV_CURRENCY_2.
          LV_PRICE_UNIT = LV_PRICE_UNIT_2.
        ENDIF.

      ELSE.
* Cloud processing
        IF LV_PRICE IS INITIAL OR LV_CURRENCY IS INITIAL OR  LV_PRICE_UNIT IS INITIAL. " removed  cloud check

*     if price not maintained at  material  level
*           logic to identify source of supply and fetch the pricing
*              information.
*       -----------------------------------------------------
*            call method to fetch source of supply information
*       -----------------------------------------------------
*      Fetch the PR price from the Pricing data information
          CALL METHOD ME->GET_PRIDOC_PRICE
            EXPORTING
              IT_PRIDOC     = LT_PRIDOC
              IS_ORDERADM_I = LS_ITEM
              IS_HEADER     = LS_HEADER
            IMPORTING
              EV_PRICE      = LV_PRICE
              EV_PRICE_UNIT = LV_PRICE_UNIT
              EV_CURRENCY   = LV_CURRENCY.
          IF LV_PRICE IS INITIAL OR LV_CURRENCY IS INITIAL OR  LV_PRICE_UNIT IS INITIAL.
*              if the price information of item not found in pridoc
*      Check if document is quotation
            ME->IDENTIFY_QUOTATION( EXPORTING IV_PROCESS_TYPE = LS_HEADER-PROCESS_TYPE
                                    IMPORTING EV_STAT_QUOT    = LV_STAT_QUOT ).
            IF LV_STAT_QUOT NE ABAP_TRUE.
*      retreive the predeccessor service order
              ME->GET_PRICE_FROM_PREDECESSOR( EXPORTING IV_HEADER_GUID = LS_HEADER-GUID
                                                        IS_ORDERADM_I  = LS_ITEM
                                              IMPORTING EV_PRICE       = LV_PRICE
                                                        EV_CURRENCY    = LV_CURRENCY
                                                        EV_PRICE_UNIT  = LV_PRICE_UNIT ).

*                IF lv_price IS NOT INITIAL AND lv_currency IS NOT INITIAL AND  lv_price_unit IS NOT INITIAL.
*                  EXIT.
*                ENDIF.
            ENDIF.
            IF LV_PRICE IS INITIAL OR LV_CURRENCY IS INITIAL OR  LV_PRICE_UNIT IS INITIAL.
              READ TABLE LT_PRICING ASSIGNING <LS_PRICING>  WITH KEY GUID = LV_ITEM_GUID.
              IF SY-SUBRC IS INITIAL.
                LV_PRICE_DATE = <LS_PRICING>-PRICE_DATE.
              ELSE.
                LV_PRICE_DATE = SY-DATUM.
              ENDIF.
              CALL FUNCTION 'CRMS4_INFORECORD_READ'
                EXPORTING
                  IV_MATNR             = LV_MATERIAL
                  IV_PLANT             = LS_PR_ITEM-PLANT
                  IV_PURGRP            = LS_PR_ITEM-PUR_GROUP
                  IV_PURORG            = LV_PURCH_ORG
                  IV_VALID_FROM        = LV_PRICE_DATE
                  IV_VALID_TO          = LV_PRICE_DATE
                  IV_SERVICE_PERFORMER = LS_PR_ITEM-SERVICEPERFORMER
                IMPORTING
                  E_NETPR              = LV_PRICE
                  E_WAERS              = LV_CURRENCY
                  E_EPEIN              = LV_PRICE_UNIT
                EXCEPTIONS
                  NO_INFOREC_FOUND     = 1
                  OTHERS               = 2.
              IF LV_PRICE IS INITIAL OR LV_CURRENCY IS INITIAL OR  LV_PRICE_UNIT IS INITIAL.
                MOVE LS_ITEM-DESCRIPTION TO LV_ERROR_PARAM1.
                CALL METHOD COLLECT_ITEM_MESSAGES
                  EXPORTING
                    IV_MSGID       = 'CRM_SRV_LOG_EXT_OLTP'
                    IV_MSGNO       = '003'
                    IV_MSGTY       = 'E'
                    IV_MSGV1       = LV_ERROR_PARAM1
                    IV_ITEM_GUID   = LS_ITEM-GUID
                  CHANGING
                    CT_ITEM_ERRORS = ET_ITEM_ERRORS.
* Implement suitable error handling here
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


*         Get dates from service order
      LS_PR_ITEM-STARTDATE = VALUE #( IS_SRVORDER_DATA-APPOINTMENT[ REF_GUID = LS_ITEM-GUID APPT_TYPE = 'SRV_CUST_BEG' ]-DATE_FROM OPTIONAL ).
      LS_PR_ITEM_X-STARTDATE = ABAP_TRUE.
      LS_PR_ITEM-ENDDATE = VALUE #( IS_SRVORDER_DATA-APPOINTMENT[ REF_GUID = LS_ITEM-GUID APPT_TYPE = 'SRV_CUST_END' ]-DATE_FROM OPTIONAL ).
      LS_PR_ITEM_X-ENDDATE = ABAP_TRUE.
      LS_PR_ITEM-PREQ_DATE =  LS_PR_ITEM-STARTDATE."In case of cloud : delivery date should be set it from Requested start date of Service Order ITEM

      IF  LV_PRICE IS NOT INITIAL AND LV_CURRENCY IS NOT INITIAL AND  LV_PRICE_UNIT IS NOT INITIAL.
        LS_PR_ITEM-PREQ_PRICE = LV_PRICE.
        LS_PR_ITEM-CURRENCY   = LV_CURRENCY.
        LS_PR_ITEM-PRICE_UNIT = LV_PRICE_UNIT.
        LS_PR_ITEM_X-PREQ_PRICE = ABAP_TRUE.
        LS_PR_ITEM_X-CURRENCY   = ABAP_TRUE.
        LS_PR_ITEM_X-PRICE_UNIT = ABAP_TRUE.
      ENDIF.

*  --------------end of logic to determine price of item ------------------------------------------*
    ELSEIF  LS_ITEM-OBJECT_TYPE EQ GC_CRMT_BUS_OBJ-CRM_SERVICE.

*         Get dates from service order
      LS_PR_ITEM-STARTDATE = VALUE #( IS_SRVORDER_DATA-APPOINTMENT[ REF_GUID = LS_ITEM-GUID APPT_TYPE = 'SRV_CUST_BEG' ]-DATE_FROM OPTIONAL ).
      LS_PR_ITEM_X-STARTDATE = ABAP_TRUE.
      LS_PR_ITEM-ENDDATE = VALUE #( IS_SRVORDER_DATA-APPOINTMENT[ REF_GUID = LS_ITEM-GUID APPT_TYPE = 'SRV_CUST_END' ]-DATE_FROM OPTIONAL ).
      LS_PR_ITEM_X-ENDDATE = ABAP_TRUE.
*
*         check if service exists
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_ITEM-ORDERED_PROD
        IMPORTING
          OUTPUT = LV_SERVICE.

*          PERFORM check_service_exist
*          USING lv_service
*          lv_service_exist.

      IF CHECK_SERVICE_EXIST( LV_SERVICE ) = ABAP_TRUE.
        LV_SERVICE_EXIST = ABAP_TRUE.
      ENDIF.

    ENDIF.

*-----------------------------------------------------------------------
*set specific (material/service) attributes
*-----------------------------------------------------------------------
*
**  spare parts
    IF LV_FLAG = ABAP_TRUE.

      IF NOT LV_MATERIAL_EXIST IS INITIAL.
*     assign material
        LS_PR_ITEM-MATERIAL_LONG   = LV_MATERIAL.
        LS_PR_ITEM_X-MATERIAL_LONG = ABAP_TRUE.

      ENDIF.

*     set serial no, so that item has a valid accounting object
      LV_SERIAL_NO = 1.
      LS_ACC-SERIAL_NO    = LV_SERIAL_NO.
      LS_ACC_X-SERIAL_NO  = LV_SERIAL_NO.
      LS_ACC_X-SERIAL_NOX = ABAP_TRUE.

*   service
    ELSEIF  LS_ITEM-OBJECT_TYPE EQ GC_CRMT_BUS_OBJ-CRM_SERVICE.

      IF NOT LV_SERVICE_EXIST IS INITIAL.
*       assign service master
        LS_SERVICE-SERVICE   = LV_SERVICE.
        LS_SERVICE_X-SERVICE = ABAP_TRUE.
      ELSE.
*       set short text of unknown service and set dflt qty unit
        LS_SERVICE-SHORT_TEXT   = LV_SERVICE.
        LS_SERVICE_X-SHORT_TEXT = ABAP_TRUE.
        LS_PR_ITEM-UNIT         = LS_SEARCH-UNIT.
      ENDIF.

*     item numbering
      LV_SERVICE_NO  = LV_SERVICE_NO + 1.
      LV_PACKAGE     = LV_SERVICE_NO.
      LV_OUTLINE     = '0000000001'.
*  package no
      LS_PR_ITEM-PCKG_NO = LV_PACKAGE.
      LS_PR_ITEM_X-PCKG_NO = ABAP_TRUE.

*   mark item as service
      LS_PR_ITEM-ITEM_CAT = '9'.
      LS_PR_ITEM_X-ITEM_CAT = ABAP_TRUE.

*    accounting item:
*    set serial no, so that the service item has
*    a valid accounting object
      LV_SERIAL_NO = 1.

      LS_ACC-SERIAL_NO    = LV_SERIAL_NO.
      LS_ACC_X-SERIAL_NO  = LV_SERIAL_NO.
      LS_ACC_X-SERIAL_NOX = ABAP_TRUE.

*    service item (service master)
      LS_SERVICE-DOC_ITEM       = LV_POS_NO.
      LS_SERVICE-OUTLINE        = LV_OUTLINE.
      LS_SERVICE-SRV_LINE       = LV_POS_NO.
      LS_SERVICE-QUANTITY       = LS_SCHEDLIN-QUANTITY.
      LS_SERVICE-UOM            = LS_PRODUCT_I-PROCESS_QTY_UNIT.
      LS_SERVICE-GROSS_PRICE    = LS_PR_ITEM-PREQ_PRICE.
      LS_SERVICE-PRICE_UNIT     = LS_CONTRACTS-PRICE_UNIT.
      LS_SERVICE-MATL_GROUP     = LS_PR_ITEM-MATL_GROUP.

      LS_SERVICE_X-DOC_ITEM     = LV_POS_NO.
      LS_SERVICE_X-OUTLINE      = LV_OUTLINE.
      LS_SERVICE_X-SRV_LINE     = LV_POS_NO.
      LS_SERVICE_X-QUANTITY     = ABAP_TRUE.
      LS_SERVICE_X-UOM          = ABAP_TRUE.
      LS_SERVICE_X-GROSS_PRICE  = ABAP_TRUE.
      LS_SERVICE_X-PRICE_UNIT   = ABAP_TRUE.
      LS_SERVICE_X-MATL_GROUP   = ABAP_TRUE.

*    accounting service item (service master)
      LS_SRV_ACC-DOC_ITEM       = LV_POS_NO.
      LS_SRV_ACC-OUTLINE        = LV_OUTLINE.
      LS_SRV_ACC-SRV_LINE       = LV_POS_NO.
      LS_SRV_ACC-SERIAL_NO      = LV_SERIAL_NO.
      LS_SRV_ACC-SERIAL_NO_ITEM = LV_SERIAL_NO.
      LS_SRV_ACC-PERCENT        = 100.

      LS_SRV_ACC_X-DOC_ITEM     = LV_POS_NO.
      LS_SRV_ACC_X-OUTLINE      = LV_OUTLINE.
      LS_SRV_ACC_X-SRV_LINE     = LV_POS_NO.
      LS_SRV_ACC_X-SERIAL_NO    = LV_SERIAL_NO.
      LS_SRV_ACC_X-SERIAL_NO_ITEM = ABAP_TRUE.
      LS_SRV_ACC_X-PERCENT      = ABAP_TRUE.

    ENDIF.

*-----------------------------------------------------------------------
*   set sourcing information (if supplied)
*-----------------------------------------------------------------------

    IF ( NOT LV_PROPOSED_VENDOR IS INITIAL ) OR
       ( NOT LV_FIXED_VENDOR    IS INITIAL ).
      LS_PR_ITEM-FIXED_VEND   = LV_FIXED_VENDOR.
      LS_PR_ITEM_X-FIXED_VEND = ABAP_TRUE.
      LS_PR_ITEM-DES_VENDOR   =  LV_PROPOSED_VENDOR.
      LS_PR_ITEM_X-DES_VENDOR = ABAP_TRUE.
      LS_SOURCE-PREQ_ITEM  = LV_POS_NO.
      LS_SOURCE-DES_VENDOR = LV_PROPOSED_VENDOR.
      LS_SOURCE-FIXED_VEND = LV_FIXED_VENDOR.
      LS_SOURCE-PURCH_ORG  = LS_PR_ITEM-PURCH_ORG.
    ENDIF.

*-----------------------------------------------------------------------
* fill return tables
*-----------------------------------------------------------------------

* add general records to export tables
    APPEND LS_PR_ITEM   TO ET_ITEM.
    APPEND LS_PR_ITEM_X TO ET_ITEM_X.

*only if accounting information pass through
    IF NOT LS_ACC IS INITIAL.
      APPEND LS_ACC       TO ET_ACC.
      APPEND LS_ACC_X     TO ET_ACC_X.
    ENDIF.

*in case vendor information are supplied
    IF NOT LS_SOURCE IS INITIAL.
      APPEND LS_SOURCE  TO ET_SOURCE.
    ENDIF.
*   in case of service add specific service records
*   to export tables
    IF LS_ITEM-OBJECT_TYPE EQ GC_CRMT_BUS_OBJ-CRM_SERVICE AND
    LV_CANCEL_FLAG IS INITIAL. "on closed PR update on service lines not possible --> Internal Order will be closed too
      APPEND LS_SERVICE       TO ET_SERVICELINES.
      APPEND LS_SRV_ACC       TO ET_SERVICEACCOUNT.
      APPEND LS_SERVICE_X     TO ET_SERVICELINESX.
      APPEND LS_SRV_ACC_X     TO ET_SERVICEACCOUNTX.
    ENDIF.

*-----------------------------------------------------------------------
*create process info
*-----------------------------------------------------------------------
*    PERFORM create_process_info
*    USING ls_task_info-task
*    is_header-process_guid
*    is_header-process_object_type
*    ls_task_info-item_guid
*    ls_item-item_object_type
*    gc_crmt_bus_obj-purch_req
*    lv_doc_no
*    lv_pos_no
*    ls_proc_info.

    CLEAR: LS_PROC_INFO.
    LS_PROC_INFO-TASK            = LS_TASK_INFO-TASK.
    LS_PROC_INFO-HEADER_GUID     = LS_HEADER-GUID.
    LS_PROC_INFO-HEADER_BUS_TYPE = LS_HEADER-OBJECT_TYPE.
    LS_PROC_INFO-ITEM_GUID       = LS_ITEM-GUID.
    LS_PROC_INFO-ITEM_BUS_TYPE   = LS_ITEM-OBJECT_TYPE.
    LS_PROC_INFO-BUS_TYPE        = GC_CRMT_BUS_OBJ-PURCH_REQ.
*   doc no
    MOVE LV_DOC_NO TO  LS_PROC_INFO-DOC_NO.
*   item no
    MOVE  LV_POS_NO TO LS_PROC_INFO-ITEM_NO.
*   set qty and qty unit
    LS_PROC_INFO-QUANTITY = LS_PR_ITEM-QUANTITY.
    LS_PROC_INFO-QUANTITY_UNIT = LS_PR_ITEM-UNIT.

    APPEND LS_PROC_INFO TO ET_PROCESS_INFO.

*clear all variables (!!!!
    CLEAR:
    LS_PR_ITEM, LS_PR_ITEM_X, LS_ACC, LS_ACC_X, LS_SOURCE,
    LS_SERVICE, LS_SRV_ACC, LV_PRICE, LV_CURRENCY, LV_PRICE_UNIT.


  ENDLOOP.

*-----------------------------------------------------------------------
*BaDI handling
*-----------------------------------------------------------------------

*  CALL METHOD cl_exithandler=>get_instance
*    EXPORTING
*      exit_name                     = 'CRM_SRV_MAP_PR_BADI'
*      null_instance_accepted        = 'X'
*    CHANGING
*      instance                      = lv_badi
*    EXCEPTIONS
*      no_reference                  = 1
*      no_interface_reference        = 2
*      no_exit_interface             = 3
*      class_not_implement_interface = 4
*      single_exit_multiply_active   = 5
*      cast_error                    = 6
*      exit_not_existing             = 7
*      data_incons_in_exit_managem   = 8
*      OTHERS                        = 9.
*
*  IF NOT lv_badi IS INITIAL.
*
*    CALL METHOD lv_badi->map_pr1
*      EXPORTING
*        iv_docno                  = lv_doc_no
*        is_header                 = is_header
*        it_task_info              = it_task_info
*        it_item                   = it_item
*        it_schedlin               = it_schedlin
*        it_text                   = it_text
*        it_status                 = it_status
*        it_partner                = it_partner
*        it_docflow                = it_docflow
*        it_bapi_ext               = it_bapi_ext
*      CHANGING
*        cv_new_pr                 = ev_new_pr
*        ct_process_info           = et_process_info
*        ct_return                 = et_return
*        ct_header                 = es_header
*        ct_header_x               = es_header_x
*        ct_item                   = et_item[]
*        ct_item_x                 = et_item_x[]
*        ct_item_export            = et_item_export[]
*        ct_source                 = et_source[]
*        ct_acc                    = et_acc[]
*        ct_acc_segm               = et_acc_segm[]
*        ct_acc_x                  = et_acc_x[]
*        ct_deliv_addr             = et_deliv_addr[]
*        ct_item_text              = et_item_text[]
*        ct_header_text            = et_header_text[]
*        ct_extensionin            = et_extensionin[]
*        ct_extensionout           = et_extensionout[]
*        ct_serviceoutline         = et_serviceoutline[]
*        ct_serviceoutlinex        = et_serviceoutlinex[]
*        ct_servicelines           = et_servicelines[]
*        ct_servicelinesx          = et_servicelinesx[]
*        ct_servicelimit           = et_servicelimit[]
*        ct_servicelimitx          = et_servicelimitx[]
*        ct_servicecontractlimits  = et_servicecontractlimits[]
*        ct_servicecontractlimitsx = et_servicecontractlimitsx[]
*        ct_serviceaccount         = et_serviceaccount[]
*        ct_serviceaccountx        = et_serviceaccountx[]
*        ct_servicelongtexts       = et_servicelongtexts[]
*        ct_serialnumber           = et_serialnumber[]
*        ct_serialnumberx          = et_serialnumberx[]
*        ct_prcomponents           = et_prcomponents[]
*        ct_prcomponentsx          = et_prcomponentsx[].
*
*  ENDIF.

*<-- Start of Insertion CME018 22.08.2024 (Update PR Data)
* Update PR Data
  ZCL_SDSCM_ENHANCEMENT=>UPDATE_DATA_IN_PR(
    EXPORTING
      IS_SRVORDER_DATA = IS_SRVORDER_DATA
      IT_PROCESS_INFO  = ET_PROCESS_INFO
      IT_ITEM          = ET_ITEM
    CHANGING
      CT_ACC           = ET_ACC
      CT_ACC_X         = ET_ACC_X ).
*--> End of Insertion CME018 22.08.2024

ENDMETHOD.


  METHOD SET_POS_NO.
    DATA(lc_bundle_util) = cl_crms4_srv_bundle_util=>get_instance( ).
    DATA(lv_bundle_item_type) = lc_bundle_util->identify_sb_item_type( iv_guid  = iv_item_guid ).
    IF  lv_bundle_item_type EQ cl_crms4_srv_bundle_util=>gc_sb_tm_sub_item_fp
     OR lv_bundle_item_type EQ cl_crms4_srv_bundle_util=>gc_sb_tm_sub_item_tm
     OR lv_bundle_item_type EQ cl_crms4_srv_bundle_util=>gc_sb_fp_sub_item.
      cv_pos_no = iv_item_no.
    ELSEIF cv_pos_no IS INITIAL.
*  set the item/position no for new items
      cv_pos_no = iv_loop_index * iv_incr.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
