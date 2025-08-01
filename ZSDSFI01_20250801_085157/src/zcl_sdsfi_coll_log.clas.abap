*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
* 10.01.2025  F36K910967  Apichat Ch.  420000106
*                                      - Add t-code on history log ( Activity log )
*-----------------------------------------------------------------------


class ZCL_SDSFI_COLL_LOG definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF gty_dom_text,
        key_value TYPE DD07T-domvalue_l,
        ddtext    TYPE DD07T-ddtext,
      END OF gty_dom_text .
  types:
    BEGIN OF gty_billing,
        vbeln       TYPE vbrp-vbeln,
        pernr	      TYPE zsdsfit029-pernr,
        action_type TYPE zsdsfit029-action_type,
        status      TYPE zsdsfit029-status,
      END OF gty_billing .
  types:
    tt_dom_text TYPE STANDARD TABLE OF gty_dom_text WITH EMPTY KEY .
  types:
    tt_billing TYPE STANDARD TABLE OF gty_billing WITH EMPTY KEY .
  types:
    tt_coll_log TYPE STANDARD TABLE OF zsdsfit029 WITH EMPTY KEY .
  types:
    tt_status_hist TYPE STANDARD TABLE OF zsdsfit038 WITH EMPTY KEY .
  types:
    tt_bank_vendor TYPE STANDARD TABLE OF zsdsfis085 WITH EMPTY KEY .

  constants:
    BEGIN OF gc_status,
        reject TYPE zsdsde_col_status VALUE '10',
      END OF gc_status .
  constants GC_TRUE type CHAR01 value 'X' ##NO_TEXT.
  class-data GT_ACTION_TYPE type TT_DOM_TEXT .
  class-data GT_STATUS type TT_DOM_TEXT .
  class-data GT_PYMT_METHOD type TT_DOM_TEXT .

  class-methods CREATE_BANK_VENDOR_LOG
    importing
      !IV_WORKDATE type ZSDSDE_WORK_DATE default SY-DATUM
      !IT_VBELN type TT_BILLING optional
      !IT_DO type TT_BILLING optional
    exporting
      !EV_RC type SYSUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods DELETE_BANK_VENDOR_LOG
    importing
      !IT_VBELN type TT_BILLING optional
      !IT_DO type TT_BILLING optional
      !IV_TESTRUN type ABAP_BOOL default 'X'
    exporting
      !EV_RC type SYSUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods GET_ACTION_TYPE_TEXT
    importing
      !IV_ACTION_TYPE type ZSDSDE_ACTION_TYPE
    returning
      value(RV_TEXT) type VAL_TEXT .
  class-methods GET_STATUS_TEXT
    importing
      !IV_STATUS type ZSDSDE_COL_STATUS
    returning
      value(RV_TEXT) type VAL_TEXT .
  class-methods GET_PYMT_METHOD_TEXT
    importing
      !IV_PYMT_METHOD type ZSDSDE_PYMT_METHOD
    returning
      value(RV_TEXT) type VAL_TEXT .
protected section.
private section.

  class-methods PROCESS_LOCK_COLL_LOG_DB
    importing
      !IT_COLL_LOG type TT_COLL_LOG
    exporting
      !EV_RC type SYSUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods PROCESS_DELETE_COLL_LOG
    importing
      !IV_TESTRUN type ABAP_BOOL
      !IT_COLL_LOG type TT_COLL_LOG
      !IT_STATUS_HIST type TT_STATUS_HIST
    exporting
      !EV_RC type SYSUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods CREATE_STATUS_HIST
    importing
      !IS_LOG type ZSDSFIT029
      !IT_STATUS_HIST type TT_STATUS_HIST
    changing
      !CT_STATUS_HIST type TT_STATUS_HIST .
  class-methods GET_BANK_VENDOR_DATA
    importing
      !IV_WORKDATE type ZSDSDE_WORK_DATE
      !IT_VBELN type TT_BILLING
      !IT_DO type TT_BILLING
    exporting
      !ET_BANK_VENDOR type TT_BANK_VENDOR .
  class-methods GET_COLL_LOG_HIST
    importing
      !IT_BANK_VENDOR type TT_BANK_VENDOR
    exporting
      !ET_COLL_LOG type TT_COLL_LOG
      !ET_STATUS_HIST type TT_STATUS_HIST .
  class-methods SAVE_BANK_VENDOR_LOG
    importing
      !IT_BANK_VENDOR type TT_BANK_VENDOR
      !IT_STATUS_HIST type TT_STATUS_HIST
    exporting
      !EV_RC type SYSUBRC
      !ET_RETURN type BAPIRET2_T .
ENDCLASS.



CLASS ZCL_SDSFI_COLL_LOG IMPLEMENTATION.


  METHOD create_bank_vendor_log.
    get_bank_vendor_data(                         "#EC CI_NO_TRANSFORM
      EXPORTING
        iv_workdate    = iv_workdate
        it_vbeln       = it_vbeln
        it_do          = it_do
      IMPORTING
        et_bank_vendor = DATA(lt_bank_vendor)
    ) .

    get_coll_log_hist(
      EXPORTING
        it_bank_vendor    = lt_bank_vendor
      IMPORTING
        et_status_hist = DATA(lt_status_hist)
    ).

    save_bank_vendor_log(                         "#EC CI_NO_TRANSFORM
      EXPORTING
        it_bank_vendor = lt_bank_vendor
        it_status_hist = lt_status_hist
      IMPORTING
        ev_rc          =  ev_rc                    " Return Code
        et_return      =  et_return                " Return table
    ).

  ENDMETHOD.


  METHOD create_status_hist.

    DATA: ls_status_hist     TYPE zsdsfit038,
          ls_status_hist_old TYPE zsdsfit038,
          ls_status_hist_new TYPE zsdsfit038,
          lt_status_hist     TYPE tt_status_hist.

    lt_status_hist = it_status_hist.

    SORT lt_status_hist BY bukrs belnr gjahr buzei seq sub_seq DESCENDING.

    "Get lastest hist data
    LOOP AT lt_status_hist
      ASSIGNING FIELD-SYMBOL(<ls_status_hist>)
      WHERE bukrs     = is_log-bukrs
      AND   belnr     = is_log-belnr
      AND   gjahr     = is_log-gjahr
      AND   buzei     = is_log-buzei
      AND   seq       = is_log-seq.
      EXIT.
    ENDLOOP.

    ls_status_hist = CORRESPONDING #( is_log ).

    IF <ls_status_hist> IS ASSIGNED.
      IF <ls_status_hist>-pernr       = is_log-pernr AND
         <ls_status_hist>-work_date   = is_log-work_date AND
         <ls_status_hist>-action_type = is_log-action_type AND
         <ls_status_hist>-status      = is_log-status.

        ls_status_hist_old = CORRESPONDING #( <ls_status_hist> EXCEPT sub_seq ernam_hist erdat_hist erzmt_hist ).
        ls_status_hist_new = CORRESPONDING #( is_log ).

        IF ls_status_hist_old = ls_status_hist_new.
          "No history log add for no action type and status change
          RETURN.
        ELSE.
          ls_status_hist-sub_seq = <ls_status_hist>-sub_seq.  "Update existing record
        ENDIF.
      ELSE.
        ls_status_hist-sub_seq = <ls_status_hist>-sub_seq + 1.
      ENDIF.

    ELSE.
      ls_status_hist-sub_seq = 1.
    ENDIF.

    ls_status_hist-tcode      = sy-tcode. "<<F36K910967 ins
    ls_status_hist-ernam_hist = sy-uname.
    ls_status_hist-erdat_hist = sy-datum.
    ls_status_hist-erzmt_hist = sy-uzeit.

    APPEND ls_status_hist TO ct_status_hist.

  ENDMETHOD.


  METHOD delete_bank_vendor_log.

    get_bank_vendor_data(
      EXPORTING
        iv_workdate    = sy-datum
        it_vbeln       = it_vbeln
        it_do          = it_do
      IMPORTING
        et_bank_vendor = DATA(lt_bank_vendor)
    ).

    get_coll_log_hist(
      EXPORTING
        it_bank_vendor  = lt_bank_vendor
      IMPORTING
        et_coll_log     = DATA(lt_coll_log)
        et_status_hist  = DATA(lt_status_hist)
    ).

    process_delete_coll_log(
      EXPORTING
        iv_testrun     = iv_testrun
        it_coll_log    = lt_coll_log
        it_status_hist = lt_status_hist
      IMPORTING
        ev_rc          = ev_rc
        et_return      = et_return
      ).

  ENDMETHOD.


  METHOD get_action_type_text.

    IF gt_action_type IS INITIAL.
      SELECT
        domvalue_l,
        ddtext
        FROM dd07t
        WHERE domname     = 'ZSDSDM_ACTION_TYPE'
        AND   ddlanguage  = 'E'
        INTO TABLE @gt_action_type.
      SORT gt_action_type BY key_value.
    ENDIF.

    READ TABLE gt_action_type
      INTO DATA(ls_action_type)
      WITH KEY key_value = iv_action_type
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      rv_text = ls_action_type-ddtext.
    ENDIF.

  ENDMETHOD.


  METHOD get_bank_vendor_data.

    CLEAR et_bank_vendor.

    IF it_vbeln IS NOT INITIAL.
      SELECT                                          "#EC *
        @iv_workdate AS work_date,
        opn~bukrs,
        opn~belnr,
        opn~gjahr,
        opn~buzei,
        opn~xref2 AS billpl_no,
        billing~pernr,
        billing~action_type,
        billing~status,
        opn~kunnr,
        opn~umskz,
        opn~xblnr,
        opn~bldat,
        opn~budat,
        opn~zfbdt,
        opn~zterm,
        docu~prctr,
        docu~prctr as kostl,
        CASE
          WHEN cust~ktokd = 'Z050' THEN
            concat_with_space( partner~name_first, partner~name_last, 1 )
          ELSE
            concat_with_space( partner~name_org1, partner~name_org2, 1 )
        END AS cust_name,
        opn~wrbtr,
        opn~waers,
        opn~shkzg,
        opn~bupla AS brnch,
        opn~belnr AS receipt_no,
        hdr~awkey  AS vbeln_vf
        FROM bsid_view AS opn
        INNER JOIN acdoca AS docu
        ON  docu~rbukrs = opn~bukrs
        AND docu~belnr  = opn~belnr
        AND docu~gjahr  = opn~gjahr
        AND docu~buzei  = opn~buzei
        INNER JOIN bkpf AS hdr
        ON  hdr~bukrs = opn~bukrs
        AND hdr~belnr = opn~belnr
        AND hdr~gjahr = opn~gjahr
        INNER JOIN t001 AS comp
        ON  opn~bukrs = comp~bukrs
        AND opn~waers = comp~waers
        INNER JOIN kna1 AS cust
        ON opn~kunnr = cust~kunnr
        INNER JOIN cvi_cust_link AS link
        ON cust~kunnr = link~customer
        INNER JOIN but000 AS partner
        ON partner~partner_guid = link~partner_guid
*        INNER JOIN zsdsfit027 AS inv
*        ON  hdr~awtyp = 'VBRK'
*        AND hdr~awkey = inv~vbeln
*        INNER JOIN zsdsfic028 AS inv_st
*        ON inv_st~stat_act = inv~zstat_act
*        INNER JOIN @it_vbeln AS billing       ##ITAB_KEY_IN_SELECT
*        ON inv~vbeln = billing~table_line
        INNER JOIN @it_vbeln AS billing       ##ITAB_KEY_IN_SELECT
        ON  hdr~awtyp = 'VBRK'
        AND hdr~awkey = billing~vbeln
        WHERE
          docu~rldnr          = '0L'
        INTO CORRESPONDING FIELDS OF TABLE @et_bank_vendor ##TOO_MANY_ITAB_FIELDS. "#EC *
    ENDIF.

    IF it_do IS NOT INITIAL.
      SELECT                                  "#EC *
        @iv_workdate AS work_date,
        opn~bukrs,
        opn~belnr,
        opn~gjahr,
        opn~buzei,
        opn~xref2 AS billpl_no,
        it_do~pernr,
        it_do~action_type,
        it_do~status,
        opn~kunnr,
        opn~umskz,
        opn~xblnr,
        opn~bldat,
        opn~budat,
        opn~zfbdt,
        opn~zterm,
        docu~prctr,
        docu~prctr as kostl,
        CASE
          WHEN cust~ktokd = 'Z050' THEN
            concat_with_space( partner~name_first, partner~name_last, 1 )
          ELSE
            concat_with_space( partner~name_org1, partner~name_org2, 1 )
        END AS cust_name,
        opn~wrbtr,
        opn~waers,
        opn~shkzg,
        opn~bupla AS brnch,
        opn~belnr AS receipt_no,
        hdr~awkey  AS vbeln_vf
        FROM bsid_view AS opn
        INNER JOIN acdoca AS docu
        ON  docu~rbukrs = opn~bukrs
        AND docu~belnr  = opn~belnr
        AND docu~gjahr  = opn~gjahr
        AND docu~buzei  = opn~buzei
        INNER JOIN bkpf AS hdr
        ON  hdr~bukrs = opn~bukrs
        AND hdr~belnr = opn~belnr
        AND hdr~gjahr = opn~gjahr
        INNER JOIN t001 AS comp
        ON  opn~bukrs = comp~bukrs
        AND opn~waers = comp~waers
        INNER JOIN kna1 AS cust
        ON opn~kunnr = cust~kunnr
        INNER JOIN cvi_cust_link AS link
        ON cust~kunnr = link~customer
        INNER JOIN but000 AS partner
        ON partner~partner_guid = link~partner_guid
        INNER JOIN vbrk AS bil_hdr
        ON  hdr~awtyp = 'VBRK'
        AND hdr~awkey = bil_hdr~vbeln
        INNER JOIN vbrp AS bil_itm
        ON bil_itm~vbeln = bil_hdr~vbeln
        INNER JOIN zsdsfit028 AS do
        ON do~vbeln = bil_itm~vgbel
*        INNER JOIN zsdsfic028 AS inv_st
*        ON inv_st~stat_act = do~zstat_act
        INNER JOIN @it_do AS it_do                         ##ITAB_KEY_IN_SELECT
        ON do~vbeln = it_do~vbeln
        WHERE
        docu~rldnr          = '0L'
        APPENDING CORRESPONDING FIELDS OF TABLE @et_bank_vendor ##TOO_MANY_ITAB_FIELDS. "#EC *
    ENDIF.

  ENDMETHOD.


  METHOD get_coll_log_hist.

    CLEAR:
      et_coll_log,
      et_status_hist.

    IF it_bank_vendor IS INITIAL.
      RETURN.
    ENDIF.

    SELECT *
      INTO TABLE @et_coll_log
      FROM zsdsfit029
      FOR ALL ENTRIES IN @it_bank_vendor          "#EC CI_NO_TRANSFORM
      WHERE data_type = ''
      AND   bukrs = @it_bank_vendor-bukrs
      AND   belnr = @it_bank_vendor-belnr
      AND   gjahr = @it_bank_vendor-gjahr
      AND   buzei = @it_bank_vendor-buzei
      AND   delete_flag = ''.

    IF et_coll_log IS NOT INITIAL.
      "Change history
      SELECT *
        INTO TABLE @et_status_hist
        FROM zsdsfit038
        FOR ALL ENTRIES IN @et_coll_log         "#EC CI_NO_TRANSFORM
        WHERE data_type = ''
        AND   bukrs = @et_coll_log-bukrs
        AND   belnr = @et_coll_log-belnr
        AND   gjahr = @et_coll_log-gjahr
        AND   buzei = @et_coll_log-buzei
        AND   seq   = @et_coll_log-seq.       "#EC CI_ALL_FIELDS_NEEDED
    ENDIF.

  ENDMETHOD.


  METHOD GET_PYMT_METHOD_TEXT.

    IF gt_pymt_method IS INITIAL.
      SELECT
        domvalue_l,
        ddtext
        FROM dd07t
        WHERE domname     = 'ZSDSDM_PYMT_METHOD'
        AND   ddlanguage  = 'E'
        INTO TABLE @gt_pymt_method.
      SORT gt_pymt_method BY key_value.
    ENDIF.

    READ TABLE gt_pymt_method
      INTO DATA(ls_pymt_method)
      WITH KEY key_value = iv_pymt_method
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      rv_text = ls_pymt_method-ddtext.
    ENDIF.

  ENDMETHOD.


  METHOD GET_STATUS_TEXT.

    IF gt_status IS INITIAL.
      SELECT
        domvalue_l,
        ddtext
        FROM dd07t
        WHERE domname     = 'ZSDSDM_COL_STATUS'
        AND   ddlanguage  = 'E'
        INTO TABLE @gt_status.
      SORT gt_status BY key_value.
    ENDIF.

    READ TABLE gt_status
      INTO DATA(ls_status)
      WITH KEY key_value = iv_status
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      rv_text = ls_status-ddtext.
    ENDIF.

  ENDMETHOD.


  METHOD process_delete_coll_log.
    TYPES: lty_coll_log TYPE zsdsfit029 WITH INDICATORS col_ind TYPE abap_bool.

    DATA:
      lt_coll_log        TYPE STANDARD TABLE OF lty_coll_log,
      lt_new_status_hist TYPE tt_status_hist.

    process_lock_coll_log_db(
      EXPORTING
        it_coll_log = it_coll_log
      IMPORTING
        ev_rc        = ev_rc
        et_return    = et_return
      ).

    IF ev_rc IS NOT INITIAL.
      RETURN.
    ENDIF.

    "Validation check
    LOOP AT it_coll_log INTO DATA(ls_coll_log)    ##INTO_OK.
      DATA(lv_hist) = REDUCE i( INIT count = 0
                           FOR ls_count IN it_status_hist
                           WHERE ( data_type = ls_coll_log-data_type AND
                                   bukrs = ls_coll_log-bukrs AND
                                   belnr = ls_coll_log-belnr AND
                                   gjahr = ls_coll_log-gjahr AND
                                   buzei = ls_coll_log-buzei AND
                                   seq   = ls_coll_log-seq
                                  )
                           NEXT count = count + 1 ).
      IF lv_hist > 1.
        ev_rc = 4.
        DATA(lv_msg) = |'Cancel Invoice Receive' { ls_coll_log-vbeln_vf } 'is not possible, invoice already processed' | ##NO_TEXT.
        et_return = VALUE #( BASE et_return ( id = '38' type = 'E' number = '000'   ##NUMBER_OK
                              message_v1 = lv_msg
*                              message_v2 = sy-msgv2
                              message    = lv_msg  ) ).
      ENDIF.

    ENDLOOP.

    IF iv_testrun IS INITIAL AND ev_rc IS INITIAL.
      lt_coll_log = CORRESPONDING #( it_coll_log ).
      LOOP AT lt_coll_log ASSIGNING FIELD-SYMBOL(<ls_coll_log>).
        <ls_coll_log>-delete_flag = gc_true.
        <ls_coll_log>-delete_date = sy-datum.
        <ls_coll_log>-status      = gc_status-reject.

        <ls_coll_log>-col_ind-delete_flag = gc_true.
        <ls_coll_log>-col_ind-delete_date = gc_true.
        <ls_coll_log>-col_ind-status      = gc_true.

        CALL METHOD zcl_sdsfi_coll_log=>create_status_hist
          EXPORTING
            is_log         = CORRESPONDING #( <ls_coll_log> )
            it_status_hist = it_status_hist
          CHANGING
            ct_status_hist = lt_new_status_hist.

      ENDLOOP.

      IF lt_new_status_hist IS NOT INITIAL.
        MODIFY zsdsfit038 FROM TABLE lt_new_status_hist.
      ENDIF.

      IF lt_coll_log IS NOT INITIAL.
        UPDATE zsdsfit029
          FROM TABLE @lt_coll_log INDICATORS SET STRUCTURE col_ind.
        IF sy-subrc EQ 0.
          APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<ls_return>).
          <ls_return>-id         = '38'.
          <ls_return>-log_msg_no = '000'.
          <ls_return>-type       = 'S'.
          <ls_return>-message_v1 = <ls_return>-message = TEXT-r01.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD process_lock_coll_log_db.

    CLEAR:
      ev_rc,
      et_return.

    LOOP AT it_coll_log INTO DATA(ls_coll_log)    ##INTO_OK
      GROUP BY ( key1 = ls_coll_log-kunnr
                 key2 = ls_coll_log-bukrs
                 key3 = ls_coll_log-belnr
                 key4 = ls_coll_log-gjahr ).

      CALL FUNCTION 'ENQUEUE_EZSDSFIS090'
        EXPORTING
          mode_zsdsfis090 = 'E'
          kunnr           = ls_coll_log-kunnr
          bukrs           = ls_coll_log-bukrs
          belnr           = ls_coll_log-belnr
          gjahr           = ls_coll_log-gjahr
          _collect        = 'X'
        EXCEPTIONS
          foreign_lock    = 1
          system_failure  = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
        ev_rc = sy-subrc.
        et_return = VALUE #( BASE et_return ( id = sy-msgid type = sy-msgty number = sy-msgno
                              message_v1 = sy-msgv1
                              message_v2 = sy-msgv2
                              message_v3 = sy-msgv3
                              message_v4 = sy-msgv4 ) ).
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'FLUSH_ENQUEUE'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      ev_rc = sy-subrc.
      et_return = VALUE #( BASE et_return ( id = sy-msgid type = sy-msgty number = sy-msgno
                            message_v1 = sy-msgv1
                            message_v2 = sy-msgv2
                            message_v3 = sy-msgv3
                            message_v4 = sy-msgv4 ) ).
    ENDIF.
  ENDMETHOD.


  METHOD save_bank_vendor_log.

    DATA:
      lv_zfbdt           TYPE rbkp-zfbdt,
      lv_zbd1t           TYPE rbkp-zbd1t,
      lt_bplog           TYPE STANDARD TABLE OF zsdsfit029,
      lt_new_status_hist TYPE tt_status_hist.

    CLEAR: ev_rc, et_return.

    SELECT
      col~bukrs,
      col~belnr,
      col~gjahr,
      col~buzei,
      col~seq
      FROM zsdsfit029 AS col
      FOR ALL ENTRIES IN @it_bank_vendor      "#EC CI_NO_TRANSFORM
      WHERE data_type = ''
      AND   bukrs = @it_bank_vendor-bukrs
      AND   belnr = @it_bank_vendor-belnr
      AND   gjahr = @it_bank_vendor-gjahr
      AND   buzei = @it_bank_vendor-buzei
      INTO TABLE @DATA(lt_max_seq).           "#EC CI_FAE_LINES_ENSURED

    SORT lt_max_seq BY bukrs belnr gjahr seq DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_max_seq COMPARING bukrs belnr gjahr.

    LOOP AT it_bank_vendor INTO DATA(ls_bank_vendor).

      APPEND INITIAL LINE TO lt_bplog ASSIGNING FIELD-SYMBOL(<ls_zsdsfit029>).

      <ls_zsdsfit029> = CORRESPONDING #( ls_bank_vendor ).

      CLEAR: lv_zfbdt, lv_zbd1t.

      READ TABLE it_bank_vendor
        INTO ls_bank_vendor
        WITH KEY
        bukrs = <ls_zsdsfit029>-bukrs
        belnr = <ls_zsdsfit029>-belnr
        gjahr = <ls_zsdsfit029>-gjahr
        buzei = <ls_zsdsfit029>-buzei.

      "Net due date
      CALL FUNCTION 'MRM_PAYMENT_TERMS_GET'
        EXPORTING
          if_zterm = ls_bank_vendor-zterm
          if_bldat = ls_bank_vendor-bldat
          if_budat = ls_bank_vendor-budat
          if_zfbdt = ls_bank_vendor-zfbdt
        IMPORTING
          ef_zfbdt = lv_zfbdt
          ef_zbd1t = lv_zbd1t.
      IF sy-subrc EQ 0  ##FM_SUBRC_OK.
        <ls_zsdsfit029>-faedt = lv_zfbdt + lv_zbd1t.
      ENDIF.

      <ls_zsdsfit029>-wrbtr = COND #( WHEN ls_bank_vendor-shkzg = 'H'
                                      THEN ls_bank_vendor-wrbtr * -1
                                      ELSE ls_bank_vendor-wrbtr ).

      "Get max sequence
      READ TABLE lt_max_seq INTO DATA(ls_max_seq)
        WITH KEY bukrs = <ls_zsdsfit029>-bukrs
                 belnr = <ls_zsdsfit029>-belnr
                 gjahr = <ls_zsdsfit029>-gjahr
                 buzei = <ls_zsdsfit029>-buzei
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_zsdsfit029>-seq = ls_max_seq-seq + 1.
      ELSE.
        <ls_zsdsfit029>-seq = 1.
      ENDIF.

      <ls_zsdsfit029>-erdat       = sy-datum.
      <ls_zsdsfit029>-erzmt       = sy-uzeit.
      <ls_zsdsfit029>-update_by   = sy-uname.
      <ls_zsdsfit029>-update_on   = sy-datum.
      <ls_zsdsfit029>-update_time = sy-uzeit.

      create_status_hist(
        EXPORTING
         is_log         = <ls_zsdsfit029>
         it_status_hist = it_status_hist
        CHANGING
         ct_status_hist = lt_new_status_hist
        ).

    ENDLOOP.

    IF lt_new_status_hist IS NOT INITIAL.
      MODIFY zsdsfit038 FROM TABLE lt_new_status_hist.
    ENDIF.

    MODIFY zsdsfit029 FROM TABLE lt_bplog.
    IF sy-subrc EQ 0.
      ev_rc = 0.

      APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      <ls_return>-id         = '38'.
      <ls_return>-log_msg_no = '000'.
      <ls_return>-type       = 'S'.
      <ls_return>-message_v1 = <ls_return>-message = TEXT-r01.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
